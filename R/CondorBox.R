#' Run Job via HTCondor and Docker with Make Options
#'
#' This function creates two Bash script files. The first script, \code{clone_job.sh},
#' handles cloning a GitHub repository (or a specific folder via sparse checkout) using the provided
#' GitHub Personal Access Token (PAT). The second script, \code{run_job.sh}, sources the clone script
#' to perform the clone, stores the working directory in a variable, schedules the deletion of the clone
#' script (to remove sensitive PAT information from disk), and then executes the remaining commands
#' (running \code{make} with user-defined options and archiving the appropriate folder). An HTCondor submit
#' file is also generated and transferred to a remote server for job submission.
#'
#' @param remote_user Character. Remote server username.
#' @param remote_host Character. Remote server address.
#' @param remote_dir Character. Remote working directory.
#' @param github_pat Character. GitHub Personal Access Token.
#' @param github_username Character. GitHub username.
#' @param github_org Character. GitHub organisation name.
#' @param github_repo Character. GitHub repository name.
#' @param docker_image Character. Docker image to use.
#' @param target_folder Character, optional. Specific folder within the repository to clone (via sparse checkout)
#'   and where \code{make} is executed. If not provided, the entire repository is used.
#' @param condor_cpus Numeric, optional. The number of CPUs to request in the HTCondor job.
#' @param condor_memory Character, optional. The amount of memory to request (e.g., "4GB") in the HTCondor job.
#' @param make_options Character, optional. Options to pass to the \code{make} command. Defaults to \code{"all"}.
#'
#' @return No return value; side effects include the creation and transfer of script files to the remote server
#'   and submission of an HTCondor job.
#'
#' @export




CondorBox <- function(
    remote_user,
    remote_host,
    remote_dir,
    github_pat,
    github_username,
    github_org,
    github_repo,
    stream_error = "FALSE",
    branch = "main",
    docker_image,
    target_folder = NULL,
    condor_cpus = NULL,
    condor_memory = NULL,
    condor_disk = NULL,
    make_options = "all",
    rmclone_script = "yes",
    ghcr_login = FALSE,
    remote_os = "linux",
    condor_environment = NULL,
    custom_batch_name = NULL
) {
  
  # Define file names
  clone_script <- "clone_job.sh"
  run_script <- "run_job.sh"
  env_file <- "job_env.txt"
  submit_file <- "condor_job.submit"
  
  # Normalize paths for Windows compatibility
  if (.Platform$OS.type == "windows") {
    remote_dir <- normalizePath(remote_dir, winslash = "/", mustWork = FALSE)
  }
  
  # 1. Create the clone_job.sh script
  clone_script_content <- sprintf("
#!/bin/bash
export GITHUB_PAT='%s'
export GITHUB_USERNAME='%s'
export GITHUB_ORGANIZATION='%s'
export GITHUB_REPO='%s'
export GITHUB_BRANCH='%s'
%s

if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    git init
    git remote add origin https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git
    git config core.sparseCheckout true
    echo \"$GITHUB_TARGET_FOLDER/\" >> .git/info/sparse-checkout
    git pull origin $GITHUB_BRANCH
else
    git clone -b $GITHUB_BRANCH https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git
fi
", 
                                  github_pat, github_username, github_org, github_repo, branch,
                                  if (!is.null(target_folder)) sprintf("export GITHUB_TARGET_FOLDER='%s'", target_folder) else "")
  
  writeLines(clone_script_content, con = clone_script, sep = "\n")
  
  # 2. Create the run_job.sh script
  run_script_content <- sprintf("
#!/usr/bin/env bash

# Execute the clone script
source %s

# Load environment variables from job_env.txt if present
if [[ -f \"%s\" ]]; then
  grep -E '^[A-Za-z_][A-Za-z0-9_]*=' \"%s\" | sed 's/^/export /' > env_exports.sh
  source env_exports.sh
fi

# Determine working directory
if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    WORK_DIR=\"$GITHUB_TARGET_FOLDER\"
else
    WORK_DIR=\"$GITHUB_REPO\"
fi

# Unset GitHub PAT for security
unset GITHUB_PAT

# Change to working directory and run make
cd \"$WORK_DIR\" || exit 1
echo \"Running make with options: %s\"
make %s

# Archive the results
cd ..
echo \"Archiving folder: $WORK_DIR...\"
tar -czvf output_archive.tar.gz \"$WORK_DIR\"
", 
                                clone_script, env_file, env_file, make_options, make_options)
  
  writeLines(run_script_content, con = run_script, sep = "\n")
  
  # 3. Create environment file if needed
  if (!is.null(condor_environment)) {
    if (is.list(condor_environment)) {
      # Handle list format: list(VAR1 = "value1", VAR2 = "value2")
      env_lines <- sapply(names(condor_environment), function(name) {
        sprintf("%s=\"%s\"", name, condor_environment[[name]])
      })
      writeLines(env_lines, env_file)
    } else if (is.character(condor_environment)) {
      # Handle string format: "VAR1=value1 VAR2=value2"
      env_pairs <- unlist(strsplit(condor_environment, "\\s+"))
      valid_env <- env_pairs[grepl("^[A-Za-z_][A-Za-z0-9_]*=", env_pairs)]
      
      quoted_env <- sapply(valid_env, function(pair) {
        parts <- strsplit(pair, "=", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          key <- parts[1]
          value <- paste(parts[-1], collapse = "=")
          sprintf("%s=\"%s\"", key, value)
        } else {
          pair
        }
      })
      writeLines(quoted_env, env_file)
    }
  }
  
  # 4. Build Condor resource requirements
  condor_options <- c()
  if (!is.null(condor_cpus)) condor_options <- c(condor_options, sprintf("request_cpus = %s", condor_cpus))
  if (!is.null(condor_memory)) condor_options <- c(condor_options, sprintf("request_memory = %s", condor_memory))
  if (!is.null(condor_disk)) condor_options <- c(condor_options, sprintf("request_disk = %s", condor_disk))
  
  # Process environment variables for Condor
  environment_string <- ""
  if (!is.null(condor_environment)) {
    if (is.list(condor_environment)) {
      env_vars <- sapply(names(condor_environment), function(name) {
        sprintf("%s=%s", name, condor_environment[[name]])
      })
      environment_string <- paste(env_vars, collapse = " ")
    } else if (is.character(condor_environment)) {
      environment_string <- condor_environment
    }
  }
  
  # **MODIFIED: Set batch name logic**
  # If custom_batch_name is provided, use it; otherwise use $(ClusterId) only
  if (!is.null(custom_batch_name)) {
    batch_name_template <- custom_batch_name
  } else {
    batch_name_template <- "$(ClusterId)"  # Just use the job ID number
  }
  
  condor_options <- paste(condor_options, collapse = "\n")
  
  # 5. Create HTCondor submit file
  submit_file_content <- sprintf("
Universe   = docker
DockerImage = %s
Executable = /bin/bash
Arguments  = %s
ShouldTransferFiles = YES
TransferInputFiles = %s, %s, %s
TransferOutputFiles = output_archive.tar.gz
Output     = condor_job.out
Error      = condor_job.err
Log        = condor_job.log
stream_error = %s
getenv = True
batch_name = %s
%s%s
Queue
", 
                                 docker_image, run_script, clone_script, run_script, env_file,
                                 stream_error, batch_name_template,
                                 if(nzchar(environment_string)) sprintf("environment = %s\n", environment_string) else "",
                                 if(nzchar(condor_options)) paste0(condor_options, "\n") else "")
  
  writeLines(submit_file_content, con = submit_file, sep = "\n")
  
  # 6. Create remote directory
  message("Creating remote directory...")
  system(sprintf("ssh %s@%s 'mkdir -p %s'", remote_user, remote_host, remote_dir))
  
  # 7. Transfer files to remote server
  message("Transferring files...")
  system(sprintf("scp %s %s@%s:%s/%s", clone_script, remote_user, remote_host, remote_dir, clone_script))
  system(sprintf("scp %s %s@%s:%s/%s", run_script, remote_user, remote_host, remote_dir, run_script))
  system(sprintf("scp %s %s@%s:%s/%s", env_file, remote_user, remote_host, remote_dir, env_file))
  system(sprintf("scp %s %s@%s:%s/%s", submit_file, remote_user, remote_host, remote_dir, submit_file))
  
  # Brief wait for file transfer completion
  Sys.sleep(1)
  
  # 8. Docker login if needed
  if (ghcr_login) {
    message("Performing docker login...")
    if (tolower(remote_os) == "windows") {
      config_cmd <- "mkdir %USERPROFILE%\\.docker && echo \"{\\\"credsStore\\\": \\\"wincred\\\"}\" > %USERPROFILE%\\.docker\\config.json"
      system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, config_cmd))
      login_cmd <- sprintf("echo %s | docker login ghcr.io -u %s --password-stdin", 
                           shQuote(github_pat), github_username)
    } else {
      config_cmd <- "mkdir -p /tmp/docker_config && echo '{}' > /tmp/docker_config/config.json"
      system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, config_cmd))
      login_cmd <- sprintf("echo %s | DOCKER_CONFIG=/tmp/docker_config docker login ghcr.io -u %s --password-stdin", 
                           shQuote(github_pat), github_username)
    }
    system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, login_cmd))
  }
  
  # 9. Submit job and capture output
  message("Submitting job...")
  submit_result <- system(sprintf("ssh %s@%s 'cd %s && condor_submit %s'", 
                                  remote_user, remote_host, remote_dir, submit_file),
                          intern = TRUE)
  
  # **IMPROVED JOB ID EXTRACTION**
  # HTCondor outputs something like: "1 job(s) submitted to cluster 8445"
  job_id <- NULL
  
  # Print submit result for debugging
  message("Submit result:")
  for (line in submit_result) {
    message(line)
  }
  
  # Extract job ID from submit output
  for (line in submit_result) {
    # Look for patterns like "submitted to cluster XXXX" or "cluster XXXX"
    if (grepl("submitted to cluster|cluster", line, ignore.case = TRUE)) {
      # Extract all numbers from the line
      numbers <- regmatches(line, gregexpr("\\d+", line))[[1]]
      if (length(numbers) > 0) {
        # The last number is typically the cluster ID (job ID)
        job_id <- numbers[length(numbers)]
        break
      }
    }
  }
  
  # Alternative extraction method if above fails
  if (is.null(job_id)) {
    for (line in submit_result) {
      # Look for any line containing numbers
      if (grepl("\\d+", line)) {
        numbers <- regmatches(line, gregexpr("\\d+", line))[[1]]
        if (length(numbers) > 0) {
          job_id <- numbers[length(numbers)]
          break
        }
      }
    }
  }
  
  # **MODIFIED: Set actual batch name based on job ID**
  if (!is.null(custom_batch_name)) {
    actual_batch_name <- custom_batch_name
  } else {
    actual_batch_name <- job_id  # Just use the job ID as batch name
  }
  
  # Report job submission results
  if (!is.null(job_id) && job_id != "") {
    message(sprintf("Job submitted successfully! Job ID: %s, Batch Name: %s", job_id, actual_batch_name))
    
    # 10. Monitor job status and delete clone script when job starts running
    if (rmclone_script == "yes") {
      message("Setting up clone script monitoring...")
      
      # Simple approach: wait for job to start running, then delete clone script
      cleanup_function <- function() {
        max_attempts <- 120  # 20 minutes maximum waiting
        attempt <- 0
        
        while (attempt < max_attempts) {
          # Check job status
          status_cmd <- sprintf("ssh %s@%s 'condor_q %s -format \"%%s\" JobStatus 2>/dev/null'", 
                                remote_user, remote_host, job_id)
          status_result <- system(status_cmd, intern = TRUE, ignore.stderr = TRUE)
          
          if (length(status_result) == 0) {
            message("Job no longer in queue - stopping monitoring")
            break
          }
          
          status_code <- as.numeric(status_result[1])
          
          if (status_code == 2) {  # Job is RUNNING
            message("Job is now RUNNING - deleting clone script")
            system(sprintf("ssh %s@%s 'rm -f %s/%s'", 
                           remote_user, remote_host, remote_dir, clone_script))
            message("Clone script deleted successfully")
            break
          } else if (status_code == 1) {  # Job is IDLE
            message("Job is IDLE - waiting...")
          } else if (status_code %in% c(3, 4)) {  # Job REMOVED or COMPLETED
            message("Job finished - stopping monitoring")
            break
          } else if (status_code == 5) {  # Job is HELD
            message("Job is HELD - continuing to monitor...")
          }
          
          Sys.sleep(10)  # Wait 10 seconds before next check
          attempt <- attempt + 1
        }
      }
      
      # Start monitoring (this will run in current session)
      cleanup_function()
    }
    
  } else {
    message("Job submitted but could not extract job ID")
    message("Submit output was:")
    for (line in submit_result) {
      message(paste("  ", line))
    }
    job_id <- "unknown"
  }
  
  # 11. Clean up local files
  unlink(c(clone_script, run_script, env_file, submit_file))
  
  message("Process completed.")
  return(job_id)
}





