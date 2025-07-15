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
    stream_out = "FALSE", # Default to streaming output
    stream_error = "FALSE",
    branch = "main",
    docker_image,
    target_folder = NULL,
    condor_cpus = NULL,
    condor_memory = NULL,
    condor_disk = NULL,
    make_options = "all", # Default make options
    rmclone_script = "yes", # Default to deleting clone_job.sh after run
    ghcr_login = FALSE,
    remote_os = "linux",   # "linux" (default) or "windows"
    condor_environment = NULL
) {
  # Helper function to normalize paths for Windows (local side)
  normalize_path <- function(path) {
    if (.Platform$OS.type == "windows") {
      normalizePath(path, winslash = "/", mustWork = FALSE)
    } else {
      path
    }
  }
  
  # Define file names
  clone_script <- "clone_job.sh"
  run_script <- "run_job.sh"
  env_file <- "job_env.txt"
  
  # Ensure paths are Windows-compatible (local side)
  remote_dir <- normalize_path(remote_dir)
  
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
  
  # Write the clone script
  writeLines(clone_script_content, con = clone_script, sep = "\n")
  
  # 2. Create the run_job.sh script
  run_script_content <- sprintf("
#!/usr/bin/env bash

# 1. Source the clone script to perform the git clone
source %s

# 2. Load environment variables from job_env.txt (if present)
if [[ -f \"%s\" ]]; then
  # Prefix each line with 'export' and source
  grep -E '^[A-Za-z_][A-Za-z0-9_]*=' \"%s\" \\
    | sed 's/^/export /' > env_exports.sh
  source env_exports.sh
fi

# 3. Save the working directory
if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    WORK_DIR=\"$GITHUB_TARGET_FOLDER\"
else
    WORK_DIR=\"$GITHUB_REPO\"
fi

# 4. Delete the clone script after sourcing it
rm -f %s

# 5. Unset the GitHub PAT
unset GITHUB_PAT

# 6. Change into the working directory and run make
cd \"$WORK_DIR\" || exit 1
echo \"Running make with options: %s\"
make %s

# 7. Archive the directory
cd ..
echo \"Archiving folder: $WORK_DIR...\"
tar -czvf output_archive.tar.gz \"$WORK_DIR\"
", 
                                clone_script,    # 1st %s: clone_job.sh
                                env_file,        # 2nd %s: job_env.txt
                                env_file,        # 3rd %s: job_env.txt
                                clone_script,    # 4th %s: clone_job.sh
                                make_options,    # 5th %s: make options for echo
                                make_options     # 6th %s: make options for make
  )
  
  # Write the run script
  writeLines(run_script_content, con = run_script, sep = "\n")
  
  # Environment variable processing with quoting to preserve spaces
  if (!is.null(condor_environment)) {
    if (is.list(condor_environment)) {
      # Handle list format: list(VAR1 = "value1", VAR2 = "value2")
      env_lines <- sapply(names(condor_environment), function(name) {
        # Wrap values in quotes to protect spaces
        sprintf("%s=\"%s\"", name, condor_environment[[name]])
      })
      writeLines(env_lines, env_file)
    } else if (is.character(condor_environment)) {
      # Handle string format: "VAR1=value1 VAR2=value2"
      # Split by whitespace but preserve = within each variable
      env_pairs <- unlist(strsplit(condor_environment, "\\s+"))
      # Filter for valid environment variable patterns
      valid_env <- env_pairs[grepl("^[A-Za-z_][A-Za-z0-9_]*=", env_pairs)]
      
      # Wrap each environment variable value in quotes
      quoted_env <- sapply(valid_env, function(pair) {
        # Split by first = only to handle values containing =
        parts <- strsplit(pair, "=", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          key <- parts[1]
          # Rejoin remaining parts in case value contains =
          value <- paste(parts[-1], collapse = "=")
          sprintf("%s=\"%s\"", key, value)
        } else {
          # Return original if no proper key=value structure
          pair
        }
      })
      
      writeLines(quoted_env, env_file)
    }
  }
  
  
  
  # 3. Create the Condor submit file
  condor_options <- c()
  if (!is.null(condor_cpus)) {
    condor_options <- c(condor_options, sprintf("request_cpus = %s", condor_cpus))
  }
  if (!is.null(condor_memory)) {
    condor_options <- c(condor_options, sprintf("request_memory = %s", condor_memory))
  }
  
  if (!is.null(condor_disk)) {
    condor_options <- c(condor_options, sprintf("request_disk = %s", condor_disk))
  }
  
  # Process environment variables
  environment_string <- ""
  if (!is.null(condor_environment)) {
    if (is.list(condor_environment)) {
      # Handle list format: list(VAR1 = "value1", VAR2 = "value2")
      env_vars <- sapply(names(condor_environment), function(name) {
        sprintf("%s=%s", name, condor_environment[[name]])
      })
      environment_string <- paste(env_vars, collapse = " ")
    } else if (is.character(condor_environment)) {
      # Handle string format: "VAR1=value1 VAR2=value2"
      environment_string <- condor_environment
    }
  }
  
  # Combine all condor options
  condor_options <- paste(condor_options, collapse = "\n")
  
  # 4. Create HTCondor submit file
  submit_file <- "condor_job.submit"
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
stream_out = %s
stream_error = %s
getenv = True
%s%s
Queue
", 
                                 docker_image, 
                                 run_script, 
                                 clone_script, 
                                 run_script,
                                 env_file,
                                 stream_out,
                                 stream_error,
                                 if(nzchar(environment_string)) sprintf("environment = %s\n", environment_string) else "",
                                 if(nzchar(condor_options)) paste0(condor_options, "\n") else ""
  )
  
  writeLines(submit_file_content, con = submit_file, sep = "\n")
  
  # 4. Check if the remote directory exists
  message("Checking if the remote directory exists...")
  system(sprintf("ssh %s@%s 'mkdir -p %s'", remote_user, remote_host, remote_dir))
  
  # 5. Transfer scripts and submit file to remote server
  message("Transferring the scripts and submit file to the remote server...")
  system(sprintf("scp %s %s@%s:%s/%s", clone_script, remote_user, remote_host, remote_dir, clone_script))
  system(sprintf("scp %s %s@%s:%s/%s", run_script, remote_user, remote_host, remote_dir, run_script))
  system(sprintf("scp %s %s@%s:%s/%s", env_file, remote_user, remote_host, remote_dir, env_file))
  system(sprintf("scp %s %s@%s:%s/%s", submit_file, remote_user, remote_host, remote_dir, submit_file))
  
  # Introduce a delay to ensure the files are written and accessible
  message("Waiting briefly to ensure file transfer completion...")
  Sys.sleep(5)  # Wait for 5 seconds
  
  # 5.5. If ghcr_login option is enabled, configure credential helper and perform docker login on the remote server
  if (ghcr_login) {
    if (tolower(remote_os) == "windows") {
      message("Configuring docker credential helper on remote Windows server...")
      config_cmd <- "mkdir %USERPROFILE%\\.docker && echo \"{\\\"credsStore\\\": \\\"wincred\\\"}\" > %USERPROFILE%\\.docker\\config.json"
      system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, config_cmd))
      login_cmd <- sprintf("echo %s | docker login ghcr.io -u %s --password-stdin", 
                           shQuote(github_pat), github_username)
    } else {
      message("Configuring minimal docker config on remote Linux server to avoid credential helper errors...")
      
      config_cmd <- "mkdir -p /tmp/docker_config && echo '{}' > /tmp/docker_config/config.json"
      system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, config_cmd))
      login_cmd <- sprintf("echo %s | DOCKER_CONFIG=/tmp/docker_config docker login ghcr.io -u %s --password-stdin", 
                           shQuote(github_pat), github_username)
    }
    message("Performing docker login on remote server to bypass pull limits...")
    login_status <- system(sprintf("ssh %s@%s '%s'", remote_user, remote_host, login_cmd))
    if (login_status == 0) {
      message("Docker login on remote server succeeded.")
    } else {
      message("Docker login on remote server failed.")
    }
  }
  
  # 6. Submit the Condor job on the remote server
  message("Submitting the Condor job on the remote server...")
  tryCatch({
    system(sprintf("ssh %s@%s 'cd %s && condor_submit %s'", remote_user, remote_host, remote_dir, submit_file))
    message("Condor job submitted successfully!")
  }, error = function(e) {
    message("Condor submission failed: ", e$message)
  })
  
  # 7. Delete clone_job.sh from the remote server regardless of submission success
  if(rmclone_script == "yes") {
    message("Waiting for 20 seconds before deleting clone_job.sh from the remote server...")
    Sys.sleep(20)  # Wait for 10 seconds
    message("Deleting clone_job.sh from the remote server...")
    system(sprintf("ssh %s@%s 'rm -f %s/%s'", remote_user, remote_host, remote_dir, clone_script))
  } else {
    message("Skipping deletion of clone_job.sh from the remote server as per user request.")
  }
  
  # 8. Clean up local files
  unlink(c(clone_script, run_script, submit_file))
  
  message("Cleanup completed.")
}


