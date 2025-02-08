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
    branch = "main",
    docker_image,
    target_folder = NULL,
    condor_cpus = NULL,
    condor_memory = NULL,
    make_options = "all" # Default make options
) {
  # Helper function to normalize paths for Windows
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
  
  # Ensure paths are Windows-compatible
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
#!/bin/bash

# Source the clone script to perform the git clone
source %s

# Save the working directory
if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    WORK_DIR=\"$GITHUB_TARGET_FOLDER\"
else
    WORK_DIR=\"$GITHUB_REPO\"
fi

# Delete the clone script after sourcing it
rm -f %s

# Unset the GitHub PAT
unset GITHUB_PAT

# Change into the working directory and run make
cd \"$WORK_DIR\" || exit 1
echo \"Running make with options: %s\"
make %s

# Archive the directory
cd ..
echo \"Archiving folder: $WORK_DIR...\"
tar -czvf output_archive.tar.gz \"$WORK_DIR\"
", clone_script, clone_script, make_options, make_options)
  
  # Write the run script
  writeLines(run_script_content, con = run_script, sep = "\n")
  
  # 3. Create the Condor submit file
  condor_options <- c()
  if (!is.null(condor_cpus)) {
    condor_options <- c(condor_options, sprintf("request_cpus = %s", condor_cpus))
  }
  if (!is.null(condor_memory)) {
    condor_options <- c(condor_options, sprintf("request_memory = %s", condor_memory))
  }
  condor_options <- paste(condor_options, collapse = "\n")
  
  submit_file <- "condor_job.submit"
  submit_file_content <- sprintf("
Universe   = docker
DockerImage = %s
Executable = /bin/bash
Arguments  = %s
ShouldTransferFiles = YES
TransferInputFiles = %s, %s
TransferOutputFiles = output_archive.tar.gz
Output     = condor_job.out
Error      = condor_job.err
Log        = condor_job.log
environment = IS_CONDOR_RUN=true
%s
Queue
", 
                                 docker_image, run_script, clone_script, run_script, condor_options)
  
  writeLines(submit_file_content, con = submit_file, sep = "\n")
  
  # 4. Check if the remote directory exists
  message("Checking if the remote directory exists...")
  system(sprintf("ssh %s@%s 'mkdir -p %s'", remote_user, remote_host, remote_dir))
  
  # 5. Transfer scripts and submit file to remote server
  message("Transferring the scripts and submit file to the remote server...")
  system(sprintf("scp %s %s@%s:%s/%s", clone_script, remote_user, remote_host, remote_dir, clone_script))
  system(sprintf("scp %s %s@%s:%s/%s", run_script, remote_user, remote_host, remote_dir, run_script))
  system(sprintf("scp %s %s@%s:%s/%s", submit_file, remote_user, remote_host, remote_dir, submit_file))
  
  # Introduce a delay to ensure the files are written and accessible
  message("Waiting briefly to ensure file transfer completion...")
  Sys.sleep(5)  # Wait for 5 seconds
  
  # 6. Submit the Condor job on the remote server
  message("Submitting the Condor job on the remote server...")
  tryCatch({
    system(sprintf("ssh %s@%s 'cd %s && condor_submit %s'", remote_user, remote_host, remote_dir, submit_file))
    message("Condor job submitted successfully!")
  }, error = function(e) {
    message("Condor submission failed: ", e$message)
  })
  
  # 7. Delete clone_job.sh from the remote server regardless of submission success
  Sys.sleep(10)  # Wait for 10 seconds
  message("Deleting clone_job.sh from the remote server...")
  system(sprintf("ssh %s@%s 'rm -f %s/%s'", remote_user, remote_host, remote_dir, clone_script))
  
  # 8. Clean up local files
  unlink(c(clone_script, run_script, submit_file))
  
  message("Cleanup completed.")
}