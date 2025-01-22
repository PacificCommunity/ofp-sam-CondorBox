
submit_github_condor_job <- function(
    remote_user,      # Remote server username
    remote_host,      # Remote server address
    remote_dir,       # Remote working directory
    github_pat,       # GitHub Personal Access Token
    github_username,  # GitHub username
    github_org,       # GitHub organization name
    github_repo,      # GitHub repository name
    docker_image,     # Docker image to use
    target_folder = NULL,   # Optional: folder within the repository for sparse checkout & make execution
    archive_folder = NULL   # Optional: folder (relative to repository root or target_folder) to archive as output
) {
  # 1. Fixed file name for the Bash script
  bash_script <- "run_job.sh"  # Fixed name for the Bash script
  
  # Create optional export for TARGET_FOLDER if provided
  target_export <- if (!is.null(target_folder)) {
    sprintf("export GITHUB_TARGET_FOLDER='%s'", target_folder)
  } else {
    ""
  }
  
  # Create optional export for ARCHIVE_FOLDER if provided
  archive_export <- if (!is.null(archive_folder)) {
    sprintf("export ARCHIVE_FOLDER='%s'", archive_folder)
  } else {
    ""
  }
  
  # Create the Bash script content.
  # Always clone the entire repository. If GITHUB_TARGET_FOLDER is set, use sparse checkout.
  # For make execution, change into GITHUB_TARGET_FOLDER if set; otherwise use GITHUB_REPO.
  # When archiving, if ARCHIVE_FOLDER is set, archive that; if not and GITHUB_TARGET_FOLDER exists, archive that;
  # otherwise, archive the entire repository.
  cat(sprintf("
#!/bin/bash

# Set environment variables
export GITHUB_PAT='%s'
export GITHUB_USERNAME='%s'
export GITHUB_ORGANIZATION='%s'
export GITHUB_REPO='%s'
%s
%s

# Clone the repository or, if GITHUB_TARGET_FOLDER is set, perform a sparse checkout.
if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    echo \"Cloning specific folder ($GITHUB_TARGET_FOLDER) from the repository...\"
    git init
    git remote add origin https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git
    git config core.sparseCheckout true
    echo \"$GITHUB_TARGET_FOLDER/\" >> .git/info/sparse-checkout
    git pull origin main
else
    echo \"Cloning the entire repository...\"
    git clone https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git
fi

# Change into the appropriate directory for make execution.
if [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    cd \"$GITHUB_TARGET_FOLDER\" || exit 1
else
    cd \"$GITHUB_REPO\" || exit 1
fi
echo \"Running make...\"
make

# Go back to the parent directory (repository root)
cd ..

# Determine which folder to archive:
# If ARCHIVE_FOLDER is set, archive that;
# Else if GITHUB_TARGET_FOLDER is set, archive that;
# Otherwise, archive the entire repository.
if [[ -n \"$ARCHIVE_FOLDER\" ]]; then
    archive_folder=\"$ARCHIVE_FOLDER\"
elif [[ -n \"$GITHUB_TARGET_FOLDER\" ]]; then
    archive_folder=\"$GITHUB_TARGET_FOLDER\"
else
    archive_folder=\"$GITHUB_REPO\"
fi

echo \"Archiving folder: $archive_folder...\"
tar -czvf output_archive.tar.gz \"$archive_folder\"

# Clean up sensitive information
unset GITHUB_PAT
", 
              github_pat, github_username, github_org, github_repo,
              target_export, archive_export), 
      file = bash_script)
  
  # 2. Create the HTCondor submit file content
  submit_file <- "condor_job.submit"  # Fixed name for the submit file
  cat(sprintf("
Universe   = docker
DockerImage = %s
Executable = /bin/bash
Arguments  = run_job.sh
ShouldTransferFiles = YES
TransferInputFiles = run_job.sh
TransferOutputFiles = output_archive.tar.gz
Output     = condor_job.out
Error      = condor_job.err
Log        = condor_job.log
Queue
", docker_image), file = submit_file)
  
  # 3. Check if the remote directory exists; if not, create it
  message("Checking if the remote directory exists...")
  system(sprintf("ssh %s@%s 'mkdir -p %s'", remote_user, remote_host, remote_dir))
  
  # 4. Transfer the Bash script and submit file to the remote server
  message("Transferring the Bash script and submit file to the remote server...")
  system(sprintf("scp %s %s@%s:%s/%s", bash_script, remote_user, remote_host, remote_dir, bash_script))
  system(sprintf("scp %s %s@%s:%s/%s", submit_file, remote_user, remote_host, remote_dir, submit_file))
  
  # 5. Submit the Condor job on the remote server
  message("Submitting the Condor job on the remote server...")
  system(sprintf("ssh %s@%s 'cd %s && condor_submit %s'", remote_user, remote_host, remote_dir, submit_file))
  
  # 6. Clean up local files
  unlink(c(bash_script, submit_file))
  
  message("Condor job submitted successfully!")
}
