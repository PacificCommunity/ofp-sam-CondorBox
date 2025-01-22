
generate_and_run_bash <- function(
    remote_user,      # Remote server username
    remote_host,      # Remote server address
    remote_dir,       # Remote working directory
    github_pat,       # GitHub Personal Access Token
    github_username,  # GitHub username
    github_org,       # GitHub organization name
    github_repo,      # GitHub repository name
    docker_image,     # Docker image to use
    target_folder = NULL # Optional: specific folder within the repository (ignored for archiving)
) {
  # 1. Fixed file name for the Bash script
  bash_script <- "run_job.sh"  # Fixed name for the bash script
  
  # Create the Bash script content
  cat(sprintf("
#!/bin/bash

# Set environment variables
export GITHUB_PAT='%s'
export GITHUB_USERNAME='%s'
export GITHUB_ORGANIZATION='%s'
export GITHUB_REPO='%s'
%s

# Clone the entire repository
echo \"Cloning the entire repository...\"
git clone https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git

# Change into the repository directory and run make
cd $GITHUB_REPO || exit 1
echo \"Running make...\"
make

# Go back to parent directory and archive the entire repository folder (including outputs)
cd ..
echo \"Archiving the entire repository folder...\"
tar -czvf output_archive.tar.gz \"$GITHUB_REPO\"

# Clean up sensitive information
unset GITHUB_PAT
", 
              github_pat, github_username, github_org, github_repo,
              if (!is.null(target_folder)) sprintf("export GITHUB_TARGET_FOLDER='%s'", target_folder) else ""), 
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

