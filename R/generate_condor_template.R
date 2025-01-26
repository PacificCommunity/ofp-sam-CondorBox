#' Generate a Condor Template Script
#'
#' This function creates a script template for setting up and running CondorBox jobs.
#' The generated script includes placeholders for necessary variables such as 
#' GitHub credentials, Docker images, and Condor job specifications.
#'
#' @param output_file The name of the output file for the template script. Default is "launch_condor.R".
#' @return None. The function writes the template script to the specified file.
#' @export
#' @examples
#' \dontrun{
#' generate_condor_template()
#' }
generate_condor_template <- function(output_file = "launch_condor.R") {
  # Define the template content
  template <- '
# ---------------------------------------------------------------------------------
# Set variables for the remote server and CondorBox job (ignore if running locally)
# ---------------------------------------------------------------------------------

remote_user <- "your_username"                # Remote server username (e.g., "kyuhank")
remote_host <- "your_server_address"          # Remote server address (e.g., "nouofpsubmit.corp.spc.int")
github_pat <- "your_github_pat"               # GitHub Personal Access Token (e.g., ghp_....)
github_username <- "your_github_username"     # GitHub username (e.g., "kyuhank")
github_org <- "your_github_organisation"      # GitHub organisation name (e.g., "PacificCommunity")
github_repo <- "your_github_repository"       # GitHub repository name (e.g., "ofp-sam-docker4mfcl-example")
docker_image <- "your_docker_image"           # Docker image to use (e.g., "kyuhank/skj2025:1.0.4")
remote_dir <- "your_remote_directory"         # Remote directory for CondorBox (e.g., "MFCLtest")
condor_memory <- "6GB"                        # Memory request for the Condor job (e.g., "6GB")
condor_cpus <- 4                              # CPU request for the Condor job (e.g., 4)

# ---------------------------------------
# Run the job on Condor through CondorBox
# ---------------------------------------

CondorBox(
  remote_user = remote_user,         # Remote server username (e.g., "kyuhank")
  remote_host = remote_host,         # Remote server address (e.g., "nouofpsubmit.corp.spc.int")
  remote_dir = remote_dir,           # Remote directory for CondorBox (e.g., "MFCLtest")
  github_pat = github_pat,           # GitHub Personal Access Token (e.g., ghp_....)
  github_username = github_username, # GitHub username (e.g., "kyuhank")
  github_org = github_org,           # GitHub organisation name (e.g., "PacificCommunity")
  github_repo = github_repo,         # GitHub repository name (e.g., "ofp-sam-docker4mfcl-example")
  docker_image = docker_image,       # Docker image to use (e.g., "kyuhank/skj2025:1.0.4")
  condor_memory = condor_memory,     # Memory request for the Condor job (e.g., "6GB")
  condor_cpus = condor_cpus          # CPU request for the Condor job (e.g., 4)
)

# ----------------------------------------------------------
# Retrieve and synchronise the output from the remote server
# ----------------------------------------------------------

CondorUnbox(
  remote_user = remote_user,         # Remote server username (e.g., "kyuhank")
  remote_host = remote_host,         # Remote server address (e.g., "nouofpsubmit.corp.spc.int")
  remote_dir = remote_dir,           # Remote directory containing the output archive (e.g., "MFCLtest")
  local_git_dir = getwd(),           # Destination directory on the local machine (e.g., getwd())
  remote_output_file = "output_archive.tar.gz", # Name of the output archive (e.g., "output_archive.tar.gz")
  overwrite = FALSE                  # Set to TRUE to overwrite existing files
)
'

# Write the template to the specified file
writeLines(template, con = output_file)
message(sprintf("Template script saved to '%s'", output_file))
}
