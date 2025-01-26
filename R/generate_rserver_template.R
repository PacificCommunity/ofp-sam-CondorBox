#' Generate an RStudio Server Docker Template
#'
#' This function creates a script template for launching the RStudio server in a Docker container.
#' The generated script includes placeholders for the Docker image, container name, and other settings.
#'
#' @param output_file The name of the output file for the template script. Default is "launch_rserver.R".
#' @return None. The function writes the template script to the specified file.
#' @export
#' @examples
#' \dontrun{
#' generate_rserver_template()
#' }
generate_rserver_template <- function(output_file = "launch_rserver.R") {
  # Define the template content
  template <- '
## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## This script launches the RStudio server in a Docker container for this project.
## By running this script, the user will be able to access the RStudio server via a web browser.
## The user can then clone the project repository on the RStudio server and work on the project.
## This approach helps maintain a consistent environment for the project across users and ensures reproducibility.
## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Install the CondorBox package from GitHub (force reinstallation if needed)
#remotes::install_github("PacificCommunity/ofp-sam-CondorBox", force = TRUE) ## Force reinstallation if updates are needed

## Specify the Docker image and container name for this project
image_for_this_project <- "your_docker_image"      # Docker image for the project (e.g., "kyuhank/skj2025:1.0.3")
container_name_for_this_project <- "your_container_name" # Container name for the project (e.g., "skj_example")

## Launch the RStudio server in a Docker container using the CondorBox package
## This will start the RStudio server in a Docker container and allow access via a web browser.
CondorBox::manage_rstudio_server(
  action = "start", 
  image = image_for_this_project,
  container_name = container_name_for_this_project
)

## Stop the RStudio server in the Docker container using the CondorBox package
CondorBox::manage_rstudio_server(
  action = "stop", 
  image = image_for_this_project, 
  container_name = container_name_for_this_project
)

################################################################################
## Copy the following code to the R console to set up the SSH keys for GitHub ##
################################################################################

# CondorBox::setup_github_ssh(
#   email = "your_email@example.com",             # GitHub email (e.g., "kh2064@gmail.com")
#   github_username = "your_github_username",     # GitHub username (e.g., "kyuhank")
#   github_token = "your_github_token_here"       # GitHub Personal Access Token (e.g., ghp_....)
# )

## Clean up the Docker resources interactively if needed
# CondorBox::clean_docker_resources_interactive()

## Clean up the GitHub SSH keys interactively if needed
# CondorBox::clean_git_ssh_keys_interactive("your_github_token_here")
'

# Write the template to the specified file
writeLines(template, con = output_file)
message(sprintf("Template script saved to '%s'", output_file))
}
