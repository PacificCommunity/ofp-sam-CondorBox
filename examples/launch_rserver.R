
## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## This script launches the RStudio server in a Docker container 
## By running this script, the user will be able to access the RStudio server via a web browser.
## This approach helps maintain a consistent environment for the project across users and ensures reproducibility.
## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(CondorBox)

## Start the RStudio server in the Docker container using the CondorBox package rocker/rstudio image is used by default
## Once the container is started, following command can also be used to access the same container
manage_rstudio_server(action = "start", 
                      image = "rocker/rstudio",  ## this is the default image
                      host_port = 9999, 
                      password = "FAME")  # login password for RStudio server (ID: rstudio, Password: FAME)


## Stop the RStudio server in the Docker container using the CondorBox package
#manage_rstudio_server(action = "stop")

## clean up the Docker resources (e.g., containers and images) interactively
## if you fail to log in to the RStudio server, clean up the container and image using the following command
#clean_docker_resources_interactive()


#### on the Rsudio server, ssh key setup can be done using the following command, which allows the user to clone the repository from GitHub through the RStudio server
# CondorBox::setup_github_ssh(
#   email = "your_email@example.com",             # GitHub email (e.g., "kh2064@gmail.com")
#   github_username = "your_github_username",     # GitHub username (e.g., "kyuhank")
#   github_token = "your_github_token_here"       # GitHub Personal Access Token (e.g., ghp_....)
# )
