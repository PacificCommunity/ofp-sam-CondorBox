#' Manage RStudio Server Containers
#'
#' This function starts or stops an RStudio Server container using Docker or Podman.
#' It supports creating new containers or restarting existing ones.
#'
#' @param action A string indicating the action to perform: "start" or "stop".
#' @param image The Docker image to use. Default is "rocker/rstudio".
#' @param container_name The name of the container. Default is "rstudio".
#' @param host_port The port on the host machine. Default is 8787.
#' @param container_port The port inside the container. Default is 8787.
#' @export

manage_rstudio_server <- function(
    action = c("start", "stop"),
    image = "kyuhank/skj2025:2.0.3",  # Use your custom image
    container_name = "rstudio33",
    host_port = 9999,                  # Set desired host port
    container_port = 8787,
    password = "yourpassword"          # Set your desired password
) {
  action <- match.arg(action)
  
  if (action == "start") {
    # Check if the container already exists
    existing_containers <- system(sprintf("docker ps -a --filter name=%s --format '{{.Names}}'", container_name), intern = TRUE)
    if (container_name %in% existing_containers) {
      message(sprintf("Container '%s' already exists. Removing existing container...", container_name))
      system(sprintf("docker stop %s", container_name), ignore.stderr = TRUE)
      system(sprintf("docker rm %s", container_name), ignore.stderr = TRUE)
    }
    
    # Run the container with port mapping and password
    run_cmd <- sprintf("docker run -d -p %d:%d --name %s -e PASSWORD=%s %s", 
                       host_port, container_port, container_name, password, image)
    message(sprintf("Running command: %s", run_cmd))
    system(run_cmd)
    
    # Wait briefly to ensure the container starts
    Sys.sleep(3)
    
    # Check if the container is running
    running <- system(sprintf("docker ps --filter name=%s --filter status=running --format '{{.Names}}'", container_name), intern = TRUE)
    if (container_name %in% running) {
      message("RStudio Server is running.")
      url <- sprintf("http://localhost:%d", host_port)
      message(sprintf("Access it at: %s", url))
      try(browseURL(url), silent = TRUE)
    } else {
      message("Failed to start RStudio Server.")
      # Optionally, display container logs for debugging
      system(sprintf("docker logs %s", container_name), wait = TRUE)
    }
    
  } else if (action == "stop") {
    # Stop the container if it's running
    running <- system(sprintf("docker ps --filter name=%s --filter status=running --format '{{.Names}}'", container_name), intern = TRUE)
    if (container_name %in% running) {
      system(sprintf("docker stop %s", container_name))
      message(sprintf("Container '%s' has been stopped.", container_name))
    } else {
      message(sprintf("Container '%s' is not running.", container_name))
    }
    
    # Remove the container if it exists
    existing_containers <- system(sprintf("docker ps -a --filter name=%s --format '{{.Names}}'", container_name), intern = TRUE)
    if (container_name %in% existing_containers) {
      system(sprintf("docker rm %s", container_name))
      message(sprintf("Container '%s' has been removed.", container_name))
    }
  }
}

