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
  
  # Function to check if a container exists
  container_exists <- function(name) {
    existing <- system(sprintf("docker ps -a --filter name=^/%s$ --format '{{.Names}}'", name), intern = TRUE)
    return(name %in% existing)
  }
  
  # Function to check if a container is running
  container_running <- function(name) {
    running <- system(sprintf("docker ps --filter name=^/%s$ --filter status=running --format '{{.Names}}'", name), intern = TRUE)
    return(name %in% running)
  }
  
  # Function to start a stopped container
  start_container <- function(name) {
    message(sprintf("Starting existing container '%s'...", name))
    system(sprintf("docker start %s", name), ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  
  # Function to run a new container
  run_container <- function(image, name, host_port, container_port, password) {
    run_cmd <- sprintf(
      "docker run -d -p %d:%d --name %s -e PASSWORD=%s %s",
      host_port,
      container_port,
      name,
      password,
      image
    )
    message(sprintf("Executing: %s", run_cmd))
    system(run_cmd, intern = FALSE, ignore.stderr = FALSE)
  }
  
  # Function to stop a running container
  stop_container <- function(name) {
    message(sprintf("Stopping container '%s'...", name))
    system(sprintf("docker stop %s", name), ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  
  # Function to remove a container
  remove_container <- function(name) {
    message(sprintf("Removing container '%s'...", name))
    system(sprintf("docker rm %s", name), ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  
  if (action == "start") {
    if (container_exists(container_name)) {
      if (container_running(container_name)) {
        message(sprintf("Container '%s' is already running.", container_name))
      } else {
        start_container(container_name)
        # Optionally, verify if the container started successfully
        Sys.sleep(2)  # Wait a moment for the container to start
        if (container_running(container_name)) {
          message("RStudio Server has been started.")
        } else {
          message("Failed to start RStudio Server. Check container logs for details.")
          system(sprintf("docker logs %s", container_name), wait = TRUE)
        }
      }
    } else {
      run_container(image, container_name, host_port, container_port, password)
      # Wait briefly to ensure the container starts
      Sys.sleep(3)
      # Check if the container is running
      if (container_running(container_name)) {
        message("RStudio Server is running.")
        url <- sprintf("http://localhost:%d", host_port)
        message(sprintf("Access it at: %s", url))
        try({
          browseURL(url)
        }, silent = TRUE)
      } else {
        message("Failed to start RStudio Server.")
        # Optionally, display container logs for debugging
        system(sprintf("docker logs %s", container_name), wait = TRUE)
      }
    }
    
  } else if (action == "stop") {
    if (container_running(container_name)) {
      stop_container(container_name)
    } else {
      message(sprintf("Container '%s' is not running.", container_name))
    }
    
    if (container_exists(container_name)) {
      remove_container(container_name)
      message(sprintf("Container '%s' has been removed.", container_name))
    } else {
      message(sprintf("Container '%s' does not exist.", container_name))
    }
  }
}

