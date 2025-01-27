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
    image = "rocker/rstudio",
    container_name = "rstudio",
    host_port = 8787,
    container_port = 8787,
    password = "your_password",
    is_rserver_run = "true" # Default value for IS_RSERVER_RUN
) {
  # Determine whether to use Docker or Podman
  cli <- if (tryCatch(
    grepl("podman", system("docker --version", intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) FALSE
  )) {
    "podman"
  } else {
    "docker"
  }
  
  message(sprintf("Using CLI: %s", cli))
  action <- match.arg(action)
  
  # Function to free up the port if rserver is running locally
  free_port_if_rserver_running <- function(port) {
    check_cmd <- sprintf("netstat -tulpn 2>/dev/null | grep ':%d ' | grep rserver || true", port)
    netstat_output <- system(check_cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(netstat_output) > 0) {
      message(sprintf("Detected 'rserver' using port %d. Attempting to stop it...", port))
      stop_cmd <- "sudo rstudio-server stop"
      system(stop_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
      
      # Recheck if the port is freed
      netstat_output_after <- system(check_cmd, intern = TRUE, ignore.stderr = TRUE)
      if (length(netstat_output_after) == 0) {
        message(sprintf("Port %d is now free.\n", port))
      } else {
        message(sprintf("Warning: Port %d is still in use by 'rserver'.\n", port))
      }
    } else {
      message(sprintf("No local 'rserver' found on port %d. Port is already free.\n", port))
    }
  }
  
  tryCatch({
    if (action == "start") {
      # Free up the host port if rserver is running
      free_port_if_rserver_running(host_port)
      
      # Check if the image is available locally, otherwise pull it
      image_check_cmd <- sprintf("%s images -q %s", cli, image)
      image_id <- system(image_check_cmd, intern = TRUE, ignore.stderr = TRUE)
      
      if (length(image_id) == 0 || nchar(image_id) == 0) {
        message(sprintf("Image '%s' not found locally. Pulling from registry...", image))
        pull_cmd <- sprintf("%s pull %s", cli, image)
        system(pull_cmd, intern = TRUE, ignore.stderr = TRUE)
        message("Image pulled successfully.")
      } else {
        message(sprintf("Image '%s' found locally.", image))
      }
      
      # Check if the container already exists
      list_cmd <- sprintf("%s ps -a --format '{{.Names}}'", cli)
      existing_containers <- system(list_cmd, intern = TRUE, ignore.stderr = TRUE)
      
      if (container_name %in% existing_containers) {
        message("Container already exists. Restarting...")
        restart_cmd <- sprintf("%s start %s", cli, container_name)
        system(restart_cmd)
      } else {
        message("Container not found. Creating a new one...")
        docker_cmd <- sprintf(
          "%s run -d -p %d:%d --name %s -e PASSWORD=%s -e IS_RSERVER_RUN=%s %s",
          cli, host_port, container_port, container_name, shQuote(password), shQuote(is_rserver_run), image
        )
        system(docker_cmd)
      }
      
      url <- sprintf("http://localhost:%d", host_port)
      message("\n=== RStudio Server is running ===")
      message(sprintf("Access it at: %s", url))
      message("=================================\n")
      try({
        browseURL(url)
      }, silent = TRUE)
      
    } else if (action == "stop") {
      stop_cmd <- sprintf("%s stop %s", cli, container_name)
      message("Stopping the container...")
      system(stop_cmd, ignore.stderr = TRUE)
      message(sprintf("Container '%s' has been stopped.", container_name))
    }
  },
  interrupt = function(e) {
    message("\nProcess interrupted by the user.")
  },
  error = function(e) {
    message("\nAn error occurred: ", e$message)
  })
}



