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
    host_port = 9999,                  # Set desired host port
    container_port = 8787,
    password = "yourpassword"          # Set your desired password
) {
  action <- match.arg(action)
  
  # Determine the operating system
  os_type <- .Platform$OS.type
  
  # Function to execute system commands in a cross-platform way
  exec_cmd <- function(cmd, args = NULL, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE) {
    tryCatch({
      output <- system2(command = cmd, args = args, stdout = if (ignore.stdout) NULL else TRUE, 
                        stderr = if (ignore.stderr) NULL else TRUE, wait = wait)
      return(output)
    }, error = function(e) {
      message(sprintf("Command execution error: %s", paste(c(cmd, args), collapse = " ")))
      message(e$message)
      return(NULL)
    })
  }
  
  # Function to check if Docker is installed
  docker_installed <- function() {
    res <- exec_cmd("docker", args = "--version", ignore.stdout = TRUE, ignore.stderr = TRUE)
    return(!is.null(res))
  }
  
  if (!docker_installed()) {
    stop("Docker is not installed or not available in PATH. Please install Docker and ensure it's running.")
  }
  
  # Function to check if a container exists
  container_exists <- function(name) {
    # Adjust the filter based on the operating system
    if (os_type == "windows") {
      filter_pattern <- sprintf("^%s$", name)
    } else {
      filter_pattern <- sprintf("^/%s$", name)
    }
    existing <- exec_cmd("docker", args = c("ps", "-a", "--filter", sprintf("name=%s", filter_pattern), "--format", "{{.Names}}"))
    return(!is.null(existing) && name %in% existing)
  }
  
  # Function to check if a container is running
  container_running <- function(name) {
    # Adjust the filter based on the operating system
    if (os_type == "windows") {
      filter_pattern <- sprintf("^%s$", name)
    } else {
      filter_pattern <- sprintf("^/%s$", name)
    }
    running <- exec_cmd("docker", args = c("ps", "--filter", sprintf("name=%s", filter_pattern), "--filter", "status=running", "--format", "{{.Names}}"))
    return(!is.null(running) && name %in% running)
  }
  
  # Function to start a stopped container
  start_container <- function(name) {
    message(sprintf("Starting existing container '%s'...", name))
    exec_cmd("docker", args = c("start", name), ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  
  # Function to check if an image exists locally
  image_exists <- function(image) {
    result <- exec_cmd("docker", args = c("images", "--format", "{{.Repository}}:{{.Tag}}"))
    return(!is.null(result) && any(grepl(image, result)))
  }
  
  run_container <- function(image, name, host_port, container_port, password) {
    # Check if the image exists, if not, pull it
    if (!image_exists(image)) {
      message(sprintf("Image '%s' not found locally. Pulling from registry...", image))
      exec_cmd("docker", args = c("pull", image))
    }
    
    # Run the container
    run_cmd <- c(
      "run", "-d",
      "-p", sprintf("%d:%d", host_port, container_port),
      "--name", name,
      "-e", sprintf("PASSWORD=%s", password),
      image
    )
    message(sprintf("Executing: docker %s", paste(run_cmd, collapse = " ")))
    exec_cmd("docker", args = run_cmd)
  }
  
  # Function to stop a running container
  stop_container <- function(name) {
    message(sprintf("Stopping container '%s'...", name))
    exec_cmd("docker", args = c("stop", name), ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  
  # Function to remove a container
  remove_container <- function(name) {
    message(sprintf("Removing container '%s'...", name))
    exec_cmd("docker", args = c("rm", name), ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  
  # Function to open URL in the default browser
  open_url <- function(url) {
    try({
      utils::browseURL(url)
    }, silent = TRUE)
  }
  
  if (action == "start") {
    if (container_exists(container_name)) {
      if (container_running(container_name)) {
        message(sprintf("Container '%s' is already running.", container_name))
        # Open URL in browser since the container is running
        url <- sprintf("http://localhost:%d", host_port)
        message(sprintf("Access it at: %s", url))
        open_url(url)
      } else {
        start_container(container_name)
        # Optionally, verify if the container started successfully
        Sys.sleep(2)  # Wait a moment for the container to start
        if (container_running(container_name)) {
          message("RStudio Server has been started.")
          url <- sprintf("http://localhost:%d", host_port)
          message(sprintf("Access it at: %s", url))
          open_url(url)
        } else {
          message("Failed to start RStudio Server.")
          # Optionally, display container logs for debugging
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
        open_url(url)
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


