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
#' @param password The password to set for RStudio Server. Default is "yourpassword".
#' @param ghcr_login Logical indicating whether to log in to GitHub Container Registry (ghcr.io). Default is FALSE.
#' @param github_username The GitHub username for ghcr.io login. Required if ghcr_login is TRUE.
#' @param github_token The GitHub personal access token for ghcr.io login. Required if ghcr_login is TRUE.
#' @param transfer_env Logical indicating whether to transfer environment variables from ~/.Renviron to the container. Default is FALSE.
#' @export

manage_rstudio_server <- function(
    action = c("start", "stop"),
    image = "rocker/rstudio",
    container_name = "rstudio",
    host_port = 9999,
    container_port = 8787,
    password = "yourpassword",
    ghcr_login = FALSE,
    github_username = NULL,
    github_token = NULL,
    transfer_env = FALSE
) {
  action <- match.arg(action)
  
  # Detect OS type
  os_type <- .Platform$OS.type
  is_windows <- (os_type == "windows")
  
  # Function to execute system commands safely
  exec_cmd <- function(cmd, args = NULL, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL) {
    tryCatch({
      output <- system2(command = cmd, args = args,
                        stdout = if (ignore.stdout) NULL else TRUE,
                        stderr = if (ignore.stderr) NULL else TRUE,
                        wait = wait,
                        input = input)
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
  
  # Function to check if Docker daemon is running
  docker_running <- function() {
    res <- exec_cmd("docker", args = "info", ignore.stdout = TRUE, ignore.stderr = TRUE)
    return(!is.null(res))
  }
  
  if (!docker_installed()) {
    stop("Docker is not installed. Please install Docker and restart your system.")
  }
  
  if (!docker_running()) {
    stop("Docker daemon is not running. Start Docker and try again.")
  }
  
  # Function to log in to GitHub Container Registry (ghcr.io)
  docker_login_ghcr <- function(username, token) {
    if (is.null(username) || is.null(token)) {
      stop("GitHub username and token must be provided for ghcr.io login.")
    }
    message("Logging in to ghcr.io securely...")
    
    # Use system2's input argument to pass the token
    login_output <- exec_cmd("docker", 
                             args = c("login", "ghcr.io", "-u", username, "--password-stdin"),
                             input = token)
    
    if (is.null(login_output)) {
      stop("Docker login failed.")
    }
    
    message("Docker login to ghcr.io successful.")
  }
  
  # Function to check if a container exists
  container_exists <- function(name) {
    existing <- exec_cmd("docker", 
                         args = c("ps", "-a", "--filter", sprintf("name=%s", name), "--format", "{{.Names}}"))
    return(!is.null(existing) && name %in% existing)
  }
  
  # Function to check if a container is running
  container_running <- function(name) {
    running <- exec_cmd("docker", 
                        args = c("ps", "--filter", sprintf("name=%s", name),
                                 "--filter", "status=running", "--format", "{{.Names}}"))
    return(!is.null(running) && name %in% running)
  }
  
  # Function to start a stopped container
  start_container <- function(name) {
    message(sprintf("Starting existing container '%s'...", name))
    exec_cmd("docker", args = c("start", name), ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  
<<<<<<< HEAD
  # Function to format Windows paths for Docker (if needed)
  format_docker_path <- function(path) {
    if (is_windows) {
      path <- gsub("\\\\", "/", path)  # Convert backslashes to forward slashes
      path <- sub("^([A-Za-z]):", "/\\L\\1", path, perl = TRUE)  # Convert "C:\..." to "/c/..."
    }
    return(path)
  }
  
  # Function to get environment variables from ~/.Renviron
  get_renviron_vars <- function() {
    env_file <- file.path(Sys.getenv("HOME"), ".Renviron")
    if (!file.exists(env_file)) {
      message(".Renviron file not found; no environment variables will be transferred.")
      return(character(0))
    }
    lines <- readLines(env_file)
    # Remove comment lines and blank lines
    lines <- lines[!grepl("^\\s*#", lines)]
    lines <- lines[nzchar(trimws(lines))]
    # Extract lines with "="
    lines <- lines[grepl("=", lines)]
    env_list <- lapply(lines, function(line) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      key <- trimws(parts[1])
      value <- trimws(parts[2])
      # Remove surrounding quotes if any
      value <- gsub('^["\']|["\']$', '', value)
      return(c(key, value))
    })
    env_list <- do.call(rbind, env_list)
    env_vars <- env_list[,2]
    names(env_vars) <- env_list[,1]
    env_args <- unlist(lapply(names(env_vars), function(var) {
      sprintf("-e %s=\"%s\"", var, env_vars[[var]])
    }))
    return(env_args)
  }
  
  # Function to run a new container (without mount options)
  run_container <- function(image, name, host_port, container_port, password) {
    if (ghcr_login) {
      if (!is.null(github_username) && !is.null(github_token)) {
        message("Logging in using GitHub credentials...")
        docker_login_ghcr(github_username, github_token)
      } else {
        message("Logging in to Docker Hub by default, as no GitHub login provided.")
      }
    } else {
      message("Logging in to Docker Hub (default).")
    }
    
    env_args <- c()
    if (transfer_env) {
      env_args <- get_renviron_vars()
    }
    
=======
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
>>>>>>> 86bd8a51d2c5a8ffcdd96106cf1bdc9df0a7023d
    run_cmd <- c(
      "run", "-d",
      "-p", sprintf("%d:%d", host_port, container_port),
      "--name", name,
      "-e", sprintf("PASSWORD=%s", password),
      env_args,
      image
    )
    
    message(sprintf("Executing: docker %s", paste(run_cmd, collapse = " ")))
    exec_cmd("docker", args = run_cmd)
  }
  
  # Function to open RStudio Server in a browser
  open_rstudio <- function(port) {
    url <- sprintf("http://localhost:%d", port)
    message(sprintf("Access RStudio Server at: %s", url))
    try({
      utils::browseURL(url)
    }, silent = TRUE)
  }
  
  if (action == "start") {
    if (container_exists(container_name)) {
      if (container_running(container_name)) {
        message(sprintf("Container '%s' is already running.", container_name))
        open_rstudio(host_port)
      } else {
        start_container(container_name)
        Sys.sleep(2)
        if (container_running(container_name)) {
          message("RStudio Server has been started.")
          open_rstudio(host_port)
        } else {
          message("Failed to start RStudio Server.")
          system(sprintf("docker logs %s", container_name), wait = TRUE)
        }
      }
    } else {
      run_container(image, container_name, host_port, container_port, password)
      Sys.sleep(3)
      if (container_running(container_name)) {
        message("RStudio Server is running.")
        open_rstudio(host_port)
      } else {
        message("Failed to start RStudio Server.")
        system(sprintf("docker logs %s", container_name), wait = TRUE)
      }
    }
  }
}






