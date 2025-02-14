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
#' @param local_dir The path to the local directory to mount into RStudio Server. Default is NULL.
#' @param mount_github_config Logical indicating whether to mount ~/.gitconfig and ~/.ssh into the container. Default is FALSE.
#' @export

manage_rstudio_server <- function(
    action = c("start", "stop", "remove"),
    image = "rocker/rstudio",
    container_name = "rstudio",
    container_port = 8787,
    password = "yourpassword",
    ghcr_login = FALSE,
    github_username = NULL,
    github_token = NULL,
    transfer_env = FALSE,
    local_dir = NULL,
    mount_github_config = FALSE  # if TRUE, mounts ~/.gitconfig and ~/.ssh into the container
) {
  action <- match.arg(action)
  
  # Detect if running on Windows vs Unix (Linux/macOS)
  os_type <- .Platform$OS.type
  is_windows <- (os_type == "windows")
  
  # We'll treat macOS the same as Linux, so no separate check needed.
  # If you need custom logic for macOS, you could do:
  # is_mac <- (Sys.info()[["sysname"]] == "Darwin")
  
  # Check for jsonlite, required for reading container env
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required but not installed. Please install it and try again.")
  }
  
  # Safely execute system commands
  exec_cmd <- function(cmd, args = NULL, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL) {
    tryCatch({
      output <- system2(
        command = cmd,
        args = args,
        stdout = if (ignore.stdout) NULL else TRUE,
        stderr = if (ignore.stderr) NULL else TRUE,
        wait = wait,
        input = input
      )
      return(output)
    }, error = function(e) {
      message(sprintf("Command execution error: %s", paste(c(cmd, args), collapse = " ")))
      message(e$message)
      return(NULL)
    })
  }
  
  # Check if Docker is installed and running
  docker_installed <- function() {
    res <- exec_cmd("docker", args = "--version", ignore.stdout = TRUE, ignore.stderr = TRUE)
    !is.null(res)
  }
  docker_running <- function() {
    res <- exec_cmd("docker", args = "info", ignore.stdout = TRUE, ignore.stderr = TRUE)
    !is.null(res)
  }
  if (!docker_installed()) {
    stop("Docker is not installed. Please install Docker and restart your system.")
  }
  if (!docker_running()) {
    stop("Docker daemon is not running. Start Docker and try again.")
  }
  
  # Log in to GitHub Container Registry (ghcr.io) if requested
  docker_login_ghcr <- function(username, token) {
    if (is.null(username) || is.null(token)) {
      stop("GitHub username and token must be provided for ghcr.io login.")
    }
    message("Logging in to ghcr.io securely...")
    login_output <- exec_cmd("docker", 
                             args = c("login", "ghcr.io", "-u", username, "--password-stdin"),
                             input = token)
    if (is.null(login_output)) {
      stop("Docker login failed.")
    }
    message("Docker login to ghcr.io successful.")
  }
  
  # Check if a container exists
  container_exists <- function(name) {
    existing <- exec_cmd("docker", 
                         args = c("ps", "-a", "--filter", sprintf("name=^%s$", name), "--format", "{{.Names}}"))
    !is.null(existing) && any(grepl(sprintf("^%s$", name), existing))
  }
  
  # Check if a container is running
  container_running <- function(name) {
    running <- exec_cmd("docker", 
                        args = c("ps", "--filter", sprintf("name=^%s$", name),
                                 "--filter", "status=running", "--format", "{{.Names}}"))
    !is.null(running) && any(grepl(sprintf("^%s$", name), running))
  }
  
  # Stop a container
  stop_container <- function(name) {
    message(sprintf("Stopping container '%s'...", name))
    exec_cmd("docker", args = c("stop", name), ignore.stderr = TRUE, ignore.stdout = TRUE)
  }
  
  # Remove a container (stop first if running)
  remove_container <- function(name) {
    if (container_running(name)) {
      message(sprintf("Container '%s' is running. Stopping it before removal.", name))
      exec_cmd("docker", args = c("stop", name), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    message(sprintf("Removing container '%s'...", name))
    exec_cmd("docker", args = c("rm", name), ignore.stdout = TRUE, ignore.stderr = TRUE)
  }
  
  # Format Windows paths for Docker
  format_docker_path <- function(path) {
    if (is_windows) {
      # Convert backslashes to forward slashes
      path <- gsub("\\\\", "/", path)
      # Convert "C:/..." to "/c/..."
      path <- sub("^([A-Za-z]):", "/\\L\\1", path, perl = TRUE)
    }
    path
  }
  
  # Read environment variables from ~/.Renviron (optional)
  get_renviron_vars <- function() {
    env_file <- file.path(Sys.getenv("HOME"), ".Renviron")
    if (!file.exists(env_file)) {
      message(".Renviron file not found; no environment variables will be transferred.")
      return(character(0))
    }
    lines <- readLines(env_file)
    # Remove comment lines, blank lines
    lines <- lines[!grepl("^\\s*#", lines)]
    lines <- lines[nzchar(trimws(lines))]
    # Only lines containing "="
    lines <- lines[grepl("=", lines)]
    env_list <- lapply(lines, function(line) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      key <- trimws(parts[1])
      value <- trimws(parts[2])
      # Strip surrounding quotes if present
      value <- gsub('^["\']|["\']$', '', value)
      c(key, value)
    })
    env_list <- do.call(rbind, env_list)
    env_vars <- env_list[,2]
    names(env_vars) <- env_list[,1]
    env_args <- unlist(lapply(names(env_vars), function(var) {
      sprintf("-e %s=\"%s\"", var, env_vars[[var]])
    }))
    env_args
  }
  
  # Read container environment variables from docker inspect
  get_container_env <- function(name) {
    out <- exec_cmd("docker", args = c("inspect", "--format=\"{{json .Config.Env}}\"", name))
    if (is.null(out) || length(out) == 0) return(NULL)
    out <- gsub('^"|"$', '', out)  # remove surrounding quotes
    env_vars <- jsonlite::fromJSON(out)
    env_list <- setNames(sapply(env_vars, function(x) {
      parts <- strsplit(x, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) parts[2] else ""
    }), sapply(env_vars, function(x) strsplit(x, "=", fixed = TRUE)[[1]][1]))
    env_list
  }
  
  # Run a new container with optional volume mounts
  run_container <- function(image, name, host_port, container_port, password, local_dir = NULL) {
    # Log in to GHCR if needed
    if (ghcr_login) {
      if (!is.null(github_username) && !is.null(github_token)) {
        message("Logging in using GitHub credentials...")
        docker_login_ghcr(github_username, github_token)
      } else {
        message("Logging in to Docker Hub by default (no GitHub login provided).")
      }
    } else {
      message("Logging in to Docker Hub (default).")
    }
    
    # Collect environment args if transferring .Renviron
    env_args <- if (transfer_env) get_renviron_vars() else character(0)
    
    volume_args <- character(0)
    
    # Mount local_dir if provided
    if (!is.null(local_dir)) {
      local_dir <- format_docker_path(local_dir)
      volume_args <- c(volume_args, "-v", sprintf("%s:/home/rstudio/%s", local_dir, basename(local_dir)))
    }
    
    # If mount_github_config is TRUE, mount ~/.gitconfig and ~/.ssh
    if (mount_github_config) {
      host_gitconfig <- path.expand("~/.gitconfig")
      if (file.exists(host_gitconfig)) {
        volume_args <- c(volume_args, "-v", sprintf("%s:/home/rstudio/.gitconfig:ro", host_gitconfig))
      }
      host_ssh <- path.expand("~/.ssh")
      if (dir.exists(host_ssh)) {
        volume_args <- c(volume_args, "-v", sprintf("%s:/home/rstudio/.ssh:ro", host_ssh))
      }
    }
    
    run_cmd <- c(
      "run", "-d",
      "-p", sprintf("%d:%d", host_port, container_port),
      "--name", name,
      "-e", sprintf("PASSWORD=%s", password),
      env_args,
      volume_args,
      image
    )
    
    message(sprintf("Executing: docker %s", paste(run_cmd, collapse = " ")))
    exec_cmd("docker", args = run_cmd)
  }
  
  # Open the RStudio Server in a browser
  open_rstudio <- function(port) {
    url <- sprintf("http://localhost:%d", port)
    message(sprintf("Access RStudio Server at: %s", url))
    try({
      utils::browseURL(url)
    }, silent = TRUE)
  }
  
  # Main logic for start/stop/remove
  if (action == "start") {
    if (container_exists(container_name)) {
      # If container exists, see if the password matches
      env_vars <- get_container_env(container_name)
      if (!is.null(env_vars) && !is.null(env_vars["PASSWORD"]) && env_vars["PASSWORD"] == password) {
        message(sprintf("Container '%s' already exists with the same password.", container_name))
        if (!container_running(container_name)) {
          message(sprintf("Starting container '%s'...", container_name))
          exec_cmd("docker", args = c("start", container_name))
          Sys.sleep(3)
        }
        # Get the host port
        host_port_info <- system(sprintf("docker port %s %d", container_name, container_port), intern = TRUE)
        if (length(host_port_info) > 0 && grepl(":", host_port_info[1])) {
          host_port <- as.numeric(sub(".*:(\\d+)$", "\\1", host_port_info[1]))
        } else {
          host_port <- NA
        }
        if (is.na(host_port)) {
          message("Could not retrieve host port from the existing container.")
        } else {
          message(sprintf("Connecting to existing container '%s' at host port %d.", container_name, host_port))
          open_rstudio(host_port)
        }
      } else {
        # Container with a different password -> create new one
        message(sprintf("Container '%s' already exists but with a different password. Launching a new container.", container_name))
        unique_container_name <- sprintf("%s_%s", container_name, format(Sys.time(), "%Y%m%d%H%M%S"))
        host_port <- sample(10000:60000, 1)
        message(sprintf("Starting new container '%s' on host port %d...", unique_container_name, host_port))
        run_container(image, unique_container_name, host_port, container_port, password, local_dir)
        Sys.sleep(3)
        if (container_running(unique_container_name)) {
          message(sprintf("RStudio Server is running at: http://localhost:%d", host_port))
          open_rstudio(host_port)
        } else {
          message("Failed to start RStudio Server.")
          system(sprintf("docker logs %s", unique_container_name), wait = TRUE)
        }
      }
    } else {
      # Container doesn't exist -> create a new one
      host_port <- sample(10000:60000, 1)
      message(sprintf("Starting new container '%s' on host port %d...", container_name, host_port))
      run_container(image, container_name, host_port, container_port, password, local_dir)
      Sys.sleep(3)
      if (container_running(container_name)) {
        message(sprintf("RStudio Server is running at: http://localhost:%d", host_port))
        open_rstudio(host_port)
      } else {
        message("Failed to start RStudio Server.")
        system(sprintf("docker logs %s", container_name), wait = TRUE)
      }
    }
  } else if (action == "stop") {
    # Stop the container
    if (!container_exists(container_name)) {
      message(sprintf("Container '%s' does not exist.", container_name))
    } else if (container_running(container_name)) {
      stop_container(container_name)
      message(sprintf("Container '%s' has been stopped.", container_name))
    } else {
      message(sprintf("Container '%s' is not running.", container_name))
    }
  } else if (action == "remove") {
    # Remove the container
    if (!container_exists(container_name)) {
      message(sprintf("Container '%s' does not exist.", container_name))
    } else {
      remove_container(container_name)
      message(sprintf("Container '%s' has been removed.", container_name))
    }
  }
}



