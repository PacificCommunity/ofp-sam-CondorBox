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
    container_port = 8787
) {
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
  
  tryCatch({
    if (action == "start") {
      list_cmd <- sprintf("%s ps -a --format '{{.Names}}'", cli)
      existing_containers <- system(list_cmd, intern = TRUE, ignore.stderr = TRUE)
      
      if (container_name %in% existing_containers) {
        message("Container already exists. Restarting...")
        restart_cmd <- sprintf("%s start %s", cli, container_name)
        system(restart_cmd)
      } else {
        message("Container not found. Creating a new one...")
        docker_cmd <- sprintf(
          "%s run -d -p %d:%d --name %s -e RSTUDIO_SERVER=1 -e RSTUDIO_PORT=%d %s",
          cli, host_port, container_port, container_name, container_port, image
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
  }, interrupt = function(e) {
    message("\nProcess interrupted by the user.")
  }, error = function(e) {
    message("\nAn error occurred: ", e$message)
  })
}

