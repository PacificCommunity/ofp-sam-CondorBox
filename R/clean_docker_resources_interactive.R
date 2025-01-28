#' Clean Docker Resources Interactively
#'
#' This function lists all Docker containers and images interactively, allowing the user to select which to remove.
#'
#' @param cli The CLI to use ("docker" or "podman"). Default is "docker".
#' @export
clean_docker_resources_interactive <- function(cli = "docker") {
  # Internal function to run Docker commands, capture output, and handle errors
  run_docker_cmd <- function(cmd, args = NULL, show_cmd = TRUE) {
    full_cmd <- paste(cmd, if (!is.null(args)) paste(args, collapse = " ") else "")
    if (show_cmd) {
      cat("\n[DEBUG] Running command:\n", full_cmd, "\n\n")
    }
    result <- tryCatch(
      system2(command = cmd, args = args, stdout = TRUE, stderr = TRUE),
      error = function(e) e$message
    )
    if (length(result) > 0 && is.character(result)) {
      cat("[CLI Output]\n", paste(result, collapse = "\n"), "\n")
    }
    return(result)
  }
  
  # Detect operating system
  os_type <- .Platform$OS.type  # "windows" or "unix"
  
  # Check if CLI is actually podman (common alternative to Docker)
  cli <- if (tryCatch(
    grepl("podman", system2("docker", "--version", stdout = TRUE, stderr = TRUE)),
    error = function(e) FALSE
  )) {
    "podman"
  } else {
    "docker"
  }
  
  # Test Docker permissions
  test_result <- run_docker_cmd(cli, "ps", show_cmd = FALSE)
  if (any(grepl("permission denied", test_result, ignore.case = TRUE))) {
    if (os_type == "windows") {
      stop("Permission denied. Please run R or RStudio as Administrator and try again.")
    } else {
      cli <- "sudo docker"
      message("Using 'sudo docker' for elevated permissions.")
    }
  }
  
  message(sprintf("\nUsing CLI: %s", cli))
  
  tryCatch({
    # --------------------------------------------------
    # 1) List all containers
    # --------------------------------------------------
    message("\nListing all containers...")
    containers <- run_docker_cmd(cli, c("ps", "-a", "--format", "{{.ID}}\t{{.Names}}\t{{.Status}}"))
    
    if (length(containers) == 0 || all(nchar(containers) == 0)) {
      message("No containers found.")
    } else {
      cat("ID\tName\tStatus\n", paste(containers, collapse = "\n"), "\n")
      
      # Prompt user for container IDs to remove
      remove_containers <- readline(
        "Enter container IDs to remove (comma-separated), or press Enter to skip: "
      )
      remove_containers <- gsub("\\s+", "", remove_containers)  # Trim whitespace
      
      if (nchar(remove_containers) > 0) {
        containers_vec <- strsplit(remove_containers, ",")[[1]]
        
        # Stop and remove containers
        for (container_id in containers_vec) {
          # Try to stop the container gracefully
          run_docker_cmd(cli, c("stop", container_id))
          
          # Remove the container
          remove_container_cmd <- c("rm", "-f", container_id)
          result <- run_docker_cmd(cli, remove_container_cmd)
          if (any(grepl("permission denied", result, ignore.case = TRUE)) && os_type != "windows") {
            message("Permission denied. Retrying with 'sudo docker'.")
            run_docker_cmd("sudo docker", remove_container_cmd)
          }
        }
        message("Selected containers removed.")
      }
    }
    
    # --------------------------------------------------
    # 2) List all images
    # --------------------------------------------------
    message("\nListing all images...")
    images <- run_docker_cmd(cli, c("images", "--format", "{{.ID}}\t{{.Repository}}\t{{.Tag}}"))
    
    if (length(images) == 0 || all(nchar(images) == 0)) {
      message("No images found.")
    } else {
      cat("ID\tRepository\tTag\n", paste(images, collapse = "\n"), "\n")
      
      remove_images <- readline(
        "Enter image IDs to remove (comma-separated), or press Enter to skip: "
      )
      remove_images <- gsub("\\s+", "", remove_images)
      
      if (nchar(remove_images) > 0) {
        images_vec <- strsplit(remove_images, ",")[[1]]
        
        # Remove images
        for (image_id in images_vec) {
          remove_image_cmd <- c("rmi", "-f", image_id)
          result <- run_docker_cmd(cli, remove_image_cmd)
          if (any(grepl("permission denied", result, ignore.case = TRUE)) && os_type != "windows") {
            message("Permission denied. Retrying with 'sudo docker'.")
            run_docker_cmd("sudo docker", remove_image_cmd)
          }
        }
        message("Selected images removed.")
      }
    }
    
    message("\n=== Cleanup Complete ===\n")
    
  }, error = function(e) {
    message("\nAn error occurred: ", e$message)
  })
}



