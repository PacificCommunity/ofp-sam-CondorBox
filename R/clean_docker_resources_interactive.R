#' Clean Docker Resources Interactively
#'
#' This function lists all Docker containers and images interactively, allowing the user to select which to remove.
#'
#' @param cli The CLI to use ("docker" or "podman"). Default is "docker".
#' @export
clean_docker_resources_interactive <- function(cli = "docker") {
  # Internal function to run Docker commands, capture output, and show errors
  run_docker_cmd <- function(cmd, show_cmd = TRUE) {
    if (.Platform$OS.type == "windows") {
      cmd <- paste("cmd.exe /c", cmd) # Use cmd.exe for Windows
    }
    if (show_cmd) {
      cat("\n[DEBUG] Running command:\n", cmd, "\n\n")
    }
    result <- tryCatch(
      system(cmd, intern = TRUE, ignore.stderr = FALSE), 
      error = function(e) e$message
    )
    if (length(result) > 0 && is.character(result)) {
      cat("[CLI Output]\n", paste(result, collapse = "\n"), "\n")
    }
    return(result)
  }
  
  # Check if we're actually using "podman" based on docker --version
  cli <- if (tryCatch(
    grepl("podman", system("docker --version", intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) FALSE
  )) {
    "podman"
  } else {
    "docker"
  }
  
  # Test Docker permissions quickly by running 'ps' 
  test_result <- run_docker_cmd(sprintf("%s ps", cli), show_cmd = FALSE)
  if (any(grepl("permission denied", test_result, ignore.case = TRUE))) {
    message("It looks like you might need sudo to run Docker.")
    use_sudo <- readline("Retry with 'sudo docker'? (y/n): ")
    if (tolower(use_sudo) == "y") {
      cli <- "sudo docker"
      message("Switched to sudo docker.")
    }
  }
  
  message(sprintf("\nUsing CLI: %s", cli))
  
  tryCatch({
    # --------------------------------------------------
    # 1) List all containers
    # --------------------------------------------------
    message("\nListing all containers...")
    list_containers_cmd <- sprintf("%s ps -a --format '{{.ID}}\t{{.Names}}\t{{.Status}}'", cli)
    containers <- tryCatch(
      system(list_containers_cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0)
    )
    
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
        
        # For each container, attempt to stop (or kill) before removing
        for (container_id in containers_vec) {
          # First, try a graceful stop
          stop_cmd <- sprintf("%s stop %s", cli, container_id)
          stop_result <- run_docker_cmd(stop_cmd)
          
          # If 'stop' doesn't work or if container is still "Up", we try kill
          ps_output <- run_docker_cmd(sprintf("%s ps -a --format '{{.Status}}' --filter 'id=%s'", 
                                              cli, container_id), 
                                      show_cmd = FALSE)
          if (length(ps_output) > 0 && any(grepl("^Up", ps_output))) {
            # Container is still running, so let's kill
            kill_cmd <- sprintf("%s kill %s", cli, container_id)
            run_docker_cmd(kill_cmd)
          }
          
          # Finally, remove the container with -f
          remove_container_cmd <- sprintf("%s rm -f %s", cli, container_id)
          result <- run_docker_cmd(remove_container_cmd)
          
          # Check if permission denied. If so, prompt for sudo unless we're already in sudo
          if (any(grepl("permission denied", result, ignore.case = TRUE)) && 
              !startsWith(cli, "sudo")) {
            message("Permission denied. You may need sudo to remove containers.")
            use_sudo <- readline("Retry with 'sudo docker'? (y/n): ")
            if (tolower(use_sudo) == "y") {
              cli <- "sudo docker"
              stop_cmd <- sprintf("%s stop %s", cli, container_id)
              kill_cmd <- sprintf("%s kill %s", cli, container_id)
              remove_container_cmd <- sprintf("%s rm -f %s", cli, container_id)
              
              run_docker_cmd(stop_cmd)
              run_docker_cmd(kill_cmd)
              run_docker_cmd(remove_container_cmd)
            }
          }
        }
        message("Selected containers removed (if they existed).")
      }
    }
    
    # --------------------------------------------------
    # 2) List all images
    # --------------------------------------------------
    message("\nListing all images...")
    list_images_cmd <- sprintf("%s images --format '{{.ID}}\t{{.Repository}}\t{{.Tag}}'", cli)
    images <- tryCatch(
      system(list_images_cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0)
    )
    
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
          remove_images_cmd <- sprintf("%s rmi -f %s", cli, image_id)
          result <- run_docker_cmd(remove_images_cmd)
          
          # Check permission denial again
          if (any(grepl("permission denied", result, ignore.case = TRUE)) && 
              !startsWith(cli, "sudo")) {
            message("Permission denied. You may need sudo to remove images.")
            use_sudo <- readline("Retry with 'sudo docker'? (y/n): ")
            if (tolower(use_sudo) == "y") {
              cli <- "sudo docker"
              remove_images_cmd <- sprintf("%s rmi -f %s", cli, image_id)
              run_docker_cmd(remove_images_cmd)
            }
          }
        }
        message("Selected images removed (if they existed).")
      }
    }
    
    message("\n=== Cleanup Complete ===\n")
    
  }, error = function(e) {
    message("\nAn error occurred: ", e$message)
  })
}




