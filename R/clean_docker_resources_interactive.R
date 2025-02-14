#' Clean Docker Resources Interactively
#'
#' This function lists all Docker containers and images interactively, allowing the user to select which to remove.
#'
#' @param cli The CLI to use ("docker" or "podman"). Default is "docker".
#' @export
clean_docker_resources_interactive <- function(cli = "docker") {
  
  # Helper function to run a system command and return its output.
  run_docker_cmd <- function(cmd) {
    if (.Platform$OS.type == "windows") {
      cmd <- paste("cmd.exe /c", cmd)
    }
    # Run command without printing debug messages
    result <- tryCatch(
      system(cmd, intern = TRUE, ignore.stderr = TRUE), 
      error = function(e) e$message
    )
    return(result)
  }
  
  # Determine whether to use "docker" or "podman" based on the output of --version
  cli <- if (tryCatch(
    grepl("podman", system("docker --version", intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) FALSE
  )) {
    "podman"
  } else {
    "docker"
  }
  
  message(sprintf("\nUsing CLI: %s", cli))
  
  tryCatch({
    # --------------------------------------------------
    # 1) List all containers
    # --------------------------------------------------
    message("\nListing all containers...")
    list_containers_cmd <- sprintf('%s ps -a --format "{{.ID}}\t{{.Names}}\t{{.Status}}"', cli)
    containers <- tryCatch(
      system(list_containers_cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0)
    )
    
    if (length(containers) == 0 || all(nchar(containers) == 0)) {
      message("No containers found.")
    } else {
      cat("ID\tName\tStatus\n", paste(containers, collapse = "\n"), "\n")
      
      remove_containers <- readline("Enter container IDs to remove (comma-separated), or press Enter to skip: ")
      remove_containers <- gsub("\\s+", "", remove_containers)
      
      if (nchar(remove_containers) > 0) {
        containers_vec <- strsplit(remove_containers, ",")[[1]]
        
        for (container_id in containers_vec) {
          # Check if container exists by ID
          check_exists_cmd <- sprintf("%s ps -a -q -f id=%s", cli, container_id)
          exists <- run_docker_cmd(check_exists_cmd)
          
          if (length(exists) == 0 || nchar(exists) == 0) {
            message(sprintf("Container '%s' does not exist. Skipping...", container_id))
            next
          }
          
          # Check if container is running
          check_running_cmd <- sprintf("%s ps -q -f id=%s", cli, container_id)
          running <- run_docker_cmd(check_running_cmd)
          
          if (length(running) > 0 && nchar(running) > 0) {
            # Try to stop the container first
            stop_cmd <- sprintf("%s stop %s", cli, container_id)
            run_docker_cmd(stop_cmd)
            Sys.sleep(1)
            # Re-check if still running
            running <- run_docker_cmd(check_running_cmd)
            if (length(running) > 0 && nchar(running) > 0) {
              # If still running, kill it
              kill_cmd <- sprintf("%s kill %s", cli, container_id)
              run_docker_cmd(kill_cmd)
            }
          } else {
            message(sprintf("Container '%s' is already stopped.", container_id))
          }
          
          # Remove the container
          remove_container_cmd <- sprintf("%s rm -f %s", cli, container_id)
          run_docker_cmd(remove_container_cmd)
          message(sprintf("Container '%s' removed.", container_id))
        }
      }
    }
    
    # --------------------------------------------------
    # 2) List all images
    # --------------------------------------------------
    message("\nListing all images...")
    list_images_cmd <- sprintf('%s images --format "{{.ID}}\t{{.Repository}}\t{{.Tag}}"', cli)
    images <- tryCatch(
      system(list_images_cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0)
    )
    
    if (length(images) == 0 || all(nchar(images) == 0)) {
      message("No images found.")
    } else {
      cat("ID\tRepository\tTag\n", paste(images, collapse = "\n"), "\n")
      
      remove_images <- readline("Enter image IDs to remove (comma-separated), or press Enter to skip: ")
      remove_images <- gsub("\\s+", "", remove_images)
      
      if (nchar(remove_images) > 0) {
        images_vec <- strsplit(remove_images, ",")[[1]]
        
        for (image_id in images_vec) {
          remove_image_cmd <- sprintf("%s rmi -f %s", cli, image_id)
          run_docker_cmd(remove_image_cmd)
          message(sprintf("Image '%s' removed.", image_id))
        }
      }
    }
    
    message("\n=== Cleanup Complete ===\n")
    
  }, error = function(e) {
    message("\nAn error occurred: ", e$message)
  })
}





