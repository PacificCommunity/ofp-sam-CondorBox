#' Clean Docker Resources Interactively
#'
#' This function lists all Docker containers and images interactively, allowing the user to select which to remove.
#'
#' @param cli The CLI to use ("docker" or "podman"). Default is "docker".
#' @export
clean_docker_resources_interactive <- function(cli = "docker") {
  # Detect if 'docker' is actually 'podman'
  cli <- if (tryCatch(
    grepl("podman", system("docker --version", intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) FALSE
  )) {
    "podman"
  } else {
    "docker"
  }
  
  message(sprintf("Using CLI: %s", cli))
  
  tryCatch({
    # --------------------------------------------------------
    # 1) List all containers
    # --------------------------------------------------------
    message("\nListing all containers:")
    list_containers_cmd <- sprintf("%s ps -a --format '{{.ID}}\t{{.Names}}\t{{.Status}}'", cli)
    containers <- tryCatch(
      system(list_containers_cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0)
    )
    
    if (length(containers) == 0 || all(nchar(containers) == 0)) {
      message("No containers found.")
    } else {
      cat("ID\tName\tStatus\n", paste(containers, collapse = "\n"), "\n")
      
      # Prompt user for container IDs
      remove_containers <- readline("Enter container IDs to remove (comma-separated, or press Enter to skip): ")
      # Trim whitespace
      remove_containers <- gsub("\\s+", "", remove_containers)
      
      if (nchar(remove_containers) > 0) {
        # Split by comma
        containers_vec <- strsplit(remove_containers, ",")[[1]]
        remove_containers_cmd <- sprintf("%s rm -f %s", cli, paste(containers_vec, collapse = " "))
        
        # Optional debugging output
        cat("\n[DEBUG] Running command:\n", remove_containers_cmd, "\n\n")
        
        # Capture CLI output (and errors) for visibility
        cmd_output <- system(remove_containers_cmd, intern = TRUE, ignore.stderr = FALSE)
        if (length(cmd_output) > 0) {
          cat("[CLI Output]\n", paste(cmd_output, collapse = "\n"), "\n")
        }
        
        message("Selected containers removed (if they existed).")
      }
    }
    
    # --------------------------------------------------------
    # 2) List all images
    # --------------------------------------------------------
    message("\nListing all images:")
    list_images_cmd <- sprintf("%s images --format '{{.ID}}\t{{.Repository}}\t{{.Tag}}'", cli)
    images <- tryCatch(
      system(list_images_cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0)
    )
    
    if (length(images) == 0 || all(nchar(images) == 0)) {
      message("No images found.")
    } else {
      cat("ID\tRepository\tTag\n", paste(images, collapse = "\n"), "\n")
      
      # Prompt user for image IDs
      remove_images <- readline("Enter image IDs to remove (comma-separated, or press Enter to skip): ")
      # Trim whitespace
      remove_images <- gsub("\\s+", "", remove_images)
      
      if (nchar(remove_images) > 0) {
        # Split by comma
        images_vec <- strsplit(remove_images, ",")[[1]]
        remove_images_cmd <- sprintf("%s rmi -f %s", cli, paste(images_vec, collapse = " "))
        
        # Optional debugging output
        cat("\n[DEBUG] Running command:\n", remove_images_cmd, "\n\n")
        
        # Capture CLI output (and errors) for visibility
        cmd_output <- system(remove_images_cmd, intern = TRUE, ignore.stderr = FALSE)
        if (length(cmd_output) > 0) {
          cat("[CLI Output]\n", paste(cmd_output, collapse = "\n"), "\n")
        }
        
        message("Selected images removed (if they existed).")
      }
    }
    
    message("\n=== Cleanup Complete ===\n")
    
  }, error = function(e) {
    message("\nAn error occurred: ", e$message)
  })
}


