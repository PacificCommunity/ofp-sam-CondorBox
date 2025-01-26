#' Clean Docker Resources Interactively
#'
#' This function lists all Docker containers and images interactively, allowing the user to select which to remove.
#'
#' @param cli The CLI to use ("docker" or "podman"). Default is "docker".
#' @export
clean_docker_resources_interactive <- function(cli = "docker") {
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
    # List all containers
    message("\nListing all containers:")
    list_containers_cmd <- sprintf("%s ps -a --format '{{.ID}}\t{{.Names}}\t{{.Status}}'", cli)
    containers <- tryCatch(
      system(list_containers_cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0)
    )
    if (length(containers) == 0) {
      message("No containers found.")
    } else {
      cat("ID\tName\tStatus\n", paste(containers, collapse = "\n"), "\n")
      
      # Prompt user to select containers to remove
      remove_containers <- readline("Enter container IDs to remove (comma-separated, or press Enter to skip): ")
      if (nchar(remove_containers) > 0) {
        remove_containers_cmd <- sprintf("%s rm -f %s", cli, paste(strsplit(remove_containers, ",")[[1]], collapse = " "))
        system(remove_containers_cmd, ignore.stderr = TRUE)
        message("Selected containers removed.")
      }
    }
    
    # List all images
    message("\nListing all images:")
    list_images_cmd <- sprintf("%s images --format '{{.ID}}\t{{.Repository}}\t{{.Tag}}'", cli)
    images <- tryCatch(
      system(list_images_cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0)
    )
    if (length(images) == 0) {
      message("No images found.")
    } else {
      cat("ID\tRepository\tTag\n", paste(images, collapse = "\n"), "\n")
      
      # Prompt user to select images to remove
      remove_images <- readline("Enter image IDs to remove (comma-separated, or press Enter to skip): ")
      if (nchar(remove_images) > 0) {
        remove_images_cmd <- sprintf("%s rmi -f %s", cli, paste(strsplit(remove_images, ",")[[1]], collapse = " "))
        system(remove_images_cmd, ignore.stderr = TRUE)
        message("Selected images removed.")
      }
    }
    
    message("\n=== Cleanup Complete ===")
  }, error = function(e) {
    message("\nAn error occurred: ", e$message)
  })
}

