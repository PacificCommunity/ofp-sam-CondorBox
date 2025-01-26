#' Manage GitHub SSH Keys Interactively
#'
#' This function allows users to interactively view and delete SSH keys from their GitHub account.
#' @param github_token A GitHub Personal Access Token with `read:public_key` and `write:public_key` scopes.
#' @return None
#' @export
#' @examples
#' \dontrun{
#' clean_github_ssh_keys_interactive("your_github_token")
#' }
clean_github_ssh_keys_interactive <- function(github_token) {
  if (is.null(github_token)) {
    stop("GitHub token is required to manage SSH keys.")
  }
  
  # Load required library
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required. Please install it using install.packages('httr').")
  }
  
  # API endpoint
  api_url <- "https://api.github.com/user/keys"
  
  # Get the list of SSH keys
  response <- httr::GET(api_url, httr::add_headers(Authorization = paste("token", github_token)))
  
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve SSH keys. Ensure your GitHub token has the 'read:public_key' scope.")
  }
  
  keys <- httr::content(response, as = "parsed", type = "application/json")
  
  if (length(keys) == 0) {
    message("No SSH keys found on your GitHub account.")
    return(invisible(NULL))
  }
  
  # Display keys interactively
  message("\nSSH keys associated with your GitHub account:\n")
  for (i in seq_along(keys)) {
    cat(sprintf("[%d] Title: %s\n    Created At: %s\n    ID: %s\n",
                i, keys[[i]]$title, keys[[i]]$created_at, keys[[i]]$id))
  }
  
  # Prompt user to enter IDs or indices to delete
  key_input <- readline("\nEnter the numbers or IDs of the keys to delete (comma-separated), or press Enter to skip: ")
  if (nchar(key_input) == 0) {
    message("No keys selected for deletion.")
    return(invisible(NULL))
  }
  
  # Parse input
  inputs <- unlist(strsplit(key_input, ","))
  ids_to_delete <- character(0)
  
  # Process input as indices or IDs
  for (input in inputs) {
    if (grepl("^[0-9]+$", input)) {
      # Treat as index
      index <- as.integer(input)
      if (!is.na(index) && index >= 1 && index <= length(keys)) {
        ids_to_delete <- c(ids_to_delete, keys[[index]]$id)
      } else {
        message(sprintf("Invalid index: %s. Skipping.", input))
      }
    } else {
      # Treat as ID
      ids_to_delete <- c(ids_to_delete, input)
    }
  }
  
  # Validate IDs
  if (length(ids_to_delete) == 0) {
    message("No valid keys selected for deletion.")
    return(invisible(NULL))
  }
  
  # Delete selected keys
  for (key_id in ids_to_delete) {
    delete_response <- httr::DELETE(
      paste0(api_url, "/", key_id),
      httr::add_headers(Authorization = paste("token", github_token))
    )
    
    if (httr::status_code(delete_response) == 204) {
      message(sprintf("Key with ID '%s' successfully deleted.", key_id))
    } else {
      warning(sprintf("Failed to delete key with ID '%s'.", key_id))
      print(httr::content(delete_response, as = "parsed"))
    }
  }
  
  message("\n=== SSH Key Management Complete ===")
}
