#' Setup SSH Key for Remote Server
#'
#' This function generates an SSH key if it does not exist and copies the public key
#' to the specified remote server.
#'
#' @param remote_user The username for the remote server.
#' @param remote_host The hostname or IP address of the remote server.
#' @return None. Prints messages about the status of the setup.
#' @examples
#' \dontrun{
#' setup_ssh_key(remote_user = "your_username", remote_host = "your_remote_host")
#' }
#' @export
setup_ssh_key <- function(remote_user, remote_host) {
  ssh_key_path <- normalizePath("~/.ssh/id_rsa", mustWork = FALSE)
  
  # Ensure the .ssh directory exists
  ssh_dir <- dirname(ssh_key_path)
  if (!dir.exists(ssh_dir)) {
    message("Creating ~/.ssh directory...")
    dir.create(ssh_dir, recursive = TRUE, mode = "0700")
  }
  
  # Check if SSH key exists
  if (!file.exists(ssh_key_path)) {
    message("Generating SSH key...")
    keygen_command <- sprintf('ssh-keygen -q -t rsa -b 4096 -C "Generated SSH Key" -f "%s" -N ""', ssh_key_path)
    keygen_status <- system(keygen_command, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    if (keygen_status != 0 || !file.exists(paste0(ssh_key_path, ".pub"))) {
      stop("SSH key generation failed. Ensure the directory is writable and the command works.")
    }
    message("SSH key generated at: ", ssh_key_path)
  } else {
    message("SSH key already exists at: ", ssh_key_path)
  }
  
  # Add SSH key to remote server
  if (!is.null(remote_user) && !is.null(remote_host)) {
    message("Copying SSH key to remote server...")
    copy_command <- sprintf("ssh-copy-id %s@%s", remote_user, remote_host)
    system(copy_command, wait = TRUE)
    message("SSH key copied to remote server successfully.")
  } else {
    message("No remote server specified. Skipping SSH key copying.")
  }
}