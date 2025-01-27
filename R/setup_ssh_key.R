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
  # Load the rstudioapi package
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("The 'rstudioapi' package is required for a hidden password prompt.")
  }
  
  # Define the path to the SSH private key
  ssh_key_path <- normalizePath("~/.ssh/id_rsa", mustWork = FALSE)
  
  # Check if the .ssh directory exists; if not, create it with appropriate permissions
  ssh_dir <- dirname(ssh_key_path)
  if (!dir.exists(ssh_dir)) {
    message("Creating ~/.ssh directory...")
    dir.create(ssh_dir, recursive = TRUE, mode = "0700")
  }
  
  # Check if the SSH key already exists; if not, generate a new one
  if (!file.exists(ssh_key_path)) {
    message("Generating SSH key...")
    keygen_command <- sprintf(
      'ssh-keygen -q -t rsa -b 4096 -C "Generated SSH Key" -f "%s" -N ""',
      ssh_key_path
    )
    keygen_status <- system(keygen_command, ignore.stdout = FALSE, ignore.stderr = FALSE)
    
    if (keygen_status != 0 || !file.exists(paste0(ssh_key_path, ".pub"))) {
      stop("SSH key generation failed. Ensure the directory is writable and the command works.")
    }
    message("SSH key generated at: ", ssh_key_path)
  } else {
    message("SSH key already exists at: ", ssh_key_path)
  }
  
  # Add the SSH key to the remote server
  if (!is.null(remote_user) && !is.null(remote_host)) {
    # Check if sshpass is installed
    sshpass_installed <- nzchar(Sys.which("sshpass"))
    
    if (!sshpass_installed) {
      message("`sshpass` is not installed on your system.")
      message("Please install `sshpass` to enable automatic SSH key copying.")
      message("Alternatively, you can manually copy the SSH key using the following command:")
      message(sprintf("  ssh-copy-id -i \"%s.pub\" %s@%s", ssh_key_path, remote_user, remote_host))
      return(invisible(NULL))
    }
    
    # Prompt for the remote server password without echoing it in the console
    remote_password <- rstudioapi::askForPassword(prompt = "Enter remote server password:")
    
    # Attempt to copy the SSH key using sshpass if a password is provided
    if (nchar(remote_password) > 0) {
      copy_command <- sprintf(
        'sshpass -p "%s" ssh-copy-id -f -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null %s@%s',
        remote_password,
        remote_user,
        remote_host
      )
      message("Copying SSH key to remote server using sshpass...")
      
      # Execute the copy command and capture the output
      copy_output <- tryCatch(
        system(copy_command, intern = TRUE, ignore.stderr = FALSE),
        error = function(e) e
      )
      
      # Check the exit status of the command
      if (inherits(copy_output, "error")) {
        warning("Failed to execute ssh-copy-id command.")
        message("Error Details: ", copy_output$message)
      } else {
        exit_status <- attr(copy_output, "status")
        
        if (is.null(exit_status) || exit_status == 0) {
          message("SSH key copied to remote server successfully.")
        } else {
          warning("Failed to copy SSH key to remote server. Please ensure `sshpass` is installed and your credentials are correct.")
          message("Command Output:\n", paste(copy_output, collapse = "\n"))
        }
      }
    } else {
      message("Password not provided. Unable to copy SSH key.")
    }
  } else {
    message("No remote server specified. Skipping SSH key copying.")
  }
}
