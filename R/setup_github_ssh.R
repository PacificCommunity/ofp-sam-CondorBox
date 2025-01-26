#' Setup GitHub SSH
#'
#' This function sets up an SSH key for GitHub on RStudio Server, including generating a key,
#' adding it to the SSH agent, and optionally uploading it to GitHub via API.
#'
#' @param email Your GitHub email address.
#' @param github_username Your GitHub username.
#' @param github_token A GitHub personal access token with `write:public_key` scope (optional).
#' @export
setup_github_ssh <- function(email, github_username, github_token = NULL) {
  ssh_key_path <- "~/.ssh/id_rsa"
  
  # Detect OS
  os <- Sys.info()[["sysname"]]
  is_windows <- os == "Windows"
  
  if (is_windows) {
    ssh_key_path <- gsub("~", Sys.getenv("USERPROFILE"), ssh_key_path)
  }
  
  # Check if SSH key exists
  if (file.exists(ssh_key_path)) {
    message("SSH key already exists at: ", ssh_key_path)
  } else {
    message("Generating SSH key...")
    system(sprintf('ssh-keygen -t rsa -b 4096 -C "%s" -f "%s" -N ""', email, ssh_key_path))
    message("SSH key generated at: ", ssh_key_path)
  }
  
  # Read public key
  public_key <- tryCatch(
    paste(readLines(paste0(ssh_key_path, ".pub")), collapse = "\n"),
    error = function(e) stop("Failed to read the public key.")
  )
  
  # Add SSH key to GitHub
  if (!is.null(github_token)) {
    library(httr)
    response <- POST(
      url = "https://api.github.com/user/keys",
      add_headers(Authorization = paste("token", github_token)),
      body = list(title = paste("RStudio Server Key -", Sys.Date()), key = public_key),
      encode = "json"
    )
    if (status_code(response) == 201) {
      message("SSH key successfully added to GitHub.")
    } else {
      message("Failed to add SSH key to GitHub. Please add it manually.")
      print(content(response))
    }
  } else {
    message("Add the SSH key manually: ", public_key)
  }
  
  # Add SSH key to the agent
  message("\nAdding SSH key to the SSH agent...")
  if (is_windows) {
    system("start-ssh-agent.cmd")
  } else {
    system("eval $(ssh-agent -s)")
  }
  system(sprintf('ssh-add "%s"', ssh_key_path), ignore.stderr = TRUE)
  
  # Test SSH connection
  message("\nTesting SSH connection to GitHub...")
  ssh_status <- system("ssh -T git@github.com", ignore.stderr = TRUE)
  if (ssh_status == 0 || ssh_status == 1) {
    message("SSH connection successful.")
  } else {
    message("SSH connection failed. Status code: ", ssh_status)
  }
  
  # Configure Git
  message("\nSetting global Git configuration...")
  system(sprintf('git config --global user.name "%s"', github_username))
  system(sprintf('git config --global user.email "%s"', email))
  
  message("\n=== Setup Complete ===")
}
