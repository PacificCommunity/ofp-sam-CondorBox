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
  # Normalize the path to the SSH private key
  ssh_key_path <- normalizePath("~/.ssh/id_rsa", mustWork = FALSE)
  
  # Check if the .ssh directory exists and create it if it doesn't
  ssh_dir <- dirname(ssh_key_path)
  if (!dir.exists(ssh_dir)) {
    message("Creating ~/.ssh directory...")
    dir.create(ssh_dir, recursive = TRUE, mode = "0700")
  }
  
  # Check if the SSH key already exists
  if (!file.exists(ssh_key_path)) {
    message("Generating SSH key...")
    
    # Specify the full path to ssh-keygen
    ssh_keygen_path <- system("which ssh-keygen", intern = TRUE)
    if (length(ssh_keygen_path) == 0 || ssh_keygen_path == "") {
      stop("ssh-keygen not found. Please install the OpenSSH client.")
    }
    
    # Construct the ssh-keygen command
    keygen_command <- sprintf('"%s" -q -t rsa -b 4096 -C "%s" -f "%s" -N ""', 
                              ssh_keygen_path, email, ssh_key_path)
    
    # Execute the ssh-keygen command and capture output
    keygen_output <- tryCatch(
      {
        system(keygen_command, intern = TRUE, ignore.stderr = FALSE)
      },
      error = function(e) {
        stop("Error running ssh-keygen: ", e$message)
      }
    )
    
    # Verify if the SSH key was generated successfully
    if (!file.exists(paste0(ssh_key_path, ".pub"))) {
      stop("SSH key generation failed. Details:\n", paste(keygen_output, collapse = "\n"))
    }
    message("SSH key generated at: ", ssh_key_path)
  } else {
    message("SSH key already exists at: ", ssh_key_path)
  }
  
  # Read the public SSH key
  public_key <- tryCatch(
    paste(readLines(paste0(ssh_key_path, ".pub")), collapse = "\n"),
    error = function(e) stop("Failed to read the public key. Ensure SSH key generation was successful.")
  )
  
  # Add the SSH key to GitHub
  if (!is.null(github_token)) {
    library(httr)
    api_url <- "https://api.github.com/user/keys"
    response <- POST(
      url = api_url,
      add_headers(Authorization = paste("token", github_token)),
      body = list(title = paste("RStudio Server Key -", Sys.Date()), key = public_key),
      encode = "json"
    )
    if (status_code(response) == 201) {
      message("SSH key successfully added to your GitHub account.")
    } else if (status_code(response) == 422 && grepl("key is already in use", content(response)$errors[[1]]$message)) {
      message("The SSH key is already registered with your GitHub account. Proceeding...")
    } else {
      message("Failed to add SSH key to GitHub. Please add it manually.")
      print(content(response))
    }
  } else {
    message("\nNo GitHub token provided. Add the SSH key manually:")
    message(public_key)
    message("\nGo to: https://github.com/settings/keys, click 'New SSH key', and paste the key above.")
  }
  
  # Add the SSH key to the SSH agent
  message("\nAdding SSH key to the SSH agent...")
  agent_status <- system("eval $(ssh-agent -s)", intern = TRUE)
  if (!grepl("Agent pid", agent_status)) {
    stop("Failed to start the SSH agent. Please start it manually.")
  }
  system(sprintf('ssh-add "%s"', ssh_key_path), ignore.stderr = TRUE)
  
  # Test SSH connection to GitHub
  message("\nTesting SSH connection to GitHub...")
  ssh_test <- tryCatch(
    {
      system("ssh -o StrictHostKeyChecking=no -T git@github.com", intern = TRUE, ignore.stderr = TRUE)
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (!is.null(ssh_test) && length(ssh_test) > 0 && any(grepl("successfully authenticated", ssh_test))) {
    message("SSH connection successful.")
  } else if (!is.null(ssh_test) && length(ssh_test) > 0) {
    message("SSH connection failed, but SSH command returned output:\n", paste(ssh_test, collapse = "\n"))
  } else {
    message("SSH connection test failed. Ensure your SSH key is added to GitHub.")
  }
  
  # Set global Git configuration
  message("\nSetting global Git configuration...")
  system(sprintf('git config --global user.name "%s"', github_username))
  system(sprintf('git config --global user.email "%s"', email))
  
  # Display Git configuration
  message("\nGit configuration set:")
  system("git config --list")
  
  message("\n=== Setup Complete ===")
  message("You can now use GitHub with SSH from RStudio Server.")
}

