#' CondorUnbox: Synchronise Remote Job Output with Local Directory
#'
#' Downloads and extracts a remote archive created by a Condor job to synchronise 
#' the output files with a local Git project directory. The function optionally 
#' skips overwriting existing files in the destination directory.
#'
#' @param remote_user Character. The username for the remote server.
#' @param remote_host Character. The remote server address.
#' @param remote_dir Character. The directory on the remote server containing the output archive.
#' @param local_git_dir Character. The local directory (typically a Git project) where 
#'   the archive contents will be extracted. Defaults to the current working directory.
#' @param remote_output_file Character. The name of the output archive on the remote server. 
#'   Defaults to "output_archive.tar.gz".
#' @param overwrite Logical. Whether to overwrite existing files in the local directory. 
#'   Defaults to \code{FALSE}.
#'
#' @return No return value. Outputs are downloaded, extracted, and synchronised with 
#'   the specified local directory.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Downloads the output archive from a remote server using \code{scp}.
#'   \item Extracts the contents of the archive to a temporary directory.
#'   \item Moves the extracted files to the specified local directory, optionally skipping files 
#'         that already exist (controlled by the \code{overwrite} parameter).
#'   \item Cleans up temporary files after synchronisation.
#' }
#'
#' A progress bar is displayed to indicate the function's progress through its steps.
#'
#' @examples
#' \dontrun{
#' CondorUnbox(
#'   remote_user = "myuser",
#'   remote_host = "remote.server.com",
#'   remote_dir = "/home/myuser/jobs",
#'   local_git_dir = "/path/to/my/local/git/project",
#'   remote_output_file = "output_archive.tar.gz",
#'   overwrite = FALSE
#' )
#' }
#'
#' @export
CondorUnbox <- function(
    remote_user,
    remote_host,
    remote_dir,
    local_git_dir = getwd(),  # Path to your local Git project directory
    remote_output_file = "output_archive.tar.gz",  # Name of the output archive on the remote server
    overwrite = FALSE  # Whether to overwrite existing files
) {
  # Define the local path to save the remote output file
  local_output_file <- file.path(tempdir(), remote_output_file)
  
  # Step 1: Download the output archive from the remote server
  message("Starting the CondorUnbox process...")
  progress <- txtProgressBar(min = 0, max = 3, style = 3)
  setTxtProgressBar(progress, 0)
  
  message("\nDownloading output archive from the remote server...")
  tryCatch({
    system(sprintf("scp %s@%s:%s/%s %s",
                   remote_user, remote_host, remote_dir, remote_output_file, local_output_file))
    setTxtProgressBar(progress, 1)
    message("Output archive downloaded successfully to: ", local_output_file)
  }, error = function(e) {
    close(progress)
    stop("Failed to download the output archive: ", e$message)
  })
  
  # Step 2: Extract the archive and flatten its structure
  message("\nExtracting the output archive to the local Git project directory...")
  tryCatch({
    # Create a temporary extraction directory
    temp_extract_dir <- file.path(tempdir(), "temp_extract")
    dir.create(temp_extract_dir, showWarnings = FALSE)
    
    # Extract the archive to the temporary directory
    utils::untar(local_output_file, exdir = temp_extract_dir, tar = "internal")
    setTxtProgressBar(progress, 2)
    
    # Identify the top-level directory in the archive
    top_level_dir <- list.dirs(temp_extract_dir, full.names = TRUE, recursive = FALSE)
    if (length(top_level_dir) != 1) {
      stop("The archive does not have a single top-level directory.")
    }
    
    # Move all files from the top-level directory to the local Git directory
    all_files <- list.files(top_level_dir, recursive = TRUE, full.names = TRUE)
    skipped_files <- 0  # Counter for skipped files
    for (file in all_files) {
      relative_path <- sub(paste0("^", top_level_dir, "/"), "", file)  # Remove the top-level directory path
      dest_path <- file.path(local_git_dir, relative_path)
      
      # Check if the destination file already exists
      if (!overwrite && file.exists(dest_path)) {
        message(sprintf("Skipping existing file: %s", relative_path))
        skipped_files <- skipped_files + 1
        next
      }
      
      # Ensure the destination directory exists
      dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
      
      # Move the file to the local Git directory
      file.rename(file, dest_path)
    }
    
    # Clean up the temporary extraction directory
    unlink(temp_extract_dir, recursive = TRUE)
    message("Archive extracted successfully to: ", local_git_dir)
    if (skipped_files > 0) {
      message(sprintf("%d files were skipped as they already exist.", skipped_files))
    }
  }, error = function(e) {
    close(progress)
    stop("Failed to extract the archive: ", e$message)
  })
  
  # Step 3: Clean up the downloaded archive
  message("\nCleaning up the downloaded archive...")
  unlink(local_output_file)
  setTxtProgressBar(progress, 3)
  close(progress)
  
  message("Cleanup completed. Files are now available in your local Git directory.")
}
