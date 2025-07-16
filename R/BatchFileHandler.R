#' Batch File Handler for Remote HTCondor Operations
#'
#' This function handles file operations (fetch/delete) on remote HTCondor batch directories,
#' including archive extraction capabilities for tar.gz and zip files.
#'
#' @param remote_user Character. SSH username for remote server access.
#' @param remote_host Character. SSH hostname for remote server access.
#' @param folder_name Character. Direct folder path on remote server (e.g., "/path/to/SWO_Batch_1").
#' @param file_name Character. File or folder name to handle. Optional when extracting archives.
#' @param action Character. Action to perform: "delete" or "fetch". Default is "delete".
#' @param fetch_dir Character. Local directory for fetched files. Default is current directory ".".
#' @param wait_if_running Logical. Whether to wait for HTCondor job to start running before delete action. Default is FALSE.
#' @param check_sec Numeric. Polling interval in seconds when waiting for job status. Default is 15.
#' @param extract_archive Logical. Whether to extract archive files. Default is FALSE.
#' @param archive_name Character. Archive file name (e.g., "output_archive.tar.gz"). Required when extract_archive is TRUE.
#' @param extract_pattern Character. Pattern to match files inside archive for selective extraction.
#' @param extract_folder Character. Specific folder to extract from archive.
#' @param extract_entire Logical. Whether to extract entire archive contents. Default is FALSE.
#'
#' @return Logical. Returns TRUE if operation was successful, FALSE otherwise (invisibly).
#'
#' @details
#' The function supports several operations:
#' \itemize{
#'   \item \strong{Delete}: Remove files or folders from remote server
#'   \item \strong{Fetch}: Download files or folders from remote server
#'   \item \strong{Archive extraction}: Download and extract tar.gz or zip archives
#'   \item \strong{Selective extraction}: Extract specific files/folders from archives
#'   \item \strong{Job monitoring}: Wait for HTCondor jobs to start running before deletion
#' }
#'
#' Archive extraction supports:
#' \itemize{
#'   \item \strong{tar.gz/tgz}: Full extraction, folder-specific, or pattern-based
#'   \item \strong{zip}: Full extraction, folder-specific, or pattern-based
#' }
#'
#' @examples
#' \dontrun{
#' # Fetch a regular file
#' BatchFileHandler(
#'   remote_user = "user",
#'   remote_host = "server.com",
#'   folder_name = "/path/to/SWO_Batch_1",
#'   file_name = "results.csv",
#'   action = "fetch",
#'   fetch_dir = "~/downloads"
#' )
#'
#' # Extract entire tar.gz archive
#' BatchFileHandler(
#'   remote_user = "user",
#'   remote_host = "server.com",
#'   folder_name = "/path/to/SWO_Batch_1",
#'   action = "fetch",
#'   fetch_dir = "~/extracted",
#'   extract_archive = TRUE,
#'   archive_name = "output_archive.tar.gz",
#'   extract_entire = TRUE
#' )
#'
#' # Extract specific folder from archive
#' BatchFileHandler(
#'   remote_user = "user",
#'   remote_host = "server.com",
#'   folder_name = "/path/to/SWO_Batch_1",
#'   action = "fetch",
#'   fetch_dir = "~/extracted",
#'   extract_archive = TRUE,
#'   archive_name = "results.zip",
#'   extract_folder = "plots"
#' )
#'
#' # Delete file after job starts running
#' BatchFileHandler(
#'   remote_user = "user",
#'   remote_host = "server.com",
#'   folder_name = "/path/to/SWO_Batch_1",
#'   file_name = "clone_job.sh",
#'   action = "delete",
#'   wait_if_running = TRUE
#' )
#' }
#'
#' @export




BatchFileHandler <- function(
    remote_user,
    remote_host,
    folder_name,
    file_name = NULL,
    action = "delete",
    fetch_dir = ".",
    wait_if_running = FALSE,
    check_sec = 15,
    extract_archive = FALSE,
    archive_name = NULL,
    extract_pattern = NULL,
    extract_folder = NULL,
    extract_entire = FALSE,
    direct_extract = FALSE
) {
  # Validate action parameter
  action <- match.arg(action, choices = c("delete", "fetch"))
  
  # Validate parameters
  if (action == "delete" && is.null(file_name)) {
    stop("file_name is required for delete action")
  }
  
  if (action == "fetch" && !extract_archive && is.null(file_name)) {
    stop("file_name is required for fetch action when not extracting archive")
  }
  
  # Construct full remote path (only if file_name is provided)
  if (!is.null(file_name)) {
    remote_path <- file.path(folder_name, file_name)
    message(sprintf("[%s] %s – %s",
                    format(Sys.time(), "%H:%M:%S"), action, remote_path))
  } else {
    message(sprintf("[%s] %s – extracting from archive in %s",
                    format(Sys.time(), "%H:%M:%S"), action, folder_name))
  }
  
  #---------------------------------------------------------------------------
  # Wait for job to start running (if requested)
  #---------------------------------------------------------------------------
  if (wait_if_running && action == "delete") {
    # Extract batch name from folder path for condor_q lookup
    batch_name <- basename(folder_name)
    if (grepl("_Batch_", batch_name)) {
      batch_name <- sub(".*_Batch_", "SWO_Batch_", batch_name)
    }
    
    message(sprintf("Waiting for batch '%s' to start running...", batch_name))
    repeat {
      q_cmd <- sprintf(
        "ssh %s@%s \"condor_q -constraint 'BatchName == \\\"%s\\\"' -format '%%s\\n' JobStatus\"",
        remote_user, remote_host, batch_name)
      s <- suppressWarnings(as.numeric(system(q_cmd, intern = TRUE, ignore.stderr = TRUE)))
      
      if (length(s) == 0) {
        message(sprintf("  No jobs found for batch '%s' - stopping wait", batch_name))
        break
      }
      if (any(s == 2)) {
        message(sprintf("  Batch '%s' is now running", batch_name))
        break
      }
      message(sprintf("  Batch '%s' still waiting (status: %s)", batch_name, paste(s, collapse=",")))
      Sys.sleep(check_sec)
    }
  }
  
  #---------------------------------------------------------------------------
  # Helper function to extract files from archive (LOCAL - EXTRACT CONTENTS ONLY)
  #---------------------------------------------------------------------------
  extract_from_archive <- function(archive_path, output_dir) {
    # Determine archive type and extraction command
    if (grepl("\\.tar\\.gz$|\\.tgz$", archive_path)) {
      # Handle tar.gz files
      if (extract_entire) {
        cmd <- sprintf("tar -xzf %s -C %s", archive_path, output_dir)
        message("  Extracting entire tar.gz archive...")
      } else if (!is.null(extract_folder)) {
        # Extract folder contents only using temp directory
        temp_extract_dir <- file.path(tempdir(), "extract_temp")
        dir.create(temp_extract_dir, recursive = TRUE)
        
        # Extract the folder to temp directory
        cmd1 <- sprintf("tar -xzf %s -C %s %s", 
                        archive_path, temp_extract_dir, extract_folder)
        result1 <- system(cmd1, ignore.stderr = TRUE)
        
        if (result1 == 0) {
          # Find the extracted folder and copy its contents
          extracted_path <- file.path(temp_extract_dir, extract_folder)
          
          if (dir.exists(extracted_path)) {
            # Copy contents to output directory
            all_files <- list.files(extracted_path, full.names = TRUE, recursive = TRUE)
            all_dirs_in_target <- list.dirs(extracted_path, full.names = TRUE, recursive = TRUE)
            
            # Create directory structure
            for (dir_path in all_dirs_in_target) {
              rel_path <- gsub(paste0(extracted_path, "/"), "", dir_path)
              if (nchar(rel_path) > 0) {
                dir.create(file.path(output_dir, rel_path), recursive = TRUE, showWarnings = FALSE)
              }
            }
            
            # Copy files
            for (file_path in all_files) {
              rel_path <- gsub(paste0(extracted_path, "/"), "", file_path)
              file.copy(file_path, file.path(output_dir, rel_path), recursive = TRUE)
            }
            
            message(sprintf("  Extracted contents of folder: %s", extract_folder))
          }
          
          # Clean up temp directory
          unlink(temp_extract_dir, recursive = TRUE)
          return(TRUE)
        } else {
          unlink(temp_extract_dir, recursive = TRUE)
          return(FALSE)
        }
      } else if (!is.null(extract_pattern)) {
        cmd <- sprintf("tar -xzf %s -C %s --wildcards '%s'", 
                       archive_path, output_dir, extract_pattern)
        message(sprintf("  Extracting files matching pattern: %s", extract_pattern))
      } else {
        message("  No extraction criteria specified for tar.gz")
        return(FALSE)
      }
    } else if (grepl("\\.zip$", archive_path)) {
      # Handle zip files
      if (extract_entire) {
        cmd <- sprintf("unzip -q %s -d %s", archive_path, output_dir)
        message("  Extracting entire zip archive...")
      } else if (!is.null(extract_folder)) {
        # For zip, extract to temp and then copy contents
        temp_extract_dir <- file.path(tempdir(), "extract_temp")
        dir.create(temp_extract_dir, recursive = TRUE)
        
        cmd1 <- sprintf("unzip -q %s '%s/*' -d %s", 
                        archive_path, extract_folder, temp_extract_dir)
        result1 <- system(cmd1, ignore.stderr = TRUE)
        
        if (result1 == 0) {
          # Find and copy folder contents
          extracted_path <- file.path(temp_extract_dir, extract_folder)
          
          if (dir.exists(extracted_path)) {
            # Copy contents to output directory
            all_files <- list.files(extracted_path, full.names = TRUE, recursive = TRUE)
            all_dirs_in_target <- list.dirs(extracted_path, full.names = TRUE, recursive = TRUE)
            
            # Create directory structure
            for (dir_path in all_dirs_in_target) {
              rel_path <- gsub(paste0(extracted_path, "/"), "", dir_path)
              if (nchar(rel_path) > 0) {
                dir.create(file.path(output_dir, rel_path), recursive = TRUE, showWarnings = FALSE)
              }
            }
            
            # Copy files
            for (file_path in all_files) {
              rel_path <- gsub(paste0(extracted_path, "/"), "", file_path)
              file.copy(file_path, file.path(output_dir, rel_path), recursive = TRUE)
            }
            
            message(sprintf("  Extracted contents of folder: %s", extract_folder))
          }
          
          unlink(temp_extract_dir, recursive = TRUE)
          return(TRUE)
        } else {
          unlink(temp_extract_dir, recursive = TRUE)
          return(FALSE)
        }
      } else if (!is.null(extract_pattern)) {
        cmd <- sprintf("unzip -q %s '%s' -d %s", 
                       archive_path, extract_pattern, output_dir)
        message(sprintf("  Extracting files matching pattern: %s", extract_pattern))
      } else {
        message("  No extraction criteria specified for zip")
        return(FALSE)
      }
    } else {
      message(sprintf("  Unsupported archive format: %s", archive_path))
      return(FALSE)
    }
    
    # Execute extraction command (for non-folder extractions)
    if (!is.null(extract_pattern) || extract_entire) {
      result <- system(cmd, ignore.stderr = TRUE)
      if (result == 0) {
        message("  ✔ Archive extracted successfully")
        return(TRUE)
      } else {
        message("  ✗ Archive extraction failed")
        return(FALSE)
      }
    }
    
    return(TRUE)  # For folder extractions, already handled above
  }
  
  #---------------------------------------------------------------------------
  # Helper function for direct extraction from remote (EXTRACT CONTENTS ONLY)
  #---------------------------------------------------------------------------
  extract_remote_direct <- function(remote_archive_path, output_dir) {
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Only support tar.gz for direct extraction
    if (!grepl("\\.tar\\.gz$|\\.tgz$", remote_archive_path)) {
      message("  Direct extraction only supports tar.gz files")
      return(FALSE)
    }
    
    # Build extraction command based on criteria
    if (extract_entire) {
      # Extract entire archive to remote temp, then stream back
      temp_dir <- sprintf("/tmp/extract_%s", basename(tempfile()))
      cmd <- sprintf("ssh %s@%s 'mkdir -p %s && tar -xzf %s -C %s && tar -czf - -C %s .' | tar -xzf - -C %s", 
                     remote_user, remote_host, temp_dir, remote_archive_path, temp_dir, temp_dir, output_dir)
      
      # Cleanup command
      cleanup_cmd <- sprintf("ssh %s@%s 'rm -rf %s'", remote_user, remote_host, temp_dir)
      
      message("  Extracting entire archive directly from remote...")
    } else if (!is.null(extract_folder)) {
      # Extract specific folder contents only (not the folder itself)
      temp_dir <- sprintf("/tmp/extract_%s", basename(tempfile()))
      
      # Use the exact path provided in extract_folder
      folder_path <- extract_folder
      
      # Extract to temp, then tar the contents of the specified folder, not the folder itself
      cmd <- sprintf("ssh %s@%s 'mkdir -p %s && tar -xzf %s -C %s %s && tar -czf - -C %s/%s .' | tar -xzf - -C %s", 
                     remote_user, remote_host, temp_dir, remote_archive_path, temp_dir, folder_path, temp_dir, folder_path, output_dir)
      
      # Cleanup command
      cleanup_cmd <- sprintf("ssh %s@%s 'rm -rf %s'", remote_user, remote_host, temp_dir)
      
      message(sprintf("  Extracting contents of folder '%s' directly from remote...", extract_folder))
    } else if (!is.null(extract_pattern)) {
      # Extract files matching pattern
      temp_dir <- sprintf("/tmp/extract_%s", basename(tempfile()))
      cmd <- sprintf("ssh %s@%s 'mkdir -p %s && tar -xzf %s -C %s --wildcards \"%s\" && tar -czf - -C %s .' | tar -xzf - -C %s", 
                     remote_user, remote_host, temp_dir, remote_archive_path, temp_dir, extract_pattern, temp_dir, output_dir)
      
      # Cleanup command
      cleanup_cmd <- sprintf("ssh %s@%s 'rm -rf %s'", remote_user, remote_host, temp_dir)
      
      message(sprintf("  Extracting files matching pattern '%s' directly from remote...", extract_pattern))
    } else {
      message("  No extraction criteria specified")
      return(FALSE)
    }
    
    # Execute command
    result <- system(cmd, ignore.stderr = TRUE)
    
    # Cleanup remote temp directory
    system(cleanup_cmd, ignore.stderr = TRUE)
    
    return(result == 0)
  }
  
  #---------------------------------------------------------------------------
  # Perform the action
  #---------------------------------------------------------------------------
  if (action == "delete") {
    # Delete file or folder
    rm_cmd <- sprintf("ssh %s@%s 'rm -rf %s'", 
                      remote_user, remote_host, remote_path)
    res <- system(rm_cmd, ignore.stderr = TRUE)
    if (res == 0) {
      message("  ✔ deleted")
    } else {
      message("  ✗ delete failed (maybe file not present)")
    }
    
  } else if (action == "fetch") {
    # Create fetch directory if it doesn't exist
    if (!dir.exists(fetch_dir)) {
      dir.create(fetch_dir, recursive = TRUE)
      message(sprintf("Created directory: %s", fetch_dir))
    }
    
    # Handle archive extraction
    if (extract_archive && !is.null(archive_name)) {
      archive_remote_path <- file.path(folder_name, archive_name)
      
      if (direct_extract) {
        # Use direct extraction
        message(sprintf("  Using direct extraction for: %s", archive_name))
        success <- extract_remote_direct(archive_remote_path, fetch_dir)
        res <- if (success) 0 else 1
        
        if (success) {
          message("  ✔ Direct extraction successful")
        } else {
          message("  ✗ Direct extraction failed")
        }
      } else {
        # Download then extract
        temp_archive <- file.path(fetch_dir, archive_name)
        
        # Download archive file
        scp_cmd <- sprintf("scp %s@%s:%s %s",
                           remote_user, remote_host, archive_remote_path, temp_archive)
        message(sprintf("  Downloading archive: %s", archive_name))
        res <- system(scp_cmd, ignore.stderr = TRUE)
        
        if (res == 0) {
          message("  ✔ Archive downloaded successfully")
          
          # Extract from archive
          if (extract_from_archive(temp_archive, fetch_dir)) {
            # Remove temporary archive file
            unlink(temp_archive)
            message("  ✔ Temporary archive file removed")
          }
        } else {
          message("  ✗ Archive download failed")
        }
      }
      
    } else {
      # Regular file/folder fetch
      if (!is.null(file_name) && (grepl("/$", file_name) || grepl("\\*", file_name))) {
        # Handle folder or wildcard pattern
        scp_cmd <- sprintf("scp -r %s@%s:%s %s",
                           remote_user, remote_host, remote_path, fetch_dir)
        message("  Fetching folder/pattern...")
      } else {
        # Handle single file
        dest_path <- file.path(fetch_dir, basename(file_name))
        scp_cmd <- sprintf("scp %s@%s:%s %s",
                           remote_user, remote_host, remote_path, dest_path)
        message("  Fetching single file...")
      }
      
      res <- system(scp_cmd, ignore.stderr = TRUE)
      if (res == 0) {
        message("  ✔ fetched successfully")
      } else {
        message("  ✗ fetch failed (maybe file not present)")
      }
    }
  }
  
  invisible(res == 0)  # Return TRUE if successful
}



