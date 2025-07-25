% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BatchFileHandler.R
\name{BatchFileHandler}
\alias{BatchFileHandler}
\title{Batch File Handler for Remote HTCondor Operations}
\usage{
BatchFileHandler(
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
)
}
\arguments{
\item{remote_user}{Character. SSH username for remote server access.}

\item{remote_host}{Character. SSH hostname for remote server access.}

\item{folder_name}{Character. Direct folder path on remote server (e.g., "/path/to/SWO_Batch_1").}

\item{file_name}{Character. File or folder name to handle. Optional when extracting archives.}

\item{action}{Character. Action to perform: "delete" or "fetch". Default is "delete".}

\item{fetch_dir}{Character. Local directory for fetched files. Default is current directory ".".}

\item{wait_if_running}{Logical. Whether to wait for HTCondor job to start running before delete action. Default is FALSE.}

\item{check_sec}{Numeric. Polling interval in seconds when waiting for job status. Default is 15.}

\item{extract_archive}{Logical. Whether to extract archive files. Default is FALSE.}

\item{archive_name}{Character. Archive file name (e.g., "output_archive.tar.gz"). Required when extract_archive is TRUE.}

\item{extract_pattern}{Character. Pattern to match files inside archive for selective extraction.}

\item{extract_folder}{Character. Specific folder to extract from archive.}

\item{extract_entire}{Logical. Whether to extract entire archive contents. Default is FALSE.}
}
\value{
Logical. Returns TRUE if operation was successful, FALSE otherwise (invisibly).
}
\description{
This function handles file operations (fetch/delete) on remote HTCondor batch directories,
including archive extraction capabilities for tar.gz and zip files.
}
\details{
The function supports several operations:
\itemize{
  \item \strong{Delete}: Remove files or folders from remote server
  \item \strong{Fetch}: Download files or folders from remote server
  \item \strong{Archive extraction}: Download and extract tar.gz or zip archives
  \item \strong{Selective extraction}: Extract specific files/folders from archives
  \item \strong{Job monitoring}: Wait for HTCondor jobs to start running before deletion
}

Archive extraction supports:
\itemize{
  \item \strong{tar.gz/tgz}: Full extraction, folder-specific, or pattern-based
  \item \strong{zip}: Full extraction, folder-specific, or pattern-based
}
}
\examples{
\dontrun{
# Fetch a regular file
BatchFileHandler(
  remote_user = "user",
  remote_host = "server.com",
  folder_name = "/path/to/SWO_Batch_1",
  file_name = "results.csv",
  action = "fetch",
  fetch_dir = "~/downloads"
)

# Extract entire tar.gz archive
BatchFileHandler(
  remote_user = "user",
  remote_host = "server.com",
  folder_name = "/path/to/SWO_Batch_1",
  action = "fetch",
  fetch_dir = "~/extracted",
  extract_archive = TRUE,
  archive_name = "output_archive.tar.gz",
  extract_entire = TRUE
)

# Extract specific folder from archive
BatchFileHandler(
  remote_user = "user",
  remote_host = "server.com",
  folder_name = "/path/to/SWO_Batch_1",
  action = "fetch",
  fetch_dir = "~/extracted",
  extract_archive = TRUE,
  archive_name = "results.zip",
  extract_folder = "plots"
)

# Delete file after job starts running
BatchFileHandler(
  remote_user = "user",
  remote_host = "server.com",
  folder_name = "/path/to/SWO_Batch_1",
  file_name = "clone_job.sh",
  action = "delete",
  wait_if_running = TRUE
)
}

}
