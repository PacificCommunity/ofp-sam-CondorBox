#' Run Commands Sequentially or in Parallel
#'
#' This function executes a list of shell commands, either sequentially or in parallel,
#' with options to specify working directories, control verbosity, and save logs.
#'
#' @param commands A character vector of commands to execute.
#' @param work_dirs A character vector of working directories corresponding to each command.
#' @param parallel Logical. If TRUE, commands are executed in parallel.
#' @param cores Integer. Number of cores to use for parallel execution.
#' @param verbose Logical. If TRUE, detailed logs are printed during execution.
#' @param save_log Logical. If TRUE, execution results are saved to a log file.
#' @param log_file Character. Path to the log file for saving results.
#'
#' @return A list of results, each containing command, success, output, and error fields.
#' @export
#' @examples
#' # Sequential execution
#' results <- run_commands(c("echo Hello", "ls"))
#'
#' # Parallel execution
#' results <- run_commands(c("echo Hello", "ls"), parallel = TRUE, cores = 2)
run_commands <- function(
    commands, 
    work_dirs = NULL, 
    parallel = FALSE, 
    cores = parallel::detectCores() - 1, 
    verbose = FALSE, 
    save_log = FALSE, 
    log_file = "execution_log.txt" 
) {
  if (!is.null(work_dirs)) {
    if (length(work_dirs) != length(commands)) {
      stop("The length of 'work_dirs' must match the length of 'commands'.")
    }
    work_dirs <- vapply(work_dirs, normalizePath, character(1), mustWork = TRUE)
  }
  
  run_command <- function(cmd, work_dir, command_index) {
    if (!is.null(work_dir)) {
      old_dir <- getwd()
      on.exit(setwd(old_dir), add = TRUE)
      setwd(work_dir)
    }
    
    cat(sprintf("[Running Command %d]: %s\n", command_index, cmd))
    cat(sprintf("[Debug] Current Working Directory: %s\n", getwd()))
    
    if (!verbose) {
      cmd <- sprintf("%s > /dev/null 2>&1", cmd)
    }
    
    res <- tryCatch({
      output <- system(cmd, intern = TRUE, ignore.stderr = !verbose)
      list(command = cmd, success = TRUE, output = if (verbose) output else NULL, error = NULL)
    }, error = function(e) {
      list(command = cmd, success = FALSE, output = NULL, error = e$message)
    })
    
    status <- if (res$success) "Finished" else "Failed"
    cat(sprintf("[%s Command %d]: %s\n", status, command_index, cmd))
    
    return(res)
  }
  
  results <- if (parallel) {
    cat(sprintf("Running %d commands in parallel using %d cores...\n", length(commands), cores))
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::parLapply(cl, seq_along(commands), function(i) {
      run_command(commands[i], ifelse(is.null(work_dirs), NULL, work_dirs[i]), i)
    })
  } else {
    cat(sprintf("Running %d commands sequentially...\n", length(commands)))
    lapply(seq_along(commands), function(i) {
      run_command(commands[i], ifelse(is.null(work_dirs), NULL, work_dirs[i]), i)
    })
  }
  
  if (save_log) {
    log_lines <- c("[Execution Results]")
    for (res in results) {
      log_lines <- c(
        log_lines,
        sprintf("- Command: %s", res$command),
        if (res$success) {
          c("  Status: Success", paste("  Output:", paste(res$output, collapse = "\n")))
        } else {
          c("  Status: Failed", paste("  Error:", res$error))
        }
      )
    }
    writeLines(log_lines, log_file)
    if (verbose) cat(sprintf("Logs saved to %s\n", log_file))
  }
  
  return(results)
}
