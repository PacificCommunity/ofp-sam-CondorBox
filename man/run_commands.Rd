% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_commands.R
\name{run_commands}
\alias{run_commands}
\title{Run Commands Sequentially or in Parallel}
\usage{
run_commands(
  commands,
  work_dirs = NULL,
  parallel = FALSE,
  cores = parallel::detectCores() - 1,
  verbose = FALSE,
  save_log = FALSE,
  log_file = "execution_log.txt"
)
}
\arguments{
\item{commands}{A character vector of commands to execute.}

\item{work_dirs}{A character vector of working directories corresponding to each command.}

\item{parallel}{Logical. If TRUE, commands are executed in parallel.}

\item{cores}{Integer. Number of cores to use for parallel execution.}

\item{verbose}{Logical. If TRUE, detailed logs are printed during execution.}

\item{save_log}{Logical. If TRUE, execution results are saved to a log file.}

\item{log_file}{Character. Path to the log file for saving results.}
}
\value{
A list of results, each containing command, success, output, and error fields.
}
\description{
This function executes a list of shell commands, either sequentially or in parallel,
with options to specify working directories, control verbosity, and save logs.
}
\examples{
# Sequential execution
results <- run_commands(c("echo Hello", "ls"))

# Parallel execution
results <- run_commands(c("echo Hello", "ls"), parallel = TRUE, cores = 2)
}
