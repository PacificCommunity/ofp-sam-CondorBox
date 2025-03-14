% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manage_rstudio_server.R
\name{manage_rstudio_server}
\alias{manage_rstudio_server}
\title{Manage RStudio Server Containers}
\usage{
manage_rstudio_server(
  action = c("start", "stop", "remove"),
  image = "rocker/rstudio",
  container_name = "rstudio",
  container_port = 8787,
  password = "yourpassword",
  ghcr_login = FALSE,
  github_username = NULL,
  github_token = NULL,
  transfer_env = FALSE,
  local_dir = NULL,
  mount_github_config = FALSE
)
}
\arguments{
\item{action}{A string indicating the action to perform: "start" or "stop".}

\item{image}{The Docker image to use. Default is "rocker/rstudio".}

\item{container_name}{The name of the container. Default is "rstudio".}

\item{container_port}{The port inside the container. Default is 8787.}

\item{password}{The password to set for RStudio Server. Default is "yourpassword".}

\item{ghcr_login}{Logical indicating whether to log in to GitHub Container Registry (ghcr.io). Default is FALSE.}

\item{github_username}{The GitHub username for ghcr.io login. Required if ghcr_login is TRUE.}

\item{github_token}{The GitHub personal access token for ghcr.io login. Required if ghcr_login is TRUE.}

\item{transfer_env}{Logical indicating whether to transfer environment variables from ~/.Renviron to the container. Default is FALSE.}

\item{local_dir}{The path to the local directory to mount into RStudio Server. Default is NULL.}

\item{mount_github_config}{Logical indicating whether to mount ~/.gitconfig and ~/.ssh into the container. Default is FALSE.}

\item{host_port}{The port on the host machine. Default is 8787.}
}
\description{
This function starts or stops an RStudio Server container using Docker or Podman.
It supports creating new containers or restarting existing ones.
}
