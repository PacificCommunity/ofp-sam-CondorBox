# ofp-sam-CondorBox

**CondorBox** packages your GitHub repository into a Docker container and submits it to an HTCondor cluster for reproducible, scalable high-performance computing.

---

## Overview

The **CondorBox** package provides the following functionalities:

1. **Clone and Run a GitHub Repository on HTCondor:**
   - Use the `CondorBox()` function to package a GitHub repository into a Docker container, submit it to an HTCondor cluster, and execute the desired operations remotely.
   
2. **Sync Results Locally:**
   - After the job completes remotely, use the `CondorUnbox()` function to download the results from the remote server, extract the output, and synchronise it with your local project directory.

---

## Dependencies

### General Requirements

To use **CondorBox**, ensure the following dependencies are installed and configured on your system:

1. **Docker**  
   - Docker is required to build and run containers.  
   - Install Docker from [https://www.docker.com/products/docker-desktop](https://www.docker.com/products/docker-desktop).  
   - On Windows, ensure **Docker Desktop** is running.

2. **GNU Make**  
   - Make is required for automating tasks defined in Makefiles.  
   - For **Windows** users, `make` can be installed via:
     - **Rtools** (Recommended for R users):  
       Download from [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/).
     - **MinGW** or **Chocolatey**:  
       Use these package managers to install `make`.

   - For **Linux** users:  
     ```bash
     sudo apt-get install build-essential
     ```

   - For **macOS** users:  
     ```bash
     xcode-select --install
     ```

3. **Git Bash (for Windows)**  
   - Required to execute UNIX-style shell commands on Windows.  
   - Install Git from [https://git-scm.com/downloads](https://git-scm.com/downloads).

4. **SSH Access**  
   - SSH should be set up for secure communication with the remote server.  
   - Test connectivity:  
     ```bash
     ssh your_username@remote.server.com
     ```

---

## GitHub Personal Access Token (PAT)

To access private GitHub repositories or to authenticate securely, you need a **GitHub Personal Access Token (PAT)**.

### Steps to Obtain a GitHub PAT

1. Log in to your GitHub account at [https://github.com/](https://github.com/).

2. Navigate to **Settings** > **Developer settings** > **Personal access tokens** > **Tokens (classic)**.

3. Click **Generate new token** > **Generate new token (classic)**.

4. Provide a descriptive **note** (e.g., "CondorBox access").

5. Set the **expiration** date for the token (e.g., 30 days).

6. Select the following **scopes**:
   - `repo` (Full control of private repositories)
   - `read:org` (Optional: Access organisational resources)

7. Click **Generate token**.

8. Copy the generated token **immediately** and store it securely. (It will not be shown again.)

### Using the GitHub PAT with CondorBox

- Save your GitHub PAT as an environment variable to avoid hardcoding it into scripts:
  ```r
  Sys.setenv(GitPass = "your_github_pat")
  ```
- Use the `Sys.getenv("GitPass")` function to access the GitHub PAT in your scripts.