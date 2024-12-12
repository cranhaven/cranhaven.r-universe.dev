## minimapR-helper.R
## LICENSE: MIT License

## Install minimap2 from Heng Li's github repository
### Requires: git2r
### Source directory should not include minimap2 name
#' @title minimap2_install
#'
#' @description Install \code{minimap2} from Heng Li's github repository. If using a Windows operating system, installation of the MSYS2 Linux emulator is required.
#'
#' @param source_directory Source directory to install minimap2. Do not include minimap2 name in the
#'  source directory. Note that this must be entered as a full path location.
#' @param verbose Logical value to print progress of the installation
#' @param return This logical value causes the \code{minimap2_install} function to return the path of minimap2
#' @return If '\code{minimap2}' is not installed, this function installs it on linux and returns the path of the installed '\code{minimap2}' tool (character). 
#' @examples
#' \dontrun{
#' install_dir <- file.path("/dir/to/install")
#' minimap2_path <- mm2_install(source_directory = install_dir, verbose = FALSE)
#' }
#' @export
#' @import git2r
mm2_install <- function(source_directory, verbose = TRUE, return = FALSE) {
  # Check if minimap2 is already installed
  check <- Sys.which("minimap2")
  if (nchar(check) <= 1) {
    # Install minimap2
    install_dir <- paste0(source_directory, "/minimap2")
    if (!dir.exists(install_dir)) {
      dir.create(install_dir)
    }

    if (verbose) {
      message("Installing minimap2 to directory", install_dir, "...")
    }

    # Git clone minimap
    download_out <- try({git2r::clone(
          url = "https://github.com/lh3/minimap2",
          local_path = install_dir,
          progress = verbose
        )}, silent = !verbose
    )

    # Install minimap2
    install_out <- try(
      {
        system(paste0("cd ", install_dir, " && make"), intern = verbose, 
          ignore.stdout = !verbose, ignore.stderr = !verbose)
      }
    )

    # Add minimap2 to PATH
    message(
      "\nminimap2 successfully installed.",
      "\nPlease add minimap2 ", install_dir, " to .bashrc or respective windows path.",
      "\n\texport PATH=$PATH:", install_dir, "\n"
    )

    if (return == TRUE) {
      return(paste0("export PATH=$PATH:", install_dir))
    }
  } else {
    message("minimap2 is already installed.")
  }
}

## Check path of minimap2 and if installed
## If return is true then the path of the executable is returned
##   given that minimap2 is installed
#' @title minimap2_check
#' @description Check if minimap2 is installed
#' @param return Logical value to return the path of minimap2
#' @return If minimap2 is installed, this function returns the path of minimap2 (character).
#' @examples
#' minimap2_check(return = TRUE)
#'
#' @export
minimap2_check <- function(return = TRUE) {
  check <- Sys.which("minimap2")
  if (nchar(check) > 1) {
    message("minimap2 is installed.")
    if (return == TRUE) {
      return(Sys.which("minimap2"))
    }
  } else {
    message(
      "minimap2 is not installed.",
      "\nPlease run minimap2_installation() to install minimap2."
    )
  }
}

## Install samtools with conda
### Requires: conda
#' @title samtools_install
#' @description Install samtools with conda
#' @param verbose Logical value to print progress of the installation
#' @return If '\code{samtools}' is not installed, this function installs it on linux and returns the path of the installed \code{'samtools'} tool (character).
#' @examples
#' \dontrun{
#' samtools_install()
#' }
#'
#' @export
samtools_install <- function(verbose = TRUE) {
  # Check if samtools is already installed
  check <- Sys.which("samtools")
  if (nchar(check) <= 1) {
    if (!is.null(Sys.which("samtools"))) {
      # Install samtools
      if (verbose) {
        message("\nInstalling samtools with conda ...")
      }
      Sys.sleep(3)

      # Install samtools
      install_out <- try(
        {
          system(paste0("conda install -c bioconda -y samtools"),
            intern = verbose, ignore.stdout = !verbose, ignore.stderr = !verbose
          )
        }, silent = !verbose
      )

      # Print output
      if (verbose) {
        message(install_out)
      }

      # Add samtools to PATH
      message("\nSamtools successfully installed.")
    } else {
      message("\nsamtools is already installed.")
    }
  }
}

## Checks if samtools is installed
## If return is true then the path of the executable is returned
##   given that samtools is installed
#' @title samtools_check
#' @description Check if samtools is installed
#' @param return Logical value to return the path of samtools
#' @return If '\code{samtools}' is installed, this function returns the path of samtools (character).
#' @examples
#' samtools_check(return = TRUE)
#'
#' @export
#' @import Rsamtools
samtools_check <- function(return = TRUE) {
  check <- Sys.which("samtools")
  if (nchar(check) > 1) {
    message("\nsamtools is installed.")
    if (return == TRUE) {
      return(Sys.which("samtools"))
    }
  } else {
    message(
      "\nsamtools is not installed.",
      "\nPlease run samtools_install() or minimap2_installation to install samtools."
    )
  }
}

# Function for OS-specific documentation and installation
#' @title minimap2_installation
#' @description This function prints installation instructions specific to the user's operating system.
#' @param source_directory Source directory to install minimap2. Do not include minimap2 name in the
#'  source directory. Note that this must be entered as a full path location.
#' @param verbose Logical value to print progress of the installation
#' @param return This logical value causes the \code{minimap2_install} function to return the path of minimap2
#' @return This function returns the path of the installed 'minimap2' tool (character).
#' @examples
#' \dontrun{
#' install_dir <- file.path("/dir/to/install")
#' minimap2_path <- minimap2_installation(source_directory = install_dir, verbose = FALSE)
#' }
#' @export
#' @import git2r
minimap2_installation <- function(source_directory, verbose = TRUE, return = FALSE) {
  # Check if minimap2 is already installed
  minimap2_path <- file.exists(Sys.which("minimap2"))
  if (.Platform$OS.type == "windows") {
    if (!minimap2_path) {
      if (verbose) {
        message("minimap2 is not installed on your system, detailed installation instructions are provided below.\n")
        message("Documentation for Windows install:\n")
        message("1. Install the 'MSYS2' Linux emulator.\n")
        message("2. In the 'MSYS2' terminal, type 'pacman -Syu'\n")
        message("3. In the 'MSYS2' terminal, type 'pacman -S mingw-w64-x86_64-samtools autotools mingw-w64-x86_64-gcc git'\n")
        message("4. Add 'C:\\msys64\\mingw64\\bin' to 'MSYS2' PATH; 'MSYS2' command: echo 'vim export PATH:$PATH:/mingw64/bin' >> ~/.bashrc\n")
        message("5. To install 'minimap2': \n\t")
        message("a) In the 'MSYS2' terminal, type 'git clone https://github.com/lh3/minimap2' \n\t")
        message("b) In the 'MSYS2' terminal, type 'cd minimap2 && make' \n")
        message("6. Create symbolic link to 'minimap2', 'MSYS2' command: 'ln -s ~/path/to/minimap2.exe /mingw64/bin'\n")
        message("7. add C:\\msys64\\mingw64\\bin to windows PATH'\n")
      }
    } else {
      message("'minimap2' is already installed. \n")
    }
  } else if (.Platform$OS.type == "unix" && Sys.info()["sysname"] != "Darwin") {
    if (!minimap2_path) {
      message("minimap2 is not installed on your system, installing now...\n")
      Sys.sleep(3)
      file_path <- mm2_install(source_directory, verbose = verbose, return = return)
      Sys.sleep(5)
      samtools_install(verbose = verbose)
      if(return == TRUE) {
        return(file_path)
      }
    } else {
      message("'minimap2' is already installed.\n")
    }
  } else if (Sys.info()["sysname"] == "Darwin") {
    message("Documentation for macOS:\n")
    # Add macOS specific instructions here
  }
}