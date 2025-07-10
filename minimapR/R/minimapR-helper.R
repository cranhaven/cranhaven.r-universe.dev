## minimapR-helper.R
## LICENSE: MIT License

## Install minimap2 using conda or mamba
#' @title minimap2_install
#'
#' @description Install \code{minimap2} using conda or mamba. Conda or mamba is required for installation.
#' @param verbose Logical value to print progress of the installation. Default is \code{TRUE}. If set to \code{FALSE}, it will suppress the output messages during installation.
#' @return If '\code{minimap2}' is not installed, this function installs it on windows, linux or macOS.
#' @examples
#' \dontrun{
#' mm2_install(verbose = TRUE)
#' }
#' @export
mm2_install <- function(verbose = TRUE) {
  # Check if minimap2 is already installed
  check <- Sys.which("minimap2")
  if (nchar(check) <= 1) {
     
      # Install samtools
      install_out <- try(
        { # Determine which anaconda package is installed
          if (Sys.which("conda") != "") {
            message("Conda is available on the system.")
            system(paste0("conda install -y bioconda::minimap2"),
              intern = verbose, ignore.stdout = !verbose, ignore.stderr = !verbose
          )
          } else if (Sys.which("mamba") != "") {
            message("Mamba is available on the system. Using mamba to install minimap2.")
            system(paste0("mamba install -y bioconda::minimap2"),
              intern = verbose, ignore.stdout = !verbose, ignore.stderr = !verbose
            )
          } else {
            stop("Conda is not available on the system. Please install conda.")
          }
        }, silent = !verbose
      )
  } else {
    # If minimap2 is already installed, return the path
    message("minimap2 is already installed at: ", check)
  }

    # Add minimap2 to PATH
    message(
      "\n", install_out,
      "\nminimap2 successfully installed."
    )
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
#' @return If '\code{samtools}' is not installed, this function installs it returns the path of the installed \code{'samtools'} tool (character).
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
        { # Determine which anaconda package is installed
          if (Sys.which("conda") != "") {
            message("Conda is available on the system.")
            system(paste0("conda install -y bioconda::samtools"),
              intern = verbose, ignore.stdout = !verbose, ignore.stderr = !verbose
          )
          } else if (Sys.which("mamba") != "") {
            message("Mamba is available on the system. Using mamba to install samtools.")
            system(paste0("mamba install -y bioconda::samtools"),
              intern = verbose, ignore.stdout = !verbose, ignore.stderr = !verbose
            )
          } else {
            stop("Conda is not available on the system. Please install conda.")
          }
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
  return(Sys.which("samtools")) # Return the path of samtools if installed
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
#' @param verbose Logical value to print progress of the installation
#' @return This function returns the path of the installed 'minimap2' tool (character).
#' @examples
#' \dontrun{
#' minimap2_installation(verbose = FALSE)
#' }
#' @export
minimap2_installation <- function(verbose = TRUE) {
  # Check if minimap2 is already installed
  minimap2_path <- file.exists(Sys.which("minimap2"))
  if (.Platform$OS.type == "windows") {
    if (!minimap2_path) {
      if (verbose) {
        message("minimap2 is not installed on your system, detailed installation instructions are provided below.\n")
        message("Documentation for Windows install:\n")
        message("1. install MSYS2 from https://www.msys2.org/ and follow the instructions to install it.\n")
        message("2. Open the MINGW64 MSYS2 installation and run:\n")
        message("   pacman -Syu\n")
        message("   pacman -S mingw-w64-x86_64-samtools\n")
        message("   pacman -S mingw-w64-x86_64-zlib mingw-w64-x86_64-gcc make autoconf automake libtool git\n")
        message("3. Then run:\n")
        message("   git clone https://github.com/lh3/minimap2.git
                    cd minimap2
                    make")
        message("4. !! After installation, add the path to the MSYS2 binaries to your system PATH !!\n")
        Sys.sleep(3)
        mm2_install(verbose = verbose)
        Sys.sleep(5)
        samtools_install(verbose = verbose)
      }
    } else {
      message("'minimap2' is already installed. \n")
    }
  } else if (.Platform$OS.type == "unix" || Sys.info()["sysname"] == "Darwin") {
    if (!minimap2_path) {
      message("minimap2 is not installed on your system, installing now...\n")
      Sys.sleep(3)
      mm2_install(verbose = verbose)
      Sys.sleep(5)
      samtools_install(verbose = verbose)
    } else {
      message("'minimap2' is already installed.\n")
    }
  }
}