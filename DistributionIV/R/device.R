#' Determine Device for Torch Computations
#'
#' This function selects the most appropriate device (e.g., CUDA, MPS, or CPU)
#' for Torch computations based on system availability.
#'
#' @keywords internal
#' @import torch
#' @noRd
use_device <- function() {
  # Check if the 'torch' package is available
  if (!requireNamespace("torch", quietly = TRUE)) {
    packageStartupMessage("The 'torch' package is not installed. Defaulting to CPU. Install it with install.packages('torch').")
    return("cpu")
  }

  # Check if torch dependencies are installed
  if (!torch::torch_is_installed()) {
    packageStartupMessage("Torch dependencies are missing. Please install them using torch::install_torch().")
    return("cpu")
  }

  # Check for available device
  if (torch::cuda_is_available()) {
    return(torch::torch_device("cuda"))
  } else if (torch::backends_mps_is_available()) {
    return(torch::torch_device("mps"))
  } else {
    return(torch::torch_device("cpu"))
  }
}
