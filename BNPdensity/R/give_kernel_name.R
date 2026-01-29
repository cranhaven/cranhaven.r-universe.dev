kern_idx <- setNames(object = c("Normal", "Gamma", "Beta", "Double Exponential", "Lognormal"), nm = as.character(seq_along(c("Normal", "Gamma", "Beta", "Double Exponential", "Lognormal"))))

#' Gives the kernel name from the integer code
#'
#' This function is used in the print methods for MixNRMI1, MixNRMI2, MixNRMI1cens, MixNRMI2cens, and all the multMixNRMIx versions
#'
#' @inheritParams MixNRMI1
#'
#' @return A character with the name of the distribution used as the kernel
#'
#' @examples
#' BNPdensity:::give_kernel_name(4)
give_kernel_name <- function(distr.k) {
  kern_idx[as.character(distr.k)]
}
