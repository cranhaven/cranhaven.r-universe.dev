#' @title Create a vector of the corregistered images based on the available MRI images modalities.
#'
#' @description  This function creates a vector of coregistered T2-weighted and/or FLAIR images for a patient.
#' The vector contains a vector of length one for only one MRI modality (T2-weighted or FLAIR)
#' or a vector of length two including both modalities (T2-weighted or FLAIR).
#'
#' @param vector output object from the coregistration function.
#' @return vector of coregistered images based on modalities available.
#' @export
coregistration_images <- function(vector){
  if (length(vector)== 2){
    imgs <- lapply(vector, function(x) x$outfile)
  }else{
    imgs <- vector$outfile
  }
  return(imgs)
}


#' Create a vector of inhomogeneity corrected images based on the available MRI images modalities.
#'
#' This function creates a vector of imhomogeneity corrected images for a patient.
#' The vector contains a vector of the length of the MRI modalities (T1-weighted, T2-weighted and/or FLAIR).
#'
#' @param modalities vector of strings containing the modalities to be preprocessed. It must always contains the T1-weighted sequence scan.
#' @param bias_T1 bias image in NifTI format.
#' @param list_corregister list of paths of corregisted images.
#' @return vector of inhomogeneity corrected images based on modalities available.
#' @export
create_bias_list <- function (modalities, bias_T1, list_corregister){
  bias_mris <- list()
  bias_mris$T1 <- bias_T1
  if (length(list_corregister)== 2){
    for (n in 1:length(list_corregister)) {
      modality <- modalities[[n + 1]]
      bias_mris[[modality]] <- list_corregister[[1]]
    }
  }else{
    bias_mris$T2 <- list_corregister
  }
  return(bias_mris)
}


#' @title Create a vector of strings with the MRI modalities available in a patient's folder.
#'
#' @description This function creates a vector with the name of the image modalitities for a patient.
#' @param patient paths of MRI scans per patient.
#' @return vector of strings containing the modalities available for a patient.
#' @export
get_modalities <- function(patient){
  names <- names(patient)
  if ("T1" %in% names){
    modalities <- names
  }else{
    stop('Preprocessing can not be performed without a T1-weighted scan. Please make sure your folders contains a T1-w image.')
  }
  return(modalities)
}
