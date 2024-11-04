#' @title Load MRI scans per patient
#'
#' @description This function loads the MRI scans from a patient. It assumes that the MRI
#' scans are contained in the same folder and refer to MRI modalities T1-weighted, T2-weighted and FLAIR.
#' Only the first MRI scan that matched the modalities keywords ('T1','T2' or 'FLAIR') will be kept.
#'
#' @param folder folder containing the MRI scans. The MRI scans should be in format NiFTI.
#' @param modalities string or vector of strings with the MRI modalities to be considered. Should be at least one of T1, T2 or FLAIR. By default, all modalities are searched within the folder.
#' @return paths of MRI scans for a patient if they exist.
#' @export
load_mri_patient <- function(folder, modalities = c('T1','T2','FLAIR')){
  folder = file.path(folder)
  mri_images <- list()
  for (modality in modalities){
    data <- list.files(folder, pattern = modality, full.names = TRUE)
    if (length(data) > 0){
      mri_images[[modality]] <- data[1]
      message(paste0('-',modality, ' image found\n'))
    }else{
      warning(paste0('- *NO* ', modality, ' images found\n'))
    }
  }
  return(mri_images)
}


#' @title Load MRI per group or disease
#'
#' @description This function loads the MRI scans from multiple patients. It assumes that the patients'
#' folders containing the MRI scans are sub-folders of a general folder.
#'
#' @param folder general folder containing the sub-folders with the MRI scans.
#' @return paths of MRI scans per patient if they exist.
#' @export
load_mri_group <- function(folder){
  folders <- list.dirs(folder, full.names = TRUE, recursive = FALSE)
  mri_images <- list()
  message('--------------------------------------------------\n')
  for (fold in folders){
    fold_name = unlist(strsplit(fold, '/'))
    fold_name = fold_name[length(fold_name)]
    message(paste0('Reading folder ', fold_name))
    mri_patient <- load_mri_patient(fold)
    if (length(mri_patient) > 0){
      mri_images[[fold_name]] <- mri_patient
    }
  }
  if(length(mri_images) == 0){
    stop("MRI scans must be provided.")
  }
  message('--------------------------------------------------\n')
  return(mri_images)
}
