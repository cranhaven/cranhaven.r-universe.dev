#' @name get_example_fMRI_data
#' @title get_example_fMRI_data
#' @description
#' This function is used to download the example data used in the Vignettes. 
#' @references
#' \insertRef{pernet2015human}{BayesDLMfMRI}
#' 
#' \insertRef{gorgolewski2017openneuro}{BayesDLMfMRI}
#' @details
#' The data for this example is related to an fMRI experiment where a sound stimulus is presented. 
#' That experiment is intended to offer a "voice localizer" scan, which allows rapid and reliable 
#' localization of the voice-sensitive "temporal voice areas" (TVA) of the human auditory cortex
#' \insertCite{pernet2015human}{BayesDLMfMRI}. The data of this "voice localizer" scan is freely  
#' available on the online platform OpenNEURO \insertCite{gorgolewski2017openneuro}{BayesDLMfMRI}.
#' @param save_path location where the data the example data is stored.
#' @param force force the download, even if the data already exists.
#' @param subject The example subject, must be 1 or 2.
#' @return It returns an array of dimensions \code{[91, 109, 91, 310]}.
#' @examples
#'\dontrun{
#' # This example can take a long time to run.
#' fMRI.data  <- get_example_fMRI_data()
#' }
#' @export
get_example_fMRI_data <- function(save_path=NULL, force=FALSE, subject=1) {
  
  if(is.null(save_path)) {
    save_path  <- tempdir()
  }
  
  if(length(subject) > 1) {
    "To load multiple subjects use get_example_fMRI_data_group"
  }
  
  if(!(subject %in% c(1,2))) {
    stop("The subject must be 1 or 2")
  }

  if(subject == 1) {
    
    url_list <- c("https://johnatanlab.github.io/files/test_1.rds",
                  "https://johnatanlab.github.io/files/test_2.rds",
                  "https://johnatanlab.github.io/files/test_3.rds"
    )
    
  } else {
    url_list <- c("https://johnatanlab.github.io/files/test_s2_1.rds",
                  "https://johnatanlab.github.io/files/test_s2_2.rds",
                  "https://johnatanlab.github.io/files/test_s2_3.rds"
    )
  }
  
  dir.create(save_path, showWarnings = FALSE)
  
  result_list <- list()
  
  for(i in 1:length(url_list)) {
    path_1 <- file.path(save_path,paste0("s",subject,"_test_",i,".rds") )
    
    if( (!file.exists(path_1)) & (!force)) {
      download.file(url_list[i], destfile = path_1, quiet = FALSE)
    }
    
    result_list[[i]] <- readRDS(path_1)
    
  }
  
  temp <- oro.nifti::dim_
  
  fMRI.data <- abind::abind(result_list, along = 1)

  attr(fMRI.data, "dimnames") <- NULL
  fMRI.data <- unname(fMRI.data)
  
  return(fMRI.data)
}


#' @name get_example_fMRI_data_group
#' @title get_example_fMRI_data_group
#' @description
#' This function is used to download the example data used in the Vignettes. 
#' @references
#' \insertRef{pernet2015human}{BayesDLMfMRI}
#' 
#' \insertRef{gorgolewski2017openneuro}{BayesDLMfMRI}
#' @details
#' The data for this example is related to an fMRI experiment where a sound stimulus is presented. 
#' That experiment is intended to offer a "voice localizer" scan, which allows rapid and reliable 
#' localization of the voice-sensitive "temporal voice areas" (TVA) of the human auditory cortex
#' \insertCite{pernet2015human}{BayesDLMfMRI}. The data of this "voice localizer" scan is freely  
#' available on the online platform OpenNEURO \insertCite{gorgolewski2017openneuro}{BayesDLMfMRI}.
#' @param save_path location where the data the example data is stored.
#' @param force force the download, even if the data already exists.
#' @return It returns a list in which each element is an array of dimensions \code{[91, 109, 91, 310]}.
#' @examples
#'\dontrun{
#' # This example can take a long time to run.
#' DatabaseGroup <- get_example_fMRI_data_group()
#' }
#' @export
get_example_fMRI_data_group  <- function(save_path=NULL, force=FALSE) {
  
  result_list <- list()
  
  result_list[[1]] <- get_example_fMRI_data(save_path = save_path,
                                            force = force, 
                                            subject = 1)
  
  result_list[[2]] <- get_example_fMRI_data(save_path = save_path, 
                                            force = force, 
                                            subject = 2)
  return(result_list)
}