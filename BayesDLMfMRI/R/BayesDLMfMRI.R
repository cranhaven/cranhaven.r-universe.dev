#' Bayesian Matrix-Variate Dynamic Linear Models for Task-based fMRI Modeling in R
#'
#' The 'BayesDLMfMRI' package performs statistical analysis for task-based functional magnetic resonance imaging (fMRI) data at both individual and group levels. The analysis to detect brain activation at the individual level is based on modeling the fMRI signal using Matrix-Variate Dynamic Linear Models (MDLM). The analysis for the group stage is based on posterior distributions of the state parameter obtained from the modeling at the individual level. In this way, this package offers several R functions with different algorithms to perform inference on the state parameter to assess brain activation for both individual and group stages. Those functions allow for parallel computation when the analysis is performed for the entire brain as well as analysis at specific voxels when it is required.
#' 
#' @section Authors:
#' Maintainer: Carlos Peréz \email{caaperezag@unal.edu.co} \cr
#' Creator: Johnatan Cardona-Jiménez \email{jcardonj@unal.edu.co} \cr
#' Contributor: Isabel Ramírez \email{iscramirezgu@unal.edu.co} 
#' @docType package
#' @name BayesDLMfMRI
#' @aliases BayesDLMfMRI-package
#' @useDynLib BayesDLMfMRI , .registration=TRUE
#' @exportPattern "^[[:alpha:]]+"
#' @import mathjaxr
#' @import stats
#' @import utils
NULL
#> NULL
#' @title Covariates related to the observed BOLD response
#' @description
#' Covariates related to the observed BOLD response and its derivative used in the examples presented in the vignettes.
#' @examples
#' data("covariates", package="BayesDLMfMRI")
"Covariates"

#' @title MNI image used to plot posterior probability maps in the vignette examples.
#' @description MNI image used to plot posterior probability maps in the examples presented in the vignettes.
#' @examples
#' data("ffd", package="BayesDLMfMRI")
"ffd"

#' @title A 3D array that works as a brain of reference (MNI atlas).
#' @description A 3D array that works as a brain of reference (MNI atlas) for the group analysis.
#' @examples
#' data("mask", package="BayesDLMfMRI")
"mask"

#' @name summary.fMRI_group_evidence
#' @title summary.fMRI_group_evidence
#' @description
#' Summary function
#' @details
#' Summary function
#' @param object is the returned value of any of the fdGroupEvidence* functions
#' @param ... Other potential arguments
#' @examples
#'\dontrun{
#' DatabaseGroup <- get_example_fMRI_data_group()
#' data("covariates", package="BayesDLMfMRI")
#' data("mask", package="BayesDLMfMRI")
#' res <- ffdGroupEvidenceFETS(ffdGroup = DatabaseGroup, covariates = Covariates, 
#'                             m0 = 0, Cova = 100, delta = 0.95, S0 = 1, 
#'                             n0 = 1, N1 = FALSE, Nsimu1 = 100, Cutpos=30, 
#'                             r1 = 1, Test = "JointTest", mask = mask, Ncores = 7)
#' summary(res)
#' }
#' @export
summary.fMRI_group_evidence  <- function(object, ...) {
    str(object)
}

#' @name print.fMRI_group_evidence
#' @title print.fMRI_group_evidence
#' @description
#' Print the structure of the object related to the ffdGroupEvidence* functions.
#' @details
#' Print the structure of the object related to the ffdGroupEvidence* functions.
#' @param x is the returned value of any of the ffdGroupEvidence* functions
#' @param ... Other potential arguments
#' @examples
#'\dontrun{
#' DatabaseGroup <- get_example_fMRI_data_group()
#' data("covariates", package="BayesDLMfMRI")
#' data("mask", package="BayesDLMfMRI")
#' res <- ffdGroupEvidenceFETS(ffdGroup = DatabaseGroup, covariates = Covariates, 
#'                             m0 = 0, Cova = 100, delta = 0.95, S0 = 1, 
#'                             n0 = 1, N1 = FALSE, Nsimu1 = 100, Cutpos=30, 
#'                             r1 = 1, Test = "JointTest", mask = mask, Ncores = 7)
#' print(res)
#' }
#' @export
print.fMRI_group_evidence  <- function(x, ...) {
    str(x)
}

#' @name summary.fMRI_single_evidence
#' @title summary.fMRI_single_evidence
#' @description
#' Summary function
#' @details
#' Summary function
#' @param object is the returned value of any of the ffdEvidence* functions
#' @param ... Other potential arguments
#' @examples
#'\dontrun{
#' fMRI.data  <- get_example_fMRI_data()
#' data("covariates", package="BayesDLMfMRI")
#' res <- ffdEvidenceFFBS(ffdc = fMRI.data, covariates = Covariates, m0=0, Cova=100,
#'                        delta=0.95, S0=1, n0=1, N1=FALSE, 
#'                        Nsimu1 = 100, Cutpos1=30, r1 = 1,
#'                        perVol = 0.10, Ncores=3)
#' summary(res)
#' }
#' @export
summary.fMRI_single_evidence  <- function(object, ...) {
    str(object)
}


#' @name print.fMRI_single_evidence
#' @title print.fMRI_single_evidence
#' @description
#' Print the structure of the object related to the ffdEvidence* functions.
#' @details
#' Print the structure of the object related to the ffdEvidence* functions.
#' @param x is the returned value of any of the ffdEvidence* functions
#' @param ... Other potential arguments
#' @examples
#'\dontrun{
#' fMRI.data  <- get_example_fMRI_data()
#' data("covariates", package="BayesDLMfMRI")
#' res <- ffdEvidenceFFBS(ffdc = fMRI.data, covariates = Covariates, m0=0, Cova=100,
#'                        delta=0.95, S0=1, n0=1, N1=FALSE, 
#'                        Nsimu1 = 100, Cutpos1=30, r1 = 1,
#'                        perVol = 0.10, Ncores=3)
#' print(res)
#' }
#' @export
print.fMRI_single_evidence  <- function(x, ...) {
    str(x)
}


#' @name plot.fMRI_single_evidence
#' @title plot.fMRI_single_evidence
#' @description
#' Plot function
#' @details
#' Plot function
#' @param x is the returned value of any of the ffdEvidence* functions.
#' @param overlay MNI image used to plot posterior probability maps. 
#' @param index the element of \code{res} to be plotted.
#' @param index2 the element of \code{res} to be plotted, only used if needed.
#' @param ... additional parameters passed to the \code{ortho2} function.
#' @examples
#'\dontrun{
#' fMRI.data  <- get_example_fMRI_data()
#' data("covariates", package="BayesDLMfMRI")
#' data("ffd", package="BayesDLMfMRI") # used for overlay.
#' res <- ffdEvidenceFETS(ffdc = fMRI.data,
#'                    covariates = Covariates,
#'                    m0 = 0, Cova = 100, delta = 0.95,
#'                    S0 = 1, n0 = 1, Nsimu1 = 100, Cutpos1 = 30,
#'                    r1 = 1, Test = "LTT", Ncores = 15)
#' plot(res, overlay=ffd, index=1, col.y = heat.colors(50), 
#'      ycolorbar = TRUE, ybreaks = seq(0.95, 1, by = 0.001))
#' }
#' @export
plot.fMRI_single_evidence  <- function(x, overlay, index, index2=NULL, ...) {
  
  res <- x

  if( (index > length(res)) | (1 > index) ) {
    stop("index out of range")
  }

  res.auxi <- res[[index]]
  
  if(length(dim(res.auxi)) > length(dim(overlay))) {
    
    if(is.null(index2)) {
      stop("you must provide a second index using index2")
    }
    
    if( (index2 > (dim(res.auxi)[1]) ) | (1 > index2) ) {
      stop("index out of range")
    }
    
    res.auxi <- res.auxi[index,,,]
    
  }
  
  Z.visual.c <- oro.nifti::nifti(res.auxi, datatype=16)
  neurobase::ortho2(x=overlay, y=ifelse(Z.visual.c > 0.95, Z.visual.c, NA), ...)
}


#' @name print.fMRI_single_voxel
#' @title print.fMRI_single_voxel
#' @description
#' Print the structure of the object related to the SingleVoxel* functions.
#' @details
#' Print the structure of the object related to the SingleVoxel* functions.
#' @param x is the returned value of any of the SingleVoxel* functions,
#' @param ... Other potential arguments
#' @examples
#'\dontrun{
#' fMRI.data  <- get_example_fMRI_data()
#' data("covariates", package="BayesDLMfMRI")
#' res.indi <- SingleVoxelFSTS(posi.ffd = c(14, 56, 40), 
#'                             covariates = Covariates,
#'                             ffdc =  fMRI.data, 
#'                             m0 = 0, Cova = 100, delta = 0.95, S0 = 1, 
#'                             n0 = 1, Nsimu1 = 100, N1 = N1, Cutpos1 = 30, 
#'                             Min.vol = 0.10, r1 = 1)
#' print(res.indi)
#' }
#' @export
print.fMRI_single_voxel  <- function(x, ...) {
    str(x)
}

#' @name print.fMRI_group_single_voxel
#' @title print.fMRI_group_single_voxel
#' @description
#' Print the structure of the object related to the SingleVoxel* functions.
#' @details
#' Print the structure of the object related to the GroupSingleVoxel* functions.
#' @param x is the returned value of any of the GroupSingleVoxel* functions
#' @param ... Other potential arguments
#' @examples
#'\dontrun{
#' DatabaseGroup <- get_example_fMRI_data_group()
#' data("covariates", package="BayesDLMfMRI")
#' res <- GroupSingleVoxelFFBS(posi.ffd = c(14, 56, 40), DatabaseGroup,
#'                             covariates = Covariates, m0 = 0, Cova = 100, 
#'                             delta = 0.95, S0 = 1, n0 = 1, N1 = FALSE, 
#'                             Nsimu1 = 100, r1 = 1, Cutpos = 30)
#' print(res)
#' }
#' @export
print.fMRI_group_single_voxel  <- function(x, ...) {
    str(x)
}

