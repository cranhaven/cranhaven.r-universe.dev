#' mnreadR: An R package for analyzing MNREAD data
#'
#' mnreadR provides simple functions to estimate the four MNREAD parameters:
#'  \itemize{
#'   \item \strong{Maximum Reading Speed} (MRS) 
#'   -> can be estimated with the standard method: alone with \code{\link{curveParam_RT}} and \code{\link{curveParam_RS}} 
#'   or simultaneously with the other MNREAD parameters with \code{\link{mnreadParam}}. 
#'   -> Alternatively, it can be estimated with NLME modeling using \code{\link{nlmeParam}}.
#'   \item \strong{Critical Print Size} (CPS) 
#'   -> can be estimated with the standard method: alone with \code{\link{curveParam_RT}} and \code{\link{curveParam_RS}} 
#'   or simultaneously with the other MNREAD parameters with \code{\link{mnreadParam}}.
#'   -> Alternatively, it can be estimated with NLME modeling using \code{\link{nlmeParam}}.
#'   \item \strong{Reading Acuity} (RA) 
#'   -> can be estimated alone with \code{\link{readingAcuity}} 
#'   or simultaneously with the other MNREAD parameters with \code{\link{mnreadParam}}.
#'   \item \strong{Reading ACCessibility Index} (ACC) 
#'   -> can be estimated alone with \code{\link{accIndex}} 
#'   or simultaneously with the other MNREAD parameters with \code{\link{mnreadParam}}.
#'   }
#'   
#' mnreadR also provides functions for graphical display:
#'  \itemize{
#'   \item Raw data can be plotted with \code{\link{mnreadCurve}} 
#'   \item Estimates from the NLME fit can be plotted with \code{\link{nlmeCurve}}
#'   }
#'
#' @section Notes
#' Feel free to email me at \email{aurelie.calabrese@univ-amu.fr} if you have any questions or spot any bugs! 
#'
#' @section Contact
#' Aur\'elie Calabr\`ese - \email{aurelie.calabrese@univ-amu.fr}
#'
#' @docType package
#' @name mnreadR
NULL
#> NULL
