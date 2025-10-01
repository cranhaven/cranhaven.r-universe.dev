#----- nlmeParam --------
#######################--

#' Maximum Reading Speed (MRS) and Critical Print Size (CPS) estimation using a nonlinear mixed-effect (NLME) modeling. 
#'
#' This function uses the NLME model created from \code{\link{nlmeModel}} to extract the following MNREAD parameters:
#'  \itemize{
#'   \item Maximum Reading Speed (MRS)
#'   \item Critical Print Size (CPS)
#'   }
#'
#' @param nlme.model The object returned by \code{\link{nlmeModel}}
#' @param CPScriterion Optional argument to specify a criterion for CPS estimation. The default criterion value is '90 of MRS'. This criterion can vary from 75 to 95 of MRS and should only be modified for specific purposes, as discussed in Cheung et al. 2008 
#' 
#' 
#' @return The function returns a new dataframe with two variables:
#'  \itemize{
#'   \item "CPS" -> contains the Critical Print Size estimate (in logMAR)
#'   \item "MRS" -> contains the Maximum Reading Speed estimate (in words/min)
#'   }
#'
#' @section Notes:
#' To ensure proper estimation of the MRS and CPS, individual MNREAD fit should be plotted using \code{\link{nlmeCurve}} and inspected visually.
#' If some of the estimated values of MRS and CPS seem off given the actual data, we advise you to run \code{\link{mnreadCurve}} 
#' and overwrite these estimates with values estimated visually from the actual MNREAD curve.
#' 
#' For more details on the nlme fit, see:\\
#' Cheung SH, Kallie CS, Legge GE, Cheong AM. Nonlinear mixed-effects modeling of MNREAD data. 
#' Invest Ophthalmol Vis Sci. 2008;49:828–835. doi: 10.1167/iovs.07-0555.
#'
#'
#' @seealso
#'  \code{\link{nlmeModel}} to fit MNREAD data using a nonlinear mixed-effect (NLME) modeling
#'
#'  \code{\link{nlmeCurve}} to plot the individual MNREAD curves estimated from the NLME model
#'  
#'  \code{\link{curveParam_RT}} for standard estimation of MRS and CPS 
#'
#'  \code{\link{mnreadParam}} for all MNREAD parameters estimation
#'
#'
#'
#' @examples 
#' # inspect the structure of the dataframe
#' head(data_low_vision, 10)
#'
#' #------
#' 
#' # restrict dataset to one MNREAD test per subject (regular polarity only)
#' data_regular <- data_low_vision %>%
#'     filter (polarity == "regular")
#'
#' # run the NLME model for data grouped by subject
#' \donttest{ nlme_model <- nlmeModel(data_regular, ps, vd, rt, err, subject) }
#'
#' #------
#'
#' # run the parameters' estimation for a default CPS criterion of '90 of MRS' 
#' \donttest{ nlmeParam(nlme_model) }
#' 
#' # run the parameters' estimation for a specific CPS criterion of '80 of MRS'
#' \donttest{ nlmeParam(nlme_model, 0.8) }
#' 
#'
#' 
#' @importFrom stats sd coef predict
#' @importFrom tibble rownames_to_column
#' @import dplyr
#' 
#' 
#'
#' @export
nlmeParam <- function(nlme.model, CPScriterion = NULL) {
  # This function estimates the Maximum reading Speed (MRS) and Critical Print Size (CPS) from the NLME model estimated with nlmeModel().

  asym <- NULL
  lrc <- NULL
  x_intercept <- NULL
  grp_var <- NULL
  rowname <- NULL
  MRS <- NULL
  CPS <- NULL
  subject <- NULL
  nested_var <- NULL
  . <- NULL

  message('Remember to check the MRS and CPS estimates visually by inspecting the NLME fit with nlmeCurve()')
  
  # extract coefficients from the nlme model
  my.coef <- rownames_to_column(as.data.frame(coef(nlme.model[[2]])))

  # set the percentage of maximum reading speed we want to use
  # CPS is the smallest print size that yields p times the maximum reading speed
  if ( missing(CPScriterion) )  {
    CPS_crit = 0.90 }
  else {
    CPS_crit = CPScriterion }
    
  # get Maximum Reading Speed (MRS) and Critical Print Size (CPS)
  nlme.estimates <- as.data.frame ( my.coef %>% 
    mutate (MRS = 10 ^ asym) %>%
    mutate (CPS = log((-log10(CPS_crit))/asym) / (-exp(lrc)) + x_intercept) %>%
    separate(rowname, into = c("subject", "nested_var"), sep = "/", fill = "right") %>%
    select (subject, nested_var, MRS, CPS) %>%
    select_if(~!all(is.na(.))) )
  
  return(nlme.estimates)
  
}
