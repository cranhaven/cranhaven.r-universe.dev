
#' @title Shiny app of drop-the-losers (DTL) design 
#' 
#' @description Interactive shiny app of drop-the-losers (DTL) design 
#' 
#' @param appname Default is "shiny". Do not change it since there is only 
#'                one shiny app in the package
#' 
#' @return The shiny app of DTL design which includes three panels:(1) family-wise
#'         type I error rate (FWER) given fixed rho; (2) correlation coefficient 
#'         boundary for rho (rho_s); (3) adjusted significance level in real 
#'         application (alpha_t). The first two panels show the graphs of change 
#'         of the FWER and the rho_s as the change of some related parameters. The 
#'         corresponding tables of the graphs are also shown. In the last 
#'         panel, the table of the significance levels alpha_s based on all possible
#'         values of response rate q and hazard ratio of responders and 
#'         non-responders gamma and the resulting minimum or called adjusted 
#'         significance level are shown.
#' 
#' @examples
#' # run dtl_shiny()
#' 
#' 
#' @export
dtl_shiny <- function(appname = "shiny") {

    appDir <- system.file(appname, package = "dtlcor")
    if (appDir == "") {
        stop("Could not find Shiny directory. Please try re-installing 'dtlcor'.",
             call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal");
}
