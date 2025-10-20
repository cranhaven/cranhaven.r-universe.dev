#' @title Plotting Upper Bounds on Relative and Attributable Risk
#'
#' @description Plots upper bounds on relative and attributable risk
#'
#' @param results estimation results from either cicc_RR or cicc_AR
#' @param parameter 'RR' for relative risk; 'AR' for attributable risk (default =  'RR')
#' @param sampling 'cc' for case-control sampling; 'cp' for case-population sampling (default =  'cc')
#' @param save_plots TRUE if the plots are saved as pdf files; FALSE if not (default = FALSE)
#' @param file_name the pdf file name to save the plots (default = Sys.Date())
#' @param plots_ctl value to determine the topleft position of the legend in the figure
#' a large value makes the legend far away from the confidence intervals (default = 0.3)
#'
#' @return A X-Y plot where the X axis shows the range of p from 0 to p_upper and
#' the Y axis depicts both point estimates and the upper end point of the one-sided confidence intervals.
#'
#' @examples
#' # use the ACS_CC dataset included in the package.
#'   y = ciccr::ACS_CC$topincome
#'   t = ciccr::ACS_CC$baplus
#'   x = ciccr::ACS_CC$age
#'   results = cicc_RR(y, t, x)
#'   cicc_plot(results)
#'
#' @references Jun, S.J. and Lee, S. (2020). Causal Inference under Outcome-Based Sampling with Monotonicity Assumptions.
#' \url{https://arxiv.org/abs/2004.08318}.
#'
#' @export
cicc_plot = function(results, parameter = 'RR', sampling = 'cc',
                     save_plots = FALSE, file_name = Sys.Date(), plots_ctl = 0.3){

  # Check whether parameter is either RR or AR
  if ( sum( !(parameter %in% c('RR','AR')) ) > 0 ){
    stop("'parameter' must be either 'RR' or 'AR'.")
  }

  # Check whether sampling is case-control, case-population, or random
  if ( sum( !(sampling %in% c('cc','cp','rs')) ) > 0 ){
    stop("'sampling' must be 'cc', 'cp', or 'rs'.")
  }

  if (parameter == 'RR'){

  xi = exp(results$est)
  xi_ci = exp(results$ci)
  pseq = results$pseq
  cov_prob = results$cov_prob

  pdf_file_name = paste(file_name,"RR.pdf",sep="-")
  ylab_name = "Relative Risk"

    if (sum(is.na(xi_ci) == TRUE) == 0){

        ylim_value = c(min(xi),(max(xi_ci)+plots_ctl*(max(xi_ci)-min(xi))))
        legend_title = c(expression=paste("Estimates of the Upper Bounds on Relative Risk"),
                         paste(cov_prob*100,"% One-Sided Uniform Confidence Band",sep=""))
    } else {

        ylim_value = c(min(xi),(1+plots_ctl)*max(xi))
        legend_title = c(expression=paste("Estimates of the Upper Bounds on Relative Risk"))

    }
  }

  if (parameter == 'AR'){

  xi = results$est
  xi_ci = results$ci
  pseq = results$pseq
  cov_prob = results$cov_prob

  pdf_file_name = paste(file_name,"AR.pdf",sep="-")
  ylab_name = "Attributable Risk"


      if (sum(is.na(xi_ci) == TRUE) == 0){

        ylim_value = c(min(xi),(max(xi_ci)+plots_ctl*(max(xi_ci)-min(xi))))
        legend_title = c(expression=paste("Estimates of the Upper Bounds on Attributable Risk"),
                          paste(cov_prob*100,"% One-Sided Pointwise Confidence Interval",sep=""))
      } else {

        ylim_value = c(min(xi),(1+plots_ctl)*max(xi))
        legend_title = c(expression=paste("Estimates of the Upper Bounds on Attributable Risk"))
      }

  }



  xlab_name = "Unknown True Case Probability"
  xlim_value = c(0,max(pseq))

  if (save_plots == TRUE){
    grDevices::pdf(pdf_file_name)
  }

  graphics::plot(pseq, xi, type = "l", lty = "solid", col = "blue",
                 xlab = xlab_name, ylab = ylab_name,
                 xlim = xlim_value, ylim = ylim_value)

    if (sum(is.na(xi_ci) == TRUE) == 0){

        graphics::lines(pseq, xi_ci, type = "l", lty = "dashed", col = "red")
        graphics::legend("topleft", legend_title, lty =c("solid", "dashed"), col = c("blue", "red"))
    } else {
        graphics::legend("topleft", legend_title, lty =c("solid"), col = c("blue"))

    }
  if (save_plots == TRUE){
    grDevices::dev.off()
  }

}
