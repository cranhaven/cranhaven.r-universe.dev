# quality control - functions to validate the output data file structure

#' Standardize analyte names
#'
#' Enforces standard capitalization and formatting of H2o and Co2
#' analyte names across calibration functions.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#' @param analyte Co2 or H2o?
#'
#' @return Standardized string for the water ('H2o') or
#'         carbon ('Co2') systems to make sure strings
#'         are standardized across package functions.
#'
#'
validate_analyte <- function(analyte) {
  # helper function to make sure the various output functions are consistent.
  # check to make sure first letter of analyte is capitalized,
  # or capitalize if it's not (also make sure it's co2 or h2o)
  if (analyte == "co2" || analyte == "h2o") {
    analyte <- paste0(toupper(substring(analyte, 1, 1)), substring(analyte, 2))
  } else if (analyte != "Co2" && analyte != "H2o") {
    stop("Invalid analyte selected in setup output file.")
  }

  return(analyte)
}


#' Validate output file.
#'
#' Function ensures that the output file has the correct
#' groups in it, as a check to ensure proper file structure at the
#' end of the calibration routines.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param analyte Carbon ('Co2') or water ('H2o') system?
#'
#' @return Nothing to environment, simply checks to make sure expected groups
#'         are in output.
#'
validate_output_file <- function(inname, outname, site, analyte) {

  analyte <- validate_analyte(analyte)

  # retrieve children of the target group from input and output files
  target_path <- paste0(site, "/dp01/data/iso", analyte)
  target_in <- h5_ls_group(inname[[1]], target_path)
  target_out <- h5_ls_group(outname, target_path)

  if (analyte == "Co2") {
    #only care about the 9m vars!
    target_in <- target_in[grepl("09m", target_in) &
                             !grepl("Arch", target_in)]
  } else {
    #only care about the 3m and 9m vars!
    target_in <- target_in[(grepl("03m", target_in) |
                              grepl("09m", target_in)) &
                             !grepl("Arch", target_in)]
  }

  # add calData to target_in, since we expect this to be added (and adding to
  # target_in confirms that it remains in target_out!)
  target_in <- c(target_in, "calData")

  # order both vectors
  target_in  <- sort(target_in)
  target_out <- sort(target_out)

  # check to see if equal
  test_result <- identical(target_in, target_out)

  # throw error (warning?) if not identical
  if (!test_result) {
    print(c(target_in, target_out))
    stop("Output file structure has diverged from input file!")
  }
}

#--------------------------------------------------------------------
# Diagnostic plots:::

#' Make plots of carbon calibration data for debugging
#'
#' Makes plots of carbon calibration data regressions, primarily for
#' debugging and validation purposes.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param caldata Data frame corresponding to a specific calibration
#'                period.
#' @param plot_filename What should the output file name for diagnostic plot be?
#' @param method Which method are we using? Currently works for gain/offset.
#' @param mtitle Fed from above routine - what should the plot title be?
#'
#' @return Nothing to the environment, but a pdf plot to a file.
#'
#' @import ggplot2
carbon_regression_plots <- function(caldata, plot_filename, method, mtitle) {

  if (method == "Bowling_2003") {
    p1 <- ggplot2::ggplot(data = caldata,
                          ggplot2::aes(x = .data$conc12CCO2_obs,
                                       y = .data$conc12CCO2_ref)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm")

    p2 <- ggplot2::ggplot(data = caldata,
                          ggplot2::aes(x = .data$conc13CCO2_obs,
                                       y = .data$conc13CCO2_ref)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm")

    grDevices::pdf(plot_filename, height = 8, width = 12)
    gridExtra::grid.arrange(p1, p2, nrow = 1, top = grid::textGrob(mtitle))
    grDevices::dev.off()
  }

}
