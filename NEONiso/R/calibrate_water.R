#' Calibrate NEON water isotope ratios using validation data sets.
#'
#' `r lifecycle::badge("experimental")`
#' This function uses NEON validation data to apply drift corrections to
#' measured ambient water isotope ratios. In brief, ambient water isotope
#' ratios are calibrated by generating regressions using reference water
#' measurements bracketing an ambient period. Three reference waters are
#' measured once per day, with several injections per reference water.
#' Due to memory effects, only the last three are used currently to generate
#' calibration equations. Regressions between measured d18O and d2H values
#' and NEON-provisioned known reference values are generated, and used to
#' calibrate the period of ambient measurements between them if the r2 of
#' the regression is greater than a threshold value (by default, this is 0.95).
#' Most of this function deals with selecting the appropriate calibration data
#' and determining calibration quality. This function also contains a wrapper
#' for `calibrate_ambient_water_linreg`, which calibrates the ambient
#' water data using the calibration parameters generated in this function.
#' This function also copies over data in the qfqm and ucrt hdf5 data groups.
#'
#' *IMPORTANT NOTE* Currently this function does not apply a correction for
#' humidity dependence of Picarro isotopic measurements. This is because the
#' data to implement these corrections is not yet publicly available.
#' Caution is suggested when analyzing data at low humidities, below ~5000 ppm,
#' with likely higher biases at lower humidity values.
#'
#' Additionally, please note that this function is meant to work on *all* files
#' for a given site at the same time. A more flexible version that can handle
#' all files or monthly files will be added to a future release.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param site Four-letter NEON code for site being processed.
#' @param inname Input file(s) that are to be calibrated. If a single file is
#'               given, output will be a single file per site per month. If a
#'               list of files corresponding to a timeseries at a given site
#'               is provided, will calibrate the whole time series.
#' @param outname Name of the output file. (character)
#' @param force_cal_to_beginning Extend first calibration to
#'                               the beginning of the file?
#' @param force_cal_to_end Extend last calibration to the end of the file?
#' @param r2_thres Minimum r2 threshold of an "acceptable" calibration. Acts to
#'            remove calibration periods where a measurement error makes
#'            relationship nonlinear. Default = 0.95
#' @param filter_data Apply median absolute deviation filter from Brock 86 to
#'             remove impulse spikes?
#' @param calibration_half_width Determines the range of standard measurements
#'             to use in determining the calibration regression dataset. Creates
#'             a moving window that is `2*calibration_half_width` days wide.
#'             Default is set to 14 for a 28 day moving window.
#' @param slope_tolerance How different from 1 should we allow
#'             'passing' regression slopes to be? Experimental parameter,
#'              off by default (e.g., default slope parameter = 9999)
#' @param correct_ref_data There are a few instances where the reference d18O
#'              and d2H values may have been switched, causing very anomalous
#'              d-excess values. If TRUE, implement a switch that corrects this
#'              issue.
#' @param write_to_file Write calibrated ambient data to file?
#'              (Mostly used for testing)
#'
#' @return nothing to the workspace, but creates a new output file of
#'         calibrated water isotope data.
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
#' @importFrom utils tail
#' @import dplyr
#' @import neonUtilities
#' @importFrom data.table rleidv
calibrate_water       <- function(inname,
                                  outname,
                                  site,
                                  calibration_half_width = 14, # days
                                  filter_data = TRUE,
                                  force_cal_to_beginning = FALSE,
                                  force_cal_to_end = FALSE,
                                  r2_thres = 0.95,
                                  slope_tolerance = 9999,
                                  correct_ref_data = TRUE,
                                  write_to_file = TRUE) {


  wiso <- ingest_data(inname, analyte = "H2o", amb_avg = 9, ref_avg = 3)

  refe <- extract_water_calibration_data(wiso$refe_stacked)

  if (correct_ref_data) {
    # add fix for NEON standard swap.
    refe <- swap_standard_isotoperatios(refe)
  }

  cal_df <- fit_water_regression(ref_data = refe,
                                 calibration_half_width = calibration_half_width,
                                 slope_tolerance = slope_tolerance,
                                 r2_thres = r2_thres,
                                 site = site)

  #=======================================================================
  # apply calibration routines
  #=======================================================================

  wiso_subset <- c(wiso$ambient, wiso$reference)

  wiso_subset_cal <- lapply(names(wiso_subset),
                            function(x) {
                              calibrate_ambient_water_linreg(
                                amb_data_list = wiso_subset[[x]],
                                caldf = cal_df,
                                site = site,
                                force_to_end = force_cal_to_end,
                                force_to_beginning = force_cal_to_beginning,
                                filter_data = filter_data,
                                r2_thres = r2_thres)
                            })

  names(wiso_subset_cal) <- names(wiso_subset)

  #-----------------------------------
  # (optionally) write out to new file
  #-----------------------------------
  if (write_to_file) {
    cal_df$timeBgn <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeBgn)
    cal_df$timeEnd <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeEnd)
    fid <- setup_output_file(inname, outname, site, analyte = "h2o",
                              attrs = wiso$attrs, keep_open = TRUE)
    write_water_calibration_data(outname, site, cal_df, fid = fid)
    write_water_ambient_data(outname, site, wiso_subset_cal, fid = fid)
    h5_close(fid)

    validate_output_file(inname, outname, site, "h2o")

  } else {
    out_data <- list()
    #convert time to NEON HDF5 time
    cal_df$timeBgn <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeBgn)
    cal_df$timeEnd <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeEnd)
    out_data$wiso_subset_cal <- wiso_subset_cal
    out_data$cal_df <- cal_df

    return(out_data)
  }
}
