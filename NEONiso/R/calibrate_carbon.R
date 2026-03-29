#' Calibrate NEON carbon isotope data using validation data sets.
#'
#' `r lifecycle::badge("experimental")`
#' This function drives a workflow that reads in NEON carbon isotope data
#' of atmospheric CO2, calibrates it to the VPDB scale, and (optionally)
#' writes the calibrated data to a new HDF5 file. Two different approaches
#' are possible: a) a calibration on 12CO2 and 13CO2 isotopologues
#' independently, after Bowling et al. 2003 (Agr. For. Met.), or b) a direct
#' calibration of d13C and CO2 values using linear regression. Most of the time
#' the results generated are extremely similar to each other.
#' Wen et al. 2013 compared several different carbon
#' isotope calibration techniques and found this to be the superior method
#' under most circumstances. We also found this to be the case for NEON data
#' (Fiorella et al. 2021; JGR-Biogeosciences).
#'
#' The 'linreg' method simply takes measured and reference d13C and CO2 values
#' and generates a transfer function between them using `lm()`. For the
#' gain-and-offset method, d13C and CO2 values are converted to 12CO2 and 13CO2
#' mole fractions. Gain and offset parameters are calculated for each
#' isotopologue independently, and are analogous to regression slope and
#' intercepts, but jointly correct for CO2 concentration dependence
#' and place d13C values on the VPDB scale.
#' The gain and offset parameters are defined by:
#'
#' \deqn{G = (X_{2,ref}-X_{1,ref})/(X_{2,meas}-X_{1,meas})}
#' \deqn{O = X_{2,ref}- G X_{2,meas}}
#' Calibrated ambient isotopologues are then given as:
#' \deqn{X_{cal} = X_{meas} G + O}
#'
#' Measurements of reference materials were considered "good" if the following
#' conditions were met:
#' \itemize{
#'   \item Measured CO2 concentrations were within 10 ppm
#'         of known "reference" concentrations.
#'   \item Variance of the CO2 concentration in standard peak was < 5 ppm.
#'   \item Measured d13C value must be within 5 per mil
#'         of known "reference" d13C value.
#' }
#' The first two criteria are intended to filter out periods where there is
#' a clear issue with the gas delivery system (i.e., nearly empty gas tank,
#' problem with a valve in the manifold, etc.); the third criterion was adopted
#' after visual inspection of data timeseries revealed that often the first
#' standard measurement following an instrument issue had higher-than-expected
#' error. This criterion clips clearly poor values. Selection of these criteria
#' will become a function argument, and therefore customizable,
#' in a future release.
#'
#' The behavior of this function will be a bit different depending on what
#' is supplied as `inname`. If a single file is provided, the output will be
#' monthly. However, a list of files corresponding to a site can also be
#' provided, and then a single output file per site will be generated.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file(s) that are to be calibrated. If a single file is
#'               given, output will be a single file per site per month. If a
#'               list of files corresponding to a timeseries at a given site
#'               is provided, will calibrate the whole time series.
#' @param outname Name of the output file. (character)
#' @param force_cal_to_beginning Extend first calibration to the beginning
#'             of the file? (default true)
#' @param force_cal_to_end Extend last calibration to the end of the file?
#'             (default true)
#' @param site Four letter NEON site code for site being processed. (character)
#' @param gap_fill_parameters Should function attempt to 'gap-fill' across a
#'            bad calibration by carrying the last good calibration forward?
#'            Implementation is fairly primitive currently, as it only carries
#'            the last known good calibration that's available forward rather
#'            than interpolating, etc. Default FALSE.
#' @param filter_ambient Apply the median absolute deviation filter (Brock 86)
#'            to remove impulse spikes in output ambient data?
#'            (logical; default true)
#' @param r2_thres Minimum r2 threshold of an "acceptable" calibration. Acts to
#'            remove calibration periods where a measurement error makes
#'            relationship nonlinear. Default = 0.95
#' @param correct_ref_data NEON has indicated there are a few instances where
#'            reported d13C or CO2 reference values are wrong. If set to true,
#'            correct known incorrect values. This argument will (hopefully,
#'            eventually) go away after NEON has fixed the reference database.
#'            Users will be warned prior to removal of this argument.
#' @param write_to_file Write calibrated ambient data to file?
#'              (Mostly used for testing)
#' @param method Are we using the Bowling et al. 2003 method
#'              ("Bowling_2003") or direct linear regression of
#'              d13C and CO2 mole fractions ("linreg")?
#' @param calibration_half_width Determines the period (in days)
#'        from which reference data are selected (period
#'        is 2*calibration_half_width).
#' @param remove_known_bad_months There are a few site months with known
#'        spectral issues where the isotope ratios are likely unrecoverable.
#'        This parameter allows removal of these files, but allows them to
#'        remain in archive.
#' @param plot_regression_data Default false; this is useful for diagnostics.
#' @param plot_directory Only used if plot_regression_data is TRUE, but specify
#'        where to write out diagnostic plot of regression data.
#' @param avg The averaging interval to extract, in minutes. Default 6.
#' @param min_nobs Minimum number of high-frequency observations to define
#'                 a peak.
#' @param standards Which reference gases (standards) to use? Default is all,
#'        but can pass a subset of "co2Low", "co2Med", and "co2High" as a vector
#'        to this argument as well.
#'
#'
#' @return Returns nothing to the environment, but creates a new output HDF5
#'         file containing calibrated carbon isotope values.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom utils str
#' @examples
#' \dontrun{fin <- system.file('extdata',
#' 'NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.20201020T211037Z.packed.h5',
#'          package = 'NEONiso', mustWork = TRUE)
#' calibrate_carbon_bymonth(inname = fin, outname = 'out.h5',
#'          site = 'ONAQ', write_to_file = FALSE)
#' calibrate_carbon_bymonth(inname = fin, outname = 'out.h5',
#'          site = 'ONAQ', method = 'linreg', write_to_file = FALSE)}
#'
calibrate_carbon         <- function(inname,
                                     outname,
                                     site,
                                     method = "Bowling_2003",
                                     calibration_half_width = 0.5,
                                     force_cal_to_beginning = TRUE,
                                     force_cal_to_end = TRUE,
                                     gap_fill_parameters = FALSE,
                                     filter_ambient = TRUE,
                                     r2_thres = 0.95,
                                     correct_ref_data = TRUE,
                                     write_to_file = TRUE,
                                     remove_known_bad_months = TRUE,
                                     plot_regression_data = FALSE,
                                     plot_directory = NULL,
                                     avg = 6,
                                     min_nobs = NA,
                                     standards = c("co2Low",
                                                   "co2Med",
                                                   "co2High")) {

  if (remove_known_bad_months) {
    if (site == "UNDE") {
      inname <- inname[!grepl("2019-05|2019-06|2019-07|2019-08|2019-09",
                              inname)]
    } else if (site == "TEAK") {
      inname <- inname[!grepl("2018-08|2018-09", inname)]
    } else if (site == "SRER") {
      inname <- inname[!grepl("2019-07", inname)]
    }
  }

  if (method == "Bowling_2003") {
    lifecycle::deprecate_warn("0.7.1",
                              "calibrate_carbon(method = 'Bowling_2003')",
                              "calibrate_carbon(method = 'gainoffset')")
  }

  #-----------------------------------------------------------
  # Extract reference data from input HDF5 file.
  #-----------------------------------------------------------

  ciso <- ingest_data(inname,
                      analyte = "Co2",
                      amb_avg = avg,
                      ref_avg = avg)

  # extract the data we need from ciso list
  refe <-  extract_carbon_cal_data(ciso$refe_stacked,
                                   standards = standards)

  # Okay this function now needs some work. *************
  if (correct_ref_data == TRUE) {

    # do some work to correct the reference data frame
    refe <- correct_carbon_ref_cval(refe, site)

    tmp_names <- names(ciso$reference)

    #apply seems to strip names from ciso$reference, so need to save above
    # and reassign below.
    ciso$reference <- lapply(names(ciso$reference),
                             function(x) {
                               ciso$reference[[x]] <- correct_carbon_ref_output(
                                ciso$reference[[x]], site = site, ref_gas = x)
                             })

    names(ciso$reference) <- tmp_names
  }

  # get calibration parameters data.frame.
  cal_df <- fit_carbon_regression(ref_data = refe, method = method,
                                  calibration_half_width = calibration_half_width,
                                  plot_regression_data = plot_regression_data,
                                  plot_dir = plot_directory,
                                  site = site,
                                  min_nobs = min_nobs)

  #----------------------------------------------------------------------------
  #  calibrate ambient data.
  #  extract ambient measurements from ciso

  ciso_subset <- c(ciso$ambient, ciso$reference)

  if (method == "gainoffset" | method == "Bowling_2003") {

    ciso_subset_cal <-
      lapply(names(ciso_subset),
             function(x) {
               calibrate_ambient_carbon_gainoffset(amb_data_list = ciso_subset[[x]],
                                                   caldf = cal_df,
                                                   site = site,
                                                   filter_data = filter_ambient,
                                                   force_to_end = force_cal_to_end,
                                                   force_to_beginning = force_cal_to_beginning,
                                                   r2_thres = r2_thres)
             })

  } else if (method == "linreg") {

    ciso_subset_cal <-
      lapply(names(ciso_subset),
             function(x) {
               calibrate_ambient_carbon_linreg(amb_data_list = ciso_subset[[x]],
                                               caldf = cal_df,
                                               site = site,
                                               filter_data = filter_ambient,
                                               force_to_end = force_cal_to_end,
                                               force_to_beginning = force_cal_to_beginning,
                                               r2_thres = r2_thres)
             })
  }

  names(ciso_subset_cal) <- names(ciso_subset)
  #-----------------------------------------------------------
  # write out these data.frames to a new output file.
  #-----------------------------------------------------------
  if (write_to_file) {
    cal_df$timeBgn <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeBgn)
    cal_df$timeEnd <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeEnd)
    fid <- setup_output_file(inname, outname, site, "co2",
                              attrs = ciso$attrs, keep_open = TRUE)
    write_carbon_calibration_data(outname, site, cal_df, method = method,
                                  fid = fid)
    write_carbon_ambient_data(outname, site, ciso_subset_cal, fid = fid)
    h5_close(fid)

    validate_output_file(inname, outname, site, "co2")
  } else { #export output directly
    out_data <- list()
    #convert time to NEON HDF5 time
    cal_df$timeBgn <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeBgn)
    cal_df$timeEnd <- convert_POSIXct_to_NEONhdf5_time(cal_df$timeEnd)
    out_data$ciso_subset_cal <- ciso_subset_cal
    out_data$cal_df <- cal_df

    return(out_data)

  }

}
