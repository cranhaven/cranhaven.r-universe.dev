# output_functions.R
# functions that write data to hdf5 output files.
#################################################
### FUNCTIONS THAT WORK FOR BOTH H2O AND CO2 ####
#################################################
#' Structure a new HDF5 file
#'
#' Creates a skeleton HDF5 file for the calibrated data,
#' essentially setting up the HDF5 groups at the /site/dp01/\{data,ucrt,qfqm\}
#' level.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param inname Input file name.
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param analyte Carbon ('Co2') or water ('H2o') system?
#' @param attrs Pre-read attributes list from the input file. If NULL
#'        (default), attributes are read from `inname`.
#' @param keep_open If TRUE, return the open file handle instead of
#'        closing it. The caller is responsible for closing via `h5_close()`.
#'
#' @return If `keep_open = TRUE`, returns the open HDF5 file handle.
#'         Otherwise nothing (creates a new data file with basic HDF5
#'         structure consistent with NEON's data files).
#'
setup_output_file <- function(inname, outname, site, analyte,
                              attrs = NULL, keep_open = FALSE) {

  analyte <- validate_analyte(analyte)

  fid <- h5_create_file(outname)
  h5_create_group(fid, site)
  h5_create_group(fid, paste0(site, "/dp01"))
  h5_create_group(fid, paste0(site, "/dp01/data"))
  h5_create_group(fid, paste0(site, "/dp01/data/iso", analyte))
  h5_create_group(fid, paste0(site, "/dp01/qfqm"))
  h5_create_group(fid, paste0(site, "/dp01/qfqm/iso", analyte))
  h5_create_group(fid, paste0(site, "/dp01/ucrt"))
  h5_create_group(fid, paste0(site, "/dp01/ucrt/iso", analyte))

  # copy attributes from source file and write to output file.
  # use pre-read attrs if provided, otherwise read from file.
  if (is.null(attrs)) {
    attrs <- h5_read_attrs(inname[1], site)
  }

  attrloc <- h5_open_group(fid, site)

  for (i in seq_along(attrs)) {
    h5_write_attr(attrloc, names(attrs)[i], attrs[[i]])
  }

  h5_close_group(attrloc)

  if (keep_open) {
    return(fid)
  }
  h5_close(fid)

}


#######################################
### FUNCTIONS THAT WORK ON ONLY CO2 ###
#######################################
#' Write carbon calibrations to file
#'
#' Write a `data.frame` with slope, intercepts, and error estimates of
#' calibrations for carbon isotope system. If `gainoffset` method was used
#' the slopes/intercepts are called gain/offsets for each isotopologue.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param cal_df Calibration data frame -
#'              this is the output from fit_carbon_regression
#' @param method Was the Bowling et al. 2003 or the linear regression
#'          method used in fit_carbon_regression?
#' @param to_file Write to file (TRUE) or to environment (FALSE).
#' @param fid Optional open HDF5 file handle. If NULL, the file is
#'        opened and closed internally.
#'
#' @return Nothing to the environment, but writes out the
#'         calibration parameters (e.g., gain and offset or
#'         regression slopes and intercepts) to the output
#'         hdf5 file.
#'
write_carbon_calibration_data <- function(outname,
                                          site,
                                          cal_df,
                                          method,
                                          to_file = TRUE,
                                          fid = NULL) {

  print("Writing calibration parameters...")

  own_fid <- is.null(fid)
  if (own_fid) fid <- h5_open(outname)

  co2_cal_outloc <- h5_create_group(fid,
                                    paste0(site,
                                           "/dp01/data/isoCo2/calData"))

  if (method == "Bowling_2003") {
    h5_write_dataset(co2_cal_outloc, "calGainsOffsets", cal_df)
  } else if (method == "linreg") {
    h5_write_dataset(co2_cal_outloc, "calRegressions", cal_df)
  }

  h5_close_group(co2_cal_outloc)
  if (own_fid) h5_close(fid)

}

#' Write calibrated carbon ambient data to file
#'
#' Write out ambient observations from the NEON EC
#' towers where the isotope data (either H2O or CO2)
#' have been calibrated using this package.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param amb_data_list Calibrated list of ambient data -
#'   this is the output from one of the calibrate_ambient_carbon* functions.
#' @param to_file Write to file (TRUE) or to environment (FALSE).
#' @param fid Optional open HDF5 file handle. If NULL, the file is
#'        opened and closed internally.
#'
#' @return Nothing to the environment, but writes data in amb_data_list to file.
#'
write_carbon_ambient_data <- function(outname,
                                      site,
                                      amb_data_list,
                                      to_file = TRUE,
                                      fid = NULL) {

  print("Writing calibrated ambient data...")

  own_fid <- is.null(fid)
  if (own_fid) fid <- h5_open(outname)

  if (length(amb_data_list) > 0) {
    for (i in seq_along(amb_data_list)) {
      amb_data_subset <- amb_data_list[i]

      co2_data_outloc <- h5_create_group(fid,
                                         paste0(site,
                                                "/dp01/data/isoCo2/",
                                                names(amb_data_subset)))

      amb_data_subset <- amb_data_subset[[1]] # list hack

      # loop through variables in amb_data_list and write as a dataframe.
      lapply(names(amb_data_subset),
             function(x) {
               h5_write_dataset(co2_data_outloc, x, amb_data_subset[[x]])
             })
      h5_close_group(co2_data_outloc)
    }

  }

  if (own_fid) h5_close(fid)

}

#######################################
### FUNCTIONS THAT WORK ON ONLY H2O ###
#######################################
#' Write water calibration parameters to file
#'
#' Write a `data.frame` with slope, intercepts, and error estimates of
#' calibrations for water isotope system.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param cal_df Calibration data frame -
#'              this is the output from fit_water_regression
#'
#' @param fid Optional open HDF5 file handle. If NULL, the file is
#'        opened and closed internally.
#'
#' @return Nothing to the environment, but writes out the
#'         calibration parameters (e.g.,
#'         regression slopes and intercepts) to the output
#'         hdf5 file.
#'
write_water_calibration_data <- function(outname, site, cal_df,
                                         fid = NULL) {

  print("Writing calibration parameters...")

  own_fid <- is.null(fid)
  if (own_fid) fid <- h5_open(outname)

  h2o_cal_outloc <- h5_create_group(fid,
                                    paste0(site,
                                           "/dp01/data/isoH2o/calData"))

  # write out dataset.
  h5_write_dataset(h2o_cal_outloc, "calRegressions", cal_df)

  # close the group and the file
  h5_close_group(h2o_cal_outloc)
  if (own_fid) h5_close(fid)

}

#' Write calibrated ambient water isotope ratio observations to file.
#'
#' Write out ambient observations from the NEON EC
#' towers where the isotope data
#' have been calibrated using this package.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param outname Output file name.
#' @param site NEON 4-letter site code.
#' @param amb_data_list Calibrated list of ambient data -
#'   this is the output from one of the calibrate_ambient_water* functions.
#' @param fid Optional open HDF5 file handle. If NULL, the file is
#'        opened and closed internally.
#'
#' @return Nothing to the environment, but writes data in amb_data_list to file.
#'
write_water_ambient_data <- function(outname, site, amb_data_list,
                                     fid = NULL) {

  print("Writing calibrated ambient data...")

  own_fid <- is.null(fid)
  if (own_fid) fid <- h5_open(outname)

  if (length(amb_data_list) > 0) {
    for (i in seq_along(amb_data_list)) {
      amb_data_subset <- amb_data_list[i]

      h2o_data_outloc <- h5_create_group(fid,
                                         paste0(site,
                                                "/dp01/data/isoH2o/",
                                                names(amb_data_subset)))

      amb_data_subset <- amb_data_subset[[1]] # list hack

      # loop through variables in amb_data_list and write as a dataframe.
      lapply(names(amb_data_subset),
             function(x) {
               h5_write_dataset(h2o_data_outloc, x, amb_data_subset[[x]])
             })
      h5_close_group(h2o_data_outloc)
    }

  }

  if (own_fid) h5_close(fid)
}
