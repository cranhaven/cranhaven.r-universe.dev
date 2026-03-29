#' swap_standard_isotoperatios
#'
#' There are a few suspected instances where the water
#' isotope ratios for oxygen and hydrogen have been flipped
#' in the reference data. This function corrects them until
#' they are corrected in the NEON database using a d-excess
#' filter.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param std_frame Standard data frame to perform swap on.
#' @param dxs_thres d-excess threshold to indicate when to swap.
#'
#' @return A data.frame based on `std_frame`, where d18O and
#'         d2H values have been swapped from NEON input files if
#'         determined to have a reference value mismatch. Mismatch
#'         is determined based on the d-excess of the standard (=
#'         d2H - 8*d18O), using a value of 500 by default.

swap_standard_isotoperatios <- function(std_frame, dxs_thres = 500) {
  # calculate d excess
  dxs <- std_frame$d2H_ref_mean - 8 * std_frame$d18O_ref_mean

  # if d-excess is extremely positive, indicates a really low d18O value for
  # a given d2H value, and potentially switched isotope ratios in the standard
  inds <- which(dxs > dxs_thres)

  tmpa <- std_frame$d2H_ref_mean
  tmpb <- std_frame$d18O_ref_mean

  if (sum(inds) > 0) {
    # swap isotope ratios where inds = TRUE
    std_frame$d2H_ref_mean[inds] <- tmpb[inds]
    std_frame$d18O_ref_mean[inds] <- tmpa[inds]
  }

  # return fixed dataframe
  return(std_frame)
}

#' correct_carbon_ref_cval
#'
#' This ugly function is present out of necessity, and will
#' only exist for as long as it is necessary. It is an internal
#' correction within the NEONiso calibration routines that is
#' required as there are some mismatches between the 'true'
#' isotope reference values and those in the NEON HDF5 files.
#' NEON is working on correcting this, and after it has been
#' corrected, this function has no need to exist and will be
#' immediately deprecated. As a result, this function is
#' fairly messy but there is little incentive to improve it.
#'
#' Current sites and time periods affected:
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param std_frame Standard data frame to perform swap on.
#' @param site NEON four letter site code.
#' @param omit_already_corrected Should we attempt correction, if it's
#'             already been corrected in the raw files.
#' @param co2_tol Tolerance to use to select co2 values that need to be
#'             replaced, in ppm. Default = 5 ppm.
#' @param d13c_tol Tolerance to use to select d13C values that need to be
#'             replaced, in ppm. Default = 0.25 per mil.
#'
#' @return A data.frame, based on `std_frame`, where NEON-supplied
#' reference values have been corrected if a mismatch has previously
#' been identified.
#' @export
#'

correct_carbon_ref_cval <- function(std_frame,
                                    site,
                                    omit_already_corrected = TRUE,
                                    co2_tol = 5, d13c_tol = 0.25) {

  # uses internal dataset, carb, which has columns:
  # site, refGas, startDate, endDate, co2_old, co2_corr, co2_repairedRaw,
  # d13C_old, d13C_corr, d13C_repairedRaw, versionAdded, notes

  # if data have already been corrected in raw files, no need to correct again.
  if (omit_already_corrected) {
    carb_red <- subset(carb,
                       carb$co2_repairedRaw == FALSE |
                         carb$d13C_repairedRaw == FALSE)
  } else {
    carb_red <- carb
  }

  carb_red <- carb_red[carb_red$site == site, ]

  # check to see if site is in carb$site, otherwise, we can skip.
  # nrow > 0 needed due to omit_already_corrected flag.
  if (nrow(carb_red) > 0 & (site %in% unique(carb$site))) {

    for (z in 1:nrow(carb_red)) {

      print(paste("Correcting data for", site, "between",
                  carb_red$startDate[z], "and", carb_red$endDate[z]))

      co2_min <- carb_red$co2_old[z] - co2_tol
      co2_max <- carb_red$co2_old[z] + co2_tol
      d13c_min <- carb_red$d13C_old[z] - d13c_tol
      d13c_max <- carb_red$d13C_old[z] + d13c_tol

      if (carb_red$co2_repairedRaw[z] == FALSE) {
        std_frame$rtioMoleDryCo2Refe.mean[std_frame$timeBgn >
                                            carb_red$startDate[z] &
                                            std_frame$timeBgn <=
                                              carb_red$endDate[z] &
                                            std_frame$verticalPosition ==
                                              carb_red$ref_gas[z] &
                                            std_frame$rtioMoleDryCo2Refe.mean <=
                                              co2_max &
                                            std_frame$rtioMoleDryCo2Refe.mean >=
                                              co2_min] <- carb_red$co2_corr[z]
      }

      if (carb_red$d13C_repairedRaw[z] == FALSE) {
        conds <- std_frame$timeBgn > carb_red$startDate[z] &
          std_frame$timeBgn <= carb_red$endDate[z] &
          std_frame$verticalPosition == carb_red$ref_gas[z] &
          std_frame$dlta13CCo2Refe.mean <= d13c_max &
          std_frame$dlta13CCo2Refe.mean >= d13c_min

        std_frame$dlta13CCo2Refe.mean[conds] <- carb_red$d13C_corr[z]
      }
    }
  }
  return(std_frame)
}

#' Correct carbon ref output
#'
#' Corrects known mismatches in the database where standard values do not
#' actually match what they should in data files per calVal measurements.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param std_list List containing reference/validation gas measurements.
#' @param site Four-letter NEON site code.
#' @param omit_already_corrected Skip correction if the reference gas
#'             values have already been corrected in the files (default TRUE)
#'             If you have older versions of the files, you may want to set
#'             this to FALSE.
#' @param co2_tol Tolerance used to identify a mismatch in CO2 values. Will
#'             correct measured CO2 values within +/- co2_tol within time period
#'             identified as having incorrect reference values.
#' @param d13c_tol Tolerance used to identify a mismatch in d13C values. Will
#'             correct measured d13C values within +/- d13c_tol within time
#'             period identified as having incorrect reference values.
#' @param ref_gas Which reference gas is being corrected? Expects "co2High",
#'             "co2Med", or "co2Low"
#'
#' @return A version of std_list with corrected reference values.
#' @export
#'
correct_carbon_ref_output <- function(std_list,
                                      site,
                                      omit_already_corrected = TRUE,
                                      co2_tol = 5, d13c_tol = 0.25,
                                      ref_gas) {

  # note: this one operates on lists! function above operates on data frames
  # uses internal dataset, carb, which has columns:
  # site, refGas, startDate, endDate, co2_old, co2_corr, co2_repairedRaw,
  # d13C_old, d13C_corr, d13C_repairedRaw, versionAdded, notes

  # if data have already been corrected in raw files, no need to correct again.
  if (omit_already_corrected) {
    carb_red <- subset(carb,
                       carb$co2_repairedRaw == FALSE |
                       carb$d13C_repairedRaw == FALSE)
  } else {
    carb_red <- carb
  }

  # strip off time indices
  carb_red <- carb_red[carb_red$site == site &
                         carb_red$ref_gas == substr(ref_gas,
                                                    1,
                                                    nchar(ref_gas) - 4), ]

  # check to see if site is in carb$site, otherwise, we can skip.
  if (nrow(carb_red) > 0 && (site %in% unique(carb$site))) {

    # check name of list to see if any corrections are needed for this standard
    for (z in 1:nrow(carb_red)) {

      print(paste("Correcting data for",
                  site,
                  substr(ref_gas, 1, nchar(ref_gas) - 4),
                  "between",
                  carb_red$startDate[z],
                  "and",
                  carb_red$endDate[z]))

      co2_min <- carb_red$co2_old[z] - co2_tol
      co2_max <- carb_red$co2_old[z] + co2_tol
      d13c_min <- carb_red$d13C_old[z] - d13c_tol
      d13c_max <- carb_red$d13C_old[z] + d13c_tol

      if (carb_red$co2_repairedRaw[z] == FALSE) {
        conds <- std_list$rtioMoleDryCo2Refe$timeBgn > carb_red$startDate[z] &
          std_list$rtioMoleDryCo2Refe$timeBgn <= carb_red$endDate[z] &
          std_list$rtioMoleDryCo2Refe$mean <= co2_max &
          std_list$rtioMoleDryCo2Refe$mean >= co2_min

        std_list$rtioMoleDryCo2Refe$mean[conds] <- carb_red$co2_corr[z]
      }

      if (carb_red$d13C_repairedRaw[z] == FALSE) {
        conds <- std_list$dlta13CCo2Refe$timeBgn > carb_red$startDate[z] &
          std_list$dlta13CCo2Refe$timeBgn <= carb_red$endDate[z] &
          std_list$dlta13CCo2Refe$mean <= d13c_max &
          std_list$dlta13CCo2Refe$mean >= d13c_min

        std_list$dlta13CCo2Refe$mean[conds] <- carb_red$d13C_corr[z]
      }
    }
  }
  return(std_list)

}