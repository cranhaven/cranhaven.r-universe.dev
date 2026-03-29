# isotope functions
# break out functions that are various transformations
# of isotope ratios and delta values.
#------------------------------------------------------
#' Return heavy-to-light isotope ratio of primary standard.
#'
#' Returns the heavy-to-light isotope ratio of the dominant
#' standard for that element. Vienna Standard Mean Ocean Water
#' (VSMOW) for oxygen and hydrogen isotopes, Vienna Pee Dee
#' Belemnite (VPDB) for carbon stable isotopes.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param element Which element to return standard ratio -
#'        carbon, oxygen, or hydrogen.
#'
#' @return Heavy-to-light isotope ratio of most common
#'         stable isotope standard. VSMOW for water,
#'         VPDB for carbon.
#'
#' @export
#' @examples
#' get_Rstd("carbon") # returns 0.0111797
#' get_Rstd("oxygen") # returns 2005.20e-6
#'
get_Rstd <- function(element) {
  # return the standard isotope ratio
  if (element == "carbon") {
    r <- 0.0111797 # 13C/12C ratio for VPBD standard.
  } else if (element == "oxygen") {
    r <- 2005.20e-6 # SMOW 18O/16O ratio
  } else if (element == "hydrogen") {
    r <- 155.76e-6 # SMOW 2H/1H ratio
  }
  return(r)
}

#' Convert heavy-to-light isotope ratio to delta values.
#'
#' Converts a heavy-to-light stable isotope ratio to
#' a corresponding delta value, in per mil values.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param R_values A vector of isotope ratios (e.g., R values).
#' @param element Which element to return delta values -
#'        carbon, oxygen, or hydrogen.
#'
#' @return Vector of isotope ratios in delta notation.
#'
#' @export
#' @examples
#' R_to_delta(R_values = 2005.20e-6, element = 'oxygen') # returns 0.
R_to_delta <- function(R_values, element) {

  # get standard isotope ratio
  Rstd <- get_Rstd(element)

  #convert R to delta
  delta <- 1000 * (R_values / Rstd - 1)

  # return delta values
  return(delta)
}

#' Converts delta value to heavy-to-light isotope ratio
#'
#' Converts a delta value (in per mil) to the heavy-to-light
#' isotope ratio.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param delta_values A vector of isotope ratios in delta notation.
#' @param element Which element to return R values -
#'        carbon, oxygen, or hydrogen.
#'
#' @return Vector of isotope ratios (R values).
#'
#' @export
#' @examples
#' delta_to_R(delta_values = 0, element = 'oxygen') # 2005.2e-6 for VSMOW.
#'
delta_to_R <- function(delta_values, element) {

  # get standard isotope ratio
  Rstd <- get_Rstd(element)
  #convert R to delta
  R <- Rstd * (delta_values / 1000 + 1)
  # return delta values
  return(R)

}

#' Calculate 12C-CO2 Mole Fractions
#'
#' This function calculates mole fractions of 12CO2 based on the total CO2 mole
#' fraction, the delta13C value of the mixture, and the assumed fraction of CO2
#' that does not correspond to 12CO2 or 13CO2 (assumed fixed at 0.00474, e.g.,
#' Griffis et al. 2004 Agricultural and Forest Meteorology)
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param total_co2 Vector of CO2 mole fractions.
#' @param delta13c Vector of d13C values.
#' @param f Fraction of CO2 that is not 12CO2 or 13CO2. Assumed fixed
#'          at 0.00474
#'
#' @return Vector of 12CO2 mole fractions.
#'
#' @export
#' @examples
#' calculate_12CO2(total_co2 = 410, delta13c = -8.5)
#'
calculate_12CO2 <- function(total_co2, delta13c, f = 0.00474) {

  # note: f technically varies, but this has little impact
  # on calibration per Griffis et al. 2004.

  # convert delta13C to R13
  r <- delta_to_R(delta13c, "carbon")

  # calculate 12CO2 from total CO2 and R
  light_co2 <- total_co2 * (1 - f) / (1 + r)

  # return 12co2
  return(light_co2)
}

#' Calculate 13C-CO2 Mole Fractions
#'
#' This function calculates mole fractions of 13CO2 based on the total CO2 mole
#' fraction, the delta13C value of the mixture, and the assumed fraction of CO2
#' that does not correspond to 12CO2 or 13CO2 (assumed fixed at 0.00474, e.g.,
#' Griffis et al. 2004 Agricultural and Frest Meteorology)
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param total_co2 Vector of CO2 mole fractions.
#' @param delta13c Vector of d13C values.
#' @param f Fraction of CO2 that is not 12CO2 or 13CO2. Assumed fixed
#'          at 0.00474
#'
#' @return Vector of 13CO2 mole fractions.
#'
#' @export
#' @examples
#' calculate_13CO2(total_co2 = 410, delta13c = -8.5)
#'
calculate_13CO2 <- function(total_co2, delta13c, f = 0.00474) {

  # note: f technically varies, but this has little impact
  # on calibration per Griffis et al. 2004.

  # calculate 13CO2 from total CO2 and R
  light_co2 <- calculate_12CO2(total_co2, delta13c)

  heavy_co2 <- total_co2 * (1 - f) - light_co2

  # return 13co2
  return(heavy_co2)

}
