#' Modified Rational Method Equation
#'
#' Computes the design peak runoff rate (Q) using the modified rational method
#' equation.
#'
#'
#'
#' @param C_F numeric vector that contains the "runoff coefficient adjustment
#'    factor to account for reduction of infiltration and other losses during
#'    high intensity storms" [Input a number between 2 and 10; 25; 50; or 100]
#' @param C numeric vector that contains the dimensionless "runoff coefficient
#'    to reflect the ratio of rainfall to surface runoff"
#' @param i numeric vector that contains the "rainfall intensity in inches per
#'    hour (in/hr)"
#' @param A numeric vector that contains the drainage area in one of the
#'    area_units values.
#' @param area_units character vector containing the units for area
#'    (default = "acre"). The other possible units are "square feet",
#'    "square mile", "hectare", or "square kilometer".
#'
#'
#'
#' @return the numeric vector Q, which is the "peak flow" in cubic feet per
#'    second (cfs or ft^3/s)
#'
#'
#' @note
#' Please note: Refer to the limitations of the Modified Rational Method
#' equation for your particular jurisdiction. Notes are only included below for
#' Oklahoma and Oregon, respectively.
#' 
#' for Oklahoma
#' "The Rational Method, first introduced in 1889, is recommended for estimating
#' the design storm peak runoff for areas up to 640 acres. The Rational Method
#' was modified in the 1980's to include a runoff coefficient correction tied to
#' the flood frequency. This Modified Rational Method is used by ODOT.
#' 
#' Some precautions should be considered when applying the Rational method:
#' --The first step in applying the Rational method is to obtain a good
#' topographic map and define the boundaries of the drainage area in question. A
#' field inspection of the area should also be made to determine if the natural
#' drainage divides have been altered.
#' --In determining surface characteristics for the drainage area, consider any
#' future changes in land use that might occur during the service life of the
#' proposed facility that could result in an inadequate drainage system. Also,
#' the effects of upstream detention facilities may be considered.
#' --Restrictions to the natural flow (e.g., highway crossings and dams that
#' exist in the drainage area) should be investigated to determine how they
#' might affect the design flows.
#' --The charts, graphs and tables included in this Section are not intended to
#' replace reasonable and prudent engineering judgment that should permeate
#' each step in the design process." [Oklahoma Department of Transportation
#' Reference]
#' 
#' 
#' for Oregon:
#' "Limitations and assumptions in the Rational Method are as follows:
#' --The drainage area should not be larger than 200 acres.
#' --The peak flow is assumed to occur when the entire watershed is contributing runoff.
#' --The rainfall intensity is assumed to be uniform over a time duration equal to or
#' greater than the time of concentration, Tc.
#' --The peak flow recurrence interval is assumed to be equal to the rainfall intensity
#' recurrence interval. In other words, the 10-year rainfall intensity is assumed to
#' produce the 10-year flood." [Oregon Department of Transportation Reference]
#'
#' The value of 1.008 is used for the unit conversion factor for English units.
#' [Tennessee Design reference]
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Design Principles for Erosion Prevention & Sediment Control for Construction Sites Level II EPSC Workshop, Fall 2017. Sponsored by The University of Tennessee Biosystems Engineering & Environmental Sciences Tennessee Water Resources Research Center, Tennessee Department of Environment and Conservation Division of Water Resources, and Tennessee Department of Transportation.
#'    \item Oklahoma Department of Transportation (ODOT) Roadway Drainage Manual Chapter 7 Hydrology, November 2014, page 7.6-1, \url{https://oklahoma.gov/content/dam/ok/en/odot/documents/chapter-7-hydrology.pdf}.
#'    \item Oregon Department of Transportation (ODOT) Geo-Environmental, ODOT Hydraulics Manual Appendix F – Rational Method, April 2014, page 7-F-1, \url{https://web.archive.org/web/20221202194502/https://www.oregon.gov/odot/GeoEnvironmental/Docs_Hydraulics_Manual/Hydraulics-07-F.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#'    \item U.S. Department of Agriculture (USDA) Natural Resources Conservation Service (NRCS), Hydrology Training Series Module 206 D - Peak Discharge (Other Methods) Study Guide, page 18 (of the PDF document) and page 26 - 27 (of the PDF document), \url{https://web.archive.org/web/20211018222532/https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb1083019.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#' }
#'
#'
#
#'
#'
#'
#'
#'
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#'
#' @examples
#'
#' # Example 1 from NRCS Reference
#' 
#' # Given
#' # Urban setting with a drainage area of 12 acres
#' # 6 acres = single family area
#' # 3 acres = park
#' # 3 acres = streets (concrete)
#' # Soil = clay loam
#' # Tc = 20 min (time of concentration)
#' 
#' # Find the instantaneous peak discharge for a 25-yr frequency flood at a
#' # road crossing in an urban/rural area located in the Kansas City, Missouri
#' # area.
#'
#'
#' library(iemisc)
#' 
#' area1 <- c(6, 3, 3)
#' C1 <- c(mean(c(0.30, 0.50)), 0.15, 0.90)
#' C1_w <- weighted_C(C = C1, area = area1)
#' 
#' i1 <- 5.1 # in/hr
#'
#' rational_formula(C_F = 25, C = C1_w, i = i1, A = sum(area1), area_units = "acre")
#'
#'
#'
#'
#'
#'
#' # Example 2 from NRCS Reference
#' 
#' # Given
#' # Urban setting with a drainage area of 18 acres
#' 
#' # 1 ac = playground
#' # 10 ac = single family area
#' # 2 ac = streets (asphaltic)
#' # 5 ac = pasture (hilly)
#' # Soil = heavy clay
#' # Tc = 20 min
#' 
#' # Find the instantaneous 100-yr frequency peak discharge for design of a
#' # channel in a developing subdivision located in an area near Asheville,
#' # North Carolina.
#'
#'
#' library(iemisc)
#' 
#' area2 <- c(1, 10, 2, 5)
#' C2 <- c(0.35, 0.50, 0.90, 0.60)
#' C2_w <- weighted_C(C = C2, area = area2)
#' 
#' i2 <- 5.5 # in/hr
#'
#' rational_formula(C_F = 100, C = C2_w, i = i2, A = sum(area2), area_units = "acre")
#'
#'
#'
#'
#'
#'
#'
#'
#' @importFrom data.table data.table between
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom stringi stri_replace_all_fixed
#'
#' @export
rational_formula <- function (C_F, C, i, A, area_units = c("acre", "square feet", "square mile", "hectare", "square kilometer")) {


ft <- acre <- mi <- hectare <- km <- NULL
# due to NSE notes in R CMD check


area_units <- area_units

checks <- c(C_F, C, i, A)

F <- 1.008

CFC <- C_F * C


# Check for checks
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either C_F, C, i, or A is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

# Check values
assert_that(between(CFC, 0, 1) == FALSE, msg = "The product of C_F and C is greater than 1. Please recheck the storm frequency and/or the runoff coefficient. Please try again.")
# only process with the product of CF and C < 1 and provide an error message if the check fails

assert_that(between(C, 0, 1) == TRUE, msg = "The runoff coefficient is greater than 1. Please check that you are using the Rational formula runoff coefficient and not the Curve Number. Please try again.")
# only process with a C value < 1 and provide an error message if the check fails


area <- as.numeric(stri_replace_all_fixed(A, ",", ""))

ifelse(length(area_units) > 1, area_units <- "acre", area_units <- area_units)

ifelse(missing(area_units), area_units <- "acre", area_units <- area_units)



# check for area_units
assert_that(qtest(area_units, "S==1"), msg = "area_units should only be a single character vector. Please specify either 'acre', 'square feet', 'square mile', 'hectare', or 'square kilometer'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(isTRUE(any(c("acre", "square feet", "square mile", "hectare", "square kilometer") %in% area_units)), msg = "Incorrect unit selection. The only possible area_units are 'acre', 'square feet', 'square mile', 'hectare', and 'square kilometer'. Please try again.")
# only process with a specified unit and provide a stop warning if not
# Source 1


if (area_units == "acre") {

area <- area


} else if (area_units == "square feet") {

area <- set_units(area, ft^2) # ft^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "square mile") {

area <- set_units(area, mi^2) # mi^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "hectare") {

area <- set_units(area, hectare) # hectare

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "square kilometer") {

area <- set_units(area, km^2) # km^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)

}


assert_that(!any(qtest(area, "N==1[0.00000000000000000000000000000001,640)") == FALSE), msg = "A (drainage area) is greater than 640 acres. The drainage area shoudl be less than 640 acres. Please try again.")
# only process with finite values and provide an error message if the check fails



C_F_table <- data.table(Storm = c(2, 5, 10, 25, 50, 100), CF = c(rep(1.0, 3), 1.1, 1.2, 1.25))

C_F_use <- lookupQT(C_F, setDF(C_F_table))

Q <- F * C_F_use * C * i * area

return(Q)
}
