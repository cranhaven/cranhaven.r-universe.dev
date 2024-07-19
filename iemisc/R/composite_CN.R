#' Composite CN (Curve Number) with Connected Impervious Area
#'
#' This function computes the composite CN (Curve Number) for connected
#' impervious areas.
#'
#'
#' @param pervious_CN numeric vector containing the pervious runoff curve number
#' @param impervious numeric vector containing the percent imperviousness
#'
#' @return the Composite Runoff Curve Number as a single numeric vector, in the
#'   range [0, 100]
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
#' @references
#' United States Department of Agriculture Natural Resources Conservation Service Conservation Engineering Division, "Urban Hydrology for Small Watersheds Technical Release 55 (TR-55)", June 1986, pages 2-11 - 2-16, \url{https://web.archive.org/web/20230810204711/https://directives.sc.egov.usda.gov/OpenNonWebContent.aspx?content=22162.wba} [Recovered with the Internet Archive: Wayback Machine]
#'
#'
#'
#'
#'
#'
#'
#'
#' @note
#' Note: Please refer to iemiscdata: Weighted CN Calculations Using the
#' Composite CN vignette in the iemiscdata package
#' 
#' 
#' 
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
c_composite_CN <- function (pervious_CN, impervious) {

# Check for pervious_CN
assert_that(qtest(pervious_CN, "N==1(0,)"), msg = "pervious_CN is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there is not exactly 1 pervious_CN value. Please try again.")
# only process enough known pervious_CN values and provide a stop warning if not enough


# Check for impervious
assert_that(qtest(impervious, "N==1(0,)"), msg = "impervious is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there is not exactly 1 impervious value. Please try again.")
# only process enough known impervious values and provide a stop warning if not enough

sum(pervious_CN, (impervious / 100) * (98 - pervious_CN))

}











#' Composite CN (Curve Number) with Unconnected Impervious Area
#'
#' This function computes the composite CN (Curve Number) for unconnected
#' impervious areas and total impervious areas less than 30 percent.
#'
#'
#' @param pervious_CN numeric vector containing the pervious runoff curve number
#' @param impervious numeric vector containing the percent imperviousness
#' @param R numeric vector containing the ratio of unconnected impervious area to total impervious area
#'
#' @return the Composite Runoff Curve Number as a single numeric vector, in the
#'   range [0, 100]
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
#' @references
#' United States Department of Agriculture Natural Resources Conservation Service Conservation Engineering Division, "Urban Hydrology for Small Watersheds Technical Release 55 (TR-55)", June 1986, pages 2-11 - 2-16, \url{https://web.archive.org/web/20230810204711/https://directives.sc.egov.usda.gov/OpenNonWebContent.aspx?content=22162.wba} [Recovered with the Internet Archive: Wayback Machine]
#'
#'
#'
#'
#' @examples
#' 
#' # Please refer to iemiscdata: Weighted CN Calculations Using the Composite CN
#' # vignette in the iemiscdata package
#' 
#' 
#' 
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
uc_composite_CN <- function (pervious_CN, impervious, R) {

# Check for pervious_CN
assert_that(qtest(pervious_CN, "N==1(0,)"), msg = "pervious_CN is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there is not exactly 1 pervious_CN value. Please try again.")
# only process enough known pervious_CN values and provide a stop warning if not enough


# Check for impervious
assert_that(qtest(impervious, "N==1(0,)"), msg = "impervious is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there is not exactly 1 impervious value. Please try again.")
# only process enough known impervious values and provide a stop warning if not enough


# Check for R
assert_that(qtest(R, "N==1(0,1]"), msg = "R is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there is not exactly 1 R value. Please try again.")
# only process enough known R values and provide a stop warning if not enough


sum(pervious_CN, (impervious / 100) * (98 - pervious_CN) * (1 - 0.5 * R))

}
