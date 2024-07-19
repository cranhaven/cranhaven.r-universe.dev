#' Calculate the Total Surface Area of Linear Surfaces
#'
#' This function computes the total surface area of linear surfaces (total sum
#' of width x length). This function was created to use in drainage area
#' calculations; however, it can be used in other calculations as well.
#' 
#' @param length numeric vector containing the length value(s) in one of the
#'    lw_units values.
#' @param width numeric vector containing the width value(s) in one of the
#'    lw_units values.
#' @param surface_area_table data.frame/data.table/tibble, list, or matrix
#'    containing the length in column 1 and the width in column 2
#' @param lw_units character vector containing the units for the length and the
#'    width (default = "feet"). The other possible units are "inch",
#'    "survey_foot", "yard", "mile", "centimeter", "meter", or "kilometer". The
#'    units should be consistent and not mixed.
#'
#' @return surface area as a numeric vector in the provided square units. The
#'    calculated value will be the same for all units in this function. The
#'    units specified in this function are used in the \code{\link{rain_garden_driveway}}
#'    function.
#'
#'
#'
#'
#'
#'
#' @seealso \code{\link{rain_garden_driveway}} for calculating the rain garden size for driveways
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
#' @examples
#'
#' # Note: the units must be consistent
#'
#' # Example 1
#'
#' library(iemisc)
#'
#' length1 <- c(220, 150, 30)
#' width1 <- c(75, 89, 80)
#' surface_area(width = width1, length = length1, lw_units = "meter")
#'
#'
#' # Example 2
#'
#' library(iemisc)
#'
#' length2 <- c(333, 681, 73)
#' width2 <- c(17.4, 9.5, 8)
#' surface_area_table = list(Length = length2, Width = width2)
#' surface_area(surface_area_table = surface_area_table, lw_units = "mile")
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @importFrom data.table as.data.table
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest testDataTable
#' @importFrom round round_r3
#'
#' @export
surface_area <- function (length = NULL, width = NULL, surface_area_table = NULL, lw_units = c("inch", "feet", "survey_foot", "yard", "mile", "centimeter", "meter", "kilometer")) {


if (missing(length) & missing(width)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is length and column 2 is width

surface_area_table <- as.data.table(surface_area_table)

assert_that(testDataTable(surface_area_table, types = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 1, min.cols = 1, ncols = 2), msg = "Any row of surface_area_table contains a value that is NA, NaN, empty, or a string. Please try again.")
# only process with enough known length and width values and provide a stop warning if not enough

length <- surface_area_table[, 1][[1]]

width <- surface_area_table[, 2][[1]]

}


checks <- c(length, width)

lw_units <- lw_units


# Check for checkss
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either length or width is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails



ifelse(length(lw_units) > 1, lw_units <- "feet", lw_units <- lw_units)

ifelse(missing(lw_units), lw_units <- "feet", lw_units <- lw_units)



# check for lw_units
assert_that(qtest(lw_units, "S==1"), msg = "lw_units should only be a single character vector. Please specify either 'inch', 'feet', 'survey_foot', 'yard', 'mile', 'centimeter', 'meter', or 'kilometer'. Please try again.")
# only process with a single string value and provide a stop warning if not

assert_that(isTRUE(any(c("inch", "feet", "survey_foot", "yard", "mile", "centimeter", "meter", "kilometer") %in% lw_units)), msg = "Incorrect unit selection. The only possible lw_units are 'inch', 'feet', 'survey_foot', 'yard', 'mile', 'centimeter', 'meter', and 'kilometer'. Please try again.")
# only process with a specified unit and provide a stop warning if not
# Source 1


surface_area <- round_r3(sum(length * width), d = 2)

# Although the units are not used within this function, the given units are needed in the rain_garden_driveway function which relies on this function

return(surface_area)

}
