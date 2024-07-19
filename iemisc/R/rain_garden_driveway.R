#' Rain Garden Sizing for Driveways
#'
#' This function computes the final rain garden dimensions based on the size of
#' the impervious roof surfaces and the initial rain garden size. This function
#' uses the \code{\link{surface_area}} values and units in the calculations.
#' 
#' @param dw_length numeric vector containing the length value(s) in one of the
#'    lw_units values for the driveway(s).
#' @param dw_width numeric vector containing the width value(s) in one of the
#'    lw_units values for the driveway(s).
#' @param driveway_table data.frame/data.table/tibble, list, or matrix
#'    containing the length in column 1 and the width in column 2 for the
#'    driveway(s).
#' @param rf_length numeric vector containing the length value(s) in one of the
#'    lw_units values for the roof(s).
#' @param rf_width numeric vector containing the width value(s) in one of the
#'    lw_units values for the roof(s).
#' @param roof_table data.frame/data.table/tibble, list, or matrix
#'    containing the length in column 1 and the width in column 2 for the
#'    roof(s).
#' @param lw_units character vector containing the units for the length and the
#'    width (default = "feet"). The other possible units are "inch",
#'    "survey_foot", "yard", "mile", "centimeter", "meter", or "kilometer". The
#'    units should be consistent and not mixed.
#' @param rainfall_depth numeric vector containing the rainfall depth in one of
#'    the rainfall_depth_units.
#' @param rainfall_depth_units character vector containing the units for the
#'    rainfall depth (default = "feet"). The other possible units are "inch",
#'    "centimeter", or "meter".
#' @param rain_garden_depth numeric vector containing the rain garden depth in
#'    one of the rain_garden_depth_units.
#' @param rain_garden_depth_units character vector containing the units for the
#'    rain garden depth (default = "feet"). The other possible units are "inch",
#'    "centimeter", or "meter".
#'
#' @return a \code{\link[data.table]{data.table}} with the following columns:
#'    Driveway Drainage Area, One-Quarter of Roof Area, Depth of Rain, Design
#'    Storm Volume, Rain Garden Depth, Rain Garden Initial Size, Rain Garden
#'    Initial Dimensions, Total Drainage Area, New Design Storm Volume, Rain
#'    Garden Final Size, Rain Garden Final Dimensions
#'
#'
#'
#'
#' @seealso \code{\link{surface_area}} for calculating the linear surface area
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
#' @references
#' Green Infrastructure Champion Training: Part 7: "How To Design and Build a Rain Garden", April 10, 2019, pages 41 - 43 of the PDF document, \url{https://water.rutgers.edu/Projects/GreenInfrastructureChampions/Talks_2020/Part_7_04102020.pdf}.
#'
#'
#'
#' @examples
#'
#' # Note: the units must be consistent for the lengths and widths
#'
#' # Example 1 (from the Reference)
#'
#' library(iemisc)
#'
#' dw_width1 <- c(15, 10)
#' dw_length1 <- c(50, 25)
#' lw_units <- "feet"
#' rf_width1 <- 50
#' rf_length1 <- 25
#' rainfall_depth1 <- 1.5
#' rainfall_depth_units <- "inch"
#' rain_garden_depth <- 6
#' rain_garden_depth_units <- "inch"
#'
#' rain_garden_driveway(dw_length = dw_length1, dw_width = dw_width1, rf_length =
#' rf_length1, rf_width = rf_width1, lw_units = lw_units, rainfall_depth =
#' rainfall_depth1, rainfall_depth_units = rainfall_depth_units, rain_garden_depth
#' = rain_garden_depth, rain_garden_depth_units = rain_garden_depth_units)
#'
#'
#'
#' # Example 2
#' # from https://www.ecoccs.com/R_Examples/Simple-Rain-Garden-Sizing_with-R.html
#' # Irucka Embry modified the Example from the Reference for this example
#'
#' install.load::load_package("iemisc", "data.table")
#'
#' dw_length2 <- c(construction_decimal("50 feet 3 1/2 inch", result <- "traditional",
#' output <- "vector"), construction_decimal("25 feet 5 7/8 inch", result <- "traditional",
#' output <- "vector"))
#' dw_width2 <- c(construction_decimal("15 feet 10 3/4 inch", result <- "traditional",
#' output <- "vector"), construction_decimal("10 feet 7 3/8 inch", result <- "traditional",
#' output <- "vector"))
#' lw_units <- "feet"
#' rf_length2 <- construction_decimal("25 feet 10 1/4 inch", result <- "traditional",
#' output <- "vector")
#' rf_width2 <- construction_decimal("12.5 feet 1 1/8 inch", result <- "traditional",
#' output <- "vector") * 4
#' rainfall_depth2 <- 2.25
#' rainfall_depth_units <- "inch"
#' rain_garden_depth <- 6
#' rain_garden_depth_units <- "inch"
#' 
#' driveway_table <- data.table(length = dw_length2, width = dw_width2)
#' 
#' roof_table <- data.table(length = rf_length2, width = rf_width2)
#' 
#' rain_garden_driveway(driveway_table = driveway_table, roof_table = roof_table,
#' lw_units = lw_units, rainfall_depth = rainfall_depth2, rainfall_depth_units =
#' rainfall_depth_units, rain_garden_depth = rain_garden_depth,
#' rain_garden_depth_units = rain_garden_depth_units)
#'
#'
#'
#' @importFrom data.table as.data.table
#' @importFrom units set_units make_units drop_units
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest testDataTable
#' @importFrom round round_r3
#'
#' @export
rain_garden_driveway <- function (dw_length = NULL, dw_width = NULL, rf_length = NULL, rf_width = NULL, driveway_table = NULL, roof_table = NULL, lw_units = c("inch", "feet", "survey_foot", "yard", "mile", "centimeter", "meter", "kilometer"), rainfall_depth, rainfall_depth_units = c("inch", "feet", "centimeter", "meter"), rain_garden_depth, rain_garden_depth_units = c("inch", "feet", "centimeter", "meter")) {


ft <- mi <- km <- US_survey_foot <- cm <- feet <- inch <- meter <- yd <- NULL
# due to NSE notes in R CMD check


# for the driveway
if (missing(dw_length) & missing(dw_width)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is dw_length and column 2 is dw_width

driveway_table <- as.data.table(driveway_table)

assert_that(testDataTable(driveway_table, types = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 1, min.cols = 1, ncols = 2), msg = "Any row of driveway_table contains a value that is NA, NaN, empty, or a string. Please try again.")
# only process with enough known length and width values and provide a stop warning if not enough

dw_length <- driveway_table[, 1][[1]]

dw_width <- driveway_table[, 2][[1]]

}



# for the roof
if (missing(rf_length) & missing(rf_width)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is rf_length and column 2 is rf_width

roof_table <- as.data.table(roof_table)

assert_that(testDataTable(roof_table, types = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 1, min.cols = 1, ncols = 2), msg = "Any row of roof_table contains a value that is NA, NaN, empty, or a string. Please try again.")
# only process with enough known length and width values and provide a stop warning if not enough

rf_length <- roof_table[, 1][[1]]

rf_width <- roof_table[, 2][[1]]

}


lw_units <- lw_units


# total drainage area (driveway only)
da <- surface_area(width = dw_width, length = dw_length, lw_units = lw_units)


# total roof area
rf <- surface_area(width = rf_width, length = rf_length, lw_units = lw_units)



if (lw_units == "feet") {

da <- da


} else if (lw_units == "inch") {

da <- set_units(da, inch) # in

units(da) <- make_units(feet) # feet

da <- drop_units(da)


} else if (lw_units == "survey_foot") {

da <- set_units(da, US_survey_foot) # US_survey_foot

units(da) <- make_units(feet) # feet

da <- drop_units(da)


} else if (lw_units == "yard") {

da <- set_units(da, yd) # yd

units(da) <- make_units(feet) # feet

da <- drop_units(da)


} else if (lw_units == "mile") {

da <- set_units(da, mi) # mi

units(da) <- make_units(feet) # feet

da <- drop_units(da)


} else if (lw_units == "centimeter") {

da <- set_units(da, cm) # cm

units(da) <- make_units(feet) # feet

da <- drop_units(da)


} else if (lw_units == "meter") {

da <- set_units(da, meter) # meter

units(da) <- make_units(feet) # feet

da <- drop_units(da)


} else if (lw_units == "kilometer") {

da <- set_units(da, km) # km

units(da) <- make_units(feet) # feet

da <- drop_units(da)

}





if (lw_units == "feet") {

rf <- rf


} else if (lw_units == "inch") {

rf <- set_units(rf, inch) # in

units(rf) <- make_units(feet) # feet

rf <- drop_units(rf)


} else if (lw_units == "survey_foot") {

rf <- set_units(rf, US_survey_foot) # US_survey_foot

units(rf) <- make_units(feet) # feet

rf <- drop_units(rf)


} else if (lw_units == "yard") {

rf <- set_units(rf, yd) # yd

units(rf) <- make_units(feet) # feet

rf <- drop_units(rf)


} else if (lw_units == "mile") {

rf <- set_units(rf, mi) # mi

units(rf) <- make_units(feet) # feet

rf <- drop_units(rf)


} else if (lw_units == "centimeter") {

rf <- set_units(rf, cm) # cm

units(rf) <- make_units(feet) # feet

rf <- drop_units(rf)


} else if (lw_units == "meter") {

rf <- set_units(rf, meter) # meter

units(rf) <- make_units(feet) # feet

rf <- drop_units(rf)


} else if (lw_units == "kilometer") {

rf <- set_units(rf, km) # km

units(rf) <- make_units(feet) # feet

rf <- drop_units(rf)

}





# 1/4 of total roof area
rf1_4 <- 0.25 * rf


# for the rainfall depth
if (rainfall_depth_units == "feet") {

rainfall_depth <- rainfall_depth


} else if (rainfall_depth_units == "inch") {

rainfall_depth <- set_units(rainfall_depth, inch) # in

units(rainfall_depth) <- make_units(feet) # feet

rainfall_depth <- drop_units(rainfall_depth)


} else if (rainfall_depth_units == "centimeter") {

rainfall_depth <- set_units(rainfall_depth, cm) # cm

units(rainfall_depth) <- make_units(feet) # feet

rainfall_depth <- drop_units(rainfall_depth)


} else if (rainfall_depth_units == "meter") {

rainfall_depth <- set_units(rainfall_depth, meter) # meter

units(rainfall_depth) <- make_units(feet) # feet

rainfall_depth <- drop_units(rainfall_depth)

}




# for the rain garden depth
if (rain_garden_depth_units == "feet") {

rain_garden_depth <- rain_garden_depth


} else if (rain_garden_depth_units == "inch") {

rain_garden_depth <- set_units(rain_garden_depth, inch) # in

units(rain_garden_depth) <- make_units(feet) # feet

rain_garden_depth <- drop_units(rain_garden_depth)


} else if (rain_garden_depth_units == "centimeter") {

rain_garden_depth <- set_units(rain_garden_depth, cm) # cm

units(rain_garden_depth) <- make_units(feet) # feet

rain_garden_depth <- drop_units(rain_garden_depth)


} else if (rain_garden_depth_units == "meter") {

rain_garden_depth <- set_units(rain_garden_depth, meter) # meter

units(rain_garden_depth) <- make_units(feet) # feet

rain_garden_depth <- drop_units(rain_garden_depth)

}



# initial volume of water for design storm
design_storm_volume <- da * rainfall_depth


# initial rain garden size
rg_size1 <- design_storm_volume / rain_garden_depth

rg_size1_length <- rg_size1 / 10


# new drainage area
da_new <- sum(da, rg_size1)


# final volume of water for design storm
design_storm_volume_new <- da_new * rainfall_depth


# final rain garden size
rg_size2 <- design_storm_volume_new / rain_garden_depth

rg_size2_length <- rg_size2 / 10


# Rain Garden Table

rain_garden_table <- data.table(`Driveway Drainage Area` = paste0(round_r3(da, d = 1), " sq. ft."), `One-Quarter of Roof Area` = paste0(round_r3(rf1_4, d = 1), " sq. ft."), `Depth of Rain` = paste0(round_r3(rainfall_depth, d = 3), " ft of rain"), `Design Storm Volume` = paste0(round_r3(design_storm_volume, d = 0), " ft^3"), `Rain Garden Depth` = paste0(round_r3(rain_garden_depth, d = 1), " feet"), `Rain Garden Initial Size` = paste0(round_r3(rg_size1, d = 0), " sq. ft."), `Rain Garden Initial Dimensions` = paste0(10, " ft wide x ", round_r3(rg_size1_length, d = 1), " ft long"), `Total Drainage Area` = paste0(round_r3(da_new, d = 1), " sq. ft."), `New Design Storm Volume` = paste0(round_r3(design_storm_volume_new, d = 0), " ft^3"), `Rain Garden Final Size` = paste0(round_r3(rg_size2, d = 0), " sq. ft."), `Rain Garden Final Dimensions` = paste0(10, " ft wide x ", round_r3(rg_size2_length, d = 1), " ft long"))

col.names <- c("Driveway Drainage Area", "One-Quarter of Roof Area", "Depth of Rain", "Design Storm Volume", "Rain Garden Depth", "Rain Garden Initial Size", "Rain Garden Initial Dimensions", "Total Drainage Area", "New Design Storm Volume", "Rain Garden Final Size", "Rain Garden Final Dimensions")

# code block below modified from data.table function
setattr(rain_garden_table, "col.names", setnames(rain_garden_table, col.names))
setattr(rain_garden_table, "class", c("data.table", "data.frame"))
rain_garden_table
}
