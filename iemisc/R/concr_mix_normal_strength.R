#' Concrete Mix Design for Normal Strength (Normal-weight) Concrete
#'
#' Calculates the amount of cement, sand, gravel, and water needed for a test
#' batch volume of normal strength concrete using the volumetric method.
#'
#'
#' @param fc numeric vector that contains the concrete compressive strength
#'    (psi)
#' @param slump_use numeric vector that contains the amount of slump (in)
#' @param max_size_aggr numeric vector that contains the maximum aggregate size
#'    (in)
#' @param FM numeric vector that contains the "Fineness Modulus of sand"
#'    (dimensionless)
#' @param dry_rod_wt_aggr numeric vector that contains the dry rodded weight of
#'    aggregate (lb/ft^3)
#' @param mc_coarse numeric vector that contains the moisture content of the
#'    coarse aggregate (whole number percent)
#' @param mc_fine numeric vector that contains the moisture content of the
#'    fine aggregate (whole number percent)
#' @param entrainment character vector that contains either Air or Nonair
#'    entrainment
#' @param construction_type character vector that contains the intended type of
#'    construction
#' @param slump_value character vector that contains the slump value (Maximum,
#'    Maximum + 1, or Minimum). It is "+ 1 in. for methods of consolidation
#'    other than vibration"
#' @param exposure character vector that contains the exposure value (Mild,
#'    Moderate, or Extreme) for use with Air entrained concrete mixes or Nonair to
#'    indicate that it is a Nonair entrained concrete mix
#' @param trial_batch character vector that contains the volume of the trial
#'    batch mix to return (1 cubic yard, 1 cubic foot, 0.5 cubic foot, 0.2
#'    cubic foot, or All)
#'
#'
#' @return the amounts of cement, sand, gravel, and water in lb, rounded to the
#'    hundredth, as a \code{\link[base]{list}} to make 1 yd^3, 1 ft^3, 0.5 ft^3,
#'    or 0.2 ft^3 of normal strength concrete or as a \code{\link[data.table]{data.table}} containing
#'    all batch volumes.
#'
#'
#'
#'
#' @source
#' \enumerate{
#'    \item r - Error when doing bilinear interpolation with 'interp2 {pracma}'; any better way for 2D interpolation? - Stack Overflow answered and edited by Zheyuan Li on Dec 8 2016. See \url{https://stackoverflow.com/questions/41032225/error-when-doing-bilinear-interpolation-with-interp2-pracma-any-better-way}.
#'    \item r - data.table 1.10.0 - why does a named column index value not work while a integer column index value works without with = FALSE - Stack Overflow answered and edited by Matt Dowle on Dec 8 2016. See \url{https://stackoverflow.com/questions/41032225/error-when-doing-bilinear-interpolation-with-interp2-pracma-any-better-way}.
#' }
#'
#'
#'
#' @references
#' Edward G. Nawy, \emph{Reinforced Concrete: A Fundamental Approach}, 5th Edition, Upper Saddle River, New Jersey: Pearson Prentice Hall, 2005, page 23-28.
#'
#' @author Irucka Embry, Hans Werner Borchers for the interp1 and interp2 functions from pracma
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @seealso \code{\link{concr_mix_lightweight_strength}} for Concrete Mix Design for
#' Structural Lightweight Concrete
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # 'Example 3.1 Mixture Design of Normal-weight Concrete' from Nawy
#' # (page 23-28)
#' # Design a concrete mix for 4000 psi concrete strength, beam, and a maximum
#' # size of aggregate = 3/4 in, with Fineness Modulus of sand = 2.6, the dry
#' # rodded weight of aggregate = 100 lb/ft^3^, and a moisture content of 3\%
#' # for the coarse aggregate and 2\% for the fine aggregate.
#'
#' concr_mix_normal_strength(fc = 4000, max_size_aggr = 3 / 4, FM = 2.6,
#' dry_rod_wt_aggr = 100, mc_coarse = 3, mc_fine = 2, entrainment = "Nonair",
#' construction_type = "Reinforced Foundation walls and footings", slump_value
#' = "Maximum", exposure = "Nonair", trial_batch = "1 cubic yard")
#'
#'
#'
#'
#' @importFrom data.table setnames setattr := setDT between melt
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom round round_r3
#' @importFrom fpCompare %==%
#' @importFrom pracma interp1
#' @importFrom pracma interp2
#'
#'
#' @export
concr_mix_normal_strength <- function (fc, slump_use = NULL, max_size_aggr, FM, dry_rod_wt_aggr, mc_coarse, mc_fine, entrainment = c("Nonair", "Air"), construction_type = c("Reinforced Foundation walls and footings", "Plain footings and caissons", "Slabs, beams and reinforced walls", "Building Columns", "Pavements and slabs", "Heavy mass construction"), slump_value = c("Maximum", "Maximum + 1", "Minimum"), exposure = c("Nonair", "Mild", "Moderate", "Extreme"), trial_batch = c("1 cubic yard", "1 cubic foot", "0.5 cubic foot", "0.2 cubic foot", "All")) {


entrainment <- entrainment

construction_type <- construction_type

slump_value <- slump_value

exposure <- exposure

trial_batch <- trial_batch


# Check fc, max_size_aggr, FM, dry_rod_wt_aggr, mc_coarse, mc_fine, entrainment, construction_type, slump_value, exposure, and trial_batch
assert_that(qtest(fc, "N==1(0,)"), msg = "fc is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(max_size_aggr, "N==1(0,)"), msg = "max_size_aggr is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(FM, "N==1(0,)"), msg = "FM is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(dry_rod_wt_aggr, "N==1(0,)"), msg = "dry_rod_wt_aggr is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(mc_coarse, "N==1(0,)"), msg = "mc_coarse is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(mc_fine, "N==1(0,)"), msg = "mc_fine is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(entrainment, "S==1"), msg = "There is not an entrainment value selected for the entrainment parameters. Please specify either 'Nonair' or 'Air'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(construction_type, "S==1"), msg = "There is not a construction type value selected for the construction_type parameters. Please specify either 'Reinforced Foundation walls and footings', 'Plain footings and caissons', 'Slabs, beams and reinforced walls', 'Building Columns', 'Pavements and slabs', or 'Heavy mass construction'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(slump_value, "S==1"), msg = "There is not a slump value selected for the slump_value parameters. Please specify either 'Unknown', 'Maximum', 'Maximum + 1', or 'Minimum'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(exposure, "S==1"), msg = "There is not an exposure value selected for the exposure parameters. Please specify either 'Nonair', 'Mild', 'Moderate', or 'Extreme'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(trial_batch, "S==1"), msg = "There is not an trial batch value selected for the trial_batch parameters. Please specify either '1 cubic yard', '1 cubic foot', '0.5 cubic foot', '0.2 cubic foot', or 'All'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("Nonair", "Air") %in% entrainment)), msg = "The entrainment has not been identified correctly. Please try again.")
# only process with a specified entrainment and provide a stop warning if not

assert_that(isTRUE(any(c("Reinforced Foundation walls and footings", "Plain footings and caissons", "Slabs, beams and reinforced walls", "Building Columns", "Pavements and slabs", "Heavy mass construction") %in% construction_type)), msg = "The construction_type has not been identified correctly. Please try again.")
# only process with a specified construction type and provide a stop warning if not

assert_that(isTRUE(any(c("Maximum", "Maximum + 1", "Minimum") %in% slump_value)), msg = "The slump_value  has not been identified correctly. Please try again.")
# only process with a specified slump value and provide a stop warning if not

assert_that(isTRUE(any(c("Nonair", "Mild", "Moderate", "Extreme") %in% exposure)), msg = "The exposure has not been identified correctly. Please try again.")
# only process with a specified exposure and provide a stop warning if not

assert_that(isTRUE(any(c("1 cubic yard", "1 cubic foot", "0.5 cubic foot", "0.2 cubic foot", "All") %in% trial_batch)), msg = "The trial_batch has not been identified correctly. Please try again.")
# only process with a specified trial batch and provide a stop warning if not




# Tables
# Table 3.1 Recommended Slumps for Various Types of Construction Slump
slump <- data.table(V1 = c("Reinforced Foundation walls and footings", "Plain footings and caissons", "Slabs, beams and reinforced walls", "Building Columns", "Pavements and slabs", "Heavy mass construction"), V2 = c(3, 3, 4, 4, 3, 2), V3 = c(1, 1, 1, 1, 1, 1))
setnames(slump, c("Types of Construction", "Maximum Slump (in)", "Minimum Slump (in)"))


# Table 3.2 Approximate mixing water and air content for different slumps and Nominal maximum sizes of aggregate
# Water (lb/yd^3 of concrete for indicated Nominal Maximum Sizes of Aggregate)

# Nonair-Entrained Concrete
water_nonair <- data.table(V1 = c("1 to 2", "3 to 4", "6 to 7", "Approximate amount of entrapped air in nonair- entrained concrete (%)"), V2 = c(350, 385, 410, 3), V3 = c(335, 365, 385, 2.5), V4 = c(315, 340, 360, 2), V5 = c(300, 325, 340, 1.5), V6 = c(275, 300, 315, 1), V7 = c(260, 285, 300, 0.5), V8 = c(220, 245, 270, 0.3), V9 = c(190, 210, NA_real_, 0.2))
setnames(water_nonair, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in.", "1 in.", "1 1/2 in.", "2 in.", "3 in.", "6 in."))


# Air Entrained Concrete
water_air <- data.table(V1 = c("1 to 2", "3 to 4", "6 to 7"), V2 = c(305, 340, 365), V3 = c(295, 325, 345), V4 = c(280, 305, 325), V5 = c(270, 295, 310), V6 = c(250, 275, 290), V7 = c(240, 265, 290), V8 = c(205, 225, 280), V9 = c(180, 200, NA_real_))
setnames(water_air, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in.", "1 in.", "1 1/2 in.", "2 in.", "3 in.", "6 in."))


# Recommended average total air content (percent for level of exposure)
avg_air_content <- data.table(V1 = c("Mild exposure", "Moderate exposure", "Extreme exposure"), V2 = c(4.5, 6, 7.5), V3 = c(4, 5.5, 7), V4 = c(3.5, 5, 6), V5 = c(3, 4.5, 6), V6 = c(2.5, 4.5, 5.5), V7 = c(2, 4, 5), V8 = c(1.5, 3.5, 4.5), V9 = c(1, 3, 4))
setnames(avg_air_content, c("Slump (in.)", "3/8 in.", "1/2 in.", "3/4 in.", "1 in.", "1 1/2 in.", "2 in.", "3 in.", "6 in."))


# Table 3.3: Relationship between Water/Cement Ratio and Compressive strength
# Water/Cement Ratio, by mass (last 2 columns)
wc <- data.table(V1 = c(6000, 5000, 4000, 3000, 2000), V2 = c(0.41, 0.48, 0.57, 0.68, 0.82), V3 = c(NA_real_, 0.40, 0.48, 0.59, 0.74))
setnames(wc, c("Compressive strength at 28 days (psi)", "Nonair entrained Concrete", "Air-entrained Concrete"))


# Table 3.4 Volume of Coarse aggregate per unit volume of concrete
# Volume of dry-rodded coarse aggregate per unit volume of concrete for different fineness moduli of sand (last 3 columns)
vol_coarse <- data.table(V1 = c(3 / 8, 1 / 2, 3 / 4, 1, 1 + 1 / 2, 2, 3, 6), V2 = c(0.50, 0.59, 0.66, 0.71, 0.75, 0.78, 0.82, 0.87), V3 = c(0.48, 0.57, 0.64, 0.69, 0.73, 0.76, 0.80, 0.85), V4 = c(0.46, 0.55, 0.62, 0.67, 0.71, 0.74, 0.78, 0.83), V5 = c(0.44, 0.53, 0.60, 0.65, 0.69, 0.72, 0.76, 0.81))
setnames(vol_coarse, c("Maximum size of aggregate (in)", "2.40", "2.60", "2.80", "3.00"))


# Table 3.5: First estimate of weight of Fresh concrete
# First Estimate of Concrete Weight (lb/yd^3)
wt_concrete <- data.table(V1 = c(3 / 8, 1 / 2, 3 / 4, 1, 1 + 1 / 2, 2, 3, 6), V2 = c(3840, 3890, 3960, 4010, 4070, 4120, 4160, 4230), V3 = c(3690, 3760, 3840, 3900, 3960, 4000, 4040, 4120))
setnames(wt_concrete, c("Max. size of Aggregate (in)", "Nonair-entrained", "Air-Entrained"))


# Concrete Mix Design for Normal Strength Concrete
# Reference: Reinforced Concrete: A Fundamental Approach, 5th Edition, Edward G. Nawy, Prentice Hall

# Procedure:

if (missing(slump_use)) {

# 1. Decide on slump from the slump (Table 3.1), if not already known
# determine the slump

if (slump_value == "Maximum") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 2][[1]] # 1 for column 2 - Maximum slump

} else if (slump_value == "Maximum + 1") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 2][[1]] + 1 # 2 for column 2 - Maximum slump

} else if (slump_value == "Minimum") {

slump_use1 <- grep(construction_type, slump$"Types of Construction")

slump_use <- slump[slump_use1, 3][[1]] # 3 for column 3 - Minimum slump

}

} else if (!missing(slump_use)) {

slump_use <- slump_use

}


# check slump_use value
assert_that(qtest(slump_use, "N==1(0,)"), msg = "slump_use is 0, NA, NaN, Inf, -Inf, a string, or empty. Or contains more than 1 value. Please try again.")
# only process with finite values and provide an error message if the check fails


# 2. Decide on the maximum size of aggregate using the following guidelines:
# Maximum size not greater than 1/5 narrower dimension between forms
#			Not greater than 1/3 depth of slab
#			Not greater 3/4 of clear spacing between reinforcing bars


# 3. Decide on amount of water and air (Table 3.2)
# determine the water content
# look at the entrainment
# determine the total air content, if air entrained


if (entrainment == "Nonair") {

water_nonair_col_numeric <- gsub(" in.", "", colnames(water_nonair)[2:ncol(water_nonair)])

water_nonair_col_numeric <- unlist(lapply(water_nonair_col_numeric, frac_to_numeric))

water_nonair_column <- which(water_nonair_col_numeric %==% max_size_aggr)+1L


# determine the water content
if (between(slump_use, 1, 2)) {

water <- water_nonair[1, ..water_nonair_column][[1]] # Source 2

} else if(between(slump_use, 3, 4)) {

water <- water_nonair[2, ..water_nonair_column][[1]] # Source 2

} else if(between(slump_use, 6, 7)) {

water <- water_nonair[3, ..water_nonair_column][[1]] # Source 2

}

} else if (entrainment == "Air") {

water_air_col_numeric <- gsub(" in.", "", colnames(water_air)[2:ncol(water_air)])

water_air_col_numeric <- unlist(lapply(water_air_col_numeric, frac_to_numeric))

water_air_column <- which(water_air_col_numeric %==% max_size_aggr)+1L



# determine the water content
if (between(slump_use, 1, 2)) {

water <- water_air[1, ..water_air_column][[1]] # Source 2

} else if(between(slump_use, 3, 4)) {

water <- water_air[2, ..water_air_column][[1]] # Source 2

} else if(between(slump_use, 6, 7)) {

water <- water_air[3, ..water_air_column][[1]] # Source 2
}

if (exposure == "Mild") {

avg_air_content_col_numeric <- gsub(" in.", "", colnames(avg_air_content)[2:ncol(avg_air_content)])

avg_air_content_col_numeric <- unlist(lapply(avg_air_content_col_numeric, frac_to_numeric))

water_avg_air_column <- which(avg_air_content_col_numeric %==% max_size_aggr)+1L

total_air <- avg_air_content[1, ..water_avg_air_column][[1]] # Source 2


} else if(exposure == "Moderate") {

avg_air_content_col_numeric <- gsub(" in.", "", colnames(avg_air_content)[2:ncol(avg_air_content)])

avg_air_content_col_numeric <- unlist(lapply(avg_air_content_col_numeric, frac_to_numeric))

water_avg_air_column <- which(avg_air_content_col_numeric %==% max_size_aggr)+1L

total_air <- avg_air_content[2, ..water_avg_air_column][[1]] # Source 2


} else if(exposure == "Extreme") {

avg_air_content_col_numeric <- gsub(" in.", "", colnames(avg_air_content)[2:ncol(avg_air_content)])

avg_air_content_col_numeric <- unlist(lapply(avg_air_content_col_numeric, frac_to_numeric))

water_avg_air_column <- which(avg_air_content_col_numeric %==% max_size_aggr)+1L

total_air <- avg_air_content[3, ..water_avg_air_column][[1]] # Source 2

}
}


wc1 <- wc[, 1][[1]]
wc2 <- wc[, 2][[1]]
wc3 <- wc[-1, 3][[1]]

# 4. Select water/cement ratio w/c (Table 3.3)

if (entrainment == "Nonair") {

# determine the water/cement ratio

water_cement <- interp1(wc1, wc2, xi = fc, method = "linear")

} else if (entrainment == "Air") {

# determine the water/cement ratio

water_cement <- interp1(wc1, c(0, wc3), xi = fc, method = "linear")
# Replace NA with 0 for this portion only

}


# 5. Calculate cement amount ( = c/w * weight of water)

cement <- water / water_cement


# 6. Choose the amount of coarse aggregate (Table 3.4)

vol_coarse_col_numeric <- as.numeric(colnames(vol_coarse[, 2:ncol(vol_coarse)]))

z <- cbind(vol_coarse[, 2][[1]], vol_coarse[, 3][[1]], vol_coarse[, 4][[1]], vol_coarse[, 5][[1]])

gravel_yd3 <- interp2(x = vol_coarse[, 1][[1]], y = vol_coarse_col_numeric, Z = t(z), xp = max_size_aggr, yp = FM, method = "linear") # Source 1

gravel <- gravel_yd3 * 27 * dry_rod_wt_aggr


# 7. Calculate the amount of fine aggregate using Estimate table the estimated weight of fresh concrete (table 3.5) and the known weights of water, cement, and course aggregates

if (entrainment == "Nonair") {

# determine the water/cement ratio

concrete <- wt_concrete[which(max_size_aggr %==% wt_concrete[, 1]), 2][[1]]

} else if (entrainment == "Air") {

# determine the water/cement ratio
#
concrete <- wt_concrete[which(max_size_aggr %==% wt_concrete[, 1]), 3][[1]]

}


sand <- concrete - water - cement - gravel


# 8. Adjust for moisture content in course and fine aggregate.

net_sand <- (1 + mc_fine / 100) * sand

net_gravel <- (1 + mc_coarse / 100) * gravel

net_water <- water - mc_fine / 100 * sand - mc_coarse / 100 * gravel


# for 1 yd^3 of concrete
if (trial_batch == "1 cubic yard") {

return(list(cement = round_r3(cement, d = 2), sand = round_r3(net_sand, d = 2), gravel = round_r3(net_gravel, d = 2), water = round_r3(net_water, d = 2)))


# for 1 ft^3 of concrete
} else if (trial_batch == "1 cubic foot") {

return(list(cement = round_r3(cement / (3 ^ 3), d = 2), sand = round_r3(net_sand / (3 ^ 3), d = 2), gravel = round_r3(net_gravel / (3 ^ 3), d = 2), water = round_r3(net_water / (3 ^ 3), d = 2)))


# for 0.5 ft^3 of concrete
} else if (trial_batch == "0.5 cubic foot") {

return(list(cement = round_r3(cement * 0.5 / (3 ^ 3), d = 2), sand = round_r3(net_sand * 0.5 / (3 ^ 3), d = 2), gravel = round_r3(net_gravel * 0.5 / (3 ^ 3), d = 2), water = round_r3(net_water * 0.5 / (3 ^ 3), d = 2)))


# for 0.2 ft^3 of concrete
} else if (trial_batch == "0.2 cubic foot") {

return(list(cement = round_r3(cement * 0.2 / (3 ^ 3), d = 2), sand = round_r3(net_sand * 0.2 / (3 ^ 3), d = 2), gravel = round_r3(net_gravel * 0.2 / (3 ^ 3), d = 2), water = round_r3(net_water * 0.2 / (3 ^ 3), d = 2)))


# for All mixes of concrete
} else if (trial_batch == "All") {

concrete_mix <- list(cement = round_r3(cement, d = 2), sand = round_r3(net_sand, d = 2), gravel = round_r3(net_gravel, d = 2), water = round_r3(net_water, d = 2))
concrete_mix <- melt(setDT(concrete_mix), measure.vars = 1:4)
setnames(concrete_mix, 2, "V1")
concrete_mix[, `:=` (v1 = round_r3(V1 / (3 ^ 3), d = 2), v2 = round_r3(V1 * 0.5 / (3 ^ 3), d = 2), v3 = round_r3(V1 * 0.2 / (3 ^ 3), d = 2))]
setnames(concrete_mix, c("Materials", "Amount (lb) for 1 yd^3", "Amount (lb) for 1 ft^3", "Amount (lb) for 0.5 ft^3", "Amount (lb) for 0.2 ft^3"))


col.names <- c("Materials", "Amount (lb) for 1 yd^3", "Amount (lb) for 1 ft^3", "Amount (lb) for 0.5 ft^3", "Amount (lb) for 0.2 ft^3")

# code block below modified from data.table function
setattr(concrete_mix, "col.names", setnames(concrete_mix, col.names))
setattr(concrete_mix, "class", c("data.table", "data.frame"))
print(concrete_mix)

}

# 9. Trial mix
# 10. End

}
