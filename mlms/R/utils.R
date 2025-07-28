# Function to calculate the specific weight of water in lb/ft^3, where temperature is in degC ----
calc_specific_weight <- function(temp) {
  checkmate::assert_numeric(temp)
  rho <- 1000 * (1 - (temp + 288.9414) / (508929.2 * (temp + 68.12963)) * (temp - 3.9863)^2) # kg/m^3
  sw <- rho * 9.80665 # N/m^3 or kg/(m^2*s^2)
  sw * 0.22480894387 / 35.314666721
}


# Function to calculate the pressure head in ft, where pressure is in absolute psi and temp in degC ----
calc_press_head <- function(press_va, baro_va, temp_va) {
  checkmate::assert_numeric(press_va)
  checkmate::assert_numeric(baro_va)
  checkmate::assert_numeric(temp_va)
  psf <- (press_va - baro_va) * 144 # lb/ft^2
  psf / calc_specific_weight(temp_va)
}


# Function for rounding of numbers using the USGS method ----
round_usgs <- function(x, digits = 0) {
  checkmate::assert_numeric(x)
  checkmate::assert_int(digits)
  z <- abs(x) * 10^digits + 0.5 + sqrt(.Machine$double.eps)
  trunc(z) / 10^digits * sign(x)
}


# Function to change null values to NA ----
null2na <- function(x) {
  lapply(x, FUN = function(x) if (is.null(x)) NA else x)
}


# Function to change null values to NA recursively ----
null_to_na <- function(x) {
  if (!is.list(x)) return(x)
  null2na(x) |>
    lapply(FUN = null_to_na)
}


# Function to get the p-value from a linear model ----
# code adapted from a blog post by Stephen Turner,
# accessed on 2023-08-09 at
# https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
get_p_value <- function (x) {
  checkmate::assert_class(x, "lm")
  f <- summary(x)$fstatistic
  p <- stats::pf(f[1], f[2], f[3], lower.tail = FALSE)
  attributes(p) <- NULL
  as.numeric(p)
}


# Function to get pressure metadata ----
get_press_metadata <- function(vars = NULL) {
  checkmate::assert_character(vars, any.missing = FALSE, null.ok = TRUE)
  l <- list(
    "total_head_va" = list(
      "var_ds" = "Hydraulic head",
      "digits" = 1
    ),
    "temp_va" = list(
      "var_ds" = "Fluid temperature",
      "digits" = 2
    ),
    "baro_va" = list(
      "var_ds" = "Atmospheric pressure",
      "digits" = 3
    ),
    "press_va" = list(
      "var_ds" = "Absolute fluid pressure",
      "digits" = 2
    )
  )
  if (is.null(vars)) return(l)
  vars <- match.arg(vars, choices = names(l), several.ok = TRUE)
  l[vars]
}
