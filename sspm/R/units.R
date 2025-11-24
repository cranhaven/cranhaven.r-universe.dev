
# Setting functions -------------------------------------------------------

set_biomass <- function(x, units = "kg", target_units = "kg", mode = "standard"){
  spm_set_units(x, units, target_units, mode)
}

set_biomass_density <- function(x, units = "kg/km^2", target_units = "kg/km^2",
                                mode = "standard"){
  spm_set_units(x, units, target_units, mode)
}

spm_set_units <- function(x, units, target_units, mode = "standard"){
  units(x) <- units
  x <- units::set_units(x, target_units, mode = mode)
  return(x)
}

# Validation functions ----------------------------------------------------

is_units <- function(x){
  checkmate::test_class(x, "units")
}

is_biomass <- function(x){
  is_units(x) && has_biomass_units(x)
}

# is_area <- function(x){
#   is_units(x) && has_area_units(x)
# }

# is_biomass_density <- function(x){
#   is_units(x) && has_biomass_density_units(x)
# }

has_biomass_units <- function(x){
  if (is_units(x)){
    num <- units(x)$numerator
    den <- units(x)$denominator
    (length(num) == 1) && (length(den) == 0) && (num == "kg")
  } else {
    FALSE
  }
}

# has_biomass_density_units <- function(x){
#   if (is_units(x)){
#     num <- units(x)$numerator
#     den <- units(x)$denominator
#     (length(num) == 1) && (length(den) == 2) && (num == "kg") && (den == c("km", "km"))
#   } else {
#     FALSE
#   }
# }

# has_area_units <- function(x){
#   if (is_units(x)){
#     num <- units(x)$numerator
#     den <- units(x)$denominator
#     (length(num) == 2) && (length(den) == 0) && (num == c("km", "km"))
#   } else {
#     FALSE
#   }
# }
