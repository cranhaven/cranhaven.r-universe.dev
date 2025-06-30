## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  # eval = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.width=8, 
  fig.height=5, 
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(Morphoscape)

## -----------------------------------------------------------------------------
data("turtles")
data("warps")

str(turtles)
str(warps)

## -----------------------------------------------------------------------------
library(Morphoscape)

data("turtles")
data("warps")

str(turtles)
str(warps)

# Create an fnc_df object for downstream use
warps_fnc <- as_fnc_df(warps, func.names = c("hydro", "curve", "mech", "fea"))
str(warps_fnc)

## -----------------------------------------------------------------------------
# Create alpha-hulled grid for kriging
grid <- resample_grid(warps, hull = "concaveman", alpha = 3, plot = TRUE)
kr_surf <- krige_surf(warps_fnc, grid = grid)
kr_surf
plot(kr_surf)

## -----------------------------------------------------------------------------
# Create alpha-hulled grid for kriging
grid <- resample_grid(warps, hull = NULL, padding = 1.1)

# Do the kriging on the grid
kr_surf <- krige_surf(warps_fnc, grid = grid)
kr_surf
plot(kr_surf)

## -----------------------------------------------------------------------------
# Do kriging on the sample dataset
kr_surf <- krige_new_data(kr_surf, new_data = turtles)
kr_surf
plot(kr_surf)

## -----------------------------------------------------------------------------
# Above steps all in one:
kr_surf <- krige_surf(warps_fnc, hull = NULL, padding = 1.1,
                      new_data = turtles)
kr_surf
plot(kr_surf)

## -----------------------------------------------------------------------------
# Generate weights to search for optimal landscapes
weights <- generate_weights(n = 10, nvar = 4)
weights <- generate_weights(step = 0.05 , data = kr_surf)


## -----------------------------------------------------------------------------
# Calculate all landscapes; setting verbose = TRUE produces
# a progress bar
all_landscapes <- calc_all_lscps(kr_surf, grid_weights = weights)

## -----------------------------------------------------------------------------
# Calculate optimal landscapes by Group

table(turtles$Ecology)

wprime_by_Group <- calcWprimeBy(all_landscapes, by = ~Ecology)
wprime_by_Group <- calcWprimeBy(all_landscapes, by = turtles$Ecology)

wprime_by_Group
summary(wprime_by_Group)
plot(wprime_by_Group, ncol = 2)


## -----------------------------------------------------------------------------
# Calculate landscapes for one Group at a time

i <- which(turtles$Ecology == "T")
wprime_T <- calcGrpWprime(all_landscapes, index = i)
wprime_T
wprime_b <- calcGrpWprime(all_landscapes, Group == "box turtle")
wprime_b
plot(wprime_b)

wprime_all <- calcGrpWprime(all_landscapes)
wprime_all


## -----------------------------------------------------------------------------

# Test for differences between Group landscapes
tests <- multi.lands.grp.test(wprime_by_Group)
tests

# Calculate landscapes for one Group at a time
wprime_b <- calcGrpWprime(all_landscapes, Group == "box turtle")
wprime_b
wprime_t <- calcGrpWprime(all_landscapes, Group == "tortoise")
wprime_t

# Test for differences between Group landscapes
lands.grp.test(wprime_b, wprime_t)

