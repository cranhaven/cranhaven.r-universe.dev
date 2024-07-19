test_kin_visc_water <- function() {

expect_equal(kin_visc_water(mu = 2.09 * 10 ^ -8, rho = 1.937, rho_units = "slug/ft^3", mu_units = "slug/ft/s"), 1.078988e-08, tolerance = 1e-06)
expect_error(kin_visc_water(mu = 34, rho = 0, rho_units = "kg/m^3", mu_units = "Pa*s or kg/m/s"))

  invisible(NULL)
}

test_kin_visc_water()
