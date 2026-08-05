fit_coeffs <- data.frame(
  estimate   = c(0.001280319, 0.021038724, 0.063032534),
  std.error  = c(0.0004714055, 0.0051576170, 0.0040073856),
  statistic  = c(2.715961, 4.079156, 15.729091),
  p.value    = c(6.608367e-03, 4.519949e-05, 9.557291e-56),
  row.names =  c("coeff_C", "coeff_alpha", "coeff_beta")
)



project_yield(yield = 0.67,
              type = "estimate",
              general_fit_coeffs = fit_coeffs[, "estimate"],
              general_fit_var_cov_mat = NULL,
              protracted_g_value = 1,
              conf_int = 0.95)
