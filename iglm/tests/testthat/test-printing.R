test_that("iglm.object print and summary works as expected", {
  n_units <- 20
  neighborhood <- matrix(1, n_units, n_units)
  
  xyz_obj <- iglm.data(neighborhood = neighborhood, directed = FALSE)
  
  # Mock estimation results to trigger printing
  model <- iglm(
    formula = xyz_obj ~ edges(mode = "local") + attribute_y,
    coef = c(-2, 0.5),
    control = control.iglm(estimate_model = FALSE)
  )
  
  # Test return value when model is not estimated (should be NULL)
  expect_null(model$print())
  expect_null(model$summary())

  # Manually inject mock results since we skipped estimation
  # Use the results$update() method
  model$results$update(
    coefficients_path = matrix(c(-2, 0.5), nrow = 1),
    var = diag(c(0.1, 0.05)),
    estimated = TRUE
  )
  
  # We also need to set .coef in the iglm.object itself
  # Use the public set_coefficients() method
  model$set_coefficients(
    coef = matrix(c(-2, 0.5), ncol = 1, 
                  dimnames = list(c("edges(mode = 'local')", "attribute_y"), NULL))
  )
  
  assign(".time_estimation", structure(1.23456, units = "secs", class = "difftime"), 
         envir = model$.__enclos_env__$private)
  
  # Test return value of print and summary (should be the coefficient matrix)
  coef_sum <- model$summary()
  expect_true(is.matrix(coef_sum))
  expect_equal(dim(coef_sum), c(2, 4))
  expect_equal(colnames(coef_sum), c("Estimate", "S.E.", "t-value", "Pr(>|t|)"))
  expect_equal(rownames(coef_sum), c("edges(mode = 'local')", "attribute_y"))

  coef_print <- model$print()
  expect_true(is.matrix(coef_print))
  expect_equal(dim(coef_print), c(2, 4))

  # Test default summary printing
  out_sum <- capture.output(model$summary())
  expect_true(any(grepl("Results:", out_sum)))
  expect_true(any(grepl("edges", out_sum)))
  expect_false(any(grepl("Formula:", out_sum))) # summary() sets print.formula = FALSE by default
  
  # Test print with formula
  out_print <- capture.output(model$print(print.formula = TRUE))
  expect_true(any(grepl("Formula:", out_print)))
  
  # Test digits argument
  out_digits <- capture.output(model$summary(digits = 2))
  # Check if "1.2" (from 1.23456) is in the output for Time
  expect_true(any(grepl("Time for estimation: 1.2 secs", out_digits)))
  
  # Test disabling results
  out_no_coef <- capture.output(model$print(print.coefmat = FALSE))
  expect_false(any(grepl("Estimate", out_no_coef)))
  
  # Test eps.Pvalue
  # Inject a very small p-value
  model$set_coefficients(
    coef = matrix(c(-200, 0.5), ncol = 1,
                  dimnames = list(c("edges(mode = 'local')", "attribute_y"), NULL))
  )
  out_eps <- capture.output(model$summary(eps.Pvalue = 0.5))
  # With eps.Pvalue = 0.5, most small p-values should be shown as <0.5 or < 0.5
  expect_true(any(grepl("<\\s*0\\.5", out_eps)))
})

test_that("summary and print show labeled names by default and canonical names when canonical_names = TRUE", {
  n_units <- 20
  neighborhood <- matrix(1, n_units, n_units)

  xyz_labeled <- iglm.data(
    neighborhood = neighborhood, directed = TRUE,
    label_x = "republican", label_y = "turnout", label_z = "advice"
  )

  # Formula strictly uses canonical names
  model <- iglm(
    formula = xyz_labeled ~ edges(mode = "local") + attribute_x + attribute_y + spillover_xy,
    coef = c(-2, 0.5, 0.3, 0.1),
    control = control.iglm(estimate_model = FALSE)
  )

  # Formulas with non-canonical names fail as expected
  expect_error(
    iglm(formula = xyz_labeled ~ attribute_republican, control = control.iglm(estimate_model = FALSE)),
    "Term 'attribute_republican' not recognized"
  )

  # Mock estimation results
  model$results$update(
    coefficients_path = matrix(c(-2, 0.5, 0.3, 0.1), nrow = 1),
    var = diag(c(0.1, 0.05, 0.05, 0.02)),
    estimated = TRUE
  )
  model$set_coefficients(
    coef = matrix(c(-2, 0.5, 0.3, 0.1), ncol = 1,
                  dimnames = list(c("edges(mode = 'local')", "attribute_x", "attribute_y", "spillover_xy"), NULL))
  )

  # Labeled names in summary (default: canonical_names = FALSE)
  sum_labeled <- model$summary()
  expect_equal(rownames(sum_labeled), c("edges(mode = 'local')", "attribute_republican", "attribute_turnout", "spillover_republican_turnout"))

  out_sum_labeled <- capture.output(model$summary())
  expect_true(any(grepl("attribute_republican", out_sum_labeled)))
  expect_true(any(grepl("attribute_turnout", out_sum_labeled)))
  expect_true(any(grepl("spillover_republican_turnout", out_sum_labeled)))

  # Canonical names in summary when canonical_names = TRUE
  sum_canonical <- model$summary(canonical_names = TRUE)
  expect_equal(rownames(sum_canonical), c("edges(mode = 'local')", "attribute_x", "attribute_y", "spillover_xy"))

  out_sum_canonical <- capture.output(model$summary(canonical_names = TRUE))
  expect_true(any(grepl("attribute_x", out_sum_canonical)))
  expect_true(any(grepl("attribute_y", out_sum_canonical)))
  expect_true(any(grepl("spillover_xy", out_sum_canonical)))

  # Print with canonical_names = FALSE (default) and TRUE
  out_print_labeled <- capture.output(model$print())
  expect_true(any(grepl("attribute_republican", out_print_labeled)))

  out_print_canonical <- capture.output(model$print(canonical_names = TRUE))
  expect_true(any(grepl("attribute_x", out_print_canonical)))
})
