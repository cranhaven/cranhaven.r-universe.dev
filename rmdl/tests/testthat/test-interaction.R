test_that('interaction estimates can be made', {
	# Survival model
	library(survival)

	# Since sex is a two level structure, interaction must happen at both levels
	x <-
		fmls(Surv(time, status) ~ .x(age) + ph.karno + .i(sex),
				 pattern = 'sequential') |>
		fit(.fn = coxph, data = lung, raw = FALSE) |>
		suppressMessages()

	mt <- model_table(int_sex = x, data = lung)
	expect_s3_class(mt, 'mdl_tbl')
	expect_equal(nrow(mt), 3)
	expect_error(estimate_interaction(mt), regexp = "single row")

	object <- dplyr::filter(mt, interaction == 'sex')
	expect_equal(nrow(object), 1)
	expect_error(
	  estimate_interaction(object, exposure = "ph.karno"),
	  regexp = "exposure"
	)
	expect_error(
	  estimate_interaction(object, exposure = "age", interaction = "ph.karno"),
	  regexp = "interaction"
	)

	i <- estimate_interaction(
	  object,
	  exposure = "age",
	  interaction = "sex",
	  conf_level = 0.95
	)

	expect_length(i, 6)
	expect_equal(nrow(i), 2)
	expect_named(i, c("estimate", "conf_low", "conf_high", "p_value", "nobs", "level"))

})

test_that("interaction for multiple levels can be performed", {

	dat <-
		mtcars |>
		dplyr::mutate(cyl = factor(cyl))

	lm(mpg ~ am*cyl, data = dat) |>
		summary()

})
