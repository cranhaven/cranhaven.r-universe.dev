test_that("model tables can be initialized/created", {

	# New model table column requirements
	model_id = character()
	formula_id = numeric()
	data_id = character()
	name = character()
	model = character()
	formula = character()
	outcome = character()
	exposure = character()
	mediator = character()
	interaction = character()
	strata = character()
	level = factor()
	model_parameters = list()
	model_summary = list()
	fit_status = logical()
	formulaMatrix = data.frame()
	termTable = data.frame()
	dataList = list()

	# Empty builds should fail
	expect_error(mdl_tbl())
	expect_error(new_model_table())

})

test_that("model constructors work for initialization", {

	x <- fit(fmls(mpg ~ wt + hp + .s(am)), .fn = lm, data = mtcars, raw = FALSE)
	expect_length(x, 2)

	# Will only handle first model
	m <- construct_table_from_models(x)
	expect_s3_class(m, "mdl_tbl")
	expect_length(m, 15)
	expect_equal(nrow(m), 1) # Only one strata level at a time

})

test_that("can handle list of models appropriately", {

	x <-
		fit(fmls(mpg ~ wt + hp + .s(am)),
				.fn = lm,
				data = mtcars,
				raw = FALSE)
	y <-
		fit(
			fmls(am ~ disp + cyl),
			.fn = glm,
			family = "binomial",
			data = mtcars,
			raw = FALSE
		)

	# Test if unnamed list of multiple objects
	dots <- list(x, y)

	z <- model_table(dots)
	expect_s3_class(z, "mdl_tbl")
	expect_output(print(z), "<mdl_tbl>")
	expect_equal(nrow(z), 3)
	expect_length(z, 15)
	expect_length(attr(z, "termTable")$term, 7)
	expect_length(unique(attr(z, "termTable")$term), 6)
	expect_length(attr(z, "formulaMatrix"), 6)

	# Test if single unnamed object
	dots <- list(y)
	z <- model_table(dots)
	expect_true(is.na(z$name))

	# Test if single named object
	dots <- list(single = x)
	z <- model_table(dots)
	expect_true(unique(z$name) == "single")

	# Test if multiple named objects
	dots <- list(linear = x, log = y)
	z <- model_table(dots)
	expect_equal(unique(z$name), c("linear", "log"))

	# Test for mixed naming of list of objects
	dots <- list(x, log = y)
	z <- model_table(dots)
	expect_equal(unique(z$name), c(NA, "log"))

})

test_that("correct number of rows are generated", {

	f <- fmls(mpg + wt + hp ~ .x(cyl) + vs + carb + am)
	m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
	x <- model_table(m)
	expect_equal(nrow(x), 3)
	expect_equal(nrow(x), length(m))

})


test_that("formulas can be input into a model table", {

	f <- mpg ~ wt + hp + am
	x <- fmls(f, pattern = "sequential")
	m <- construct_table_from_formulas(list(x))
	expect_s3_class(m, "mdl_tbl")
	expect_length(m, 15)
	expect_equal(nrow(m), 3)

})

test_that("dplyr compatibility", {

	m1 <-
		fit(fmls(mpg ~ wt + hp + .s(am)),
				.fn = lm,
				data = mtcars,
				raw = FALSE)
	m2 <-
		fit(
			fmls(vs ~ .x(mpg)),
			.fn = glm,
			family = "binomial",
			data = mtcars,
			raw = FALSE
		)

	# MPG is an exposure and an outcome in different formulas
	# Should be able to use reconstruct methods to update scalar attributes
	x <- model_table(m1, m2)
	expect_equal(model_table(list(m1, m2)), x)
	y <- x[1:2, ]
	a <- attributes(model_table_reconstruct(x, y))
	expect_length(a$formulaMatrix, 3)
	expect_equal(nrow(a$formulaMatrix), 2)
	expect_length(a$termTable$term, 4)
	expect_false("vs" %in% a$termTable$term)
	expect_equal(attributes(dplyr_reconstruct(x, y)), a)

	# Would want attributes to downscale with less information present
	f <- fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential")
	m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
	x <- model_table(m)

	y <- filter(x, formula_call == "mpg ~ wt")
	a <- attributes(y)
	expect_length(a$termTable$term, 3) # mpg wt (strata = am)

})


test_that("attributes of models will adjust appropriately", {

	# Sequential/stratified models
	m1 <-
		fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table()
	expect_length(m1, 15)
	expect_equal(nrow(m1), 6)
	expect_length(attr(m1, "formulaMatrix"), 4)
	expect_equal(nrow(attr(m1, "termTable")), 5)

	m2 <-
		fmls(wt ~ mpg + cyl, pattern = "parallel") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table()

	# Combining tables
	m3 <- vec_c(m1, m2)
	expect_s3_class(m3, "mdl_tbl")
	expect_equal(nrow(m3), 8)
	expect_length(attr(m3, "formulaMatrix"), 4)
	expect_equal(nrow(attr(m3, "termTable")), 7)

	# Filtering tables
	m4 <- filter(m3, outcome == "wt")
	expect_s3_class(m4, "mdl_tbl")
	expect_equal(nrow(m4), 2)
	expect_length(attr(m4, "formulaMatrix"), 3)

	# TODO
	# STRATA ACCIDENTALLY INCLUDED, MUST BE REMOVED
	#expect_true(length(attr(m4, 'termTable')$term) == 3)

})

test_that("data can be attached to a model table", {

	# Should be able to attach data secondarily
	x <-
		fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table()

	y <- attach_data(x, data = mtcars)
	z <- attach_data(y, data = lung)
	dat <- attr(z, "dataList")
	expect_length(dat, 2)
	expect_named(dat, c("mtcars", "lung"))

	# Model should also take data directly
	x <-
		fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE)

	m <- model_table(x, data = mtcars)
	dat <- attr(m, "dataList")
	expect_named(dat, "mtcars")
	expect_length(dat, 1)

})

test_that("table can be simplified or flattened", {

	library(survival) # Using lung data
	f <- Surv(time, status) ~ ph.karno + meal.cal + cluster(sex)
	object <- fmls(f, pattern = 'sequential')
	m <- fit(object, .fn = coxph, data = lung, raw = FALSE)
	x <- model_table(m)
	y <- flatten_models(x)

	expect_s3_class(flatten_models(x), "data.frame")
	expect_equal(min(y$number), 1)
	expect_equal(max(y$number), 3)
	expect_type(y$var_cov[[1]], 'double')
	expect_true(inherits(y$var_cov[[1]], 'matrix'))
	
	# Now for multiple tables and flatten_model selections
	m1 <- 
	  fmls(am ~ wt) |>
	  fit(.fn = glm, family = "binomial", data = mtcars, raw = FALSE)
	
	m2 <-
	  fmls(am ~ wt) |>
	  fit(.fn = glm, family = "binomial", data = mtcars, raw = FALSE)
	
	mt <- model_table(log = m1, exp = m2)
	fm <- flatten_models(mt, exponentiate = TRUE, which = "exp")
	
	expect_equal(exp(fm$estimate[2]), fm$estimate[4])
	
})

test_that("model table can be filtered", {

	# Two messages here
	expect_message(expect_message(
		f <- vec_c(
			fmls(hp + mpg ~ .x(wt) + .i(am) + cyl),
			fmls(hp + mpg ~ .x(wt) + .i(vs) + cyl)
		)
	))
	m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
	object <- model_table(linear = m, data = mtcars)

	# Filtering example
	x <- object
	to <- object[object$outcome == "hp", ]
	expect_length(to$outcome, 2)

	# Filtering
	obj <-
		object |>
		dplyr::filter(outcome == "hp")

	expect_length(obj$outcome, 2)
})

test_that("models with interactions can be made", {

})

