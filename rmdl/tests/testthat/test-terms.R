# Term vector implementation ----

test_that("`tm` objects can be generated and printed", {
	# Basic generation
	expect_length(new_tm(), 0)
	expect_s3_class(new_tm(), "tm")
	expect_output(print(new_tm()), "<term\\[0\\]>")
	expect_length(tm(), 0)
	expect_s3_class(tm(), "tm")
	expect_output(print(tm()), "<term\\[0\\]>")
	expect_equal(tm(), tm(tm()))


	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(tm()) |>
			print() |>
			expect_output("<tm>")
	}
})

test_that("new `tm` can be made from character/atomic components", {

	ty <- tm(
		x = "Y",
		role = "outcome",
		label = "Dependent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tx <- tm(
		"X",
		role = "exposure",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tm <- tm(
		"M",
		role = "mediator",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tc <- tm(
		"C",
		role = "confounder",
		label = "Confounder",
		description = "Artificially created",
		distribution = "ordinal",
		type = "categorical"
	)

	tp <- tm(
		"P",
		role = "predictor",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "ordinal",
		type = "categorical"
	)

	ts <- tm(
		"S",
		role = "strata",
		label = "Stratification Variable",
		description = "Levels for data set",
		distribution = "binary",
		type = "categorical",
	)

	ti <- tm(
		"I",
		role = "interaction",
		label = "Interaction Variable",
		description = "Interaction for the exposure variable",
		distribution = "binary",
		type = "categorical",
	)

	t <- c(ty, tx, tm, tp, tc, ts, ti)

	expect_length(t, 7)
	expect_true(is_tm(t))
	expect_length(tm(character()), 0)

})

test_that("terms can be generated from a formula", {

	# Formula stump
	x <- ~ input
	t <- tm(x)
	expect_length(t, 1)

	# Simple formulas
	f1 <- output ~ input + modifier
	f2 <- output ~ .x(input) + modifier
	f3 <- output ~ .x(input) + log(modifier) + log(variable) + another
	expect_equal(lhs(f1), lhs(f2))
	expect_match(rhs(f2), ".x", all = FALSE)

	group = type = distribution = description = transformation = formula()
	rl <- input ~ "exposure"
	lb <- list(output ~ "The Final Outcome", input ~ "The First Mover")
	allArgs <-
		list(
			role = rl,
			label = lb,
			group = group,
			type = type,
			distribution = distribution,
			description = description,
			transformation = transformation
		)

	tms <- tm(f1, role = rl, label = lb)
	d <- vec_data(tms)
	expect_equal(d$role[d$term == "input"], "exposure")

	tms <- tm(f3, role = rl, label = lb)
	expect_s3_class(tms, "tm")
	expect_length(tms, 5)
	expect_equal(vec_data(tms)$label[1], "The Final Outcome")
	expect_length(tm(formula()), 0)

})

test_that("term coercion works", {

	f <- fmls(.o(wicked) ~ .x(witch) + west)
	t <- tm(f)
	expect_s3_class(t, "tm")

})

# Special terms ----

test_that("mediation terms will be assessed correctly", {

	x <- .o(output) ~ .x(input) + .m(mediator) + random
	t <- tm(x)
	expect_equal(describe(t, "role")$mediator, "mediator")

	x <- output ~ input + .m(mediator) + random
	expect_warning(t <- tm(x))
	expect_equal(describe(t, "role")$mediator, "predictor")

})

test_that("term groups can be established", {

	x <- witch ~ glinda + wicked + west
	role = label = type = distribution = description = transformation = formula()
	group <- list(wicked ~ 1, west ~ 1)
	allArgs <-
		list(
			role = role,
			label = label,
			group = group,
			type = type,
			distribution = distribution,
			description = description,
			transformation = transformation
		)

	# Group by term implementation external to formula
	t <- tm(x, group = group)
	d <- vec_data(t)
	expect_equal(d$group[d$term == "wicked"], 1)

	### Group term implementation using shortcuts

	# Expect .g to become .g0, and .g1 to be g1
	x1 <- witch ~ west + .g(green) + .g(wicked)
	x2 <- witch ~ west + .g(green) + .g1(wicked)
	t1 <- tm(x1)
	t2 <- tm(x2)
	expect_equal(describe(t1, "group")$green, describe(t2, "group")$green)
	expect_equal(describe(t2, "group")$wicked, 1)

	# Expect appropriate group levels
	x1 <- witch ~ west + .g1(green) + .g1(wicked)
	x2 <- witch ~ west + .g1(green) + .g2(wicked)
	t1 <- tm(x1)
	t2 <- tm(x2)
	expect_equal(describe(t1, "group")$green, describe(t2, "group")$green)
	expect_equal(describe(t2, "group")$wicked, 2)
})

test_that("interaction terms are appropriately made", {

	# From formula
	f <- witch ~ green + wicked*west
	expect_length(tm(f), 5)

	# Expanded formula
	x <- witch ~ wicked + west + wicked:west + green
	t <- tm(x)
	expect_length(t, 5)
	expect_equal(describe(t, "role")$'wicked:west', "interaction")

	# Role-based interaction
	x <- witch ~ .x(wicked) + green + .i(west)
	role = label = group = type = distribution = description = transformation =
		formula()
	expect_message(t <- tm(x))
	expect(length(t), 5)

	# Interaction role must be given to the correct term
	x <- wicked ~ .x(witch) + .i(green)
	tmTab <- vec_proxy(tm(x))
	expect_equal(tmTab$role[tmTab$term == "green"], "interaction")
	expect_equal(tmTab$role[tmTab$term == "witch:green"], "interaction")

	# Multiple interactions
	# Two messages about two interaction terms
	expect_message(expect_message(t <- tm(mpg ~ .x(hp) + .i(am) + .i(vs))))
	expect_length(t, 6)

	# Multiple exposures with interaction
	# Two messages for two exposures
	x <- mpg ~ .x(hp) + .x(cyl) + .i(am)
	expect_message(expect_message(t <- tm(x)))
	expect_length(t, 6)

	# Most importantly, an interaction term has a grouping
	# Groups must "go" together in the sequence of terms
	x <- wicked ~ .x(witch) + .i(west) + green
	t <- tm(x)
	expect_equal(vec_data(t)$term,
									c("wicked", "witch", "west", "witch:west", "green"))



})

# Term helpers ----

test_that("terms can be found and updated and attributes can be found", {

	object <- tm(output ~ input + .c(modifier))
	t <- filter(object, role == "outcome")
	expect_length(t, 1)
	expect_equal(describe(t, "role")[[1]], "outcome")

	dots <- list(
		role = input ~ "exposure",
		label = list(output ~ "The Final Outcome", input ~ "The First Mover")
	)

	x <- update(
		object,
		role = input ~ "exposure",
		label = list(output ~ "The Final Outcome", input ~ "The First Mover")
	)

	y <- update(object, dots)

	expect_equal(x, y)
	expect_equal(vec_data(x)$role, c("outcome", "exposure", "confounder"))
})

