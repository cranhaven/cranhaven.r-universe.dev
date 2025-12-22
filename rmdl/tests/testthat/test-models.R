test_that("empty models can be made/initialized", {

	data("mtcars")

	# Initialization
	expect_s3_class(new_model(), "mdl")
	expect_length(new_model(), 0)
	expect_equal(suppressMessages(mdl()), new_model())

	# Output
	expect_output(print(mdl()), "<model\\[0\\]>")

	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(mdl()) |>
			print() |>
			expect_output("<mdl>")
	}
})


test_that("pre-specified models of type character can be made", {

	x <- stats::lm(mpg ~ wt + hp, data = mtcars)
	f <- stats::formula(x) |> fmls()
	pe <- possible_tidy(x)
	si <- possible_glance(x)
	dn <- "mtcars"

	m <- mdl.character(
		x = "lm",
		formulas = f,
		parameter_estimates = pe,
		data_name = "mtcars"
	)

	expect_s3_class(m, "mdl")
	expect_equal(nrow(field(m, "parameterEstimates")[[1]]), 3)

})

test_that("simple regression models can be built", {

	# Linear
	x <- lm(mpg ~ wt + hp, model = FALSE, data = mtcars)
	m1 <- mdl(x)
	expect_s3_class(m1, "mdl")
	expect_length(m1, 1)
	expect_output(print(m1, "<model\\[1\\]>"))

	# Binomial...
	x <- glm(am ~ wt + vs, family = "binomial", data = mtcars)
	m2 <- suppressWarnings(mdl(x))
	expect_s3_class(m2, "mdl")
	expect_length(m2, 1)
	expect_type(field(m2, "modelArgs"), "list")
	expect_named(field(m2, "modelArgs")[[1]], "family")
	expect_equal(field(m2, "modelArgs")[[1]]$family, "binomial")

	# Vectorization
	m3 <- vec_c(m1, m2)
	expect_s3_class(m3, "mdl")
	expect_length(m3, 2)
	expect_output(print(m3, "<model\\[2\\]>"))

})

test_that("cox models can be made from survival package", {

	# Survival with coxph
	cancer <- survival::lung
	x <- survival::coxph(survival::Surv(time, status) ~ age + ph.karno, data = cancer)
	m <- mdl(x)

	expect_length(vec_proxy(m), 6)
	expect_length(fields(m), 6)

})

test_that("competing risk models can be made", {

	skip_if_not_installed("cmprsk")
	skip_if_not_installed("tidycmprsk")

	dat <- na.omit(survival::mgus2)
	time <- with(dat, ifelse(pstat == 0, futime, ptime))
	status <- with(dat, ifelse(pstat == 0, 2 * death, 1))
	status <- factor(status, levels = 0:2, labels = c("censor", "pcm", "death"))
	dat$time <- time
	dat$status <- status

	# Requires `crr` from {cmprsk}
	x <- cmprsk::crr(
		ftime = dat$time,
		fstatus = dat$status,
		cov1 = stats::get_all_vars(~ mspike, data = dat),
		failcode = "death",
		cencode = "censor"
	)

	expect_s3_class(x, "crr")

	# Requires `crr` from {tidycmprsk}
	x <- tidycmprsk::crr(
		survival::Surv(time, status) ~ sex + age,
		data = dat,
		failcode = "death"
	)

	expect_s3_class(x, "tidycrr")
})

