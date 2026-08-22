library(testthat)
library(vartest)

# Validation framework
# The unit tests use three complementary layers of evidence:
# 1. Technical validation checks the returned object structure,
#    numeric output types, and admissible p-value range.
# 2. Statistical correctness validation compares both the test statistic
#    and p-value with an established reference implementation or with an
#    independent implementation of the published formula.
# 3. Reproducibility/regression validation checks whether previously reported
#    results for the fixed iris dataset remain unchanged across revisions.
# 4. Robustness validation checks whether invalid inputs and unsupported
#    argument values are rejected instead of producing misleading results.

# expect_valid_vht function verifies that each variance-homogeneity test returns a valid
# 'vht' object with the documented components and numerically valid outputs.

expect_valid_vht <- function(result) {
  expect_s3_class(result, "vht")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.numeric(result$statistic))
  expect_s3_class(result$data, "data.frame")
  expect_s3_class(result$formula, "formula")
}

# expect_matches_reference function compares the package result with an external reference 
# package or an independently coded formula. 

expect_matches_reference <- function(result, statistic, p.value, tolerance = 1e-10) {
  expect_equal(unname(result$statistic), unname(statistic), tolerance = tolerance)
  expect_equal(result$p.value, as.numeric(p.value), tolerance = tolerance)
}

# Common data preparation for independent reference calculations.
# The response is median-centered within each species where required by the method.

validation_data <- transform(iris, y_centered = Sepal.Length - ave(Sepal.Length, Species, FUN = median))
validation_data <- validation_data[order(validation_data$y_centered), , drop = FALSE]
n <- nrow(validation_data)

# expect_vht_error function verifies that invalid inputs are rejected.
# The optional regular expression can be used to check the error message.

expect_vht_error <- function(fun, data, regexp = NULL, formula = y ~ group, ...) {
  arguments <- c(
    list(formula = formula, data = data),
    list(...)
  )
  
  expect_error(
    do.call(fun, arguments),
    regexp = regexp
  )
}

test_that("select_vartest works", {
  
  result <- select_vartest(Sepal.Length ~ Species, data = iris)
  # Technical checks: 
  expect_s3_class(result, "data.frame")
  expect_true(is.numeric(result$power))
  expect_true(all(result$power >= 0 & result$power <= 1))
  expect_true(is.numeric(result$typeIerror))
  expect_true(all(result$typeIerror >= 0 & result$typeIerror <= 1))
  expect_true(is.numeric(result$adjpower))
  expect_true(all(result$adjpower >= 0 & result$adjpower <= 1))

})

test_that("Bartletts test works", {
  result <- bartletts.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- stats::bartlett.test(Sepal.Length ~ Species, data = iris) 
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})

test_that("Ansari test works", {
  result <- ansari.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- coin::ansari_test(y_centered ~ Species, data = validation_data, ties.method = "mid-ranks", distribution = "asymptotic") 
  expect_matches_reference(result, coin::statistic(validation), coin::pvalue(validation))
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.009)
})

test_that("Capon test works", {
  result <- capon.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation_data$capon_score <- qnorm(ppoints(n, a = 3 / 8))^2
  validation <- coin::oneway_test(capon_score ~ Species, data = validation_data, distribution = "asymptotic")
  expect_matches_reference(result, coin::statistic(validation), coin::pvalue(validation))
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.006) 
})

test_that("David Barton test works", {
  result <- david.barton.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  r <- rank(validation_data$y_centered, ties.method = "average")
  validation_data$db_score <- abs(r - (n + 1) / 2) + 1 / (2 - n %% 2)
  validation <- coin::oneway_test(db_score ~ Species, data = validation_data, distribution = "asymptotic")
  expect_matches_reference(result, coin::statistic(validation), coin::pvalue(validation))
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.009)
})

test_that("Duran test works", {
  result <- duran.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation_data$duran_score <-rank(abs(validation_data$y_centered), ties.method = "average")^2
  validation <- coin::oneway_test(duran_score ~ Species, data = validation_data, distribution = "asymptotic")
  expect_matches_reference(result, coin::statistic(validation), coin::pvalue(validation))
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.008)
})

test_that("Fligner-Killeen test works", {
  result <- fk.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- stats::fligner.test(Sepal.Length ~ Species, data = iris) 
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.003) 
})

test_that("Klotz test works", {
  result <- klotz.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- coin::klotz_test(y_centered ~ Species, data = validation_data, ties.method = "mid-ranks", distribution = "asymptotic") 
  expect_matches_reference(result, coin::statistic(validation), coin::pvalue(validation))
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.004) 
})

test_that("Mood test works", {
  result <- mood.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- coin::mood_test(y_centered ~ Species, data = validation_data, ties.method = "mid-ranks", distribution = "asymptotic") 
  expect_matches_reference(result, coin::statistic(validation), coin::pvalue(validation))
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.009) 
})

test_that("Siegel Tukey test works", {
  result <- siegel.tukey.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- PMCMRplus::GSTTest(y_centered ~ Species, data = validation_data, dist = "Chisquare") 
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.015) 
})

test_that("Talwar Gentle test works", {
  result <- talwar.gentle.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation_data$absolute_deviation <- abs(validation_data$Sepal.Length - ave(validation_data$Sepal.Length, validation_data$Species, FUN = median)) 
  validation <- stats::kruskal.test(absolute_deviation ~ Species, data = validation_data) 
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.008) 
})


test_that("Cochran's C test works", {
  result <- cochrans.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- GAD::C.test(lm(Sepal.Length ~ Species, data = iris)) 
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.003) 
})

test_that("Modified Z test works", {
  result <- mzv.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  y_split <- split(validation_data$Sepal.Length, validation_data$Species) 
  k  <- length(y_split) 
  ni <- vapply(y_split, length, integer(1)) 
  Species_mean <- vapply(y_split, mean, numeric(1)) 
  Species_variance <- vapply(y_split, var, numeric(1)) 
  g_fourth <- mapply(FUN = function(x, xbar, s2, n_i) { 
    standardized <- (x - xbar) / sqrt(((n_i - 1) / n_i) * s2) 
    sum(standardized^4)}, x = y_split, xbar = Species_mean, s2 = Species_variance, n_i = ni) 
  K <- g_fourth / (ni - 2) 
  c_i <- 2 * ((2.9 + 0.2 / ni) / mean(K))^(1.6 * (ni - 1.8 * K + 14.7) / ni) 
  SSE <- sum(mapply(FUN = function(x, xbar) {sum((x - xbar)^2)}, 
                    x = y_split, 
                    xbar = Species_mean)) 
  MSE <- SSE / (sum(ni) - k) 
  z_i <- sqrt(c_i * (ni - 1) * Species_variance / MSE) - sqrt(c_i * (ni - 1) - c_i / 2) 
  expected_statistic <-sum(z_i^2) / (k - 1) 
  expected_p_value <- stats::pf(expected_statistic, df1 = k - 1, df2 = Inf, lower.tail = FALSE) 
  expect_matches_reference(result, expected_statistic, expected_p_value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})

test_that("Fisher F test works", {
  result <- f.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  Species_variances <- tapply(iris$Sepal.Length, iris$Species, var) 
  max_Species <- names(which.max(Species_variances)) 
  min_Species <- names(which.min(Species_variances)) 
  x_max <- iris$Sepal.Length[iris$Species == max_Species] 
  x_min <- iris$Sepal.Length[iris$Species == min_Species] 
  expected <- stats::var.test(x_max, x_min, alternative = "two.sided") 
  expect_matches_reference(result, expected$statistic, expected$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})

test_that("G test works", {
  result <- g.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  Species_n <- tapply(validation_data$Sepal.Length, validation_data$Species, length) 
  Species_df <- Species_n - 1
  Species_variance <- tapply(validation_data$Sepal.Length, validation_data$Species, var)
  maximum_index <- which.max(Species_variance)
  expected_G <- Species_df[maximum_index] * Species_variance[maximum_index] / sum(Species_df * Species_variance)
  transformed_F <- (sum(Species_df) /Species_df[maximum_index] - 1) /(1 / expected_G - 1)
  k <- length(Species_n)
  mean_n <- mean(Species_n)
  expected_df1 <- mean_n - 1
  expected_df2 <- (mean_n - 1) * (k - 1)
  expected_p_value <- min(1, k * stats::pf(transformed_F, df1 = expected_df1, df2 = expected_df2, lower.tail = FALSE))
  expect_matches_reference(result, expected_G, expected_p_value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.003) 
})

test_that("Hartley test with mean Species size works", {
  result <- hartley.test(Sepal.Length ~ Species, data = iris, size = "mean")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  Species_n <- as.numeric(table(validation_data$Species))
  Species_var <- tapply(validation_data$Sepal.Length, validation_data$Species, var)
  expected_statistic <- max(Species_var) / min(Species_var)
  expected_df <- mean(Species_n) - 1
  expected_p_value <- SuppDists::pmaxFratio(expected_statistic, df = expected_df, k = length(Species_n), lower.tail = FALSE)
  expect_matches_reference(result, expected_statistic, expected_p_value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})

test_that("Hartley test with harmonic Species size works", {
  result <- hartley.test(Sepal.Length ~ Species, data = iris, size = "harmonic")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  Species_n <- as.numeric(table(validation_data$Species))
  Species_var <- tapply(validation_data$Sepal.Length, validation_data$Species, var)
  harmonic_n <- length(Species_n) / sum(1 / Species_n)
  expected_statistic <- max(Species_var) / min(Species_var)
  expected_df <- harmonic_n - 1
  expected_p_value <- SuppDists::pmaxFratio(expected_statistic, df = expected_df, k = length(Species_n), lower.tail = FALSE)
  expect_matches_reference(result, expected_statistic, expected_p_value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})


test_that("Hartley test with maximum Species size works", {
  result <- hartley.test(Sepal.Length ~ Species, data = iris, size = "maxn")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  Species_n <- as.numeric(table(validation_data$Species))
  Species_var <- tapply(validation_data$Sepal.Length, validation_data$Species, var)
  expected_statistic <- max(Species_var) / min(Species_var)
  expected_df <- max(Species_n) - 1
  expected_p_value <- SuppDists::pmaxFratio(expected_statistic, df = expected_df, k = length(Species_n), lower.tail = FALSE)
  expect_matches_reference(result, expected_statistic, expected_p_value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})

test_that("Hartley test with minimum variance Species size works", {
  result <- hartley.test(Sepal.Length ~ Species, data = iris, size = "minvar")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- PMCMRplus::hartleyTest(Sepal.Length ~ Species, data = iris) 
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})

test_that("Levene test with mean and absolute deviation works", {
  result <- levene.test(Sepal.Length ~ Species, data = iris, center = "mean", deviation = "absolute")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- car::leveneTest(Sepal.Length ~ Species, data = iris, center = "mean") 
  expect_matches_reference(result, validation$`F value`[1], validation$`Pr(>F)`[1])
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})

test_that("Levene test with median and absolute deviation works", {
  result <- levene.test(Sepal.Length ~ Species, data = iris, center = "median", deviation = "absolute")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- car::leveneTest(Sepal.Length ~ Species, data = iris, center = "median") 
  expect_matches_reference(result, validation$`F value`[1], validation$`Pr(>F)`[1])
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.002) 
})

test_that("Levene test with trimmed mean and absolute deviation works", {
  result <- levene.test(Sepal.Length ~ Species, data = iris, center = "trim.mean", deviation = "absolute", trim.rate = 0.25)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- car::leveneTest(Sepal.Length ~ Species, data = iris, center = "mean", trim = 0.25) 
  expect_matches_reference(result, validation$`F value`[1], validation$`Pr(>F)`[1])
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.002) 
})


squared_levene_reference <- function(formula, data, center = c("mean", "median", "trim.mean"),trim.rate = 0.25) {
  center <- match.arg(center)
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  y <- mf[[1L]]
  Species <- droplevels(factor(mf[[2L]]))
  centers <- switch(
    center,
    mean = tapply(y, Species, base::mean),
    median = tapply(y, Species, stats::median),
    trim.mean = tapply(y, Species, base::mean, trim = trim.rate))
  squared_deviation <- (y - unname(centers[as.character(Species)]))^2
  fit <- stats::lm(squared_deviation ~ Species)
  anova_result <- stats::anova(fit)
  list(statistic = unname(anova_result[1L, "F value"]),
       parameter = c(df1 = unname(anova_result[1L, "Df"]), df2 = unname(anova_result[2L, "Df"])), p.value = unname(anova_result[1L, "Pr(>F)"]))
}

test_that("Levene test with mean and squared deviation works", {
  result <- levene.test(Sepal.Length ~ Species, data = iris, center = "mean", deviation = "squared")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- squared_levene_reference(Sepal.Length ~ Species, data = iris, center = "mean")
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.002) 
})

test_that("Levene test with median and squared deviation works", {
  result <- levene.test(Sepal.Length ~ Species, data = iris, center = "median", deviation = "squared")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- squared_levene_reference(Sepal.Length ~ Species, data = iris, center = "median")
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.002) 
})

test_that("Levene test with trimmed mean and squared deviation works", {
  result <- levene.test(Sepal.Length ~ Species, data = iris, center = "trim.mean", deviation = "squared", trim.rate = 0.25)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- squared_levene_reference(Sepal.Length ~ Species, data = iris, center = "trim.mean")
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.002) 
})

obrien_reference <- function(formula, data, center = c("mean", "median", "trim.mean"), trim.rate = 0.25) {
  center <- match.arg(center)
  mf <- stats::model.frame( formula, data = data, na.action = stats::na.omit)
  y <- mf[[1L]]
  Species <- droplevels(factor(mf[[2L]]))
  split_y <- split(y, Species)
  obrien_values <- lapply(
    split_y,
    function(x) { n_i <- length(x)
    if (n_i <= 2L) {stop("Each Species must contain at least three observations.")}
    center_i <- switch(
      center,
      mean = base::mean(x),
      median = stats::median(x),
      trim.mean = base::mean(x, trim = trim.rate))
    variance_i <- stats::var(x)
    ((n_i - 1.5) * n_i * (x - center_i)^2 - 0.5 * variance_i * (n_i - 1)) / ((n_i - 1) * (n_i - 2))
    }
  )
  transformed <- unsplit(obrien_values, Species)
  fit <- stats::lm(transformed ~ Species)
  anova_table <- stats::anova(fit)
  list(statistic = unname(anova_table[1L, "F value"]),
       parameter = c(df1 = unname(anova_table[1L, "Df"]), df2 = unname(anova_table[2L, "Df"])), 
       p.value = unname(anova_table[1L, "Pr(>F)"]))
}

test_that("O'Brien test with mean center method works", {
  result <- obrien.test(Sepal.Length ~ Species, data = iris, center = "mean")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- obrien_reference(Sepal.Length ~ Species, data = iris, center = "mean")
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.002) 
})

test_that("O'Brien test with median center method works", {
  result <- obrien.test(Sepal.Length ~ Species, data = iris, center = "median")
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- obrien_reference(Sepal.Length ~ Species, data = iris, center = "median")
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.002) 
})

test_that("O'Brien test with trimmed mean center method works", {
  result <- obrien.test(Sepal.Length ~ Species, data = iris, center = "trim.mean", trim.rate = 0.25)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- obrien_reference(Sepal.Length ~ Species, data = iris, center = "trim.mean")
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_equal(round(result$p.value, 3), 0.002)
})

zv_reference <- function(formula, data) {
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  y <- mf[[1L]]
  Species <- droplevels(factor(mf[[2L]]))
  split_y <- split(y, Species)
  k <- length(split_y)
  n_i <- vapply(split_y, length, integer(1L))
  variance_i <- vapply(split_y, stats::var, numeric(1L))
  n_total <- sum(n_i)
  pooled_sse <- sum(
    vapply(split_y,
           function(x) {
             sum((x - mean(x))^2)},
           numeric(1L)
    )
  )
  pooled_mse <- pooled_sse / (n_total - k)
  c_i <- 2 + 1 / n_i
  z_i <- sqrt(c_i * (n_i - 1) * variance_i / pooled_mse) - sqrt(c_i * (n_i - 1) - c_i / 2)
  statistic <- sum(z_i^2) / (k - 1)
  parameter <- c(df1 = k - 1, df2 = Inf)
  p_value <- stats::pf(statistic, df1 = parameter["df1"], df2 = parameter["df2"], lower.tail = FALSE)
  list(statistic = unname(statistic),
       parameter = unname(parameter),
       p.value = unname(p_value))
}

test_that("Z test works", {
  result <- zv.test(Sepal.Length ~ Species, data = iris)
  # Step 1: Technical validation of the returned object.
  expect_valid_vht(result)
  # Step 2: Statistical correctness check against an independent reference.
  validation <- zv_reference(Sepal.Length ~ Species, data = iris)
  expect_matches_reference(result, validation$statistic, validation$p.value)
  # Step 3: Reproducibility check against the previously reported iris result.
  expect_lt(result$p.value, 0.001) 
})


# Step 4: Robustness validation

test_that("Non-numeric response variables are rejected", {
  non_numeric_data <- data.frame(y = c("1", "2", "3", "4", "5", "6"),
                                group = factor(rep(c("A", "B"), each = 3)))
  expect_vht_error(bartletts.test, data = non_numeric_data, regexp = "numeric|continuous|response")
})

test_that("Variables absent from the data are rejected", {
  expect_vht_error(bartletts.test, data = iris, formula = unknown_response ~ Species, regexp = "unknown_response|not found")
  expect_vht_error(bartletts.test, data = iris, formula = Sepal.Length ~ unknown_group, regexp = "unknown_group|not found")
})

test_that("Invalid Levene arguments are rejected", {
  valid_test_data <- data.frame(y = c(1, 2, 3, 4, 5, 7, 8, 10), group = factor(rep(c("A", "B"), each = 4)))
  expect_vht_error(levene.test, data = valid_test_data, regexp = "center|arg|one of", center = "invalid")
  expect_vht_error(levene.test, data = valid_test_data, regexp = "deviation|arg|one of", center = "median", deviation = "invalid")
})

test_that("Invalid Hartley size values are rejected", {
  valid_test_data <- data.frame(y = c(1, 2, 3, 4, 5, 7, 8, 10), group = factor(rep(c("A", "B"), each = 4)))
  expect_vht_error(hartley.test, data = valid_test_data, regexp = "size|arg|one of", size = "invalid")
})

test_that("Missing observations are handled consistently", {
  missing_data <- iris
  missing_data$Sepal.Length[c(1, 10, 60)] <- NA_real_
  result <- bartletts.test(Sepal.Length ~ Species, data = missing_data)
  expect_valid_vht(result)
  validation <- stats::bartlett.test(Sepal.Length ~ Species, data = missing_data)
  expect_matches_reference(result, validation$statistic, validation$p.value)
})

test_that("Unused factor levels do not change the result", {
  unused_level_data <- iris
  unused_level_data$Species <- factor(unused_level_data$Species,
                                      levels = c(levels(iris$Species), "unused_species"))
  original <- bartletts.test(Sepal.Length ~ Species, data = iris)
  result <- bartletts.test(Sepal.Length ~ Species, data = unused_level_data)
  expect_valid_vht(result)
  expect_matches_reference(result, original$statistic, original$p.value)
})

