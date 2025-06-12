# General optimization tests ----
set.seed(64, kind = "Mersenne-Twister")
z <- c(rep(0, 14), rep(1, 6))
data <- data.frame(color = c(rep("Red", 4), rep("White", 7), rep("Blue", 3), rep("White", 2), rep("Red", 4)),
                   number = rnorm(20),
                   category = c(rep(c("1", "2"), 5), "1", rep("2", 3), "1", rep("2", 4), "1"))
data$number[c(1, 5, 11)] <- NA
constraints <- suppressWarnings(generate_constraints(list(color + number ~ 2 * category), z, data = data,
                                                     autogen_missing = 4))
results <- optimize_controls(z = z, X = constraints$X, st = data$category, ratio = 1.5,
                             importances = constraints$importances,
                             integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                             time_limit = Inf)

test_that("optimization gives correct results", {
  expect_equal(results$lpdetails$objective, 6.124562, tolerance = .0001)
  expect_equal(results$objective, 6.264861, tolerance = .0001)
})

test_that("optimization chooses correct number of units", {
  expect_equal(sum(results$pr[results$pr < 1 & results$pr > 0]),
               sum(results$selected[results$pr < 1 & results$pr > 0]))
  expect_equal(sum(results$selected[z == 0]), sum(round(table(z, data$category) * 1.5) [2,]))
})

test_that("importances incorporated properly", {
  expect_equal(results$objective, sum(results$importances * results$eps))
  expect_equal(results$objective_wo_importances, sum(results$eps))
  expect_equal(results$lpdetails$objective, sum(results$importances * results$lpdetails$eps))
  expect_equal(results$lpdetails$objective_wo_importances, sum(results$lpdetails$eps))

})

# SD vs epsilon tests ----

data_for_sds <- cbind(data[, 1, drop = FALSE], is.na(data$number))
names(data_for_sds)[2] <- "number_missing"
sds <- check_balance(z, data_for_sds, data$category, results$selected, message = FALSE)

test_that("epsilons across strata equal standardized diff in means across when no missingness", {
  expect_equal(sds$sd_across[, "abs_stand_diff_after"],
               as.numeric(rowSums(results$eps[paste0(row.names(sds$sd_across), "_1"),])))
})

test_that("sum of epsilons within strata equal weighted avg of within strata standardized diff in means
          when ratios lead to integer q_s and no missingness", {
            covs <- row.names(sds$sd_across)
            stripped_row_names <- sapply(strsplit(row.names(results$eps), "_"),
                                         function(k) {paste0(k[-length(k)], collapse = "_")})
            sum_eps_within <- sapply(covs, function(cov) {
              sum(results$eps[stripped_row_names == cov &
                                grepl("_category", row.names(results$eps))])})
            expect_equal(sds$sd_strata_avg[, "abs_stand_diff_after"],
                         as.numeric(sum_eps_within))

          })



# EMD Tests ----

z <- c(rep(0, 15), rep(1, 5))
data <- data.frame(color = c(rep("Red", 5), rep("White", 2), rep("Blue", 5), rep("White", 4), rep("Red", 4)),
                   number = 1:20,
                   category = c("1", "1", "1", rep(c( "2", "3"), 6), "1", rep("2", 2), "3", "1"))
data$number[c(1, 5, 11, 16)] <- NA
constraints <- suppressWarnings(generate_constraints(list(color + number ~ 2 * category), z, data = data,
                                                     autogen_missing = 4))
strata_dist <- matrix(c(0, 2, 1, 2, 0, 1, 1, 1, 0), byrow = TRUE, ncol = 3, dimnames = list(c("1", "2", "3"), c("1", "2", "3")))
q_s <- generate_qs(z, st = data$category, ratio = 2.5, max_ratio = NULL, max_extra_s = NULL, strata_dist = strata_dist)
results_emd <- suppressWarnings(optimize_controls(z = z, X = constraints$X, st = data$category, q_s = q_s,
                                                  importances = constraints$importances,
                                                  integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                                                  time_limit = Inf))

test_that("EMD chooses correct number of units", {
  expect_equal(sum(results_emd$selected),
               sum(round(2.5 * table(z, data$category)[2, ])) + sum(table(z, data$category)[2, ]))
})

test_that("EMD gives right objective", {
  expect_equal(results_emd$objective, 27.89788, tolerance = 0.0001)
})

test_that("Choosing from closest strata", {
  expect_equal(as.numeric(table(data$category[results_emd$selected & !z]))[3], 4)
})

test_that("Specified max ratio and max extra working", {
  q_s2 <- generate_qs(z, st = data$category, ratio = 2.5, max_ratio = 0, max_extra_s = 1, strata_dist = strata_dist)
  results_emd2 <- suppressWarnings(optimize_controls(z = z, X = constraints$X, st = data$category, q_s = q_s2,
                                                     importances = constraints$importances,
                                                     integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                                                     time_limit = Inf))
  expect_equal(as.numeric(table(data$category[results_emd2$selected & !z]))[3], 3)
  q_s3 <- generate_qs(z, st = data$category, ratio = 2.5, max_ratio = Inf, max_extra_s = 1, strata_dist = strata_dist)
  results_emd3 <- suppressWarnings(optimize_controls(z = z, X = constraints$X, st = data$category,  q_s = q_s3,
                                                     importances = constraints$importances,
                                                     integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                                                     time_limit = Inf))
  expect_equal(as.numeric(table(data$category[results_emd3$selected & !z]))[3], 4)
  q_s4 <- generate_qs(z, st = data$category, ratio = 2.5,  max_ratio = 3, max_extra_s = 0, strata_dist = strata_dist)
  results_emd4 <- suppressWarnings(optimize_controls(z = z, X = constraints$X, st = data$category, q_s = q_s4,
                                                     importances = constraints$importances,
                                                     integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                                                     time_limit = Inf))
  expect_equal(as.numeric(table(data$category[results_emd4$selected & !z]))[3], 3)
})



# Multiple control group tests ----

z <- c(rep(0, 10), rep(1, 10), rep(2, 8))
data <- data.frame(color = c(rep("Red", 5), rep("White", 2), rep("Blue", 3),
                             rep("White", 2), rep("Red", 8), rep("White", 3), rep("Blue", 5)),
                   number = 1:28,
                   category = c(rep(c("1", "2"), 5), "1", rep("2", 1), rep(c("1", "2"), 8)))
data$number[c(1, 5, 11)] <- NA
constraints <- suppressWarnings(generate_constraints(list(color + number ~ 2 * category),
                                                     z, data = data, treated = 2,
                                                     autogen_missing = 4, denom_variance = "pooled"))
results <- optimize_controls(z = z, X = constraints$X, st = data$category, ratio = .5,
                             treated = 2, importances = constraints$importances,
                             integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                             time_limit = Inf)

test_that("optimization gives correct results", {
  expect_equal(results$lpdetails$objective, 60.63904, tolerance = .0001)
  expect_equal(results$lpdetails$objective, sum(results$lpdetails$eps * constraints$importances),
               tolerance = .0001)
  expect_equal(results$lpdetails$objective_wo_importances, 40.06041, tolerance = .0001)
  expect_equal(results$lpdetails$objective_wo_importances, sum(results$lpdetails$eps), tolerance = .0001)
  expect_equal(results$objective, 61.7425, tolerance = .0001)
  expect_equal(results$objective, sum(results$eps * constraints$importances),
               tolerance = .0001)
  expect_equal(results$objective_wo_importances, 40.97886, tolerance = .0001)
  expect_equal(results$objective_wo_importances, sum(results$eps), tolerance = .0001)
})

test_that("optimization chooses correct number of units", {
  expect_equal(sum(results$pr[results$pr < 1 & results$pr > 0]),
               sum(results$selected[results$pr < 1 & results$pr > 0]))
  expect_equal(sum(results$selected[z == 0]), sum(round(table(z, data$category)[3,] * 0.5) ))
  expect_equal(sum(results$selected[z == 1]), sum(round(table(z, data$category)[3,] * 0.5) ))
  expect_equal(sum(results$selected[z == 2]), sum(round(table(z, data$category)[3,] * 1) ))
})

data_for_sds <- cbind(data[, 1, drop = FALSE], is.na(data$number))
names(data_for_sds)[2] <- "number_missing"
# Between group 0 and 1
sds1 <- check_balance(z = z, control = 0, treated = 1, X = data_for_sds,
                      st = data$category, selected = results$selected,
                      denom_variance = "pooled", message = FALSE)
# Between group 0 and 2
sds2 <- check_balance(z = z, control = 0, treated = 2, X = data_for_sds,
                      st = data$category, selected = results$selected,
                      denom_variance = "pooled", message = FALSE)
# Between group 1 and 2
sds3 <- check_balance(z = z, control = 1, treated = 2, X = data_for_sds,
                      st = data$category, selected = results$selected,
                      denom_variance = "pooled", message = FALSE)

test_that("epsilons across strata equal standardized diff in means across when no missingness", {
  expect_equal(sds1$sd_across[, "abs_stand_diff_after"],
               as.numeric(rowSums(results$eps[paste0(row.names(sds1$sd_across), "_1_0:1"),])))
  expect_equal(sds2$sd_across[, "abs_stand_diff_after"],
               as.numeric(rowSums(results$eps[paste0(row.names(sds2$sd_across), "_1_0:2"),])))
  expect_equal(sds3$sd_across[, "abs_stand_diff_after"],
               as.numeric(rowSums(results$eps[paste0(row.names(sds3$sd_across), "_1_1:2"),])))
})

test_that("sum of epsilons within strata equal weighted avg of within strata standardized diff in means
          when no missingness and ratios lead to integer q_s", {
            covs <- row.names(sds1$sd_across)
            stripped_row_names <- sapply(strsplit(row.names(results$eps), "_"),
                                         function(k) {paste0(k[1:(length(k)-2)], collapse = "_")})
            sum_eps_within <- sapply(covs, function(cov) {
              sum(results$eps[stripped_row_names == cov &
                                grepl("_category", row.names(results$eps)) &
                                grepl("_0:1", row.names(results$eps))])})
            expect_equal(sds1$sd_strata_avg[, "abs_stand_diff_after"],
                         as.numeric(sum_eps_within))

            sum_eps_within <- sapply(covs, function(cov) {
              sum(results$eps[stripped_row_names == cov &
                                grepl("_category", row.names(results$eps)) &
                                grepl("_0:2", row.names(results$eps))])})
            expect_equal(sds2$sd_strata_avg[, "abs_stand_diff_after"],
                         as.numeric(sum_eps_within))

            sum_eps_within <- sapply(covs, function(cov) {
              sum(results$eps[stripped_row_names == cov &
                                grepl("_category", row.names(results$eps)) &
                                grepl("_1:2", row.names(results$eps))])})
            expect_equal(sds3$sd_strata_avg[, "abs_stand_diff_after"],
                         as.numeric(sum_eps_within))
          })

results <- optimize_controls(z = z, X = constraints$X, st = data$category, ratio = c(.5, .75, 1),
                             treated = 2, importances = constraints$importances,
                             integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                             time_limit = Inf)
test_that("multiple ratios still choose correct number of units", {
  expect_equal(sum(results$pr[results$pr < 1 & results$pr > 0]),
               sum(results$selected[results$pr < 1 & results$pr > 0]))
  expect_equal(sum(results$selected[z == 0]), sum(round(table(z, data$category)[3,] * 0.5) ))
  expect_equal(sum(results$selected[z == 1]), sum(round(table(z, data$category)[3,] * 0.75) ))
  expect_equal(sum(results$selected[z == 2]), sum(round(table(z, data$category)[3,] * 1) ))
})



# Two comparisons tests ----

set.seed(64, kind = "Mersenne-Twister")
z <- c(rep(0, 20), rep(1, 12), rep(2, 8))
data <- data.frame(color = c(rep(c(rep("Red", 4), rep("White", 1), rep("Blue", 2)), 3), "Blue",
                             rep("White", 2), rep("Red", 8), rep("White", 3), rep("Blue", 5)),
                   number = c(sample(1:20, 20, replace = TRUE), sample(10:30, 12, replace = TRUE),
                              sample(10:20, 8, replace = TRUE)),
                   category = c(rep(c("1", "2"), 20)))
data$number[c(1, 5, 11, 22, 30, 39)] <- NA
constraints <- suppressWarnings(generate_constraints(list(color + number ~ 2 * category),
                                                     z, data = data, treated = 2,
                                                     autogen_missing = 4, denom_variance = "pooled"))
results_two <- optimize_controls(z = z, X = constraints$X, st = data$category, ratio = 1,
                             q_star_s = matrix(c(rep(2, 4), rep(0, 2)), nrow = 3,
                                               byrow = TRUE, dimnames = list(NULL, c("1", "2"))),
                             treated = 2, treated_star = 1, weight_star = 2,
                             importances = constraints$importances,
                             integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                             time_limit = Inf, correct_sizes = FALSE, low_memory = FALSE)


test_that("optimization gives correct results", {
  expect_equal(results_two$lpdetails$objective, 44.17337, tolerance = .0001)
  expect_equal(results_two$lpdetails$objective,
               sum(results_two$lpdetails$eps * constraints$importances) +
                 sum(results_two$lpdetails$eps_star * results_two$weight_star * constraints$importances),
               tolerance = .0001)
  expect_equal(results_two$lpdetails$objective_wo_importances,  17.92818, tolerance = .0001)
  expect_equal(results_two$lpdetails$objective_wo_importances,
               sum(results_two$lpdetails$eps) + sum(results_two$lpdetails$eps_star), tolerance = .0001)
  expect_equal(results_two$objective, 44.17337, tolerance = .0001)
  expect_equal(results_two$objective, sum(results_two$eps * constraints$importances) +
                 sum(results_two$eps_star * results_two$weight_star * constraints$importances),
               tolerance = .0001)
  expect_equal(results_two$objective_wo_importances, 17.92818, tolerance = .0001)
  expect_equal(results_two$objective_wo_importances, sum(results_two$eps) + sum(results_two$eps_star), tolerance = .0001)
})

# Since sample sizes only correct in expectation, this varies from seed to seed
test_that("number of units chosen is approximately what we wanted for each group", {
  expect_equal(as.numeric(table(z[results_two$selected], data$category[results_two$selected])), rep(4, 6))
  expect_equal(as.numeric(table(z[results_two$selected_star], data$category[results_two$selected_star])), rep(2, 4))
})

test_that("units chosen for either main or supplemental group", {
  expect_equal(sum(results_two$selected & results_two$selected_star), 0)
})



# Three comparisons tests ----

set.seed(64, kind = "Mersenne-Twister")

results_three <- optimize_controls(z = z, X = constraints$X, st = data$category, ratio = 1,
                             q_star_s = list(matrix(c(0, rep(1, 3), rep(0, 2)), nrow = 3,
                                               byrow = TRUE, dimnames = list(NULL, c("1", "2"))),
                                             matrix(c(1, 0, 1, 0, rep(0, 2)), nrow = 3,
                                                    byrow = TRUE, dimnames = list(NULL, c("1", "2")))),
                             treated = 2, treated_star = c(1, 1), weight_star = c(2, 1),
                             importances = constraints$importances,
                             integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                             time_limit = Inf, correct_sizes = FALSE, low_memory = FALSE)

test_that("optimization gives correct results", {
  expect_equal(results_three$lpdetails$objective, 49.5996, tolerance = .0001)
  lp_sum_w_imp <- sum(results_three$lpdetails$eps * constraints$importances)
  lp_sum_wo_imp <- sum(results_three$lpdetails$eps)
  sum_w_imp <- sum(results_three$eps * constraints$importances)
  sum_wo_imp <- sum(results_three$eps)

  for (supp_comp in 1:2) {
    lp_sum_w_imp <- lp_sum_w_imp + sum(results_three$lpdetails$eps_star[[supp_comp]] *
                                   results_three$weight_star[supp_comp] * constraints$importances)
    lp_sum_wo_imp <- lp_sum_wo_imp + sum(results_three$lpdetails$eps_star[[supp_comp]])
    sum_w_imp <- sum_w_imp + sum(results_three$eps_star[[supp_comp]] *
                                   results_three$weight_star[supp_comp] * constraints$importances)
    sum_wo_imp <- sum_wo_imp + sum(results_three$eps_star[[supp_comp]])
  }
  expect_equal(results_three$lpdetails$objective,
               lp_sum_w_imp,
               tolerance = .0001)
  expect_equal(results_three$lpdetails$objective_wo_importances,  20.88223, tolerance = .0001)
  expect_equal(results_three$lpdetails$objective_wo_importances,
               lp_sum_wo_imp, tolerance = .0001)
  expect_equal(results_three$objective, 50.44668, tolerance = .0001)
  expect_equal(results_three$objective, sum_w_imp,
               tolerance = .0001)
  expect_equal(results_three$objective_wo_importances, 21.08154, tolerance = .0001)
  expect_equal(results_three$objective_wo_importances, sum_wo_imp, tolerance = .0001)
})

# Since sample sizes only correct in expectation, this varies from seed to seed
test_that("number of units chosen is approximately what we wanted for each group", {
  expect_equal(as.numeric(table(z[results_three$selected], data$category[results_three$selected])), rep(4, 6))
  expect_equal(as.numeric(table(z[results_three$selected_star[[1]]], data$category[results_three$selected_star[[1]]])), c(0, 1, 1, 1))
  expect_equal(as.numeric(table(z[results_three$selected_star[[2]]], data$category[results_three$selected_star[[2]]])), rep(1, 2))
})

test_that("units chosen for either main or supplemental group", {
  expect_equal(sum(results_three$selected + results_three$selected_star[[1]] + results_three$selected_star[[2]] > 1), 0)
})



# Tests for low memory ----

set.seed(64, kind = "Mersenne-Twister")
results_three_low_mem <- optimize_controls(z = z, X = constraints$X, st = data$category, ratio = 1,
                             q_star_s = list(matrix(c(0, rep(1, 3), rep(0, 2)), nrow = 3,
                                                    byrow = TRUE, dimnames = list(NULL, c("1", "2"))),
                                             matrix(c(1,0,1,0, rep(0, 2)), nrow = 3,
                                                    byrow = TRUE, dimnames = list(NULL, c("1", "2")))),
                             treated = 2, treated_star = c(1, 1), weight_star = c(2, 1),
                             importances = constraints$importances,
                             integer = FALSE, solver = "Rglpk", seed = 1, runs = 5,
                             time_limit = Inf, correct_sizes = FALSE, low_memory = TRUE)


test_that("optimization gives correct results", {
  expect_equal(results_three_low_mem$lpdetails$objective,
               results_three$lpdetails$objective, tolerance = .0001)
  expect_equal(results_three_low_mem$lpdetails$objective_wo_importances,
               results_three$lpdetails$objective_wo_importances, tolerance = .0001)
 expect_equal(results_three_low_mem$objective,
              results_three$objective, tolerance = .0001)
  expect_equal(results_three_low_mem$objective_wo_importances,
               results_three$objective_wo_importances, tolerance = .0001)
})

test_that("eps not reported", {
  expect_true(is.null(results_three_low_mem$eps))
  expect_true(is.null(results_three_low_mem$eps_star))
  expect_true(is.null(results_three_low_mem$lpdetails$eps))
  expect_true(is.null(results_three_low_mem$lpdetails$eps_star))
})
