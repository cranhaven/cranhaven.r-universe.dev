library(incidence2)

context("Bootstrapping incidence")

set.seed(1)

dates <- as.integer(sample(-3:100, 50, replace = TRUE))
DATES <- as.Date("2016-09-20") + dates
groups <- sample(c("toto", "tata"), 50, replace = TRUE)
dat <- data.frame(DATES, groups = groups, stringsAsFactors = FALSE)


test_that("Bootstrap needs an incidence object", {
    expect_error(bootstrap(DATES), "`DATES` is not an 'incidence2' object")
})


test_that("estimate_peak needs an incidence object", {
    expect_error(estimate_peak(DATES), "`DATES` is not an 'incidence2' object")
})


test_that("Bootstrap incidence with groups", {
    skip_on_cran()

    x <- incidence(dat, date_index = "DATES", interval = 3, groups = "groups")
    y <- bootstrap(x)
    z <- bootstrap(x, randomise_groups = TRUE)

    expect_identical(sum(x$count), sum(y$count))

    expect_identical(sum(x$count), sum(z$count))

    expect_identical(names(x), names(y))

    expect_identical(names(x), names(z))

    expect_true(setequal(y$groups, z$groups))
})




context("Mountain Climbing")

test_that("find_peak can find the peak", {
    skip_on_cran()

    x <- incidence(dat, date_index = "DATES", interval = 3, groups = "groups")
    tata_x <- dplyr::filter(x, groups == "tata")
    toto_x <- dplyr::filter(x, groups == "toto")
    no_group_x <- regroup(x)
    group_peaks <- find_peak(x)
    no_group_peaks <- find_peak(no_group_x)


    expect_equal(
        tibble::as_tibble(no_group_x[which.max(no_group_x$count), ]),
        tibble::as_tibble(find_peak(no_group_x))
    )

    expect_error(find_peak(1:10), "`x` must be an incidence2 object.")

    expect_equal(nrow(no_group_peaks), 1L)

    expect_equal(group_peaks$groups, c("tata", "toto"))

    # TODO - fix this (data.table conversion issue!)
    # expect_equal(
    #   group_peaks[group_peaks$groups == "tata", ],
    #   as_tibble(tata_x[which.max(tata_x$count), ][c("groups", "date_index", "count")])
    # )

    # TODO - fix this (data.table conversion issue!)
    # expect_equal(
    #   group_peaks[group_peaks$groups == "toto", ],
    #   as_tibble(toto_x[which.max(toto_x$count), ][c("groups", "date_index", "count")])
    # )


})


test_that("estimate_peak can roughly estimate it", {

    x <- incidence(dat, date_index = "DATES", interval = 3, groups = "groups")
    y <- incidence(dat, date_index = "DATES", interval = 3)

    e1 <- estimate_peak(x)
    e2 <- estimate_peak(y)
    expect_named(
        e1,
        c("groups", "count_variable", "observed_peak", "observed_count",
          "bootstrap_peaks", "lower_ci", "median", "upper_ci")
    )
    expect_named(
        e2,
        c("count_variable", "observed_peak", "observed_count", "bootstrap_peaks", "lower_ci",
          "median", "upper_ci")
    )

    # The observed is identical to find_peak
    tmp <- find_peak(y)
    expect_equal(e2$observed_peak, tmp[[1L]])
    expect_equal(e2$observed_count, tmp[[3L]])

    # The number of peaks defaults to 100
    expect_identical(nrow(e1$bootstrap_peaks[[1]]), 100L)
    expect_identical(nrow(e2$bootstrap_peaks[[1]]), 100L)

    # The observed falls within the confidence interval
    expect_gte(as.integer(e1$observed_peak[1]), as.integer(e1$lower_ci[1]))
    expect_lte(as.integer(e1$observed_peak[1]), as.integer(e1$upper_ci[1]))
    expect_gte(as.integer(e2$observed_peak[1]), as.integer(e2$lower_ci[1]))
    expect_lte(as.integer(e2$observed_peak[1]), as.integer(e2$upper_ci[1]))
})
