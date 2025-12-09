### * Setup

new_networkModel <- function() {
    isotracer::new_networkModel(quiet = TRUE)
}

set_prop_family <- function(...) {
    isotracer::set_prop_family(..., quiet = TRUE)
}

### * project()

test_that("project() works", {
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(tibble::tibble(comps = c("NH4", "algae", "daphnia"),
                        sizes = c(0.2, 1, 2),
                        props = c(0.8, 0.004, 0.004)),
                 comp = "comps", size = "sizes", prop = "props") %>%
        set_params(c("eta" = 0.2, "lambda_algae" = 0, "lambda_daphnia" = 0, 
                     "lambda_NH4" = 0, "upsilon_algae_to_daphnia" = 0.15, 
                     "upsilon_NH4_to_algae" = 0.25, "upsilon_daphnia_to_NH4" = 0.04,
                     "zeta" = 0.1)) %>%
        project(end = 10)
    # Check that the projected trajectories have the correct format
    expect_equal(nrow(x), 1)
    expect_true("trajectory" %in% colnames(x))
    t <- x$trajectory[[1]]
    expect_is(t, c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(t), 1)
    expect_setequal(colnames(t), c("timepoints", "unmarked", "marked", "sizes", "proportions"))
    tpts <- t$timepoints[[1]]
    expect_equal(tpts, seq(0, 10, length.out = 257))
    for (col in c("unmarked", "marked", "sizes", "proportions")) {
        d <- t[[col]][[1]]
        expect_equal(dim(d), c(257, 3))
        expect_setequal(colnames(d), c("algae", "daphnia", "NH4"))
        expect_equal(dim(na.omit(d)), c(257, 3))
    }
    expect_equal(t[["unmarked"]][[1]] + t[["marked"]][[1]], t[["sizes"]][[1]])
    expect_equal(t[["marked"]][[1]]/t[["sizes"]][[1]], t[["proportions"]][[1]])
})

test_that("sample_from() does not crash with different families", {
    x <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(tibble::tibble(comps = c("NH4", "algae", "daphnia"),
                        sizes = c(0.2, 1, 2),
                        props = c(0.8, 0.004, 0.004)),
                 comp = "comps", size = "sizes", prop = "props") %>%
        set_params(c("eta" = 0.2, "lambda_algae" = 0, "lambda_daphnia" = 0, 
                     "lambda_NH4" = 0, "upsilon_algae_to_daphnia" = 0.15, 
                     "upsilon_NH4_to_algae" = 0.25, "upsilon_daphnia_to_NH4" = 0.04,
                     "zeta" = 0.1)) %>%
        project(end = 10)
    # gamma_cv
    x <- set_prop_family(x, "gamma_cv")
    at <- 1:10
    expect_error({ z <- sample_from(x, at = at, error.draws = 5)}, NA)
    expect_equal(nrow(z), 150)
    expect_equal(ncol(z), 4)
    # normal_cv
    x <- set_prop_family(x, "normal_cv")
    at <- 1:10
    expect_error(sample_from(x, at = at, error.draws = 5), NA)
    # normal_sd
    x <- set_prop_family(x, "normal_sd")
    at <- 1:10
    expect_error(sample_from(x, at = at, error.draws = 5), NA)
    # beta_phi
    x <- set_prop_family(x, "beta_phi")
    at <- 1:10
    expect_error(sample_from(x, at = at, error.draws = 5), NA)
})
