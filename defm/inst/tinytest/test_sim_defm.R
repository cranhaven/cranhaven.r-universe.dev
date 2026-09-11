n_K <- 1L
n_Y <- 4
n_id <- 2L
n_T <- 4L

set.seed(31231)
id <- cbind(rep(1L:n_id, each = n_T))
Y <- matrix(0L, nrow = n_T * n_id, ncol = n_Y)
X <- matrix(rnorm(n_K * n_T * n_id), ncol = n_K)

# ------------------------------------------------------------------------------
# new_defm
# ------------------------------------------------------------------------------

expect_error({
  d_model_formula <- new_defm(id = id, Y = Y, X = X, order = 1)
})

colnames(X) <- paste0("col", 1:n_K)
colnames(Y) <- paste0("y", 1:n_Y)

d_model_formula <- new_defm(id = id, Y = Y, X = X, order = 1)

td_logit_intercept(
  d_model_formula
)

init_defm(d_model_formula)

set.seed(331)
expect_silent({
  Y_sim <- sim_defm(
    d_model_formula,
    par = c(-1, -.5, .5, 1)
  )
})

# We now create a new model using the new output
d_model_formula <- new_defm(id = id, Y = Y_sim, X = X, order = 1)

td_logit_intercept(
  d_model_formula
)

init_defm(d_model_formula)

# ------------------------------------------------------------------------------
# Stats
# ------------------------------------------------------------------------------

expect_silent({
  Y_stats <- get_stats(d_model_formula)
})

expect_true(ncol(Y_stats) == length(get_counters(d_model_formula)))
expect_true(ncol(Y_stats) == n_Y)

# We know this counter matches the data.
Y_sim[c(1, 5),] <- NA
expect_equivalent(Y_stats, Y_sim)

expect_stdout(
  print_stats(d_model_formula),
  "counts: "
)

expect_equal(get_Y_names(d_model_formula), colnames(Y_sim))
expect_equal(get_X_names(d_model_formula), colnames(X))

# ------------------------------------------------------------------------------
# Motif census
# ------------------------------------------------------------------------------
expect_stdout({
  print(motif_census(d_model_formula, c(0,1)))
}, "census for variable")


# ------------------------------------------------------------------------------
# Log odds
# ------------------------------------------------------------------------------
expect_silent({
  lo <- logodds(d_model_formula, c(-1, -.5, .5, 1), i=1, j=1)
})


# ------------------------------------------------------------------------------
# Counters
# ------------------------------------------------------------------------------
expect_stdout({
  print(get_counters(d_model_formula))
}, "Counters \\(4\\)")

expect_stdout({
  print(get_counters(d_model_formula)[1])
}, "Counter[:]")


set_counter_info(
  counter = get_counters(d_model_formula)[1],
  new_name = "lame", new_desc = "whatever"
  )

tmp <- get_counters(d_model_formula)[1] |>
  as.list()

expect_equal(
  list(name = "lame", description = "whatever"),
  tmp
)

# ------------------------------------------------------------------------------
# Misc
# ------------------------------------------------------------------------------
expect_inherits(as.list(get_counters(d_model_formula)), "list")