# Shared test fixtures loaded automatically by testthat before tests run.

# ---------------------------------------------------------------------------
# Tiny toy datasets (30 train / 10 test) for fast tests
# ---------------------------------------------------------------------------
set.seed(42)
.n_train <- 30L
.n_test  <- 10L

toy_train <- data.frame(
  LDH        = round(runif(.n_train, 150, 500), 1),
  hs_CRP     = round(runif(.n_train, 1, 100), 1),
  Lymphocyte = round(runif(.n_train, 1, 50), 1),
  Outcome    = factor(sample(0:1, .n_train, replace = TRUE))
)

toy_test <- data.frame(
  LDH        = round(runif(.n_test, 150, 500), 1),
  hs_CRP     = round(runif(.n_test, 1, 100), 1),
  Lymphocyte = round(runif(.n_test, 1, 50), 1),
  Outcome    = factor(sample(0:1, .n_test, replace = TRUE), levels = levels(toy_train$Outcome))
)

rm(.n_train, .n_test)

# ---------------------------------------------------------------------------
# Skip helper for expensive / extended tests
# ---------------------------------------------------------------------------
skip_unless_extended <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("DTGAP_RUN_EXTENDED_TESTS"), "true"),
    "Set DTGAP_RUN_EXTENDED_TESTS=true to run extended tests"
  )
}

# ---------------------------------------------------------------------------
# Cached classification fixture (built once per session using toy data)
# ---------------------------------------------------------------------------
.fixture_env <- new.env(parent = emptyenv())

build_classification_fixture <- function() {
  if (!is.null(.fixture_env$cached)) return(.fixture_env$cached)

  data_all <- add_data_type(data_train = toy_train, data_test = toy_test)
  data_all <- prepare_features(data_all, target_lab = "Outcome", task = "classification")

  train_result <- train_tree(
    data_train = toy_train,
    target_lab = "Outcome",
    model = "rpart"
  )
  fit     <- train_result$fit
  var_imp <- train_result$var_imp

  tree_res <- compute_tree(
    fit,
    model = "rpart", show = "test",
    data = data_all, target_lab = "Outcome",
    task = "classification"
  )

  sorted_dat <- sorted_mat(
    tree_res,
    target_lab = "Outcome", show = "test"
  )

  .fixture_env$cached <- list(
    data_all   = data_all,
    fit        = fit,
    var_imp    = var_imp,
    tree_res   = tree_res,
    sorted_dat = sorted_dat
  )

  .fixture_env$cached
}
