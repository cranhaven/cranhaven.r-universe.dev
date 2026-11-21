design_fixture <- function() {
  data.frame(
    exposure = factor(rep(c("ref", "exp"), each = 50), levels = c("ref", "exp")),
    outcome = factor(c(rep("No", 25), rep("Yes", 25), rep("No", 10), rep("Yes", 40)),
      levels = c("No", "Yes")
    ),
    age = rep(30:79, 2)
  )
}

advice_ids <- function(x) {
  vapply(x$advice, `[[`, character(1), "id")
}

make_diag_data <- function(tp, fp, fn, tn,
                           ref_levels = c("No", "Yes"),
                           test_levels = c("No", "Yes")) {
  test <- c(
    rep(test_levels[2], tp),
    rep(test_levels[2], fp),
    rep(test_levels[1], fn),
    rep(test_levels[1], tn)
  )
  ref <- c(
    rep(ref_levels[2], tp),
    rep(ref_levels[1], fp),
    rep(ref_levels[2], fn),
    rep(ref_levels[1], tn)
  )

  data.frame(
    rapid = factor(test, levels = test_levels),
    gold = factor(ref, levels = ref_levels)
  )
}
