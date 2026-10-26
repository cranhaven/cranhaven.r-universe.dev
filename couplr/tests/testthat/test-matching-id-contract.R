# ==============================================================================
# The id contract: which column keys a matching, and what every downstream
# verb joins on.
# ==============================================================================

make_sides <- function(n_left = 6, n_right = 12, id_left = NULL, id_right = NULL) {
  set.seed(11)
  left <- data.frame(x = rnorm(n_left), y = rnorm(n_left))
  right <- data.frame(x = rnorm(n_right), y = rnorm(n_right))
  if (!is.null(id_left)) left$id <- id_left
  if (!is.null(id_right)) right$id <- id_right
  list(left = left, right = right)
}

# ------------------------------------------------------------------------------
# Duplicate ids (#35)
# ------------------------------------------------------------------------------

test_that("match_couples rejects a duplicate id on the left", {
  d <- make_sides(id_left = c("a", "b", "c", "c", "e", "f"),
                  id_right = as.character(1:12))
  expect_error(
    match_couples(d$left, d$right, vars = c("x", "y")),
    "Duplicate IDs found in left dataset"
  )
})

test_that("match_couples rejects a duplicate id on the right", {
  d <- make_sides(id_left = letters[1:6],
                  id_right = c(rep("r1", 2), paste0("r", 2:11)))
  expect_error(
    match_couples(d$left, d$right, vars = c("x", "y")),
    "Duplicate IDs found in right dataset"
  )
})

test_that("duplicate ids on both sides are reported for the side hit first", {
  d <- make_sides(id_left = c("a", "a", "c", "d", "e", "f"),
                  id_right = c(rep("r1", 2), paste0("r", 2:11)))
  expect_error(
    match_couples(d$left, d$right, vars = c("x", "y")),
    "Duplicate IDs found in left dataset"
  )
})

test_that("the duplicate error names the offending values", {
  d <- make_sides(id_left = c("a", "b", "c", "c", "e", "e"),
                  id_right = as.character(1:12))
  expect_error(
    match_couples(d$left, d$right, vars = c("x", "y")),
    "c, e"
  )
})

test_that("synthesized ids are unique by construction and are not rejected", {
  d <- make_sides()
  expect_warning(
    res <- match_couples(d$left, d$right, vars = c("x", "y")),
    "No id column found in left"
  )
  expect_equal(nrow(res$pairs), 6L)
  expect_false(anyDuplicated(res$pairs$left_id) > 0L)
})

test_that("join_matched returns one row per pair, never more", {
  d <- make_sides(id_left = letters[1:6], id_right = paste0("r", 1:12))
  res <- match_couples(d$left, d$right, vars = c("x", "y"))
  joined <- join_matched(res, d$left, d$right)
  expect_equal(nrow(joined), nrow(res$pairs))
})

# ------------------------------------------------------------------------------
# Numeric-looking character ids (#36)
# ------------------------------------------------------------------------------

test_that("zero-padded character ids join to their own covariates", {
  d <- make_sides(id_left = sprintf("%03d", 1:6),
                  id_right = sprintf("%03d", 7:18))
  res <- match_couples(d$left, d$right, vars = c("x", "y"))
  joined <- join_matched(res, d$left, d$right)

  expect_equal(nrow(joined), nrow(res$pairs))
  expect_false(anyNA(joined$x_left))
  expect_false(anyNA(joined$x_right))
  expect_type(joined$left_id, "character")
  expect_true(all(joined$left_id %in% d$left$id))
})

test_that("decimal- and exponent-looking character ids survive the join", {
  d <- make_sides(id_left = c("1.10", "1.1", "1e3", "1000", "TRUE", "T2"),
                  id_right = paste0("c", 1:12))
  res <- match_couples(d$left, d$right, vars = c("x", "y"))
  joined <- join_matched(res, d$left, d$right)

  expect_false(anyNA(joined$x_left))
  expect_setequal(joined$left_id, d$left$id)
})

test_that("a factor id column joins to its levels, not to its codes", {
  d <- make_sides(id_left = factor(paste0("T", 1:6)),
                  id_right = factor(paste0("C", 1:12)))
  res <- match_couples(d$left, d$right, vars = c("x", "y"))
  joined <- join_matched(res, d$left, d$right)

  expect_false(anyNA(joined$x_left))
  expect_false(anyNA(joined$x_right))
  expect_setequal(as.character(joined$left_id), as.character(d$left$id))
})

test_that("numeric ids still come back numeric", {
  d <- make_sides(id_left = 1:6, id_right = 7:18)
  res <- match_couples(d$left, d$right, vars = c("x", "y"))
  joined <- join_matched(res, d$left, d$right)

  expect_type(joined$left_id, "integer")
  expect_false(anyNA(joined$x_left))
})

# ------------------------------------------------------------------------------
# Caller-named id columns (#38)
# ------------------------------------------------------------------------------

test_that("left_id / right_id key the matching on the caller's column", {
  set.seed(3)
  left <- tibble::tibble(patient_id = paste0("T", 1:10),
                         age = rnorm(10, 50, 5))
  right <- tibble::tibble(patient_id = paste0("C", 1:20),
                          age = rnorm(20, 50, 5))

  res <- match_couples(left, right, vars = "age",
                       left_id = "patient_id", right_id = "patient_id")

  expect_true(all(res$pairs$left_id %in% left$patient_id))
  expect_true(all(res$pairs$right_id %in% right$patient_id))

  joined <- join_matched(res, left, right,
                         left_id = "patient_id", right_id = "patient_id")
  expect_equal(nrow(joined), nrow(res$pairs))
  expect_false(anyNA(joined$age_left))
  expect_false(anyNA(joined$age_right))
})

test_that("a named id column that is absent is an error, not a fallback", {
  d <- make_sides(id_left = letters[1:6], id_right = paste0("r", 1:12))
  expect_error(
    match_couples(d$left, d$right, vars = c("x", "y"), left_id = "patient_id"),
    "id column 'patient_id' not found in left"
  )
})

test_that("synthesizing ids warns and names the argument that fixes it", {
  set.seed(5)
  left <- tibble::tibble(site = paste0("s", 1:4), x = rnorm(4))
  right <- tibble::tibble(site = paste0("q", 1:8), x = rnorm(8))
  expect_warning(
    match_couples(left, right, vars = "x"),
    "left_id"
  )
})

# ------------------------------------------------------------------------------
# match_data weights and subclass (#17, #24)
# ------------------------------------------------------------------------------

test_that("match_data keys weights to units, not to lexicographic id order", {
  set.seed(9)
  # Numeric ids whose lexicographic order differs from their own: "1", "10",
  # "11", "2", ... A positional assignment after a sorting merge scrambles
  # here and matches on the keyed path.
  left <- data.frame(id = 1:11, x = rnorm(11))
  right <- data.frame(id = 101:130, x = rnorm(30))

  res <- full_match(left, right, vars = "x")
  md <- match_data(res, left, right)

  groups <- res$groups
  for (i in seq_len(nrow(md))) {
    row <- md[i, ]
    g <- groups[as.character(groups$id) == as.character(row$id) &
                  groups$side == (if (row$treatment == 1L) "left" else "right"), ]
    expect_equal(row$weights, g$weight[1])
    expect_equal(row$subclass, g$group_id[1])
  }
})

test_that("match_data keys CEM stratum weights to units", {
  set.seed(13)
  left <- data.frame(id = 1:11, x = rnorm(11, 0, 1))
  right <- data.frame(id = 101:130, x = rnorm(30, 0.2, 1))

  res <- cem_match(left, right, vars = "x")
  md <- match_data(res, left, right)
  matched <- res$matched

  for (i in seq_len(nrow(md))) {
    row <- md[i, ]
    m <- matched[as.character(matched$id) == as.character(row$id) &
                   matched$side == (if (row$treatment == 1L) "left" else "right"), ]
    expect_equal(row$weights, m$weight[1])
    expect_equal(as.character(row$subclass), as.character(m$stratum[1]))
  }
})

test_that("match_data emits one row per pair under ratio > 1", {
  set.seed(21)
  left <- data.frame(id = paste0("T", 1:5), x = rnorm(5))
  right <- data.frame(id = paste0("C", 1:20), x = rnorm(20))

  res <- match_couples(left, right, vars = "x", ratio = 2L)
  md <- match_data(res, left, right)

  expect_equal(nrow(md), 2L * nrow(res$pairs))
  expect_equal(length(unique(md$subclass)), nrow(res$pairs))
  # Every matched left unit totals weight 1, and the two sides balance.
  expect_equal(sum(md$weights[md$treatment == 1L]),
               length(unique(res$pairs$left_id)))
  expect_equal(sum(md$weights[md$treatment == 1L]),
               sum(md$weights[md$treatment == 0L]))
})

test_that("match_data emits one row per pair with replacement", {
  set.seed(23)
  left <- data.frame(id = paste0("T", 1:8), x = rnorm(8))
  right <- data.frame(id = paste0("C", 1:3), x = rnorm(3))

  res <- match_couples(left, right, vars = "x", replace = TRUE)
  md <- match_data(res, left, right)

  # A right unit reused across pairs appears once per pair it is in.
  expect_equal(nrow(md), 2L * nrow(res$pairs))
  expect_gt(nrow(md[md$treatment == 0L, ]), length(unique(res$pairs$right_id)))
  expect_equal(length(unique(md$subclass)), nrow(res$pairs))
})

test_that("match_data is 1:1-identical on a 1:1 design", {
  set.seed(27)
  left <- data.frame(id = 1:6, x = rnorm(6))
  right <- data.frame(id = 7:18, x = rnorm(12))

  res <- match_couples(left, right, vars = "x")
  md <- match_data(res, left, right)

  expect_equal(nrow(md), 2L * nrow(res$pairs))
  expect_true(all(md$weights == 1))
  expect_equal(sort(unique(md$subclass)), seq_len(nrow(res$pairs)))
})

test_that("match_data errors when a matched id is absent from the data", {
  set.seed(29)
  left <- data.frame(id = 1:6, x = rnorm(6))
  right <- data.frame(id = 7:18, x = rnorm(12))
  res <- match_couples(left, right, vars = "x")

  expect_error(match_data(res, left[1:2, ], right),
               "absent from `left`")
})

# ------------------------------------------------------------------------------
# The estimand (#29)
# ------------------------------------------------------------------------------

test_that("every front door records the estimand its design identifies", {
  set.seed(31)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:40, x = rnorm(30))

  expect_equal(match_couples(left, right, vars = "x")$info$estimand, "ATT")
  expect_equal(full_match(left, right, vars = "x")$info$estimand, "ATT")
  expect_equal(cem_match(left, right, vars = "x")$info$estimand, "ATT")
})

test_that("the estimand survives return_diagnostics = FALSE", {
  set.seed(33)
  left <- data.frame(id = 1:6, x = rnorm(6))
  right <- data.frame(id = 7:18, x = rnorm(12))
  res <- match_couples(left, right, vars = "x", return_diagnostics = FALSE)
  expect_equal(res$info$estimand, "ATT")
  expect_equal(res$info$focal, "left")
})

test_that("a design that discards focal units says how many", {
  set.seed(35)
  left <- data.frame(id = 1:10, x = c(rnorm(9), 500))
  right <- data.frame(id = 11:30, x = rnorm(20))
  res <- match_couples(left, right, vars = "x", max_distance = 1)
  expect_gt(res$info$focal_discarded, 0L)
})

test_that("as_matchit reads the estimand instead of guessing it", {
  set.seed(37)
  left <- data.frame(id = 1:10, x = rnorm(10), z = rnorm(10))
  right <- data.frame(id = 11:40, x = rnorm(30), z = rnorm(30))
  res <- match_couples(left, right, vars = c("x", "z"))

  mi <- as_matchit(res, left, right)
  expect_equal(mi$estimand, "ATT")

  # A caller whose `left` holds the controls says so; the design cannot know.
  mi_atc <- as_matchit(res, left, right, estimand = "ATC")
  expect_equal(mi_atc$estimand, "ATC")

  expect_error(as_matchit(res, left, right, estimand = "AT?"),
               "estimand must be one of")
})

test_that("as_matchit refuses to label a result that carries no estimand", {
  set.seed(39)
  left <- data.frame(id = 1:6, x = rnorm(6))
  right <- data.frame(id = 7:18, x = rnorm(12))
  res <- match_couples(left, right, vars = "x")
  res$info$estimand <- NULL

  expect_error(as_matchit(res, left, right), "carries no estimand")
})

test_that("as_matchit warns when the design did not retain every focal unit", {
  set.seed(41)
  left <- data.frame(id = 1:10, x = c(rnorm(9), 500))
  right <- data.frame(id = 11:30, x = rnorm(20))
  res <- match_couples(left, right, vars = "x", max_distance = 1)

  expect_warning(as_matchit(res, left, right), "matched focal subset")
})

test_that("as_matchit names each unit once when the two sides share ids (#66)", {
  set.seed(43)
  left <- data.frame(id = 1:8, x = rnorm(8))
  right <- data.frame(id = 1:24, x = rnorm(24))
  res <- match_couples(left, right, vars = "x")

  mi <- as_matchit(res, left, right)
  expect_length(mi$treat, 16L)
  expect_false(anyDuplicated(names(mi$treat)) > 0)
  expect_setequal(rownames(mi$match.matrix), paste0("left:", res$pairs$left_id))
  expect_true(all(mi$match.matrix[, 1] %in% names(mi$treat)[mi$treat == 0L]))
  expect_identical(rownames(mi$X), names(mi$treat))

  fm <- full_match(left, right, vars = "x")
  mi_full <- as_matchit(fm, left, right)
  expect_length(mi_full$treat, 32L)
})

test_that("as_matchit gives one entry per unit for ratio and replacement designs (#66)", {
  set.seed(45)
  left <- data.frame(id = 1:8, x = rnorm(8))
  right <- data.frame(id = 9:40, x = rnorm(32))

  mi2 <- as_matchit(match_couples(left, right, vars = "x", ratio = 2L),
                    left, right)
  expect_equal(dim(mi2$match.matrix), c(8L, 2L))
  expect_length(mi2$treat, 24L)
  expect_equal(as.vector(tapply(mi2$weights, mi2$treat, sum)), c(8, 8))
  expect_equal(nlevels(mi2$subclass), 8L)

  rep_res <- match_couples(left, right, vars = "x", replace = TRUE)
  mi_rep <- as_matchit(rep_res, left, right)
  expect_false(anyDuplicated(names(mi_rep$treat)) > 0)
  expect_equal(sum(mi_rep$treat == 0L),
               length(unique(rep_res$pairs$right_id)))
  if (anyDuplicated(rep_res$pairs$right_id) > 0) {
    expect_null(mi_rep$subclass)
  }
})

# ------------------------------------------------------------------------------
# Ecosystem generics (#39)
# ------------------------------------------------------------------------------

test_that("augment is the generics generic, not a second one", {
  expect_identical(couplr::augment, generics::augment)
  expect_true(!is.null(
    getS3method("augment", "matching_result", optional = TRUE,
                envir = asNamespace("couplr"))
  ))
})

test_that("generics::augment dispatches to the couplr method", {
  set.seed(43)
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:15, x = rnorm(10))
  res <- match_couples(left, right, vars = "x")

  expect_equal(
    generics::augment(res, left, right),
    join_matched(res, left, right)
  )
})

test_that("bal.tab methods are registered on cobalt's generic", {
  skip_if_not_installed("cobalt")
  loadNamespace("cobalt")
  # Delayed registration puts the methods in cobalt's own S3 table, which is
  # what lets a call made from inside another package dispatch to them.
  registered <- ls(get(".__S3MethodsTable__.", envir = asNamespace("cobalt")))
  for (cls in c("matching_result", "full_matching_result",
                "cem_result", "subclass_result")) {
    expect_true(paste0("bal.tab.", cls) %in% registered, info = cls)
  }
})

test_that("cobalt::bal.tab reaches the couplr method by dispatch", {
  skip_if_not_installed("cobalt")
  set.seed(45)
  left <- data.frame(id = 1:20, x = rnorm(20), z = rnorm(20))
  right <- data.frame(id = 21:80, x = rnorm(60), z = rnorm(60))
  res <- match_couples(left, right, vars = c("x", "z"))

  bt <- cobalt::bal.tab(res, left, right)
  expect_s3_class(bt, "bal.tab")
})

test_that("bal.tab splits ... between as_matchit and cobalt", {
  skip_if_not_installed("cobalt")
  set.seed(47)
  left <- data.frame(plot_id = 1:20, x = rnorm(20), z = rnorm(20))
  right <- data.frame(plot_id = 21:80, x = rnorm(60), z = rnorm(60))
  res <- match_couples(left, right, vars = c("x", "z"),
                       left_id = "plot_id", right_id = "plot_id")

  # `left_id` belongs to as_matchit(); `un` belongs to cobalt::bal.tab().
  bt <- cobalt::bal.tab(res, left, right,
                        left_id = "plot_id", right_id = "plot_id", un = TRUE)
  expect_s3_class(bt, "bal.tab")
})
