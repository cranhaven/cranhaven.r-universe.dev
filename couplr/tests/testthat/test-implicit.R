# memory_mode = "implicit": the public surface of the edge-generation loop.
#
# The 298 Catch2 cases in cpp_tests/ own the loop itself -- the restricted
# master, the pricing round, the feasibility phase and the certificate the three
# of them assemble. What this file covers is the layer above them: the
# orientation the loop is handed, the answer read back in the caller's units,
# the certificate and the search record arriving where a caller finds them, and
# the designs the mode declines rather than solving as something they are not.
#
# The claim every equality test makes is condition 1: the loop's answer is the
# answer a solve over every pair gives, on the same numbers, in the same
# session. It is asserted on the total rather than on the match vector, because
# the two paths reach an optimum through different arc sets and are free to name
# different ones whenever the optimum is not unique.

IMPLICIT_COST_TOL <- 1e-9

test_that("the loop reaches the complete solve's optimum", {
  set.seed(4041)
  shapes <- list(c(6, 6), c(10, 40), c(25, 25), c(40, 120), c(1, 30))

  for (shape in shapes) {
    cost <- cert_problem(shape[1], shape[2])
    dense <- assignment(cost)
    loop <- assignment(cost, memory_mode = "implicit")

    expect_equal(loop$total_cost, dense$total_cost, tolerance = IMPLICIT_COST_TOL)
    expect_equal(loop$status, "optimal")
    expect_equal(loop$method_used, "implicit")
    expect_length(loop$match, shape[1])
    expect_true(all(loop$match > 0L))
    expect_equal(anyDuplicated(loop$match), 0L)
  }
})

test_that("the answer carries a certificate of its own", {
  set.seed(4042)
  cost <- cert_problem(15, 60)
  loop <- assignment(cost, memory_mode = "implicit")

  expect_s3_class(loop$certificate, "assignment_certificate")
  expect_true(loop$certificate$certified_optimal)
  expect_true(loop$certificate$dual_feasible)
  expect_true(loop$certificate$complementary_slackness)
  expect_equal(loop$certificate$primal_objective, loop$total_cost,
               tolerance = IMPLICIT_COST_TOL)
  # The certificate is over the complete problem, not over the pairs the master
  # held, so its row and column counts are the caller's.
  expect_equal(loop$certificate$n_rows, 15)
  expect_equal(loop$certificate$n_cols, 60)
})

test_that("the certificate bounds how far the answer can be from the optimum", {
  set.seed(4044)
  cost <- cert_problem(15, 60)
  loop  <- assignment(cost, memory_mode = "implicit")
  dense <- assignment(cost, method = "jv")

  bound <- loop$certificate$max_suboptimality
  expect_true(is.finite(bound))
  expect_gte(bound, 0)
  # What the bound claims: no feasible matching beats this one by more than
  # this much. The dense solve is one such matching.
  expect_lte(loop$total_cost - dense$total_cost, bound + IMPLICIT_COST_TOL)
})

test_that("the duals it returns certify the matching independently", {
  set.seed(4043)
  cost <- cert_problem(12, 50)
  loop <- assignment(cost, memory_mode = "implicit")

  # verify_assignment() reads u and v off the result and checks them against
  # every admissible pair of the caller's matrix. Duals in the wrong
  # orientation, or on the wrong scale, fail here rather than agreeing.
  again <- verify_assignment(loop, cost)
  expect_true(again$certified_optimal)
  expect_equal(again$primal_objective, loop$total_cost,
               tolerance = IMPLICIT_COST_TOL)
})

test_that("a problem with more rows than columns is solved on its transpose", {
  set.seed(4044)
  cost <- cert_problem(12, 40)
  tall <- t(cost)

  dense <- assignment(tall)
  loop <- assignment(tall, memory_mode = "implicit")

  expect_equal(loop$total_cost, dense$total_cost, tolerance = IMPLICIT_COST_TOL)
  expect_length(loop$match, 40)
  expect_equal(sum(loop$match > 0L), 12)
  expect_true(loop$certificate$transposed)
  expect_true(loop$certificate$certified_optimal)
})

test_that("maximization is the same problem with the sign turned around", {
  set.seed(4045)
  cost <- cert_problem(10, 30)

  dense <- assignment(cost, maximize = TRUE)
  loop <- assignment(cost, maximize = TRUE, memory_mode = "implicit")

  expect_equal(loop$total_cost, dense$total_cost, tolerance = IMPLICIT_COST_TOL)
  expect_true(loop$certificate$certified_optimal)
  # Reported in the caller's sign, which is what makes the objective the total.
  expect_equal(loop$certificate$primal_objective, loop$total_cost,
               tolerance = IMPLICIT_COST_TOL)
  expect_true(loop$total_cost > 0)
})

test_that("forbidden pairs are pairs the loop never generates", {
  set.seed(4046)
  cost <- cert_problem(20, 60, sparsity = 0.4)

  dense <- assignment(cost)
  loop <- assignment(cost, memory_mode = "implicit")

  expect_equal(loop$total_cost, dense$total_cost, tolerance = IMPLICIT_COST_TOL)
  expect_true(loop$certificate$certified_optimal)
  expect_equal(loop$certificate$n_forbidden_matched, 0)
})

test_that("the search record says what the search cost", {
  set.seed(4047)
  cost <- cert_problem(30, 90)
  loop <- assignment(cost, memory_mode = "implicit")
  search <- loop$search

  expect_equal(search$possible_edges, 30 * 90)
  expect_true(search$candidate_edges > 0)
  expect_true(search$candidate_edges < search$possible_edges)
  expect_true(search$edges_evaluated > 0)
  expect_equal(nrow(search$rounds), search$n_rounds)
  # Every pair the candidate set holds was put there by a round.
  expect_equal(sum(search$rounds$pairs_added), search$candidate_edges)
  # The last round is the one that proved there was nothing left to add.
  expect_equal(search$rounds$n_violators[search$n_rounds], 0)
  expect_true(all(search$rounds$kind %in% c("priced", "reseeded")))
})

test_that("certify = FALSE returns the answer without the proof", {
  set.seed(4048)
  cost <- cert_problem(10, 40)
  loop <- assignment(cost, memory_mode = "implicit", certify = FALSE)

  expect_null(loop$certificate)
  expect_equal(loop$status, "optimal")
  expect_equal(loop$total_cost, assignment(cost)$total_cost,
               tolerance = IMPLICIT_COST_TOL)
})

test_that("an infeasible problem comes back with Hall's witness", {
  # Two rows whose only admissible column is the same one: no assignment
  # matches both, and the deficient set is the pair of them.
  cost <- matrix(NA_real_, 3, 4)
  cost[1, 1] <- 1
  cost[2, 1] <- 1
  cost[3, 3] <- 1

  loop <- assignment(cost, memory_mode = "implicit")

  expect_equal(loop$status, "infeasible")
  expect_true(all(loop$match == 0L))
  expect_true(setequal(loop$witness$rows, c(1L, 2L)))
  expect_equal(loop$witness$cols, 1L)
  expect_true(loop$witness$verified)
  expect_true(loop$witness$certified)
  expect_false(loop$witness$transposed)
  # Nothing was proven optimal, so nothing claims to have been.
  expect_null(loop$certificate)
})

# ---------------------------------------------------------------------------
# The lazy specification, which is the shape the memory saving is for
# ---------------------------------------------------------------------------

implicit_units <- function(n, seed) {
  set.seed(seed)
  data.frame(id = paste0("u", seq_len(n)), x = stats::rnorm(n),
             y = stats::rnorm(n), z = stats::rnorm(n))
}

test_that("match_couples solves the same matching either way", {
  left <- implicit_units(40, 5501)
  right <- implicit_units(200, 5502)

  dense <- match_couples(left, right, vars = c("x", "y", "z"),
                         check_costs = FALSE)
  loop <- match_couples(left, right, vars = c("x", "y", "z"),
                        memory_mode = "implicit", check_costs = FALSE)

  expect_equal(loop$info$total_distance, dense$info$total_distance,
               tolerance = IMPLICIT_COST_TOL)
  expect_equal(nrow(loop$pairs), nrow(dense$pairs))
  expect_equal(loop$status, "optimal")
})

test_that("a caliper and a distance bound reach the same optimum", {
  left <- implicit_units(30, 5503)
  right <- implicit_units(150, 5504)

  dense <- match_couples(left, right, vars = c("x", "y", "z"),
                         max_distance = 2, calipers = list(x = 1),
                         check_costs = FALSE)
  loop <- match_couples(left, right, vars = c("x", "y", "z"),
                        max_distance = 2, calipers = list(x = 1),
                        memory_mode = "implicit", check_costs = FALSE)

  expect_equal(loop$info$total_distance, dense$info$total_distance,
               tolerance = IMPLICIT_COST_TOL)
  expect_equal(nrow(loop$pairs), nrow(dense$pairs))
})

test_that("the mahalanobis metric crosses as the same inverse covariance", {
  left <- implicit_units(20, 5505)
  right <- implicit_units(90, 5506)

  dense <- match_couples(left, right, vars = c("x", "y", "z"),
                         distance = "mahalanobis", check_costs = FALSE)
  loop <- match_couples(left, right, vars = c("x", "y", "z"),
                        distance = "mahalanobis", memory_mode = "implicit",
                        check_costs = FALSE)

  expect_equal(loop$info$total_distance, dense$info$total_distance,
               tolerance = IMPLICIT_COST_TOL)
})

test_that("the certificate and the search record survive a default call", {
  left <- implicit_units(25, 5507)
  right <- implicit_units(120, 5508)

  loop <- match_couples(left, right, vars = c("x", "y", "z"),
                        memory_mode = "implicit", check_costs = FALSE)

  # return_diagnostics is FALSE by default and truncates info to the solver
  # summary plus the design fields, which is why neither of these lives there.
  expect_named(loop$info, c("method", "n_matched", "total_distance",
                            "estimand", "focal", "focal_discarded"))
  expect_s3_class(loop$certificate, "assignment_certificate")
  expect_true(loop$certificate$certified_optimal)
  expect_equal(loop$search$possible_edges, 25 * 120)
  expect_true(loop$search$candidate_edges < loop$search$possible_edges)
})

test_that("an infeasible caliper is reported with the witness, not an error", {
  left <- implicit_units(20, 5509)
  right <- implicit_units(60, 5510)

  expect_warning(
    loop <- match_couples(left, right, vars = c("x", "y", "z"),
                          max_distance = 1e-6, memory_mode = "implicit",
                          check_costs = FALSE),
    "No valid pairs"
  )

  expect_equal(loop$status, "infeasible")
  expect_equal(nrow(loop$pairs), 0L)
  expect_true(loop$witness$verified)
})

test_that("a specification built for the loop is solved by it", {
  left <- implicit_units(15, 5511)
  right <- implicit_units(70, 5512)

  spec <- build_cost_matrix(left, right, vars = c("x", "y", "z"),
                            memory_mode = "implicit")
  expect_s3_class(spec, "lazy_cost_spec")
  expect_equal(lazy_cost_spec_mode(spec), "implicit")

  # memory_mode is not named here: the specification carries which mode it was
  # resolved to, and that is what decides the solve.
  loop <- assignment(spec)
  expect_equal(loop$method_used, "implicit")
  expect_true(loop$certificate$certified_optimal)

  # Naming the other mode overrides it, and both answer the same question.
  lazy <- assignment(spec, memory_mode = "lazy")
  expect_equal(lazy$method_used, "jv")
  expect_equal(loop$total_cost, lazy$total_cost, tolerance = IMPLICIT_COST_TOL)
})

test_that("a specification built without a mode is a lazy one", {
  left <- implicit_units(8, 5513)
  right <- implicit_units(20, 5514)
  spec <- build_cost_matrix(left, right, vars = c("x", "y"),
                            memory_mode = "lazy")
  spec$mode <- NULL

  expect_equal(lazy_cost_spec_mode(spec), "lazy")
  expect_equal(assignment(spec)$method_used, "jv")
})

# ---------------------------------------------------------------------------
# What the mode declines
# ---------------------------------------------------------------------------

test_that("auto never resolves to implicit", {
  # Large enough that the RAM guard has an opinion, which is the only rule
  # "auto" has and it is not a rule about the loop. The guard says so out loud
  # on a problem this size, which is the warning being asserted here.
  expect_warning(
    resolved <- resolve_memory_mode(1e5, 1e5, "auto",
                                    solver_supports_lazy = FALSE,
                                    solver_supports_implicit = TRUE),
    "dense solve of this problem peaks"
  )
  expect_equal(resolved, "dense")
  expect_equal(resolve_memory_mode(50, 50, "auto",
                                   solver_supports_lazy = TRUE,
                                   solver_supports_implicit = TRUE),
               "dense")
})

test_that("a path that cannot run the loop says so", {
  expect_error(resolve_memory_mode(10, 20, "implicit"),
               "not supported")
  expect_equal(resolve_memory_mode(10, 20, "implicit",
                                   solver_supports_implicit = TRUE),
               "implicit")
})

test_that("the designs the loop does not solve are declined", {
  left <- implicit_units(12, 5515)
  right <- implicit_units(40, 5516)
  vars <- c("x", "y", "z")

  expect_error(
    match_couples(left, right, vars = vars, memory_mode = "implicit",
                  method = "greedy"),
    "does not support memory_mode = \"implicit\""
  )
})

test_that("blocking and the loop are alternatives, not a combination", {
  left <- implicit_units(12, 5517)
  right <- implicit_units(40, 5518)
  left$block <- rep(c("a", "b"), length.out = 12)
  right$block <- rep(c("a", "b"), length.out = 40)

  expect_error(
    match_couples(left, right, vars = c("x", "y"), block_id = "block",
                  memory_mode = "implicit"),
    "not supported with blocking"
  )
})

test_that("certify belongs to the path that produces one", {
  left <- implicit_units(10, 5519)
  right <- implicit_units(30, 5520)

  expect_error(
    match_couples(left, right, vars = c("x", "y"), certify = TRUE),
    "certify. applies to memory_mode"
  )
})

test_that("a named solver is not something the loop can honour", {
  set.seed(4049)
  cost <- cert_problem(8, 20)

  expect_error(assignment(cost, method = "hungarian", memory_mode = "implicit"),
               "does not apply under memory_mode")
  expect_error(assignment(cost, memory_mode = "implicit",
                          cardinality = "maximum"),
               "not supported under memory_mode")
})

test_that("the search knobs are checked before the loop runs", {
  set.seed(4050)
  cost <- cert_problem(6, 12)

  expect_error(couplr:::.assignment_implicit(cost, keep_per_row = 0),
               "keep_per_row")
  expect_error(couplr:::.assignment_implicit(cost, width = 2.5), "width")
  expect_error(couplr:::.assignment_implicit(cost, width = -1), "width")
  expect_error(couplr:::.assignment_implicit(cost, max_rounds = 0), "max_rounds")
  expect_error(couplr:::.assignment_implicit(cost, tol = -1), "tol")
  expect_error(couplr:::.assignment_implicit(cost, certify = NA), "certify")
})

test_that("the knobs change the rounds and not the answer", {
  set.seed(4051)
  cost <- cert_problem(20, 80)
  reference <- assignment(cost)$total_cost

  for (knobs in list(list(keep_per_row = 1, width = 1),
                     list(keep_per_row = 3, width = 20),
                     list(keep_per_row = 10, width = 2))) {
    loop <- do.call(couplr:::.assignment_implicit,
                    c(list(cost = cost), knobs))
    expect_equal(loop$total_cost, reference, tolerance = IMPLICIT_COST_TOL)
    expect_true(loop$certificate$certified_optimal)
  }
})

test_that("the seed is sized from the problem when no width is named", {
  set.seed(4053)
  cost <- cert_problem(40, 400)
  reference <- assignment(cost)$total_cost

  sized <- couplr:::.assignment_implicit(cost)
  # Six columns per doubling of ncol, which is what implicit_seed_width() reads
  # off the source, and the run says which width it ran on.
  expect_equal(sized$search$seed_width, 6L * as.integer(ceiling(log2(400))))
  expect_equal(sized$total_cost, reference, tolerance = IMPLICIT_COST_TOL)
  expect_true(sized$certificate$certified_optimal)

  # A source with fewer columns than the rule asks for takes all of them.
  narrow <- couplr:::.assignment_implicit(cert_problem(4, 8))
  expect_equal(narrow$search$seed_width, 8L)

  # A named width is still the width it names, and it is the candidate set the
  # seed leaves behind that it moves, not the answer.
  named <- couplr:::.assignment_implicit(cost, width = 3)
  expect_equal(named$search$seed_width, 3L)
  expect_equal(named$total_cost, reference, tolerance = IMPLICIT_COST_TOL)
  expect_true(named$certificate$certified_optimal)
  expect_lt(named$search$candidate_edges, sized$search$candidate_edges)
})

test_that("a round cap the loop cannot finish inside is reported as one", {
  set.seed(4052)
  cost <- cert_problem(30, 90)

  loop <- couplr:::.assignment_implicit(cost, keep_per_row = 1, width = 1,
                                        max_rounds = 1)
  # One round expands the seed and no more, so the loop stops on the cap with
  # nothing proven, and says so rather than reporting the master's own word.
  expect_equal(loop$status, "iteration_limit")
  expect_null(loop$certificate)
})

test_that("an empty problem is refused the way every other path refuses it", {
  expect_error(assignment(matrix(numeric(0), 0, 0), memory_mode = "implicit"),
               "at least one row and one column")
  expect_error(assignment(matrix(NaN, 2, 3), memory_mode = "implicit"),
               "NaN not allowed")
})

test_that("the loop repairs a master its rounding left short of the exact optimum (#65)", {
  # Standardised covariates give costs whose differences sit below their
  # rounding, where the flow master stopped on a flow a residual cycle of
  # exactly negative cost still improved, and the certificate fell back to a
  # tolerance.
  data("hospital_staff", package = "couplr", envir = environment())
  treated <- transform(hospital_staff$nurses_extended, id = nurse_id)
  control <- transform(hospital_staff$controls_extended, id = nurse_id)
  covars <- c("age", "experience_years", "certification_level")
  for (sc in c("standardize", "robust")) {
    m <- suppressMessages(match_couples(treated, control, vars = covars,
                                        scale = sc, memory_mode = "implicit",
                                        certify = TRUE))
    expect_identical(m$certificate$arithmetic, "exact", info = sc)
    expect_equal(m$certificate$max_suboptimality, 0, info = sc)
    dense <- suppressMessages(match_couples(treated, control, vars = covars,
                                            scale = sc, memory_mode = "dense"))
    expect_equal(m$info$total_distance, dense$info$total_distance,
                 tolerance = 1e-12, info = sc)
  }
})
