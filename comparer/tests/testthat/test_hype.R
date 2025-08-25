library(testthat)

# Hype par ----
test_that("hype par general", {
  parlist <- list(
    par_unif('a', 1, 3),
    par_log10('b', 1e-4, 1e2),
    par_discretenum('c', c(1,3,10,13,30)),
    par_integer('d', 9, 98),
    par_ordered('e', letters[5:14]),
    par_unordered('f', letters[15:18])
  )

  # All par should have same standard for these functions
  for (pp in parlist) {
    expect_true("par_hype" %in% class(pp))
    expect_true("R6" %in% class(pp))
    expect_error(spp <- pp$generate(runif(100)), NA)
    expect_true(all(pp$isvalid(spp)))
    expect_error(pp$getseq(10), NA)
    expect_error(capture.output(print(pp)), NA)
    # getseq
    expect_error(ppseq <- pp$getseq(n=30), NA)
    expect_true("list" %in% class(ppseq))
    expect_equal(c("trans", "raw"), names(ppseq))
    expect_true(all(pp$isvalid(ppseq$raw)))
    # Check conversion to mopar
    expect_error(mopp <- pp$convert_to_mopar(raw_scale = TRUE), NA)
    expect_true("mixopt_par" %in% class(mopp))
    expect_true("list" %in% class(mopp))
    expect_error(mopp <- pp$convert_to_mopar(raw_scale = FALSE), NA)
    expect_true("mixopt_par" %in% class(mopp))
    expect_true("list" %in% class(mopp))
  }
})

test_that("hype par", {
  # Create params
  # generic
  expect_error(p1 <- R6_par_hype$new(), NA)
  expect_true("par_hype" %in% class(p1))
  # Uniform
  expect_error(p1 <- par_unif("a", -1, 1), NA)
  expect_true("par_unif" %in% class(p1))
  expect_error(capture.output(print(p1)), NA)
  # Log scale
  expect_error(plog <- par_log10("a", 1e-8, 1), NA)
  expect_true("par_log10" %in% class(plog))
  expect_error(capture.output(print(plog)), NA)
  # Unordered
  expect_error(pun <- par_unordered("puno", letters[1:10]), NA)
  expect_error(capture.output(print(pun)), NA)
  expect_equal(pun$isvalid(c('a','e', 'z')), c(T,T,F))
  expect_error(pun <- par_unordered("puno", 13:19), NA)
  expect_error(capture.output(print(pun)), NA)
  expect_equal(pun$isvalid(18:21), c(T,T,F,F))
  # Unordered: logical
  expect_error(pun <- par_unordered("puno", c(T, F)), NA)
  expect_true(all(pun$isvalid(c(T, F))))
  expect_false(pun$isvalid(5))
  expect_false(pun$isvalid('a'))
  # Ordered
  expect_error(por <- par_ordered("puno", letters[3:11]), NA)
  expect_error(capture.output(print(por)), NA)
  expect_equal(por$isvalid(c('c','d','y','z')), c(T,T,F,F))
  expect_error(por <- par_ordered("puno", 4:9), NA)
  expect_error(capture.output(print(por)), NA)
  expect_equal(por$isvalid(8:11), c(T,T,F,F))
  # Discrete num
  expect_error(pdn <- par_discretenum("puno", letters))
  expect_error(pdn <- par_discretenum("puno", c(1,3,2)))
  expect_error(pdn <- par_discretenum("puno", c(.1,1,19)), NA)
  expect_error(capture.output(print(pdn)), NA)
  expect_equal(pdn$isvalid(c(1,3,13,19)), c(T,F,F,T))
  # Integer
  expect_error(pi <- par_integer('pint', 4,'c'))
  expect_error(pi <- par_integer('pint', 'c', 9))
  expect_error(pi <- par_integer('pint', 4:9))
  expect_error(pi <- par_integer('pint', 4,14), NA)
  expect_error(capture.output(print(pi)), NA)
  expect_equal(pi$isvalid(c(8,6,-2,22)), c(T,T,F,F))
})

# Hype basics ----
test_that("hype works", {
  # Create params
  p1 <- R6_par_hype$new()
  expect_true("par_hype" %in% class(p1))
  p1 <- par_unif("a", -1, 1)
  expect_true("par_unif" %in% class(p1))
  plog <- par_log10("a", 1e-8, 1)
  expect_true("par_log10" %in% class(plog))

  # Create hype
  h1 <- hype(eval_func = function(a) {a^2}, p1, n_lhs=3)
  # Check basics
  expect_true("hype" %in% class(h1))
  # expect_equal(nrow(h1$X), 3)
  expect_true(is.null(h1$X))
  expect_equal(nrow(h1$ffexp$rungrid2()), 3)
  expect_equal(length(h1$Z), 0)
  expect_error(h1$run_all(), NA)
  expect_equal(length(h1$Z), 3)

  # Check add EI
  expect_error(suppressWarnings(h1$add_EI(1)), NA)
  expect_error(suppressWarnings(h1$add_EI(1)), NA)
  expect_error(h1$run_all(), NA)
  expect_error({suppressWarnings(h1$run_EI_for_time(1, 1))}, NA)

  # Check plots
  expect_error(plotorder <- h1$plotorder(), NA)
  expect_is(plotorder, 'ggplot')
  expect_error(plotX <- h1$plotX(), NA)
  expect_is(plotX, 'ggplot')
  expect_error(plotplot <- plot(h1), NA)
  expect_is(plotplot, 'ggplot')
  expect_error(plotXorder <- h1$plotXorder(), NA)
  expect_is(plotXorder, 'ggplot')
  rm(h1)
})
test_that("2 inputs", {
  # Two inputs
  expect_error({
    h2 <- hype(eval_func = function(a, b) {a^2 - sin(2*pi*b)},
               par_unif("a", -1, 1),
               par_unif("b", -1,1),
               n_lhs=3)
  }, NA)
  expect_error(h2$run_all(), NA)
  expect_error(suppressWarnings(h2$add_EI(n=3)), NA)
  expect_error(h2$run_all(), NA)
  # Add LHS
  expect_error(h2$add_LHS(n=3), NA)
  expect_error(h2$run_all(), NA)
  # Plot interaction
  if (requireNamespace('ContourFunctions', quietly = TRUE)) {
    expect_error(plotint <- h2$plotinteractions(), NA)
    expect_is(plotint, 'ggplot')
  }
  # Plot pairs
  expect_error(plotpairs <- h2$pairs(), NA)
  expect_is(plotpairs, 'ggplot')
  # Print object
  expect_error(printout <- capture.output(print(h2)), NA)
  printout <- capture.output(print(h2))
  expect_true(is.character(printout), length(printout) >= 1)

  # Break if no params
  expect_error(hype(eval_func = function(a) {a^2}, n_lhs=3))
  # Break if not a param
  expect_error(hype(eval_func = function(a) {a^2}, sin, n_lhs=3))
  # Break if X0 given as matrix
  expect_error(hype(eval_func = function(a) {a^2}, par_unif('a',1,3),
                    X0=matrix(1:3, ncol=1), n_lhs=3))
  # Break if n_lhs not given
  expect_error(hype(eval_func = function(a) {a^2}, par_unif('a',1,3)))

})

test_that("4 inputs", {
  # 4 inputs --
  expect_error({
    h2 <- hype(eval_func = function(a, b, c, d) {10*a^2*c -
        abs(d)^.3*sin(2*pi*b) -.1*c*d},
               par_unif("a", -1, 1),
               par_unif("b", -1,1),
               par_unif("c", -10,1),
               par_unif("d", -100,100),
               n_lhs=3)
  }, NA)
  expect_error(h2$run_all(), NA)
  expect_error(suppressWarnings(h2$add_EI(n=3)), NA)
  expect_error(h2$run_all(), NA)
  # Add LHS
  expect_error(h2$add_LHS(n=3), NA)
  expect_error(h2$run_all(), NA)
  # Plot interactions
  if (requireNamespace('ContourFunctions', quietly = TRUE)) {
    expect_error(h2$plotinteractions(), NA)
  }
})


# Hype add data ----
test_that("Hype add data", {
  # Test adding in data using add_data

  f1 <- function(a, b, c) {-a^2*log(b,10)^2}

  n0 <- 10
  x0 <- data.frame(a=runif(n0, -1,1),
                   b=10^runif(n0, -3,4),
                   c=runif(n0,  1,2))
  y0 <- numeric(n0)
  for (i in 1:n0) {
    y0[i] <- f1(x0$a[i], x0$b[i], x0$c[i])
  }
  cbind(x0, y0)

  # 3 inputs, 2 matter, interaction
  expect_error({
    x9 <- hype(eval_func = f1,
               par_unif("a", -1, 1),
               par_log10("b", 10^-3, 10^4),
               par_unif("c", 1,2),
               n_lhs=6)
    x9$run_all()
  }, NA)
  expect_true(length(x9$Z) == 6)
  # x9$plotX()
  # debugonce(x9$add_data)
  expect_error({
    x9$add_data(X=x0, Z=y0)
  }, NA)
  # x9
  # x9$plotX2()
  expect_error(suppressWarnings({
    x9$add_EI(1)
    x9$run_all()
  }), NA)
  # x9$plotorder()
  # x9$plotX()
  # x9$plotinteractions()



  # Test adding data when creating object

  # Give in X0, but not Z0
  expect_error({
    r5 <- hype(eval_func = f1,
               par_unif("a", -1, 1),
               par_log10("b", 10^-3, 10^4),
               par_unif("c", 1,2),
               X0=x0)
  }, NA)
  expect_true(length(r5$ffexp$completed_runs) == 10,
              is.null(r5$X),
              !r5$ffexp$completed_runs)
  # r5

  # Give in X0 and Z0
  expect_error({
    r8 <- hype(eval_func = f1,
               par_unif("a", -1, 1),
               par_log10("b", 10^-3, 10^4),
               par_unif("c", 1,2),
               X0=x0, Z0=y0)
  }, NA)
  # r8
  # r8$plotX()

  # Test changing parameter bounds

  n2 <- hype(eval_func = f1,
             par_unif("a", -1, 1),
             par_log10("b", 10^-3, 10^4),
             par_log10("c", 1,100),
             n_lhs=6)
  n2$run_all()
  # n2$plotX()
  # n2$parlist
  # n2$parlowerraw
  # n2$parlowertrans
  # n2$parupperraw
  # n2$paruppertrans
  # expect_equal(n2$parlowerraw, c(-1, .001, 1))
  expect_equal(n2$parlowertrans, c(-1, -3, 0))
  # expect_equal(n2$parupperraw, c(1, 1e4, 1e2))
  expect_equal(n2$paruppertrans, c(1, 4, 2))
  n2$change_par_bounds('a', lower=0)
  n2$change_par_bounds('b', upper=10^8)
  n2$change_par_bounds('c', lower=.1, upper=1e3)
  # expect_equal(n2$parlowerraw, c(0, .001, .1))
  expect_equal(n2$parlowertrans, c(0, -3, -1))
  # expect_equal(n2$parupperraw, c(1, 1e8, 1e3))
  expect_equal(n2$paruppertrans, c(1, 8, 3))
  # expect_true(n2$)
  # n2$parlowerraw
  # n2$parlowertrans
  # n2$parupperraw
  # n2$paruppertrans
  # n2$plotX()

  expect_error({
    hype(eval_func = f1,
         par_unif("a", -1, 1),
         par_log10("b", 10^-3, 10^4),
         par_unif("c", 1,2),
         X0=list(a=runif(5)))
  })
})

# Discrete params ----
test_that("discrete params", {
  # Test discrete par
  expect_error({
    hp <- hype(
      eval_func = function(a, b, c) {
        -1e-3*a^2*log(b,10)^2*ifelse(c=='a', 1, 2) + rnorm(length(a),0,1e-1)
      },
      par_unif("a", 6, 8),
      par_log10("b", 1e-8, 1e-2),
      par_unordered("c", c('a', 'b')),
      n_lhs=21)
  }, NA)
  expect_true(!hp$par_all_cts)
  # plotX doesn't work until something has been evaluated
  expect_error(hp$plotX())
  expect_error(hp$plotXorder())
  expect_error(hp$plotinteractions())
  hp$run_all()
  expect_equal(length(hp$Z), 21)
  expect_error({
    hp$plotX(addEIlines = T, addlines = T)
  }, NA)
  expect_error({
    hp$plotXorder()
  }, NA)
  expect_error({
    hp$add_EI(1, model='gaupro')
    hp$run_all()
  }, NA)
  # print('hpZ length is'); print(length(hp$Z))
  expect_equal(length(hp$Z), 22)
})


# All param types ----
test_that("hype with all params type", {
  # Test all param types
  expect_error({
    hp <- hype(eval_func = function(a, b, c, d, e, f) {
      -1e-3*a^2*log(b,10)^2*ifelse(c=='a', 1, 2) +
        e +
        .2*f +
        rnorm(length(a),0,1e-1)
    },
    par_unif("a", 6, 8),
    par_log10("b", 1e-8, 1e-2),
    par_unordered("c", c('a', 'b')),
    par_ordered("d", c('a', 'b')),
    par_discretenum("e", c(1,3,10)),
    par_integer('f', 5, 15),
    n_lhs=21)
  }, NA)
  expect_true(!hp$par_all_cts)
  hp$run_all()
  expect_equal(length(hp$Z), 21)
  expect_error({
    hp$plotX(addEIlines = T, addlines = T)
  }, NA)
  expect_error({
    hp$plotXorder()
  }, NA)
  # Find best_param
  expect_error(hppb <- hp$best_params(), NA)
  expect_true(is.list(hppb))
  expect_true(all(names(hppb) %in% c("unevaluated", "evaluated")))
  expect_warning({
    hp$add_EI(1, model='gaupro')
    hp$run_all()
  }, NA)
  expect_equal(length(hp$Z), 22)
  expect_error({
    hp$add_EI(2, model='gaupro')
    hp$run_all()
  }, NA)
})

