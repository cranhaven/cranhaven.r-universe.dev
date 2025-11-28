#########################
# Author : Gireg Willame
# June 2022.
#
# The goal is to check that the BT_Summary
#   computation is correct.
#
########################

testthat::test_that("Check the BT_Summary function - Inputs", {
  skip_on_cran()

  # Create datasets.
  set.seed(4)
  n <- 10000 #100000

  Gender <- factor(sample(c("male", "female"), n, replace = TRUE))
  Age <- sample(c(18:65), n, replace = TRUE)
  Split <- factor(sample(c("yes", "no"), n, replace = TRUE))
  Sport <- factor(sample(c("yes", "no"), n, replace = TRUE))

  lambda <- 0.1 * ifelse(Gender == "male", 1.1, 1)
  lambda <- lambda * (1 + 1 / (Age - 17) ^ 0.5)
  lambda <- lambda * ifelse(Sport == "yes", 1.15, 1)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR * lambda)
  Y_normalized <- Y / ExpoR
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)

  # Run a BT algo.
  set.seed(4)
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  BT_algo <- do.call(BT, paramsBT)

  # Check n.iter
  n.iter <- 0
  expect_error(summary(BT_algo, n.iter = n.iter))
  cBars <- 0.4
  expect_error(summary(BT_algo, n.iter = n.iter))
  cBars <- 2.546
  expect_error(summary(BT_algo, n.iter = n.iter))
  n.iter <- c(3, 4)
  expect_error(summary(BT_algo, n.iter = n.iter))
  n.iter <- NULL
  expect_error(summary(BT_algo, n.iter = n.iter))
  n.iter <- NA
  expect_error(summary(BT_algo, n.iter = n.iter))
  n.iter <- "Text"
  expect_error(summary(BT_algo, n.iter = n.iter))
  n.iter <- F
  expect_error(summary(BT_algo, n.iter = n.iter))

  # Check cBars
  cBars <- 0.4
  expect_error(summary(BT_algo, cBars = cBars))
  cBars <- 2.546
  expect_error(summary(BT_algo, cBars = cBars))
  cBars <- c(3, 4)
  expect_error(summary(BT_algo, cBars = cBars))
  cBars <- NULL
  expect_error(summary(BT_algo, cBars = cBars))
  cBars <- NA
  expect_error(summary(BT_algo, cBars = cBars))
  cBars <- "Text"
  expect_error(summary(BT_algo, cBars = cBars))
  # cBars <- F ; expect_error(summary(BT_algo, cBars=cBars)) # cBars = F -> considered as 0.

  # Check plot_it
  plot_it <- 1
  expect_error(summary(BT_algo, plot_it = plot_it))
  plot_it <- 0.4
  expect_error(summary(BT_algo, plot_it = plot_it))
  plot_it <- 2.785
  expect_error(summary(BT_algo, plot_it = plot_it))
  plot_it <-
    c(3, 4)
  expect_error(summary(BT_algo, plot_it = plot_it))
  plot_it <- NULL
  expect_error(summary(BT_algo, plot_it = plot_it))
  plot_it <- NA
  expect_error(summary(BT_algo, plot_it = plot_it))
  plot_it <-
    "Text"
  expect_error(summary(BT_algo, plot_it = plot_it))

  # Check order_it
  order_it <- 1
  expect_error(summary(BT_algo, order_it = order_it))
  order_it <-
    0.4
  expect_error(summary(BT_algo, order_it = order_it))
  order_it <-
    2.785
  expect_error(summary(BT_algo, order_it = order_it))
  order_it <-
    c(3, 4)
  expect_error(summary(BT_algo, order_it = order_it))
  order_it <-
    NULL
  expect_error(summary(BT_algo, order_it = order_it))
  order_it <- NA
  expect_error(summary(BT_algo, order_it = order_it))
  order_it <-
    "Text"
  expect_error(summary(BT_algo, order_it = order_it))

  # Check normalize
  normalize <-
    1
  expect_error(summary(BT_algo, normalize = normalize))
  normalize <-
    0.4
  expect_error(summary(BT_algo, normalize = normalize))
  normalize <-
    2.785
  expect_error(summary(BT_algo, normalize = normalize))
  normalize <-
    c(3, 4)
  expect_error(summary(BT_algo, normalize = normalize))
  normalize <-
    NULL
  expect_error(summary(BT_algo, normalize = normalize))
  normalize <-
    NA
  expect_error(summary(BT_algo, normalize = normalize))
  normalize <-
    "Text"
  expect_error(summary(BT_algo, normalize = normalize))

  # Check method
  expect_error(summary(BT_algo, method = NonExistingMethod))

})

testthat::test_that("Check the BT_Summary function - Results", {
  # Create datasets.
  set.seed(4)
  n <- 10000#100000

  Gender <- factor(sample(c("male", "female"), n, replace = TRUE))
  Age <- sample(c(18:65), n, replace = TRUE)
  Split <- factor(sample(c("yes", "no"), n, replace = TRUE))
  Sport <- factor(sample(c("yes", "no"), n, replace = TRUE))

  lambda <- 0.1 * ifelse(Gender == "male", 1.1, 1)
  lambda <- lambda * (1 + 1 / (Age - 17) ^ 0.5)
  lambda <- lambda * ifelse(Sport == "yes", 1.15, 1)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR * lambda)
  Y_normalized <- Y / ExpoR
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)

  # Run a BT algo.
  set.seed(4)
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = T,
      n.iter = 200,
      train.fraction = 0.8,
      interaction.depth = 4,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = F,
      cv.folds = 3,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  BT_algo <- do.call(BT, paramsBT)

  ####
  # Check results with validation n.iter.
  ####
  n.iter <- BT_perf(BT_algo, plot.it = F, method = "validation")
  ri <- .BT_relative_influence(BT_algo, n.iter = n.iter)
  ri[ri < 0] <- 0

  normalizedRI <- 100 * ri / sum(ri)
  ordering <-
    order(-ri)
  orderedRI <-
    ri[ordering]
  normalizedAndOrderedRI <- normalizedRI[ordering]
  orderVarNames <- BT_algo$var.names[ordering]

  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = F,
      normalize = F
    ),
    data.frame(var = BT_algo$var.names, rel_inf = ri)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = T,
      normalize = F
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = F,
      normalize = T
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(BT_algo, plot_it = F, n.iter = n.iter),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )

  # Visual check.
  expect_equal(
    summary(BT_algo, plot_it = T, n.iter = n.iter),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 0
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 44
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 3
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )

  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 0
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 44
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 3
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )

  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 0
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 44
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 3
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )

  ####
  # Check results with cv n.iter.
  ####
  n.iter <- BT_perf(BT_algo, plot.it = F, method = "cv")
  ri <- .BT_relative_influence(BT_algo, n.iter = n.iter)
  ri[ri < 0] <- 0

  normalizedRI <- 100 * ri / sum(ri)
  ordering <-
    order(-ri)
  orderedRI <-
    ri[ordering]
  normalizedAndOrderedRI <- normalizedRI[ordering]
  orderVarNames <- BT_algo$var.names[ordering]

  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = F,
      normalize = F
    ),
    data.frame(var = BT_algo$var.names, rel_inf = ri)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = T,
      normalize = F
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = F,
      normalize = T
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(BT_algo, plot_it = F, n.iter = n.iter),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )

  # Visual check.
  expect_equal(
    summary(BT_algo, plot_it = T, n.iter = n.iter),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 0
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 44
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 3
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )

  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 0
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 44
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 3
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )

  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 0
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 44
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 3
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )

  ####
  # Check results with validation n.iter.
  ####
  expect_message(n.iter <-
                   BT_perf(BT_algo, plot.it = F, method = "OOB"))
  ri <- .BT_relative_influence(BT_algo, n.iter = n.iter)
  ri[ri < 0] <- 0

  normalizedRI <- 100 * ri / sum(ri)
  ordering <-
    order(-ri)
  orderedRI <-
    ri[ordering]
  normalizedAndOrderedRI <- normalizedRI[ordering]
  orderVarNames <- BT_algo$var.names[ordering]

  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = F,
      normalize = F
    ),
    data.frame(var = BT_algo$var.names, rel_inf = ri)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = T,
      normalize = F
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      n.iter = n.iter,
      order_it = F,
      normalize = T
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(BT_algo, plot_it = F, n.iter = n.iter),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )

  # Visual check.
  expect_equal(
    summary(BT_algo, plot_it = T, n.iter = n.iter),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 0
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 44
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      cBars = 3
    ),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )

  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 0
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 44
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = F,
      normalize = T,
      cBars = 3
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )

  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 0
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 44
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      n.iter = n.iter,
      order_it = T,
      normalize = F,
      cBars = 3
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )

  ####
  # Check results with max n.iter.
  ####
  n.iter <- BT_algo$BTParams$n.iter
  ri <- .BT_relative_influence(BT_algo, n.iter = n.iter)
  ri[ri < 0] <- 0

  normalizedRI <- 100 * ri / sum(ri)
  ordering <-
    order(-ri)
  orderedRI <-
    ri[ordering]
  normalizedAndOrderedRI <- normalizedRI[ordering]
  orderVarNames <- BT_algo$var.names[ordering]

  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      order_it = F,
      normalize = F
    ),
    data.frame(var = BT_algo$var.names, rel_inf = ri)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      order_it = T,
      normalize = F
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = F,
      order_it = F,
      normalize = T
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(BT_algo, plot_it = F),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )

  # Visual check.
  expect_equal(
    summary(BT_algo, plot_it = T),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(BT_algo, plot_it = T, cBars = 0),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(BT_algo, plot_it = T, cBars = 44),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )
  expect_equal(
    summary(BT_algo, plot_it = T, cBars = 3),
    data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI)
  )

  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      order_it = F,
      normalize = T
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      order_it = F,
      normalize = T,
      cBars = 0
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      order_it = F,
      normalize = T,
      cBars = 44
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      order_it = F,
      normalize = T,
      cBars = 3
    ),
    data.frame(var = BT_algo$var.names, rel_inf = normalizedRI)
  )

  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      order_it = T,
      normalize = F
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      order_it = T,
      normalize = F,
      cBars = 0
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      order_it = T,
      normalize = F,
      cBars = 44
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )
  expect_equal(
    summary(
      BT_algo,
      plot_it = T,
      order_it = T,
      normalize = F,
      cBars = 3
    ),
    data.frame(var = orderVarNames, rel_inf = orderedRI)
  )

  # With n.iter > max n.iter performed => we expect the same results as previously obtained and warning message
  n.iter <- 450

  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = F,
        n.iter = n.iter,
        order_it = F,
        normalize = F
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res, data.frame(var = BT_algo$var.names, rel_inf = ri))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = F,
        n.iter = n.iter,
        order_it = T,
        normalize = F
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res, data.frame(var = orderVarNames, rel_inf = orderedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = F,
        n.iter = n.iter,
        order_it = F,
        normalize = T
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = BT_algo$var.names, rel_inf = normalizedRI))
  expect_warning(
    res <-
      summary(BT_algo, plot_it = F, n.iter = n.iter),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI))

  # Visual check.
  expect_warning(
    res <-
      summary(BT_algo, plot_it = T, n.iter = n.iter),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        cBars = 0
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        cBars = 44
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        cBars = 3
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = orderVarNames, rel_inf = normalizedAndOrderedRI))

  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        order_it = F,
        normalize = T
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = BT_algo$var.names, rel_inf = normalizedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        order_it = F,
        normalize = T,
        cBars = 0
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = BT_algo$var.names, rel_inf = normalizedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        order_it = F,
        normalize = T,
        cBars = 44
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = BT_algo$var.names, rel_inf = normalizedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        order_it = F,
        normalize = T,
        cBars = 3
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res,
               data.frame(var = BT_algo$var.names, rel_inf = normalizedRI))

  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        order_it = T,
        normalize = F
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res, data.frame(var = orderVarNames, rel_inf = orderedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        order_it = T,
        normalize = F,
        cBars = 0
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res, data.frame(var = orderVarNames, rel_inf = orderedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        order_it = T,
        normalize = F,
        cBars = 44
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res, data.frame(var = orderVarNames, rel_inf = orderedRI))
  expect_warning(
    res <-
      summary(
        BT_algo,
        plot_it = T,
        n.iter = n.iter,
        order_it = T,
        normalize = F,
        cBars = 3
      ),
    "Exceeded total number of BT terms. Results use n.iter=200 terms.\n"
  )
  expect_equal(res, data.frame(var = orderVarNames, rel_inf = orderedRI))

})
