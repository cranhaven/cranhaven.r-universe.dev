#########################
# Author : Gireg Willame
# June 2022.
#
# The goal is to check that the .BT_Relative_Influence
#   computation is correct.
#
########################

testthat::test_that("Check the BT_Relative_Influence function - Inputs", {
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

  # empty or wrong BTFit_object
  expect_error(.BT_relative_influence(list()))
  expect_error(.BT_relative_influence(BTFit_object = seq(1, 10)))
  expect_error(.BT_relative_influence())

  # Check the n.iter parameter.
  expect_message(tempRes <- .BT_relative_influence(BT_algo))
  expect_equal(tempRes,
               .BT_relative_influence(BT_algo, .BT_callPerformance(BT_algo, "validation")))

  n.iter <- 100
  # Check rescale parameter.
  rescale <-
    1
  expect_error(.BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <-
    0.4
  expect_error(.BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <-
    2.785
  expect_error(.BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <-
    c(3, 4)
  expect_error(.BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <-
    NULL
  expect_error(.BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <-
    NA
  expect_error(.BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <-
    "Text"
  expect_error(.BT_relative_influence(BT_algo, n.iter, rescale = rescale))

  # Check sort.it parameter.
  sort.it <-
    1
  expect_error(.BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <-
    0.4
  expect_error(.BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <-
    2.785
  expect_error(.BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <-
    c(3, 4)
  expect_error(.BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <-
    NULL
  expect_error(.BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <-
    NA
  expect_error(.BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <-
    "Text"
  expect_error(.BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))

  # Check if n.iter is not well defined (or bigger than number of algo iters).
  n.iter <-
    400
  expect_error(.BT_relative_influence(BT_algo, n.iter))
  n.iter <- 0
  expect_error(.BT_relative_influence(BT_algo, n.iter))
  n.iter <-
    c(3, 4)
  expect_error(.BT_relative_influence(BT_algo, n.iter))
  n.iter <-
    NULL
  expect_error(.BT_relative_influence(BT_algo, n.iter))
  n.iter <-
    NA
  expect_error(.BT_relative_influence(BT_algo, n.iter))
  n.iter <-
    "Text"
  expect_error(.BT_relative_influence(BT_algo, n.iter))
  n.iter <- F
  expect_error(.BT_relative_influence(BT_algo, n.iter))

  # Change inputs - test OOB.
  paramsBT$train.fraction <- 1
  BT_algo <- do.call(BT, paramsBT)
  # Check the n.iter parameter.
  expect_message(tempRes <- .BT_relative_influence(BT_algo))
  expect_message(tempResExpected_NbIter <-
                   .BT_callPerformance(BT_algo, "OOB"))
  expect_equal(tempRes,
               .BT_relative_influence(BT_algo, tempResExpected_NbIter))

  # Change inputs - test cv
  paramsBT$cv.folds <- 3
  BT_algo <- do.call(BT, paramsBT)
  # Check the n.iter parameter.
  expect_message(tempRes <- .BT_relative_influence(BT_algo))
  expect_equal(tempRes,
               .BT_relative_influence(BT_algo, .BT_callPerformance(BT_algo, "cv")))

})

testthat::test_that("Check the BT_Relative_Influence function - Results - Without surrogates",
                    {
                      skip_on_cran()

                      # Create datasets.
                      set.seed(4)
                      n <- 20000

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
                      varName <- c("Age", "Sport", "Split", "Gender")

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
                          cv.folds = 4,
                          folds.id = c(
                            rep(1, 0.8 * nrow(datasetFull) / 4),
                            rep(2, 0.8 * nrow(datasetFull) / 4),
                            rep(3, 0.8 * nrow(datasetFull) / 4),
                            rep(4, 0.8 * nrow(datasetFull) / 4)
                          ),
                          n.cores = 1,
                          weights = datasetFull$ExpoR
                        )

                      BT_algo <- do.call(BT, paramsBT)

                      # Function derived from rpart itself. We modified it a bit to not consider the surrogates in the output.
                      importance2 <- function(fit)
                      {
                        ff <- fit$frame
                        fpri <-
                          which(ff$var != "<leaf>")  # points to primary splits in ff
                        spri <-
                          1 + cumsum(c(0, 1 + ff$ncompete[fpri] + ff$nsurrogate[fpri]))
                        spri <-
                          spri[seq_along(fpri)] # points to primaries in the splits matrix
                        nsurr <- ff$nsurrogate[fpri]  # number of surrogates each has

                        sname <- vector("list", length(fpri))
                        sval <- sname

                        ## The importance for primary splits needs to be scaled
                        ## It was a printout choice for the anova method to list % improvement in
                        ##  the sum of squares, an importance calculation needs the total SS.
                        ## All the other methods report an unscaled change.
                        scaled.imp <-
                          if (fit$method == "anova")
                            fit$splits[spri, "improve"] * ff$dev[fpri]
                        else
                          fit$splits[spri, "improve"]

                        sdim <- rownames(fit$splits)
                        for (i in seq_along(fpri)) {
                          ## points to surrogates
                          if (nsurr[i] > 0L) {
                            indx <- spri[i] + ff$ncompete[fpri[i]] + seq_len(nsurr[i])
                            sname[[i]] <- sdim[indx]
                            sval[[i]] <- scaled.imp[i] * fit$splits[indx, "adj"]
                          }
                        }

                        import <- tapply(c(scaled.imp),
                                         c(as.character(ff$var[fpri])),
                                         sum)
                        sort(c(import), decreasing = TRUE) # a named vector
                      }

                      ####
                      # Test n.iter coming from validation.set
                      ####

                      n.iter <- .BT_callPerformance(BT_algo, method = "validation")
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currTree <- treesList[[iTree]]
                        currRes <- importance2(currTree)
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(relInf[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, rescale = F, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfNormalized[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, rescale = T, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfSorted,
                                   .BT_relative_influence(BT_algo, rescale = F, sort.it = T))
                      expect_equal(relInfSortedAndNormalized,
                                   .BT_relative_influence(BT_algo, rescale = T, sort.it = T))

                      ####
                      # Test n.iter coming from CV.
                      ####

                      paramsBT$train.fraction <- 1
                      paramsBT$folds.id <-
                        c(rep(1, nrow(datasetFull) / 4),
                          rep(2, nrow(datasetFull) / 4),
                          rep(3, nrow(datasetFull) / 4),
                          rep(4, nrow(datasetFull) / 4))
                      BT_algo <- do.call(BT, paramsBT)

                      n.iter <- BT_perf(BT_algo, method = "cv", plot.it = F)
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currTree <- treesList[[iTree]]
                        currRes <- importance2(currTree)
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(relInf[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, n.iter, rescale = F, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfNormalized[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, n.iter, rescale = T, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfSorted,
                                   .BT_relative_influence(BT_algo, rescale = F, n.iter, sort.it = T))
                      expect_equal(
                        relInfSortedAndNormalized,
                        .BT_relative_influence(BT_algo, n.iter, rescale = T, sort.it = T)
                      )

                      ####
                      # Test n.iter coming from OOB.
                      ####

                      paramsBT$cv.folds <- 1
                      paramsBT$folds.id <- NULL
                      BT_algo <- do.call(BT, paramsBT)

                      n.iter <- BT_perf(BT_algo, method = "OOB", plot.it = F)
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currTree <- treesList[[iTree]]
                        currRes <- importance2(currTree)
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(relInf[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, rescale = F, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfNormalized[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, rescale = T, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfSorted,
                                   .BT_relative_influence(BT_algo, rescale = F, sort.it = T))
                      expect_equal(relInfSortedAndNormalized,
                                   .BT_relative_influence(BT_algo, rescale = T, sort.it = T))

                      ####
                      # Test n.iter max.
                      ####

                      n.iter <- BT_algo$BTParams$n.iter
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currTree <- treesList[[iTree]]
                        currRes <- importance2(currTree)
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(relInf[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, n.iter, rescale = F, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfNormalized[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, n.iter, rescale = T, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfSorted,
                                   .BT_relative_influence(BT_algo, n.iter, rescale = F, sort.it = T))
                      expect_equal(
                        relInfSortedAndNormalized,
                        .BT_relative_influence(BT_algo, n.iter, rescale = T, sort.it = T)
                      )

                      ####
                      # Test n.iter random.
                      ####

                      n.iter <- 92
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currTree <- treesList[[iTree]]
                        currRes <- importance2(currTree)
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(relInf[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, n.iter, rescale = F, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfNormalized[BT_algo$var.names],
                                   .BT_relative_influence(BT_algo, n.iter, rescale = T, sort.it = F)[BT_algo$var.names])
                      expect_equal(relInfSorted,
                                   .BT_relative_influence(BT_algo, n.iter, rescale = F, sort.it = T))
                      expect_equal(
                        relInfSortedAndNormalized,
                        .BT_relative_influence(BT_algo, n.iter, rescale = T, sort.it = T)
                      )

                    })

# By default, rpart variable.importance returns the primary improvement as well as the surrogates one.
# We normally do not want surrogates in BT approach. However, user might be interested to do it (-> Tested hereafter).
# It also allows us to verify whether the get_rel_inf_of_vars is working as expected.
testthat::test_that("Check the BT_Relative_Influence function - Results - With surrogates",
                    {
                      skip_on_cran()

                      # Create datasets.
                      set.seed(4)
                      n <- 20000 #100000

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
                      varName <- c("Age", "Sport", "Split", "Gender")

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
                          cv.folds = 4,
                          folds.id = c(
                            rep(1, 0.8 * nrow(datasetFull) / 4),
                            rep(2, 0.8 * nrow(datasetFull) / 4),
                            rep(3, 0.8 * nrow(datasetFull) / 4),
                            rep(4, 0.8 * nrow(datasetFull) / 4)
                          ),
                          n.cores = 1,
                          weights = datasetFull$ExpoR
                        )

                      BT_algo <- do.call(BT, paramsBT)

                      ####
                      # Test n.iter coming from validation.set
                      ####

                      n.iter <- .BT_callPerformance(BT_algo, method = "validation")
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currRes <- treesList[[iTree]]$variable.importance
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(
                        relInf[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          rescale = F,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfNormalized[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          rescale = T,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfSorted,
                        .BT_relative_influence(
                          BT_algo,
                          rescale = F,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )
                      expect_equal(
                        relInfSortedAndNormalized,
                        .BT_relative_influence(
                          BT_algo,
                          rescale = T,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )

                      ####
                      # Test n.iter coming from CV.
                      ####

                      paramsBT$train.fraction <- 1
                      paramsBT$folds.id <-
                        c(rep(1, nrow(datasetFull) / 4),
                          rep(2, nrow(datasetFull) / 4),
                          rep(3, nrow(datasetFull) / 4),
                          rep(4, nrow(datasetFull) / 4))
                      BT_algo <- do.call(BT, paramsBT)

                      n.iter <- BT_perf(BT_algo, method = "cv", plot.it = F)
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currRes <- treesList[[iTree]]$variable.importance
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(
                        relInf[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = F,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfNormalized[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = T,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfSorted,
                        .BT_relative_influence(
                          BT_algo,
                          rescale = F,
                          n.iter,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )
                      expect_equal(
                        relInfSortedAndNormalized,
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = T,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )

                      ####
                      # Test n.iter coming from OOB.
                      ####

                      paramsBT$cv.folds <- 1
                      paramsBT$folds.id <- NULL
                      BT_algo <- do.call(BT, paramsBT)

                      n.iter <- BT_perf(BT_algo, method = "OOB", plot.it = F)
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currRes <- treesList[[iTree]]$variable.importance
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(
                        relInf[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          rescale = F,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfNormalized[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          rescale = T,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfSorted,
                        .BT_relative_influence(
                          BT_algo,
                          rescale = F,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )
                      expect_equal(
                        relInfSortedAndNormalized,
                        .BT_relative_influence(
                          BT_algo,
                          rescale = T,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )

                      ####
                      # Test n.iter max.
                      ####

                      n.iter <- BT_algo$BTParams$n.iter
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currRes <- treesList[[iTree]]$variable.importance
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(
                        relInf[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = F,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfNormalized[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = T,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfSorted,
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = F,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )
                      expect_equal(
                        relInfSortedAndNormalized,
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = T,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )

                      ####
                      # Test n.iter random.
                      ####

                      n.iter <- 92
                      # Extract trees of interest and compute relative influence.
                      treesList <- BT_algo$BTIndivFits[seq_len(n.iter)]
                      resMatrix <- matrix(0, ncol = length(varName), nrow = n.iter)
                      colnames(resMatrix) <- varName
                      for (iTree in seq_len(n.iter)) {
                        currRes <- treesList[[iTree]]$variable.importance
                        for (colName in varName) {
                          if (colName %in% names(currRes)) {
                            resMatrix[iTree, colName] <-
                              unname(currRes[names(currRes) == colName])
                          }
                        }
                      }
                      relInf <- colSums(resMatrix)
                      relInfNormalized <- relInf / max(relInf)
                      relInfSorted <- rev(sort(relInf))
                      relInfSortedAndNormalized <- relInfSorted / max(relInfSorted)

                      expect_equal(
                        relInf[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = F,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfNormalized[BT_algo$var.names],
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = T,
                          sort.it = F,
                          consider.surrogates = T
                        )[BT_algo$var.names]
                      )
                      expect_equal(
                        relInfSorted,
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = F,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )
                      expect_equal(
                        relInfSortedAndNormalized,
                        .BT_relative_influence(
                          BT_algo,
                          n.iter,
                          rescale = T,
                          sort.it = T,
                          consider.surrogates = T
                        )
                      )

                    })
