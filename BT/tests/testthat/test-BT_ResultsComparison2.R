#########################
# Author : Gireg Willame
# June 2022.
#
# The goal is to check that the BT algorithm provides the same outputs
# as the initial MVP code written by J. Trufin & M. Denuit.
# We'll tests it on two databases : One similar to the one used in the testing.
# The other one similar to the original code provided.
#
# We now focus on the classical BT rather than ABT.
#
########################

testthat::test_that("Same results as MVP code - BT without bagging", {
  skip_on_cran()
  library(rpart)

  # Create datasets.
  set.seed(4)
  # dataset
  n <- 100000 # size of training set (number of observations)

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

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
  training.set <-
    datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
  validation.set <-
    datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

  ###############
  # Usage of Julien T. & Michel D. code for comparison purpose.
  ###############

  modelForm <-
    as.formula("Y ~ Age+Sport+Split+Gender+offset(log(ExpoRUpdated))")

  M <- 100 # number of trees
  NbSplits <- 4 # number of splits in the trees
  shrinkage.param <- 0.01
  alpha <- 1 # bag fraction

  score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

  min.error.boost.oos <-
    matrix(0, nrow = length(NbSplits), ncol = length(alpha))
  nb.trees.opt.boost.oos <-
    matrix(0, nrow = length(NbSplits), ncol = length(alpha))

  ###############
  # Boosting with log-link function.
  ###############
  set.seed(4)
  for (j in 1:length(NbSplits)) {
    depth <- NbSplits[j] # depth of the trees (before being pruned)

    # if (NbSplits[j]==1){
    #   depth=2
    # }

    for (k in 1:length(alpha)) {
      training.set$ExpoRUpdated <-
        training.set$ExpoR * exp(score0) # initialisation
      validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

      gen.error.boost <- rep(0, M) # generalization error
      gen.error.boost.oos <-
        rep(0, M) # generalization error out of sample
      gen.error.boost.inbag <- rep(0, M)

      size.tree <-
        rep(F, M) # size (number of internal nodes) of the trees

      for (m in 1:M) {
        inSubset = seq(1, nrow(training.set))# createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
        trainingsubset = training.set[inSubset, ]

        tree <- rpart(
          formula = modelForm,
          data = trainingsubset,
          method = "poisson",
          control = rpart.control(
            cp = -Inf,
            maxdepth = depth,
            xval = 0,
            minsplit = 2,
            maxcompete = 0,
            maxsurrogate = 0
          )
        ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

        ff <-
          as.data.frame(cbind(
            "node" = as.numeric(rownames(tree$frame[tree$frame$var != "<leaf>",])),
            "improve" = unname(tree$splits[, "improve"])
          ))
        ff <- ff[order(ff$improve, decreasing = T), ]
        for (currSplit in seq(1, depth)) {
          if (currSplit == 1) {
            nodeToKeep <- c(1)
            children <- c(2, 3)
          } else{
            indx <- match(children, ff$node)
            if (all(is.na(indx))) {
              break
            } else{
              bestNode <- children[which.min(indx)]
              nodeToKeep <- c(nodeToKeep, bestNode)
              children <-
                c(setdiff(children, bestNode),
                  c(bestNode * 2, bestNode * 2 + 1))
            }
          }
        }
        if (!is.null(tree$splits) &
            length(setdiff(ff$node, nodeToKeep)) > 0) {
          tree.opt <- snip.rpart(tree, setdiff(ff$node, nodeToKeep))
        } else{
          tree.opt <- tree
        }

        size.tree[m] <-
          (nrow(tree.opt$frame[tree.opt$frame$var != "<leaf>", ]) == depth)

        boostScoreHat <-
          log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
        training.set$ExpoRUpdated <-
          training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

        gen.error.boost[m] <-
          1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                        + sum(log((training.set$Y /
                                                     training.set$ExpoRUpdated) ^ (training.set$Y)
                                        )))

        gen.error.boost.inbag[m] <-
          -1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                           + sum(log((trainingsubset$Y /
                                                        trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                           )))

        boostScoreHat <-
          log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
        validation.set$ExpoRUpdated <-
          validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

        gen.error.boost.oos[m] <-
          1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                          + sum(log((validation.set$Y /
                                                       validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                          )))
      }

      min.error.boost.oos[j, k] <- min(gen.error.boost.oos)
      nb.trees.opt.boost.oos[j, k] <- which.min(gen.error.boost.oos)
    }
  }

  ###############
  # Comparison w/ BT algorithm.
  ###############

  # Define BT parameters.
  set.seed(44552) # We should have similar results as everything is non-stochastic.
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = F,
      n.iter = M,
      train.fraction = trainFraction,
      interaction.depth = NbSplits,
      shrinkage = shrinkage.param,
      bag.fraction = alpha,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = T,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  BT_algo <- do.call(BT, paramsBT)

  expect_equal(gen.error.boost, BT_algo$BTErrors$training.error)
  expect_equal(gen.error.boost.oos, BT_algo$BTErrors$validation.error)
  expect_equal(nb.trees.opt.boost.oos[1, 1],
               BT::BT_perf(BT_algo, method = "validation", plot.it = F))

  expect_true(all(size.tree))

})


#
# Comparison with bagging.
#
testthat::test_that("Same results as MVP code - BT with bagging", {
  skip_on_cran()
  library(rpart)

  # Create datasets.
  set.seed(4)
  # dataset
  n <- 100000 # size of training set (number of observations)

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

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
  training.set <-
    datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
  validation.set <-
    datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

  ###############
  # Usage of Julien T. & Michel D. code for comparison purpose.
  ###############

  modelForm <-
    as.formula("Y ~ Age+Sport+Split+Gender+offset(log(ExpoRUpdated))")

  M <- 100 # number of trees
  NbSplits <- 4 # number of splits in the trees
  shrinkage.param <- 0.01
  alpha <- 0.5 # bag fraction

  score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

  min.error.boost.oos <-
    matrix(0, nrow = length(NbSplits), ncol = length(alpha))
  nb.trees.opt.boost.oos <-
    matrix(0, nrow = length(NbSplits), ncol = length(alpha))
  nb.trees.opt.boost.oob <-
    matrix(0, nrow = length(NbSplits), ncol = length(alpha))

  ###############
  # Boosting with log-link function.
  ###############
  set.seed(4)
  for (j in 1:length(NbSplits)) {
    depth <- NbSplits[j] # depth of the trees (before being pruned)

    # if (NbSplits[j]==1){
    #   depth=2
    # }

    for (k in 1:length(alpha)) {
      training.set$ExpoRUpdated <-
        training.set$ExpoR * exp(score0) # initialisation
      validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

      gen.error.boost <- rep(0, M) # generalization error
      gen.error.boost.oos <-
        rep(0, M) # generalization error out of sample
      gen.error.boost.inbag <- rep(0, M)
      oob.improvement <- rep(0, M)

      size.tree <-
        rep(F, M) # size (number of internal nodes) of the trees

      for (m in 1:M) {
        inSubset <-
          sample(1:nrow(training.set), alpha[k] * nrow(training.set)) # to be align : createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
        trainingsubset <- training.set[inSubset, ]
        oobsubset <-
          training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]

        # OOB computed thanks to previous model.
        oldOOBError <-
          1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                     + sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^
                                                 (oobsubset$Y)
                                     )))

        tree <- rpart(
          formula = modelForm,
          data = trainingsubset,
          method = "poisson",
          control = rpart.control(
            cp = 0,
            maxdepth = depth,
            xval = 0,
            minsplit = 2,
            maxcompete = 0,
            maxsurrogate = 0
          )
        ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

        ff <-
          as.data.frame(cbind(
            "node" = as.numeric(rownames(tree$frame[tree$frame$var != "<leaf>",])),
            "improve" = unname(tree$splits[, "improve"])
          ))
        ff <- ff[order(ff$improve, decreasing = T), ]
        for (currSplit in seq(1, depth)) {
          if (currSplit == 1) {
            nodeToKeep <- c(1)
            children <- c(2, 3)
          } else{
            indx <- match(children, ff$node)
            if (all(is.na(indx))) {
              break
            } else{
              bestNode <- children[which.min(indx)]
              nodeToKeep <- c(nodeToKeep, bestNode)
              children <-
                c(setdiff(children, bestNode),
                  c(bestNode * 2, bestNode * 2 + 1))
            }
          }
        }
        if (!is.null(tree$splits) &
            length(setdiff(ff$node, nodeToKeep)) > 0) {
          tree.opt <- snip.rpart(tree, setdiff(ff$node, nodeToKeep))
        } else{
          tree.opt <- tree
        }
        size.tree[m] <-
          (nrow(tree.opt$frame[tree.opt$frame$var != "<leaf>", ]) == depth)

        boostScoreHat <-
          log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
        training.set$ExpoRUpdated <-
          training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

        gen.error.boost[m] <-
          1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                        + sum(log((training.set$Y /
                                                     training.set$ExpoRUpdated) ^ (training.set$Y)
                                        )))


        trainingsubset <-
          training.set[inSubset, ] # Add by Gireg to have the in-bag error - note that training.set is updated before, we just slice again.
        gen.error.boost.inbag[m] <-
          1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                          + sum(log((trainingsubset$Y /
                                                       trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                          )))

        oobsubset <-
          training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]
        oob.improvement[m] <-
          (oldOOBError - 1 / nrow(oobsubset) * 2 * (
            sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
            + sum(log((oobsubset$Y /
                         oobsubset$ExpoRUpdated) ^ (oobsubset$Y)
            ))
          ))

        boostScoreHat <-
          log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
        validation.set$ExpoRUpdated <-
          validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

        gen.error.boost.oos[m] <-
          1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                          + sum(log((validation.set$Y /
                                                       validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                          )))
      }
      min.error.boost.oos[j, k] <- min(gen.error.boost.oos)
      nb.trees.opt.boost.oos[j, k] <- which.min(gen.error.boost.oos)
      nb.trees.opt.boost.oob[j, k] <-
        which.min(-cumsum(oob.improvement))
    }
  }

  ###############
  # Comparison w/ BT algorithm.
  ###############

  # Define BT parameters.
  set.seed(4)
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = F,
      n.iter = M,
      train.fraction = trainFraction,
      interaction.depth = NbSplits,
      shrinkage = shrinkage.param,
      bag.fraction = alpha,
      colsample.bytree = NULL,
      keep.data = T,
      is.verbose = T,
      cv.folds = 1,
      folds.id = NULL,
      n.cores = 1,
      weights = datasetFull$ExpoR
    )

  BT_algo <- do.call(BT, paramsBT)

  expect_equal(gen.error.boost.inbag, BT_algo$BTErrors$training.error)
  expect_equal(gen.error.boost.oos, BT_algo$BTErrors$validation.error)
  expect_equal(training.set$ExpoRUpdated / training.set$ExpoR,
               exp(BT_algo$fitted.values))
  expect_equal(oob.improvement, BT_algo$BTErrors$oob.improvement)
  expect_equal(nb.trees.opt.boost.oob[1, 1],
               BT::BT_perf(BT_algo, method = "OOB", plot.it = F)) # Can be different due to the smoother...
  expect_equal(nb.trees.opt.boost.oos[1, 1],
               BT::BT_perf(BT_algo, method = "validation", plot.it = F))

  expect_true(all(size.tree))
})


#
# Comparison with bagging and colSampleByTree
#

testthat::test_that("Same results as MVP code - BT with bagging and colsample.bytree argument",
                    {
                      skip_on_cran()
                      library(rpart)

                      # Create datasets.
                      set.seed(4)
                      # dataset
                      n <- 100000 # size of training set (number of observations)

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

                      trainFraction <- 0.8
                      datasetFull <-
                        data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
                      training.set <-
                        datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
                      validation.set <-
                        datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

                      ###############
                      # Usage of Julien T. & Michel D. code for comparison purpose.
                      ###############

                      M <- 100 # number of trees
                      NbSplits <- 3 # number of splits in the trees
                      shrinkage.param <- 0.1
                      alpha <- 0.8 # bag fraction
                      colsample.bytree <- 3

                      score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

                      min.error.boost.oos <-
                        matrix(0, nrow = length(NbSplits), ncol = length(alpha))
                      nb.trees.opt.boost.oos <-
                        matrix(0, nrow = length(NbSplits), ncol = length(alpha))
                      nb.trees.opt.boost.oob <-
                        matrix(0, nrow = length(NbSplits), ncol = length(alpha))

                      varUsed <- list()

                      ###############
                      # Boosting with log-link function.
                      ###############
                      for (j in 1:length(NbSplits)) {
                        depth <- NbSplits[j] # depth of the trees (before being pruned)

                        # if (NbSplits[j]==1){
                        #   depth=2
                        # }

                        for (k in 1:length(alpha)) {
                          training.set$ExpoRUpdated <-
                            training.set$ExpoR * exp(score0) # initialisation
                          validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

                          gen.error.boost <- rep(0, M) # generalization error
                          gen.error.boost.oos <-
                            rep(0, M) # generalization error out of sample
                          gen.error.boost.inbag <- rep(0, M)
                          oob.improvement <- rep(0, M)

                          size.tree <-
                            rep(F, M) # size (number of internal nodes) of the trees

                          set.seed(4)
                          varVec <-
                            c("Age", "Sport", "Split", "Gender") # Same order as it is given by the database...
                          for (m in 1:M) {
                            inSubset <-
                              sample(1:nrow(training.set), alpha[k] * nrow(training.set)) # to be align : createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
                            trainingsubset <- training.set[inSubset, ]
                            oobsubset <-
                              training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]

                            # OOB computed thanks to previous model.
                            oldOOBError <-
                              1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                                         + sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^
                                                                     (oobsubset$Y)
                                                         )))


                            # varSelect <- sample(seq(1,length(varVec)), colsample.bytree)
                            varVecSample <-
                              varVec[sample(1:length(varVec), colsample.bytree)]
                            varUsed[[m]] <- varVecSample
                            modelForm <-
                              as.formula(paste(
                                "Y ~ offset(log(ExpoRUpdated)) + ",
                                paste(varVecSample, collapse = " + ")
                              ))

                            tree <- rpart(
                              formula = modelForm,
                              data = trainingsubset,
                              method = "poisson",
                              control = rpart.control(
                                cp = 0,
                                maxdepth = depth,
                                xval = 0,
                                minsplit = 2,
                                maxcompete = 0,
                                maxsurrogate = 0
                              )
                            ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

                            ff <-
                              as.data.frame(cbind(
                                "node" = as.numeric(rownames(tree$frame[tree$frame$var != "<leaf>",])),
                                "improve" = unname(tree$splits[, "improve"])
                              ))
                            ff <- ff[order(ff$improve, decreasing = T), ]
                            for (currSplit in seq(1, depth)) {
                              if (currSplit == 1) {
                                nodeToKeep <- c(1)
                                children <- c(2, 3)
                              } else{
                                indx <- match(children, ff$node)
                                if (all(is.na(indx))) {
                                  break
                                } else{
                                  bestNode <- children[which.min(indx)]
                                  nodeToKeep <- c(nodeToKeep, bestNode)
                                  children <-
                                    c(setdiff(children, bestNode),
                                      c(bestNode * 2, bestNode * 2 + 1))
                                }
                              }
                            }
                            if (!is.null(tree$splits) &
                                length(setdiff(ff$node, nodeToKeep)) > 0) {
                              tree.opt <- snip.rpart(tree, setdiff(ff$node, nodeToKeep))
                            } else{
                              tree.opt <- tree
                            }
                            size.tree[m] <-
                              (nrow(tree.opt$frame[tree.opt$frame$var != "<leaf>", ]) == depth)

                            boostScoreHat <-
                              log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
                            training.set$ExpoRUpdated <-
                              training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                            gen.error.boost[m] <-
                              1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                                            + sum(log((training.set$Y /
                                                                         training.set$ExpoRUpdated) ^ (training.set$Y)
                                                            )))


                            trainingsubset <-
                              training.set[inSubset, ] # Add by Gireg to have the in-bag error - note that training.set is updated before, we just slice again.
                            gen.error.boost.inbag[m] <-
                              1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                                              + sum(log((trainingsubset$Y /
                                                                           trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                                              )))

                            oobsubset <-
                              training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]
                            oob.improvement[m] <-
                              (oldOOBError - 1 / nrow(oobsubset) * 2 * (
                                sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                + sum(log((oobsubset$Y /
                                             oobsubset$ExpoRUpdated) ^ (oobsubset$Y)
                                ))
                              ))

                            boostScoreHat <-
                              log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
                            validation.set$ExpoRUpdated <-
                              validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                            gen.error.boost.oos[m] <-
                              1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                                              + sum(log((validation.set$Y /
                                                                           validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                                              )))
                          }
                          min.error.boost.oos[j, k] <- min(gen.error.boost.oos)
                          nb.trees.opt.boost.oos[j, k] <- which.min(gen.error.boost.oos)
                          nb.trees.opt.boost.oob[j, k] <-
                            which.min(-cumsum(oob.improvement))
                        }
                      }
                      print(varVec)

                      ###############
                      # Comparison w/ BT algorithm.
                      ###############

                      # Define BT parameters.
                      set.seed(4)
                      paramsBT <-
                        list(
                          formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
                          data = datasetFull,
                          tweedie.power = 1,
                          ABT = F,
                          n.iter = M,
                          train.fraction = trainFraction,
                          interaction.depth = NbSplits,
                          shrinkage = shrinkage.param,
                          bag.fraction = alpha,
                          colsample.bytree = colsample.bytree,
                          keep.data = T,
                          is.verbose = T,
                          cv.folds = 1,
                          folds.id = NULL,
                          n.cores = 1,
                          weights = datasetFull$ExpoR
                        )

                      BT_algo <- do.call(BT, paramsBT)

                      expect_equal(gen.error.boost.inbag, BT_algo$BTErrors$training.error)
                      expect_equal(gen.error.boost.oos, BT_algo$BTErrors$validation.error)
                      expect_equal(training.set$ExpoRUpdated / training.set$ExpoR,
                                   exp(BT_algo$fitted.values))
                      expect_equal(oob.improvement, BT_algo$BTErrors$oob.improvement)
                      expect_equal(nb.trees.opt.boost.oob[1, 1],
                                   seq_len(BT_algo$BTParams$n.iter)[which.min(-cumsum(BT_algo$BTErrors$oob.improvement))]) # Can be different due to the smoother if we used BT_perf.
                      expect_equal(nb.trees.opt.boost.oos[1, 1],
                                   BT::BT_perf(BT_algo, method = "validation", plot.it = F))

                      expect_true(all(size.tree))
                    })


#
# Comparison with CV.
#
testthat::test_that("Same results as MVP code - BT with CV - no bagging and colsample.bytree argument",
                    {
                      skip_on_cran()

                      library(rpart)

                      # Create datasets.
                      set.seed(4)
                      # dataset
                      n <- 100000 # size of training set (number of observations)

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

                      trainFraction <- 0.8
                      datasetFull <-
                        data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
                      training.set <-
                        datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
                      validation.set <-
                        datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

                      ###############
                      # Usage of Julien T. & Michel D. code for comparison purpose.
                      ###############

                      modelForm <-
                        as.formula("Y ~ Age+Sport+Split+Gender+offset(log(ExpoRUpdated))")

                      M <- 200 # number of trees
                      NbSplits <- 4 # number of splits in the trees
                      shrinkage.param <- 0.01
                      alpha <- 1 # bag fraction
                      colsample.bytree <- 3
                      varVec <- c("Age", "Sport", "Split", "Gender")

                      depth <- NbSplits # depth of the trees (before being pruned)

                      score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

                      ###############
                      # Boosting with log-link function.
                      ###############

                      training.set$ExpoRUpdated <-
                        training.set$ExpoR * exp(score0) # initialisation
                      validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

                      gen.error.boost <- rep(0, M) # generalization error
                      gen.error.boost.oos <- rep(0, M) # generalization error out of sample
                      gen.error.boost.inbag <- rep(0, M)
                      oob.improvement <- rep(0, M)

                      size.tree <- rep(F, M) # size (number of internal nodes) of the trees

                      varUsed <- list()

                      set.seed(44)
                      for (m in 1:M) {
                        if (alpha != 1) {
                          inSubset <-
                            sample(1:nrow(training.set), alpha * nrow(training.set)) # to be align : createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
                        } else{
                          inSubset <- 1:nrow(training.set)
                        }
                        trainingsubset <- training.set[inSubset, ]
                        oobsubset <-
                          training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]

                        # OOB computed thanks to previous model.
                        oldOOBError <-
                          1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                                     + sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^
                                                                 (oobsubset$Y)
                                                     )))

                        if (length(varVec) > colsample.bytree) {
                          varVecSample <- varVec[sample(1:length(varVec), colsample.bytree)]
                          varUsed[[m]] <- varVecSample
                          modelForm <-
                            as.formula(paste(
                              "Y ~ offset(log(ExpoRUpdated)) + ",
                              paste(varVecSample, collapse = " + ")
                            ))
                        }

                        tree <- rpart(
                          formula = modelForm,
                          data = trainingsubset,
                          method = "poisson",
                          control = rpart.control(
                            cp = 0,
                            maxdepth = depth,
                            xval = 0,
                            minsplit = 2,
                            maxcompete = 0,
                            maxsurrogate = 0
                          )
                        ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

                        ff <-
                          as.data.frame(cbind(
                            "node" = as.numeric(rownames(tree$frame[tree$frame$var != "<leaf>",])),
                            "improve" = unname(tree$splits[, "improve"])
                          ))
                        ff <- ff[order(ff$improve, decreasing = T), ]
                        for (currSplit in seq(1, depth)) {
                          if (currSplit == 1) {
                            nodeToKeep <- c(1)
                            children <- c(2, 3)
                          } else{
                            indx <- match(children, ff$node)
                            if (all(is.na(indx))) {
                              break
                            } else{
                              bestNode <- children[which.min(indx)]
                              nodeToKeep <- c(nodeToKeep, bestNode)
                              children <-
                                c(setdiff(children, bestNode),
                                  c(bestNode * 2, bestNode * 2 + 1))
                            }
                          }
                        }
                        if (!is.null(tree$splits) &
                            length(setdiff(ff$node, nodeToKeep)) > 0) {
                          tree.opt <- snip.rpart(tree, setdiff(ff$node, nodeToKeep))
                        } else{
                          tree.opt <- tree
                        }
                        size.tree[m] <-
                          (nrow(tree.opt$frame[tree.opt$frame$var != "<leaf>", ]) == depth)

                        boostScoreHat <-
                          log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
                        training.set$ExpoRUpdated <-
                          training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                        gen.error.boost[m] <-
                          1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                                        + sum(log((training.set$Y /
                                                                     training.set$ExpoRUpdated) ^ (training.set$Y)
                                                        )))


                        trainingsubset <-
                          training.set[inSubset, ] # Add by Gireg to have the in-bag error - note that training.set is updated before, we just slice again.
                        gen.error.boost.inbag[m] <-
                          1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                                          + sum(log((trainingsubset$Y /
                                                                       trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                                          )))

                        oobsubset <-
                          training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]
                        oob.improvement[m] <-
                          (oldOOBError - 1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) -
                                                                      sum(oobsubset$Y)
                                                                    + sum(log((oobsubset$Y /
                                                                                 oobsubset$ExpoRUpdated) ^ (oobsubset$Y)
                                                                    ))))

                        boostScoreHat <-
                          log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
                        validation.set$ExpoRUpdated <-
                          validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                        gen.error.boost.oos[m] <-
                          1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                                          + sum(log((validation.set$Y /
                                                                       validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                                          )))
                      }

                      min.error.boost.oos.fullRun <- min(gen.error.boost.oos)
                      nb.trees.opt.boost.oos.fullRun <- which.min(gen.error.boost.oos)
                      nb.trees.opt.boost.oob.fullRun <-
                        which.min(-cumsum(oob.improvement))

                      trainPred <- training.set$ExpoRUpdated / training.set$ExpoR

                      ##########################
                      # Cross-validation.
                      ##########################

                      nFolds <- 4
                      folds <-
                        c(
                          rep(1, n * trainFraction / nFolds),
                          rep(2, n * trainFraction / nFolds),
                          rep(3, n * trainFraction / nFolds),
                          rep(4, n * trainFraction / nFolds)
                        )

                      gen.error.boost.cv <-
                        matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-training set.
                      gen.error.boost.oos.cv <-
                        matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-out of sample.
                      gen.error.boost.inbag.cv <-
                        matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-inbag sample (without OOB similar to gen.error.boost.cv)
                      oob.improvement.cv <- matrix(0, ncol = nFolds, nrow = M)
                      # gen.error.boost.cv.val <- matrix(0, ncol=nFolds, nrow=M) # generalization error on the validation set that has been kept aside.

                      size.tree.matrix <-
                        matrix(F, ncol = nFolds, nrow = M) # size (number of internal nodes) of the trees

                      # Save full sets before performing cv.
                      fullTrainingSet <-
                        datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
                      fullValidationSet <-
                        datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

                      # score0<-log(sum(fullTrainingSet$Y)/sum(fullTrainingSet$ExpoR))
                      #
                      # fullTrainingSet$ExpoRUpdated<-fullTrainingSet$ExpoR*exp(score0)
                      # fullValidationSet$ExpoRUpdated<-fullValidationSet$ExpoR*exp(score0)

                      cv.pred <- list()

                      # Start CV.
                      # set.seed(4)
                      for (iFolds in seq(1, nFolds)) {
                        set.seed((iFolds + 1) * 44)
                        inCV <- which(folds == iFolds)
                        outCV <- which(folds != iFolds)
                        training.set <- fullTrainingSet[outCV, ]
                        validation.set <- fullTrainingSet[inCV, ]

                        # Initialization.
                        score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

                        training.set$ExpoRUpdated <- training.set$ExpoR * exp(score0)
                        validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

                        cv.pred[[iFolds]] <- list()

                        for (m in 1:M) {
                          if (alpha != 1) {
                            inSubset <-
                              sample(1:nrow(training.set), alpha * nrow(training.set)) # to be align : createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
                          } else{
                            inSubset <- 1:nrow(training.set)
                          }
                          trainingsubset <- training.set[inSubset, ]
                          oobsubset <-
                            training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]

                          # OOB computed thanks to previous model.
                          oldOOBError <-
                            1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                                       + sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^
                                                                   (oobsubset$Y)
                                                       )))

                          if (length(varVec) > colsample.bytree) {
                            varVecSample <- varVec[sample(1:length(varVec), colsample.bytree)]
                            modelForm <-
                              as.formula(paste(
                                "Y ~ offset(log(ExpoRUpdated)) + ",
                                paste(varVecSample, collapse = " + ")
                              ))
                          }

                          tree <- rpart(
                            formula = modelForm,
                            data = trainingsubset,
                            method = "poisson",
                            control = rpart.control(
                              cp = 0,
                              maxdepth = depth,
                              xval = 0,
                              minsplit = 2,
                              maxcompete = 0,
                              maxsurrogate = 0
                            )
                          ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

                          ff <-
                            as.data.frame(cbind(
                              "node" = as.numeric(rownames(tree$frame[tree$frame$var != "<leaf>",])),
                              "improve" = unname(tree$splits[, "improve"])
                            ))
                          ff <- ff[order(ff$improve, decreasing = T), ]
                          for (currSplit in seq(1, depth)) {
                            if (currSplit == 1) {
                              nodeToKeep <- c(1)
                              children <- c(2, 3)
                            } else{
                              indx <- match(children, ff$node)
                              if (all(is.na(indx))) {
                                break
                              } else{
                                bestNode <- children[which.min(indx)]
                                nodeToKeep <- c(nodeToKeep, bestNode)
                                children <-
                                  c(setdiff(children, bestNode),
                                    c(bestNode * 2, bestNode * 2 + 1))
                              }
                            }
                          }
                          if (!is.null(tree$splits) &
                              length(setdiff(ff$node, nodeToKeep)) > 0) {
                            tree.opt <- snip.rpart(tree, setdiff(ff$node, nodeToKeep))
                          } else{
                            tree.opt <- tree
                          }
                          size.tree.matrix[m, iFolds] <-
                            (nrow(tree.opt$frame[tree.opt$frame$var != "<leaf>", ]) == depth)

                          boostScoreHat <-
                            log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
                          training.set$ExpoRUpdated <-
                            training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                          gen.error.boost.cv[m, iFolds] <-
                            1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                                          + sum(log((training.set$Y /
                                                                       training.set$ExpoRUpdated) ^ (training.set$Y)
                                                          )))


                          trainingsubset <-
                            training.set[inSubset, ] # Add by Gireg to have the in-bag error - note that training.set is updated before, we just slice again.
                          gen.error.boost.inbag.cv[m, iFolds] <-
                            1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                                            +
                                                              sum(log((trainingsubset$Y / trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                                              )))

                          oobsubset <-
                            training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]
                          oob.improvement.cv[m, iFolds] <-
                            (oldOOBError - 1 / nrow(oobsubset) * 2 * (
                              sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                              +
                                sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^ (oobsubset$Y)
                                ))
                            ))

                          boostScoreHat <-
                            log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
                          validation.set$ExpoRUpdated <-
                            validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                          gen.error.boost.oos.cv[m, iFolds] <-
                            1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                                            + sum(log((validation.set$Y /
                                                                         validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                                            )))
                          cv.pred[[iFolds]][[m]] <-
                            log(validation.set$ExpoRUpdated / validation.set$ExpoR)

                          # boostScoreHat<-log(predict(tree.opt, fullValidationSet, type = "vector")) # predict: does not account for exposure-to-risk
                          # fullValidationSet$ExpoRUpdated<-fullValidationSet$ExpoRUpdated*exp(shrinkage.param*boostScoreHat)
                          # gen.error.boost.cv.val[m, iFolds]<-1/nrow(fullValidationSet)*2*(sum(fullValidationSet$ExpoRUpdated)-sum(fullValidationSet$Y)
                          #                                                                 +sum(log((fullValidationSet$Y/fullValidationSet$ExpoRUpdated)^(fullValidationSet$Y))))
                        }
                      }
                      cv.errors <-
                        rowSums(gen.error.boost.oos.cv * n * trainFraction / nFolds) / (n * trainFraction)
                      min.cv.errors <- min(cv.errors)
                      nb.trees.opt.boost.cv <- which.min(cv.errors)


                      ###############
                      # Comparison w/ BT algorithm.
                      ###############

                      # Define BT parameters.
                      paramsBT <-
                        list(
                          formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
                          data = datasetFull,
                          tweedie.power = 1,
                          ABT = F,
                          n.iter = M,
                          train.fraction = trainFraction,
                          interaction.depth = NbSplits,
                          shrinkage = shrinkage.param,
                          bag.fraction = alpha,
                          colsample.bytree = colsample.bytree,
                          keep.data = T,
                          is.verbose = T,
                          cv.folds = 4,
                          folds.id = folds,
                          n.cores = 1,
                          weights = ExpoR,
                          seed = 44
                        )

                      BT_algo <- do.call(BT, paramsBT)

                      expect_equal(gen.error.boost.inbag, BT_algo$BTErrors$training.error)
                      expect_equal(gen.error.boost.oos, BT_algo$BTErrors$validation.error)
                      expect_equal(cv.errors, BT_algo$BTErrors$cv.error)

                      expect_equal(trainPred, exp(BT_algo$fitted.values))
                      # expect_equal(oob.improvement, BT_algo$BTErrors$oob.improvement)

                      # expect_equal(nb.trees.opt.boost.oob.fullRun, seq_len(BT_algo$BTParams$n.iter)[which.min(-cumsum(BT_algo$BTErrors$oob.improvement))]) # Can be different due to the smoother if we used BT_perf.
                      expect_equal(
                        nb.trees.opt.boost.oos.fullRun,
                        BT::BT_perf(BT_algo, method = "validation", plot.it = F)
                      )
                      expect_equal(nb.trees.opt.boost.cv,
                                   BT::BT_perf(BT_algo, method = "cv", plot.it = F))

                      expect_equal(folds, BT_algo$folds)
                      expect_equal(length(unique(folds)), BT_algo$cv.folds)

                      expect_equal(c(cv.pred[[1]][[nb.trees.opt.boost.cv]], cv.pred[[2]][[nb.trees.opt.boost.cv]], cv.pred[[3]][[nb.trees.opt.boost.cv]],
                                     cv.pred[[4]][[nb.trees.opt.boost.cv]]),
                                   BT_algo$cv.fitted)

                      expect_true(all(size.tree))
                      expect_true(all(size.tree.matrix))

                    })


testthat::test_that("Same results as MVP code - BT with CV - WITH bagging and colsample.bytree argument",
                    {
                      skip_on_cran()
                      library(rpart)

                      # Create datasets.
                      set.seed(4)
                      # dataset
                      n <- 100000 # size of training set (number of observations)

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

                      trainFraction <- 0.8
                      datasetFull <-
                        data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
                      training.set <-
                        datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
                      validation.set <-
                        datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

                      ###############
                      # Usage of Julien T. & Michel D. code for comparison purpose.
                      ###############

                      modelForm <-
                        as.formula("Y ~ Age+Sport+Split+Gender+offset(log(ExpoRUpdated))")

                      M <- 400 # number of trees
                      NbSplits <- 4 # number of splits in the trees
                      shrinkage.param <- 0.01
                      alpha <- 0.5# bag fraction
                      colsample.bytree <- 3
                      varVec <- c("Age", "Sport", "Split", "Gender")

                      depth <- NbSplits # depth of the trees (before being pruned)

                      score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

                      ###############
                      # Boosting with log-link function.
                      ###############

                      training.set$ExpoRUpdated <-
                        training.set$ExpoR * exp(score0) # initialisation
                      validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

                      gen.error.boost <- rep(0, M) # generalization error
                      gen.error.boost.oos <- rep(0, M) # generalization error out of sample
                      gen.error.boost.inbag <- rep(0, M)
                      oob.improvement <- rep(0, M)

                      size.tree <- rep(F, M) # size (number of internal nodes) of the trees

                      varUsed <- list()

                      set.seed(400)
                      for (m in 1:M) {
                        if (alpha != 1) {
                          inSubset <-
                            sample(1:nrow(training.set), alpha * nrow(training.set)) # to be align : createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
                        } else{
                          inSubset <- 1:nrow(training.set)
                        }
                        trainingsubset <- training.set[inSubset, ]
                        oobsubset <-
                          training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]

                        # OOB computed thanks to previous model.
                        oldOOBError <-
                          1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                                     + sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^
                                                                 (oobsubset$Y)
                                                     )))

                        if (length(varVec) > colsample.bytree) {
                          varVecSample <- varVec[sample(1:length(varVec), colsample.bytree)]
                          varUsed[[m]] <- varVecSample
                          modelForm <-
                            as.formula(paste(
                              "Y ~ offset(log(ExpoRUpdated)) + ",
                              paste(varVecSample, collapse = " + ")
                            ))
                        }

                        tree <- rpart(
                          formula = modelForm,
                          data = trainingsubset,
                          method = "poisson",
                          control = rpart.control(
                            cp = 0,
                            maxdepth = depth,
                            xval = 0,
                            minsplit = 2,
                            maxcompete = 0,
                            maxsurrogate = 0
                          )
                        ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

                        ff <-
                          as.data.frame(cbind(
                            "node" = as.numeric(rownames(tree$frame[tree$frame$var != "<leaf>",])),
                            "improve" = unname(tree$splits[, "improve"])
                          ))
                        ff <- ff[order(ff$improve, decreasing = T), ]
                        for (currSplit in seq(1, depth)) {
                          if (currSplit == 1) {
                            nodeToKeep <- c(1)
                            children <- c(2, 3)
                          } else{
                            indx <- match(children, ff$node)
                            if (all(is.na(indx))) {
                              break
                            } else{
                              bestNode <- children[which.min(indx)]
                              nodeToKeep <- c(nodeToKeep, bestNode)
                              children <-
                                c(setdiff(children, bestNode),
                                  c(bestNode * 2, bestNode * 2 + 1))
                            }
                          }
                        }
                        if (!is.null(tree$splits) &
                            length(setdiff(ff$node, nodeToKeep)) > 0) {
                          tree.opt <- snip.rpart(tree, setdiff(ff$node, nodeToKeep))
                        } else{
                          tree.opt <- tree
                        }
                        size.tree[m] <-
                          (nrow(tree.opt$frame[tree.opt$frame$var != "<leaf>", ]) == depth)

                        boostScoreHat <-
                          log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
                        training.set$ExpoRUpdated <-
                          training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                        gen.error.boost[m] <-
                          1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                                        + sum(log((training.set$Y /
                                                                     training.set$ExpoRUpdated) ^ (training.set$Y)
                                                        )))


                        trainingsubset <-
                          training.set[inSubset, ] # Add by Gireg to have the in-bag error - note that training.set is updated before, we just slice again.
                        gen.error.boost.inbag[m] <-
                          1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                                          + sum(log((trainingsubset$Y /
                                                                       trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                                          )))

                        oobsubset <-
                          training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]
                        oob.improvement[m] <-
                          (oldOOBError - 1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) -
                                                                      sum(oobsubset$Y)
                                                                    + sum(log((oobsubset$Y /
                                                                                 oobsubset$ExpoRUpdated) ^ (oobsubset$Y)
                                                                    ))))

                        boostScoreHat <-
                          log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
                        validation.set$ExpoRUpdated <-
                          validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                        gen.error.boost.oos[m] <-
                          1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                                          + sum(log((validation.set$Y /
                                                                       validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                                          )))
                      }

                      min.error.boost.oos.fullRun <- min(gen.error.boost.oos)
                      nb.trees.opt.boost.oos.fullRun <- which.min(gen.error.boost.oos)
                      nb.trees.opt.boost.oob.fullRun <-
                        which.min(-cumsum(oob.improvement))

                      trainPred <- training.set$ExpoRUpdated / training.set$ExpoR

                      ##########################
                      # Cross-validation.
                      ##########################

                      nFolds <- 4
                      folds <-
                        c(
                          rep(1, n * trainFraction / nFolds),
                          rep(2, n * trainFraction / nFolds),
                          rep(3, n * trainFraction / nFolds),
                          rep(4, n * trainFraction / nFolds)
                        )

                      gen.error.boost.cv <-
                        matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-training set.
                      gen.error.boost.oos.cv <-
                        matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-out of sample.
                      gen.error.boost.inbag.cv <-
                        matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-inbag sample (without OOB similar to gen.error.boost.cv)
                      oob.improvement.cv <- matrix(0, ncol = nFolds, nrow = M)
                      # gen.error.boost.cv.val <- matrix(0, ncol=nFolds, nrow=M) # generalization error on the validation set that has been kept aside.

                      size.tree.matrix <-
                        matrix(F, ncol = nFolds, nrow = M) # size (number of internal nodes) of the trees

                      # Save full sets before performing cv.
                      fullTrainingSet <-
                        datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
                      fullValidationSet <-
                        datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

                      cv.pred <- list()

                      # Start CV.
                      # set.seed(4)
                      for (iFolds in seq(1, nFolds)) {
                        set.seed((iFolds + 1) * 400)

                        inCV <- which(folds == iFolds)
                        outCV <- which(folds != iFolds)
                        training.set <- fullTrainingSet[outCV, ]
                        validation.set <- fullTrainingSet[inCV, ]

                        # Initialization.
                        score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

                        training.set$ExpoRUpdated <- training.set$ExpoR * exp(score0)
                        validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

                        cv.pred[[iFolds]] <- list()

                        for (m in 1:M) {
                          if (alpha != 1) {
                            inSubset <-
                              sample(1:nrow(training.set), alpha * nrow(training.set)) # to be align : createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
                          } else{
                            inSubset <- 1:nrow(training.set)
                          }
                          trainingsubset <- training.set[inSubset, ]
                          oobsubset <-
                            training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]

                          # OOB computed thanks to previous model.
                          oldOOBError <-
                            1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                                       + sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^
                                                                   (oobsubset$Y)
                                                       )))

                          if (length(varVec) > colsample.bytree) {
                            varVecSample <- varVec[sample(1:length(varVec), colsample.bytree)]
                            modelForm <-
                              as.formula(paste(
                                "Y ~ offset(log(ExpoRUpdated)) + ",
                                paste(varVecSample, collapse = " + ")
                              ))
                          }

                          tree <- rpart(
                            formula = modelForm,
                            data = trainingsubset,
                            method = "poisson",
                            control = rpart.control(
                              cp = 0,
                              maxdepth = depth,
                              xval = 0,
                              minsplit = 2,
                              maxcompete = 0,
                              maxsurrogate = 0
                            )
                          ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

                          ff <-
                            as.data.frame(cbind(
                              "node" = as.numeric(rownames(tree$frame[tree$frame$var != "<leaf>",])),
                              "improve" = unname(tree$splits[, "improve"])
                            ))
                          ff <- ff[order(ff$improve, decreasing = T), ]
                          for (currSplit in seq(1, depth)) {
                            if (currSplit == 1) {
                              nodeToKeep <- c(1)
                              children <- c(2, 3)
                            } else{
                              indx <- match(children, ff$node)
                              if (all(is.na(indx))) {
                                break
                              } else{
                                bestNode <- children[which.min(indx)]
                                nodeToKeep <- c(nodeToKeep, bestNode)
                                children <-
                                  c(setdiff(children, bestNode),
                                    c(bestNode * 2, bestNode * 2 + 1))
                              }
                            }
                          }
                          if (!is.null(tree$splits) &
                              length(setdiff(ff$node, nodeToKeep)) > 0) {
                            tree.opt <- snip.rpart(tree, setdiff(ff$node, nodeToKeep))
                          } else{
                            tree.opt <- tree
                          }
                          size.tree.matrix[m, iFolds] <-
                            (nrow(tree.opt$frame[tree.opt$frame$var != "<leaf>", ]) == depth)

                          boostScoreHat <-
                            log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
                          training.set$ExpoRUpdated <-
                            training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                          gen.error.boost.cv[m, iFolds] <-
                            1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                                          + sum(log((training.set$Y /
                                                                       training.set$ExpoRUpdated) ^ (training.set$Y)
                                                          )))


                          trainingsubset <-
                            training.set[inSubset, ] # Add by Gireg to have the in-bag error - note that training.set is updated before, we just slice again.
                          gen.error.boost.inbag.cv[m, iFolds] <-
                            1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                                            +
                                                              sum(log((trainingsubset$Y / trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                                              )))

                          oobsubset <-
                            training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]
                          oob.improvement.cv[m, iFolds] <-
                            (oldOOBError - 1 / nrow(oobsubset) * 2 * (
                              sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                              +
                                sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^ (oobsubset$Y)
                                ))
                            ))

                          boostScoreHat <-
                            log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
                          validation.set$ExpoRUpdated <-
                            validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

                          gen.error.boost.oos.cv[m, iFolds] <-
                            1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                                            + sum(log((validation.set$Y /
                                                                         validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                                            )))
                          cv.pred[[iFolds]][[m]] <-
                            log(validation.set$ExpoRUpdated / validation.set$ExpoR)

                          # boostScoreHat<-log(predict(tree.opt, fullValidationSet, type = "vector")) # predict: does not account for exposure-to-risk
                          # fullValidationSet$ExpoRUpdated<-fullValidationSet$ExpoRUpdated*exp(shrinkage.param*boostScoreHat)
                          # gen.error.boost.cv.val[m, iFolds]<-1/nrow(fullValidationSet)*2*(sum(fullValidationSet$ExpoRUpdated)-sum(fullValidationSet$Y)
                          #                                                                 +sum(log((fullValidationSet$Y/fullValidationSet$ExpoRUpdated)^(fullValidationSet$Y))))
                        }
                      }
                      cv.errors <-
                        rowSums(gen.error.boost.oos.cv * n * trainFraction / nFolds) / (n * trainFraction)
                      min.cv.errors <- min(cv.errors)
                      nb.trees.opt.boost.cv <- which.min(cv.errors)


                      ###############
                      # Comparison w/ BT algorithm.
                      ###############

                      # Define BT parameters.
                      paramsBT <-
                        list(
                          formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
                          data = datasetFull,
                          tweedie.power = 1,
                          ABT = F,
                          n.iter = M,
                          train.fraction = trainFraction,
                          interaction.depth = NbSplits,
                          shrinkage = shrinkage.param,
                          bag.fraction = alpha,
                          colsample.bytree = colsample.bytree,
                          keep.data = T,
                          is.verbose = T,
                          cv.folds = 4,
                          folds.id = folds,
                          n.cores = 1,
                          weights = ExpoR,
                          seed = 400
                        )

                      BT_algo <- do.call(BT, paramsBT)

                      expect_equal(gen.error.boost.inbag, BT_algo$BTErrors$training.error)
                      expect_equal(gen.error.boost.oos, BT_algo$BTErrors$validation.error)
                      expect_equal(cv.errors, BT_algo$BTErrors$cv.error, tolerance = 1e-7)

                      expect_equal(trainPred, exp(BT_algo$fitted.values))
                      expect_equal(oob.improvement, BT_algo$BTErrors$oob.improvement)

                      expect_equal(nb.trees.opt.boost.oob.fullRun,
                                   seq_len(BT_algo$BTParams$n.iter)[which.min(-cumsum(BT_algo$BTErrors$oob.improvement))]) # Can be different due to the smoother if we used BT_perf.
                      expect_equal(
                        nb.trees.opt.boost.oos.fullRun,
                        BT::BT_perf(BT_algo, method = "validation", plot.it = F)
                      )
                      expect_equal(nb.trees.opt.boost.cv,
                                   BT::BT_perf(BT_algo, method = "cv", plot.it = F))

                      expect_equal(folds, BT_algo$folds)
                      expect_equal(length(unique(folds)), BT_algo$cv.folds)

                      expect_equal(c(cv.pred[[1]][[nb.trees.opt.boost.cv]], cv.pred[[2]][[nb.trees.opt.boost.cv]], cv.pred[[3]][[nb.trees.opt.boost.cv]],
                                     cv.pred[[4]][[nb.trees.opt.boost.cv]]),
                                   BT_algo$cv.fitted)

                      expect_equal(unique(unlist(lapply(varUsed, length))), colsample.bytree)
                      expect_equal(colsample.bytree, BT_algo$BTParams$colsample.bytree)

                      expect_true(all(size.tree))
                      expect_true(all(size.tree.matrix))

                    })


testthat::test_that(
  "Same results as MVP code - BT with CV - WITH bagging and colsample.bytree argument + interaction.depth NULL",
  {
    skip_on_cran()
    library(rpart)

    # Create datasets.
    set.seed(4)
    # dataset
    n <- 100000 # size of training set (number of observations)

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

    trainFraction <- 0.8
    datasetFull <-
      data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
    training.set <-
      datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
    validation.set <-
      datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

    ###############
    # Usage of Julien T. & Michel D. code for comparison purpose.
    ###############

    modelForm <-
      as.formula("Y ~ Age+Sport+Split+Gender+offset(log(ExpoRUpdated))")

    M <- 400 # number of trees
    NbSplits <- 4 # number of splits in the trees
    shrinkage.param <- 0.01
    alpha <- 0.5# bag fraction
    colsample.bytree <- 2
    varVec <- c("Age", "Sport", "Split", "Gender")

    depth <- NbSplits # depth of the trees (before being pruned)

    score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

    ###############
    # Boosting with log-link function.
    ###############

    training.set$ExpoRUpdated <-
      training.set$ExpoR * exp(score0) # initialisation
    validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

    gen.error.boost <- rep(0, M) # generalization error
    gen.error.boost.oos <- rep(0, M) # generalization error out of sample
    gen.error.boost.inbag <- rep(0, M)
    oob.improvement <- rep(0, M)

    size.tree <- rep(F, M) # size (number of internal nodes) of the trees

    varUsed <- list()

    set.seed(4)
    for (m in 1:M) {
      if (alpha != 1) {
        inSubset <-
          sample(1:nrow(training.set), alpha * nrow(training.set)) # to be align : createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
      } else{
        inSubset <- 1:nrow(training.set)
      }
      trainingsubset <- training.set[inSubset, ]
      oobsubset <-
        training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]

      # OOB computed thanks to previous model.
      oldOOBError <-
        1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                   + sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^
                                               (oobsubset$Y)
                                   )))

      if (length(varVec) > colsample.bytree) {
        varVecSample <- varVec[sample(1:length(varVec), colsample.bytree)]
        varUsed[[m]] <- varVecSample
        modelForm <-
          as.formula(paste(
            "Y ~ offset(log(ExpoRUpdated)) + ",
            paste(varVecSample, collapse = " + ")
          ))
      }

      tree <- rpart(
        formula = modelForm,
        data = trainingsubset,
        method = "poisson",
        control = rpart.control(
          cp = 0,
          maxdepth = depth,
          xval = 0,
          minsplit = 2
        )
      ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

      # NbSplits.up<-NbSplits
      # while (sum(printcp(tree)[,2]==NbSplits.up)==0){
      #   NbSplits.up=NbSplits.up-1
      # }
      #
      # size.tree[m]<-NbSplits.up+1
      #
      # cp.opt<-printcp(tree)[,1][printcp(tree)[,2]==NbSplits.up]
      #
      # tree.opt<-prune(tree,cp=cp.opt)
      tree.opt <- tree

      boostScoreHat <-
        log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
      training.set$ExpoRUpdated <-
        training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

      gen.error.boost[m] <-
        1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                      + sum(log((training.set$Y /
                                                   training.set$ExpoRUpdated) ^ (training.set$Y)
                                      )))


      trainingsubset <-
        training.set[inSubset, ] # Add by Gireg to have the in-bag error - note that training.set is updated before, we just slice again.
      gen.error.boost.inbag[m] <-
        1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                        + sum(log((trainingsubset$Y /
                                                     trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                        )))

      oobsubset <-
        training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]
      oob.improvement[m] <-
        (oldOOBError - 1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) -
                                                    sum(oobsubset$Y)
                                                  + sum(log((oobsubset$Y /
                                                               oobsubset$ExpoRUpdated) ^ (oobsubset$Y)
                                                  ))))

      boostScoreHat <-
        log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
      validation.set$ExpoRUpdated <-
        validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

      gen.error.boost.oos[m] <-
        1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                        + sum(log((validation.set$Y /
                                                     validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                        )))
    }

    min.error.boost.oos.fullRun <- min(gen.error.boost.oos)
    nb.trees.opt.boost.oos.fullRun <- which.min(gen.error.boost.oos)
    nb.trees.opt.boost.oob.fullRun <-
      which.min(-cumsum(oob.improvement))

    trainPred <- training.set$ExpoRUpdated / training.set$ExpoR
    validPred <- validation.set$ExpoRUpdated / validation.set$ExpoR

    ##########################
    # Cross-validation.
    ##########################

    nFolds <- 4
    folds <-
      c(
        rep(1, n * trainFraction / nFolds),
        rep(2, n * trainFraction / nFolds),
        rep(3, n * trainFraction / nFolds),
        rep(4, n * trainFraction / nFolds)
      )

    gen.error.boost.cv <-
      matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-training set.
    gen.error.boost.oos.cv <-
      matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-out of sample.
    gen.error.boost.inbag.cv <-
      matrix(0, ncol = nFolds, nrow = M) # generalization error on the cv-inbag sample (without OOB similar to gen.error.boost.cv)
    oob.improvement.cv <- matrix(0, ncol = nFolds, nrow = M)
    # gen.error.boost.cv.val <- matrix(0, ncol=nFolds, nrow=M) # generalization error on the validation set that has been kept aside.

    size.tree.matrix <-
      matrix(F, ncol = nFolds, nrow = M) # size (number of internal nodes) of the trees

    # Save full sets before performing cv.
    fullTrainingSet <-
      datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
    fullValidationSet <-
      datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

    cv.pred <- list()

    # Start CV.
    # set.seed(4)
    for (iFolds in seq(1, nFolds)) {
      set.seed((iFolds + 1) * 4)

      inCV <- which(folds == iFolds)
      outCV <- which(folds != iFolds)
      training.set <- fullTrainingSet[outCV, ]
      validation.set <- fullTrainingSet[inCV, ]

      # Initialization.
      score0 <- log(sum(training.set$Y) / sum(training.set$ExpoR))

      training.set$ExpoRUpdated <- training.set$ExpoR * exp(score0)
      validation.set$ExpoRUpdated <- validation.set$ExpoR * exp(score0)

      cv.pred[[iFolds]] <- list()

      for (m in 1:M) {
        if (alpha != 1) {
          inSubset <-
            sample(1:nrow(training.set), alpha * nrow(training.set)) # to be align : createDataPartition(training.set$Y, p=alpha[k], list=FALSE)
        } else{
          inSubset <- 1:nrow(training.set)
        }
        trainingsubset <- training.set[inSubset, ]
        oobsubset <-
          training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]

        # OOB computed thanks to previous model.
        oldOOBError <-
          1 / nrow(oobsubset) * 2 * (sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
                                     + sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^
                                                 (oobsubset$Y)
                                     )))

        if (length(varVec) > colsample.bytree) {
          varVecSample <- varVec[sample(1:length(varVec), colsample.bytree)]
          modelForm <-
            as.formula(paste(
              "Y ~ offset(log(ExpoRUpdated)) + ",
              paste(varVecSample, collapse = " + ")
            ))
        }

        tree <- rpart(
          formula = modelForm,
          data = trainingsubset,
          method = "poisson",
          control = rpart.control(
            cp = 0,
            maxdepth = depth,
            xval = 0,
            minsplit = 2
          )
        ) # cp = complexity parameter, maxdepth = maximum depth of any node of the final tree (root node counted as depth 0), xval=0: no cross validation

        # NbSplits.up<-NbSplits
        # while (sum(printcp(tree)[,2]==NbSplits.up)==0){
        #   NbSplits.up=NbSplits.up-1
        # }
        #
        # size.tree[m, iFolds]<-NbSplits.up+1
        #
        # cp.opt<-printcp(tree)[,1][printcp(tree)[,2]==NbSplits.up]

        # tree.opt<-prune(tree,cp=cp.opt)
        tree.opt <- tree

        boostScoreHat <-
          log(predict(tree.opt, training.set, type = "vector")) # predict: does not account for exposure-to-risk
        training.set$ExpoRUpdated <-
          training.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

        gen.error.boost.cv[m, iFolds] <-
          1 / nrow(training.set) * 2 * (sum(training.set$ExpoRUpdated) - sum(training.set$Y)
                                        + sum(log((training.set$Y /
                                                     training.set$ExpoRUpdated) ^ (training.set$Y)
                                        )))


        trainingsubset <-
          training.set[inSubset, ] # Add by Gireg to have the in-bag error - note that training.set is updated before, we just slice again.
        gen.error.boost.inbag.cv[m, iFolds] <-
          1 / nrow(trainingsubset) * 2 * (sum(trainingsubset$ExpoRUpdated) - sum(trainingsubset$Y)
                                          +
                                            sum(log((trainingsubset$Y / trainingsubset$ExpoRUpdated) ^ (trainingsubset$Y)
                                            )))

        oobsubset <-
          training.set[setdiff(seq(1, nrow(training.set)), inSubset), ]
        oob.improvement.cv[m, iFolds] <-
          (oldOOBError - 1 / nrow(oobsubset) * 2 * (
            sum(oobsubset$ExpoRUpdated) - sum(oobsubset$Y)
            +
              sum(log((oobsubset$Y / oobsubset$ExpoRUpdated) ^ (oobsubset$Y)
              ))
          ))

        boostScoreHat <-
          log(predict(tree.opt, validation.set, type = "vector")) # predict: does not account for exposure-to-risk
        validation.set$ExpoRUpdated <-
          validation.set$ExpoRUpdated * exp(shrinkage.param * boostScoreHat)

        gen.error.boost.oos.cv[m, iFolds] <-
          1 / nrow(validation.set) * 2 * (sum(validation.set$ExpoRUpdated) - sum(validation.set$Y)
                                          + sum(log((validation.set$Y /
                                                       validation.set$ExpoRUpdated) ^ (validation.set$Y)
                                          )))
        cv.pred[[iFolds]][[m]] <-
          log(validation.set$ExpoRUpdated / validation.set$ExpoR)

        # boostScoreHat<-log(predict(tree.opt, fullValidationSet, type = "vector")) # predict: does not account for exposure-to-risk
        # fullValidationSet$ExpoRUpdated<-fullValidationSet$ExpoRUpdated*exp(shrinkage.param*boostScoreHat)
        # gen.error.boost.cv.val[m, iFolds]<-1/nrow(fullValidationSet)*2*(sum(fullValidationSet$ExpoRUpdated)-sum(fullValidationSet$Y)
        #                                                                 +sum(log((fullValidationSet$Y/fullValidationSet$ExpoRUpdated)^(fullValidationSet$Y))))
      }
    }
    cv.errors <-
      rowSums(gen.error.boost.oos.cv * n * trainFraction / nFolds) / (n * trainFraction)
    min.cv.errors <- min(cv.errors)
    nb.trees.opt.boost.cv <- which.min(cv.errors)


    ###############
    # Comparison w/ BT algorithm.
    ###############

    # Define BT parameters.
    paramsBT <-
      list(
        formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
        data = datasetFull,
        tweedie.power = 1,
        ABT = F,
        n.iter = M,
        train.fraction = trainFraction,
        interaction.depth = NULL,
        shrinkage = shrinkage.param,
        bag.fraction = alpha,
        colsample.bytree = colsample.bytree,
        keep.data = T,
        is.verbose = T,
        cv.folds = 4,
        folds.id = folds,
        n.cores = 1,
        weights = ExpoR,
        seed = 4,
        tree.control = rpart.control(
          cp = 0,
          maxdepth = depth,
          xval = 0,
          minsplit = 2
        )
      )

    BT_algo <- do.call(BT, paramsBT)

    expect_equal(gen.error.boost.inbag, BT_algo$BTErrors$training.error)
    expect_equal(gen.error.boost.oos, BT_algo$BTErrors$validation.error)
    expect_equal(cv.errors, BT_algo$BTErrors$cv.error)

    expect_equal(trainPred, exp(BT_algo$fitted.values))
    expect_equal(oob.improvement, BT_algo$BTErrors$oob.improvement)
    expect_equal(
      validPred,
      predict(
        BT_algo,
        newdata = fullValidationSet,
        n.iter = M,
        type = 'response'
      )
    )

    expect_equal(nb.trees.opt.boost.oob.fullRun,
                 seq_len(BT_algo$BTParams$n.iter)[which.min(-cumsum(BT_algo$BTErrors$oob.improvement))]) # Can be different due to the smoother if we used BT_perf.
    expect_equal(
      nb.trees.opt.boost.oos.fullRun,
      BT::BT_perf(BT_algo, method = "validation", plot.it = F)
    )
    expect_equal(nb.trees.opt.boost.cv,
                 BT::BT_perf(BT_algo, method = "cv", plot.it = F))

    expect_equal(folds, BT_algo$folds)
    expect_equal(length(unique(folds)), BT_algo$cv.folds)

    expect_equal(c(cv.pred[[1]][[nb.trees.opt.boost.cv]], cv.pred[[2]][[nb.trees.opt.boost.cv]], cv.pred[[3]][[nb.trees.opt.boost.cv]],
                   cv.pred[[4]][[nb.trees.opt.boost.cv]]),
                 BT_algo$cv.fitted)

    expect_equal(unique(unlist(lapply(varUsed, length))), colsample.bytree)
    expect_equal(colsample.bytree, BT_algo$BTParams$colsample.bytree)

  }
)




#########################################################################
#########################################################################
####
#### Series of test to check whether we end up with the expected
#### number of internal nodes using classical BT.
####
#########################################################################
#########################################################################



testthat::test_that("Check expected ID", {
  skip_on_cran()
  # Create datasets.
  set.seed(1001) # 10
  # dataset
  n <- 10000 # size of training set (number of observations)

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

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
  training.set <-
    datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
  validation.set <-
    datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

  # Define BT parameters.
  n.iter <- 5000
  interaction.depth <- 8
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = F,
      n.iter = n.iter,
      train.fraction = trainFraction,
      interaction.depth = interaction.depth,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      n.cores = 1,
      weights = ExpoR,
      seed = 444
    )

  # Some tests to check whether the expected ID is reached.
  BT_algo1 <- do.call(BT, paramsBT)
  treesSize1 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo1$BTIndivFits[[xx]]$frame[BT_algo1$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 10
  paramsBT$interaction.depth <- interaction.depth
  BT_algo2 <- do.call(BT, paramsBT)
  treesSize2 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo2$BTIndivFits[[xx]]$frame[BT_algo2$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  paramsBT$formula <- as.formula("Y_normalized ~ Age")
  BT_algo3 <- do.call(BT, paramsBT)
  treesSize3 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo3$BTIndivFits[[xx]]$frame[BT_algo3$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 7
  paramsBT$interaction.depth <- interaction.depth
  paramsBT$formula <-
    as.formula("Y_normalized ~ Sport + Split + Gender")
  BT_algo4 <- do.call(BT, paramsBT)
  treesSize4 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo4$BTIndivFits[[xx]]$frame[BT_algo4$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  expect_true(all(treesSize1))
  expect_true(all(treesSize2))
  expect_true(all(treesSize3))
  expect_true(all(treesSize4))

})

testthat::test_that("Check expected ID - Decrease sample size", {
  skip_on_cran()
  # Create datasets.
  set.seed(22) #101
  # dataset
  n <- 5000 # size of training set (number of observations)

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

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
  training.set <-
    datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
  validation.set <-
    datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

  # Define BT parameters.
  n.iter <- 5000
  interaction.depth <- 8
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = F,
      n.iter = n.iter,
      train.fraction = trainFraction,
      interaction.depth = interaction.depth,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      n.cores = 1,
      weights = ExpoR,
      seed = 4012
    )

  # Some tests to check whether the expected ID is reached.
  BT_algo1 <- do.call(BT, paramsBT)
  treesSize1 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo1$BTIndivFits[[xx]]$frame[BT_algo1$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 10
  paramsBT$interaction.depth <- interaction.depth
  BT_algo2 <- do.call(BT, paramsBT)
  treesSize2 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo2$BTIndivFits[[xx]]$frame[BT_algo2$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  paramsBT$formula <- as.formula("Y_normalized ~ Age")
  BT_algo3 <- do.call(BT, paramsBT)
  treesSize3 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo3$BTIndivFits[[xx]]$frame[BT_algo3$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 7
  paramsBT$interaction.depth <- interaction.depth
  paramsBT$formula <-
    as.formula("Y_normalized ~ Sport + Split + Gender")
  BT_algo4 <- do.call(BT, paramsBT)
  treesSize4 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo4$BTIndivFits[[xx]]$frame[BT_algo4$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  expect_true(all(treesSize1))
  expect_true(all(treesSize2))
  expect_true(all(treesSize3))
  expect_true(all(treesSize4))

})

testthat::test_that("Check expected ID - Increase sample size", {
  skip_on_cran()
  # Create datasets.
  set.seed(1)
  # dataset
  n <- 100000 # size of training set (number of observations)

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

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
  training.set <-
    datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
  validation.set <-
    datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

  # Define BT parameters.
  n.iter <- 2500
  interaction.depth <- 8
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = F,
      n.iter = n.iter,
      train.fraction = trainFraction,
      interaction.depth = interaction.depth,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      n.cores = 1,
      weights = ExpoR,
      seed = 555
    )

  # Some tests to check whether the expected ID is reached.
  BT_algo1 <- do.call(BT, paramsBT)
  treesSize1 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo1$BTIndivFits[[xx]]$frame[BT_algo1$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 10
  paramsBT$interaction.depth <- interaction.depth
  BT_algo2 <- do.call(BT, paramsBT)
  treesSize2 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo2$BTIndivFits[[xx]]$frame[BT_algo2$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  paramsBT$formula <- as.formula("Y_normalized ~ Age")
  BT_algo3 <- do.call(BT, paramsBT)
  treesSize3 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo3$BTIndivFits[[xx]]$frame[BT_algo3$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 7
  paramsBT$interaction.depth <- interaction.depth
  paramsBT$formula <-
    as.formula("Y_normalized ~ Sport + Split + Gender")
  BT_algo4 <- do.call(BT, paramsBT)
  treesSize4 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo4$BTIndivFits[[xx]]$frame[BT_algo4$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  expect_true(all(treesSize1))
  expect_true(all(treesSize2))
  expect_true(all(treesSize3))
  expect_true(all(treesSize4))

})


# Random datas.

testthat::test_that("Check expected ID - Random datas", {
  skip_on_cran()
  # Create datasets.
  set.seed(100)
  # dataset
  n <- 10000 # size of training set (number of observations)

  Gender <- factor(sample(c("male", "female"), n, replace = TRUE))
  Age <- sample(c(18:65), n, replace = TRUE)
  Split <- factor(sample(c("yes", "no"), n, replace = TRUE))
  Sport <- factor(sample(c("yes", "no"), n, replace = TRUE))

  lambda <- runif(n, min = 0, max = 2)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR * lambda)
  Y_normalized <- Y / ExpoR

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
  training.set <-
    datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
  validation.set <-
    datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

  # Define BT parameters.
  n.iter <- 5000
  interaction.depth <- 8
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = F,
      n.iter = n.iter,
      train.fraction = trainFraction,
      interaction.depth = interaction.depth,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      n.cores = 1,
      weights = ExpoR,
      seed = 44
    )

  # Some tests to check whether the expected ID is reached.
  BT_algo1 <- do.call(BT, paramsBT)
  treesSize1 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo1$BTIndivFits[[xx]]$frame[BT_algo1$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 10
  paramsBT$interaction.depth <- interaction.depth
  BT_algo2 <- do.call(BT, paramsBT)
  treesSize2 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo2$BTIndivFits[[xx]]$frame[BT_algo2$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  paramsBT$formula <- as.formula("Y_normalized ~ Age")
  BT_algo3 <- do.call(BT, paramsBT)
  treesSize3 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo3$BTIndivFits[[xx]]$frame[BT_algo3$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 7
  paramsBT$interaction.depth <- interaction.depth
  paramsBT$formula <-
    as.formula("Y_normalized ~ Sport + Split + Gender")
  BT_algo4 <- do.call(BT, paramsBT)
  treesSize4 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo4$BTIndivFits[[xx]]$frame[BT_algo4$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  expect_true(all(treesSize1))
  expect_true(all(treesSize2))
  expect_true(all(treesSize3))
  expect_true(all(treesSize4))

})

testthat::test_that("Check expected ID - Decrease sample size - Random datas", {
  skip_on_cran()
  # Create datasets.
  set.seed(204) # 200
  # dataset
  n <- 5000 # size of training set (number of observations)

  Gender <- factor(sample(c("male", "female"), n, replace = TRUE))
  Age <- sample(c(18:65), n, replace = TRUE)
  Split <- factor(sample(c("yes", "no"), n, replace = TRUE))
  Sport <- factor(sample(c("yes", "no"), n, replace = TRUE))

  lambda <- runif(n, min = 0, max = 2)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR * lambda)
  Y_normalized <- Y / ExpoR

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
  training.set <-
    datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
  validation.set <-
    datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

  # Define BT parameters.
  n.iter <- 5000
  interaction.depth <- 8
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = F,
      n.iter = n.iter,
      train.fraction = trainFraction,
      interaction.depth = interaction.depth,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      n.cores = 1,
      weights = ExpoR,
      seed = 1
    )

  # Some tests to check whether the expected ID is reached.
  BT_algo1 <- do.call(BT, paramsBT)
  treesSize1 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo1$BTIndivFits[[xx]]$frame[BT_algo1$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 10
  paramsBT$interaction.depth <- interaction.depth
  BT_algo2 <- do.call(BT, paramsBT)
  treesSize2 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo2$BTIndivFits[[xx]]$frame[BT_algo2$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  paramsBT$formula <- as.formula("Y_normalized ~ Age")
  BT_algo3 <- do.call(BT, paramsBT)
  treesSize3 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo3$BTIndivFits[[xx]]$frame[BT_algo3$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 7
  paramsBT$interaction.depth <- interaction.depth
  paramsBT$formula <-
    as.formula("Y_normalized ~ Sport + Split + Gender")
  BT_algo4 <- do.call(BT, paramsBT)
  treesSize4 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo4$BTIndivFits[[xx]]$frame[BT_algo4$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  expect_true(all(treesSize1))
  expect_true(all(treesSize2))
  expect_true(all(treesSize3))
  expect_true(all(treesSize4))

})

testthat::test_that("Check expected ID - Increase sample size - Random datas", {
  skip_on_cran()
  # Create datasets.
  set.seed(2022) # 10000
  # dataset
  n <- 100000 # size of training set (number of observations)

  Gender <- factor(sample(c("male", "female"), n, replace = TRUE))
  Age <- sample(c(18:65), n, replace = TRUE)
  Split <- factor(sample(c("yes", "no"), n, replace = TRUE))
  Sport <- factor(sample(c("yes", "no"), n, replace = TRUE))

  lambda <- runif(n, min = 0, max = 2)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR * lambda)
  Y_normalized <- Y / ExpoR

  trainFraction <- 0.8
  datasetFull <-
    data.frame(Y, Gender, Age, Split, Sport, ExpoR, Y_normalized)
  training.set <-
    datasetFull[seq(1, trainFraction * nrow(datasetFull)), ]
  validation.set <-
    datasetFull[seq(trainFraction * nrow(datasetFull) + 1, nrow(datasetFull)), ]

  # Define BT parameters.
  n.iter <- 2500
  interaction.depth <- 8
  paramsBT <-
    list(
      formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
      data = datasetFull,
      tweedie.power = 1,
      ABT = F,
      n.iter = n.iter,
      train.fraction = trainFraction,
      interaction.depth = interaction.depth,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      colsample.bytree = NULL,
      keep.data = F,
      is.verbose = F,
      cv.folds = 1,
      n.cores = 1,
      weights = ExpoR,
      seed = 50
    )

  # Some tests to check whether the expected ID is reached.
  BT_algo1 <- do.call(BT, paramsBT)
  treesSize1 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo1$BTIndivFits[[xx]]$frame[BT_algo1$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 10
  paramsBT$interaction.depth <- interaction.depth
  BT_algo2 <- do.call(BT, paramsBT)
  treesSize2 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo2$BTIndivFits[[xx]]$frame[BT_algo2$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  paramsBT$formula <- as.formula("Y_normalized ~ Age")
  BT_algo3 <- do.call(BT, paramsBT)
  treesSize3 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo3$BTIndivFits[[xx]]$frame[BT_algo3$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  interaction.depth <- 7
  paramsBT$interaction.depth <- interaction.depth
  paramsBT$formula <-
    as.formula("Y_normalized ~ Sport + Split + Gender")
  BT_algo4 <- do.call(BT, paramsBT)
  treesSize4 <-
    (sapply(seq(1, n.iter), function(xx) {
      nrow(BT_algo4$BTIndivFits[[xx]]$frame[BT_algo4$BTIndivFits[[xx]]$frame$var != "<leaf>", ])
    }) == interaction.depth)

  expect_true(all(treesSize1))
  expect_true(all(treesSize2))
  expect_true(all(treesSize3))
  expect_true(all(treesSize4))

})
