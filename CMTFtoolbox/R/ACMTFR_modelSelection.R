#' Model selection for ACMTFR
#'
#' @inheritParams acmtfr_opt
#' @inheritParams ACMTF_modelSelection
#' @param normalize Normalize the X blocks to frobenius norm 1 (default TRUE).
#' @param normY Normalize Y to a specific value, (default: 1).
#'
#' @return List object containing plots of all metrics and dataframes containing the data used to create them.
#'
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom foreach %dopar%
#'
#' @examples
#' set.seed(123)
#'
#' I = 10
#' J = 5
#' K = 3
#' df = array(rnorm(I*J*K), c(I,J,K))
#' df2 = array(rnorm(I*J*K), c(I,J,K))
#' datasets = list(df, df2)
#' modes = list(c(1,2,3), c(1,4,5))
#' Y = as.matrix(rnorm(I))
#'
#' # A very small procedure is run to limit computational requirements
#' result = ACMTFR_modelSelection(datasets,
#'                               modes,
#'                               Y,
#'                               pi=1.0,
#'                               maxNumComponents=2,
#'                               nstart=2,
#'                               cvFolds=2,
#'                               rel_tol=0.5,
#'                               abs_tol=0.5)
#'
#' result$plots$overview
ACMTFR_modelSelection = function(datasets, modes, Y,
                                 sharedMode=1,
                                 maxNumComponents = 5,
                                 alpha = 1,
                                 beta = rep(1e-3, length(Z$object)),
                                 epsilon = 1e-8,
                                 pi = 0.5,
                                 normalize = TRUE,
                                 normY = 1,
                                 method = "CG",
                                 cg_update = "HS",
                                 line_search = "MT",
                                 max_iter = 10000,
                                 max_fn = 10000,
                                 abs_tol = 1e-10,
                                 rel_tol = 1e-10,
                                 grad_tol = 1e-10,
                                 nstart = 5,
                                 numCores = 1,
                                 cvFolds = 2) {

  numDatasets = length(datasets)
  numModes = max(unlist(modes))
  numSubjects = dim(datasets[[1]])[1]

  # Compute FMS random, variance explained, and degeneracy using the full data
  Z = setupCMTFdata(datasets, modes, normalize=TRUE)

  ## Prepare Y
  Ycnt = Y - mean(Y)
  Ynorm = Ycnt / norm(Ycnt, "2")
  Yfinal = Ynorm * normY

  varExps = list()
  FMS_random_result = list()
  degeneracy_result = list()
  RMSE_result = list()

  for(i in 1:maxNumComponents){
    models = acmtfr_opt(Z, Yfinal, i, alpha=alpha, beta=beta, epsilon=epsilon, pi=pi, method=method, cg_update=cg_update, line_search=line_search, max_iter=max_iter, max_fn=max_fn, rel_tol=rel_tol, abs_tol=abs_tol, grad_tol=grad_tol, nstart=nstart, allOutput = TRUE)

    # Compute FMS_random
    numCombinations = (nstart * (nstart-1))/2
    FMS = matrix(0L, nrow=numCombinations, ncol=numDatasets)
    iterator = 1
    for(j in 1:(nstart-1)){
      for(k in (j+1):nstart){
        result = rep(0L, numDatasets)
        for(p in 1:numDatasets){
          Fac1 = models[[j]]$Fac[modes[[p]]]
          Fac2 = models[[k]]$Fac[modes[[p]]]
          result[p] = FMS_random(Fac1, Fac2)
        }
        FMS[iterator,] = result
        iterator = iterator + 1
      }
    }
    FMS_random_result[[i]] = FMS

    # Find best model for variance explained
    loss = unlist(lapply(models, FUN=function(x){x$f}))
    index = which(loss == min(loss))
    bestModel = models[[index]]
    varExps[[i]] = c(bestModel$varExp, bestModel$varExpY)

    # Check degeneracy
    degeneracy_result[[i]] = unlist(lapply(models, FUN=function(x){degenScore(x$Fac[[1]])}))

    # Compute RMSE
    pred = bestModel$Yhat
    pred_norm = pred / normY
    pred_cnt = pred_norm * norm(Ycnt, "2")
    pred_original = pred_cnt + mean(Y)

    RMSE_result[[i]] = sqrt(mean((Y - pred_original)^2))
  }

  degeneracy_result = unlist(degeneracy_result)
  RMSE_result = unlist(RMSE_result)
  varExps = do.call(rbind, varExps)

  # Create CV folds
  indices = seq_len(numSubjects)
  foldsPartition = split(indices, cut(seq_along(indices), breaks = cvFolds, labels = FALSE))
  uniqueFolds = seq_len(cvFolds)

  ## --- Create Settings Data Frame ---
  settings = expand.grid(numComponents = 1:maxNumComponents,
                         fold = uniqueFolds,
                         replicate = 1:nstart)
  settings = settings[order(settings$numComponents, settings$fold, settings$replicate), ]

  ## --- Run the Parallel Loop Over All Settings ---
  if (numCores > 1) {
    cl = parallel::makeCluster(numCores)
    doParallel::registerDoParallel(cl)
    resultsList = foreach::foreach(i = 1:nrow(settings),
                                   .packages = c("CMTFtoolbox")) %dopar% {
                                     currentRow = settings[i, ]
                                     currentComp = currentRow$numComponents
                                     foldID = currentRow$fold
                                     repID = currentRow$replicate

                                     testIdx = foldsPartition[[foldID]]
                                     trainIdx = setdiff(seq_len(numSubjects), testIdx)

                                     ## Prepare X
                                     Xtrain_final = list()
                                     Xtest_final = list()
                                     for(p in 1:length(Z$object)){

                                       if(length(dim(Z$object[[p]]@data))==3){
                                         Xtrain = rTensor::as.tensor(Z$object[[p]]@data[trainIdx, ,])
                                         Xtest = rTensor::as.tensor(Z$object[[p]]@data[testIdx, ,])
                                       } else{
                                         Xtrain = rTensor::as.tensor(Z$object[[p]]@data[trainIdx,])
                                         Xtest = rTensor::as.tensor(Z$object[[p]]@data[testIdx,])
                                       }


                                       # Centering Xtrain
                                       unfoldedXtrain = rTensor::k_unfold(Xtrain, 1)@data
                                       means = colMeans(unfoldedXtrain, na.rm=TRUE)
                                       unfoldedXtrain_cnt = sweep(unfoldedXtrain, 2, means, FUN="-")
                                       Xtrain_cnt = rTensor::k_fold(unfoldedXtrain_cnt, m=1, modes=Xtrain@modes)

                                       # Use the means to center Xtest as well
                                       unfoldedXtest = rTensor::k_unfold(Xtest, 1)@data
                                       unfoldedXtest_cnt = sweep(unfoldedXtest, 2, means, FUN="-")
                                       Xtest_cnt = rTensor::k_fold(unfoldedXtest_cnt, m=1, modes=Xtest@modes)

                                       # Scaling Xtrain
                                       unfoldedXtrain = rTensor::k_unfold(Xtrain_cnt, 2)@data
                                       stds = apply(unfoldedXtrain, 1, function(x){stats::sd(x, na.rm=TRUE)})
                                       unfoldedXtrain_scl = sweep(unfoldedXtrain, 1, stds, FUN="/")
                                       Xtrain_cnt_scl = rTensor::k_fold(unfoldedXtrain_scl, m=2, modes=Xtrain@modes)

                                       # Use the stds to scale Xtest as well
                                       unfoldedXtest = rTensor::k_unfold(Xtest_cnt, 2)@data
                                       unfoldedXtest_scl = sweep(unfoldedXtest, 1, stds, FUN="/")
                                       Xtest_cnt_scl = rTensor::k_fold(unfoldedXtest_scl, m=2, modes=Xtest@modes)

                                       if(normalize){
                                         norm = rTensor::fnorm(Xtrain_cnt_scl)
                                         Xtrain_final[[p]] = Xtrain_cnt_scl@data / norm
                                         Xtest_final[[p]] = Xtest_cnt_scl@data / norm
                                       } else{
                                         Xtrain_final[[p]] = Xtrain_cnt_scl@data
                                         Xtest_final[[p]] = Xtest_cnt_scl@data
                                       }
                                     }

                                     Ztrain = setupCMTFdata(Xtrain_final, Z$modes, normalize=FALSE) # do not normalize again

                                     ## Prepare Y
                                     Ytrain = Y[trainIdx]
                                     Ymean = mean(Ytrain)
                                     Ytrain_cnt = Ytrain - Ymean

                                     Ynorm = norm(Ytrain_cnt, "2")
                                     Ytrain_normalized = Ytrain_cnt / Ynorm

                                     Ytrain_final = Ytrain_normalized * normY

                                     # Each replicate is fitted with a single initialization and one core.
                                     model_fit = CMTFtoolbox::acmtfr_opt(Ztrain, Ytrain_final,
                                                                         numComponents = currentComp,
                                                                         alpha         = alpha,
                                                                         beta          = beta,
                                                                         epsilon       = epsilon,
                                                                         pi            = pi,
                                                                         method        = method,
                                                                         cg_update     = cg_update,
                                                                         line_search   = line_search,
                                                                         max_iter      = max_iter,
                                                                         max_fn        = max_fn,
                                                                         abs_tol       = abs_tol,
                                                                         rel_tol       = rel_tol,
                                                                         grad_tol      = grad_tol,
                                                                         nstart        = 1,
                                                                         numCores      = 1)
                                     list(numComponents = currentComp,
                                          fold = foldID,
                                          replicate = repID,
                                          testIdx = testIdx,
                                          Ztrain = Ztrain,
                                          model = model_fit,
                                          Xtest = Xtest_final,
                                          Ymean = Ymean,
                                          Ynorm = Ynorm)
                                   }
    parallel::stopCluster(cl)
  } else {
    resultsList = vector("list", nrow(settings))
    for (i in 1:nrow(settings)) {
      currentRow = settings[i, ]
      currentComp = currentRow$numComponents
      foldID = currentRow$fold
      repID = currentRow$replicate

      testIdx = foldsPartition[[foldID]]
      trainIdx = setdiff(seq_len(numSubjects), testIdx)

      ## Prepare X
      Xtrain_final = list()
      Xtest_final = list()
      for(p in 1:length(Z$object)){

        if(length(dim(Z$object[[p]]@data))==3){
          Xtrain = rTensor::as.tensor(Z$object[[p]]@data[trainIdx, ,])
          Xtest = rTensor::as.tensor(Z$object[[p]]@data[testIdx, ,])
        } else{
          Xtrain = rTensor::as.tensor(Z$object[[p]]@data[trainIdx,])
          Xtest = rTensor::as.tensor(Z$object[[p]]@data[testIdx,])
        }

        # Centering Xtrain
        unfoldedXtrain = rTensor::k_unfold(Xtrain, 1)@data
        means = colMeans(unfoldedXtrain, na.rm=TRUE)
        unfoldedXtrain_cnt = sweep(unfoldedXtrain, 2, means, FUN="-")
        Xtrain_cnt = rTensor::k_fold(unfoldedXtrain_cnt, m=1, modes=Xtrain@modes)

        # Use the means to center Xtest as well
        unfoldedXtest = rTensor::k_unfold(Xtest, 1)@data

        if(length(testIdx) == 1){ # deal with jack-knife corner case
          unfoldedXtest_cnt = unfoldedXtest - means
        } else{
          unfoldedXtest_cnt = sweep(unfoldedXtest, 2, means, FUN="-")
        }

        Xtest_cnt = rTensor::k_fold(unfoldedXtest_cnt, m=1, modes=Xtest@modes)

        # Scaling Xtrain
        unfoldedXtrain = rTensor::k_unfold(Xtrain_cnt, 2)@data
        stds = apply(unfoldedXtrain, 1, function(x){stats::sd(x, na.rm=TRUE)})
        unfoldedXtrain_scl = sweep(unfoldedXtrain, 1, stds, FUN="/")
        Xtrain_cnt_scl = rTensor::k_fold(unfoldedXtrain_scl, m=2, modes=Xtrain@modes)

        # Use the stds to scale Xtest as well
        unfoldedXtest = rTensor::k_unfold(Xtest_cnt, 2)@data

        if(length(testIdx) == 1){ # deal with jack-knife corner case
          unfoldedXtest_scl = unfoldedXtest / stds
        } else{
          unfoldedXtest_scl = sweep(unfoldedXtest, 1, stds, FUN="/")
        }

        Xtest_cnt_scl = rTensor::k_fold(unfoldedXtest_scl, m=2, modes=Xtest@modes)

        if(normalize){
          norm = rTensor::fnorm(Xtrain_cnt_scl)
          Xtrain_final[[p]] = Xtrain_cnt_scl@data / norm
          Xtest_final[[p]] = Xtest_cnt_scl@data / norm
        } else{
          Xtrain_final[[p]] = Xtrain_cnt_scl@data
          Xtest_final[[p]] = Xtest_cnt_scl@data
        }
      }

      Ztrain = setupCMTFdata(Xtrain_final, Z$modes, normalize=FALSE) # do not normalize again

      ## Prepare Y
      Ytrain = Y[trainIdx]
      Ymean = mean(Ytrain)
      Ytrain_cnt = Ytrain - Ymean

      Ynorm = norm(Ytrain_cnt, "2")
      Ytrain_normalized = Ytrain_cnt / Ynorm

      Ytrain_final = Ytrain_normalized * normY

      # Fit model
      model = CMTFtoolbox::acmtfr_opt(Ztrain, Ytrain_final,
                                      numComponents = currentComp,
                                      alpha         = alpha,
                                      beta          = beta,
                                      epsilon       = epsilon,
                                      pi            = pi,
                                      method        = method,
                                      cg_update     = cg_update,
                                      line_search   = line_search,
                                      max_iter      = max_iter,
                                      max_fn        = max_fn,
                                      abs_tol       = abs_tol,
                                      rel_tol       = rel_tol,
                                      grad_tol      = grad_tol,
                                      nstart        = 1,
                                      numCores      = 1)

      resultsList[[i]] = list(numComponents = currentComp,
                              fold = foldID,
                              replicate = repID,
                              testIdx = testIdx,
                              Ztrain = Ztrain,
                              model = model,
                              Xtest = Xtest_final,
                              Ymean = Ymean,
                              Ynorm = Ynorm)
    }
  }

  ## --- Group the Results by (numComponents, fold) and Select the Best Model --- ##
  # Create a grouping key for each result.
  keys = sapply(resultsList, function(x) paste(x$numComponents, x$fold, sep = "_"))
  resultsByGroup = split(resultsList, keys)

  bestModels = lapply(resultsByGroup, function(group) {
    f_vals = sapply(group, function(x) x$model$f)
    group[[which.min(f_vals)]]
  })

  ## --- Calculate FMS_CV for all folds --- ##
  FMS_CV_result = list()
  numCombinations = (cvFolds * (cvFolds-1)) / 2
  for(i in 1:maxNumComponents){
    FMS = matrix(0L, nrow=numCombinations, ncol=numDatasets)
    iterator = 1

    for(j in 1:(cvFolds-1)){
      for(k in (j+1):cvFolds){
        result = rep(0L, numDatasets)
        for(p in 1:numDatasets){
          Fac1 = bestModels[[paste(i, j, sep="_")]]$model$Fac[modes[[p]]]
          Fac2 = bestModels[[paste(i, k, sep="_")]]$model$Fac[modes[[p]]]
          result[p] = FMS_cv(Fac1, Fac2, sharedMode=sharedMode)
        }
        FMS[iterator,] = result
        iterator = iterator + 1
      }
    }
    FMS_CV_result[[i]] = FMS
  }

  ## RMSE
  RMSEdata = dplyr::tibble(numComponents = 1:maxNumComponents,
                           RMSE = RMSE_result)

  ## --- Assemble Predictions and Compute RMSE for Each Number of Components --- ##
  RMSE_list = rep(NA, maxNumComponents)
  predictionsByComp = vector("list", maxNumComponents)

  for (comp in 1:maxNumComponents) {
    Ytest_comp = rep(NA, numSubjects)
    Ypred_comp = rep(NA, numSubjects)
    foldsToUse = uniqueFolds

    for (fold in foldsToUse) {
      key = paste(comp, fold, sep = "_")
      if (!is.null(bestModels[[key]])) {
        bestEntry = bestModels[[key]]
        pred = npred(bestEntry$model, bestEntry$Xtest, bestEntry$Ztrain)
        pred_norm = pred / normY
        pred_cnt = pred_norm * bestEntry$Ynorm
        pred_original = pred_cnt + bestEntry$Ymean

        Ypred_comp[bestEntry$testIdx] = pred_original
      }
    }
    predictionsByComp[[comp]] = Ypred_comp
    RMSE_list[comp] = sqrt(mean((Y - Ypred_comp)^2))
  }

  RMSECVdata = dplyr::tibble(numComponents = 1:maxNumComponents,
                           RMSECV = RMSE_list)

  metrics = list("varExp" = varExps,
                 "FMS_random" = FMS_random_result,
                 "FMS_CV" = FMS_CV_result,
                 "degeneracyScore" = degeneracy_result,
                 "RMSECV" = RMSECVdata,
                 "RMSE" = RMSEdata)

  # Make plots
  plots = list()

  # varExp
  df = varExps %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(numComponents = 1:maxNumComponents)

  colnames(df) = c(c(paste0("X", 1:numDatasets), "Y"), "numComponents")

  plots$varExp = df %>%
    tidyr::pivot_longer(-numComponents) %>%
    ggplot2::ggplot(ggplot2::aes(x=as.factor(numComponents),y=value,col=as.factor(name),group=as.factor(name))) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab("Number of components") +
    ggplot2::ylab("Variance explained (%)") +
    ggplot2::guides(colour=ggplot2::guide_legend(title="Dataset"))

  # FMS_random
  df = do.call(rbind, FMS_random_result) %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(numComponents=rep(1:maxNumComponents,each=nrow(FMS_random_result[[1]])))

  colnames(df) = c(paste0("X", 1:numDatasets), "numComponents")

  plots$FMS_random = df %>%
    tidyr::pivot_longer(-numComponents) %>%
    ggplot2::ggplot(ggplot2::aes(x=as.factor(numComponents),y=value,fill=as.factor(name))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Number of components") +
    ggplot2::ylab(expression(FMS[random])) +
    ggplot2::guides(fill=ggplot2::guide_legend(title="Dataset"))

  # RMSE of the randomly initialized models
  plots$RMSE = RMSEdata %>%
    ggplot2::ggplot(ggplot2::aes(x=as.factor(numComponents),y=RMSE,group=1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab("Number of components") +
    ggplot2::ylab("RMSE")

  # Degeneracy score
  df = degeneracy_result %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(numComponents = rep(1:maxNumComponents, each=nstart))
  colnames(df) = c("degeneracy", "numComponents")

  plots$degeneracyScore = df %>%
    ggplot2::ggplot(ggplot2::aes(x=as.factor(numComponents),y=degeneracy)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Number of components") +
    ggplot2::ylab("Degeneracy score") +
    ggplot2::guides(colour=ggplot2::guide_legend(title="Dataset"))

  # FMS_CV
  df = do.call(rbind, FMS_CV_result) %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(numComponents=rep(1:maxNumComponents,each=nrow(FMS_CV_result[[1]])))

  colnames(df) = c(paste0("X", 1:numDatasets), "numComponents")

  plots$FMS_CV = df %>%
    tidyr::pivot_longer(-numComponents) %>%
    ggplot2::ggplot(ggplot2::aes(x=as.factor(numComponents),y=value,fill=as.factor(name))) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Number of components") +
    ggplot2::ylab(expression(FMS[CV])) +
    ggplot2::guides(fill=ggplot2::guide_legend(title="Dataset"))

  # RMSECV
  plots$RMSECV = RMSECVdata %>%
    ggplot2::ggplot(ggplot2::aes(x=as.factor(numComponents),y=RMSECV,group=1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab("Number of components") +
    ggplot2::ylab("RMSECV")

  plots$overview = ggpubr::ggarrange(plots$varExp, plots$FMS_random, plots$RMSE, plots$degeneracyScore, plots$FMS_CV, plots$RMSECV, common.legend=TRUE)

  return(list("metrics" = metrics,
              "plots"=plots))
}

# Ugly solution to namespace issues caused by dplyr
RMSE = NULL
RMSECV = NULL
value = NULL
name = NULL
numComponents = NULL
