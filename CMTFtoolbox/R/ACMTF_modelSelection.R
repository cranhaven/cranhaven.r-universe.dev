#' Model selection for ACMTF
#'
#' @inheritParams acmtf_opt
#' @inheritParams setupCMTFdata
#' @param sharedMode Mode that is shared between all blocks, used to remove fibers for numFolds randomly initialized models.
#' @param maxNumComponents Maximum number of components to check (default 3).
#' @param nstart Number of randomly initialized models to create (default 10).
#' @param cvFolds Number of CV folds to create (default 10).
#' @param numCores Number of cores to use (default 1). A number higher than 1 will run the process in parallel.
#' @param plots Boolean to state if plots should be made of the outcome.
#'
#' @return List object containing variation explained, FMS, and degeneracy score metrics. If plots=TRUE, plots will be attached to the list.
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
#'
#' # A very small procedure is run to limit computational requirements
#' # Plots are not made to reduce CRAN runtime.
#' result = ACMTF_modelSelection(datasets,
#'                               modes,
#'                               maxNumComponents=1,
#'                               nstart=2,
#'                               cvFolds=2,
#'                               rel_tol=1e-1,
#'                               abs_tol=1e-1,
#'                               max_iter=2,
#'                               plots=FALSE)
ACMTF_modelSelection = function(datasets, modes, maxNumComponents=3, sharedMode=1, alpha=1, beta=rep(0.001, length(datasets)), epsilon=1e-8, nstart=10, cvFolds=10, numCores=1, method="CG", cg_update="HS", line_search="MT", max_iter=10000, max_fn=100000, rel_tol=1e-8, abs_tol=1e-8, grad_tol=1e-8, plots=TRUE){

  numDatasets = length(datasets)
  numModes = max(unlist(modes))
  numSubjects = dim(datasets[[1]])[1]

  # Compute FMS random, variance explained, and degeneracy using the full data
  Z = setupCMTFdata(datasets, modes, normalize=TRUE)
  varExps = list()
  FMS_random_result = list()
  degeneracy_result = list()

  for(i in 1:maxNumComponents){
    models = acmtf_opt(Z, i, alpha=alpha, beta=beta, epsilon=epsilon, method=method, cg_update=cg_update, line_search="MT", max_iter=max_iter, max_fn=max_fn, rel_tol=rel_tol, abs_tol=abs_tol, grad_tol=grad_tol, nstart=nstart, allOutput = TRUE)

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
    varExps[[i]] = bestModel$varExp

    # Check degeneracy
    degeneracy_result[[i]] = unlist(lapply(models, FUN=function(x){degenScore(x$Fac[[1]])}))
  }

  degeneracy_result = unlist(degeneracy_result)
  varExps = do.call(rbind, varExps)
  random_models = models

  # Compute FMS per CV fold

  # Create CV folds
  indices = seq_len(numSubjects)
  foldsPartition = split(indices, cut(seq_along(indices), breaks = cvFolds, labels = FALSE))
  uniqueFolds = seq_len(cvFolds)

  ## --- Create Settings Data Frame ---
  settings = expand.grid(numComponents = 1:maxNumComponents,
                         fold = uniqueFolds,
                         replicate = 1:nstart)

  ## Run CV scheme
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
                                     newDatasets = list()
                                     for(p in 1:numDatasets){
                                       X = rTensor::as.tensor(datasets[[p]])
                                       newModes = X@modes
                                       newModes[1] = length(trainIdx)

                                       # Center
                                       unfoldedX = rTensor::k_unfold(X, 1)@data
                                       unfoldedX = unfoldedX[trainIdx,] # mask is applied here to work for 2-3 way
                                       means = colMeans(unfoldedX, na.rm=TRUE)
                                       unfoldedX_cnt = sweep(unfoldedX, 2, means, FUN="-")
                                       X_cnt = rTensor::k_fold(unfoldedX_cnt, m=1, modes=newModes)

                                       # Scale
                                       unfoldedX = rTensor::k_unfold(X_cnt, 2)@data
                                       stds = apply(unfoldedX, 1, function(x){stats::sd(x, na.rm=TRUE)})
                                       unfoldedX_scl = sweep(unfoldedX, 1, stds, FUN="/")
                                       X_cnt_scl = rTensor::k_fold(unfoldedX_scl, m=2, modes=newModes)

                                       newDatasets[[p]] = X_cnt_scl@data
                                     }

                                     newZ = setupCMTFdata(newDatasets, modes, normalize=TRUE)

                                     # Each replicate is fitted with a single initialization and one core.
                                     model_fit = CMTFtoolbox::acmtf_opt(newZ,
                                                                        numComponents = currentComp,
                                                                        alpha         = alpha,
                                                                        beta          = beta,
                                                                        epsilon       = epsilon,
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
                                          Z = newZ,
                                          model = model_fit)
                                   }
    parallel::stopCluster(cl)
  } else {
    resultsList = vector("list", nrow(settings))
    for(i in 1:nrow(settings)){
      currentRow = settings[i, ]
      currentComp = currentRow$numComponents
      foldID = currentRow$fold
      repID = currentRow$replicate

      testIdx = foldsPartition[[foldID]]
      trainIdx = setdiff(seq_len(numSubjects), testIdx)

      ## Prepare X
      newDatasets = list()
      for(p in 1:numDatasets){
        X = rTensor::as.tensor(datasets[[p]])
        newModes = X@modes
        newModes[1] = length(trainIdx)

        # Center
        unfoldedX = rTensor::k_unfold(X, 1)@data
        unfoldedX = unfoldedX[trainIdx,] # mask is applied here to work for 2-3 way
        means = colMeans(unfoldedX, na.rm=TRUE)
        unfoldedX_cnt = sweep(unfoldedX, 2, means, FUN="-")
        X_cnt = rTensor::k_fold(unfoldedX_cnt, m=1, modes=newModes)

        # Scale
        unfoldedX = rTensor::k_unfold(X_cnt, 2)@data
        stds = apply(unfoldedX, 1, function(x){stats::sd(x, na.rm=TRUE)})
        unfoldedX_scl = sweep(unfoldedX, 1, stds, FUN="/")
        X_cnt_scl = rTensor::k_fold(unfoldedX_scl, m=2, modes=newModes)

        newDatasets[[p]] = X_cnt_scl@data
      }

      # Prepare data
      newZ = setupCMTFdata(newDatasets, modes, normalize=TRUE)

      # Each replicate is fitted with a single initialization and one core.
      model_fit = CMTFtoolbox::acmtf_opt(newZ,
                                         numComponents = currentComp,
                                         alpha         = alpha,
                                         beta          = beta,
                                         epsilon       = epsilon,
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
                              Z = newZ,
                              model = model_fit)
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

  cv_models = resultsList

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

  metrics = list("varExp" = varExps,
                 "FMS_random" = FMS_random_result,
                 "FMS_CV" = FMS_CV_result,
                 "degeneracyScore" = degeneracy_result)

  if(plots){
    # Make plots
    plots = list()

    # varExp
    df = varExps %>%
      as.data.frame() %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(numComponents = 1:maxNumComponents)

    colnames(df) = c(paste0("X", 1:numDatasets), "numComponents")

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

    plots$overview = ggpubr::ggarrange(plots$varExp, plots$FMS_random, plots$degeneracyScore, plots$FMS_CV, common.legend=TRUE)

    result = list("metrics"=metrics,
                  "plots"=plots)
  } else{
    result = list("metrics"=metrics)
  }

  return(result)
}

# Ugly solution to namespace issues caused by dplyr
degeneracy = NULL
