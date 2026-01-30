#' Fits Gaussian Mixture model and computes the KSD value for the model
#'
#' @description We fit a Gaussian Mixture Model for a given dataset (Fisher's Iris),
#' and we compute the KSD P-value on the hold-out test dataset. User may tune
#' the parameters and observe the change in results. Reports average of p-values obtained during each k-fold.
#' It also plots the contour for each k-fold iteration if only 2 dimensions of data are used.
#' If a vector is specified for nClust, the code tries each element as the number of clusters
#' and reports the optimal parameter by choosing one with highest p-value.
#'
#' @import stats graphics datasets
#' @param cols : Columns of iris data set to use. If 2 dimensions, plots the contour for each k-fold.
#' @param nClust : Number of clusters want to estimate with
#'                If vector, use each element as number of clusters and reports the optimal number.
#' @param kfold : Number of k to use for k-fold
#'
#'
#'
#' @export
demo_iris <- function(cols = c(1,2), nClust=3, kfold=5){

      if (!requireNamespace("datasets", quietly = TRUE)) {
            stop("datasets needed for this demo to work. Please install it.",
                 call. = FALSE)
      }
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop("ggplot2 needed for this demo to work. Please install it.",
                 call. = FALSE)
      }
      if (!requireNamespace("gridExtra", quietly = TRUE)) {
            stop("gridExtra needed for this demo to work. Please install it.",
                 call. = FALSE)
      }
      if (!requireNamespace("mclust", quietly = TRUE)) {
            stop("mclust needed for this demo to work. Please install it.",
                 call. = FALSE)
      }
      if (!requireNamespace("pryr", quietly = TRUE)) {
            stop("pryr needed for this demo to work. Please install it.",
                 call. = FALSE)
      }

      set.seed(0)

      pVals <- rep(0,kfold)
      indexing <- 1:nrow(datasets::iris)
      kfoldGroups <- split(indexing, cut(indexing,kfold))
      irisSet <- datasets::iris[sample(nrow(datasets::iris)),cols]
      factorNames <- colnames(irisSet)
      d <- length(cols)


      # Only if two dimensional
      if(d==2){
            minX = min(irisSet[,1]); maxX = max(irisSet[,1])
            minY = min(irisSet[,2]); maxY = max(irisSet[,2])
            gridx <- seq(minX,maxX, length.out=50)
            gridy <- seq(minY,maxY, length.out=50)
            expandGrid = expand.grid(gridx,gridy)
            grids <- data.frame(gridx=expandGrid[,1],gridy=expandGrid[,2])
      }

      maxp = 0; maxClust = 0;
      max_plot_list = list()
      avgpVals = c()

      for (k in nClust){
            print(sprintf('Fitting GMM with %d clusters', k))
            plot_list = list()

            for (i in 1:kfold){
                  # Divide the dataset
                  testIndex <- kfoldGroups[[i]]
                  trainIndex <- setdiff(indexing, testIndex)
                  trainSet <- irisSet[trainIndex,]
                  testSet <- irisSet[testIndex,]

                  # Get the estimated parameters
                  clustModel <- mclust::Mclust(trainSet,G=k)
                  clustMu <- clustModel$parameters$mean
                  pro <- clustModel$parameters$pro
                  pro <- sapply(pro,custround)
                  #print(clustMu)
                  clustSigma <- clustModel$parameters$variance$sigma
                  clustWeights <-  clustModel$parameters$pro

                  # Compute the KSD Value
                  model <- gmm(nComp = k, mu = clustMu, sigma=clustSigma, weights = clustWeights, d = d)
                  score_function = pryr::partial(scorefunctiongmm, model=model)
                  result <- KSD(data.matrix(testSet),score_function=score_function, 'rbf',-1.0)
                  pVals[i] = result$p

                  # Plot the result only if we choose two factors
                  if(d==2){
                        z <- likelihoodgmm(model = model, X = expandGrid)
                        grids$z <- as.numeric(z)
                        pointsx = c(trainSet[,1],testSet[,1],clustMu[1,])
                        pointsy = c(trainSet[,2],testSet[,2],clustMu[2,])
                        trainSize = dim(trainSet)[1]; testSize = dim(testSet)[1];
                        type <- c(rep('Train',trainSize),rep('Test',testSize),rep('Mu',k))
                        points <- data.frame(pointsx, pointsy, type)

                        par(mar=c(4,4,4,1))
                        v <- ggplot2::ggplot(data=grids, ggplot2::aes(gridx, gridy)) +
                              ggplot2::ggtitle(paste('Fold ',i, ' ( pvalue = ',pVals[i],')')) +
                              ggplot2::labs(x=factorNames[1],y=factorNames[2])
                        v <- v + ggplot2::stat_contour(ggplot2::aes(z=z))
                        v <- v + ggplot2::geom_point(data=points, ggplot2::aes(x=pointsx,y=pointsy,shape=factor(type),color=factor(type)),size=2)+
                              ggplot2::annotate("text", x=clustMu[1,], y=clustMu[2,], label= pro)
                        legend <- get_legend(v)
                        v <- v + ggplot2::theme(legend.position="none")
                        plot_list[[i]] = v
                  }
            }
            print(sprintf('Average p value : %.3f', mean(pVals)))
            if(d==2)
                  plot_list[[kfold+1]] = legend
            avgpVals <- c(avgpVals, mean(pVals))

            if(mean(pVals) > maxp ){
                  maxp <- mean(pVals)
                  max_plot_list = plot_list
                  maxClust <-  k
            }

      }

      # Only if we have more than one option
      if(length(nClust) > 1){
            pFrame <- data.frame(nClust=nClust,avgpVals = avgpVals)
            pValGraph <- ggplot2::ggplot(pFrame, ggplot2::aes(nClust, avgpVals)) +
                  ggplot2::ggtitle('P-value vs Number of clusters') +
                  ggplot2::labs(x='Number of clusters',y='p-value')
            pValGraph <- pValGraph + ggplot2::geom_line() + ggplot2::scale_x_continuous(breaks=seq(min(nClust),max(nClust),1))

            if(d==2)
                  max_plot_list[[kfold+2]] <-  pValGraph
            else
                  max_plot_list[[1]] <- pValGraph
            print(sprintf('Optimal number of clusters : %d',maxClust))
            print(sprintf('Average P-value :  %.3f',maxp))
      }

      if(d==2 || length(nClust) > 1)
            do.call(gridExtra::grid.arrange,max_plot_list)


}
