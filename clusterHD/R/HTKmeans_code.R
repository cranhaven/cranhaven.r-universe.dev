############################
## Main HTKmeans function ##
############################

HTKmeans <- function(X, k, lambdas = NULL,
                     standardize = TRUE,
                     iter.max = 100,
                     nstart = 100,
                     nlambdas = 50,
                     lambda_max = 1,
                     verbose = FALSE) {
  # Regularized k-means for a fixed value of k over a grid of lambda
  # values 
  #
  # args:
  #   X: a n x p data matrix
  #   lambdas: a (sequence of) value(s) for the regularization parameter lambda
  #   iter.max: the maximum number of iterations
  #   nstart: number of starts used by the regular k-means algorithm
  # 
  # returns:
  #   HTKmeans.out: a list with components:
  #   centers: the centers 
  #   cluster: the labels of the data points
  # 
  
  inputargs <- list(k = k,
                    lambdas = lambdas,
                    iter.max = iter.max,
                    nstart = nstart,
                    standardize = standardize,
                    X = X)
  if (standardize) {
    X <- scale(X)
  } else {
    X <- scale(X, center = TRUE, scale = FALSE)
  }
  
  HTKmeans.out <- list()
  lambdas      <- sort(lambdas)
  startval.out <- getStartvalues(X = X, k = k,
                                 iter.max = iter.max,
                                 nstart = nstart)
  
  if (is.null(lambdas)) { # automatic grid selection
    
    lambdas <- c(0, lambda_max)
    nbPar   <- rep(0, 2)
    
    centers <- list()
    objvals <- rep(0, length(startval.out))
    for (j in 1:length(startval.out)) {
      centers[[j]]  <- getCenters_cpp2(X = X,
                                       clusID = startval.out[[j]],
                                       k = k,
                                       lambda = lambdas[1])$centers
      objvals[j]    <- getObjective_cpp(X, centers[[j]],
                                        startval.out[[j]],
                                        lambdas[1])$obj
    }
    # initial centers and IDs:
    centers <- centers[[which.min(objvals)]]
    IDs     <- startval.out[[which.min(objvals)]]
    
    HTKmeans.out[[1]] <- HTKmeans_inner_cpp(X,
                                            centers,
                                            IDs,
                                            lambdas[1],
                                            iter.max)
    nbPar[1] <- sum(colSums(abs(HTKmeans.out[[1]]$centers)) > 0)
    
    centers <- list()
    objvals <- rep(0, length(startval.out))
    for (j in 1:length(startval.out)) {
      centers[[j]]  <- getCenters_cpp2(X = X,
                                       clusID = startval.out[[j]],
                                       k = k,
                                       lambda = lambdas[2])$centers
      objvals[j]    <- getObjective_cpp(X, centers[[j]],
                                        startval.out[[j]],
                                        lambdas[2])$obj
    }
    # initial centers and IDs:
    centers <- centers[[which.min(objvals)]]
    IDs     <- startval.out[[which.min(objvals)]]
    
    
    HTKmeans.out[[2]] <- HTKmeans_inner_cpp(X,
                                            centers,
                                            IDs,
                                            lambdas[2],
                                            iter.max)
    nbPar[2] <- sum(colSums(abs(HTKmeans.out[[2]]$centers)) > 0)
    
    
    ## start
    forbiddenSplits <- c()
    nusefulLambdas  <- 2
    usefulLambdas   <- c(1, 2)# indices of useful Lambdas in sorted lambda vector
    
    while (nusefulLambdas < nlambdas) {
      lambda.order <- order(lambdas)
      lambdas      <- lambdas[lambda.order]
      nbPar        <- nbPar[lambda.order]
      HTKmeans.out  <- HTKmeans.out[lambda.order]
      
      
      
      orderedDiffs <- order(diff(log(pmax(1, nbPar), 2)))
      low_idx      <- orderedDiffs[min(which(!orderedDiffs %in% forbiddenSplits))]
      lambda_low   <- lambdas[low_idx]
      lambda_high  <- lambdas[low_idx + 1]
      newLambda    <- (lambda_low + lambda_high) / 2
      
      
      
      centers <- list()
      objvals <- rep(0, length(startval.out))
      for (j in 1:length(startval.out)) {
        centers[[j]]  <- getCenters_cpp2(X = X,
                                         clusID = startval.out[[j]],
                                         k = k,
                                         lambda = newLambda)$centers
        objvals[j]    <- getObjective_cpp(X, centers[[j]],
                                          startval.out[[j]],
                                          newLambda)$obj
      }
      # initial centers and IDs:
      centers <- centers[[which.min(objvals)]]
      IDs     <- startval.out[[which.min(objvals)]]
      
      
      candidateOut <- HTKmeans_inner_cpp(X, centers,
                                         IDs,
                                         newLambda,
                                         iter.max)
      newNbPars <- sum(colSums(abs(candidateOut$centers)) > 0)
      
      
      if (newNbPars > nbPar[low_idx] | newNbPars < nbPar[low_idx + 1]) {
        # this is unusual, though not impossible.
        # So we recheck the solution by iterating the soltuions
        # left and right from the new lambda until convergence with
        # the new lambda parameter as regularization
        
        HTKmeans.out_down <- HTKmeans_inner_cpp(X, HTKmeans.out[[low_idx]]$centers,
                                                HTKmeans.out[[low_idx]]$cluster,
                                                newLambda,
                                                iter.max)
        
        HTKmeans.out_up <- HTKmeans_inner_cpp(X, HTKmeans.out[[low_idx + 1]]$centers,
                                              HTKmeans.out[[low_idx + 1]]$cluster,
                                              newLambda,
                                              iter.max)
        
        objVals <- c(getObjective_cpp(X, candidateOut$centers,
                                      candidateOut$cluster,
                                      newLambda)$obj,
                     getObjective_cpp(X, HTKmeans.out_down$centers,
                                      HTKmeans.out_down$cluster,
                                      newLambda)$obj,
                     getObjective_cpp(X, HTKmeans.out_up$centers,
                                      HTKmeans.out_up$cluster,
                                      newLambda)$obj)
        if (which.min(objVals) == 2) {
          candidateOut <- HTKmeans.out_down
        }
        if (which.min(objVals) == 3) {
          candidateOut <- HTKmeans.out_up
        }
      }
      
      
      newNbPars <- sum(colSums(abs(candidateOut$centers)) > 0)
      
      
      
      HTKmeans.out[[length(lambdas) + 1]] <- candidateOut
      nbPar[length(lambdas) + 1] <- sum(abs(candidateOut$centers) > 0)
      lambdas[length(lambdas) + 1] <- newLambda
      # print(paste0("newLambda = ", newLambda, " || nbPar = ", nbPar[length(lambdas)]  ))
      forbiddenSplits[which(forbiddenSplits > low_idx)] <- forbiddenSplits[which(forbiddenSplits > low_idx)] + 1
      usefulLambdas[which(usefulLambdas > low_idx)] <- usefulLambdas[which(usefulLambdas > low_idx)] + 1
      
      
      if ((newNbPars == nbPar[low_idx])) {
        forbiddenSplits <- c(forbiddenSplits, low_idx)
      } else if ((newNbPars == nbPar[low_idx + 1])) {
        forbiddenSplits <- c(forbiddenSplits, low_idx + 1)# don't split on new lambda
      } else {
        nusefulLambdas <- nusefulLambdas + 1
        usefulLambdas <- c(usefulLambdas, low_idx + 1)
        if (verbose) {
          print(nusefulLambdas / nlambdas)
        }
      }
      
      if (newLambda - lambda_low < 1e-3) {
        forbiddenSplits <- c(forbiddenSplits, low_idx)
      }
      if (lambda_high - newLambda < 1e-3) {
        forbiddenSplits <- c(forbiddenSplits, low_idx + 1)
      }
      if (length(unique(forbiddenSplits)) == (length(lambdas) - 1)) {
        break
      }
    }
    lambda.order <- order(lambdas)
    lambdas      <- lambdas[lambda.order]
    nbPar        <- nbPar[lambda.order]
    HTKmeans.out <- HTKmeans.out[lambda.order]
    
    usefulLambdas <- sort(usefulLambdas)
    lambdas       <- lambdas[usefulLambdas]
    nbPar         <- nbPar[usefulLambdas]
    HTKmeans.out  <- HTKmeans.out[usefulLambdas]
    ## stop
    
    
    
    
  } else {
    for (i in 1:length(lambdas)) {
      centers <- list()
      objvals <- rep(0, length(startval.out))
      for (j in 1:length(startval.out)) {
        centers[[j]]  <- getCenters_cpp2(X = X,
                                         clusID = startval.out[[j]],
                                         k = k,
                                         lambda = lambdas[i])$centers
        objvals[j]    <- getObjective_cpp(X, centers[[j]],
                                          startval.out[[j]],
                                          lambdas[i])$obj
      }
      # initial centers and IDs:
      centers <- centers[[which.min(objvals)]]
      IDs     <- startval.out[[which.min(objvals)]]
      
      
      HTKmeans.out[[i]] <- HTKmeans_inner_cpp(X,
                                              centers,
                                              IDs,
                                              lambdas[i],
                                              iter.max)
      if (max(abs(HTKmeans.out[[i]]$centers)) < 1e-10) {
        HTKmeans.out[[i]]$cluster <- rep(0, dim(X)[1])
        if (i < length(lambdas)) {
          for (j in (i + 1):length(lambdas)) {
            HTKmeans.out[[j]] <- HTKmeans.out[[i]]
          }
          break
        }
      }
    }
  }
  output <- list()
  output$HTKmeans.out <- HTKmeans.out
  output$lambdas   <- lambdas
  output$inputargs <- inputargs
  return(output)
}





####################################
## Support functions for HTKmeans ##
####################################

getIDs <- function(X, lambda, centers, oldIDs) {
  # calculate the new cluster assignments
  #
  
  n        <- dim(X)[1]
  IDs      <- rep(0, n)
  
  # assign new IDs
  for (i in 1:n) {
    x      <- X[i, ]
    cost   <- apply(centers, 1, function(y) sum((y - x)^2)) / n
    IDs[i] <- which.min(cost)
  }
  return(IDs)
}


getClassicCenters <- function(X, clusID, k) {
  # calculate classical centers
  #
  
  muhat <- sapply(1:k, FUN = function(y) colMeans(X[which(clusID == y),
                                                    , drop = FALSE]))
  return(t(muhat))
}

getCenters2 <- function(X, clusID, k,
                        lambda,
                        iter.max = 100) {
  # Updates the centers for the regularized k-means, assuming fixed
  # assignment of the observations to k clusters
  # args:
  #   X: n x p data matrix
  #   clusID: vector of length n with cluster memberships in 1, ..., k
  #   k: number of clusters
  #   lambda: penalization parameter
  # returns:
  #   centers: k x p matrix of updated cluster centers
  #
  
  n <- dim(X)[1]
  p <- dim(X)[2]
  centers <- matrix(0, k, p)
  
  centers_clas <- getClassicCenters(X = X, clusID = clusID, k = k)
  lambdaMat    <- matrix(lambda, k, p)
  
  for (j in 1:p) {
    muhat        <- centers_clas[, j]
    withinSS     <- sum(sapply(1:k, FUN = function(y)
      sum((X[which(clusID == y), j] - muhat[y])^2)))
    centers[, j] <- (sum(X[, j]^2)  > (withinSS + n * lambdaMat[1, j] )) * muhat
  }
  
  return(centers)
}


getStartvalues <- function(X, k, 
                           iter.max,
                           nstart) {
  # returns initial IDs
  #
  
  km.out   <- kmeans(X, centers = k, nstart = nstart)
  meansize <- apply(km.out$centers, 2, function(y) sum(y^2))
  
  pervars  <- c(1, 2, 5, 10, 25, 50, 100) / 100
  ordering <- order(meansize, decreasing = TRUE)
  IDs <- list()
  for (i in 1:length(pervars)) {
    Inds <- ordering[1:(pervars[i] * dim(X)[2])]
    if (length(Inds) > 0) {
      if (i == length(pervars)) {
        kmeans.out <- km.out
      } else {
        kmeans.out <- kmeans(X[, Inds], k, nstart = nstart)
      }
      IDs[[i]]      <- kmeans.out$cluster
    }
  }
  
  return(IDs)
}


getStartvalues2 <- function(X, k, lambda, 
                            iter.max,
                            nstart) {
  # returns initial centers
  #
  
  if (lambda == 0) {
    kmeans.out <- kmeans(X, k, nstart = nstart)
    centers    <- kmeans.out$centers
    IDs <- kmeans.out$cluster
  } else {
    km.out   <- kmeans(X, centers = k, nstart = nstart)
    meansize <- apply(km.out$centers, 2, function(y) sum(y^2))
    
    pervars  <- c(1, 2, 5, 10, 25, 50, 100) / 100
    objvals  <- rep(Inf, length(pervars))
    ordering <- order(meansize, decreasing = TRUE)
    centers  <- IDs <- list()
    for (i in 1:length(pervars)) {
      Inds <- ordering[1:(pervars[i] * dim(X)[2])]
      if (length(Inds) > 0) {
        if (i == length(pervars)) {
          kmeans.out <- km.out
        } else {
          kmeans.out <- kmeans(X[, Inds], k, nstart = nstart)
        }
        centers[[i]]  <- getCenters2(X = X, clusID = kmeans.out$cluster, k = k,
                                     lambda = lambda,
                                     iter.max = iter.max)
        IDs[[i]]      <- kmeans.out$cluster
        objvals[i]    <- getObjective_cpp(X, centers[[i]],
                                          kmeans.out$cluster,lambda)$obj
      }
    }
    centers <- centers[[which.min(objvals)]]
    IDs     <- IDs[[which.min(objvals)]]
  }
  
  return(list(centers = centers, IDs = IDs))
}



#############################################
## Functions for analyzing HTKmeans output ##
#############################################

diagPlot = function(HTKmeans.out, type = 1)  {
  # Make the diagnostic plot based on the output of HTKmeans
  #
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  if (type == 1) {
    lambdas <- HTKmeans.out$lambdas
    
    plotdata <- sapply(1:length(lambdas),
                       function(z)
                         colSums(abs(HTKmeans.out$HTKmeans.out[[z]]$centers)))
    xlims <- rev(range(lambdas))
    
    # visual adjustment
    plotdata <- cbind(plotdata[, 1], plotdata,
                      plotdata[, length(lambdas)])
    lambdas <- c(lambdas[1], lambdas[2] - 1e-10,
                 lambdas[2:(length(lambdas) - 1)],
                 lambdas[(length(lambdas) - 1)] + 1e-10,
                 lambdas[length(lambdas)]) 
    matplot(lambdas, t(plotdata), type = "l",
            xlab = expression(lambda), ylab = "norm of center vector",
            col = 1:ncol(plotdata), xlim = xlims, lwd = 4, lty = 1,
            cex.lab = 2, cex.axis = 2)
    
  } else {
    
    
    clusterInfo <- extractClusterInfo(HTKmeans.out,  summarize = TRUE)
    IDmat <- matrix(unlist(clusterInfo$IDs),
                    ncol = length(clusterInfo$IDs), byrow = FALSE)
    
    deltaARI <- rep(0, dim(IDmat)[2] - 1)
    for (i in 1:(dim(IDmat)[2] - 1)) {
      deltaARI[i] <-  1 - mclust::adjustedRandIndex(IDmat[, i+1], IDmat[,i])
    }
    
    par(mar=c(3.5, 5, 1.5, 6))
    
    plot(rev(clusterInfo$nbActiveVars)[-1],
         diff(rev(clusterInfo$WCSS_active)) /
           diff(rev(clusterInfo$nbActiveVars)),
         ylab = "",
         xlab = "",
         type = "l", lwd = 3,axes = FALSE,
         ylim = c(0, 1))
    axis(2, ylim=c(0,1),col="black",las=1, lwd = 3,
         lwd.ticks = 2, cex.axis = 2)  ## las=1 makes horizontal labels
    mtext(expression(paste(Delta," WCSS")), side=2, line=3.5, cex = 2)
    
    
    ## Allow a second plot on the same graph
    par(new=TRUE)
    
    # plot delta ARI
    plot(clusterInfo$nbActiveVars[-c(length(deltaARI), length(deltaARI)-1)],
         deltaARI[-c(length(deltaARI))],
         xlab="", ylab="", ylim = c(0, 1),
         axes=FALSE, type="l", xlim = c(1,max(clusterInfo$nbActiveVars)),
         col="firebrick", lwd = 3)
    mtext(expression(paste(Delta," ARI")),side=4,col="firebrick",line=4, cex = 2) 
    axis(4, ylim=c(0, 1), col="firebrick",col.axis="firebrick",
         las=1, cex.axis = 2, 
         lwd = 3, lwd.ticks = 2, at = seq(0,1, by = 0.2),
         labels = seq(0,1, by = 0.2))
    
    ## Draw the x axis
    axis(1,seq(max(clusterInfo$nbActiveVars), 1, -1),lwd = 3,
         lwd.ticks = 2, cex.axis = 2)
    mtext("Number of active variables",
          side=1,col="black",line=2.5, cex = 2)  
  }
}

extractClusterInfo <- function(HTKmeans.out,
                               y = NULL,
                               summarize = FALSE) {
  # Extract and organize cluster info from the output of HTKmeans 
  # can be used to make diagnostic plots
  #
  
  lambdas    <- HTKmeans.out$lambdas
  objective  <- penalty <- WCSS <- WCSS_nonZero <- nbActiveVars <- ARIs <- rep(0, length(lambdas))
  activeVars <- list()
  centers    <- list()
  IDs        <- list()
  
  if (HTKmeans.out$inputargs$standardize) {
    Z <- scale(HTKmeans.out$inputargs$X)
  } else {
    Z <- scale(HTKmeans.out$inputargs$X, scale = FALSE)
  }
  for (i in 1:length(lambdas)) {
    temp <- getObjective_cpp(Z, HTKmeans.out$HTKmeans.out[[i]]$centers,
                             HTKmeans.out$HTKmeans.out[[i]]$cluster, lambdas[i])
    WCSS[i]         <- temp$obj_WCSS
    objective[i]    <- temp$obj
    penalty[i]      <- temp$obj_penalty
    WCSS_nonZero[i] <- temp$WCSS_nonZero
    nbActiveVars[i] <- temp$nbActive
    activeVars[[i]] <- temp$activeVars
    IDs[[i]]        <- HTKmeans.out$HTKmeans.out[[i]]$cluster
    centers[[i]]    <- HTKmeans.out$HTKmeans.out[[i]]$centers
    if (!is.null(y)) {
      ARIs[i] <- mclust::adjustedRandIndex(HTKmeans.out$HTKmeans.out[[i]]$cluster, y)
    }
  }
  
  if (summarize) {
    uniqNbVars <- sort(unique(nbActiveVars), decreasing = TRUE)
    objective_sum <- penalty_sum <- WCSS_sum  <- WCSS_nonZero_sum <- nbActiveVars_sum <- ARIs_sum <- rep(0, length(uniqNbVars))
    activeVars_sum <- centers_sum <- IDs_sum <- list()
    lambdas_sum <- rep(0, length(uniqNbVars))
    for (j in 1:length(uniqNbVars)) {
      simInds <- which(nbActiveVars == uniqNbVars[j])
      
      best.one            <- simInds[which.min(objective[simInds])]
      WCSS_sum[j]         <- WCSS[best.one]
      WCSS_nonZero_sum[j] <- WCSS_nonZero[best.one]
      objective_sum[j]    <- objective[best.one]
      penalty_sum[j]      <- penalty[best.one]
      nbActiveVars_sum[j] <- nbActiveVars[best.one]
      activeVars_sum[[j]] <- activeVars[[best.one]]
      centers_sum[[j]]    <- centers[[best.one]]
      lambdas_sum[j]      <- lambdas[best.one]
      IDs_sum[[j]]        <- IDs[[best.one]]
      if (!is.null(y)) {
        ARIs_sum[j] <- ARIs[best.one]
      }
    }
    
    objective    <- objective_sum
    penalty      <- penalty_sum
    WCSS         <- WCSS_sum
    WCSS_nonZero <- WCSS_nonZero_sum
    nbActiveVars <- nbActiveVars_sum
    activeVars   <- activeVars_sum
    ARIs         <- ARIs_sum
    lambdas      <- lambdas_sum
    centers      <- centers_sum
    IDs          <- IDs_sum
  }
  
  
  return(list(objective = objective,
              penalty = penalty,
              WCSS = WCSS,
              WCSS_active = WCSS_nonZero, 
              nbActiveVars = nbActiveVars,
              activeVars = activeVars,
              ARIs = ARIs,
              lambdas = lambdas,
              centers = centers,
              IDs = IDs))
}

##################################################
## Functions for selecting the tuning parameter ##
##################################################

getLambda <- function(HTKmeans.out, type  = "AIC") {
  # Select the value of the regularization parameter lambda
  # based on the output of HTKmeans with hard thresholding
  #
  
  clusterInfo <- extractClusterInfo(HTKmeans.out,
                                    y = NULL,
                                    summarize = TRUE)
  k       <- HTKmeans.out$inputargs$k
  lambdas <- clusterInfo$lambdas
  n       <- nrow(HTKmeans.out$inputargs$X)
  if (type == "AIC") {
    AICvals <- clusterInfo$WCSS * n +
      2 * k * clusterInfo$nbActiveVars
    lambda <- lambdas[which.min(AICvals)]
  } else if (type == "BIC") {
    BICvals <- clusterInfo$WCSS * n +
      k * log(n) * clusterInfo$nbActiveVars
    lambda <- lambdas[which.min(BICvals)]
  }
  
  return(lambda)
}

