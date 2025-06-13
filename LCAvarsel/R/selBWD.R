selBWD <- function(X, G, ctrlLCA = controlLCA(), ctrlReg = controlReg(),
                   independence = FALSE, swap = FALSE, start = NULL, bicDiff = 0,
                   covariates = NULL, parallel = FALSE, checkG = TRUE, 
                   verbose = interactive())
{
  i <- NULL # avoid 'no visible binding for global variable'
  varnames <- colnames(X)
  N <- nrow(X)
  M <- length(varnames)
  
  # select clustering variables, thus G > 1
  G <- setdiff(G, 1)
  
  # store information during search and better printing
  long <- varnames[which.max(nchar(varnames))]
  info <- as.data.frame(
    list(Iter = 111, Step = "Remove",
         Var = if ( swap ) paste(long, "-", long) else long,
         BICdiff = -1111.11, Decision = "Rejected"), stringsAsFactors = FALSE )
  colnames(info) <- c("", "Step", " Variable", " BICdiff", " Decision")
  
  # parallel and do operator
  parallel <- if( is.logical(parallel) ) {
    if ( parallel ) GA::startParallel(parallel) else FALSE
  } else { GA::startParallel(parallel) }
  on.exit( if(parallel) parallel::stopCluster(attr(parallel, "cluster")) )
  `%DO%` <- if ( parallel ) foreach::`%dopar%` else foreach::`%do%`

  # initialize clustering variables
  clus <- if ( is.null(start) ) varnames else start
  noclus <- NULL

  # model on clustering variables
  GG <- if ( checkG ) suppressWarnings( maxG(X[,clus], G) ) else G
  clusMod <- fitLCA(X[,clus], GG, X = covariates, ctrlLCA = ctrlLCA)
  bicClusMod <- clusMod$bic
  if ( verbose ) cat(" Starting clustering set: \n",
                     if ( is.null(start) ) "all variables" else clus, "\n", "\n")

  # variable selection------------------------------------------------------
  crit <- TRUE
  toRem <- toAdd <- NULL
  # zero <- (.Machine$double.eps)^(1/3)
  zero <- bicDiff
  first <- is.null(start)    # two removal steps when first iteration and starting with all variables
  iter <- 0

  while ( crit ) {
    # removal step...................................................
    #
    # do not remove the variable just added
    Clus <- if ( !is.null(toAdd) ) {
      if ( is.na(match(toAdd, clus)) ) clus else clus[ -match(toAdd, clus) ]
    } else  clus
    out <- foreach::foreach( i = Clus, .combine = "rbind", .errorhandling = "remove",
                             .final = function(o) matrix(o, ncol = 2) ) %DO% {
                               j <- match(i, Clus)
                               set <- Clus[-j]                     # clustering set after removing variable j
                               bicReg <- regressionStep( y = X[,i], X = X[,set], ctrlReg = ctrlReg,
                                                         independence = independence )
                               GG <- if ( checkG ) suppressWarnings( maxG(X[,set], G) ) else G
                               modNoClus <- if ( length(GG) > 0 )
                                 fitLCA( X[,set], G = GG, X = covariates, ctrlLCA = ctrlLCA )$bic else NA
                               return( c(bicReg, modNoClus) )
                             }
    iter <- iter + 1
    rownames(out) <- Clus
    bicRem <- rowSums(out)

    if ( any( !is.na(bicRem) ) ) {
      # variable proposed for removal
      best <- which.max(bicRem)
      Max <- max(bicRem, na.rm = TRUE)
      toRem <- names(bicRem)[best]
      bicRemDiff <- Max - bicClusMod
      
      # remove
      info[iter+1,1] <- iter
      info[iter+1,2] <- "Remove"
      info[iter+1,3] <- toRem
      info[iter+1,4] <- bicRemDiff
      if ( bicRemDiff > zero ) {
        info[iter+1,5] <- "Accepted"
        removed <- TRUE
        j <- match(toRem, clus)
        clus <- clus[-j]
        noclus <- c(noclus, toRem)
        bicClusMod <- out[best,2]
        if ( verbose ) monitor(info, iter)
      } else {
        info[iter+1,5] <- "Rejected"
        removed <- FALSE
        toRem <- NULL
        if ( verbose ) monitor(info, iter)
        if ( length(clus) == M ) break      # all variables are selected
      }
    } else {
      info[iter+1,1] <- iter
      info[iter+1,2] <- "Remove"
      info[iter+1,3] <- NA
      info[iter+1,4] <- NA
      removed <- FALSE
      toRem <- NULL
      if ( verbose ) monitor(info, iter)
    }

    # two removal steps when first iteration and starting with all variables
    if ( first & is.null(start) ) {
      first <- FALSE
      next
    }
    #................................................................

    # swap step......................................................
    if ( swap ) {
      iter <- iter + 1
      srt <- sort(bicRem, decreasing = TRUE)
      # if ( length(srt) > 1 ) {
      if ( length(srt) > 1 | (length(srt) >= 1 & !removed) ) {
        # which variable in the clustering set do we swap with all the ones in the non-clustering set?
        if ( removed ) {
          # the variable to swap is the second with the largest evidence
          toSwap <- names(srt[2])
        } else {
          # or the first if none has removed
          toSwap <- names(srt[1])
        }
        indSwap <- match(toSwap, clus)

        # swap variable 'toSwap' in the clustering set with one of the non-clustering set
        out <- foreach::foreach( i = noclus, .combine = "rbind", .errorhandling = "remove",
                                 .final = function(o) matrix(o, ncol = 3) ) %DO% {
                                   # clustering set changing 'toSwap' with one of the non-clustering set
                                   set <- c( clus[-indSwap], i )
                                   bicReg <- regressionStep( y = X[,i], X = X[,clus], ctrlReg = ctrlReg,
                                                             independence = independence )
                                   bicRegSwap <- regressionStep( y = X[,toSwap], X = X[,set], ctrlReg = ctrlReg )
                                   GG <- if ( checkG ) suppressWarnings( maxG(X[,set], G) ) else G
                                   modNoClus <- if ( length(GG) > 0 )
                                     fitLCA( X[,set], G = GG, X = covariates, ctrlLCA = ctrlLCA )$bic else NA
                                   return( c(bicReg, bicRegSwap, modNoClus) )
                                 }
        rownames(out) <- noclus
        bicSwapClus <- bicClusMod + out[,1]         # bic with 'toSwap' in the clustering set
        bicSwapNoClus <- out[,3] + out[,2]          # bic with 'toSwap' in the non clustering set
        bicSwapDiff <- bicSwapNoClus - bicSwapClus

        if ( any( !is.na(bicSwapDiff) ) ) {
          # variable in 'noclus' proposed for swapping with 'toSwap'
          best <- which.max(bicSwapDiff)
          Max <- max(bicSwapDiff, na.rm = TRUE)
          toSwapIn <- names(bicSwapDiff)[best]
          
          # swap
          info[iter+1,1] <- iter
          info[iter+1,2] <- "Swap"
          info[iter+1,3] <- paste0(toSwap, "-", toSwapIn)
          info[iter+1,4] <- Max
          if ( Max > zero ) {
            info[iter+1,5] <- "Accepted"
            swapped <- TRUE
            Max <- max(bicSwapDiff, na.rm = TRUE)
            bicClusMod <- out[best,3]
            j <- match(toSwap, clus)
            clus <- c( clus[-j], toSwapIn )
            noclus <- c(noclus[-best], toSwap)
            if ( verbose ) monitor(info, iter)
          } else {
            info[iter+1,5] <- "Rejected"
            swapped <- FALSE
            if ( verbose ) monitor(info, iter)
          }
        } else {
          info[iter+1,1] <- iter
          info[iter+1,2] <- "Swap"
          info[iter+1,3] <- NA
          info[iter+1,4] <- NA
          swapped <- FALSE
          if ( verbose ) monitor(info, iter)
        }
      } else {
        info[iter+1,1] <- iter
        info[iter+1,2] <- "Swap"
        info[iter+1,3] <- NA
        info[iter+1,4] <- NA
        swapped <- FALSE
        if ( verbose ) monitor(info, iter)
      }
    } else swapped <- FALSE
    #................................................................

    # add step.......................................................
    #
    # do not add the variable just removed
    Noclus <- if ( !is.null(toRem) ) {
      if ( is.na(match(toRem, noclus)) ) noclus else noclus[ -match(toRem, noclus) ]
    } else noclus
    #
    if ( length(Noclus) != 0 ) {            # there are no variables to add
      out <- foreach::foreach( i = Noclus, .combine = "rbind", .errorhandling = "remove",
                               .final = function(o) matrix(o, ncol = 2) ) %DO% {
                                 set <- c(clus, i)               # clustering set after adding variable j
                                 bicReg <- regressionStep( y = X[,i], X = X[,clus], ctrlReg = ctrlReg,
                                                           independence = independence )
                                 GG <- if ( checkG ) suppressWarnings( maxG(X[,set], G) ) else G
                                 modClus <- if ( length(GG) > 0 )
                                   fitLCA( X[,set], G = GG, X = covariates, ctrlLCA = ctrlLCA )$bic else NA
                                 return( c(bicReg, modClus) )
                               }
      iter <- iter + 1
      rownames(out) <- Noclus
      bicNoClus <- bicClusMod + out[,1]
      bicAdd <- out[,2]
      bicAddDiff <- bicAdd - bicNoClus
      
      if ( any( !is.na(bicAddDiff) ) ) {
        # variable proposed for adding
        best <- which.max(bicAddDiff)
        Max <- max(bicAddDiff, na.rm = TRUE)
        toAdd <- names(bicAddDiff)[best]
        
        # add
        info[iter+1,1] <- iter
        info[iter+1,2] <- "Add"
        info[iter+1,3] <- toAdd
        info[iter+1,4] <- Max
        if ( bicAddDiff[best] > zero ) {
          info[iter+1,5] <- "Accepted"
          added <- TRUE
          bicClusMod <- bicAdd[best]
          j <- match(toAdd, noclus)
          clus <- c(clus, toAdd)
          noclus <- noclus[-j]
          if ( verbose ) monitor(info, iter)
        } else {
          info[iter+1,5] <- "Rejected"
          added <- FALSE
          toAdd <- NULL
          if ( verbose ) monitor(info, iter)
        }
        #
      } else {
        info[iter+1,1] <- iter
        info[iter+1,2] <- "Add"
        info[iter+1,3] <- NA
        info[iter+1,4] <- NA
        added <- FALSE
        toAdd <- NULL
        if ( verbose ) monitor(info, iter)
      }
    } else { # if ( length(Noclus) != 0 )
      added <- FALSE
      toAdd <- NULL
    }
    #................................................................

    # swap step......................................................
    if ( swap ) {
      iter <- iter + 1
      srt <- sort(bicAddDiff, decreasing = TRUE)
      # if ( length(srt) > 1 ) {
      if ( length(srt) > 1 | (length(srt) >= 1 & !added) ) {
        if ( length(noclus) != 0 ) {
          # which variable in the non-clustering set do we swap with all the ones in the clustering set?
          if ( added ) {
            # the variable to swap is the second with the largest evidence
            toSwap <- names(srt[2])
          } else {
            # or the first if none has removed
            toSwap <- names(srt[1])
          }
          indSwap <- match(toSwap, noclus)

          # swap variable 'toSwap' in the non-clustering set with one of the clustering set
          bicReg <- regressionStep( y = X[,toSwap], X = X[,clus], ctrlReg = ctrlReg )
          out <- foreach::foreach( i = clus, .combine = "rbind", .errorhandling = "remove",
                                   .final = function(o) matrix(o, ncol = 2) ) %DO% {
                                     j <- match(i, clus)
                                     # clustering set changing 'toSwap' with one of the clustering set
                                     set <- c( toSwap, clus[-j] )
                                     bicRegSwap <- regressionStep( y = X[,i], X = X[,set], ctrlReg = ctrlReg,
                                                                   independence = independence )
                                     GG <- if ( checkG ) suppressWarnings( maxG(X[,set], G) ) else G
                                     modClus <- if ( length(GG) > 0 )
                                       fitLCA( X[,set], G = GG, X = covariates, ctrlLCA = ctrlLCA )$bic else NA
                                     return( c(bicRegSwap, modClus) )
                                   }
          rownames(out) <- clus
          bicSwapClus <- out[,2] + out[,1]            # bic with 'toSwap' in the clustering set
          bicSwapNoClus <- bicClusMod + bicReg        # bic with 'toSwap' in the non clustering set
          bicSwapDiff <- bicSwapClus - bicSwapNoClus
          
          if ( any( !is.na(bicSwapDiff) ) ) {
            # variable in 'clus' proposed for swapping with 'toSwap'
            best <- which.max(bicSwapDiff)
            Max <- max(bicSwapDiff, na.rm = TRUE)
            toSwapOut <- names(bicSwapDiff)[best]
            
            # swap
            info[iter+1,1] <- iter
            info[iter+1,2] <- "Swap"
            info[iter+1,3] <- paste0(toSwapOut, "-", toSwap)
            info[iter+1,4] <- Max
            if ( Max > zero ) {
              info[iter+1,5] <- "Accepted"
              swapped <- TRUE
              bicClusMod <- out[best,2]
              j <- match(toSwapOut, clus)
              clus <- c( clus[-j], toSwap )
              noclus <- c(noclus[-indSwap], toSwapOut)
              if ( verbose ) monitor(info, iter)
            } else {
              info[iter+1,5] <- "Rejected"
              swapped <- FALSE
              if ( verbose ) monitor(info, iter)
            }
          } else {
            info[iter+1,1] <- iter
            info[iter+1,2] <- "Swap"
            info[iter+1,3] <- NA
            info[iter+1,4] <- NA
            swapped <- FALSE
            if ( verbose ) monitor(info, iter)
          }
        } else swapped <- FALSE
      } else {
        info[iter+1,1] <- iter
        info[iter+1,2] <- "Swap"
        info[iter+1,3] <- NA
        info[iter+1,4] <- NA
        swapped <- FALSE
        if ( verbose ) monitor(info, iter)
      }
    } else swapped <- FALSE
    #................................................................

    # check
    crit <- ( (removed | added | swapped) & (length(clus) >= 3) )
    # we need at least 3 binary variables to identify a 2-class LCA model
  }
  #-------------------------------------------------------------------------

  # stop parallel computing
  # if ( parallel ) parallel::stopCluster( attr(parallel, "cluster") )

  # fit LCA model on selected variables
  clus <- varnames[ sort(match(clus, varnames)) ]
  GG <- if ( checkG ) suppressWarnings( maxG(X[,clus], G) ) else G
  mod <- fitLCA( X[,clus], G = GG, X = covariates, ctrlLCA = ctrlLCA )

  rownames(info) <- info[,1]
  info <- info[,-1]
  return( list(variables = clus, model = mod, info = info[-1,], 
               search = "backward", swap = swap, independence = independence) )
}

