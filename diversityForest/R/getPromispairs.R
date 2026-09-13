# -------------------------------------------------------------------------------
#   This file is part of 'diversityForest'.
#
# 'diversityForest' is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# 'diversityForest' is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with 'diversityForest'. If not, see <http://www.gnu.org/licenses/>.
#
#  NOTE: 'diversityForest' is a fork of the popular R package 'ranger', written by Marvin N. Wright.
#  Most R and C++ code is identical with that of 'ranger'. The package 'diversityForest'
#  was written by taking the original 'ranger' code and making any
#  changes necessary to implement diversity forests.
#
# -------------------------------------------------------------------------------

# Function to pre-select the pairs of promising variables:

getPromispairs <- function(X, y, npreselpairs = 5000) {
  
  # Number of variables:
  p <- ncol(X)
  
  # If there are fewer than 100 variables, no pre-selection
  # is performed:
  if(choose(p, 2) <= npreselpairs) {  ### p <= 100
    promispairs <- t(combn(1:p, 2))
  }
  else {
    
    # If 100 < p < 448, all pairs are tested for interaction effect
    # and the npreselpairs pairs with smallest p-values are selected:
    if(p < 448) {
      
      # If there are more than 500 observations in the data,
      # they are subset to contain only 500 observations to
	  # make the computations easier:
      nsubsample <- 500
      if(nrow(X) > nsubsample) {
        if(length(unique(y))==2) {
          classsizes <- table(y)
          nclasssmall <- min(classsizes)
          classes <- as.numeric(names(classsizes))
          smallclass <- classes[which.min(classsizes)]
          largeclass <- classes[setdiff(c(1,2), smallclass)]
          
          if(nclasssmall <= 30) {
            subsetind <- c(which(y==smallclass), sample(which(y==largeclass), size=nsubsample-sum(y==smallclass)))
          } else {
            subsetind <- sample(1:nrow(X), size=nsubsample)
            if(sum(y[subsetind]==smallclass) < 30) {
              subsetind <- c(sample(which(y==smallclass), size=30), sample(which(y==largeclass), size=nsubsample-30))
            }
          }
        } else {
          subsetind <- sample(1:nrow(X), size=nsubsample)
        }
        
        X <- X[subsetind,]
        y <- y[subsetind]
      }
      
      pairindmat <- t(combn(1:p, 2))
      
      pval <- 0
      
      for(i in 1:nrow(pairindmat)) {
        x1 <- X[,pairindmat[i,1]]
        x2 <- X[,pairindmat[i,2]]
        
        mod <- RcppEigen::fastLmPure(cbind(1, x1, x2, x1*x2), as.numeric(y)-1)
        pval[i] <- 2*(1-pnorm(abs(mod$coefficients[4]), sd=mod$se[4]))
      }
      
      orderind <- order(pval)[1:npreselpairs]
      promispairs <- pairindmat[orderind,]
      
    }
    else {
      
	  # If p >= 448 we need to employ the BOLT-SSI procedure.
	  # Because this procedure is computationally expensive,
	  # we subset the data to contain only 200 observations:
	  
      Xsafe <- X; ysafe <- y
      nsubsample <- 200
      if(nrow(X) > nsubsample) {
        if(length(unique(y))==2) {
          classsizes <- table(y)
          nclasssmall <- min(classsizes)
          classes <- as.numeric(names(classsizes))
          smallclass <- classes[which.min(classsizes)]
          largeclass <- classes[setdiff(c(1,2), smallclass)]
          
          if(nclasssmall <= 30) {
            subsetind <- c(which(y==smallclass), sample(which(y==largeclass), size=nsubsample-sum(y==smallclass)))
          } else {
            subsetind <- sample(1:nrow(X), size=nsubsample)
            if(sum(y[subsetind]==smallclass) < 30) {
              subsetind <- c(sample(which(y==smallclass), size=30), sample(which(y==largeclass), size=nsubsample-30))
            }
          }
        } else {
          subsetind <- sample(1:nrow(X), size=nsubsample)
        }
        
        X <- X[subsetind,]
        y <- y[subsetind]
      }
      
      # If 448 <= p <= npreselpairs, a combination of the BOLT-SSI procedure and
      # pre-selection using testing is applied:
	  
      if(p <= npreselpairs) {
        
        # First apply BOLT-SSI to the whole data set:
        model <- BOLTSSIRR::BOLT_SSI(X, y-1, extra_pairs = npreselpairs)
        
        foundpairs <- model[,1:2]
        foundpairschar <- apply(foundpairs, 1, paste, collapse="_")
        
        # --> This results in p pre-selected pairs, because
        # BOLT-SSI cannot select more pairs than max{n,p}.
        
        # Apply BOLT-SSI 20 times to subsets of
        # the variables in order to identify further
        # relevant pairs:
        
        count <- 1
        while(nrow(foundpairs) < npreselpairs & count < 20) {
          
          # Random selection of variables:
          inds <- sort(sample(1:p, size=floor((1/3)*p)))
          
          # Apply BOLT-SSI:
          model <- BOLTSSIRR::BOLT_SSI(X[,inds], y-1, extra_pairs = npreselpairs)
          
          foundpairstemp <- model[,1:2]
          foundpairstemp[,1] <- inds[foundpairstemp[,1]]
          foundpairstemp[,2] <- inds[foundpairstemp[,2]]
          
          foundpairschartemp <- apply(foundpairstemp, 1, paste, collapse="_")
          
          # Add newly identified promising pairs to the collection
          # of all identified pairs:
          newbool <- !(foundpairschartemp %in% foundpairschar)
          
          foundpairs <- rbind(foundpairs, foundpairstemp[newbool,])
          foundpairschar <- c(foundpairschar, foundpairschartemp[newbool])
          
          count <- count + 1
        }
        

      }
      else {
        
        if (p <= 30000) {
          
          # If npreselpairs < p <= 30000, we apply BOLT-SSI once to identify the npreselpairs most
          # promising feature pairs:
          
          model <- BOLTSSIRR::BOLT_SSI(X,y-1, extra_pairs = npreselpairs)
          foundpairs <- model[,1:2]
          
        } else {
          
		  # If p > 30000, BOLT-SSI becomes computationally too expensive, 
		  # which is why before applying BOLT-SSI we preselect
		  # the 30000 features with smalles p-values in univariate
		  # tests:
		  
          # If there are more than 500 observations in the data,
          # they are subset to contain only 500 observations:
		  
          X <- Xsafe; y <- ysafe
          nsubsample <- 500
          if(nrow(X) > nsubsample) {
            if(length(unique(y))==2) {
              classsizes <- table(y)
              nclasssmall <- min(classsizes)
              classes <- as.numeric(names(classsizes))
              smallclass <- classes[which.min(classsizes)]
              largeclass <- classes[setdiff(c(1,2), smallclass)]
              
              if(nclasssmall <= 30) {
                subsetind <- c(which(y==smallclass), sample(which(y==largeclass), size=nsubsample-sum(y==smallclass)))
              } else {
                subsetind <- sample(1:nrow(X), size=nsubsample)
                if(sum(y[subsetind]==smallclass) < 30) {
                  subsetind <- c(sample(which(y==smallclass), size=30), sample(which(y==largeclass), size=nsubsample-30))
                }
              }
            } else {
              subsetind <- sample(1:nrow(X), size=nsubsample)
            }
            
            X <- X[subsetind,]
            y <- y[subsetind]
          }
          
          
          # Test each feature for effect using simple linear regression
		  # and subset the 30000 variables with smallest p-values:
          
          pval <- 0
          
          for(i in 1:ncol(X)) {
            mod <- RcppEigen::fastLmPure(cbind(1, X[,i]), as.numeric(y)-1)
            pval[i] <- 2*(1-pnorm(abs(mod$coefficients[2]), sd=mod$se[2]))
          }
          
          varsubset <- sort(order(pval)[1:30000])
          


          # Before applying BOLT-SSI, we again have to 
		  # randomly subset to 200 observations, because
		  # BOLT-SSI is computationally expensive:
		  
          X <- Xsafe; y <- ysafe
          nsubsample <- 200
          if(nrow(X) > nsubsample) {
            if(length(unique(y))==2) {
              classsizes <- table(y)
              nclasssmall <- min(classsizes)
              classes <- as.numeric(names(classsizes))
              smallclass <- classes[which.min(classsizes)]
              largeclass <- classes[setdiff(c(1,2), smallclass)]
              
              if(nclasssmall <= 30) {
                subsetind <- c(which(y==smallclass), sample(which(y==largeclass), size=nsubsample-sum(y==smallclass)))
              } else {
                subsetind <- sample(1:nrow(X), size=nsubsample)
                if(sum(y[subsetind]==smallclass) < 30) {
                  subsetind <- c(sample(which(y==smallclass), size=30), sample(which(y==largeclass), size=nsubsample-30))
                }
              }
            } else {
              subsetind <- sample(1:nrow(X), size=nsubsample)
            }
            
            X <- X[subsetind,]
            y <- y[subsetind]
          }
          
          
          # Apply BOLT-SSI to the subset feature space:
          model <- BOLTSSIRR::BOLT_SSI(X[,varsubset], y-1, extra_pairs = npreselpairs)
          foundpairs <- model[,1:2]
          
		  # Get the indices of the selected feature pairs in the un-subset featur space:
          foundpairs[,1] <- varsubset[foundpairs[,1]]
          foundpairs[,2] <- varsubset[foundpairs[,2]]
          
        }
        
      }
      
   
      # If still less than npreselpairs promising pairs were identified
	  # in the above, the remaining pairs are identified by randomly
      # sampling 20 times the number of remaining feature pairs
      # to pre-select, testing each of these and selecting those
      # with smallest p-values:
      
      nadd <- 20*(npreselpairs - nrow(foundpairs))
      
      if(nadd <= 0) {
        
        promispairs <- foundpairs[1:npreselpairs,]
        
        rm(foundpairs);gc()
        
      } else {
        
        # If there are more than 1000 observations in the data,
        # they are subset to contain only 1000 observations:
		
        X <- Xsafe; y <- ysafe
        nsubsample <- 1000
        if(nrow(X) > nsubsample) {
          if(length(unique(y))==2) {
            classsizes <- table(y)
            nclasssmall <- min(classsizes)
            classes <- as.numeric(names(classsizes))
            smallclass <- classes[which.min(classsizes)]
            largeclass <- classes[setdiff(c(1,2), smallclass)]
            
            if(nclasssmall <= 30) {
              subsetind <- c(which(y==smallclass), sample(which(y==largeclass), size=nsubsample-sum(y==smallclass)))
            } else {
              subsetind <- sample(1:nrow(X), size=nsubsample)
              if(sum(y[subsetind]==smallclass) < 30) {
                subsetind <- c(sample(which(y==smallclass), size=30), sample(which(y==largeclass), size=nsubsample-30))
              }
            }
          } else {
            subsetind <- sample(1:nrow(X), size=nsubsample)
          }
          
          X <- X[subsetind,]
          y <- y[subsetind]
        }
        
        # Randomly draw feature pairs and exclude those,
        # which already exist in the previously pre-selected
        # pairs:
        
        pairscand <- matrix(nrow=nadd, ncol=2)
        pairscand[,1] <- sample(1:p, size=nadd, replace=TRUE)
        pairscand[,2] <- sample(1:p, size=nadd, replace=TRUE)
        
        pairscand <- pairscand[apply(pairscand, 1, function(x) x[1]!=x[2]),]
        pairscand <- t(apply(pairscand, 1, sort))
        
        pairscandstr <- apply(pairscand, 1, function(x) paste(x, collapse="_"))
        inclbool <- !duplicated(pairscandstr)
        
        pairscand <- pairscand[inclbool,]
        pairscandstr <- pairscandstr[inclbool]
        
        foundpairsstr <- apply(foundpairs, 1, function(x) paste(x, collapse="_"))
        pairscand <- pairscand[sapply(pairscandstr, function(x) !(x %in% foundpairsstr)),]
        
        
        # Again draw randomly feature pairs (two times as many, as needed) and exclude 
        # already pre-selected ones:
        
        pairscand2 <- matrix(nrow=2*(nadd - nrow(pairscand)), ncol=2)
        pairscand2[,1] <- sample(1:p, size=2*(nadd - nrow(pairscand)), replace=TRUE)
        pairscand2[,2] <- sample(1:p, size=2*(nadd - nrow(pairscand)), replace=TRUE)
        
        pairscand2 <- pairscand2[apply(pairscand2, 1, function(x) x[1]!=x[2]),]
        pairscand2 <- t(apply(pairscand2, 1, sort))
        
        pairscandstr2 <- apply(pairscand2, 1, function(x) paste(x, collapse="_"))
        inclbool <- !duplicated(pairscandstr2)
        
        pairscand2 <- pairscand2[inclbool,]
        pairscandstr2 <- pairscandstr2[inclbool]
        
        pairscandstr <- apply(pairscand, 1, function(x) paste(x, collapse="_"))
        
        inclbool <- !(pairscandstr2 %in% c(pairscandstr, foundpairsstr))
        pairscand2 <- pairscand2[inclbool,]
        
        
        # If enough feature pairs not among the already pre-selected features
        # have been selected, the pre-selection is finished...
        if(nrow(pairscand2) >= nadd - nrow(pairscand)) {
          pairscand <- rbind(pairscand, pairscand2[1:(nadd - nrow(pairscand)),])
        }
        else {
          # ...otherwise we randomly sample feature pairs until enough
          # feature pairs were sampled:
          pairscand <- rbind(pairscand, pairscand2)
          nrest <- nadd - nrow(pairscand)
          
          pairscandstr <- apply(pairscand, 1, function(x) paste(x, collapse="_"))
          
          count <- 0
          while(count < nrest) {
            cand <- sort(sample(1:p, size=2))
            candstr <- paste(cand, collapse="_")
            if(!(candstr %in% pairscandstr)) {
              pairscand <- rbind(pairscand, cand)
              pairscandstr <- c(pairscandstr, candstr)
            }
          }
          
        }
        
        
        
        # Test each of the sampled feature pairs for interaction
        # effect and keep those with the smallest p-values:
        
        pval <- 0
        
        for(i in 1:nrow(pairscand)) {
          x1 <- X[,pairscand[i,1]]
          x2 <- X[,pairscand[i,2]]
          
          mod <- RcppEigen::fastLmPure(cbind(1, x1, x2, x1*x2), as.numeric(y)-1)
          pval[i] <-2*(1-pnorm(abs(mod$coefficients[4]), sd=mod$se[4]))
        }
        
        orderind <- order(pval)[1:(nrow(pairscand)/20)]
        pairsrest <- pairscand[orderind,]
        
        
        # Finally, add the feature pairs pre-selected using linear regression
        # to those pre-selected using BOLT-SSI:
        
        foundpairs <- rbind(foundpairs, pairsrest)
        
        promispairs <- foundpairs
        
        rm(foundpairs);gc()
        
      } 
      
      
    }
    
  }
  
  # Substract 1, because in C++ counters start at 0 instead of 1:
  promispairs <- promispairs - 1
  
  # Return the matrix of the indices of the promising pairs:
  return(promispairs)
  
}
