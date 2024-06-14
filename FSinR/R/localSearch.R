# Local Search

#' @author Francisco Aragón Royón
#' @title Hill-Climbing
#' @description Generates a search function based on the hill climbing method. This function is called internally within the \code{\link{searchAlgorithm}} function. The Hill-Climbing \insertCite{Russell2009}{FSinR} method starts with a certain set of features and in each iteration it searches among its neighbors to advance towards a better solution. The method ends as soon as no better solutions are found.
#'
#' @param start Binary vector with the set of initial features
#' @param nneigh Number of neighbors to evaluate in each iteration of the algorithm. By default: all posibles. It is important to note that a high value of this parameter considerably increases the computation time.
#' @param repeats Number of repetitions of the algorithm
#' @param verbose Print the partial results in each iteration
#'
#' @return Returns a search function that is used to guide the feature selection process.
#'
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a search process in a feature space
#' ## Classification problem
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the search function with Hill-Climbing
#' hc_search <- hillClimbing()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' hc_search(iris, 'Species', filter_evaluator)
#' }
hillClimbing <- function(start=NULL, nneigh=NULL, repeats=1, verbose=FALSE){

  hillClimbingSearch <- function(data, class, featureSetEval){
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    
    # Check parameters
    if(is.null(nneigh)){
      nneigh <- length(data)-1
    }else{
      if((nneigh < 1) || (nneigh>(length(data)-1))) stop('The number of neighbors to consider must be a number between 1 and the total number of features')
    }
    
    if(!is.null(start)){
      if(length(start) != ncol(data)-1) stop('The initial vector has a number of erroneous features')
    }
    
    
    if(repeats < 1) stop('Incorrect number of repetitions')
    
    # Extract and eliminate the class to have only the features in the variable 'features' 
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- column.names[-class.position]
    
    # Check for maximization-minimization
    metricTarget <- attr(featureSetEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    generateNeighbors <- function(bestFeatures, nneigh){
      
      repeat{
        # Generate the features to change to generate the neighbors
        pos <- sample(1:length(bestFeatures), nneigh, replace=FALSE)
        
        # Matrix with the neighbor
        neigh <- matrix(rep(NA,length(bestFeatures)*nneigh), nrow=nneigh)
        
        # Generate the neighbors
        for(i in 1:length(pos)){
          auxNeigh <- bestFeatures
          auxNeigh[pos[i]] <- abs(auxNeigh[pos[i]]-1)
          neigh[i,] <- auxNeigh
        }
        
        # Any neighbor equal to 0?
        zero <- which( apply(neigh,1,function(x){sum(x)}) == 0)
        if(length(zero)!=0){
          neigh <- neigh[-zero,]
        }
        
        
        # If not, return the neigborgs
        if(nrow(neigh)!=0){
          break
        }     
      }
      
      return(neigh) 
    }
    
    # Generate a random start point
    if(is.null(start)){
      repeat{
        random <- sample(0:1, length(features), replace = TRUE)     
        if(sum(random!=0)){
          start <- random
          break
        }      
      } 
    }
    
    
    # List with results
    res <- list(NULL)
    res[[1]] <- NA
    res[[2]] <- NA
    res[[3]] <- list(NULL)
    res[[4]] <- list(NULL)
    ncol <- 2 + (ncol(data)-1)
    res[[5]] <- list(NULL)
    names(res) <- c("bestFeatures","bestFitness","initialVector","initialFitness","trace")
    
    
    # Algorithm repetition
    for(j in 1:repeats){
      
      # Initialize features
      if(j==1){
        if(all(start==0)){
          repeat{
            start <- sample(0:1,ncol(data)-1,replace=TRUE)
            if(!all(start==0)){
              break
            }
          }
        } 
      }else{
        repeat{
          start <- sample(0:1,ncol(data)-1,replace=TRUE)
          if(!all(start==0)){
            break
          }
        }   
      }
      
      
      bestFeatures <- start
      bestFitness <- featureSetEval(data, class, features[which(start==1)])
      if(j==1){ # First repetition, it's the best
        bestGlobalFeatures <- bestFeatures
        bestGlobalFitness <- bestFitness
      }else{ # If not, check with the best global
        if(max){
          if(bestFitness>bestGlobalFitness){
            bestGlobalFitness <- bestFitness
            bestGlobalFeatures <- bestFeatures
          }else if(bestFitness==bestGlobalFitness){
            featuresOld <- sum(bestGlobalFeatures)
            featuresNew <- sum(bestFeatures)
            
            if(featuresNew<featuresOld){
              bestGlobalFitness <- bestFitness
              bestGlobalFeatures <- bestFeatures               
            }
          }
        }else{
          if(bestFitness<bestGlobalFitness){
            bestGlobalFitness <- bestFitness
            bestGlobalFeatures <- bestFeatures
          }else if(bestFitness==bestGlobalFitness){
            featuresOld <- sum(bestGlobalFeatures)
            featuresNew <- sum(bestFeatures)
            
            if(featuresNew<featuresOld){
              bestGlobalFitness <- bestFitness
              bestGlobalFeatures <- bestFeatures               
            }
          }
        }
        
      }
      res[[3]][[j]] <- bestFeatures; names(res$initialVector)[j] <- paste("Repetition",j,sep="")
      res[[4]][[j]] <- bestFitness; names(res$initialFitness)[j] <- paste("Repetition",j,sep="")
      if(verbose==TRUE){
        if(repeats>1){
          cat(paste("HC | Repetition = ",j,"\n",sep=""))
        }
        cat(paste("HC | InitialVector = ",paste(bestFeatures,collapse="")," | InitialFitness = ",round(bestFitness,7),"\n"))
      }
      
      iter <- 1
      
      # Algorithm start
      repeat{
        
        # Generate the neighbors
        neighbors <- generateNeighbors(bestFeatures, nneigh)
        
        # Calculate the fitness
        fitness <- c()
        for(i in 1:nrow(neighbors)){
          fitness <- c(fitness, featureSetEval(data,class,features[which(neighbors[i,]==1)]))
        }
        
        # For maximization
        if(max){
          # Get the best fitness
          maxFitness <- max(fitness)
          neighMaxFitness <- neighbors[which(fitness==max(fitness)),]
          if(!is.null(nrow(neighMaxFitness))){ # With the fewest number of variables
            lengths <- apply(neighMaxFitness,1,function(x){sum(x)})
            pos <- which(lengths==min(lengths))
            if(length(pos)==1){
              neighMaxFitness <- neighMaxFitness[pos,]
            }else{
              neighMaxFitness <- neighMaxFitness[sample(pos,1),]
            }
          }
          
          # Check the fitness
          if(maxFitness < bestFitness){ # If it's worse, end
            end <- "yes"
          }else if(maxFitness == bestFitness){ # If they're the same, the one with the least features
            featuresOld <- sum(bestFeatures)
            featuresNew <- sum(neighMaxFitness)
            
            if(featuresOld<=featuresNew){
              end <- "yes"
            }else{
              bestFitness <- maxFitness
              bestFeatures <- neighMaxFitness
              
              # Check with the best global
              if(bestFitness>bestGlobalFitness){
                bestGlobalFitness <- bestFitness
                bestGlobalFeatures <- bestFeatures
              }else if(bestFitness==bestGlobalFitness){
                featuresOld <- sum(bestGlobalFeatures)
                featuresNew <- sum(bestFeatures)
                
                if(featuresNew<featuresOld){
                  bestGlobalFitness <- bestFitness
                  bestGlobalFeatures <- bestFeatures               
                }
              }
              
              end <- "no"
            }
            
          }else{ # If it's better, it's continued
            bestFitness <- maxFitness
            bestFeatures <- neighMaxFitness
            
            # Check with the best global
            if(bestFitness>bestGlobalFitness){
              bestGlobalFitness <- bestFitness
              bestGlobalFeatures <- bestFeatures
            }else if(bestFitness==bestGlobalFitness){
              featuresOld <- sum(bestGlobalFeatures)
              featuresNew <- sum(bestFeatures)
              
              if(featuresNew<featuresOld){
                bestGlobalFitness <- bestFitness
                bestGlobalFeatures <- bestFeatures               
              }
            }
            
            end <- "no"
          }
          
          if(verbose==TRUE){
            cat(paste("HC | Vector = ",paste(neighMaxFitness,collapse="")," | Fitness = ",round(maxFitness,7)," | End? = ",end,"\n"))
          }
          
          # Save the iteration results
          if(iter==1){
            if(end=="no"){
              res$trace[[j]] <- matrix(rep(NA,ncol), ncol=ncol, dimnames = list(c(),c(c("iter"), paste0("x", 1:(length(start))), c("fitness"))))
              names(res$trace)[j] <- paste("Repetition",j,sep="")
              res$trace[[j]][iter,] <- c(iter, neighMaxFitness, maxFitness)            
            }
          }else{
            if(end=="no"){
              res$trace[[j]] <- rbind(res$trace[[j]], c(iter, neighMaxFitness, maxFitness))    
            }
          }
          iter <- iter + 1
          
          if(end=="yes"){
            break
          }
        }else{
          # Get the best fitness
          minFitness <- min(fitness)
          neighMinFitness <- neighbors[which(fitness==min(fitness)),]
          if(!is.null(nrow(neighMinFitness))){ # With the fewest number of variables
            lengths <- apply(neighMinFitness,1,function(x){sum(x)})
            pos <- which(lengths==min(lengths))
            if(length(pos)==1){
              neighMinFitness <- neighMinFitness[pos,]
            }else{
              neighMinFitness <- neighMinFitness[sample(pos,1),]
            }
          }
          
          # Check the fitness
          if(minFitness > bestFitness){ # If it's worse, end
            end <- "yes"
          }else if(minFitness == bestFitness){ # If they're the same, the one with the least features
            featuresOld <- sum(bestFeatures)
            featuresNew <- sum(neighMinFitness)
            
            if(featuresOld<=featuresNew){
              end <- "yes"
            }else{
              bestFitness <- minFitness
              bestFeatures <- neighMinFitness
              
              # Check with the best global
              if(bestFitness<bestGlobalFitness){
                bestGlobalFitness <- bestFitness
                bestGlobalFeatures <- bestFeatures
              }else if(bestFitness==bestGlobalFitness){
                featuresOld <- sum(bestGlobalFeatures)
                featuresNew <- sum(bestFeatures)
                
                if(featuresNew<featuresOld){
                  bestGlobalFitness <- bestFitness
                  bestGlobalFeatures <- bestFeatures               
                }
              }
              
              end <- "no"
            }
            
          }else{ # If it's better, it's continued
            bestFitness <- minFitness
            bestFeatures <- neighMinFitness
            
            # Check with the best global
            if(bestFitness<bestGlobalFitness){
              bestGlobalFitness <- bestFitness
              bestGlobalFeatures <- bestFeatures
            }else if(bestFitness==bestGlobalFitness){
              featuresOld <- sum(bestGlobalFeatures)
              featuresNew <- sum(bestFeatures)
              
              if(featuresNew<featuresOld){
                bestGlobalFitness <- bestFitness
                bestGlobalFeatures <- bestFeatures               
              }
            }
            
            end <- "no"
          }
          
          if(verbose==TRUE){
            cat(paste("HC | Vector = ",paste(neighMinFitness,collapse="")," | Fitness = ",round(minFitness,7)," | End? = ",end,"\n"))
          }
          
          # Save the iteration results
          if(iter==1){
            if(end=="no"){
              res$trace[[j]] <- matrix(rep(NA,ncol), ncol=ncol, dimnames = list(c(),c(c("iter"), paste0("x", 1:(length(start))), c("fitness"))))
              names(res$trace)[j] <- paste("Repetition",j,sep="")
              res$trace[[j]][iter,] <- c(iter, neighMinFitness, minFitness)            
            }
          }else{
            if(end=="no"){
              res$trace[[j]] <- rbind(res$trace[[j]], c(iter, neighMinFitness, minFitness))     
            }
          }
          iter <- iter + 1
          
          if(end=="yes"){
            break
          }
        }
        
      }
    }
    
    
    
    res[[1]] <- bestGlobalFeatures  
    res[[2]] <- bestGlobalFitness
    
    res[[1]] <- matrix(res[[1]], ncol=(ncol(data)-1), byrow=FALSE, dimnames=list(c(),column.names[-class.position]))
    
    res
  }
  attr(hillClimbingSearch,'shortName') <- "hc"
  attr(hillClimbingSearch,'name') <- "Hill Climbing"
  
  return(hillClimbingSearch)
}


#' @author Francisco Aragón Royón
#' @title Tabu Search
#' @description Generates a search function based on the tabu search. This function is called internally within the \code{\link{searchAlgorithm}} function. The Tabu Search\insertCite{glover1986}{FSinR} \insertCite{glover1990}{FSinR} method starts with a certain set of features and in each iteration it searches among its neighbors to advance towards a better solution. The method has a memory (tabu list) that prevents returning to recently visited neighbors. The method ends when a certain number of iterations are performed, or when a certain number of iterations are performed without improvement, or when there are no possible neighbors. Once the method is finished, an intensification phase can be carried out that begins in the space of the best solutions found, or a diversification phase can be carried out in which solutions not previously visited are explored.
#' 
#' @param start Binary vector with the set of initial features
#' @param numNeigh The number of neighbor to consider in each iteration. By default: all posibles. It is important to note that a high value of this parameter considerably increases the computation time.
#' @param tamTabuList The size of the tabu list. By default: 5
#' @param iter The number of iterations of the algorithm. By default: 100
#' @param iterNoImprovement Number of iterations without improvement to start/reset the intensification/diversification phase. By default, it is not taken into account (all iterations are performed)
#' @param intensification Number of times the intensification phase is applied. None by default
#' @param iterIntensification Number of iterations of the intensification phase
#' @param interPercentaje Percentage of the most significant features to be taken into account in the intensification phase
#' @param tamIntermediateMemory Number of best solutions saved in the intermediate memory
#' @param diversification Number of times the diversification phase is applied. None by default
#' @param iterDiversification Number of iterations of the diversification phase
#' @param forgetTabuList Forget tabu list for intensification/diversification phases. By default: TRUE
#' @param verbose Print the partial results in each iteration
#'
#' @return Returns a search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a search process in a feature space
#' ## Classification problem
#' 
#' # Generates the filter evaluation function
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the search function wit Tabu search
#' ts_search <- tabu()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' ts_search(iris, 'Species', filter_evaluator)
#' }
tabu <- function(start=NULL, numNeigh=NULL, tamTabuList=5, iter=100, iterNoImprovement=NULL, intensification=NULL, iterIntensification=50, interPercentaje=75, tamIntermediateMemory=5, diversification=NULL, iterDiversification=50, forgetTabuList=TRUE, verbose=FALSE){
  
  tabuSearch <- function(data, class, featureSetEval){
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    
    # Check parameters
    if(is.null(numNeigh)){
      numNeigh <- length(data)-1
    }else{
      if((numNeigh < 1) || (numNeigh>(length(data)-1))) stop('The number of neighbors to consider must be a number between 1 and the total number of features')
    }
    
    if(tamTabuList >= ncol(data)*ncol(data)) stop('The size of the taboo list cannot be equal to or greater than the number of possible moves')
    
    # Extract and eliminate the class to have only the features in the variable 'features' 
    column.names <- names(data) 
    class.position <- which(column.names == class) 
    features <- column.names[-class.position]
    
    # Check for maximization-minimization
    metricTarget <- attr(featureSetEval,'target')
    if(metricTarget=="maximize"){
      max <- TRUE
    }else if(metricTarget=="minimize"){
      max <- FALSE
    }else{ # Metric is not specified
      # Wrapper methods use by default RMSE for regression and Acuraccy for classification (in filter methods the metric is always specified)
      max <- ifelse(is.factor(data[,class]), TRUE, FALSE)
    }
    
    ## FUNCTIONS
    
    generateFeasibleNeighbors <- function(actualPosition, tabuList, featuresExcluded){
      
      # Generates all the neighbors of the current point
      allNeighbors <- matrix(rep(NA,length(features)*length(features)), ncol=length(features), nrow=length(features), dimnames=list(paste0("Neighbor", 1:length(features), sep=""), features))
      for(j in 1:length(features)){
        actualPositionAux <- actualPosition
        actualPositionAux[j] <- abs(1 - actualPositionAux[j])
        allNeighbors[j,] <- actualPositionAux
      }
      
      # Check if a neighbor is the absence of features
      notFeasibleNeighbors <- NULL
      if(length( which(apply(allNeighbors,1, sum)==0) ) != 0 ){ # If all features to zero is a neighborg...
        notFeasibleNeighbors <- c(notFeasibleNeighbors,which(apply(allNeighbors,1, sum)==0))
      }
      # Check if there are any excluded features in the neighbors
      if(!is.null(featuresExcluded)){
        
        if(length(featuresExcluded)==1){
          excludedAux <- as.matrix(allNeighbors[,featuresExcluded])
        }else{
          excludedAux <- allNeighbors[,featuresExcluded]
        }
        notFeasibleNeighbors <- c(notFeasibleNeighbors, which( apply(excludedAux,1,function(x){return(1%in%x)}) ==TRUE ) )
      }
      
      # Check if any of the neighbors are on the taboo list
      for(j in 1:nrow(tabuList)){
        
        # If the tabu list value is not empty
        if( all( is.na(tabuList[j,1:length(features)]) ) == FALSE ){
          
          # Check if the item in the list is a neighbor
          isIn <- t(apply(allNeighbors,1,function(x){tabuList[j,]==x}))
          neighbor <- which(apply(isIn,1,function(x){all(x)})==TRUE)
          notFeasibleNeighbors <- c(notFeasibleNeighbors,neighbor)
          
        }
        
      }
      
      # Get the visitable nodes
      feasiblesNeighbors <- which(!(1:length(features)%in%notFeasibleNeighbors)==TRUE)
      
      if(length(feasiblesNeighbors)==0){
        res <- NULL
      }else{
        if(length(feasiblesNeighbors)==1){
          res <- matrix(allNeighbors[feasiblesNeighbors,], ncol=length(features), dimnames=list(NULL,features))
        }else{
          res <- allNeighbors[feasiblesNeighbors,]        
        }
        
      }
      
      return(res)
    }
    
    refreshTabuList <- function(tabuList, actualPosition){
      
      # Moves an iteration existing nodes
      for(i in (nrow(tabuList)-1):1){
        tabuList[i+1,] <- tabuList[i,]
      }
      # Add new node
      tabuList[1,] <- c(actualPosition)
      
      return(tabuList)
    }
    
    tabuSearch <- function(tabuList, actualPosition, iter, longMemory, intermediateMemory, featuresExcluded){
      
      # Initializate the best set of features and the best fitness
      bestFeatures <- actualPosition
      bestFitness <- featureSetEval(data, class, features[which(actualPosition==1)])
      
      # Initialize the long memory
      longMemory <- longMemory + actualPosition
      # Initialize the intermediate memory    
      intermediateMemory[1,] <- c(bestFeatures,bestFitness)
      
      if(verbose){
        cat(paste("TS | InitialVector = ", paste(bestFeatures,collapse="")," | InitialFitness = ",round(bestFitness,7), "\n"))
      }
      
      # Initialize the list with results
      res <- list(NULL)
      res[[1]] <- NA
      res[[2]] <- NA
      res[[3]] <- list(NULL)
      res[[4]] <- list(NULL)
      names(res) <- c("bestFeatures","bestFitness", "bestNeighbor","tabuList") 
      
      # Refresh tabu list
      tabuList <- refreshTabuList(tabuList, actualPosition)
      
      # Save the neighbor and the tabu list
      res[[3]][[1]] <- matrix(c(actualPosition,bestFitness), ncol=length(features)+1, dimnames=list("neighborInitial",c(features,"Fitness")))
      res[[4]][[1]] <- tabuList
      names(res$bestNeighbor)[[1]] <- "IterInitial"
      names(res$tabuList)[[1]] <- "IterInitial"
      
      # Initializes the counter of the number of iterations without improvement
      count <- 0
      
      
      
      # Iterations of Tabu Search
      for(i in 1:iter){
        
        # Generate feasible neighbors
        feasibleNeighbors <- generateFeasibleNeighbors(actualPosition, tabuList, featuresExcluded)
        
        # Check if there are no feasible neighbors, or there are more than you want to compare
        if( (length(feasibleNeighbors)/length(features))==0 ){ # If there are no feasible neighbors
          if(verbose){
            cat(paste("TS | Iter = ",i," | There are no more feasible movements \n"))
          }
          break
        }else if( (length(feasibleNeighbors)/length(features)) > numNeigh ){ # If there are more feasible neighbors than are checked in each iteration the necessary ones are selected
          feasibleNeighbors <- feasibleNeighbors[sample(1:(length(feasibleNeighbors)/length(features)), numNeigh, replace=FALSE),]
        }
        
        # Evaluate feasible movements
        moveFitness <- c()
        for(j in 1:(length(feasibleNeighbors)/length(features))){
          moveFitness <- c(moveFitness, featureSetEval(data, class, features[which(feasibleNeighbors[j,]==1)]))
        }
        
        
        if(max){ # For maximization
          
          # Move
          actualPosition <- feasibleNeighbors[which(moveFitness==max(moveFitness)),]
          if(length(which(moveFitness==max(moveFitness)))>1){
            actualPosition <- actualPosition[sample(1:length(which(moveFitness==max(moveFitness))),1),]
          }
          
          if(max(moveFitness) > bestFitness){ # If the new solution is better
            bestFeatures <- actualPosition
            bestFitness <- max(moveFitness)
            
            count <- 0
          }else if(max(moveFitness) == bestFitness){ # The solution with the least features
            if(sum(bestFeatures)>sum(actualPosition)){
              bestFeatures <- actualPosition
              bestFitness <- max(moveFitness)
            }
          }
          
          # Add to intermedaite memory
          # Check if the solution is already in the memory
          isIn <- t(apply(intermediateMemory[,1:length(features)],1,function(x){actualPosition==x}))
          appears <- which(apply(isIn,1,function(x){all(x)})==TRUE)
          
          if(length(appears)==0){ # If it's not in the memory...
            if(all(!is.na(intermediateMemory[,1]))){ # If it is complete, it is checked if the solution deserves to enter
              if(max(moveFitness) > min(intermediateMemory[,ncol(intermediateMemory)])){
                pos <- which( intermediateMemory[,ncol(intermediateMemory)] == min(intermediateMemory[,ncol(intermediateMemory)]) )[1]
                intermediateMemory[pos,] <- c(actualPosition,max(moveFitness))              
              }
            }else{ # It is placed in the first free site
              intermediateMemory[which(is.na(intermediateMemory[,1])==TRUE)[1],] <- c(actualPosition,max(moveFitness))
            }          
          }
        }else{ # For minimization
          
          # Move
          actualPosition <- feasibleNeighbors[which(moveFitness==min(moveFitness)),]
          if(length(which(moveFitness==min(moveFitness)))>1){
            actualPosition <- actualPosition[sample(1:length(which(moveFitness==min(moveFitness))),1),]
          }
          
          if(min(moveFitness) < bestFitness){ # If the new solution is better
            bestFeatures <- actualPosition
            bestFitness <- min(moveFitness)
            
            count <- 0
          }else if(min(moveFitness) == bestFitness){ # The solution with the least features
            if(sum(bestFeatures)>sum(actualPosition)){
              bestFeatures <- actualPosition
              bestFitness <- min(moveFitness)
            }
          } 
          
          # Add to intermedaite memory
          # Check if the solution is already in the memory
          isIn <- t(apply(intermediateMemory[,1:length(features)],1,function(x){actualPosition==x}))
          appears <- which(apply(isIn,1,function(x){all(x)})==TRUE)
          
          if(length(appears)==0){ # If it's not in the memory...
            # Check add to intermedaite memory
            if(all(!is.na(intermediateMemory[,1]))){ # If it is complete, it is checked if the solution deserves to enter
              if(min(moveFitness) < max(intermediateMemory[,ncol(intermediateMemory)])){
                pos <- which( intermediateMemory[,ncol(intermediateMemory)] == max(intermediateMemory[,ncol(intermediateMemory)]) )[1]
                intermediateMemory[pos,] <- c(actualPosition,min(moveFitness))
              }
            }else{ # It is placed in the first free site
              intermediateMemory[which(is.na(intermediateMemory[,1])==TRUE)[1],] <- c(actualPosition,min(moveFitness))
            }        
          }
          
        }
        
        # Actualize the longMemory
        longMemory <- longMemory + actualPosition
        
        # Refresh tabu list
        tabuList <- refreshTabuList(tabuList, actualPosition)
        
        
        # Save the neighbor and the tabu list
        if(max){
          res[[3]][[i+1]] <- matrix(c(actualPosition,max(moveFitness)), ncol=length(features)+1, dimnames=list("bestNeighbor",c(features,"Fitness")))
        }else{
          res[[3]][[i+1]] <- matrix(c(actualPosition,min(moveFitness)), ncol=length(features)+1, dimnames=list("bestNeighbor",c(features,"Fitness")))
        }
        res[[4]][[i+1]] <- tabuList
        names(res$bestNeighbor)[[i+1]] <- paste("Iter",i,sep="")
        names(res$tabuList)[[i+1]] <- paste("Iter",i,sep="")
        
        
        if(verbose){
          if(max){
            cat(paste("TS | Iter = ",i, " | Vector = ", paste(actualPosition,collapse="")," | Fitness = ",round(max(moveFitness),7), "| BestFitness = ",round(bestFitness,7), "\n"))
          }else{
            cat(paste("TS | Iter = ",i, " | Vector = ", paste(actualPosition,collapse="")," | Fitness = ",round(min(moveFitness),7), "| BestFitness = ",round(bestFitness,7), "\n"))
          }
        }
        
        
        # Actualize the counter
        if(max){ # For maximization
          if(max(moveFitness) <= res[[3]][[i]][length(features)+1]){
            count <- count + 1
          }else{
            count <- 0
          }
        }else{ # For minimization
          if(min(moveFitness) <= res[[3]][[i]][length(features)+1]){
            count <- count + 1
          }else{
            count <- 0
          }
        }
        
        
        # Check the counter
        if(!is.null(iterNoImprovement)){
          if(count==iterNoImprovement){
            if(verbose){cat(paste("TS | Iter = ",i," | Maximum number of iterations without improvement achieved \n"))}
            break
          }
        }
        
      }
      
      res[[1]] <- bestFeatures
      res[[2]] <- bestFitness
      
      return(list(res,tabuList,longMemory,intermediateMemory))
    }
    
    generateFromlongMemory <- function(longMemory, totalIter){
      res <- rep(0,length(features))
      
      repeat{
        
        for(i in 1:length(res)){
          prob <- 1-(longMemory[i]/totalIter)
          random <- runif(1, min = 0, max = 1)
          if(random<=prob){
            res[i] <- 1
          }
        }
        
        if(sum(res!=0)){
          break
        } 
        
      }
      
      return(res)
    } 
    
    ## PARAMETERS INITIALIZATION
    
    # Generate a random start point
    if(is.null(start)){
      repeat{
        random <- sample(0:1, length(features), replace = TRUE)     
        if(sum(random!=0)){
          start <- random
          break
        }      
      } 
    }
    actualPosition <- start
    
    # Initialize the tabu list
    tabuList <- matrix(rep(NA,length(features)*tamTabuList), ncol=length(features), nrow=tamTabuList, dimnames=list(paste0(tamTabuList:1, "IterToLeave", sep=""),features))
    
    # Initialize the features frecuency (Long-Term memory)
    longMemory <- matrix(rep(0,length(features)), ncol=length(features), nrow=1, dimnames=list(NULL, features))
    # Initializate the best solutions (Intermediate memory)
    intermediateMemory <- matrix(rep(NA,(length(features)+1)*tamIntermediateMemory), ncol=length(features)+1, dimnames=list(NULL,c(features,"Fitness")))
    
    # Initialize the list with results
    res <- list(NULL)
    res[[1]] <- NA
    res[[2]] <- NA
    res[[3]] <- list(NULL)
    names(res) <- c("bestFeatures","bestFitness", "basicStage") 
    
    
    
    ## BASIC TABU SEARCH
    resBasic <- tabuSearch(tabuList, actualPosition, iter, longMemory, intermediateMemory, NULL)
    
    tabuList <- resBasic[[2]]
    longMemory <- resBasic[[3]]
    intermediateMemory <- resBasic[[4]]
    resBasic <- resBasic[[1]]  
    
    
    res[[1]] <- resBasic[[1]]
    res[[2]] <- resBasic[[2]]  
    res[[3]][[1]] <- resBasic[[3]]
    names(res$basicStage)[[1]] <- "bestNeighbor"
    res[[3]][[2]] <- resBasic[[4]]
    names(res$basicStage)[[2]] <- "tabuList"
    
    ## INTENSIFICATION STAGE
    if(!is.null(intensification)){
      for(i in 1:intensification){
        if(verbose){
          cat(paste("TS | Intensification stage ",i,"\n"))        
        }
        if(forgetTabuList){
          tabuList <- matrix(rep(NA,length(features)*tamTabuList), ncol=length(features), nrow=tamTabuList, dimnames=list(paste0(tamTabuList:1, "IterToLeave", sep=""),features))
        }
        
        # Calculates the number of features to take into account
        numFeatures <- round((length(features)*interPercentaje)/100,0)
        frequency <- apply(intermediateMemory[,1:length(features)],2,sum)
        excluded <- order(frequency, decreasing=TRUE)[(numFeatures+1):length(features)]
        # Choose a solution without the excluded features (if it posible)
        if(length(excluded)==1){
          excludedAux <- as.matrix(intermediateMemory[,excluded])
        }else{
          excludedAux <- intermediateMemory[,excluded]
        }
        if( any( apply( excludedAux,1,function(x){return(all(x==0))} ) ) ){
          # The solution without the excluded features
          solutions <- which(apply( excludedAux,1,function(x){return(all(x==0))} ) == TRUE )
          if(length(solutions)>1){
            solutions <- sample(1:length(solutions),1)
            actualPosition <- intermediateMemory[solutions,1:length(features)]
          }
          
        }else{
          actualPosition <- rep(0,length(features))
          actualPosition[(1:length(features))[-excluded]] <- 1  # All features not excluded
        }
        
        resInter <- tabuSearch(tabuList, actualPosition, iterIntensification, longMemory, intermediateMemory, excluded)
        
        tabuList <- resInter[[2]]
        longMemory <- resInter[[3]]
        intermediateMemory <- resInter[[4]]
        resInter <- resInter[[1]] 
        
        # Check if it's the best
        if(max){ # For maximization
          if(resInter[[2]]>res[[2]]){
            res[[1]] <- resInter[[1]]
            res[[2]] <- resInter[[2]]
          }else if(resInter[[2]]==res[[2]]){ # The solution with the least features
            if(sum(res[[1]])>sum(resInter[[1]])){
              res[[1]] <- resInter[[1]]
            }
          }
        }else{ # For minimization
          if(resInter[[2]]<res[[2]]){
            res[[1]] <- resInter[[1]]
            res[[2]] <- resInter[[2]]
          } else if(resInter[[2]]==res[[2]]){ # The solution with the least features
            if(sum(res[[1]])>sum(resInter[[1]])){
              res[[1]] <- resInter[[1]]
            }
          }       
        }
        
        res[[length(res)+1]] <- list()
        names(res)[[length(res)]] <- paste("intensificationStage",i,sep="")
        res[[length(res)]][[1]] <- resInter[[3]]
        names(res[[length(res)]])[[1]] <- "bestNeighbor"
        res[[length(res)]][[2]] <- resInter[[4]] 
        names(res[[length(res)]])[[2]] <- "tabuList"
      }
    }
    
    ## DIVERSIFICATION STAGE
    if(!is.null(diversification)){
      for(i in 1:diversification){
        if(verbose){
          cat(paste("TS | Diversification stage ",i,"\n"))
        }
        if(forgetTabuList){
          tabuList <- matrix(rep(NA,length(features)*tamTabuList), ncol=length(features), nrow=tamTabuList, dimnames=list(paste0(tamTabuList:1, "IterToLeave", sep=""),features))
        }
        
        # Generates a new solution according to the longMemory of appearance of each feature
        totalIter <- 0
        for(j in 3:length(res)){
          totalIter <- totalIter + length(res[[j]][[1]])
        }
        actualPosition <- generateFromlongMemory(longMemory, totalIter)
        
        resDiver <- tabuSearch(tabuList, actualPosition, iterDiversification, longMemory, intermediateMemory, NULL)
        
        tabuList <- resDiver[[2]]
        longMemory <- resDiver[[3]]
        intermediateMemory <- resDiver[[4]]
        resDiver <- resDiver[[1]] 
        
        # Check if it's the best
        if(max){ # For maximization
          if(resDiver[[2]]>res[[2]]){
            res[[1]] <- resDiver[[1]]
            res[[2]] <- resDiver[[2]]
          }else if(resDiver[[2]]==res[[2]]){ # The solution with the least features
            if(sum(res[[1]])>sum(resDiver[[1]])){
              res[[1]] <- resDiver[[1]]
            }
          }
        }else{ # For minimization
          if(resDiver[[2]]<res[[2]]){
            res[[1]] <- resDiver[[1]]
            res[[2]] <- resDiver[[2]]
          }else if(resDiver[[2]]==res[[2]]){ # The solution with the least features
            if(sum(res[[1]])>sum(resDiver[[1]])){
              res[[1]] <- resDiver[[1]]
            }
          }        
        }
        
        res[[length(res)+1]] <- list()
        names(res)[[length(res)]] <- paste("diversificationStage",i,sep="")
        res[[length(res)]][[1]] <- resDiver[[3]]
        names(res[[length(res)]])[[1]] <- "bestNeighbor"
        res[[length(res)]][[2]] <- resDiver[[4]] 
        names(res[[length(res)]])[[2]] <- "tabuList"
      }
    }
    
    # Modify the output
    res[[1]] <- matrix(res[[1]], nrow=1, byrow=FALSE, dimnames=list(c(),features))
    
    return(res)
  }
  
  attr(tabuSearch,'shortName') <- "tabu"
  attr(tabuSearch,'name') <- "Tabu Search"
  
  return(tabuSearch)
}
