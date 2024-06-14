# Metaheuristics
capture.output(library(GA))

#' @author Francisco Aragón Royón
#' @title Genetic Algorithm
#' @description Generates a search function based on a genetic algorithm. This function is called internally within the \code{\link{searchAlgorithm}} function. The geneticAlgorithm method \insertCite{yang1998feature}{FSinR} starts with an initial population of solutions and at each step applies a series of operators to the individuals in order to obtain new and better population of individuals. These operators are selection, crossing and mutation methods. This method uses the GA package implementation \insertCite{GAPkg1}{FSinR} \insertCite{GAPkg2}{FSinR}.
#' 
#' @param popSize The popuplation size
#' @param pcrossover The probability of crossover between individuals
#' @param pmutation The probability of mutation between individuals
#' @param maxiter The number of iterations
#' @param run Number of consecutive iterations without fitness improvement to stop the algorithm
#' @param verbose Print the partial results in each iteration. This functionality is not available if the objective of the evaluation method is to minimize the target value (e.g. regression methods)
#'
#' @return Returns a search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @export
#' @import GA
#' @importFrom utils capture.output
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
#' # Generates the search function with Genetic algorithm
#' ga_search <- geneticAlgorithm()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' ga_search(iris, 'Species', filter_evaluator)
#' }
geneticAlgorithm <- function(popSize=20, pcrossover = 0.8, pmutation = 0.1, maxiter = 100, run = 100, verbose = FALSE){

  geneticAlgorithmSearch <- function(data, class, featureSetEval){
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
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
    
    # Fitness function
    fitness <- function(ind, data, class){
      # Features selected
      feat <- features[which(ind==1)]
      
      if(length(feat)==0){ # No features implies bad solution
        if(max){ # Classification
          value <- 0
        }else{ # Regression
          value <- -Inf
        }
      }else{ # Evaluation
        if(max){ # Classification -> Maximize
          value <- featureSetEval(data, class, feat)  
        }else{ # Regression -> Minimize
          value <- -(featureSetEval(data, class, feat))
        }
        
      }
      
      return(value)
    }
    
    # Genetic Algorithm
    if(!verbose){
      capture.output(GA <- GA::ga(type = "binary", fitness = fitness, data=data, class=class, nBits = length(features), 
                                  popSize = popSize, 
                                  pcrossover =pcrossover, 
                                  pmutation = pmutation, 
                                  maxiter = maxiter,
                                  run = run)
      )
    }else{
      if(max){
        GA <- GA::ga(type = "binary", fitness = fitness, data=data, class=class, nBits = length(features), 
                     popSize = popSize, 
                     pcrossover =pcrossover, 
                     pmutation = pmutation, 
                     maxiter = maxiter,
                     run = run)     
      }else{
        capture.output(GA <- GA::ga(type = "binary", fitness = fitness, data=data, class=class, nBits = length(features), 
                                    popSize = popSize, 
                                    pcrossover =pcrossover, 
                                    pmutation = pmutation, 
                                    maxiter = maxiter,
                                    run = run)
        )  
      }
      
    }
    
    # List with results
    res <- list(NULL)
    res[[1]] <- GA@solution
    colnames(res[[1]]) <- features
    res[[2]] <- GA@fitnessValue
    if(!max){
      res[[2]] <- -res[[2]]
    }
    ncol <- ncol(data)
    res[[3]] <- matrix(cbind(GA@population,GA@fitness), ncol=ncol, dimnames = list(c(), c(paste0("x", 1:((ncol(data)-1))), c("fitness"))) )
    names(res) <- c("bestFeatures","bestFitness","population") 
    if(!max){
      res[[3]][,'fitness'] <- -res[[3]][,'fitness']
    }
    
    res
  }
  
  attr(geneticAlgorithmSearch,'shortName') <- "ga"
  attr(geneticAlgorithmSearch,'name') <- "Genetic Algorithm"
  
  return(geneticAlgorithmSearch)
}



#' @author Francisco Aragón Royón
#' @title Simulated Annealing
#' @description Generates a search function based on simulated annealing. This function is called internally within the \code{\link{searchAlgorithm}} function. The simulatedAnnealing method \insertCite{KirkpatrickGelattVecchi1983}{FSinR} starts with a certain set of features and in each iteration modifies an element of the previous feature vector and decreases the temperature. If the energy of the new feature vector is better than that of the old vector, it is accepted and moved towards it, otherwise it is moved towards the new vector according to an acceptance probability.  The algorithm ends when the minimum temperature has been reached. Additionally, a number of internal iterations can be performed within each iteration of the algorithm. In this case, the same temperature value of the outer iteration is used for the inner iterations
#' 
#' @param start Binary vector with the set of initial features
#' @param temperature Temperature initial
#' @param temperature_min Temperature to stops in the outer loop
#' @param reduction Temperature reduction in the outer loop
#' @param innerIter Number of iterations of inner loop. By default no inner iterations are established
#' @param verbose Print the partial results in each iteration
#'
#' @return Returns a search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#'
#' @importFrom stats runif
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
#' # Generates the search function with Simulated annealing
#' sa_search <- simulatedAnnealing()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' sa_search(iris, 'Species', filter_evaluator)
#' }
simulatedAnnealing <- function(start=NULL, temperature=1, temperature_min=0.01, reduction=0.6, innerIter=1, verbose = FALSE) {
  
  simulatedAnnealingSearch <- function(data, class, featureSetEval) {
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
        
    generateRandomNeighbor <- function(subset){
      repeat{
        pos <- sample(1:length(subset), 1)
        if(!all(subset[-pos]==0)){
          break
        }
      }
      subset[pos] <- abs(subset[pos]-1)
      subset
    }
    
    acceptanceProbability <- function(old, new, t){
      exp(-abs(new-old)/t)
      #exp(-(new-old)/t)
      #exp(-((new-old)/t))
    }
    
    # Check parameters
    t <- temperature 
    if(temperature < 0) stop('Temperature less than 0')
    t_min <- temperature_min
    if(t_min < 0) stop('Minimum temperature less than 0')
    if(t_min > temperature) stop('Minimum temperature greater than temperature')
    alpha <- reduction
    if(t_min < 0) stop('Temperature descent less than 0')
    if(innerIter < 0) stop('InnerIter less than 0')
    
    # If the initial feature set is not passed, it is generated randomly
    if(is.null(start)){
      start <- sample(0:1,ncol(data)-1,replace=TRUE)  
    }
    
    if(all(start==0)){
      repeat{
        start <- sample(0:1,ncol(data)-1,replace=TRUE)
        if(!all(start==0)){
          break
        }
      }
    }
    
    # The data should be a data.frame
    data <- data.frame(data)
    
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
    
    # List with results
    res <- list(NULL)
    res[[1]] <- NA
    res[[2]] <- NA
    res[[3]] <- start  
    res[[4]] <- NA
    ncol <- 4 + length(start)
    res[[5]] <- matrix(rep(NA,ncol), ncol=ncol, dimnames = list(c(),c(c("n_outter", "temperature","move?"), paste0("x", 1:(length(start))), c("energy"))))
    if(innerIter>1){
      res[[6]] <- list(matrix(rep(NA,ncol), ncol=ncol, dimnames = list(c(),c(c("n_inner", "temperature","move?"), paste0("x", 1:(length(start))), c("energy")))))
    }else{
      res[[6]] <- NA
    }
    names(res) <- c("bestFeatures","bestFitness","initialVector","initialEnergy","traceOutter","traceInner") 
    
    # 
    if(all(start==0)){
      repeat{
        start <- sample(0:1,ncol(data)-1,replace=TRUE)
        if(!all(start==0)){
          old_subset <- start
          res[[3]] <- start  
          break
        }
      }
    }else{
      old_subset <- start
    }
    old_cost <- featureSetEval(data, class, features[which(old_subset==1)])
    res$initialEnergy <- old_cost
    best.set <- old_subset
    best.value <- old_cost
    
    if(verbose){
      cat(paste("SA | InitialVector = ",paste(old_subset,collapse="")," | Energy = ",round(old_cost,7),"\n"))
    }
    
    # Number of outer iterations
    o <- 1
    
    while(t>t_min){
      
      # Number of inner iterations
      i <- 1
      moveOutter <- FALSE
      while(i <= innerIter){
        
        moveInner <- FALSE
        new_subset <- generateRandomNeighbor(old_subset) # Generate new Neighbor
        new_cost <- featureSetEval(data, class, features[which(new_subset==1)]) # Evaluate the energy
        
        if(max){ # Classification -> maximize
          move <- new_cost>old_cost
        }else{ # Regression -> minimize
          move <- new_cost<old_cost  
        }
        
        if(move){ # Move
          old_subset <- new_subset
          old_cost <- new_cost
          
          if(max){ # Classification -> maximize
            best <- new_cost>best.value
          }else{ # Regression -> minimize
            best <- new_cost<best.value
          }
          
          if(best){ # Actualize best value
            best.set <- new_subset
            best.value <- new_cost
          }else if(new_cost==best.value){
            best.set <- rbind(best.set,new_subset)
          }
          moveOutter <- TRUE
          moveInner <- TRUE
        }else{
          if(new_cost==best.value){
            best.set <- rbind(best.set,new_subset)          
          }
          a <- acceptanceProbability(old_cost, new_cost, t) 
          ran <- runif(1, 0, 1)
          if(a > ran){ # Move
            old_subset <- new_subset
            old_cost <- new_cost
            
            moveOutter <- TRUE
            moveInner <- TRUE
          } # Else don't move
          
        }
        
        # Save the inner iteration results
        if(innerIter>1){
          if(i==1){
            if(o==1){
              res$traceInner[[o]][i,] <- c(i, t, moveInner, old_subset, old_cost)
            }else{
              res$traceInner[[o]] <- matrix(c(i, t, moveInner, old_subset, old_cost), ncol=ncol, dimnames = list(c(),c(c("n_inner", "temperature","move?"), paste0("x", 1:(length(start))), c("energy"))))
            }
            names(res$traceInner)[o] <- paste("Outter",o,sep="")
          }else{
            res$traceInner[[o]] <- rbind(res$traceInner[[o]], c(i, t, moveInner, old_subset, old_cost))    
          }
        }
        
        
        i <- i + 1
      }
      
      # Save the outer iteration results
      if(o==1){
        res$traceOutter[o,] <- c(o, t, moveOutter, old_subset, old_cost)
      }else{
        res$traceOutter <- rbind(res$traceOutter, c(o, t, moveOutter, old_subset, old_cost))    
      }
      
      if(verbose){
        cat(paste("SA | iter = ",o," | Temp = ",round(t,4)," | Vector = ",paste(old_subset,collapse="")," | Energy = ",round(old_cost,7),"\n"))
      }
      
      # Updates the temperature
      t <- t * alpha
      o <- o + 1
    }
    
    if(is.null(nrow(best.set))){
      res$bestFeatures <- matrix(best.set, ncol=length(features), byrow=FALSE, dimnames=list(c(),features))   
    }else{
      res$bestFeatures <- matrix(unique(best.set), ncol=length(features), byrow=FALSE, dimnames=list(c(),features))    
    }
    colnames(res$bestFeatures) <- features
    res$bestFitness <- best.value
    return(res)
  }
  
  attr(simulatedAnnealingSearch,'shortName') <- "sa"
  attr(simulatedAnnealingSearch,'name') <- "Simmulated Annealing"
  
  return(simulatedAnnealingSearch)
}


#' @author Francisco Aragón Royón
#' @title Whale Optimization Algorithm (Binary Whale Optimization Algorithm)
#' @description Generates a search function based on the whale optimization algorithm. This function is called internally within the \code{\link{searchAlgorithm}} function. Binary Whale Optimization Algorithm \insertCite{Kumar2018}{FSinR} is an algorithm that simulates the social behavior of humpback whales. This algorithm employs a binary version of the bubble-net hunting strategy. The algorithm starts with an initial population of individuals, and in each iteration updates the individuals according to several possible actions: Encircling prey, Bubble-net attacking or Search for prey
#' 
#' @param population The number of whales population
#' @param iter The number of iterations of the algorithm
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
#' # Generates the search function with WOA
#' woa_search <- whaleOptimization()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' woa_search(iris, 'Species', filter_evaluator)
#' }
whaleOptimization <- function(population=10, iter=10, verbose = FALSE) {

  whaleOptimizationSearch <- function(data, class, featureSetEval) {
    
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    
    # Check parameters
    if(population <= 1) stop('The population should be at least 2')
    if(population < 1) stop('The number of iterations should be at least 1')
    
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
    
    # Generate and initializate the population of whales
    whales <- matrix(NA, nrow=population, ncol=length(features),dimnames=list(paste0("Whale", 1:population),features))
    for(i in 1:nrow(whales)){
      repeat{
        random <- sample(0:1, length(features), replace = TRUE)     
        if(sum(random!=0)){
          break
        }      
      }
      whales[i,] <- random
    }
    
    # Calculate the fitness of each search agents (the fitness of each whale)
    whalesFitness <- rep(NA, population)
    for(i in 1:nrow(whales)){
      whalesFitness[i] <- featureSetEval(data, class, features[which(whales[i,]==1)])
    }  
    
    # Best search agent (best whale's fitness)
    if(max){
      posBestSearchAgent <- which(whalesFitness==max(whalesFitness))
      if(length(posBestSearchAgent)>1){ # If there's more than one...
        aux <- apply(whales[posBestSearchAgent,], 1, sum) # The one with the fewest number of features
        posBestSearchAgent <- posBestSearchAgent[which(aux==min(aux))]
        if(length(posBestSearchAgent)>1){ # If there's more than one...
          posBestSearchAgent <- sample(posBestSearchAgent, size=1) # Randomly
        }
      }
      bestSearchAgent <- whales[posBestSearchAgent,]
      bestFitness <- max(whalesFitness)    
    }else{
      posBestSearchAgent <- which(whalesFitness==min(whalesFitness))
      if(length(posBestSearchAgent)>1){ # If there's more than one...
        aux <- apply(whales[posBestSearchAgent,], 1, sum) # The one with the fewest number of features
        posBestSearchAgent <- posBestSearchAgent[which(aux==min(aux))]
        if(length(posBestSearchAgent)>1){ # If there's more than one...
          posBestSearchAgent <- sample(posBestSearchAgent, size=1) # Randomly
        }
      }
      bestSearchAgent <- whales[posBestSearchAgent,]
      bestFitness <- min(whalesFitness)     
    }
    
    # List with results
    res <- list(NULL)
    res[[1]] <- NA
    res[[2]] <- NA
    res[[3]] <- list(NULL)
    res[[3]][[1]] <- cbind(whales,whalesFitness)
    names(res) <- c("bestFeatures","bestFitness","popIter") 
    names(res[[3]])[1] <- "InitialIteration"
    
    if(verbose==TRUE){
      cat(paste("WOA | BestInitialWhale = ", paste(bestSearchAgent,collapse="")," | BestFitness = ",bestFitness, " | MeanFitness", mean(whalesFitness),"\n"))
    }
    
    # For each iteration...
    for (t in 1:iter){
      # The a parameter is updated
      a <- 2-t*(2/iter) # a decrease from 2 to 0 over the course of iterations
      
      # For each whale...
      for(w in 1:nrow(whales)){
        
        # Repeats if the resulting search space is all 0
        repeat{
          
          # The parameters are updated
          r1 <- runif(1) # Random value between 0 and 1
          A <- 2*a*r1-a # Vector A
          r2 <- runif(1) # Random value between 0 and 1
          C <- 2*r2 # Vector C
          p <- runif(1) # Random number to select the attacking method
          
          # Parameters for Spiral updating position
          #b <- 1 # Constant number for defining the shape of the spiral
          #l <- runif(1, -1, 1) # Random number in [-1,1]
          
          # Salvar por si acaso lo deja todo a cero
          
          # For each feature...
          for (j in 1:ncol(whales)){
            if(p<0.5){ # Shrinking encircling prey
              if(abs(A)<1){ # Encircling prey (shrinking encircling)
                D <- abs(C*bestSearchAgent[j]-whales[w,j])
                cstep <- (1)/(1+exp(-10*(A*D-0.5)))
                
                whales[w,j] <- ifelse(p<cstep,1-(whales[w,j]),whales[w,j])
                #whales[w,j] <- bestSearchAgent[j]-A*D            
              }else if(abs(A)>=1){ # Search for prey (exploration phase)
                # Choose a random whale
                repeat{
                  randomWhale <- floor(nrow(whales)*runif(1)+1) 
                  if(randomWhale!=j){
                    break
                  }
                }
                D <- abs(C*whales[randomWhale,][j]-whales[w,j])
                cstep <- (1)/(1+exp(-10*(A*D-0.5)))
                
                whales[w,j] <- ifelse(p<cstep,1-(whales[w,j]),whales[w,j])
                #whales[w,j] <- whales[randomWhale,][j]-A*D  
              }
            }else if(p >= 0.5){ # Spiral position updating
              D <- abs(bestSearchAgent[j]-whales[w,j]) # Distance between the whale and prey
              cstep <- (1)/(1+exp(-10*(A*D-0.5)))
              
              whales[w,j] <- ifelse(p<cstep,1-(whales[w,j]),whales[w,j])
              #whales[w,j] <- D*exp(b*l)*cos(l*2*pi)+bestSearchAgent[j]
            }
            
          }
          
          if(sum(whales[w,])!=0){
            break
          }
          
        }
        
        
      }
      
      # Calculate the new fitness of each search agents (the fitness of the whales)
      for(i in 1:nrow(whales)){
        whalesFitness[i] <- featureSetEval(data, class, features[which(whales[i,]==1)])
      } 
      
      # Check the new fitness
      if(max){
        if(bestFitness < max(whalesFitness)){
          # New best search agent (best whale's fitness)
          posBestSearchAgent <- which(whalesFitness==max(whalesFitness))
          if(length(posBestSearchAgent)>1){ # If there's more than one...
            aux <- apply(whales[posBestSearchAgent,], 1, sum) # The one with the fewest number of features
            posBestSearchAgent <- posBestSearchAgent[which(aux==min(aux))]
            if(length(posBestSearchAgent)>1){ # If there's more than one...
              posBestSearchAgent <- sample(posBestSearchAgent, size=1) # Randomly
            }
          }
          bestSearchAgent <- whales[posBestSearchAgent,]
          bestFitness <- max(whalesFitness)
        } 
      }else{
        if(bestFitness > max(whalesFitness)){
          # New best search agent (best whale's fitness)
          posBestSearchAgent <- which(whalesFitness==min(whalesFitness))
          if(length(posBestSearchAgent)>1){ # If there's more than one...
            aux <- apply(whales[posBestSearchAgent,], 1, sum) # The one with the fewest number of features
            posBestSearchAgent <- posBestSearchAgent[which(aux==min(aux))]
            if(length(posBestSearchAgent)>1){ # If there's more than one...
              posBestSearchAgent <- sample(posBestSearchAgent, size=1) # Randomly
            }
          }
          bestSearchAgent <- whales[posBestSearchAgent,]
          bestFitness <- min(whalesFitness)
        }   
      }
      
      if(verbose==TRUE){
        cat(paste("WOA | iter = ",t," | BestWhale = ",paste(bestSearchAgent,collapse="")," | BestFitness = ",bestFitness, " | MeanFitness", mean(whalesFitness),"\n"))
      }
      
      res[[3]][[t+1]] <- cbind(whales, whalesFitness)
      names(res[[3]])[t+1] <- paste("Iteration",t,sep="")
    }
    
    res[[1]] <- bestSearchAgent
    res[[2]] <- bestFitness
    
    # Modify the output
    res[[1]] <- matrix(res[[1]], nrow=1, byrow=FALSE, dimnames=list(c(),features))
    
    res
  }
  attr(whaleOptimizationSearch,'shortName') <- "woa"
  attr(whaleOptimizationSearch,'name') <- "Whale Optimization Algorithm"
  
  return(whaleOptimizationSearch)
}  



#' @author Francisco Aragón Royón
#' @title Ant Colony Optimization (Advanced Binary Ant Colony Optimization)
#' @description Generates a search function based on the ant colony optimization. This function is called internally within the \code{\link{searchAlgorithm}} function. The Ant Colony Optimization (Advanced Binary Ant Colony Optimization) \insertCite{Kashef2015}{FSinR} algorithm consists of generating in each iteration a random population of individuals (ants) according to the values of a pheromone matrix (which is updated each iteration according to the paths most followed by the ants) and a heuristic (which determines how good is each path to follow by the ants). The evaluation measure is calculated for each individual. The algorithm ends once the established number of iterations has been reached
#' 
#' @param population The number of ants population
#' @param iter The number of iterations
#' @param a Parameter to control the influence of the pheromone (If a=0, no pheromone information is used)
#' @param b Parameter to control the influence of the heuristic (If b=0, the attractiveness of the movements is not taken into account)
#' @param p Rate of pheromone evaporation
#' @param q Constant to determine the amount of pheromone deposited by the best ant. This amount is determined by the Q/F equation (for minimization) where F is the cost of the solution (F/Q for maximization)
#' @param t0 Initial pheromone level
#' @param tmin Minimum pheromone value
#' @param tmax Maximum pheromone value
#' @param mode Heuristic information measurement. 1 -> min redundancy (by default). 2-> max-relevance and min-redundancy. 3-> feature-feature. 4-> based on F-score
#' @param verbose Print the partial results in each iteration
#'
#' @return Returns a search function that is used to guide the feature selection process.
#' 
#' @references
#'    \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @importFrom stats cor.test
#' @export
#'
#' @examples
#'\dontrun{ 
#'
#' ## The direct application of this function is an advanced use that consists of using this 
#' # function directly and performing a search process in a feature space
#' ## Classification problem
#' 
#' # Generates the filter evaluation function with ACO
#' filter_evaluator <- filterEvaluator('determinationCoefficient')
#' 
#' # Generates the search function
#' aco_search <- antColony()
#' # Performs the search process directly (parameters: dataset, target variable and evaluator)
#' aco_search(iris, 'Species', filter_evaluator)
#' }
antColony <- function(population=10, iter=10, a=1, b=1, p=0.2, q=1, t0=0.2, tmin=0, tmax=1, mode=1, verbose = FALSE) {
  
  antColonySearch <- function(data, class, featureSetEval) {
    if (attr(featureSetEval, 'kind') == "Individual measure") {
      stop('Only feature set measures can be used');
    }
    
    # Check parameters
    if(population <= 1) stop('The population should be at least 2')
    if(iter < 1) stop('The number of iterations should be at least 1')
    if(p > 1 || p <= 0) stop('The rate of pheromone evaporation should be between the range (0,1]')
    if(tmin>=tmax) stop('The maximum value of the pheromone may not be less than the minimum value')
    if(t0<tmin || t0>tmax) stop('The initial pheromone level should be between the range [tmin,tmax]]')  
    if(a<0) stop('The value of a should be >= 0')  
    if(b<0) stop('The value of b should be >= 0') 
    if(mode>4||mode<1) stop('Invalid mode') 
    if( !all( lapply(data[1:(ncol(data)-1)], is.numeric)==TRUE ) ) stop('All features must contain numerical values and not character, boolean, or factor type values since heuristics work only with numerical values') 
    
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
    
    # Function to initialize the heuristic values between nodes
    initializeHeuristic <- function(mode){
      
      # Matrix of general heuristics between features (correlation between i and j)
      graphHeuristicInfoAux <- matrix(rep(NA,length(features)*length(features)),ncol=length(features), nrow=length(features), dimnames=list(features,features)) 
      for(i in 1:length(features)){
        for(j in 1:length(features)){
          if(i!=j){
            graphHeuristicInfoAux[j,i] <- cor.test(data[,i], data[,j], method="pearson")$estimate[[1]] # Calculate the pearson's correlation between features          
          }
        }
      }
      
      # Matrix of general heuristics between features (correlation between i and y)
      graphHeuristicInfoAux2 <- matrix(rep(NA,length(features)),ncol=length(features), nrow=1, dimnames=list("y",features)) 
      for(i in 1:length(features)){
        if(max){
          graphHeuristicInfoAux2[1,i] <- cor.test(data[,i], as.numeric(data[,length(data)]), method="pearson")$estimate[[1]] # Calculate the pearson's correlation between features          
          
        }else{
          graphHeuristicInfoAux2[1,i] <- cor.test(data[,i], data[,length(data)], method="pearson")$estimate[[1]] # Calculate the pearson's correlation between features          
        }
      }
      
      # Matrix for final heuristic between subnodes
      if(mode==1){ # Min redundancy
        
        graphHeuristicInfo <- matrix(rep(NA,(length(features)*2)*(length(features)*2)),ncol=length(features)*2, nrow=length(features)*2, dimnames=list(do.call(paste0, expand.grid(features, c(0, 1))),do.call(paste0, expand.grid(features, c(0, 1))))) # The reading is from top to bottom
        for(i in 1:(length(features)*2)){
          for(j in 1:(length(features)*2)){
            
            # Check if the characteristics are the same
            if(i%%length(features)!=0){
              featureI <- features[i%%length(features)]    
            }else{
              featureI <- features[length(features)]     
            }
            if(j%%length(features)!=0){
              featureJ <- features[j%%length(features)]    
            }else{
              featureJ <- features[length(features)]     
            }
            
            if(featureI!=featureJ){ # Not between the same feature
              
              if(j<=length(features)){ # To no selected (zeros)
                if(i%%length(features)!=0){
                  graphHeuristicInfo[j,i] <- abs(graphHeuristicInfoAux[j,i%%length(features)])              
                }else{
                  graphHeuristicInfo[j,i] <- abs(graphHeuristicInfoAux[j,length(features)])       
                }
              }else{  # To selected (ones)
                if(i%%length(features)!=0){
                  if(j%%length(features)!=0){
                    graphHeuristicInfo[j,i] <- 1 - abs(graphHeuristicInfoAux[(j%%length(features)),(i%%length(features))])                  
                  }else{
                    graphHeuristicInfo[j,i] <- 1 - abs(graphHeuristicInfoAux[(length(features)),(i%%length(features))])  
                  }
                }else{
                  if(j%%length(features)!=0){
                    graphHeuristicInfo[j,i] <- 1 - abs(graphHeuristicInfoAux[(j%%length(features)),length(features)])                  
                  }else{
                    graphHeuristicInfo[j,i] <- 1 - abs(graphHeuristicInfoAux[(length(features)),length(features)])  
                  }
                  
                }
                
              }
              
            }
            
          }
        } 
        
      }else if(mode==2){ # Max-relevance and min-redundancy
        
        graphHeuristicInfo <- matrix(rep(NA,(length(features)*2)*(length(features)*2)),ncol=length(features)*2, nrow=length(features)*2, dimnames=list(do.call(paste0, expand.grid(features, c(0, 1))),do.call(paste0, expand.grid(features, c(0, 1))))) # The reading is from top to bottom
        for(i in 1:(length(features)*2)){
          for(j in 1:(length(features)*2)){
            
            # Check if the characteristics are the same
            if(i%%length(features)!=0){
              featureI <- features[i%%length(features)]    
            }else{
              featureI <- features[length(features)]     
            }
            if(j%%length(features)!=0){
              featureJ <- features[j%%length(features)]    
            }else{
              featureJ <- features[length(features)]     
            }
            
            if(featureI!=featureJ){ # Not between the same feature
              
              if(j<=length(features)){ # To no selected (zeros)
                if(i%%length(features)!=0){
                  graphHeuristicInfo[j,i] <- sqrt( abs(graphHeuristicInfoAux[j,i%%length(features)]) * ( 1 - abs(graphHeuristicInfoAux2[j]) ) )
                }else{
                  graphHeuristicInfo[j,i] <- sqrt( abs(graphHeuristicInfoAux[j,length(features)]) * ( 1 - abs(graphHeuristicInfoAux2[j]) ) )
                }
              }else{  # To selected (ones)
                if(i%%length(features)!=0){
                  if(j%%length(features)!=0){
                    graphHeuristicInfo[j,i] <- sqrt( ( 1 - abs(graphHeuristicInfoAux[j%%length(features),i%%length(features)]) ) * ( abs(graphHeuristicInfoAux2[j%%length(features)]) ) )
                  }else{
                    graphHeuristicInfo[j,i] <- sqrt( ( 1 - abs(graphHeuristicInfoAux[length(features),i%%length(features)]) ) * ( abs(graphHeuristicInfoAux2[length(features)]) ) )
                  }
                }else{
                  if(j%%length(features)!=0){
                    graphHeuristicInfo[j,i] <- sqrt( ( 1 - abs(graphHeuristicInfoAux[j%%length(features),length(features)]) ) * ( abs(graphHeuristicInfoAux2[j%%length(features)]) ) )
                  }else{
                    graphHeuristicInfo[j,i] <- sqrt( ( 1 - abs(graphHeuristicInfoAux[length(features),length(features)]) ) * ( abs(graphHeuristicInfoAux2[length(features)]) ) )
                  }
                  
                }
                
              }
              
            }
            
          }
        } 
      }else if(mode==3){ #  feature–feature
        
        graphHeuristicInfo <- matrix(rep(NA,(length(features)*2)*(length(features)*2)),ncol=length(features)*2, nrow=length(features)*2, dimnames=list(do.call(paste0, expand.grid(features, c(0, 1))),do.call(paste0, expand.grid(features, c(0, 1))))) # The reading is from top to bottom
        for(i in 1:(length(features)*2)){
          for(j in 1:(length(features)*2)){
            
            # Check if the characteristics are the same
            if(i%%length(features)!=0){
              featureI <- features[i%%length(features)]    
            }else{
              featureI <- features[length(features)]     
            }
            if(j%%length(features)!=0){
              featureJ <- features[j%%length(features)]    
            }else{
              featureJ <- features[length(features)]     
            }
            
            if(featureI!=featureJ){ # Not between the same feature
              
              if(i<=length(features)&&(j<=length(features))){ # Zero to zero
                graphHeuristicInfo[j,i] <- 1 - abs(graphHeuristicInfoAux2[j])
              }
              
              if(i<=length(features)&&(j>length(features))){ # Zero to one
                graphHeuristicInfo[j,i] <- abs(graphHeuristicInfoAux2[j-length(features)])
              }
              
              if(i>length(features)&&(j<=length(features))){ # One to zero
                graphHeuristicInfo[j,i] <- abs(graphHeuristicInfoAux[i-length(features),j])
              }
              
              if(i>length(features)&&(j>length(features))){ # One to one
                graphHeuristicInfo[j,i] <- 1 - abs(graphHeuristicInfoAux[i-length(features),j-length(features)])
              }
              
            }
            
          }
        }       
      }else{ # based on F-score
        
        # Matrix of general heuristics between features (fscore between i and y)
        graphHeuristicInfoAux3 <- matrix(rep(NA,length(features)),ncol=length(features), nrow=1, dimnames=list("y",features)) 
        for(i in 1:length(features)){
          
          # numerator
          discriminationAmongCategories <- lapply(levels(data[,class.position]), function(x){
            ( mean( data[which(data[,class.position]==x), i] ) - mean(data[,i]) ) ^ 2
          })
          # denominator
          discriminatioWithinEachCategory <- lapply(levels(data[,class.position]), function(x){
            first <- (1 / ((length(data[which(data[,class.position]==x),i])) - 1) )
            
            esima <- data[which(data[,class.position]==x),i]
            media <- mean(data[which(data[,class.position]==x),i])
            second <- lapply(esima, function(x){
              ( x - media ) ^ 2
            })
            
            first*sum(unlist(second))
            
          })
          # fscore
          graphHeuristicInfoAux3[i] <- ( sum( unlist(discriminationAmongCategories) ) ) / ( sum( unlist(discriminatioWithinEachCategory) ) )
        }
        
        graphHeuristicInfo <- matrix(rep(NA,(length(features)*2)*(length(features)*2)),ncol=length(features)*2, nrow=length(features)*2, dimnames=list(do.call(paste0, expand.grid(features, c(0, 1))),do.call(paste0, expand.grid(features, c(0, 1))))) # The reading is from top to bottom
        for(i in 1:(length(features)*2)){
          for(j in 1:(length(features)*2)){
            
            # Check if the characteristics are the same
            if(i%%length(features)!=0){
              featureI <- features[i%%length(features)]    
            }else{
              featureI <- features[length(features)]     
            }
            if(j%%length(features)!=0){
              featureJ <- features[j%%length(features)]    
            }else{
              featureJ <- features[length(features)]     
            }
            
            if(featureI!=featureJ){ # Not between the same feature
              
              if(i<=length(features)&&(j<=length(features))){ # Zero to zero
                graphHeuristicInfo[j,i] <- (1/length(features)) * sum(graphHeuristicInfoAux3)
              }
              
              if(i<=length(features)&&(j>length(features))){ # Zero to one
                graphHeuristicInfo[j,i] <- graphHeuristicInfoAux3[j-length(features)]
              }
              
              if(i>length(features)&&(j<=length(features))){ # One to zero
                graphHeuristicInfo[j,i] <- (1/length(features)) * sum(graphHeuristicInfoAux3)
              }
              
              if(i>length(features)&&(j>length(features))){ # One to one
                graphHeuristicInfo[j,i] <- graphHeuristicInfoAux3[j-length(features)]
              }
              
            }
          }
        }  
        
        
      }
      
      
      return(graphHeuristicInfo)
    }
    
    # Function to initialize the pheromone values between nodes
    initializePheromone <- function(){
      
      # Matrix for final pheromone between subnodes
      graphPheromone <- matrix(rep(NA,(length(features)*2)*(length(features)*2)),ncol=length(features)*2, nrow=length(features)*2, dimnames=list(do.call(paste0, expand.grid(features, c(0, 1))),do.call(paste0, expand.grid(features, c(0, 1))))) # The reading is from top to bottom
      for(i in 1:(length(features)*2)){
        for(j in 1:(length(features)*2)){
          
          # Check if the characteristics are the same
          if(i%%length(features)!=0){
            featureI <- features[i%%length(features)]    
          }else{
            featureI <- features[length(features)]     
          }
          if(j%%length(features)!=0){
            featureJ <- features[j%%length(features)]    
          }else{
            featureJ <- features[length(features)]     
          }
          
          if(featureI!=featureJ){ # Not between the same feature
            
            if(j<=length(features)){ # To no selected (zeros)
              if(i%%length(features)!=0){
                graphPheromone[j,i] <- t0      
              }else{
                graphPheromone[j,i] <- t0      
              }
            }else{  # To selected (ones)
              if(i%%length(features)!=0){
                if(j%%length(features)!=0){
                  graphPheromone[j,i] <- t0                
                }else{
                  graphPheromone[j,i] <- t0 
                }
              }else{
                if(j%%length(features)!=0){
                  graphPheromone[j,i] <- t0                
                }else{
                  graphPheromone[j,i] <- t0
                }
                
              }
              
            }
            
          }
          
        }
      }    
      
      return(graphPheromone)
    }
    
    # Function to generate a solution to an ant
    generateSolutions <- function(graphHeuristicInfo, graphPheromone){
      
      # Nodes visited for the ant
      nodesToVisit <- matrix(rep("not visited",length(features)),ncol=length(features), nrow=1, dimnames=list("nodes",features))
      # Features selected for the ant
      antFeatures <- matrix(rep(NA,length(features)),ncol=length(features), nrow=1, dimnames=list("selection",features))
      
      # Select in a random way the node of the features graph to start the ant
      randomStartFeature <- sample(1:length(features), 1) 
      actualNode <- randomStartFeature
      # Add the node visited to the variables
      nodesToVisit[randomStartFeature] <- "visited"
      antFeatures[randomStartFeature] <- sample(0:1,1) # In a random way
      
      # Road followed by the ant
      road <- matrix(rep(NA,length(features)*2),ncol=length(features)*2, nrow=1, dimnames=list(c("position"),do.call(paste0, expand.grid(features, c(0, 1)))))
      road[(actualNode)+(length(features)*antFeatures[actualNode])] <- 1
      
      # Until all variables are visited...
      for(j in 1:(length(features)-1)){
        
        # New node selection (only nodes not visited before)
        nodeFrom <- actualNode+(length(features)*antFeatures[actualNode])
        heuristic <- graphHeuristicInfo[, nodeFrom]
        pheromone <- graphPheromone[, nodeFrom]    
        
        # Matrix for the values for the probability of transition
        transitionProbability <- matrix(rep(NA,length(features)*2),ncol=length(features)*2, nrow=1, dimnames=list(c("transitionProbability"),do.call(paste0, expand.grid(features, c(0, 1))))) # Generate the values for the probability of transition
        # Nodes not visited
        nodes <- which(nodesToVisit=="not visited") 
        
        for(i in nodes){ # Generates the values of the transition functions
          
          heuristicsZeros <- apply( as.matrix(heuristic[1:length(features)]), 1, function(x){x^b}) # To zeros
          pheromonesZeros <- apply( as.matrix(pheromone[1:length(features)]), 1, function(x){x^a}) # To zeros
          heuristicsOnes <- apply( as.matrix(heuristic[(length(features)+1):(length(features)*2)]), 1, function(x){x^b}) # To ones
          pheromonesOnes <- apply( as.matrix(pheromone[(length(features)+1):(length(features)*2)]), 1, function(x){x^a}) # To ones     
          
          transitionProbability[i] <- ( (heuristic[i]^b)*(pheromone[i]^a) ) / ( sum(heuristicsZeros*pheromonesZeros, na.rm=TRUE) + sum(heuristicsOnes*pheromonesOnes, na.rm=TRUE))
          transitionProbability[i+length(features)] <- ( (heuristic[i+length(features)]^b)*(pheromone[i+length(features)]^a) ) / ( sum(heuristicsZeros*pheromonesZeros, na.rm=TRUE) + sum(heuristicsOnes*pheromonesOnes, na.rm=TRUE))
        }
        
        # Selects the node with the highest probability
        maxProbability <- which(transitionProbability==max(transitionProbability, na.rm=TRUE))
        if(length(maxProbability)>1){
          maxProbability <- sample(maxProbability, 1)
        }
        nextFeatureAux <- colnames(transitionProbability)[maxProbability]
        nextFeature <- substr(nextFeatureAux, 0, nchar(nextFeatureAux)-1)
        nextValue <- as.integer(substr(nextFeatureAux, nchar(nextFeatureAux), nchar(nextFeatureAux)))
        
        # Adds the node to the list of visited nodes
        nodesToVisit[which(colnames(nodesToVisit)==nextFeature)] <- "visited"
        antFeatures[which(colnames(nodesToVisit)==nextFeature)] <- nextValue  
        
        # Updates the current node
        actualNode <- which(colnames(nodesToVisit)==nextFeature)
        road[(actualNode)+(length(features)*antFeatures[actualNode])] <- (j+1)
      }
      
      return(list(antFeatures,road))
    }
    
    # Function to generate the fitness of each ant
    generateFitness <- function(ants){
      
      fitness <- rep(NA, population)
      
      for(i in 1:nrow(ants)){
        fitness[i] <- featureSetEval(data, class, features[which(ants[i,]==1)])
      }
      
      return(fitness)
    }
    
    # Function to update the pheromone level
    pheromoneUpdate <- function(graphPheromone, road, cost){
      
      # Pheromone evaporation
      for(i in 1:(length(features)*2)){
        for(j in 1:(length(features)*2)){
          
          if(!is.na(graphPheromone[j,i])){
            graphPheromone[j,i] <- ((1-p)*graphPheromone[j,i])
          }
          
        }
      }
      
      # Pheromone deposition
      for(i in 1:(length(features)-1)){
        iniAux <- names(road)[which(road==i)]
        endAux <- names(road)[which(road==(i+1))]
        
        ini <- which(colnames(graphPheromone)==iniAux)
        end <- which(rownames(graphPheromone)==endAux)
        
        if(max){
          graphPheromone[end,ini] <- graphPheromone[end, ini] + (cost/q)        
        }else{
          graphPheromone[end,ini] <- graphPheromone[end,ini] + (q/cost)
        }
        
      }
      
      # Pheromone between limits (max-min)
      for(i in 1:(length(features)*2)){
        for(j in 1:(length(features)*2)){
          
          if(!is.na(graphPheromone[j,i])){
            if(graphPheromone[j,i] > tmax){
              graphPheromone[j,i] <- tmax
            }else if(graphPheromone[j,i] < tmin){
              graphPheromone[j,i] <- tmin
            }
          }
          
        }
      }
      
      return(graphPheromone)
    }
    
    
    ## PARAMETERS INITIALIZATION
    
    # Ants population
    ants <- matrix(rep(NA,population*length(features)),ncol=length(features), nrow=population, dimnames=list(paste0("Ant", 1:population),features))
    antsRoads <- matrix(rep(NA,population*length(features)*2),ncol=length(features)*2, nrow=population, dimnames=list(paste0("Ant", 1:population),do.call(paste0, expand.grid(features, c(0, 1)))))
    # Initialize heuristic between nodes
    graphHeuristic <- initializeHeuristic(mode)
    # Initialize pheromone between nodes
    graphPheromone <- initializePheromone()
    
    # Initialize the list with results
    res <- list(NULL)
    res[[1]] <- NA
    res[[2]] <- NA
    res[[3]] <- list(NULL)
    res[[4]] <- list(NULL)
    names(res) <- c("bestFeatures","bestFitness","antsIter","pheromoneIter") 
    
    
    ## ITERATIONS
    
    # In each iteration of the algorithm...
    for(i in 1:iter){
      
      # Generate candidate solutions
      for(j in 1:population){
        aux <- generateSolutions(graphHeuristic, graphPheromone)
        ants[j,] <- aux[[1]]
        antsRoads[j,] <- aux[[2]]
      }
      
      # Calculate the fitness of each ants
      antsFitness <- rep(NA, population)
      antsFitness <- generateFitness(ants)
      
      
      # The ant with the best fitness
      if(max){ # For maximization
        bestAnt <- which(antsFitness==max(antsFitness))
        if(length(bestAnt)>1){
          bestAnt <- sample(bestAnt, 1)
        }
        # Save the best fitness
        if(i==1){
          bestFeatures <- ants[bestAnt,]
          bestFitness <- max(antsFitness)
        }else{
          if(max(antsFitness)>bestFitness){
            bestFeatures <- ants[bestAnt,]
            bestFitness <- max(antsFitness)
          }
        }      
      }else{ # For minimization
        bestAnt <- which(antsFitness==min(antsFitness))
        if(length(bestAnt)>1){
          bestAnt <- sample(bestAnt, 1)
        }
        # Save the best fitness
        if(i==1){
          bestFeatures <- ants[bestAnt,]
          bestFitness <- min(antsFitness)
        }else{
          if(min(antsFitness)<bestFitness){
            bestFeatures <- ants[bestAnt,]
            bestFitness <- min(antsFitness)
          }
        }       
      }
      
      
      # Pheromones actualization and evaporation
      if(max){
        graphPheromone <- pheromoneUpdate(graphPheromone, antsRoads[bestAnt,], max(antsFitness))
      }else{
        graphPheromone <- pheromoneUpdate(graphPheromone, antsRoads[bestAnt,], min(antsFitness))
      }
      
      
      if(verbose){
        if(max){
          cat(paste("ACO | iter = ",i," | BestIterAnt = ",paste(bestFeatures,collapse="")," | BestIterFitness = ",max(antsFitness), " | MeanIterFitness", mean(antsFitness)," | BestGlobalAnt",bestFitness," \n")) 
        }else{
          cat(paste("ACO | iter = ",i," | BestIterAnt = ",paste(bestFeatures,collapse="")," | BestIterFitness = ",min(antsFitness), " | MeanIterFitness", mean(antsFitness)," | BestGlobalAnt",bestFitness," \n")) 
        }
      }
      
      # Save the population
      res[[3]][[i]] <- cbind(ants,antsFitness)
      names(res$antsIter)[[i]] <- paste("Iter",i,sep="")
      # Save the pheromone
      res[[4]][[i]] <- graphPheromone
      names(res$pheromoneIter)[[i]] <- paste("Iter",i,sep="")    
      
    }
    
    res[[1]] <- bestFeatures
    res[[2]] <- bestFitness
    
    # Modify the output
    res[[1]] <- matrix(res[[1]], nrow=1, byrow=FALSE, dimnames=list(c(),features))
    
    return(res)
  }
  
  attr(antColonySearch,'shortName') <- "aco"
  attr(antColonySearch,'name') <- "Ant Colony Optimization"
  
  return(antColonySearch)
}
