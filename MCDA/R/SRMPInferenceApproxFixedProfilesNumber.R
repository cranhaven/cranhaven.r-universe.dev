#' Approximative inference of an SRMP model given the number of reference
#' profiles
#' 
#' Approximative inference approach from pairwise comparisons of alternatives
#' for the SRMP ranking model. This method outputs an SRMP model that fulfils
#' as many pairwise comparisons as possible. The number of reference profiles
#' is fixed beforehand, however the algorithm will explore any lexicographic
#' order between them.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria. "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param profilesNumber The number of reference profiles of the SRMP model.
#' @param preferencePairs A two column matrix containing on each row a pair of
#' alternative names where the first alternative is considered to be strictly
#' preferred to the second.
#' @param indifferencePairs A two column matrix containing on each row a pair
#' of alternative names the two alternatives are considered to indifferent with
#' respect to each other.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @param timeLimit Allows to fix a time limit of the execution, in seconds
#' (default 60).
#' @param populationSize Allows to change the size of the population used by
#' the genetic algorithm (default 20).
#' @param mutationProb Allows to change the mutation probability used by the
#' genetic algorithm (default 0.1).
#' @return The function returns a list containing: \item{criteriaWeights}{The
#' inferred criteria weights.} \item{referenceProfiles}{The inferred reference
#' profiles.} \item{lexicographicOrder}{The inferred lexicographic order of the
#' reference profiles.} \item{fitness}{The percentage of fulfilled pair-wise
#' relations.}
#' @references A-L. OLTEANU, V. MOUSSEAU, W. OUERDANE, A. ROLLAND, Y. ZHENG,
#' Preference Elicitation for a Ranking Method based on Multiple Reference
#' Profiles, forthcoming 2018.
#' @keywords methods
#' @examples
#' 
#' \donttest{
#' # the performance table
#' 
#' performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
#'                           c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
#'                           c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
#'                           c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))
#' 
#' criteriaMinMax <- c("max","max","max")
#' 
#' rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11",
#'                                 "a12","a13","a14","a15","a16","a17","a18","a19","a20",
#'                                 "a21","a22","a23","a24")
#' 
#' colnames(performanceTable) <- c("c1","c2","c3")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' # expected result for the tests below
#' 
#' expectedpreorder <- list("a16","a13",c("a3","a9"),"a14",c("a1","a7"),"a15")
#' 
#' # test - preferences and indifferences
#' 
#' preferencePairs <- matrix(c("a16","a13","a3","a14","a17","a1","a18","a15","a2","a11",
#'                             "a5","a10","a4","a12","a13","a3","a14","a17","a1","a18",
#'                             "a15","a2","a11","a5","a10","a4","a12","a6"),14,2)
#' indifferencePairs <- matrix(c("a3","a1","a2","a11","a11","a20","a10","a10","a19","a12",
#'                               "a12","a21","a9","a7","a8","a20","a22","a22","a19","a24",
#'                               "a24","a21","a23","a23"),12,2)
#' 
#' set.seed(1)
#' 
#' result<-SRMPInferenceApproxFixedProfilesNumber(performanceTable, criteriaMinMax, 3,
#'                                                preferencePairs, indifferencePairs,
#'                                                alternativesIDs = c("a1","a3","a7","a9",
#'                                                "a13","a14","a15","a16"))
#' 
#' }
#' @export SRMPInferenceApproxFixedProfilesNumber
SRMPInferenceApproxFixedProfilesNumber <- function(performanceTable, criteriaMinMax, profilesNumber, preferencePairs, indifferencePairs = NULL, alternativesIDs = NULL, criteriaIDs = NULL, timeLimit = 60, populationSize = 20, mutationProb = 0.1){
  
  ## check the input data
  
  if (!(is.matrix(performanceTable) || is.data.frame(performanceTable))) 
    stop("performanceTable should be a matrix or a data frame")
  
  if(is.null(colnames(performanceTable)))
    stop("performanceTable columns should be named")
  
  if (!is.matrix(preferencePairs) || is.data.frame(preferencePairs)) 
    stop("preferencePairs should be a matrix or a data frame")
  
  if (!(is.null(indifferencePairs) || is.matrix(indifferencePairs) || is.data.frame(indifferencePairs))) 
    stop("indifferencePairs should be a matrix or a data frame")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if(!all(sort(colnames(performanceTable)) == sort(names(criteriaMinMax))))
    stop("criteriaMinMax should be named as the columns of performanceTable")
  
  if (!(is.numeric(profilesNumber)))
    stop("profilesNumber should be numberic")
  
  profilesNumber <- as.integer(profilesNumber)
  
  if (!(is.null(timeLimit)))
  {
    if(!is.numeric(timeLimit))
      stop("timeLimit should be numeric")
    if(timeLimit <= 0)
      stop("timeLimit should be strictly positive")
  }
  
  if (!(is.null(populationSize)))
  {
    if(!is.numeric(populationSize))
      stop("populationSize should be numeric")
    if(populationSize < 10)
      stop("populationSize should be at least 10")
  }
  
  if (!(is.null(mutationProb)))
  {
    if(!is.numeric(mutationProb))
      stop("mutationProb should be numeric")
    if(mutationProb < 0 || mutationProb > 1)
      stop("mutationProb should be between 0 and 1")
  }

  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if(dim(preferencePairs)[2] != 2)
    stop("preferencePairs should have two columns")
  
  if(!is.null(indifferencePairs))
    if(dim(indifferencePairs)[2] != 2)
      stop("indifferencePairs should have two columns")
  
  if (!(profilesNumber > 0))
    stop("profilesNumber should be strictly pozitive")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    preferencePairs <- preferencePairs[(preferencePairs[,1] %in% alternativesIDs) & (preferencePairs[,2] %in% alternativesIDs),]
    if(dim(preferencePairs)[1] == 0)
      preferencePairs <- NULL
    if(!is.null(indifferencePairs))
    {
      indifferencePairs <- indifferencePairs[(indifferencePairs[,1] %in% alternativesIDs) & (indifferencePairs[,2] %in% alternativesIDs),]
      if(dim(indifferencePairs)[1] == 0)
        indifferencePairs <- NULL
    }
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  if (is.null(dim(preferencePairs))) 
    stop("preferencePairs is empty or the provided alternativesIDs have filtered out everything from within")
  
  # -------------------------------------------------------
  
  numAlt <- dim(performanceTable)[1]
  numCrit <- dim(performanceTable)[2]
  minEvaluations <- apply(performanceTable, 2, min)
  maxEvaluations <- apply(performanceTable, 2, max)
  
  # -------------------------------------------------------
  
  outranking <- function(alternativePerformances1, alternativePerformances2, profilePerformances, criteriaWeights, lexicographicOrder, criteriaMinMax){
    for (k in lexicographicOrder)
    {
      weightedSum1 <- 0
      weightedSum2 <- 0
      for (i in 1:numCrit)
      {
        if (criteriaMinMax[i] == "min")
        {
          if (alternativePerformances1[i] %<=% profilePerformances[k,i])
            weightedSum1 <- weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[i] %<=% profilePerformances[k,i])
            weightedSum2 <- weightedSum2 + criteriaWeights[i]
        }
        else
        {
          if (alternativePerformances1[i] %>=% profilePerformances[k,i])
            weightedSum1 <- weightedSum1 + criteriaWeights[i]
          if (alternativePerformances2[i] %>=% profilePerformances[k,i])
            weightedSum2 <- weightedSum2 + criteriaWeights[i]
        }
      }
      
      # can we differentiate between the two alternatives?
      if(weightedSum1 > weightedSum2)
        return(1)
      else if(weightedSum1 < weightedSum2)
        return(-1)
    }
    # could not differentiate between the alternatives
    return(0)
  }
  
  InitializePopulation <- function(){
    population <- list()
    for(i in 1:populationSize)
    {
      values <- c(0,sort(runif(numCrit-1,0,1)),1)
      weights <- sapply(1:numCrit, function(i) return(values[i+1]-values[i]))
      names(weights) <- colnames(performanceTable)
      
      profiles <- NULL
      for(j in 1:numCrit)
      {
        if(criteriaMinMax[j] == 'max')
          profiles <- cbind(profiles,sort(runif(profilesNumber,minEvaluations[j],maxEvaluations[j])))
        else
          profiles <- cbind(profiles,sort(runif(profilesNumber,minEvaluations[j],maxEvaluations[j]), decreasing = TRUE))
      }
      colnames(profiles) <- colnames(performanceTable)
      
      lexicographicOrder <- sample(1:profilesNumber, profilesNumber)
      
      population[[length(population)+1]] <- list(criteriaWeights = weights, referenceProfiles = profiles, lexicographicOrder = lexicographicOrder)
    }
    return(population)
  }
  
  Fitness <- function(individual){
    total <- 0
    ok <- 0
    for (i in 1:dim(preferencePairs)[1]){
      comparison <- outranking(performanceTable[preferencePairs[i,1],],performanceTable[preferencePairs[i,2],],individual$referenceProfiles, individual$criteriaWeights, individual$lexicographicOrder, criteriaMinMax)
      if(comparison == 1)
        ok <- ok + 1
      total <- total + 1
    }
    if(!is.null(indifferencePairs))
      for (i in 1:dim(indifferencePairs)[1]){
        comparison <- outranking(performanceTable[indifferencePairs[i,1],],performanceTable[indifferencePairs[i,2],],individual$referenceProfiles, individual$criteriaWeights, individual$lexicographicOrder, criteriaMinMax)
        if(comparison == 0)
          ok <- ok + 1
        total <- total + 1
      }
    return(ok/total)
  }
  
  Reproduce <- function(parents){
    children <- list()
    
    numPairs <- as.integer(length(parents)/2)
    
    pairings <- matrix(sample(1:length(parents),numPairs*2),numPairs,2)
    
    for(i in 1:numPairs)
    {
      parent1 <- parents[[pairings[i,1]]]
      
      parent2 <- parents[[pairings[i,2]]]
      
      # crossover bewtween profiles
      
      criteria <- sample(colnames(performanceTable), numCrit)
      
      pivot <- runif(1,1,numCrit - 1)
      
      profiles1 <- matrix(rep(0,numCrit*profilesNumber),profilesNumber,numCrit)
      profiles2 <- matrix(rep(0,numCrit*profilesNumber),profilesNumber,numCrit)
      
      colnames(profiles1) <- colnames(performanceTable)
      colnames(profiles2) <- colnames(performanceTable)
      
      for(k in 1:profilesNumber)
        for(j in 1:numCrit)
        {
          if(j <= pivot)
          {
            profiles1[k,criteria[j]] <- parent1$referenceProfiles[k,criteria[j]]
            profiles2[k,criteria[j]] <- parent2$referenceProfiles[k,criteria[j]]
          }
          else
          {
            profiles1[k,criteria[j]] <- parent2$referenceProfiles[k,criteria[j]]
            profiles2[k,criteria[j]] <- parent1$referenceProfiles[k,criteria[j]]
          }
        }
      
      # child identical to first parent - will get mutated in the second step
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent1$referenceProfiles, lexicographicOrder = parent1$lexicographicOrder)
      
      # child identical to second parent
      
      children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent2$lexicographicOrder)
      
      # child takes weights from first parent and profiles from second
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from second parent and profiles from first
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from first parent and profiles from first crossover
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = profiles1, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from first parent and profiles from second crossover
      
      children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = profiles2, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from second parent and profiles from first crossover
      
      children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = profiles1, lexicographicOrder = parent1$lexicographicOrder)
      
      # child takes weights from second parent and profiles from second crossover
      
      children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = profiles2, lexicographicOrder = parent1$lexicographicOrder)
      
      # if the lexicographic orders are different then we add more children
      
      if(!all(parent1$lexicographicOrder == parent2$lexicographicOrder))
      {
        children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = parent2$referenceProfiles, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = profiles1, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent1$criteriaWeights, referenceProfiles = profiles2, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = profiles1, lexicographicOrder = parent2$lexicographicOrder)
        
        children[[length(children)+1]] <- list(criteriaWeights = parent2$criteriaWeights, referenceProfiles = profiles2, lexicographicOrder = parent2$lexicographicOrder)
      }
    }
    
    # mutate children
    
    numChildren <- length(children)
    
    for(i in 1:numChildren)
    {
      for(k in 1:profilesNumber)
      {
        for(criterion in colnames(performanceTable))
        {
          if(runif(1,0,1) < mutationProb)
          {
            # mutate profile evaluation
            
            maxVal <- maxEvaluations[criterion]
            
            minVal <- minEvaluations[criterion]
            
            if(k < profilesNumber)
            {
              if(criteriaMinMax[criterion] == 'max')
                maxVal <- children[[i]]$referenceProfiles[k+1,criterion]
              else
                minVal <- children[[i]]$referenceProfiles[k+1,criterion]
            }
            
            if(k > 1)
            {
              if(criteriaMinMax[criterion] == 'max')
                minVal <- children[[i]]$referenceProfiles[k-1,criterion]
              else
                maxVal <- children[[i]]$referenceProfiles[k-1,criterion]
            }
            
            children[[i]]$referenceProfiles[k,criterion] <- runif(1,minVal,maxVal)
          }
        }
      }
      
      for(j1 in 1:(numCrit-1))
      {
        for(j2 in (j1+1):numCrit)
        {
          if(runif(1,0,1) < mutationProb)
          {
            # mutate two criteria weights
            
            criteria <- c(colnames(performanceTable)[j1],colnames(performanceTable)[j2])
            
            minVal <- 0 - children[[i]]$criteriaWeights[criteria[1]]
            
            maxVal <- children[[i]]$criteriaWeights[criteria[2]]
            
            tradeoff <- runif(1,minVal,maxVal)
            
            children[[i]]$criteriaWeights[criteria[1]] <- children[[i]]$criteriaWeights[criteria[1]] + tradeoff
            
            children[[i]]$criteriaWeights[criteria[2]] <- children[[i]]$criteriaWeights[criteria[2]] - tradeoff
          }
        }
      }
      
      if(runif(1,0,1) < mutationProb && profilesNumber > 1)
      {
        # mutate the lexicographic order
        
        i1 <- sample(1:profilesNumber, 1)
        
        adjacent <- NULL
        
        if(i1 > 1)
          adjacent <- c(adjacent, i1 - 1)
        
        if(i1 < profilesNumber)
          adjacent <- c(adjacent, i1 + 1)
        
        i2 <- sample(adjacent, 1)
        
        temp <- children[[i]]$lexicographicOrder[i1]
        
        children[[i]]$lexicographicOrder[i1] <- children[[i]]$lexicographicOrder[i2]
        
        children[[i]]$lexicographicOrder[i2] <- temp
      }
    }
    
    return(children)
  }
  
  # -------------------------------------------------------
  
  startTime <- Sys.time()
  
  # Initialize population
  
  population <- InitializePopulation()
  
  bestIndividual <- list(fitness = 0)
  
  # Main loop
  
  ct <- 0
  
  while(as.double(difftime(Sys.time(), startTime, units = 'secs')) < timeLimit)
  {
    # Evaluate population
    
    evaluations <- unlist(lapply(population, Fitness))
    
    # Store best individual if better than the overall best
    
    maxFitness <- max(evaluations)
    
    if(maxFitness > bestIndividual$fitness)
    {
      bestIndividual <- population[[match(maxFitness,evaluations)]]
      
      bestIndividual$fitness <- maxFitness
    }
    
    # report
    
    if(as.double(difftime(Sys.time(), startTime, units = 'secs')) / 5 > ct)
    {
      ct <- ct + 1
      
      # print(sprintf("Best fitness so far: %6.2f%%", bestIndividual$fitness * 100))
    }
    
    # check if we are done
    
    if(bestIndividual$fitness == 1)
      break
    
    # Selection - not the first iteration
    
    if(length(population) > populationSize)
    {
      evaluations <- evaluations^2
      
      newPopulation <- list()
      
      i <- 1
      
      while(length(newPopulation) < populationSize)
      {
        if(runif(1,0,1) <= evaluations[i])
        {
          evaluations[i] <- -1
          
          newPopulation[[length(newPopulation)+1]] <- population[[i]]
        }
        
        i <- i + 1
        
        if(i > length(population))
          i <- 1
      }
      
      population <- newPopulation
    }
    
    # Reproduction
    
    population <- Reproduce(population)
  }
  
  # print(sprintf("Final model fitness: %6.2f%%", bestIndividual$fitness * 100))
  
  return(bestIndividual)
}
