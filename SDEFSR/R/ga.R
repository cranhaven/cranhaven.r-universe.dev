##############################################################################
#                                                                            #
#                        GENETIC ALGORITHMS in R                             #
#                                                                            #      
#       This functions are the genectic algorithms used by the EFS           #
#             algorithms that are available in the package.                  #
#                                                                            #
##############################################################################

#' @title Genetic algorithm for SDIGA
#' 
#' @description This method execute the genetic algorithm for the SDIGA algorithm
#' 
#' @param type type of representation used. For DNF rules is "binary", for CAN rules, "Real-valued"
#' @param fitness The fitness function that calculate the quality function of each individual of the population
#' @param ... Additional parameters for the fitness function
#' @param min A vector with the minimum value for each variable
#' @param max A vector with the maximum value for each variable
#' @param nBits For DNF Rules, a number that specify the length of a chromosome
#' @param population The method that generate the initial population
#' @param crossover The crossover function
#' @param mutation The mutation operator
#' @param popSize The size of the population
#' @param pcrossover The probability of crossover, in [0,1]
#' @param pmutation The probability of mutation of a gene, in [0,1]
#' @param elitism A number that specifiy the number of elite individuals
#' @param maxiter The number of evaluations to perform
#' @param run The number of evaluations to perform
#' @param maxfitness The maximum value of the fitness function, if reached, the GA stops.
#' @param names Names of the parameters
#' @param suggestions A vector that indicate possible initial individuals
#' @param keepBest Logical, keep the best individual ? 
#' @param parallel perform calculation of the fitness in parallel
#' @param monitor Show intermediate results
#' @param DNFRules Logical indicating if DNFRules are used
#' @param seed The random number generator seed
#' 
#' 
#' @details The genetic algorithm used in this method is a mono-objective genetic algorithm with a fitness function
#'     equal to the weighted mean of the selected objectives. It uses a stationary model when only the best individuals are
#'     chosen to cross. 
#'     
#'     The mutation operator used here is a special one where it selects randomly to types of work: Eliminate the variable
#'     or change its value.
#'  
#' @noRd
#' 
.gaSDIGA <- function(type = c("binary", "real-valued", "permutation"), 
               fitness, ...,
               min, max, nBits,
               population ,
               selection,
               crossover, 
               mutation,
               popSize = 50, 
               pcrossover = 0.8, 
               pmutation = 0.1, 
               elitism = base::max(1, round(popSize*0.05)), 
               maxiter = 100,
               run = maxiter,
               maxfitness = Inf,
               names = NULL,
               suggestions = NULL, 
               keepBest = FALSE,
               parallel = FALSE,
               monitor = NULL,
               DNFRules = FALSE,
               seed = NULL) 
{
  
  call <- match.call()
  
  type <- match.arg(type)

  
  if(missing(fitness))
  { stop("A fitness function must be provided") }
  if(!is.function(fitness)) 
  { stop("A fitness function must be provided") }
  if(popSize < 10) 
  { warning("The population size is less than 10.") }
  if(maxiter < 1) 
  { stop("The maximum number of iterations must be at least 1.") }
  if(elitism > popSize) 
  { stop("The elitism cannot be larger that population size.") }
  if(pcrossover < 0 | pcrossover > 1)
  { stop("Probability of crossover must be between 0 and 1.") }
  if(is.numeric(pmutation))
  { if(pmutation < 0 | pmutation > 1)
  { stop("If numeric probability of mutation must be between 0 and 1.") }
  else if(!is.function(population))
  { stop("pmutation must be a numeric value in (0,1) or a function.") }
  }
  if(missing(min) & missing(max) & missing(nBits))
  { stop("A min and max range of values (for 'real-valued' or 'permutation' GA) or nBits (for 'binary' GA) must be provided!") }
  
  switch(type, 
         "binary"      = { nBits <- as.vector(nBits)[1]
                           #min <- max <- NA
                           nvars <- nBits 
         },
         "real-valued" = { min <- as.vector(min)
                           max <- as.vector(max)
                           nBits <- NA
                           if(length(min) != length(max))
                           { stop("min and max must be vector of the same length!") }
                           nvars <- length(max) 
         },
         "permutation" = { min <- as.vector(min)[1]
                           max <- as.vector(max)[1]
                           nBits <- NA
                           nvars <- length(seq(min,max)) 
         }
  )
  
 
  
  # Preallocate memory for fitness values. 
  # +2 is for the two individuals generated by the genetic operators 
  Fitness <- rep(NA, popSize + 2)
  
  
  # Declare a new '.ga' object to store all results
  object <- new(".ga", 
                call = call, 
                type = type,
                min = min, 
                max = max, 
                nBits = nBits, 
                names = if(is.null(names)) character() else names,
                popSize = popSize,
                iter = 0, 
                run = 1, 
                maxiter = maxiter,
                suggestions = matrix(),
                population = matrix(), 
                elitism = elitism, 
                pcrossover = pcrossover, 
                pmutation = if(is.numeric(pmutation)) pmutation else NA,
                fitness = Fitness, 
                summary = matrix(),
                bestSol = list()
                )
  n_evals <- 0
  if(!is.null(seed)) set.seed(seed)
  
  #Precalculte the number of genes in the population to optimize operations
  if(!DNFRules)
    nGenes <- nvars * popSize
  else
    nGenes <- (length(max) - 1) * popSize
  
  # Precalculate the number of mutations that will be executed
  numMutations <- ceiling(pmutation * nGenes)
  
  
  object@DNFRules <- DNFRules
  object@maxValuesRule <- max 
  object@popNew <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  
  # generate beginning population
  Pop <- matrix(as.double(NA), nrow = popSize + 2, ncol = nvars)
  
   if( ! DNFRules) 
    Pop[1:popSize,] <- population(object)[1:popSize,]
    else 
    Pop[1:popSize,] <- population(object)[1:popSize,]
  
  object@population <- Pop
  
  
  # start iterations (GA general cycle)
  for(iter in seq_len(maxiter))
  {
    # evalute fitness function (when needed) 
   for(i in seq_len(popSize + 2)){
      if(is.na(Fitness[i])){ 
        Fitness[i] <- fitness(Pop[i,], ...) 
        n_evals <- n_evals + 1
        } 
   }



# update objectwith the information of the evaluation
object@iter <- iter
object@population <- Pop
object@fitness <- Fitness


#Keep the population sorted by fitness
ord <- order(Fitness, decreasing = TRUE)
PopSorted <- Pop[ord,,drop=FALSE]
FitnessSorted <- Fitness[ord]

object@population <- PopSorted
object@fitness <- FitnessSorted


# check stopping criteria

if(n_evals >= run) break  
if(max(Fitness, na.rm = TRUE) >= maxfitness) break
if(object@iter == maxiter) break  

# Apply genetic operators:
# selection
sel <- selection(object)
PopSorted <- sel$population
FitnessSorted <- sel$fitness

object@population <- PopSorted
object@fitness <- FitnessSorted


# crossover: Only cross the 2 best individuals

parents <- c(1,2) #As the population is sorted by fitness it is not neccessary to find the two best individuals
Crossover <- crossover(object, parents) # Only the best individuals are crossed
PopSorted[popSize + parents,] <- Crossover$children
FitnessSorted[popSize + parents] <- Crossover$fitness

# mutation (only mutate popLength * probMut chromosomes)
pm <- if(is.function(pmutation)) pmutation(object) else pmutation
if(DNFRules) nvars <- length(max) - 1
if(is.function(mutation) & pm > 0)
{ 
  # Select numMutations genes randomly 
  genes <- sample(x = seq_len(nGenes), size = numMutations, replace = TRUE)
  
  #Determine the chromosomes that those genes belongs to
  cromosomes <- floor(genes / nvars) + 1
  #Also, determine the variable number
  vars <- (cromosomes %% nvars) + 1
  #Preallocate memory to store the mutated chromosomes
  if(!DNFRules)
    Mutation <- matrix(data = NA, nrow = numMutations, ncol = nvars)
  else
    Mutation <- matrix(data = NA, nrow = numMutations, ncol = nBits)
  # Those chromoses are new ones, so the fitness value is NA because we need to re-evaluate those chromosomes
  FitnessSorted[cromosomes] <- NA
  
  #Apply the mutation operator
  for(i in seq_len(length(vars))) 
  {     
    Mutation[i,] <- mutation(object, cromosomes[i], vars[i])
  }
  PopSorted[cromosomes,] <- Mutation 
  
  object@population <- PopSorted
  object@fitness <- FitnessSorted
}

Pop <- PopSorted
Fitness <- FitnessSorted

}


# get solution(s): The best individual
object@fitnessValue <- max(object@fitness, na.rm = TRUE)

# return an object of class 'ga'
return(object)
}




















#' @title Genetic algorithm for MESDIF
#' 
#' @description This method execute the genetic algorithm for the MESDIF algorithm
#' 
#' @param type type of representation used. For DNF rules is "binary", for CAN rules, "Real-valued"
#' @param fitness The fitness function that calculate the quality function of each individual of the population
#' @param ... Additional parameters for the fitness function
#' @param min A vector with the minimum value for each variable
#' @param max A vector with the maximum value for each variable
#' @param nBits For DNF Rules, a number that specify the length of a chromosome
#' @param population The method that generate the initial population
#' @param crossover The crossover function
#' @param mutation The mutation operator
#' @param popSize The size of the population
#' @param pcrossover The probability of crossover, in [0,1]
#' @param pmutation The probability of mutation of a gene, in [0,1]
#' @param elitism A number that specifiy the number of elite individuals
#' @param maxiter The number of evaluations to perform
#' @param run The number of evaluations to perform
#' @param maxfitness The maximum value of the fitness function, if reached, the GA stops.
#' @param names Names of the parameters
#' @param suggestions A vector that indicate possible initial individuals
#' @param keepBest Logical, keep the best individual ? 
#' @param parallel perform calculation of the fitness in parallel
#' @param monitor Show intermediate results
#' @param DNFRules Logical indicating if DNFRules are used
#' @param seed The random number generator seed
#' 
#' 
#' @details The genetic algorithm used in this method is a multi-objective genetic algorithm based on the 
#'     SPEA2 approach where an elite population is used along the whole evolutive process. 
#'     At the end of the process, the rules stored in the elite population where returned as result.  
#'     The multi-objective approach is based on a niches schema based on the dominance in the Pareto front.
#'     This schema allows to find non-dominated individuals to fill the elite population, that has a fixed size.
#'  
#' @noRd
#' 
.gaMESDIF <- function(type = c("binary", "real-valued", "permutation"), 
               fitness, ...,
               min, max, nBits,
               population ,
               selection ,
               crossover , 
               mutation ,
               popSize = 50, 
               pcrossover = 0.8, 
               pmutation = 0.1, 
               elitism = base::max(1, round(popSize*0.05)), 
               maxiter = 100,
               run = maxiter,
               maxfitness = Inf,
               names = NULL,
               suggestions = NULL, 
               keepBest = FALSE,
               parallel = FALSE,
               monitor = NULL,
               DNFRules = FALSE,
               seed = NULL) 
{
  
  call <- match.call()
  
  type <- match.arg(type)

  
  if(missing(fitness))
  { stop("A fitness function must be provided") }
  if(!is.function(fitness)) 
  { stop("A fitness function must be provided") }
  if(popSize < 10) 
  { warning("The population size is less than 10.") }
  if(maxiter < 1) 
  { stop("The maximum number of iterations must be at least 1.") }
  if(elitism > popSize) 
  { stop("The elitism cannot be larger that population size.") }
  if(pcrossover < 0 | pcrossover > 1)
  { stop("Probability of crossover must be between 0 and 1.") }
  if(is.numeric(pmutation))
  { if(pmutation < 0 | pmutation > 1)
  { stop("If numeric probability of mutation must be between 0 and 1.") }
  else if(!is.function(population))
  { stop("pmutation must be a numeric value in (0,1) or a function.") }
  }
  if(missing(min) & missing(max) & missing(nBits))
  { stop("A min and max range of values (for 'real-valued' or 'permutation' GA) or nBits (for 'binary' GA) must be provided!") }
  
  switch(type, 
         "binary"      = { nBits <- as.vector(nBits)[1]
                           #min <- max <- NA
                           nvars <- nBits 
         },
         "real-valued" = { min <- as.vector(min)
                           max <- as.vector(max)
                           nBits <- NA
                           if(length(min) != length(max))
                           { stop("min and max must be vector of the same length!") }
                           nvars <- length(max) 
         },
         "permutation" = { min <- as.vector(min)[1]
                           max <- as.vector(max)[1]
                           nBits <- NA
                           nvars <- length(seq(min,max)) 
         }
  )
  
  
 
  #Define Fitness as a matrix, in MESDIF, each objective value is evaluated individually
  Fitness <- matrix(NA, nrow = popSize + elitism, ncol = 4)
  
  #This counts the number of indivuals that are dominated by this one
  Dominated <- numeric(popSize + elitism)
  
  
  object <- new(".ga", 
                call = call, 
                type = type,
                min = min, 
                max = max, 
                nBits = nBits, 
                names = if(is.null(names)) character() else names,
                popSize = popSize,
                iter = 0, 
                run = 1, 
                maxiter = maxiter,
                suggestions = matrix(),
                population = matrix(), 
                elitism = elitism, 
                pcrossover = pcrossover, 
                pmutation = if(is.numeric(pmutation)) pmutation else NA,
                fitness = Fitness, 
                summary = matrix(),
                bestSol = list())
  
  #This GA runs until a number of evaluations is reached, not iterations !
  n_evals <- 0
  if(!is.null(seed)) set.seed(seed)
  
  #Compute the number of genes, the mutation probability is applied over the gene.
  if(!DNFRules)
    nGenes <- nvars * popSize
  else
    nGenes <- (length(max) - 1) * popSize
  
  numMutations <- floor(pmutation * nGenes)
  
  object@DNFRules <- DNFRules
  object@maxValuesRule <- max 
  object@popNew <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  
  # generate beginning population and elite population
  Pop <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  elitePop <- matrix(as.double(NA), nrow = elitism, ncol = nvars)
  
  #Sets the next gene to mute
  Mu_next <- ceiling(log(runif(1)) / log(1 - pmutation))

  
  


  # Generate the initial population
   if( ! DNFRules) 
    Pop[1:popSize,] <- population(object, 0.25, round(nvars*0.25))[1:popSize,]
    else 
      Pop[1:popSize,] <- population(object, 0.25, round((length(max) - 1)*0.25))[1:popSize,]

  object@population <- Pop
  
  #Indicate wheter an individual is non-dominated
  NonDominated <- logical(popSize + elitism) 
  
  #Indicate the inviduals which domain this one
  WhoDominateMe <- vector(mode = "list", length = popSize + elitism) 
 # AdaptationValue <- numeric(popSize + elitism) 
  
  dots <- list(...) #Catch dots arguments
  nObjs <- length( which(!is.na(dots[[9]])) ) - 1
  
  # Calculate the volume of a sphere in n dimensions for SPEA2 fitness calculation.
  sphereVolume <- .volSphere(nObjs)
  
  
  nvariables <- nvars
  # start iterations (GA general cycle)
  for(iter in seq_len(maxiter))
  {
    #Create the union of this two populations (elite and population) and reinitializate all values
    NonDominated[] <- FALSE
    Dominated[] <- 0
    
    UnionPop <- matrix(NA, nrow = popSize + elitism, ncol = nvariables)
   
    UnionPop[seq_len(NROW(Pop)), ] <- Pop
    UnionPop[(NROW(Pop) + 1):NROW(UnionPop),] <- elitePop
    
    #normalize DNF RULES
#     if(DNFRules){
#       UnionPop <- matrix(unlist(apply(X = UnionPop, MARGIN = 1, FUN = .normalizeDNFRule, max)), ncol = nBits, byrow = TRUE)
#     }
    
    #Remove duplicated individuals in UnionPop
    UnionPop <- na.exclude(UnionPop)
    duplicate <- which(! duplicated(UnionPop))
    UnionPop <- UnionPop[duplicate, , drop = F]
    Fitness <- Fitness[duplicate, , drop = F]


    # evalute fitness function (when needed) 
    for(i in seq_len( NROW(UnionPop) )){
      if(all(is.na(Fitness[i,]))){
        Fitness[i,] <- fitness(UnionPop[i,], ...) 
        n_evals <- n_evals + 1
      }
    }
    
    

#Compute dominated and non-dominated rules and initial adaptation Value
# Calculate number of individuals each rule domain
f <- na.exclude(Fitness)[, seq_len(nObjs), drop = F]
n_Ind <- NROW(f)
for(i in seq_len(n_Ind)){
  # A rule domain another one if all his values are greater than or equal than the other one and at least one of them is strictly greater
  nd <- apply(X = f, MARGIN = 1, FUN = function(x, rule){ all(rule <= x) & any(rule < x)}, f[i,])
  #Calculate the number of individuals this rule domain
  Dominated[i] <- sum( apply(X = f, MARGIN = 1, FUN = function(x, rule){ all(rule >= x) & any(rule > x)}, f[i,]) )
  # If no one dominate this rule, then it is a non-dominated rule
  NonDominated[i] <- all( ! nd ) 
  WhoDominateMe[[i]] <- which(nd)
}

#Initial Adaptation Value
AdaptationValue <- vapply(X = WhoDominateMe[seq_len(n_Ind)], FUN = function(x, dominates) sum(dominates[x]) ,  FUN.VALUE = 2, Dominated)
kthDistances <- numeric(length(AdaptationValue))
#Distance measurement (Distance between fitness values)
distances <- as.matrix( dist(x = f, method = "euclidean", upper = TRUE, diag = TRUE))^2

# Order distance (The first colum is the distance with respect to himself !! )
# And calculate the final adaptation value for each rule
# To calculate the distance to the k-th neighbour, the 'k' value is calculated as:
kTH <- floor( sqrt(n_Ind - 1) ) 
for(i in seq_len(NROW(distances))){
  distances[i,] <- distances[i, order(distances[i, ]), drop = F]
  
  #Gets k-th closest neighbor , in this case, k = sqrt(popSize - 1)
  if(distances[i, kTH] == 0){
    #If k-th closest is 0, get the next closest greater than 0
    aux <- distances[i, (kTH + 1):ncol(distances)] > 0
    closest <- which(aux, useNames = F)
    if(length(closest) > 0)
      dist <- distances[i, closest[1] + kTH]
    else 
      #Exception: All individuals are equal
      dist <- 1
  } else {
    dist <- distances[i, kTH]
  }
  
  kthDistances[i] <-  1 / dist^nObjs * kTH / n_Ind / sphereVolume
}

#Normalize kthDistances
kthDistances <- kthDistances / sum(kthDistances)
AdaptationValue <- AdaptationValue + kthDistances
  
  
  
#Fill elite population
#Compute the number of non-dominated individuals
numberOfNonDominated <- sum(NonDominated)

if( numberOfNonDominated <= elitism){
  
  #Adition operator (Adds the 'elitism' best adaptation values to elite population)
  eliteIndividuals <- order(AdaptationValue)[seq_len(elitism)]
  elitePop <- UnionPop[eliteIndividuals, , drop = F]
 
} else {
  #Truncation operator
  lista <- .truncOperator(NonDominatedPop = UnionPop[which(NonDominated), , drop = F], elitePopSize = elitism, FitnessND = Fitness[which(NonDominated), , drop =  F])
  elitePop <- lista[[1]]
  eliteIndividuals <- which(NonDominated)[lista[[2]]]
  
  } 

# update object with the individuals evaluated
object@iter <- iter
object@population <- Pop
object@fitness <- Fitness



# check stopping criteria

if(n_evals >= run) break  
if(max(Fitness, na.rm = TRUE) >= maxfitness) break
if(object@iter == maxiter) break  

# APPLY GENETIC OPERATORS
# selection by Binary Tournament (Adaptation Values is the value for the "fitness")
# Copy the selected population into an intermediary population
sel <- selection(elitePop, popSize, nvariables, AdaptationValue[eliteIndividuals], Fitness[eliteIndividuals, , drop = F])
interPop <- sel$population
AdaptationValue <- sel$fitness
Fitness <- sel$obj

object@population <- interPop


# crossover performed by a double-point crossover
      if(pcrossover > 0)
        { 
          nmating <- round( (popSize/2) * pcrossover )
          
          #Create the population where we allocate the descendents from crossover and mutation
          descPop <- matrix(NA, ncol = nvariables, nrow = popSize*2)
          
          #mating <- matrix(sample(seq_len(popSize), size = (2*nmating), replace = TRUE), ncol = 2, byrow = T)
          for(i in seq_len(nmating))
            { 
                parents <- sample(seq_len(popSize), size = 2, replace = TRUE)
                Crossover <- crossover(object, parents)
                descPop[c(2*i - 1, 2*i),] <- Crossover$children
   
            }             
          #object@population <- descPop
          #nGenes <- NROW(na.exclude(descPop)) * nvariables
        }

# mutation (only mutate popLength * probMut chromosomes)
pm <- pmutation
if(DNFRules) nvars <- length(max) - 1
if(pm > 0) { 

suma <- nmating*2 + 1 #This is where we must store the indivuduals generated by mutations on the descendants population

while(Mu_next <= nGenes){

chromosome <- ceiling( Mu_next  / nvars ) 
gen <- (Mu_next %% nvars) + 1

descPop[suma, ] <- mutation(object, chromosome, gen)
suma <- suma + 1
#Calcuate next gene
Mu_next <- Mu_next + ceiling(log( runif(1) ) /  log(1 - pmutation))
}

Mu_next <- Mu_next - nGenes


#Replace the worst individuals in the population with the genereted in crossovers and mutations

orden <- order(AdaptationValue)
Pop <- interPop
orden <- c(orden, (popSize+1):NROW(Fitness))

Pop[orden[popSize:(popSize - (suma - 2))], ] <- descPop[seq_len(suma - 1),]
Fitness[orden[popSize:(popSize - (suma - 2))], ] <- NA

  
  }
} # End genetic cycle
  

# Return Non-duplicated individuals in elite pop
  if(DNFRules){
    elitePop <- matrix(unlist(apply(X = elitePop, MARGIN = 1, FUN = .normalizeDNFRule, max)), ncol = nBits, byrow = TRUE)
    } 
    elitePop[which(!duplicated(elitePop)), , drop = F]
  
}
























#' @title Genetic algorithm for NMEEF-SD
#' 
#' @description This method execute the genetic algorithm for the NMEEF-SD algorithm
#' 
#' @param type type of representation used. For DNF rules is "binary", for CAN rules, "Real-valued"
#' @param fitness The fitness function that calculate the quality function of each individual of the population
#' @param ... Additional parameters for the fitness function
#' @param min A vector with the minimum value for each variable
#' @param max A vector with the maximum value for each variable
#' @param nBits For DNF Rules, a number that specify the length of a chromosome
#' @param population The method that generate the initial population
#' @param crossover The crossover function
#' @param mutation The mutation operator
#' @param popSize The size of the population
#' @param pcrossover The probability of crossover, in [0,1]
#' @param pmutation The probability of mutation of a gene, in [0,1]
#' @param elitism A number that specifiy the number of elite individuals
#' @param maxiter The number of evaluations to perform
#' @param run The number of evaluations to perform
#' @param maxfitness The maximum value of the fitness function, if reached, the GA stops.
#' @param names Names of the parameters
#' @param suggestions A vector that indicate possible initial individuals
#' @param keepBest Logical, keep the best individual ? 
#' @param parallel perform calculation of the fitness in parallel
#' @param monitor Show intermediate results
#' @param DNFRules Logical indicating if DNFRules are used
#' @param seed The random number generator seed
#' @param porcCob Percentage of examples that must be generated by initialization based on coverage
#' @param StrictDominance Apply strict dominances? i.e. all values must be grater than the other ?
#' @param reInitPop Use the reinitiazation operator? 
#' @param minCnf Minimum confidence to filter rules
#' 
#' @details This genetic algorithm is a multi-objective genetic algorithm that uses the NSGA-II schema that uses a main
#'     population and an offsrping population where the individuals generated by the genetic operators are stored
#'     Then, both populations are joined and ordered by dominance by a fast sorting algorithm.
#'     If the population does not evolve (i.e. the Pareto front does not change) the main population is reinitialized
#'     by an operator based on coverage of examples of the dataset.
#'     
#' @noRd
#' 
.gaNMEEF <- function(type = c("binary", "real-valued", "permutation"), 
                     fitness, ...,
                     min, max, nBits,
                     population ,
                     selection ,
                     crossover , 
                     mutation ,
                     popSize = 50, 
                     pcrossover = 0.8, 
                     pmutation = 0.1, 
                     elitism = base::max(1, round(popSize*0.05)), 
                     maxiter = 100,
                     run = maxiter,
                     maxfitness = Inf,
                     names = NULL,
                     suggestions = NULL, 
                     keepBest = FALSE,
                     parallel = FALSE,
                     monitor = NULL,
                     DNFRules = FALSE,
                     seed = NULL,
                     porcCob = 0.5,
                     StrictDominance = TRUE,
                     reInitPop = TRUE, 
                     minCnf = 0.6) 
{
  
  call <- match.call()
  
  type <- match.arg(type)

  
  if(missing(fitness))
  { stop("A fitness function must be provided") }
  if(!is.function(fitness)) 
  { stop("A fitness function must be provided") }
  if(popSize < 10) 
  { warning("The population size is less than 10.") }
  if(maxiter < 1) 
  { stop("The maximum number of iterations must be at least 1.") }
  if(elitism > popSize) 
  { stop("The elitism cannot be larger that population size.") }
  if(pcrossover < 0 | pcrossover > 1)
  { stop("Probability of crossover must be between 0 and 1.") }
  if(is.numeric(pmutation))
  { if(pmutation < 0 | pmutation > 1)
  { stop("If numeric probability of mutation must be between 0 and 1.") }
    else if(!is.function(population))
    { stop("pmutation must be a numeric value in (0,1) or a function.") }
  }
  if(missing(min) & missing(max) & missing(nBits))
  { stop("A min and max range of values (for 'real-valued' or 'permutation' GA) or nBits (for 'binary' GA) must be provided!") }
  
  switch(type, 
         "binary"      = { nBits <- as.vector(nBits)[1]
         #min <- max <- NA
         nvars <- nBits 
         },
         "real-valued" = { min <- as.vector(min)
         max <- as.vector(max)
         nBits <- NA
         if(length(min) != length(max))
         { stop("min and max must be vector of the same length!") }
         nvars <- length(max) 
         },
         "permutation" = { min <- as.vector(min)[1]
         max <- as.vector(max)[1]
         nBits <- NA
         nvars <- length(seq(min,max)) 
         }
  )
  
  
  
  #Define Fitness as a matrix, in NMEEF, each objective value is evaluated individually
  Fitness <- matrix(NA, nrow = popSize * 2, ncol = 4)

  
  
  object <- new(".ga", 
                call = call, 
                type = type,
                min = min, 
                max = max, 
                nBits = nBits, 
                names = if(is.null(names)) character() else names,
                popSize = popSize,
                iter = 0, 
                run = 1, 
                maxiter = maxiter,
                suggestions = matrix(),
                population = matrix(), 
                elitism = elitism, 
                pcrossover = pcrossover, 
                pmutation = if(is.numeric(pmutation)) pmutation else NA,
                fitness = Fitness, 
                summary = matrix(),
                bestSol = list())
  #This GA runs until a number of EVALUATIONS is reached, not iterations !
  n_evals <- 0
  if(!is.null(seed)) set.seed(seed)
  
  
  #Compute the number of genes, the mutation probability is applied over the gene.
  if(!DNFRules)
    nGenes <- nvars * popSize
  else
    nGenes <- (length(max) - 1) * popSize
  
  
  #Set the next gene to mute
  numMutations <- round(pmutation * nGenes)
  
  object@DNFRules <- DNFRules
  object@maxValuesRule <- max 
  object@popNew <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  
  # generate beginning population, Offspring pop and Union Pop
  Pop <- matrix(as.integer(NA), nrow = popSize, ncol = nvars)
  OffspringPop <- matrix(as.integer(NA), nrow = popSize, ncol = nvars)
  UnionPop <- matrix(as.integer(NA), nrow = popSize *2, ncol = nvars)
  
 
  if( ! DNFRules) 
    Pop[1:popSize,] <- population(object, 0.25, round(nvars*0.25))
  else 
    Pop[1:popSize,] <- population(object, 0.25, round((length(max) - 1)*0.25))
  
  object@population <- Pop
  
  #This counts the number of indivuals that domain this one
  Dominated <- numeric(popSize *2)
  
  #Indicate the inviduals which are dominated by this one
  WhoIDomain <- vector(mode = "list", length = popSize *2) 
  
  #Inidicate the rank of the individual
  rank <- numeric(popSize * 2) 
  
  #Indicate the crowding distance of the individual
  CrowdingDistance <- numeric(popSize * 2) 
  
  #Catch dots arguments
  dots <- list(...) 
  
  # Number of objetives we are using. -1 because the last value indicate the use of dnf or can representation for fitness calc.
  nObjs <- sum(! is.na(dots[[9]])) - 1 
  
  fronts <- vector(mode = "list", length = popSize * 2)
  #Individuals finess values by front
  fitnessFronts <- vector(mode = "list", length = popSize * 2)
  
  #Individuals covered by fronts
  coveredByIndividualFronts <- vector(mode = "list", length = popSize * 2)
  
  #Individuals covered
  coveredByIndividual <- matrix(FALSE, ncol = popSize * 2, nrow = length( dots[[1]][["data"]] ))
  coveredNow <- coveredBefore <- logical(length( dots[[1]][["data"]] ))
  nIterEvolve <- 0 #Evaluation where the population evolved the last time
  fivePercent <- floor(run * 0.05)
  nvariables <- nvars
  dataset <- matrix(unlist(dots[[1]][["data"]]), nrow = nvariables + 1)
  targetClass <- which(dots[[1]][["class_names"]] == dots[[3]]) - 1
  
  
 
  #Evaluation of the population
  for(i in seq_len( NROW(Pop) )){
    if(all(is.na(Fitness[i,]))){
      fit <- fitness(Pop[i,], ...) 
      Fitness[i,] <- fit[[1]]
      coveredByIndividual[,i] <- as.logical(fit[[2]])
      n_evals <- n_evals + 1
    } 
  }
  
  
  # start iterations (GA general cycle)
  for(iter in seq_len(maxiter))
  {
    #reinitialize all values
    UnionPop[] <- NA
    OffspringPop[] <- NA
    
    
    #Check stopping criteria
    if(n_evals >= run) break  
    if(max(Fitness, na.rm = TRUE) >= maxfitness) break
    if(object@iter == maxiter) break  
    
    fronts <- vector(mode = "list", length = popSize * 2)
    
    #APPLY GENETIC OPERATORS to generate the Offspring Population
    # selection by Binary Tournament 
    # Copy the selected population into the offspring population
 
    sel <- selection(Pop, popSize, rank, CrowdingDistance, Fitness, coveredByIndividual) 
    OffspringPop <- sel[[1]]
    FitnessOffspring <- sel[[2]]
    coveredByIndividual[,(popSize + 1):(popSize*2)] <- sel[[3]]
   
    
    object@population <- OffspringPop
    
    
    # crossover performed by a double-point crossover on Offspring Pop
    if(pcrossover > 0)
    { 
      nmating <- round( (popSize/2) * pcrossover )
      
      #mating <- matrix(sample(seq_len(popSize), size = 2*popSize, replace = TRUE), ncol = 2)
      mating <- matrix(sample(seq_len(popSize), size = floor(popSize/2) * 2, replace = TRUE), ncol = 2)
      equals <- which(mating[,1] == mating[,2])
      
      while(length(equals) > 0){
        mating[equals,] <- matrix(sample(seq_len(popSize), size = 2*length(equals), replace = TRUE), ncol = 2)
        equals <- which(mating[,1] == mating[,2])
      }
      
      #throw popSize/2 random numbers
      dices <- runif(floor(popSize/2))
    
      
      #Check which pair of individuals cross and apply the crossover operator
      mating <- mating[which(dices <= pcrossover), , drop = F]
    
      for(i in seq_len(NROW(mating)))
      { 
        parents <- mating[i,]
        Crossover <- crossover(object, parents)
        OffspringPop[parents,] <- Crossover$children
        FitnessOffspring[parents,] <- NA
        
      }             
    
      object@population <- OffspringPop
      nGenes <- NROW(na.exclude(OffspringPop)) * nvariables
      
    }
    
    # mutation 
    pm <- pmutation
    if(DNFRules) nvars <- length(max) - 1
    if(pm > 0)
    { 
      dices <- runif(nGenes)
      genes <- which(dices <= pmutation)
     
      cromosomes <- ceiling(genes / nvariables)
      vars <- (genes %% nvariables) + 1
      
      for(i in seq_len(length(vars))) 
      {     
        object@population[cromosomes[i],] <- mutation(object, cromosomes[i], vars[i])
      }
    
      OffspringPop <- object@population
      
      FitnessOffspring[cromosomes,] <- NA
    
      
    }
    
    #Copy FitnessOffspring and evaluate individuals crossed and mutated
    Fitness[(NROW(Pop) + 1):(popSize*2), ] <- FitnessOffspring
  
    
   #Generate the next population.
    
    #Combine Pop and  OffspringPop into UnionPop
    UnionPop[seq_len(NROW(Pop)), ] <- Pop
    UnionPop[(NROW(Pop) + 1):NROW(UnionPop), ] <- OffspringPop
    #Evaluation of indivduals in UnionPop
    for(i in (NROW(Pop) + 1):(popSize*2) )
      if(all(is.na(Fitness[i,]))){
        fit <- fitness(UnionPop[i,], ...) 
        Fitness[i,] <- fit[[1]]
        coveredByIndividual[,i] <- as.logical( fit[[2]] )
        n_evals <- n_evals + 1
      } 
   
  #Compute dominance values for performing fast sorting algorithm
    f <- na.exclude(Fitness)[,seq_len(nObjs),drop=F]
    n_Ind <- NROW(f)
    for(i in seq_len(n_Ind)){
      nd <- apply(X = f, MARGIN = 1, FUN = .calculateDominance, f[i,,drop=F], StrictDominance) 
      Dominated[i] <- length(which(nd == 1L))
      WhoIDomain[[i]] <- which(nd <= 0L)
    }
    
    #Get the Pareto front
    fronts[[1]] <- which(Dominated == 0)
    
    #Order UnionPop by dominance fronts (fast sorting algorithm of NSGA-II)
    p <- 1
    while(length(fronts[[p]]) != 0){
      p <- p+1
      
      for(i in fronts[[p - 1]]){
        
       if(length(WhoIDomain[[i]] > 0)){
        Dominated[ WhoIDomain[[i]] ] <- Dominated[ WhoIDomain[[i]] ] - 1
        toTheFront <-  which(Dominated == 0)
        if(length(toTheFront) > 0){
          fronts[[p]] <- c(fronts[[p]], toTheFront)
          rank[toTheFront] <- p-1
        }
       } 
      } 

    }

  
    #Check if non-dominated front covers new examples and evolve
    inTheFront <- fronts[[1]]
    coveredNow <- apply(X = coveredByIndividual[, inTheFront, drop = F], MARGIN = 1, FUN = any)
    
    #Check if there are new examples covered 
    evolve <- any(! coveredBefore[which(coveredNow)])
    

    
    for(i in seq_len(p - 1)){
      fitnessFronts[[i]] <- Fitness[fronts[[i]], ,drop = F]
      coveredByIndividualFronts[[i]] <- coveredByIndividual[,fronts[[i]], drop = F]
      fronts[[i]] <- UnionPop[fronts[[i]], , drop = F]
      
    }

      if(evolve){
        nIterEvolve <- n_evals
      } 
    coveredBefore <- coveredNow
    
      # fill the next population 
      aux <- .fillPopulation(fronts, p - 1, fitnessFronts, coveredByIndividualFronts, popSize, nObjs)
      Pop <- aux[[1]]
      CrowdingDistance[seq_len(popSize)] <- aux[[2]]
      Fitness[seq_len(popSize), ] <- aux[[3]]
      rank <- aux[[4]]
      coveredByIndividual <- aux[[5]]
      
      # Checks if we need to reinitialize the population
      if(! evolve & ! aux[[6]] & reInitPop){
      # Check reinit condicion
      if(n_evals - nIterEvolve >= fivePercent){
        
        #re-initialize population
    
        #sel <- .reInitPob(elitePop = fronts[[1]], fitnessElite = fitnessFronts[[1]], coveredElite = coveredByIndividual[,inTheFront], calculateDominance = CrowdingDistance, pctVariables = 0.5, coveredNow = coveredNow, dataset = dataset, maxRule = dots[[1]][["sets"]], cate = dots[[14]], num = dots[[15]], crispSets = dots[[1]][["crispSets"]], targetClass = targetClass, popSize = popSize )
        sel <- .reInitPob(elitePop = Pop, fitnessElite = Fitness[seq_len(popSize), ], coveredElite = coveredByIndividual[, seq_len(popSize)], crowdingDistance = CrowdingDistance, pctVariables = porcCob, coveredNow = coveredNow, dataset = dataset, maxRule = dots[[1]][["sets"]], cate = dots[[14]], num = dots[[15]], crispSets = dots[[1]][["crispSets"]], targetClass = targetClass, popSize = popSize )
        Pop <- sel[[1]]
        Fitness <- sel[[2]]
        CrowdingDistance <- sel[[3]]
        coveredByIndividual[,seq_len(popSize) ] <- sel[[4]]
        
        #Evaluation of generated pop
        for(i in seq_len(popSize) )
          if(all(is.na(Fitness[i,]))){
            fit <- fitness(Pop[i,], ...) 
            Fitness[i,] <- fit[[1]]
            coveredByIndividual[,i] <- as.logical( fit[[2]] )
            newCovered <- which(! coveredBefore[which(as.logical( fit[[2]] ))]) # Check if this rule covers new uncovered examples
            n_evals <- n_evals + 1
            if(length(newCovered) > 0){
              nIterEvolve <- n_evals
              coveredBefore[newCovered] <- TRUE
            }
          }
        
        #Check if new population evolves
        coveredNow <- apply(X = coveredByIndividual[, inTheFront, drop = F], MARGIN = 1, FUN = any)
        evolve <- any(! coveredBefore[which(coveredNow)])
        coveredBefore <- coveredNow
        
        if(evolve)
          nIterEvolve <- n_evals
         
        rank[] <- 0
        
      }
      }
    
      object@population <- Pop
      object@fitness <- Fitness
    
  }
  
  
  
  
  #get the last ranking
  
  #Compute dominance values
  f <- na.exclude(Fitness)[1:popSize,seq_len(nObjs), drop = F]
  n_Ind <- NROW(f)
  for(i in seq_len(n_Ind)){
    nd <- apply(X = f, MARGIN = 1, FUN = .calculateDominance, f[i,], TRUE)
    Dominated[i] <- length(which(nd == 1L))
    WhoIDomain[[i]] <- which(nd <= 0L)
  }
  
  #Get the Pareto front
  fronts[[1]] <- which(Dominated == 0)
  fronts[[1]] <- Pop[fronts[[1]], , drop = F]

  
  #Return individuals of the Pareto that has more confidence than the minimum.
  unicos <- which(!duplicated(fronts[[1]]))
  fronts[[1]] <- fronts[[1]][unicos, , drop = F]
  
  #Evaluate indivuduals for getting fuzzy confidence
  dots[[9]] <- list(.fuzzyConfidence, NA, NA, FALSE)
  
  for(i in seq_len(NROW( fronts[[1]])) ){
      fit <- fitness(fronts[[1]][i,], dots[[1]],dots[[2]],dots[[3]],dots[[4]],dots[[5]],dots[[6]],dots[[7]],dots[[8]],dots[[9]],dots[[10]],dots[[11]],dots[[12]],dots[[13]],dots[[14]],dots[[15]],dots[[16]]) 
      Fitness[i,4] <- fit[[1]][1]
  }
  
  fronts[[1]] <- fronts[[1]][which(Fitness[seq_len(NROW(fronts[[1]])),4] > minCnf), , drop = F]
  fronts[[1]] #Return
  
}







#' @title Genetic algorithm for the FuGePSD algorithm 
#' 
#' @description It executes the genetic algorithm for the FuGePSD algorithm
#' 
#' @param type the type of execution (1 for OVA (One vs All) execution, != 1 for normal execution)
#' @param dataset A SDEFSR_Dataset object 
#' @param selection The selection function 
#' @param crossober The crossover function
#' @param mutation The mutation function
#' @param popSize Size of the population 
#' @param pcrossover Crossover probability
#' @param pmutation Mutation probability
#' @param pinsertion Insertion probability
#' @param pmdropping Dropping probability
#' @param selectionSize Size of the tournament for the tournament selection
#' @param AllClass the ALL_CLASS attribute
#' @param T_Norm the T-norm used, 1  minimum t-norm, != product t-norm
#' @param ruleWeight The rule weight method to use.
#' @param frm The fuzzy reasoning method to use.
#' @param maxiter Maximum of generations to run this algorithm
#' @param weightsGlobalFitness Weights used in the evaluation of the population 
#' @param seed the seed used for the random number genarator.
#' 
#' @details The FuGePSD algorithm uses a programming genetic approach, where individuals are represented as trees
#'     with variable length instead of vectors. Also, the consecuent of the rule is represented. 
#'     This schema has the advantage of get rules for all possible values of a given target variable in one 
#'     execution. Furthermore, FuGePSD has a variable population length, which can change over the evolutive 
#'     process based on an competitive-cooperative approach, the Token Competition.
#'     
#'     The evolutionary process of FuGePSD works as follow:
#'
#' \begin{enumerate}
#'  \item Create a random population of a fixed length.
#'  \item Create a new population called Offspring Propulation, which is generated via genetic operators.
#'  \item Join original population and offspring and execute the token competition procedure
#'  \item Get global fitnees and replace best population if necessary.
#'  \item return to 2 until number of generations is reached.
#'  \item Apply to the best population a Screening function and return the resulting rules.
#' \end{enumerate}
#' 
#' @noRd
.gaFuGePSD <- function(type,           # Type of execution (1 for One vs All, != 1 for normal execution)
                       dataset,        # SDEFSR_Dataset object asociated to this genetic Algorithm (training file)
                       selection ,     # Selection function !
                       crossover ,     # Crossover function !
                       mutation ,      # mutation function
                       popSize = 50,      #size of the population
                       pcrossover = 0.8,  #Crossover Probability
                       pmutation = 0.1,   #Mutation Probability
                       pinsertion = 0.05,    #Insertion Probability
                       pdropping = 0.05,     #Dropping Probability
                       selectionSize = 2, #Tournament selection size
                       AllClass = TRUE,   #ALL_CLASS Attribute
                       T_norm = 1,        # T-norm used
                       ruleWeight = 0 ,    # Rule Weighting method to use
                       frm = 0,           # Fuzzy Reasoning Method to use
                       maxiter = 100,    #Max generations to run this genetic Algorithm.
                       weightsGlobalFitness = c(0.25, 0.25, 0.25, 0.25), #Weights Used in population Global Evaluation
                       seed = .randInt(0, 20000000)
                      )
{
  #First of all, we must check types of all attributes
  if(class(dataset) != "SDEFSR_Dataset")
    stop("'dataset' must be a SDEFSR_Dataset dataset object.")
  if(! is.function(selection))
    stop("'selection' must be function.")
  if(! is.function(crossover))
    stop("'crossover' must be function.")
  if(! is.function(mutation))
    stop("'mutation' must be function.")  
  if(popSize <= 0)
    stop("'popSize' must be greater than zero.")
 
  if(selectionSize < 2)
    stop("'selectionSize' must be greater than 2.")
  if(! is.logical(AllClass))
    stop("'AllClass' must be a logical value.")
  if(maxiter < 1)
    stop("'maxiter' must be greater than zero")

  if(length(weightsGlobalFitness) != 4)
    stop("length of 'weightsGlobalFitness' must be 4")
  
  suma <- sum(pcrossover, pmutation, pinsertion, pdropping)
  if(suma != 1 ){
    pcrossover <- pcrossover / suma
    pmutation <- pmutation / suma
    pinsertion <- pinsertion / suma
    pdropping <- pdropping / suma
  }
  
  #Once checked, the evolutive process can start
  if(type == 0){
    #Execution One Vs all (NOT IMPLEMENTED YET)
    stop("'type = 0' is not implemented yet ")
  } else {
    #Normal execution and Return
    executionPSD( clas = NULL,
                  dataset,        
                  selection ,    
                  crossover ,     
                  mutation ,      
                  popSize,      
                  pcrossover, 
                  pmutation ,  
                  pinsertion,    
                  pdropping ,     
                  selectionSize, 
                  AllClass,   
                  T_norm,       
                  ruleWeight,    
                  frm,           
                  maxiter,   
                  weightsGlobalFitness,
                  seed)
  }
  
}


#' @title Exectution of the FuGePSD genetic algorithm 
#' @param clas number of the class to generate rules
#' @param dataset A SDEFSR_Dataset object 
#' @param selection The selection function 
#' @param crossober The crossover function
#' @param mutation The mutation function
#' @param popSize Size of the population 
#' @param pcrossover Crossover probability
#' @param pmutation Mutation probability
#' @param pinsertion Insertion probability
#' @param pmdropping Dropping probability
#' @param selectionSize Size of the tournament for the tournament selection
#' @param AllClass the ALL_CLASS attribute
#' @param T_Norm the T-norm used, 1  minimum t-norm, != product t-norm
#' @param ruleWeight The rule weight method to use.
#' @param frm The fuzzy reasoning method to use.
#' @param maxiter Maximum of generations to run this algorithm
#' @param weightsGlobalFitness Weights used in the evaluation of the population 
#' @param seed the seed used for the random number genarator. 
#' 
#' @noRd
#' 
executionPSD <- function(clas = NULL,   # number of the class to generate rules.
                       dataset,        # SDEFSR_Dataset object asociated to this genetic Algorithm (training file)
                       selection ,     # Selection function !
                       crossover ,     # Crossover function !
                       mutation ,      # mutation function
                       popSize = 50,      #size of the population
                       pcrossover = 0.8,  #Crossover Probability
                       pmutation = 0.1,   #Mutation Probability
                       pinsertion = 0.05,    #Insertion Probability
                       pdropping = 0.05,     #Dropping Probability
                       selectionSize = 2, #Tournament selection size
                       AllClass = TRUE,   #ALL_CLASS Attribute
                       T_norm = 1,        # T-norm used
                       ruleWeight = 0 ,    # Rule Weighting method to use
                       frm = 0,           # Fuzzy Reasoning Method to use
                       maxiter = 100,    #Max generations to run this genetic Algorithm.
                       weightsGlobalFitness = c(0.25, 0.25, 0.25, 0.25), #Weights Used in population Global Evaluation
                       seed = .randInt(0, 20000000)
){
  populationFitness <- bestPopulationFitness <- numeric(1)
  exampleClass <- unlist(.getClassAttributes(dataset$data))
  
  #get categorical and numerical variables
  categorical <- dataset$attributeTypes == "c"
  categorical <- categorical[-length(categorical)]
  numerical <- !categorical
  
  datasetNoClass <- matrix(unlist(.separate(dataset)), nrow = dataset$nVars, ncol = dataset$Ns)
  bestPop <- vector(mode = "list", length = popSize)
  
  #Init population
  pop <- lapply(seq_len(popSize), function(x, dataset, tnorm, tconorm, rule_weight, clas){
    createNewRule(dataset, tnorm, tconorm, rule_weight, clas)
  }, dataset, T_norm, T_norm, ruleWeight, clas)
  
  #evaluate initial population individuals (In parallel for Linux)
  if(length(pop) >= 20 & Sys.info()[1] == "Linux"){
    pop <- parallel::mclapply(pop, Rule.evaluate, dataset, datasetNoClass, categorical, numerical, T_norm, ruleWeight, mc.cores = parallel::detectCores())
  } else {
   pop <- lapply(pop, Rule.evaluate, dataset, datasetNoClass, categorical, numerical, T_norm, ruleWeight)
  }
  #evaluate the whole population
  populationFitness <- Pop.evaluate(pop, dataset, datasetNoClass, exampleClass, frm, categorical, numerical, T_norm, weightsGlobalFitness)
  
  #best population is now initial population.
  bestPop <- pop
  bestPopulationFitness <- populationFitness
  message(paste("Global Fitness obtained in generation [0]:", bestPopulationFitness, "\n", sep = " "))
  
  
  
  #Init the evolutive process
  for(generation in seq_len(maxiter - 1)){
    #First, create a join population with twice length of population. Then add pop to joinPop
    joinPop <- vector(mode = "list", length = length(pop) * 2)
    joinPop[seq_len(length(pop))] <- pop
    
    #Now we need to generate an offspring population length equal to pop length.
    #This offspring population is generated via genetic operators.
    
    dices <- runif(length(pop))
    first_parents <- vapply(X = seq_len(length(pop)), 
                            FUN = function(x, pop, tam){
                                  tournamentSelection(pop, tam)}, numeric(1), pop, selectionSize)
    
    #Specify the genetic operator to apply according to their probability in 'dices'
    crosses <- first_parents[which(dices < pcrossover)]
    mutates <- first_parents[which(pcrossover <= dices & dices < (pcrossover + pmutation))]
    inserts <- first_parents[which((pcrossover + pmutation) <= dices & dices < (pcrossover + pmutation + pinsertion))]
    drops <- first_parents[which(pcrossover + pmutation + pinsertion <= dices)]
    
    posJoinPop <- length(pop) + 1
    #Make crossovers
  for(i in crosses){
      second_parent <- .randIntExcluded(1, length(pop), i)
      joinPop[[posJoinPop]] <- FuGePSD_crossover(rule1 = pop[[i]], rule2 = pop[[second_parent]], nvars = dataset$nVars + 1)
      posJoinPop <- posJoinPop + 1  
    }
    
    #Make mutations 
    for(i in mutates){
      joinPop[[posJoinPop]] <- FuGePSD_Mutation(pop[[i]], dataset)
      posJoinPop <- posJoinPop + 1
    }
    
    #Make insertions
    for(i in inserts){
      if(length(pop[[i]][[1]]) == dataset[[6]]){ 
        #If we cannot add more variables, we introduce a rule with an empty antecedent.
        joinPop[[posJoinPop]] <- Rule.clearAntecedent(pop[[i]])
      } else {
        #Add a random variable
        joinPop[[posJoinPop]] <- Rule.addVariable(pop[[i]], dataset)
      }
      posJoinPop <- posJoinPop + 1
    }
    
    #Make droppings
    for(i in drops){
      if(length(pop[[i]][[1]]) == 1){
        #We cannot delete more variables, return an empty rule
        joinPop[[posJoinPop]] <- Rule.clearAntecedent(pop[[i]])
      } else {
        joinPop[[posJoinPop]] <- Rule.deleteVariable(pop[[i]])
      }
      posJoinPop <- posJoinPop + 1
    }
    
    #Evaluate joinPop.
    joinPop <- lapply(joinPop, Rule.evaluate, dataset, datasetNoClass, categorical, numerical, T_norm, ruleWeight)
    
    #Apply Token Competition
    pop <- tokenCompetition(joinPop, dataset)
    
    #Evaluate Global Fitness
    populationFitness <- Pop.evaluate(pop, dataset, datasetNoClass, exampleClass, frm, categorical, numerical, T_norm, weightsGlobalFitness)
    
    #Substitute best population if actual if better.
    if(bestPopulationFitness < populationFitness){
      bestPopulationFitness <- populationFitness
      bestPop <- pop
      message(paste("Global Fitness obtained in generation [", generation, "]: ", bestPopulationFitness, "\n", sep = ""))
    }
    
    #cat("\r", (generation / (maxiter-1)) * 100, "% Completed.", sep = "")
  }
  
  #Order bestPop by conf_f (desc. order)
  fuzzy_conf <- vapply(X = bestPop, FUN = function(x){x$qm_Cnf_f}, numeric(1))
  sens <- vapply(X = bestPop, FUN = function(x){x$qm_Sens}, numeric(1))
  orden <- order(fuzzy_conf, decreasing = TRUE)
  fuzzy_conf <- fuzzy_conf[orden]
  sens <- sens[orden]
  
  bestPop <- bestPop[orden]
  
  #Return 
  list(bestPop = bestPop, conf = fuzzy_conf, sensitivity = sens)

}



#-------------------------------------------------------------------------------
# ---  THis part is part of the definition of the "ga" class done in the GA Package
#--------------------------------------------------------------------------------

methods::setClassUnion(".numericOrNA", members = c("numeric", "logical", "matrix"))

methods::setClassUnion(".matrixOrList", members = c("matrix", "list"))

#Modification of the class 'ga' provided by the package "GA" created by Luca Scrucca.

methods::setClass(Class = ".ga", 
         representation(call = "language",
                        type = "character",
                        min = ".numericOrNA", 
                        max = ".numericOrNA", 
                        nBits = ".numericOrNA", 
                        names = "character",
                        popSize = "numeric",
                        iter = "numeric", 
                        run = "numeric", 
                        maxiter = "numeric",
                        suggestions = "matrix",
                        population = ".matrixOrList",
                        popNew = "matrix",
                        elitism = "numeric", 
                        pcrossover = "numeric", 
                        pmutation = ".numericOrNA",
                        fitness = ".numericOrNA",
                        summary = "matrix",
                        bestSol = "list",
                        fitnessValue = "numeric",
                        solution = "matrix",
                        maxValuesRule = ".numericOrNA",
                        DNFRules = "logical"
         ),
         package = "SDEFSR" 
) 






#---------------------------------------------------------------------------------------------
#
#  GENETIC OPERATORS
#  
#  -------------------------------------------------------------------------------------------






#'
#' @title generates an initial population for the SDIGA algorithm 
#' 
#' @param object A "ga" class object
#' @param ... Addition parameters
#' 
#' @return a matrix with the generated initial population 
#' @noRd
#' 
.generatePopulation <- function(object, ...)
{
  # Generate a random permutation of size popSize in the range [min, max]  
  min <- object@min
  max <- object@max
  type <- object@type
  size <- object@popSize
  
  
  if(! object@DNFRules){ #CAN rules
    population <- matrix(as.double(NA), nrow = size, ncol = length( max ) )
    for(i in 1:size)
      for(j in 1:length(max))
        #Initial population is generated, no participation value it does not taken into account!
        population[i,j] <- sample(0:(max[j]), size = 1, replace = TRUE)
    
  } else { # DNF rules 
    v <- sample(x = c(0,1), size = max[length(max)] * size, replace = TRUE)
    population <- matrix(data = v, nrow = size, ncol = max[length(max)],byrow = TRUE)
  }
  
  return(population)
}





#'
#' @title generates an initial population for the SDIGA algorithm 
#' 
#' @param object A "ga" class object
#' @param ... Addition parameters
#' 
#' @details In the '...' argument it must go first the percentage of population generated completely random
#'         The second argument is the maximum number of variables that participate in the rule.
#' @return a matrix with the generated initial population 
#' @noRd
#' 
.generateMESDIFPopulation <- function(object, ...)
{
  # Generate a random permutation of size popSize in the range [min, max]  
  min <- object@min
  max <- object@max
  type <- object@type
  size <- object@popSize
  lista <- list(...)
  pctRandom <- lista[[1]]
  numVarMax <- lista[[2]]
  var_init <- logical(length(max))
  
  reglas <- ceiling(size * (1-pctRandom))
  if(! object@DNFRules){ # CAN Rules
    population <- matrix(max, nrow = size, ncol = length( max ), byrow = TRUE )
    
    # Biased Initialization
    for(i in seq_len(reglas)){
      var_init[] <- F
      numVar <- sample(numVarMax, size = 1)
      for(j in seq_len(numVar)){
        var <- sample(length(max), size = 1)
        while(var_init[var]){  
          var <- sample(length(max), size = 1)
        }
        population[i, var] <- sample(0:(max[var]), size = 1, replace = TRUE) # No-Participate value is not taken into account
        var_init[var] <- T
        }
    }
    
    
    #Random Initialization
    for(i in (reglas + 1):size){
      
      for(j in seq_len(length(max)))
        
        population[i,j] <- sample(0:(max[j]), size = 1, replace = TRUE)
    }
    
    
  } else { # DNF rules 
    
    # Random Initialization 
    v <- sample(x = c(0,1), size = max[length(max)] * size, replace = TRUE)
    population <- matrix(data = v, nrow = size, ncol = max[length(max)],byrow = TRUE)
    
    #Biased Init
    nRulesToErase <- length(max) - 1 - numVarMax
    for(i in (reglas + 1):size){
      varia <- sample(length(max) - 1, size = nRulesToErase, replace = FALSE)
      for(j in seq_len(nRulesToErase)){
        population[i, ] <- .eraseGene(rule = population[i,], variable = varia[j], maxVariablesValue = max, DNF_Rules = TRUE)
      }
    }
  }
  return(population)
}









#'
#' @title generates an initial population for the SDIGA algorithm 
#' 
#' @param object A "ga" class object
#' @param ... Addition parameters
#' 
#' @details In the '...' argument it must go first the percentage of population generated completely random
#'         The second argument is the maximum number of variables that participate in the rule.
#' @return a matrix with the generated initial population 
#' @noRd
#' 
.generarPoblacionNMEEF <- function(object, ...)
{
  # Generate a random permutation of size popSize in the range [min, max]  
  min <- as.integer(object@min)
  max <- as.integer(object@max)
  type <- object@type
  size <- object@popSize
  lista <- list(...)
  pctRandom <- lista[[1]]
  numVarMax <- lista[[2]]
  var_init <- logical(length(max))
  
  reglas <- ceiling(size * (1-pctRandom))
  if(! object@DNFRules){ # real-valued indica que se usan reglas tipo CAN
    population <- matrix(max, nrow = size, ncol = length( max ), byrow = TRUE )
    # Biased Init
    for(i in seq_len(reglas)){
      var_init[] <- F
      numVar <- sample(numVarMax, size = 1)
      for(j in seq_len(numVar)){
        var <- sample(length(max), size = 1)
        while(var_init[var]){  #Hay que incluir esto tambien a reglas DNF
          var <- sample(length(max), size = 1)
        }
        population[i, var] <- sample(0:(max[var] - 1), size = 1, replace = TRUE) # No-Participate value is not into account
        var_init[var] <- T
      }
    }
      
      #Random Init
      for(i in (reglas + 1):size){
        
        for(j in seq_len(length(max)))
          
          population[i,j] <- sample(0:(max[j]), size = 1, replace = TRUE)
      }
      
  } else { # reglas DNF 
    
    # Random Init 
    v <- sample(x = c(0,1), size = max[length(max)] * size, replace = TRUE)
    population <- matrix(data = v, nrow = size, ncol = max[length(max)],byrow = TRUE)
    
    #Biased Init
    nReglasABorrar <- length(max) - 1 - numVarMax
    for(i in (reglas + 1):size){
      varia <- sample(length(max) - 1, size = nReglasABorrar, replace = FALSE)
      for(j in seq_len(nReglasABorrar)){
        population[i, ] <- .eraseGene(rule = population[i,], variable = varia[j], maxVariablesValue = max, DNF_Rules = TRUE)
      }
    }
  }

  return(population)
}









#'
#' @title Double-point crossover operator 
#' 
#' @param object A "ga" class object
#' @param parents A vector of size = 2 with the index of the parents to cross.
#' @param ... Additional parameters
#' 
#' @return a matrix with the two new generated individuals by rows
#' @noRd
#' 
.ga_dpCrossover <- function(object, parents, ...)
{


  if( ! object@DNFRules){ #CAN RULES
     
      parents <- object@population[parents,,drop = FALSE]
      n <- ncol(parents)
      children <- matrix(as.double(NA), nrow = 2, ncol = n)
      fitnessChildren <- rep(NA, 2)
      crossOverPoint1 <- sample(seq_len(n), size = 1, replace = TRUE)  
      if(crossOverPoint1 == (n) )
      { crossOverPoint2 <- n   } else {
      crossOverPoint2 <- sample((crossOverPoint1 + 1):n, size = 1, replace = TRUE)
      }
      
      children[1,] <- parents[1,]
      children[2,] <- parents[2,]
      
      
      
      children[1, crossOverPoint1:crossOverPoint2] <- parents[2, crossOverPoint1:crossOverPoint2]
      children[2, crossOverPoint1:crossOverPoint2] <- parents[1, crossOverPoint1:crossOverPoint2]
      
      out <- list(children = children, fitness = fitnessChildren)
      return(out)
  
      } else { # DNF RULES
      
        
        parents <- object@population[parents,,drop = FALSE]
        max <- object@max
        n <- length(max)
        children <- matrix(as.double(NA), nrow = 2, ncol = max[length(max)])
        fitnessChildren <- rep(NA, 2)
        
        rangCrossover1 <- 2:(n) 
        if(length(rangCrossover1) > 1){
          crossOverPoint1 <- sample(rangCrossover1, size = 1, replace = TRUE)  
        } else {
          crossOverPoint1 <- rangCrossover1
        }
        
        if(crossOverPoint1 == (n) )
        { crossOverPoint2 <- n   } else {
          rangCrossover2 <- (crossOverPoint1 + 1):n
          if(length(rangCrossover2) > 1){
            crossOverPoint2 <- sample(rangCrossover2, size = 1, replace = TRUE)
          } else {
            crossOverPoint2 <- rangCrossover2
          }
        }
      
          range <- (max[crossOverPoint1 - 1] + 1):max[crossOverPoint2]
        
        
        children[1,] <- parents[1,]
        children[2,] <- parents[2,]
        
        children[1, range] <- parents[2, range]
        children[2, range] <- parents[1, range]
        
        out <- list(children = children, fitness = fitnessChildren)
        return(out)
        
    }
}









#'
#' @title Biased mutation operator for SDIGA
#' 
#' @param object A "ga" class object
#' @param parent A vector of size = 1 with the index of the parents to cross.
#' @param ... Additional parameters
#' 
#' @return A new individual.
#' @noRd
#' 
.gaCAN_Mutation <- function(object, parent, ...)
{
  
  toMutate <- parent <- as.vector(object@population[parent,]) 
  toMutate <-  .mutate(chromosome = toMutate, variable = ...[[1]], maxVariablesValue = object@maxValuesRule, DNF_Rule = object@DNFRules )
  
  return(toMutate)
}







#'
#' @title Biased mutation operator for MESDIF
#' 
#' @param object A "ga" class object
#' @param parent A vector of size = 1 with the index of the parents to cross.
#' @param ... Additional parameters
#' 
#' @return A new individual.
#' @noRd
#' 
..gaMESDIF_Mutation <- function(object, parent, ...)
{
  
  toMutate <- parent <- as.vector(object@population[parent,]) 
  toMutate <-  .mutateMESDIF(chromosome = toMutate, variable = ...[[1]], maxVariableValues = object@maxValuesRule, DNF_Rule = object@DNFRules )
  
  return(toMutate)
}







#'
#' @title Biased mutation operator for NMEEF-SD
#' 
#' @param object A "ga" class object
#' @param parent A vector of size = 1 with the index of the parents to cross.
#' @param ... Additional parameters
#' 
#' @return A new individual.
#' @noRd
#' 
..gaNMEEF_Mutation <- function(object, parent, ...)
{
  
  toMutate <- parent <- as.vector(object@population[parent,]) 
  toMutate <-  .mutateNMEEF(chromosome = toMutate, variable = ...[[1]], maxVariableValues = object@maxValuesRule, DNF_Rule = object@DNFRules )
  
  return(toMutate)
}







#'
#' @title Selection operator for SDIGA
#' 
#' @param object A "ga" class object
#' @return A list with the two individuals to add on the population 
#' @noRd
#' 
.ga_SDIgaSelection <- function(object){
  n <- object@popSize 
  object@population[(n + 1):nrow(object@population), ] <- NA
  
  
  object@fitness[(n + 1):length(object@fitness)] <- NA
  
  list(population = object@population, fitness = object@fitness)
  
}






#'
#' @title Binary tournament selection operator for MESFID
#' 
#' @param elitePop Size of the elite population
#' @param sizePop Size of the population
#' @param nvars Number of variables 
#' @param FitnessElite A matrix with the fitness values of the elite population
#' @param ObjValues A matrix with the fitness functions for the rest of the individuals of the population
#' @param ... Additional parameters
#' @return 
#' A list with the new population, the elite fitness values and the fitness for the population
#' @noRd
#' 

.ga_MESDIFBinTournamentSelection <- function(elitePop, sizePop, nvars, FitnessElite, ObjValues){
  newPop <- matrix(NA, nrow = sizePop, ncol = nvars)
 
  nas <- which(is.na(elitePop[,1,drop = F]))
  if(length(nas) > 0 )ObjValues <- ObjValues[- nas, , drop = F]
  elitePop <- na.exclude(elitePop)

  selection <- sample(NROW(elitePop), size = sizePop * 2, replace = TRUE)
  Fitness <- numeric(sizePop)
  Obj <- matrix(NA, nrow = sizePop + NROW(elitePop), ncol = 4)
  
  fit <- FitnessElite[selection] 
  sel <- matrix(selection, nrow = 2)
  fit <- matrix(fit, nrow = 2)
  
  winner <- fit[1,] <= fit[2,]

  num <- sum(winner)
 
  if(num > 0){
    b <- which(winner)
    a <- sel[1, b]
    newPop[b, ] <- elitePop[a, ]
    Fitness[b] <- FitnessElite[a]
    Obj[b, ] <- ObjValues[a, ]
  } 
  b <- which(!winner)
  a <- sel[2, b]
  newPop[b, ] <- elitePop[a, ]
  Fitness[b] <- FitnessElite[a]
  Obj[b, ] <- ObjValues[a, ]

  Obj[(sizePop + 1):NROW(Obj), ] <- ObjValues
  list(population = newPop, fitness = Fitness, obj = Obj)
  
  
}





#
#
# This function execute the corresponding genetic algorithm in function of the value of 'algorithm'
#
#

.executeGA <- function(algorithm, dataset, targetClass, n_vars, to_cover, nLabels, N_evals, tam_pob, p_cross = 0.5, p_mut, seed, Objectives = c(.LocalSupport, .confidence, NULL, FALSE), Weights = c(0.7,0.3,0), DNFRules = FALSE, cate, num, elitism = 5, porcCob = 0.5, strictDominance = TRUE, reInit = TRUE, minCnf = 0.6){
  
  ma <- dataset$sets
  
if(DNFRules) {
  ma <- Reduce(f = '+', x = ma, accumulate = TRUE)
  ma <- c(0,ma)
}
  # For DNF rules, we must use 'binary' as 'type' instead of 'min' and 'max'
  # also, we must use the value nBits, that indicates the number of genes per chromosome
  # The value max is neccessary to know how many genes belongs to each variable
  switch(algorithm, 
  "SDIGA" = { result <- .gaSDIGA(type = if(!DNFRules) "real-valued" else "binary", 
                  fitness = .fitnessFunction, dataset, matrix(unlist(.separate(dataset)), nrow = length(dataset[[2]]) - 1, ncol = length(dataset[[7]])), targetClass, to_cover, n_vars, nLabels, ma, FALSE, Objectives, Weights, DNFRules, Objectives[[4]], FALSE,  cate, num,
                  min = dataset[[4]][-length(dataset[[4]])],
                  max = ma,
                  nBits = ma[length(ma)],
                  population = .generatePopulation,
                  selection = .ga_SDIgaSelection,
                  crossover = .ga_dpCrossover,
                  mutation = .gaCAN_Mutation,  
                  popSize = tam_pob,
                  pcrossover = 1 / tam_pob, 
                  pmutation = p_mut, # / length(ma), #Mutation probability applied at the gene
                  elitism = 0,
                  maxiter = N_evals,#floor( (N_evals - tam_pob) / (2 + tam_pob  * p_mut)),
                  run = N_evals,
                  #  maxfitness = 1,
                  names = dataset[[2]][1:n_vars],
                  keepBest = FALSE,
                  parallel = FALSE,
                  monitor = NULL,
                  DNFRules = DNFRules,
                  seed = seed) 
  }, 
  "MESDIF" =  { result <- .gaMESDIF(type = if(!DNFRules) "real-valued" else "binary", 
                                #fitness = .fit12, dataset, .separate(dataset = dataset), targetClass, to_cover, n_vars, nLabels, ma, FALSE, Objectives, Weights, DNFRules, Objectives[[4]], FALSE,  cate, num,# Parametros de .fit12
                                fitness = .fitnessMESDIF, dataset, matrix(unlist(.separate(dataset)), nrow = length(dataset[[2]]) - 1, ncol = length(dataset[[7]])), targetClass, to_cover, n_vars, nLabels, ma, FALSE, Objectives, c(0.7, 0.3, 0), DNFRules, Objectives[[4]], FALSE,  cate, num,
                                min = dataset[[4]][-length(dataset[[4]])],
                                max = ma,
                                nBits = ma[length(ma)],
                                population = .generateMESDIFPopulation,
                                selection = .ga_MESDIFBinTournamentSelection,
                                crossover = .ga_dpCrossover,
                                mutation = ..gaMESDIF_Mutation,  
                                popSize = tam_pob,
                                pcrossover = p_cross, 
                                pmutation = p_mut / length(ma),
                                elitism = elitism,
                                maxiter = N_evals,
                                run = N_evals, # No queremos que se detenga la evaluacion.
                                names = dataset[[2]][1:n_vars],
                                keepBest = FALSE,
                                parallel = FALSE,
                                monitor = NULL,
                                DNFRules = DNFRules,
                                seed = seed) 
                return(result)
  }, 
  "NMEEFSD" =  { result <- .gaNMEEF(type = if(!DNFRules) "real-valued" else "binary", 
                                      #fitness = .fit12, dataset, .separate(dataset = dataset), targetClass, to_cover, n_vars, nLabels, ma, FALSE, Objectives, Weights, DNFRules, Objectives[[4]], FALSE,  cate, num,# Parametros de .fit12
                                      fitness = .fitnessMESDIF, dataset, matrix(unlist(.separate(dataset)), nrow = length(dataset[[2]]) - 1, ncol = length(dataset[[7]])), targetClass, to_cover, n_vars, nLabels, ma, FALSE, Objectives, c(0.7,0.3,0), DNFRules, Objectives[[4]], FALSE,  cate, num, TRUE, 
                                      min = dataset[[4]][-length(dataset[[4]])],
                                      max = ma,
                                      nBits = ma[length(ma)],
                                      population = .generarPoblacionNMEEF,
                                      selection = .selectionNMEEF,
                                      crossover = .ga_dpCrossover,
                                      mutation = ..gaNMEEF_Mutation,  
                                      popSize = tam_pob,
                                      pcrossover = p_cross, 
                                      pmutation = p_mut,
                                      elitism = 0,
                                      maxiter = N_evals,
                                      run = N_evals, 
                                      names = dataset[[2]][1:n_vars],
                                      keepBest = FALSE,
                                      parallel = FALSE,
                                      monitor = NULL,
                                      DNFRules = DNFRules,
                                      seed = seed, 
                                      porcCob = porcCob,
                                      StrictDominance = strictDominance,
                                      reInitPop = reInit,
                                      minCnf = minCnf) 
  return(result)
  }
  )
  #Only for SDIGA
  .getBestRule(result)
}







#' @title Calculates the fitness function of a rule.
#' 
#' @param rule The rule in CAN or DNF representation
#' @param dataset A SDEFSR_Dataset object 
#' @param noClass The data field of the SDEFSR_Dataset object without the target class
#' @param targetClass The target variable
#' @param to_cover The number of examples belonging to the target class that are not covered yet.
#' @param n_Vars The number of variables in the dataset
#' @param nLabels The number of fuzzy labels for numeric variables
#' @param maxRule The maximum value of fuzzy labels or the number of categorical values for each variable
#' @param mark Indicates if the examples must be marked (only for SDIGA)
#' @param Objectives A vector with the objective values
#' @param Weights The weight of each objective
#' @param DNFRules Indicates is rule are in DNF representation
#' @param fuzzy Indicates the computation of the fuzzy belonging degree
#' @param test Indicates if the computation must be done to get test results
#' @param cate A vector with index for categorical variables
#' @param num A vector with index for numerical variables
#' 
#' @return 
#' Return the fitness values for each objective or a sets of values to mark examples on SDIGA algorithm
#' 
#' @noRd
.fitnessFunction <- function(rule, dataset, noClass, targetClass, to_cover, n_Vars, nLabels, maxRule, mark = FALSE, Objectives = c(.LocalSupport, .confidence, NULL, FALSE), Weights = c(0.7,0.3,0), DNFRules = FALSE, fuzzy = FALSE ,test = FALSE, cate, num){

  
  if( ! any(is.na(rule))) { #If rule has NAs, it cannot be evaluated
    
    rule <- as.integer(rule)
    participants <- logical(length(maxRule))
    participants <- .getParticipants(rule = rule, maxRule = maxRule, DNFRules = DNFRules)
    
    
    #If it's not the empty rule
    if(any(participants)){
      
      cat_particip <- which(cate & participants)
      num_particip <- which(num & participants)
      
      max_rule_cat <- maxRule[cat_particip]
      max_rule_num <- maxRule[num_particip]
      
      if(!DNFRules) { # CAN RULES
        
        #Split into numerical variables and categorical ones. (And participate in the rule)
        if(length(cat_particip) > 0){
          rule_cat <- rule[cat_particip]
        }
        
        if(length(num_particip) > 0){
          rule_num <- rule[num_particip]
          
          fuzzy_sets <- dataset[["fuzzySets"]][1:nLabels, 1:3, num_particip, drop = F]
          crispSets <- dataset[["crispSets"]][1:nLabels, 1:2, num_particip, drop = F]
          #  Get values for xmin, xmedio and xmax for fuzzy computation.   
          n_matrices <- dim(fuzzy_sets)[3]  
   
          xmin <- fuzzy_sets[cbind(rule_num + 1, 1, seq_len(n_matrices))]
          xmax <- fuzzy_sets[cbind(rule_num + 1, 3, seq_len(n_matrices))]
          xmedio <- fuzzy_sets[cbind(rule_num + 1, 2, seq_len(n_matrices))]
          
          #Get values for xmin and xmax for crisp computation
          n_matricesCrisp <- dim(crispSets)[3]  
          xminC <- crispSets[cbind(rule_num + 1, 1, seq_len(n_matricesCrisp))]
          xmaxC <- crispSets[cbind(rule_num + 1, 2, seq_len(n_matricesCrisp))]
        }
        
        gr_perts <- .compareCAN(example = noClass, rule_cat = rule_cat, rule_num = rule_num, catParticip = cat_particip, numParticip = num_particip, xmin = xmin, xmedio = xmedio, xmax = xmax, n_matrices = n_matrices, xminCrisp = xminC, xmaxCrisp = xmaxC,  max_rule_cat)
        
      } else { # DNF RULES 
        
        
        
        
        valNum <- mapply(FUN = ':', (max_rule_num + 1), (max_rule_num + nLabels), SIMPLIFY = FALSE)  
        ruleNum <- lapply(X = valNum, FUN = function(x, rule) rule[x], rule )
        
        if(length(num_particip) > 0){
          
          fuzzy_sets <- dataset[["fuzzySets"]][1:nLabels, 1:3, num_particip, drop = F]
          crispSets <- dataset[["crispSets"]][1:nLabels, 1:2, num_particip, drop = F]
          
          #  Gets values for xmin, xmedio and xmax for fuzzy computation. 
          # The format is a matrix, which columns has at first value the number of numerical 
          # variable, and then, the values for xmin, xmedio, xmax, and only for values that participate in the rule
          n_matrices <- dim(fuzzy_sets)[3] 
          valuesFuzzy <- .getFuzzyValues(rule_num = ruleNum, fuzzy = fuzzy_sets)
         
          #Gets values for xmin and xmax for crisp computation
          n_matricesCrisp <- dim(crispSets)[3]  
          valuesCrisp <- .getFuzzyValues(rule_num = ruleNum, fuzzy = crispSets, crisp = TRUE)
        }
        
        #gr_perts <- lapply(X = noClass, FUN = .comparaDNF3, rule = rule, ruleNum, cat_particip, num_particip,  max_rule_cat, max_rule_num, nLabels, fuzzy_sets, crispSets, valuesFuzzy, valuesCrisp)
        gr_perts <- .compareDNF(example = noClass, rule = rule, ruleNum, cat_particip, num_particip,  max_rule_cat, max_rule_num, nLabels, fuzzy_sets, crispSets, valuesFuzzy, valuesCrisp)
        
        
        #gr_perts <- unlist(gr_perts)
      }
      
      
      values <- .getValuesForQualityMeasures(gr_perts = gr_perts, classNames = dataset[["class_names"]], dataset = dataset[["data"]], targetClass = targetClass, examples_perClass = dataset[["examplesPerClass"]],cov = dataset[["covered"]], Ns = dataset[["Ns"]], N_vars = n_Vars + 1, to_cover = to_cover, mark = mark, test = test, fuzzy = fuzzy)
      
      #Compute fitness
      if(! mark){
      
        fitness <- 0
        if(is.function(Objectives[[1]]) && Weights[1] > 0){ 
          fitness <- fitness + (Objectives[[1]](values) * Weights[1])
        }
        if(is.function(Objectives[[2]]) && Weights[2] > 0){ 
          fitness <- fitness + (Objectives[[2]](values) * Weights[2])
        }
        if(is.function(Objectives[[3]]) && Weights[3] > 0) {
          fitness <- fitness + (Objectives[[3]](values) * Weights[3])
        }      
        fitness <- fitness / (sum(Weights))
        # cat("Ns:", values[[4]], " - Local Support: ", .LocalSupport(values), " - .confidence:", .confidence(values), " - Support: ", .Csupport(values)," - .coverage:", .coverage(values), " - Fitness: ", fitness, file = "", fill = TRUE)
        
        fitness #Return
      } else {
        
        values #Return
      }
      
    } else{
      0 #Return
    }
    
  } else {
    0 #Return
  }
  
}


#' @title Calculates the fitness function of a rule (FOR MESDIF and NMEEF-SD)
#' 
#' @param rule The rule in CAN or DNF representation
#' @param dataset A SDEFSR_Dataset object 
#' @param noClass The data field of the SDEFSR_Dataset object without the target class
#' @param targetClass The target variable
#' @param to_cover The number of examples belonging to the target class that are not covered yet.
#' @param n_Vars The number of variables in the dataset
#' @param nLabels The number of fuzzy labels for numeric variables
#' @param maxRule The maximum value of fuzzy labels or the number of categorical values for each variable
#' @param mark Indicates if the examples must be marked (only for SDIGA)
#' @param Objectives A vector with the objective values
#' @param Weights The weight of each objective
#' @param DNFRules Indicates is rule are in DNF representation
#' @param fuzzy Indicates the computation of the fuzzy belonging degree
#' @param test Indicates if the computation must be done to get test results
#' @param cate A vector with index for categorical variables
#' @param num A vector with index for numerical variables
#' @param NMEEF Indicates is the fitness returned is for the NMEEF-SD algorithm or not.
#' 
#' @return 
#' Return the fitness values for each objective or a sets of values to mark examples on SDIGA algorithm
#' 
#' @noRd
.fitnessMESDIF <- function(rule, dataset, noClass, targetClass, to_cover, n_Vars, nLabels, maxRule, mark = FALSE, Objectives = c(.LocalSupport, .confidence, NULL, FALSE), Weights = c(0.7,0.3,0), DNFRules = FALSE, fuzzy = FALSE ,test = FALSE, cate, num, NMEEF = FALSE){
  
  if( ! any(is.na(rule))) {
    
    rule <- as.numeric(rule)
    participants <- logical(length(maxRule))
    participants <- .getParticipants(rule = rule, maxRule = maxRule, DNFRules = DNFRules)
    
    
    #If it's not the empty rule
    if(any(participants)){
      
      cat_particip <- which(cate & participants)
      num_particip <- which(num & participants)
      
      max_rule_cat <- maxRule[cat_particip]
      max_rule_num <- maxRule[num_particip]
      
      if(!DNFRules) { # CAN RULES
        
        #Split into numerical variables and categorical ones. (And participate in the rule)
        
        rule_cat <- rule[cat_particip]
        rule_num <- rule[num_particip]
        
        if(length(num_particip) > 0){
          fuzzy_sets <- dataset[["fuzzySets"]][1:nLabels, 1:3, num_particip, drop = F]
          crispSets <- dataset[["crispSets"]][1:nLabels, 1:2, num_particip, drop = F]
          #  Get values for xmin, xmedio and xmax for fuzzy computation.   
          n_matrices <- dim(fuzzy_sets)[3]  
          xmin <- fuzzy_sets[cbind(rule_num + 1, 1, seq_len(n_matrices))]
          xmax <- fuzzy_sets[cbind(rule_num + 1, 3, seq_len(n_matrices))]
          xmedio <- fuzzy_sets[cbind(rule_num + 1, 2, seq_len(n_matrices))]
          
          #Get values for xmin and xmax for crisp computation
          n_matricesCrisp <- dim(crispSets)[3]  
          xminC <- crispSets[cbind(rule_num + 1, 1, seq_len(n_matricesCrisp))]
          xmaxC <- crispSets[cbind(rule_num + 1, 2, seq_len(n_matricesCrisp))]
        }
        
        gr_perts <- .compareCAN(example = noClass, rule_cat = rule_cat, rule_num = rule_num, catParticip = cat_particip, numParticip = num_particip, xmin = xmin, xmedio = xmedio, xmax = xmax, n_matrices = n_matrices, xminCrisp = xminC, xmaxCrisp = xmaxC,  max_rule_cat)
        
      } else { # DNF RULES
        
        
        
        
        valNum <- mapply(FUN = ':', (max_rule_num + 1), (max_rule_num + nLabels), SIMPLIFY = FALSE)  
        ruleNum <- lapply(X = valNum, FUN = function(x, rule) rule[x], rule )
        
        if(length(num_particip) > 0){
          
          fuzzy_sets <- dataset[["fuzzySets"]][1:nLabels, 1:3, num_particip, drop = F]
          crispSets <- dataset[["crispSets"]][1:nLabels, 1:2, num_particip, drop = F]
          
          #  Gets values for xmin, xmedio and xmax for fuzzy computation. 
          # The format is a matrix, which columns has at first value the number of numerical 
          # variable, and then, the values for xmin, xmedio, xmax, and only for values that participate in the rule
          n_matrices <- dim(fuzzy_sets)[3] 
          valuesFuzzy <- .getFuzzyValues(rule_num = ruleNum, fuzzy = fuzzy_sets)
          
          #Gets values for xmin and xmax for crisp computation
          n_matricesCrisp <- dim(crispSets)[3]  
          valuesCrisp <- .getFuzzyValues(rule_num = ruleNum, fuzzy = crispSets, crisp = TRUE)
        }
        
        gr_perts <- .compareDNF(example = noClass, rule = rule, ruleNum, cat_particip, num_particip,  max_rule_cat, max_rule_num, nLabels, fuzzy_sets, crispSets, valuesFuzzy, valuesCrisp)
        

      }
      
      
      if(!DNFRules)
        values <- .getValuesForQualityMeasures(gr_perts = gr_perts, classNames = dataset[["class_names"]], dataset = dataset[["data"]], targetClass = targetClass, examples_perClass = dataset[["examplesPerClass"]],cov = dataset[["covered"]], Ns = dataset[["Ns"]], N_vars = n_Vars + 1, to_cover = to_cover, mark = mark, test = test, fuzzy = fuzzy, NMEEF)
      else
        values <- .getValuesForQualityMeasures(gr_perts = gr_perts, classNames = dataset[["class_names"]], dataset = dataset[["data"]], targetClass = targetClass, examples_perClass = dataset[["examplesPerClass"]],cov = dataset[["covered"]], Ns = dataset[["Ns"]], N_vars = n_Vars + 1, to_cover = to_cover, mark = mark, test = test, fuzzy = fuzzy, NMEEF)
      
      #Compute fitness
      if(! mark){
        fitness <- numeric(4)
        fitness[1] <- if(is.function( Objectives[[1]])) Objectives[[1]](values) else 0
        fitness[2] <- if(is.function( Objectives[[2]])) Objectives[[2]](values) else 0
        fitness[3] <- if(is.function( Objectives[[3]])) Objectives[[3]](values) else 0
        if(! NMEEF)
          fitness #Return
        else 
          list(fit = fitness, covered = values[[13]]) # Return
      } else {
        
        values #Return
      }
      
    } else{
      c(0,0,0,0) #Return
    }
    
  } else {
    c(0,0,0,0) #Return
  }
  
}


#'
#' Obtains the belonging degree of every example of a dataset to a given rule
#' 
#' @param rule The rule to compare example. This rule must be in canonica vector representation. (See Rule.toRuleCANRepresentation function)
#' @param dataset The complete SDEFSR_Dataset dataset object to get the examples
#' @param noClass a matrix with all examples without the class attribute. One examples PER COLUMN
#' @param nLabels number of fuzzy Labels that have numerical attributes
#' @param maxRule maximum value of all attributes ($sets of the SDEFSR_Dataset dataset)
#' @param cate logical vector indicating which attributes are categorical
#' @param num logical vector indicating which attributes are numerical
#' @param The T-norm to use. 0 to Minimum T-norm, 1 to Product T-norm.
#'
#' @return a numeric vector with the belonging degree of every example to the given rule.
#' @noRd
.fitnessFuGePSD <- function(rule, dataset, noClass, nLabels, maxRule, cate, num, t_norm){
  
  
  if( ! any(is.na(rule))) {
    
    rule <- as.integer(rule)
    participants <- logical(length(maxRule))
    participants <- .getParticipants(rule = rule, maxRule = maxRule, DNFRules = FALSE)
    
    
    #If it's not the empty rule
    if(any(participants)){
      
      cat_particip <- which(cate & participants)
      num_particip <- which(num & participants)
      
      max_rule_cat <- maxRule[cat_particip]
      max_rule_num <- maxRule[num_particip]
      
     
        
        #Split into numerical variables and categorical ones. (And participate in the rule)
        if(length(cat_particip) > 0){
          rule_cat <- rule[cat_particip]
        }
        
        if(length(num_particip) > 0){
          rule_num <- rule[num_particip]
          
          fuzzy_sets <- dataset[["fuzzySets"]][1:nLabels, 1:3, num_particip, drop = F]
          crispSets <- dataset[["crispSets"]][1:nLabels, 1:2, num_particip, drop = F]
          #  Get values for xmin, xmedio and xmax for fuzzy computation.   
          n_matrices <- dim(fuzzy_sets)[3]  
          
          xmin <- fuzzy_sets[cbind(rule_num + 1, 1, seq_len(n_matrices))]
          xmax <- fuzzy_sets[cbind(rule_num + 1, 3, seq_len(n_matrices))]
          xmedio <- fuzzy_sets[cbind(rule_num + 1, 2, seq_len(n_matrices))]
          
          #Get values for xmin and xmax for crisp computation
          n_matricesCrisp <- dim(crispSets)[3]  
          xminC <- crispSets[cbind(rule_num + 1, 1, seq_len(n_matricesCrisp))]
          xmaxC <- crispSets[cbind(rule_num + 1, 2, seq_len(n_matricesCrisp))]
        }
        
      #return
        Rule.compatibility(example = noClass, rule_cat = rule_cat, rule_num = rule_num, catParticip = cat_particip, numParticip = num_particip, xmin = xmin, xmedio = xmedio, xmax = xmax, n_matrices = n_matrices, max_cat = max_rule_cat, max_num = max_rule_num, t_norm = t_norm)
        
      
    }
  }
}
#         values <- .getValuesForQualityMeasures(gr_perts = gr_perts, classNames = dataset[["class_names"]], dataset = dataset[["data"]], targetClass = targetClass, examples_perClass = dataset[["examplesPerClass"]],cov = dataset[["covered"]], Ns = dataset[["Ns"]], N_vars = n_Vars + 1, por_cubrir = por_cubrir, mark = mark, test = test, fuzzy = fuzzy)
#       
#       #Compute fitness
#       if(! mark){
#         
#         fitness <- 0
#         if(is.function(Objetivos[[1]]) && Weights[1] > 0){ 
#           fitness <- fitness + (Objetivos[[1]](values) * Weights[1])
#         }
#         if(is.function(Objetivos[[2]]) && Weights[2] > 0){ 
#           fitness <- fitness + (Objetivos[[2]](values) * Weights[2])
#         }
#         if(is.function(Objetivos[[3]]) && Weights[3] > 0) {
#           fitness <- fitness + (Objetivos[[3]](values) * Weights[3])
#         }      
#         fitness <- fitness / (sum(Weights))
#         # cat("Ns:", values[[4]], " - Local Support: ", .LocalSupport(values), " - .confidence:", .confidence(values), " - Support: ", .Csupport(values)," - .coverage:", .coverage(values), " - Fitness: ", fitness, file = "", fill = TRUE)
#         
#         fitness #Return
#       } else {
#         
#         values #Return
#       }
#       
#     } else{
#       0 #Return
#     }
#     
#   } else {
#     0 #Return
#   }
  







