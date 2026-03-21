
#
#
#    Re-initialization operator based on coverage.
#  
#
#

.reInitPob <- function(elitePop, fitnessElite, coveredElite, crowdingDistance, pctVariables, coveredNow, dataset, maxRule, cate, num, crispSets, targetClass, popSize){
  #Allocate memory for all neccesary variables
  returnn <- matrix(ncol = NCOL(elitePop), nrow = popSize)
  fitnessReturnn <- matrix(ncol = 4, nrow = popSize*2)
  crowding <- numeric(popSize * 2)
  numVariables <- round(NCOL(elitePop) * pctVariables)
  coveredReturn <- matrix(FALSE, nrow = NROW(coveredElite), ncol = popSize)
  
  #Remove duplicated in elitePop and add this into returnn
  notDuplicated <- which(! duplicated(elitePop))
  elitePop <- elitePop[notDuplicated,, drop = F]
  fitnessElite <- fitnessElite[notDuplicated,, drop = F]
  crowdingDistance <- crowdingDistance[notDuplicated]
  coveredElite <- coveredElite[, notDuplicated, drop = F]
  numIndividuals <- NROW(elitePop)
    
  #Add the elitePop (the pareto front) to the returnn population
    returnn[seq_len(numIndividuals), ] <- elitePop
    fitnessReturnn[seq_len(NROW(fitnessElite)), ] <- fitnessElite
    crowding[seq_len(length(crowdingDistance))] <- crowdingDistance
    coveredReturn[,seq_len(NCOL(coveredElite))] <- coveredElite

  remaining <- popSize - numIndividuals
  # Initialization based on coverage of the rest of the population 
  # Find an example not covered and has the target class
  rule <- numeric(length(maxRule))
  
  for(i in seq_len(remaining)){
    rule[] <- 0
    rule <- rule + maxRule  #Add the no-participation value to all variables
    
    # Find an example not covered and has the target class
    notCovered <- which(! coveredNow)
    notCovered <- which( dataset[NROW(dataset), notCovered] == targetClass )
    
    if(length(notCovered) > 0)
           if(length(notCovered > 1))  #If there is more than one example, select one randomly
            example <- dataset[seq_len(NROW(dataset) - 1), sample(notCovered, size = 1)]
           else
             example <- dataset[seq_len(NROW(dataset) - 1), notCovered]
         else # If all examples are covered, get one randomly
           example <- dataset[seq_len(NROW(dataset) - 1), sample(NCOL(dataset), size = 1)]
      
         #Get the values of all variables that cover the example
            values <- numeric(length(example))
            for(ii in seq_len(length(values))){
              if(cate[ii])
                values[ii] <- example[ii] + 1   # +1 because categories are numbered from zero to maxValue -1
              
              if(num[ii]){ # Numerical variable
                #Get the interval the variable belongs to
               values[ii] <- which( example[ii] > (crispSets[,1,ii] + .tolerance) & example[ii] <= (crispSets[,2,ii] + .tolerance)) - 1
              }
            }
            
            #Now we have a rule that fits perfectly the selected example.
            #The next lines select a random number of these variales to generate a rule
            #that cover the selected example but with less variables 
            numInterv <- sample(numVariables, size = 1)
            vars <- sample(length(maxRule), size = numInterv, replace = FALSE)
            
            rule[vars] <- values[vars]
            numIndividuals <- numIndividuals + 1
            returnn[numIndividuals,] <- rule
  }
  
  #Return 
  list(pop = returnn, fitness = fitnessReturnn, crowd = crowding, cov = coveredReturn)
}





#
#
# Selection operator for NMEEF-SD
#
#

.selectionNMEEF <- function(pop, popSize, rank, crowding, fitness, covered) {
  returnn <- matrix(nrow = popSize, ncol = ncol(pop))
  fitnessReturnn <- matrix(nrow = popSize, ncol = 4)
  coveredReturn <- matrix(nrow = NROW(covered), ncol = popSize)
  mating <- matrix(NA, nrow = 2, ncol = popSize)
  
  equals <- seq_len(popSize) # Tournaments among the same individual isnt allowed
  #Precompute which individals are chosen for the tournament
 while(length(equals > 0)){
    mating[,equals] <- matrix(sample(seq_len(popSize), size = length(equals) * 2, replace = TRUE) , nrow = 2)
    equals <- which(mating[1,] == mating[2,])
  }
  
  # First, we compare the individuals by his rank value (less is better)
  winners1 <- which(rank[mating[1,]] < rank[mating[2,]])
  winners2 <- which(rank[mating[2,]] < rank[mating[1,]])
  
  # Put the winners individuals into the returned population
  pos <- 1
  if (length(winners1) > 0) {
    returnn[seq_len(length(winners1)),] <- pop[mating[1,winners1],]
    fitnessReturnn[seq_len(length(winners1)),] <- fitness[mating[1,winners1],]
    coveredReturn[,seq_len(length(winners1))] <- covered[,mating[1,winners1]]
    pos <- pos + length(winners1)
  }
  if (length(winners2) > 0) {
    returnn[pos:(pos+length(winners2) -1),] <- pop[mating[2,winners2],]
    fitnessReturnn[pos:(pos+length(winners2) -1 ),] <- fitness[mating[2,winners2],]
    coveredReturn[,pos:(pos+length(winners2) -1 )] <- covered[,mating[2,winners2]]
    pos <- pos + length(winners2)
  }
  
  
  #If there are ties, we solve it by his crowding distance (more is better)
  equals <- which(rank[mating[2,]] == rank[mating[1,]])
  if (length(equals) > 0) {
    mating <- mating[,equals, drop = F]
    
    winners1 <- which(crowding[mating[1,]] >= crowding[mating[2,]])
    winners2 <- which(crowding[mating[2,]] > crowding[mating[1,]])
    
    #Put the winners into the returned population
    if (length(winners1) > 0) {
      returnn[pos:(pos+length(winners1) - 1),] <- pop[mating[1,winners1],]
      fitnessReturnn[pos:(pos+length(winners1) -1),] <- fitness[mating[1,winners1],]
      coveredReturn[,pos:(pos+length(winners1) -1 )] <- covered[,mating[1,winners1]]
      pos <- pos + length(winners1)
    }
    if (length(winners2) > 0) {
      returnn[pos:(pos+length(winners2) -1),] <- pop[mating[2,winners2],]
      fitnessReturnn[pos:(pos+length(winners2) -1),] <- fitness[mating[2,winners2],]
      coveredReturn[,pos:(pos+length(winners2) -1 )] <- covered[,mating[2,winners2]]
    }
  }
  
  #Returns the selected population, the fitness of this individuals and his covered examples. (crowding distance will be computed after)
  list(population = returnn, fitness = fitnessReturnn, cov = coveredReturn)
  
}







#
#
#  Function to calculate dominance between individuals.
#
#

.calculateDominance <- function(q, p, strictDominance){
  #We calculate if p domain q only.
  
  domineP <- F
  domineQ <- F
  
  if(strictDominance){
    domineP <- any(p > q)
    domineQ <- any(p < q)
  } else {
    domineQ <- any(p < q)
    domineP <- any(p >= q)
  }
  
  #return
  if(domineQ == domineP)
    return(0L)
  if(domineQ)
    return(1L)
  if(domineP)
    return(-1L)
}






#
#
#  This function calculate the crowding distance of an individual in a front
#
#

.crowdingDistance <- function(pop){
  if(is.vector(pop)) 
    size <- 1
  else
    size <- NROW(pop)
  
 #Preallocate memory 
  distance <- numeric(size)
  measures <- numeric(size)
  num_measures <- seq_len(NCOL(pop))
  
  #Calculate the crowding distance
  #If there are only two or less individuals, its crowding distance is infinity
  if(size <= 2){
    distance <- Inf
  } else {
    #For every measure 
    for( i in num_measures){
      measures <- pop[,i]
      #Order measures of individuals 
      indices <- order(measures, decreasing = FALSE)

      #Set boundary individuals distance as infinity
      #(For this reason, with size <= 2 we dont need to compute the distance)
      distance[c(indices[1], indices[size])] <- Inf
      
      #Compute the crowding distance of the remaining individuals
      for(j in 2:(size - 1)){
        auxDistance <- measures[indices[j + 1]] - measures[indices[j - 1]]
        if(auxDistance != 0){
          distance[indices[j]] <- distance[indices[j]] + (auxDistance / (measures[indices[size]] - measures[indices[1]]) )
        }
      }
    }
  }
  #Return
  distance
}






#
#
# This function fills the population that participate in the next generation of the evolutionary process
#
#

.fillPopulation <- function(fronts, numFronts, fitness, coveredFronts, popSize, nObjs){
  suma <- 0
  if(is.vector(fronts[[1]]))
    cols <- length(fronts[[1]])
  else
    cols <- NCOL(fronts[[1]])
  
  newPop <- matrix(nrow = popSize, ncol = cols)
  newRank <- numeric(popSize * 2) + Inf  #No ranked indivudals cant be selected
  distance <- numeric(popSize)
  fit <- matrix(ncol = 4, nrow = popSize)
  front <- 1
  coveredbyInd <- matrix(FALSE, nrow = NROW(coveredFronts[[1]]), ncol= popSize * 2)
  #Indicate if the last front introduced in the population fits perfectly or we have to make the ordering by crowding distance of the front
  FitPerfectly <- TRUE
  
  
  for(i in seq_len(numFronts)){
    #If front fits in newPop, we introduce completely in it
    front <- i
    if(! is.vector(fronts[[i]]))
      rows <- NROW(fronts[[i]])
    else 
      rows <- 1
    
    if( rows + suma <= popSize ){
      
      #Calculate crowding Distance
      distance[(suma+1):(rows + suma)] <- .crowdingDistance(fitness[[i]][, seq_len(nObjs)])
      #Add front in the new population, and update the rest of parameters.
      newPop[(suma+1):(rows + suma), ] <- fronts[[i]]
      newRank[(suma+1):(rows + suma)] <- i - 1
      fit[(suma+1):(rows + suma), ] <- fitness[[i]]
      coveredbyInd[, (suma+1):(rows + suma)] <- coveredFronts[[i]]
      suma <- suma + rows
    } else {
      break
    }
  }
  
    #If suma is less than popSize, front "front" doesnt fit completely, so we must order by crowding distance
    #and insert only the best individuals
    if(suma < popSize){
      FitPerfectly <- FALSE
      toFill <- popSize - suma
      #Calculate crowding distance
      distance2 <- .crowdingDistance(fitness[[front]])
      
      #Order by crowding distance and get the best individuals till the population is filled
      #orden <- order(distance2, decreasing = TRUE)[seq_len(toFill)]
      orden <- .qsort(distance2, 1, length(distance2), index = seq_len(length(distance2)))
      orden <- orden$indices[length(orden$vector):(length(orden$vector) - toFill + 1) ]
      
      #Fill the population and update parameters
      newPop[(suma + 1):popSize, ] <- fronts[[front]][orden, , drop = F]
      distance[(suma + 1):popSize] <- distance2[orden]
      newRank[(suma+1):popSize] <- front - 1
      fit[(suma + 1):popSize, ] <- fitness[[front]][orden, , drop = F]
      coveredbyInd[, (suma + 1):popSize] <- coveredFronts[[front]][,orden,drop = F]
    }
  
  #Return
  list(population = newPop, distance = distance, fitness = fit, rank = newRank, cov = coveredbyInd, fits = FitPerfectly)
  
  
}






#
#
# Mutation operator for NMEEF-SD
#
#

.mutateNMEEF <- function(chromosome, variable, maxVariableValues, DNF_Rule){
  

  mutation_type <- sample(x = 1:2, size = 1, prob = c(6/11, 5/11))   # Type 1 - type 2 randomly
  
  
  if(! DNF_Rule){  #CAN Rules
    if(mutation_type == 1L){
      #If type == 1, erase the variable
      chromosome[variable] <- maxVariableValues[variable] #No participation value is the maximum value for the variable
      
    } else {  
      #Assing a random value to the variable (no participation value is not taken into account)
      value <- sample(x = 0:(maxVariableValues[variable] - 1), size = 1)
      chromosome[variable] <- value 
      
    }
    
  } else { #DNF Rules
    #Get the range where the values of the chromosome belongs to the variable
    variable <- variable + 1
    range <- (maxVariableValues[variable - 1] + 1):maxVariableValues[variable]
    
    
    if(mutation_type == 1){  
      #If a rule is DNF, to erase the variable we put all his values to 0
      chromosome[range] <- 0
      
    } else {  
      #Assign a random value to each value within the range.
      
      chromosome[range] <- sample(x = 0:1 , size = length(range), replace = TRUE)
      
    }
    
  }
  
  
  
  chromosome  # Return
  
}












#' @title Non-dominated Multi-objective Evolutionary algorithm for Extracting Fuzzy rules in Subgroup Discovery (NMEEF-SD)
#' @description Perfoms a subgroup discovery task executing the algorithm NMEEF-SD
#'
#' @param paramFile The path of the parameters file. \code{NULL} If you want to use training and test \code{SDEFSR_Dataset} variables
#' @param training A \code{SDEFSR_Dataset} class variable with training data.
#' @param test A \code{SDEFSR_Dataset} class variable with training data.
#' @param output character vector with the paths of where store information file, rules file and test quality measures file, respectively.
#' @param seed An integer to set the seed used for generate random numbers.
#' @param nLabels Number of linguistic labels for numerical variables.
#' @param nEval An integer for set the maximum number of evaluations in the evolutionary process.
#' @param popLength An integer to set the number of individuals in the population.
#' @param crossProb Sets the crossover probability. A number in [0,1].
#' @param mutProb Sets the mutation probability. A number in [0,1].
#' @param Obj1 Sets the Objective number 1. See \code{Objective values} for more information about the possible values.
#' @param Obj2 Sets the Objective number 2. See \code{Objective values} for more information about the possible values.
#' @param Obj3 Sets the Objective number 3. See \code{Objective values} for more information about the possible values.
#' @param minCnf Sets the minimum confidence that must have a rule in the Pareto front for being returned. A number in [0,1].
#' @param reInitCoverage Sets if the algorithm must perform the reinitialitation based on coverage when it is needed. A string with "yes" or "no".
#' @param porcCob Sets the maximum percentage of variables that participate in the rules generated in the reinitialitation based on coverage. A number in [0,1]
#' @param StrictDominance Sets if the comparison between individuals must be done by strict dominance or not. A string with "yes" or "no".
#' @param targetVariable The name or index position of the target variable (or class). It must be a categorical one.
#' @param targetClass A string specifing the value the target variable. \code{null} for search for all possible values.
#' 
#' 
#' @details This function sets as target variable the last one that appear in \code{SDEFSR_Dataset} object. If you want 
#'     to change the target variable, you can set the \code{targetVariable} to change this target variable.
#'     The target variable MUST be categorical, if it is not, throws an error. Also, the default behaviour is to find
#'     rules for all possible values of the target varaible. \code{targetClass} sets a value of the target variable where the
#'     algorithm only finds rules about this value.
#'     
#'     If you specify in \code{paramFile} something distinct to \code{NULL} the rest of the parameters are
#'     ignored and the algorithm tries to read the file specified. See "Parameters file structure" below 
#'     if you want to use a parameters file.
#' 
#' 
#' @section How does this algorithm work?:
#'     NMEEF-SD is a multiobjetctive genetic algorithm based on a NSGA-II approach. The algorithm
#'     first makes a selection based on binary tournament and save the individuals in a offspring population.
#'     Then, NMEEF-SD apply the genetic operators over individuals in offspring population
#'     
#'     For generate the population which participate in the next iteration of the evolutionary process
#'     NMEEF-SD calculate the dominance among all individuals (join main population and offspring) and then, apply the NSGA-II fast sort algorithm to order
#'     the population by fronts of dominance, the first front is the non-dominated front (or Pareto), the second is 
#'     where the individuals dominated by one individual are, the thirt front dominated by two and so on.
#'     
#'     To promove diversity NMEEF-SD has a mechanism of reinitialization of the population based on coverage
#'     if the Pareto doesnt evolve during a 5%% of the total of evaluations.
#'     
#'     At the final of the evolutionary process, the algorithm returns only the individuals in the Pareto front
#'     which has a confidence greater than a minimum confidence level.
#'     
#' @section Parameters file structure:
#'   The \code{paramFile} argument points to a file which has the necesary parameters for NMEEF-SD works.
#'   This file \strong{must} be, at least, those parameters (separated by a carriage return):
#'   \itemize{
#'     \item \code{algorithm}  Specify the algorithm to execute. In this case. "NMEEFSD"
#'     \item \code{inputData}  Specify two paths of KEEL files for training and test. In case of specify only the name of the file, the path will be the working directory.
#'     \item \code{seed}  Sets the seed for the random number generator
#'     \item \code{nLabels}  Sets the number of fuzzy labels to create when reading the files
#'     \item \code{nEval}  Set the maximun number of \strong{evaluations of rules} for stop the genetic process
#'     \item \code{popLength}  Sets number of individuals of the main population
#'     \item \code{ReInitCob}  Sets if NMEEF-SD do the reinitialization based on coverage. Values: "yes" or "no"  
#'     \item \code{crossProb}  Crossover probability of the genetic algorithm. Value in [0,1]
#'     \item \code{mutProb}  Mutation probability of the genetic algorithm. Value in [0,1]
#'     \item \code{RulesRep}  Representation of each chromosome of the population. "can" for canonical representation. "dnf" for DNF representation.
#'     \item \code{porcCob}  Sets the maximum percentage of variables participe in a rule when doing the reinitialization based on coverage. Value in [0,1]
#'     \item \code{Obj1} Sets the objective number 1. 
#'     \item \code{Obj2} Sets the objective number 2. 
#'     \item \code{Obj3} Sets the objective number 3. 
#'     \item \code{minCnf} Minimum confidence for returning a rule of the Pareto. Value in [0,1] 
#'     \item \code{StrictDominance} Sets if the comparison of individuals when calculating dominance must be using strict dominance or not. Values: "yes" or "no"
#'     \item \code{targetClass}  Value of the target variable to search for subgroups. The target variable \strong{is always the last variable.}. Use \code{null} to search for every value of the target variable
#'   }
#'   
#'   An example of parameter file could be:
#'  \preformatted{
#' algorithm = NMEEFSD
#' inputData = "irisd-10-1tra.dat" "irisd-10-1tra.dat" "irisD-10-1tst.dat"
#' outputData = "irisD-10-1-INFO.txt" "irisD-10-1-Rules.txt" "irisD-10-1-TestMeasures.txt"
#' seed = 1
#' RulesRep = can
#' nLabels = 3
#' nEval = 500
#' popLength = 51
#' crossProb = 0.6
#' mutProb = 0.1
#' ReInitCob = yes
#' porcCob = 0.5
#' Obj1 = comp
#' Obj2 = unus
#' Obj3 = null
#' minCnf = 0.6
#' StrictDominance = yes
#' targetClass = Iris-setosa
#' }
#'   
#' @section Objective values:
#'      You can use the following quality measures in the ObjX value of the parameter file using this values:
#'       \itemize{
#'         \item Unusualness -> \code{unus}
#'         \item Crisp Support -> \code{csup}
#'         \item Crisp Confidence -> \code{ccnf}
#'         \item Fuzzy Support -> \code{fsup}
#'         \item Fuzzy Confidence -> \code{fcnf}
#'         \item Coverage -> \code{cove}
#'         \item Significance -> \code{sign}
#'       }
#'     
#'     If you dont want to use a objetive value you must specify \code{null}
#'  
#'   
#' @return The algorithm shows in the console the following results:
#' \enumerate{
#'  \item The parameters used in the algorithm
#'  \item The rules generated.
#'  \item The quality measures for test of every rule and the global results.
#' 
#'     Also, the algorithms save those results in the files specified in the \code{output} parameter of the algorithm or 
#'     in the \code{outputData} parameter in the parameters file.
#' }
#' 
#' @references Carmona, C., Gonzalez, P., del Jesus, M., & Herrera, F. (2010). NMEEF-SD: Non-dominated Multi-objective Evolutionary algorithm for Extracting Fuzzy rules in Subgroup Discovery. 
#' @examples  
#'    NMEEF_SD(paramFile = NULL, 
#'                training = habermanTra, 
#'                test = habermanTst, 
#'                output = c(NA, NA, NA),
#'                seed = 0, 
#'                nLabels = 3,
#'                nEval = 300, 
#'                popLength = 100, 
#'                mutProb = 0.1,
#'                crossProb = 0.6,
#'                Obj1 = "CSUP",
#'                Obj2 = "CCNF",
#'                Obj3 = "null",
#'                minCnf = 0.6,
#'                reInitCoverage = "yes",
#'                porcCob = 0.5,
#'                StrictDominance = "yes",
#'                targetClass = "positive"
#'                )  
#' \dontrun{
#'       NMEEF_SD(paramFile = NULL, 
#'                training = habermanTra, 
#'                test = habermanTst, 
#'                output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
#'                seed = 0, 
#'                nLabels = 3,
#'                nEval = 300, 
#'                popLength = 100, 
#'                mutProb = 0.1,
#'                crossProb = 0.6,
#'                Obj1 = "CSUP",
#'                Obj2 = "CCNF",
#'                Obj3 = "null",
#'                minCnf = 0.6,
#'                reInitCoverage = "yes",
#'                porcCob = 0.5,
#'                StrictDominance = "yes",
#'                targetClass = "null"
#'                )
#'      }
#' @export       
NMEEF_SD <- function(paramFile = NULL, 
                     training = NULL, 
                     test = NULL, 
                     output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
                     seed = 0, 
                     nLabels = 3,
                     nEval = 10000, 
                     popLength = 100, 
                     mutProb = 0.1,
                     crossProb = 0.6,
                     #RulesRep = "can",
                     Obj1 = "CSUP",
                     Obj2 = "CCNF",
                     Obj3 = "null",
                     minCnf = 0.6,
                     reInitCoverage = "yes",
                     porcCob = 0.5,
                     StrictDominance = "yes",
                     targetVariable = NA,
                     targetClass = "null"
                     )
{
 
  RulesRep <- "can" #NMEEF-SD use always CAN rules
  if(is.null(paramFile)){
    #Generate our "parameters file" from the variables of the function call
    #Check conditions
   
    
    if(is.null(training)) 
      stop("Not provided a 'test' or 'training' file and neither a parameter file. Aborting...")
    
    if(is.null(test))
      test <- training #To execute only one dataset
    
    if(class(training) != "SDEFSR_Dataset" | class(test) != "SDEFSR_Dataset")
      stop("'training' or 'test' parameters is not a SDEFSR_Dataset class")
    
    if(training[[1]] != test[[1]] )
      stop("datasets ('training' and 'test') does not have the same relation name.")
    
    if(length(output) != 3 )
      stop("You must specify three files to save the results.")
    
    parameters <- list(seed = seed, 
                       algorithm = "NMEEFSD",
                       outputData = output,
                       nEval = nEval, 
                       popLength = popLength,
                       nLabels = nLabels,
                       mutProb = mutProb,
                       crossProb = crossProb,
                       RulesRep = RulesRep,
                       Obj1 = Obj1, 
                       Obj2 = Obj2,
                       Obj3 = Obj3,
                       minCnf = minCnf,
                       reInitPob = reInitCoverage,
                       porcCob = porcCob,
                       StrictDominance = StrictDominance,
                       targetClass = targetClass,
                       targetVariable = if(is.na(targetVariable)) training$attributeNames[length(training$attributeNames)] else targetVariable)
  } else {

    # Parameters --------------------------
    parameters <- .read.parametersFile2(file = paramFile)  # Algorithm Parameters
    
    if(parameters[[1]] != "NMEEFSD") stop("Parameters file has parameters for another algorithm, no for \"NMEEF-SD\"")
   
    training <- read.dataset(file = parameters$inputData[1])   # training data 
    if(is.na(parameters$inputData[2])){
      test <- training
    } else {
      test <- read.dataset(file = parameters$inputData[2])        # test data
    }
  }
  if(is.na(parameters$targetVariable))
    parameters$targetVariable <- training$attributeNames[length(training$attributeNames)]
  #Change target variable if it is neccesary
  training <- changeTargetVariable(training, parameters$targetVariable)
  test <- changeTargetVariable(test, parameters$targetVariable)
  #Check if the last variable is categorical.
  if(training$attributeTypes[length(training$attributeTypes)] != 'c' | test$attributeTypes[length(test$attributeTypes)] != 'c')
    stop("Target variable is not categorical.")
  #Set the number of fuzzy labels
  training <- modifyFuzzyCrispIntervals(training, parameters$nLabels)
  training$sets <- .giveMeSets(data_types = training$attributeTypes, max = training$max, n_labels = parameters$nLabels)
  test <- modifyFuzzyCrispIntervals(test, parameters$nLabels)
  test$sets <- .giveMeSets(data_types = test$attributeTypes, max = test$max, n_labels = parameters$nLabels)
  #Set Covered
  #training$covered <- logical(training$Ns)
  test$covered <- logical(test$Ns)
  
  #Remove older result file to overwrite it later.
  #file.remove(parameters$outputData[which(file.exists(parameters$outputData))])
 
  # Set the rule representation for the genetic algorithm
  if(tolower(parameters$RulesRep) == "can"){
    DNF = FALSE
  } else {
    DNF = TRUE
    vars <-  Reduce(f = '+', x = training[["sets"]], accumulate = TRUE)
    vars <- vars[length(vars)]
  }
  
  #Show the execution parameters to the user
  .show_parameters(params = parameters, train = training, test = test)
  contador <- 0
  
  Objectives <- .parseObjectives(parameters = parameters, "NMEEFSD", DNF)
  
  if(all(is.na(Objectives[1:3]))) stop("No objective values selected. You must select, at least, one objective value. Aborting...")
 
  #Determine which attibutes are numeric or categorical (this variables have different kind of computation)
   cate <- training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'c'
  num <- training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'r' | training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'e'
  

  
  #----- EXECUTION OF THE ALGORITHM  -------------------
  if(parameters$targetClass != "null"){ # Execution for an only class
    message("\n\nSearching rules for only one value of the target class...\n\n") 
    #Execute the genetic algorithm
    rules <- .findRule(parameters$targetClass, "NMEEFSD", training, parameters, DNF, cate, num, Objectives, as.numeric(parameters[["porcCob"]]), parameters[["StrictDominance"]] == "yes", parameters[["reInitPob"]] == "yes", minCnf)
    
    if(! is.null(unlist(rules)) ){
      if(! DNF) 
        rules <-  matrix(unlist(rules), ncol =  training[["nVars"]] + 1 , byrow = TRUE)
      else 
        rules <-  matrix(unlist(rules), ncol = vars + 1 , byrow = TRUE)
      
      #Print the rule in the console and save into the rules file.
      for(i in seq_len(NROW(rules))){
        message(paste("\nGENERATED RULE", i), appendLF = T)
        if(! is.na(parameters$outputData[2])){
          cat("GENERATED RULE", i,   file = parameters$outputData[2], sep = " ",fill = TRUE, append = TRUE)
        }
        .print.rule(rule = as.numeric( rules[i, - NCOL(rules)] ), max = training$sets, names = training$attributeNames, consecuent = rules[i, NCOL(rules)], types = training$attributeTypes,fuzzySets = training$fuzzySets, categoricalValues = training$categoricalValues, DNF, rulesFile = parameters$outputData[2])
        
        if(! is.na(parameters$outputData[2]))
          cat("\n",  file = parameters$outputData[2], sep = "",fill = TRUE, append = TRUE)
      }
      
    } else {
      warning("No rules found with a confidence greater than the specified")
      if(! is.na(parameters$outputData[2]))
        cat("No rules found with a confidence greater than the specified\n", file = parameters$outputData[2], append = TRUE)
      rules <- numeric()
    }
    
  } else {  #Execution for all classes
    
    message("\n\nSearching rules for all values of the target class...\n\n")  
    
    #Launch the genetic algorithm for each class, paralelize the process for a better performance
    if(Sys.info()[1] == "Windows"){
      rules <- lapply(X = training$class_names, FUN = .findRule, "NMEEFSD", training, parameters, DNF, cate, num, Objectives, as.numeric(parameters[["porcCob"]]), parameters[["StrictDominance"]] == "yes", parameters[["reInitPob"]] == "yes", minCnf  )
    } else { 
      rules <- parallel::mclapply(X = training$class_names, FUN = .findRule, "NMEEFSD", training, parameters, DNF, cate, num, Objectives, as.numeric(parameters[["porcCob"]]), parameters[["StrictDominance"]] == "yes", parameters[["reInitPob"]] == "yes"   , mc.cores = parallel::detectCores() - 1, minCnf)
    }  
    
    if(! is.null(unlist(rules)) ){
      if(! DNF) 
        rules <-  matrix(unlist(rules), ncol =  training[["nVars"]] + 1 , byrow = TRUE)
      else 
        rules <-  matrix(unlist(rules), ncol = vars + 1 , byrow = TRUE)
      
      #Print Rules in console and in the rules file
      for(i in seq_len(NROW(rules))){
        message(paste("\nGENERATED RULE", i), appendLF = T)
        if(! is.na(parameters$outputData[2]))
          cat("GENERATED RULE", i,   file = parameters$outputData[2], sep = " ",fill = TRUE, append = TRUE)
        .print.rule(rule = as.numeric( rules[i, - NCOL(rules)] ), max = training$sets, names = training$attributeNames, consecuent = rules[i, NCOL(rules)], types = training$attributeTypes,fuzzySets = training$fuzzySets, categoricalValues = training$categoricalValues, DNF, rulesFile = parameters$outputData[2])
        if(! is.na(parameters$outputData[2]))
          cat("\n",  file = parameters$outputData[2], sep = "",fill = TRUE, append = TRUE)
      }
    } else {
      warning("No rules found with a confidence greater than the specified")
      if(! is.na(parameters$outputData[2]))
        cat("No rules found with a confidence greater than the specified\n", file = parameters$outputData[2], append = TRUE)
      rules <- numeric()
    }
    
  }
  
  #---------------------------------------------------
  
  message("\n\nTesting rules...\n\n")
  
  #--------  Rule testing --------------------
  if(length(rules) > 0){
    # Store accumulated values to create the 'Global' values as a mean of indivuals rules
  sumNvars <- 0
  sumCov <- 0
  sumFsup <- 0
  sumCsup <- 0
  sumCconf <- 0
  sumFconf <- 0
  sumUnus <- 0
  sumSign <- 0
  sumAccu <- 0
  sumTpr <- 0
  sumFpr <- 0
  
  n_rules <- NROW(rules)
  rulesToReturn <- vector(mode = "list", length = n_rules)
  #Print indivual quality measures for each rule in the console and into the quality measures file
  for(i in seq_len(n_rules)){
    val <- .proveRule(rule = rules[i, - NCOL(rules)], testSet = test, targetClass = rules[i, NCOL(rules)], numRule = i, parameters = parameters, Objectives = Objectives, Weights = c(0.7,0.3, 0), cate = cate, num = num, DNF = DNF)
    test[["covered"]] <- val[["covered"]]
    sumNvars <- sumNvars + val[["nVars"]]
    sumCov <- sumCov + val[["coverage"]]
    sumFsup <- sumFsup + val[["fsupport"]]
    sumCconf <- sumCconf + val[["cconfidence"]]
    sumFconf <- sumFconf + val[["fconfidence"]]
    sumUnus <- sumUnus + val[["unusualness"]]
    sumSign <- sumSign + val[["significance"]]
    sumAccu <- sumAccu + val[["accuracy"]]
    sumTpr <- sumTpr + val[["tpr"]]
    sumFpr <- sumFpr + val[["fpr"]]
    #Remove the name of the vector for significance
    names(val[["significance"]]) <- NULL
    #Add values to the rulesToReturn Object
    rulesToReturn[[i]] <- list(rule = createHumanReadableRule(rules[i,], training, DNF),
                               qualityMeasures = list(nVars = val[["nVars"]],
                                                       Coverage = val[["coverage"]],
                                                       Unusualness = val[["unusualness"]],
                                                       Significance = val[["significance"]],
                                                       FuzzySupport = val[["fsupport"]],
                                                       Support = val[["csupport"]],
                                                       FuzzyConfidence = val[["fconfidence"]],
                                                       Confidence = val[["cconfidence"]],
                                                       TPr = val[["tpr"]],
                                                       FPr = val[["fpr"]]))
  }
  
  
  
  #Print global quality measure on console
  message("Global:",appendLF = T)
  message(paste(paste("\t - N_rules:", NROW(rules), sep = " "),
      paste("\t - N_vars:", round(sumNvars / n_rules, 6), sep = " "),
      paste("\t - Coverage:", round(sumCov / n_rules, 6), sep = " "),
      paste("\t - Significance:", round(sumSign / n_rules, 6), sep = " "),
      paste("\t - Unusualness:", round(sumUnus / n_rules, 6), sep = " "),
      paste("\t - Accuracy:", round(sumAccu / n_rules, 6), sep = " "),
      paste("\t - CSupport:", round(sum(test[["covered"]]) / test[["Ns"]], 6), sep = " "),
      paste("\t - FSupport:", round(sumFsup / n_rules, 6), sep = " "),
      paste("\t - FConfidence:", round(sumFconf / n_rules, 6), sep = " "),
      paste("\t - CConfidence:", round(sumCconf / n_rules, 6), sep = " "),
      paste("\t - True Positive Rate:", round(sumTpr / n_rules, 6), sep = " "),
      paste("\t - False Positive Rate:", round(sumFpr / n_rules, 6), sep = " "),
      sep = "\n")
  )
  

#Print global quality measures on the quality measures file  
  if(! is.na(parameters$outputData[3]))
  cat( "Global:",
       paste("\t - N_rules:", nrow(rules), sep = " "),
       paste("\t - N_vars:", round(sumNvars / n_rules, 6), sep = " "),
       paste("\t - Coverage:", round(sumCov / n_rules, 6), sep = " "),
       paste("\t - Significance:", round(sumSign / n_rules, 6), sep = " "),
       paste("\t - Unusualness:", round(sumUnus / n_rules, 6), sep = " "),
       paste("\t - Accuracy:", round(sumAccu / n_rules, 6), sep = " "),
       paste("\t - CSupport:", round(sum(test[["covered"]] / test[["Ns"]]), 6), sep = " "),
       paste("\t - FSupport:", round(sumFsup / n_rules, 6), sep = " "),
       paste("\t - FConfidence:", round(sumFconf / n_rules, 6), sep = " "),
       paste("\t - CConfidence:", round(sumCconf / n_rules, 6), sep = " "),
       paste("\t - True Positive Rate:", round(sumTpr / n_rules, 6), sep = " "),
       paste("\t - False Positive Rate:", round(sumFpr / n_rules, 6), sep = " "),
       file = parameters$outputData[3], sep = "\n", append = TRUE
  )
  
  class(rulesToReturn) <- "SDEFSR_Rules"
  rulesToReturn # Return
  } else {
    warning("No rules for testing")
    if(! is.na(parameters$outputData[2]))
      cat("No rules for testing", file = parameters$outputData[2], append = TRUE)
    NULL # return
  }
  #---------------------------------------------------
  

}