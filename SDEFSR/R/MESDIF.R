
#
#
# Calculate the volume of a sphere in n dimensions for SPEA2 fitness calculation.
#
#
.volSphere <- function(dimensions){
  vol = 1
  
  if(dimensions %% 2 == 0){
    d2 <- dimensions / 2
    for ( i in seq_len(d2))
      vol <- vol * i
    vol <- (pi^d2) / vol
    
  } else {
    v <- (dimensions-1)/2+1
    for ( i in v:dimensions)
      vol <- vol * i;
    vol = 2^dimensions * pi^(v - 1) * vol
  }
  vol
}





#' @title Mutation operator for MESDIF
#' @description  The biased mutation operator for MESDIF
#' @param chromosome The chromosome to mute.
#' @param variable The variable chosen to mutate.
#' @param maxVariableValues The maximum possible value for each variable on CAN representation or the beginning of each
#'     variable for DNF representation
#' @param DNF_Rule A logical indicating if we are processing a DNF Rule.
#' @noRd
.mutateMESDIF <- function(chromosome, variable, maxVariableValues, DNF_Rule){
  
  
  mutation_type <- sample(x = 1:2, size = 1)   #Type 1 -> Eliminate the variable, Type 2 -> change the value for a random one
  
  
  if(! DNF_Rule){  #CAN RULES
    if(mutation_type == 1L){
      
      #If type == 1, we eliminate the variable (eliminate is put the max value)
      chromosome[variable] <- maxVariableValues[variable] 
      
    } else {  #Assign a random value (elimination value NOT INCLUDED)
      
      value <- sample(x = 0:(maxVariableValues[variable] - 1), size = 1)
      chromosome[variable] <- value 
      
    }
    
  } else { #DNF RULE
    
    #For a DNF rule, first, get the range of values for the variable, because we change all his values
    variable <- variable + 1
    range <- (maxVariableValues[variable - 1] + 1):maxVariableValues[variable]
    
    
    if(mutation_type == 1){  #Erase the variable (put all values of the range 0 or 1)
      
      chromosome[range] <- 0
      
    } else {  
      #Asign a random value (0 or 1) to each value of the range
      #Note that this could erase the variable.
      chromosome[range] <- sample(x = 0:1 , size = length(range), replace = TRUE)
      
    }
    
  }
  
  
  
  chromosome  # Return
  
}





#
#
#' @title  Truncation operator for the elite population in MESDIF
#' @description  This is called when the number of non-dominated individuals are greater than elite population size.
#' @param NonDominatedPop A matrix with non.dominated individuals
#' @param elitePopSize The size of the elite population
#' @param FitnessND A matrix with the fitness values of the non-dominated indiciduals.
#' @noRd
.truncOperator <- function(NonDominatedPop, elitePopSize, FitnessND ){
  #Calculate distance between individuals
  distance <- as.matrix( dist(x = FitnessND, method = "euclidean") ) ^2
 
  #Distance between themselves eliminated.
  diag(distance) <- Inf
  
  #Order the distance matrix
  sortedIndex <- apply(X = distance, MARGIN = 1,FUN = order)
  
  individuals <- NROW(NonDominatedPop)
  
  # True values indicates the elimination of the indivual
  dontKeep <- logical(individuals)
  
  #Iterative removal of individuals until the size of the elite population is reached
  while(individuals > elitePopSize){
    
    #Find the minimal distance among individuals
    minimum <- which(distance == min(distance), arr.ind = TRUE,useNames = FALSE)
  
    if(NROW(minimum) == 1){
      #Remove the individual directly
      dontKeep[minimum[,2]] <- T
      distance[minimum[,2],  ] <- Inf
      distance[,minimum[,2]  ] <- Inf
      
    } else {
      
    row <- minimum[1,1]
    column <- minimum[1,2]
    
    
    #We found the two closest individuals, now we have to erase one of them. This will be who have the minimum distance between his k-th closest neighbour
    pos <- 1
    while(distance[sortedIndex[pos,row], row]== distance[sortedIndex[pos,column], column] & pos < NROW(distance)){
      pos <- pos + 1
    }
 
    #Erase the closest individual
    if(distance[sortedIndex[pos,row],row] < distance[sortedIndex[pos,column],column]){
   
      dontKeep[row] <- T
      distance[row,  ] <- Inf
      distance[,row  ] <- Inf
      
      #The position in sortedIndex is now the last.
      sortedIndex <- apply(sortedIndex, MARGIN = 2, function(x, value, individuals){
        x[which(x == value):(individuals - 1)] <- x[which(x == value):(individuals - 1) + 1];
        x[individuals] <- value;
        x
      }, row, individuals)
      
    } else {
      
      dontKeep[column] <- T
      distance[, column] <- Inf
      distance[column, ] <- Inf
     
      sortedIndex <- apply(sortedIndex, MARGIN = 2, function(x, value, individuals){
            x[which(x == value):(individuals - 1)] <- x[which(x == value):(individuals - 1) + 1];
            x[individuals] <- value;
            x
            }, column, individuals)
      
    }
    }
    
    individuals <- individuals - 1
  }
 
 list(poblation = NonDominatedPop[which(! dontKeep), , drop = F], individuals = which(! dontKeep) )
}



#' 
#' @title Multiobjective Evolutionary Subgroup DIscovery Fuzzy rules (MESDIF) Algorithm
#' @description Performs a subgroup discovery task executing the MESDIF algorithm.
#' 
#' @param paramFile The path of the parameters file. \code{NULL} If you want to use training and test \code{SDEFSR_Dataset} variables
#' @param training A \code{SDEFSR_Dataset} class variable with training data.
#' @param test A \code{SDEFSR_Dataset} class variable with test data. \code{NULL} if you only want to use training data.
#' @param output character vector with the paths where store information file, rules file and quality measures file, respectively.
#' @param seed An integer to set the seed used for generate random numbers.
#' @param nLabels Number of linguistic labels that represents numerical variables.
#' @param nEval An integer for set the maximum number of evaluations in the evolutive process. Large values of this parameter increments the computing time.
#' @param popLength An integer to set the number of individuals in the population.
#' @param eliteLength An integer to set the number of individuals in the elite population.
#' @param crossProb Sets the crossover probability. A number in [0,1].
#' @param mutProb Sets the mutation probability. A number in [0,1].
#' @param RulesRep Representation used in the rules. "can" for canonical rules, "dnf" for DNF rules.
#' @param Obj1 Sets the Objective number 1. See \code{Objective values} for more information about the possible values.
#' @param Obj2 Sets the Objective number 2. See \code{Objective values} for more information about the possible values.
#' @param Obj3 Sets the Objective number 3. See \code{Objective values} for more information about the possible values.
#' @param Obj4 Sets the Objective number 4. See \code{Objective values} for more information about the possible values.
#' @param targetVariable The name or index position of the target variable (or class). It must be a categorical one.
#' @param targetClass A string specifing the value of the target variable. \code{null} for search for all possible values.
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
#' @section How does this algorithm work?:
#'   This algorithm performs a multi-objective genetic algorithm based on elitism (following the SPEA2 approach). The elite population has 
#'   a fixed size and it is filled by non-dominated individuals.
#'   
#'   An individual is non-dominated when \code{(! all(ObjI1 <= ObjI2) & any(ObjI1 < ObjI2))} where ObjI1
#'   is the objective value for our individual and ObjI2 is the objetive value for another individual.
#'   The number of dominated individuals by each one determine, in addition with a niches technique that considers
#'   the proximity among values of the objectives a fitness value for the selection.
#'   
#'   The number of non-dominated individuals might be greater or less than elite population size and in those cases
#'   MESDIF implements a truncation operator and a fill operator respectively. Then, genetic operators are
#'   applied.
#'   
#'   At the final of the evolutive process it returns the rules stored in elite population. Therefore, the number of rules is fixed with the \code{eliteLength} parameter.
#'   
#' @section Parameters file structure:
#'   The \code{paramFile} argument points to a file which has the necesary parameters for MESDIF works.
#'   This file \strong{must} have, at least, those parameters (separated by a carriage return):
#'   \itemize{
#'     \item \code{algorithm}  Specify the algorithm to execute. In this case. "MESDIF"
#'     \item \code{inputData}  Specify two paths of KEEL files for training and test. In case of specify only the name of the file, the path will be the working directory.
#'     \item \code{seed}  Sets the seed for the random number generator
#'     \item \code{nLabels}  Sets the number of fuzzy labels to create when reading the files
#'     \item \code{nEval}  Set the maximun number of \strong{evaluations of rules} for stop the genetic process
#'     \item \code{popLength}  Sets number of individuals of the main population
#'     \item \code{eliteLength}  Sets number of individuals of the elite population. Must be less than \code{popLength}  
#'     \item \code{crossProb}  Crossover probability of the genetic algorithm. Value in [0,1]
#'     \item \code{mutProb}  Mutation probability of the genetic algorithm. Value in [0,1]
#'     \item \code{Obj1} Sets the objective number 1. 
#'     \item \code{Obj2} Sets the objective number 2. 
#'     \item \code{Obj3} Sets the objective number 3. 
#'     \item \code{Obj4} Sets the objective number 4.
#'     \item \code{RulesRep}  Representation of each chromosome of the population. "can" for canonical representation. "dnf" for DNF representation.
#'     \item \code{targetVariable} The name or index position of the target variable (or class). It must be a categorical one.
#'     \item \code{targetClass}  Value of the target variable to search for subgroups. The target variable \strong{is always the last variable.} Use \code{null} to search for every value of the target variable
#'   }
#'   
#'   An example of parameter file could be:
#'  \preformatted{
#'  algorithm = MESDIF
#'  inputData = "irisd-10-1tra.dat" "irisd-10-1tst.dat"
#'  outputData = "irisD-10-1-INFO.txt" "irisD-10-1-Rules.txt" "irisD-10-1-TestMeasures.txt"
#'  seed = 0
#'  nLabels = 3
#'  nEval = 500
#'  popLength = 100
#'  eliteLength = 3
#'  crossProb = 0.6
#'  mutProb = 0.01
#'  RulesRep = can
#'  Obj1 = comp
#'  Obj2 = unus
#'  Obj3 = null
#'  Obj4 = null
#'  targetClass = Iris-setosa }
#'
#'   @section Objective values:
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
#'     If you dont want to use a objective value you must specify \code{null}
#' 
#' 
#' @return The algorithm shows in the console the following results:
#' \enumerate{
#'  \item The parameters used in the algorithm
#'  \item The rules generated.
#'  \item The quality measures for test of every rule and the global results. This globals results shows the number of 
#'       rules generated and means results for each quality measure.
#' }
#' 
#' Also, the algorithms save those results in the files specified in the \code{output} parameter of the algorithm or 
#' in the \code{outputData} parameter in the parameters file.
#'     
#' Additionally a \code{SDEFSR_Rules} object is returned with this information.
#' 
#' 
#' @references 
#' \itemize{
#'  \item Berlanga, F., Del Jesus, M., Gonzalez, P., Herrera, F., & Mesonero, M. (2006). Multiobjective Evolutionary Induction of Subgroup Discovery Fuzzy Rules: A Case Study in Marketing.
#'  \item Zitzler, E., Laumanns, M., & Thiele, L. (2001). SPEA2: Improving the Strength Pareto Evolutionary Algorithm. 
#' }
#' 
#' @examples 
#'  MESDIF( paramFile = NULL,
#'         training = habermanTra, 
#'         test = habermanTst, 
#'         output = c(NA, NA, NA),
#'         seed = 0, 
#'         nLabels = 3,
#'         nEval = 300, 
#'         popLength = 100, 
#'         eliteLength = 3,
#'         crossProb = 0.6,
#'         mutProb = 0.01, 
#'         RulesRep = "can",
#'         Obj1 = "CSUP", 
#'         Obj2 = "CCNF",
#'         Obj3 = "null",
#'         Obj4 = "null",
#'         targetClass = "positive"
#'         )
#' 
#' \dontrun{
#' Execution for all classes, see 'targetClass' parameter
#' MESDIF( paramFile = NULL,
#'         training = habermanTra, 
#'         test = habermanTst, 
#'         output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
#'         seed = 0, 
#'         nLabels = 3,
#'         nEval = 300, 
#'         popLength = 100, 
#'         eliteLength = 3,
#'         crossProb = 0.6,
#'         mutProb = 0.01, 
#'         RulesRep = "can",
#'         Obj1 = "CSUP", 
#'         Obj2 = "CCNF",
#'         Obj3 = "null",
#'         Obj4 = "null",
#'         targetClass = "null"
#'         )
#'  }
#' 
#' @export
MESDIF <- function(paramFile = NULL,
                   training = NULL, 
                   test = NULL, 
                   output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
                   seed = 0, 
                   nLabels = 3,
                   nEval = 10000, 
                   popLength = 100, 
                   eliteLength = 3,
                   crossProb = 0.6,
                   mutProb = 0.01, 
                   RulesRep = "can",
                   Obj1 = "CSUP", 
                   Obj2 = "CCNF",
                   Obj3 = "null",
                   Obj4 = "null",
                   targetVariable = NA,
                   targetClass = "null"
                   )
{
 
  if(is.null(paramFile)){
    #Generate our "parameters file"
   
    
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
                       algorithm = "MESDIF",
                       outputData = output,
                       nEval = nEval, 
                       popLength = popLength,
                       elitePop = eliteLength,
                       nLabels = nLabels,
                       mutProb = mutProb,
                       crossProb = crossProb,
                       RulesRep = RulesRep,
                       Obj1 = Obj1, 
                       Obj2 = Obj2,
                       Obj3 = Obj3,
                       Obj4 = Obj4,
                       targetClass = targetClass,
                       targetVariable = if(is.na(targetVariable)) training$attributeNames[length(training$attributeNames)] else targetVariable)
  } else {
  # Parameters --------------------------
    parameters <- .read.parametersFile2(file = paramFile)  # parameters of the algorithm
    if(parameters$algorithm != "MESDIF") 
      stop(paste("The algorithm specificied (", parameters$algorithm, ") in parameters file is not \"MESDIF\". Check parameters file. Aborting program..."))
    
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
  
    #Remove files
  #file.remove(parameters$outputData[which(file.exists(parameters$outputData))])
 
  # Set the representation of rules (CAN or DNF)
  if(tolower(parameters$RulesRep) == "can"){
    DNF = FALSE
  } else {
    DNF = TRUE
    vars <-  Reduce(f = '+', x = training[["sets"]], accumulate = TRUE)
    vars <- vars[length(vars)]
  }
  
  # Show information of the parameters to the user
  .show_parameters(params = parameters, train = training, test = test)
  counter <- 0
  
  #Get the quality measures uses as objectives
  Objectives <- .parseObjectives(parameters = parameters, "MESDIF", DNF)
  
  if(all(is.na(Objectives[1:3]))) stop("No objective values selected. You must select, at least, one objective value. Aborting...")
  
  #Get which variable are categorical and numerical to improve the eficiency
  cate <- training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'c'
  num <- training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'r' | training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'e'
 
  
  #---------------------------------------------------
  
  
  #----- EXECUTION OF THE ALGORITHM -------------------
  if(parameters$targetClass != "null"){ # Execution for one value of the target class
    message("\n\nSearching rules for only one value of the target class...\n\n") 
    #Execution of the algorithm
    rules <- .findRule(parameters$targetClass, "MESDIF", training, parameters, DNF, cate, num, Objectives)
    if(! DNF) 
      rules <-  matrix(unlist(rules), ncol =  training[["nVars"]] + 1 , byrow = TRUE)
    else 
      rules <-  matrix(unlist(rules), ncol = vars + 1 , byrow = TRUE)
    
  } else {  #Execution for all classes
    
    message("\n\nSearching rules for all values of the target class...\n\n" )
    
    #If we are on Windowns, we cant use mclapply because it use FORK() for parallelism
    if(Sys.info()[1] == "Windows")
      rules <- lapply(X = training$class_names, FUN = .findRule, "MESDIF",training, parameters, DNF, cate, num, Objectives)
    else
      rules <- parallel::mclapply(X = training$class_names, FUN = .findRule, "MESDIF",training, parameters, DNF, cate, num, Objectives   , mc.cores = parallel::detectCores() - 1)
  
    
    if(! DNF) 
      rules <-  matrix(unlist(rules), ncol =  training[["nVars"]] + 1 , byrow = TRUE)
    else 
      rules <-  matrix(unlist(rules), ncol = vars + 1 , byrow = TRUE)
    
    
  }
  #Print rules to the user on console and save into a file
  for(i in seq_len(NROW(rules))){
    message(paste("\nGENERATED RULE", i,   sep = " "), appendLF = T)
    if(!is.na(parameters$outputData[2]))
      cat("GENERATED RULE", i,   file = parameters$outputData[2], sep = " ",fill = TRUE, append = TRUE)
    #Print the rule into a human-readable format
    .print.rule(rule = as.numeric( rules[i, - NCOL(rules)] ), max = training$sets, names = training$attributeNames, consecuent = rules[i, NCOL(rules)], types = training$attributeTypes,fuzzySets = training$fuzzySets, categoricalValues = training$categoricalValues, DNF, rulesFile = parameters$outputData[2])
    if(!is.na(parameters$outputData[2]))
     cat("\n",  file = parameters$outputData[2], sep = "",fill = TRUE, append = TRUE)
  }
    
    
  
  
  #---------------------------------------------------
  
  message("\n\nTesting rules...\n\n")
  
  #--------  Rule Testing --------------------
  #
  # Store accumulated values to show the 'Global' result as a mean of results
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
  for(i in seq_len(n_rules)){
    #Take the test values of each rule
    val <- .proveRule(rule = rules[i, - NCOL(rules)], testSet = test, targetClass = rules[i, NCOL(rules)], numRule = i, parameters = parameters, Objectives = Objectives, Weights = c(0.7,0.3,0), cate = cate, num = num, DNF = DNF)
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
    rulesToReturn[[i]] <- list( rule = createHumanReadableRule(rules[i,], training, DNF),
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
  
  
  
  #Global quality measures as a mean of individual values
  message("Global:", file ="", fill = TRUE)
  message(paste(paste("\t - N_rules:", NROW(rules), sep = " "),
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
      sep = "\n")
  )
  
  #Save in testMeasures File the global results
  if(!is.na(parameters$outputData[3])){
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
  }
  
  #---------------------------------------------------
  class(rulesToReturn) <- "SDEFSR_Rules"
  rulesToReturn # Return
}




#
#
# This function calls the executeGA method which launch a genetic algorithm of an SDEFSR algorithm to find rules
# 
#
.findRule <- function(targetClass, algorithm, training, parameters, DNF, cate, num, Objectives, porcCob = 0.5, strictDominance = TRUE, reInit = TRUE, minCnf = 0.6){
  #Check if target class is valid
  if(! any(training$class_names == targetClass)) stop("Invalid target class value provided.")
  #cat(" ? Target value:", targetClass ,"\n", file = "", sep = " ", fill = TRUE)
  
  # Sets the examples to cover (for SDIGA)
  to_cover = training$examplesPerClass[[targetClass]]
  #Launch the genetic algorithm
  rule <- .executeGA(algorithm = algorithm, dataset = training, targetClass = targetClass, n_vars = training$nVars, to_cover = to_cover, nLabels = parameters$nLabels, N_evals = parameters$nEval,  tam_pob = parameters$popLength, p_cross = parameters$crossProb, p_mut = parameters$mutProb, seed = parameters$seed, Objectives = Objectives, Weights = c(0.7,0.3,0), DNFRules = DNF, cate = cate, num = num, elitism = parameters[["elitePop"]], porcCob = porcCob, strictDominance = strictDominance, reInit = reInit, minCnf = minCnf)     
  
  #For each finded rule
  rules <- vector(mode = "list", length = NROW(rule))
  if(length(rule > 0)){
    rule <- cbind(rule, targetClass) #Add at the end the target class
    for(i in seq_len(length(rules))){
      #Add it to the list
      rules[[i]] <- rule[i,]
    }
  }
  rules  #Return
}