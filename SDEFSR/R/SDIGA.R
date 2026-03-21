


#
# Mark examples of the dataset covered by the rule returned by genetic algorithm
# ONLY FOR SDIGA
#

.markExamples <- function(rule, dataset, targetClass, nVars, maxRule, to_cover, nLabels, Objectives = c(.LocalSupport, .confidence, NA, FALSE), Weights = c(0.7,0.3,0), DNFRules = FALSE, cate, num){
  
  #Return new covered examples from the objective class
  cover <- .fitnessFunction(rule = rule, dataset = dataset, noClass = matrix(unlist(.separate(dataset)), nrow = length(dataset[[2]]) - 1, ncol = length(dataset[[7]])), targetClass = targetClass, to_cover = to_cover, n_Vars = nVars,nLabels = nLabels, maxRule = maxRule , mark = TRUE, Objectives = Objectives, Weights = Weights, DNFRules = DNFRules, fuzzy = Objectives[[4]], cate = cate, num = num)
  
  sumNews <- sum(cover[[1]]) - sum(dataset[["covered"]])
  confi <- cover[[2]]
  to_cover <- to_cover - sumNews
  
  return( list(cubreNuevos = sumNews > 0, covered = cover , porCubrir = to_cover, confidence = confi) )
  
}












#
# Returns de best rule of the genetic algorithm. In case of draw, returns the rule with less atributes.
# 
# ONLY FOR SDIGA 
#
.getBestRule <- function(result){
  bestFitness <- result@fitnessValue
  
  draws <- which(result@fitness == bestFitness)
  if(length(draws) > 1){
    if(!result@DNFRules){
      lista <- apply(X = result@population[draws, ], MARGIN = 1, FUN = function(x, max) sum(x != max), result@max )
    } else{
      
      lista <- apply(X = result@population[draws, ], MARGIN = 1, FUN = function(x, max){
        particip <- .getParticipants(rule = x, maxRule = max, DNFRules = TRUE)
        val <- numeric(0)
        for(i in 2:length(max)){
          if(particip[i-1])
            val <- c(val, (max[i-1]+1):max[i])
        }
        sum(x[val] != 0)
      } , result@max ) 
      
    }
    orden <- order(lista[which(lista > 0)])
    
    return(result@population[ orden[1] , ])
  } else {
    return(result@population[ draws , ])
  }
}





#
# @title Mutation operator for SDIGA
# @description It can mutate a variable in two ways: erasing the variable or changing the value randomly
# @param chromosome The chromosome to mutate
# @param variable The variable of the chromosome to mute
# @param maxVariablesValue The maximum value of each variable or the beginning of a variable in a DNF representation
# @param DNF_Rule Indicates if we are processing a DNF rule
# 
.mutate <- function(chromosome, variable, maxVariablesValue, DNF_Rule){
                                            
 
  mutation_type <- sample(x = 1:2, size = 1)   # Type 1 or 2 randomly
  
  
  if(! DNF_Rule){  #CAN rules
    if(mutation_type == 1L){
      # If type == 1 the variable must be removed, this is done by putting the max value for that variable.
      chromosome[variable] <- maxVariablesValue[variable] 
      
    } else {  
      #Assing a random value where the no-participation value is taken into account
      value <- sample(x = 0:(maxVariablesValue[variable] ), size = 1)
      chromosome[variable] <- value 
      
    }
    
  } else { #DNF RULES
    
    #For DNF rules, we have to get the range of values that this variable has, and then apply the mutation operator 
    #to all the range
     variable <- variable + 1
     range <- (maxVariablesValue[variable - 1] + 1):maxVariablesValue[variable]
   
    
    if(mutation_type == 1){  
      #To erase the variable, put all the range with zeros (or ones)
      chromosome[range] <- 0
      
    } else {  
      #Assing a random value to all the range of values.
      #Note that the elimination of the rule is a possibility
      chromosome[range] <- sample(x = 0:1 , size = length(range), replace = TRUE)
      
    }
    
  }
  
  
  
  chromosome  # Return
  
}











#' @title Subgroup Discovery Iterative Genetic Algorithm (SDIGA)
#' @description Perfoms a subgroup discovery task executing the algorithm SDIGA
#' 
#' @param parameters_file The path of the parameters file. \code{NULL} If you want to use training and test \code{SDEFSR_Dataset} variables
#' @param training A \code{SDEFSR_Dataset} class variable with training data.
#' @param test A \code{SDEFSR_Dataset} class variable with training data.
#' @param output character vector with the paths of where store information file, rules file and test quality measures file, respectively.
#' @param seed An integer to set the seed used for generate random numbers.
#' @param nLabels Number of linguistic labels that represents numerical variables.
#' @param nEval An integer for set the maximum number of evaluations in the evolutive process.
#' @param popLength An integer to set the number of individuals in the population.
#' @param mutProb Sets the mutation probability. A number in [0,1].
#' @param RulesRep Representation used in the rules. "can" for canonical rules, "dnf" for DNF rules.
#' @param Obj1 Sets the Objective number 1. See \code{Objective values} for more information about the possible values.
#' @param w1 Sets the weight of \code{Obj1}.
#' @param Obj2 Sets the Objective number 2. See \code{Objective values} for more information about the possible values.
#' @param w2 Sets the weight of \code{Obj2}.
#' @param Obj3 Sets the Objective number 3. See \code{Objective values} for more information about the possible values.
#' @param w3 Sets the weight of \code{Obj3}.
#' @param minConf Sets the minimum confidence that must have the rule returned by the genetic algorithm after the local optimitation phase. A number in [0,1].
#' @param lSearch Sets if the local optimitation phase must be performed. A string with "yes" or "no".
#' @param targetVariable A string with the name or an integer with the index position of the target variable (or class). It must be a categorical one.
#' @param targetClass A string specifing the value the target variable. \code{null} for search for all possible values.
#' 
#' 
#'@details This function sets as target variable the last one that appear in \code{SDEFSR_Dataset} object. If you want 
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
#'     This algorithm has a genetic algorithm in his core. This genetic algorithm returns only the best
#'     rule of the population and it is executed so many times until a stop condition is reached. The stop condition is 
#'     that the rule returned must cover at least one new example (not covered by previous rules) and must have a confidence
#'     greater than a minimum.
#'     
#'    After returning the rule, a local improvement could be applied for make the rule more general. This local improve is done
#'    by means of a hill-climbing local search.
#'    
#'    The genetic algorithm cross only the two best individuals. But the mutation operator is applied over all the population,
#'    individuals from cross too.
#'    
#' @section Parameters file structure:
#'   The \code{parameters_file} argument points to a file which has the necesary parameters for SDIGA works.
#'   This file \strong{must} be, at least, those parameters (separated by a carriage return):
#'   \itemize{
#'     \item \code{algorithm}  Specify the algorithm to execute. In this case. "SDIGA"
#'     \item \code{inputData}  Specify two paths of KEEL files for training and test. In case of specify only the name of the file, the path will be the working directory.
#'     \item \code{seed}  Sets the seed for the random number generator
#'     \item \code{nLabels}  Sets the number of fuzzy labels to create when reading the files
#'     \item \code{nEval}  Set the maximun number of \strong{evaluations of rules} for stop the genetic process
#'     \item \code{popLength}  Sets number of individuals of the main population
#'     \item \code{mutProb}  Mutation probability of the genetic algorithm. Value in [0,1]
#'     \item \code{RulesRep}  Representation of each chromosome of the population. "can" for canonical representation. "dnf" for DNF representation.
#'     \item \code{Obj1} Sets the objective number 1. 
#'     \item \code{w1} Sets the weigth assigned to the objective number 1. Value in [0,1]
#'     \item \code{Obj2} Sets the objective number 2. 
#'     \item \code{w2} Sets the weigth assigned to the objective number 2. Value in [0,1]
#'     \item \code{Obj3} Sets the objective number 3. 
#'     \item \code{w3} Sets the weigth assigned to the objective number 3. Value in [0,1]
#'     \item \code{minConf} Sets the minimum confidence of the rule for checking the stopping criteria of the iterative process
#'     \item \code{lSearch} Perform the local search algorithm after the execution of the genetic algorithm? Values: "yes" or "no"
#'     \item \code{targetVariable} The name or index position of the target variable (or class). It must be a categorical one.
#'     \item \code{targetClass}  Value of the target variable to search for subgroups. The target variable \strong{is always the last variable.}. Use \code{null} to search for every value of the target variable
#'   }
#'   
#'   An example of parameter file could be:
#'  \preformatted{
#' algorithm = SDIGA
#' inputData = "irisD-10-1tra.dat" "irisD-10-1tst.dat"
#' outputData = "irisD-10-1-INFO.txt" "irisD-10-1-Rules.txt" "irisD-10-1-TestMeasures.txt"
#' seed = 0
#' nLabels = 3
#' nEval = 500
#' popLength = 100
#' mutProb = 0.01
#' minConf = 0.6
#' RulesRep = can
#' Obj1 = Comp
#' Obj2 = Unus
#' Obj3 = null
#' w1 = 0.7
#' w2 = 0.3
#' w3 = 0.0
#' lSearch = yes
#' }
#' 
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
#' }
#' 
#'     Also, the algorithms save those results in the files specified in the \code{output} parameter of the algorithm or 
#'     in the \code{outputData} parameter in the parameters file.
#' 
#' 
#' 
#' @references 
#' M. J. del Jesus, P. Gonzalez, F. Herrera, and M. Mesonero, "Evolutionary
#' Fuzzy Rule Induction Process for Subgroup Discovery: A case study in
#' marketing," IEEE Transactions on Fuzzy Systems, vol. 15, no. 4, pp.
#' 578-592, 2007.
#' 
#' @examples 
#' SDIGA(parameters_file = NULL, 
#'       training = habermanTra, 
#'       test = habermanTst, 
#'       output = c(NA, NA, NA),
#'       seed = 0, 
#'       nLabels = 3,
#'       nEval = 300, 
#'       popLength = 100, 
#'       mutProb = 0.01, 
#'       RulesRep = "can",
#'       Obj1 = "CSUP", 
#'       w1 = 0.7,
#'       Obj2 = "CCNF",
#'       w2 = 0.3,
#'       Obj3 = "null",
#'       w3 = 0,
#'       minConf = 0.6,
#'       lSearch = "yes",
#'       targetClass = "positive")
#' \dontrun{
#' SDIGA(parameters_file = NULL, 
#'       training = habermanTra, 
#'       test = habermanTst, 
#'       output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
#'       seed = 0, 
#'       nLabels = 3,
#'       nEval = 300, 
#'       popLength = 100, 
#'       mutProb = 0.01, 
#'       RulesRep = "can",
#'       Obj1 = "CSUP", 
#'       w1 = 0.7,
#'       Obj2 = "CCNF",
#'       w2 = 0.3,
#'       Obj3 = "null",
#'       w3 = 0,
#'       minConf = 0.6,
#'       lSearch = "yes",
#'       targetClass = "positive")
#'       }
#' @export
SDIGA <- function(parameters_file = NULL, 
                  training = NULL, 
                  test = NULL, 
                  output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
                  seed = 0, 
                  nLabels = 3,
                  nEval = 10000, 
                  popLength = 100, 
                  mutProb = 0.01, 
                  RulesRep = "can",
                  Obj1 = "CSUP", 
                  w1 = 0.7,
                  Obj2 = "CCNF",
                  w2 = 0.3,
                  Obj3 = "null",
                  w3 = 0,
                  minConf = 0.6,
                  lSearch = "yes",
                  targetVariable = NA,
                  targetClass = "null")
{
  
  
  
  if(is.null(parameters_file)){
    #Generate our "parameters file" if no parameters file is provided
   
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
                       algorithm = "SDIGA",
                       outputData = output,
                       nEval = nEval, 
                       popLength = popLength,
                       nLabels = nLabels,
                       mutProb = mutProb,
                       RulesRep = RulesRep,
                       Obj1 = Obj1, 
                       w1 = w1, 
                       Obj2 = Obj2, 
                       w2 = w2,
                       Obj3 = Obj3,
                       w3 = w3, 
                       lSearch = lSearch,
                       minConf = minConf,
                       targetClass = targetClass,
                       targetVariable = if(is.na(targetVariable)) training$attributeNames[length(training$attributeNames)] else targetVariable)
      
  } else {
  # Parameters file --------------------------
  parameters <- .read.parametersFile2(file = parameters_file)  # Algorithm parameters
    if(parameters$algorithm != "SDIGA")
      stop("Parameters file is not for \"SDIGA\"")
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
  #Set Covered (examples are not covered yet)
  training$covered <- logical(training$Ns)
  test$covered <- logical(test$Ns)
  
  #file.remove(parameters$outputData[which(file.exists(parameters$outputData))])

  #Determine the representation of the rules (CAN or DNF)
  if(tolower(parameters$RulesRep) == "can"){
    DNF = FALSE
  } else {
    DNF = TRUE
  }
  
  #Get quality measures used as objective
  Objectives <- .parseObjectives(parameters = parameters, "SDIGA", DNF)
  if(all(is.na(Objectives[1:3]))) stop("No objetive values selected. You must select, at least, one objective value. Aborting...")
  
  #Set the weight of each quality measure
  Weights <- c(parameters$w1, parameters$w2, parameters$w3)
  if(sum(Weights) == 0) stop("Sum of weigths must be a value greater than zero.")
  
  Best <- TRUE  
  
  #Here is where the rules obtained are stored
  rules <- list()
  
  #Show parameters to the user
  .show_parameters(params = parameters, train = training, test = test)
  contador <- 0
  
  #Determine which variables are categorical or numeric
  cate <- training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'c'
  num <- training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'r' | training[["attributeTypes"]][- length(training[["attributeTypes"]])] == 'e'
  
  
  #---------------------------------------------------
  
  
  #----- EXECUTION OF THE ALGORITHM-------------------
  if(parameters$targetClass != "null"){ # Execution for a single class
    #Check if target class is valid
    targetClass <- parameters$targetClass
    if(! any(training$class_names == targetClass)) stop("No valid target value provided.")
    message("\n\nSearching rules for only one value of the target class...\n\n")  
    
    
    message(paste(" - Target value:", targetClass ), appendLF = T)
    if(!is.na(parameters$outputData[2]))
      cat("\n - Target value:", targetClass , file = parameters$outputData[2], sep = " ", fill = TRUE, append = TRUE)
    #If it is the  first rule obtained, it must be returned.
    first_rule <- TRUE
    Best = TRUE
    to_cover = training$examplesPerClass[[targetClass]]
    
    while(Best){
      Best <- FALSE
      
      rule <- .executeGA(algorithm = "SDIGA", dataset = training, targetClass = targetClass, n_vars = training$nVars, to_cover = to_cover, nLabels = parameters$nLabels, N_evals = parameters$nEval,  tam_pob = parameters$popLength, p_mut = parameters$mutProb, seed = parameters$seed, Objectives = Objectives, Weights = Weights, DNFRules = DNF, cate = cate, num = num)
      maxRule <-  if(!DNF) training$sets else c(0, Reduce(f = '+', x = training[["sets"]], accumulate = TRUE))
      values <- .fitnessFunction(rule = rule, dataset = training, noClass = matrix(unlist(.separate(training)), nrow = length(training[[2]]) - 1, ncol = length(training[[7]])), targetClass = targetClass, to_cover = to_cover, n_Vars = training$nVars, nLabels = parameters$nLabels, maxRule = maxRule, mark = TRUE, Objectives = Objectives, Weights = Weights, DNFRules = DNF, fuzzy = Objectives[[4]], test = TRUE, cate = cate, num = num)[[2]]
      
      if(tolower(parameters$lSearch) == "yes") rule <- .localSearch(att_obj = targetClass, rule = rule, DNF_Rules = DNF, dataset = training , minimumConfidence = parameters$minConf, x = values, maxRule = maxRule, to_cover = to_cover, nLabels = parameters$nLabels, Objectives = Objectives, cate = cate, num = num)
      x <- .markExamples(rule = rule, dataset = training, targetClass = targetClass, nVars = training$nVars, maxRule = maxRule, to_cover = to_cover, nLabels = parameters$nLabels, Objectives = Objectives, Weights = Weights, DNFRules = DNF, cate = cate, num = num)
      
      if(x$cubreNuevos && x$confidence > parameters$minConf || first_rule){
        first_rule <- FALSE
        Best <- TRUE
        contador <- contador + 1
        training$covered <- x$covered[[1]]
        message(paste("\n GENERATED RULE", contador, ":"),appendLF = T)
        if(!is.na(parameters$outputData[2]))
          cat("\n"," GENERATED RULE", contador, ":",file = parameters$outputData[2], sep = " ", fill = TRUE, append = TRUE)
        .print.rule(rule = rule, max = training$sets, names = training$attributeNames, consecuent = targetClass, types = training$attributeTypes,fuzzySets = training$fuzzySets, categoricalValues = training$categoricalValues, DNF, rulesFile = parameters$outputData[2])
    
        rule[length(rule) + 1] <- targetClass
        rules[[contador]] <- rule
        to_cover <- x$porCubrir
        if(to_cover <= 0) Best <- FALSE #If number of examples to cover is zero, it is not neccesary to call the genetic algorithm again.
        
      } else {
        message("\n GENERATED RULE :", appendLF = T)
        message("# Invalid (Low confidence or support)\n\n")
        
        if(!is.na(parameters$outputData[2]))
          cat("\n GENERATED RULE", ":", "\n",
          "# Invalid (Low confidence or support)", "\n", file = parameters$outputData[2], sep= " ", fill = TRUE, append = TRUE)
        
      }
      
    }
    
    
    
  } else {  #Execution for all classes
    message("\n\nSearching rules for all values of the target class...\n\n")  
    #For each value of the target class execute the algorithm...
    for(i in seq_len(length(training[["class_names"]]))) {
    #for(i in training$class_names[ seq_len(length(training$class_names)) ]){
      targetClass <- training[["class_names"]][i]
      message(paste(" - Target value:", targetClass,"\n"))
      if(!is.na(parameters$outputData[2]))
        cat(" \n - Target value:", targetClass , file = parameters$outputData[2], sep = " ", fill = TRUE, append = TRUE)
      first_rule <- TRUE
      Best = TRUE
      #Set the number of examples to cover as the number of examples of the class
      to_cover = training$examplesPerClass[[i]]
      
      #Begin iterative rule learning (IRL) procedure
      while(Best){
        Best <- FALSE
        #Get the best rule an evolutive process
        rule <- .executeGA(algorithm = "SDIGA", dataset = training, targetClass = targetClass, n_vars = training$nVars, to_cover = to_cover, nLabels = parameters$nLabels, N_evals = parameters$nEval,  tam_pob = parameters$popLength, p_mut = parameters$mutProb, seed = parameters$seed, Objectives = Objectives, Weights = Weights, DNFRules = DNF, cate = cate, num = num)
        maxRule <-  if(!DNF) training$sets else c(0, Reduce(f = '+', x = training[["sets"]], accumulate = TRUE))
        
        #Evaluate the rule obtained
        values <- .fitnessFunction(rule = rule, dataset = training, noClass = matrix(unlist(.separate(training)), nrow = length(training[[2]]) - 1, ncol = length(training[[7]])), targetClass = targetClass, to_cover = to_cover, n_Vars = training$nVars, nLabels = parameters$nLabels, maxRule = maxRule, mark = TRUE, Objectives = Objectives, Weights = Weights, DNFRules = DNF, fuzzy = Objectives[[4]], test = TRUE, cate = cate, num = num)[[2]]
        
        #Perform the local search procedure if neccessary
        if(tolower(parameters$lSearch) == "yes"){
          rule <- .localSearch(att_obj = targetClass, rule = rule, DNF_Rules = DNF, dataset = training , minimumConfidence = parameters$minConf, x = values, maxRule = maxRule, to_cover = to_cover, nLabels = parameters$nLabels, Objectives = Objectives, cate = cate, num = num)
        }
       
          #Mark examples covered by this rule
          x <- .markExamples(rule = rule, dataset = training, targetClass = targetClass, nVars = training$nVars, maxRule = maxRule, to_cover = to_cover, nLabels = parameters$nLabels, Objectives = Objectives, Weights = Weights, DNFRules = DNF, cate = cate, num = num)
        
          #Check stopping criteria
        if(x$cubreNuevos && x$confidence > parameters$minConf || first_rule){
          first_rule <- FALSE # The next rule is not the first anymore
          Best <- TRUE #Continue the iteration
          contador <- contador + 1
          #Substitute actual covered examples by the covered examples of the new set of rules
          training$covered <- x$covered[[1]]
          
          #Print the rule to the user.
          message(paste("\n GENERATED RULE", contador, ":\n"))
          if(!is.na(parameters$outputData[2]))
            cat("\n"," GENERATED RULE", contador, ":",file = parameters$outputData[2], sep = " ", fill = TRUE, append = TRUE)
          .print.rule(rule = rule, max = training$sets, names = training$attributeNames, consecuent = targetClass, types = training$attributeTypes,fuzzySets = training$fuzzySets, categoricalValues = training$categoricalValues, DNF, rulesFile = parameters$outputData[2])
          
          #Add the target class to the end of the rule and add it to the list of rules.
          rule[length(rule) + 1] <- targetClass
          rules[[contador]] <- rule
          to_cover <- x$porCubrir
          if(to_cover <= 0) Best <- FALSE #If there arent examples to cover, we finish the execution.
          
        } else {
          #If the rule is not valid, report the user this situation
          message(" GENERATED RULE:\n")
          message("# Invalid (Low confidence or support)\n\n")
          if(!is.na(parameters$outputData[2]))
            cat("\n GENERATED RULE", ":", "\n",
              "# Invalid (Low confidence or support)", "\n","\n", file = parameters$outputData[2], sep= " ", fill = TRUE, append = TRUE)
          
        }
        
      }
    }
    
    
  }
  
  #---------------------------------------------------
  
  message("\n\nTesting rules...\n\n")
  
  #--------  Test Rules  --------------------
  # accumulated values to print the 'Global' result as the mean value of each rule
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
  
  n_rules <- length(rules)
  rulesToReturn <- vector(mode = "list", length = n_rules)
  
  for(i in 1:n_rules){
    val <- .proveRule(rule = rules[[i]][-length(rules[[i]])], testSet = test, targetClass = rules[[i]][length(rules[[i]])], numRule = i, parameters = parameters, Objectives = Objectives, Weights = Weights, cate = cate, num = num, DNF = DNF)
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
    rulesToReturn[[i]] <- list( rule = createHumanReadableRule(rules[[i]], training, DNF),
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
  
  
  
  #Print Global Quality Measures as the mean value of the set of rules
  message("Global:\n")
  message(paste(paste("\t - N_rules:", length(rules), sep = " "),
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
  
 #Save the global result to a file
  if(!is.na(parameters$outputData[3])){
  cat( "Global:",
     paste("\t - N_rules:", length(rules), sep = " "),
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
  rulesToReturn  # Return
  
}



#
#
# Evaluates a given rule on a test set.
#
#
.proveRule <- function(rule, testSet, targetClass, numRule, parameters, Objectives, Weights, cate, num, DNF = FALSE){
  stopifnot(class(testSet) == "SDEFSR_Dataset")
   
  #Get the maximium value or the non-participation value for each variable
  maxRule <- .giveMeSets(data_types = testSet[[3]], max = testSet[[5]], n_labels = parameters$nLabels)
   
  if(DNF) maxRule <- c(0,Reduce(f= '+', x = maxRule, accumulate = TRUE))
  
  #Evaluate the rule on the test set.
  p <- .fitnessFunction(rule = rule, dataset = testSet, noClass = matrix(unlist(.separate(testSet)), nrow = length(cate)), targetClass = targetClass, to_cover = testSet$examplesPerClass[[targetClass]], n_Vars = testSet$nVars, nLabels = parameters$nLabels, maxRule = maxRule, mark = TRUE, Objectives = Objectives, Weights = Weights, DNFRules = DNF, fuzzy = Objectives[[4]], test = TRUE, cate = cate, num = num)
  #Get the values
   values <- p[[2]]
   #Set the new covered examples 
  testSet[["covered"]] <- testSet[["covered"]] | p[[1]] #For the global quality measure
  
  #get quality measures
  Cov <- round(.coverage(values), 6)
  sig <- round(.significance(values), 6)
  unus <- round(.unusualness(values), 6)
  acc <- round(.accuracy(values), 6)
  Csup <- round(.Csupport(values), 6)
  Fsup <- round(.Fsupport(values), 6)
  Ccnf <- round(.confidence(values), 6)
  Fcnf <- round(.fuzzyConfidence(values), 6)
  tpr <- round(p[[2]]$tpr, 6)
  fpr <- round(p[[2]]$fpr, 6)
  
  
  if(DNF) {
    participants <- .getParticipants(rule = rule,  maxRule = maxRule, DNFRules = TRUE) 
    nVars <- sum(participants) + 1
  } else{
    nVars <- sum(rule < testSet[["sets"]]) + 1 # +1 for the consecuent variable.
  }
  
  #Print the quality measures on console
  message(paste("Rule", numRule,":\n"))
  message(paste(paste("\t - N_vars:", nVars, sep = " "),
      paste("\t - Coverage:", Cov, sep = " "),
      paste("\t - Significance:", sig, sep = " "),
      paste("\t - Unusualness:", unus, sep = " "),
      paste("\t - Accuracy:", acc, sep = " "),
      paste("\t - CSupport:", Csup, sep = " "),
      paste("\t - FSupport:", Fsup, sep = " "),
      paste("\t - CConfidence:", Ccnf, sep = " "),
      paste("\t - FConfidence:", Fcnf, sep = " "),
      paste("\t - True Positive Rate:", tpr, sep = " "),
      paste("\t - False Positive Rate:", fpr, "\n", sep = " "),
    sep = "\n")
  )
  
  
  
  #Save the measures in a file
  if(!is.na(parameters$outputData[3])){
  cat(paste("Rule", numRule,":"),
      paste("\t - N_vars:", nVars, sep = " "),
      paste("\t - Coverage:", Cov, sep = " "),
      paste("\t - Significance:", sig, sep = " "),
      paste("\t - Unusualness:", unus, sep = " "),
      paste("\t - Accuracy:", acc, sep = " "),
      paste("\t - CSupport:", Csup, sep = " "),
      paste("\t - FSupport:", Fsup, sep = " "),
      paste("\t - CConfidence:", Ccnf, sep = " "),
      paste("\t - FConfidence:", Fcnf, sep = " "),
      paste("\t - True Positive Rate:", tpr, sep = " "),
      paste("\t - False Positive Rate:", fpr, "\n", sep = " "),
      file = parameters$outputData[3], sep = "\n", append = TRUE
  )
  }
  
#Return
    list( covered = testSet[["covered"]], 
                nVars = nVars,
                coverage = Cov, 
                significance = sig, 
                unusualness = unus,
                accuracy = acc,
                csupport = Csup,
                fsupport = Fsup,
                cconfidence = Ccnf,
                fconfidence = Fcnf,
                tpr = tpr,
                fpr = fpr) 
}



#--------------------------------------------------------------------------------------------------
#                Erase a variable of a rule
#
# - rule: The rule to modify
# - variable: The variable to erase
# - maxVariablesValue: The maximum value of the variables
# - DNF_Rules: logical indicating the use of DNF rules
#
#--------------------------------------------------------------------------------------------------

.eraseGene <- function(rule, variable, maxVariablesValue, DNF_Rules){
  
  if(!DNF_Rules){ # CAN RULES
    #For CAN rule put the maximum value to erase the variable
    rule[variable] <- maxVariablesValue[variable]  
    
    
  } else {   #DNF Rules
    
    #Get the range of values that belongs to the variable and fill all with zeroes
   range <- (maxVariablesValue[variable] + 1):maxVariablesValue[variable + 1]
   rule[range] <- 0
   
  }
  
  rule   #Return
  
}




#--------------------------------------------------------------------------------------------------
#         Local search as a post-processing stage of SDIGA Algorithm
#-------------------------------------------------------------------------------------------------

.localSearch <- function(att_obj, rule, DNF_Rules, dataset, minimumConfidence, x, maxRule, to_cover, nLabels, Objectives, cate, num){
  #Store the rule as the best rule at the moment
  bestRule <- rule
  #Store the significance of the rule
   ruleSignificance <- .significance(x)
  participants <- .getParticipants(rule = rule, maxRule = maxRule, DNFRules = DNF_Rules)
  
  if(ruleSignificance == 1 || sum(participants) == 1 ){
    return(rule) # If local support it is 1 or rule has only one attribute, we can not improve the rule
  }
  
  #Store the best significance
    bestSignificance <-  ruleSignificance
    #Control variable for the loop
    best = TRUE
    len = if(DNF_Rules) length(maxRule) - 1 else length(maxRule)
    while(best){
      best = FALSE
      #Get the variables that participate in the rule
      participants <- .getParticipants(rule = bestRule, maxRule = maxRule, DNFRules = DNF_Rules)
      for(i in seq_len(len) ){ # for each gene
       
        if(participants[i]){
          #if the variable participates in the rule, then create a new one without the variable 'i'
          regla_m <- .eraseGene(rule = bestRule, variable = i, maxVariablesValue = maxRule, DNF_Rules = DNF_Rules)
          
          #evaluete this new rule
          x1 <- .fitnessFunction(rule = regla_m, dataset = dataset, noClass = matrix(unlist(.separate(dataset)), nrow = length(dataset[[2]]) - 1, ncol = length(dataset[[7]])), targetClass = att_obj, to_cover = to_cover, n_Vars = dataset$nVars, nLabels = nLabels, maxRule = maxRule, mark = TRUE, Objectives = Objectives, DNFRules = DNF_Rules, fuzzy = Objectives[[4]], test = TRUE, cate = cate, num = num)
          if(length(x1) > 1){
            x1 <- x1[[2]]
            supp1 <- .significance(x1)
          } else {
            supp1 <- 0 # It is the empty rule
          }
          
          #If the new rule has a better significance than the best rule:
          if( supp1 >= bestSignificance ){
            #Calculate its confidence
            c1 <- .confidence(x1)
            c2 <-  .confidence(x)
            # And if the rule has better confidence, update bestRule
            if( (supp1 > bestSignificance) &&  c1 >= c2 ){
              bestSignificance <- supp1
              bestRule <- regla_m
              best = TRUE
             }
          }
        }
        
      }
      
    }
    
    #Evalute again the best rule
  x1 <- .fitnessFunction(rule = bestRule, dataset = dataset, noClass = matrix(unlist(.separate(dataset)), nrow = length(dataset[[2]]) - 1, ncol = length(dataset[[7]])), targetClass = att_obj, to_cover = to_cover, n_Vars = dataset$nVars, nLabels = nLabels, maxRule = maxRule, mark = TRUE, Objectives = Objectives, DNFRules = DNF_Rules, fuzzy = Objectives[[4]], test = TRUE, cate = cate, num = num)[[2]]
  
  #If the best rule has a confidence greater than the minimum, 
  #return the best rule, else return the original one
  if(.confidence(x1) >= minimumConfidence){
    bestRule
  } else {
    rule
  }
  
}

