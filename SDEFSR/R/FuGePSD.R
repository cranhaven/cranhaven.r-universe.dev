              #############################################################################
              #                                                                           #
              #                     R implementation for FuGePSD                          #
              #                                                                           #
              #       This file contains functions related with FuGePSD algorithm         #
              #                                                                           #
              # Reference: A fuzzy genetic programming-based algorithm for subgroup       #
              # discovery and the application to one problem of pathogenesis of acute     #
              # sore throat conditions in humans, Carmona, C.J., Ruiz-Rodado V., del      #
              # Jesus M.J., Weber A., Grootveld M., Gonzalez P., and Elizondo D. ,        #
              # Information Sciences, Volume 298, p.180-197, (2015)                       #
              #                                                                           #
              #       Written on R by: Angel M. Garcia <amgv0009@red.ujaen.es>            #
              #############################################################################



#'
#' Creates a random general rule
#' 
#' @details It creates a random general rule for a provided dataset with, at most, 50 %% of the variables.
#' 
#' @param dataset The SDEFSR_Dataset dataset where getting the data
#' @param tnorm The T-norm to use: 0 -> minimum T-norm, 1 -> product T-norm
#' @param tconorm The T-conorm to use: 0 -> maximum T-conorm, 1 -> Probabilistic sum t-conorm
#' @param rule_weight The Rule Weighting method: 0 -> Wining Rule, 1 -> Normalized sum, 2 -> Arithmetic mean
#' @param class Integer specifying the creation of a rule for the given class number. By default \code{'NULL'}, makes a rule with a random class in the consecuent.
#' 
#' @return  A new \code{'Rule'} object
#' @noRd
createNewRule <- function(dataset, tnorm, tconorm, rule_weight, class = NULL ){
  
   rule <- structure( list(antecedent = list(),                 # Antecedent part of the rule
                              clas = integer(1),                 # Consecuent
                              weight = numeric(1),               # Weight associated with the rule
                              raw_fitness = numeric(1),          # Raw Fitness associated to the rule
                              penalized_fitness = numeric(1),    # Penalize Fitness of the rule after applying token competition
                              t_norm = integer(1),               # T-norm used to compute compatibility degree
                              t_conorm = integer(1),             # T-conorm used to compute compatibility degree
                              ruleWeight = integer(1),           # Way of compute the weight of the rule
                              level = integer(1),                # 1 for general rules, 2 for specific rules
                              ideal = integer(1),                # Number of examples that this rule can seize
                              evaluated = logical(1),            # indicate if the rule is evaluated or not.
                              tokens = logical(dataset$Ns),      # used in token competition procedure.
                              
                              #Quality Measures
                              qm_Cnf_n = numeric(1),             # Crisp Confidence
                              qm_Cnf_f = numeric(1),             # Fuzzy Confidence
                              qm_Sig = numeric(1),               # Significance
                              qm_Sens = numeric(1),              # Sensitivity
                              qm_Unus = numeric(1),              # Unusualness
                              qm_Sup = numeric(1),               # Support
                              qm_Cov = numeric(1),               # Coverage
                              qm_tpr = numeric(1),               # True Positive Rate
                              qm_fpr = numeric(1),               # False Positive Rate
                              
                              #Variables for ranking NSGA-II
                              rank = integer(1),
                              crowdingDistance = numeric(1)
                    ),  
                    class = "Rule")
  
  
   # Select Randomly variables of the dataset to create the antecedent (maximum 50 % of the variables are selected)
  numVarsOfRule <- ceiling(.randIntClosed(1,dataset$nVars) * 0.5)
  variables <- sample(dataset$nVars, numVarsOfRule)
  rule$antecedent <- lapply(variables, .createNewFuzzyAntecedent, dataset)
  
  
  # Fill the rest of the data.
  rule$t_norm <- tnorm
  rule$t_conorm <- tconorm
  rule$ruleWeight <- rule_weight
  rule$level = 1L
  
  if(is.null(class))
    rule$clas <- sample(0:(length(dataset$class_names) - 1), 1)
  else
    if(length(dataset$class_names) - 1 <= class & class >= 0)
      rule$clas <- class
    else
      stop("Error when creating a new Rule: 'class' is greater than te number of classes or is less than zero.")
  #Return 
  rule
}




#'
#'
#' Creates a random fuzzy antecedent for a specific variable
#' 
#' @param  variable The position of the variable to use.
#' @param dataset The SDEFSR_Dataset dataset where the function takes the data.
#' 
#' @noRd
#' 
.createNewFuzzyAntecedent <- function(variable, dataset){
  antecedent <- structure(list(labels = list(), 
                               max_label = integer(1), 
                               var = integer(1), 
                               operator = integer(1)),
                            class = "FuzzyAntecedent")
  
  antecedent$var <- variable
  
  if(dataset$attributeTypes[variable] == "c"){
    antecedent$max_label <- dataset$max[variable]
  } else {
    antecedent$max_label <- dataset$sets[variable]
  }
  
  
  #Select the variable randomly

    i <- sample(antecedent$max_label, size = 1)
    antecedent$labels <- list(list(name = dataset$attributeNames[variable], value = i - 1))
    antecedent$operator <- round(runif(1), digits = 0)

  antecedent
}






#'
#' Delete a random variable from a rule 
#' 
#' @param rule The rule to work with.
#' 
#' @return A new rule with a random variable in the antecedent removed.
#' @noRd
Rule.deleteVariable <- function(rule){
  if(class(rule) == "Rule"){
    #Select the variable to remove in the antecedent
    variable <- .randIntClosed(1, length(rule$antecedent))
    #Remove the antecedent
    rule$antecedent <- rule$antecedent[- variable]
    
    #Reset the values, because it is a new rule
    rule[c(3:5, 8,10, 13:19)] <- 0
    rule$evaluated <- FALSE
    rule$level <- 1L
    
  
    #Return 
    rule
  }
}




#'
#'  Remove completely the antecedent part of a rule.
#'  
#'  @param rule The rule to where we want to remove the antecedent.
#'  
#'  @return A new rule with an empty antecedent
#' @noRd
Rule.clearAntecedent <- function(rule){
  if(class(rule) == "Rule"){
    #Remove antecedent part overwrittin by an empty list.
    rule$antecedent <- list()
    
    #Reset the values, because it is a new rule
    rule[c(3:5, 8,10, 13:19)] <- 0
    rule$evaluated <- FALSE
    rule$level <- 1L
    
    
    
    #Return
    rule
  }
}


#'
#' Add a new random variable to the antecedent of the rule.
#' 
#' @param rule The rule to work with.
#' @param dataset The dataset to get the data.
#' @return A new rule with an added variable.
#' @noRd
Rule.addVariable <- function(rule, dataset){
  if(class(rule) == "Rule"){
    
    nVars <- dataset$nVars
    old_antecedent <- rule$antecedent
    
    if(length(old_antecedent) >= nVars){
      stop("It is not possible to add new vars to this rule because it has all possible variables.")
    }
    
    # Get variables that are now in the rule.
    selected <- logical(nVars)
    selected[vapply(old_antecedent, function(x){x$var}, integer(1))] <- TRUE
    
    #From the not selected variables, we pick up one randomly
    notSel <- which(!selected)
    old_antecedent[[length(old_antecedent) + 1]] <- 
      .createNewFuzzyAntecedent(variable = notSel[.randIntClosed(1,length(notSel))],
                               dataset = dataset)
    rule$antecedent <- old_antecedent
    
    #RESET VALUES, it is a new rule
    rule[c(3:5, 8,10, 13:19)] <- 0
    rule$evaluated <- FALSE
    rule$level <- 1L
    
    
    
    #Return
    rule
  }
}
  
  
  #'
  #' Adds a new random label to the variable selected in the rule.
  #' 
  #'  @param rule The rule to work with
  #'  @param variable the selected variable (is the index of the list $antecedent of the rule)
  #'  @param dataset, the dataset where getting all the data.
  #'  
  #'  @return the same rule but with the params reinitialized and marked as non evaluated.
  #' @noRd
  Rule.addLabel <- function(rule, variable, dataset){
    if(class(rule) == "Rule" & class(dataset) == "SDEFSR_Dataset"){
      
      if(variable > length(rule$antecedent)){
        stop("Can not add label to this rule. Variable used is grater than antecedent size.")
      }
      
      fuzzyAnt <- rule$antecedent[[variable]]
      datasetVar <- fuzzyAnt$var
      
      selected <- logical(fuzzyAnt$max_label)
      #Find a label that is not in the rule yet.
      selected[fuzzyAnt[[1]][[1]][[2]]] <- TRUE
      if(! all(selected)){
         labelSelected <- sample(which(!selected), 1)
        
        #Add the label to the fuzzy Antecedent
      
          if(length(fuzzyAnt$labels) < 1){
            fuzzyAnt$labels <- list(list(name = dataset$attributeNames[datasetVar], value = labelSelected - 1))
          }
     
      }
      
      rule$antecedent[[variable]] <- fuzzyAnt
      #Reset the values, because it is a new rule
      rule[c(3:5, 8,10, 13:19)] <- 0
      rule$evaluated <- FALSE
      rule$level <- 1L
      
      
      #return
      rule
    }
  }
  
  
  
  
  
  #'
  #' Change randomly a label associated to the rule with a non-existing label on the rule
  #' 
  #'  @param rule The rule to work with
  #'  @param variable the selected variable (is the index of the list $antecedent of the rule)
  #' @noRd
  Rule.changeLabel <- function(rule, variable){
    if(class(rule) == "Rule"){
      if(variable > length(rule$antecedent)){
        stop("Can not change Label. 'variable' is greater than antecedent length.")
      }
      
      fuzzyAnt <- rule$antecedent[[variable]]
      selected <- logical(fuzzyAnt$max_label)
      
      #Find a label that is not in the rule yet.
      if(length(fuzzyAnt$labels) > 0)
        selected[fuzzyAnt[[1]][[1]][[2]] + 1] <- TRUE
      
      if(!all(selected)){
        labelSelected <- sample(which(!selected), 1) - 1
        #Select a label and change it.
        fuzzyAnt[[1]][[1]][[2]] <- labelSelected
      }
      
      rule$antecedent[[variable]] <- fuzzyAnt
      #Reset the values, because it is a new rule
      rule[c(3:5, 8,10, 13:19)] <- 0
      rule$evaluated <- FALSE
      rule$level <- 1L
     
      
      #return
      rule
      
    }
  }
  
  
  #'
  #' Create a new rule by mixing the antecedent part of two rules.
  #' 
  #' @param rule1 A rule
  #' @param rule2 Another rule
  #' 
  #' @return a list with the antecedents mixed. (This list would be the $antecedent part of another rule)
  #' @noRd
  Rule.exchangeVariables <- function(rule1, rule2){
    if(class(rule1) == "Rule" & class(rule2) == "Rule"){
      fuzzyAnt1 <- rule1$antecedent
      fuzzyAnt2 <- rule2$antecedent
      
      #Select randomly variables from antecedent 1 and 2
      if(length(fuzzyAnt1) > 1){
        selected1 <- runif(length(fuzzyAnt1)) < 0.5
      } else {
        selected1 <- TRUE
      }
      
      if(length(fuzzyAnt2) > 1){
        selected2 <- runif(length(fuzzyAnt2)) < 0.5
      } else {
        selected2 <- TRUE
      }
      
      #Mix this variables
      fuzzyAnt1 <- fuzzyAnt1[selected1]
      fuzzyAnt2 <- fuzzyAnt2[selected2]
      
      values1 <- vapply(fuzzyAnt1, function(x){x$var}, integer(1))
      
      # Forma muy INEFICIENTE !! 
      for(x in fuzzyAnt2){
        found <- which(x$var == values1)
        if(length(found) > 0){  # FuzzyAnt1 now has this variable. We try to mix them.
            if(length(fuzzyAnt1[[found]][[1]]) < 1)
              fuzzyAnt1[[found]][[1]][[1]] <- x[[1]][[1]]
        } else {
          fuzzyAnt1[[length(fuzzyAnt1) + 1]] <- x
        }
      }
    
      #Return 
      fuzzyAnt1
    }
  }
  
  
  
  #' 
  #'  Evaluate a single rule. 
  #'  
  #'  @param rule The rule we want to evaluate (Class "Rule").
  #'  @param dataset The SDEFSR_Dataset dataset object with the examples to compare with the rule (Class "SDEFSR_Dataset")
  #'  @param data Matrix with the data of the dataset, one colum per rule. The data must not contain the last column, the class. (use .separate for this task and convert the list into a matrix)
  #'  @param categoricalValues a logical vector indicating which attributes in the dataset are categorical
  #'  @param numericalValues a logical vector indicating which attributes in the dataset are numerical
  #'  @param t_norm The T-norm to use. 0 for minimum t-norm, 1 for product t-norm (default: 1)
  #'  @param ruleWeight An integer with the rule weighting method. \itemize{
  #'         \item 0 -> Classic Certainty Factor weight
  #'         \item 1 -> Penalized Certainty Factor weight II
  #'         \item 2 > Penalized Certainty Factor weight IV
  #'         \item 3 -> No Rule Weight
  #'         }
  #' @return The rule evaluated.
  #' @noRd
  Rule.evaluate <- function(rule, dataset, data, categoricalValues, numericalValues, t_norm = 1, ruleWeight = 0){
    if(class(rule) == "Rule" & class(dataset) == "SDEFSR_Dataset"){
      if(! rule$evaluated){
        
        correctly_matching_examples_by_clas <- integer(length(dataset$class_names)) # For calculate significance
        compatibility_matching_examples_by_clas <- numeric(length(dataset$class_names))
        compatibility_matching_examples <- numeric(1)
        matching_examples <- integer(1)
        compatibility_correctly_matching_examples <- numeric(1)
        correctly_matching_examples <- integer(1) #TP
        
        #Get compatibility
        perts <- .fitnessFuGePSD(rule = Rule.toCANVectorRepresentation(rule, dataset), 
                                 dataset = dataset, 
                                 noClass = data,
                                 nLabels = dim(dataset$fuzzySets)[1], 
                                 maxRule = dataset$sets,
                                 cate = categoricalValues, 
                                 num = numericalValues,
                                 t_norm = t_norm)
       
        
        #Calculate covered examples.
        covered <- which(perts > 0) 
        notCovered <- which(perts == 0)
        classes <- unlist(.getClassAttributes(dataset$data[covered]))
        correctly_matching_examples_by_clas[as.integer(names(table(classes + 1)))] <- table(classes + 1)
        matching_examples <- length(covered)
        compatibility_matching_examples <- sum(perts[covered])
        
        #Calculate compatibility per class (for rule weigth)
        for(i in seq_len(length(classes))){
          compatibility_matching_examples_by_clas[classes[i] + 1] <- compatibility_matching_examples_by_clas[classes[i] + 1] + perts[covered[i]]
        }
        
        corr_covered <- covered[which(classes == rule$clas)]
        #False positives: Examples covered with a distinct consecuent
        fp <- length(covered[which(classes != rule$clas)])
        #True negatives: Examples not covered with distinc consecuent
        tn <- length(notCovered[which(classes != rule$clas)])
        #False negatives: Examples not covered with the same consecuent
        fn <- length(notCovered[which(classes == rule$clas)])
        
        compatibility_correctly_matching_examples <- sum(perts[corr_covered])
        correctly_matching_examples <- length(corr_covered)
        
        rule$ideal <- length(corr_covered)  #Number of ideal covered examples for token competition procedure
        
        #Mark tokens of the rule
        rule$tokens <- logical(dataset[[16]])
        rule$tokens[corr_covered] <- TRUE
        
        #Calculate quality measures
        rule$qm_Cov <- matching_examples / dataset$Ns
        rule$qm_Sup <- correctly_matching_examples / dataset$Ns
        rule$qm_Sens <- correctly_matching_examples / dataset$examplesPerClass[[rule$clas + 1]]
        if(matching_examples > 0){
          rule$qm_Cnf_n <- correctly_matching_examples / matching_examples
        } else {
          rule$qm_Cnf_n <- 0
        }
        
        if(compatibility_correctly_matching_examples > 0){
          rule$qm_Cnf_f <- compatibility_correctly_matching_examples / compatibility_matching_examples
        } else {
          rule$qm_Cnf_f <- 0
        }
        
        rule$qm_Unus <- rule$qm_Cov * (rule$qm_Cnf_n - (dataset$examplesPerClass[[rule$clas + 1]] / dataset$Ns))
        #TPR and FPR
        rule$qm_tpr <- correctly_matching_examples / (correctly_matching_examples + fn)
        rule$qm_fpr <- fp / (fp + tn)
        
        #Significance computation
        by_class <- which(correctly_matching_examples_by_clas > 0)
        values <- unlist(dataset$examplesPerClass[by_class])
        rule$qm_Sig <- 2 * sum(correctly_matching_examples_by_clas[by_class] * log10(values / (values * rule$qm_Cov) ))
        
        #Assing fitness and weights
        rule$raw_fitness <- rule$qm_Cnf_f
        rule$penalized_fitness <- -1
        
        #Assign rule weight
        if(ruleWeight == 0){ #Classis Certainty Factor weight
          total <- sum(compatibility_matching_examples_by_clas)
          if(total != 0){
             rule$ruleWeight <- compatibility_matching_examples_by_clas[rule$clas + 1] / total
          } else {
            rule$ruleWeight <- 0
          }
        }  else if(ruleWeight == 1){ #Penalized Certainty Factor weight II
          total <- sum(compatibility_matching_examples_by_clas)
          if(total != 0){
            suma <- (total - compatibility_matching_examples_by_clas[rule$clas + 1]) / (length(dataset$class_names) - 1)
            rule$ruleWeight <- (compatibility_matching_examples_by_clas[rule$clas + 1] - suma) / total
          } else {
            rule$ruleWeight <- 0
          }
        } else if(ruleWeight == 2){ #Penalized Certainty Factor weight II
          total <- sum(compatibility_matching_examples_by_clas)
          if(total != 0){
            suma <- total - compatibility_matching_examples_by_clas[rule$clas + 1] 
            rule$ruleWeight <- (compatibility_matching_examples_by_clas[rule$clas + 1] - suma) / total
          } else {
            rule$ruleWeight <- 0
          }
        } else {
          rule$ruleWeight <- 1
        }
        #Set rule as evaluated
        rule$evaluated <- TRUE
      }
      #Return
      rule
    }
  }
  
  
  
  
  #'
  #' Converts a rule antecedent to a CANONICA vector representation
  #' 
  #' The function converts a rule into a CANONICA vector representation for ease the evaluation.
  #' The evaluation of a rule with a vector representation can be evaluated througth functions
  #' of evaluation of rule (.fit13 or .fitnessMESDIF) that are available inside the package SDEFSR
  #' 
  #' @param rule The rule that we want to evaluate.
  #' @param dataset The SDEFSR_Dataset dataset which rule refers to.
  #' 
  #' @return a matrix with one column to use easily with fitness functions of this package
  #' @noRd
  Rule.toCANVectorRepresentation <- function(rule, dataset){
    vector <- dataset$sets
    
    vars <- vapply(rule$antecedent, function(x) x$var, integer(1))
    values <- vapply(rule$antecedent, function(x) x$labels[[1]][[2]], numeric(1))
    
    
    vector[vars] <- values
    
    as.matrix(vector)
  }
  
  
  #'
  #' Performs tournament selection for the FuGePSD algorithm
  #' 
  #' It makes a tournament selection for the FuGePSD algorithm with variable tournament size.
  #' 
  #' @param pop The rule population
  #' @param tamTournament The size of the tornament (>= 2)
  #' 
  #' @return the index in \code{'pop'} of the best individual in the tournament.
  #' @noRd
  tournamentSelection <- function(pop, tamTournament){
    if(tamTournament < 2)
      stop("'tamTournament' must be 2 or greater than 2.")
    
    #select individuals of the populations
    individuals <- sample(seq_len(length(pop)), size = tamTournament, replace = FALSE)
    rawFitnessIndividuals <- vapply(pop[individuals], function(x, pop){x$raw_fitness}, numeric(1), pop)
    
    individuals[which(rawFitnessIndividuals == max(rawFitnessIndividuals))][1]
  }
  
  
  
  
  #'
  #' Perfom the mutation operator for FuGePSD algorithm.
  #' 
  #' @param rule The rule object to be mutated.
  #' @param dataset a SDEFSR_Dataset object associated with the rule.
  #' @return a new rule object with the rule mutated.
  #' @noRd
  FuGePSD_Mutation <- function(rule, dataset){
 
    #select a random variable to mute.
    variable <- .randInt(1, length(rule$antecedent))
    
    if(runif(1) < 0.5){
      #Apply label adition
      new_rule <- Rule.addLabel(rule = rule, variable = variable, dataset = dataset)
    } else {
      #Apply label change
      new_rule <- Rule.changeLabel(rule, variable)
    }
    
    #Return
    new_rule
  }
  
  #' 
  #' Performs the FuGePSD crossover function.
  #' 
  #' @param rule1 a rule object
  #' @param rule2 another rule object.
  #' @param nvars number of variables in the dataset INCLUDING class variable.
  #' @return a new rule object.
  #' @noRd
  FuGePSD_crossover <- function(rule1, rule2, nvars){
    new_rule <- rule1
    if( .randIntClosed(1, nvars) >= nvars ){
      # Cutpoint in the class
      new_rule$clas <- rule2$clas
    } else {
      # Cutpoint in the variables. 
      if(runif(1) < 0.5)
        new_rule$antecedent <- Rule.exchangeVariables(rule1, rule2)
    }
    
    #Reset values of new_rule, this is a new rule
    new_rule[c(3:5, 8,10, 13:19)] <- 0
    new_rule$evaluated <- FALSE
    new_rule$level <- 1L
   
    
    
    #return 
    new_rule
  }
  
  
  
  
  #'
  #' @title Fuzzy Genetic Programming-based learning for Subgroup Discovery (FuGePSD) Algorithm.
  #' @description Make a subgroup discovery task using the FuGePSD algorithm.
  #' 
  #' @param paramFile The path of the parameters file. \code{NULL} If you want to use training and test \code{SDEFSR_Dataset} variables
  #' @param training A \code{SDEFSR_Dataset} class variable with training data.
  #' @param test A \code{SDEFSR_Dataset} class variable with test data.
  #' @param output Character vector with the paths where store information file, rules file and test quality measures file, respectively. For rules and quality measures files, the algorithm generate 4 files, each one with the results of a given filter of fuzzy confidence.
  #' @param seed An integer to set the seed used for generate random numbers.
  #' @param nLabels Number of linguistic labels for numerical variables. By default 3. We recommend an odd number between 3 and 9.
  #' @param t_norm A string with the t-norm to use when computing the compatibilty degree of the rules. Use \code{'Minimum/Maximum'} to specify the minimum t-norm, if not, we use product t-norm that is the default method. 
  #' @param ruleWeight String with the method to calculate the rule weight. Possible values are: 
  #' \itemize{
  #'  \item \code{Certainty_Factor}: It uses the Classic Certainty Factor Weight method.
  #'  \item \code{Average_Penalized_Certainty_Factor}: It uses Penalized Certainty Factor weight II by Ishibuchi.
  #'  \item \code{No_Weights}: There are no weight calculation.
  #'  \item Default: If none of this are specificied, the default method is Penalized Certainty Factor Weight IV by Ishibuchi.
  #'      }
  #' @param frm A string specifying the Fuzzy Reasoning Method to use. Possible Values are:
  #' \itemize{
  #'  \item \code{Normalized_Sum}: It uses the Normalized Sum or Additive Combination Fuzzy Reasoning Method.
  #'  \item \code{Arithmetic_Mean}: It uses the Arithmetic Mean Fuzzy Reasoning Method.
  #'  \item Default: By default, Winning Rule Fuzzy Reasoning Method are selected.
  #' }
  #' @param numGenerations An integer to set the number of generations to perfom before stop the evolutionary process.
  #' @param numberOfInitialRules An integer to set the number individuals or rules in the initial population.
  #' @param crossProb Sets the crossover probability. We recommend a number in [0,1].
  #' @param mutProb Sets the mutation probability. We recommend a number in [0,1].
  #' @param insProb Sets the insertion probability. We recommend a number in [0,1].
  #' @param dropProb Sets the dropping probability. We recommend a number in [0,1].
  #' @param tournamentSize Sets the number of individuals that will be chosen in the tournament selection procedure. This number must be greater than or equal to 2.
  #' @param globalFitnessWeights A numeric vector of length 4 specifying the weights used in the computation of the Global Fitness Parameter. 
  #' @param minCnf A value in [0,1] to filter rules with a minimum confidence
  #' @param ALL_CLASS if TRUE, the algorithm returns, at least, the best rule for each target class, even if it does not pass the filters. If FALSE, it only returns, at least, the best rule if there are not rules that passes the filters.
  #' @param targetVariable The name or index position of the target variable (or class). It must be a categorical one.
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
  #'  @return The algorithm shows in console the following results:
  #'  \enumerate{
  #'    \item Information about the parameters used in the algorithm.
  #'    \item Results for each filter:
  #'      \enumerate{
  #'        \item Rules generated that passes the filter.
  #'        \item The test quality measures for each rule in that filter.
  #'      }
  #'  }
  #'  Also, this results are saved in a file with rules and other with the quality measures, one file per filter.
  #'  
  #'  @section How does this algorithm work?:
  #'  This algorithm performs a EFS based on a genetic programming algorithm. This algorithm starts with an initial 
  #'  population generated in a random manner where individuals are represented through the "chromosome = individual"
  #'  approach includind both antecedent and consequent of the rule. The representation of the consequent has the advantage
  #'  of getting rules for all target class with only one execution of the algorithm.  
  #'  
  #'  The algorithm employs a cooperative-competition approach were rules of the population cooperate and compete between them in order to 
  #'  obtain the optimal solution. So this algorithm performs to evaluation, one for individual rules to competition and other for the total population 
  #'  for cooperation.  
  #'  
  #'  The algorithm evolves generating an offspring population of the same size than initial generated by the application of the
  #'  genetic operators over the main population. Once applied, both populations are joined a token competition is performed in order to 
  #'  maintain the diversity of the rules generated. Also, this token competition reduce the population sice deleting those rules that are not competitive.  
  #'  
  #'  After the evolutionary process a screening function is applied over the best population. This screening function filter the rules that have a minimum
  #'  level of confidence and sensitivity. Those levels are 0.6 for sensitivy and four filters of 0.6, 0.7, 0.8 and 0.9 for fuzzy confidence are performed.  
  #'  
  #'  Also, the user can force the algorithm return at least one rule for all target class values, even if not pass the screening function. This 
  #'  behaviour is specified by the ALL_CLASS parameter.
  #'  
  #'  
  #'  @section Parameters file structure:
  #'   The \code{paramFile} argument points to a file which has the neccesary parameters to execute FuGePSD.
  #'   This file \strong{must} be, at least, this parameters (separated by a carriage return):
  #'   \itemize{
  #'     \item \code{algorithm}  Specify the algorithm to execute. In this case. "MESDIF"
  #'     \item \code{inputData}  Specify two paths of KEEL files for training and test. In case of specify only the name of the file, the path will be the working directory.
  #'     \item \code{seed}  Sets the seed for the random number generator
  #'     \item \code{nLabels}  Sets the number of fuzzy labels to create when reading the files
  #'     \item \code{nEval}  Set the maximum number of \strong{evaluations of rules} for stop the genetic process
  #'     \item \code{popLength}  Sets number of individuals of the main population
  #'     \item \code{eliteLength}  Sets number of individuals of the elite population. Must be less than \code{popLength}  
  #'     \item \code{crossProb}  Crossover probability of the genetic algorithm. Value in [0,1]
  #'     \item \code{mutProb}  Mutation probability of the genetic algorithm. Value in [0,1]
  #'     \item \code{Obj1} Sets the objetive number 1. 
  #'     \item \code{Obj2} Sets the objetive number 2. 
  #'     \item \code{Obj3} Sets the objetive number 3. 
  #'     \item \code{Obj4} Sets the objetive number 4.
  #'     \item \code{RulesRep}  Representation of each chromosome of the population. "can" for canonical representation. "dnf" for DNF representation.
  #'     \item \code{targetClass}  Value of the target variable to search for subgroups. The target variable \strong{is always the last variable.} Use \code{null} to search for every value of the target variable
  #'   }
  #'   
  #'   An example of parameter file could be:
  #'  \preformatted{
  #'  algorithm = FUGEPSD
  #'  inputData = "banana-5-1tra.dat" "banana-5-1tst.dat"
  #'  outputData = "Parameters_INFO.txt" "Rules.txt" "TestMeasures.txt"
  #'  seed = 23783
  #'  Number of Labels = 3
  #'  T-norm/T-conorm for the Computation of the Compatibility Degree = Normalized_Sum
  #'  Rule Weight = Certainty_Factor
  #'  Fuzzy Reasoning Method = Normalized_Sum
  #'  Number of Generations = 300
  #'  Initial Number of Fuzzy Rules = 100
  #'  Crossover probability = 0.5
  #'  Mutation probability = 0.2
  #'  Insertion probability = 0.15
  #'  Dropping Condition probability = 0.15
  #'  Tournament Selection Size = 2 
  #'  Global Fitness Weight 1 = 0.7
  #'  Global Fitness Weight 2 = 0.1 
  #'  Global Fitness Weight 3 = 0.05
  #'  Global Fitness Weight 4 = 0.2
  #'  All Class = true}
  #'
  #' @references 
  #' A fuzzy genetic programming-based algorithm for subgroup discovery and the application to one problem of pathogenesis of acute sore throat conditions in humans, Carmona, C.J., Ruiz-Rodado V., del Jesus M.J., Weber A., Grootveld M., Gonzalez P., and Elizondo D. , Information Sciences, Volume 298, p.180-197, (2015)    
  #'  
  #' @examples 
  #' FUGEPSD(training = habermanTra,
  #'          test = habermanTst,
  #'          output = c(NA, NA, NA),
  #'          seed = 23783,
  #'          nLabels = 3,
  #'          t_norm = "Minimum/Maximum",
  #'          ruleWeight = "Certainty_Factor",
  #'          frm = "Normalized_Sum",
  #'          numGenerations = 20,
  #'          numberOfInitialRules = 15,
  #'          crossProb = 0.5,
  #'          mutProb = 0.2,
  #'          insProb = 0.15,
  #'          dropProb = 0.15,
  #'          tournamentSize = 2,
  #'          globalFitnessWeights = c(0.7, 0.1, 0.3, 0.2),
  #'          ALL_CLASS = TRUE)
  #'          
  #' \dontrun{
  #' # Execution with a parameters file called 'ParamFile.txt' in the working directory:
  #' 
  #' FUGEPSD("ParamFile.txt")
  #' 
  #' }
  #' 
  #' @author Written on R by Angel M. Garcia <amgv0009@@red.ujaen.es>
  #'  
  #' @export
  #' 
  FUGEPSD <- function(paramFile = NULL,
                      training = NULL,
                      test = NULL,
                      output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
                      seed = 0,
                      nLabels = 3,
                      t_norm = "product_t-norm",
                      ruleWeight = "Certainty_Factor",
                      frm = "Normalized_Sum",
                      numGenerations = 300,
                      numberOfInitialRules = 100,
                      crossProb = 0.5,
                      mutProb = 0.2,
                      insProb = 0.15,
                      dropProb = 0.15,
                      tournamentSize = 2,
                      globalFitnessWeights = c(0.7, 0.1, 0.05, 0.2),
                      minCnf = 0.6,
                      ALL_CLASS = TRUE,
                      targetVariable = NA
                      ){
    #Catch start time
    init_time <- as.numeric(Sys.time())
    
    if(is.null(paramFile)){
      #Generate our parameters file
     
      
      if(is.null(test))
        test <- training #To execute only one dataset
      
      if(class(training) != "SDEFSR_Dataset" | class(test) != "SDEFSR_Dataset")
        stop("Training or test or both object must be 'SDEFSR_Dataset' class.")
      if(training[[1]] != test[[1]] )
        stop("datasets ('training' and 'test') does not have the same relation name.")
      if(length(output) != 3 )
        stop("You must specify three files to save the results.")
      if(length(globalFitnessWeights) != 4)
        stop("'globalFitnessWeights' must be a length 4 vector.")
      if(tournamentSize < 2)
        stop("'tournamentSize' must be greater than or equal to 2.")
      parameters <- list(algorithm = "FUGEPSD", 
                    inputData = c(as.character(substitute(training)), as.character(substitute(test))),
                    outputData = output, 
                    seed = seed, 
                    nLabels = nLabels, 
                    nGens = numGenerations, 
                    popLength = numberOfInitialRules, 
                    crossProb = crossProb, 
                    mutProb = mutProb,
                    insPro = insProb,
                    dropProb = dropProb,
                    frm = tolower(frm),
                    tnorm = tolower(t_norm),
                    ruleWeight = tolower(ruleWeight),
                    tournamentSize = tournamentSize,
                    allClass = ALL_CLASS,
                    alphaFitness = 0,
                    executionType = 1,
                    gfw1 = globalFitnessWeights[1],
                    gfw2 = globalFitnessWeights[2],
                    gfw3 = globalFitnessWeights[3],
                    gfw4 = globalFitnessWeights[1],
                    targetVariable = if(is.na(targetVariable)) training$attributeNames[length(training$attributeNames)] else targetVariable)
      
      #Print parameters in console
      printFuGePSDParameters(parameters, training, FALSE)
    } else {
      #Read parameters file
      parameters <- .read.parametersFile2(paramFile)
      #Read traing/test data
      training <- read.dataset(parameters$inputData[1])
      if(is.na(parameters$inputData[2])){
        test <- training
      } else {
        test <- read.dataset(file = parameters$inputData[2])        # test data
      }
      
      #Print parameters in console
      printFuGePSDParameters(parameters, training, FALSE)
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
    
    categorical <- training$attributeTypes == "c"
    categorical <- categorical[-length(categorical)]
    numerical <- !categorical
    
    #Parse parameters
    
    if(parameters$tnorm == "minimum/maximum"){
      parameters$tnorm = 0
    } else {
      parameters$tnorm = 1
    }
    
    if(parameters$ruleWeight == "certainty_factor"){
      parameters$ruleWeight <- 0
    } else if(parameters$ruleWeight == "average_penalized_certainty_factor"){
      parameters$ruleWeight <- 2
    } else if(parameters$ruleWeight == "no_weights"){
      parameters$ruleWeight <- 3
    } else {
      parameters$ruleWeight <- 1
    }
    
    if(parameters$frm == "normalized_sum"){
      parameters$frm <- 1
    } else if(parameters$frm == "arithmetic_mean") {
      parameters$frm <- 2
    } else {
      parameters$frm <- 0
    }
    set.seed(parameters$seed)
    
    message("\n\nSearching Rules for all classes...\n\n")
    
    #Execute the genetic algorithm
    pop <- .gaFuGePSD(type = parameters[[18]],
               dataset = training,
               selection = tournamentSelection,
               mutation = FuGePSD_Mutation,
               crossover = FuGePSD_crossover,
               popSize = parameters[[7]],
               pcrossover = parameters[[8]],
               pmutation = parameters[[9]],
               pinsertion = parameters[[10]],
               pdropping = parameters[[11]],
               selectionSize = parameters[[15]],
               AllClass = as.logical(parameters[[16]]),
               T_norm = parameters[[13]],
               ruleWeight = parameters[[14]],
               frm = parameters[[12]],
               maxiter = parameters[[6]],
               weightsGlobalFitness = c(parameters[[19]], parameters[[20]], parameters[[21]], parameters[[22]]),
               seed = parameters[[4]])
    
    
    AllClass <- as.logical(parameters[[16]])
    exampleClass <- unlist(.getClassAttributes(test$data))
    datasetNoClass <- matrix(unlist(.separate(test)), nrow = test$nVars, ncol = test$Ns)
    
    ###################
    #  TESTING RULES  #
    ###################
    
    
    #Now we have the best population obtained by the evolutionary process. Then, we apply 0.6, 0.7, 0.8 and 0.9
    #filters of fuzzy confidence.
    bestPop <- pop[[1]]
    lapply(seq_len(length(bestPop)), function(x){bestPop[[x]]$evaluated <<- FALSE; invisible()})
    classes <- vapply(bestPop, function(x){x$clas}, integer(1)) + 1L
   
    
    #SCREENING FUNCTION
     examples_class <- logical(length(training$class_names))
     pasan_filtro <- which(pop[[2]] >= minCnf & pop[[3]] >= 0.6)
     new_pop <- bestPop[pasan_filtro]
     
     if(length(new_pop) > 0){
       examples_class[classes[pasan_filtro]] <- TRUE
     }
     
     if(!all(examples_class) & AllClass){
       cl <- which(!examples_class)
       cl <- which(classes %in% cl)
       pos <- which(pop[[3]][cl] >= 0.6)
       posi <- na.exclude(pmatch(which(!examples_class), classes[pos]))
       if(length(posi) > 0){
         new_pop[(length(new_pop) + 1):(length(new_pop) + length(posi))] <- bestPop[pos[posi]]
         examples_class[classes[pos[posi]]] <- TRUE
       }
     }
     
     #Verify the use of parameter ALL_CLASS
     if(AllClass){
       if(!all(examples_class)){
       #As the bestPop is ordered by fuzzy confidence, we take the best rule in terms of confidence for 
       #each class that are not in the new population yet.
       pos <- na.exclude(pmatch(which(!examples_class), classes))
       new_pop[(length(new_pop) + 1):(length(new_pop) + length(pos))] <- bestPop[pos]
       }
     } else {
       #If ! ALL_CLASS and new_pop is empty, the algorithm returns the best of the population.
       if(length(new_pop) == 0){
         new_pop[[1]] <- bestPop[[1]]
       }
     }
     
     #Order new_pop by class
     new_classes <- vapply(new_pop, function(x){x$clas}, integer(1))
     new_pop <- new_pop[order(new_classes)]
     
     #Evalute population against test data
     testGlobalFitness <- Pop.evaluate(pop = bestPop, dataset = test, examplesNoClass = datasetNoClass, exampleClass = exampleClass, frm = parameters[[12]], categorical = categorical, numerical = numerical, t_norm = parameters[[13]], weights = c(parameters[[19]], parameters[[20]], parameters[[21]], parameters[[22]]) )
     new_pop <- lapply(new_pop, Rule.evaluate, test, datasetNoClass, categorical, numerical, parameters[[13]], parameters[[14]])
     
     #Print results in files.
     contador <- 1
     
     #Change the name of the output file by the following: $fileName$_filtro_AllClass.txt for rules file with filter 'filtro'
     #                                                     $fileName$_filtro_AllClass_QM.txt
     if(! is.na(parameters$outputData[2])){
     ruleFileName <- paste(substr(parameters$outputData[2], 1, regexpr("\\.[^\\.]*$", parameters$outputData[2]) - 1),
                                "_f", paste(substr(as.character(minCnf), 1, 1) , substr(as.character(minCnf), 3, 3) , sep = ""), "_", 
                                AllClass, ".txt", sep = "")
     
     testQMFileName <- paste(substr(parameters$outputData[2], 1, regexpr("\\.[^\\.]*$", parameters$outputData[2]) - 1),
                           "_f", paste(substr(as.character(minCnf), 1, 1) , substr(as.character(minCnf), 3, 3) , sep = ""), "_", 
                           AllClass, "_QM", ".txt", sep = "")
      
      message("\n\n---- FILTER: ", minCnf, " ----\n\n", sep = "")
     
      writeRuleFile(new_pop, training, ruleFileName)
      message("\n QUALITY MEASURES OF THE RULES GENERATED: \n\n")
      writeTestQMFile(new_pop, testQMFileName, test$Ns)
      }
      #Create the rulesToReturn structure to return the rules
      ##Add values to the rulesToReturn Object
      rulesToReturn <- vector(mode = "list", length = length(new_pop))
      count <- 1
      for(i in new_pop){
      #Create the CAN vector representation of the rule with the string of the class to add it to the rule object
      ruleAsCAN <- c(as.vector(Rule.toCANVectorRepresentation(rule = new_pop[[count]], dataset = training)), training$class_names[new_pop[[count]]$clas + 1])
      rulesToReturn[[count]] <- list(rule = createHumanReadableRule(ruleAsCAN, training, FALSE), #FuGePSD does not have DNF representation !
                                qualityMeasures = list(nVars = length(i$antecedent),
                                                       Coverage = i$qm_Cov,
                                                       Unusualness = i$qm_Unus,
                                                       Significance = i$qm_Sig,
                                                       FuzzySupport = i$qm_Sup,
                                                       Support = sum(i$tokens) / length(i$tokens),
                                                       FuzzyConfidence = i$qm_Cnf_f,
                                                       Confidence = i$qm_Cnf_n,
                                                       TPr = i$qm_tpr,
                                                       FPr = i$qm_fpr))
      count <- count + 1
      }
   
   message("\n\nAlgorithm finished. \nExecution time: ", parseTime(as.numeric(Sys.time()), init_time),  "\n\n", sep = "")
    
   #return
   class(rulesToReturn) <- "SDEFSR_Rules"
   rulesToReturn
  }
  
  
#'
#' Calcultes the Fuzzy Reasoning Method for all examples for a given rule population
#' 
#' @param pop A list with all rule object that define the population
#' @param dataset the SDEFSR_Dataset object with the dataset information.
#' @param examplesNoClass matrix with all examples of the dataset without the class attribute. One examples per column.
#' @param frm An integer specifing the tipo of fuzzy reasoning method to use. 0 for Winning Rule, 1 for Normalized Sum and 2 for Arithmetic Mean.
#' @param categorical Logical vector indicating which attributes of the dataset are categorical.
#' @param numerical Logical vector indicating which attributes of the dataset are numerical.
#' @param t_norm The t_norm to use. 0 for minimum t-norm. 1 for product t-norm.
#' 
#' @return a vector indicating the class predicted for each example.
#' @noRd
Pop.fuzzyReasoningMethod <- function(pop, dataset, examplesNoClass, frm, categorical, numerical, t_norm){
  rules <- lapply(X = pop, Rule.toCANVectorRepresentation, dataset)
  
  #Calculate compatibility of all examples with all rules.
  perts <- lapply(X = rules,FUN = .fitnessFuGePSD, dataset, examplesNoClass, dim(dataset$fuzzySets)[1], dataset$sets, categorical, numerical, t_norm)
  #Multiply this compatibilty with the ruleWeight
  weight <- vapply(X = pop, function(x){x$ruleWeight}, numeric(1))
  perts <- lapply(X = seq_len(length(pop)), FUN = function(x, lista, weights){lista[[x]] * weights[x]}, perts, weight)
  
  df <- as.data.frame(matrix(unlist(perts), nrow = length(pop)))
  
  if(frm == 1){ #Normalized Sum
    class_degrees <- numeric(length(dataset$class_names))
    classes <- integer(dim(df)[2])
    example <- 1
    for(i in df){ #For each example
      cont <- 1
      for(j in i){ #For each rule
        class_degrees[pop[[cont]]$clas + 1] <-  class_degrees[pop[[cont]]$clas + 1] <- class_degrees[pop[[cont]]$clas + 1] + .subset2(df, c(example, cont))
        cont <- cont + 1
      }
      classes[example] <- which(class_degrees == max(class_degrees))[1] - 1
      example <- example + 1
    }
    
    #Return
    classes
    
  } else if(frm == 2) {  #Arithmetic Mean
    
    class_degrees <- numeric(length(dataset$class_names))
    
    #Count of numbers of rules which have a class name.
    class_cont <- integer(length(dataset$class_names))
    names(class_cont) <- dataset$class_names
    cuenta <- table(vapply(pop, function(x){x$clas}, integer(1)))
    class_cont[as.integer(names(cuenta)) + 1] <- cuenta
    
    #Count the sumatory of rules.
    classes <- integer(dim(df)[2])
    example <- 1
    for(i in df){ #For each example
      cont <- 1
      for(j in i){ #For each rule
        class_degrees[pop[[cont]]$clas + 1] <- class_degrees[pop[[cont]]$clas + 1] + .subset2(df, c(example, cont))
        cont <- cont + 1
      }
      class_degrees <- class_degrees / class_cont
      classes[example] <- which(class_degrees == max(class_degrees))[1] - 1
      example <- example + 1
      class_degrees[] <- 0
    }
    
    #Return
    classes
  } else { #Wining rule
  
    #For each example, we take the maximum compatibility degree and take the class associated with the rule with maximum compatibility
    classes <- vapply(X = df, FUN = function(x){which(x == max(x))[1]}, integer(1))
    
    #Return
    vapply(X = classes, function(x, popu){popu[[x]]$clas}, integer(1), pop) - 1
  }
  
}
  

#'
#'  Evaluates the entire population for the Global Fitness computation procedure.
#'  
#' @param pop A list of 'Rule' objects.
#' @param dataset A 'SDEFSR_Dataset' object with all the information of the dataset we are working
#' @param examplesNoClass Matrix with the data of the dataset, one colum per rule. The data must not contain the last column, the class. (use .separate for this task and convert the list into a matrix)
#' @param exampleClass Vector with the classes of all examples of the dataset
#' @param frm An integer specifing the tipo of fuzzy reasoning method to use. 0 for Winning Rule, 1 for Normalized Sum and 2 for Arithmetic Mean.
#' @param categorical A logical vector indicating which attributes of the dataset are categorical.
#' @param numerical A logical vector indicating which attributes of the dataset are numerical.
#' @param t_norm An integer specifying the t-norm to use. 0 for minimum t_norm, other value for product t-norm.
#' @param weights A numeric vector of length 4 indicating the weights used to calculate the global fitness of this population.
#'  
#' @return A number which indicate the global fitness for this population.
#'  
#' @noRd
Pop.evaluate <- function(pop, dataset, examplesNoClass, exampleClass, frm, categorical, numerical, t_norm, weights){
  nLabels <- dim(dataset$fuzzySets)[1]
  
  prediction <- Pop.fuzzyReasoningMethod(pop, dataset, examplesNoClass, frm, categorical, numerical, t_norm)
  
  hits <- integer(1)
  hitsPerClass <- vapply(X = seq_len(length(dataset$class_names)) - 1, FUN = function(x, prediction, exampleClass){sum(exampleClass == prediction & exampleClass == x)}, numeric(1), prediction, exampleClass)
  #Compute accuracy
  accuracy <- sum(hitsPerClass / unlist(dataset$examplesPerClass))
  accuracy <- accuracy / length(dataset$class_names)
  
  #Compute the average number of variables and conditions
  num_var <- num_cond <- sum(vapply(X = pop, FUN = function(x){length(x$antecedent)}, numeric(1)))
  ave_var <- num_var / length(pop)
  ave_cond <- num_cond / length(pop)
  
  #Normalize values
  norm_var <- (ave_var - 1) / (dataset$nVars - 1)
  norm_cond <- (ave_cond - 1) / ((dataset$nVars * (nLabels - 1)) - 1)
  norm_rul <- (length(pop) - length(dataset$class_names)) / (dataset$Ns - length(dataset$class_names))
  
  #Compute Global fitness and 
  weights[1] * accuracy + weights[2] * (1 - norm_var) + weights[4] * (1 - norm_rul)
}
  




#'
#' Runs a token competition procedure.
#' 
#' @param pop A list of 'Rule' objects.
#' @param dataset A 'SDEFSR_Dataset' object with all the information about the dataset we are working.
#' 
#' @return A list of 'Rule' objects with that rules that pass this token competition procedure.
#' @noRd
tokenCompetition <- function(pop, dataset){
  
  #Order pop by raw fitness
  fitness <- vapply(pop, function(x){x$raw_fitness}, numeric(1))
  pop <- pop[order(fitness, decreasing = TRUE)]
  
  #Empty tokens are false initialy
  tokens <- logical(dataset$Ns)
  
  #Update penalized fitness value for all the population (Token competition procedure)
  pos <- 1
  for(i in pop){
    count <- 0
    if(i$ideal == 0){
        i$penalize_fitness <- 0
    } else {
      cover <- i$tokens & !tokens
      tokens[which(cover)] <- TRUE
      count <- sum(cover)
      
      i$penalized_fitness <- i$raw_fitness * (count / i$ideal)
    }
    pop[[pos]] <- i
    pos <- pos + 1
  }
  
  #Now, we delete the individuals with penalized fitness equal to 0
  fitness <- vapply(pop, function(x){x$penalized_fitness}, numeric(1))
  
  #Return the populations with deleted individuals
  pop[which(fitness > 0)]
}





#'
#' Writes rules into a human-readable format into a file.
#'
#' @param pop A list of 'Rule' objects with the population we want to save.
#' @param dataset A 'SDEFSR_Dataset' object with all the information about the dataset the rules are pointing.
#' @param fileName String with the path to store the rules.
#' 
#' @details 
#' This function overwrites if the file specified by \code{fileName} exists. Be careful!
#' @noRd
writeRuleFile <- function(pop, dataset, fileName){
  contador <- 1
  RulesLine <- ""
  sumVars <- 0
  numRules <- length(pop)
  
  for(rule in pop){
     #Make the human-readable rule representation
     ruleRep <- sapply(rule$antecedent, function(x){paste(x$labels[[1]]$name, " IS L_", x$labels[[1]]$value, sep = "")})
     ruleRep <- paste(ruleRep, collapse = " AND ")
     RulesLine <- paste(RulesLine, contador,": IF ", ruleRep, " THEN ", dataset$class_names[rule$clas + 1], " with Rule Weight: ", rule$ruleWeight, "\n", sep = "")
     sumVars <- sumVars + length(rule$antecedent)
     contador <- contador + 1
  }
  
  finalLine <- paste("@Number of rules: ", numRules, "\n@Average number of variables: ", round(sumVars / numRules, 2), "\n\n", RulesLine, sep = "")
  
  message(finalLine) #Prints in the console
  cat(finalLine, file = fileName) #Prints in the file
  
  invisible()
}

#'
#' Writes rules into a human-readable format into a file.
#'
#' @param pop A list of 'Rule' objects with the population we want to save.
#' @param fileName String with the path to store the rules.
#' @param numExamples An integer with the number of examples in the test file.
#' 
#' @details 
#' This function overwrites if the file specified by \code{fileName} exists. Be careful!
#' @noRd
writeTestQMFile <- function(pop, fileName, numExamples){
  #Get classes of the rules.
  sum_nVars <- numeric(1)
  sum_Cov <- numeric(1)
  sum_Sig <- numeric(1)
  sum_Unus <- numeric(1)
  sum_Sens <- numeric(1)
  tokens <- logical(numExamples)  #For total Support Calculation.
  sum_ConfN <- numeric(1)
  sum_ConfF <- numeric(1)
  
  nRules <- length(pop)
  contador <- 1
  Salida <- ""
  for(rule in pop){
    sum_nVars <- sum_nVars + length(rule$antecedent)
    sum_Cov <- sum_Cov + rule$qm_Cov
    sum_Sig <- sum_Sig + rule$qm_Sig
    sum_Unus <- sum_Unus + rule$qm_Unus
    sum_Sens <- sum_Sens + rule$qm_Sens
    sum_ConfN <- sum_ConfN + rule$qm_Cnf_n
    sum_ConfF <- sum_ConfF + rule$qm_Cnf_f
    
    tokens[which(rule$tokens)] <- TRUE
    
   #Rule QM
   Salida <-  paste(Salida, paste("Rule ", contador, ":", sep = ""),
         paste("\t - N_vars:", length(rule$antecedent), sep = " "),
         paste("\t - Coverage:", round(rule$qm_Cov, 6), sep = " "),
         paste("\t - Significance:", round(rule$qm_Sig, 6), sep = " "),
         paste("\t - Unusualness:", round(rule$qm_Unus, 6), sep = " "),
         paste("\t - Sensitivity:", round(rule$qm_Sens, 6), sep = " "),
         paste("\t - Support:", round(rule$qm_Sup, 6), sep = " "),
         paste("\t - FConfidence:", round(rule$qm_Cnf_f,6), sep = " "),
         paste("\t - CConfidence:", round(rule$qm_Cnf_n, 6), sep = " "),
         sep = "\n"
    )
   contador <- contador + 1
  }
  
  #Global 
  Salida <- paste(Salida, "Global:",
       paste("\t - N_rules:", nRules, sep = " "),
       paste("\t - N_vars:", round(sum_nVars / nRules, 6), sep = " "),
       paste("\t - Coverage:", round(sum_Cov / nRules, 6), sep = " "),
       paste("\t - Significance:", round(sum_Sig / nRules, 6), sep = " "),
       paste("\t - Unusualness:", round(sum_Unus / nRules, 6), sep = " "),
       paste("\t - Sensitivity:", round(sum_Sens / nRules, 6), sep = " "),
       paste("\t - Support:", round(sum(tokens) / numExamples, 6), sep = " "),
       paste("\t - FConfidence:", round(sum_ConfF / nRules, 6), sep = " "),
       paste("\t - CConfidence:", round(sum_ConfN / nRules, 6), sep = " "),
       sep = "\n"
  )
  #Print exit
  message(Salida) # On console
  cat(Salida, file = fileName) #On file.
  
  invisible()
  }




#'
#' Prints parameters information aboute the execution of the algorithm in console and in the output file 
#' specified by this parameters.
#' 
#' @param parameters A list with all neccesary parameters.
#' @param dataset A \code{SDEFSR_Dataset} object with all the information about the dataset.
#' @param inputFromFiles A logical indicating if the input datasets are given from a file or from an object in the R environment.
#' @noRd
printFuGePSDParameters <- function(parameters, dataset, inputFromFiles = TRUE){
  line <- paste("\n------------------------------------------------------\n", 
      "Algorithm: FuGePSD\n",
      "Relation: ", dataset[[1]], "\n",
      "Training file: ", parameters$inputData[1], if(!inputFromFiles) " object\n",
      "Test file: ", parameters$inputData[2], if(!inputFromFiles) " object\n",
      "Seed: ", parameters$seed, "\n",
      "Number of Labels: ", parameters$nLabels, "\n",
      "Number of Generations: ", parameters$nGens, "\n",
      "Initial Number of Rules: ", parameters$popLength, "\n",
      "Crossover Probability: ", parameters$crossProb, "\n",
      "Mutation Probability: ", parameters$mutProb, "\n",
      "Insertion Probability: ", parameters$insPro, "\n",
      "Dropping Condition Probability: ", parameters$dropProb, "\n",
      "Fuzzy Reasoning Method: ", parameters$frm, "\n",
      "T-norm/T-conorm for the Computation of the Compatibility Degree: ", parameters$tnorm, "\n",
      "Rule Weight: ", parameters$ruleWeight, "\n",
      "Tournament Selection Size: ", parameters$tournamentSize, "\n",
      "Global Fitness Weight 1: ", parameters$gfw1, "\n",
      "Global Fitness Weight 2: ", parameters$gfw2, "\n",
      "Global Fitness Weight 3: ", parameters$gfw3, "\n",
      "Global Fitness Weight 4: ", parameters$gfw4, "\n",
      "Target Variable: ", parameters$targetVariable, "\n",
      "All Class: ", parameters$allClass, "\n",
      "-----------------------------------------------------------\n\n", sep = "")
 
  message(line)  #Print in console
  if(! is.na(parameters$outputData[1])){
    cat(line, file = parameters$outputData[1])
  }
  
}
