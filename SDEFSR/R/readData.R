#'
#'
#' Reads a KEEL, ARFF or CSV data format file.
#'
#' This function reads a KEEL (.dat), ARFF (.arff) or CSV dataset file and store the information
#'   in a \code{SDEFSR_Dataset} class. 
#'
#' @param file The path of the file to read
#' @param sep The separator character to use 
#' @param quote The character used to take quotes
#' @param dec The character used to define decimal characters
#' @param na.strings The character to detect lost data
#'
#' @details  A KEEL data file must have the following structure:
#'  \itemize{
#'    \item{ @@relation: Name of the data set }
#'    \item{ @@attribute: Description of an attribute (one for each attribute)}
#'    \item{ @@inputs: List with the names of the input attributes }
#'    \item{ @@output: Name of the output attribute (Not used in this algorithms implementation) }
#'    \item{ @@data: Starting tag of the data}
#' }
#'    The rest of the file contains all the examples belonging to the data set, expressed in comma sepparated values format.
#'    ARFF file format is a well-know dataset format from WEKA data mining tool.
#'    CSV is a format which means comma-separated values. Where each examples is on a line and each value of the variables of the examples
#'    are separated by commas.
#' 
#' @author Angel M. Garcia <agvico@@ujaen.es> 
#'    
#' @references J. Alcala-Fdez, A. Fernandez, J. Luengo, J. Derrac, S. Garcia, L. Sanchez, F. Herrera. KEEL Data-Mining Software Tool: Data Set Repository, Integration of Algorithms and Experimental Analysis Framework. Journal of Multiple-Valued Logic and Soft Computing 17:2-3 (2011) 255-287.
#' 
#' @seealso KEEL Dataset Repository (Standard Classification): \url{https://sci2s.ugr.es/keel/category.php?cat=clas}
#'
#' @examples
#'     \dontrun{
#'        Reads a KEEL dataset from a file.
#'        read.dataset(file = "C:\KEELFile.dat")
#'
#'        read.dataset(file = "C:\KEELFile.dat", nLabels = 7)
#'        
#'      Reads an ARFF dataset from a file.
#'        read.dataset(file = "C:\ARFFFile.arff")
#'
#'        read.dataset(file = "C:\ARFFFile.arff", nLabels = 7)
#'     }
#'     
#' @export
read.dataset <- function(file, sep = ",", quote = "\"", dec = ".", na.strings = "?") {
 
  if(length(file) > 1){
    stop(paste(substitute(file), "must be of length 1."))
  }
  
  #Detect extension
  ext <- regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]]
  
  if(ext == ".dat"){
    #Reads a keel file
    data <- .readFile(file)
    value <- which(data == "@data") - 1
    if (length(value) == 0)
      stop("No '@data' field found, this is not a KEEL format dataset. Aborting load...")
    properties <- data[1:value]
    data <- data[(value + 2):length(data)]
    
    # Get the data properties (value ranges, type, etc.)
    
    properties <- lapply(X = properties, FUN = .preprocessHeader)
    
    num_atribs <-
      length(properties) - 3 # @relation, @inputs and @outputs fields are ignored
    atribs_names <- character(num_atribs)
    atribs_types <- character(num_atribs)
    atribs_min <- numeric(num_atribs)
    atribs_max <- numeric(num_atribs)
    categorical_values <- vector(mode = "list", length = num_atribs)
    
    #Process @relation
    relation_pos <- grep(pattern = "@relation", x = properties, fixed = TRUE) #NOTE: It is better use pmatch in this line! Is 77x faster!
    if (length(relation_pos) == 0)
      stop("No '@relation' field provided, this is not a KEEL format dataset. Aborting load... ")
    relation <- properties[[relation_pos]][2]
    
    #Process the rest of attributes
    attributes <- properties[-c(relation_pos, grep(pattern = "@inputs|@output", x = properties))]
    aux <- vector(mode = "list", length = 5)
    
    if (length(attributes) == 0)
      stop(
        "No '@input' or '@output' fields found, this is not a KEEL format dataset. Aborting load..."
      )
    
    # To process an attribute line we use the function .processLine which gets name, type (categorical or real) and range of a variable
    for (i in seq_len(length(attributes))) {
      aux <- .processLine(line = attributes[[i]])
      
      atribs_names[i] <- aux[[1]]
      atribs_types[i] <- aux[[2]]
      atribs_min[i] <- aux[[3]]
      atribs_max[i] <- aux[[4]]
      categorical_values[[i]] <- aux[[5]]
    }
    
    
    #Data preparation with the function .processData which takes a line that represent an instance an return all neccesary information
    #We use parallel computing for better performance.
    if (Sys.info()[1] != "Windows")
  
      data <-  
      parallel::mclapply(
        X = data, FUN = .processData, categorical_values, atribs_types, mc.cores = parallel::detectCores() - 1 # -1 to prevent system hang up until the task finish
      )
    else
      #In windows mclapply doesnt work (USE of 'snow' package)
      data <-
      parallel::mclapply(
        X = data, FUN = .processData, categorical_values, atribs_types, mc.cores = 1
      )
    
    
    #Prepare the rest of attributes of the SDEFSR_Dataset object
    
    covered <- NA     #Examples covered
    fuzzySets <- NA   # Fuzzy sets definitons
    crispSets <- NA   # Crisp sets definitions
    classNames <- categorical_values[[length(categorical_values)]]   # Categories of the target variable
    clValues <- unlist(lapply(data, '[', length(attributes)))    # gets the class value for each example
    examplesPerClass <-
      lapply(
        X = seq_len(length(classNames)) - 1, FUN = function(x, data)
          sum(data == x), clValues
      )
    names(examplesPerClass) <- classNames
    
    sets <- NA
    
    lostData <- FALSE 
    
    # Creation of the SDEFSR_Dataset object as a list
    lista <- list(
      relation = relation,                    # Relation name
      attributeNames = atribs_names,          # Names of the attributes
      attributeTypes = atribs_types,          # Types of attributes (real, integer or categorical)
      min = atribs_min,                       # Min value for real attributes
      max = atribs_max,                       # Max value for real attributes
      nVars = length(atribs_min) - 1,         # Number of variables
      data = data,                            # The processed data
      class_names = classNames,               # Names of the values of the target variable
      examplesPerClass = examplesPerClass,    # Number of examples per value of the target variable
      lostData = lostData,                    # Boolean that indicate the presence or ausence of lost data (unnecessary)
      covered = covered,                      # Indicate which examples are covered
      fuzzySets = fuzzySets,                  # The fuzzy sets definitions
      crispSets = crispSets,                  # The crisp sets definitions
      sets = sets,                            # The number of sets per variable (for categorical variables indicates the number of different values)
      categoricalValues = categorical_values, # The different values for categorical attributes
      Ns = length(data)                       # Number of examples
    )
    
    
    class(lista) <- "SDEFSR_Dataset"  
    lista
  } else if(ext == ".arff"){
    #Reads an ARFF from WEKA.
    #SDEFSR_DatasetFromARFF(file)
    readFromArff(file)
  } else if(ext == ".csv"){
    #Reads a CSV file
    SDEFSR_DatasetFromCSV(file, relation_name = basename(file), sep = sep, quote = quote, dec = dec, na.strings = na.strings)
  } else{
    stop("Invalid format. Valid formats are: '.arff', '.dat' or '.csv'")
  }
  
}




#
#
#  Reads a parameter file for an implemented algorithm
# @param file The path to the parameters file
# 
# @return a list with the parsed parameters
#
.read.parametersFile2 <- function(file) {
  data <- .readFile(file)
  
  # Format is 'paramName = value'
  data <- strsplit(x = data, split = " = ")
  
  
  #look at parameter's position and get it
  alg <- grep(pattern = "algorithm", x = data, fixed = TRUE)
  iData <- grep(pattern = "inputData", x = data, fixed = TRUE)
  oData <- grep(pattern = "outputData", x = data, fixed = TRUE)
  seed <- grep(pattern = "seed", x = data, fixed = TRUE)
  labels <- grep(pattern = "nLabels", x = data, fixed = TRUE)
  evals <- grep(pattern = "nEval", x = data, fixed = TRUE)
  len <- grep(pattern = "popLength", x = data, fixed = TRUE)
  cross <- grep(pattern = "crossProb", x = data, fixed = TRUE)
  mut <- grep(pattern = "mutProb", x = data, fixed = TRUE)
  rep <- grep(pattern = "RulesRep", x = data, fixed = TRUE)
  tC <- grep(pattern = "targetClass", x = data, fixed = TRUE)
  #MESDIF Parameters
  elit <- grep(pattern = "eliteLength", x = data, fixed = TRUE)
  ech <- grep(pattern = "echo", x = data, fixed = TRUE)
  #SDIGA Parameters
  ob1 <- grep(pattern = "Obj1", x = data, fixed = TRUE)
  ob2 <- grep(pattern = "Obj2", x = data, fixed = TRUE)
  ob3 <- grep(pattern = "Obj3", x = data, fixed = TRUE)
  ob4 <- grep(pattern = "Obj4", x = data, fixed = TRUE)
  w1 <- grep(pattern = "w1", x = data, fixed = TRUE)
  w2 <- grep(pattern = "w2", x = data, fixed = TRUE)
  w3 <- grep(pattern = "w3", x = data, fixed = TRUE)
  search <- grep(pattern = "lSearch", x = data, fixed = TRUE)
  miConf <- grep(pattern = "minConf", x = data, fixed = TRUE)
  #NMEEF-SD Parameters
  div <- grep(pattern = "diversity", x = data, fixed = TRUE)
  rInit <- grep(pattern = "ReInitCob", x = data, fixed = TRUE)
  pCob <- grep(pattern = "porcCob", x = data, fixed = TRUE)
  dom <- grep(pattern = "StrictDominance", x = data, fixed = TRUE)
  miCf <- grep(pattern = "minCnf", x = data, fixed = TRUE)
  #FuGePSD Parameters
  nLabels <- grep(pattern = "Number of Labels", x = data, fixed = TRUE)
  tnorm <- grep(pattern = "T-norm/T-conorm for the Computation of the Compatibility Degree", x = data, fixed = TRUE)
  ruleWeight <- grep(pattern = "Rule Weight", x = data, fixed = TRUE)
  frm <- grep(pattern = "Fuzzy Reasoning Method", x = data, fixed = TRUE)
  numGens <- grep(pattern = "Number of Generations", x = data, fixed = TRUE)
  tamPop <- grep(pattern = "Initial Number of Fuzzy Rules", x = data, fixed = TRUE)
  crossProb <- grep(pattern = "Crossover probability", x = data, fixed = TRUE)
  mutProb <- grep(pattern = "Mutation probability", x = data, fixed = TRUE)
  insProb <- grep(pattern = "Insertion probability", x = data, fixed = TRUE)
  dropProb <- grep(pattern = "Dropping Condition probability", x = data, fixed = TRUE)
  tsSize <- grep(pattern = "Tournament Selection Size", x = data, fixed = TRUE)
  gfw1 <- grep(pattern = "Global Fitness Weight 1", x = data, fixed = TRUE)
  gfw2 <- grep(pattern = "Global Fitness Weight 2", x = data, fixed = TRUE)
  gfw3 <- grep(pattern = "Global Fitness Weight 3", x = data, fixed = TRUE)
  gfw4 <- grep(pattern = "Global Fitness Weight 4", x = data, fixed = TRUE)
  allClass <- grep(pattern = "All Class", x = data, fixed = TRUE)
  #--------------------------------------------------------
  
  if (length(alg) == 0)
    stop("Param file error: 'algorithm' not especified. ")
  algorithm <- data[[alg]][2]  
  if (length(iData) == 0)
    stop("Param file error: 'inputData' not especified. ")
  if (length(oData) == 0)
    stop("Param file error: 'outputData' not especified. ")
  if (length(seed) == 0)
    stop("Param file error: 'seed' not especified. ")
  
  if(any(algorithm == c("SDIGA", "MESDIF", "NMEEFSD"))){
    if (length(labels) == 0)
      stop("Param file error: 'nLabels' not especified. ")
    if (length(evals) == 0)
      stop("Param file error: 'nEval' not especified. ")
    if (length(len) == 0)
      stop("Param file error: 'popLength' not especified. ")
    if (length(cross) == 0)
      stop("Param file error: 'crossProb' not especified. ")
    if (length(mut) == 0)
      stop("Param file error: 'mutProb' not especified. ")
    if (length(rep) == 0)
      stop("Param file error: 'RulesRep' not especified. ")
  }
  
  if (length(tC) == 0)
    stop("Param file error: 'targetClass' not especified. ")

  if (!any(algorithm == c("SDIGA", "MESDIF", "NMEEFSD", "FUGEPSD")))
    stop("Param file error: 'Algorithm' must be \"SDIGA\", \"MESDIF\", \"NMEEFSD\" or \"FUGEPSD\"  ")
  
  # Now, parse the parameters
  # Get general parameters
  # input data
  input_data <- character(2) # Two inputs, training and test
  
  input_string <-
    gsub(
      pattern = '\"', replacement = "", x = data[[iData]][2], fixed = TRUE
    )
  input_data <-
    strsplit(x = input_string, split = " ", fixed = TRUE)[[1]]
  
  #Parse outputs
  output_data <- character(4) # Rules, tra_qua, tra_seg y tst_quac
  
  input_string <-
    gsub(
      pattern = '\"', replacement = "", x = data[[oData]][2], fixed = TRUE
    )
  output_data <-
    strsplit(x = input_string, split = " ", fixed = TRUE)[[1]]
  
  # 'semilla' is 'seed' in spanish
  semilla <- as.integer(data[[seed]][2])
  
  if(length(input_data) > 2){ #If the are more than 2 input files we warn the user.
    warning("More than two input files have been specified. Only the first two will be used !")
  }
  
  # Parse parameters that are specific for SDIGA, MESDIF or NMEEFSD
  if(any(algorithm == c("SDIGA", "MESDIF", "NMEEFSD"))){
    n_intervals <- as.integer (data[[labels]][2])
    n_evals <- as.integer (data[[evals]][2])
    popLenght <- as.integer(data[[len]][2])
    crossProb <- as.double(data[[cross]][2])
    prob_mutacion <- as.double(data[[mut]][2])
    rule_type <- data[[rep]][2]
  }
  
  # Catch target variable and value, this is represented in the file as 'Variable -> value'
  target <- strsplit(data[[tC]][2], "[[:blank:]]*->[[:blank:]]*")[[1]]
  if(length(target) < 2){
    target <- c(NA, target)
  }
  
  #SDIGA own parameters
  if (algorithm == "SDIGA") {
    if (length(miConf) == 0)
      stop("Param file error: 'minConf' not specified.")
    if (length(ob1) == 0)
      stop("Param file error: 'Obj1' not specified.")
    if (length(ob2) == 0)
      stop("Param file error: 'Obj2' not specified.")
    if (length(ob3) == 0)
      stop(
        "Param file error: 'Obj3' not specified (If you dont want specify, you must write null)."
      )
    if (length(w1) == 0)
      stop("Param file error: 'w1' not specified.")
    if (length(w2) == 0)
      stop("Param file error: 'w2' not specified.")
    if (length(w3) == 0)
      stop("Param file error: 'w3' not specified.")
    if (length(search) == 0)
      stop("Param file error: 'localSearch' not specified.")
    
    minimun_confidence <- as.double(data[[miConf]][2])
    Obj1 <- data[[ob1]][2]
    Obj2 <- data[[ob2]][2]
    Obj3 <- data[[ob3]][2]
    peso1 <- as.double(data[[w1]][2])
    peso2 <- as.double(data[[w2]][2])
    peso3 <- as.double(data[[w3]][2])
    local_search <- data[[search]][2]
    
    # Make the list with the paremeters
    lista <-
      list(
        algorithm = algorithm, inputData = input_data, outputData = output_data, seed = semilla, nLabels = n_intervals, nEval = n_evals, popLength = popLenght, crossProb = crossProb, mutProb = prob_mutacion, minConf = minimun_confidence, RulesRep = rule_type, Obj1 = Obj1, Obj2 = Obj2, Obj3 = Obj3, w1 = peso1, w2 = peso2, w3 = peso3, lSearch = local_search, targetClass = target[2], targetVariable = target[1]
      )
    
  }
  
  #MESDIF own parameters
  if (algorithm == "MESDIF") {
    if (length(elit) == 0)
      stop("Param file error: 'elitePop' not specified.")
    if (length(ech) == 0)
      stop("Param file error: 'echo' not specified.")
    if (length(ob1) == 0)
      stop("Param file error: 'Obj1' not specified.")
    if (length(ob2) == 0)
      stop("Param file error: 'Obj2' not specified.")
    if (length(ob3) == 0)
      stop(
        "Param file error: 'Obj3' not specified (If you dont want specify, you must write null)."
      )
    if (length(ob4) == 0)
      stop(
        "Param file error: 'Obj4' not specified (If you dont want specify, you must write null)."
      )
    
    
    elite <- as.numeric(data[[elit]][2])
    echo <- data[[ech]][2]
    Obj1 <- data[[ob1]][2]
    Obj2 <- data[[ob2]][2]
    Obj3 <- data[[ob3]][2]
    Obj4 <- data[[ob4]][2]
    
    # Make the list with the paremeters
    lista <-
      list(
        algorithm = algorithm, inputData = input_data, outputData = output_data, seed = semilla, nLabels = n_intervals, nEval = n_evals, popLength = popLenght, crossProb = crossProb, mutProb = prob_mutacion, RulesRep = rule_type, targetClass = target[2], elitePop = elite, echo = echo, Obj1 = Obj1, Obj2 = Obj2, Obj3 = Obj3, Obj4 = Obj4, targetVariable = target[1]
      )
    
  }
  
  #NMEEF-SD own parameters
  if (algorithm == "NMEEFSD") {
    #if(length(div) == 0) stop("Param file error: 'diversity' not specified.")
    if (length(rInit) == 0)
      stop("Param file error: 'ReInitCob' not specified.")
    if (length(pCob) == 0)
      stop("Param file error: 'porcCob' not specified.")
    if (length(dom) == 0)
      stop("Param file error: 'StrictDominance' not specified.")
    if (length(miCf) == 0)
      stop("Param file error: 'minCnf' not specified.")
    
    diversity <- data[[div]][2]
    reInit <- data[[rInit]][2]
    porcCob <- data[[pCob]][2]
    dominance <- data[[dom]][2]
    minConf <- data[[miCf]][2]
    Obj1 <- data[[ob1]][2]
    Obj2 <- data[[ob2]][2]
    Obj3 <- data[[ob3]][2]
    
    # Make the list with the paremeters
    lista <-
      list(
        algorithm = algorithm, inputData = input_data, outputData = output_data, seed = semilla, nLabels = n_intervals, nEval = n_evals, popLength = popLenght, crossProb = crossProb, mutProb = prob_mutacion, RulesRep = rule_type, targetClass = target[2], StrictDominance = dominance, diversity = diversity, porcCob = porcCob, reInitPob = reInit, minConf = minConf, Obj1 = Obj1, Obj2 = Obj2, Obj3 = Obj3, targetVariable = target[1]
      )
    
  }
  
  
  #FuGePSD Own Parameters
  if(algorithm == "FUGEPSD"){
    if(length(nLabels) == 0)
      stop("'Number of Labels' not specified.")
    if(length(tnorm) == 0)
      stop("'T-norm/T-conorm for the Computation of the Compatibility Degree' not specified")
    if(length(ruleWeight) == 0)
      stop("'Rule Weight' not specified.")
    if(length(frm) == 0)
      stop("'Fuzzy Reasoning Method' not specified.")
    if(length(numGens) == 0)
      stop("'Number of Generations' not specified.")
    if(length(tamPop) == 0)
      stop("'Initial Number of Fuzzy Rules (0 for 5*n_var)' not specified.")
    if(length(crossProb) == 0)
      stop("'Crossover probability' not specified.")
    if(length(mutProb) == 0)
      stop("'Mutation probability' not specified.")
    if(length(insProb) == 0)
      stop("'Insertion probability' not specified.")
    if(length(dropProb) == 0)
      stop("'Dropping Condition probability' not specified.")
    if(length(tsSize) == 0)
      stop("'Tournament Selection Size' not specified.")
    if(length(gfw1) == 0)
      stop("'Global Fitness Weight 1' not specified.")
    if(length(gfw2) == 0)
      stop("'Global Fitness Weight 2' not specified.")
    if(length(gfw3) == 0)
      stop("'Global Fitness Weight 3' not specified.")
    if(length(gfw4) == 0)
      stop("'Global Fitness Weight 4' not specified.")
    if(length(allClass) == 0)
      stop("'All Class' not specified.")
  
    # Make the list with the paremeters
    lista <- list(algorithm = algorithm, 
                  inputData = input_data, 
                  outputData = output_data, 
                  seed = semilla, 
                  nLabels = as.integer(data[[nLabels]][2]), 
                  nGens = as.integer(data[[numGens]][2]), 
                  popLength = as.integer(data[[tamPop]][2]), 
                  crossProb = as.double(data[[crossProb]][2]), 
                  mutProb = as.double(data[[mutProb]][2]),
                  insPro = as.double(data[[insProb]][2]),
                  dropProb = as.double(data[[dropProb]][2]),
                  frm = tolower(data[[frm]][2]),
                  tnorm = tolower(data[[tnorm]][2]),
                  ruleWeight = tolower(data[[ruleWeight]][2]),
                  tournamentSize = as.integer(data[[tsSize]][2]),
                  allClass = data[[allClass]][2],
                  alphaFitness = 0,
                  executionType = 1,
                  gfw1 = as.double(data[[gfw1]][2]),
                  gfw2 = as.double(data[[gfw2]][2]),
                  gfw3 = as.double(data[[gfw3]][2]),
                  gfw4 = as.double(data[[gfw4]][2]),
                  targetVariable = target[1]
      )
    
  }
  
  lista
  
}



#' @title Shows in console information about the parameters used in an execution of an SDEFSR algorithm
#' 
#' @param params A list with the parameters used
#' @param train A \code{SDEFSR_Dataset} object used as training
#' @param test A \code{SDEFSR_Dataset} object used as test
#' @noRd
.show_parameters <- function(params, train, test) {
  #Show parameters in the console
  algo <- params$algorithm
  message(
    "--------------------------------", appendLF = T
  )
  message(paste("Algorithm:", algo), appendLF = T)
  message(
    paste("Relation:", train$relation), appendLF = T
  )
  message(
    paste("Training dataset:", if("inputData" %in% names(params)) params$inputData[1] else substitute(train)),appendLF = T
    )
  message(
    paste("Test dataset:", if("inputData" %in% names(params)) params$inputData[2] else substitute(test)), appendLF = T
  )
  message(
    paste("Rules Representation: ", if (tolower(params$RulesRep) == "can")
      "CAN"
    else
      "DNF"), appendLF = T
  )
  message(
    paste("Number of evaluations:", params$nEval), appendLF = T
  )
  message(
    paste("Number of fuzzy partitions:", params$nLabels), appendLF = T
  )
  message(
    paste("Population Length:", params$popLength), appendLF = T
  )
  if (algo == "MESDIF")  
    message(
      paste("Elite Population Length:", params$elitePop), appendLF = T
    )
  if (algo != "SDIGA")
    message(
      paste("Crossover Probability:", params$crossProb), appendLF = T
    )
  message(
    paste("Mutation Probability:", params$mutProb), appendLF = T
  )
  message(
    paste("Obj1:", params$Obj1, "  (Weigth:", params$w1,")"), appendLF = T
  )
  message(
   paste("Obj2:", params$Obj2, "  (Weigth:", params$w2,")"), appendLF = T
  )
  message(
    paste("Obj3:", params$Obj3, "  (Weigth:", params$w3,")"), appendLF = T
  )
  if (algo == "MESDIF")
    message(paste("Obj4:", params$Obj4),appendLF = T)
  if (algo == "SDIGA")
    message(
     paste("Local Search optimization?:", params$lSearch),appendLF = T
    )
  if (algo == "NMEEFSD") {
    message(
      paste("Reinitilization based on coverage?: ", params$reInitPob), appendLF = T
      )
    message(
      paste("Max Pct of variables in reinitialization: ", as.numeric(params$porcCob) * 100, "%"),appendLF = T
    )
  }
  message(
   paste("Number of examples in training:", train$Ns), appendLF = T
  )
  message(
   paste("Number of examples in test:", test$Ns), appendLF = T
  )
  
  message(paste("Target Variable:", params$targetVariable), appendLF = T)
  message(paste("Target Class:", if(params$targetClass == "null") "All Variables" else params$targetClass), appendLF = T)
  message(
    "--------------------------------", appendLF = T
    )
  
  
  #Save parameters in the outputFile
  algo <- params$algorithm
  if(!is.na(params$outputData[1])){
  cat(
    "--------------------------------" , "\n",
    "Algorithm:",algo ,"\n",
    "Relation:", train$relation, "\n",
    "Training file:", params$inputData[1], "\n",
    "Test file:", params$inputData[2], "\n",
    "Rules Representation: ", if (tolower(params$RulesRep) == "can")
      "CAN"
    else
      "DNF", "\n",
    "Number of evaluations:", params$nEval,"\n",
    "Number of fuzzy partitions:", params$nLabels, "\n",
    "Population Length:", params$popLength, "\n",
    if (algo == "MESDIF")
      paste("Elite Population Length:", params$elitePop, "\n"),
    if (algo != "SDIGA")
      paste("Crossover Probability:", params$crossProb, "\n"),
    "Mutation Probability:", params$mutProb, "\n",
    paste("Obj1:", params$Obj1, "  (Weigth:", params$w1,")", "\n"),
    paste("Obj2:", params$Obj2, "  (Weigth:", params$w2,")", "\n"),
    paste("Obj3:", params$Obj3, "  (Weigth:", params$w3,")", "\n"),
    if (algo == "MESDIF")
      paste("Obj4:", params$Obj4, "\n"),
    if (algo == "SDIGA")
      paste("Local Search optimization?:", params$lSearch, "\n"),
    if (algo == "NMEEFSD") {
      paste(
        "Reinitilization based on coverage?: ", params$reInitPob, "\n",
        "Max Pct of variables in reinitialization: ", as.numeric(params$porcCob) * 100, "%", "\n",
        "Compare individuals using strict dominance? ", params$StrictDominance, "\n"
      )
    },
    "Number of examples in training:", train$Ns, "\n",
    "Number of examples in test:", test$Ns, "\n",
    "Target Variable:", params$targetVariable, "\n",
    "Target Class:", if(params$targetClass == "null") "All Variables" else params$targetClass, "\n",
    "--------------------------------", file = params$outputData[1], sep = " "
  )
  }
  
}


#'
#' @name .giveMeSets
#' @description Return the number of sets (fuzzy or not) depending on the type of the variable
#' If there is a real variable the result is the number of fuzzy sets.
#' If the variable is categorical, it return the number of categories.
#' 
#' @return A vector with the specified information
#' @noRd
.giveMeSets <- function(data_types, max, n_labels) {
  data_types <- data_types[-length(data_types)] # The last is the class, we don't use it
  # Split into categorical attributes and real ones. They have a different processing
  returnSets <- numeric(length(data_types))
  cat <- which(data_types == 'c')
  if (length(cat > 0)) {

    returnSets[cat] <- max[cat] #Max is the number of categories for categorical variables
    returnSets[-cat] <- n_labels
  } else {
    #If there are no categorical data, all has the 'n_labels' value
    returnSets[] <- n_labels
  }
  returnSets
  
}



#
#
# This function prints a rule for show to the user
#
#
.print.rule <-
  function(rule, max, names, consecuent, types, fuzzySets, categoricalValues, DNFRules = FALSE, rulesFile = "rulesFile.txt") {
    
    #Print Canonical rule
    if (!DNFRules) {
      participants <- which(rule < max) #Take variables that participate in the rule, if rule == max, variable doesn't participate
      name <- names[participants]
      values <- rule[participants]
      types <- types[participants]
      fuzzy <- fuzzySets[,,participants, drop = F]
      cate <- categoricalValues[participants]
      
      val <- replicate(n = length(participants), expr = NA)
      
      # Get the value of a variable that participate in a rule
      # If is categorical, the value is the name of the category
      # If it is real, print the fuzzy definition as 'Label n-label (minValue, mediumValue, maxValue)'
      for (p in seq_len(length(participants))) {
        if (types[p] == 'c') {
          val[p] <- cate[[p]][values[p] + 1]
        } else {
          val[p] <-
            paste("Label", values[p], "(", fuzzy[values[p] + 1,1,p], ",", fuzzy[values[p] +
                                                                                    1,2,p], ",",fuzzy[values[p] + 1,3,p], ")", sep = " ")
        }
      }
      name <- paste("Variable", name, sep = " ")
      lines <- paste(name, val, sep = " = ")
      lines <- paste(lines, collapse = "\n")
      message(
        paste(lines, "\n","THEN", consecuent), appendLF = T
        )
      
      #Save in file
      if(!is.na(rulesFile)){
      cat(lines, "\n","THEN", consecuent, file = rulesFile, sep = " ", fill = TRUE, append = TRUE)
      }
      
    } else {
      #Print DNF rule
      
      #Get the position of the last value that belongs to a variable
      max <- Reduce(f = '+', x = max, accumulate = TRUE)
      
      before <- 1
      pos <- 1
      lines <- ""
      # Check if a variable participate in a rule
      # A variable does not participate in a DNF rule if all his values are 0 or 1
      for (i in max) {
        variable <- rule[before:i]
        notParticipate <- all(variable == 1) | all(variable == 0)
        if (!notParticipate) {
          values <- which(variable == 1)  # A variable with '1' indicate that this value participate in the rule
          variableName <- names[pos]
          if (types[pos] == 'c') {
            valueNames <- categoricalValues[[pos]][values]
            valueNames <-
              paste(valueNames, sep = " ", collapse = " OR ")
          } else {
            valueNames <-
              paste(
                "Label", values - 1, "(", fuzzySets[values,1,pos], ",", fuzzySets[values,2,pos], ",",fuzzySets[values,3,pos], ")", sep = " ", collapse = " OR "
              )
          }
          lines <-
            paste(lines, "Variable", variableName, valueNames, "\n", sep = " ")
          
          
        }
        pos <- pos + 1
        before <- i + 1
      }
      message(
        paste(lines, "\n","THEN", consecuent), appendLF = T
      )
      #Save in file
      if(!is.na(rulesFile)){
        cat(lines, "\n","THEN", consecuent, file = rulesFile, sep = " ", fill = TRUE, append = TRUE)
      }
      
    }
  }




#
#
# Function to get the name of the objective value in the parameters file
# and return the corresponding functions.
#
#
.parseObjectives <- function(parameters, algorithm, DNF) {
  Objectives <- list(NA,NA,NA,NA) # Preallocate memory
  
  if (algorithm == "SDIGA") {
    if (parameters$Obj1 == "CSUP") {
      Objectives[[1]] <- .LocalSupport
      Objectives[[4]] <- FALSE
    } else{
      Objectives[[1]] <- .FLocalSupport
      Objectives[[4]] <- TRUE
    }
    
    if (parameters$Obj2 == "CCNF") {
      Objectives[[2]] <- .confidence
    } else{
      Objectives[[2]] <- .fuzzyConfidence
    }
    
    if (parameters$Obj3 == "UNUS") {
      Objectives[[3]] <- .norm_unusualness
    } else if (parameters$Obj3 == "SIGN") {
      Objectives[[3]] <- .significance
    } else if (parameters$Obj3 == "COVE") {
      Objectives[[3]] <- .coverage
    }
    
  } else {
    valores <- c(parameters$Obj1, parameters$Obj2, parameters$Obj3)
    
    for (i in seq_len(3)) {
      if (valores[i] == "CSUP")
        Objectives[[i]] <- .Csupport
      if (valores[i] == "FSUP")
        Objectives[[i]] <- .Fsupport
      if (valores[i] == "CCNF")
        Objectives[[i]] <- .confidence
      if (valores[i] == "FCNF")
        Objectives[[i]] <- .fuzzyConfidence
      if (valores[i] == "UNUS")
        Objectives[[i]] <- .norm_unusualness
      if (valores[i] == "SIGN")
        Objectives[[i]] <- .significance
      if (valores[i] == "COVE")
        Objectives[[i]] <- .coverage
      
    }
    
    Objectives[[4]] <- DNF
  }
  return(Objectives)
}




#
#
# Preprocessing of a single line in the header of the KEEL file.
#
#
.preprocessHeader <- function(line) {
  #The regular expression eliminate the "{}" , "[]", symbols and commas that separate every element inside them
  regex <-  "[[:blank:]]*\\{|[[:blank:]]*\\}|[[:blank:]]*\\[|[[:blank:]]*\\]|,[[:blank:]]*" 
  line <- gsub(pattern = regex, replacement = " ", x = line)
  
  #Return
  strsplit(line, " ", fixed = TRUE)[[1]]
  
}



#
#
# This function parses all examples in the @data field
#
#
.processData <- function(data, categoricalValues, types, fromDataFrame = FALSE) {
  line <- as.character(data)
  if(!fromDataFrame){
    # Split as a comma-separated values
    line <- gsub(pattern = ",[[:blank:]]*", replacement = " ", x = line) 
    line <- strsplit(x = line, split = " ",fixed = TRUE)[[1]]
  }
  cat <- which(types == 'c') #Determine which variables are categorical
  lc <- line[cat]
  cv <- categoricalValues[cat]  #Get the values of the categoricals variables
  
  #Code the categorical values of each variable as the position of this value in categoricalValues minus 1.
  for (i in seq_len(length(lc))) {
    pos <- which(cv[[i]] == lc[i])
    if (length(pos) > 0) {
      lc[i] <- pos - 1
    } else{
      #LostData
      lc[i] <- length(cv[[i]]) + 1
    }
  }
  line[cat] <- lc
  
  #Return
  as.numeric(line)
}


#
# Return a list with the following values:
# - Attribute name
# - Type
# - Minimum
# - Maximum
# - Categorical values if it has, NA if not.
.processLine <- function(line) {
  returnList <- vector(mode = "list", length = 5)
  returnList[[1]] <- line[2] # Attribute name
  
  if (line[3] != "real" & line[3] != "integer") {
    # Categorical value
    returnList[[2]] <- 'c' # Attribute type
    returnList[[3]] <- 0   #Minimum
    returnList[[4]] <-
      length(line) - 2 #Maximun number of categorical values
    returnList[[5]] <- line[3:length(line)]
    
  } else {
    #Numerical Values
    returnList[[2]] <- if (line[3] == "integer")
      'e'
    else
      'r'
    returnList[[3]] <- as.numeric(line[4])
    returnList[[4]] <- as.numeric(line[5])
    returnList[[5]] <- NA
  }
  
  returnList
}




#
#
# This function reads an entire file in a block and it is splitted by the \n or \r character.
# It is 8X faster than using scan()
#
# Thanks to F. Charte! 
#
.readFile <- function(file) {
  con <- file(file, "rb")
  if(!isOpen(con))
    open(con, "rb")
  
  contents <- readChar(con, file.info(file)$size, useBytes = TRUE)
  close(con)
  
  #Return
  strsplit(x = contents, split = "\\\r\n|\\\r|\\\n", fixed = FALSE, useBytes = TRUE)[[1]]
 
}


#' Saves a \code{SDEFSR_Dataset} dataset into a KEEL dataset format file.
#'
#' This function exports a SDEFSR_Dataset dataset stored in the R environment into a KEEL format file on the hard disk.
#' This function can not save information about the fuzzy
#' definition created by the function \link{read.dataset} because the SDEFSR_Dataset format does not
#' define that kind of information.
#'
#' @param dataset The \code{SDEFSR_Dataset} object stored in R environment.
#' @param file The file name (or path) to save the KEEL dataset.
#'
#' @details  A KEEL data file must have the following structure:
#'  \itemize{
#'    \item{ @@relation: Name of the data set }
#'    \item{ @@attribute: Description of an attribute (one for each attribute)}
#'    \item{ @@inputs: List with the names of the input attributes }
#'    \item{ @@output: Name of the output attribute (Not used in this algorithms implementation) }
#'    \item{ @@data: Starting tag of the data}
#' }
#'    The rest of the file contains all the examples belonging to the data set, expressed in comma sepparated values format.
#'
#' @author Angel M. Garcia <amgv0009@@red.ujaen.es>
#'
#' @references J. Alcala-Fdez, A. Fernandez, J. Luengo, J. Derrac, S. Garcia, L. Sanchez, F. Herrera. KEEL Data-Mining Software Tool: Data Set Repository, Integration of Algorithms and Experimental Analysis Framework. Journal of Multiple-Valued Logic and Soft Computing 17:2-3 (2011) 255-287.
#' @seealso KEEL Dataset Repository (Standard Classification): \url{https://sci2s.ugr.es/keel/category.php?cat=clas}
#' 
#' @noRd
save.SDEFSR_Dataset <- function(dataset, file) {
  #First, we need to ask the user if he want to save the file
  
  if (is.null(file) | is.na(file) | missing(file)) {
    stop("Parameter 'file' can not be NULL or NA.")
  }
  
  if (length(file) > 1) {
    stop("'file' must be of length 1.")
  }
  
  if (class(dataset) != "SDEFSR_Dataset") {
    stop("'dataset' must be of class 'SDEFSR_Dataset'.")
  }
  
  answer <-
    .yesno("Do you really want to save this dataset? (y/n): ")
  
  if (answer == "y") {
    #Add .dat extension to the file.
    file <- paste(file, ".dat", sep = "")
    #Save file
    #get relation name
    message("Getting attributes...", appendLF = T)
    line <- paste("@relation", dataset[[1]])
    
    #get attributes
    aux <- character(length(dataset[[2]]))
    aux[which(dataset[[3]] == "e")] <- "integer"
    aux[which(dataset[[3]] == "r")] <- "real"
    pos <- which(dataset[[3]] == "c")
    aux_values <- character(length(dataset[[2]]))
    aux_values[pos] <- dataset$categoricalValues[pos]
    aux_values[-pos] <- # Numerical values are represented in a range like '[100, 105]'
      paste("[",dataset$min[-pos], ", ", dataset$max[-pos], "] ", sep = "")
    a <- lapply(aux_values, function(x)
      if (length(x) > 1) {
        aux <- paste(x , collapse = ", ")
        paste("{", aux, "}", sep = "")
      } else{
        x
      })
    a <- unlist(a)
    
    attributes <- paste("@attribute", dataset$attributeNames, aux, a)
    attributes <- paste(attributes, collapse = "\n")
    
    line <- paste(line, attributes, sep = "\n")
    
    #get inputs and outputs
    inputs <-
      paste(dataset[[2]][-length(dataset[[2]])], collapse = ", ")  # Inputs are are the variables except the class, the last one
    output <- dataset[[2]][length(dataset[[2]])] # Outputs is the last attribute, the class
    line <- paste(line, "\n", "@inputs ", inputs, "\n", sep = "")
    line <- paste(line, "@outputs ", output, "\n@data",  sep = "")
    message("Done\nGetting data (this may take some time)...", appendLF = T)
    
    #get data
    categ <- which(dataset[[3]] == "c")
    if (Sys.info()[1] != "Windows") {
      data <-
        parallel::mclapply(
          X = dataset$data, FUN = function(x, pos, catValues) {
            result <-
              lapply(
                X = seq_len(length(pos)), FUN = function(y, pos, data) {
                  data[pos[y]] <- catValues[[pos[y]]][data[pos[y]] + 1]   # Catch the categorical value of a categorical variable, in a SDEFSR_Dataset object, this values are coded into a number that specify the position of the value in the 'categoricalValues' variable
                  data[pos[y]]
                }, categ, x
              )
            x[pos] <- result
            unlist(x)
          }, categ, dataset$categoricalValues, mc.cores = parallel::detectCores() - 1
        )
    } else {
      data <- lapply(
        X = dataset$data, FUN = function(x, pos, catValues) {
          result <-
            lapply(
              X = seq_len(length(pos)), FUN = function(y, pos, data) {
                data[pos[y]] <- catValues[[pos[y]]][data[pos[y]] + 1]
                data[pos[y]]
              }, categ, x
            )
          x[pos] <- result
          unlist(x)
        }, categ, dataset$categoricalValues
      )
    }
    
    
    #Paste data into the line (put each line as a comma-separated string)
    data <- lapply(data, function(x) {
      paste(x, collapse = ", ")
    })
    
    data <- unlist(data)
    line <- paste(line, paste(data, collapse = "\n"), sep = "\n")
    message("Done\n")
    
    #Save file
    cat(line, file = file,  sep = "", append = FALSE)
    message("File succesfully saved.")
  } else {
    warning("File not saved.")
  }
}


#'
#' Add one or a set of instances to a SDEFSR_Dataset dataset.
#'
#' Take a data vector or a list of data vectors and inserts at the end of a \code{SDEFSR_Dataset} data set.
#' 
#' @param items Vector or list of instance/s
#' @param dataset The \code{SDEFSR_Dataset} dataset to insert the data.
#' 
#' @details You can add the data in four ways:
#' \itemize{
#'  \item A single element, using a vector.
#'    \itemize{
#'      \item Coded.
#'      \item Uncoded.
#'    }
#'  \item More than one element, using a list of vectors.
#'   \itemize{
#'      \item Coded.
#'      \item Uncoded.
#'    }
#' }
#' 
#' Coded means that vectors of data are all numeric (including class) and, obviously, 
#' all values are within the bounds stablished. This way is the returned after a \code{read.dataset()} call
#' and it is the ideal for introduce data from one dataset to another, for example.
#' 
#'  Uncoded means that vectors of data are characters, because it has at least one value that is a string (class value) and values are valid. This is common when we read
#'  a csv file or we introduce data manually, for example.   
#' 
#' @return Returns the new dataset with data introduced. This dataset is a list with a vectors of every instace.  
#' This dataset should be stored into the \code{$data}
#' field of a \code{SDEFSR_Dataset} class variable.
#' 
#' @author Angel M. Garcia <amgv0009@@red.ujaen.es>
#'
#' @references J. Alcala-Fdez, A. Fernandez, J. Luengo, J. Derrac, S. Garcia, L. Sanchez, F. Herrera. KEEL Data-Mining Software Tool: Data Set Repository, Integration of Algorithms and Experimental Analysis Framework. Journal of Multiple-Valued Logic and Soft Computing 17:2-3 (2011) 255-287.
#' @seealso KEEL Dataset Repository (Standard Classification): \url{https://sci2s.ugr.es/keel/category.php?cat=clas}
#'
#' @noRd
addSDEFSR_DatasetRegister <- function(items, dataset) {
  if (class(dataset) != "SDEFSR_Dataset") {
    stop("Provided dataset is not of class 'SDEFSR_Dataset'.")
  }
  
  
  # If items is not a list, is a single element.
  if (class(items) != "list") {
    if (.checkElement(item = items, dataset = dataset)) { # If the element is valid
      #We use this because it is only a single element !
      if (class(items) == "numeric") { #Coded data
        dataset$data[[length(dataset$data) + 1]] <- items
        dataset$data
      } else { #Uncoded date, we need to process the line to code the data.
        items <- paste(items, collapse = ", ")
        items <- .processData(items, dataset$categoricalValues, dataset$attributeTypes)
        
        dataset$data[[length(dataset$data) + 1]] <- items
        dataset$data
      }
    } else {
      stop("Adding an invalid element into the dataset.")
    }
    
  } else {
    #If it is a list, there are more than one item, add it efficiently.
    resultDataset <- vector(mode = "list", length = length(dataset$data) + length(items))
    
    #Copy old data to the new list
    resultDataset[seq_len(length(dataset$data))] <- dataset$data
    
    #Check if all new data are correct.
    allElements <- unlist(lapply(X = items, FUN = .checkElement, dataset))
    
    if (all(allElements)) {
      if (class(items[[1]]) == "character") {
        #Process the elements, tranform every vector into a string line to use the .processData() function.
        items <- lapply(items, paste, collapse = ", ")
        items <- lapply(items, .processData, dataset$categoricalValues, dataset$attributeTypes)
      }
      #Introduce the elements at the end of the $data atribute of the dataset.
      resultDataset[(length(dataset$data) + 1):length(resultDataset)] <-
        items
      
      #Return
      resultDataset
    } else {
      stop("One or more new items are invalid. No items added.")
    }
  }
  
}






#Checks if a single instance has correct data.
.checkElement <- function(item, dataset) {
  #Check lengths
  if (length(item) != length(dataset$max))
    return(FALSE)
  
  if (class(item) == "numeric") {
    # If all item elements are numeric it is because:
    # 1.- all his attributes are numeric
    # 2.- categorical values are coded into a numeric number, this is how read.dataset() do the reading of data.
    all(item < dataset$max)
    
  } else {
    # If not, we need to check every categorical value if they have valid values. This is slower than the former option.
    catData <- which(dataset$attributeTypes == "c")
    numData <-
      which(dataset$attributeTypes == "r" | dataset$attributeTypes == "e")
    
    cvalues <- dataset$categoricalValues[catData]
    cItem <- item[catData]
    
    #Check if categorical values of an item have valid values.
    values <- lapply(
      X = seq_len(length(cItem)),
      FUN = function(x, lista, items) {
        any(lista[[x]] == items[x])
      }, cvalues, cItem
    )
    values <- unlist(values)
    
    # If all values are valid, continue the process
    if (!all(values)) {
      return (FALSE)
    }
    
    # Check numerical values
    min <- dataset$min[numData]
    max <- dataset$max[numData]
    nItem <- as.numeric(item[numData])
    
    # If not all the elements are within the bounds, throw false.
    if (!all(nItem >= min & nItem <= max)) {
      return (FALSE)
    }
    
    #Return, the element is ok.
    TRUE
    
  }
  
  

  
}


#' 
#' Reads an ARFF file
#' 
#' This function reads an ARFF file and get the subyacent \code{SDEFSR_Dataset} object
#'
#' @param file The ARFF file to read.
#' 
#' 
#' 
#' @return a 'SDEFSR_Dataset' object ready to use with the algorithms that are in the package
#' @noRd
#' 
readFromArff <- function(file){
  # Initial checks
  #if(!require(foreign)) stop("'foreign' package not found.")
  warnPrevio <- getOption("warn")
  options(warn = -1)
  
  # Read arff data using foreign (returns a data.frame)
  data <- foreign::read.arff(file)
  
  # Return the dataset.
  readFromDataFrame(data)
}

#' 
#' Reads an ARFF file
#' 
#' This function reads an ARFF file and get the subyacent \code{SDEFSR_Dataset} object
#'
#' @param file The ARFF file to read.
#' 
#' 
#' 
#' @return a 'SDEFSR_Dataset' object ready to use with the algorithms that are in the package
#' @references  The read_arff function code has been taken from package \code{mldr} from F. Charte and D. Charte:
#' F.Charte and D.Charte: Working with Multilabel Datasets in {R}: The mldr Package, The R Journal, vol. 7, num. 2, pp. 149--162. (2015)
#' @noRd
#' 
SDEFSR_DatasetFromARFF <- function(file){
 
  warnPrevio <- getOption("warn")
  options(warn = -1)
  set <- read_arff(file)
  
  relation <- strsplit(set[[1]], " ")[[1]][2]
  
  #Make attribute names and types
  attributeNames <- names(set[[2]])
  categoricalVariables <- regmatches(x = set[[2]], gregexpr(pattern = "[/^{*}/$]", text = set[[2]])) #Check which variable is categoric
  set[[2]] <- gsub(pattern = "[/^{*}/$]", replacement = "", x = set[[2]])
  categoricalVariables <- unlist(lapply(categoricalVariables, function(x){length(x) > 0}))
  
  #Put the types of the files 'r' for real and 'c' for categorical
  types <- character(length(categoricalVariables)) 
  types[] <- "r" 
  types[which(categoricalVariables)] <- "c"
  
  if(types[length(types)] != "c")
    stop("Last value of the dataset is not categorical. We cannot create a dataset which class is not categorical.")
  
  # Get min, max 
  set[[2]] <- sub("^\\s+|\\s+$", "", set[[2]]) #Erase initial and final whitespaces
  #Parse the categorical variables with this regular expression.
  categoricalLength <- regmatches(x = set[[2]], gregexpr(pattern = "(?<=')([^,]*?)(?=')|(^|(?<=,)|(?<=, ))([^,' ]*?)((?=,)|$)", perl = T, text = set[[2]]))
  len <- unlist(lapply(categoricalLength, length))
  values <- matrix(data = unlist(lapply(set[[3]][,which(!categoricalVariables)], 
                                        function(x){
                                          x <- as.numeric(x)
                                          c(min(na.exclude(x)), max(na.exclude(x)))
                                          })),
                   ncol = 2, byrow = TRUE)
  min <- max <- numeric(length(categoricalVariables))
  min[which(!categoricalVariables)] <- values[,1]
  max[which(!categoricalVariables)] <- values[,2]
  max[which(categoricalVariables)] <- len[which(categoricalVariables)]
 
  nVars <- NCOL(set[[3]]) - 1
  
  class_names <- categoricalLength[[length(categoricalLength)]]
  
  #Examples per class
  examplesClass <- unlist(lapply(categoricalLength[[length(categoricalLength)]], function(x, values){
    sum(x == values)
  }, set[[3]][, NCOL(set[[3]])]))
  names(examplesClass) <- class_names
  
  #Ns
  Ns <- NROW(set[[3]])
  #Lost Data
  lostData <- FALSE
  #Covered
  covered <- logical(Ns)
  #Categorical Values
  categoricalLength[which(len <= 1)] <- NA
  
  #Fuzzy and crisp sets
  fuzzySets <- NA
  crispSets <- NA
  
  #Conjuntos
  sets <- NA
  
  
  # Remove inital and final ' signs for categorical variables with spaces or special on the data section
  set[[3]] <- sapply(set[[3]], function(x) gsub("^'+|'+$", "", x))
  #DATA
  df_aux <- as.data.frame(t(set[[3]]), stringsAsFactors = FALSE)
  # The algorithm fails here, values in dataset has '' when in categoricalLength it has not ''.
  if (Sys.info()[1] != "Windows")
    data <-
    parallel::mclapply(
      X = df_aux, FUN = .processData, categoricalLength, types, TRUE, mc.cores = parallel::detectCores() - 1
    )
  else
    #In windows mclapply doesnt work
    data <-
    parallel::mclapply(
      X = as.data.frame(t(set[[3]])), FUN = .processData, categoricalLength, types, TRUE, mc.cores = 1
    )
  
  lista <- list(
    relation = relation,
    attributeNames = attributeNames,
    attributeTypes = types,
    min = min,
    max = max,
    nVars = nVars,
    data = data,
    class_names = class_names,
    examplesPerClass = examplesClass,
    lostData = lostData,
    covered = covered,
    fuzzySets = fuzzySets,
    crispSets = crispSets,
    sets = sets,
    categoricalValues = categoricalLength,
    Ns = Ns
  )
  class(lista) <- "SDEFSR_Dataset"
  
  #Restore option warn of the user.
  options(warn = warnPrevio)
  lista
}




#'
#' Reads a CSV file and return a SDEFSR_Dataset object to be executed by an algorithm of the SDEFSR package
#' 
#' @param file The path to the csv file.
#' @param relation_name The name of the relation to use
#' @param sep Separator used to separate between values in the file, as it is csv, the default is ","
#' @param quote The character used to identify strings in the csv file
#' @param dec The character used to identify decimal values. By default it is "."
#' @param na.strings The character used to identify lost data. By default it is "?"
#' 
#' @return An \code{SDEFSR_Dataset} object that contains all neccesary information to execute the algorithms
#' 
#' @author Angel M. Garcia <agvico@ujaen.es>
#' @noRd
SDEFSR_DatasetFromCSV <- function(file, relation_name, sep = ",", quote = "\"", dec = ".", na.strings = "?"){
  #read the csv
  data <- read.csv(file = file, sep = sep, quote = quote, dec = dec, na.strings = na.strings)
  
  #get attribute names
  attNames <- colnames(data)
  
  #get attributes types:
  types <- sapply(data, function(x){
    if(class(x) %in% c("numeric", "integer", "double")){
      "r"
    } else {
      "c"
    }
  })
  
  #get mins and max values
  minMax <- sapply(data, function(x){
    if(class(x) %in% c("numeric", "integer", "double")){
      c(min(x, na.rm = T), max(x, na.rm = T))
    } else {
      c(0, length(levels(x)))
    }
  })
  
  min <- minMax[1,]
  max <- minMax[2,]
  
  #Get number of variables (except class)
  nvars <- ncol(data) - 1
  
  #get class names
  class_names <- levels(data[,ncol(data)])
  
  #get examples per class
  examplesClass <- as.list(table(data[,ncol(data)]))
  
  #get cataegorical values variable
  catValues <- lapply(data, function(x){
    if(class(x) == "numeric"){
      NA
    } else{
      levels(x)
    }
    })
  
  #Get number of examples
  numExamples <- nrow(data)
  
  #Now, parse the data (the most expensive part)
  #execute on parallel for better performance
  if (Sys.info()[1] != "Windows"){
    data <- parallel::mclapply(as.data.frame(t(data), stringsAsFactors = F), .processData, categoricalValues = catValues, types = types, fromDataFrame = TRUE, mc.cores = parallel::detectCores() - 1)
  } else {
    #In Windows mclapply does not work (look 'snow' package)
    data <- parallel::mclapply(as.data.frame(t(data), stringsAsFactors = F), .processData, categoricalValues = catValues, types = types, fromDataFrame = TRUE, mc.cores = 1)
  }
  
  #Generate the SDEFSR_Dataset Object
  dataset <- list(relation = relation_name,
                  attributeNames = attNames,
                  attributeTypes = types,
                  min = min,
                  max = max,
                  nVars = nvars,
                  data = data,
                  class_names = class_names,
                  examplesPerClass = examplesClass,
                  lostData = NA,
                  covered = logical(numExamples),
                  fuzzySets <- NA,
                  crispSets <- NA, 
                  sets <- NA,
                  categoricalValues = catValues,
                  Ns = numExamples)
  
  class(dataset) <- "SDEFSR_Dataset"
  
  #Return the object
  dataset
}


#'
#' Creates a \code{SDEFSR_Dataset} object from a \code{data.frame}
#' 
#' Creates a \code{SDEFSR_Dataset} object from a \code{data.frame} and create fuzzy labels for numerical variables too.
#' 
#' @param data A \code{data.frame} object with all neccesary information. See details.
#' @param relation A string that indicate the name of the relation.
#' @param names An optional character vector indicating the name of the attributes.
#' @param types An optional character vector indicating 'c' if variable is categorical, 'r' if is real and 'e' if it is an integer
#' @param classNames An optional character vector indicating the values of the target class.
#' 
#' @details The information of the data.frame must be stored with instances in rows and variables in columns
#' If you dont specify any of the optional parameter the function try to obtain them automatically. 
#' 
#' For \code{'names'} if it is NA, the function takes the name of the columns by \code{colnames}.
#' 
#' For \code{'types'} if it is NA, the function takes the type of an attribute asking the type of the column of the data.frame.
#' If it is \code{'character'} it is assumed that it is categorical, and if \code{'numeric'} it is assumed that it is a real number.
#' PLEASE, PAY ATTENTION TO THIS WAY OF WORK. It can cause tranformation errors taking a numeric variable as categorical or vice-versa.
#' 
#' For \code{'classNames'} if it is NA, the function returns unique values of the last attribute of the data.frame that is considered the class attribute.
#' 
#' @return A \code{SDEFSR_Dataset} object with all the information of the dataset.
#' 
#' @examples 
#' library(SDEFSR)
#' df <- data.frame(matrix(runif(1000), ncol = 10))
#' #Add class attribute
#' df[,11] <- c("0", "1", "2", "3")
#' SDEFSR_DatasetObject <- SDEFSR_DatasetFromDataFrame(df, "random")
#' invisible()
#' 
#' @seealso \code{\link{read.dataset}}
#' 
#' @author Angel M Garcia <amgv0009@@red.ujaen.es>
#' 
#' @export
SDEFSR_DatasetFromDataFrame <- function(data, relation, names = NA, types = NA, classNames = NA){
  #check data.frame
  if(! is.data.frame(data))
    stop(paste(substitute(data), "must be a data.frame"))
  
  #Create the data.frame without factors
  data <- as.data.frame(lapply(data, function(x) if(is.factor(x)) as.character(x) else x), stringsAsFactors = F)
  #Check if the last attribute (class attribute) is categorical
  if(! is.character(.subset2(data, NCOL(data))))
    stop("Last attribute of the dataset, that define the class attribute, must be categorical.")
  if(missing(relation))
    relation <- substitute(data)
  
  #Checks parameters
  checks <- is.na(c(names, types, classNames))
  
  if(checks[1]){
    #Get names from colnames
    names <- colnames(data)
  }
  
  if(checks[2]){
    #Try to get the type of the attributes.
    types <- vapply(data, function(x){
                            if(is.character(x) || is.factor(x)){
                              'c'
                            } else {
                              'r'
                            }
                              }, character(1))
  }
  
  if(checks[3]){
    classNames <- unique(.subset2(data, NCOL(data)))
  }
  
  #get Nvars and Ns
  nVars <- NCOL(data) - 1
  Ns <- NROW(data)
  
  #get min and max
  min <- max <- numeric(nVars + 1)
  matriz <- vapply(data, function(x){
    if(is.numeric(x)){
      c(min(na.exclude(x)), max(na.exclude(x)))
    } else {
      c(0, length(unique(x)))
    }
  }, numeric(2))
  
  matriz <- matrix(matriz, ncol = 2, byrow = TRUE)
  min <- matriz[,1]
  max <- matriz[,2]
  

  
  
  #Lost Data
  lostData <- FALSE
  #Covered
  covered <- logical(Ns)
  #Categorical Values
  categoricalValues <- lapply(data, function(x){ if(is.character(x)) unique(x) else NA})
  
  #Examples per class
  examplesClass <- unlist(lapply(categoricalValues[[length(categoricalValues)]], function(x, values){
    sum(x == values)
  }, .subset2(data, nVars + 1)))
  names(examplesClass) <- classNames
  
  #Fuzzy and crisp sets
  fuzzySets <- NA
  crispSets <- NA
  
  #Sets
  sets <- NA
  
  #DATA
  if(Ns > 150){
  if (Sys.info()[1] != "Windows")
    data <-
    parallel::mclapply(
      X = as.data.frame(t(data), stringsAsFactors = FALSE), FUN = .processData, categoricalValues, types, TRUE, mc.cores = parallel::detectCores() - 1
    )
  else
    #In windows mclapply doesnt work
    data <-
    parallel::mclapply(
      X = as.data.frame(t(data), stringsAsFactors = FALSE), FUN = .processData, categoricalValues, types, TRUE, mc.cores = 1
    )
  } else {
    data <-
      lapply(
        X = as.data.frame(t(data), stringsAsFactors = FALSE), FUN = .processData, categoricalValues, types, TRUE
      )
  }
  
  lista <- list(
    relation = relation,
    attributeNames = names,
    attributeTypes = types,
    min = min,
    max = max,
    nVars = nVars,
    data = data,
    class_names = classNames,
    examplesPerClass = examplesClass,
    lostData = lostData,
    covered = covered,
    fuzzySets = fuzzySets,
    crispSets = crispSets,
    sets = sets,
    categoricalValues = categoricalValues,
    Ns = Ns
  )
  class(lista) <- "SDEFSR_Dataset"
  lista
  
  
}


readFromDataFrame <- function(data){
  
  warnPrevio <- getOption("warn")
  options(warn = -1)
  
  #check data.frame
  if(! is.data.frame(data))
    stop(paste(substitute(data), "must be a data.frame"))
  
  # Check column format (only numeric, character or factor)
  if(! all(sapply(data, class) %in% c("character", "numeric", "factor"))){
    incorrect <- which(! (sapply(data, class) %in% c("character", "numeric", "factor")))
    stop(paste0("Column(s): ", incorrect, " have an incorrect format. Valid formats are numeric, character or factor"))
  }
  
  # Everything is fine, let's process data
  
  # Get attribute names and types
  attributeNames <- colnames(data)
  categoricalVariables <- sapply(data, class) != "numeric"
  
  # if the last variable (class) is not categorical
  if(! categoricalVariables[length(categoricalVariables)])
    stop("Last value of the dataset is not categorical. We cannot create a dataset whose class is not categorical.")
  
  # get Number of variables 
  nVars <- length(attributeNames) - 1
  
  # Get min and max 
  min <- sapply(data, function(x) if(class(x) == "numeric") min(x) else NA)
  max <- sapply(data, function(x) if(class(x) == "numeric") max(x) else NA)
  
  # Get categorical values for categorical variables
  categoricalValues <- sapply(data, levels)
  
  # Get the class values:
  classNames <- categoricalValues[[length(categoricalValues)]]
  
  # Get the number of examples for each class
  examplesPerClass <- unlist(lapply(classNames, function(x) sum(data[, ncol(data)] == x)))
  names(examplesPerClass) <- classNames
  
  # Create the dataset and return to the user.
  lista <- list(
    attributeNames = attributeNames,
    attributeTypes = categoricalVariables,
    min = min,
    max = max,
    nVars = nVars,
    data = data,
    class_names = classNames,
    examplesPerClass = examplesPerClass,
    fuzzySets = NA,
    categoricalValues = categoricalValues,
    Ns = nrow(data)
  )
  class(lista) <- "SDEFSR_Dataset"
  
  #Restore option warn of the user.
  options(warn = warnPrevio)
  lista
}

