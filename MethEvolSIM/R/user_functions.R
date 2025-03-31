#' Get Default Parameter Values
#'
#' This function retrieves parameter values for the DNA methylation simulation.
#'
#' @param rootData NULL to return default parameter values. For data parameter values, provide rootData as the output of simulate_initialData()$data.
#'
#' @return A data frame containing default parameter values.
#'
#' @details The function called without arguments returns default parameter values.
#' When rootData (as $data output of simulate_initialData()) is given, it returns data parameter values.
#'
#' @examples
#' # Get default parameter values
#' default_values <- get_parameterValues()
#'
#' # Get parameter values of simulate_initialData() output
#' custom_params <- get_parameterValues()
#' infoStr <- data.frame(n = c(5, 10), globalState = c("M", "U"))
#' rootData <- simulate_initialData(infoStr = infoStr, params = custom_params)$data
#' rootData_paramValues <- get_parameterValues(rootData = rootData)
#'
#' @export
get_parameterValues <- function(rootData = NULL){
  if(is.null(rootData)){
    obj <- singleStructureGenerator$new("U", 10)
    infoStr <- data.frame(n = c(13, 1, 5),
                          globalState = c("M", "M", "M"))
    combi_obj <- combiStructureGenerator$new(infoStr)
  } else {
    if(class(rootData)[1] != "combiStructureGenerator"){
      stop("'rootData' needs to be the $data output of simulate_initialData or simulate_evolData")
    } else {
      obj <- rootData$get_singleStr(1)
      combi_obj <- rootData
    }
  }
  data.frame(alpha_pI = obj$get_alpha_pI(),
             beta_pI = obj$get_beta_pI(),
             alpha_mI = obj$get_alpha_mI(),
             beta_mI = obj$get_beta_mI(),
             alpha_pNI = obj$get_alpha_pNI(),
             beta_pNI = obj$get_beta_pNI(),
             alpha_mNI = obj$get_alpha_mNI(),
             beta_mNI = obj$get_beta_mNI(),
             mu = combi_obj$get_mu(),
             alpha_Ri = obj$get_alpha_Ri(),
             iota = obj$get_iota(),
             Ri1 = obj$get_Ri_values()[1],
             Ri2 = obj$get_Ri_values()[2],
             Ri3 = obj$get_Ri_values()[3])
}



#' Simulate Initial Data
#'
#' This function simulates initial data based on the provided information and parameters.
#'
#' @param infoStr A data frame containing columns 'n' for the number of sites, and 'globalState' for the favoured global methylation state.
#'  If customized equilibrium frequencies are given, it also contains columns 'u_eqFreq', 'p_eqFreq' and 'm_eqFreq'
#'  with the equilibrium frequency values for unmethylated, partially methylated and methylated.
#' @param params Optional data frame with specific parameter values.
#' @param CFTP Default FALSE. TRUE for calling cftp algorithm to set root state according to model equilibrium (Note that current implementation neglects IWE process).
#' Structure as in get_parameterValues() output. If not provided, default values will be used.
#' @param CFTP_step_limit when CFTP = TRUE, maximum number of steps before applying an approximation method 
#'        (default 327680000 corresponding to size of CFTP info of approx 6.1 GB).
#'
#' @return A list containing the simulated data ($data) and parameters ($params).
#'
#' @details The function performs several checks on the input data and parameters
#'  to ensure they meet the required criteria and simulates DNA methylation data.
#'
#' @examples
#' # Example data
#' infoStr <- data.frame(n = c(10, 100, 10), globalState = c("M", "U", "M"))
#'
#' # Simulate initial data  with default parameters
#' simulate_initialData(infoStr = infoStr)
#'
#' # Simulate data evolution along a tree with custom parameters
#' custom_params <- get_parameterValues()
#' custom_params$iota <- 0.5
#' simulate_initialData(infoStr = infoStr, params = custom_params)
#'
#' @export
simulate_initialData <- function(infoStr, params = NULL, CFTP = FALSE, CFTP_step_limit = 327680000){
  
  # Control input
  if (!is.data.frame(infoStr) ||
      !all(c("n", "globalState") %in% colnames(infoStr))) {
    stop("infoStr should be a dataframe with columns: 'n', 'globalState'")
  }
  if(!is.numeric(infoStr$n)) {
    stop("column 'n' needs to be a vector of numeric CpG counts (integer values)")
  }
  if(!is.character(infoStr$globalState) || !all(infoStr$globalState %in% c("U","M"))){
    stop("column 'globalState' needs to be a character vector with values 'U' or 'M'")
  }
  if(all(c("u_eqFreq", "p_eqFreq", "m_eqFreq") %in% colnames(infoStr))){
    for (i in 1:nrow(infoStr)){
      eqFreqs <- c(infoStr$u_eqFreq[i], infoStr$p_eqFreq[i], infoStr$m_eqFreq[i])
      if(any(is.na(eqFreqs))){
        stop(paste("if 'u_eqFreq', 'p_eqFreq' and 'm_eqFreq' are given, they need to be frequency values. Missing values in row ", i))
      } else if(!is.numeric(eqFreqs) || !length(eqFreqs) == 3 || !sum(eqFreqs)==1){
        stop(paste("if 'u_eqFreq', 'p_eqFreq' and 'm_eqFreq' are given, they need to be frequency values (sum 1). Incorrect values in row ", i))
      }
    }
  }
  if(!is.null(params)){
    if(!is.data.frame(params) || !all(c("alpha_pI", "beta_pI", "alpha_mI", "beta_mI", "alpha_pNI", "beta_pNI", "alpha_mNI", "beta_mNI", "mu", "alpha_Ri", "iota") %in% colnames(params))){
      stop("if 'params' is given, it needs to be a dataframe with column names as in get_parameterValues() output")
    }
    message("Simulating initial data with customized parameter values")
  } else {
    message("Simulating initial data with default parameter values")
  }
  data <- combiStructureGenerator$new(infoStr = infoStr, params = params)
  if (CFTP){
    message("Calling CFTP algorithm.")
    data$cftp(step_limit = CFTP_step_limit)
  }
  if(is.null(params)){
    params <- get_parameterValues()
  }
  return(list(data = data,
              params = params))
}

#' Transform Methylation States to Methylation Frequencies
#'
#' This function transforms methylation states encoded as 1 (unmethylated), 2 (partially methylated),
#' and 3 (methylated) to their corresponding methylation frequency values: 0 (unmethylated),
#' 0.5 (partially methylated), and 1 (methylated).
#'
#' @param x A vector of integers representing methylation states.
#' @return A vector of values representing the methylation frequencies.
#' @details
#' The transformation is based on the following mapping:
#' \itemize{
#'   \item 1 (unmethylated) transforms to 0 (methylation frequency)
#'   \item 2 (partially methylated) transforms to 0.5 (methylation frequency)
#'   \item 3 (methylated) transforms to 1 (methylation frequency)
#' }
#' @noRd
transform_methStateEncoding <- function(x) {
  lookup_table <- c(0, 0.5, 1)
  return(lookup_table[x])
}

#' Extract simulation data from R6 object of class treeMultiRegionSimulator
#'
#' This function takes an R6 object containing simulation data and extracts various information including:
#'
#' - The name of the branch (NULL for the tree root and inner nodes, and the name of the tips for the tree tips)
#' - Information of IWE events on that branch (NULL for the tree root and the branches in which no IWE event was sampled,
#'   and a list containing $islands with the number of the island structure that went through the IWE event and $times
#'   for the branch time point in which the IWE was sampled)
#' - A list with the sequence of methylation states for each structure (the index of the list corresponds to the index
#'   of the structures). The methylation states are encoded as 0 for unmethylated, 0.5 for partially methylated, and 1 for methylated.
#' - A list with the methylation equilibrium frequencies for each structure (the index of the list corresponds to the index
#'   of the structures). Each structure has a vector with 3 values, the first one corresponding to the frequency of unmethylated,
#'   the second one to the frequency of partially methylated, and the third one to the frequency of methylated CpGs.
#'
#' The information is extracted for all of the branches in the tree used to simulate the data.
#'
#' @param R6obj An R6 object of class treeMultiRegionSimulator containing simulation data
#' @return A list containing information about tree architecture, sequence data, and equilibrium frequencies
#'
#' @noRd
extract_simD <- function(R6obj){
  simD <- vector("list", length(R6obj$Branch))
  # Add info about tree architecture (own, parent and offspring index)
  for (br in 1:length(R6obj$Branch)){
    simD[[br]] <- list(
      name = if (!is.null(R6obj$Branch[[br]]$get_name())) R6obj$Branch[[br]]$get_name() else NULL,
      IWE = if (!is.null(R6obj$Branch[[br]]$get_IWE_events())) R6obj$Branch[[br]]$get_IWE_events() else NULL,
      seq = vector("list", R6obj$Branch[[br]]$get_singleStr_number()),
      eqFreqs = vector("list", R6obj$Branch[[br]]$get_singleStr_number())
    )
    for(str in 1:R6obj$Branch[[br]]$get_singleStr_number()){
      simD[[br]]$seq[[str]] = transform_methStateEncoding(R6obj$Branch[[br]]$get_singleStr(str)$get_seq())
      simD[[br]]$eqFreqs[[str]] = R6obj$Branch[[br]]$get_singleStr(str)$get_eqFreqs()
    }
  }
  simD
}

#' Extract tip-specific simulation data from R6 object
#'
#' This function extracts tip-specific simulation data from an R6 object, including:
#'
#' - The name of each tip in the simulated tree
#' - A list with the sequence of methylation states for each tip-specific structure (the index of the list corresponds to the index
#'   of the structures). The methylation states are encoded as 0 for unmethylated, 0.5 for partially methylated, and 1 for methylated.
#'
#' The information is extracted for all of the tips in the tree used to simulate the data.
#'
#' @param R6obj An R6 object containing simulation data
#' @return A list containing the extracted tip-specific information
#'
#'
#' @noRd
extract_tipD <- function(R6obj){
  n_tip <- sum(sapply(R6obj$Branch, function(x) !is.null(x$get_name())))
  tipD <- vector("list", n_tip)
  tip_index <- 1
  for(br in 1:length(R6obj$Branch)){
    if (!is.null(R6obj$Branch[[br]]$get_name())){
      tipD[[tip_index]] <- list(
        name = R6obj$Branch[[br]]$get_name(),
        seq = vector("list", R6obj$Branch[[br]]$get_singleStr_number())
      )
      for (str in 1:R6obj$Branch[[br]]$get_singleStr_number()){
        tipD[[tip_index]]$seq[[str]] = transform_methStateEncoding(R6obj$Branch[[br]]$get_singleStr(str)$get_seq())
      }
      tip_index = tip_index +1
    }
  }
  tipD
}

#' Simulate Data Evolution along a Tree
#'
#' This function simulates methylation data evolution along a tree. Either by simulating data at the root of the provided evolutionary tree
#' (if infoStr is given) or by using pre-existing data at the root (if rootData is given) and letting it evolve along the tree.
#'
#' @param infoStr A data frame containing columns 'n' for the number of sites, and 'globalState' for the favoured global methylation state.
#'  If customized initial equilibrium frequencies are given, it also contains columns 'u_eqFreq', 'p_eqFreq', and 'm_eqFreq'
#'  with the equilibrium frequency values for unmethylated, partially methylated, and methylated.
#' @param rootData The output of the simulate_initialData()$data function. It represents the initial data at the root of the evolutionary tree.
#' @param tree A string in Newick format representing the evolutionary tree.
#' @param params Optional data frame with specific parameter values.
#' Structure as in get_parameterValues() output. If not provided, default values will be used.
#' @param CFTP Default FALSE. TRUE for calling cftp algorithm to set root state according to model equilibrium (Note that current implementation neglects IWE process).
#' @param CFTP_step_limit when CFTP = TRUE, maximum number of steps before applying an approximation method 
#'        (default 327680000 corresponding to size of CFTP info of approx 6.1 GB).
#' @param dt Length of time step for Gillespie's Tau-Leap Approximation (default is 0.01).
#' @param n_rep Number of replicates to simulate (default is 1).
#' @param only_tip Logical indicating whether to extract data only for tips (default is TRUE, FALSE to extract the information for all the tree branches).
#'
#' @examples
#' # Example data
#' infoStr <- data.frame(n = c(10, 100, 10), globalState = c("M", "U", "M"))
#'
#' # Simulate data evolution along a tree with default parameters
#' simulate_evolData(infoStr = infoStr, tree = "(A:0.1,B:0.1);")
#'
#' # Simulate data evolution along a tree with custom parameters
#' custom_params <- get_parameterValues()
#' custom_params$iota <- 0.5
#' simulate_evolData(infoStr = infoStr, tree = "(A:0.1,B:0.1);", params = custom_params)
#'
#' @return A list containing the parameters used (\code{$params}), the length of the time step used for the Gillespie's tau-leap approximation (\code{$dt}, default 0.01), the tree used (\code{$tree}).
#' simulated data and the simulated data (\code{$data}). In \code{$data}, each list element corresponds to a simulation replicate.
#'
#' \itemize{
#'   \item If only_tip is TRUE: In \code{$data}, each list element corresponds to a simulation replicate.
#'   Each replicate includes one list per tree tip, each containing:
#'   \itemize{
#'     \item The name of each tip in the simulated tree (e.g. replicate 2, tip 1: \code{$data[[2]][[1]]$name}).
#'     \item A list with the sequence of methylation states for each tip-specific structure (e.g. replicate 1, tip 2, 3rd structure: \code{$data[[1]][[2]]$seq[[3]]}.
#'         The methylation states are encoded as 0 for unmethylated, 0.5 for partially methylated, and 1 for methylated.
#'   }
#'   \item If only_tip is FALSE, \code{$data} contains 2 lists:
#'   \itemize{
#'     \item \code{$data$branchInTree}: a list in which each element contains the information of the relationship with other branches:
#'     \itemize{
#'       \item Index of the parent branch (e.g. branch 2): \code{$data$branchInTree[[2]]$parent_index})
#'       \item Index(es) of the offspring branch(es) (e.g. branch 1 (root)): \code{$data$branchInTree[[1]]$offspring_index})
#'     }
#'     \item \code{$data$sim_data}: A list containing simulated data. Each list element corresponds to a simulation replicate.
#'     Each replicate includes one list per tree branch, each containing:
#'     \itemize{
#'       \item The name of each branch in the simulated tree. It's NULL for the tree root and inner nodes, and the name of the tips for the tree tips.
#'           (e.g. replicate 2, branch 1: \code{$data$sim_data[[2]][[1]]$name})
#'       \item Information of IWE events on that branch. It's NULL for the tree root and FALSE for the branches in which no IWE event was sampled,
#'           and a list containing \code{$islands} with the index(ces) of the island structure(s) that went through the IWE event and \code{$times}
#'           for the branch time point(s ) in which the IWE was sampled.
#'           (e.g. replicate 1, branch 3: \code{$data$sim_data[[1]][[3]]$IWE})
#'       \item A list with the sequence of methylation states for each structure (the index of the list corresponds to the index
#'           of the structures). The methylation states are encoded as 0 for unmethylated, 0.5 for partially methylated, and 1 for methylated.
#'           (e.g. replicate 3, branch 2, structure 1: \code{$data$sim_data[[3]][[2]]$seq[[1]]})
#'       \item A list with the methylation equilibrium frequencies for each structure (the index of the list corresponds to the index
#'           of the structures). Each structure has a vector with 3 values, the first one corresponding to the frequency of unmethylated,
#'           the second one to the frequency of partially methylated, and the third one to the frequency of methylated CpGs.
#'           (e.g. replicate 3, branch 2, structure 1: \code{$data$sim_data[[3]][[2]]$eqFreqs[[1]]})
#'     }
#'   }
#' }
#'
#' @export
#'
#'
simulate_evolData <- function(infoStr = NULL, rootData = NULL, tree = NULL, params = NULL, dt = 0.01, CFTP = FALSE, CFTP_step_limit = 327680000, n_rep = 1, only_tip = TRUE){
  
  # Control input
  if (is.null(infoStr) && is.null(rootData)) stop("At least one argument of 'infoStr' or 'rootData' must be provided.")
  if (!is.null(infoStr) && !is.null(rootData)) stop("Only one argument of 'infoStr' or 'rootData' should be provided.")
  if(is.null(tree)) stop("Argument 'tree' is missing with no default.")
  if(!is.character(tree)) stop("tree needs to be given as string in newick format")
  if(!is.null(rootData) && !is.null(params)) stop("When rootData is given, rootData parameter values are used. Argument 'params' needs to be null. \n To get rootData parameter values use get_parameterValues(rootData). \n To customize rootData parameter values use get_parameterValues() and modify the desired value(s) in the output dataframe. Then provide the  customized dataframe in the 'params' argument of the function simulate_initialData() to generate a new rootData instance with the customized parameter values.")
  if(!is.null(rootData) && class(rootData)[1] != "combiStructureGenerator") stop("rootData should be the output of simulate_initialData()")
  if(!is.null(infoStr)){
    if (!is.data.frame(infoStr) || !all(c("n", "globalState") %in% colnames(infoStr))) stop("infoStr should be a dataframe with columns: 'n', 'globalState'")
    if (is.null(params)) message("Using default parameter values")
  }
  if(all(c("u_eqFreq", "p_eqFreq", "m_eqFreq") %in% colnames(infoStr))){
    for (i in 1:nrow(infoStr)){
      eqFreqs <- c(infoStr$u_eqFreq[i], infoStr$p_eqFreq[i], infoStr$m_eqFreq[i])
      if(any(is.na(eqFreqs))) stop(paste("if 'u_eqFreq', 'p_eqFreq' and 'm_eqFreq' are given, they need to be frequency values. Missing values in row ", i))
      if(!is.numeric(eqFreqs) || !length(eqFreqs) == 3 || !sum(eqFreqs)==1) stop(paste("if 'u_eqFreq', 'p_eqFreq' and 'm_eqFreq' are given, they need to be frequency values (sum 1). Incorrect values in row ", i))
    }
  }
  if(!is.null(params)){
    if(!is.data.frame(params) || !all(c("alpha_pI", "beta_pI", "alpha_mI", "beta_mI", "alpha_pNI", "beta_pNI", "alpha_mNI", "beta_mNI", "mu", "alpha_Ri", "iota") %in% colnames(params))){
      stop("if 'params' is given, it needs to be a dataframe with column names as in get_parameterValues() output")
    }
  }
  
  # Simulate data
  sim_data = vector("list", n_rep)
  for (r in 1:n_rep){
    R6_obj <- treeMultiRegionSimulator$new(infoStr = infoStr, 
                                           rootData = rootData, 
                                           tree = tree, 
                                           params = params, 
                                           CFTP = CFTP, 
                                           CFTP_step_limit = CFTP_step_limit, 
                                           dt = dt)
    if(only_tip){
      sim_data[[r]] <- extract_tipD(R6obj = R6_obj)
    } else {
      sim_data[[r]] <- extract_simD(R6obj = R6_obj)
    }
  }
  if(!only_tip){
    branchInTree = vector("list", length(R6_obj$Branch))
    for (br in 1:length(R6_obj$Branch))
      branchInTree[[br]] <- list(
        parent_index = R6_obj$Branch[[br]]$get_parent_index(),
        offspring_index = R6_obj$Branch[[br]]$get_offspring_index()
      )
    #return(data = list(sim_data = sim_data, branchInTree = branchInTree))
    sim_data = list(sim_data = sim_data, branchInTree = branchInTree)
  } else {
    #return(data = list(tip_data = sim_data))
    sim_data
  }
  # Save simulation's parameter values
  if(is.null(params)){
    params <- get_parameterValues(rootData) # if rootData is null, then it returns default parameter values
  }
  return(list(data = sim_data,
              params = params,
              tree = tree,
              dt = dt))
}


