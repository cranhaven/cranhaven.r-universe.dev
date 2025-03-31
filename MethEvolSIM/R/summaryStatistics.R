#' Validate Structure Indices for Island and Non-Island Data
#'
#' This function checks whether the provided indices for islands and non-islands 
#' are within the valid range of structures in the dataset. It also warns if 
#' any indices are present in both `index_islands` and `index_nonislands`.
#'
#' @param data A nested list \code{data[[tip]][[structure]]}. 
#'   Assumes that the number of structures is consistent across tips and that
#'   within each structure, all tips have the same number of sites. 
#'   The number of structures is inferred from `length(data[[1]])`.
#' @param index_islands An integer vector specifying indices that correspond to island structures.
#' @param index_nonislands An integer vector specifying indices that correspond to non-island structures.
#'
#' @details The funct@exportion performs the following checks:
#' - Ensures that all indices in `index_islands` and `index_nonislands` are within 
#'   the range of available structures.
#' - Throws an error if any index is out of bounds.
#' - Issues a warning if the same index appears in both `index_islands` and `index_nonislands`.
#'
#' @return No return value. The function stops execution if invalid indices are detected.
validate_structureIndices <- function(data, index_islands, index_nonislands) {
  
  # Get the number of structures
  str_n <- length(data[[1]])
  
  # Define the valid range
  valid_indices <- 1:str_n
  
  # Check whether there are invalid indices
  invalid_island_indices <- index_islands[!(index_islands %in% valid_indices)]
  invalid_nonisland_indices <- index_nonislands[!(index_nonislands %in% valid_indices)]
  
  # If there are any invalid indices, throw an error
  if (length(invalid_island_indices) > 0) {
    stop(sprintf("Invalid island indices detected: %s. Number of structures in given data: %d", 
                 paste(invalid_island_indices, collapse = ", "), str_n))
  }
  
  if (length(invalid_nonisland_indices) > 0) {
    stop(sprintf("Invalid non-island indices detected: %s. Number of structures in given data: %d", 
                 paste(invalid_nonisland_indices, collapse = ", "), str_n))
  }
  
  # Check for overlapping indices
  overlapping_indices <- intersect(index_islands, index_nonislands)
  
  if (length(overlapping_indices) > 0) {
    warning(sprintf("The following indices are present in both 'index_islands' and 'index_nonislands': %s", 
                    paste(overlapping_indices, collapse = ", ")))
  }
}


#### #### #### Frequency of methylation states per structure type #### #### ####

#' Calculate the Mean Frequency of Partially Methylated Sites in Islands
#'
#' This function computes the mean frequency of partially methylated sites (with methylation state 0.5) 
#' for the set of genomic structures identified as islands.
#'
#' @param index_islands A vector containing the structural indices for islands.
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' 
#' @param sample_n The number of samples (tips) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean frequency of partially methylated sites in the islands.
#' @examples
#' # Example usage:
#' index_islands <- c(1, 3)
#' data <- list(
#'   list(c(0.5, 1, 0.5), c(0, 0.5, 1), c(1, 0, 0.5)), # tip 1
#'   list(c(0.5, 0.5, 0), c(1, 0.5, 0.5), c(0.5, 0.5, 1)) # tip 2
#' )
#' sample_n <- 2
#' get_islandMeanFreqP(index_islands, data, sample_n, categorized_data = TRUE)
#' 
#' @export
get_islandMeanFreqP <- function(index_islands, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }
  
  
  tryCatch({
    
    # Validate index islands
    if(length(index_islands) == 0) stop("'index_islands' has no indices")
    validate_structureIndices(data, index_islands, index_nonislands = c())
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  
  mean_island <- c()
  island_counter <- 1
  for (i in index_islands){
    mean_tip <- c()
    for(s in 1:sample_n){
      mean_tip[s] <- mean(data[[s]][[i]]==0.5)
    }
    mean_island[island_counter] <- mean(mean_tip)
    island_counter <- island_counter + 1
  }
  return(mean(mean_island))
}


#' Calculate the Mean Frequency of Partially Methylated Sites in Non-Islands
#'
#' This function computes the mean frequency of partially methylated sites (with methylation state 0.5) 
#' for a set of genomic structures identified as non-islands.
#'
#' @param index_nonislands A vector containing the structural indices for non-islands.
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of samples (tips) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean frequency of partially methylated sites in the non-islands.
#' @examples
#' # Example usage:
#' index_nonislands <- c(1, 3)
#' data <- list(
#'   list(c(0.5, 1, 0.5), c(0, 0.5, 1), c(1, 0, 0.5)), # tip 1
#'   list(c(0.5, 0.5, 0), c(1, 0.5, 0.5), c(0.5, 0.5, 1)) # tip 2
#' )
#' sample_n <- 2
#' get_nonislandMeanFreqP(index_nonislands, data, sample_n, categorized_data = TRUE)
#' 
#' @export
get_nonislandMeanFreqP <- function(index_nonislands, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }
  
  
  tryCatch({
    
    # Validate index non-islands
    if(length(index_nonislands) == 0) stop("'index_nonislands' has no indices")
    validate_structureIndices(data, index_islands = c(), index_nonislands)
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  mean_nonisland <- c()
  nonisland_counter <- 1
  for (i in index_nonislands){
    mean_tip <- c()
    for(s in 1:sample_n){
      mean_tip[s] <- mean(data[[s]][[i]]==0.5)
    }
    mean_nonisland[nonisland_counter] <- mean(mean_tip)
    nonisland_counter <- nonisland_counter + 1
  }
  return(mean(mean_nonisland))
}




#' Calculate the Mean Frequency of Methylated Sites in Islands
#'
#' This function computes the mean frequency of methylated sites (with methylation state 1) 
#' for a set of structures identified as islands.
#'
#' @param index_islands A vector containing the structural indices for islands.
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of samples (tips) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean frequency of methylated sites in the islands.
#' @examples
#' # Example usage:
#' index_islands <- c(1, 3)
#' data <- list(
#'   list(c(0.5, 1, 0.5), c(0, 0.5, 1), c(1, 0, 0.5)), # tip 1
#'   list(c(0.5, 0.5, 0), c(1, 0.5, 0.5), c(0.5, 0.5, 1)) # tip 2
#' )
#' sample_n <- 2
#' get_islandMeanFreqM(index_islands, data, sample_n, categorized_data = TRUE)
#' 
#' @export
get_islandMeanFreqM <- function(index_islands, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list   
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }
  
  tryCatch({
    # Validate index islands
    if(length(index_islands) == 0) stop("'index_islands' has no indices")
    validate_structureIndices(data, index_islands, index_nonislands = c())
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  mean_island <- c()
  island_counter <- 1
  for (i in index_islands){
    mean_tip <- c()
    for(s in 1:sample_n){
      mean_tip[s] <- mean(data[[s]][[i]]==1)
    }
    mean_island[island_counter] <- mean(mean_tip)
    island_counter <- island_counter + 1
  }
  return(mean(mean_island))
}

#' Calculate the Mean Frequency of Methylated Sites in Non-Islands
#'
#' This function computes the mean frequency of methylated sites (with methylation state 1) 
#' for a set of structures identified as non-islands.
#'
#' @param index_nonislands A vector containing the structural indices for non-islands.
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of samples (tips) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean frequency of methylated sites in the non-islands.
#' @examples
#' # Example usage:
#' index_nonislands <- c(1, 3)
#' data <- list(
#'   list(c(1, 0, 1), c(0.5, 1, 1), c(1, 0, 0.5)), # tip 1
#'   list(c(1, 0.5, 1), c(0.5, 1, 1), c(1, 0.5, 0.5)) # tip 2
#' )
#' sample_n <- 2
#' get_nonislandMeanFreqM(index_nonislands, data, sample_n, categorized_data = TRUE)
#' 
#' @export
get_nonislandMeanFreqM <- function(index_nonislands, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }
  
  tryCatch({
    
    # Validate index non-islands
    if(length(index_nonislands) == 0) stop("'index_nonislands' has no indices")
    validate_structureIndices(data, index_islands = c(), index_nonislands)
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  mean_nonisland <- c()
  nonisland_counter <- 1
  for (i in index_nonislands){
    mean_tip <- c()
    for(s in 1:sample_n){
      mean_tip[s] <- mean(data[[s]][[i]]==1)
    }
    mean_nonisland[nonisland_counter] <- mean(mean_tip)
    nonisland_counter <- nonisland_counter + 1
  }
  return(mean(mean_nonisland))
}



#' Calculate the Mean Standard Deviation of Partially Methylated Sites in Islands
#'
#' This function computes the mean standard deviation of partially methylated sites 
#' (with methylation state 0.5) for a set of genomic structures identified as islands.
#'
#' @param index_islands A vector containing the structural indices for islands.
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of samples (tips) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean standard deviation of partially methylated sites in the islands.
#' @examples
#' # Example usage:
#' index_islands <- c(1, 3)
#' data <- list(
#'   list(c(0.5, 1, 0.5), c(0, 0.5, 1), c(1, 0, 0.5)), # tip 1
#'   list(c(0.5, 0.5, 0), c(1, 0.5, 0.5), c(0.5, 0.5, 1)) # tip 2
#' )
#' sample_n <- 2
#' get_islandSDFreqP(index_islands, data, sample_n, categorized_data = TRUE)
#' 
#' @export
get_islandSDFreqP <- function(index_islands, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }

  tryCatch({
    
    # Validate index islands
    validate_structureIndices(data, index_islands, index_nonislands = c())
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  sd_tip <- c() 
  for (s in 1:sample_n){
    freq_island <- c()
    island_counter <- 1
    for (i in index_islands){
      # Compute proportion of partially methylated sites at each island
      freq_island[island_counter] <- mean(data[[s]][[i]] == 0.5)
      island_counter <- island_counter + 1
    }
    sd_tip[s] <- stats::sd(freq_island)
  }
  return(mean(sd_tip))
}

#' Calculate the Mean Standard Deviation of Partially Methylated Sites in Non-Islands
#'
#' This function computes the mean standard deviation of partially methylated sites 
#' (with methylation state 0.5) for a set of genomic structures identified as non-islands.
#'
#' @param index_nonislands A vector containing the structural indices for non-islands.
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of samples (tips) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean standard deviation of partially methylated sites in the non-islands.
#' @examples
#' # Example usage:
#' index_nonislands <- c(1, 3)
#' data <- list(
#'   list(c(0.5, 1, 0.5), c(0, 0.5, 1), c(1, 0, 0.5)), # tip 1
#'   list(c(0.5, 0.5, 0), c(1, 0.5, 0.5), c(0.5, 0.5, 1)) # tip 2
#' )
#' sample_n <- 2
#' get_nonislandSDFreqP(index_nonislands, data, sample_n, categorized_data = TRUE)
#' 
#' @export
get_nonislandSDFreqP <- function(index_nonislands, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }

  tryCatch({
    
    # Validate index islands
    validate_structureIndices(data, index_islands = c(), index_nonislands)
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  sd_tip <- c() 
  for (s in 1:sample_n){
    freq_nonisland <- c()
    nonisland_counter <- 1
    for (i in index_nonislands){
      # Compute proportion of partially methylated sites at each island
      freq_nonisland[nonisland_counter] <- mean(data[[s]][[i]] == 0.5)
      nonisland_counter <- nonisland_counter + 1
    }
    sd_tip[s] <- stats::sd(freq_nonisland)
  }
  return(mean(sd_tip))
}

#' Calculate the Mean Standard Deviation of Methylated Sites in Islands
#'
#' This function computes the mean standard deviation of methylated sites 
#' (with methylation state 1) for a set of genomic structures identified as islands.
#'
#' @param index_islands A vector containing the structural indices for islands.
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of tips (samples) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean standard deviation of methylated sites in the islands.
#' @examples
#' # Example usage:
#' index_islands <- c(1, 3)
#' data <- list(
#'   list(c(0.5, 1, 0.5), c(0, 0.5, 1), c(1, 0, 0.5)), # tip 1
#'   list(c(0.5, 0.5, 0), c(1, 0.5, 0.5), c(0.5, 0.5, 1)) # tip 2
#' )
#' sample_n <- 2
#' get_islandSDFreqM(index_islands, data, sample_n, categorized_data = TRUE)
#' 
#' @export
get_islandSDFreqM <- function(index_islands, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }
  
  tryCatch({
    
    # Validate index islands
    validate_structureIndices(data, index_islands, index_nonislands = c())
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  sd_tip <- c() 
  for (s in 1:sample_n){
    freq_island <- c()
    island_counter <- 1
    for (i in index_islands){
      # Compute proportion of methylated sites at each island
      freq_island[island_counter] <- mean(data[[s]][[i]] == 1)
      island_counter <- island_counter + 1
    }
    sd_tip[s] <- stats::sd(freq_island)
  }
  return(mean(sd_tip))
}


#' Calculate the Mean Standard Deviation of Methylated Sites in Non-Islands
#'
#' This function computes the mean standard deviation of methylated sites 
#' (with methylation state 1) for a set of genomic structures identified as non-islands.
#'
#' @param index_nonislands A vector containing the structural indices for non-islands.
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of tips (samples) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean standard deviation of methylated sites in the non-islands.
#' @examples
#' # Example usage:
#' index_nonislands <- c(1, 3)
#' data <- list(
#'   list(c(1, 1, 1), c(0, 1, 0.5), c(1, 0, 1)), # tip 1
#'   list(c(1, 0.5, 0), c(1, 1, 0.5), c(1, 1, 1)) # tip 2
#' )
#' sample_n <- 2
#' get_nonislandSDFreqM(index_nonislands, data, sample_n, categorized_data = TRUE)
#' 
#' @export
get_nonislandSDFreqM <- function(index_nonislands, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list   
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }
  
  tryCatch({
    
    # Validate index non-islands
    validate_structureIndices(data, index_islands = c(), index_nonislands)
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  sd_tip <- c() 
  for (s in 1:sample_n){
    freq_nonisland <- c()
    nonisland_counter <- 1
    for (i in index_nonislands){
      # Compute proportion of methylated sites at each non-island
      freq_nonisland[nonisland_counter] <- mean(data[[s]][[i]] == 1)
      nonisland_counter <- nonisland_counter + 1
    }
    sd_tip[s] <- stats::sd(freq_nonisland)
  }
  return(mean(sd_tip))
}


#### #### #### Mean neighbor correlations per structure type #### #### ####



#' Compute the Mean Correlation of Methylation State in Islands
#'
#' This function calculates the mean correlation of methylation states within 
#' island structures, allowing to exclude the shores.
#'
#' @param index_islands A vector containing the structural indices for islands.
#' @param minN_CpG The minimum number of central CpGs required for computation.
#' @param shore_length The number of CpGs at each side of an island to exclude (shores).
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of tips (samples) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean correlation of methylation states in the central CpGs of islands.
#' @details The function processes only islands with a minimum length equal to \code{2 * shore_length + minN_CpG}. 
#' If none has minimum length, returns NA
#' @examples
#' # Example usage:
#' index_islands <- c(1, 2)
#' data <- list(
#'   list(c(0, 1, 0.5, 1, 0.5, 0), c(0.5, 0.5, 1, 1, 0, 0)), # tip 1
#'   list(c(1, 0, 1, 1, 0.5, 0), c(1, 1, 0.5, 0.5, 0, 1))   # tip 2
#' )
#' minN_CpG <- 2
#' shore_length <- 1
#' sample_n <- 2
#' compute_meanCor_i(index_islands, minN_CpG, shore_length, data, sample_n,
#'                    categorized_data = TRUE)
#' 
#' @export
compute_meanCor_i <- function(index_islands, minN_CpG, shore_length, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list 
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }

  tryCatch({
    
    # Validate index non-islands
    validate_structureIndices(data, index_islands, index_nonislands = c())
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  
  str_counter <- 1
  cor <- c(NA) 
  for (tip in 1:sample_n){
    for (i in index_islands){
      if(length(data[[tip]][[i]]) >= 2*shore_length + minN_CpG){
        # Define start and end indices to extract the middle segment
        start1 <- shore_length + 1
        start2 <- start1 + 1
        end2 <- length(data[[tip]][[i]]) - shore_length
        end1 <- end2 - 1
        # Extract the sequence info
        segment1 <- data[[tip]][[i]][start1:end1]
        segment2 <- data[[tip]][[i]][start2:end2]
        # Compute the correlation of those segments with methylation state variation
        if (stats::sd(segment1) != 0 && stats::sd(segment2) != 0) {
          cor[str_counter] <- cor(segment1, segment2)
          str_counter <- str_counter + 1
        }
      }
      
    }
  }
  return(mean(cor))
}


#' Compute the Mean Correlation of Methylation State in Non-islands
#'
#' This function calculates the mean correlation of methylation states within 
#' non-island structures, allowing to exclude the shores.
#'
#' @param index_nonislands A vector containing the structural indices for non-islands.
#' @param minN_CpG The minimum number of central CpGs required for computation.
#' @param shore_length The number of CpGs at each side of an non-island to exclude (shores).
#' @param data A list containing methylation states at tree tips for each genomic structure (island / non-island) 
#'   For a single tip: \code{data[[structure]]}. 
#'   For multiple tips: \code{data[[tip]][[structure]]}.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param sample_n The number of tips (samples) to process.
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A numeric value representing the mean correlation of methylation states in the central CpGs of non-islands.
#' @details The function processes only non-islands with a minimum length equal to \code{2 * shore_length + minN_CpG}. 
#' If none has minimum length, returns NA
#' @examples
#' # Example usage:
#' index_nonislands <- c(1, 2)
#' data <- list(
#'   list(c(0, 1, 0.5, 1, 0.5, 0), c(0.5, 0.5, 1, 1, 0, 0)), # tip 1
#'   list(c(1, 0, 1, 1, 0.5, 0), c(1, 1, 0.5, 0.5, 0, 1))   # tip 2
#' )
#' minN_CpG <- 2
#' shore_length <- 1
#' sample_n <- 2
#' compute_meanCor_ni(index_nonislands, minN_CpG, shore_length, data, sample_n,
#'                     categorized_data = TRUE)
#' 
#' @export
compute_meanCor_ni <- function(index_nonislands, minN_CpG, shore_length, data, sample_n, categorized_data = FALSE){
  
  if(!is.list(data)) stop("Input 'data' is not list.")
  
  if (!(is.numeric(sample_n) && length(sample_n) == 1 && sample_n == floor(sample_n))) {
    stop("sample_n must be a single non-decimal numeric value.")
  }
  
  # Restructure data as nested list 
  if (sample_n == 1){
    data_list <- list()
    data_list[[1]] <- data
    data <- data_list
  }
  
  tryCatch({
    # Validate index non-islands
    validate_structureIndices(data, index_islands = c(), index_nonislands)
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  str_counter <- 1
  cor <- c(NA) 
  for (tip in 1:sample_n){
    for (i in index_nonislands){
      if(length(data[[tip]][[i]]) >= 2*shore_length + minN_CpG){
        # Define start and end indices to extract the middle segment
        start1 <- shore_length + 1
        start2 <- start1 + 1
        end2 <- length(data[[tip]][[i]]) - shore_length
        end1 <- end2 - 1
        # Extract the sequence info
        segment1 <- data[[tip]][[i]][start1:end1]
        segment2 <- data[[tip]][[i]][start2:end2]
        # Compute the correlation of those segments with methylation state variation
        if (stats::sd(segment1) != 0 && stats::sd(segment2) != 0) {
          cor[str_counter] <- cor(segment1, segment2)
          str_counter <- str_counter + 1
        }
      }
      
    }
  }
  return(mean(cor))
}



#### #### #### Tree cherries comparisons #### #### ####

#' Validate and Parse a Phylogenetic Tree
#'
#' This function checks whether the input is a valid phylogenetic tree, either as a character string in Newick format
#' or as an object of class \code{phylo} from the \code{ape} package. If the input is a Newick string, it is parsed into 
#' a \code{phylo} object. The function also ensures that the tree contains at least two tips.
#'
#' @param tree A phylogenetic tree in Newick format (as a character string) or an object of class \code{phylo} from the \code{ape} package.
#'   - If the input is a character string, it must follow the Newick format (e.g., \code{"((tip_1:1,tip_2:1):5,tip_3:6);"}).
#'   - If an object of class \code{phylo} is provided, it should be a valid phylogenetic tree.
#'
#' @return A \code{phylo} object representing the validated and parsed tree.
#'
#' @details 
#' - The function first verifies that the input is either a valid \code{phylo} object or a character string.
#' - If the input is a Newick string, it attempts to parse it into a \code{phylo} object using \code{ape::read.tree()}.
#' - If parsing fails, an informative error message is returned.
#' - The function also checks that the tree contains at least two tips, as a valid phylogenetic tree should have at least one split.
#'
#' @importFrom ape read.tree
validate_tree <- function(tree){
  
  # Check that the input is a character string or phylo object
  if (!is.character(tree) && !inherits(tree, "phylo")) {
    stop(paste("Argument 'tree' must be a character string in newick format (e.g. '((tip_1:1,tip_2:1):5,tip_3:6);').\n",
               "or an object of class 'phylo' from the 'ape' package. For more details, see ?phylo or ?ape."))
  }
  
  # Transform the newick tree in a phylo object managing issues with given tree format
  if(!inherits(tree, "phylo")) {
    tryCatch({
      # Attempt to read the tree
      tree <- ape::read.tree(text = tree)
      # Check if the result is NULL, which indicates an issue with the tree format
      if (is.null(tree)) {
        stop("Error in ape::read.tree(tree): Tree could not be parsed. Invalid format.")
      }
    }, warning = function(w) {
      stop(paste("Error in ape::read.tree(tree): Invalid 'tree' format: ", conditionMessage(w), "\n",
                 "Argument 'tree' must be a character string in newick format (e.g. '((tip_1:1,tip_2:1):5,tip_3:6);').\n",
                 "or an object of class 'phylo' from the 'ape' package. For more details, see ?phylo or ?ape."))
    }, error = function(e) {
      stop(paste("Error in ape::read.tree(tree): Invalid 'tree' format: ", conditionMessage(e), "\n",
                 "Argument 'tree' must be a character string in newick format (e.g. '((tip_1:1,tip_2:1):5,tip_3:6);').\n",
                 "or an object of class 'phylo' from the 'ape' package. For more details, see ?phylo or ?ape."))
    })
  }
  
  # Check given tree has minium two tips
  if (length(tree$tip.label)<2) stop("The input 'tree' must have a minimum of 2 tips.")
  
  # Check given tree has unique tip labels
  if (any(duplicated(tree$tip.label))) {
    stop(paste("The input tree has duplicated tree labels:", paste(unique(tree$tip.label[duplicated(tree$tip.label)]), collapse = ", ")))
  }
  
  # Return tree as phylo object
  tree
}

#' Get Cherry Pair Distances from a Phylogenetic Tree
#'
#' This function computes the pairwise distances between the tips of a phylogenetic tree 
#' that are part of cherries. A cherry is a pair of leaf nodes (also called tips or terminal nodes) 
#' in a phylogenetic tree that share a direct common ancestor. 
#' In other words, if two leaves are connected to the same internal node and no other leaves 
#' are connected to that internal node, they form a cherry.
#' The distance is calculated as the sum of the branch lengths between the two cherry tips.
#'
#' @param tree A tree in Newick format (as a character string) or an object of class \code{phylo} from the \code{ape} package.
#' If the input is a character string, it must follow the Newick or New Hampshire format (e.g. \code{"((tip_1:1,tip_2:1):5,tip_3:6);"}).
#' If an object of class \code{phylo} is provided, it should represent a valid phylogenetic tree.
#'
#' @param input_control A logical value indicating whether to validate the input tree. 
#' If \code{TRUE} (default), the function checks that the tree is in a valid format and has at least two tips.
#' If \code{FALSE}, the function assumes the tree is already valid and skips the validation step.
#'
#' @return A data frame with five columns:
#'   \item{first_tip_name}{A character string representing the name of the first tip in the cherry.}
#'   \item{second_tip_name}{A character string representing the name of the second tip in the cherry.}
#'   \item{first_tip_index}{An integer representing the index of the first tip in the cherry.}
#'   \item{second_tip_index}{An integer representing the index of the second tip in the cherry.}
#'   \item{dist}{A numeric value representing the sum of the branch lengths between the two tips (i.e., the distance between the cherries).}
#'   
#' @details The function first checks if the input is either a character string in the Newick format or an object of class \code{phylo}, 
#' unless \code{input_control} is set to \code{FALSE}. It then computes the pairwise distances between the tips in the tree and 
#' identifies the sister pairs (cherries). The distance between each cherry is the sum of the branch lengths leading to the sister tips.
#'   
#'   The tips of each cherry are identified by their names and indices. 
#'   The tip indices correspond to (a) the index from left to right on the Newick string, 
#'   (b) the order of the tip label in the \code{phylo_object$tip.label}, and 
#'   (c) the index in the methylation data list (\code{data[[tip]][[structure]]}) as obtained with the function \code{simulate_evolData()} when the given tree has several tips.
#'   
#'   If the tree is provided in Newick format, it will be parsed using the \code{ape::read.tree} function.
#'   
#' @importFrom ape read.tree cophenetic.phylo
#'   
#' @examples
#' # Example of a tree in Newick format
#' 
#' newick_tree <- "((a:1,b:2):5,c:6);"
#' 
#' get_cherryDist(newick_tree)
#' 
#' # Example of using a phylo object from ape
#' 
#' library(ape)
#' tree_phylo <- read.tree(text = "((a:1,b:1):5,c:6);")
#' 
#' get_cherryDist(tree_phylo)
#' 
#' @export
get_cherryDist <- function(tree, input_control = TRUE){
  
  # Check input tree format and minium two tips, get tree in phylo format (ape package)
  if (input_control) tree <- validate_tree(tree)
  
  # Compute the pairwise distances between the tips from a phylogenetic tree
  dist <- ape::cophenetic.phylo(tree)
  # Set a vector to save the cherry tips for which the distance has already been extracted (because dist is symmetric)
  cherry_tips <- c()
  # set df to store the cherry names, tip indices and distances
  cherry_dist <- data.frame(first_tip_name=character(), 
                            second_tip_name=character(),
                            first_tip_index=integer(),
                            second_tip_index=integer(),
                            dist=numeric()) 
  # Get the tip names
  tips <- rownames(dist)

  for(tip in tips) {
    # If tip is not already in cherry_dist
    if (!tip %in% cherry_tips) {
      # find the closest tip
      tip_a <- names(which(dist[tip,] == min(dist[tip, colnames(dist) != tip])))
      # if there is one single closest tip
      if(length(tip_a)==1){
        # find the closes tip 
        tip_b <- names(which(dist[tip_a,] == min(dist[tip_a, colnames(dist) != tip_a])))
        # if there is one single closest tip
        if (length(tip_b)==1){
          # if both are mutually closest tips 
          if(tip_b == tip){
            # save the tip labels, the distance and the tip indices
            cherry_dist[nrow(cherry_dist)+1,] <- c(tip, tip_a, which(rownames(dist)==tip), which(rownames(dist)==tip_a), dist[tip,tip_a])
            cherry_tips <- c(cherry_tips, tip, tip_a)
          }
        }
      }
    }
  }
  cherry_dist$dist <- as.numeric(cherry_dist$dist)
  cherry_dist$first_tip_index <- as.integer(cherry_dist$first_tip_index)
  cherry_dist$second_tip_index <- as.integer(cherry_dist$second_tip_index)
  cherry_dist
}


#' Validate Structure of Input Data for Cherry Distance Computation
#'
#' This function checks whether the provided input data has the required structure.
#' It ensures that the number of tips is sufficient and that the data structure is consistent across tips and structures.
#'
#' @param cherryDist A data frame containing cherry pair distances, including tip indices (output from \code{get_cherryDist})
#' @param data A nested list representing structured data for each tip, following the format \code{data[[tip]][[structure]]}.
#'
#' @details The function performs several validation steps:
#' - Ensures that the number of tips in \code{data} is at least as large as the highest tip index in \code{cherryDist}.
#' - Checks that all tips contain at least one structure and that the number of structures is consistent across tips.
#' - Verifies that within each structure, all tips have the same number of sites and no zero-length structures.
#'
#' If any of these conditions fail, the function throws an error with a descriptive message.
validate_data_cherryDist <- function(cherryDist, data){
  
  # Get the number of tips in data
  num_tips<- length(data)
  
  # Check the number of tips is equal or greater to the maximum tip index in cherryDist argument
  if(!num_tips>=max(c(cherryDist$first_tip_index, cherryDist$second_tip_index))){
    stop("Argument 'data' with required structure data[[tip]][[structure]] does not have enough tips")
  }
  
  # Ensure the number of structures is > 0 and equal across tips
  lengths_per_tip <- sapply(seq_len(num_tips), function(tip) length(data[[tip]]))
  if (!(all(lengths_per_tip > 0) && length(unique(lengths_per_tip)) == 1)) {
    stop("Input 'data' with required structure data[[tip]][[structure]] has some tips with zero or differing number of structures. Check given 'data' structure.")
  }
  
  # Set the number of structures after checking consistency across tips
  str_n <- length(data[[1]]) 
  
  # Iterate over each structure and check length (number of sites) consistency across tips
  for (structure in seq_len(str_n)) {
    lengths_per_tip <- sapply(seq_len(num_tips), function(tip) length(data[[tip]][[structure]]))
    
    # Ensure all lengths are > 0 and equal across tips
    if (!(all(lengths_per_tip > 0) && length(unique(lengths_per_tip)) == 1)) {
      stop(paste("Error: Input 'data' with required structure data[[tip]][[structure]] has inconsistent lengths across tips or zero length at some tips at structure", structure, "."))
    }
  }
}

#' Count Methylation Differences Between Cherry Pairs
#'
#' This function calculates the number of methylation differences between pairs of cherry tips in a phylogenetic tree.
#' A cherry is a pair of leaf nodes that share a direct common ancestor. The function quantifies full and half methylation
#' differences for each genomic structure (e.g., island/non-island) across all sites.
#'
#' @param cherryDist A data frame containing pairwise distances between the tips of a phylogenetic tree that form cherries.
#'   This should be as the output of \code{get_cherryDist}, and must include the following columns:
#'   \describe{
#'     \item{first_tip_name}{A character string representing the name of the first tip in the cherry.}
#'     \item{second_tip_name}{A character string representing the name of the second tip in the cherry.}
#'     \item{first_tip_index}{An integer representing the index of the first tip in the cherry.}
#'     \item{second_tip_index}{An integer representing the index of the second tip in the cherry.}
#'     \item{dist}{A numeric value representing the sum of the branch lengths between the two tips (i.e., the distance between the cherries).}
#'   }
#'
#' @param data A list containing methylation states at tree tips for each genomic structure (e.g., island/non-island).
#'   The data should be structured as `data[[tip]][[structure]]`, where each structure has the same number of sites across tips.
#'   The input data must be prefiltered to ensure CpG sites are represented consistently across different tips.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#'
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'   
#' @param input_control A logical value indicating whether to validate the input data. 
#' If \code{TRUE} (default), the function checks that the data has the required structure.
#' It ensures that the number of tips is sufficient and that the data structure is consistent across tips and structures.
#' If \code{FALSE}, the function assumes the tree is already valid and skips the validation step.
#'
#' @return A data frame with one row per cherry, containing the following columns:
#'   \describe{
#'     \item{tip_names}{A character string representing the names of the two tips in the cherry, concatenated with a hyphen.}
#'     \item{tip_indices}{A character string representing the indices of the two tips in the cherry, concatenated with a hyphen.}
#'     \item{dist}{A numeric value representing the sum of the branch distances between the cherry tips.}
#'     \item{One column for each structure named with the structure number followed by _f}{An integer count of the sites with a full methylation difference (where one tip is methylated and the other is unmethylated) for the given structure.}
#'     \item{One column for each structure named with the structure number followed by _h}{An integer count of the sites with a half methylation difference (where one tip is partially methylated and the other is either fully methylated or unmethylated) for the given structure.}
#'   }
#'
#' @details The function first verifies that \code{cherryDist} contains the required columns and has at least one row.
#'   It also ensures that \code{data} contains a sufficient number of tips and that all structures have the same number of sites.
#'   The function then iterates over each cherry and genomic structure to compute the number of full and half methylation differences
#'   between the two tips of each cherry.
#'
#' @examples
#' # Example data setup
#' 
#' data <- list(
#'   list(c(0, 1, 0.5, 0), c(1, 1, 0, 0.5)),
#'   list(c(1, 0, 0.5, 1), c(0, 1, 0.5, 0.5))
#' )
#' 
#' tree <- "(tip1:0.25, tip2:0.25);"
#' 
#' cherryDist <- get_cherryDist(tree)
#' 
#' countSites_cherryMethDiff(cherryDist, data, categorized_data = TRUE)
#'
#' @export
countSites_cherryMethDiff <- function(cherryDist, data, categorized_data = FALSE, input_control = TRUE) {
  
  # Check argument cherryDist has all columns
  if(!all(c("first_tip_name", "second_tip_name", "first_tip_index", "second_tip_index", "dist") %in% names(cherryDist))){
    stop("Argument 'cherryDist' misses a required column")
  }
  
  # Check argument cherryDist has at least one row
  if(!nrow(cherryDist)>0) stop("Argument cherryDist has 0 rows.")
  
  # Check input data format and minium number of tips
  if (input_control) validate_data_cherryDist(cherryDist, data)
  
  # Categorize methylation states
  if(!categorized_data) data <- categorize_siteMethSt(data)
  
  # Set the number of structures after checking consistency across tips
  str_n <- length(data[[1]]) 
  
  # Initialize a data frame to store the count of methylation differences at each cherry
  df <- data.frame(tip_names=character(), tip_indices=character(), dist=numeric()) 
  
  # For each structure in the data add two integer columns named as
  # [structure_index]_f for full methylation changes and [structure_index]_h for half methylation changes
  for(str in 1:str_n) {
    df <- cbind(df, integer(), integer())
    names(df)[c(ncol(df)-1, ncol(df))] <- paste0(str,"_",c("f", "h"))
  }
  
  # Loop through the cherries
  for(i in 1:nrow(cherryDist)) {
    
    # Extract current cherry info
    ch <- cherryDist[i,]
    
    # Save current cherry tip names, tip indices and distance
    df[i, "tip_names"] <- paste0(ch$first_tip_name,"-",ch$second_tip_name) 
    df[i, "tip_indices"] <- paste0(ch$first_tip_index,"-",ch$second_tip_index) 
    df[i, "dist"] <- ch$dist 
    
    # Loop through the structures
    for(str in 1:str_n) {
      # Get the sequence of methylation states for the given structure at the cherry tips
      data_first_tip <- data[[ch$first_tip_index]][[str]]
      data_second_tip <- data[[ch$second_tip_index]][[str]]
      
      # Count number of sites with full methylation change (from u to m or m to u)
      df[i, paste0(str, "_f")] <- sum(abs(data_first_tip-data_second_tip)==1)
      
      # Count number of sites with half methylation change (p in one tip and m or u in the other)
      df[i, paste0(str, "_h")] <- sum(abs(data_first_tip-data_second_tip)==0.5)
    }
  }
  # Return the data frame
  df
}

#' Compute Methylation Frequency Differences Between Cherry Pairs
#'
#' This function calculates the frequency of methylation differences between pairs of cherry tips in a phylogenetic tree.
#' A cherry is a pair of leaf nodes that share a direct common ancestor. The function quantifies full and half methylation
#' differences for each genomic structure (e.g., island/non-island) across all sites and normalizes these counts by the number
#' of sites per structure to obtain frequencies.
#'
#' @param tree A phylogenetic tree object. The function assumes it follows an appropriate format for downstream processing.
#'   
#' @param data A list containing methylation states at tree tips for each genomic structure (e.g., island/non-island).
#'   The data should be structured as `data[[tip]][[structure]]`, where each structure has the same number of sites across tips.
#'   The input data must be prefiltered to ensure CpG sites are represented consistently across different tips.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#'
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @param input_control A logical value indicating whether to validate the input data.
#'   If `TRUE` (default), the function checks that the data has the required structure.
#'   It ensures that the number of tips is sufficient and that the data structure is consistent across tips and structures.
#'   If `FALSE`, the function assumes the tree is already valid and skips the validation step.
#'
#' @return A data frame with one row per cherry, containing the following columns:
#'   \describe{
#'     \item{tip_names}{A character string representing the names of the two tips in the cherry, concatenated with a hyphen.}
#'     \item{tip_indices}{A character string representing the indices of the two tips in the cherry, concatenated with a hyphen.}
#'     \item{dist}{A numeric value representing the sum of the branch distances between the cherry tips.}
#'     \item{One column for each structure named with the structure number followed by _f}{A numeric value representing the frequency of sites with a full methylation difference (where one tip is methylated and the other is unmethylated) for the given structure.}
#'     \item{One column for each structure named with the structure number followed by _h}{A numeric value representing the frequency of sites with a half methylation difference (where one tip is partially methylated and the other is either fully methylated or unmethylated) for the given structure.}
#'   }
#'
#' @details The function first validates the tree structure and extracts pairwise distances between cherry tips.
#'   It then counts methylation differences using `countSites_cherryMethDiff` and normalizes these counts by the number
#'   of sites per structure to compute frequencies. The resulting data frame provides a per-cherry frequency
#'   of methylation differences (half or full difference) across different genomic structures.
#'
#' @examples
#' # Example data setup
#' 
#' data <- list(
#' list(rep(1,10), rep(0,5), rep(1,8)),
#' list(rep(1,10), rep(0.5,5), rep(0,8)),
#' list(rep(1,10), rep(0.5,5), rep(0,8)),
#' list(c(rep(0,5), rep(0.5, 5)), c(0, 0, 1, 1, 1), c(0.5, 1, rep(0, 6))))
#' 
#' tree <- "((a:1.5,b:1.5):2,(c:2,d:2):1.5);"
#' 
#' freqSites_cherryMethDiff(tree, data, categorized_data = TRUE)
#'
#' @export
freqSites_cherryMethDiff <- function(tree, data, categorized_data = FALSE, input_control = TRUE){
  
  tryCatch({
    
    # Check input tree format and minium two tips, get tree in phylo format (ape package)
    if (input_control) tree <- validate_tree(tree)
    
    # Get cherry distances avoiding duplicate input control
    cherryDist <- get_cherryDist(tree, input_control = FALSE)
    
    # Check input tree data format and minium number of tips
    if (input_control) validate_data_cherryDist(cherryDist, data)
    
    # Get the count numbers per type (full or half) of methylation change per cherry
    # avoiding duplicate input control
    df <- countSites_cherryMethDiff(cherryDist, data, categorized_data = categorized_data, input_control = FALSE)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  
  # Get the number of structures
  str_n <- length(data[[1]])
  
  # Get the number of sites per structure
  sites_n <- numeric(length = str_n)
  for(str in 1:str_n) sites_n[str] <- length(data[[1]][[str]]) 
  
  # Duplicate them so that there is one-to-one correspondence with the counts in df
  dup_str_n <- rep(sites_n, each = 2)
  
  # Filter the counts (3 first columns correspond to tip_names, tip_indices and dist)
  # to be transformed to frequencies per structure and methylation type
  freqs <- df[,4:ncol(df)]
  
  # Divide each value by the number of sites in the corresponding strucute
  for(cherry in 1:nrow(freqs)) freqs[cherry,] <- freqs[cherry,]/dup_str_n
  
  # Update the frequencies in the dataframe containing tip_names, tip_indices and dist
  df[,4:ncol(df)] <- freqs
  
  df
}


#' Compute Site Frequency of Methylation Changes per Cherry
#'
#' This function calculates the total frequency of methylation differences (both full and half changes)
#' for each genomic structure for each cherry in a phylogenetic tree. 
#' A cherry is a pair of leaf nodes (also called tips or terminal nodes) 
#' in a phylogenetic tree that share a direct common ancestor. 
#' In other words, if two leaves are connected to the same internal node and no other leaves 
#' are connected to that internal node, they form a cherry.
#'
#' @param tree A phylogenetic tree in Newick format or a phylo object from the ape package. The function ensures
#'   the tree has a valid structure and at least two tips.
#'
#' @param data A list containing methylation states at tree tips for each genomic structure (e.g., island/non-island).
#'   The data should be structured as `data[[tip]][[structure]]`, where each structure has the same number of sites across tips.
#'   The input data must be prefiltered to ensure CpG sites are represented consistently across different tips.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#'
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#'
#' @return A data frame with one row per cherry, containing the following columns:
#'   \describe{
#'     \item{tip_names}{A character string representing the names of the two tips in the cherry, concatenated with a hyphen.}
#'     \item{tip_indices}{A character string representing the indices of the two tips in the cherry, concatenated with a hyphen.}
#'     \item{dist}{A numeric value representing the sum of the branch distances between the cherry tips.}
#'     \item{One column for each structure named with the structure number}{A numeric value representing the total frequency of methylation changes (both full and half) for the given structure.}
#'   }
#'
#' @details The function first verifies that \code{tree} and \code{data} have valid structures and the minimum number of tips.
#'   It then extracts per-cherry methylation differences using \code{freqSites_cherryMethDiff}, handling potential errors.
#'   Finally, it aggregates the full and half methylation differences for each genomic structure at each cherry.
#'
#' @examples
#' # Example data setup
#' 
#' data <- list(
#' list(rep(1,10), rep(0,5), rep(1,8)),
#' list(rep(1,10), rep(0.5,5), rep(0,8)),
#' list(rep(1,10), rep(0.5,5), rep(0,8)),
#' list(c(rep(0,5), rep(0.5, 5)), c(0, 0, 1, 1, 1), c(0.5, 1, rep(0, 6))))
#' 
#' tree <- "((a:1.5,b:1.5):2,(c:2,d:2):1.5);"
#' 
#' get_siteFChange_cherry(tree, data, categorized_data = TRUE)
#'
#' @export
get_siteFChange_cherry <- function(tree, data, categorized_data = FALSE){
  
  # Check input tree format and minium two tips, get tree in phylo format (ape package),
  # check input tree data format and minium number of tips, and 
  # get per-cherry frequency of (half and full) methylation differences across different genomic structures
  tryCatch({
    freqSites_perMethDiffType <- freqSites_cherryMethDiff(tree, data, categorized_data = categorized_data)
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })

  # Identify the unique structures in the column names (3 first columns correspond to tip_names, tip_indices and dist)
  structures <- unique(sub("_[fh]$", "", names(freqSites_perMethDiffType)[-c(1, 2, 3)]))
  
  # Create the new dataframe
  siteFChange_cherry <- data.frame(
    tip_names = freqSites_perMethDiffType$tip_names,
    tip_indices = freqSites_perMethDiffType$tip_indices,
    dist = freqSites_perMethDiffType$dist
  )
  
  # Sum for each structure the total frequency of methylation change (both half and full)
  for (str in structures){
    siteFChange_cherry[[str]] <- rowSums(freqSites_perMethDiffType[, grep(paste0("^", str, "_"), names(freqSites_perMethDiffType))])
  }
  
  siteFChange_cherry
}






#' Compute the Mean Site Frequency of Methylation Changes per Cherry
#'
#' This function calculates the weighted mean frequency of methylation changes at island and non-island genomic structures 
#' for each cherry in a phylogenetic tree. A cherry is a pair of leaf nodes (also called tips or terminal nodes) 
#' in a phylogenetic tree that share a direct common ancestor.
#'
#' @param data A list containing methylation states at tree tips for each genomic structure (e.g., island/non-island).
#'   The data should be structured as `data[[tip]][[structure]]`, where each structure has the same number of sites across tips.
#'   The input data must be prefiltered to ensure CpG sites are represented consistently across different tips.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#' @param tree A phylogenetic tree in Newick format or a \code{phylo} object from the \code{ape} package. 
#'   The function ensures the tree has a valid structure and at least two tips.
#' @param index_islands A numeric vector specifying the indices of genomic structures corresponding to islands.
#' @param index_nonislands A numeric vector specifying the indices of genomic structures corresponding to non-islands.
#'
#' @details The function first validates the tree and the input data structure. It then computes the 
#'   per-cherry frequency of sites with different methylation states using \code{get_siteFChange_cherry}. 
#'   The indices provided for islands and non-islands are checked for validity using \code{validate_structureIndices}. 
#'   Finally, the function calculates the weighted mean site frequency of methylation changes for each cherry, 
#'   separately for islands and non-islands.
#'
#' @return A data frame with one row per cherry, containing the following columns:
#'   \describe{
#'     \item{tip_names}{A character string representing the names of the two tips in the cherry, concatenated with a hyphen.}
#'     \item{tip_indices}{A character string representing the indices of the two tips in the cherry, concatenated with a hyphen.}
#'     \item{dist}{A numeric value representing the sum of the branch distances between the cherry tips.}
#'     \item{nonisland_meanFChange}{A numeric value representing the weighted mean frequency of methylation changes in non-island structures.}
#'     \item{island_meanFChange}{A numeric value representing the weighted mean frequency of methylation changes in island structures.}
#'   }
#'
#' @examples
#' # Example data setup
#' data <- list(
#' list(rep(1,10), rep(0,5), rep(1,8)), # Tip a
#' list(rep(1,10), rep(0.5,5), rep(0,8)), # Tip b
#' list(rep(1,10), rep(0.5,5), rep(0,8)), # Tip c
#' list(c(rep(0,5), rep(0.5, 5)), c(0, 0, 1, 1, 1), c(0.5, 1, rep(0, 6)))) # Tip d
#'
#' tree <- "((a:1.5,b:1.5):2,(c:2,d:2):1.5);"
#' 
#' index_islands <- c(1,3)
#' index_nonislands <- c(2)
#'
#' MeanSiteFChange_cherry(data = data, 
#'                        categorized_data = TRUE, 
#'                        tree = tree, 
#'                        index_islands = index_islands,
#'                        index_nonislands = index_nonislands)
#'
#' @export
MeanSiteFChange_cherry <- function(data, categorized_data = FALSE, tree, index_islands, index_nonislands){

  # Check input tree format and minium two tips, get tree in phylo format (ape package),
  # check input tree data format and minium number of tips, and 
  # get per-cherry frequency of sites with different methylation states for each genomic structures
  # Validate input structure indices
  tryCatch({
    siteFChange_cherry <- get_siteFChange_cherry(tree, data, categorized_data = categorized_data)
    validate_structureIndices(data, index_islands, index_nonislands)
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  # Subset the columns with frequencies (3 first columns correspond to tip_names, tip_indices and dist)
  siteFChange <- as.data.frame(siteFChange_cherry[,-c(1,2,3)])
  
  # Initiate vectors to store per cherry the mean siteFChange at islands and nonislands 
  island_meanFChange <- rep(NA, nrow(siteFChange))
  nonisland_meanFChange <- rep(NA, nrow(siteFChange))
  
  # Get the number of structures
  str_n <- length(data[[1]])
  
  # Get the number of sites per structure
  sites_n <- numeric(length = str_n)
  for(str in 1:str_n) sites_n[str] <- length(data[[1]][[str]]) 
  
  # Compute for non_islands
  if(length(index_nonislands)>0){
    nonisland_meanFChange <- apply(as.data.frame(siteFChange[,index_nonislands]),
                                   1, stats::weighted.mean, w = sites_n[index_nonislands])
  }
  
  # Compute for islands
  if(length(index_islands)>0){
    island_meanFChange <- apply(as.data.frame(siteFChange[,index_islands]),
                                1, stats::weighted.mean, w = sites_n[index_islands])
  }
  
  MeanSiteFChange_cherry  <- data.frame(
    tip_names = siteFChange_cherry$tip_names,
    tip_indices = siteFChange_cherry$tip_indices,
    dist = siteFChange_cherry$dist,
    nonisland_meanFChange = nonisland_meanFChange,
    island_meanFChange = island_meanFChange
  )
  
  MeanSiteFChange_cherry
}

#### #### #### Fitch estimation of minimum number of global methylation changes per island #### #### ####

#' Compute the Mean Methylation of CpG Islands
#'
#' This function calculates the mean methylation level for CpG islands across all tree tips.
#'
#' @param index_islands A numeric vector specifying the indices of genomic structures corresponding to islands.
#' @param data A list containing methylation states at tree tips for each genomic structure 
#'   (e.g., island/non-island). The data should be structured as \code{data[[tip]][[structure]]}, 
#'   where each tip has the same number of structures, and each structure has the same number of sites across tips.
#'
#' @return A list where each element corresponds to a tree tip and contains a numeric vector 
#'   representing the mean methylation levels for the indexed CpG islands.
#'
#' @examples
#' # Example data setup
#' 
#' data <- list(
#'   # Tip 1
#'   list(rep(1,10), rep(0,5), rep(1,8)),
#'   # Tip 2
#'   list(rep(1,10), rep(0.5,5), rep(0,8))
#' )
#' 
#' index_islands <- c(1,3)
#' 
#' get_meanMeth_islands(index_islands, data)
#'
#' @export
get_meanMeth_islands <- function(index_islands, data){
  
  tryCatch({
    
    # Validate index islands
    if(length(index_islands) == 0) stop("'index_islands' has no indices")
    validate_structureIndices(data, index_islands, index_nonislands = c())
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  # Compute the mean methylation of CpG islands
  meanMeth <- lapply(data, function(x) sapply(x[index_islands], mean))
  
  meanMeth
}


#' Categorize Global States of CpG Islands
#'
#' This function categorizes CpG islands into unmethylated, methylated, or partially methylated states 
#' based on specified thresholds.
#'
#' @param meanMeth_islands A numeric vector containing the mean methylation levels for CpG islands at each tip.
#' @param u_threshold A numeric value (0-1) defining the threshold for categorization as unmethylated.
#' @param m_threshold A numeric value (0-1) defining the threshold for categorization as methylated.
#'
#' @details The function assigns each island a state:
#'   \describe{
#'     \item{"u"}{if mean methylation lower or equal than \code{u_threshold}}
#'     \item{"m"}{if mean methylation greater or equal than \code{m_threshold}}
#'     \item{"p"}{if mean methylation is in between}
#'   }
#'
#' @return A character vector of length equal to \code{meanMeth_islands}, containing "u", "p", or "m" for each island.
#'
#' @examples
#' meanMeth_islands <- c(0.1, 0.4, 0.8)
#'
#' categorize_islandGlbSt(meanMeth_islands, 0.2, 0.8)
#'
#' @export
categorize_islandGlbSt <- function(meanMeth_islands, u_threshold, m_threshold) {
  
  # Check correct values for u and m thresholds
  if (!all(c(u_threshold, m_threshold) >= 0 & c(u_threshold, m_threshold) <= 1) || u_threshold >= m_threshold) {
    stop("Both 'u_threshold' and 'm_threshold' must be between 0 and 1, and 'u_threshold' must be smaller than 'm_threshold'.")
  }
  
  ## categorize region global states 
  categorized_state <- character()
  for(i in 1:length(meanMeth_islands)) {
    if(meanMeth_islands[[i]] <= u_threshold) {
      categorized_state[i] <- "u"
    } else {
      if(meanMeth_islands[[i]] >= m_threshold) {
        categorized_state[i] <- "m"
      } else {
        categorized_state[i] <- "p"
      }
    }
  }
  return(categorized_state)
}

#' Compute Fitch Parsimony for Methylation Categories
#'
#' This function applies Fitch parsimony to determine the minimum number of changes required for 
#' methylation categories at tree tips.
#'
#' @param tree A rooted binary tree in Newick format (character string) or as an \code{ape} phylo object. 
#'   Must have at least two tips.
#' @param meth A matrix of methylation categories at the tree tips, with rows corresponding to tips 
#'   (names matching tree tip labels) and columns corresponding to sites or structures.
#' @param input_control Logical; if \code{TRUE}, validates input consistency.
#'
#' @return A list containing:
#'   \describe{
#'     \item{\code{optStateSet}}{A list of sets of optimal states for the root at each site/structure.}
#'     \item{\code{minChange_number}}{A numeric vector indicating the minimum number of changes for each site.}
#'   }
#'
#' @examples
#' tree <- "((a:1,b:1):2,(c:2,d:2):1.5);"
#' 
#' meth <- matrix(c("u", "m", "p", "u", "p", "m", "m", "u"), 
#'                nrow=4, byrow=TRUE, dimnames=list(c("a", "b", "c", "d")))
#'                
#' compute_fitch(tree, meth)
#'
#' @export
compute_fitch <- function(tree, meth, input_control = TRUE) {
  
  if (input_control){
    
    tryCatch({
      
      # Validate tree
      phylo_tree <- validate_tree(tree)
      
      # If tree is an ape's phylo object, convert it to Newick format
      if (inherits(tree, "phylo")) {
        tree <- ape::write.tree(tree)  # Convert to Newick string
      }
      
    }, warning = function(w) {
      stop(conditionMessage(w))
    }, error = function(e) {
      stop(conditionMessage(e))
    })
    
    # Check rownames in meth matrix correspond to the tips of the tree
    if (!identical(rownames(meth), phylo_tree$tip.label)) {
      stop("Input 'meth' matrix rownames must match tree tip labels exactly, in the same order as tips from left to right in the Newick tree.")
    }
  }
  
  ## If tree corresponds to a tip set a list with 
  # [[1]] the states at the tip for each structure and
  # [[2]] initialize a vector for the counts of minimum number of changes with 0s 
  if(substr(tree,1,1)!="(") {
    ## 'tree' here corresponds to the tip name
    return(list(meth[tree, ], rep(0, dim(meth)[2])))
  }
  
  ## I tree is not a tip, subset again
  subtree <- split_newick(tree)$unit

  if(length(subtree)!=2) stop("function compute_fitch works only for rooted binary trees")
  
  # Recursively call the function to start counting from the tree tips
  L <- compute_fitch(subtree[1], meth, input_control = FALSE)
  R <- compute_fitch(subtree[2], meth, input_control = FALSE)
  
  # Save the minimum number of changes needed for the states at the two subtrees
  minchan <- L[[2]] + R[[2]]
  
  # Initialize a list to store the optimal state at the root of a each subtree
  optset <- list()
  
  # For each island
  for(i in 1:length(minchan)) {
    
    # Check whether there is a common state
    optset[[i]] <- intersect(L[[1]][[i]], R[[1]][[i]])
    
    # If they dont have the same state
    if(length(optset[[i]])==0) { 
      
      # Save both states
      optset[[i]] <- union(L[[1]][[i]], R[[1]][[i]]) 
      
      # Increase the number of minimum changes needed for that island
      minchan[i] <- minchan[i] + 1 
    } 
  }
  
  return(list(optStateSet = optset, minChange_number = minchan))
}

#' Compute Fitch Parsimony for Global Methylation States at CpG Islands
#'
#' This function categorizes CpG islands into methylation states and applies Fitch parsimony 
#' to estimate the minimum number of state changes in a phylogenetic tree.
#'
#' @param index_islands A numeric vector specifying the indices of genomic structures corresponding to islands.
#' @param data A list containing methylation states at tree tips, structured as \code{data[[tip]][[structure]]}, 
#'   where each tip has the same number of structures, and each structure has the same number of sites across tips.
#' @param tree A rooted binary tree in Newick format (character string) or as an \code{ape} phylo object. 
#'   Must have at least two tips.
#' @param u_threshold A numeric threshold value (0-1) defining the unmethylated category.
#' @param m_threshold A numeric threshold value (0-1) defining the methylated category.
#' @param testing Logical; if \code{TRUE}, returns additional intermediate data.
#'
#' @details The function first validates the input data and categorizes CpG islands using \code{categorize_islandGlbSt}. 
#'   It then structures the data into a matrix matching tree tip labels and applies \code{compute_fitch} 
#'   to infer the minimum number of changes.
#'
#' @return If \code{testing = TRUE}, returns a list containing the categorized data matrix; otherwise, 
#'   returns a numeric vector of minimum state changes.
#'
#' @examples
#' tree <- "((a:1,b:1):2,(c:2,d:2):1.5);"
#' 
#' data <- list(
#'   list(rep(1,10), rep(0,5), rep(1,8)),
#'   list(rep(1,10), rep(0.5,5), rep(0,8))
#' )
#' 
#' index_islands <- c(1,3)
#' 
#' computeFitch_islandGlbSt(index_islands, data, tree, 0.2, 0.6)
#'
#' @export
computeFitch_islandGlbSt <- function(index_islands, data, tree, u_threshold, m_threshold, testing = FALSE){
  
  tryCatch({
    
    # Validate tree
    tree <- validate_tree(tree)
    
    # Validate index islands and get mean methylation of CpG islands
    meanMeth_islands <- get_meanMeth_islands(index_islands, data)
    
    # Validate u and m thresholds and convert mean methylation to categories "u", "p", and "m"
    upmdata_list <- lapply(meanMeth_islands, categorize_islandGlbSt, u_threshold, m_threshold)
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  # Organize data in matrix with rownames as the corresponding tip labels
  upmdata  <- matrix(unlist(upmdata_list), nrow=length(tree$tip.label), byrow=TRUE)
  rownames(upmdata)<-tree$tip.label
  
  # Compute fitch to extract the minimum number of changes for the observed methylation states at the tips
  result <- compute_fitch(ape::write.tree(tree), upmdata, input_control = FALSE)$minChange_number
  
  if (testing){
    list(upmdata_list = upmdata_list,
         upmdata = upmdata)
  } else {
    result
  }
}

#' Count Methylation States
#'
#' This internal function counts the number of sites with unmethylated, partially-methylated,
#' and methylated states in a given vector.
#'
#' @param data A numeric vector with methylation values: \code{0} (unmethylated), \code{0.5} (partially-methylated),
#'   and \code{1} (methylated).
#'
#' @return An integer vector of length 3 containing counts of unmethylated, partially-methylated,
#'   and methylated sites, respectively.
count_upm <- function(data){
  counts <- table(factor(data, levels = c(0, 0.5, 1)))
  as.integer(counts)
}

#' Compare Methylation Frequencies Between Two Tips
#'
#' Performs a chi-squared test to compare the distribution of methylation states 
#' (unmethylated \code{0}, partially-methylated \code{0.5}, and methylated \code{1}) 
#' between two cherry tips. A cherry is a pair of leaf nodes (also called tips or terminal nodes) 
#' in a phylogenetic tree that share a direct common ancestor.
#'
#' @details The function uses \code{simulate.p.value = TRUE} in \code{\link[stats]{chisq.test}} 
#' to compute the p-value via Monte Carlo simulation to improve reliability 
#' regardless of whether the expected frequencies meet the assumptions of the chi-squared test 
#' (i.e., expected counts of at least 5 in each category). 
#'
#' @param tip1 A numeric vector representing methylation states (\code{0}, \code{0.5}, \code{1}) at tip 1.
#' @param tip2 A numeric vector representing methylation states (\code{0}, \code{0.5}, \code{1}) at tip 2.
#' @param testing Logical; if \code{TRUE}, returns additional intermediate data including the contingency table and test result.
#'
#' @return If \code{testing = TRUE}, returns a list with the contingency table and chi-squared test results.
#'   Otherwise, returns the p-value of the test.
#'
#' @examples
#' tip1 <- c(0, 0, 1, 0.5, 1, 0.5)
#' tip2 <- c(0, 1, 1, 0, 0.5, 0.5)
#' compare_CherryFreqs(tip1, tip2)
#'
#' @export
compare_CherryFreqs <- function(tip1, tip2, testing = FALSE){
  
  # Count the number of sites at each tip with methylation states
  # 0, 0.5, 1 (unmethylated, partially-methylated, methylated)
  tip1_upmCounts <- count_upm(tip1)
  tip2_upmCounts <- count_upm(tip2)
  
  # If they are all equal return 1
  if(all(tip1_upmCounts == tip2_upmCounts)){
    return(1)
  }
  
  # Create a matrix with both tip counts
  contingency_table <- rbind(tip1_upmCounts, tip2_upmCounts)
  
  # Check for zero marginals before running chi-squared test
  if(any(colSums(contingency_table) == 0)){
    contingency_table <- contingency_table[,-which(colSums(contingency_table)==0)]
  }
  
  # Perform the chi-squared test
  chi_sq_result <- stats::chisq.test(contingency_table, simulate.p.value = TRUE)
  
  if(testing){
    list(contingency_table = contingency_table,
         chi_sq_result = chi_sq_result)
  } else {
    # Return the p-value
    chi_sq_result$p.value
  }
}


#' Compute p-Values for Methylation Frequency Changes in Cherries
#'
#' Calculates p-values for changes in methylation frequency between pairs of cherry tips in a phylogenetic tree.
#' A cherry is a pair of leaf nodes (also called tips or terminal nodes) 
#' in a phylogenetic tree that share a direct common ancestor.
#' 
#' @details The function uses \code{simulate.p.value = TRUE} in \code{\link[stats]{chisq.test}} 
#' to compute the p-value via Monte Carlo simulation to improve reliability 
#' regardless of whether the expected frequencies meet the assumptions of the chi-squared test 
#' (i.e., expected counts of at least 5 in each category). 
#'
#' @param data A list containing methylation states at tree tips for each genomic structure (e.g., island/non-island).
#'   The data should be structured as `data[[tip]][[structure]]`, where each structure has the same number of sites across tips.
#'   The input data must be prefiltered to ensure CpG sites are represented consistently across different tips.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#'
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#' @param index_islands A numeric vector specifying the indices of islands to analyze.
#' @param tree A rooted binary tree in Newick format (character string) or as an \code{ape} phylo object with minimum 2 tips.
#' @param input_control Logical; if \code{TRUE}, validates input.
#'
#' @return A data frame containing tip pair information (first tip name, second tip name, first tip index, second tip index, distance)
#'   and one column per island with the p-values from the chi-squared tests.
#'
#' @examples
#' # Example with hypothetical tree and data structure
#' 
#' tree <- "((d:1,e:1):2,a:2);"
#' data <- list(
#'   #Tip 1
#'   list(c(rep(1,9), rep(0,1)), 
#'        c(rep(0,9), 1), 
#'        c(rep(0,9), rep(0.5,1))), 
#'   #Tip 2
#'   list(c(rep(0,9), rep(0.5,1)), 
#'        c(rep(0.5,9), 1), 
#'        c(rep(1,9), rep(0,1))), 
#'   #Tip 3
#'   list(c(rep(1,9), rep(0.5,1)), 
#'        c(rep(0.5,9), 1), 
#'        c(rep(0,9), rep(0.5,1)))) 
#' 
#' index_islands <- c(1,3)
#' 
#' pValue_CherryFreqsChange_i(data, categorized_data = TRUE, index_islands, tree)
#'
#' @export
pValue_CherryFreqsChange_i <- function(data, categorized_data = FALSE, index_islands, tree, input_control = TRUE){
  
  tryCatch({
    
    # Check input tree format and minium two tips, get tree in phylo format (ape package)
    if (input_control) tree <- validate_tree(tree)
    
    # Get cherry distances avoiding duplicate input control
    cherryDist <- get_cherryDist(tree, input_control = FALSE)
    
    # Check input tree data format and minium number of tips
    if (input_control) validate_data_cherryDist(cherryDist, data)
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
    # Validate index islands
    if (length(index_islands) == 0) stop("'index_islands' has no indices.")
    if (input_control) validate_structureIndices(data, index_islands, index_nonislands = c())
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  # Add to the dataframe columns to store the counts of significant changes
  island_indices <- paste0("island_", index_islands)
  cherryDist[island_indices] <- NA
  
  for(cherry in 1:nrow(cherryDist)){
    
    tip1_idx <- cherryDist[cherry, "first_tip_index"]
    tip2_idx <- cherryDist[cherry, "second_tip_index"]
    
    for(i in 1:length(index_islands)){
      data_tip1 <- data[[tip1_idx]][[index_islands[i]]]
      data_tip2 <- data[[tip2_idx]][[index_islands[i]]]
      cherryDist[cherry, island_indices[i]] <- compare_CherryFreqs(data_tip1, data_tip2)
    }
  }
  
  cherryDist
}

#' Mean Number of Significant Methylation Frequency Changes per Island in Cherries
#'
#' Computes the mean number of significant changes per island in phylogenetic tree cherries,
#' based on a specified p-value threshold.
#'
#' @details The function uses \code{simulate.p.value = TRUE} in \code{\link[stats]{chisq.test}} 
#' to compute the p-value via Monte Carlo simulation to improve reliability 
#' regardless of whether the expected frequencies meet the assumptions of the chi-squared test 
#' (i.e., expected counts of at least 5 in each category). 
#'
#' @param data A list containing methylation states at tree tips for each genomic structure (e.g., island/non-island).
#'   The data should be structured as `data[[tip]][[structure]]`, where each structure has the same number of sites across tips.
#'   The input data must be prefiltered to ensure CpG sites are represented consistently across different tips.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#' @param index_islands A numeric vector specifying the indices of islands to analyze.
#' @param tree A rooted binary tree in Newick format (character string) or as an \code{ape} phylo object.
#' @param pValue_threshold A numeric value between 0 and 1 that serves as the threshold for statistical significance in 
#'   the chi-squared test.
#'
#' @return A data frame containing the same information as \code{pValue_CherryFreqsChange_i},
#'   but with additional columns indicating whether p-values are below the threshold (significant changes)
#'   and the mean frequency of significant changes per island.
#'
#' @examples
#' tree <- "((d:1,e:1):2,a:2);"
#' data <- list(
#'   #Tip 1
#'   list(c(rep(1,9), rep(0,1)), 
#'        c(rep(0,9), 1), 
#'        c(rep(0,9), rep(0.5,1))), 
#'   #Tip 2
#'   list(c(rep(0,9), rep(0.5,1)), 
#'        c(rep(0.5,9), 1), 
#'        c(rep(1,9), rep(0,1))), 
#'   #Tip 3
#'   list(c(rep(1,9), rep(0.5,1)), 
#'        c(rep(0.5,9), 1), 
#'        c(rep(0,9), rep(0.5,1)))) 
#' 
#' index_islands <- c(1,3)
#' mean_CherryFreqsChange_i(data, categorized_data = TRUE,
#'                           index_islands, tree, pValue_threshold = 0.05)
#'
#' @export
mean_CherryFreqsChange_i <- function(data, categorized_data = FALSE, index_islands, tree, pValue_threshold){
  
  # Validate given threshold for pValue
  if (!(pValue_threshold > 0 & pValue_threshold < 1)) stop("pValue_threshold needs to be between 0 and 1.")
  
  tryCatch({
    # Get the pValue for the change in frequencies of u,p,m at each island at each cherry
    # Check input tree format and minium two tips, get tree in phylo format (ape package)
    # Check input tree data format and minium number of tips
    # If methylation states uncategorized, categorize methylation states
    # Validate index islands
    pValue_freqsChange <- pValue_CherryFreqsChange_i(data, 
                                                     categorized_data = categorized_data, 
                                                     index_islands, 
                                                     tree)
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })

  # Transform columns with pValues under a threshold to TRUE, else FALSE
  pValue_freqsChange[, grep("^island_", colnames(pValue_freqsChange))] <- 
    pValue_freqsChange[, grep("^island_", colnames(pValue_freqsChange))] < pValue_threshold
  
  # Compute the mean number of changes (TRUE) per island
  pValue_freqsChange$FreqsChange <- rowMeans(pValue_freqsChange[, grep("^island_", colnames(pValue_freqsChange))])
  pValue_freqsChange
}




#' Validate Data Structure Across Tips
#'
#' This function ensures that `data` follows the required nested structure 
#' `data[[tip]][[structure]]`, where:
#' - `data` is a list of at least two `tip` elements.
#' - Each `tip` is a list of `structure` elements.
#' - Each `structure` contains a numeric vector of equal length across all tips.
#'
#' @param data A list structured as `data[[tip]][[structure]]`.
#' 
#' @details Throws errors if:
#'   - `data` is not a list.
#'   - It has fewer than two tips.
#'   - Any tip is not a list.
#'   - The number of structures is inconsistent across tips.
#'   - Any structure has zero-length data at any tip.
#'   - Structures have different site lengths across tips.
validate_dataAcrossTips <- function(data) {
  if (!is.list(data)) {
    stop(
      "Error: 'data' must be a list structured as data[[tip]][[structure]]."
    )
  }
  
  num_tips <- length(data)
  if (num_tips < 2) {
    stop(
      "Error: 'data' must contain at least two tips.",
      " Found ", num_tips, 
      ". Required structure: data[[tip]][[structure]]."
    )
  }
  
  if (!all(vapply(data, is.list, logical(1)))) {
    stop(
      "Error: Each element of 'data' (data[[tip]]) must be a list of structures.",
      " Required structure: data[[tip]][[structure]]."
    )
  }
  
  str_n <- length(data[[1]])
  if (str_n < 1) {
    stop(
      "Error: Each tip in 'data' must contain at least one structure.",
      " Required structure: data[[tip]][[structure]]."
    )
  }
  
  # Ensure all tips have the same number of structures
  if (any(vapply(data, length, integer(1)) != str_n)) {
    stop(
      "Error: All tips in 'data' must have the same number of structures.",
      " Found inconsistencies in the number of structures."
    )
  }
  
  # Check structure consistency across tips
  lengths_per_structure <- matrix(nrow = num_tips, ncol = str_n)
  
  for (tip in seq_len(num_tips)) {
    lengths_per_structure[tip, ] <- vapply(data[[tip]], length, integer(1))
  }
  
  if (any(lengths_per_structure == 0)) {
    stop(
      "Error: Each structure in 'data' must contain non-empty numeric vectors.",
      " Some structures have zero-length data at one or more tips.",
      " Required: data[[tip]][[structure]] with consistent structure lengths across tips."
    )
  }
  
  if (any(apply(lengths_per_structure, 2, function(x) length(unique(x)) > 1))) {
    stop(
      "Error: All structures in 'data' must have the same number of sites across tips.",
      " Found inconsistencies in site lengths.",
      " Required: data[[tip]][[structure]] with consistent structure lengths across tips."
    )
  }
}


#' Categorize Methylation Frequencies Based on Thresholds
#'
#' This function categorizes the values in `data[[tip]][[structure]]` into three categories:
#' - `0` for unmethylated sites, where values are smaller or equal to `u_threshold`.
#' - `0.5` for partially methylated sites, where values are between `u_threshold` and `m_threshold`.
#' - `1` for methylated sites, where values are larger or equal to `m_threshold`.
#'
#' If any value in `data[[tip]][[structure]]` is outside these categories, it is transformed based on the given thresholds.
#'
#' @param data A list structured as `data[[tip]][[structure]]`, where `tip` corresponds to tree tips, and `structure`
#'   corresponds to each genomic structure (e.g., island/non-island).
#' @param u_threshold A numeric value representing the upper bound for values to be classified as unmethylated (`0`). Default 0.2.
#' @param m_threshold A numeric value representing the lower bound for values to be classified as methylated (`1`). Default 0.8.
#'
#' @return A transformed version of `data` where each value is categorized as `0` (unmethylated), `0.5` (partially methylated),
#'   or `1` (methylated).
#'
#' @examples
#' data <- list(
#' list(c(0.1, 0.2, 0.02), c(0.05, 0.25, 0.15)), # tip 1
#' list(c(0.01, 0.7, 0.85), c(0.3, 0.1, 0.98)) # tip 2
#' )
#' 
#' transformed_data <- categorize_siteMethSt(data, u_threshold = 0.2, m_threshold = 0.8)
#' @export
categorize_siteMethSt <- function(data, u_threshold = 0.2, m_threshold = 0.8) {
  
  # Iterate through all tips
  for (tip in seq_along(data)) {
    # Iterate through all structures in each tip
    for (structure in seq_along(data[[tip]])) {
      # Get the current structure
      current_structure <- data[[tip]][[structure]]
      
      # Check if any value is outside the valid range [0, 1]
      if (any(current_structure < 0 | current_structure > 1)) {
        stop("All values in data must be between 0 and 1 as they represent methylation frequencies.")
      }
      
      # Check if all values are already categorized as 0, 0.5, or 1
      if (!all(current_structure %in% c(0, 0.5, 1))) {
        # Transform values based on the thresholds into 0 (unmethylated), 0.5 (partially methylated), or 1 (methylated)
        data[[tip]][[structure]] <- ifelse(current_structure <= u_threshold, 0, 
                                           ifelse(current_structure >= m_threshold, 1, 0.5))
      }
    }
  }
  
  return(data)
}






#' Mean Number of Significant Frequency Changes per Island Across all Tree Tips
#'
#' This function analyzes the frequency changes of methylation states (unmethylated, partially methylated, methylated)
#' across tree tips for a given set of islands. It performs a chi-squared test for each island to check for significant 
#' changes in frequencies across tips and returns the proportion of islands showing significant changes.
#'
#' @details The function uses \code{simulate.p.value = TRUE} in \code{\link[stats]{chisq.test}} 
#' to compute the p-value via Monte Carlo simulation to improve reliability 
#' regardless of whether the expected frequencies meet the assumptions of the chi-squared test 
#' (i.e., expected counts of at least 5 in each category). 
#'
#' @param tree A phylogenetic tree object, typically of class `phylo`, containing tip labels.
#' @param data A list containing methylation states at tree tips for each genomic structure (e.g., island/non-island).
#'   The data should be structured as `data[[tip]][[structure]]`, where each structure has the same number of sites across tips.
#'   The input data must be prefiltered to ensure CpG sites are represented consistently across different tips.
#'   Each element contains the methylation states at the sites in a given tip and structure
#'   represented as 0, 0.5 or 1 (for unmethylated, partially-methylated and methylated). 
#'   If methylation states are not represented as 0, 0.5, 1 they are categorized
#'   as 0 when value equal or under 0.2
#'   0.5 when value between 0.2 and 0.8
#'   and 1 when value over 0.8.
#'   For customized categorization thresholds use \code{categorize_siteMethSt}
#' @param categorized_data Logical defaulted to FALSE. 
#'   TRUE to skip redundant categorization when methylation states are represented as 0, 0.5, and 1.
#' @param index_islands A vector of indices of genomic structures corresponding to islands in data.
#' @param pValue_threshold A numeric value between 0 and 1 that serves as the threshold for statistical significance in 
#'   the chi-squared test.
#' @param testing Logical defaulted to FALSE. TRUE for testing output.
#' 
#' @return A numeric value representing the mean proportion of islands with significant frequency changes across tips.
#' 
#' @details Throws errors if:
#' - The `tree` is not valid.
#' - `data` is not structured correctly across tips.
#' - `index_islands` is empty.
#' - `pValue_threshold` is not between 0 and 1.
#' 
#' @examples
#' # Example of usage:
#' 
#' tree <- "((d:1,e:1):2,a:2);"
#' 
#' data <- list(
#'   #Tip 1
#'   list(c(rep(1,9), rep(0,1)), 
#'        c(rep(0,9), 1), 
#'        c(rep(0,9), rep(0.5,1))), 
#'   #Tip 2
#'   list(c(rep(1,9), rep(0.5,1)), 
#'        c(rep(0.5,9), 1), 
#'        c(rep(1,9), rep(0,1))), 
#'   #Tip 3
#'   list(c(rep(1,9), rep(0.5,1)), 
#'        c(rep(0.5,9), 1), 
#'        c(rep(0,9), rep(0.5,1)))) 
#'        
#' index_islands <- c(1,3)
#' 
#' 
#' mean_TreeFreqsChange_i(tree, 
#'                        data, categorized_data = TRUE,
#'                        index_islands, 
#'                        pValue_threshold = 0.05)
#' 
#' @export
mean_TreeFreqsChange_i <- function(tree, data, categorized_data = FALSE, index_islands, pValue_threshold, testing=FALSE) {
  
  tryCatch({
    # Validate tree
    tree <- validate_tree(tree)
    
    # Validate data across tips
    validate_dataAcrossTips(data)
    
    # Categorize methylation states
    if(!categorized_data) data <- categorize_siteMethSt(data)
    
    # Validate index islands
    if (length(index_islands) == 0) stop("'index_islands' has no indices.")
    validate_structureIndices(data, index_islands, index_nonislands = c())
    
    # Validate given threshold for pValue
    if (!(pValue_threshold > 0 & pValue_threshold < 1)) stop("pValue_threshold needs to be between 0 and 1.")
    
  }, warning = function(w) {
    stop(conditionMessage(w))
  }, error = function(e) {
    stop(conditionMessage(e))
  })
  
  # Get the number of tree tips
  n_tips <- length(tree$tip.label)
  
  # Set a vector to store the pValues
  pValues <- c()
  
  # Set a list to store the contingency tables when testing
  if(testing){
    island_upmCounts_list <- list()
  }
  
  # For each island
  for (i in 1:length(index_islands)) {
    # Set a list to store the counts of the number of sites at each tip
    # with methylation states 0, 0.5, 1 (unmethylated, partially-methylated, methylated)
    island_upmCounts <- list()
    
    for (tip in 1:n_tips) {
      island_upmCounts[[tip]] <- count_upm(data[[tip]][[index_islands[i]]])
    }
    
    # Save the island upm counts as a matrix
    island_upmCounts <- do.call(rbind, island_upmCounts)
    
    # If they are all equal save as p-value 1
    if(nrow(unique(island_upmCounts))==1){
      
      pValues[i] <- 1
      
      #Save as NA the contingency table when testing
      if(testing){
        island_upmCounts_list[[i]] <- NA
      }
      
    } else {
      
      # Check for zero marginals before running chi-squared test
      if(any(colSums(island_upmCounts) == 0)){
        
        island_upmCounts <- island_upmCounts[,-which(colSums(island_upmCounts)==0)]
        
        #Save the contingency table when testing
        if(testing){
          island_upmCounts_list[[i]] <- island_upmCounts
        }
      }
      
      # Perform the chi-squared test and save the island's pValue
      pValues[i] <- stats::chisq.test(island_upmCounts, simulate.p.value = TRUE)$p.value
    }
    
  }
  
  # Get a logical vector for island significant changes in freqs across tips
  signifFreqChange <- pValues < pValue_threshold
  
  if(testing){
    list(pValues = pValues,
         island_upmCounts_list = island_upmCounts_list)
  } else {
    
    # Return the mean number of changes observed across tips per island
    mean(signifFreqChange)
    
  }
}











