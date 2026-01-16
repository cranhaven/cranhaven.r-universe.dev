#' Calculate diversity indices for a sample
#'
#' This is the main function to compute barcode diversity indices for a given sample.
#' It calculates three common diversity measures: species richness (q = 0),
#' Shannon diversity (q = 1), and dominance-based diversity (q = infinity).
#'
#' Internally, the function calls:
#' - format_sample() to reshape the data
#' - calculate_q_0(), calculate_q_1(), and calculate_q_inf() to compute each diversity index
#'
#' @param input_data A data frame with columns ID, Time, and Reads.
#'   Represents barcode counts per lineage at each time point.
#'
#' @return A data frame containing three diversity indices over time:
#'   q_0 (richness), q_1 (Shannon), and q_inf (dominance).
#' @export
#' @name calculateDiversity
#' 
#' @examples
#' # Load demo barcode count data (installed with the package)
#' demo_file <- system.file("extdata", "demo_input.csv", package = "doblin")
#' input_dataframe <- readr::read_csv(demo_file, show_col_types = FALSE)
#'
#' # Calculate diversity indices over time
#' diversity_df <- calculate_diversity(input_dataframe)

calculate_diversity <- function(input_data) {
  generations <- format_sample(input_data)
  m <- as.matrix(generations[, -1])
  mat <- as.data.frame(sweep(m, 2, colSums(m, na.rm = TRUE), `/`))  # Normalize by column sums
  mat$ID <- generations$X1
  
  q0 <- calculate_q_0(mat)
  q1 <- calculate_q_1(mat)
  qinf <- calculate_q_inf(mat)
  
  qall <- cbind(q0, q1, qinf)
  qall$Generations <- as.double(row.names(qall))
  return(qall)
}



#################
#' Format input data for diversity calculation
#'
#' Reshapes a long-format data frame into wide format, with lineage IDs as rows
#' and time points as columns. Replaces missing values with zeros.
#'
#' @param sample A data frame with columns ID, Time, and Reads.
#'
#' @return A wide-format data frame suitable for diversity calculations.
#' @export
#' @rdname calculateDiversity

format_sample <- function(sample){
  casted = reshape2::dcast(sample, ID ~ Time, value.var = 'Reads')

  casted[is.na(casted)] <- 0
  return(casted)
}

#################
#' Compute species richness (q = 0)
#'
#' Calculates the number of lineages with nonzero frequency at each time point.
#'
#' @param mat A matrix of relative abundances, with IDs as rows and time points as columns.
#'
#' @return A data frame with one column: q_0.
#' @export
#' @rdname calculateDiversity

calculate_q_0 <- function(mat) {
  matbool = mat
  matbool[] = TRUE
  matbool[mat==0] <- FALSE
  # Sum of clusters with nonzero frequency for each timepoint
  q_0 = as.data.frame(colSums(matbool))
  colnames(q_0)="q_0"
  return(q_0)
}

#################
#' Compute Shannon diversity (q = 1)
#'
#' Calculates the Shannon entropy at each time point and returns its exponential form.
#' This measure considers both the number and evenness of lineages.
#'
#' @param mat A matrix of relative abundances, with IDs as rows and time points as columns.
#'
#' @return A data frame with one column: q_1.
#' @export
#' @rdname calculateDiversity

calculate_q_1 <- function(mat) {
  matbool = mat
  matbool[] = TRUE
  matbool[mat==0] <- FALSE
  q_1 = as.data.frame(exp(sapply(mat, function(x) entropy::entropy.empirical(x,unit = "log"))))
  colnames(q_1)="q_1"
  return(q_1)
}

#################
#' Compute dominance-based diversity (q = infinity)
#'
#' Calculates the reciprocal of the most abundant lineage's frequency at each time point.
#' This measure reflects the dominance of the most frequent lineage.
#'
#' @param mat A matrix of relative abundances, with IDs as rows and time points as columns.
#'
#' @return A data frame with one column: q_inf.
#' @export
#' @rdname calculateDiversity

calculate_q_inf <- function(mat) {
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  q_inf = as.data.frame(1/colMax(mat))
  colnames(q_inf)="q_inf"
  return(q_inf)
}


