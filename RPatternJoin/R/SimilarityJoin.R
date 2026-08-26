.checkStringList <- function(strings) {
  invalid_idx <- which(!nzchar(strings) | !grepl("^[A-Za-z]+$", strings))
  invalid_strings <- strings[invalid_idx]
  if (length(invalid_strings) > 0) {
    warning(
      "Warning: Input vector contains strings: ",
      paste(invalid_strings, collapse = ", "),
      "\nThese string contains with not [a-zA-Z] letters: ",
      "\nTo disable this warning, change special_chars_warning to FALSE."
    )
  }
}

.similarityJoinCheckArgs <- function(
  strings,
  cutoff,
  metric,
  method,
  drop_deg_one,
  special_chars,
  output_format
) {
  if (!is.character(strings) || length(strings) == 0)
    stop("Error: `strings` must be a non-empty character vector.")
  if (special_chars)
    .checkStringList(strings)
  if (!is.numeric(cutoff) || !any(cutoff == c(0, 1, 2)))
    stop("Error: `cutoff` must be a numeric from (0, 1, 2).")
  if (!is.character(metric) || !any(metric == c("Hamming", "Levenshtein")))
    stop("Error: `metric` must be a string from (\"Hamming\", \"Levenshtein\").")
  if (!is.character(method) || !any(method == c("pattern", "semi_pattern", "partition_pattern")))
    stop("Error: `method` must be a string from (\"pattern\", \"semi_pattern\", \"partition_pattern\").")
  if (!is.logical(drop_deg_one))
    stop("Error: `drop_deg_one` must be a logical.")
  if (!is.character(output_format) || !any(output_format == c("adj_matrix", "adj_pairs")))
    stop("Error: `output_format` must be a string from (\"adj_matrix\", \"list\").")
  if (output_format == "adj_pairs" && drop_deg_one == TRUE)
    warning("drop_deg_one argument is ignored when output_format is 'adj_pairs'.")
}

#' Generate Example Strings with Edit Distance 1
#'
#' This function generates a random list of `num_strings = 5n` strings
#' such that each of `n` strings has one duplicate, one string with a deleted letter,
#' one string with an inserted letter, and one string with a substituted letter.
#'
#' @param avg_len Average length of the strings.
#' @param num_strings Number of strings to generate.
#' @return A character vector of generated strings.
#' @seealso \code{\link[=similarityJoin]{similarityJoin}}
#' @export
edit_dist1_example <- function(avg_len = 25, num_strings = 5000) {
  n <- ceiling(num_strings / 5)
  generate_string <- function(avg_len_ = avg_len) {
    letters <- c(letters)
    length <- rpois(1, lambda = avg_len_)
    paste0(sample(letters, size = length, replace = TRUE, ), collapse = "")
  }
  strings <- replicate(n, generate_string(avg_len))
  rnd_idx <- sample(1:avg_len / 2)
  strings_del <- unname(vapply(
    strings, 
    function(S) { paste0(substr(S, 1, rnd_idx), substr(S, rnd_idx + 2, nchar(S))) }, 
    FUN.VALUE = '1'))
  strings_ins <- unname(vapply(
    strings, 
    function(S) { paste0(substr(S, 1, rnd_idx), "a", substr(S, rnd_idx + 1, nchar(S))) }, 
    FUN.VALUE = '1'))
  strings_subst <- unname(vapply(
    strings, 
    function(S) { paste0(substr(S, 1, rnd_idx), "z", substr(S, rnd_idx + 2, nchar(S))) }, 
    FUN.VALUE = '1'))
  strings <- c(strings, strings_del, strings_ins, strings_subst, strings)
  return(strings)
}



#' Build Adjacency Matrix
#'
#' @param strings Input vector of strings.
#' To avoid hidden errors, the function will give a warning if strings contain characters not in the English alphabet. 
#' To disable this warning, change `special_chars` to `FALSE`.
#' @param cutoff Cutoff: `0`,`1`,`2`.
#' The function will search all pairs of strings with edit distance less than or equal to the `cutoff`.
#' @param metric Edit distance type: `Hamming`, `Levenshtein`.
#' @param method Method: `partition_pattern`, `semi_pattern`, `pattern`.
#' This parameter determines what algorithm will be used for similarity join.
#' Methods will differ in time and space complexity, but produce the same output.  
#' By default, we recommend using `partition_pattern`, since it is the most memory efficient.
#' @param drop_deg_one Drop isolated strings: `TRUE`, `FALSE`. Works only for `output_format`=`adj_matrix`. 
#' The default is `FALSE`.
#' @param special_chars Enable check for special characters in strings: `TRUE`, `FALSE`. 
#' The default is `TRUE`.
#' @param output_format Output format: `adj_matrix`, `adj_pairs`.
#' The default is `adj_matrix`.
#'
#' @return If `output_format = adj_pairs` - 2-column matrix where
#' each row is a pair of indices of strings with an edit distance \eqn{\leq} `cutoff`. \cr
#' If `output_format = adj_matrix` - the same output is presented as a sparse adjacency matrix with corresponding strings and 
#' their indices in the original vector are stored in dimnames of the adjacency matrix. \cr
#' I.e. (`adj_matrix[i, j]=1`) \eqn{\Leftrightarrow} distance between `dimnames(adj_matrix)[[1]][i]` and `dimnames(adj_matrix)[[1]][i]` is  \eqn{\leq} `cutoff`. \cr
#' If `drop_deg_one` is `FALSE`, then `dimnames(adj_matrix)[[1]] = strings` and `dimnames(adj_matrix)[[2]]=1:length(strings)`. \cr
#' Otherwise, `dimnames(adj_matrix)[[1]] = strings` without isolated strings and` dimnames(adj_matrix)[[2]]`=original indices 
#' of strings in `dimnames(adj_matrix)[[1]]` (original = index in input `strings` vector).
#' 
#' @seealso \code{\link[=edit_dist1_example]{edit_dist1_example}}
#' @export
#'
#' @examples
#' library(RPatternJoin)
#' library(Matrix)
#'
#' ## Example 1
#' # Consider the following example with small similar words:
#' strings <- c("cat", "ecast", "bat", "cats", "chat")
#' # Let's find all pairs s.t. strings can be modified
#' # to each other with at most 2 substitutions.
#' # For this we choose our metric to be Hamming distance and cutoff to be 2.
#' metric <- "Hamming"
#' cutoff <- 2
#' # By default we use 'partition_pattern' method
#' # since it is the most memory efficient.
#' method <- "partition_pattern"
#' # Let's output the result as an adjacency matrix.
#' output_format <- "adj_matrix"
#' drop_deg_one <- TRUE
#'
#' similarityJoin(
#'   strings, cutoff, metric,
#'   method = method, drop_deg_one = drop_deg_one)
#' # 3 x 3 sparse Matrix of class "dgCMatrix"
#' #   cat bat cats
#' # 1   1   1    1
#' # 3   1   1    1
#' # 4   1   1    1
#'
#'
#' ## Example 2
#' # On the same strings, let's calculate pairs of strings with edit distance \eqn{\leq} 1.
#' cutoff <- 1
#' metric <- "Levenshtein"
#' # Let's output the result as an adjacency matrix, but drop strings without any connections.
#' drop_deg_one <- FALSE
#'
#' similarityJoin(
#'   strings, cutoff, metric,
#'   method = method, drop_deg_one = drop_deg_one)
#' #   cat ecast bat cats chat
#' # 1   1     .   1    1    1
#' # 2   .     1   .    .    .
#' # 3   1     .   1    .    .
#' # 4   1     .   .    1    .
#' # 5   1     .   .    .    1
#'
#'
#' ## Example 3
#' # Now let's simulate a larger example.
#'
#' # The `edit_dist1_example` function generate a random list
#' # of `num_strings` strings with the average string length=`avg_len`.
#' strings <- edit_dist1_example(avg_len = 25, num_strings = 5000)
#'
#' # Firstly let's do it with `stringdist` package.
#' \donttest{
#' library(stringdist)
#' system.time({
#'   which(stringdist::stringdistmatrix(strings, strings, "lv") <= 1, arr.ind = TRUE)
#' })["elapsed"]
#' # Runtime on macOS machine with 2.2 GHz i7 processor and 16GB of DDR4 RAM:
#' # elapsed
#' # 63.773
#' }
#'
#' # Now let's do it with similarityJoin function.
#' system.time({
#'   similarityJoin(strings, 1, "Levenshtein", output_format = "adj_pairs")
#' })["elapsed"]
#' # Runtime on the same machine:
#' # elapsed
#' # 0.105
similarityJoin <- function(
  strings,
  cutoff,
  metric,
  method = "partition_pattern",
  drop_deg_one = FALSE,
  special_chars = TRUE,
  output_format = "adj_matrix"
) {
  .similarityJoinCheckArgs(
    strings, cutoff, metric, method, drop_deg_one, special_chars, output_format
  )
  metric <- substr(metric, 1, 1)
  result <- .similarityJoin(
    strings, cutoff, metric, method, drop_deg_one, output_format
  )
  if (output_format == "adj_matrix") {
    adj_matrix <- result$adj_matrix
    non_triv_ids <- result$non_triv_ids
    dimnames(adj_matrix)[[1]] <- non_triv_ids
    dimnames(adj_matrix)[[2]] <- strings[non_triv_ids]
    return(adj_matrix)
  } else if (output_format == "adj_pairs") {
    return(matrix(result$adj_pairs, ncol = 2, byrow = TRUE))
  }
}