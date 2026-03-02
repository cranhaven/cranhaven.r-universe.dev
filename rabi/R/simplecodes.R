#' Simple color coding scheme generator
#'
#' Creates a simple color (or symbol) coding scheme used to mark and identify individual animals. The sum of each IDs numeric sequence is a multiple of the number of colors used in the scheme. Even if one marking is removed, the entire ID code can be reconstructed.
#'
#' @param total.length the number of unique positions to be marked on the animal. (This can be thought of as the total number of positions on which color bands or paint marks will be applied.)
#' @param alphabet an integer representing the 'alphabet size.' This is the number of unique markings (think different paint colors, symbols, or varieties of bands) at your disposal.
#' @param available.colors an optional list of strings that contains the names of the unique markings which compose the given 'alphabet' (e.g. "blue", "red", "yellow", etc.). If left blank, the mapping can be done at any later time using \code{\link{codes_to_colors}}. Additionally, the length of this list must match the 'alphabet size' given above.
#' @return  a list containing the maximum possible number of unique ID codes that fit the provided parameters.
#'
#'If an appropriate argument for \code{available.colors} is provided, each code will be a sequence of strings, otherwise, each code will be a sequence of numeric values.
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @references Burchill, A. T., & Pavlic, T. P. (2019). Dude, where's my mark? Creating robust animal identification schemes informed by communication theory. \emph{Animal Behaviour}, 154, 203-208. \href{https://doi.org/10.1016/j.anbehav.2019.05.013}{doi:10.1016/j.anbehav.2019.05.013}
#' @seealso \code{\link{rs_IDs}}, \code{\link{brute_IDs}}.
#' @examples
#' total.length <- 4  #we have four positions to mark
#' alphabet <- 5      #and we currently have five types of paint in stock
#'
#'  #Generate codes where the sum of the sequence is a multiple of five
#' simple_IDs(total.length, alphabet)
#'
#'  #Let's make those into human-readable color sequences
#' color.names <- c("blue","red","green","pink","cyan")
#' simple_IDs(total.length, alphabet, available.colors = color.names)
#'
#' @export
#' @importFrom methods is
#'


simple_IDs <- function(total.length, alphabet, available.colors = NULL){

  if (missing(alphabet)) {
    stop("Error: you need to enter an 'alphabet size,' e.g. the number of paint colors you have")
  }
  if (missing(total.length)) {
    stop("Error: you need to enter the total length of the ID, e.g. how many color bands or paint drops on each organism")
  }

  perms <- rep(list(seq_len(alphabet)),total.length - 1 )
  df <- as.matrix(expand.grid(perms)) - 1
  df <- cbind(df,apply(df, 1, function(x) alphabet - (sum(x) %% alphabet)))
  df[df == alphabet] <- 0
  df <- split(df, 1:nrow(df))
  names(df) <- NULL
  df <- codes_to_colors(df, available.colors)
  if (is(df[[1]],"numeric")) message(paste0("Each ID sequence sums to a multiple of ", alphabet, "."))
  return(df)
}
