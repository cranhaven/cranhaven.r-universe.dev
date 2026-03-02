#' Polynomial color coding scheme generator
#'
#' Creates color (or symbol) coding schemes used to mark and identify individual animals using polynomial oversampling based on Reed-Solomon error-correction codes. The codes are robust to an arbitrary number of color-slot erasures.
#'
#' @param total.length the number of unique positions to be marked on the animal. (This can be thought of as the total number of positions on which color bands or paint marks will be applied.) Note: Reed-Solomon coding requires the total length of the ID to be less than or equal to the value of \code{alphabet}.
#' @param redundancy the number of erasures that can occur without disrupting surety of unique identification. This value determines how robust the scheme is to erasures.
#' @param alphabet an integer representing the 'alphabet size.' This is the number of unique markings (think different paint colors, symbols, or varieties of bands) at your disposal. Note: Reed-Solomon coding requires this value to be a prime number. If a non-prime is entered, the function will automatically adjust it to the nearest previous prime.
#' @param available.colors an optional list of strings that contains the names of the unique markings which compose the given 'alphabet' (e.g. "blue", "red", "yellow", etc.). If left blank, the mapping can be done at any later time using \code{\link{codes_to_colors}}. Additionally, the length of this list must match the 'alphabet size' given above.
#'
#' @return a list containing the maximum possible number of unique ID codes that fit the provided parameters.
#' @references For information on \href{https://en.wikipedia.org/wiki/Reed-Solomon_error_correction}{Reed-Solomon error correction}.
#' For information on \href{https://en.wikipedia.org/wiki/Erasure_code#Polynomial_oversampling}{polynomial oversampling}.
#'
#' Burchill, A. T., & Pavlic, T. P. (2019). Dude, where's my mark? Creating robust animal identification schemes informed by communication theory. \emph{Animal Behaviour}, 154, 203-208. \href{https://doi.org/10.1016/j.anbehav.2019.05.013}{doi:10.1016/j.anbehav.2019.05.013}
#'
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @seealso \code{\link{brute_IDs}}, \code{\link{tweaked_IDs}}, \code{\link{simple_IDs}}. See the vignette \href{../doc/loosebirdtag.html}{\code{loosebirdtag}} for demonstrations and additional uses. Run \code{\link{exampleGUI}} for a more user-friendly Shiny GUI version of the function.
#'
#' If an appropriate argument for \code{available.colors} is provided, each code will be a sequence of strings, otherwise, each code will be a sequence of numeric values.
#'
#' @examples
#' total.length <- 6  #we have six positions to mark,
#' redundancy <- 2    #we want surety even with two erasures,
#' alphabet <- 5      #and we currently have five types of paint in stock
#'
#'  #This gives a warning because rs_IDs() doesn't
#'  #allow 'total.length' to be larger than 'alphabet'
#' codes <- rs_IDs(total.length, redundancy, alphabet)
#' length(codes)
#'
#'  #Now the output should be the same as above, but no warning is issued.
#' codes <- rs_IDs(total.length = 5, redundancy, alphabet)
#' length(codes)
#'
#'  #Let's make those into human-readable color sequences
#' color.names <- c("blue","red","pink-striped-orange", "yellow", "green")
#' codes_to_colors(codes, color.names)
#'
#' @export
#' @importFrom polynom polynomial
#' @importFrom numbers isPrime previousPrime
#' @importFrom stats predict
#' @importFrom methods is


rs_IDs <- function(total.length, redundancy, alphabet, available.colors = NULL) {

  #The annoying "are your inputs valid?" error checking---------------

  if (missing(alphabet)) {
    stop("Error: you need to enter an 'alphabet size,' e.g. the number of paint colors you have")
  }
  if (missing(total.length)) {
    stop("Error: you need to enter the total length of the ID, e.g. how many color bands or paint drops on each organism")
  }
  if (missing(redundancy)) {
    stop("Error: you need specify to how many erasure events the IDs should be robust. Note, an increase in robustness requires an increase in the total length of the ID. ")
  }
  if (redundancy >= total.length || redundancy == 0) {
    stop("Error: the code must be robust to at least one erasure. It also cannot be robust to a number of positions equal to or greater than the total length.")
  }
  if (!numbers::isPrime(alphabet)) {
    warning(paste0("NOTE: Reed-Solomon codes require the 'alphabet size' (e.g. the number of paint colors you have) to be a prime number. Automatically adjusting to use an alphabet size ",numbers::previousPrime(alphabet)," instead of the entered value of ", alphabet, "."))
    alphabet <- numbers::previousPrime(alphabet)
  }
  if (total.length > alphabet) {
    warning(paste0("NOTE: Reed-Solomon coding requires the total length of the ID to be less than or equal to the size of the 'alphabet' (e.g. the number of paint colors you have). 'total.length' being changed to ", alphabet, " instead."))
    total.length <- alphabet
  }


  #okay now let's get to the actual code!----------------------------------

  #creates a matrix full of all possible messages of right length
  message <- total.length - redundancy

  perms <- rep(list(seq_len(alphabet)), message )
  combos <- as.matrix(expand.grid(perms))
  # we want to go from 0 to (max - 1) because of modulo
  combos <- combos - 1
  #creates a matrix to fill with the codewords
  codes <- matrix(data=NA, nrow = dim(combos)[1], ncol = total.length)
  #dynamically creates a polynomial from the permutations such as a + b*x + c*x^2 + d*x^3 ...
  #then evaluates it at x = the column number minus one (0,1,2,3...)
  for (i in seq_len(total.length)) codes[, i] <-
    apply(combos, 1, function(x) stats::predict(polynomial(x), i - 1) %% alphabet)

  codes <- split(codes, 1:nrow(codes))
  names(codes) <- NULL
  codes <- codes_to_colors(codes, available.colors)
  return(codes)
}

