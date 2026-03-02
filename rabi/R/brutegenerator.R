#' Brute force color coding scheme generator
#'
#' Generates "color" coding schemes used to mark and identify individual animals. The codes are robust to an arbitrary number of partial erasures. This method uses a sloppy, very slow, stochastic brute force method.
#'
#'
#' @param total.length the number of unique positions to be marked on the animal. (This can be thought of as the total number of positions on which color bands or paint marks will be applied.)
#' @param redundancy the number of erasures that can occur without disrupting surety of unique identification. This value determines how robust the scheme is to erasures.
#' @param alphabet an integer representing the 'alphabet size.' This is the number of unique markings (think different paint colors, symbols, or varieties of bands) at your disposal.Note: unlike the Reed-Solomon inspired function, \code{\link{rs_IDs}}, this function can take non-prime values.
#' @param num.tries the number of iterations that will be run before choosing the best option. Increasing this number increases the running time.
#' @param available.colors an optional list of strings that contains the names of the unique markings which compose the given 'alphabet' (e.g. "blue", "red", "yellow", etc.). If left blank, the mapping can be done at any later time using \code{\link{codes_to_colors}}. Additionally, the length of this list must match the 'alphabet size' given above.
#'
#' @details This function generates the list of all possible unique ID codes given the 'alphabet size' (\code{alphabet}) and the number of unique positions available for marking (\code{total.length}). The list of combinations is then iteratively pruned down until the required robustness has been reached; the \href{https://en.wikipedia.org/wiki/Hamming_distance}{distance} between any two ID codes must greater than the value specified in \code{redundancy}.
#'
#' However, the iterative pruning is done randomly, so it is likely that resulting list of codes does not contain the maximum possible number of robust codes. Thus, the process is repeated multiple times (\code{num.tries}) and the list that contains the largest number of robust codes is kept and returned.
#'
#'
#' @return a list of unique ID codes that fit the provided parameters.
#'
#' If an appropriate argument for \code{available.colors} is provided, each code will be a sequence of strings, otherwise, each code will be a sequence of numeric values.
#'
#' @note  the \code{\link{rs_IDs}} function always generates the maximum number of unique codes per scheme. However, \code{\link{rs_IDs}} suffers from certain limitations that \code{\link{brute_IDs}} does not: it requires \code{alphabet} to be a prime number, \code{total.length} to be less than or equal to \code{alphabet}, etc.
#'
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @references Burchill, A. T., & Pavlic, T. P. (2019). Dude, where's my mark? Creating robust animal identification schemes informed by communication theory. \emph{Animal Behaviour}, 154, 203-208. \href{https://doi.org/10.1016/j.anbehav.2019.05.013}{doi:10.1016/j.anbehav.2019.05.013}
#' @seealso \code{\link{rs_IDs}}, \code{\link{tweaked_IDs}}, \code{\link{simple_IDs}}. Also see the vignette \href{../doc/loosebirdtag.html}{\code{loosebirdtag}} for demonstrations and additional uses.
#'
#' @examples
#' total.length <- 6  #we have six positions to mark,
#' redundancy <- 2    #we want surety even with two erasures,
#' alphabet <- 4      #and we currently have five types of paint in stock
#'
#'  #This gives a warning because rs_IDs() only accepts
#'  #a pretty constrained set of parameters
#' codes <- rs_IDs(total.length, redundancy, alphabet)
#' length(codes)
#'
#'  #However, we can do it with brute_IDs() to get more unique IDs
#' codes <- brute_IDs(total.length, redundancy, alphabet, num.tries = 1)
#' length(codes)
#'
#'  #Let's make those into human-readable color sequences
#' color.names <- c("blue","red","green","pink","yellow-with-a-stripe")
#' color.codes <- codes_to_colors(codes, color.names)
#'
#'
#'
#' @export
#' @importFrom stringdist seq_distmatrix
#' @importFrom methods is

brute_IDs <- function(total.length, redundancy, alphabet, num.tries = 10, available.colors = NULL) {

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
  if (!is(num.tries, "numeric")) {
    stop(paste0("Error: the variable 'num.tries' must be of the class 'numeric,' not '", class(num.tries),".'"))
  }

  tester <- function(total.length, redundancy, alphabet) {
     #generate all sequences and turn into a list
    perms <- rep(list(seq_len(alphabet)),total.length)
    combos <- as.matrix(expand.grid(perms)) - 1
    combo.list <- split(combos, 1:nrow(combos))
    names(combo.list) <- NULL
     #pick a random sequence and start making the "safe" list with it
    x <- sample(1:length(combo.list), 1)
    new.combs <- combo.list[x]
    names(new.combs) <- NULL
     #remove everything too similar to the chosen sequence from the old list
    combo.list <- combo.list[stringdist::seq_distmatrix(combo.list, new.combs, method = "hamming")[, length(new.combs)] > redundancy]
    names(combo.list) <- 1:length(combo.list)
     #do this again and again until everything is removed
    while (length(combo.list) > 0) {
      x <- sample(1:length(combo.list), 1)
      new.combs[length(new.combs) + 1] <- (combo.list[x])
      combo.list <- combo.list[stringdist::seq_distmatrix(combo.list, new.combs, method = "hamming")[, length(new.combs)] > redundancy]
      if (length(combo.list) != 0) {
        names(combo.list) <- 1:length(combo.list)
      }
    }
    # print(length(new.combs)) table(unlist(seq_distmatrix(new.combs,new.combs,method='hamming')))
    return(new.combs)
  }
   #run through the function several times and keep the best
  temp1 <- NULL
  temp2 <- 0
  for (i in 1:num.tries) {
    temp1 <- invisible(tester(total.length, redundancy, alphabet))
    if (length(temp1) > length(temp2))
      temp2 <- temp1
  }
  temp2 <- codes_to_colors(temp2, available.colors)
  return(temp2)
}
