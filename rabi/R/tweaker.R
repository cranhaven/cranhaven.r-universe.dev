#' Tweakable brute force color coding scheme generator
#'
#' Generates "color" coding schemes used to mark and identify individual animals, given a list of numeric sequences. The codes are robust to an arbitrary number of partial code erasures. This method uses a sloppy, slow, stochastic brute force method.
#'
#' @note This function is aimed at more advanced users. We would suggest using other functions to generate ID lists unless you are familiar with how the \code{rabi} package works.
#'
#' @param combos a list of numeric sequences or a matrix where each row is a unique sequence. The length of the sequences or the width matrix corresponds to the \code{total.length} variable seen in \code{\link{rs_IDs}}. The numeric elements should ideally be between zero and one less than the alphabet size (\code{0:(alphabet - 1)}
#'
#' @param redundancy the number of erasures that can occur without disrupting surety of unique identification. This value determines how robust the scheme is to erasures.
#'@param num.tries the number of iterations that will be run before choosing the best option. Increasing this number increases the running time.
#' @param available.colors an optional list of strings that contains the names of the unique markings which compose the given 'alphabet' (e.g. "blue", "red", "yellow", etc.). If left blank, the mapping can be done at any later time using \code{\link{codes_to_colors}}. Additionally, the length of this list must match the 'alphabet size' given above.
#'
#' @details \code{tweaked_IDs} runs pretty much the same as \code{\link{brute_IDs}}. However, unlike \code{\link{brute_IDs}}, \code{tweaked_IDs} must be first given a list or matrix of acceptable ID sequences. Instead of randomly pruning down a list of ALL possible ID sequences, we can specify our constraints first and then generate the final ID scheme. This allows the user, in the face of some constraints, to potentially generate more unique IDs that otherwise available.
#'
#' However, the iterative pruning is done randomly, so it is likely that resulting list of codes does not contain the maximum possible number of robust codes. Thus, the process is repeated multiple times (\code{num.tries}) and the list that contains the largest number of robust codes is kept and returned.
#'
#'
#' @return a list of unique ID codes that fit the provided parameters.
#'
#' If an appropriate argument for \code{available.colors} is provided, each code will be a sequence of strings, otherwise, each code will be a sequence of numeric values.
#'
#'
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @references Burchill, A. T., & Pavlic, T. P. (2019). Dude, where's my mark? Creating robust animal identification schemes informed by communication theory. \emph{Animal Behaviour}, 154, 203-208. \href{https://doi.org/10.1016/j.anbehav.2019.05.013}{doi:10.1016/j.anbehav.2019.05.013}
#' @seealso \code{\link{brute_IDs}}. Also see the vignette \href{../doc/loosebirdtag.html}{\code{loosebirdtag}} for demonstrations and additional uses.
#'
#' @examples
#' alphabet <- 8      # the number of colors or symbols we have
#' total.length <- 5  # the number of positions we want mark
#' redundancy <- 2    # how many marks we can lose but still ID perfectly
#'
#'   #Create a function for determining odd or even
#'  odd <- function(x){ x %% 2 == 1 }
#'
#'   #Create a matrix of all possible sequences
#' perms <- rep(list(seq_len(alphabet)),total.length)
#' combos <- as.matrix(expand.grid(perms)) - 1
#'   #Only keep sequences that fit our constraints.
#'   #We want the first position to only be odd numbers
#'   #and the second position to only be even.
#' combos <- combos[which(odd(combos[,1]) & !odd(combos[,2])), ]
#' \dontrun{
#' codes <- tweaked_IDs(combos, redundancy, num.tries = 1)
#'
#'
#' print(paste0("The 'tweaked' list contains ", length(codes), " unique IDs."))
#' }
#' @export
#' @importFrom stringdist seq_distmatrix
#' @importFrom methods is


tweaked_IDs <- function(combos, redundancy, num.tries = 10, available.colors = NULL) {
  {
    if (missing(redundancy)) {
      stop("Error: you need specify to how many erasure events the IDs should be robust. Note, an increase in robustness requires an increase in the total length of the ID. ")
    }
    if (!is(num.tries, "numeric")) {
      stop(paste0("Error: the variable 'num.tries' must be of the class 'numeric,' not '", class(num.tries)[1],".'"))
    }
    if (is(combos,"matrix")) {
      combos <- split(combos, 1:nrow(combos))
      names(combos) <- NULL
    } else if (!is(combos,"list")) {
      stop("Error: the variable 'combos' must be either a list of numeric sequences or a matrix, where each row is a unique sequence. See the examples for a better idea.")
    }
  }


  tester <- function(combos, redundancy) {

    combo.list <- combos
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
    temp1 <- invisible(tester(combos, redundancy))
    if (length(temp1) > length(temp2))
      temp2 <- temp1
  }
  temp2 <- codes_to_colors(temp2, available.colors)
  return(temp2)
}

