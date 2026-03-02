#' Assistance with choosing ID scheme parameters
#'
#' Displays the maximum number of unique and robust IDs possible given various combinations of parameters used in the \code{\link{rabi}} package. Several tables, centered around the supplied inputs or the default values, are printed to help the user choose which set of physical parameters would be most useful in their study. This is based on the equation: \deqn{max\# of IDs = alphabet^{ total\_length - redundancy}}{max # of IDs = alphabet^(total.length - redundancy) }
#'
#' @param total.length the desired number (or estimation) of unique positions to be marked on the animal. (This can be thought of as the total number of positions on which color bands or paint marks will be applied.)
#' @param redundancy the desired number (or estimation) of erasures that can occur without disrupting surety of unique identification. This value determines how robust the scheme is to erasures.
#' @param alphabet an integer representing the desired (or estimated) 'alphabet size.' This is the number of unique markings (think different paint colors, symbols, or varieties of bands) at your disposal.
#'
#' @note The \code{\link{rs_IDs}} function generates codes that have the maximum number of unique IDs; these are the theoretical values listed in the tables. However, \code{\link{rs_IDs}} has several restrictions on the parameter combinations it can accept. Asterisks ('*') are used in the table to indicate which values are a result of such illegal combinations. Other functions such as \code{\link{brute_IDs}} or \code{\link{simple_IDs}} can be used generate schemes from those particular parameter combinations, but they may fail to achieve the theoretical maximums listed in the table.
#'
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @references Burchill, A. T., & Pavlic, T. P. (2019). Dude, where's my mark? Creating robust animal identification schemes informed by communication theory. \emph{Animal Behaviour}, 154, 203-208. \href{https://doi.org/10.1016/j.anbehav.2019.05.013}{doi:10.1016/j.anbehav.2019.05.013}
#' @seealso \code{\link{how_robust}}.
#'
#' @examples
#'  #Let's generate some tables to see the number of unique IDs we could get given:
#' total.length <- 4  #we have ~4 positions to mark,
#' redundancy <- 1    #we're interested in being robust to a single erasure,
#' alphabet <- 5      #and we currently have 5 types of color bands in stock
#'
#' how_many(total.length, redundancy, alphabet)
#'
#' @export
#' @importFrom methods is

how_many <- function(total.length = 5, redundancy = 2, alphabet = 6) {

  # try(alphabet[length(alphabet)+1] <- previousPrime(min(alphabet)), silent = TRUE)
  # try(alphabet[length(alphabet)+1] <- previousPrime(min(alphabet)), silent = TRUE) alphabet[length(alphabet)+1]
  # <- nextPrime(max(alphabet)) alphabet[length(alphabet)+1] <- nextPrime(max(alphabet))
  alphabet <- (alphabet - 2):(alphabet + 2)
  alphabet <- sort(alphabet)
  names(alphabet) <- paste0("alphabet: ", alphabet, " ")

  total.length <- (total.length - 2):(total.length + 2)
  names(total.length) <- paste0("length: ", total.length, " ")
  total.length <- total.length[total.length > 0]

  redundancy <- (redundancy - 2):(redundancy + 2)
  names(redundancy) <- paste0("redundancy: ", redundancy)
  redundancy <- redundancy[redundancy > 0]

  a1 <- outer(total.length, redundancy, FUN = function(x, y) x - y)
  a1[a1 < 0] <- NA
  a1 <- outer(a1, alphabet, FUN = function(x, y) y^x)
  a1 <- aperm(a1, c(1, 3, 2))

  # for rs_IDs, find all the cells where total length > alphabet size OR where alphabet size is non-prime
  removed <- which(is.na(outer(total.length, alphabet, FUN = function(x, y) ifelse(x > y | !numbers::isPrime(y), NA, 1))), arr.ind = TRUE)
  rownames(removed) <- NULL
  for (i in 1:dim(removed)[1]) {
    a1[removed[i, 1], removed[i, 2], ] <- paste0(a1[removed[i, 1], removed[i, 2], ], "*")
  }
  a1[a1 == "NA*"] <- NA

  print.table(a1)
  cat("*: Asterisk indicates this particular parameter combination is outside what rs_IDs() will accept as an input")
}
