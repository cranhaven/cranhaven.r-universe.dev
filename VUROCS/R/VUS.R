#' @title Volume under the ROC surface
#' @description This function computes the volume under the ROC surface (VUS) for a vector of realisations \code{y} (i.e. realised categories) and a vector of predictions \code{fx} (i.e. values of the a ranking function f) for the purpose of assessing the discrimiatory power in a multi-class classification problem. This is achieved by counting the number of r-tuples that are correctly ranked by the ranking function f. Thereby, r is the number of classes of the response variable \code{y}.
#' @param y a vector of realized categories.
#' @param fx a vector of predicted values of the ranking function f.
#' @return The implemented algorithm is based on Waegeman, De Baets and Boullart (2008). A list of length two is returned, containing the following components:
#' \item{val}{volume under the ROC surface} 
#' \item{count}{counts the number of observations falling into each category}
#' @examples VUS(rep(1:5,each=3),c(3,3,3,rep(2:5,each=3)))
#' @references Waegeman W., De Baets B., Boullart L., 2008. On the scalability of ordered multi-class ROC analysis. Computational Statistics & Data Analysis 52, 3371-3388.

VUS <- function(y,fx){

  if (any(is.na(fx)) | any(is.na(y))) {
    stop("\n no NA values allowed for neither 'fx' nor 'y'")
  }
  
  if (length(fx)!=length(y)) {
   stop("\n 'y' and 'fx' need to have the same length.")
  }


  n     <- length(y)
  u     <- sort(unique(y))
  r     <- length(u)
  dat   <- cbind(y,fx)
  dat <- dat[order(y,decreasing=TRUE), ]
  dat <- dat[order(dat[,2]), ]
  psi   <- rep(0,r)
  count <- rep(0,r)

  for (i in 1:length(y)) {
    cat <- which(dat[i,1]==u)
    if (cat == 1) {
      psi[1] <- psi[1]+1
    } else {
      psi[cat] <- psi[cat]+psi[cat-1]
    }
    count[cat] <- count[cat] + 1
  }

  return(list(val=psi[r]/prod(count), count= count))
}