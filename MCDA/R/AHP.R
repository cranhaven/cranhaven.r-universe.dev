#' Analytic Hierarchy Process (AHP) method
#' 
#' AHP is a multi-criteria decision analysis method which was originally
#' developed by Thomas L. Saaty in 1970s.
#' 
#' 
#' @param criteriaWeightsPairwiseComparisons Matrix or data frame containing
#' the pairwise comparison matrix for the criteria weights. Lines and columns
#' are named according to the IDs of the criteria.
#' @param alternativesPairwiseComparisonsList A list containing a matrix or
#' data frame of pairwise comparisons (comparing alternatives) for each
#' criterion. The elements of the list are named according to the IDs of the
#' criteria. In each matrix, the lines and the columns are named according to
#' the IDs of the alternatives. If one criteria is already a score (i.e. it 
#' is a numeric value between 0 and 1 where higher values indicate better 
#' performance), then providing a nAlt-length vector, or a nAlt x 1 matrix
#' containing the score associated with each alternative will be enough, but
#' the vector or rows of the matrix must be named as the alternatives.
#' 
#' @return The function returns a vector containing the AHP score for each
#' alternative.
#' @references The Analytic Hierarchy Process: Planning, Priority Setting
#' (1980), ISBN 0-07-054371-2, McGraw-Hill
#' @examples
#' alts <- c("Corsa","Clio","Fiesta","Sandero")
#' style <- matrix(c(1.0, 1/4, 4.0, 1/6,
#'                   4.0, 1.0, 4.0, 1/4,
#'                   1/4, 1/4, 1.0, 1/5,
#'                   6.0, 4.0, 5.0, 1.0), 
#'                 nrow=length(alts), ncol=length(alts), byrow=TRUE, 
#'                 dimnames=list(alts,alts))
#' reliability <- matrix(c(1.0, 2.0, 5.0, 1.0,
#'                         1/2, 1.0, 3.0, 2.0,
#'                         1/5, 1/3, 1.0, 1/4,
#'                         1.0, 1/2, 4.0, 1.0), 
#'                       nrow=length(alts), ncol=length(alts), byrow=TRUE, 
#'                       dimnames=list(alts,alts))
#' fuel <- matrix(c(1.0, 2.0, 4.0, 1.0,
#'                  0.5, 1.0, 3.0, 2.0,
#'                  1/4, 1/3, 1.0, 1/5,
#'                  1.0, 1/2, 5.0, 1.0), 
#'                nrow=length(alts), ncol=length(alts), byrow=TRUE, 
#'                dimnames=list(alts,alts))
#' alternativesPairwiseComparisonsList <- list(style       = style, 
#'                                             reliability = reliability, 
#'                                             fuel        = fuel)
#' crit <- c("style","reliability","fuel")
#' criteriaWeightsPairwiseComparisons <- matrix(c(1.0, 1/2, 3.0,
#'                                                2.0, 1.0, 4.0,
#'                                                1/3, 1/4, 1.0), 
#'                                              nrow=length(crit), 
#'                                              ncol=length(crit), 
#'                                              dimnames=list(crit,crit))
#' # All attributes have pairwise comparisons
#' AHP(criteriaWeightsPairwiseComparisons, alternativesPairwiseComparisonsList)
#' # Fuel is a score
#' newFuel <- c(Corsa=34, Clio=27, Fiest=24, Sandero=28)
#' newFuel <- newFuel/sum(newFuel)
#' alternativesPairwiseComparisonsList$fuel <- newFuel
#' AHP(criteriaWeightsPairwiseComparisons, alternativesPairwiseComparisonsList)
#' 
#' @export AHP
AHP <- function(criteriaWeightsPairwiseComparisons, alternativesPairwiseComparisonsList){
	cw <- criteriaWeightsPairwiseComparisons
	pw <- alternativesPairwiseComparisonsList
  
	## check the input data is correct
	# Check criteriaWeightsPairwiseComparisons
	if(!is.matrix(cw) && !is.data.frame(cw)) stop("criteriaWeightsPairwiseComparisons must be a matrix or a data frame")
	if(nrow(cw)!=ncol(cw)) stop("criteriaWeightsPairwiseComparisons must be a square matrix or a data frame")
	if(!all(cw == t(1/cw))) stop("criteriaWeightsPairwiseComparisons must be a reciprocal matrix (i.e. value on one side must = 1/value)")
	if(length(alternativesPairwiseComparisonsList) < 2) stop("list alternativesPairwiseComparisonsList must contain at least 2 matrices or data frames")
	# Check alternativesPairwiseComparisonsList
	if(!is.list(pw)) stop("alternativesPairwiseComparisonsList must be a list")
	test <- all(sapply(pw, function(x) is.matrix(x) || is.data.frame(x) || is.vector(x)))
	if(!test) stop("All elements in alternativesPairwiseComparisonsList must be vectors, matrices or data.frames")
	if(is.matrix(pw[[1]])) nAlt <- nrow(pw[[1]]) else nAlt <- length(pw[[1]])
	if(is.null(names(pw))) stop("alternativesPairwiseComparisonsList must have all its elements named as the corresponding criteria")
	for (i in 1:length(pw)){
	  if(is.data.frame(pw[[i]])) pw[[i]] <- data.matrix(pw[[i]]) # turn data.frame into matrix
	  if(!is.numeric(pw[[i]])) stop("Element ", names(pw)[i], " inside alternativesPairwiseComparisonsList is not numeric")
	  if(is.matrix(pw[[i]]) && ncol(pw[[i]])>1){
	    if(nrow(pw[[i]])!=ncol(pw[[i]])) stop("matrices (or data.frames) in the list alternativesPairwiseComparisonsList must be square")
	    if(!all(pw[[i]] == t(1/pw[[i]]))) stop("matrices (or data.frames) in list alternativesPairwiseComparisonsList must be a reciprocal matrix (i.e. value on one side must = 1/value)")
	  }
	  if(is.vector(pw[[i]]) || (is.matrix(pw[[i]]) && ncol(pw[[i]])==1)){
	    if(is.vector(pw[[i]])) pw[[i]] <- matrix(pw[[i]], nrow=length(pw[[i]]), ncol=1, 
	                                             dimnames=list(names(pw[[i]]), NULL))
	    if(any(pw[[i]]<0)) stop("Element ", names(pw)[i], " is a set of scores, so it cannot contain any negative values")
	    if(abs(sum(pw[[i]])-1)>1e-5) stop("Element ", names(pw)[i], " is a set of scores, its values must add up to 1")
	  }
	  if(nrow(pw[[i]])!=nAlt) stop("all elements in list alternativesPairwiseComparisonsList must be the same number of rows")
	}

	critno <- nrow(cw)
	altno  <- nAlt

	## Estimate the principle eigenvector of the weights matrix to 10 digits
	pairwisematrix <-  cw  %*% cw
	sumrows <- rowSums(cw)
	sumtotal <- sum(sumrows)
	normalisedsumrows <- sumrows / sumtotal	
	previous <- vector()
	while(!identical(round(previous, digits = 10), round(normalisedsumrows, digits = 10))){
	  previous <- normalisedsumrows
	  pairwisematrix <-  pairwisematrix  %*% pairwisematrix
	  sumrows <- rowSums(pairwisematrix)
	  sumtotal <- sum(sumrows)
	  normalisedsumrows <- sumrows / sumtotal
	}
	weights <- normalisedsumrows

	## Estimate the principle eigenvectors of each of the score matrices to 10 digits
	savedscores <- matrix(nrow=critno, ncol=altno)
	for(i in 1:length(pw)){
	  if(ncol(pw[[i]])==1) savedscores[i,] <- as.vector(pw[[i]]) else {
	    pairwisematrix <-  pw[[i]]  %*% pw[[i]]
	    sumrows <- rowSums(pw[[i]])
	    sumtotal <- sum(sumrows)
	    normalisedsumrows <- sumrows / sumtotal	
	    previous <- vector()
	    while (!identical(round(previous, digits = 10), round(normalisedsumrows, digits = 10))){
	      previous <- normalisedsumrows
	      pairwisematrix <-  pairwisematrix  %*% pairwisematrix
	      sumrows <- rowSums(pairwisematrix)
	      sumtotal <- sum(sumrows)
	      normalisedsumrows <- sumrows / sumtotal
	    }
	    savedscores[i,] <- normalisedsumrows
	  }
	}

	## Calculate the results
	results <- matrix(nrow=1, ncol=altno)
	for(i in 1:altno){
	  altscore <- 0
	  for(j in 1:critno) altscore <- altscore + (weights[j] * savedscores[j,i])
	  results[1,i] <- altscore
	}
  results <- as.vector(results)
  names(results) <- row.names(pw[[1]])
	return(results)
}
