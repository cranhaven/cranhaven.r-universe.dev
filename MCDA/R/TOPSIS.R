#' Technique for Order of Preference by Similarity to Ideal Solution (TOPSIS)
#' method
#' 
#' TOPSIS is a multi-criteria decision analysis method which was originally
#' developed by Hwang and Yoon in 1981.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param criteriaWeights Vector containing the weights of the criteria. The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria. "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized). The elements are named according to the IDs of the
#' criteria.
#' @param positiveIdealSolutions Vector containing the positive ideal solutions
#' for each criteria. The elements are named according to the IDs of the
#' criteria.
#' @param negativeIdealSolutions Vector containing the negative ideal solutions
#' for each criteria. The elements are named according to the IDs of the
#' criteria.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the data should be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @return The function returns a vector containing the TOPSIS score for each
#' alternative.
#' @references Hwang, C.L.; Yoon, K. (1981). Multiple Attribute Decision
#' Making: Methods and Applications. New York: Springer-Verlag.
#' http://hodgett.co.uk/topsis-in-excel/
#' @examples
#' 
#' performanceTable <- matrix(c(5490,51.4,8.5,285,6500,70.6,7,
#'                               288,6489,54.3,7.5,290),
#'                               nrow=3,
#'                               ncol=4,
#'                               byrow=TRUE)
#' 
#' row.names(performanceTable) <- c("Corsa","Clio","Fiesta")
#' 
#' colnames(performanceTable) <- c("Purchase Price","Economy",
#'                                    "Aesthetics","Boot Capacity")
#' 
#' weights <- c(0.35,0.25,0.25,0.15)
#' 
#' criteriaMinMax <- c("min", "max", "max", "max")
#' 
#' positiveIdealSolutions <- c(0.179573776, 0.171636015, 0.159499658, 0.087302767)
#' negativeIdealSolutions <- c(0.212610118, 0.124958799, 0.131352659, 0.085797547)
#' 
#' names(weights) <- colnames(performanceTable)
#' names(criteriaMinMax) <- colnames(performanceTable)
#' names(positiveIdealSolutions) <- colnames(performanceTable)
#' names(negativeIdealSolutions) <- colnames(performanceTable)
#' 
#' overall1 <- TOPSIS(performanceTable, weights, criteriaMinMax)
#' 
#' overall2 <- TOPSIS(performanceTable,
#'                        weights, 
#'                        criteriaMinMax,
#'                        positiveIdealSolutions,
#'                        negativeIdealSolutions)
#' 
#' overall3 <- TOPSIS(performanceTable,
#'                       weights,
#'                       criteriaMinMax,
#'                       alternativesIDs = c("Corsa","Clio"),
#'                       criteriaIDs = c("Purchase Price","Economy","Aesthetics"))
#' 
#' overall4 <- TOPSIS(performanceTable, 
#'                     weights,
#'                     criteriaMinMax,
#'                     positiveIdealSolutions,
#'                     negativeIdealSolutions,
#'                     alternativesIDs = c("Corsa","Clio"), 
#'                     criteriaIDs = c("Purchase Price","Economy","Aesthetics"))
#' 
#' @export TOPSIS
TOPSIS <- function(performanceTable, criteriaWeights, criteriaMinMax, positiveIdealSolutions = NULL, negativeIdealSolutions = NULL, alternativesIDs = NULL, criteriaIDs = NULL){
	
	## check the input data

        if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
        	stop("alternatives IDs should be in a vector")
        	
        if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
        	stop("criteria IDs should be in a vector")

	if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
        stop("performanceTable must be a matrix or a data frame")

	if (!(length(criteriaWeights) == ncol(performanceTable))) 
		stop("the number of criteriaWeights must equal the number of columns in the performanceTable")

	if (missing(criteriaMinMax))
		stop("the input criteriaMinMax is required.")

	## filter the performance table and the criteria according to the given alternatives and criteria
	
	if (!is.null(alternativesIDs)) performanceTable <- performanceTable[alternativesIDs,]
	
	if (!is.null(criteriaIDs)) 	{
							performanceTable <- performanceTable[,criteriaIDs]
							criteriaWeights <- criteriaWeights[criteriaIDs]
							if (!missing(positiveIdealSolutions)) positiveIdealSolutions <- positiveIdealSolutions[criteriaIDs]
							if (!missing(negativeIdealSolutions)) negativeIdealSolutions <- negativeIdealSolutions[criteriaIDs]
							criteriaMinMax <- criteriaMinMax[criteriaIDs]
						}
	
	critno <- length(criteriaWeights)
	altno <- nrow(performanceTable)

	## Calculate the weighted normalised matrix
	divby <- c(1:critno)
	for (i in 1:critno)
		{
			divby[i] <- sqrt(sum(performanceTable[,i]^2))
		}
	normalisedm <- t(t(performanceTable) / divby)
	wnm <- t(t(normalisedm) * criteriaWeights)

	## Identify positive and negative ideal solutions
	pis <- c(1:critno)
	nis <- c(1:critno)
	if (missing(positiveIdealSolutions) || missing(negativeIdealSolutions))
		{
		for (i in 1:critno)
			{
				if (criteriaMinMax[i] == "max")
				{
					pis[i] <- max(wnm[,i])
					nis[i] <- min(wnm[,i])
				}
				else
				{
					pis[i] <- min(wnm[,i])
					nis[i] <- max(wnm[,i])
				}
			}
		}
	else
		{
		## check the input data is correct
		if (!(length(positiveIdealSolutions) == length(negativeIdealSolutions) || length(positiveIdealSolutions) == critno)) 
		stop("the number of postive and negaitve ideal solutions need to equal the number of alternaitves.")
		pis <- positiveIdealSolutions
		nis <- negativeIdealSolutions
		}

	## Identify separation from positive and negative ideal solutions
	spis <- sweep(wnm,2,pis)^2
	snis <- sweep(wnm,2,nis)^2	
	spisv <- c(1:altno)
	snisv <- c(1:altno)

	for (i in 1:altno)
			{
				spisv[i] <- sqrt(sum(spis[i,]))
				snisv[i] <- sqrt(sum(snis[i,]))
			}

	## Calculate results
	results <- c(1:altno)
	for (i in 1:altno)
			{
				results[i] <- snisv[i] / (snisv[i] + spisv[i])
			}
	names(results) <- rownames(performanceTable)
		return(results)
}
