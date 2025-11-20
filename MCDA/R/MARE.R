#' Multi-Attribute Range Evaluations (MARE)
#' 
#' MARE is a multi-criteria decision analysis method which was originally
#' developed by Hodgett et al. in 2014.
#' 
#' 
#' @param performanceTableMin Matrix or data frame containing the minimum
#' performance table. Each column corresponds to an alternative, and each row
#' to a criterion. Columns (resp. rows) must be named according to the IDs of
#' the alternatives (resp. criteria).
#' @param performanceTable Matrix or data frame containing the most likely
#' performance table. Each column corresponds to an alternative, and each row
#' to a criterion. Columns (resp. rows) must be named according to the IDs of
#' the alternatives (resp. criteria).
#' @param performanceTableMax Matrix or data frame containing the maximum
#' performance table. Each column corresponds to an alternative, and each row
#' to a criterion. Columns (resp. rows) must be named according to the IDs of
#' the alternatives (resp. criteria).
#' @param criteriaWeights Vector containing the weights of the criteria. The
#' elements are named according to the IDs of the criteria.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria. "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized). The elements are named according to the IDs of the
#' criteria.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the data should be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @return The function returns an element of type mare which contains the MARE
#' scores for each alternative.
#' @references Richard E. Hodgett, Elaine B. Martin, Gary Montague, Mark
#' Talford (2014). Handling uncertain decisions in whole process design.
#' Production Planning & Control, Volume 25, Issue 12, 1028-1038.
#' @examples
#' 
#' performanceTableMin <- t(matrix(c(78,87,79,19,8,68,74,8,90,89,74.5,9,20,81,30),
#'                   nrow=3,ncol=5, byrow=TRUE)) 
#' performanceTable <- t(matrix(c(80,87,86,19,8,70,74,10,90,89,75,9,33,82,30),
#'                               nrow=3,ncol=5, byrow=TRUE))
#' performanceTableMax <- t(matrix(c(81,87,95,19,8,72,74,15,90,89,75.5,9,36,84,30),
#'                                  nrow=3,ncol=5, byrow=TRUE))  
#' 
#' row.names(performanceTable) <- c("Yield","Toxicity","Cost","Separation","Odour")
#' colnames(performanceTable) <- c("Route One","Route Two","Route Three")
#' row.names(performanceTableMin) <- row.names(performanceTable)
#' colnames(performanceTableMin) <- colnames(performanceTable)
#' row.names(performanceTableMax) <- row.names(performanceTable)
#' colnames(performanceTableMax) <- colnames(performanceTable)
#' 
#' weights <- c(0.339,0.077,0.434,0.127,0.023) 
#' names(weights) <- row.names(performanceTable)
#' 
#' criteriaMinMax <- c("max", "max", "max", "max", "max")
#' names(criteriaMinMax) <- row.names(performanceTable)
#' 
#' overall1 <- MARE(performanceTableMin, 
#'                    performanceTable, 
#'                    performanceTableMax, 
#'                    weights, 
#'                    criteriaMinMax)
#' 
#' overall2 <- MARE(performanceTableMin, 
#'                     performanceTable,
#'                     performanceTableMax,
#'                     weights,
#'                     criteriaMinMax,
#'                     alternativesIDs = c("Route Two","Route Three"),
#'                     criteriaIDs = c("Yield","Toxicity","Cost","Separation"))
#' 
#' @export MARE
MARE <- function(performanceTableMin, performanceTable, performanceTableMax, criteriaWeights, criteriaMinMax, alternativesIDs = NULL, criteriaIDs = NULL){
	
	## check the input data is correct
	if (sum(criteriaWeights) != 1) 
        stop("criteria weights must add to 1")
	if (!((is.matrix(performanceTableMin) || (is.data.frame(performanceTableMin))))) 
        stop("performanceTableMin must be a matrix or a data frame")	
	if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
        stop("performanceTable must be a matrix or a data frame")
	if (!((is.matrix(performanceTableMax) || (is.data.frame(performanceTableMax))))) 
        stop("performanceTableMax must be a matrix or a data frame")
	if (!(length(criteriaWeights) == nrow(performanceTableMin) && length(criteriaWeights) == nrow(performanceTable) && length(criteriaWeights) == nrow(performanceTableMax))) 
		stop("the number of criteria weights must equal the number of rows in the scores matrices")
	if (missing(criteriaMinMax))
		stop("the input criteriaMinMax is required.")
	for (i in 1:ncol(performanceTable))
		{
		for (j in 1:nrow(performanceTable))
			{
				if (performanceTable[j,i] >  performanceTableMax[j,i] || performanceTableMin[j,i] >  performanceTable[j,i])
				{
					stop("performanceTableMax > performanceTable > performanceTableMin is not true.")
				}
			}
		}

	## filter the performance table and the criteria according to the given alternatives and criteria
	
	if (!is.null(alternativesIDs)) 	{
							performanceTableMin <- performanceTableMin[,alternativesIDs]
							performanceTable <- performanceTable[,alternativesIDs]
							performanceTableMax <- performanceTableMax[,alternativesIDs]
							}
	if (!is.null(criteriaIDs)) 		{
							performanceTableMin <- performanceTableMin[criteriaIDs,]
							performanceTable <- performanceTable[criteriaIDs,]
							performanceTableMax <- performanceTableMax[criteriaIDs,]
							criteriaWeights <- criteriaWeights[criteriaIDs]
							criteriaMinMax <- criteriaMinMax[criteriaIDs]
							}

	critno <- length(criteriaWeights)
	altno <- ncol(performanceTable)

	## Inverse minimising criterion scores
	for (i in 1:critno)
		{
			if (!(criteriaMinMax[i] == "max"))
			{
				formax <- performanceTableMin[i,]^-1
				performanceTable[i,] <- performanceTable[i,]^-1
				formin <- performanceTableMax[i,]^-1
				performanceTableMin[i,] <- formin
				performanceTableMax[i,] <- formax
			}
		}

	## Normalise matrices
	maxv <- c(1:critno)
	for (i in 1:critno)
		{
			maxv[i] <- max(c(max(performanceTableMin[i,]),max(performanceTable[i,]),max(performanceTableMax[i,])))
		}
	for (i in 1:critno)
		{
			performanceTableMin[i,] <- performanceTableMin[i,] / maxv[i]
			performanceTable[i,] <- performanceTable[i,] / maxv[i]
			performanceTableMax[i,] <- performanceTableMax[i,] / maxv[i]
		}

	## Calculate the results
	results <- matrix(nrow=3,ncol=altno)
	colnames(results) <- colnames(performanceTable)
	row.names(results) <- c("Minimum", "Most Likely", "Maximum")
	for (i in 1:altno)
		{
			resultmin <- 0
			result <- 0
			resultmax <- 0
			for (j in 1:critno)
			{
				resultmin <- resultmin + (performanceTableMin[j,i] * criteriaWeights[j])
				result <- result + (performanceTable[j,i] * criteriaWeights[j])
				resultmax <- resultmax + (performanceTableMax[j,i] * criteriaWeights[j])
			}
			results[1,i] <- resultmin
			results[2,i] <- result
			results[3,i] <- resultmax
		}	

	return(results)

	rm(results)
	rm(performanceTableMin)
	rm(performanceTable)
	rm(performanceTableMax)
	rm(criteriaWeights)
}



#' Plot Multi-Attribute Range Evaluations (MARE)
#' 
#' Plots the output of function MARE()
#' 
#' 
#' @param x Output from function MARE()
#' @examples
#' 
#' performanceTableMin <- t(matrix(c(78,87,79,19,8,68,74,8,90,89,74.5,9,20,81,30),
#'                                   nrow=3,ncol=5, byrow=TRUE)) 
#' performanceTable <- t(matrix(c(80,87,86,19,8,70,74,10,90,89,75,9,33,82,30),
#'                                 nrow=3,ncol=5, byrow=TRUE))
#' performanceTableMax <- t(matrix(c(81,87,95,19,8,72,74,15,90,89,75.5,9,36,84,30),
#'                                    nrow=3,ncol=5, byrow=TRUE))  
#' 
#' row.names(performanceTable) <- c("Yield","Toxicity","Cost","Separation","Odour")
#' colnames(performanceTable) <- c("Route One","Route Two","Route Three")
#' row.names(performanceTableMin) <- row.names(performanceTable)
#' colnames(performanceTableMin) <- colnames(performanceTable)
#' row.names(performanceTableMax) <- row.names(performanceTable)
#' colnames(performanceTableMax) <- colnames(performanceTable)
#' 
#' weights <- c(0.339,0.077,0.434,0.127,0.023) 
#' names(weights) <- row.names(performanceTable)
#' 
#' criteriaMinMax <- c("max", "max", "max", "max", "max")
#' names(criteriaMinMax) <- row.names(performanceTable)
#' 
#' overall1 <- MARE(performanceTableMin, performanceTable, performanceTableMax, 
#'                            weights, criteriaMinMax)
#' plotMARE(overall1)
#' 
#' overall2 <- MARE(performanceTableMin,
#'                     performanceTable,
#'                     performanceTableMax, 
#'                     weights,
#'                     criteriaMinMax, 
#'                     alternativesIDs = c("Route Two","Route Three"),
#'                     criteriaIDs = c("Yield","Toxicity","Cost","Separation"))
#' plotMARE(overall2)
#' 
#' @export plotMARE
plotMARE <- function(x){

	## check the input data is correct
	if (!((is.matrix(x) || (is.data.frame(x))))) 
        stop("the results must be a matrix or a data frame")

	## plot the most likely values
	plot(1:ncol(x), x[2,], las=1, xaxt = "n", pch=19, main="MARE Results", xlab="Alternatives", ylab="Scores", ylim=c(min(x), max(x)))
	axis(1, at = 1:ncol(x), labels = colnames(x))

	## plot the minimum and maximum ranges
	arrows(1:ncol(x), x[1,], 1:ncol(x), x[3,], code=3, angle=90, length=0.1)
}
