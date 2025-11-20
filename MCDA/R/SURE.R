#############################################################################
#
# Copyright Patrick Meyer, Sébastien Bigaret, Richard Hodgett & Alexandru-Liviu Olteanu, 2019
#
# Contributors:
#   Patrick Meyer <patrick.meyer@telecom-bretagne.eu>
#   Sebastien Bigaret <sebastien.bigaret@telecom-bretagne.eu>
#   Richard Hodgett <r.e.hodgett@leeds.ac.uk>
#   Alexandru-Liviu Olteanu
#		
# This software, MCDA, is a package for the R statistical software which 
# allows to use MCDA algorithms and methods. 
# 
# This software is governed by the CeCILL license (v2) under French law
# and abiding by the rules of distribution of free software. You can
# use, modify and/ or redistribute the software under the terms of the
# CeCILL license as circulated by CEA, CNRS and INRIA at the following
# URL "http://www.cecill.info".
# 
# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.
#		
# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.
#		
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.
#
##############################################################################

utils::globalVariables(c("values", "ind", "grp.mean"))



#' Simulated Uncertainty Range Evaluations (SURE)
#' 
#' SURE is a multi-criteria decision analysis method which was developed by
#' Richard Hodgett and Sajid Siraj. More details on the method are available in
#' https://doi.org/10.1016/j.eswa.2018.08.048
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
#' @param NoOfSimulations Integer stating the number of Simulations to use.
#' @return The function returns an element of type SURE which contains the SURE
#' simulated scores for each alternative.
#' @references Richard E. Hodgett, Sajid Siraj (2019). SURE: A method for
#' decision-making under uncertainty. Expert Systems with Applications, Volume
#' 115, 684-694.
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
#' criteriaWeights <- c(0.339,0.077,0.434,0.127,0.023) 
#' names(criteriaWeights) <- row.names(performanceTable)
#' 
#' criteriaMinMax <- c("max", "max", "max", "max", "max")
#' names(criteriaMinMax) <- row.names(performanceTable)
#' 
#' test1 <- SURE(performanceTableMin, 
#'                  performanceTable, 
#'                  performanceTableMax, 
#'                  criteriaWeights, 
#'                  criteriaMinMax, NoOfSimulations = 101)
#' 
#' summary(test1)
#' plotSURE(test1)
#' plotSURE(test1, greyScale = TRUE, separate = TRUE)
#' 
#' test2 <- SURE(performanceTableMin, 
#'               performanceTable,
#'               performanceTableMax,
#'               criteriaWeights,
#'               criteriaMinMax,
#'               alternativesIDs = c("Route Two","Route Three"),
#'               criteriaIDs = c("Yield","Toxicity","Separation"),
#'               NoOfSimulations = 101)
#' 
#' summary(test2)
#' plotSURE(test2)
#' plotSURE(test2, greyScale = TRUE, separate = TRUE)
#' 
#' @import triangle
#' @importFrom utils stack
#' @import plyr
#' @export SURE
SURE <- function (performanceTableMin, performanceTable, performanceTableMax, 
                  criteriaWeights, criteriaMinMax, alternativesIDs = NULL, 
                  criteriaIDs = NULL, NoOfSimulations = 100000) 
{
  ### Package checks
  
  if (!requireNamespace("triangle", quietly = TRUE)) stop("triangle package could not be loaded")
  
  if (!requireNamespace("plyr", quietly = TRUE)) stop("plyr package could not be loaded")
  
  # if (!requireNamespace("datasynthR", quietly = TRUE)) stop("datasynthR package could not be loaded")
  
  # if (require(triangle, datasynthR, plyr)){
  # } else {
  #   print("Packges required. Installing packages.")
  #   install.packages("triangle")
  #   library("triangle")
  #   install.packages("plyr")
  #   library(plyr)
  #   install.packages("devtools")
  #   library(devtools)
  #   install_github("jknowles/datasynthR")
  #   library("datasynthR")
  #   if(require(triangle, datasynthR, plyr)){
  #     print("Required packages installed.")
  #   } else {
  #     stop("Could not install required packages.")
  #   }
  # }
  
  ### Data checks
  
  if (NoOfSimulations < 100) 
    stop("You should have more than 100 simulations")
  if (sum(criteriaWeights) != 1) 
    stop("criteria weights must add to 1")
  if (!((is.matrix(performanceTableMin) || (is.data.frame(performanceTableMin))))) 
    stop("performanceTableMin must be a matrix or a data frame")
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("performanceTable must be a matrix or a data frame")
  if (!((is.matrix(performanceTableMax) || (is.data.frame(performanceTableMax))))) 
    stop("performanceTableMax must be a matrix or a data frame")
  if (!(length(criteriaWeights) == nrow(performanceTableMin) && 
        length(criteriaWeights) == nrow(performanceTable) && 
        length(criteriaWeights) == nrow(performanceTableMax))) 
    stop("the number of criteria weights must equal the number of rows in the scores matrices")
  if (missing(criteriaMinMax)) 
    stop("the input criteriaMinMax is required.")
  for (i in 1:ncol(performanceTable)) {
    for (j in 1:nrow(performanceTable)) {
      if (performanceTable[j, i] > performanceTableMax[j, 
                                                       i] || performanceTableMin[j, i] > performanceTable[j, 
                                                                                                          i]) {
        stop("performanceTableMax > performanceTable > performanceTableMin is not true.")
      }
    }
  }
  if (!is.null(alternativesIDs)) {
    performanceTableMin <- performanceTableMin[, alternativesIDs]
    performanceTable <- performanceTable[, alternativesIDs]
    performanceTableMax <- performanceTableMax[, alternativesIDs]
  }
  if (!is.null(criteriaIDs)) {
    performanceTableMin <- performanceTableMin[criteriaIDs, 
                                               ]
    performanceTable <- performanceTable[criteriaIDs, ]
    performanceTableMax <- performanceTableMax[criteriaIDs, 
                                               ]
    criteriaWeights <- criteriaWeights[criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  critno <- length(criteriaWeights)
  altno <- ncol(performanceTable)
  s <- NoOfSimulations
  
  ### Calculations
  
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
  
  # Create performance table to save simulations
  performanceTables <- array(NA, c(critno,altno,s))
  
  for (i in 1:critno) {
    for (j in 1:altno) {
      performanceTables[i,j,] <- rtriangle(s, a=performanceTableMin[i,j], b=performanceTableMax[i,j], c=performanceTable[i,j])
    }
  }
  
  ### Normalise decision tables using summation ratio normalisation
  for (i in 1:s){
    sumj <- c(1:critno)
    for (j in 1:critno)
    {
      sumj[j] <- sum(performanceTables[j,,i])
    }
    for (j in 1:critno)
    {
      performanceTables[j,,i] <- performanceTables[j,,i] / sumj[j]
    }
  }
  
  ### Calculate results
  
  results <- data.frame(matrix(NA, nrow = s, ncol = altno))
  
  for (k in 1:s){
    for (i in 1:altno)
    {
      result <- 0
      for (j in 1:critno)
      {
        result <- result + (performanceTables[j,i,k] * criteriaWeights[j])
      }
      results[k,i] <- result
    }	
  }
  
  colnames(results) <- colnames(performanceTable)
  
  ### Return results
  
  return(results)
  rm(results)
  rm(performanceTables)
  rm(performanceTableMin)
  rm(performanceTable)
  rm(performanceTableMax)
  rm(criteriaWeights)
}



#' Plot SURE kernel density plots.
#' 
#' Plots the output of function SURE()
#' 
#' 
#' @param SURE Output from function SURE().
#' @param greyScale TRUE/FALSE indicating if you want the plot to be in
#' greyscale.
#' @param separate TRUE/FALSE indicating if you want the density plots to be
#' separated.
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
#' criteriaWeights <- c(0.339,0.077,0.434,0.127,0.023) 
#' names(criteriaWeights) <- row.names(performanceTable)
#' 
#' criteriaMinMax <- c("max", "max", "max", "max", "max")
#' names(criteriaMinMax) <- row.names(performanceTable)
#' 
#' test1 <- SURE(performanceTableMin, 
#'                  performanceTable, 
#'                  performanceTableMax, 
#'                  criteriaWeights, 
#'                  criteriaMinMax,
#'                  NoOfSimulations = 101)
#' 
#' summary(test1)
#' plotSURE(test1)
#' plotSURE(test1, greyScale = TRUE, separate = TRUE)
#' 
#' @import ggplot2
#' @export plotSURE
plotSURE <- function(SURE, greyScale = FALSE, separate = FALSE){
  
  ### Package checks
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package could not be loaded")
  
  
  # if (require(ggplot2)){
  # } else {
  #   print("ggplot2 package required. Installing package.")
  #   install.packages("ggplot2", dependencies = T)
  #   library("ggplot2")
  #   if(require(ggplot2)){
  #     print("ggplot2 installed.")
  #   } else {
  #     stop("Could not install ggplot2.")
  #   }
  # }
  
  ## check the input data is correct
  if (!(is.data.frame(SURE))) 
    stop("the input must be from function SURE (a data frame)")
  
  altno <- ncol(SURE)
  s <- nrow(SURE)
  plot <- stack(SURE)
  
  if (separate)
  {
    if (greyScale)
    {
      mu <- ddply(plot, "ind", summarise, grp.mean=mean(values))
      ggplot(plot, aes(x = values, fill = ind)) + geom_density(alpha = 0.5,  fill="grey") + facet_grid(ind ~ .) + theme(legend.position="none") + geom_vline(data=mu, aes(xintercept=grp.mean),color="black", linetype="solid", size=1)
      
    } else {
      mu <- ddply(plot, "ind", summarise, grp.mean=mean(values))
      ggplot(plot, aes(x = values, fill = ind)) + geom_density(alpha = 0.5) + facet_grid(ind ~ ., labeller = labeller(ind = label_wrap_gen(10))) + theme(legend.position="none") + geom_vline(data=mu, aes(xintercept=grp.mean),color="black", linetype="solid", size=1)
    }
  } else {
    if (greyScale)
    {
      ggplot(plot, aes(x = values, fill = ind)) + geom_density(alpha = 0.5) + scale_fill_grey()
    } else {
      ggplot(plot, aes(x = values, fill = ind)) + geom_density(alpha = 0.5) 
    }
  }
}
