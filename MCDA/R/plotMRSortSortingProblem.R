#' Plot the categories and assignments of an Electre TRI-like sorting problem
#' (via separation profiles).
#' 
#' The profiles shown are the separation profiles between the classes. They are
#' stored as the lower profiles of the categories.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param categoriesLowerProfiles Matrix containing, in each row, the lower
#' profiles of the categories (the separation profiles in fact). The columns
#' are named according to the criteria, and the rows are named according to the
#' categories. The index of the row in the matrix corresponds to the rank of
#' the category.
#' @param categoriesRanks A vector containing the ranks of the categories (1
#' for the best, with higher values for increasingly less preferred
#' categories). The vector needs to be named with the categories names, whereas
#' the ranks need to be a range of values from 1 to the number of categories.
#' @param assignments Vector containing the assignments (IDs of the categories)
#' of the alternatives to the categories. The elements are named according to
#' the alternatives.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria. "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized). The elements are named according to the IDs of the
#' criteria.
#' @param criteriaLBs Vector containing the lower bounds of the criteria to be
#' considered for the plotting. The elements are named according to the IDs of
#' the criteria.
#' @param criteriaUBs Vector containing the upper bounds of the criteria to be
#' considered for the plotting. The elements are named according to the IDs of
#' the criteria.
#' @param categoriesDictators Matrix containing, in each row, the lower
#' dictator profiles of the categories. The columns are named according to the
#' criteria, and the rows are named according to the categories. The index of
#' the row in the matrix corresponds to the rank of the category.
#' @param categoriesVetoes Matrix containing, in each row, the lower veto
#' profiles of the categories. The columns are named according to the criteria,
#' and the rows are named according to the categories. The index of the row in
#' the matrix corresponds to the rank of the category.
#' @param majorityRule A string containing one of the following values: 'V' ,
#' 'D', 'v', 'd', 'dV', 'Dv', 'dv'. This indicates the type of majority rule
#' that will be used by the MRSort model. 'V' stands for MRSort with vetoes,
#' 'D' stands for MRSort with dictators, 'v' stands for MRSort with vetoes
#' weakened by dictators, 'd' stands for MRSort with dictators weakened by
#' vetoes, 'dV' stands for MRSort with vetoes dominating dictators, 'Dv' stands
#' for MRSort with dictators dominating vetoes, while 'dv' stands for MRSort
#' with conflicting vetoes and dictators.
#' @param criteriaWeights Vector containing the criteria weights. The elements
#' are named according to the IDs of the criteria.
#' @param majorityThreshold A value corresponding to the majority threshold.
#' Along with the criteria weights, this value is used to determine when a
#' coalition of criteria is sufficient in order to assert that an alternative
#' is at least as good as a category profile.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @param legendRatio The ratio between the legend and plot heights. By defaut
#' 0.2.
#' @keywords methods
#' @examples
#' 
#' # the performance table
#' 
#' performanceTable <- rbind(
#'   c(1,10,1),
#'   c(4,20,2),
#'   c(2,20,0),
#'   c(6,40,0),
#'   c(30,30,3))
#' 
#' rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
#' 
#' colnames(performanceTable) <- c("Price","Time","Comfort")
#' 
#' # lower profiles of the categories 
#' # (best category in the first position of the list)
#' 
#' categoriesLowerProfiles <- rbind(c(3, 11, 3),c(7, 25, 2),c(30,30,0))
#' 
#' colnames(categoriesLowerProfiles) <- colnames(performanceTable)
#' 
#' rownames(categoriesLowerProfiles)<-c("Good","Medium","Bad")
#' 
#' categoriesRanks <-c(1,2,3)
#' 
#' names(categoriesRanks) <- c("Good","Medium","Bad")
#' 
#' # criteria to minimize or maximize
#' 
#' criteriaMinMax <- c("min","min","max")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' # lower bounds of the criteria for the determination of value functions
#' 
#' criteriaLBs=c(0,5,0)
#' 
#' names(criteriaLBs) <- colnames(performanceTable)
#' 
#' # upper bounds of the criteria for the determination of value functions
#' 
#' criteriaUBs=c(50,50,4)
#' 
#' names(criteriaUBs) <- colnames(performanceTable)
#' 
#' # weights
#' 
#' criteriaWeights <- c(1,3,2)
#' 
#' names(criteriaWeights) <- colnames(performanceTable)
#' 
#' assignments <- assignments<-MRSort(performanceTable, 
#'                                    categoriesLowerProfiles, 
#'                                    categoriesRanks,
#'                                    criteriaWeights, 
#'                                    criteriaMinMax, 3)
#' 
#' names(assignments) <- rownames(performanceTable)
#' 
#' plotMRSortSortingProblem(performanceTable, categoriesLowerProfiles, 
#'                          categoriesRanks, assignments, criteriaMinMax, 
#'                          criteriaUBs, criteriaLBs)
#' 
#' @export plotMRSortSortingProblem
#' @import RColorBrewer
#' @importFrom grDevices palette rainbow
#' @importFrom graphics arrows axis layout legend lines par plot plot.new points text title polygon
plotMRSortSortingProblem <- function(performanceTable, categoriesLowerProfiles, categoriesRanks, assignments, criteriaMinMax, criteriaUBs, criteriaLBs, categoriesDictators = NULL, categoriesVetoes = NULL, majorityRule = NULL, criteriaWeights = NULL, majorityThreshold = NULL, alternativesIDs = NULL, criteriaIDs = NULL, legendRatio = 0.2){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.matrix(categoriesLowerProfiles)))
    stop("categoriesLowerProfiles should be a matrix")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaLBs)))
    stop("criteriaLBs should be a vector")
  
  if (!(is.vector(criteriaUBs)))
    stop("criteriaUBs should be a vector")

  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if(is.null(names(categoriesRanks)))
    stop("categoriesRanks should be named")
  
  if(!all(sort(categoriesRanks) == 1:length(categoriesRanks)))
    stop("categoriesRanks should contain a permutation of the category indices (from 1 to the number of categories)")

  if (!(is.null(categoriesDictators) || is.matrix(categoriesDictators)))
    stop("categoriesDictators should be a matrix")
  
  if (!(is.null(categoriesVetoes) || is.matrix(categoriesVetoes)))
    stop("categoriesVetoes should be a matrix")
  
  if (!(is.null(criteriaWeights) || is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  
  if (!(is.null(majorityThreshold) || is.numeric(majorityThreshold)))
    stop("majorityThreshold should be a number")
  
  if (!(is.null(majorityRule) || (majorityRule %in% c("V","D","v","d","dV","Dv","dv"))))
    stop("majorityRule should be: 'V' , 'D', 'v', 'd', 'dV', 'Dv', 'dv'")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  ## filter the data according to the given alternatives and criteria
  if (!is.null(alternativesIDs)){
    if(any(alternativesIDs %in% rownames(performanceTable)))
    {
      performanceTable <- performanceTable[alternativesIDs,]
      assignments <- assignments[alternativesIDs]
    }
    else
    {
      performanceTable <- NULL
      assignments <- NULL
    }
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
    categoriesLowerProfiles <- categoriesLowerProfiles[,criteriaIDs]
    if (!is.null(criteriaWeights))
      criteriaWeights <- criteriaWeights[,criteriaIDs]
    if (!is.null(categoriesDictators))
      categoriesDictators <- categoriesDictators[,criteriaIDs]
    if (!is.null(categoriesVetoes))
      categoriesVetoes <- categoriesVetoes[,criteriaIDs]
    criteriaUBs <- criteriaUBs[criteriaIDs]
    criteriaLBs <- criteriaLBs[criteriaIDs]
  }
  
  # data is filtered, check for some data consistency
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  #if (is.null(dim(performanceTable))) 
  #  stop("less than 2 criteria or 2 alternatives")
  
  # get model assignments
  
  model.assignments <- assignments

  if (!is.null(criteriaWeights) && !is.null(majorityThreshold) && !is.null(performanceTable))
  {
    if (is.null(categoriesDictators) && is.null(categoriesVetoes))
      model.assignments <- MRSort(performanceTable, categoriesLowerProfiles, categoriesRanks,
                                  criteriaWeights, criteriaMinMax, majorityThreshold)
    else
    {
      if (is.null(majorityRule))
      {
        if (is.null(categoriesDictators) && !is.null(categoriesVetoes))
          majorityRule <- "V"
        else if (!is.null(categoriesDictators) && is.null(categoriesVetoes))
          majorityRule <- "D"
        else
          stop("majorityRule should be: 'V' , 'D', 'v', 'd', 'dV', 'Dv', 'dv'")
      }
      model.assignments <- LPDMRSort(performanceTable, categoriesLowerProfiles,
                                     categoriesRanks, criteriaWeights, criteriaMinMax,
                                     majorityThreshold,
                                     criteriaVetos=categoriesVetoes, 
                                     criteriaDictators=categoriesDictators, 
                                     majorityRule = majorityRule)
    }
  }
  
  # -------------------------------------------------------
  
  numCrit <- dim(categoriesLowerProfiles)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  if(is.null(numAlt))
    numAlt <- 0
  
  numCat <- dim(categoriesLowerProfiles)[1]
  
  # -------------------------------------------------------
  if(!is.null(performanceTable))
  {
    normalizedPerformanceTable <- matrix(nrow=numAlt,ncol=numCrit)
    
    for (j in 1:numAlt){
      for (i in 1:numCrit){
        if(criteriaMinMax[i] == "min")
          normalizedPerformanceTable[j,i] <- 1-(performanceTable[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
        else
          normalizedPerformanceTable[j,i] <- (performanceTable[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
      }
    }
  }

  if (!is.null(categoriesLowerProfiles))
  {
    normalizedProfiles <- matrix(nrow=numCat,ncol=numCrit)
    
    for (j in 1:numCat){
      for (i in 1:numCrit){
        if(criteriaMinMax[i] == "min")
          normalizedProfiles[j,i] <- 1-(categoriesLowerProfiles[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
        else
          normalizedProfiles[j,i] <- (categoriesLowerProfiles[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
      }
    }
  }
  
  if (!is.null(categoriesDictators))
  {
    normalizedDictators <- matrix(nrow=numCat,ncol=numCrit)
    
    for (j in 1:numCat){
      for (i in 1:numCrit){
        if(criteriaMinMax[i] == "min")
          if(!is.null(categoriesDictators[j,i]) & !is.na(categoriesDictators[j,i]))
            normalizedDictators[j,i] <- 1-(categoriesDictators[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
          else
            normalizedDictators[j,i] <- -0.5
        else
          if(!is.null(categoriesDictators[j,i]) & !is.na(categoriesDictators[j,i]))
            normalizedDictators[j,i] <- (categoriesDictators[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
          else
            normalizedDictators[j,i] <- 1.5
        if (normalizedDictators[j,i] > 1)
          normalizedDictators[j,i] <- 1.5
        else if (normalizedDictators[j,i] < 0)
          normalizedDictators[j,i] <- -0.5
      }
    }
  }

  if (!is.null(categoriesVetoes))
  {
    normalizedVetoes <- matrix(nrow=numCat,ncol=numCrit)
    
    for (j in 1:numCat)
    {
      for (i in 1:numCrit)
      {
        if(criteriaMinMax[i] == "min")
          if(!is.null(categoriesVetoes[j,i]) & !is.na(categoriesVetoes[j,i]))
            normalizedVetoes[j,i] <- 1-(categoriesVetoes[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
          else
            normalizedVetoes[j,i] <- 1.2
        else
          if(!is.null(categoriesVetoes[j,i]) & !is.na(categoriesVetoes[j,i]))
            normalizedVetoes[j,i] <- (categoriesVetoes[j,i]-criteriaLBs[i])/(criteriaUBs[i]-criteriaLBs[i])
          else
            normalizedVetoes[j,i] <- -0.2
        if (normalizedVetoes[j,i] > 1)
          normalizedVetoes[j,i] <- 1.2
        else if (normalizedVetoes[j,i] < 0)
          normalizedVetoes[j,i] <- -0.2
      }
    }
  }
  # color palette when number of categories outside ColorBrewer range
  col.cat <- rainbow(20)
  col.alt.lines <- rainbow(20)
  col.alt.markers <- rainbow(20)
  
  if (numCat >= 3 && numCat <= 11)
    col.cat <- RColorBrewer::brewer.pal(numCat,"Dark2")
  
  names(col.cat) <- rownames(categoriesLowerProfiles)
  
  palette(col.cat)
  
  # color palette for alternatives lines - the color of the category to which the model assigned them
  if(!is.null(model.assignments))
    col.alt.lines <- col.cat[model.assignments]

  # color palette for alternatives markers - the color of the category to which they should have been assigned
  if(!is.null(assignments))
    col.alt.markers <- col.cat[assignments]
  
  # name profiles as delimiting categories
  profiles.names <- paste(sapply(1:(numCat-1), function(x) names(categoriesRanks)[categoriesRanks == x]),sapply(2:numCat, function(x) names(categoriesRanks)[categoriesRanks == x]),sep = "-")
  
  ylim=c(-0.1, 1.1)
  
  layout(matrix(c(1:(numCat-1),rep(numCat,numCat-1)),2,numCat-1, byrow = TRUE), widths = rep(1,numCat-1), heights = c(1,legendRatio))
  
  par(mar=c(2, 2, 6, 2))
  
  # one plot for each pair of consecutive categories
  
  for(i in rev(1:(numCat-1)))
  {
    plot(1:numCrit, normalizedProfiles[i,], type="l", col="black", ylim=ylim, xlab = "weights", ylab="", xaxt="n", yaxt="n", lwd=2)
    
    # title of the two categories
    title(profiles.names[i])
    
    # criteria axes
    for (j in 1:numCrit){
      lines(c(j,j),ylim, col="gray")
    }
    
    # dictator region
    if (!is.null(categoriesDictators))
      polygon(c(0,0,1:numCrit,numCrit+1,numCrit+1),c(1.2,normalizedDictators[i,1+0.2],normalizedDictators[i,],normalizedDictators[i,numCrit]+0.2,1.2), col = "snow3", border = NA)
    
    # veto region
    if (!is.null(categoriesVetoes))
      polygon(c(0,0,1:numCrit,numCrit+1,numCrit+1),c(-0.2,normalizedVetoes[i,1]-0.2,normalizedVetoes[i,],normalizedVetoes[i,numCrit]-0.2,-0.2), col = "black", border = NA)
    
    # criteria names at the top
    axis(3,at=c(1:numCrit),labels=colnames(performanceTable))
    
    # criteria weights at the bottom
    if (!is.null(criteriaWeights))
    {
      axis(1,at=c(1:numCrit),labels=sapply(criteriaWeights,function(x) round(x,digits = 4)))
    }
    
    points(1:numCrit,normalizedProfiles[i,],type="p",col="black", pch=19, cex=1.5)
    
    # profiles values
    if (!is.null(categoriesLowerProfiles))
      text(c(1:numCrit), normalizedProfiles[i,], labels = sapply(categoriesLowerProfiles[i,],function(x) round(x,digits = 4)) , pos=3, offset = 1)
    
    if (!is.null(categoriesDictators))
      text(c(1:numCrit), normalizedDictators[i,], labels = sapply(categoriesDictators[i,],function(x) round(x,digits = 4)), pos=3, offset =1.1, font = 2)
    
    if (!is.null(categoriesVetoes))
      text(c(1:numCrit), normalizedVetoes[i,], labels = sapply(categoriesVetoes[i,],function(x) round(x,digits = 4)), col = "white", pos = 1, offset = 1.1, font = 2)
    
    # alternatives
    if (numAlt > 0)
    {
      for (j in (1:numAlt))
        points(1:numCrit,normalizedPerformanceTable[j,],type="c",pch=26, col=col.alt.lines[j], lwd=2)
      
      for (j in (1:numAlt))
        points(1:numCrit,normalizedPerformanceTable[j,],type="p",pch=c(0,2,5,3,4)[(j-1)%%5 + 1], col=col.alt.markers[j], lwd=2, cex = 2)
    }
    
  }
  
  par(mar=c(1, 3, 1, 3))
  
  plot.new()
  
  if (!is.null(majorityThreshold))
  {
    if(!is.null(performanceTable))
    {
      legend("left", c(paste("majorityThreshold =",majorityThreshold,'  '),rownames(performanceTable),names(col.cat)), cex=1.0, col=c('black',col.alt.lines,col.cat), 
         lwd=2, bty="n",pch=c(NA,rep(NA,numAlt),rep(NA,numCat)), lty = c(0,rep(2,numAlt),rep(1,numCat)), ncol = 4)
      legend("left", c(paste("majorityThreshold =",majorityThreshold,'  '),rownames(performanceTable),names(col.cat)), cex=1.0, col=c('black',col.alt.markers,col.cat), 
             lwd=2, bty="n",pch=c(NA,rep(c(0,2,5,3,4),floor(numAlt/5) + 1)[1:numAlt],rep(NA,numCat)), lty = c(0,rep(0,numAlt+numCat)), ncol = 4)
    }
    else
      legend("left", c(paste("majorityThreshold =",majorityThreshold,'  '),names(col.cat)), cex=1.0, col=c('black',col.cat), 
             lwd=2, bty="n",pch=rep(NA,numCat+1), lty = c(0,rep(1,numCat)), ncol = 4)
  }
  else
  {
    if(!is.null(performanceTable))
    {
      legend("left", c(rownames(performanceTable),names(col.cat)), cex=1.0, col=c(col.alt.lines,col.cat), 
         lwd=2, bty="n",pch=c(rep(c(0,2,5,3,4),floor(numAlt/5) + 1)[1:numAlt],rep(NA,numCat)), lty = c(0,rep(2,numAlt+numCat)), ncol = 4)
    }
  }
}
