webplot = function(data, alternativeID = NULL, criteriaIDs = NULL, main = NULL, add = FALSE, col = "red", lty = 1, lwd=2) {
  
  # code adapted from Alan Vaughn's code at http://statisticstoproveanything.blogspot.fr/2013/11/spider-web-plots-in-r.html
  
  if (!is.matrix(data) & !is.data.frame(data)) 
    stop("data should be a matrix or data.frame")
  
  if (is.null(criteriaIDs)) 
    criteriaIDs = colnames(data)
  
  if (is.null(alternativeID)) 
    alternativeID = 1
  
  if (is.character(alternativeID)) 
    if (alternativeID %in% rownames(data)) {
      alternativeID = which(rownames(data) == alternativeID)
    } else {
      stop("Invalid value for alternativeID.")
    }
  
  if (is.null(main)) 
    main = rownames(data)[alternativeID]
  
  # scale data
  data = scale(data[, criteriaIDs])
  data = apply(data, 2, function(x) x/max(abs(x)))
  data = as.data.frame(data)
  
  n.y = length(criteriaIDs)
  min.rad = 360/n.y
  polar.vals = (90 + seq(0, 360, length.out = n.y + 1)) * pi/180
  
  if (add == FALSE) {
    plot(0, xlim = c(-2.2, 2.2), ylim = c(-2.2, 2.2), type = "n", axes = F, 
         xlab = "", ylab = "")
    title(main)
    lapply(polar.vals, function(x) lines(c(0, 2 * cos(x)), c(0, 2 * sin(x))))
    lapply(1:n.y, function(x) text(2.15 * cos(polar.vals[x]), 2.15 * sin(polar.vals[x]), 
                                   criteriaIDs[x], cex = 0.8))
    
    lapply(seq(0.5, 2, 0.5), function(x) lines(x * cos(seq(0, 2 * pi, length.out = 100)), 
                                               x * sin(seq(0, 2 * pi, length.out = 100)), lwd = 0.5, lty = 2, col = "gray60"))
    lines(cos(seq(0, 2 * pi, length.out = 100)), sin(seq(0, 2 * pi, length.out = 100)), 
          lwd = 1.2, col = "gray50")
  }
  
  r = 1 + data[alternativeID, criteriaIDs]
  xs = r * cos(polar.vals)
  ys = r * sin(polar.vals)
  xs = c(xs, xs[1])
  ys = c(ys, ys[1])
  
  lines(xs, ys, col = col, lwd = lwd, lty = lty)
}




#' Function to plot radar plots of alternatives of a performance table.
#' 
#' Plots radar plots of alternatives contained in a performance table, either
#' in one radar plot, or on multiple radar plots. For a given alternative, the
#' plot shows how far above/below average (the thick black line) each of the
#' criteria performances values are (average taken w.r.t. to the filtered
#' performance table).
#' 
#' 
#' @param performanceTable A matrix containing the performance table to be
#' plotted. The columns are labelled according to the criteria IDs, and the
#' rows according to the alternatives IDs.
#' @param criteriaMinMax Vector indicating whether criteria should be minimized
#' or maximized. If it is given, a "higher" value in the radar plot corresponds
#' to a more preferred value according to the decision maker. "min" (resp.
#' "max") indicates that the criterion has to be minimized (maximized). The
#' elements are named according to the IDs of the criteria.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the data should be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @param overlay Boolean value indicating if the plots should be overlayed on
#' one plot (TRUE), or not (FALSE)
#' @param bw Boolean value indicating if the plots should be in black/white
#' (TRUE) or color (FALSE)
#' @param lwd Value indicating the line width of the plot.
#' @keywords methods
#' @examples
#' 
#' library(MCDA)
#' 
#' performanceTable <- matrix(runif(6*9), ncol=6)
#' 
#' row.names(performanceTable) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9")
#' 
#' colnames(performanceTable) <- c("g1","g2","g3","g4","g5","g6")
#' 
#' criteriaMinMax <- c("min","max","min","max","min","max")
#' 
#' names(criteriaMinMax) <- c("g1","g2","g3","g4","g5","g6")
#' 
#' # plotRadarPerformanceTable(performanceTable, criteriaMinMax, overlay=TRUE)
#' 
#' plotRadarPerformanceTable(performanceTable, criteriaMinMax, 
#'                           alternativesIDs = c("x1","x2","x3","x4"), 
#'                           criteriaIDs = c("g1","g3","g4","g5","g6"), 
#'                           overlay=FALSE, bw=FALSE)
#' 
#' # plotRadarPerformanceTable(performanceTable, criteriaMinMax, 
#' #                          alternativesIDs = c("x1","x2"), 
#' #                          criteriaIDs = c("g1","g3","g4","g5","g6"),
#' #                          overlay=FALSE)
#' 
#' 
#' @export plotRadarPerformanceTable
plotRadarPerformanceTable <- function(performanceTable, criteriaMinMax=NULL, alternativesIDs = NULL, criteriaIDs = NULL, overlay=FALSE, bw=FALSE, lwd=2){
  
  ## check the input data
  
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performance table, should be a matrix or a data frame")
  
  if (!(is.null(criteriaMinMax) || is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternatives IDs should be in a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteria IDs should be in a vector")
  
  ## filter the performance table and the criteria according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)) performanceTable <- performanceTable[alternativesIDs,]
  
  if (!is.null(criteriaIDs)) performanceTable <- performanceTable[,criteriaIDs]
  
  if (!is.null(criteriaIDs) && !is.null(criteriaMinMax)) criteriaMinMax <- criteriaMinMax[criteriaIDs]
  
  ## the criteria which are to be minimized have to be transformed for the plot to inverse the scales
  ## in case criteriaMinMax is given
  
  if (!is.null(criteriaMinMax)){
    for (i in (1:length(criteriaMinMax))){
      if (criteriaMinMax[i] == "min")
        performanceTable[,i] <- -performanceTable[,i]
    }
  }
  
  if (overlay ==TRUE){
    par(mfcol=c(1,1))
    palette(rainbow(dim(performanceTable)[1], s = 0.6, v = 0.75))
    
    if (bw){
      webplot(performanceTable, alternativeID=1, main="", col="black", lty=1, lwd=lwd)  
    } else{
      webplot(performanceTable, alternativeID=1, main="", col=1, lwd=lwd)  
    }
    
    if (bw){
      for (i in 2:dim(performanceTable)[1]){
        webplot(performanceTable, alternativeID=i, col="black", lty=((i-1)%%5+1),add=T, lwd=lwd)
      }
    } else{
      for (i in 2:dim(performanceTable)[1]){
        webplot(performanceTable, alternativeID=i, col=i, add=T, lwd=lwd)
      }
    }
    
    if (bw){
      tmp <- c(1)
      for (i in 2:dim(performanceTable)[1]){
        tmp <- c(tmp, ((i-1)%%5+1))
      }
      legend("bottomright", lty = tmp, lwd = lwd, col = "black", row.names(performanceTable), bty = "n")  
    }else{
      legend("bottomright", lty = 1, lwd = lwd, col = c(1:dim(performanceTable)[1]), row.names(performanceTable), bty = "n")    
    }
    
  }
  else{
    palette(rainbow(dim(performanceTable)[1], s = 0.6, v = 0.75))
    par(mfcol = c(ceiling(sqrt(dim(performanceTable)[1])), ceiling(sqrt(dim(performanceTable)[1]))))
    if (bw){
      for (i in 1:dim(performanceTable)[1]){
        webplot(performanceTable, alternativeID=i, col="black", lty=1, add=F, lwd=lwd)
      }
    } else{
      for (i in 1:dim(performanceTable)[1]){
        webplot(performanceTable, alternativeID=i, col=i, add=F, lwd=lwd)
      }  
    }
    
  }
  
  
  
  
  
}

