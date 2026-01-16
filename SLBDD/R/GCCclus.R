#' Clustering of Time Series Using the Generalized Cross Correlation Measure of Linear dependency
#'
#' Clustering of time series using the Generalized Cross Correlation (GCC) measure of linear dependency
#' proposed in Alonso and Peña (2019).
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param lag Selected lag for computing the GCC between the pairs of series.
#' Default value is computed inside the program.
#' @param rs Relative size of the minimum group considered. Default value is  0.05.
#' @param thres Percentile in the distribution of distances that define observations that are not considered outliers.
#' Default value is 0.9.
#' @param plot If the value is TRUE, a clustermatrix plot of distances and a dendogram are presented.
#' Default is FALSE.
#' @param printSummary If the value is TRUE, the function prints a summary table of the clustering. Default is TRUE.
#' @param lag.set If lag is not specified and the user wants to use instead of lags from 1 to 'lag' a non consecutive set of lags they can be defined as lag.set = c(1, 4, 7).
#' @param silh If silh = 1 standard silhoutte statistics and if silh = 2 modified procedure. Default value is 1.
#'
#' @details
#' First, the matrix of Generalized Cross correlation (GCC) is built by
#' using the subrutine GCCmatrix, then a hierarchical grouping is constructed and the number of
#' clusters is selected by either the silhouette statistics or a modified silhouette statistics
#' The modified silhouette statistics is as follows:
#' \itemize{
#' \item (1) Series that join the groups at a distance larger than a given threshold of the
#' distribution of the distances are disregarded.
#' \item (2) A minimum  size for the groups is defined by rs, relative size, groups smaller than rs are
#' disregarded.
#' \item (3) The final groups are obtained in two steps:
#' \itemize{
#' \item First the silhouette statistics is applied to the set of
#' time series that verify conditions (1) and (2).
#' \item Second, the series disregarded in steps (1) and (2) are candidates
#' to be assigned to its closest group. It is checked using the median and the MAD of the group if the point
#' is or it is not an outlier with respect to the group. If it is an outlier it is included in a group 0 of outlier series.
#' The distance between a series and a group is usually to the closest in the group (simple linkage) but could be to
#' the mean of the group.
#' }
#' }
#'
#' @return A list containing:
#' \itemize{
#' \item - Table of number of clusters found and number of observations in each cluster. Group 0 indicates the outlier group in the case it exists.
#' \item - sal: A list with four objects
#' \itemize{
#' \item labels: assignments of time series to each of the groups.
#' \item groups: is a list of matrices. Each matrix corresponds to the set of time series that make up each group.
#' For example, $groups[[i]] contains the set of time series that belong to the ith group.
#' \item matrix: GCC distance matrix.
#' \item gmatrix: GCC distance matrices in each group.
#' }
#' \item Two plots are included (1) A clustermatrix plot with the distances inside each group in the diagonal boxes
#' and the distances  between series in two groups in off-diagonal boxes (2) the dendogram.
#' }
#'
#' @import cluster
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- GCCclus(TaiwanAirBox032017[1:50,1:8])
#'
#' @export
#'
#' @references Alonso, A. M. and Peña, D. (2019). Clustering time series by linear
#' dependency. \emph{Statistics and Computing}, 29(4):655–676.
#'
GCCclus <- function(x, lag, rs, thres, plot, printSummary = TRUE, lag.set, silh = 1){
  zData <- x

  if(is.null(names(zData))){
    colnames(zData) <- 1:ncol(zData)
  }
  if(missing(lag)){
    DM <- GCCmatrix(zData)
    message("> lag used for GCC:", DM$k,  "\n\n")
    lag <- DM$k
  }
  if(missing(rs)) rs <- 0.05
  if(missing(thres))thres <- 0.9
  if(missing(plot)) plot <- FALSE
  if(missing(lag.set))   DM  <- GCCmatrix(zData, lag)

  Percentage <- rs
  Threshold <- thres
  toPlot <- plot

  DM  <- GCCmatrix(zData, lag.set = lag.set)
  PP <- ncol(zData)

  distanceMatrix <- as.dist(DM$DM)

  lr <- hclust(distanceMatrix, method = "single")

  if (silh == 1){
    NCL0 <- silh.clus(min(20, ncol(DM$DM)-1), DM$DM, "single")
    message("> Number of clusters by using Silhouette statistic ", NCL0$nClus,   "\n\n")

    CL <- cutree(lr, NCL0$nClus)

    TAB <- table(CL)

    tab <- data.frame(labels = as.numeric(as.character(names(TAB))),
                      abs.freq = as.matrix(TAB)[, 1],
                      rel.freq = as.matrix(TAB)[, 1]/sum(TAB))
    if(printSummary){
      print(tab)
    }

    groupAsign <- list()
    for(i in 1:max(unique(CL))){
      groupAsign[[i]]  <-  zData[, which(CL == i)]
    }

    groupDist <- list()
    for(i in 1:max(unique(CL))){
      groupDist[[i]]  <-  DM$DM[which(CL == i), which(CL == i)]
    }

  }
  if (silh == 2){
    PRC <- quantile(lr$height, probs = Threshold)
    where  <- which(lr$height>PRC )

    if(length(where)>0){
      ncMax <- max(PP - where[1], 20)
    } else {
      ncMax <- 20
    }

    clusters <- cutree(lr, k = ncMax)
    TAB <- table(clusters)

    W1 <- which(TAB <= Percentage*PP)
    W2 <- which(TAB > Percentage*PP)

    R1 <- NULL
    R2 <- NULL
    for(i in 1:length(W1)){
      R1 <- c(R1, which(clusters == W1[i]))
    }
    for(i in 1:length(W2)){
      R2 <- c(R2, which(clusters == W2[i]))
    }

    if(toPlot==TRUE){
      tab <- data.frame(labels = names(TAB),
                        abs.freq = as.matrix(TAB)[, 1],
                        rel.freq = as.matrix(TAB)[, 1]/sum(TAB))
      rownames(tab) <- NULL
      if(printSummary){
        cat("> first cluster assignation \n\n")
        print(tab)
        cat("\n")
      }

      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
      par(mfrow = c(2, 1))
      plot(lr, hang = -1, main = "Dendrogram using all data")
    }


    D <- DM$DM

    D <- D[R2,R2]

    lr <- hclust(as.dist(D), "single")
    if(toPlot==TRUE){
      plot(lr, hang = -1, main = "Dendogram after removing outliers")
    }


    ncMax <- min(20, ncol(D)-1)
    clusters <- cutree(lr, 1:ncMax)

    NCL1 <- silh.clus(ncMax, D, "single")
    NCL2 <- gap.clus(D, clusters, 100)

    NCL <- NCL1$nClus

    TAB <- table(clusters[, NCL])

    W1 <- which(TAB <= Percentage*PP)
    W2 <- which(TAB > Percentage*PP)

    R12 <- NULL
    R22 <- NULL
    for(i in 1:length(W1)){
      R12 <- c(R12, which(clusters[, NCL] == W1[i]))
    }
    for(i in 1:length(W2)){
      R22 <- c(R22, which(clusters[, NCL] == W2[i]))
    }

    CL <- rep(0, PP)
    CL[R2[R22]] <- clusters[R22, NCL]

    TAB <- table(CL)

    tab <- data.frame(labels = as.numeric(as.character(names(TAB))),
                      abs.freq = as.matrix(TAB)[, 1],
                      rel.freq = as.matrix(TAB)[, 1]/sum(TAB))
    rownames(tab) <- NULL


    NCL <- sum(as.numeric(names(TAB))>0) - sum(TAB==0)
    if(max(as.numeric(names(TAB)))>NCL){
      WCL <- CL
      k <- 1
      if(sum(names(TAB)==0)==1)     k <- 0

      for(i in 1:max(as.numeric(names(TAB)))){
        if(!is.na(tab[i, 1])){
          C <- as.numeric(names(TAB))[i]
          WCL[CL==C] <- k
          k <- k + 1
        }
      }

      CL <- WCL
    }

    groupAsign <- list()
    for(i in 1:max(unique(CL))){
      groupAsign[[i]]  <-  zData[, which(CL == i)]
    }

    groupAsign[["outlier"]] <- zData[, which(CL == 0)]

    groupDist <- list()
    for(i in 1:max(unique(CL))){
      groupDist[[i]]  <-  DM$DM[which(CL == i), which(CL == i)]
    }

    groupDist[["outlier"]] <- DM$DM[which(CL == 0), which(CL == 0)]

    fMedian <- function(x) median(as.dist(x))
    fMAD <- function(x) mad(as.dist(x))

    medianG <- lapply(groupDist, fMedian)
    MADG <- lapply(groupDist, fMAD)

    criteriaG <- rep(0, max(CL))
    for(i in 1:max(CL)){
      criteriaG[i] <- medianG[[i]] + 3 * MADG[[i]]
    }

    names(criteriaG) <- 1:max(CL)


    where <- which(CL == 0)

    if(length(where) >0){
      nearbor <- DM$DM[where,-where]
      for(i in 1:nrow(nearbor)){
        disSort <- sort(nearbor[i, ])
        id <- names(disSort)[1]
        where0 <- which(colnames(zData)==id)
        groupId <- paste(CL[where0])
        if(disSort[1] < criteriaG[groupId]){
          CL[where[i]] <- CL[where0]
        }
      }
    }

    message("> Number final of clusters by using Silhouette modified algorithm", length(unique(CL)), "\n \n")

    message("> frecuency table\n \n")

    TAB <- table(CL)
    tab <- data.frame(labels = as.numeric(as.character(names(TAB))),
                      abs.freq = as.matrix(TAB)[, 1],
                      rel.freq = as.matrix(TAB)[, 1]/sum(TAB))


    if(printSummary){
      print(tab)
      cat("\n\n")
    }
  }

  sal <- list()
  sal$labels <- CL
  sal$groups <- groupAsign
  sal$matrix <- DM$DM
  sal$gmatrix <- groupDist

  if(toPlot==TRUE){
    graphMatrix(DM$DM, CL)
  }

  return(sal)

}

GCC_sim <- function(xData, yData, k){

  N <- length(xData)

  if(missing(k)){
    k <- floor(N/10)
  }

  M_xy <- matrix(nrow = N-k, ncol = 2*(k+1))

  for(i in 1:(k+1)){
    M_xy[, i]       <- xData[i:(N-k+i-1)]
    M_xy[, i+(k+1)] <- yData[i:(N-k+i-1)]
  }


  M_x <- M_xy[, 1:(k+1)]
  M_y <- M_xy[, (k+2):(2*(k+1))]


  R_xy <- cor(M_xy)

  if(is.null(dim(M_x))){
    R_x  <- cor(M_x, M_x)
    R_y  <- cor(M_y, M_y)
    GCC <- 1 - det(R_xy)^(1/ (1 * (k+1)))  / (R_x^(1/ (1 * (k+1))) * R_y^(1/ (1 * (k+1))))




  } else  {
    R_x  <- cor(M_x)
    R_y  <- cor(M_y)
    GCC <- 1 - det(R_xy)^(1/ (1 * (k+1)))  / (det(R_x)^(1/ (1 * (k+1))) * det(R_y)^(1/ (1 * (k+1))))

  }


  return(GCC)

}

graphMatrix <- function(distMatrix, clusters){

  colnames(distMatrix) <- 1:nrow(distMatrix)
  rownames(distMatrix) <- 1:nrow(distMatrix)

  nClus <- length(unique(clusters))

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(nClus, nClus))
  for(i in min(clusters):max(clusters)){
    for(j in min(clusters):max(clusters)){
      subset <- which(clusters%in%c(i, j))
      sampleDM <- as.dist(distMatrix[subset, subset])

      if(i != j){
        mainPlot <- paste("Density of distances for clusters", i, "and",j)
      } else {
        mainPlot <- paste("Density of distances for cluster", j)
      }
      if(length(sampleDM) >= 2){
        plot(density(sampleDM), main = mainPlot,
             xlab = paste("median dist ", round(median(sampleDM), 4), sep =""))
      }else{
        mainPlot <- paste("Density of distances for cluster", j, "*")
        xlab = "*The plot is emty because there is less than two points in this cluster"
        plot(NULL, xlim=c(0,1), ylim=c(0,1), main = mainPlot, xlab = xlab, ylab = "")

      }

    }
  }
}

gap.clus <- function(DistanceMatrix, Clusters, B){

  N <- dim(Clusters)[1]
  nClus <- dim(Clusters)[2]

  W <-  WithinDispersion(DistanceMatrix, Clusters, nClus);

  mds <- cmdscale(DistanceMatrix,eig=TRUE)
  eps <- 2^(-52)
  f <-  sum(mds$eig > eps^(1/4))
  mds <- cmdscale(DistanceMatrix,eig=TRUE, k = f)

  Xmatrix <- mds$points

  svd <- svd(Xmatrix); U <- svd$u; V <- svd$v; D <- svd$d;
  Zmatrix = Xmatrix %*% V;
  Zmin = apply(Zmatrix, 2, min);
  Zmax = apply(Zmatrix, 2, max);

  Wstar = matrix(ncol = B, nrow = nClus)
  for (b in 1:B){

    for (ff in 1:f){
      Zmatrix[ ,ff] = runif(N, Zmin[ff], Zmax[ff]);
    }

    Zmatrix <- Zmatrix %*% t(V);
    ZDistanceMatrix = (dist(Zmatrix));
    L = hclust(dist(Zmatrix), method = "single");
    ZClusters = cutree(L, k = 1:nClus);
    ZDistanceMatrix <- as.matrix(ZDistanceMatrix)
    Wstar[,b] = WithinDispersion(ZDistanceMatrix, ZClusters, nClus);

  }


  logWmean = apply(log(Wstar), 1, mean);
  logWstd  = apply(log(Wstar), 1, sd)*sqrt(1 + 1/B);


  GAPstat = logWmean - log(W);

  WhoseK = GAPstat[1:nClus-1] - GAPstat[2:nClus] + logWstd[2:nClus];
  gap.values <- data.frame(gap = WhoseK)

  R <-  min(which(WhoseK >= 0))
  sal <- list(optim.k = R, gap.values = gap.values)
  return(sal)

}

WithinDispersion <- function(DistanceMatrix, Clusters, nClus){

  RW <- NULL
  for (k in 1:nClus){

    D <- NULL
    n <- NULL
    for (r in 1:k){
      Indexes <- which(Clusters[,k] == r)
      D[r] <- sum(sum(DistanceMatrix[Indexes,Indexes]))
      n[r] = 2*sum(length(Indexes));
    }
    RW[k] <- sum(D/n)
  }
  return(RW)
}
