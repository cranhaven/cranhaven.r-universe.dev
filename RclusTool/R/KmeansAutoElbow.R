# RclusTool: clustering of items in datasets
#
# Copyright 2013 Guillaume Wacquet, Pierre-Alexandre Hebert, Emilie Poisson-Caillault
#                
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' ElbowFinder return K number of groups according to Elbow method
#' @title Elbow Finder
#' @description Perform Elbow method for the automatic estimation of the number of clusters.
#' @param x : x-axis
#' @param y : y-axis 
#' @return K number of clusters obtained by Elbow method
#' @importFrom stats coef lm
#' @references \url{https://stackoverflow.com/a/42810075}
#' @examples 
#' 
#' y <- c(824,248,32,28,26,19,20,20,17)           
#' x <- 1:length(y) 
#'       
#' K <- ElbowFinder(x,y)
#' 
#' plot(x,y,type='o')
#' abline(v=K,col="red")
#' 
#' @keywords internal
#'

ElbowFinder <- function(x, y) {
  # Max values to create line
  max_x_x <- max(x)
  max_x_y <- y[which.max(x)]
  max_y_y <- max(y)
  max_y_x <- x[which.max(y)]
  max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))

  # Creating straight line between the max values
  fit <- stats::lm(max_df$y ~ max_df$x)

  # Distance from point to line
  distances <- c()
  for(i in 1:length(x)) {
    distances <- c(distances, abs(stats::coef(fit)[2]*x[i] - y[i] + stats::coef(fit)[1]) / sqrt(stats::coef(fit)[2]^2 + 1^2))
  }

  # Max distance point
  K <- x[which.max(distances)]
  
  return(K)
}

#' KmeansAutoElbow return partition and K number of groups according to kmeans clustering and Elbow method
#' @title Kmeans clustering with automatic estimation of number of clusters
#' @description Perform Elbow method and kmeans algorithm for the automatic estimation of the number of clusters and data clustering.
#' @param features matrix of raw data (point by line).
#' @param Kmax maximum number of clusters.
#' @param StopCriteria elbow method cumulative explained variance > criteria to stop K-search.
#' @param graph boolean: if TRUE, figures for total of within-class inertia and explained variance are plotted.
#' @param Elbow boolean: if TRUE, Elbow method is used for finding the knee point of a curve.
#' @importFrom stats kmeans
#' @importFrom grDevices dev.new
#' @importFrom graphics plot
#' @return The function returns a list containing:
#' \item{K}{number of clusters obtained by Elbow method.}
#' \item{res.kmeans}{results obtained from kmeans algorithm.}
#' @seealso \code{\link{KmeansQuick}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#'            
#' res <- KmeansAutoElbow(dat, Kmax=20, graph=TRUE)
#' 
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'		col = res$res.kmeans$cluster, main = "K-means clustering")
#' 
#' @keywords internal
#'
KmeansAutoElbow <- function(features, Kmax, StopCriteria=0.99, graph=FALSE, Elbow=TRUE) {
    N <- nrow(features)
    Within <- rep(-1,Kmax)
    explainedVar <- rep(-1,Kmax)
    fastCenters <- NULL
    for (i in 1:Kmax) {
        fastCenters <- NULL; #find center quickly on 5% of points
        #if (N>20000) {
        #    idx <- sample((1:N), round(N*0.05), replace=FALSE)
        #    res <- stats::kmeans(features[idx,,drop=FALSE], centers=i, iter.max = 200, nstart = 20, algorithm = c("Lloyd"))
        #    fastCenters <- res$centers
        #} else 
            fastCenters <- i
        #kmeans with this initial centers  if many data
        Res.km <- stats::kmeans(features, centers=fastCenters, iter.max = 200, nstart = 1, algorithm = c("Lloyd"))
        names(Res.km$cluster) <- rownames(features)
        Within[i] <- Res.km$tot.withinss
        explainedVar[i] <- Res.km$betweenss/Res.km$totss
        if (Elbow==FALSE){
          if (explainedVar[i]>StopCriteria) {
             K <- i
             break
             }
             K <- i
        }
    }
    if (Elbow==TRUE){
    	K=ElbowFinder(1:Kmax,Within[1:Kmax])
    	Res.km <- stats::kmeans(features, centers=K, iter.max = 200, nstart = 1, algorithm = c("Lloyd"))
    	Res.km[["Withins_tot"]]=Within
    }
    if (graph==TRUE) {
        wind1<- tktoplevel()
        tkwm.title(wind1, "Elbow method")
        AbdP <- tkrplot(wind1, hscale = 1.2, 
        	            function() { 
        	                graphics::plot(Within, type="b", main="total of within-class inertie",ylab="Number of clusters")
        	                graphics::abline(v=K,col="red")})
        tkgrid(AbdP,sticky="w")
    }
    out <- list(K=K, res.kmeans=Res.km)
}

#' KmeansQuick partition and K number of groups according to kmeans clustering
#' @title Quick kmeans clustering
#' @description Perform quick kmeans algorithm for data clustering.
#' @param features matrix of raw data (point by line).
#' @param K number of clusters.
#' @importFrom stats kmeans
#' @return res.kmeans results obtained from kmeans algorithm.
#' @seealso \code{\link{KmeansAutoElbow}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#'            
#' res <- KmeansQuick(dat, K=3)
#' 
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'		col = res$cluster, main = "K-means clustering")
#' 
#' @keywords internal 
#'
KmeansQuick <- function(features, K) {
    N <- nrow(features)
    Within <- -1
    explainedVar <- -1

    fastCenters <- NULL; #find center quickly on 5% of points
    if (N>20000) {
        sample.size <- round(N*0.05)
        idx <- sample((1:N), sample.size, replace=FALSE)
        res <- stats::kmeans(features[idx,], centers=min(K,sample.size-1), iter.max = 200, nstart = 20, algorithm = c("Hartigan-Wong"))
        fastCenters <- res$centers
    } else
        fastCenters <- min(K, N-1)

    #kmeans with this initial centers if many data
    if (nrow(features)>1)
    {
        res.kmeans <- stats::kmeans(features, centers=fastCenters, iter.max = 200, nstart = 1, algorithm = c("Hartigan-Wong"))
        Within <- res.kmeans$tot.withinss
        explainedVar <- res.kmeans$betweenss/res.kmeans$totss
    } else
    {
		cluster.vec <- 1
		names(cluster.vec) <- rownames(features)
        res.kmeans <- list(cluster=cluster.vec, centers=features, totss=0, withinss=0, tot.withinss=0, betweenss=0, size=1, iter=0, ifault=0)
    }
	names(res.kmeans$cluster) <- rownames(features)
    res.kmeans
}

#' FindNumberK return K number according spectral gap on reduced points
#' @title Automatic estimation of the number of clusters
#' @description Compute gap on a similarity matrix obtained from raw data matrix to estimate the number of clusters automatically.
#' @param features matrix of raw data (point by line).
#' @param Kmax maximum number of clusters.
#' @param StopCriteria elbow method cumulative explained variance > criteria to stop K-search.
#' @param graph boolean: if TRUE, figures for total of within-class inertia and explained variance are plotted.
#' @return K number of clusters obtained.
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#'            
#' res <- FindNumberK(dat, Kmax=20, graph=TRUE)
#'
#' @keywords internal 
#'
FindNumberK <- function(features, Kmax, StopCriteria=0.99, graph=FALSE) {
    #selection of representative point by kmeans
    res <- KmeansAutoElbow(features, Kmax, StopCriteria=StopCriteria, graph=graph)
    selected <- res$res.kmeans$centers
    sim <- computeGaussianSimilarityZP(selected, min(7,nrow(selected)-1))
    out <- computeGap(sim, Kmax)
    out$Kmax
}
