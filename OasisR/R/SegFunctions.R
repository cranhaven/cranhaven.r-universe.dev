library(seg)
library(outliers)
library(spdep)


################################################## 

# SEGREGATION INDEXES

################################################## 

# EVENESS INDEXES

################### 


#' A function to compute Duncan & Duncan segregation index
#'
#' @usage ISDuncan (x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return a numeric vector with values of the Duncan's segregation index 
#' for each group
#' @references Duncan O. D. and Duncan B. (1955) \emph{ 
#' Residential Distribution and Occupational Stratification}. 
#' American Journal of Sociology 60 (5), pp. 493-503
#' @description Duncan's segregation index is one-group form of 
#' dissimilarity index \code{\link{DIDuncan}} and  
#' measures the unevenness of a group distribution  
#' compared to the rest of the population. It can be interpreted
#' as the share of the group that would have to move to achieve 
#' an even distribution compared to the rest of the population.
#' @examples x <- segdata@data[ ,1:2]
#' ISDuncan(x) 
#' @seealso One-group evenness indices: 
#' \code{\link{Gini}}, \code{\link{Atkinson}}, \code{\link{Gorard}}, 
#' \code{\link{HTheil}}, '\code{\link{ISWong}}, \code{\link{ISMorrill}},
#' \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{Gini2}}, 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export


ISDuncan <- function(x) {
  x <- segdataclean(as.matrix(x))$x
  result <- vector(length = ncol(x))
  for (i in 1:ncol(x))
    result[i] <- 0.5 * sum(abs((x[,i]/sum(x[,i])) - ((rowSums(x)-x[,i])/sum((rowSums(x)-x[,i])))))
  return(round(result, 4))
}


#' A function to compute Atkinson segregation index
#'
#' @usage Atkinson (x, delta = 0.5) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param delta - an inequality aversion parameter
#' @return a numeric vector containing the Atkinson's segregation index value for 
#' each group 
#' @references James, D. and K. E. Taeuber (1985)  \emph{Measures 
#' of Segregation}. Sociological Methodology 15, pp. 1-32
#' @description The spatial version of Atkinson inequality index is based on 
#' Lorenz curves. The user can decide wich part of the curve contributes more 
#' to the index, by choosing the value of the shape parameter, delta. 
#' @examples x <- segdata@data[ ,7:8]
#' Atkinson(x) 
#' Atkinson(x, 0.1)
#' Atkinson(x, delta = 0.9)
#' @seealso One-group evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{HTheil}}, '\code{\link{ISWong}}, \code{\link{ISMorrill}},
#' \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{Gini2}}, 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export

Atkinson <- function(x, delta = 0.5) {
  x <- segdataclean(as.matrix(x))$x
  result <- vector(length = ncol(x))
  pTotal <- colSums(x)/sum(x)
  px <- x/rowSums(x)
  provi <- px
  for (k in 1:ncol(x)) {
    provi[, k] <- (((1 - px[, k])^(1 - delta) * px[, k]^delta * rowSums(x))/(pTotal[k] * sum(x)))
    result[k] <- 1 - (pTotal[k]/(1 - pTotal[k])) * sum(provi[, k])^(1/(1 - delta))
  }
  return(round(result, 4))
}


#' A function to compute Theil's entropy segregation index
#'
#' @usage HTheil (x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return a numeric vector containing Theils's entropy index value for 
#' each group 
#' @references Theil H. (1972)  \emph{Statistical decomposition analysis: with 
#' applications in the social and administrative.} Amsterdam, North-Holland, 337 p.
#' @description The entropy index (also called information index) measures
#' departure from evenness by assessing each spatial unit deviation from the 
#' entropy in the area. 
#' @examples x <- segdata@data[ ,1:2]
#' HTheil(x) 
#' @seealso One-group evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{Atkinson}}, '\code{\link{ISWong}}, \code{\link{ISMorrill}},
#' \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{Gini2}}, 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export


HTheil <- function(x) {
  x <- segdataclean(as.matrix(x))$x
  result <- vector(length = ncol(x))
  E <- matrix(data = 0, nrow = nrow(x), ncol = ncol(x))
  pTotal <- colSums(x)/sum(x)
  px <- x/rowSums(x)
  Etot <- rep(0, ncol(x))
  for (k in 1:ncol(x)) Etot[k] <- pTotal[k] * log(1/pTotal[k]) + (1 - pTotal[k]) * log(1/(1 - pTotal[k]))
  for (k in 1:ncol(x)) {
    E[, k] <- px[, k] * log(1/px[, k]) + (1 - px[, k]) * log(1/(1 - px[, k]))
    if (any(is.nan(E[, k]))) 
      E[, k][is.nan(E[, k])] <- 0
    result[k] <- sum((rowSums(x) * (Etot[k] - E[, k]))/(Etot[k] * sum(x)))
  }
  return(round(result, 4))
}




#' A function to compute K-th order Morrill's segregation index 
#'
#' @usage ISMorrillK(x, ck = NULL, queen = FALSE, spatobj = NULL, folder = NULL, 
#' shape = NULL, K = 2, f = 'exp', beta = 1, prec = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greaterTR than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param ck - a list containing contiguity matrices coresponding to each order 
#' (from 1 to K)
#' @param queen - logical parameter defining criteria used for contiguity 
#' matrix computation, TRUE for queen, FALSE (by default) for rook 
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the driveis located.
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @param K - the order of the contiguity matrix
#' @param f - the distance function, f = 'exp' (by default) for negative 
#' exponential function and f = 'rec' for reciprocal function
#' @param prec - precision parameter. If not NULL, the function stop computing
#' the spatial interaction when the impact on the indice is bellow 10 ^ (-prec)
#' @param beta - distance decay parameter
#' @return a matrix with Generalized Morrill's dissimilarity index values 
#' @references Morrill B. (1991) \emph{On the measure of geographic 
#' segregation}. Geography research forum, 11, pp. 25-36.
#' @description This function computes an adaptation of Morrill's segregation 
#' index which takes into account the interactions between spatial units 
#' defined by K-th ordered contiguity matrix. The index can be used in two   
#' ways: to provide a contiguity units defined by K order contiguity matrix. The 
#' function can be used in two matrix or a external geographic information source 
#' (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ISMorrillK(x, spatobj = segdata, queen = FALSE, K = 3)
#' 
#' ISMorrillK(x, folder = foldername, shape = shapename, K = 4, f = 'rec') 
#' 
#' @seealso One-group evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{HTheil}}, \code{\link{Atkinson}}, '\code{\link{ISWong}},
#' \code{\link{ISMorrill}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{Gini2}}, 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export


ISMorrillK <- function(x, ck = NULL, queen = FALSE, spatobj = NULL, folder = NULL, shape = NULL, K = 2, f = "exp", beta = 1, prec = NULL) {
    x <- as.matrix(x)
    if (K == 1) 
        result <- ISMorrill(x, ck[[1]], queen, spatobj, folder, shape)
    if (K > 1) {
        if (is.null(ck)) {
            if (is.null(spatobj)) 
                spatobj <- sf::st_read(dsn = folder, layer = shape)
            ngb <- spdep::poly2nb(spatobj, queen = queen)
            ngbk <- spdep::nblag(ngb, K)
            ck <- vector("list", K)
            for (k in 1:K) ck[[k]] <- spdep::nb2mat(ngbk[[k]], style = "B", zero.policy = TRUE)
        } else {
            cldata <- segdataclean(x, ck = ck)
            x <- cldata$x
            ck <- cldata$ck
            if (is.null(K)) 
                K <- length(ck)
        }
        result <- vector(length = ncol(x))
        if (ncol(x) == 2) {
            result[1] <- DIMorrillK(x, ck = ck, K = K, f = f)[1, 2]
            result[2] <- result[1]
        }
        if (ncol(x) > 2) {
            xprovi <- x[, 1:2]
            for (i in 1:ncol(x)) {
                xprovi[, 1] <- x[, i]
                xprovi[, 2] <- rowSums(x[, -i])
                result[i] <- DIMorrillK(xprovi, ck, queen, spatobj, folder, shape, K, f, beta, prec)[1, 2]
            }
        }
    }
    return(round(result, 4))
}



#' A function to compute Morrill's segregation index 
#'
#' @usage ISMorrill(x, c = NULL, queen = FALSE, 
#' spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param c - a standard binary contiguity (adjacency) symmetric matrix where 
#' each element \emph{Cij} equals 1 if \emph{i}-th and \emph{j}-th spatial 
#' units are adjacent, and 0 otherwise.
#' @param queen - a logical parameter difining criteria used for the contiguity 
#' matrix computation, TRUE for queen, FALSE (by default) for rook 
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) .
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing the Morrill's segregation index value for 
#' each group
#' @examples x <- segdata@data[ ,1:2]
#' contiguity <- contig(segdata)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ISMorrill(x, c = contiguity) 
#' 
#' ISMorrill(x, spatobj = segdata)
#' 
#' ISMorrill(x, folder = foldername, shape = shapename) 
#' 
#' @references Morrill B. (1991) \emph{On the measure of geographic 
#' segregation}. Geography research forum, 11, pp. 25-36.
#' @description Morrill's segregation index is a development of 
#' \code{\link{ISDuncan}}'s index which takes into account the 
#' interactions between spatial units(contiguity). 
#' The function can be used in two ways: to provide a contiguity 
#' matrix or a external geographic information source (spatial object 
#' or shape file).
#' @seealso One-group evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{HTheil}}, \code{\link{Atkinson}}, '\code{\link{ISWong}},
#' \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{Gini2}}, 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export


ISMorrill <- function(x, c = NULL, queen = FALSE, spatobj = NULL, folder = NULL, shape = NULL) {
  x <- as.matrix(x)
  if (is.null(c)) 
    c <- contig(spatobj = spatobj, folder = folder, shape = shape, queen = queen)
  cldata <- segdataclean(x, c = c)
  x <- cldata$x
  c <- cldata$c
  IS <- ISDuncan(x)
  result <- vector(length = ncol(x))
  pij <- array(0, dim = c(ncol(x), nrow(x), nrow(x)))
  p <- x/rowSums(x)
  for (k in 1:ncol(x)) {
    for (i in 1:nrow(x)) pij[k, , i] <- p[i, k]
    for (i in 1:nrow(x)) pij[k, i, ] <- abs(pij[k, i, ] - p[i, k])
    matprovi <- c %*% pij[k, , ]
    matprovi <- matprovi * diag(nrow(x))
    result[k] <- IS[k] - sum(matprovi)/sum(c)
  }
  return(round(result, 4))
}


#' A function to compute Wong's segregation index 
#'
#' @usage ISWong(x, b = NULL,  a = NULL, p = NULL, ptype = 'int', variant = 's', 
#' spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' totals because this will be interpreted as a group
#' @param b - a common boundaries matrix where each element \emph{Bij} 
#' equals the shared boundary of \emph{i}-th and \emph{j}-th spatial units.
#' @param p - a numeric vector containing spatial units perimeters.
#' @param ptype - a string variable giving two options for perimeter calculation
#' when a spatial object or shapefile is provided: 'int' to use only interior
#' boundaries of spatial units, and 'all' to use entire boundaries, 
#' including the boundaries to the exterior
#' @param a - a numeric vector containing spatial unit areas
#' @param variant - a character variable that allows to choose the index version: 
#' variant = 's' for the index adjusted for contiguous spatial/organizational units
#' boundary lengths and perimeter/area ratio (by default) and variant = 'w' 
#' for the version based only on shared boundaries length
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing the Wong's segregation index value for 
#' each group
#' @examples x <- segdata@data[ ,1:2]
#' bound <- boundaries(segdata)
#' per <- perimeter(segdata)
#' ar <- area(segdata)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ISWong(x, b = bound, p = per, a = ar) 
#' 
#' ISWong(x, spatobj = segdata, variant = 's', ptype = 'int')
#' 
#' ISWong(x, folder = foldername, shape = shapename, variant = 'w') 
#' 
#' @references Wong D. W. S. (1998) \emph{Measuring multiethnic spatial 
#' segregation}. Urban Geography, 19 (1), pp. 77-87.
#' @description Wong's segregation index is a development of 
#' \code{\link{ISDuncan}}'s which takes into account the interactions 
#' between spatial units (common boundaries and perimeter/area ratio). 
#' The function can be used in two ways: to provide spatial data (
#' boundaries matrix, a perimeter vector and an area vector) 
#' or a external geographic information source (spatial object or shape file).
#' @seealso One-group evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{HTheil}}, '\code{\link{Atkinson}}, \code{\link{ISMorrill}},
#' \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{Gini2}}, 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export



ISWong <- function(x, b = NULL, a = NULL, p = NULL, ptype = "int", variant = "s", spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(b)) 
        b <- boundaries(spatobj = spatobj, folder = folder, shape = shape)
    if (is.null(p)) {
        if (ptype == "all") 
            p <- perimeter(spatobj = spatobj, folder = folder, shape = shape)
        if (ptype == "int") 
            p <- rowSums(b)
    }
    if (is.null(a)) 
        a <- area(spatobj = spatobj, folder = folder, shape = shape)
    cldata <- segdataclean(x, b = b, p = p, a = a)
    x <- cldata$x
    b <- cldata$b
    p <- cldata$p
    a <- cldata$a
    IS <- ISDuncan(x)
    result <- vector(length = ncol(x))
    pij <- array(0, dim = c(ncol(x), nrow(x), nrow(x)))
    pp <- x/rowSums(x)
    w <- b/sum(b)
    if (variant == "w") 
        for (k in 1:ncol(x)) {
            for (i in 1:nrow(x)) pij[k, , i] <- pp[i, k]
            for (i in 1:nrow(x)) pij[k, i, ] <- abs(pij[k, i, ] - pp[i, k])
            matprovi <- w %*% (pij[k, , ])
            matprovi <- matprovi * diag(nrow(x))
            result[k] <- IS[k] - sum(matprovi)
        }
    if (variant == "s") {
        PerAij <- matrix(data = 0, nrow = nrow(x), ncol = nrow(x))
        for (i in 1:nrow(x)) PerAij[, i] <- p[i]/a[i]
        for (i in 1:nrow(x)) PerAij[i, ] <- PerAij[i, ] + p[i]/a[i]
        maxPA <- max(p/a)
        for (k in 1:ncol(x)) {
            for (i in 1:nrow(x)) pij[k, , i] <- pp[i, k]
            for (i in 1:nrow(x)) pij[k, i, ] <- abs(pij[k, i, ] - pp[i, k])
            for (i in 1:nrow(x)) PerAij[, i] <- p[i]/a[i]
            for (i in 1:nrow(x)) PerAij[i, ] <- PerAij[i, ] + p[i]/a[i]
            matprovi <- w %*% (pij[k, , ] * PerAij)
            matprovi <- matprovi * diag(nrow(x))
            result[k] <- IS[k] - sum(matprovi)/(2 * maxPA)
        }
    }
    return(round(result, 4))
}


#' A function to compute Gorard's segregation index 
#'
#' @usage Gorard(x)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' totals because this will be interpreted as a group
#' @return a numeric vector containing the Gorard's segregation index value for 
#' each group
#' @examples x <- segdata@data[ ,1:2]
#' Gorard(x)
#' @references Gorard S. (2000) \emph{Education and Social Justice}. 
#' Cardiff, University of Wales Press
#' @description Gorard's index is an alternative to \code{\link{ISDuncan}}'s 
#' index, which measures the dissimilarity between the distribution of a 
#' group and the total population. 
#' @seealso One-group evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Atkinson}}, 
#' \code{\link{HTheil}}, '\code{\link{ISWong}}, \code{\link{ISMorrill}},
#' \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{Gini2}}, 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export


Gorard <- function(x) {
  x <- segdataclean(as.matrix(x))$x
  result <- vector(length = ncol(x))
  varTotal <- colSums(x)
  for (k in 1:ncol(x)) result[k] <- 0.5 * sum(abs(x[, k]/varTotal[k] - rowSums(x)/sum(x)))
  return(round(result, 4))
}

#' A function to compute Spatial Gini's segregation index 
#'
#' @usage Gini(x)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return a numeric vector containing the Gini's segregation index value for 
#' each group
#' @examples x <- segdata@data[ ,1:2]
#' Gini(x)
#' @references Duncan O. D. and Duncan B. (1955) \emph{A Methodological 
#' Analysis of Segregation Indexes}. American Sociological Review 41, 
#' pp. 210-217
#' @description The spatial version of the Gini index can be derived from 
#' the Lorenz curve as the area between the segregation curve and the 
#' diagonal. 
#' @seealso Other one-group  evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Atkinson}}, \code{\link{Gorard}}, 
#' \code{\link{HTheil}}, '\code{\link{ISWong}}, \code{\link{ISMorrill}},
#' \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{Gini2}}, 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export


Gini <- function(x) {
  x <- segdataclean(as.matrix(x))$x
  result <- vector(length = ncol(x))
  pij <- array(0, dim = c(ncol(x), nrow(x), nrow(x)))
  p <- x/rowSums(x)
  pTotal <- colSums(x)/sum(x)
  for (k in 1:ncol(x)) {
    for (i in 1:nrow(x)) pij[k, , i] <- p[i, k]
    for (i in 1:nrow(x)) pij[k, i, ] <- abs(pij[k, i, ] - p[i, k])
    matprovi <- (rowSums(x) %*% t(rowSums(x))) * pij[k, , ]
    result[k] <- sum(matprovi)/(2 * sum(x)^2 * pTotal[k] * (1 - pTotal[k]))
  }
  return(round(result, 4))
}



#' A function to compute Spatial Gini's between group index 
#'
#' @usage Gini2(x)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return a matrix with between group Gini index 
#' @examples x <- segdata@data[ ,1:2]
#' Gini2(x)
#' @references Duncan O. D. and Duncan B. (1955) \emph{A Methodological 
#' Analysis of Segregation Indexes}. American Sociological Review 41, 
#' pp. 210-217
#' @description The between group version of Gini index is obtained 
#' by computing the index for a subpopulation formed by each pair of groups 
#' @seealso Other one-group  evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, 
#' \code{\link{Gorard}}, \code{\link{Atkinson}}, 
#' \code{\link{HTheil}}, '\code{\link{ISWong}}, \code{\link{ISMorrill}},
#' \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{DIMorrill}}, 
#' \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export

Gini2 <- function(x) {
    x <- segdataclean(as.matrix(x))$x
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    for (k1 in 1:(ncol(x) - 1)) for (k2 in (k1 + 1):ncol(x)) {
        xprovi <- x[, c(k1, k2)]
        xprovi <- xprovi[rowSums(xprovi) > 0, ]
        result[k1, k2] <- Gini(xprovi)[1]
    }
    result <- result + t(result)
    return(round(result, 4))
}


#' A function to compute Duncan dissimilarity segregation index
#'
#' @usage DIDuncan(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return a matrix containing dissimilarity index values
#' @references Duncan O. D. and Duncan B. (1955) \emph{A Methodological 
#' Analysis of Segregation Indexes}. American Sociological Review 41, 
#' pp. 210-217
#' @description Duncan's dissimilarity index is the segregation index 
#' most commonly used in the literature. It is derived from Lorenz 
#' curves as the maximum difference between the segregation curve 
#' and the diagonal. The index measures the unevenness of a group's 
#' spatial distribution compared to another group. It can be 
#' interpreted as the share of the group that would have to move to 
#' achieve an even distribution compared to another group.
#' @examples x <- segdata@data[ ,1:2]
#' DIDuncan(x) 
#' @seealso Other one-group  evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{Atkinson}}, \code{\link{HTheil}}, 
#' \code{\link{ISWong}}, \code{\link{ISMorrill}}, \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export



DIDuncan <- function(x) {
  x <- segdataclean(as.matrix(x))$x
  result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
  varTotal <- colSums(x)
  for (k1 in 1:(ncol(x) - 1)) for (k2 in (k1 + 1):ncol(x)) 
    result[k1, k2] <- 0.5 * sum(abs(x[, k1]/varTotal[k1] - x[, k2]/varTotal[k2]))
  result <- result+t(result)
  return(round(result, 4))
}




#' A function to compute Morrill's dissimilarity index
#'
#' @usage DIMorrill(x, c = NULL, queen = FALSE, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param c - a standard binary contiguity (adjacency) symmetric matrix where 
#' each element \emph{Cij} equals 1 if \emph{i}-th and \emph{j}-th spatial 
#' units are adjacent, and 0 otherwise.
#' @param queen - a logical parameter difining criteria used for contiguity 
#' matrix computation, TRUE for queen, FALSE (by default) for rook 
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) .
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix with Morrill's dissimilarity index values 
#' @references Morrill B. (1991) \emph{On the measure of geographic 
#' segregation}. Geography research forum, 11, pp. 25-36.
#' @description Morrill's dissimilarity index is a development of 
#' \code{\link{DIDuncan}}'s index which takes into account the 
#' interactions between spatial units(contiguity). The function can 
#' be used in two ways: to provide a contiguity matrix or a external 
#' geographic information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' contiguity <- contig(segdata)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' DIMorrill(x, c = contiguity) 
#' 
#' DIMorrill(x, spatobj = segdata, queen = FALSE)
#' 
#' DIMorrill(x, folder = foldername, shape = shapename) 
#' @seealso Other one-group  evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{Atkinson}}, \code{\link{HTheil}}, 
#' \code{\link{ISWong}}, \code{\link{ISMorrill}}, \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{DIMorrillK}}, \code{\link{DIWong}}
#' @export


DIMorrill <- function(x, c = NULL, queen = FALSE, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(c)) 
        c <- contig(spatobj = spatobj, folder = folder, shape = shape, queen = queen)
    cldata <- segdataclean(x, c = c)
    x <- cldata$x
    c <- cldata$c
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    DI <- DIDuncan(x)
    pij <- array(0, dim = c(ncol(x), nrow(x), nrow(x)))
    for (k1 in 1:(ncol(x) - 1)) for (k2 in (k1 + 1):ncol(x)) {
        for (i in 1:nrow(x)) pij[k1, , i] <- x[i, k1]/(x[i, k1] + x[i, k2])
        for (i in 1:nrow(x)) pij[k1, i, ] <- abs(pij[k1, i, ] - x[i, k1]/(x[i, k1] + x[i, k2]))
        pij[k1, , ][is.nan(pij[k1, , ])] <- 0
        matprovi <- c %*% pij[k1, , ]
        matprovi <- matprovi * diag(nrow(x))
        result[k1, k2] <- DI[k1, k2] - sum(matprovi)/sum(c)
        result[k2, k1] <- result[k1, k2]
    }
    return(round(result, 4))
}



#' A function to compute K-th order Morrill's dissimilarity index
#'
#' @usage DIMorrillK(x, ck = NULL, queen = FALSE, spatobj = NULL, 
#' folder = NULL, shape = NULL, K = 2, f = 'exp', beta = 1, prec = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param ck - a list with contiguity matrix for each order (from 1 to K)
#' @param queen - logical parameter difining criteria used for contiguity 
#' matrix computation, TRUE for queen, FALSE (by default) for rook 
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) .
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @param K - contiguity matrix order
#' @param f - the distance function, f = 'exp' (by default) for negative 
#' exponential function and f = 'rec' for reciprocal function
#' @param beta - distance decay parameter
#' @param prec - precision parameter. If not NULL, the function stop computing
#' the spatial interaction when the impact on the indice is bellow 10 ^ (-prec)
#' @return a matrix with Generalized Morrill's dissimilarity index values 
#' @references Morrill B. (1991) \emph{On the measure of geographic 
#' segregation}. Geography research forum, 11, pp. 25-36.
#' @description This function compute an adaptation of Morrill's dissimilarity 
#' index which takes into account the interactions between spatial units
#' defined by K order contiguity matrix. The function can be used in two ways: 
#' to provide a contiguity matrix or a external geographic information source 
#' (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' DIMorrillK(x, spatobj = segdata, queen = FALSE, K = 3)
#' 
#' DIMorrillK(x, folder = foldername, shape = shapename, K = 4, f = 'rec') 
#' @seealso Other one-group  evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{Atkinson}}, \code{\link{HTheil}}, 
#' \code{\link{ISWong}}, \code{\link{ISMorrill}}, \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{DIMorrill}}, \code{\link{DIWong}}
#' @export

DIMorrillK <- function(x, ck = NULL, queen = FALSE, spatobj = NULL, folder = NULL, 
                       shape = NULL, K = 2, f = "exp", beta = 1, prec = NULL) {
    x <- as.matrix(x)
    if (K == 1) 
        result <- DIMorrill(x, ck[[1]], queen, spatobj, folder, shape)
    if (K > 1) {
        if (is.null(ck)) {
            if (is.null(spatobj)) 
                spatobj <- sf::st_read(dsn = folder, layer = shape)
            ngb <- spdep::poly2nb(spatobj, queen = queen)
            ngbk <- spdep::nblag(ngb, K)
            if (sum(spdep::card(ngbk[[K]])) == 0) {
                for (k in K:1) if (sum(spdep::card(ngbk[[k]])) == 0) 
                  kk <- k
                K <- kk - 1
                ngbk <- spdep::nblag(ngb, K)
            }
            ck <- vector("list", K)
            for (k in 1:K) ck[[k]] <- spdep::nb2mat(ngbk[[k]], style = "B", zero.policy = TRUE)
        } else {
            cldata <- segdataclean(x, ck = ck)
            x <- cldata$x
            ck <- cldata$ck
            if (is.null(K)) 
                K <- length(ck)
        }
        result <- DIDuncan(x)
        pij <- array(0, dim = c(ncol(x), nrow(x), nrow(x)))
        for (k1 in 1:(ncol(x) - 1)) for (k2 in (k1 + 1):ncol(x)) {
            for (i in 1:nrow(x)) pij[k1, , i] <- x[i, k1]/(x[i, k1] + x[i, k2])
            for (i in 1:nrow(x)) pij[k1, i, ] <- abs(pij[k1, i, ] - x[i, k1]/(x[i, k1] + x[i, k2]))
            pij[k1, , ][is.nan(pij[k1, , ])] <- 0
            k <- 1
            cond <- TRUE
            while (cond) {
                if (f == "exp") 
                  interact <- exp(beta * (-k + 1))
                if (f == "rec") 
                  interact <- 1/(k^beta)
                matprovi <- ck[[k]] %*% pij[k1, , ]
                matprovi <- matprovi * diag(nrow(x))
                result[k1, k2] <- result[k1, k2] - sum(matprovi)/sum(ck[[k]]) * interact
                result[k2, k1] <- result[k1, k2]
                k <- k + 1
                if (k > K) 
                  cond <- FALSE
                if (!is.null(prec)) 
                  if (interact < 10^-prec) 
                    cond <- FALSE
            }
        }
    }
    return(round(result, 4))
}

#' A function to compute Wongs's dissimilarity index
#'
#' @usage DIWong(x, b = NULL,  a = NULL, p = NULL, ptype = 'int', variant = 's', 
#' spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' totals because this will be interpreted as a group
#' @param b - a common boundaries matrix where each element \emph{Bij} 
#' equals the shared boundary of \emph{i}-th and \emph{j}-th spatial units.
#' @param p - a numeric vector containing spatial units perimeters.
#' @param ptype - a string variable giving two options for perimeter calculation
#' when a spatial object or shapefile is provided: 'int' to use only interior
#' borders of spatial units, and 'all' to use entire borders, including to
#' the exterior of the area
#' @param a - a numeric vector containing spatial unit areas
#' @param variant - a character variable that allows to choose the index version: 
#' variant = 's' for the dissimilarity index adjusted for contiguous spatial units
#' boundary lengths and perimeter/area ratio (by default) and variant = 'w' 
#' for the version without perimeter/area ratio
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix containing Wong's dissimilarity index values 
#' @references Wong D. W. S. (1993) \emph{Spatial Indices of Segregation}. 
#' Urban Studies, 30 (3), pp. 559-572.
#' @description Wong's dissimilarity index is a development of 
#' \code{\link{DIDuncan}}'s which takes into account the interactions 
#' between spatial units(common boundaries and perimeter/area ratios). 
#' The function can be used in two ways: to provide spatial data (
#' boundaries matrix, a perimeter vector and an area vector) 
#' or a external geographic information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' bound <- boundaries(segdata)
#' per <- perimeter(segdata)
#' ar <- area(segdata)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' DIWong(x, b = bound, p = per, a = ar) 
#' 
#' DIWong(x, spatobj = segdata, variant = 'w') 
#' 
#' DIWong(x, folder = foldername, shape = shapename, ptype ='all') 
#' @seealso Other one-group  evenness indices: 
#' \code{\link{ISDuncan}}, \code{\link{Gini}}, \code{\link{Gorard}}, 
#' \code{\link{Atkinson}}, \code{\link{HTheil}}, 
#' '\code{\link{ISWong}}, \code{\link{ISMorrill}}, \code{\link{ISMorrillK}}
#' @seealso Between groups dissimilarity indices: 
#' \code{\link{DIDuncan}}, \code{\link{DIMorrill}}, \code{\link{DIMorrillK}}
#' @export


DIWong <- function(x, b = NULL, a = NULL, p = NULL, ptype = "int", variant = "s", spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(b)) 
        b <- boundaries(spatobj = spatobj, folder = folder, shape = shape)
    if (is.null(p)) {
        if (ptype == "all") 
            p <- perimeter(spatobj = spatobj, folder = folder, shape = shape)
        if (ptype == "int") 
            p <- rowSums(b)
    }
    if (is.null(a)) 
        a <- area(spatobj = spatobj, folder = folder, shape = shape)
    cldata <- segdataclean(x, b = b, p = p, a = a)
    x <- cldata$x
    b <- cldata$b
    p <- cldata$p
    a <- cldata$a
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    DI <- DIDuncan(x)
    pij <- array(0, dim = c(ncol(x), nrow(x), nrow(x)))
    w <- b/sum(b)
    if (variant == "w") 
        for (k1 in 1:(ncol(x) - 1)) for (k2 in (k1 + 1):ncol(x)) {
            for (i in 1:nrow(x)) pij[k1, , i] <- x[i, k1]/(x[i, k1] + x[i, k2])
            for (i in 1:nrow(x)) pij[k1, i, ] <- abs(pij[k1, i, ] - x[i, k1]/(x[i, k1] + x[i, k2]))
            pij[k1, , ][is.nan(pij[k1, , ])] <- 0
            matprovi <- w %*% (pij[k1, , ])
            matprovi <- matprovi * diag(nrow(x))
            result[k1, k2] <- DI[k1, k2] - sum(matprovi)
            result[k2, k1] <- result[k1, k2]
        }
    if (variant == "s") {
        PerAij <- matrix(data = 0, nrow = nrow(x), ncol = nrow(x))
        for (i in 1:nrow(x)) PerAij[, i] <- p[i]/a[i]
        for (i in 1:nrow(x)) PerAij[i, ] <- PerAij[i, ] + p[i]/a[i]
        maxPA <- max(p/a)
        for (k1 in 1:(ncol(x) - 1)) for (k2 in (k1 + 1):ncol(x)) {
            for (i in 1:nrow(x)) pij[k1, , i] <- x[i, k1]/(x[i, k1] + x[i, k2])
            for (i in 1:nrow(x)) pij[k1, i, ] <- abs(pij[k1, i, ] - x[i, k1]/(x[i, k1] + x[i, k2]))
            pij[k1, , ][is.nan(pij[k1, , ])] <- 0
            matprovi <- w %*% (pij[k1, , ] * PerAij)
            matprovi <- matprovi * diag(nrow(x))
            result[k1, k2] <- DI[k1, k2] - sum(matprovi)/(2 * maxPA)
            result[k2, k1] <- result[k1, k2]
        }
    }
    return(round(result, 4))
}



#################### 

# EXPOSITION INDEXES

#################### 

#' A function to compute Bell's isolation index (xPx)
#'
#' @usage xPx(x, exact = FALSE) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param exact - a logical variable to specifiy the index version: 
#' exact = FALSE (by default) for the approximate version of the index, 
#' and exact = TRUE for the exact version
#' @return a numeric vector containing the isolation index value for 
#' each group
#' @references Bell W. (1954) \emph{A probability model for the 
#' measurement of ecological segregation}. Social Forces 32(4), 
#' pp. 357-364
#' @description The isolation index, xPx, is an exposure index 
#' that measures the probability that two members of a group share 
#' the same spatial unit. This index can be calculated using the 
#' approximate or the exact method (see Bell, 1954).
#' @examples x <- segdata@data[ ,7:8]
#' xPx(x) 
#' xPx(x, exact = TRUE)
#' @seealso Isolation indices: 
#' \code{\link{Eta2}},  \code{\link{DPxx}}
#' @seealso Interaction indices: 
#' \code{\link{xPy}}, \code{\link{DPxy}}
#' @export

xPx <- function(x, exact = FALSE) {
  x <- segdataclean(as.matrix(x))$x
  result <- vector(length = ncol(x))
  varTotal <- colSums(x)
  if (exact == FALSE) 
    for (k in 1:ncol(x)) result[k] <- sum(x[, k]/varTotal[k] * x[, k]/rowSums(x))
  if (exact == TRUE) 
    for (k in 1:ncol(x)) result[k] <- sum(x[, k]/varTotal[k] * (x[, k] - 1)/(rowSums(x) - 1))
  return(round(result, 4))
}



#' A function to compute the distance-decay isolation index (DPxx)
#'
#' @usage DPxx(x, d = NULL, distin = 'm',  distout = 'm', diagval = '0', beta = 1, 
#' spatobj = NULL, folder = NULL, shape = NULL) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param d - a matrix of the distances between spatial unit centroids
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) 
#' (White, 1983) 
#' @param beta - distance decay parameter
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing the distance-decay isolation index 
#' value for each group
#' @references Morgan, B. S. (1983) \emph{A Distance-Decay Based Interaction 
#' Index to Measure Residential Segregation}. Area 15(3),  pp. 211-217.
#' @description The distance decay isolation index, DPxx, is a spatial
#' adaptation of isolation index \code{\link{xPx}}. The function can be 
#' used in two ways: to provide a distance matrix or a external geographic 
#' information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' ar <- area(segdata)
#' dist <- distance(segdata)
#' diag(dist)<-sqrt(ar) * 0.6
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' DPxx(x, d = dist)
#' 
#' DPxx(x, spatobj = segdata, diagval = 'a')
#' 
#' DPxx(x, folder = foldername, shape = shapename, diagval = '0') 
#' @seealso Isolation indices: 
#' \code{\link{xPx}},  \code{\link{Eta2}}
#' @seealso Interaction indices: 
#' \code{\link{xPy}}, \code{\link{DPxy}}
#' @export


DPxx <- function(x, d = NULL, distin = "m", distout = "m", diagval = "0", beta = 1, spatobj = NULL, folder = NULL, shape = NULL) {
  x <- as.matrix(x)
  if (is.null(d)) 
    d <- distance(spatobj = spatobj, folder = folder, shape = shape, distin = distin, distout = distout, diagval = diagval)
  cldata <- segdataclean(x, d = d)
  x <- cldata$x
  d <- cldata$d
  result <- vector(length = ncol(x))
  varTotal <- colSums(x)
  dd <- exp(-beta * d)
  K1 <- dd %*% rowSums(x)
  K2 <- dd * rowSums(x)
  K <- K2/as.vector(K1)
  for (k in 1:ncol(x)) {
    X1 <- K %*% (x[, k]/rowSums(x))
    X2 <- (x[, k]/varTotal[k]) * X1
    result[k] <- sum(X2)
  }
  return(round(result, 4))
}




#' A function to compute adjusted isolation index (Eta2)
#'
#' @usage Eta2(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return a numeric vector containing the adjusted isolation index value for 
#' each group
#' @references Bell W. (1954) \emph{A probability model for the 
#' measurement of ecological segregation}. Social Forces 32(4), 
#' pp. 357-364
#' @references Duncan O. D. and Duncan B. (1955) \emph{ 
#' Residential Distribution and Occupational Stratification.}. 
#' American Journal of Sociology 60 (5), pp. 493-503
#' @description The adjusted isolation index is the standardized 
#' version of the isolation index, \code{\link{xPx}}, which 
#' controls for the effect of total population structure. Using 
#' the approximate version of xPx, the adjusted index is equal 
#' to Eta2 (the square of the correlation ratio) which, in the 
#' case of the binomial variable, is identical to the square of 
#' the mean square contingency coefficient phi. It can be used 
#' as a segregation score and varies from 0 (minimum segregation) 
#' to 1 (maximum segregation).
#' @examples x <- segdata@data[ ,1:2]
#' Eta2(x) 
#' @seealso Isolation indices: 
#' \code{\link{xPx}},  \code{\link{DPxx}}
#' @seealso Interaction indices: 
#' \code{\link{xPy}}, \code{\link{DPxy}}
#' @export



Eta2 <- function(x) {
  x <- segdataclean(as.matrix(x))$x
  result <- vector(length = ncol(x))
  varTotal <- colSums(x)
  pTotal <- colSums(x)/sum(x)
  for (k in 1:ncol(x)) {
    result[k] <- sum(x[, k]/varTotal[k] * x[, k]/ rowSums(x))
    result[k] <- (result[k] - pTotal[k])/(1 - pTotal[k])
  }
  return(round(result, 4))
}



#' A function to compute interaction index (xPy)
#'
#' @usage xPy(x, exact = FALSE) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param exact - a logical variable to specifiy the index version: 
#' exact = FALSE (by default) for the approximate version of the index, 
#' and exact = TRUE for the exact version
#' @return a matrix with interaction index values
#' @references Bell W. (1954) \emph{A probability model for the 
#' measurement of ecological segregation}. Social Forces 32(4),
#'  pp. 357-364
#' @description The interaction index, xPy, is an exposure 
#' between groups index which measures the probability that a member 
#' of a group shares the same spatial unit with a member of another 
#' group. The index can be calculated with the approximate or exact 
#' method (see Bell, 1954).
#' @examples x <- segdata@data[ ,1:2]
#' xPy(x) 
#' @seealso Isolation indices: 
#' \code{\link{xPx}}, \code{\link{Eta2}},  \code{\link{DPxx}}
#' @seealso Distance decay interaction index: \code{\link{DPxy}}
#' @export



xPy <- function(x, exact = FALSE) {
    x <- segdataclean(as.matrix(x))$x
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    varTotal <- colSums(x)
    t <- rowSums(x)
    if (exact == FALSE) 
        for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) result[k1, k2] <- sum(x[, k1]/varTotal[k1] * x[, k2]/t)
    if (exact == T) 
        for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) if (k1 != k2) {
            result[k1, k2] <- sum(x[, k1]/varTotal[k1] * x[, k2]/(t - 1))
        } else result[k1, k2] <- sum(x[, k1]/varTotal[k1] * (x[, k2] - 1)/(t - 1))
    return(round(result, 4))
}





#' A function to compute the distance-decay interaction index (DPxy)
#'
#' @usage DPxy(x, d = NULL, distin = 'm',  distout = 'm', diagval = '0', 
#' beta = 1, spatobj = NULL, folder = NULL, shape = NULL) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param d - a matrix of the distances between spatial unit centroids
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) (White, 1983) 
#' @param beta - distance decay parameter
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric matrix containing the distance-decay isolation index 
#' values
#' @references Morgan, B. S. (1983) \emph{An Alternate Approach to the 
#' Development of a Distance-Based Measure of Racial Segregation}. 
#' American Journal of Sociology 88,  pp. 1237-1249.
#' @description The distance decay interaction index, DPxy, is a 
#' spatial adaptation of interaction index \code{\link{xPy}}. 
#' The function can be used in two ways: to provide a distance matrix 
#' or a external geographic information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' ar <- area(segdata)
#' dist <- distance(segdata)
#' diag(dist)<-sqrt(ar) * 0.6
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' DPxy(x, d = dist)
#' 
#' DPxy(x, spatobj = segdata, diagval = 'a')
#' 
#' DPxy(x, folder = foldername, shape = shapename, diagval = '0') 
#' @seealso Isolation indices: 
#' \code{\link{xPx}}, \code{\link{Eta2}},  \code{\link{DPxx}}
#' @seealso Interaction index: \code{\link{xPy}}
#' @export


DPxy <- function(x, d = NULL, distin = "m", distout = "m", diagval = "0", beta = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(d)) 
        d <- distance(spatobj = spatobj, folder = folder, shape = shape, distin = distin, distout = distout, diagval = diagval)
    cldata <- segdataclean(x, d = d)
    x <- cldata$x
    d <- cldata$d
    varTotal <- colSums(x)
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    dd <- exp(-beta * d)
    K1 <- dd %*% rowSums(x)
    K2 <- dd * rowSums(x)
    K <- K2/as.vector(K1)
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
        X1 <- K %*% (x[, k2]/rowSums(x))
        X2 <- (x[, k1]/varTotal[k1]) * X1
        result[k1, k2] <- sum(X2)
    }
    return(round(result, 4))
}


#' A function adapted from seg package to compute spatial exposure/isolation indices
#'
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) .
#' @param ... - other parameters of spseg function from seg package.
#' @return A matrix with Reardon's spatial exposure/isolation indices
#' @references Reardon, S. F. and O'Sullivan, D. (2004) 
#' \emph{Measures of spatial segregation}.
#' Sociological Methodology, 34, 121-162.
#' @references Hong S.Y., O'Sullivan D., Sadahiro Y. (2014) 
#' \emph{Implementing Spatial Segregation Measures in R'}.
#' PLoS ONE, 9(11)
#' @description  A function adapted from seg package (Hong et al. 2014) 
#' to compute spatial exposure/isolation indices developed by 
#' Reardon and O'Sullivan (2004)
#' @examples x <- segdata@data[ ,1:2]
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' spatinteract(x, spatobj = segdata)
#' 
#' spatinteract(x, folder = foldername, shape = shapename) 
#' 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}}, \code{\link{DMulti}},  
#' \code{\link{HMulti}}, \code{\link{RelDivers}}
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' \code{\link{ISimpson}}, 
#' @export 


spatinteract <- function(x, spatobj = NULL, folder = NULL, shape = NULL, ...) {
    
    if (is.null(spatobj) & !is.null(folder) & !is.null(shape)) 
        spatobj <- sf::st_read(dsn = folder, layer = shape)
    if (class(spatobj)[1]!="SpatialPolygonsDataFrame") spatobj <- sf::as_Spatial(spatobj)
    result <- seg::spseg(spatobj, x, ...)
    return(round(result@p, 4))
}



####################### 

# CONCENTRATION INDEXES

####################### 


#' A function to compute Delta index
#'
#' @usage Delta(x, a = NULL, spatobj = NULL, folder = NULL, shape = NULL) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param a - a numeric vector containing spatial unit areas
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing Delta index value for 
#' each group
#' @references Duncan O. D., Cuzzoert  and Duncan B. (1961) 
#' \emph{Problems in analyzing areal data}. Statistical geography, 
#' Glencoe, Illinois: The free press of Glencoe
#' @description The Delta index is a specific application of dissimilarity 
#' index \code{\link{DIDuncan}} which simply measures the dissimilarity
#' between the spatial distribution of a group and the spatial 
#' distribution of available land. It can be interpreted as the share of group 
#' that would have to move to achieve uniform density over all spatial units. 
#' The function can be used in two ways: to provide an area vector or 
#' a external geographic information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' ar <- area(segdata)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' Delta(x, a = ar) 
#' 
#' Delta(x, spatobj = segdata)
#' 
#' Delta(x, folder = foldername, shape = shapename) 
#' @seealso Absolute Concentration Index: \code{\link{ACO}}
#' @seealso Relative Concentration Index: \code{\link{RCO}}
#' @export


Delta <- function(x, a = NULL, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(a)) 
        a <- area(spatobj = spatobj, folder = folder, shape = shape)
    cldata <- segdataclean(x, a = a)
    x <- cldata$x
    a <- cldata$a
    result <- vector(length = ncol(x))
    varTotal <- colSums(x)
    for (k in 1:ncol(x)) result[k] <- 0.5 * sum(abs(x[, k]/varTotal[k] - a/sum(a)))
    return(round(result, 4))
}



#' A function to compute Absolute Concentration index (ACO)
#'
#' @usage ACO(x, a = NULL, spatobj = NULL, folder = NULL, shape = NULL) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param a - a numeric vector containing spatial unit areas
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing Absolute Concentration index value for 
#' each group
#' @references Massey D. S. and Denton N. A. (1988) \emph{
#' The dimensions of residential segregation}. 
#' Social Forces 67(2),  pp. 281-315.
#' @description The absolute concentration index, ACO, computes 
#' the total area inhabited by a group, and compares the result 
#' to the minimum and maximum possible areas that could be 
#' inhabited by that group in the study area. The function can be 
#' used in two ways: to provide an area vector or a external 
#' geographic information source (spatial object or shape file). 
#' @examples x <- GreHSize@data[ ,3:5]
#' ar <- area(GreHSize)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'GreHSize'
#' 
#' ACO(x, a = ar) 
#' 
#' ACO(x, spatobj = GreHSize)
#' 
#' ACO(x, folder = foldername, shape = shapename) 
#' 
#' @seealso Delta Index: \code{\link{Delta}}
#' @seealso Relative Concentration Index: \code{\link{RCO}}
#' @export


ACO <- function(x, a = NULL, spatobj = NULL, folder = NULL, shape = NULL) {
  x <- as.matrix(x)
  if (is.null(a)) 
    a <- area(spatobj = spatobj, folder = folder, shape = shape)
  cldata <- segdataclean(x, a = a)
  x <- cldata$x
  a <- cldata$a
  varTotal <- colSums(x)
  xprovi <- as.data.frame(cbind(x, a))
  xprovi <- xprovi[order(xprovi$a), ]
  xprovi$Total <- rowSums(xprovi) - xprovi$a
  result <- vector(length = ncol(x))
  n1 <- vector(length = ncol(x))
  n2 <- vector(length = ncol(x))
  T1 <- vector(length = ncol(x))
  T2 <- vector(length = ncol(x))
  for (k in 1:ncol(x)) {
    T1[k] <- 0
    i <- 0
    while (T1[k] < varTotal[k]) {
      i <- i + 1
      T1[k] <- T1[k] + xprovi$Total[i]
    }
    n1[k] <- i
    T2[k] <- 0
    i <- nrow(xprovi) + 1
    while (T2[k] < varTotal[k]) {
      i <- i - 1
      T2[k] <- T2[k] + xprovi$Total[i]
    }
    n2[k] <- i
  }
  for (k in 1:ncol(x)) {
    vartemp1 <- sum(xprovi[, k] * xprovi$a/varTotal[k])
    vartemp2 <- sum(xprovi$Total[1:n1[k]] * xprovi$a[1:n1[k]]/T1[k])
    vartemp3 <- sum(xprovi$Total[n2[k]:nrow(xprovi)] * xprovi$a[n2[k]:nrow(xprovi)]/T2[k])
    result[k] <- 1 - (vartemp1 - vartemp2)/(vartemp3 - vartemp2)
  }
  return(round(result, 4))
}


#' A function to compute Relative Concentration index (RCO)
#'
#' @usage RCO(x, a = NULL, spatobj = NULL, folder = NULL, shape = NULL) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param a - a numeric vector containing spatial unit areas
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix containing relative concentration index values
#' @references Massey D. S. and Denton N. A. (1988) \emph{
#' The dimensions of residential segregation}. 
#' Social Forces 67(2),  pp. 281-315.
#' @description The relative concentration index, measures 
#' the share of space occupied by a group compared to another group.
#' The function can be used in two ways: to provide an area vector or a 
#' external geographic information source (spatial object or shape file).
#' @examples x <- GreHSize@data[ ,3:5]
#' ar <- area(GreHSize)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'GreHSize'
#' 
#' RCO(x, a = ar) 
#' 
#' RCO(x, spatobj = GreHSize)
#' 
#' RCO(x, folder = foldername, shape = shapename) 
#' 
#' @seealso one-group concentration indices: 
#' \code{\link{Delta}},  \code{\link{ACO}}
#' @export


RCO <- function(x, a = NULL, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(a)) 
        a <- area(spatobj = spatobj, folder = folder, shape = shape)
    cldata <- segdataclean(x, a = a)
    x <- cldata$x
    a <- cldata$a
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    varTotal <- colSums(x)
    xprovi <- as.data.frame(cbind(x, a))
    xprovi <- xprovi[order(xprovi$a), ]
    xprovi$Total <- rowSums(xprovi) - xprovi$a
    n1 <- vector(length = ncol(x))
    n2 <- vector(length = ncol(x))
    T1 <- vector(length = ncol(x))
    T2 <- vector(length = ncol(x))
    for (k in 1:ncol(x)) {
        T1[k] <- 0
        i <- 0
        while (T1[k] < varTotal[k]) {
            i <- i + 1
            T1[k] <- T1[k] + xprovi$Total[i]
        }
        n1[k] <- i
        T2[k] <- 0
        i <- nrow(xprovi) + 1
        while (T2[k] < varTotal[k]) {
            i <- i - 1
            T2[k] <- T2[k] + xprovi$Total[i]
        }
        n2[k] <- i
    }
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
        vartemp1 <- sum(xprovi[, k1] * xprovi$a/varTotal[k1])
        vartemp1b <- sum(xprovi[, k2] * xprovi$a/varTotal[k2])
        vartemp2 <- sum(xprovi$Total[1:n1[k1]] * xprovi$a[1:n1[k1]]/T1[k1])
        vartemp3 <- sum(xprovi$Total[n2[k2]:nrow(xprovi)] * xprovi$a[n2[k2]:nrow(xprovi)]/T2[k2])
        result[k1, k2] <- ((vartemp1/vartemp1b) - 1)/((vartemp2/vartemp3) - 1)
    }
    return(round(result, 4))
}

####################### 

# CLUSTERING INDEXES

####################### 


#' A function to compute Absolute Clustering Index (ACL)
#'
#' @usage ACL(x, spatmat = 'c', c = NULL, queen = FALSE, d = NULL, distin = 'm',  
#' distout = 'm', diagval = '0', beta = 1, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param spatmat - the method used for spatial calculations: 'c' for the 
#' contiguity matrix (by default) or any other user spatial interaction matrix 
#' and 'd' for the inverse exponential function of the distance. 
#' @param c - a modified binary contiguity (adjacency) symmetric matrix where 
#' each element \emph{Cij} equals 1 if \emph{i}-th and \emph{j}-th spatial 
#' units are adjacent or identical, and 0 otherwise.
#' @param d - a matrix of the distances between spatial unit centroids
#' @param queen - logical parameter difining criteria used for contiguity 
#' matrix computation, TRUE for queen, FALSE (by default) for rook 
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) (White, 1983) 
#' @param beta - distance decay parameter
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing Absolute Clustering index value for 
#' each group
#' @references Massey D. S. and Denton N. A. (1988) \emph{
#' The dimensions of residential segregation}. 
#' Social Forces 67(2),  pp. 281-315.
#' @description The absolute clustering index, ACL, expresses the 
#' average number of a group's members in nearby spatial units, as 
#' a proportion of the total population in those spatial units. 
#' The spatial interactions can be expressed as a contiguity matrix 
#' (with diagonal equal to 1), as an inverse exponential function of the 
#' distance between spatial units centers (with diagonal equal 
#' to 0.6 of the square root of each spatial units area) or other 
#' user specified interaction matrix. The function can be used in two 
#' ways: to provide a spatial interactions matrix (a contiguity matrix 
#' or a distance matrix) or a external  geographic information source 
#' (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' contiguity <- contig(segdata)
#' diag(contiguity) <- 1
#' ar<-area(segdata)
#' dist <- distance(segdata)
#' diag(dist)<-sqrt(ar) * 0.6
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ACL(x, c = contiguity) 
#' 
#' ACL(x, spatobj = segdata)
#' 
#' ACL(x, spatmat = 'd', folder = foldername, shape = shapename) 
#'  
#' ACL(x,  spatmat = 'd', diagval = 'a', spatobj = segdata)
#' 
#' ACL(x, d = dist, spatmat = 'd')
#'
#' @seealso Proximity measures: \code{\link{Pxx}}, 
#' \code{\link{Pxy}}, \code{\link{Poo}}, \code{\link{SP}}
#' @seealso Relative Clustering Index: \code{\link{RCL}}
#' @export


ACL <- function(x, spatmat = "c", c = NULL, queen = FALSE, d = NULL, distin = "m", distout = "m", diagval = "0", beta = 1, spatobj = NULL, folder = NULL, 
    shape = NULL) {
    x <- as.matrix(x)
    if (spatmat == "c" & is.null(c)) {
        c <- contig(spatobj = spatobj, folder = folder, shape = shape, queen = queen)
        diag(c) <- 1
    }
    if (spatmat == "d" & is.null(d)) 
        d <- distance(spatobj = spatobj, folder = folder, shape = shape, distin = distin, distout = distout, diagval = diagval)
    if (spatmat == "d") 
        c <- exp(-beta * d)
    cldata <- segdataclean(x, c = c)
    x <- cldata$x
    c <- cldata$c
    result <- vector(length = ncol(x))
    varTotal <- colSums(x)
    t <- as.vector(rowSums(x))
    for (k in 1:ncol(x)) {
        vartemp1 <- sum((c %*% x[, k]) * x[, k]/varTotal[k])
        vartemp2 <- (sum(c) * varTotal[k])/(nrow(x)^2)
        vartemp3 <- sum((c %*% t) * x[, k]/varTotal[k])
        result[k] <- (vartemp1 - vartemp2)/(vartemp3 - vartemp2)
    }
    return(round(result, 4))
}


#' A function to compute the mean proximity between members of a group (Pxx)
#'
#' @usage Pxx(x, d = NULL, fdist = 'e', distin = 'm',  distout = 'm', diagval = '0', 
#' beta = 1, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param d - a matrix of the distances between spatial unit centroids
#' @param fdist - the method used for distance interaction matrix: 
#' e' for inverse exponential function (by default) and 'l' for linear.
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) (White, 1983)
#' @param beta - distance decay parameter 
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing Pxx index value for 
#' each group
#' @references White M. J. (1983) \emph{The Measurement of Spatial 
#' Segregation}. American Journal of Sociology, 88, p. 1008-1019
#' @description  Mean proximity, Pxx, computes the mean distance 
#' between the members of a group. The distance matrix can be expressed as  
#' a linear or as an inverse exponential function of the distance between 
#' spatial unit centroids.The function can be used in two ways: to provide  
#' a distance matrix  or a external geographic information source (spatial 
#' object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' ar<-area(segdata)
#' dist <- distance(segdata)
#' diag(dist)<-sqrt(ar) * 0.6
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' Pxx(x, spatobj = segdata)
#' 
#' Pxx(x, folder = foldername, shape = shapename, fdist = 'l') 
#' 
#' Pxx(x, spatobj = segdata, diagval ='a')
#' 
#' Pxx(x, d = dist, fdist = 'e')
#' 
#' @seealso Proximity measures: 
#' \code{\link{Pxy}}, \code{\link{Poo}}, \code{\link{SP}}
#' @seealso Clustering Indices: 
#' \code{\link{ACL}}, \code{\link{RCL}}
#' @export



Pxx <- function(x, d = NULL, fdist = "e", distin = "m", distout = "m", diagval = "0", beta = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(d)) 
        d <- distance(spatobj = spatobj, folder = folder, shape = shape)
    cldata <- segdataclean(x, d = d)
    x <- cldata$x
    d <- cldata$d
    if (fdist == "e") 
        d <- exp(-beta * d)
    varTotal <- colSums(x)
    result <- vector(length = ncol(x))
    for (k in 1:ncol(x)) result[k] <- sum((d %*% x[, k]) * x[, k]/(varTotal[k])^2)
    return(round(result, 4))
}




#' A function to compute the mean proximity between 
#' persons without regard to group (Poo)
#'
#' @usage Poo(x, d = NULL, fdist = 'e', distin = 'm',  distout = 'm', diagval = '0', 
#' itype = 'multi', beta = 1, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param d - a matrix of the distances between spatial unit centroids
#' @param fdist - the method used for distance interaction matrix: 
#' e' for inverse exponential function (by default) and 'l' for linear.
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) (White, 1983) 
#' @param itype - a character string defining the index type:
#' itype = 'multi' (by default) for the multi-group index (White, 1986)
#' or itype = 'between' for the between groups version (White, 1983)
#' @param beta - distance decay parameter
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return Poo index value(s) 
#' @references White M. J. (1983) \emph{The Measurement of Spatial 
#' Segregation}. American Journal of Sociology, 88, p. 1008-1019
#' @references  White, M. J. (1986) \emph{Segregation and Diversity Measures 
#' in Population Distribution}E. Population Index 52(2): 198-221.
#' @description Mean proximity, Poo, computes the mean distance 
#' between the individuals in the area with no regard for group.
#' The function can be used in two ways: to provide a distance matrix 
#' or a external geographic information source (spatial object 
#' or shape file) 
#' @examples x <- segdata@data[ ,1:2]
#' ar<-area(segdata)
#' dist <- distance(segdata)
#' diag(dist)<-sqrt(ar) * 0.6
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' Poo(x, spatobj = segdata)
#' 
#' Poo(x, folder = foldername, shape = shapename, fdist = 'l') 
#' 
#' Poo(x, spatobj = segdata, diagval ='a')
#' 
#' Poo(x, d = dist, fdist = 'e') 
#'
#' @seealso Proximity measures: \code{\link{Pxx}}, 
#' \code{\link{Pxy}},  \code{\link{SP}}
#' @seealso Clustering Indices: 
#' \code{\link{ACL}}, \code{\link{RCL}}
#' @export


Poo <- function(x, d = NULL, fdist = "e", distin = "m", distout = "m", diagval = "0", itype = "multi", beta = 1, spatobj = NULL, folder = NULL, shape = NULL) {
  x <- as.matrix(x)
  if (is.null(d)) 
    d <- distance(spatobj = spatobj, folder = folder, shape = shape, distin = distin, distout = distout, diagval = diagval)
  cldata <- segdataclean(x, d = d)
  x <- cldata$x
  d <- cldata$d
  varTotal <- colSums(x)
  if (fdist == "e") 
    d <- exp(-beta * d)
  if (itype == "between") {
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) result[k1, k2] <- sum((d %*% (x[, k1] + x[, k2])) * (x[, k1] + x[, k2])/(varTotal[k1] + varTotal[k2])^2)
  }
  if (itype == "multi") 
    result <- sum((d %*% rowSums(x)) * rowSums(x)/(sum(rowSums(x)))^2)
  return(round(result, 4))
}



#' A function to compute the mean proximity between 
#' persons of different groups (Pxy)
#'
#' @usage Pxy(x, d = NULL, fdist = 'e', distin = 'm',  distout = 'm', diagval = '0', 
#' beta = 1, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param d - a matrix of the distances between spatial unit centroids
#' @param fdist - the method used for distance interaction matrix: 
#' e' for inverse exponential function (by default) and 'l' for linear.
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) (White, 1983) 
#' @param beta - distance decay parameter
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix containing Pxy index values for each pair of groups
#' @references White M. J. (1983) \emph{The Measurement of 
#' Spatial Segregation}. American Journal of Sociology, 88, p. 1008-1019
#' @description Mean proximity, Pxy, computes the mean distance 
#' between the members of different groups.The function can be used in 
#' two ways: to provide a distance matrix or a external geographic 
#' information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' ar<-area(segdata)
#' dist <- distance(segdata)
#' diag(dist)<-sqrt(ar) * 0.6
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' Pxy(x, spatobj = segdata)
#' 
#' Pxy(x, folder = foldername, shape = shapename, fdist = 'l') 
#' 
#' Pxy(x, spatobj = segdata, diagval ='a')
#' 
#' Pxy(x, d = dist, fdist = 'e')
#'
#' @seealso Proximity measures: \code{\link{Pxx}}, 
#' \code{\link{Poo}},  \code{\link{SP}}
#' @seealso Clustering Indices: 
#' \code{\link{ACL}}, \code{\link{RCL}}
#' @export


Pxy <- function(x, d = NULL, fdist = "e", distin = "m", distout = "m", diagval = "0", beta = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(d)) 
        d <- distance(spatobj = spatobj, folder = folder, shape = shape, distin = distin, distout = distout, diagval = diagval)
    cldata <- segdataclean(x, d = d)
    x <- cldata$x
    d <- cldata$d
    if (fdist == "e") 
        d <- exp(-beta * d)
    varTotal <- colSums(x)
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) result[k1, k2] <- sum((d %*% (x[, k2])) * (x[, k1]))/varTotal[k1]/varTotal[k2]
    return(round(result, 4))
}


#' A function to compute the spatial proximity index (SP)
#'
#' @usage SP(x, d = NULL, fdist = 'e', distin = 'm',  distout = 'm', diagval = '0', 
#' itype = 'multi', beta = 1, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param d - a matrix of the distances between spatial unit centroids
#' @param fdist - the method used for distance interaction matrix: 
#' e' for inverse exponential function (by default) and 'l' for linear.
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) (White, 1983) 
#' @param itype - a character string defining the index type:
#' itype = 'multi' (by default) for the multi-group index (White, 1986),
#' itype = 'between' for the between groups version (White, 1983), or
#' itype = 'one' for the one-group version (Apparicio et al, 2008)
#' @param beta - distance decay parameter
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return spatial proximity index value(s) 
#' @references White M. J. (1983) \emph{The Measurement of Spatial 
#' Segregation}. American Journal of Sociology, 88, p. 1008-1019.
#' @references  White, M. J. (1986) \emph{Segregation and Diversity Measures 
#' in Population Distribution}E. Population Index 52(2): 198-221.
#' @references  Apparicio, P., V. Petkevitch and M. Charron (2008): \emph{Segregation 
#' Analyzer: A C#.Net application for calculating residential segregation indices}, 
#' Cybergeo: European Journal of Geography, 414, 1-27.
#' @description The spatial proximity index, SP, compares the clustering 
#' level (mean proximity) of a group compared to another group. 
#' The function can be used in two ways: to provide a distance matrix 
#' or a external geographic information source (spatial object 
#' or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' ar<-area(segdata)
#' dist <- distance(segdata)
#' diag(dist)<-sqrt(ar) * 0.6
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' SP(x, spatobj = segdata)
#' 
#' SP(x, folder = foldername, shape = shapename, fdist = 'l', itype = 'between') 
#' 
#' SP(x, spatobj = segdata, diagval ='a', itype = 'one')
#' 
#' SP(x, d = dist, fdist = 'e')
#'
#' @seealso Proximity measures: \code{\link{Pxx}}, 
#' \code{\link{Pxy}},  \code{\link{Poo}}
#' @seealso Clustering Indices: 
#' \code{\link{ACL}}, \code{\link{RCL}}
#' @export



SP <- function(x, d = NULL, fdist = "e", distin = "m", distout = "m", diagval = "0", itype = "multi", beta = 1, spatobj = NULL, folder = NULL, shape = NULL) {
  x <- as.matrix(x)
  if (is.null(d)) 
    d <- distance(spatobj = spatobj, folder = folder, shape = shape, distin = distin, distout = distout, diagval = diagval)
  cldata <- segdataclean(x, d = d)
  x <- cldata$x
  d <- cldata$d
  if (fdist == "e") 
    d <- exp(-beta * d)
  varTotal <- colSums(x)
  if (itype == "between") {
    Poo1 <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) Poo1[k1, k2] <- sum((d %*% (x[, k1] + x[, k2])) * (x[, k1] + x[, k2])/(varTotal[k1] + varTotal[k2])^2)
  }
  if (itype == "multi" || itype == "one") 
    Poo1 <- sum((d %*% rowSums(x)) * rowSums(x)/(sum(rowSums(x)))^2)
  Pxx1 <- vector(length = ncol(x))
  for (k in 1:ncol(x)) Pxx1[k] <- sum((d %*% x[, k]) * x[, k]/(varTotal[k])^2)
  if (itype == "multi") 
    result <- sum(Pxx1 * varTotal)/(sum(x) * Poo1)
  if (itype == "one") 
    result <- Pxx1/Poo1
  if (itype == "between") {
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) result[k1, k2] <- (varTotal[k1] * Pxx1[k1] + varTotal[k2] * Pxx1[k2])/((varTotal[k1] + varTotal[k2]) * 
                                                                                                                         Poo1[k1, k2])
  }
  return(round(result, 4))
}





#' A function to compute the relative clustering index (RCL)
#'
#' @usage RCL(x, d = NULL, fdist = 'e', distin = 'm',  distout = 'm', diagval = '0', 
#' beta = 1, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param d - a matrix of the distances between spatial unit centroids
#' @param fdist - the method used for distance interaction matrix: 
#' e' for inverse exponential function (by default) and 'l' for linear.
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) (White, 1983) 
#' @param beta - distance decay parameter
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix containing relative clustering index values for each pair of groups
#' @references Massey D. S. and Denton N. A. (1988) \emph{The dimensions 
#' of residential segregation}. Social Forces 67(2),  pp. 281-315.
#' @description The relative clustering index, RCL, compares the mean 
#' proximity of a group to the mean proximity of another group. 
#' The function can be used in two ways: to provide a distance matrix 
#' or a external geographic information source (spatial object 
#' or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' ar<-area(segdata)
#' dist <- distance(segdata)
#' diag(dist)<-sqrt(ar) * 0.6
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' RCL(x, spatobj = segdata)
#' 
#' RCL(x, folder = foldername, shape = shapename, fdist = 'l') 
#' 
#' RCL(x, spatobj = segdata, diagval ='a')
#' 
#' RCL(x, d = dist, fdist = 'e')
#' 
#' @seealso Proximity measures: \code{\link{Pxx}}, 
#' \code{\link{Pxy}},  \code{\link{Poo}},  \code{\link{SP}}
#' @seealso Clustering Indices: \code{\link{ACL}}
#' @export


RCL <- function(x, d = NULL, fdist = "e", distin = "m", distout = "m", diagval = "0", beta = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(d)) 
        d <- distance(spatobj = spatobj, folder = folder, shape = shape, distin = distin, distout = distout, diagval = diagval)
    if (fdist == "e") 
        d <- exp(-beta * d)
    cldata <- segdataclean(x, d = d)
    x <- cldata$x
    d <- cldata$d
    varTotal <- colSums(x)
    Pxx1 <- vector(length = ncol(x))
    for (k in 1:ncol(x)) Pxx1[k] <- sum((d %*% x[, k]) * x[, k]/(varTotal[k])^2)
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
        if (fdist == "e") 
            result[k1, k2] <- Pxx1[k1]/Pxx1[k2] - 1
        if (fdist == "l") 
            result[k1, k2] <- 1 - Pxx1[k1]/Pxx1[k2]
    }
    return(round(result, 4))
}

######################## 

# CENTRALISATION INDEXES

######################## 

#' A function to compute Duncan's Absolute Centralisation Index (ACEDuncan)
#'
#' @usage ACEDuncan(x, dc = NULL, center = 1, 
#' spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param dc - a numeric vector containing the distances between spatial units
#' centroids and the central spatial unit
#' @param center - a numeric value giving the number of the spatial unit that 
#' represents the center in the table
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing Duncan's asolute centralisation index 
#' value for each group
#' @references Duncan O. D. and Duncan B. (1955) \emph{A 
#' Methodological Analysis of Segregation Indexes}. 
#' American Sociological Review 41, pp. 210-217
#' @description Duncan's absolute centralization index measures the 
#' proportion of a group that should change its localization to 
#' achieve the same level of centralization as the rest of the population.
#' The function can be used in two ways: to provide a vector containing 
#' the distances between spatial/organizational unit centroids or a external geographic 
#' information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' distc<- distcenter(segdata, center = 28)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ACEDuncan(x, dc=distc) 
#' 
#' ACEDuncan(x, spatobj = segdata, center = 28) 
#' 
#' ACEDuncan(x, folder = foldername, shape = shapename, center = 28) 
#'
#' @seealso \code{\link{ACEDuncanPoly}}, \code{\link{ACEDuncanPolyK}},
#' @seealso \code{\link{RCE}}, \code{\link{RCEPoly}}, \code{\link{RCEPolyK}}
#' @seealso \code{\link{ACE}}, \code{\link{ACEPoly}}
#' @export


ACEDuncan <- function(x, dc = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(dc)) 
        dc <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center)
    cldata <- segdataclean(x, dc = dc)
    x <- cldata$x
    dc <- cldata$dc
    result <- vector(length = ncol(x))
    varTotal <- colSums(x)
    prop <- varTotal/sum(varTotal)
    for (k in 1:ncol(x)) {
        provi <- RCE(cbind(x[, k], rowSums(x)), dc = dc, center = center, spatobj = spatobj, folder = folder, shape = shape)/(1 - prop[k])
        result[k] <- provi[1, 2]
    }
    return(round(result, 4))
}


#' A function to compute Duncan's Polycentric Absolute Centralisation Index
#'
#' @usage ACEDuncanPoly(x, dc = NULL, center = 1, 
#' spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param dc - a numeric matrix/vector containing the distances between spatial units
#' centroids and the central spatial unit(s). 
#' @param center - a numeric vector giving the number of the spatial/organizational units that 
#' represents the centers in the table
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing Duncan's asolute centralisation index 
#' value for each group
#' @references Duncan O. D. and Duncan B. (1955) \emph{A 
#' Methodological Analysis of Segregation Indexes}. 
#' American Sociological Review 41, pp. 210-217
#' @description Polycentric version of Duncan's absolute centralization index. 
#' The function can be used in two ways: to provide a vector containing 
#' the distances between spatial/organizational unit centroids or a 
#' external geographic information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ACEDuncanPoly(x, spatobj = segdata, center = c(28, 83) )
#' 
#' ACEDuncanPoly(x, folder = foldername, shape = shapename, center = c(28, 83))
#' 
#' center <- c(28, 83)
#' polydist <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
#' for (i in 1:ncol(polydist))
#'   polydist[,i] <- distcenter(spatobj = segdata, center = center[i])
#' ACEDuncanPoly(x, dc = polydist)
#' 
#' distmin <- vector(length = nrow(x))
#' for (i in 1:nrow(polydist)) distmin[i] <- min(polydist[i,])
#' ACEDuncan(x, dc = distmin)
#'
#' @seealso \code{\link{ACEDuncan}}, \code{\link{ACEDuncanPolyK}},
#' @seealso \code{\link{RCE}}, \code{\link{RCEPoly}}, \code{\link{RCEPolyK}}
#' @seealso \code{\link{ACE}}, \code{\link{ACEPoly}}
#' @export



ACEDuncanPoly <- function(x, dc = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(dc)) {
        dc <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
        for (i in 1:ncol(dc)) dc[, i] <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center = center[i])
    }
    distmin <- vector(length = nrow(x))
    for (i in 1:nrow(dc)) distmin[i] <- min(dc[i, ])
    dc <- distmin
    cldata <- segdataclean(x, dc = dc)
    x <- cldata$x
    dc <- cldata$dc
    result <- vector(length = ncol(x))
    for (k in 1:ncol(x))
        result[k] <- RCE(as.matrix(cbind(x[, k], rowSums(x) - x[, k])), dc = dc, center = center, spatobj = spatobj, folder = folder, shape = shape)[1, 2]
    return(round(result, 4))
}


#' A function to compute Duncan's Constrained Absolute Centralisation Index 
#'
#' @usage ACEDuncanPolyK(x, dc = NULL,  K = NULL, kdist = NULL, center = 1,
#'                 spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param dc - a numeric matrix/vector containing the distances between spatial units
#' centroids and the central spatial unit(s). 
#' @param center - a numeric vector  giving the number of the spatial units that 
#' represent the centers in the table
#' @param K - the number of neighbourhoods under the influence of a center
#' @param kdist - the maximal distance that defines the neighbourhoods influenced
#' by a center
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix containing relative centralisation index values 
#' @references Duncan O. D. and Duncan B. (1955) \emph{A 
#' Methodological Analysis of Segregation Indexes}. 
#' American Sociological Review 41, pp. 210-217
#' @references Folch D.C and Rey S. J (2016) \emph{The centralization index: 
#' A measure of local spatial segregation}. Papers in Regional 
#' Science 95 (3), pp. 555-576
#' @description Constrained (local) version of Duncan's centralization index.
#' The function can be used in two ways: to provide a matrix containing 
#' the distances between spatial/organizational unit centroids or a external geographic 
#' information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ACEDuncanPolyK(x, spatobj = segdata, center = c(28, 83))
#' 
#' ACEDuncanPolyK(x, folder = foldername, shape = shapename, center = c(28, 83), K = 3)
#' 
#' center <- c(28, 83)
#' polydist <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
#' for (i in 1:ncol(polydist))
#'   polydist[,i] <- distcenter(spatobj = segdata, center = center[i])
#' ACEDuncanPolyK(x, dc = polydist, kdist = 2)
#' 
#' @seealso \code{\link{ACEDuncan}}, \code{\link{ACEDuncanPoly}},
#' @seealso \code{\link{RCE}}, \code{\link{RCEPoly}}, \code{\link{RCEPolyK}}
#' @seealso \code{\link{ACE}}, \code{\link{ACEPoly}}
#' @export



ACEDuncanPolyK <- function(x, dc = NULL, K = NULL, kdist = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(K) & is.null(kdist)) 
        K <- round(sqrt(nrow(x)^2 + ncol(x)^2))
    if (is.null(dc)) {
        dc <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
        for (i in 1:ncol(dc)) dc[, i] <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center = center[i])
    }
    cldata <- segdataclean(x, dc = dc)
    x <- cldata$x
    dc <- cldata$dc
    result <- vector(length = ncol(x))
    for (i in 1:ncol(x)) {
        x1 <- x[, i]
        x2 <- rowSums(x) - x1
        result[i] <- RCEPolyK(cbind(x1, x2), dc = dc, K = K, kdist = kdist, center = center, spatobj = spatobj, folder = folder, shape = shape)[1, 2]
    }
    return(round(result, 4))
}





#' A function to compute Relatice Centralisation Index (RCE)
#'
#' @usage RCE(x, dc = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param dc - a numeric vector containing the distances between spatial units
#' centroids and the central spatial unit
#' @param center - a numeric value giving the number of the spatial unit that 
#' represents the center in the table
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix containing relative centralisation index values 
#' @references Duncan O. D. and Duncan B. (1955) \emph{A 
#' Methodological Analysis of Segregation Indexes}. 
#' American Sociological Review 41, pp. 210-217
#' @description The relative centralisation index measures the 
#' proportion of a group that should change its localization to 
#' achieve the same level of centralization as another group.
#' The function can be used in two ways: to provide a vector containing 
#' the distances between spatial unit centroids or a external geographic 
#' information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' distc<- distcenter(segdata, center = 28)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' RCE(x, dc=distc) 
#' 
#' RCE(x, spatobj = segdata, center = 28) 
#' 
#' RCE(x, folder = foldername, shape = shapename, center = 28) 
#'
#' @seealso \code{\link{RCEPoly}}, \code{\link{RCEPolyK}},
#' @seealso \code{\link{ACEDuncan}}, \code{\link{ACEDuncanPoly}}, 
#' @seealso \code{\link{ACEDuncanPolyK}}, \code{\link{ACE}}, \code{\link{ACEPoly}}
#' @export


RCE <- function(x, dc = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(dc)) 
        dc <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center)
    cldata <- segdataclean(x, dc = dc)
    x <- cldata$x
    dc <- cldata$dc
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    varTotal <- colSums(x)
    xprovi <- cbind(x, dc)
    xprovi <- as.data.frame(xprovi[order(xprovi[, ncol(xprovi)]), ])
    xprovi2 <- xprovi[1:length(unique(xprovi$dc)), ]
    xprovi2$dc <- unique(xprovi$dc)
    for (i in 1:ncol(x)) xprovi2[, i] <- tapply(xprovi[, i], xprovi$dc, sum)
    xprovi <- xprovi2
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
        XI1 <- cumsum(xprovi[, k1])[1:(nrow(xprovi) - 1)]/varTotal[k1]
        XI <- cumsum(xprovi[, k1])[2:nrow(xprovi)]/varTotal[k1]
        YI1 <- cumsum(xprovi[, k2])[1:(nrow(xprovi) - 1)]/varTotal[k2]
        YI <- cumsum(xprovi[, k2])[2:nrow(xprovi)]/varTotal[k2]
        result[k1, k2] <- XI1 %*% YI - XI %*% YI1
    }
    return(round(result, 4))
}



#' A function to compute Polycentric Relative Centralisation Index
#'
#' @usage RCEPoly(x, dc = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param dc - a numeric matrix/vector containing the distances between spatial units
#' centroids and the central spatial unit(s). 
#' @param center - a numeric vector  giving the number of the spatial units that 
#' represent the centers in the table
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix containing relative centralisation index values 
#' @references Duncan O. D. and Duncan B. (1955) \emph{A 
#' Methodological Analysis of Segregation Indexes}. 
#' American Sociological Review 41, pp. 210-217
#' @description The polycentric version of the relative centralisation index. 
#' The function can be used in two ways: to provide a matrix containing 
#' the distances between spatial/organizational unit centroids or a external  
#' geographic information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' RCEPoly(x, spatobj = segdata, center = c(28, 83) )
#' 
#' RCEPoly(x, folder = foldername, shape = shapename, center = c(28, 83))
#' 
#' center <- c(28, 83)
#' polydist <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
#' for (i in 1:ncol(polydist))
#'   polydist[,i] <- distcenter(spatobj = segdata, center = center[i])
#' RCEPoly(x, dc = polydist)
#' 
#' distmin <- vector(length = nrow(x))
#' for (i in 1:nrow(polydist)) distmin[i] <- min(polydist[i,])
#' RCE(x, dc = distmin)
#'
#' @seealso \code{\link{RCE}}, \code{\link{RCEPolyK}},
#' @seealso \code{\link{ACEDuncan}}, \code{\link{ACEDuncanPoly}}, 
#' @seealso \code{\link{ACEDuncanPolyK}}, \code{\link{ACE}}, \code{\link{ACEPoly}}
#' @export


RCEPoly <- function(x, dc = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(dc)) {
        dc <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
        for (i in 1:ncol(dc)) dc[, i] <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center = center[i])
    }
    distmin <- vector(length = nrow(x))
    for (i in 1:nrow(dc)) distmin[i] <- min(dc[i, ])
    dc <- distmin
    cldata <- segdataclean(x, dc = dc)
    x <- cldata$x
    dc <- cldata$dc
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    varTotal <- colSums(x)
    xprovi <- cbind(x, dc)
    xprovi <- xprovi[order(xprovi[, ncol(xprovi)]), ]
    xprovi <- as.data.frame(xprovi)
    xprovi2 <- xprovi[1:length(unique(xprovi$dc)), ]
    xprovi2$dc <- unique(xprovi$dc)
    for (i in 1:ncol(x)) xprovi2[, i] <- tapply(xprovi[, i], xprovi$dc, sum)
    xprovi <- xprovi2
    for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
        XI1 <- cumsum(xprovi[, k1])[1:(nrow(xprovi) - 1)]/varTotal[k1]
        XI <- cumsum(xprovi[, k1])[2:nrow(xprovi)]/varTotal[k1]
        YI1 <- cumsum(xprovi[, k2])[1:(nrow(xprovi) - 1)]/varTotal[k2]
        YI <- cumsum(xprovi[, k2])[2:nrow(xprovi)]/varTotal[k2]
        result[k1, k2] <- XI1 %*% YI - XI %*% YI1
    }
    return(round(result, 4))
}




#' A function to compute Constrained Relative Centralisation Index
#'
#' @usage RCEPolyK(x, dc = NULL,  K = NULL, kdist = NULL, center = 1,
#'                 spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param dc - a numeric matrix/vector containing the distances between spatial units
#' centroids and the central spatial unit(s). 
#' @param center - a numeric vector  giving the number of the spatial units that 
#' represent the centers in the table
#' @param K - the number of neighbourhoods under the influence of a center
#' @param kdist - the maximal distance that defines the neighbourhoods influenced
#' by a center
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a matrix containing relative centralisation index values 
#' @references Duncan O. D. and Duncan B. (1955) \emph{A 
#' Methodological Analysis of Segregation Indexes}. 
#' American Sociological Review 41, pp. 210-217
#' @references Folch D.C and Rey S. J (2016) \emph{The centralization index: 
#' A measure of local spatial segregation}. Papers in Regional 
#' Science 95 (3), pp. 555-576
#' @description The constrained (local) version of relative centralization index.
#' The function can be used in two ways: to provide a matrix containing 
#' the distances between spatial unit centroids or a external geographic 
#' information source (spatial object or shape file).
#' @examples x <- segdata@data[ ,1:2]
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' RCEPolyK(x, spatobj = segdata, center = c(28, 83))
#' 
#' RCEPolyK(x, folder = foldername, shape = shapename, center = c(28, 83), K = 3)
#' 
#' center <- c(28, 83)
#' polydist <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
#' for (i in 1:ncol(polydist))
#'   polydist[,i] <- distcenter(spatobj = segdata, center = center[i])
#' RCEPolyK(x, dc = polydist, kdist = 2)
#' 
#' @seealso \code{\link{RCE}}, \code{\link{RCEPoly}},
#' @seealso \code{\link{ACEDuncan}}, \code{\link{ACEDuncanPoly}}, 
#' @seealso \code{\link{ACEDuncanPolyK}}, \code{\link{ACE}}, \code{\link{ACEPoly}}
#' @export
#' @export



RCEPolyK <- function(x, dc = NULL, K = NULL, kdist = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(K) & is.null(kdist)) 
        K <- round(sqrt(nrow(x)^2 + ncol(x)^2))
    if (is.null(dc)) {
        dc <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
        for (i in 1:ncol(dc)) dc[, i] <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center = center[i])
    }
    distmin <- vector(length = nrow(dc))
    for (i in 1:nrow(dc)) distmin[i] <- min(dc[i, 1:ncol(dc)])
    cldata <- segdataclean(x, dc = distmin)
    x <- cldata$x
    dc <- cldata$dc
    distmin <- dc
    xprovi <- cbind(x, distmin)
    xprovi <- as.data.frame(xprovi[order(xprovi[, ncol(xprovi)]), ])
    xprovi2 <- xprovi[1:length(unique(xprovi$distmin)), ]
    xprovi2$distmin <- unique(xprovi$distmin)
    for (i in 1:ncol(x)) xprovi2[, i] <- tapply(xprovi[, i], xprovi$distmin, sum)
    xprovi <- xprovi2
    if (!is.null(K)) {
        if (K >= nrow(xprovi2)) 
            K <- nrow(xprovi2) - 1
        xprovi <- xprovi[1:(K + 1), ]
    }
    if (!is.null(kdist)) 
        xprovi <- xprovi[xprovi$distmin <= kdist, ]
    result <- matrix(data = 0, nrow = ncol(x), ncol = ncol(x))
    xprovi <- xprovi[, -ncol(xprovi)]
    varTotal <- colSums(xprovi)
    test <- 0
    if (sum(varTotal) == 0) 
        test <- 1
    for (i in length(varTotal)) if (varTotal[i] == 0 & sum(varTotal) > 0) {
        test <- 1
        for (j in 1:length(varTotal)) {
            result[j, i] <- 1
            result[i, j] <- -1
        }
    }
    if (nrow(xprovi) <= 1) 
        test <- 1
    if (test == 0) 
        for (k1 in 1:ncol(x)) for (k2 in 1:ncol(x)) {
            XI1 <- cumsum(xprovi[, k1])[1:(nrow(xprovi) - 1)]/varTotal[k1]
            XI <- cumsum(xprovi[, k1])[2:nrow(xprovi)]/varTotal[k1]
            YI1 <- cumsum(xprovi[, k2])[1:(nrow(xprovi) - 1)]/varTotal[k2]
            YI <- cumsum(xprovi[, k2])[2:nrow(xprovi)]/varTotal[k2]
            result[k1, k2] <- XI1 %*% YI - XI %*% YI1
        }
    return(round(result, 4))
}


#' A function to compute Massey Absolute Centralisation Index (ACE)
#'
#' @usage ACE(x, a = NULL, dc = NULL, center = 1, 
#' spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param a - a numeric vector containing spatial unit areas
#' @param dc - a numeric vector containing the distances between spatial units
#' centroids and the central spatial unit
#' @param center - a numeric value giving the number of the spatial unit that 
#' represents the center in the table
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing asolute centralisation index value for 
#' each group
#' @references Massey D. S. and Denton N. A. (1988) \emph{
#' The dimensions of residential segregation}. 
#' Social Forces 67(2),  pp. 281-315.
#' @description The absolute centralization index measures a group
#' spatial distribution compared to the distribution of land area 
#' around the city center. The function can be used in two ways: to provide 
#' an area vector and a vector containing the distances between spatial units
#' centroids and  the central spatial unit or a external geographic information 
#' source (spatial object or shape file).
#' 
#' @examples x <- segdata@data[ ,1:2]
#' ar<-area(segdata)
#' distc<- distcenter(segdata, center = 28)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ACE(x, a = ar, dc=distc) 
#' 
#' ACE(x, spatobj = segdata, center = 28) 
#' 
#' ACE(x, folder = foldername, shape = shapename, center = 28) 
#' 
#' @seealso \code{\link{ACEPoly}},
#' @seealso \code{\link{RCE}}, \code{\link{RCEPoly}}, \code{\link{RCEPolyK}},
#' @seealso \code{\link{ACEDuncan}}, \code{\link{ACEDuncanPoly}}, 
#' @seealso \code{\link{ACEDuncanPolyK}} 
#' @export


ACE <- function(x, a = NULL, dc = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(a)) 
        a <- area(spatobj = spatobj, folder = folder, shape = shape)
    if (is.null(dc)) 
        dc <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center)
    cldata <- segdataclean(x, a = a, dc = dc)
    x <- cldata$x
    a <- cldata$a
    dc <- cldata$dc
    
    result <- vector(length = ncol(x))
    varTotal <- colSums(x)
    t <- rowSums(x)
    prop <- varTotal/sum(varTotal)
    xprovi <- cbind(x, a, dc)
    xprovi <- xprovi[order(xprovi[, ncol(xprovi)]), ]
    xprovi <- as.data.frame(xprovi)
    
    for (k in 1:ncol(x)) {
        XI1 <- cumsum(xprovi[, k])[1:(nrow(xprovi) - 1)]/varTotal[k]
        AI <- cumsum(xprovi$a)[2:nrow(xprovi)]/sum(xprovi$a)
        XI <- cumsum(xprovi[, k])[2:nrow(xprovi)]/varTotal[k]
        AI1 <- cumsum(xprovi$a)[1:(nrow(xprovi) - 1)]/sum(xprovi$a)
        result[k] <- XI1 %*% AI - XI %*% AI1
    }
    return(round(result, 4))
}




#' A function to compute Massey's Polycentric Absolute Centralisation Index
#'
#' @usage ACEPoly(x, a = NULL, dc = NULL, center = 1, 
#' spatobj = NULL, folder = NULL, shape = NULL)
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param a - a numeric vector containing spatial unit areas
#' @param dc - a numeric matrix containing the distances between spatial units
#' centroids and the central spatial units
#' @param center - a numeric vector giving the number of the spatial units that 
#' represent the centers in the table
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive

#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return a numeric vector containing asolute centralisation index value for 
#' each group
#' @references Massey D. S. and Denton N. A. (1988) \emph{
#' The dimensions of residential segregation}. 
#' Social Forces 67(2),  pp. 281-315.
#' @description The absolute centralization index measures a group
#' spatial distribution compared to the distribution of land area 
#' around the city center. The function can be used in two ways: to provide 
#' an area vector and a vector containing the distances between spatial units
#' centroids and  the central spatial unit or a external geographic information 
#' source (spatial object or shape file).
#' 
#' @examples x <- segdata@data[ ,1:2]
#' ar<-area(segdata)
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' ACEPoly(x, spatobj = segdata, center = c(28, 83) )
#' 
#' ACEPoly(x, folder = foldername, shape = shapename, center = c(28, 83))
#' 
#' center <- c(28, 83)
#' polydist <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
#' for (i in 1:ncol(polydist))
#'   polydist[,i] <- distcenter(spatobj = segdata, center = center[i])
#' ACEPoly(x, a = ar, dc = polydist)
#' 
#' distmin <- vector(length = nrow(x))
#' for (i in 1:nrow(polydist)) distmin[i] <- min(polydist[i,])
#' ACE(x, a = ar, dc = distmin)
#' 
#' @seealso \code{\link{ACE}}, \code{\link{RCE}}, 
#' @seealso \code{\link{RCEPoly}}, \code{\link{RCEPolyK}},
#' @seealso \code{\link{ACEDuncan}}, \code{\link{ACEDuncanPoly}}, 
#' @seealso \code{\link{ACEDuncanPolyK}} 
#' @export


ACEPoly <- function(x, a = NULL, dc = NULL, center = 1, spatobj = NULL, folder = NULL, shape = NULL) {
    x <- as.matrix(x)
    if (is.null(a)) 
        a <- area(spatobj = spatobj, folder = folder, shape = shape)
    if (is.null(dc)) {
        dc <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
        for (i in 1:ncol(dc)) dc[, i] <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center = center[i])
    }
    distmin <- vector(length = nrow(x))
    for (i in 1:nrow(dc)) distmin[i] <- min(dc[i, ])
    dc <- distmin
    cldata <- segdataclean(x, dc = dc, a = a)
    x <- cldata$x
    a <- cldata$a
    dc <- cldata$dc
    result <- vector(length = ncol(x))
    varTotal <- colSums(x)
    t <- rowSums(x)
    prop <- varTotal/sum(varTotal)
    xprovi <- cbind(x, a, dc)
    xprovi <- xprovi[order(xprovi[, ncol(xprovi)]), ]
    xprovi <- as.data.frame(xprovi)
    for (k in 1:ncol(x)) {
        XI1 <- cumsum(xprovi[, k])[1:(nrow(xprovi) - 1)]/varTotal[k]
        AI <- cumsum(xprovi$a)[2:nrow(xprovi)]/sum(xprovi$a)
        XI <- cumsum(xprovi[, k])[2:nrow(xprovi)]/varTotal[k]
        AI1 <- cumsum(xprovi$a)[1:(nrow(xprovi) - 1)]/sum(xprovi$a)
        result[k] <- XI1 %*% AI - XI %*% AI1
    }
    return(round(result, 4))
}




######################## 

# MULTI-GROUP INDEXES

######################## 


#' A function to compute Shannon-Wiener diversity (entropy) index
#'
#' @usage HShannon(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return Shannon-Wiener diversity index 
#' @references Shannon C. E. (1948) \emph{A mathematical theory 
#' of communication}. Bell System Technical Journal (27) 
#' @description The Shannon-Wiener diversity index is based on 
#' the notion of entropy and measures population heterogeneity.
#' @examples x <- segdata@data[ ,1:2]
#' HShannon(x) 
#' @seealso  Social diversity indices: 
#' \code{\link{NShannon}}, \code{\link{ISimpson}}, 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}}, \code{\link{DMulti}},  
#' \code{\link{HMulti}}, \code{\link{CMulti}}, \code{\link{RelDivers}}
#' @export


HShannon <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    pTotal <- colSums(x)/sum(x)
    result <- -sum(pTotal * log(pTotal))
    return(round(result, 4))
}


#' A function to compute Shannon-Wiener diversity normalized index
#'
#' @usage NShannon(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return Shannon-Wiener normalized diversity index 
#' @references Shannon C. E. (1948) \emph{A mathematical theory 
#' of communication}. Bell System Technical Journal (27) 
#' @description The Shannon-Wiener diversity index is based on 
#' the notion of entropy and measures population heterogeneity.
#' @examples x <- segdata@data[ ,1:2]
#' NShannon(x) 
#' @seealso  Other multi-group eveness indices: 
#' \code{\link{HShannon}}, \code{\link{ISimpson}}, 
#' \code{\link{GiniMulti}}, \code{\link{DMulti}}, \code{\link{HMulti}}, 
#' \code{\link{CMulti}}
#' @seealso Other multi-group indices: \code{\link{PMulti}}, 
#' \code{\link{RelDivers}}
#' @export


NShannon <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    pTotal <- colSums(x)/sum(x)
    result <- -sum(pTotal * log(pTotal))
    result <- result/log(ncol(x))
    return(round(result, 4))
}

#' A function to compute Simpson's interaction index
#'
#' @usage ISimpson(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return Simpson's interaction index 
#' @references Simpson E. H. (1949) \emph{Measurement of diversity}. 
#' Nature 163:688 
#' @description Simpson's interaction index measures the probability 
#' that randomly selected individuals are not in the same group. 
#' @examples x <- segdata@data[ ,1:2]
#' ISimpson(x) 
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}}, \code{\link{DMulti}},  
#' \code{\link{HMulti}}, \code{\link{CMulti}}, \code{\link{RelDivers}}
#' @export



ISimpson <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    pTotal <- colSums(x)/sum(x)
    result <- sum(pTotal * (1 - pTotal))
    return(round(result, 4))
}



#' A function to compute multi-group Gini index
#'
#' @usage GiniMulti(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @references Reardon S. F. (1998) \emph{Measures of racial 
#' diversity and segregation in multi-group and hierarchical 
#' structured Populations}. Annual meeting of the Eastern 
#' Sociological Society, Philadelphia 
#' @description Multi-group Gini is a multi-group version of 
#' the \code{\link{Gini}} index 
#' @examples x <- segdata@data[ ,1:2]
#' GiniMulti(x) 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}},   
#' \code{\link{HMulti}}, \code{\link{CMulti}}, \code{\link{RelDivers}}
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' \code{\link{ISimpson}}, 
#' @export



GiniMulti <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    pTotal <- colSums(x)/sum(x)
    II <- sum(pTotal * (1 - pTotal))
    vartemp <- vector(length = ncol(x))
    t <- rowSums(x)
    Total <- sum(x)
    p <- x/t
    pij <- array(0, dim = c(ncol(x), nrow(x), nrow(x)))
    for (k in 1:ncol(x)) {
        for (i in 1:nrow(x)) pij[k, , i] <- p[i, k]
        for (i in 1:nrow(x)) pij[k, i, ] <- abs(pij[k, i, ] - p[i, k])
        vartemp[k] <- sum(t * (t %*% pij[k, , ]))
    }
    result <- sum(vartemp)/(2 * Total * Total * II)
    return(round(result, 4))
}





#' A function to compute multi-group dissimilarity index
#'
#' @usage DMulti(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return multi-group dissimilarity index 
#' @references Sakoda J. N. (1981) \emph{A generalized Index of 
#' dissimilarity}. Demography,18, 245-250 
#' @description Multi-group dissimilarity index, is a multi-group 
#' version of  Duncan's dissimilarity index (\code{\link{DIDuncan}})
#' @examples x <- segdata@data[ ,1:2]
#' DMulti(x) 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}},   
#' \code{\link{HMulti}}, \code{\link{CMulti}}, \code{\link{RelDivers}}
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' \code{\link{ISimpson}}, 
#' @export 



DMulti <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    result <- 0
    pTotal <- colSums(x)/sum(x)
    II <- sum(pTotal * (1 - pTotal))
    t <- rowSums(x)
    Total <- sum(x)
    p <- x/t
    for (k in 1:ncol(x)) result <- result + t %*% abs(p[, k] - pTotal[k])
    result <- result/(2 * Total * II)
    return(round(result, 4))
}




#' A function to compute multi-group normalised exposure (PMulti)
#'
#' @usage PMulti(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return multi-group normalised isolation index 
#' @references  James, F. J. (1986) \emph{A New Generalized 'Exposure-Based' 
#' Segregation Index}. Sociological Methods and Research, 14, pp. 301-316
#' @references  Reardon S. F. and G. Firebaugh (2002) \emph{Measures of 
#' multi-group Segregation}. Sociological Methodology, 32(1), pp 33-67
#' @description The multi-group normalised isolation index is a 
#' multi-group version of the isolation index (\code{\link{xPx}})
#' @examples x <- segdata@data[ ,1:2]
#' PMulti(x) 
#' @seealso Multi-group indices: 
#' \code{\link{GiniMulti}}, \code{\link{DMulti}},  
#' \code{\link{HMulti}}, \code{\link{CMulti}}, \code{\link{RelDivers}}
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' \code{\link{ISimpson}}, 
#' @export 



PMulti <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    result <- 0
    pTotal <- colSums(x)/sum(x)
    t <- rowSums(x)
    Total <- sum(x)
    p <- x/t
    for (k in 1:ncol(x)) result <- result + t %*% ((p[, k] - pTotal[k]) * (p[, k] - pTotal[k])/(1 - pTotal[k]))
    result <- result/Total
    return(round(result, 4))
}


#' A function to compute multi-group relative diversity index
#'
#' @usage RelDivers(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return multi-group relative diversity index 
#' @references Carlson S. M. (1992) \emph{Trends in race/sex 
#' occupational inequality:  conceptual and measurement issues}. 
#' Social Problems, 39, p. 269-290
#' @description The relative diversity index is a multi-group 
#' index based on Simpson's interaction index \code{\link{ISimpson}} 
#' @examples x <- segdata@data[ ,1:2]
#' RelDivers(x) 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}}, \code{\link{DMulti}},  
#' \code{\link{HMulti}}, \code{\link{CMulti}}
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' \code{\link{ISimpson}}, 
#' @export 



RelDivers <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    result <- 0
    pTotal <- colSums(x)/sum(x)
    II <- sum(pTotal * (1 - pTotal))
    t <- rowSums(x)
    Total <- sum(x)
    p <- x/t
    for (k in 1:ncol(x)) result <- result + t %*% ((p[, k] - pTotal[k])^2)
    result <- result/(Total * II)
    return(round(result, 4))
}

#' A function to compute multi-group entropy segregation index
#'
#' @usage HMulti(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group. 
#' @return multi-group entropy segregation index
#' @references Theil H. (1972)  \emph{Statistical decomposition analysis: with 
#' applications in the social and administrative.} Amsterdam, North-Holland, 337 p.
#' @description The multi-group version of Theil's entropy index \code{\link{HTheil}} 
#' @examples x <- segdata@data[ ,1:2]
#' HMulti(x) 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}}, \code{\link{DMulti}},  
#' \code{\link{CMulti}}, \code{\link{RelDivers}}
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' \code{\link{ISimpson}}, 
#' @export 



HMulti <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    E <- vector("list", ncol(x))
    Total <- sum(x)
    pTotal <- colSums(x)/Total
    tx <- rowSums(x)
    px <- x/tx
    Etot <- sum(pTotal * log(1/pTotal))
    result <- 0
    for (k in 1:ncol(x)) {
        E[[k]] <- tx * px[, k] * log(px[, k]/pTotal[k])
        E[[k]][is.na(E[[k]])] <- 0
        result <- result + sum(E[[k]])
    }
    result <- result/(Etot * Total)
    return(round(result, 4))
}


#' A function to compute multi-group squared coefficient of variation index
#'
#' @usage CMulti(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return multi-group entropy segregation index
#' @references Reardon S. F. and Firebaugh G. (2002) \emph{Measures of multi-group 
#' segregation}. Sociological Methodology, 32, pp. 33-67.
#' @description The index can be interpreted as a measure of the variance of the 
#' spatial representation of the groups accros spatial unite, or as a normalized 
#' chi-squared measure of association between groups and units. 
#' @examples x <- segdata@data[ ,1:2]
#' CMulti(x) 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}}, \code{\link{DMulti}},  
#' \code{\link{HMulti}}, \code{\link{RelDivers}}
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' \code{\link{ISimpson}}, 
#' @export 



CMulti <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    Total <- sum(x)
    pTotal <- colSums(x)/Total
    tx <- rowSums(x)
    px <- x/tx
    pTotal2 <- matrix(rep(pTotal, nrow(x)), nrow = nrow(x), ncol = ncol(x), byrow = T)
    result <- sum((tx/Total) * ((px - pTotal2)^2)/((ncol(x) - 1) * pTotal2))
    return(round(result, 4))
}



#' A function to compute Reardon multi-group ordinal segregation indices
#'
#' @usage ordinalseg(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group. The rows
#' represent the nominal categories (spatial units) and the columns the ordinal 
#' categories.
#' @return A vector with Reardon multi-group ordinal segregation indices:
#' Lambda1 - ordinal generalization of the information theory index
#' Lambda2 - ordinal generalization of the variation ratio index
#' Lambda3 - ordinal square root index
#' Lambda4 - ordinal absolute difference index
#' @references Reardon S. F. (2009) \emph{Measures of ordinal segregation}. 
#' Research on Economic Inequality, 17, pp. 129-155.
#' @description  A function to compute Reardon (2009) ordinal indices
#' @examples x <- GreHSize@data[ ,3:5]
#' ordinalseg(x) 
#' 
#' x1 <- matrix(nrow = 4, ncol = 3)
#' x1[1,] <- c(0, 0, 30)
#' x1[2,] <- c(0, 20, 10)
#' x1[3,] <- c(10, 20 ,0)
#' x1[4,] <- c(30, 0 ,0)
#' 
#' x2 <- matrix(nrow = 4, ncol = 3)
#' x2[1,] <- c(0, 30, 0)
#' x2[2,] <- c(0, 10, 20)
#' x2[3,] <- c(10, 0, 20)
#' x2[4,] <- c(30, 0, 0)
#'
#' ordinalseg(x1)
#' ordinalseg(x2)
#' @seealso \code{\link{rankorderseg}}
#' @export 


ordinalseg <- function(x) {
    f1 <- function(xx) {
        result <- -(xx * log2(xx) + (1 - xx) * log2(1 - xx))
        result[is.na(result)] <- 0
        return(result)
    }
    
    f2 <- function(xx) {
        result <- 4 * xx * (1 - xx)
        return(result)
    }
    
    f3 <- function(xx) {
        result <- 2 * sqrt(xx * (1 - xx))
        return(result)
    }
    
    f4 <- function(xx) {
        result <- 1 - abs(2 * xx - 1)
        return(result)
    }
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    ti <- rowSums(x)
    tk <- colSums(x)
    T <- sum(x)
    cik <- x
    for (i in 1:nrow(x)) cik[i, ] <- cumsum(x[i, ])/ti[i]
    cik <- cik[, -ncol(cik)]
    ck <- cumsum(tk)/T
    ck <- ck[-length(ck)]
    
    v <- vector(length = 4)
    result <- v
    vi <- matrix(0, nrow = 4, ncol = nrow(x))
    
    v[1] <- sum(f1(ck))/(ncol(x) - 1)
    v[2] <- sum(f2(ck))/(ncol(x) - 1)
    v[3] <- sum(f3(ck))/(ncol(x) - 1)
    v[4] <- sum(f4(ck))/(ncol(x) - 1)
    
    if (ncol(x) > 2) {
        vi[1, ] <- rowSums(f1(cik))/(ncol(x) - 1)
        vi[2, ] <- rowSums(f2(cik))/(ncol(x) - 1)
        vi[3, ] <- rowSums(f3(cik))/(ncol(x) - 1)
        vi[4, ] <- rowSums(f4(cik))/(ncol(x) - 1)
    }
    if (ncol(x) == 2) {
        vi[1, ] <- f1(cik)/(ncol(x) - 1)
        vi[2, ] <- f2(cik)/(ncol(x) - 1)
        vi[3, ] <- f3(cik)/(ncol(x) - 1)
        vi[4, ] <- f4(cik)/(ncol(x) - 1)
    }
    
    for (vers in 1:4) for (i in 1:nrow(x)) result[vers] <- result[vers] + (v[vers] - vi[vers, i]) * ti[i]/(T * v[vers])
    return(result)
}





#' A function to compute rank-ordered segregation indices
#'
#' @usage rankorderseg(x, polorder = 4, pred = NULL) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group. The rows
#' represent the nominal categories (spatial units) and the columns the population
#' distribution as ordered groups divided by thresholds
#' @param polorder - order of the polynomial approximation (4 by default)
#' @param pred  - a numerical vector with percentiles to be predicted. 
#' If NULL, the predictions are made at threshold levels
#' @return A list containing the results for three rank-ordered indices: 
#' rank-order information theory index (Hr), rank-order variation ratio 
#' index (Rr) and rank-order square root index (Sr). For each index, a sublist 
#' is provided, containing: 
#' Index - the rank-ordered index value
#' Hp/Rp/Sp - a vector with ordinal information theory/variance ratio/square root 
#' segregation index values at thresholds
#' Coefficients - Coefficients extracted from the polynomial estimation model,
#' including basic statistics
#' Predict - a list contining predicted values of the coresponding ordinal index (fit); 
#' standard error of predicted means (se.fit); degrees of freedom for residual (df); 
#' and residual standard deviations (residuale.scale). If pred is NULL, the function 
#' will return the the statistics at thresholds
#' @references Reardon S. F. (2011) \emph{Measures of Income Segregation
#' }. The Stanford Center on Poverty and Inequality
#' @description  A function computing Reardon (2011) rank-ordered 
#' segregation indices
#' @examples x1 <- matrix(nrow = 4, ncol = 7)
#' x1[1,] <- c( 10,  10, 10, 20, 30, 40, 50)
#' x1[2,] <- c( 0, 20, 10, 10, 10, 20, 20)
#' x1[3,] <- c(10, 20,  10, 10, 10, 0, 0 )
#' x1[4,] <- c(30, 30,  20, 10, 10, 0, 0 )
#' x2 <- x1
#' x2[,c(3,4,6,7)] <- x1[,c(6,7,3,4)]
#' 
#' rankorderseg(x1)
#' rankorderseg(x2, pred = seq(0, 1, 0.1))
#' 
#' @seealso \code{\link{ordinalseg}} 
#' @export 



rankorderseg <- function(x, polorder = 4, pred = NULL) {
    x <- as.matrix(x)
    stats::glm.control(epsilon = 1e-04, maxit = 1e+06, trace = FALSE)
    x <- segdataclean(x)$x
    result1 <- vector("list", 4)
    result2 <- vector("list", 4)
    result3 <- vector("list", 4)
    names(result1) <- c("Index", "Hp", "Coefficients", "Predict")
    names(result2) <- c("Index", "Rp", "Coefficients", "Predict")
    names(result3) <- c("Index", "Sp", "Coefficients", "Predict")
    poptot <- sum(x)
    p <- matrix(nrow = nrow(x), ncol = 2)
    Hp <- vector(length = ncol(x) - 1)
    Rp <- vector(length = ncol(x) - 1)
    Sp <- vector(length = ncol(x) - 1)
    Pk <- vector(length = ncol(x) - 1)
    
    for (k in 1:(ncol(x) - 1)) {
        if (k == 1) 
            p[, 1] <- x[, 1] else p[, 1] <- rowSums(x[, 1:k])
        p[, 2] <- rowSums(x) - p[, 1]
        ordseg <- ordinalseg(p)
        Hp[k] <- ordseg[1]
        Rp[k] <- ordseg[2]
        Sp[k] <- ordseg[3]
        Pk[k] <- sum(p[, 1])/poptot
    }
    result1[[2]] <- Hp
    result2[[2]] <- Rp
    result3[[2]] <- Sp
    
    estim <- stats::lm(Hp ~ poly(Pk, polorder, raw = T))
    result1[[3]] <- stats::coefficients(summary(estim))
    estim <- stats::lm(Rp ~ poly(Pk, polorder, raw = T))
    result2[[3]] <- stats::coefficients(summary(estim))
    estim <- stats::lm(Sp ~ poly(Pk, polorder, raw = T))
    result3[[3]] <- stats::coefficients(summary(estim))
    
    if (is.null(pred)) 
        pred <- data.frame(Pk) else pred <- as.data.frame(pred)
    
    names(pred) <- "x"
    x1 <- x
    x <- Pk
    
    y <- Hp
    result1[[4]] <- stats::predict(stats::lm(y ~ poly(x, polorder, raw = T)), newdata = pred, se.fit = T)
    y <- Rp
    result2[[4]] <- stats::predict(stats::lm(y ~ poly(x, polorder, raw = T)), newdata = pred, se.fit = T)
    y <- Sp
    result3[[4]] <- stats::predict(stats::lm(y ~ poly(x, polorder, raw = T)), newdata = pred, se.fit = T)
    
    coef1 <- as.vector(result1[[3]][, 1])
    coef2 <- as.vector(result2[[3]][, 1])
    coef3 <- as.vector(result3[[3]][, 1])
    delta1 <- rep(0, length(coef1))
    delta2 <- rep(0, length(coef1))
    delta3 <- rep(1, length(coef1))
    for (m in 0:(length(coef1) - 1)) {
        for (n in 0:m) {
            delta1[m + 1] <- delta1[m + 1] + (choose(m, n) * (-1)^(m - n))/((m - n + 2)^2)
            delta3[m + 1] <- delta3[m + 1] * (2 * n + 1)/(2 * n + 4)
        }
        
        delta1[m + 1] <- 2 * delta1[m + 1] + (2/((m + 2)^2))
        delta2[m + 1] <- 6/((m + 2) * (m + 3))
        delta3[m + 1] <- delta3[m + 1] * 4
    }
    result1[[1]] <- sum(delta1 * coef1)
    result2[[1]] <- sum(delta2 * coef2)
    result3[[1]] <- sum(delta3 * coef3)
    result <- list(result1, result2, result3)
    names(result) <- c("Hr", "Rr", "Sr")
    return(result)
}









#' A function from seg package to compute spatial multi-group segregation indices
#'
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) .
#' @param ... - other parameters of spseg function from seg package.
#' @return A vector with Reardon's spatial multi-group segregation indices:
#' D* - spatial multi-group dissimilarity index
#' R* - spatial multi-group relative diversity index
#' H* - spatial multi-group information theory index 
#' @references Reardon, S. F. and O'Sullivan, D. (2004) 
#' \emph{Measures of spatial segregation}.
#' Sociological Methodology, 34, 121-162.
#' @references Hong S.Y., O'Sullivan D., Sadahiro Y. (2014) 
#' \emph{Implementing Spatial Segregation Measures in R'}.
#' PLoS ONE, 9(11)
#' @description  A function adapted from seg package (Hong et al. 2014) 
#' to compute spatial multi-group segregation indices developed by 
#' Reardon and O'Sullivan (2004)
#' @examples x <- segdata@data[ ,1:2]
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' 
#' spatmultiseg(x, spatobj = segdata)
#' 
#' spatmultiseg(x, folder = foldername, shape = shapename) 
#' 
#' @seealso Multi-group indices: 
#' \code{\link{PMulti}}, \code{\link{GiniMulti}}, \code{\link{DMulti}},  
#' \code{\link{HMulti}}, \code{\link{RelDivers}}
#' @seealso  Social diversity indices: 
#' \code{\link{HShannon}}, \code{\link{NShannon}}, 
#' \code{\link{ISimpson}}, 
#' @export 


spatmultiseg <- function(x, spatobj = NULL, folder = NULL, shape = NULL, ...) {
    x <- as.matrix(x)
    if (is.null(spatobj)) 
        spatobj <- sf::st_read(dsn = folder, layer = shape)
    if (class(spatobj)[1]!="SpatialPolygonsDataFrame") spatobj <- sf::as_Spatial(spatobj)
    cldata <- segdataclean(x, spatobj = spatobj)
    x <- cldata$x
    spatobj <- cldata$spatobj
    result <- vector(length = 3)
    provi <- seg::spseg(spatobj, x, ...)
    result[1] <- provi@d
    result[2] <- provi@r
    result[3] <- provi@h
    return(result)
}

######################## 

# LOCAL INDEXES

######################## 

#' A function to compute location quotients (LQs)
#'
#' @usage LQ(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return a matrix of location quotiens 
#' @references Isard W. (1960) \emph{Methods of regional analysis: 
#' an introduction to regional science}. The MIT Press, Cambridge
#' @description Location quotients compare the relative part of a 
#' group in a particular spatial unit, to the relative part of that 
#' same group in the area.
#' @examples x <- segdata@data[ ,1:2]
#' LQ(x) 
#' @seealso Other local indices \code{\link{LShannon}}
#' \code{\link{HLoc}}, \code{\link{LSimpson}}   
#' @export

LQ <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    result <- x
    Total <- sum(x)
    t <- rowSums(x)
    for (i in 1:ncol(x)) result[, i] <- (x[, i]/t)/(sum(x[, i])/sum(t))
    return(round(result, 4))
}




#' A function to compute local diversity index
#'
#' @usage HLoc(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return a numeric vector containing  diversity index value for 
#' each group
#' @references Theil H. (1972) \emph{Statistical Decomposition Analysis}. 
#' North-Holland, Amsterdam
#' @description Local diversity index, HLoc, is a local 
#' adaptation of Pielou's normalized diversity index \code{\link{NShannon}}.
#' @examples x <- segdata@data[ ,1:2]
#' HLoc(x) 
#' @seealso Other local indices \code{\link{LQ}}
#' \code{\link{LShannon}}, \code{\link{LSimpson}}   
#' @export


HLoc <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    Total <- sum(x)
    t <- rowSums(x)
    result <- x/t
    result <- cbind(result, 0)
    for (i in 1:nrow(result)) {
        n <- 0
        for (j in 1:(ncol(result) - 1)) if (result[i, j] > 0) {
            result[i, ncol(result)] <- result[i, ncol(result)] + result[i, j] * log(result[i, j])
            n <- n + 1
        }
        result[i, ncol(result)] <- -result[i, ncol(result)]/log(n)
    }
    result <- result[, ncol(result)]
    result[is.na(result)] <- 0
    return(round(result, 4))
}


#' A function to compute Shannon-Wiener local diversity (entropy) index
#'
#' @usage LShannon(x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return Local Shannon-Wiener diversity index 
#' @references Shannon C. E. (1948) \emph{A mathematical theory 
#' of communication}. Bell System Technical Journal (27) 
#' @description The Shannon-Wiener diversity index is based on 
#' the notion of entropy and measures population heterogeneity.
#' @examples x <- segdata@data[ ,1:2]
#' LShannon(x) 
#' @seealso Other local indices: \code{\link{LQ}}, 
#' \code{\link{HLoc}}, \code{\link{LSimpson}}   
#' @export


LShannon <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    Total <- sum(x)
    t <- rowSums(x)
    result <- x/t
    result <- cbind(result, 0)
    for (i in 1:nrow(result)) {
        n <- 0
        for (j in 1:(ncol(result) - 1)) if (result[i, j] > 0) {
            result[i, ncol(result)] <- result[i, ncol(result)] + result[i, j] * log(result[i, j])
            n <- n + 1
        }
        result[i, ncol(result)] <- -result[i, ncol(result)]
    }
    result <- result[, ncol(result)]
    result[is.na(result)] <- 0
    return(round(result, 4))
}


#' A function to compute local Simpson's index
#'
#' @usage LSimpson (x) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @return Local Simpson's interaction index 
#' @references Simpson E. H. (1949) \emph{Measurement of diversity}. 
#' Nature 163:688 
#' @description Local Simpson's interaction index measures the probability 
#' that randomly selected individuals are not in the same group in 
#' each spatial unit. 
#' @examples x <- segdata@data[ ,1:2]
#' LSimpson (x) 
#' @seealso Other local indices: \code{\link{LQ}}, 
#' \code{\link{HLoc}}, \code{\link{LShannon}}   
#' @export


LSimpson <- function(x) {
    x <- as.matrix(x)
    x <- segdataclean(x)$x
    p <- x/rowSums(x)
    result <- rowSums(p * (1 - p))
    result[is.na(result)] <- 0
    return(round(result, 4))
}


################################################## 

# DATA CLEANING

################################################## 

#' A function to clean and prepare the data for segregation analysis
#'
#' @usage segdataclean (x, c = NULL, b = NULL, a = NULL, p = NULL, 
#' ck = NULL, d = NULL, dc = NULL, spatobj = NULL, folder = NULL, shape = NULL, 
#' warnings = T) 
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param c - a standard binary contiguity (adjacency) symmetric matrix where 
#' each element \emph{Cij} equals 1 if \emph{i}-th and \emph{j}-th spatial 
#' units are adjacent, and 0 otherwise.
#' @param b - a common boundaries matrix where each element \emph{Bij} 
#' @param a - a numeric vector containing spatial unit areas
#' @param p - a numeric vector containing spatial units perimeters.
#' @param ck - a list containing contiguity matrices coresponding to each order 
#' (from 1 to K)
#' @param d - a matrix of the distances between spatial unit centroids
#' @param dc - a numeric vector containing the distances between spatial units
#' centroids and the central spatial unit
#' @param warnings - warning alert (by default TRUE)
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @return The objects (data matrix, geographical vectors/matrices, spatial objects)
#' cleaned from null rows or columns 
#' @description The function cleans and prepares the data for segregation analysis 
#' @examples x <- segdata@data[ ,1:2]
#' x[ ,3] <- rep (0 ,100)
#' x[1:3, ] <- rep (c(0, 0, 0), 3)
#' x1 <- x
#' spatobj <- segdata
#' cldata <- segdataclean(x1, segdata)
#' x1 <- cldata$x
#' spatobj <- cldata$spatobj
#' 
#' c <- contig (segdata)
#' c <- segdataclean(x, c = c)$c
#' 
#' @seealso Other local indices: \code{\link{LQ}}, 
#' \code{\link{HLoc}}, \code{\link{LShannon}}   
#' @export

segdataclean <- function(x, c = NULL, b = NULL, a = NULL, p = NULL, 
                         ck = NULL, d = NULL, dc = NULL, spatobj = NULL, 
                         folder = NULL, shape = NULL, warnings = T) {
    x <- as.matrix(x)
    spatobj2 <- NULL
    x <- x[, colSums(x) != 0]
    x2 <- cbind(x, 1:nrow(x))
    x1 <- x2[rowSums(x2[, 1:(ncol(x2) - 1)]) == 0, ]
    x2 <- x2[rowSums(x2[, 1:(ncol(x2) - 1)]) != 0, ]
    if (is.matrix(c)) 
        c <- c[x2[, ncol(x2)], x2[, ncol(x2)]]
    if (is.matrix(d)) 
        d <- d[x2[, ncol(x2)], x2[, ncol(x2)]]
    if (is.matrix(b)) 
        b <- b[x2[, ncol(x2)], x2[, ncol(x2)]]
    if (is.matrix(b)) 
        b <- b[x2[, ncol(x2)], x2[, ncol(x2)]]
    if (is.vector(p)) 
        p <- p[x2[, ncol(x2)]]
    if (is.vector(a)) 
        a <- a[x2[, ncol(x2)]]
    if (is.vector(dc)) 
        dc <- dc[x2[, ncol(x2)]]
    if (is.matrix(dc)) 
        dc <- dc[x2[, ncol(x2)], ]
    if (is.list(ck)) 
        for (k in 1:length(ck)) ck[[k]] <- ck[[k]][x2[, ncol(x2)], x2[, ncol(x2)]]
    
    if (is.null(spatobj) & !is.null(folder) & !is.null(shape)) 
        spatobj <- sf::st_read(dsn = folder, layer = shape)
    if (!is.null(spatobj)) {
        xx <- matrix(0, nrow = nrow(x), ncol = ncol(x))
        for (i in 1:ncol(x)) xx[, i] <- as.numeric(x[, i])
        spatobj@data <- as.data.frame(xx)
        for (i in 1:ncol(x)) spatobj@data[, i] <- as.numeric(spatobj@data[, i])
        spatobj2 <- subset(spatobj, rowSums(spatobj@data) != 0)
        spatobj2@data <- as.data.frame(x2[, -ncol(x2)])
    }
    x2 <- x2[, -ncol(x2)]
    if (nrow(x) != nrow(x2) & warnings) {
        if (is.vector(x1)) 
            warning("following line was deleted because of null population: ", paste(as.character(x1[length(x1)]), sep = " ", collapse = ", ")) else warning("following lines were deleted because of null population: ", paste(as.character(x1[, ncol(x1)]), sep = " ", collapse = ", "))
        
    }
    
    if (ncol(x) != ncol(x2) & warnings) {
        delcol <- NULL
        for (i in 1:ncol(x)) if (sum(x[, i]) == 0) 
            delcol <- c(delcol, i)
        warning("following variables were deleted because of null population: ", paste(as.character(delcol), sep = " ", collapse = ", "))
    }
    return(list(x = x2, c = c, b = b, a = a, p = p, ck = ck, d = d, dc = dc, spatobj = spatobj2))
}



################################################## 

# RESAMPLING

################################################## 




#' A function to test segregation indices by resampling 
#'
#' @param x - an object of class matrix (or which can be coerced to that class), 
#' where each column represents the distribution of a group within 
#' spatial units. The number of columns should be greater than 1 (at least 2 
#' groups are required). You should not include a column with total 
#' population, because this will be interpreted as a group.
#' @param fun - a character vector with the segregation function 
#' to be tested 
#' @param var - vector with the variables to be tested 
#' @param simtype - a character vector with the type of simulation. 
#' If simtype = 'Boot', the function generates bootstrap replications
#' If simtype = 'Jack', the function generates jackknife replications
#' If simtype = 'MonteCarlo', the function produces a randomization test 
#'  using Monte Carlo simulations
#' @param sampleunit = 'unit' (by default) when the sampling unit is the 
#' spatial/organisational unit and sampleunit = 'ind' for individual sampling
#' @param samplesize - the size of the sample used for bootstraping. If null, 
#' the samplesize equals the number of spatial/organizational units(sampleunit = 'unit') or 
#' the total total population (sampleunit = 'ind')
#' @param  perc - the percentiles for the bootstrap replications 
#' @param outl - logical parameter for jackknife simulations, if TRUE 
#' the function provides the outliers obtained by jackknife iterations 
#' @param outmeth - a character vector designing the outliers detection method:
#' outmeth = 'bp' (by default) for boxplot method 
#' outmeth = 'sd'  for standard deviation method 
#' outmeth = 'z'  for normal scores method
#' outmeth = 't'  for t Student scores method
#' outmeth = 'chisq'  for chi-squared scores method
#' outmeth = 'mad'  for median absolute deviation method
#' The estimations based on scoring methods are obtained using outliers package 
#' @param sdtimes - multiplication factor of the standard deviation used for
#' outliers detection with jackknife simulations (2 by default)
#' @param IQRrange - determines the boxplot thresholds (1.5 by default) as multiplication of 
#' IQR (Inter Quartile Range)
#' @param proba - for Monte Carlo simulations, proba is a vector with location 
#' probabilities. If proba = NULL, the vector is equiprobable. If outliers are determined 
#' with jackknife technique, proba indicates the probability (confidence interval) for 
#' scoring tests.
#' @param nsim - the number of simulations
#' @param setseed - if TRUE, specify zero seed for repetead simulation
#' @param a - a numeric vector containing spatial unit areas
#' @param c - a standard binary contiguity (adjacency) symmetric matrix where 
#' each element \emph{Cij} equals 1 if \emph{i}-th and \emph{j}-th spatial 
#' units are adjacent, and 0 otherwise.
#' @param ck - a list containing contiguity matrices coresponding to each order 
#' (from 1 to K)
#' @param K - the order of the contiguity matrix
#' @param queen - logical parameter defining criteria used for contiguity 
#' matrix computation, TRUE for queen, FALSE (by default) for rook 
#' @param b - a common boundaries matrix where each element \emph{Bij} 
#' @param p - a numeric vector containing spatial units perimeters.
#' @param ptype - a string variable giving two options for perimeter calculation
#' when a spatial object or shapefile is provided: 'int' to use only interior
#' boundaries of spatial units, and 'all' to use entire boundaries, 
#' including the boundaries to the exterior
#' @param d - a matrix of the distances between spatial unit centroids
#' @param distin - input metric conversion, based on  \pkg{bink} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{bink} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param dc - a numeric vector containing the distances between spatial units
#' centroids and the central spatial unit
#' @param center - a numeric value giving the number of the spatial unit that 
#' represents the center in the table
#' @param fdist - the method used for distance interaction matrix: 
#' e' for inverse exponential function (by default) and 'l' for linear.
#' @param f - the distance function, f = 'exp' (by default) for negative 
#' exponential function and f = 'rec' for reciprocal function
#' @param spatmat - the method used for spatial calculations: 'c' for the 
#' contiguity matrix (by default) or any other user spatial interaction matrix 
#' and 'd' for the inverse exponential function of the distance. 
#' @param diagval - when providing a spatial object or a shape file, 
#' the user has the choice of the spatial matrix diagonal definition: 
#' diagval = '0' (by default) for an null diagonal and diagval = 'a' 
#' to compute the diagonal as 0.6 * square root (spatial/organizational unitsarea) 
#' (White, 1983) 
#' @param itype - a character string defining the index type:
#' itype = 'multi' (by default) for the multi-group index (White, 1986)
#' or itype = 'between' for the between groups version (White, 1983)
#' @param variant - a character variable that allows to choose the index version: 
#' variant = 's' for the dissimilarity index adjusted for contiguous spatial units
#' boundary lengths and perimeter/area ratio (by default) and variant = 'w' 
#' for the version without perimeter/area ratio
#' @param delta - an inequality aversion parameter
#' @param exact - a logical variable to specifiy the index version: 
#' exact = FALSE (by default) for the approximate version of the index, 
#' and exact = TRUE for the exact version
#' @param polorder - order of the polynomial approximation (4 by default)
#' @param pred  - a numerical vector with percentiles to be predicted. 
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile is located on the drive
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension).
#' @param spatobj - a spatial object (SpatialPolygonsDataFrame) with 
#' geographic information
#' @param ... - other specific parameters
#' @return A list with: 
#' - index's name
#' - simulation type 
#' - statistics summary of the simulations
#' - simulated index distribution
#' - simulated population distribution
#' - matrix with outliers (jackknife)
#' - list with outliers values (jackknife)
#' @references Efron, B., and Tibshirani, R. J. (1993). 
#' \emph{An Introduction to the Bootstrap}. New York, Chapman and Hall
#' @references Tivadar M. (2019) 
#' \emph{OasisR: An R Package to Bring Some Order to the World of Segregation Measurement}.
#' Journal of Statistical Software,  89 (7), pp 1-39
#' @description Resampling tests for segregation indexes.
#' @examples x <- segdata@data[ ,1:2]
#' 
#' xtest <- ResampleTest (x, fun ='ISMorrill', simtype = 'MonteCarlo', 
#'                        sampleunit = 'ind', spatobj = segdata)
#' xtest$Summary
#' 
#' xtest <- ResampleTest (x, fun ='ISMorrill', simtype = 'Boot', 
#'                        sampleunit = 'unit', spatobj = segdata)
#' xtest$Summary
#' 
#' xtest <- ResampleTest (GreHSize@data[,3:5], fun='ISDuncan', simtype = 'Jack', 
#'                        sampleunit = 'unit',  spatobj = GreHSize, 
#'                        outl = TRUE, outmeth = 'sd', sdtimes = 3)
#' xtest$Summary
#' xtest$OutliersVal
#' 
#' @seealso \code{\link{ResamplePlot}}
#' @importFrom stats density
#' @importFrom graphics plot segments mtext
#' @export


ResampleTest <- function(x, fun, var = NULL, simtype = "MonteCarlo", sampleunit = "unit", samplesize = NULL, perc = c(0.05, 0.95), outl = FALSE, outmeth = "bp", 
    sdtimes = 2, IQRrange = 1.5, proba = NULL, nsim = NULL, setseed = FALSE, spatobj = NULL, folder = NULL, shape = NULL, delta = 0.5, exact = FALSE, 
    d = NULL, c = NULL, a = NULL, ck = NULL, f = "exp", b = NULL, p = NULL, spatmat = "c", queen = FALSE, distin = "m", distout = "m", diagval = "0", 
    fdist = "e", itype = "multi", dc = NULL, center = 1, polorder = 4, pred = NULL, K = 2, ptype = "int", variant = "s", ...) {
    
    # INIT
    
    if (setseed) 
        set.seed(0)
    x <- as.matrix(x)
    cldata <- segdataclean(x, c = c, b = b, a = a, p = p, ck = ck, d = d, dc = dc, spatobj = spatobj)
    x <- cldata$x
    ck <- cldata$ck
    c <- cldata$c
    b <- cldata$b
    a <- cldata$a
    p <- cldata$p
    d <- cldata$d
    dc <- cldata$dc
    spatobj <- cldata$spatobj
    if (is.null(var)) 
        var <- 1:ncol(x)
    nvar <- length(var)
    if (simtype == "Jack") {
        if (sampleunit == "unit" & is.null(nsim)) 
            nsim <- nrow(x)
        if (sampleunit == "ind" & is.null(nsim)) 
            nsim <- sum(x[, 1])
        if (sampleunit == "ind" & nsim > sum(x[, 1])) 
            nsim <- sum(x[, 1])
        if (sampleunit == "unit" & nsim > nrow(x)) 
            nsim <- nrow(x)
    } else if (is.null(nsim)) 
        nsim <- 99
    ntot <- nsim + 1
    if (is.element(fun, c("ACEDuncanPoly", "ACEPoly", "ACEDuncanPolyK"))) 
        if (is.null(dc)) {
            dc <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
            for (i in 1:ncol(dc)) dc[, i] <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center = center[i])
        }
    if (is.null(c) & (is.element(fun, c("ISMorrill", "DIMorrill", "ACL", "ISMorrillK", "DIMorrillK")))) 
        c <- contig(spatobj = spatobj, folder = folder, shape = shape, queen = queen)
    if (is.null(ck) & is.element(fun, c("ISMorrillK", "DIMorrillK"))) {
        if (K > 1) {
            if (is.null(spatobj)) 
                spatobj <- sf::st_read(dsn = folder, layer = shape)
            xx <- as.data.frame(x)
            # row.names(xx) <- labels(spatobj@data) spatobj <- SpatialPolygonsDataFrame(spatobj, xx)@data
            spatobj@data <- xx
            spatobj <- subset(spatobj, rowSums(spatobj@data) != 0)
            x <- segdataclean(spatobj@data)$x
            ngb <- spdep::poly2nb(spatobj, queen = queen)
            ngbk <- spdep::nblag(ngb, K)
            ck <- vector("list", K)
            for (k in 1:K) ck[[k]] <- spdep::nb2mat(ngbk[[k]], style = "B", zero.policy = TRUE)
        }
    }
    if (is.null(b) & is.element(fun, c("ISWong", "DIWong"))) 
        b <- boundaries(spatobj = spatobj, folder = folder, shape = shape)
    if ((is.element(fun, c("ISWong", "DIWong", "Delta", "ACO", "RCO")) & variant == "s") & is.null(a)) 
        a <- area(spatobj = spatobj, folder = folder, shape = shape)
    if ((is.element(fun, c("ISWong", "DIWong")) & variant == "s") & is.null(p)) {
        if (ptype == "all") 
            p <- perimeter(spatobj = spatobj, folder = folder, shape = shape)
        if (ptype == "int") 
            p <- rowSums(b)
    }
    if (is.null(d) & (is.element(fun, c("DPxx", "DPxy", "Pxx", "Pxy", " Poo", "SP", "RCL", "")) || spatmat == "d")) 
        d <- distance(spatobj = spatobj, folder = folder, shape = shape, distin = distin, distout = distout, diagval = diagval)
    if (is.null(dc) & is.element(fun, c("RCE", "ACEDuncan", "ACE"))) 
        dc <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center)
    if (is.null(dc) & is.element(fun, c("RCEPoly", "ACEDuncanPoly", "ACEPoly", "RCEPolyK", "ACEDuncanPolyK"))) {
        dc <- matrix(data = NA, nrow = nrow(x), ncol = length(center))
        for (i in 1:ncol(dc)) dc[, i] <- distcenter(spatobj = spatobj, folder = folder, shape = shape, center = center[i])
    }
    ILmultiSimple <- c("HMulti", "PMulti", "GiniMulti", "DMulti", "RelDivers", "CMulti", "ordinalseg")
    ILmultiAll <- c(ILmultiSimple, "spatmultiseg", "rankorderseg")
    ILx <- c(ILmultiSimple, "ISDuncan", "Gini", "HTheil", "Gorard", "Eta2")
    ILbetween <- c("DIDuncan", "Gini2", "DIMorrill", "DIMorrillK", "DIWong", "xPy", "DPxy", "RCO", "Pxy", "RCL", "RCE", "RCEPoly", "RCEPolyK")
    if (is.element(fun, c(ILmultiSimple, "rankorderseg"))) {
        nvar <- 1
        var <- 1
    }
    if (is.element(fun, c("SP", "Poo")) && itype == "multi") {
        nvar <- 1
        var <- 1
    }
    if (fun == "ordinalseg") {
        nvar <- 4
        var <- 1:4
    }
    if (fun == "spatmultiseg") {
        nvar <- 3
        var <- 1:3
    }
    
    IndTest <- matrix(nrow = nvar, ncol = 5)
    between <- FALSE
    if (is.element(fun, ILbetween) || (fun == "Poo" & itype == "between") || (fun == "SP" & itype == "between") || (fun == "spatinteract" & itype == "between")) 
        between <- TRUE
    
    if (between) {
        IndTest <- matrix(nrow = nvar * (nvar - 1), ncol = 5)
        resim <- matrix(nrow = nvar * (nvar - 1), ncol = nsim)
    } else {
        IndTest <- matrix(nrow = nvar, ncol = 5)
        resim <- matrix(nrow = nvar, ncol = nsim)
    }
    
    IndTest <- as.data.frame(IndTest)
    assign("func", eval(parse(text = fun)))
    xdistr <- vector("list", nsim)
    
    
    # DISTRIBUTIONS
    
    if (simtype == "Jack") {
        if (sampleunit == "unit") {
            klist <- sort(sample(1:nrow(x), nsim))
            cjack <- list(length = nsim)
            djack <- list(length = nsim)
            ajack <- list(length = nsim)
            bjack <- list(length = nsim)
            pjack <- list(length = nsim)
            dcjack <- list(length = nsim)
            ckjack <- list(length = nsim)
            for (k in 1:nsim) {
                xdistr[[k]] <- x[-klist[k], ]
                if (!is.null(c)) 
                  cjack[[k]] <- c[-k, -k]
                if (!is.null(d)) 
                  djack[[k]] <- d[-k, -k]
                if (!is.null(b)) 
                  bjack[[k]] <- b[-k, -k]
                if (!is.null(a)) 
                  ajack[[k]] <- a[-k]
                if (!is.null(p)) 
                  pjack[[k]] <- p[-k]
                if (!is.null(dc) & is.vector(dc)) 
                  dcjack[[k]] <- dc[-k]
                if (!is.null(dc) & is.matrix(dc)) 
                  dcjack[[k]] <- dc[-k, ]
                if (!is.null(ck)) {
                  ckprovi <- ck
                  for (j in 1:length(ck)) ckprovi[[j]] <- ckprovi[[j]][-k, -k]
                  ckjack[[k]] <- ckprovi
                }
            }
        }
        if (sampleunit == "ind") {
            x2 <- x
            k <- 0
            for (i in 1:nsim) {
                loc <- sample(1:nrow(x), 1, prob = rowSums(x2)/sum(rowSums(x2)))
                cat <- sample(1:ncol(x), 1, prob = x2[loc, ]/sum(x2[loc, ]))
                x2[loc, cat] <- max(0, x2[loc, cat] - 1)
                k <- k + 1
                xdistr[[k]] <- x
                xdistr[[k]][loc, cat] <- xdistr[[k]][loc, cat] - 1
            }
        }
    }
    if (simtype == "MonteCarlo") {
        if (sampleunit == "unit") 
            for (k in 1:nsim) {
                xdistr[[k]] <- matrix(nrow = nrow(x), ncol = ncol(x))
                neworder <- sample(c(1:nrow(x)), size = nrow(x))
                for (i in 1:nrow(x)) for (j in 1:ncol(x)) xdistr[[k]][i, j] <- x[neworder[i], j]
            }
        if (sampleunit == "ind") {
            if (is.null(proba)) 
                proba <- rep(1/nrow(x), nrow(x))
            for (i in 1:nsim) {
                xdistr[[i]] <- matrix(0, nrow = nrow(x), ncol = ncol(x))
                for (k in 1:ncol(x)) {
                  xprovi <- table(sample(nrow(x), size = sum(x[, k]), replace = T, prob = proba))
                  dimv <- as.numeric(dimnames(xprovi)[[1]])
                  xdistr[[i]][dimv, k] <- xprovi
                }
                xdistr[[i]] <- xdistr[[i]][rowSums(xdistr[[i]]) > 0, ]
            }
        }
    }
    if (simtype == "Boot") {
        if (sampleunit == "unit") {
            if (is.null(samplesize) || samplesize > nrow(x)) 
                samplesize <- nrow(x)
            for (k in 1:nsim) {
                xdistr[[k]] <- matrix(nrow = samplesize, ncol = ncol(x))
                neworder <- sample(c(1:samplesize), size = samplesize, replace = T, prob = proba)
                for (i in 1:samplesize) for (j in 1:ncol(x)) xdistr[[k]][i, j] <- x[neworder[i], j]
            }
        }
        if (sampleunit == "ind") {
            if (is.null(samplesize) || samplesize > sum(x)) 
                samplesize <- sum(x)
            for (i in 1:nrow(x)) {
                j <- 1
                while (x[i, j] == 0) j <- j + 1
                if (i == 1) {
                  xprovi2 <- rep(j, x[i, j])
                  xprovi1 <- rep(i, x[i, j])
                } else {
                  xprovi2 <- c(xprovi2, rep(j, x[i, j]))
                  xprovi1 <- c(xprovi1, rep(i, x[i, j]))
                }
                j <- j + 1
                if (j <= ncol(x)) 
                  for (k in j:ncol(x)) if (x[i, k] > 0) {
                    xprovi2 <- c(xprovi2, rep(k, x[i, k]))
                    xprovi1 <- c(xprovi1, rep(i, x[i, k]))
                  }
            }
            xprovi <- cbind(xprovi2, xprovi1)
            for (k in 1:nsim) {
                xprovi2 <- xprovi[sample(nrow(xprovi), samplesize, replace = T), ]
                xdistr[[k]] <- t(table(xprovi2[, 1], xprovi2[, 2]))
            }
        }
    }
    
    # FUNCTIONS
    
    
    if (is.element(fun, ILx)) {
        for (k in 1:nsim) resim[, k] <- func(xdistr[[k]])[var]
        IndTest[, 2] <- func(x)[var]
    }
    
    if (fun == "DIDuncan" || fun == "Gini2") {
        for (k in 1:nsim) {
            xvect <- NULL
            resprovi <- func(xdistr[[k]])
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    
    if (fun == "Atkinson") {
        for (k in 1:nsim) resim[, k] <- func(xdistr[[k]], delta)[var]
        IndTest[, 2] <- func(x, delta)[var]
    }
    
    
    if (fun == "ISMorrill") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                c1 <- cjack[[k]] else c1 <- c
            resim[, k] <- func(xdistr[[k]], c = c1, queen = queen, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, c = c, queen = queen, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    if (fun == "DIMorrill") {
        for (k in 1:nsim) {
            xvect <- NULL
            if (simtype == "Jack" & sampleunit == "unit") 
                c1 <- cjack[[k]] else c1 <- c
            resprovi <- func(xdistr[[k]], c = c1, queen = queen, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, c = c, queen = queen, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    
    if (fun == "ISMorrillK") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                ck1 <- ckjack[[k]] else ck1 <- ck
            resim[, k] <- func(xdistr[[k]], ck1 = ck, K = K, queen = queen, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, ck = ck, K = K, queen = queen, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    if (fun == "DIMorrillK") {
        if (K == 1) {
            for (k in 1:nsim) {
                xvect <- NULL
                if (simtype == "Jack" & sampleunit == "unit") 
                  c1 <- cjack[[k]] else c1 <- c
                resprovi <- DIMorrill(xdistr[[k]], c = c1, queen = queen, spatobj = spatobj, folder = folder, shape = shape)
                for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
                resim[, k] <- xvect
            }
            xvect <- NULL
            resprovi <- DIMorrill(x, c = c, queen = queen, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            IndTest[, 2] <- xvect
        } else {
            for (k in 1:nsim) {
                xvect <- NULL
                if (simtype == "Jack" & sampleunit == "unit") 
                  ck1 <- ckjack[[k]] else ck1 <- ck
                resprovi <- func(xdistr[[k]], ck = ck1, K = K, queen = queen, spatobj = spatobj, folder = folder, shape = shape)
                for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
                resim[, k] <- xvect
            }
            xvect <- NULL
            resprovi <- func(x, ck = ck, K = K, queen = queen, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            IndTest[, 2] <- xvect
        }
    }
    
    if (fun == "ISWong") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                b1 <- bjack[[k]] else b1 <- b
            if (simtype == "Jack" & sampleunit == "unit" & variant == "s") {
                a1 <- ajack[[k]]
                p1 <- pjack[[k]]
            } else {
                a1 <- a
                p1 <- p
            }
            resim[, k] <- func(xdistr[[k]], b = b1, a = a, p = p, ptype = ptype, variant = variant, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, b = b, a = a, p = p, ptype = ptype, variant = variant, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    if (fun == "DIWong") {
        for (k in 1:nsim) {
            xvect <- NULL
            if (simtype == "Jack" & sampleunit == "unit") 
                b1 <- bjack[[k]] else b1 <- b
            if (simtype == "Jack" & sampleunit == "unit" & variant == "s") {
                a1 <- ajack[[k]]
                p1 <- pjack[[k]]
            } else {
                a1 <- a
                p1 <- p
            }
            resprovi <- func(xdistr[[k]], b = b, a = a, p = p, ptype = ptype, variant = variant, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, b = b, a = a, p = p, ptype = ptype, variant = variant, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    if (fun == "xPx") {
        for (k in 1:nsim) resim[, k] <- func(xdistr[[k]], exact = exact)[var]
        IndTest[, 2] <- func(x, exact = exact)[var]
    }
    
    if (fun == "xPy") {
        for (k in 1:nsim) {
            xvect <- NULL
            resprovi <- func(xdistr[[k]], exact = exact)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, exact = exact)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    if (fun == "DPxx") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                d1 <- djack[[k]] else d1 <- d
            resim[, k] <- func(xdistr[[k]], d = d1, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, d = d, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    if (fun == "DPxy") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                d1 <- djack[[k]] else d1 <- d
            xvect <- NULL
            resprovi <- func(xdistr[[k]], d = d1, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, d = d, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    
    if (fun == "spatinteract" & itype != "between") {
        for (k in 1:nsim) resim[, k] <- diag(func(xdistr[[k]], spatobj = spatobj, folder = folder, shape = shape))[var]
        IndTest[, 2] <- diag(func(x, spatobj = spatobj, folder = folder, shape = shape))[var]
    }
    
    if (fun == "spatinteract" & itype == "between") {
        for (k in 1:nsim) {
            xvect <- NULL
            resprovi <- func(xdistr[[k]], spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    if (fun == "Delta" || fun == "ACO") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                a1 <- ajack[[k]] else a1 <- a
            resim[, k] <- func(xdistr[[k]], a = a1, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, a = a, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    
    if (fun == "RCO") {
        for (k in 1:nsim) {
            xvect <- NULL
            if (simtype == "Jack" & sampleunit == "unit") 
                a1 <- ajack[[k]] else a1 <- a
            resprovi <- func(xdistr[[k]], a = a, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, a = a, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    
    if (fun == "ACL") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit" & spatmat == "c") 
                c1 <- cjack[[k]] else c1 <- c
            if (simtype == "Jack" & sampleunit == "unit" & spatmat == "d") 
                d1 <- djack[[k]] else d1 <- d
            resim[, k] <- func(xdistr[[k]], c = c1, d = d1, queen = queen, spatmat = spatmat, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, 
                folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, c = c, d = d, queen = queen, spatmat = spatmat, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, 
            folder = folder, shape = shape)[var]
    }
    
    
    if (fun == "RCL") {
        for (k in 1:nsim) {
            xvect <- NULL
            if (simtype == "Jack" & sampleunit == "unit") 
                d1 <- djack[[k]] else d1 <- d
            resprovi <- func(xdistr[[k]], d = d1, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, d = d, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    
    if (fun == "Pxx") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                d1 <- djack[[k]] else d1 <- d
            resim[, k] <- func(xdistr[[k]], d = d1, fdist = fdist, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, 
                shape = shape)[var]
        }
        IndTest[, 2] <- func(x, d = d, fdist = fdist, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    
    if (fun == "Pxy") {
        for (k in 1:nsim) {
            xvect <- NULL
            if (simtype == "Jack" & sampleunit == "unit") 
                d1 <- djack[[k]] else d1 <- d
            resprovi <- func(xdistr[[k]], d = d1, fdist = fdist, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, 
                shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, d = d, fdist = fdist, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    if (fun == "Poo" || fun == "SP") {
        if (itype != "between") {
            for (k in 1:nsim) {
                if (simtype == "Jack" & sampleunit == "unit") 
                  d1 <- djack[[k]] else d1 <- d
                resim[, k] <- func(xdistr[[k]], d = d1, fdist = fdist, itype = itype, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, 
                  folder = folder, shape = shape)[var]
            }
            IndTest[, 2] <- func(x, d = d, fdist = fdist, itype = itype, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, 
                shape = shape)[var]
        } else {
            for (k in 1:nsim) {
                xvect <- NULL
                if (simtype == "Jack" & sampleunit == "unit") 
                  d1 <- djack[[k]] else d1 <- d
                resprovi <- func(xdistr[[k]], d = d1, fdist = fdist, itype = itype, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, 
                  folder = folder, shape = shape)
                for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
                resim[, k] <- xvect
            }
            xvect <- NULL
            resprovi <- func(x, d = d, fdist = fdist, itype = itype, distin = distin, distout = distout, diagval = diagval, spatobj = spatobj, folder = folder, 
                shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            IndTest[, 2] <- xvect
        }
    }
    
    if (fun == "ACEDuncan") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                dc1 <- dcjack[[k]] else dc1 <- dc
            resim[, k] <- func(xdistr[[k]], dc = dc1, center = center, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, dc = dc, center = center, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    if (fun == "RCE") {
        for (k in 1:nsim) {
            xvect <- NULL
            if (simtype == "Jack" & sampleunit == "unit") 
                dc1 <- dcjack[[k]] else dc1 <- dc
            resprovi <- func(xdistr[[k]], dc = dc1, center = center, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, dc = dc, center = center, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    
    if (fun == "ACEDuncanPoly") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                dc1 <- dcjack[[k]] else dc1 <- dc
            resim[, k] <- func(xdistr[[k]], dc = dc1, center = center, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, dc = dc, center = center, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    
    if (fun == "RCEPoly") {
        for (k in 1:nsim) {
            xvect <- NULL
            if (simtype == "Jack" & sampleunit == "unit") 
                dc1 <- dcjack[[k]] else dc1 <- dc
            resprovi <- func(xdistr[[k]], dc = dc1, center = center, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, dc = dc, center = center, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    if (fun == "ACEDuncanPolyK") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                dc1 <- dcjack[[k]] else dc1 <- dc
            resim[, k] <- func(xdistr[[k]], dc = dc1, K = K, center = center, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, dc = dc, K = K, center = center, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    if (fun == "RCEPolyK") {
        for (k in 1:nsim) {
            xvect <- NULL
            if (simtype == "Jack" & sampleunit == "unit") 
                dc1 <- dcjack[[k]] else dc1 <- dc
            resprovi <- func(xdistr[[k]], dc = dc1, K = K, center = center, spatobj = spatobj, folder = folder, shape = shape)
            for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
            resim[, k] <- xvect
        }
        xvect <- NULL
        resprovi <- func(x, dc = dc, K = K, center = center, spatobj = spatobj, folder = folder, shape = shape)
        for (i in 1:length(var)) xvect <- c(xvect, resprovi[var[i], var[-i]])
        IndTest[, 2] <- xvect
    }
    
    
    if (fun == "ACE" || fun == "ACEPoly") {
        for (k in 1:nsim) {
            if (simtype == "Jack" & sampleunit == "unit") 
                dc1 <- dcjack[[k]] else dc1 <- dc
            resim[, k] <- func(xdistr[[k]], dc = dc, a = a, center = center, spatobj = spatobj, folder = folder, shape = shape)[var]
        }
        IndTest[, 2] <- func(x, dc = dc, a = a, center = center, spatobj = spatobj, folder = folder, shape = shape)[var]
    }
    
    if (fun == "rankorderseg") {
        for (k in 1:nsim) resim[, k] <- func(xdistr[[k]], polorder = polorder, pred = pred)$Hr
        IndTest[, 2] <- func(x, polorder = polorder, pred = pred)$Hr
    }
    
    if (fun == "spatmultiseg") {
        for (k in 1:nsim) resim[, k] <- func(xdistr[[k]], spatobj = spatobj, folder = folder, shape = shape, ...)[var]
        IndTest[, 2] <- func(x, spatobj = spatobj, folder = folder, shape = shape, ...)[var]
    }
    
    # RESULTS
    
    
    if (!between) 
        IndTest[, 1] <- 1:nvar else {
        contor <- 0
        for (i in var) for (j in var) if (i != j) {
            contor <- contor + 1
            IndTest[contor, 1] <- paste0(i, "-", j)
        }
    }
    
    if (simtype == "MonteCarlo") {
        names(IndTest) <- c("Var", fun, "Mean", "Rank", "P.Value")
        if (fun == "ordinalseg" || fun == "spatmultiseg") 
            names(IndTest)[1] <- "Index"
        for (i in 1:nrow(IndTest)) {
            IndTest[i, 3] <- round(mean(resim[i, ]), 4)
            IndTest[i, 4] <- rank(c(IndTest[i, 2], resim[i, ]), ties.method = "min")[1]
            IndTest[i, 5] <- (ntot - IndTest[i, 4] + 1)/ntot
        }
    }
    
    if (simtype == "Boot" || simtype == "Jack") {
        term <- "th"
        if (substr(as.character(perc[1]), 3, 4) == "01") 
            term <- "st"
        if (substr(as.character(perc[1]), 3, 4) == "02") 
            term <- "nd"
        if (substr(as.character(perc[1]), 3, 4) == "03") 
            term <- "rd"
        names(IndTest) <- c("Var", fun, paste0(substr(as.character(perc[1]), 4, 4), term, "_percentile"), "Median", paste0(substr(as.character(perc[2]), 
            3, 4), "th_percentile"))
        if (fun == "ordinalseg" || fun == "spatmultiseg") 
            names(IndTest)[1] <- "Index"
        for (i in 1:nrow(IndTest)) {
            IndTest[i, 3] <- round(stats::quantile(resim[i, ], perc[1]), 4)
            IndTest[i, 4] <- round(stats::quantile(resim[i, ], 0.5), 4)
            IndTest[i, 5] <- round(stats::quantile(resim[i, ], perc[2]), 4)
        }
    }
    if (simtype == "Jack") {
        jack.bias <- vector(length = nvar)
        jack.se <- vector(length = nvar)
        for (i in 1:nvar) {
            jack.bias[i] <- (nsim - 1) * (mean(resim[i, ]) - IndTest[i, 2])
            jack.se[i] <- sqrt(((nsim - 1)/nsim) * sum((resim[i, ] - mean(resim[i, ]))^2))
        }
        IndTest$JackBias <- jack.bias
        IndTest$JackSE <- jack.se
    }
    if (simtype == "Boot") {
        boot.se <- vector(length = nvar)
        for (i in 1:nvar) boot.se[i] <- sqrt(var(resim[i, ])/length(resim[i, ]))
        IndTest$BootSE <- boot.se
    }
    sqrt(var(resim[i, ])/length(resim[i, ]))
    stats::sd(resim[i, ])
    
    # OUTLIERS
    
    if (outl == TRUE & simtype == "Jack") {
        outl <- matrix(FALSE, nrow = ncol(x), ncol = nsim)
        if (outmeth == "sd") {
            for (i in 1:nrow(resim)) outl[i, ] <- resim[i, ] >= mean(resim[i, ]) + sdtimes * stats::sd(resim[i, ]) | resim[i, ] < mean(resim[i, ]) - sdtimes * 
                stats::sd(resim[i, ])
        }
        if (outmeth == "bp") {
            for (i in 1:nrow(resim)) outl[i, ] <- is.element(resim[i, ], grDevices::boxplot.stats(resim[i, ], coef = IQRrange)$out)
        }
        if (is.element(outmeth, c("z", "t", "chisq", "mad"))) {
            if (is.null(proba)) 
                proba <- 0.9
            for (i in 1:nrow(resim)) outl[i, ] <- outliers::scores(resim[i, ], type = outmeth, prob = proba)
        }
        outval <- vector("list", nrow(resim))
        for (i in 1:length(outval)) outval[[i]] <- resim[i, ][outl[i, ]]
        graphics::boxplot(t(resim))
        outl <- t(outl)
        if (sampleunit == "ind" & length(which(outl[, 1])) > 0) {
            olist <- which(outl[, 1])
            outl <- list(length(which(outl[, 1])))
            for (i in 1:length(olist)) outl[[i]] <- xdistr[[olist[i]]]
        }
        result <- list(fun, simtype, IndTest, resim, xdistr, outl, outval)
        names(result) <- c("Index", "SimType", "Summary", "IndexDist", "RandomDist", "Outliers", "OutliersVal")
    } else {
        result <- list(fun, simtype, IndTest, resim, xdistr)
        names(result) <- c("Index", "SimType", "Summary", "IndexDist", "RandomDist")
    }
    return(result)
}


#' A function to plot the results of resampling methods
#'
#' @usage ResamplePlot(ResampleTest, var = 1, coldist = 'red', colind = 'blue', 
#' legend = TRUE, legendpos = 'top', cex.legend = 1, bty = 'o')
#' @param ResampleTest - a ResampleTest object prodused with \code{\link{ResampleTest}} function
#' @param var - the number of the variable to be plot 
#' @param coldist - color used to plot the simulated distribution 
#' @param colind - color used to plot the index
#' @param legend - logical parameter, to control the legend's plots
#' @param legendpos - a character string giving the legend's position: 
#' 'bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 
#' 'topright', 'right' and 'center'.
#' @param cex.legend - a numerical value giving the amount by which 
#' plotting text and symbols in legend should be magnified relative to the default. 
#' @param bty - a character string which determines the type of box 
#' of the legend. If bty is one of 'o' (the default), 'l', '7', 'c', 
#' 'u', or ']' the resulting box resembles the corresponding upper 
#' case letter. A value of 'n' suppresses the box.
#' @return A plot with resampling distribution
#' @references Tivadar M. (2019) 
#' \emph{OasisR: An R Package to Bring Some Order to the World of Segregation Measurement}.
#' Journal of Statistical Software,  89 (7), pp 1-39
#' @description Plot of Monte Carlo simulations results. The function can
#' be used in two ways: buy providing a ResampleTest object, using \code{\link{ResampleTest}} 
#' or a simulated distribution vector, a value and a name of the index
#' @examples x <- segdata@data[ ,1:2]
#' 
#' xtest <- ResampleTest (x, fun ='ISMorrill', simtype = 'MonteCarlo', 
#'                        sampleunit = 'unit', spatobj = segdata)
#'                        
#' ResamplePlot(xtest, var = 1)
#' 
#' @seealso \code{\link{ResampleTest}} 
#' @export


ResamplePlot <- function(ResampleTest, var = 1, coldist = "red", colind = "blue", legend = TRUE, 
                         legendpos = "top", cex.legend = 1, bty = "o") {
    indexname <- ResampleTest$Index
    dens <- ResampleTest$IndexDist[var, ]
    ind <- ResampleTest$Summary[var, 2]
    simtype <- ResampleTest$SimType
    if (simtype == "Boot") 
        simtype <- "Bootstrap"
    if (simtype == "Jack") 
        simtype <- "JackKnife"
    if (simtype == "MonteCarlo") 
        simtype <- "Monte Carlo"
    nsim <- length(dens)
    ntot <- nsim + 1
    dens2 <- density(dens)
    liminf <- min(dens2$x, ind)
    limsup <- max(dens2$x, ind)
    ylimit = c(0, ((max(dens2$y)) + 2))
    if (simtype == "Monte Carlo") {
        statname <- "mean"
        esper <- ResampleTest$Summary[var, 3]
    } else {
        statname <- "median"
        esper <- ResampleTest$Summary[var, 4]
    }
    plot(dens2, xlim = c(liminf, limsup), ylim = ylimit, lwd = 2, col = coldist, main = "", xlab = "Index values")
    segments(esper, 0, esper, max(dens2$y), lwd = 1, col = coldist)
    segments(ind, 0, ind, max(dens2$y), lwd = 3, col = colind)
    mtext(paste0(simtype, " Test: ", indexname), side = 3, font = 2, line = 2)
    if (legend) 
        legend(legendpos, c("Simulated distribution", paste("Simulated", statname), indexname), col = c(coldist, coldist, colind), lty = 1, lwd = c(2, 
            1, 2), bty = bty, cex = cex.legend)
}


