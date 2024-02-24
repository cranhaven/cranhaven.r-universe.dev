library(sf)
library(lwgeom)
library(measurements)


################################################## 

# GEOGRAPHIC FUNCTIONS

################################################## 


#' A function to compute the spatial units' areas
#'
#' @usage area(spatobj = NULL, folder = NULL, shape = NULL)
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile with the geographic information 
#' is located.
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) which contains the geographic information
#' @param spatobj - a spatial object (class sf, sfc or sfg) containing 
#' geographic information
#' @return A area vector
#' @description The function is based on \pkg{sf} package and can be 
#' used with a shape file or an R spatial object (class sf, sfc or sfg).
#' @examples  area(segdata) 
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' area(folder = foldername, shape = shapename)
#' @seealso  Other spatial functions used for segregation indices 
#' computation: \code{\link{contig}}, \code{\link{perimeter}}, 
#' \code{\link{distance}}, \code{\link{distcenter}}, 
#' \code{\link{boundaries}}
#' @importFrom methods slot
#' @export

area <- function (spatobj = NULL, folder = NULL, shape = NULL) 
{
    if (is.null(spatobj)) spatobj <- sf::st_read(dsn = folder, layer = shape) else spatobj <- sf::st_as_sf(spatobj)
    area <- sf::st_area(spatobj)
    units(area) <- NULL
    return(area)
}


#' A function to compute the contiguity matrix
#'
#' @usage contig(spatobj = NULL, folder = NULL, shape = NULL, queen = FALSE)
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile with the geographic information 
#' is located.
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) which contains the geographic information
#' @param spatobj - a spatial object (class sf, sfc or sfg) containing 
#' geographic information
#' @param queen = TRUE for queen criteria, FALSE (by default)  for rook criteria
#' @return A first order contiguity (adjacency) matrix, where each 
#' element [\emph{i,j}] equals 1 if \emph{i}-th  and \emph{j}-th  
#' spatial units are adjacent, 0 otherwise (queen or rook criteria)
#' @description The function is based on \pkg{sf}  
#' package and can be used with a shape file 
#' or an R spatial object (class sf, sfc or sfg).
#' @examples  contig(segdata) 
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' contig(folder = foldername, shape = shapename)
#' @seealso  Other spatial functions used for segregation indices 
#' computation: \code{\link{area}}, \code{\link{perimeter}}, 
#' \code{\link{distance}}, \code{\link{distcenter}}, 
#' \code{\link{boundaries}}
#' @export


contig <- function (spatobj = NULL, folder = NULL, shape = NULL, queen = FALSE) 
{
    if (is.null(spatobj)) spatobj <- sf::st_read(dsn = folder, layer = shape) else spatobj <- sf::st_as_sf(spatobj)
    if(queen) contig <- sf::st_touches(spatobj,spatobj, sparse = F)*1
    else contig <- sf::st_relate(spatobj, spatobj, pattern = "F***1****", sparse = F)*1
    return(contig)
}



#' A function to compute the spatial units' perimeters
#'
#' @usage perimeter(spatobj = NULL, folder = NULL, shape = NULL)
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile with the geographic information 
#' is located.
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) which contains the geographic information
#' @param spatobj - a spatial object (class sf, sfc or sfg) containing 
#' geographic information
#' @return A perimeter vector 
#' @description The function is based on on \pkg{sf} and \pkg{lwgeom}  
#' packages and can be used with a shape file or an R spatial object 
#' (class sf, sfc or sfg).
#' @examples  perimeter(segdata)  
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' perimeter(folder = foldername, shape = shapename)
#' @seealso  Other spatial functions used for segregation indices 
#' computation:  \code{\link{area}}, \code{\link{contig}}, 
#' \code{\link{distance}}, \code{\link{distcenter}}, 
#' \code{\link{boundaries}}
#' @export

perimeter <- function (spatobj = NULL, folder = NULL, shape = NULL) 
{
    if (is.null(spatobj)) spatobj <- sf::st_read(dsn = folder, layer = shape) else spatobj <- sf::st_as_sf(spatobj)
    perim <- lwgeom::st_perimeter(spatobj)
    units(perim) <- NULL
    return(perim)
}




#' A function to compute the distance matrix between centroids 
#' of spatial units
#'
#' @usage distance(spatobj = NULL, folder = NULL, shape = NULL,
#' distin = 'm',  distout = 'm', diagval = '0')
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile with the geographic information 
#' is located.
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) which contains the geographic information
#' @param spatobj - a spatial object (class sf, sfc or sfg) containing 
#' geographic information
#' @param distin - input metric conversion, based on  \pkg{measurements} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{measurements} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param diagval -  the user has the choice of the definition of the diagonal: 
#' diagval = '0'  (by default) for an 'empty' diagonal and diagval = 'a'
#' to compute  the diagonal as 0.6 * square root (spatial units area) (White, 1983) 
#' @return A matrix with the distance between centroids
#' @description The function is based on \pkg{sf} 
#' package and can be used with a shape file or an R spatial object 
#' (class sf, sfc or sfg).
#' @examples  distance(segdata) 
#' 
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' distance(folder = foldername, shape = shapename)
#' @seealso  Other spatial functions used for segregation indices 
#' computation: \code{\link{area}}, \code{\link{contig}}, 
#' \code{\link{perimeter}}, \code{\link{distcenter}}, 
#' \code{\link{boundaries}}
#' @export

distance <- function (spatobj = NULL, folder = NULL, shape = NULL, distin = "m", 
                      distout = "m", diagval = "0") 
{
    if (is.null(spatobj)) spatobj <- sf::st_read(dsn = folder, layer = shape) else spatobj <- sf::st_as_sf(spatobj)
    centroids <- sf::st_centroid(spatobj) 
    dist <- sf::st_distance(centroids)
    if (diagval == "a") {
        a <- area(spatobj = spatobj, folder = folder, shape = shape)
        diag(dist) <- sqrt(a) * 0.6
    }
    units(dist) <- NULL
    dist <- measurements::conv_unit(dist, from = distin, to = distout)
    return(dist)
}

#' A function to compute the distance from spatial units centroids 
#' to the center
#'
#' @usage distcenter(spatobj = NULL, folder = NULL, shape = NULL, 
#' center = 1, distin = 'm',  distout = 'm')
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile with the geographic information 
#' is located.
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) which contains the geographic information
#' @param spatobj - a spatial object (class sf, sfc or sfg) containing 
#' geographic information
#' @param center - the row number of the center
#' @param distin - input metric conversion, based on  \pkg{measurements} package and 
#' includes conversions from 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @param distout - output metric conversion, based on  \pkg{measurements} package and 
#' includes conversions to 'm', 'km', 'inch', 'ft', 'yd', 'mi', 'naut_mi', etc.
#' @return A vector with the distance to the center's centroid
#' @description The function is based on \pkg{sf} 
#' package and it can be used with a shape file or an R 
#' spatial object (class sf, sfc or sfg).
#' @examples  distcenter(segdata, center = 46) 
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' distcenter(folder = foldername, shape = shapename, center = 19)
#' @seealso  Other spatial functions used for segregation indices 
#' computation: \code{\link{area}}, \code{\link{contig}}, 
#' \code{\link{perimeter}}, \code{\link{distance}}, 
#' \code{\link{boundaries}}
#' @export


distcenter <- function (spatobj = NULL, folder = NULL, shape = NULL, center = 1, 
                        distin = "m", distout = "m") 
{
    if (is.null(spatobj)) spatobj <- sf::st_read(dsn = folder, layer = shape) else spatobj <- sf::st_as_sf(spatobj)
    distcenter <- vector(length = length(spatobj[[1]]))
    centroids <- sf::st_centroid(spatobj) 
    distcenter <- sf::st_distance(centroids, centroids[center,])
    units(distcenter) <- NULL
    distcenter <- as.vector(distcenter)
    distcenter <- measurements::conv_unit(distcenter, from = distin, to = distout)
    return(distcenter)
}

#' A function to compute the matrix of common boundaries
#'
#' @usage boundaries(spatobj = NULL, folder = NULL, shape = NULL)
#' @param folder - a character vector with the folder (directory) 
#' name indicating where the shapefile with the geographic information 
#' is located.
#' @param shape - a character vector with the name of the shapefile 
#' (without the .shp extension) which contains the geographic information
#' @param spatobj - a spatial object (class sf, sfc or sfg) containing 
#' geographic information
#' @return A common boundaries matrix
#' @description The function is based on \pkg{sf}  
#' package and it can be used with a shape file 
#' or an R spatial object (class sf, sfc or sfg).
#' @examples  boundaries(segdata) 
#' foldername <- system.file('extdata', package = 'OasisR')
#' shapename <- 'segdata'
#' boundaries(folder = foldername, shape = shapename)
#' @seealso  Other spatial functions used for segregation indices 
#' computation:  \code{\link{area}}, \code{\link{contig}}, 
#' \code{\link{perimeter}}, \code{\link{distance}}, 
#' \code{\link{distcenter}}
#' @export


boundaries <-function (spatobj = NULL, folder = NULL, shape = NULL) 
{
    if (is.null(spatobj)) spatobj <- sf::st_read(dsn = folder, layer = shape) else spatobj <- sf::st_as_sf(spatobj)
    boundaries <- contig(spatobj)
    lines <- sf::st_cast(sf::st_intersection(spatobj,spatobj))
    l_lines <- sf::st_length(lines)
    l_lines <- l_lines[l_lines != l_lines[[1]]]
    k <- 1
    for (i in 1:(length(spatobj[[1]]))) 
        for (j in 1:length(spatobj[[1]])) 
            if (boundaries[i, j] == 1) {
                boundaries[i, j] <- l_lines[k]
                k <- k+1
            }
    units(boundaries) <- NULL
    return(boundaries)
}

