#'
#' @title A function to order the elements of the m_i-subrrounds
#'
#' @description An auxiliary function. In the case of obtaining the list of neighbors of class
#' nb or \code{\link{poly2nb}}, it is necessary to reorder the elements based on distance and/or angle.
#' @param listw an object of the nb class.
#' @param sf the sf object used to get the \code{listw} .
#' @usage nb2nb_order(listw = listw, sf = NULL)
#' @details Sort the elements of a list nb. First by distance and
#  second (in case of equal distances) by angle.
#' @return An object of the nb class with the elements in order.
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paez@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#'   @references
#'   \itemize{
#'     \item Ruiz, M., López, F., and Páez, A. (2021).
#'     A test for global and local homogeneity of categorical data based on spatial runs.
#'       \emph{Working paper}.
#'   }
#' @seealso
#' \code{\link{dgp.spq}}, \code{\link{sp.runs.test}}, \code{\link{local.sp.runs.test}}
#' @export
#' @examples
#'
#' # With a sf object (irregular lattice)
#' library(sf)
#' fname <- system.file("shape/nc.shp", package="sf")
#' nc <- sf::st_read(fname)
#' listw <- spdep::poly2nb(as(nc,"Spatial"), queen = FALSE)
#' listw.order <- nb2nb_order(listw = listw, sf = nc)
#'
#' # With a sf object (regular lattice: hexagons)
#' sfc = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))
#' hexs <- sf::st_make_grid(sfc, cellsize = 0.1, square = FALSE)
#' hexs.sf <- sf::st_sf(hexs)
#' listw  <- spdep::poly2nb(as(hexs.sf, "Spatial"), queen = FALSE)
#' listw.order <- nb2nb_order(listw = listw, sf = hexs.sf)
#'
nb2nb_order <- function(listw = listw, sf = NULL){

# n <- length(listw)
# co <- as.data.frame(sf::st_coordinates(sf::st_centroid(sf)))
# co <- st_as_sf(co,coords=c("X","Y"), crs = 32630)
# dis <- 1/matrix(as.numeric(st_distance(co,co)),ncol=n,nrow=n)
# diag(dis) <- 0
# matw <- nb2mat(listw,style = 'B', zero.policy = TRUE)
# m <- rowSums(matw)
# matwdis <- dis*matw
# NB <- list()
# for (i in 1:n){
#   or <-  order(matwdis[i,], decreasing = TRUE)
#   NB[[i]]<- or[1:m[i]]
#   if (m[i]==0){NB[[i]]<- as.integer(0)}
# }
# class(NB)<- 'nb'
# }
  if (!is.null(sf)){
  n <- length(listw)
  matw <- spdep::nb2mat(listw, style = 'B',
                        zero.policy = TRUE)
  m <- rowSums(matw)
  co <- st_coordinates(st_centroid(sf))
  NB <- list()
for (i in 1:dim(co)[1]){
  if (m[i] == 0){NB[[i]]<- as.integer(0)}
  else {
  a <- co[listw[[i]],] - t(matrix(co[i,],nrow = 2,ncol = length(listw[[i]])))
  a <- round(cbind(sqrt(a[,1]^2+a[,2]^2),atan2(a[,1],a[,2])*180/pi),digits = 6)
  a <- order(a[,1],a[,2])
  # a <- order(a,decreasing = FALSE) # sort(a,index.return=TRUE)$ix
  NB[[i]] <- listw[[i]][a]
  }
}
  class(NB)<- 'nb'
  }
  else if (is.null(sf)) {

  NB <- list()
  res <- apply(listw,1,function(i){
    out <- order(i, decreasing = TRUE)
    iszero <- i == 0
    c(out[!iszero[out]], i[iszero])
  })
  res <- t(res)
  for (i in 1:dim(res)[1]){
    NB[[i]] <- as.integer(res[i,][res[i,]!=0])
    if (length(NB[[i]])==0) {NB[[i]]<- as.integer(0)}
  }
  class(NB)<- 'nb'

  # NB <- list()
  # for (i in 1:dim(listw)[1]){
  #   or <-  order(listw[i,], decreasing = TRUE)
  #   NB[[i]]<- or[1:m[i]]
  #   if (m[i]==0){NB[[i]]<- as.integer(0)}
  # }
  # class(NB)<- 'nb'
  }

return <- NB
}

