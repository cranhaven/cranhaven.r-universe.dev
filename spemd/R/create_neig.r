#' @title create.neig
#' @description Internal function, initiates the neighbourhood relationships between the points in the processed data set.
#' @aliases  create.neig
#' @param data.set Data set to create neighbourhood from.
#' @param nb.nn Number of nearest neighbours. Defaults to 4.
#' @param duplicate Ignored.
#' @param verbose Prints progress information messages. Defaults to FALSE.
#' @author Pierre Roudier
#' @importFrom sp coordinates
#' @importFrom spdep knearneigh knn2nb
create.neig <- function(
  data.set,
  # gridded.data,
  nb.nn = 4,
  duplicate = 'remove',
  verbose = FALSE
){

  coords <- as.data.frame(coordinates(data.set))
  names(coords) <- c("x","y")

  # if (TRUE) {
    #   if (gridded.data){


  # Finding nearest neighbours
  data.set.nn <- knearneigh(as.matrix(coords), k = nb.nn, longlat = FALSE)
  # Converting to nb object
  data.set.nb <- knn2nb(data.set.nn)

  neig <- list(NULL)
  neig$x <- coords[,1]
  neig$y <- coords[,2]
  neig$n <- nrow(coords)
  neig$neig <- data.set.nb

  class(neig) <- c(class(neig),"neig")

  return(neig)
}
