#' @export
#' @param rank This is the rank of the maze.
#' @param satPercent The percentage of saturation. Between 0-1.
#' @description Calculate how many possible variation of black dotes for a given saturation.
#' @details Calculate how many possible variation of black dotes for a given saturation. The first node will not be a black dot.
#' @author Aiden Loe
#' @title howMany
#' @seealso \code{\link{lowerGrid}}
#' @keywords lowerGrid
#' @examples
#' howMany(rank=5, satPercent=0.5)

howMany <- function(rank,satPercent){
  t<-0
  for (i in 1:rank){
    t<-t + i
  }
  saturation<- ceiling(t*satPercent)
  #print(saturation)
  return(choose(t, saturation))
}

