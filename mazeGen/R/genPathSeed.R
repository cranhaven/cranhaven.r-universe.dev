#' @export
#' @import igraph
#' @param path Selecting the specific number of paths to achieve the maximum score.
#' @param rank This is the rank of the maze.
#' @param satPercent This is of saturation percentage ranging from 0-1.
#' @param seed The starting seed to begin searching for the seed with specific paths.
#' @param runSeed This determines the number of searches for the specific paths before stopping.
#' @description This generate the solution by searching for the SEED that returns the specific number of paths to achieve the maximum score for a given rank and saturation.
#' @details This might be computationally intensive as the maze size increases. The seed is necessary so that the algorithm does not always begin from the smallest seed value. Based on the starting seed value, it will search for the next seed that returns the desired number of path defined by the user. To limit the search time, The function will stop looking for the seed based on the runSeed value. Using this function does not guarantee that the minimum number of steps will be the same for all possible paths to achieve the maximum score. To ensure that the number of steps are equal across all possible paths for the maximum score, please use the \code{\link{genEMLseed}} function instead.
#' @author Aiden Loe and Maria Sanchez
#' @title Generate Path Seed
#' @seealso \code{\link{np}},\code{\link{mazeEst}}, \code{\link{genEMLseed}}
#' @examples
#'
#' rank <- 5
#' satPercent <- 0.5
#' seed <- 1
#'
#' #Search for just one unique path
#' justOne <- genPathSeed(path=1,rank=rank,satPercent=satPercent,seed=seed)
#' nodePosition <- np(rank,satPercent,seed=justOne)
#' mazeEst(nodePosition)
#'
#' #Search for three path
#' justThree <- genPathSeed(path=3,rank=rank,satPercent=satPercent,seed=seed, runSeed=300)
#' nodePosition <- np(rank,satPercent,seed=justThree)
#' mazeEst(nodePosition)

genPathSeed<-function(path=3,rank=5,satPercent=0.5,seed=1, runSeed=500){
  if(path==1){
    num <- 1
    count <- 1
  while (num>1){
    seed<-seed+1
    num<-lookUniqueSolution(rank,satPercent,seed)
    count <- count + 1
    if(count > runSeed){
      num <- 1
      stop("No unique solution can be found. Please reduce saturation percentage or increase the runSeed value.")
    }
   }
  }else{
    num <- 1
    count <- 1
    while(num != path){
      seed<-seed+1
      (num<-pathSolution(rank,satPercent,seed))
      count <- count + 1
      if(count > runSeed){
        num <- 1
        stop("The desired path cannot be found. Consider lowering number of paths or increasing the runSeed value.")
      }
    }
  }
  return(seed)
}


# justThree <- genPathSeed(path=7,rank=rank,satPercent=satPercent,seed=seed, runSeed=300)
#  nodePosition <- np(rank,satPercent,seed=justThree)
#   mazeEst(nodePosition)
# justThree
# rank
#
#  #Generate item
#  mazeHTML(rank,satPercent,seed=justThree,grid = gridEightUp,wd=NULL,
#  background="#7abcff",boxBackground="#66CDAA", fontColour="white ",
#  Timer=TRUE)
