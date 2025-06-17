#' @export
#' @param rank This is the Rank of the maze.
#' @description The node length calculates all the nodes on the longest row for a given rank.
#' @details This needs to have a rank value of greater than 1. This is needed so that you can cross check how many coloured nodes are located on the longest row.
#' @author Aiden Loe
#' @title Top Nodes
#' @examples
#' rank <-3
#' topNodes(rank)

# Longest row
#lengthOfLongestRow <- rank
topNodes <- function(rank){
  if(rank<2){
    stop('Please select a rank that is greater than 1.')
  }
  topNodes <- NULL

  topNodes[[1]] <- rank
for(i in 1:(rank-1))
{
  topNodes[[i+1]]<- topNodes[[i]] + rank-1
}
return(topNodes)
}


