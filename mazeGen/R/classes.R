#' @export
#'
print.est <-function(x,...){
  est <- x
  cat("Summary:")
  cat(paste0("\nMaze: Rank ", est$rank ))
  cat("\nThe coloured dots are located in: ")
  cat(est$nodePosition)
  cat(paste0("\nThe maximum score(dots) that can be achieved in this maze is ",est$maxScore, "."))
  cat(paste0("\nThe minimum number of steps to achieve a maximum score is ", est$minStep, "."))
  cat(paste0("\nThe path(s) with the minimum number of steps to achieve a maximum score is ", est$minPath$minRoutes, "."))
  cat(paste0("\nAll possible paths to achieve a maximum score is ", est$allPP$maxScoreRoutes, ".\n"))
  cat("\nThe number of separate paths across the different number of dots are: \n")
  cat('\n')
  print(est$possibleBlackNodeRoutes)
  #print(allPath)

  cat(paste0("\nThe paths with minimum steps are: \n"))
  print(est$minPath$allminPath)
  cat(paste0("\nAll possible paths are: \n"))
  print(est$allPP$allPath)
}


#' @export
#'
print.np <- function(x,...){
  nodePosition<- x
  cat("Result:")
  cat("\nMaze",nodePosition$rank)
  cat("\nSaturation:",nodePosition$satPercent*100,"%")
  cat("\nThe coloured dots are in:", nodePosition$nodePosition)
}


#' @export
#'
print.ab <- function(x,...){
  ability<- x
  cat("Result\n")
  cat("Connected Dots:", ability$dot,"\n")
  cat("Most Frequent Dots:", as.numeric(ability$mostFreq[,1]), "with", as.numeric(ability$mostFreq[,2]), "paths\n")
  cat("Ability Score based on model",ability$model,":",ability$userResult[,3],"\n")
  cat("\n No. of dots linked\n")
  print(ability$result)
}

#' @export
#'
# For minimum leg routes function
print.min<- function(x,...){
  mLRoutes <- x
  cat("The number of solution is:", mLRoutes$totalminPaths, "\n")
  cat("The optimium path(s) with minimum legs is:",mLRoutes$minSteps,"\nThe minimum number of steps for the optimal solution is:\n")
  print(mLRoutes$minPaths)
}

