#' @export
#' @import igraph
#' @param dot This is the number of black dots.
#' @param nodePosition You need to calculate the nodePosition.
#' @param model There are 4 models to estimate ability (t1,t2,t3,t4).
#' @description The ability function returns the weighted score of the individual given his raw score (i.e. the number of black dotes collected).
#' @details This function calculates the weighted score of the participant given the number of dots collected. The function adopts 4 different models which follows the Davies & Davies (1965) paper. The formula for is Model 1:
#'
#' \deqn{log(2^{R}/U_{m})}
#'
#' where \eqn{2^R} is the total number of paths and \eqn{U_{m}} is the paths through the specified number of dots. The formula for Model 2:
#'
#' \deqn{log(U_{\hat{m}}/U_{m})}
#'
#' where \eqn{U_{\hat{m}}} is the value with the maximum number of connected dots. The formula for Model 3:
#'
#'  \deqn{log(2^{R}*s^{4}/U_{m})}
#'
#' where \eqn{s^{4}} is the saturation value. The formula for Model 4 is:
#'
#' \deqn{log(U_{\hat{m}}*s^{4}/U_{m})}
#'
#' We included all four models to calculate maze ability.
#' @author Aiden Loe and Maria Sanchez
#' @title mazeAbility
#' @seealso \code{\link{mazeDiff}}, \code{\link{np}}
#' @return An 'ab' class is created which will be used for other functions in the package.
#' @examples
#'  nodePosition <- np(rank=6,satPercent=0.5,seed=1)
#'  mazeAbility(nodePosition,dot=3, model="t2")



mazeAbility <- function(nodePosition,dot=2, model="t2"){

  if(model!="t1" && model !="t2" && model !="t3" && model != "t4"){
    stop("Please select either t1,t2,t3,t4.")
  }

#   if(exists("nodePosition")==FALSE){
#     stop("Please include an object for nodePosition")
#   }

  if("np" %in% class(nodePosition) == FALSE){
    stop("nodePosition must be calculated using the np function.")
  }
nodePosition
  rank<-nodePosition$rank

  #calculates all possible black node Routes
  (totalScore<- blackNodeRoutes(nodePosition))

  #table(totalScore) #frequently of path crossing varying series of connecting dots

  totalScores <- as.data.frame(totalScore)
  names(totalScores)[names(totalScores)=="Var1"] <- "Dot"

  # Just frequency of path crossing
  Frematrix<-matrix(totalScore)
  #Frematrix
  (maxsc<-length(Frematrix))

  #highest frequency of path crossing
  (mostcom<-max(Frematrix))
  (rowMostFreq <- which(grepl(mostcom,Frematrix[,1]))) #row highest frequency of path crossing

  #maxscore path freq ###
  (maxsc<-length(Frematrix))
  (maxPath <- Frematrix[maxsc]) #row maxscore path freq
  (maxscore <- as.numeric(totalScores[maxsc,1]))
maxsc

#   k <- 1
#   if(k == maxsc)
#   while(k  < maxsc ){
#     weightedScores <-log(mostcom/Frematrix)
#     k <- k + 1
#   }


if(model=="t1"){
  if(totalScores$Dot[1]==0){
    if (dot  > maxscore-1){ #-1 because 0 is a row in itself.
      stop(paste('Not a possible score since', dot, 'connected dots does not exist'))
    }

    scoreroutes<-totalScores[dot+1,1] #nodes starts from 0, not 1. Select rows
    Ability<-log(2^rank/Frematrix)
    result <- cbind(totalScores,Ability)
    userResult<- result[dot+1,]

    if(dot<as.numeric(as.character(totalScores[rowMostFreq,1]))){
      warning("Input dot is less than most frequent path")
    }

  }


  if(totalScores$Dot[1]==1){
    if (dot  > maxscore){
      stop(paste('Not a possible score since', dot, 'connected dots does not exist'))
    }
    scoreroutes<-totalScores[dot,1] #nodes starts from 0, not 1.
    Ability<-log(2^rank/Frematrix)
    result <- cbind(totalScores,Ability)
    userResult<- result[dot,]

    if(dot<as.numeric(as.character(totalScores[rowMostFreq,1]))){
      warning("Input dot is less than most frequent path")
    }

  }
}

if(model=="t2"){
if(totalScores$Dot[1]==0){

  if (dot  > maxscore-1){ #-1 because 0 is a row in itself.
    stop(paste('Not a possible score since', dot, 'connected dots does not exist'))
  }

        scoreroutes<-totalScores[dot+1,1] #nodes starts from 0, not 1.
        Ability<-log(mostcom/Frematrix)
        result <- cbind(totalScores,Ability)
        userResult<- result[dot+1,]

        if(dot<as.numeric(as.character(totalScores[rowMostFreq,1]))){
          warning("Input dot is less than most frequent path")
        }

}


if(totalScores$Dot[1]==1){
  if (dot  > maxscore){
    stop(paste('Not a possible score since', dot, 'connected dots does not exist'))
  }
  scoreroutes<-totalScores[dot,1] #nodes starts from 0, not 1.
  Ability<-log(mostcom/Frematrix)
  result <- cbind(totalScores,Ability)
  userResult<- result[dot,]

  if(dot<as.numeric(as.character(totalScores[rowMostFreq,1]))){
    warning("Input dot is less than most frequent path")
  }


  }
}
if(model=="t3"){
  if(totalScores$Dot[1]==0){
    if (dot  > maxscore-1){ #-1 because 0 is a row in itself.
      stop(paste('Not a possible score since', dot, 'connected dots does not exist'))
    }
    scoreroutes<-totalScores[dot+1,1] #nodes starts from 0, not 1.
    Ability<-log(2^rank * nodePosition$satPercent^4/Frematrix)
    result <- cbind(totalScores,Ability)
    userResult<- result[dot+1,]

    if(dot<as.numeric(as.character(totalScores[rowMostFreq,1]))){
      warning("Input dot is less than most frequent path")
    }

  }


  if(totalScores$Dot[1]==1){
    if (dot  > maxscore){
      stop(paste('Not a possible score since', dot, 'connected dots does not exist'))
    }
    scoreroutes<-totalScores[dot,1] #nodes starts from 0, not 1.
    Ability<-log(2^rank * nodePosition$satPercent^4/Frematrix)
    result <- cbind(totalScores,Ability)
    userResult<- result[dot,]

    if(dot<as.numeric(as.character(totalScores[rowMostFreq,1]))){
      warning("Input dot is less than most frequent path")
    }

  }
}
if(model=="t4"){
  if(totalScores$Dot[1]==0){
    if (dot  > maxscore-1){ #-1 because 0 is a row in itself.
      stop(paste('Not a possible score since', dot, 'connected dots does not exist'))
    }
    scoreroutes<-totalScores[dot+1,1] #nodes starts from 0, not 1.
    Ability<-log(mostcom*nodePosition$satPercent^4/Frematrix)
    result <- cbind(totalScores,Ability)
    userResult<- result[dot+1,]

    if(dot<as.numeric(as.character(totalScores[rowMostFreq,1]))){
      warning("Input dot is less than most frequent path")
    }

  }


  if(totalScores$Dot[1]==1){
    if (dot  > maxscore){
      stop(paste('Not a possible score since', dot, 'connected dots does not exist'))
    }
    scoreroutes<-totalScores[dot,1] #nodes starts from 0, not 1.
    Ability<-log(mostcom*nodePosition$satPercent^4/Frematrix)
    result <- cbind(totalScores,Ability)
    userResult<- result[dot,]

    if(dot<as.numeric(as.character(totalScores[rowMostFreq,1]))){
      warning("Input dot is less than most frequent path")
    }


  }
}


mostFreq<- totalScores[rowMostFreq,]
rownames(mostFreq) <- 1

ability <- list(model=model,
                mostFreq=mostFreq,
                dot=dot,
                result=result,
                userResult=userResult)
class(ability) <- "ab"

      return(ability)
}


  #nodePosition <- np(rank=12,satPercent=0.3,seed=1)
  #mazeAbility(nodePosition,dot=1,model="t3")


