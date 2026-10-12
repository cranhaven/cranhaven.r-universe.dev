
# assigns global variable for package check
globalVariables(c("N", "freq", "winner"))



# produces a list of Z dice with N faces
dice_list.generator <- function(dice, faces, max_value){
  dice_list <- vector(mode= "numeric", length = 0)
  for(k in 1:dice){
    dice_list <- cbind(dice_list, sample(0:max_value, faces, replace = TRUE))
  }
  dice_list <- cbind(dice_list, dice_list[,1])
  dice_list
}


# checks if the first die wins the second
is.winner <- function(first, second, prob, error){
  comb <- as.data.table(expand.grid(first, second))
  comb$winner <- ifelse(comb$Var1 > comb$Var2, "first", "second")
  results <- comb[, .N, by = winner][, freq:= N/sum(N)]
  if(!missing(prob)){
    return(results[winner == "first", freq > prob-abs(error) && freq<prob+abs(error)])
  } else {
    return(length(results[winner == "first", freq]) > 0 && results[winner == "first", freq] > results[winner == "second", freq])
  }
}


# checks if one set of randomly generated dice is Efron's
nonTransitive.check <- function(dice, faces, max_value, prob, error){
    # check for arguments
    if(missing(prob)){
      if(dice < 2 || faces < dice || dice%%1!=0 || faces%%1!=0){
        stop("Please check the validity of the arguments that you have assigned. Probabilities must be 0 <= P <= 1, dice < 2 and faces < dice must be integers.")
      }
      dice_list <- dice_list.generator(dice, faces, max_value)

      truth <- vector(mode="logical", length=0)
      for(j in 1:dice){
        truth <- cbind(truth, is.winner(dice_list[,j], dice_list[,j+1], error = error))
      }
      if(is.na(all(truth)) || !all(truth)){
        values <- NULL
      } else {
        dice_list <- dice_list[, -(dice+1)]
        values <- as.data.table(dice_list)
        colnames(values) <- paste0("die_", 1:dice)
      }
      values
    } else {
      if(dice < 2 || faces < dice || prob < 0 || prob > 1 || dice%%1!=0 || faces%%1!=0){
        stop("Please check the validity of the arguments that you have assigned. Probabilities must be 0 <= P <= 1, dice < 2 and faces < dice must be integers.")
      }

      dice_list <- dice_list.generator(dice, faces, max_value)
      #print(dice_list)

      truth <- vector(mode="logical", length=0)
      for(j in 1:dice){
        #print(dice_list[,j])
        #print(dice_list[,j+1])
        truth <- cbind(truth, is.winner(dice_list[,j], dice_list[,j+1], prob = prob, error = error))
        #print(truth)
        #print(all(truth))
      }
      if(is.na(all(truth)) || !all(truth)){
        values <- NULL
      } else {
        dice_list <- dice_list[, -(dice+1)]
        values <- as.data.table(dice_list)
        colnames(values) <- paste0("die_", 1:dice)
      }
      values
    }
}


# generated Efron's dice
#' @export
nonTransitive.generator <- function(dice, faces, max_value = faces, prob, error = 0.001){
  start_time <- proc.time()
  if(missing(prob)){
    repeat{
      z <- nonTransitive.check(dice, faces, max_value, error = error)
      delta_time <- proc.time() - start_time
      if(!is.null(z)) break
    }
    return(z)
  } else {
    repeat{
      z <- nonTransitive.check(dice, faces, max_value, prob = prob, error = error)
      delta_time <- proc.time() - start_time
      if(!is.null(z)) break
    }
    return(z)
  }
}


# checks if a set of dice is Efron's
#' @export
is.nonTransitive <- function(df, prob){
  truth <- vector(mode="logical", length=0)
  for(j in 1:(dim(df)[2]-1)){
    truth <- cbind(truth, is.winner(df[,j], df[,j+1], prob = prob, error = 0.001))
  }
  truth <- cbind(truth, is.winner(df[,dim(df)[2]], df[,1], prob = prob, error = 0.001))
  return(all(truth))
}


