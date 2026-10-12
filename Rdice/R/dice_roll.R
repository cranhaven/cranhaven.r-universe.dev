# function to simulate a die roll
#' @import data.table

values.formatting <- function(values, dice){
  if(dice > 1){
    values <- t(values)
    colnames(values) <- paste0("die_", 1:dice)
  }
  values <- as.data.table(values)
  return(values)
}


frequency.counts <- function(values){
  freq_table <- as.data.table(table(values))
  freq_table$freq <- freq_table$N/sum(freq_table$N)
  freq_table <- freq_table[N>0]
  return(freq_table)
}


sum.counts <- function(values){
  values$sum <- apply(values, 1, sum)
  sum_table  <- values[, .N, by = sum]
  sum_table$freq    <- sum_table$N/sum(sum_table$N)
  setorder(sum_table, sum)
  sum_table$cum_sum <- cumsum(sum_table$freq)
  return(sum_table)
}


#' @export
dice.roll <- function(faces = 6, dice = 2, rolls = 5, weights){
  if(missing(weights)){
    # case of a fair die
    values <- replicate(rolls, sample(1:faces, dice, replace = TRUE, prob = NULL))
    values <- values.formatting(values, dice)
    freq_table <- frequency.counts(values)
    sum_table  <- sum.counts(values)
    exp_value_sum  <- sum(sum_table$sum*sum_table$freq)
  }
   else {
    e <-tryCatch(
      {
        !(length(weights)==faces & sum(weights)==1)
      },
      error = function(){
        return(TRUE)
      }
    )
    if(!e){
      # case of an unfair die
      values <- replicate(rolls, sample(1:faces, dice, replace = TRUE, prob = weights))
      values <- values.formatting(values, dice)
      freq_table <- frequency.counts(values)
      sum_table  <- sum.counts(values)
      exp_value_sum  <- sum(sum_table$sum*sum_table$freq)
    } else {
      stop("The vector of weights must be of the same lenght as the number of faces. Also, the weights must sum up to 1.")
    }
  }
  return_object <-list(results = values
                       ,frequencies = freq_table
                       ,sums_freq = sum_table
                       ,exp_value_sum = exp_value_sum
                       ,rolls = rolls
                       ,dice = dice)

  return_object$call <- match.call()
  class(return_object) <- "diceRoll"
  return(return_object)
}


#' @export
print.diceRoll <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\n Results after", x$rolls, "rolls of", x$dice, "dice:\n")
  print(x$results)
  cat("\n Frequency table for each occurrency:\n")
  print(x$frequencies)
  cat("\n Frequency table of the sums:\n")
  print(x$sums_freq)
  cat("\n Expectation value:", x$exp_value_sum)
}

