# function to simulate a coin flip
#' @import data.table



flip.renaming <- function(my_df, coins){
  for(j in 1:coins){
    my_df[, j] <- ifelse(my_df[, j, with = FALSE] == 1, "H", "T")
  }
  my_df
}


#' @export
coin.flip <- function(coins = 5, flips = 100, weights = c(0.5, 0.5), getExact){
  if(!missing(getExact)){
    e2 <-tryCatch(
      {
        !(length(getExact)<= coins)
      },
      error = function(){
        return(TRUE)
      }
    )
    if(!e2){
      # case of exact matches with getExact
      coinExact <- ifelse(getExact == "H", 1, 2)
      flips_table <- dice.combinations(faces = 2, dice = coins, rolls = flips, weights, getExact = coinExact, toSum = FALSE)
      flips_table <- flip.renaming(flips_table, coins)
      if(coins > 1){
        colnames(flips_table)[1:coins] <- paste0("value_", 1:coins)
      }
      flips_table <- flips_table[flips_table[, .I[is.exact(flips_table, coins, getExact)==TRUE]]]
    } else {
      stop("The number of elements in getExact must be at most the number of dice, hence length(getExact) <= dice")
    }
  } else {
    flips_table <- dice.combinations(faces = 2, dice = coins, rolls = flips, weights, toSum = FALSE)
    flips_table <- flip.renaming(flips_table, coins)
    if(coins > 1){
      colnames(flips_table)[1:coins] <- paste0("value_", 1:coins)
    }
  }

  flips_table
}

