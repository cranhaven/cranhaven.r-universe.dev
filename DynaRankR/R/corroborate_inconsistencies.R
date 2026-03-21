#' @importFrom rlang .data
corroborate_inconsistencies <- function(intx.matrix, period, interactions, periods){
  ####Corroborate inconsistencies: Must be supported by future observations
  all.is <- list_inconsistencies(intx.matrix)
  if(is.null(all.is)){invisible(intx.matrix)}
  period.index <- which(periods == period)
  step.forward <- 1
  while(length(all.is) > 0){
    current.i <- all.is[1:2]
    current.i <- row.names(intx.matrix)[current.i[order(current.i)]]
    ##if reached the end of the study period, remove inconsistancy and move on to next inconsistancy
    if(step.forward + period.index > length(periods)){
      intx.matrix[current.i[1], current.i[2]] <- 0
      intx.matrix[current.i[2], current.i[1]] <- 0
      all.is <- all.is[-1:-2]
      step.forward <- 1
    }else{
      check.intx <- dplyr::filter(interactions, .data$winner %in% current.i, .data$loser %in% current.i, .data$period == periods[period.index + step.forward])[,c('winner','loser')]
      if(!nrow(check.intx)){
        step.forward <- step.forward + 1
      }else if(nrow(check.intx[check.intx$winner == current.i[1] & check.intx$loser == current.i[2],]) == nrow(check.intx[check.intx$winner == current.i[2] & check.intx$loser == current.i[1],])){
        ##If next period is a tie, keep checking subsequent periods
        step.forward <- step.forward + 1
      }else if(nrow(check.intx[check.intx$winner == current.i[1] & check.intx$loser == current.i[2],]) < nrow(check.intx[check.intx$winner == current.i[2] & check.intx$loser == current.i[1],])){
        ###if inconsistancy is supported by data from subsequent period, leave it and move on to next inconsistancy
        all.is <- all.is[-1:-2]
        step.forward <- 1
      }else if(nrow(check.intx[check.intx$winner == current.i[1] & check.intx$loser == current.i[2],]) > nrow(check.intx[check.intx$winner == current.i[2] & check.intx$loser == current.i[1],])){
        ###if inconsistancy is contradicted by data from subsequent period, remove it 
        intx.matrix[current.i[1], current.i[2]] <- 0
        intx.matrix[current.i[2], current.i[1]] <- 0
        all.is <- all.is[-1:-2]
        step.forward <- 1
      }
    }
  }
  invisible(intx.matrix)
}
