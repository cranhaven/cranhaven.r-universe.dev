#'  Title addRate
#'
#' @param dat matrix
#' @param lab_rate rate that corresponds to the cycles of the analysis performed
#' @param lab_cycles number of the cycles of the analysis performed
#'
#' @return the input matrix with one added column with the values of the rate of the cycles performed
#' @export
#'
#' @description add to the thermogram the value of rate(s) for each cycle(s) as provided by the user

addRate <- function(dat, lab_rate, lab_cycles) {
  id_cycle<- NULL
  dat$rate <- 0
  id_rate <- "rate"
  for (n in 1:length(lab_cycles)) {
    dat[id_cycle == lab_cycles[n], (id_rate) := lab_rate[n]]
    print(dat)
  }
  return(dat)
}
