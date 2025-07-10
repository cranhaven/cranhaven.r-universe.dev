# Uniformidade de germinacao

UnifG <- function(time, nger) {

  result <- T90(time,nger) - T10(time,nger)

  return(result)
}
