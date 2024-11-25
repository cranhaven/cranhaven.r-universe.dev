#Compute the current value of the stringy scagnostic for a set of points

getStringy <- function(Ind){
  RS <- rowSums(Ind)
  V1 <- length(which(RS == 1))
  V2 <- length(which(RS == 2))
  V <- nrow(Ind)
  
  CurrentStringy <- (V2/(V-V1))^3
  
  return(list(CS=CurrentStringy,RS=RS))
}