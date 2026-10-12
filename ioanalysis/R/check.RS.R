check.RS <- function(io){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  RS_label <- io$RS_label
  regions <- unique(RS_label[, 1])
  RS.1 <- matrix(c(RS_label[RS_label[, 1] == regions[1],]), ncol = 2)
  the.same <- "yes"
  for(r in 2:length(regions)){
    i <- which(RS_label[, 1] == regions[r])
    if(!all(RS.1[, 2] == RS_label[i, 2])){
      the.same <- "no"
    }
  }
  if(the.same == "yes"){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
