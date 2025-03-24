expandR <-
function(R, n_i){
  COL <- NULL
  for(j in 1:nrow(R)){
    ROW <- NULL
    for(i in 1:ncol(R)){
      ROW <- cbind(ROW, diag(R[i, j], n_i))
    }
    COL <- rbind(COL, ROW)
  }
  COL
}
