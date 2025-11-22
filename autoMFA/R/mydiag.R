mydiag <- function(d){
  if(length(d)==1 && d%%1!=0){return(d)}else{
    return(diag(d))
  }

}
