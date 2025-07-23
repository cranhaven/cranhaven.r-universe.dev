digits <- function(x){
  # calculate the digits of a number
  if(x >= 1){pos <- 0    
  }else{
    x <- as.character(x)
    x <- substr(x,3,nchar(x))
    xsplit <- substring(x, seq(1,nchar(x),1), seq(1,nchar(x),1))
    pos <- which(xsplit != "0")
  }
  return(pos[1])
}