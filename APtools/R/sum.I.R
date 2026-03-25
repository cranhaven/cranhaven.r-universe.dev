sum.I <- function(yy,FUN,Yi,Vi=NULL){
  if (FUN=="=") {
    out = rank(c(Yi,yy),ties.method="f")[-c(1:length(Yi))]-rank(c(yy,Yi),ties.method="f")[1:length(yy)]
    return(out)
  } else {
    if (FUN=="<"|FUN==">=") { yy <- -yy; Yi <- -Yi}
    # for each distinct ordered failure time t[j], number of Xi < t[j]
    pos <- rank(c(yy,Yi),ties.method='f')[1:length(yy)]-rank(yy,ties.method='f')
    if (substring(FUN,2,2)=="=") pos <- length(Yi)-pos # number of Xi>= t[j]
    if (!is.null(Vi)) {
      ## if FUN contains '=', tmpind is the order of decending
      if(substring(FUN,2,2)=="=") tmpind <- order(-Yi) else  tmpind <- order(Yi)
      ##Vi <- cumsum2(as.matrix(Vi)[tmpind,])
      Vi <- apply(as.matrix(Vi)[tmpind,,drop=F],2,cumsum)
      return(rbind(0,Vi)[pos+1,])
    } else return(pos)
  }
}

