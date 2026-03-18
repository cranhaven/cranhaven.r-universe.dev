add.VineMatrix <- function(obj,help.env,level) {
  dez <- get("dez",help.env)
  VineMatrix <- get("VineMatrix",help.env)
  p <- get("p",help.env)
  size.obj <- length(obj)
  for(i in 1:size.obj) {
     tmp <- obj[[i]]
     val <- rep(NA,p+1)
     val[1:(2+length(tmp$D))] <- c(tmp$j1,tmp$j2,tmp$D)
     if(dez==10) val[p+1]<-as.numeric(paste(level,".",i,sep=""))
     if(dez==100) {
      if(i<10) val[p+1]<-as.numeric(paste(level,".0",i,sep=""))
      if(i>9) val[p+1]<-as.numeric(paste(level,".",i,sep=""))
     }
     VineMatrix <- rbind(VineMatrix,val)
  }
  rownames(VineMatrix) <- rep("",dim(VineMatrix)[1])
  assign("VineMatrix",VineMatrix,help.env)
}
