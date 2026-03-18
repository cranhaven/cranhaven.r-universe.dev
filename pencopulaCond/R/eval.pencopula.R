eval.pencopula <- function(val,d,D,ck.val,p,ddb,q) {
  if(!is.matrix(val)) {
    if(is.data.frame(val)) val <- as.matrix(val) else stop("val has to be a data.frame or a matrix")
  }
  penden.env<-new.env()
  assign("base","B-spline",penden.env)
  assign("q",q,penden.env)
  assign("alpha",0,penden.env)
  assign("symmetric",TRUE,penden.env)
  assign("ddb",ddb,penden.env)
  assign("margin.normal",FALSE,penden.env)
  tilde.Psi.d <-  array(NA, dim=c(dim(val)[1],ddb,p))
  dimension <- c(rep(0,q+1),rep(1:d,2**(0:(d-1))))
  DIMENSION <- dimension
  Index.basis <- matrix(1:ddb)
  index.sparse <- DIMENSION <= D
  Index.basis.D <- matrix(Index.basis[index.sparse,])
  DIMENSION <- DIMENSION[index.sparse]
    
  for ( j in 2:p)
    {
    DIMENSION.j <-  kronecker(matrix(1,ddb,1),DIMENSION) + kronecker( dimension, matrix(1, length(DIMENSION),1))
    Index.basis.plus.1 <- matrix(NA, dim(Index.basis.D)[1] * ddb , j)
    Index.basis.plus.1[,j] <- kronecker(matrix(1:ddb), matrix(1,dim(Index.basis.D)[1],1))
    Index.basis.plus.1[, 1:(j-1)] <-  kronecker(matrix(1, ddb,1),Index.basis.D)
    index.sparse <- DIMENSION.j <= D
    Index.basis.D <- Index.basis.plus.1[index.sparse,]
    DIMENSION <- DIMENSION.j[index.sparse]
  }

 for (j in 1:p)
    {
      tilde.Psi.d[,,j] <-  hierarch.bs(val[,j], d = d, plot.bsp = FALSE,typ=3,penden.env,int=FALSE)$B.tilde
    }
  assign("tilde.Psi.d",tilde.Psi.d,penden.env)
  rm(tilde.Psi.d)
  assign("tilde.PSI.d.D",get("tilde.Psi.d",penden.env)[,Index.basis.D[,1],1],penden.env)
  for (j in 2:p)
    {
      assign("tilde.PSI.d.D",get("tilde.PSI.d.D",penden.env) * get("tilde.Psi.d",penden.env)[,Index.basis.D[,j],j],penden.env)
    }
  val<-get("tilde.PSI.d.D",penden.env)%*%ck.val
  val[val<0]<-0
  return(val)
}
