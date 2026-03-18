hierarchbs.cond.cop<-function(data,coef,intp=c(TRUE,FALSE),d,D,p,data.cal=NULL,cond=FALSE,q) {
  obj1<-new.env()
  assign("adapt.grid",FALSE,obj1)
  assign("cond",cond,obj1)
  assign("plot.bsp",plot.bsp<-FALSE,obj1)
  assign("symmetric",TRUE,obj1)
  assign("base","B-spline",obj1)
  assign("q",q,obj1)
  #assign("Y",data,obj1)
  assign("p",p,obj1)
  assign("n",dim(data)[1],obj1)
  assign("d",d,obj1)
  assign("D",D,obj1)
  assign("alpha",0,obj1)
  assign("margin.normal",FALSE,obj1)
  dd <- (2**d)+1 # Anzahl Knoten
  ddb <- dd+(q-1) # Anzahl Basisfunktionen
  assign("ddb",ddb,obj1)
  DIMENSION <- dimension <- c(rep(0,q+1),rep(1:d,2**(0:(d-1))))
    Index.basis <- matrix(1:ddb)
    index.sparse <- DIMENSION <= D
    Index.basis.D <- matrix(Index.basis[index.sparse,])
    DIMENSION <- DIMENSION[index.sparse]
    
    for ( j in 2:p)
      {
                                        #print(j)
        DIMENSION.j <-  kronecker(matrix(1,ddb,1),DIMENSION) + kronecker( dimension, matrix(1, length(DIMENSION),1))
        Index.basis.plus.1 <- matrix(NA, dim(Index.basis.D)[1] * ddb , j)
        Index.basis.plus.1[,j] <- kronecker(matrix(1:ddb), matrix(1,dim(Index.basis.D)[1],1))
        Index.basis.plus.1[, 1:(j-1)] <-  kronecker(matrix(1, ddb,1),Index.basis.D)
        index.sparse <- DIMENSION.j <= D
        Index.basis.D <- Index.basis.plus.1[index.sparse,]
        DIMENSION <- DIMENSION.j[index.sparse]
      }
    DD <- dim(Index.basis.D)[1] # Dimension of sparse grid basis
    assign("DD",DD,obj1) # DD Anzahl Koeffizienten
  assign("Index.basis.D",Index.basis.D,obj1)

  assign("knots",knots.transform(d=d,alpha=0,symmetric=TRUE),obj1)
    q <- get("q",obj1)
    
    if(q==1) {
      x.help <- c()
      for(j in 1:(length(get("knots",obj1))-1)) {
         x.help <- c(x.help,(get("knots",obj1)[j+1]-get("knots",obj1)[j])/2+get("knots",obj1)[j])
      }
      assign("knots.help",sort(c(x.help,get("knots",obj1))),obj1)
    }
    if(q==2) {
      x.help <- x.help2<-c()
      for(j in 1:(length(get("knots",obj1))-1)) {
        x.help2 <- c(x.help2,(get("knots",obj1)[j+1]-get("knots",obj1)[j])/3+get("knots",obj1)[j],2*(get("knots",obj1)[j+1]-get("knots",obj1)[j])/3+get("knots",obj1)[j])
      }
      for(j in 1:(length(get("knots",obj1))-1)) {
        x.help <- c(x.help,(get("knots",obj1)[j+1]-get("knots",obj1)[j])/2+get("knots",obj1)[j])
      }
      assign("knots.help",sort(c(x.help,x.help2,get("knots",obj1))),obj1)
    }
    assign("tilde.Psi.knots.d.help",hierarch.bs(get("knots.help",obj1), d = (get("d",obj1)),plot.bsp=FALSE,typ=3,obj1,int=FALSE)$B.tilde,obj1)

if(is.null(data.cal)) {
  tilde.Psi.d <-  array(NA, dim=c(get("n",obj1),ddb=ddb,p=p))
   for (j in 1:p)
    {
      if(!intp[j]) tilde.Psi.d[,,j] <-  hierarch.bs(data[,j], d = d, plot.bsp = FALSE,typ=3,penden.env=obj1,int=FALSE)$B.tilde
      if(intp[j]) tilde.Psi.d[,,j] <-  hierarch.bs(data[,j], d = d, plot.bsp = FALSE,typ=3,penden.env=obj1,int=TRUE)$int.B.tilde
    }
  }
else {
  tilde.Psi.d <-  array(NA, dim=c(dim(data.cal)[1],ddb=ddb,p=p))
   for (j in 1:p)
    {
      if(!intp[j]) tilde.Psi.d[,,j] <-  hierarch.bs(data.cal[,j], d = d, plot.bsp = FALSE,typ=3,penden.env=obj1,int=FALSE)$B.tilde
if(intp[j]) tilde.Psi.d[,,j] <-  hierarch.bs(data.cal[,j], d = d, plot.bsp = FALSE,typ=3,penden.env=obj1,int=TRUE)$int.B.tilde
    }
  }
tilde.PSI.d.D<-tilde.Psi.d[,Index.basis.D[,1],1]
for (j in 2:p)
    {
      tilde.PSI.d.D<-tilde.PSI.d.D*tilde.Psi.d[,Index.basis.D[,j],j]
    }
rm(obj1)
val<-tilde.PSI.d.D%*%coef
#val<-round(tilde.PSI.d.D%*%coef,16)
val[val<0]<-0
if(any(intp)) val[val>1]<-1

return(val)
}
