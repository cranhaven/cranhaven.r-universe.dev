knots.start <- function(penden.env) {
  if(get("base",penden.env)=="B-spline") {
    assign("knots",knots.transform(get("d",penden.env),get("alpha",penden.env),get("symmetric",penden.env)),penden.env)
    q <- get("q",penden.env)
    
    if(q==1) {
      x.help <- c()
      for(j in 1:(length(get("knots",penden.env))-1)) {
         x.help <- c(x.help,(get("knots",penden.env)[j+1]-get("knots",penden.env)[j])/2+get("knots",penden.env)[j])
      }
      assign("knots.help",sort(c(x.help,get("knots",penden.env))),penden.env)
    }

    if(q==1) {
      assign("k.order",knots.order(penden.env),penden.env)
      assign("knots.t",get("knots",penden.env),penden.env)
    }
    #Knoten nach hierarchischer Basis anordnen
    
    assign("tilde.Psi.knots.d",hierarch.bs(get("knots.t",penden.env), d = get("d",penden.env),plot.bsp=get("plot.bsp",penden.env),typ=3,penden.env,int=FALSE)$B.tilde,penden.env)
    assign("tilde.Psi.knots.d.help",hierarch.bs(get("knots.help",penden.env), d = (get("d",penden.env)),plot.bsp=get("plot.bsp",penden.env),typ=3,penden.env,int=FALSE)$B.tilde,penden.env)
    assign("knots.t",get("knots.t",penden.env)[get("k.order",penden.env)],penden.env)
   ###################

   dimension<-  c(rep(0,get("q",penden.env)+1),rep(1:get("d",penden.env),2**(0:(get("d",penden.env)-1)))) 
   DIMENSION <- dimension
   Index.basis <- matrix(1:get("ddb",penden.env))
   index.sparse <- DIMENSION <= get("D",penden.env)
   Index.basis.D <- matrix(Index.basis[index.sparse,])
   DIMENSION <- DIMENSION[index.sparse]
   for ( j in 2:2)
     {
        DIMENSION.j <-  kronecker(matrix(1,get("ddb",penden.env),1),DIMENSION) + kronecker( dimension, matrix(1, length(DIMENSION),1))
        Index.basis.plus.1 <- matrix(NA, dim(Index.basis.D)[1] * get("ddb",penden.env) , j)
        Index.basis.plus.1[,j] <- kronecker(matrix(1:get("ddb",penden.env)), matrix(1,dim(Index.basis.D)[1],1))
        Index.basis.plus.1[, 1:(j-1)] <-  kronecker(matrix(1, get("ddb",penden.env),1),Index.basis.D)
        index.sparse <- DIMENSION.j <= get("D",penden.env)
        Index.basis.D <- Index.basis.plus.1[index.sparse,]
        DIMENSION <- DIMENSION.j[index.sparse]
      }
   assign("Index.basis.D.p2",Index.basis.D,penden.env)

   if(get("cond",penden.env)&get("D.struc",penden.env)==1) {
     knots.d2 <-cbind(get("knots.t", penden.env)[Index.basis.D[,1]],get("knots.t", penden.env)[Index.basis.D[,2]])    
     tilde.knots.d <-  array(NA, dim=c(dim(knots.d2)[1],length(get("knots.t",penden.env)),2))
 
     for (j in 1:2)
       {
         tilde.knots.d[,,j] <-  hierarch.bs(knots.d2[,j], d = get("d",penden.env), plot.bsp = FALSE,typ=3,penden.env,int=FALSE)$B.tilde
       }
     assign("tilde.knots.d",tilde.knots.d,penden.env)
     assign("tilde.PSI.knots.D",tilde.knots.d[,Index.basis.D[,1],1],penden.env)
     for (j in 2:2)
      {
        assign("tilde.PSI.knots.D",get("tilde.PSI.knots.D",penden.env) * get("tilde.knots.d",penden.env)[,Index.basis.D[,j],j],penden.env)
      }
   }
  }

  if(get("base",penden.env)=="Bernstein") {
    assign("knots.t",get("knots",penden.env),penden.env)
    assign("tilde.Psi.knots.d",hierarch.bs(get("knots.t",penden.env), d = get("d",penden.env),plot.bsp=get("plot.bsp",penden.env),typ=3,penden.env,int=FALSE)$B.tilde,penden.env)
  }
}
