penalty.matrix <- function(penden.env,temp=FALSE) {
  # Bestimmung der Penalisierungsmatrizen
# Zunaechst brauchen wir die Transformationsmatrix AA
 
if(get("base",penden.env)=="B-spline") {
  d <-  get("d",penden.env)
  alpha <- get("alpha",penden.env)
  q <- get("q",penden.env)
  ddb <- get("ddb",penden.env)
  dd <- get("dd",penden.env)
  DD <- get("DD",penden.env)
  Index.basis.D <- get("Index.basis.D",penden.env)
  p <- get("p",penden.env)
  
  if (q ==0)
    {
      x <- (1:2**d)/(2**d)-1/(2**(d+1))
  }
  if (q==1)
    {
      x <- (0:2**d)/(2**d)
    }

  tilde.Psi.A.d <- hierarch.bs(x, d = d, plot.bsp=FALSE,typ=3,penden.env=penden.env)$B.tilde

  # Das ist die Hierarchische B-spline Basis

  knots.val <- list()
  knots.val$val <- seq(0,1,length=dd)
 
  obj <- my.bspline(h=1/(ddb-1),q=q+1,y=x, knots=knots.val$val,K=length(knots.val$val),typ=2,plot.bsp=FALSE)
  Psi.A.d <- obj$base.den
  C <- obj$base.den

  AA <- solve(Psi.A.d) %*% tilde.Psi.A.d
  assign("AA",AA,penden.env)

# AA transformiert die normale B spline Basis
# in die hierarchische Form (Matrix A in Deinen Aufzeichnungen) 

# Erstellung der Differenzenmatrix
# Differenzenordnung = B-spline Polynom
  dd.A <- dim(Psi.A.d)[1]

  pen.order <- get("pen.order",penden.env)
  
  if(pen.order==1) L <- diag(dd.A)[1:(dd.A-1),] - diag(dd.A)[2:dd.A,]
  if(pen.order==2) L <- diag(dd.A)[1:(dd.A-2),] - 2*diag(dd.A)[2:(dd.A-1),] + diag(dd.A)[3:dd.A,]
  if(pen.order==3) L <- diag(dd.A)[1:(dd.A-3),] - 3*diag(dd.A)[2:(dd.A-2),] + 3*diag(dd.A)[3:(dd.A-1),] +diag(dd.A)[4:dd.A,]
  if(pen.order==4) L <- diag(dd.A)[1:(dd.A-4),] - 4*diag(dd.A)[2:(dd.A-3),] + 6*diag(dd.A)[3:(dd.A-2),] -4*diag(dd.A)[4:(dd.A-1),]+diag(dd.A)[5:dd.A,]

  dd.A<-dim(L)[1]

  D.mat <- crossprod(L)

# Penalisierungsmatrix fuer NORMALE B-splines

  ADA <- t(C%*%AA) %*% D.mat %*% (C%*%AA)

  assign("ADA",ADA,penden.env)

# Penalisierungsmatrix fuer hierarchische B-splines

  AIA <- crossprod(C%*%AA)

  assign("AIA",AIA,penden.env)

# Erstellung einer Penalisierungsmatrix mit Koeffizienten lambda

  DDD3 <- DDD3.pen <-array(NA, c(DD,DD, p))
  for (k in 1:p)
    {
      l.ind <- (1:p)[-k]
      i <- 1:DD
      j <- 1:DD
      DDD3[i,j,k] <- ADA[Index.basis.D[i,k], Index.basis.D[j,k]]
      for (l in l.ind)   DDD3[i,j,k] <- DDD3[i,j,k] * AIA[Index.basis.D[i,l], Index.basis.D[j,l]]
    }
  DDD.sum <- DDD3[,,1]
  for(j in 2:p) DDD.sum <- DDD.sum+DDD3[,,j]
  assign("DDD.sum",DDD.sum,penden.env)
  assign("DDD",DDD3,penden.env)
  assign("eigen.pen.mat",help2 <- eigen(DDD.sum),penden.env)
  assign("index.eigen.pen.mat",index <- which(help2$values>1e-08),penden.env)
  assign("Utilde.eigen.pen.mat",help2$vectors[,index],penden.env)
}
}
