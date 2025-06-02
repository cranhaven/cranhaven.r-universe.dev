#' @importFrom stats runif
solqp<- function(Q,A,b,c,x,verbose=FALSE,toler=1e-5,beta=0.8){

# Function called by cecm.
# This function solves quadratic program in standard form:
#
#     minimize    0.5*(x'*Q*x)+c'*x
#     subject to  A*x=b, x>=0.

#  Input
#      Q: Sparse symmetric objective matrix.
#      A: Sparse constraint left-hand matrix
#      b: constraint right-hand column vector
#      c: objective column vector
#      toler: relative stopping tolerance: the objective value close to
#             the local optimal one in the range of tolerance.

#      beta : step size: 0 < beta < 1.

# Output
#     x: (local) optimal solution
#     y: optimal dual solution (Lagrangien multiplier)
#     obhis : objective value history vs iterations

#    This program is the implementation of the interior ellipsoidal trust
#  region and barrier function algorithm with dual solution updating
#  technique in the standard QP form.

#  Technical Reference

#  Y. Ye, "An extension of Karmarkar's algorithm and the trust region method
#         for convex quadratic programming," in Progress in Mathematical
#         Programming (N. Megiddo ed.), Springer-Verlag, NY (1989) 49-63.

#  Y. Ye, "On affine-scaling algorithm for nonconvex quadratic programming,"
#         Math. Programming 56 (1992) 285-300.

#  Comment: Each iteration we solve a linear KKT system like

#  ( Q+mu X^{-2}   A^T )(dx) = c'
#  (     A          0  )(dy) = 0

#  where X = diag(x)  which is a positive diagonal matrix.

  m<-nrow(A)
  n<-ncol(A)
  eps<-.Machine$double.eps

  ob=0.5*(t(x)%*% Q %*% x)+c%*%x

  alpha <- 0.9
  comp<- runif(n)
  comp<-solve(rbind(cbind(Diagonal(n),t(A)),cbind(A,Matrix(0,m,m))),
              c(as.vector(comp), double(m)))
  comp<-comp[1:n]
  nora<-min(comp/x)
  if(nora < 0) nora <- -.01/nora else{
    nora <- max(comp/x)
    if(nora == 0){
      print('The problem has a unique feasible point')
      return()
      }
    nora <- .01/nora
    }
  x <- x + nora*comp
  obvalue<- as.numeric(t(x) %*% (Q%*%x)/2+c%*%x)
  obhis<-obvalue
  lower <- -Inf
  zhis <- lower
  gap<-1
  lamda<-max(1, abs(obvalue)/sqrt(sqrt(n)))
  iter<-0

  while(gap >= toler){
    iter<-iter+1

#-------------------------
#   spphase2

    lamda<-(1-beta)*lamda
    go<-0
    gg <- Q%*%x+c
    XX <- Diagonal(x=x)
    AA <- A%*%XX
    XX <- XX%*%Q%*%XX


    #  Repeatly solve an ellipsoid constrained QP problem by solving a linear
    #  system equation until find a positive solution.


    while(go <= 0){
      u<-solve(rbind(cbind(XX+lamda*Diagonal(n),t(AA)),cbind(AA,Matrix(0,m,m))),
               c(-as.vector(x*gg), double(m)))
      xx<-x+x*u[1:n]
      go<-min(xx)
      if(go > 0){
        ob<-as.numeric(t(xx)%*%Q%*%xx/2+c%*%xx)
        go <- min(c(go,obvalue-ob+eps))
        }
      lamda<-2*lamda
      if(lamda >= (1+abs(obvalue))/toler){
         #disp('The problem seems unbounded.')
          y=-u(n+1:n+m)
         return()
         }
      }

       y<- -u[(n+1):(n+m)]
       u<-u[1:n]
       nora <- min(u);
       if(nora < 0) nora=-alpha/nora else{
         if(nora == 0) nora=alpha else nora=Inf
         }

       u <-  x*u
       w1 <- as.numeric(t(u)%*%Q%*%u)
       w2 <- as.numeric(-t(u)%*%gg)
       if(w1 > 0) nora=min(w2/w1,nora)
       if(nora == Inf) ob <- -Inf else{
         x <-x+nora*u
         ob=as.numeric(t(x)%*%Q%*%x/2+c%*%x)
         }

  #  This is the Phase 2 procedure called by SPSOLQP.
    if(ob == -Inf){
      gap <- 0
      print('The problem is unbounded.')
      return()
    }else{
      obhis<-c(obhis,ob)
      comp<-Q%*%x+c-t(A)%*%y
      if(min(comp)>=0){
        zhis=c(zhis,ob-t(x)%*%comp)
        lower<-zhis[iter+1]
        gap<-(ob-lower)/(1+abs(ob))
        obvalue<-ob
      }else{
        zhis=c(zhis,zhis[iter])
        lower=zhis[iter+1]
        gap=(obvalue-ob)/(1+abs(ob))
        obvalue<-ob
      }
    }

    if(iter>200){
      print(c(gap,toler))
    }
  }

  if(verbose) print('A (local) optimal solution is found.')

  return(list(x=x,y=y,obhis=obhis))
}


