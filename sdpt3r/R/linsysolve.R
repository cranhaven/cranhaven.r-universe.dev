linsysolve <- function(par, schur, UU, Afree, EE, rhs){
  
  spdensity <- par$spdensity
  iter <- par$iter
  err <- max(par$relgap, par$pinfeas, par$dinfeas)
  
  m <- max(dim(schur))
  if(iter == 1){
    assign("use_LU",0, pos=sys.frame(which = -3))
    assign("matfct_options_old","", pos=sys.frame(which = -3))
    assign("diagR",matrix(1,m,1), pos=sys.frame(which = -3))
    assign("numpertdiagschur",0, pos=sys.frame(which = -3))
  }
  if(exists("nnzmatold", where=sys.frame(which = -3)) && length(get("nnzmatold", pos=sys.frame(which = -3))) == 0){
    assign("nnzmatold",0, pos=sys.frame(which = -3))
  }
  assign("diagRold",get("diagR", pos=sys.frame(which = -3)), pos=sys.frame(which = -3))
  ##
  ##
  ##
  minrho <- rep(0,2)
  diagschur <- abs(diag(schur))
  if(par$ublksize != 0){
    minrho[1] <- 1e-15
  }else{
    minrho[1] <- 1e-17
  }
  minrho[1] <- max(minrho[1], 1e-6/(3^iter))
  minrho[2] <- max(1e-4, .7^iter)
  minlam <- max(1e-10, 1e-4/(2^iter))
  rho <- min(minrho[1], minrho[2]*(1+base::norm(rhs,type="2"))/(1 + base::norm(diagschur * par$y, type="2")))
  lam <- min(minlam, 0.1*rho*base::norm(diagschur, type="2")/par$normAAt)
  
  if(get("exist_analytic_term", pos=sys.frame(which = -3))){
    rho <- 0
  }
  ratio <- max(get("diagR",pos=sys.frame(which = -3)))/min(get("diagR",pos=sys.frame(which = -3)))
  if(par$depconstr | ratio > 1e10 | iter < 5){
    pertdiagschur <- pmin(rho*diagschur, (1e-4)/pmax(1,abs(par$dy)))
    schur <- mexschurfun(schur, pertdiagschur)
  }
  if(par$depconstr | par$ZpATynorm > 1e10 | par$ublksize | iter < 10){
    lam <- min(lam,1e-4/max(1,base::norm(par$AAt %*% par$dy, type="2")))
    if(get("exist_analytic_term", pos=sys.frame(which = -3))){
      lam <- 0
    }
    schur <- mexschurfun(schur, lam*par$AAt)
  }
  if(max(diagschur)/min(diagschur) > 1e14 & par$blkdim[2] == 0 & iter > 10){
    tol <- 1e-8
    idx <- which(diagschur < tol)
    len <- length(idx)
    pertdiagschur <- matrix(0,m,1)
    if((len > 0 & len < 5) && base::norm(rhs[idx],type="2") < tol){
      pertdiagschur[idx] <- 1
      schur <- mexschurfun(schur, pertdiagschur)
      assign("numpertdiagschur",get("numpertdiagschur", pos=sys.frame(which = -3)) + 1, pos=sys.frame(which = -3))
    }
  }
  ##
  ## Assemble Coefficient Matrix
  ##
  
  coeff <- list(mat11 = c(),
                mat12 = c(),
                mat21 = c(),
                mat22 = c())
  
  len <- ncol(Afree)
  if(is.null(len)){
    len <- 0
  }
  if(length(EE) > 0){
    EE[,c(1,2)] <- len + EE[,c(1,2)]
  }
  if(len > 0){
    EE <- rbind(cbind(c(1:len), c(1:len), rep(0,len)),EE)
  }
  if(length(EE) == 0){
    coeff$mat22 <- c()
  }else{
    coeff$mat22 <- matrix(0,nrow(EE),ncol(EE))
    for(i in 1:nrow(EE)){
      coeff$mat22[EE[i,1],EE[i,2]] <- EE[i,3]
    }
  }
  len2 <- ncol(UU)
  if(is.null(len2)){
    len2 <- 0
  }
  if(len > 0 | len2 > 0){
    if(!is.null(Afree)){
      coeff$mat12 <- cbind(Afree,UU)
    }else{
      coeff$mat12 <- UU
    }
  }else{
    coeff$mat12 <- c()
  }
  coeff$mat11 <- schur
  ncolU <- ncol(coeff$mat12)
  if(is.null(ncolU)){
    ncolU <- 0
  }
  ##
  ## decide which solution methods to use
  ##
  
  if(m + ncolU - length(rhs) > 0){
    rhs <- rbind(rhs, matrix(0, m+ncolU-length(rhs), 1))
  }
  if(ncolU > 300){
    assign("use_LU",1, pos=sys.frame(which = -3))
  }
  
  L <- list(matdim=c(),
            matfct_options = c(),
            R =c(),
            perm = c(),
            Rt = c(),
            Ml = c(),
            Mu = c(),
            Mp = c())
  resnrm <- base::norm(rhs,type="2")
  xx <- matrix(Inf,m,1)
  if(!get("use_LU", pos=sys.frame(which = -3))){
    assign("solve_ok",1, pos=sys.frame(which = -3))
    solvesys <- 1
    assign("nnzmat",mexnnz(coeff$mat11), pos=sys.frame(which = -3))
    nnzmatdiff <- (get("nnzmat", pos=sys.frame(which = -3)) != get("nnzmatold", pos=sys.frame(which = -3)))
    if(get("nnzmat",pos=sys.frame(which = -3)) > spdensity*m^2 | m < 500){
      assign("matfct_options","chol", pos=sys.frame(which = -3))
    }else{
      #assign("matfct_options","spchol", pos=sys.frame(which = -3))
      assign("matfct_options","chol", pos=sys.frame(which = -3))
    }
    L$matdim <- max(dim(schur))
    if(get("matfct_options",pos=sys.frame(which = -3)) == "chol"){
      
      if(is(schur, "sparseMatrix")){
        schur <- as.matrix(schur)
      }
      
      if(iter <= 5){
        schur <- mexschurfun(schur,matrix(1e-20, nrow=nrow(schur), ncol=ncol(schur),2))
      }
      indef <- !(min(Re(eigen(schur)$values)) > 0)
      if(!indef){
        L$matfct_options <- "chol"
        L$R <- chol(as.matrix(schur))
        L$perm <- c(1:m)
        assign("diagR",diag(L$R)^2, pos=sys.frame(which = -3))
      }else{
        L$matfct_options <- "chol"
        L$R <- chol(as.matrix(schur), pivot=TRUE)
        L$perm <- c(1:m)
        assign("diagR",diag(L$R)^2, pos=sys.frame(which = -3))
      }
    }else if(get("matfct_options",pos=sys.frame(which = -3)) == "spchol"){
     # L$matfct_options <- "spchol"
      L$matfct_options <- "chol"
      L$R <- chol(schur, pivot=TRUE)
      L$perm <- attr(L$R, "pivot")
      L$Rt <- t(L$R)
      assign("diagR",diag(L$R)^2, pos=sys.frame(which = -3))
      indef <- !(min(eigen(schur)$values) > 0)
    }
    
    if(indef){
      #print("indef")
      assign("diagR",get("diagRold", pos=sys.frame(which = -3)), pos=sys.frame(which = -3))
      assign("solve_ok",-2, pos=sys.frame(which = -3))
      solvesys <- 0
    }
    
    if(solvesys){
      if(ncolU != 0){
        tmp <- t(coeff$mat12) %*% linsysolvefun(L,coeff$mat12) - coeff$mat22
        tmp <- 0.5*(tmp + t(tmp))
        out <- expand(lu(as.matrix(tmp)))
        L$Ml <- out$L
        L$Mu <- out$U
        L$Mp <- out$P
        tol <- 1e-16
        condest <- max(abs(diag(L$Mu)))/min(abs(diag(L$Mu)))
        idx <- which(abs(diag(L$Mu)) < tol)
        if(length(idx) > 0 | condest > 1e30){
          solvesys <- 0
          assign("solve_ok",-4, pos=sys.frame(which = -3))
          assign("use_LU",1, pos=sys.frame(which = -3))
        }
      }
      out <- symqmr(coeff,rhs,L,c(),c(),get("printlevel", pos=sys.frame(which = -3)))
      xx <- out$xx
      resnrm <- out$resnrm
      assign("solve_ok",out$solve_ok, pos=sys.frame(which = -3))
      flag <- out$flag
    }
    if(get("solve_ok", pos=sys.frame(which = -3)) <= 0.3 || get("solve_ok", pos=sys.frame(which = -3)) == 2){
      tol <- 1e-10
      if((m < 1e4 & get("matfct_options", pos=sys.frame(which = -3)) == "chol" & err > tol) | (m < 2e5 & get("matfct_options", pos=sys.frame(which = -3)) == "spchol" & err > tol)){
        assign("use_LU",1, pos=sys.frame(which = -3))
        #print("Switching to LU")
      }
    }
  }
  
  if(get("use_LU", pos=sys.frame(which = -3))){
    assign("nnzmat",mexnnz(coeff$mat11) + mexnnz(coeff$mat12), pos=sys.frame(which = -3))
    nnzmatdiff <- (get("nnzmat", pos=sys.frame(which = -3)) != get("nnzmatold", pos=sys.frame(which = -3)))
    assign("solve_ok",1, pos=sys.frame(which = -3))
    solvesys <- 1
    if(length(coeff$mat22) > 0){
      raugmat <- rbind(cbind(coeff$mat11, coeff$mat12),cbind(t(coeff$mat12), coeff$mat22))
    }else{
      raugmat <- coeff$mat11
    }
    if(get("nnzmat", pos=sys.frame(which = -3)) > spdensity*m^2 | (m+ncolU) < 500){
      assign("matfct_options","lu", pos=sys.frame(which = -3))
    }else{
      assign("matfct_options","splu", pos=sys.frame(which = -3))
    }
    L$matdim <- max(dim(raugmat))
    
    if(get("matfct_options", pos=sys.frame(which = -3)) == "lu"){
      L$matfct_options <- "lu"
      out <- expand(lu(as.matrix(raugmat),warnSing = FALSE))
      L$L <- out$L
      L$U <- out$U
      L$p <- out$P
    }else if(get("matfct_options", pos=sys.frame(which = -3)) == "splu"){
      L$matfct_options <- "splu"
      out <- expand(lu(Matrix(raugmat, sparse=TRUE)))
      L$L <- out$L
      L$U <- out$U
      L$p <- out$P
      L$q <- out$Q
      
      #Check for singularity
      #if(any(diag(L$L) == 0) | any(diag(L$U) == 0)){
      #  return(list(xx=xx, coeff=coeff, L=L, resnrm=resnrm))
      #}
      
    }else if(get("matfct_options", pos=sys.frame(which = -3)) == "ldl"){
      stop("LDL not implemented")
    }else if(get("matfct_options", pos=sys.frame(which = -3)) == "spldl"){
      stop("Sparse LDL not implemented")
    }
    if(solvesys){
      out <- symqmr(coeff, rhs, L, c(), c(),get("printlevel", pos=sys.frame(which = -3)))
      xx <- out$xx
      resnrm <- out$resnrm
      assign("solve_ok",out$solve_ok, pos=sys.frame(which = -3))
      flag <- out$flag
    }
  }
  assign("nnzmatold",get("nnzmat", pos=sys.frame(which = -3)), pos=sys.frame(which = -3))
  assign("matfct_options_old",get("matfct_options", pos=sys.frame(which = -3)), pos=sys.frame(which = -3))
  
  return(list(xx=xx, coeff=coeff, L=L, resnrm=resnrm,flag=flag))
}
