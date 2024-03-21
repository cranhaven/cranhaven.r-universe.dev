IRhomogen <- function(par, InvTimes){
  ##
  ## Inversion Recovery MRI  1 compartment (used for CSF)
  ##
  ## S_{InvTime} = par[1] * abs( 1 - 2 * exp(-InvTime*par[2]) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irfluid,
                as.double(par),
                as.double(InvTimes),
                as.integer(n),
                fval = double(n),
                grad = double(2*n))[c("fval", "grad")]
  fval <- z$fval
  attr(fval,"gradient") <- matrix(z$grad, n, 2)
  fval
}

IRmix2 <- function(par, InvTimes, S0f, Rf){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid parameters
  ##
  ## S_{InvTime} = abs(S0f *  par[1] * (1-2*exp(-InvTime*Rf)) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmix,
                as.double(par),
                as.double(InvTimes),
                as.double(S0f),
                as.double(Rf),
                as.integer(n),
                fval = double(n),
                grad = double(3*n))[c("fval", "grad")]
  fval <- z$fval
  attr(fval, "gradient") <- matrix(z$grad, n, 3)
  fval
}

IRmix2fv <- function(par, InvTimes, S0f, Rf){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid parameters
  ##
  ## S_{InvTime} = abs(S0f *  par[1] * (1-2*exp(-InvTime*Rf)) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmixfv,
                as.double(par),
                as.double(InvTimes),
                as.double(S0f),
                as.double(Rf),
                as.integer(n),
                fval = double(n))[c("fval")]
  fval <- z$fval
  fval
}

IRmix5 <- function(par, InvTimes){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid parameters
  ##
  ## S_{InvTime} = abs(th[5] *  par[1] * (1-2*exp(-InvTime*th[4])) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmix5,
                as.double(par),
                as.double(InvTimes),
                as.integer(n),
                fval = double(n),
                grad = double(5*n))[c("fval", "grad")]
  fval <- z$fval
  attr(fval, "gradient") <- matrix(z$grad, n, 5)
  fval
}

IRmix5fv <- function(par, InvTimes){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid parameters
  ##
  ## S_{InvTime} = abs(th[5] *  par[1] * (1-2*exp(-InvTime*th[4])) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmix5fv,
                as.double(par),
                as.double(InvTimes),
                as.integer(n),
                fval = double(n))[c("fval")]
  fval <- z$fval
  fval
}

LSIRmix2 <- function(par, Y, InvTimes, S0f, Rf){
   fval <- IRmix2fv(par, InvTimes, S0f, Rf)
   kval <- (Y - fval) 
   value <- sum(kval^2)
   value
}

LSIRmix2grad <- function(par, Y, InvTimes, S0f, Rf){
  fval <- IRmix2(par, InvTimes, S0f, Rf)
  grad <- attr(fval,"gradient")
  kval <- (Y - fval) 
  gradient <- -2 * kval %*% grad
  gradient
}

LSIRmix5 <- function(par, Y, InvTimes){
  fval <- IRmix5fv(par, InvTimes)
  kval <- (Y - fval) 
  value <- sum(kval^2)
  value
}

LSIRmix5grad <- function(par, Y, InvTimes){
  fval <- IRmix5(par, InvTimes)
  grad <- attr(fval,"gradient")
  kval <- (Y - fval) 
  gradient <- -2 * kval %*% grad
  gradient
}

IRmix2fix <- function(par, InvTimes, S0f, S0s, Rf, Rs){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid and solid parameters
  ##              mixture parameter only
  ##
  ## S_{InvTime} = abs(S0f *  par[1] * (1-2*exp(-InvTime*Rf)) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmix0,
                as.double(par),
                as.double(InvTimes),
                as.double(Rs),
                as.double(S0s),
                as.double(S0f),
                as.double(Rf),
                as.integer(n),
                fval = double(n),
                grad = double(n))[c("fval", "grad")]
  fval <- z$fval
  attr(fval, "gradient") <- matrix(z$grad, n, 1)
  fval
}

IRhomogenQL <- function(par, InvTimes, CL, sig, L){
  ##
  ## Inversion Recovery MRI  1 compartment (used for CSF)
  ##
  ## S_{InvTime} = par[1] * abs( 1 - 2 * exp(-InvTime*par[2]) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irfluid,
                as.double(par),
                as.double(InvTimes),
                as.integer(n),
                fval = double(n),
                grad = double(2*n))[c("fval", "grad")]
  sfval <- z$fval/sig
  fval <- CL * hg1f1(-.5, L, -sfval*sfval/2)
  CC <- CL * hg1f1(.5, L+1, -sfval*sfval/2) * sfval/2/L/sig
  grad <- matrix(CC*z$grad, n, 2)
  ind <- sfval>100
  if(any(is.na(ind))){
    warning(paste("IRhomogenQL\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",CC,"ind",ind),call.=FALSE)
  }
  if(any(ind)){
    # use LS if there is no real difference, factor to keep monotonicity
    fval[ind] <- z$fval[ind]*1.0001
    grad[ind,] <- matrix(z$grad, n, 2)[ind, ]*1.0001
  }
  if(any(is.na(grad))){
    warning(paste("IRhomogenQL/grad\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",CC,"ind",ind,"\n grad",grad),call.=FALSE)
  }
  attr(fval, "gradient") <- grad
  fval
}

IRmix2QL <- function(par, InvTimes, S0f, Rf, CL, sig, L){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid parameters
  ##
  ## S_{InvTime} = S0f * abs( par[1] * (1-2*exp(-InvTime*Rf)) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmix,
                as.double(par),
                as.double(InvTimes),
                as.double(S0f),
                as.double(Rf),
                as.integer(n),
                fval = double(n),
                grad = double(3*n))[c("fval", "grad")]
  sfval <- z$fval/sig
  fval <- CL * hg1f1(-.5, L, -sfval*sfval/2)
  CC <- CL * hg1f1(.5, L+1, -sfval*sfval/2) * sfval/2/L/sig
  grad <- matrix(CC*z$grad, n, 3)
  ind <- sfval>100
  if(any(is.na(ind))){
    warning(paste("IRmix2QL\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",CC,"ind",ind),call.=FALSE)
  }
  if(any(ind)){
    # use LS if there is no real difference, factor to keep monotonicity
    fval[ind] <- z$fval[ind]*1.0001
    grad[ind,] <- matrix(z$grad, n, 3)[ind, ]*1.0001
  }
  if(any(is.na(grad))){
    warning(paste("IRmix2QL/grad\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",CC,"ind",ind,"\n grad",grad),call.=FALSE)
  }
  attr(fval, "gradient") <- grad
  fval
}

IRmix2fvQL <- function(par, InvTimes, S0f, Rf, CL, sig, L){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid parameters
  ##
  ## S_{InvTime} = S0f * abs( par[1] * (1-2*exp(-InvTime*Rf)) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmixfv,
                as.double(par),
                as.double(InvTimes),
                as.double(S0f),
                as.double(Rf),
                as.integer(n),
                fval = double(n))[c("fval")]
  sfval <- z$fval/sig
  fval <- CL * hg1f1(-.5, L, -sfval*sfval/2)
  ind <- sfval>100
  if(any(is.na(ind))){
    warning(paste("IRmix2QL\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",fval,"ind",ind),call.=FALSE)
  }
  if(any(ind)){
    # use LS if there is no real difference, factor to keep monotonicity
    fval[ind] <- z$fval[ind]*1.0001
  }
  fval
}

IRmix5QL <- function(par, InvTimes, CL, sig, L){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid parameters
  ##
  ## S_{InvTime} = S0f * abs( par[1] * (1-2*exp(-InvTime*Rf)) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmix5,
                as.double(par),
                as.double(InvTimes),
                as.integer(n),
                fval = double(n),
                grad = double(3*n))[c("fval", "grad")]
  sfval <- z$fval/sig
  fval <- CL * hg1f1(-.5, L, -sfval*sfval/2)
  CC <- CL * hg1f1(.5, L+1, -sfval*sfval/2) * sfval/2/L/sig
  grad <- matrix(CC*z$grad, n, 3)
  ind <- sfval>100
  if(any(is.na(ind))){
    warning(paste("IRmix2QL\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",CC,"ind",ind),call.=FALSE)
  }
  if(any(ind)){
    # use LS if there is no real difference, factor to keep monotonicity
    fval[ind] <- z$fval[ind]*1.0001
    grad[ind,] <- matrix(z$grad, n, 3)[ind, ]*1.0001
  }
  if(any(is.na(grad))){
    warning(paste("IRmix2QL/grad\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",CC,"ind",ind,"\n grad",grad),call.=FALSE)
  }
  attr(fval, "gradient") <- grad
  fval
}

IRmix5fvQL <- function(par, InvTimes, CL, sig, L){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid parameters
  ##
  ## S_{InvTime} = S0f * abs( par[1] * (1-2*exp(-InvTime*Rf)) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmix5fv,
                as.double(par),
                as.double(InvTimes),
                as.integer(n),
                fval = double(n))[c("fval")]
  sfval <- z$fval/sig
  fval <- CL * hg1f1(-.5, L, -sfval*sfval/2)
  ind <- sfval>100
  if(any(is.na(ind))){
    warning(paste("IRmix2QL\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",fval,"ind",ind),call.=FALSE)
  }
  if(any(ind)){
    # use LS if there is no real difference, factor to keep monotonicity
    fval[ind] <- z$fval[ind]*1.0001
  }
  fval
}

LSIRmix2QL <- function(par, Y, InvTimes, S0f, Rf, CL, sig, L){
  fval <- IRmix2fvQL(par, InvTimes, S0f, Rf, CL, sig, L)
  kval <- (Y - fval) 
  value <- sum(kval^2)
  value
}

LSIRmix2QLgrad <- function(par, Y, InvTimes, S0f, Rf, CL, sig, L){
  fval <- IRmix2QL(par, InvTimes, S0f, Rf, CL, sig, L)
  grad <- attr(fval,"gradient")
  kval <- (Y - fval) 
  value <- sum(kval^2)
  gradient <- -2 * kval %*% grad
  gradient
}

LSIRmix5QL <- function(par, Y, InvTimes, CL, sig, L){
  fval <- IRmix5fvQL(par, InvTimes, CL, sig, L)
  kval <- (Y - fval) 
  value <- sum(kval^2)
  value
}

LSIRmix5QLgrad <- function(par, Y, InvTimes, CL, sig, L){
  fval <- IRmix2QL(par, InvTimes, CL, sig, L)
  grad <- attr(fval,"gradient")
  kval <- (Y - fval) 
  value <- sum(kval^2)
  gradient <- -2 * kval %*% grad
  gradient
}

IRmix2fixQL <- function(par, InvTimes, S0f, S0s, Rf, Rs, CL, sig, L){
  ##
  ## Inversion Recovery MRI 2 compartments (fluid/solid) with fixed fluid and solid parameters
  ##              mixture parameter only
  ##
  ## S_{InvTime} = S0f * abs( par[1] * (1-2*exp(-InvTime*Rf)) +
  ##                        (1-par[1])*par[3]* (1-2*exp(-InvTime*par[2])) )
  ##
  n <- length(InvTimes)
  z <- .Fortran(C_irmix0,
                as.double(par),
                as.double(InvTimes),
                as.double(Rs),
                as.double(S0s),
                as.double(S0f),
                as.double(Rf),
                as.integer(n),
                fval = double(n),
                grad = double(n))[c("fval", "grad")]
  sfval <- z$fval/sig
  fval <- CL * hg1f1(-.5, L, -sfval*sfval/2)
  CC <- CL * hg1f1(.5, L+1, -sfval*sfval/2) * sfval/2/L/sig
  grad <- matrix(CC*z$grad, n, 1)
  ind <- sfval>100
  if(any(is.na(ind))){
    warning(paste("IRmix2fixQL\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",CC,"ind",ind),call.=FALSE)
  }
  if(any(ind)){
    # use LS if there is no real difference, factor to keep monotonicity
    fval[ind] <- z$fval[ind]*1.0001
    grad[ind,] <- matrix(z$grad, n, 1)[ind, ]*1.0001
  }
  if(any(is.na(grad))){
    warning(paste("IRmix2fixQL/grad\n","par",par,"sigma",sig,
                  "\n fv",z$fval,"\n CC",CC,"ind",ind,"\n grad",grad),call.=FALSE)
  }
  attr(fval, "gradient") <- grad
  fval
}

