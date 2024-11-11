oeiinfo <- function(py,yobs,barval)
{
    d2 <- py$d2
    L <- length(yobs)
    p <- length(d2)
    n <- ncol(py$coeff)
    bz <- drop(t(py$basis)%*%yobs)
    z2 <- drop(crossprod(yobs))
    d2c <- py$coeff*d2
    scalec <- apply(py$coeff*d2c,2,sum)
    bzc <- apply(bz*py$coeff,2,sum)
    scales2 <- d2*py$coeffs2
    amat <- scales2+py$varres     #input
    sand <- py$coeffs2/amat
    sbz <- sand*bz
    sd2c <- sand*d2c
    sbz2 <- apply(sbz*bz,2,sum)
    sd2c2 <- apply(sd2c*d2c,2,sum)
    sbzd2c <- apply(sand*d2c*bz,2,sum)
    iomemu2 <- z2+scalec-2*bzc
    iomemu2 <- iomemu2-sbz2-sd2c2+2*sbzd2c
    iomemu2 <- iomemu2/py$varres    #input
    mubstar <- sbz-sd2c
    mub2star <- (bz-d2c)*mubstar        #input
    bound <- apply(amat,2,max)
    bound <- 0.5/bound
    mumk <- z2+scalec-2*bzc+apply(scales2,2,sum)+py$varres*L
    mumk <- mumk - barval
    ret <- .C("oeiinfo_R", as.integer(n), as.integer(p),
              as.integer(L), as.double(py$varres),
              as.double(barval),as.double(iomemu2),
              as.double(bound), as.double(amat),
              as.double(mub2star), as.double(mumk),
              info = double(n), PACKAGE="DynamicGP")
    info <- ret$info-mumk
    return(info)
}
