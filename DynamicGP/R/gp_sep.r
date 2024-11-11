#*******************************************************************************
#
# Local Approximate Gaussian Process Regression
# Copyright (C) 2013, The University of Chicago
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
# Questions? Contact Robert B. Gramacy (rbg@vt.edu)
#
#*******************************************************************************


## newGPsep:
##
## build an initial separable GP representation on the C-side
## using the X-Z data and d/g paramterization.

newGPsep <- function(X, Z, d, g, dK=FALSE)
{
    n <- nrow(X)
    m <- ncol(X)
    if(is.null(n)) stop("X must be a matrix")
    if(length(Z) != n) stop("must have nrow(X) = length(Z)")
    if(length(d) == 1) d <- rep(d, m)
    else if(length(d) != m) stop("must have length(d) = ncol(X)")

    out <- .C("newGPsep_R",
              m = as.integer(m),
              n = as.integer(n),
              X = as.double(t(X)),
              Z = as.double(Z),
              d = as.double(d),
              g = as.double(g),
              dK = as.integer(dK),
              gpsepi = integer(1),
              PACKAGE = "DynamicGP")

    ## return C-side GP index
    return(out$gpsepi)
}


## deleteGPsep:
##
## deletes the C-side of a particular separable GP

deleteGPsep <- function(gpsepi)
{
    .C("deleteGPsep_R",
       gpsepi = as.integer(gpsepi), PACKAGE="DynamicGP")
    invisible(NULL)
}


## deleteGPseps:
##
## deletes all gpseps on the C side

deleteGPseps <- function()
{
    .C("deleteGPseps_R", PACKAGE="DynamicGP")
    invisible(NULL)
}


## llikGPsep:
##
## calculate the log likelihood of the GP

llikGPsep <- function(gpsepi, dab=c(0,0), gab=c(0,0))
{
    r <- .C("llikGPsep_R",
            gpsepi = as.integer(gpsepi),
            dab = as.double(dab),
            gab = as.double(gab),
            llik = double(1),
            PACKAGE = "DynamicGP")

    return(r$llik)
}


## getmGPsep
##
## acces the input dimension of a separable GP
##
## totall new to GPsep

getmGPsep <- function(gpsepi)
{
    .C("getmGPsep_R", gpsepi = as.integer(gpsepi), m = integer(1), PACKAGE="DynamicGP")$m
}


## getdGPsep
##
## acces the separable lengthscale of a separable gp
##
## totally new to GPsep

getdGPsep <- function(gpsepi)
{
    m <- getmGPsep(gpsepi)
    .C("getdGPsep_R", gpsepi = as.integer(gpsepi), d = double(m), PACKAGE="DynamicGP")$d
}


## getgGPsep
##
## acces the input dimension of a separable GP
##
## totally new to GPsep

getgGPsep <- function(gpsepi)
{
    .C("getgGPsep_R", gpsepi = as.integer(gpsepi), g = double(1), PACKAGE="DynamicGP")$g
}


## newparamsGPsep:
##
## change the separable GP lengthscale and nugget parameerization
## (without destroying the object and creating a new one)

newparamsGPsep <- function(gpsepi, d, g=-1)
{
    if(all(d <= 0) & g < 0) stop("one of d or g must be new")
    m <- getmGPsep(gpsepi)
    if(length(d) != m) stop("length(d) !=", m)

    r <- .C("newparamsGPsep_R",
            gpi = as.integer(gpsepi),
            d = as.double(d),
            g = as.double(g),
            PACKAGE = "DynamicGP")

    invisible(NULL)
}

## jmleGPsep
##
## interface to C-version for jmleGPsep;
## right now doesn't take an N argument -- the C-side hard-codes
## N=100

jmleGPsep <- function(gpsepi, drange=c(sqrt(.Machine$double.eps), 10),
                      grange=c(sqrt(.Machine$double.eps), 1), dab=c(0,0), gab=c(0,0), maxit=100,
                      verb=0)
{
    ## sanity check tmin and tmax
    m <- getmGPsep(gpsepi)
    if(length(drange) != 2) stop("drange should be a two vector for c(dmin, dmax)")
    dmin <- rep(drange[1], m)
    dmax <- rep(drange[2], m)
    if(length(grange) != 2) stop("grange should be a 2-vector for c(gmin, gmax)")

    ## sanity check ab
    if(length(dab) != 2 || any(dab < 0)) stop("dab should be a positive 2-vector")
    if(length(gab) != 2 || any(gab < 0)) stop("gab should be a positive 2-vector")

    ## call the C-side function
    r <- .C("jmleGPsep_R",
            gpsepi = as.integer(gpsepi),
            maxit = as.integer(maxit),
            verb = as.integer(verb),
            dmin = as.double(dmin),
            dmax = as.double(dmax),
            grange = as.double(grange),
            dab = as.double(dab),
            gab = as.double(gab),
            d = double(m),
            g = double(1),
            dits = integer(1),
            gits = integer(1),
            dconv = integer(1),
            PACKAGE = "DynamicGP")

    return(data.frame(d=t(r$d), g=r$g, tot.its=r$dits+r$gits,
                      dits=r$dits, gits=r$gits, dconv=r$dconv))
}


## predGPsep
##
## obtain the parameters to a multivariate-t
## distribution describing the predictive surface
## of the fitted GP model

predGPsep <- function(gpsepi, XX)
{
    nn <- nrow(XX)
    out <- .C("predGPsep_R",
              gpsepi = as.integer(gpsepi),
              m = as.integer(ncol(XX)),
              nn = as.integer(nn),
              XX = as.double(t(XX)),
              mean = double(nn),
              s2 = double(nn),
              df = double(1),
              llik = double(1),
              PACKAGE="DynamicGP")
        return(list(mean=out$mean, s2=out$s2, df=out$df, llik=out$llik))
}
