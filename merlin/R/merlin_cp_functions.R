
rcs <- function(gml,var,df=1,orthog=T,log=F,event=F)
{
    var     = deparse(substitute(var))
    if (event)  {
        d = gml$y[[gml$modelind]][2]
        d = gml$data[gml$datause[[gml$modelind]],d]
        nrows 	= length(gml$data[gml$datause[[gml$modelind]],var][d==1])
    }
    else        nrows 	= length(gml$data[gml$datause[[gml$modelind]],var])

    if (df==1) 			    index = c(1,nrows)
    else {
        if      (df==2) index = 50
        else if (df==3) index = c(33.3333333333,66.66666666)
        else if (df==4) index = c(25,50,75)
        else if (df==5) index = c(20,40,60,80)
        else if (df==6) index = c(17,33,50,67,83)
        else if (df==7) index = c(14,29,43,57,71,86)
        else if (df==8) index = c(12.5,25,37.5,50,62.5,75,87.5)
        else if (df==9) index = c(11.1,22.2,33.3,44.4,55.6,66.7,77.8,88.9)
        else if (df==10) index = c(10,20,30,40,50,60,70,80,90)
        index = c(1,round(index/100 * nrows),nrows)
    }
    if (event) {
        if (log) {
            knots = sort(log(gml$data[gml$datause[[gml$modelind]],var][d==1]))[index]
        }
        else     knots = sort(gml$data[gml$datause[[gml$modelind]],var][d==1])[index]
        if (orthog) {
          if (log) rmat = merlin_orthog(merlin_rcs(log(gml$data[gml$datause[[gml$modelind]],var]),knots))
          else     rmat = merlin_orthog(merlin_rcs(gml$data[gml$datause[[gml$modelind]],var],knots))
        }
    }
    else {
        rmat = NULL
        if (log) knots = sort(log(gml$data[gml$datause[[gml$modelind]],var]))[index]
        else     knots = sort(gml$data[gml$datause[[gml$modelind]],var])[index]
        if (orthog) {
            if (log) rmat = merlin_orthog(merlin_rcs(log(gml$data[gml$datause[[gml$modelind]],var]),knots))
            else     rmat = merlin_orthog(merlin_rcs(gml$data[gml$datause[[gml$modelind]],var],knots))
        }
    }

    if (length(gml$timevar[gml$modelind])) {
      ist = var==gml$timevar[gml$modelind]
    }
    else ist = 0

    res = list("ist"=ist,"var"=var,"knots"=knots,"rmat"=rmat,"log"=log,"event"=event)
    return(res)
}

# merlin_orthog
merlin_orthog <- function(x) { # based on rcsgen.ado SSC
    meanx <- colMeans(x)
    v <- x - matrix(rep(meanx,each=nrow(x)),nrow=nrow(x))
    v <- cbind(v,rep(1,nrow(v)))
    q <- vector()
    R <- matrix(0,nrow=ncol(v),ncol=ncol(v))
    R[ncol(v),] <- c(meanx,1)
    for (i in 1:ncol(x)) {
        r <- norm(matrix(v[,i],ncol=1),type="2")/sqrt(nrow(v))
        q <- cbind(q, (v[,i] / r))
        R[i,i] <- r
        if (i < ncol(x)) {
            for (j in (i+1):ncol(x)) {
                r <- (t(q[,i]) %*% v[,j])/nrow(v)
                v[,j] <- v[,j] - r%*%q[,i]
                R[i,j] = r
            }
        }
    }
    return(R)
}

# rcs
merlin_rcs <- function(tvar,knots,deriv=0,rmat=NULL) {

    # extract knots locations
    Nobs    <- length(tvar)
    Nknots  <- length(knots)
    kmin    <- knots[1]
    kmax    <- knots[Nknots]

    if (Nknots == 2)  interior <- 0 # if theres only 2 knots they are at kmin and kmax
    else              interior <- Nknots - 2
    Nparams <- interior + 1

    splines <- matrix(NA,nrow=Nobs,ncol=Nparams)
    if (Nparams > 1) { # so there are interior knots
        lambda <- matrix(rep(((kmax-knots[2:Nparams]) / (kmax - kmin)),each=Nobs),nrow=Nobs,ncol=(Nparams-1)) # check this is correct
        knots2 <- matrix(rep(knots[2:Nparams],each=Nobs),nrow=Nobs,ncol=(Nparams-1))
    }

    # calculate splines
    if (deriv==0) {
        splines[,1] <- tvar
        if (Nparams > 1) {
            splines[,2:Nparams]    <-  (tvar - knots2)^3 * (tvar > knots2) -
              lambda * ((tvar - kmin)^3) * (tvar > kmin) -
            (1 - lambda) * ((tvar - kmax)^3) * (tvar > kmax)
        }
    }
    else {
        splines[,1] <- 1
        if (Nparams > 1) {
            splines[,2:Nparams]    <-  3*(tvar - knots2)^2 * (tvar > knots2) -
              lambda * (3*(tvar - kmin)^2) * (tvar > kmin) -
              (1 - lambda) * (3*(tvar - kmax)^2) * (tvar > kmax)
        }
    }

    if (length(rmat) > 0) {
        rmatrix <- solve(rmat)
        splines <- cbind(splines,rep(1,nrow(splines))) %*% rmatrix[,1:Nparams]
    }
    return(splines)
}

fp <- function(gml,var,powers=1)
{
    var = deparse(substitute(var))

    if (length(powers)>2) stop("Error in fp() -> degree of max 2 is allowed")

    if (length(gml$timevar[gml$modelind])) {
        ist = var==gml$timevar[gml$modelind]
    }
    else ist = 0

    res = list("ist"=ist,"var"=var,"powers"=powers)
    return(res)
}

merlin_fp <- function(var,powers)
{
    np   = length(powers)
    Nobs = length(var)

    if (np==1) {
        if (powers==0) fps = as.matrix(log(var))
        else           fps = as.matrix(var ^ powers)
    }
    else {
        fps = matrix(NA, nrow = Nobs, ncol = 2)
        if (powers[1]==powers[2]) {
            if (powers[1]==0) fps[,1] = log(var)
            else              fps[,1] = var^powers[1]
            fps[,2] = fps[,1] * log(var)
        }
        else {
            for (p in 1:2) {
                if (powers[p]==0) fps[,p] = log(var)
                else              fps[,p] = var ^ powers
            }
        }
    }
    return(fps)
}

# extract variable column vector
merlin_xz_var <- function(gml,t,m,c,e)
{
  varname = gml$elinfo[[m]][[c]][[e]]
  if (length(t)>0 & varname %in% gml$timevar) return(t)
  return(gml$data[gml$datause[[gml$modtouse]],varname])
}

# extract correct elements of vcv*nodes
merlin_xz_b <- function(gml,m,c,e)
{
  info = gml$elinfo[[m]][[c]][[e]]
  if (info[1] == gml$Nlevels) {
    if (gml$intmethod[info[1]]!="aghermite") {
      ret <- as.matrix(gml$nmat[[info[1]]][info[2],]) # at the lowest level this will be a whole row
      ret = matrix(ret,nrow=gml$Nobs[gml$modtouse],ncol=length(ret),byrow=TRUE)
    }
    else {

    }
  }
  else {
    if (gml$intmethod[info[1]]!="aghermite") {
      ret = as.vector(gml$nmat[[info[1]]][info[2],gml$qind[(info[1]+1)]]) # only want one column for higher levels
    }
    else {

    }
  }
  return(ret)
}

# calculate restricted cubic splines
merlin_xz_rcs <- function(gml,m,c,e,t=NULL)
{
  info = gml$elinfo[[m]][[c]][[e]]
  ist  = info[["ist"]]
  if (ist) {
    if (length(t)) var = t
    else           var = gml$data[gml$datause[[gml$modtouse]],gml$timevar[gml$modelind]]
  }
  else var = gml$data[gml$datause[[gml$modtouse]],info[["var"]]]

  if (info[["log"]]) return(merlin_rcs(log(var),info[["knots"]],0,info[["rmat"]]))
  else               return(merlin_rcs(var,info[["knots"]],0,info[["rmat"]]))
}

# calculate restricted cubic splines
merlin_xz_fp <- function(gml,m,c,e,t=NULL)
{
  info = gml$elinfo[[m]][[c]][[e]]
  ist  = info[["ist"]]
  if (ist) {
    if (length(t)) var = t
    else           var = gml$data[gml$datause[[gml$modtouse]],gml$timevar[gml$modelind]]
  }
  else var = gml$data[gml$datause[[gml$modtouse]],info[["var"]]]

  return(merlin_fp(var,info[["powers"]]))
}
