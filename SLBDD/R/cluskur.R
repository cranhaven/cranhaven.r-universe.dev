#' Cluster Identification Procedure using Projections on Directions of Extreme Kurtosis Coefficient
#'
#' Identification of groups using projections of a vector of features of each time series
#' in directions of extreme kurtosis coefficient.
#'
#' @param x p by k data matrix: p features or variables for each time series
#' and k time series in columns.
#'
#' @return A list containing:
#' \itemize{
#' \item lbl - Cluster labels (possible outliers get negative labels).
#' \item ncl - Number of clusters.
#' }
#'
#' @export
#'
#' @examples
#' data(Stockindexes99world)
#' S <- Stockindexes99world[,-1]
#' v1 <- apply(S,2, mean)
#' v2 <- apply(S,2, sd)
#' M <- rbind(v1,v2)
#' out <- ClusKur(M)
#'
ClusKur <- function(x) {
  xs <- t(x)
  sx = dim(xs)
  n = sx[1]
  p = sx[2]

  clm = ClusMain(xs)

  cln = ClusGrp(xs,clm$lbl)

  return(list(lbl = cln$lbl, ncl = cln$nclus))
}

################### Function ClusGrp

ClusGrp <- function(xs,lbl) {

  dimx = dim(xs)
  n = dimx[1]
  p = dimx[2]

  oc1 = OrdClusLbl(xs,lbl)
  lbl0 = oc1$lbl
  ncl0 = oc1$ncl

  mdlbl = 0
  lbl1 = lbl0
  ncl1 = ncl0

  if (ncl0 > 1) {

    xref = qchisq(0.99,p-1)
    md = matrix(xref,n,1)
    detS = matrix(0,ncl0,1)

    for (i in 1:ncl0) {
      ixx = which(lbl0 == i)
      lixx = length(ixx)
      if (lixx > (p+1)) {
        xx = xs[ixx,]
        S = cov(xx)
        m = colMeans(xx)
        md[ixx] = mahalanobis(xx,m,S)
        detS[i] = det(S)
      }
    }

    for (i in 1:(ncl0-1)) {
      ixx = which(lbl0 == i)
      lixx = length(ixx)
      xx = xs[ixx,]
      S1 = cov(xx)
      mm1 = colMeans(xx)
      detS1 = det(S1)
      if (lixx >= (p+1)) {
        for (j in (i+1):ncl0) {
          ixy = which(lbl0 == j)
          lixy = length(ixy)
          if (lixy >= (p+1)) {
            xy = xs[ixy,]
            S2 = cov(xy)
            mm2 = colMeans(xy)
            detS2 = det(S2)

            xz = rbind(xx,xy)
            S0 = cov(xz)
            m0 = colMeans(xz)
            mdp = mahalanobis(xz,m,S)
            detS0 = det(S0)
            nn0 = lixx + lixy
            ict0 = nn0*log(detS0) + nn0*p*log(2*pi) + sum(mdp)
            ict0 = ict0 + 0.5*log(nn0)*p*(p+3)

            alph = lixx/nn0
            mdx = mahalanobis(xz,mm1,S1)
            mdy = mahalanobis(xz,mm2,S2)
            dens1 = exp(-0.5*mdx)
            dens2 = exp(-0.5*mdy)
            dens = alph*dens1/detS1^0.5 + (1-alph)*dens2/detS2^0.5
            ict1 = -2*sum(log(dens)) + nn0*p*log(2*pi) + log(nn0)*(p*(p+3) + 1)
            if (ict0 < ict1) {
              lbl1[ixy] = i
              detS[i] = detS
              ixx = ixz
              mdlbl = mdlbl + lixy
            }
          }
        }
        xx = xs[ixx,]
        S = cov(xx)
        m = colMeans(xx)
        ixy = which((lbl0 > i)|(lbl0 < 0))
        xy = xs[ixy,]
        mdp = mahalanobis(xy,m,S)
        ixz = which(mdp < md[ixy])
        lixz = length(ixz)
        if (lixz >= 1) {
          mdlbl = mdlbl + lixz
          lbl1[ixy[ixz]] = i
        }
      }
    }


    if (mdlbl > 0) {
      oc2 = OrdClusLbl(xs,lbl1)
      lbl1 = oc2$lbl
      ncl1 = oc2$ncl
    }

  }

  rval = list(lbl = lbl1, nclus = ncl1, nrlb = mdlbl)
  return(rval)
}

OrdClusLbl <- function(x,lbl) {

  dim.x = dim(x)
  n = dim.x[1]
  p = dim.x[2]

  lbl1 = lbl
  n_lbl = 0

  ix0 = which(lbl >= 1)
  lbl0 = lbl[ix0]

  if (length(lbl0) >= 1) {
    lbl_a = as.data.frame(table(lbl0))
    lbl_b = lbl_a[order(-lbl_a$Freq),]
    lbl_c = as.vector(lbl_b$lbl0)
    n_lbl = length(lbl_c)

    for (i in 1:n_lbl) {
      ixx = which(lbl==lbl_c[i])
      if (length(ixx) >= p) lbl1[ixx] = i else lbl1[ixx] = -1
    }
  }

  rval = list(lbl = lbl1, ncl = n_lbl)
  return(rval)

}


ClusMain <- function(x, pdr = 10) {
  xs <- x

  sx = dim(xs)
  n = sx[1]
  p = sx[2]

  lbl = matrix(1,n,1)

  rfval_out = 3

  cutoff = 0.1
  alphac = cutoff/(p^3.322)
  delta = 1 - alphac^(1/n)

  delta = qbeta(0.99^(1/n),1,n-1)

  xsp = preproc(xs,pdr)
  xk = xsp$x
  mm = xsp$mm
  pctvar = xsp$pvar

  V = KurNwm(xk,c(p,p,0))

  dmV = dim(V)
  nt = dmV[1]
  mt = dmV[2]

  ref = 1
  k = 1

  while (k <= mt) {

    t = xk %*% V[,k]

    ts = sort(t)
    ix = sort.list(t)

    rt = pnorm(ts)
    rdif = diff(rt)

    rdif1 = rdif[(p+1):(n-p)]
    idf = which(rdif1 > delta)

    mm1 = length(idf)
    mm1 = mm1 + 2
    if (mm1 > 2) {
      idf = c(0,idf + p,n)
    } else {
      idf = c(0,n)
    }


    if (mm1 > 2) {
      rr = 1
      while (rr < (mm1-1)) {
        wref = -1
        lwl = idf[rr] + 1
        upl = idf[rr+1]
        auxix = ix[lwl:upl]

        if ((upl-lwl) <= p) {
          lbl[auxix] = -1
        } else {
          auxw = lbl[auxix]
          auxw1 = sort(auxw)
          iy = sort.list(auxw)
          for (ss in 1:(upl-lwl+1)) {
            tt = auxix[iy[ss]]
            vref = lbl[tt]
            if (vref > 0) {
              if (vref != wref) {
                ref = ref + 1
                wref = vref
              }
              lbl[tt] = ref
            }
          }
        }
        rr = rr + 1
      }
    }
    k = k + 1
  }

  idf1 = which(rdif[1:p] > delta)
  if (length(idf1) > 0) {
    ixy1 = max(idf1)
    lbl[ix[1:ixy1]] = -1
  }

  idf2 = which(rdif[(n-p):(n-1)] > delta)
  if (length(idf2) > 0) {
    ixy2 = min(idf2)
    lbl[ix[(n - p + ixy2):n]] = -1
  }


  return(list(V = V, lbl = lbl, ncl = ref, pvar = pctvar))
}


preproc <- function(x0, pdr = 10,mode = 2) {

  dmx = dim(x0)
  n = dmx[1]
  p = dmx[2]

  mm = matrix(c(colMeans(x0)),1,p)
  xx = x0 - matrix(1,n,1) %*% mm

  if (n < p+1) {
    qrxx = qr(t(xx))
    Q = qr.Q(qrxx)
    R = qr.R(qrxx)
    xf = t(R[1:(n-1),])
    pm = n-1

    if (mode == 1) {
      S = cov(xf)

      eigS = eigen(S)
      Sval = eigS$values
      Svct = eigS$vectors

      ndr = floor(min(0.5*pm,pdr))
      ival = sort.list(Sval)
      jval = c(ival[(pm-ndr+1):pm])

      KQ = Svct[,jval]

      pctvar = sum(Sval[jval])/sum(Sval)
    } else {
      D = as.matrix(sqrt(apply(xf^2,2,sum)))

      yf = (matrix(1,n,1) %*% t(D)) * xf
      K = t(yf) %*% yf

      eigK = eigen(K)
      Kval = eigK$values
      Kvct = eigK$vectors

      ndr = floor(min(0.5*pm,pdr))
      ndr1 = max(1,floor(0.3*ndr))
      ndr2 = ndr - ndr1

      ival = sort.list(Kval)
      jval = c(ival[1:ndr2],ival[(pm-ndr1+1):pm])

      KQ = Kvct[,jval]
      pctvar = mean(Kval[ival[1:ndr2]])/mean(Kval)
    }
    B = t(KQ)

    xg = xf %*% KQ

    qrxx = qr(xg)
    R = qr.R(qrxx)
    xx = sqrt(n-1)*t(solve(t(R),t(xg)))
  } else {
    S = cov(xx)
    Rr = chol(S)
    xx = t(forwardsolve(t(Rr),t(xx)))
    pctvar = 1
  }

  res = list(x = xx, mm = mm, pvar = pctvar)
  return(res)

}

KurNwm <- function(x0,ndk) {

  sx0 = dim(x0)
  n = sx0[1]
  p0 = sx0[2]

  tol = 1.0e-10

  ndr = sum(ndk)

  Vv = matrix(0,p0,0)

  dk = 1

  while (dk < 3) {

    pmx = min(ndk[dk],p0-1)

    if (pmx > 0) {

      xx = x0
      p = p0
      M = diag(p0)

      for (i in 1:pmx) {

        if (dk == 1) av = OptKur(xx,1) else if (dk == 2) av = OptKur(xx,-1)
        a = av$v

        la = length(a)
        za = matrix(0,la,1)
        za[1] = 1
        w = a - za
        nw = t(w) %*% a
        if (abs(nw) > tol) Q = diag(la) - (w %*% t(w))/c(nw) else Q = diag(la)

        if ((i > 1)||(dk > 1)) Vv = cbind(Vv,M %*% a) else Vv = M %*% a

        Qp = Q[,2:p]
        M = M %*% Qp

        Tt = xx %*% Q
        xx = Tt[,2:p]

        p = p - 1

      }

      if (ndk[dk] > (p0 - 1)) Vv = cbind(Vv,M)

    }

    dk = dk + 1

  }

  if (ndk[3] > 0) {
    V = SdwDir(x0,ndk[3])
    Vv = cbind(Vv,V)
  }

  return(Vv)
}

OptKur <- function(x,tipo,maxit = 30) {

  if (tipo > 0) fact = 1 else fact = -1

  maxitini = 1
  mxitbl = 20
  tol = 1.0e-4
  tol1 = 1.0e-7
  tol2 = 1.0e-2
  tol3 = 2.2e-16
  beta = 1.0e-4
  rho0 = 0.1

  dx = dim(x)
  n = dx[1]
  p = dx[2]

  ep = matrix(1,p,1)

  a = InitDir(x,tipo)

  z = x %*% a
  sk = sum(z^4)
  lam = 2*sk
  f = sk
  g = t(4*t(z^3) %*% x)
  zaux = z^2
  xaux = t(x)*(ep %*% t(zaux))
  H = 12*xaux %*% x

  al = 0
  it = 0
  diff = 1
  rho = rho0
  clkr = 0

  cc = 0
  gl = g - 2*c(lam)*a
  A = 2*t(a)
  aqr = qr(a)
  Q = qr.Q(aqr, complete = T)
  Z = Q[,(2:p)]
  crit = norm(gl,type = "F") + abs(cc)

  while ((crit > tol)&&(it < maxit)) {

    Hl = H - 2*lam[1]*diag(p)
    Hr = t(Z) %*% Hl %*% Z
    Hre = eigen(Hr)
    V = Hre$vectors
    E = Hre$values
    if (length(E) > 1) {
      if (tipo > 0) Es1 = pmin(-abs(E),-tol) else Es1 = pmax(abs(E),tol)
      Hs = V %*% diag(Es1) %*% t(V)
    } else {
      if (tipo > 0) Es1 = min(-abs(E),-tol) else Es1 = max(abs(E),tol)
      Hs = Es1 * V %*% t(V)
    }
    py = - cc/(A %*% t(A))
    rhs = t(Z) %*% (g + H %*% t(A) %*% py)
    pz = -solve(Hs,rhs)
    pp = Z %*% pz + t(A) %*% py

    dlam = (t(a) %*% (gl + H %*% pp))/(2*t(a) %*% a)

    f0d = t(gl) %*% pp - 2*fact*rho*cc*t(a) %*% pp - dlam*cc
    crit1 = beta*norm(pp,type = "F")^2

    if (f0d < crit1) {
      rho1 = 2*(crit1 - fact*f0d)/(tol3 + cc^2)
      rho = max(c(2*rho1,1.5*rho,rho0))
      f = sk - lam*cc - 0.5*fact*rho*cc^2
      f0d = t(gl) %*% pp - 2*fact*rho*cc*t(a) %*% pp - dlam*cc
      clkr = 0
    } else if ((fact*f0d > 1000*crit1)&&(rho > rho0)) {
      rho1 = 2*(crit1 - t(gl) %*% pp + dlam*cc)/(tol3 + cc^2)
      if ((clkr == 4)&&(rho > 2*rho1)) {
        rho = 0.5*rho
        f = sk - lam*cc - 0.5*fact*rho*cc^2
        f0d = t(gl) %*% pp - 2*fact*rho*cc*t(a) %*% pp - dlam*cc
        clkr = 0
      } else clkr = clkr + 1
    }

    if (abs(f0d/(norm(g-2*rho*a*c(cc),type = "F")+abs(cc))) < tol1) break

    al = 1
    itbl = 0
    while (itbl < mxitbl) {
      aa = a + al*pp
      lama = lam + al*dlam
      zz = x %*% aa
      cc = t(aa) %*% aa - 1
      sk = sum(zz^4)
      ff = sk - lama*cc - 0.5*fact*rho*cc^2
      if (fact*ff > fact*(f + 0.0001*al*f0d)) break
      al = al/2
      itbl = itbl + 1
    }

    if (itbl >= mxitbl) break

    a = aa
    lam = lama
    z = zz

    nmd2 = t(a) %*% a
    cc = nmd2 - 1
    f = sk - lam*cc - 0.5*fact*rho*cc^2
    g = t(4*t(z^3) %*% x)
    zaux = z^2
    xaux = t(x)*(ep %*% t(zaux))
    H = 12*xaux %*% x

    gl = g - 2*c(lam)*a
    A = 2*t(a)
    aqr = qr(a)
    Q = qr.Q(aqr, complete = T)
    Z = Q[,(2:p)]
    crit = norm(gl,type = "F") + abs(cc)

    it = it + 1
  }

  a = matrix(c(a),p,1)
  v = a/norm(a,type = "F")
  xa = x %*% v
  f = sum(xa^4)/n

  rval = list(v = v, f = f)
  return(rval)
}

InitDir <- function(x,tipo) {

  if (tipo > 0) fact = 1 else fact = -1

  tol = 1.0e-12
  tol2 = 1.0e-2
  maxitini = 1

  dx = dim(x)
  n = dx[1]
  p = dx[2]

  ep = matrix(1,p,1)

  uv = matrix(apply(x*x,1,sum),n,1)
  uw = 1/(tol + sqrt(uv))
  uu = x*(uw %*% t(ep))

  Su = cov(uu)
  Sue = eigen(Su)
  V = Sue$vectors
  D = Sue$values
  x = data.matrix(x)

  for (i in 1:p) {
    if (i == 1) r = ValKur(x,V[,i]) else r = cbind(r,ValKur(x,V[,i]))
  }
  if (tipo > 0) ik = which.max(r) else ik = which.min(r)
  v = r[ik]
  a = V[,ik[1]]

  itini = 1
  difa = 1
  while ((itini <= maxitini)&&(difa > tol2)) {
    z = x %*% a
    zaux = z^2
    xaux = t(x)*(ep %*% t(zaux))
    H = 12*xaux %*% x
    He = eigen(H)
    V = He$vectors
    E = He$values
    if (tipo > 0) ik = which.max(E) else ik = which.max(E)
    vv = E[ik]
    aa = V[,ik[1]]
    difa = norm(as.matrix(a - aa),type = "F")
    a = aa
    itini = itini + 1
  }

  a = matrix(c(a),p,1)
  v = a/norm(a,type = "F")

  return(v)
}

ValKur <- function(x,d){

  n <- nrow(x)

  xd <- x %*% d

  xdc <- xd - mean(xd)
  vr <- sum(xdc^2)/(n-1)
  kr <- sum(xdc^4)/n
  mcv <- kr/(vr^2)

  return(mcv)

}
