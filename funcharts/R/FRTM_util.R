# Utils -------------------------------------------------------------------

center_fd<-function(x_fd,mean=NULL){
  if(is.null(mean)){
    fda::center.fd(x_fd)
  }
  else{
    mean_rep<-fda::fd(matrix(rep(mean$coefs,dim(x_fd$coefs)[2]),ncol =dim(x_fd$coefs)[2]) ,mean$basis)
    x_fd-mean_rep
  }
}
cut_fd_ht<-function(fd,t_i,monotone=FALSE,eval_grid=NULL){
  eval_grid<-if(is.null(eval_grid))seq(fd$basis$rangeval[1],t_i,length.out = 200)else eval_grid
  eval_fd<-fda::eval.fd(eval_grid,fd)
  basis<-fda::create.bspline.basis(c(fd$basis$rangeval[1],t_i),breaks =  eval_grid,norder = 2)
  out<-fda::fd(eval_fd,basis)
  return(out)
}
cut_fd_xt<-function(fd,t_i,monotone=FALSE,eval_grid=NULL){
  eval_grid<-if(is.null(eval_grid))seq(fd$basis$rangeval[1],t_i,length.out = 200)else eval_grid
  eval_fd<-fda::eval.fd(eval_grid,fd)
  basis<-fda::create.bspline.basis(c(fd$basis$rangeval[1],t_i),nbasis = min(length(eval_grid),fd$basis$nbasis),norder = min(length(eval_grid),fd$basis$nbasis-length(fd$basis$params),fd$basis$nbasis))
  out<-fda::smooth.basis(eval_grid,eval_fd,basis)$fd
  return(out)
}
cut_fd_x<-function(x_fd,h_fd,t_x,seq_t){
  range_t<-h_fd$basis$rangeval
  eval_seq=seq(0,1,length.out = 500)
  eval_h<-fda::eval.fd(eval_seq,h_fd)
  eval_x<-fda::eval.fd(eval_seq,x_fd)
  eval_h_new<-eval_h[eval_h<=t_x]
  eval_x_new<-eval_x[eval_h<=t_x]
  eval_seq_new<-eval_seq[eval_h<=t_x]
  npoints<-length(eval_seq_new)
  if(length(eval_h_new)>1){
    if(npoints<4)
      basis_x_r<-fda::create.bspline.basis(range(eval_seq_new),nbasis =npoints,norder = npoints)
    else
      basis_x_r<-fda::create.bspline.basis(range(eval_seq_new),nbasis = max(4,round(0.5*npoints)))
    x_fd_r<-fda::smooth.basis(eval_seq_new,eval_x_new,basis_x_r)$fd
    basis_h_r<-fda::create.bspline.basis(range(eval_seq_new),breaks = eval_seq_new,norder = 2)
    h_fd_r<-fda::fd(eval_h_new,basis_h_r)
  }
  else{
    x_fd_r=h_fd_r=NULL
  }
  out<-list(x_fd_r=x_fd_r,
            h_fd_r=h_fd_r)
  return(out)
}
der_func<-function(asn,smin,smax,der_0=1){
  div<-atan(asn)
  if(asn>=(smin-10^-10)&asn<=(smax+10^-10))return(((abs(der_0-asn)^2)) )else return(Inf)
}
loss2<-function(z,y,z1,y1,alpha,der_h)alpha^2*(z-y)^2+(1-alpha)^2*(z1*der_h-y1)^2
new_sd.fd<-function (e1) {
  range<-e1$basis$rangeval
  seq_eval<-seq(range[1],range[2],length.out = 400)
  eval_fd<-fda::eval.fd(seq_eval,e1)
  eval_fde2<-apply(eval_fd,1,stats::sd)
  basis<-fda::create.bspline.basis(range,nbasis = min(60,e1$basis$nbasis*3))
  fd<-fda::smooth.basis(seq_eval,eval_fde2,basis)$fd
  return(fd)
}
new_mul.fd<-function (e1,e2) {
  range<-e1$basis$rangeval
  seq_eval<-seq(range[1],range[2],length.out =400)
  eval_fd1<-fda::eval.fd(seq_eval,e1)
  eval_fd2<-fda::eval.fd(seq_eval,e2)
  if(dim(eval_fd1)[2]>dim(eval_fd2)[2]) eval_fd2<-matrix(rep(eval_fd2,dim(eval_fd1)[2]),dim(eval_fd1)[1],dim(eval_fd1)[2])
  eval_fde2<-eval_fd1*eval_fd2
  fd<-fda::smooth.basis(seq_eval,eval_fde2,e1$basis)$fd
  return(fd)
}
new_mul.fd2<-function (e1,e2) {
  range<-e1$basis$rangeval
  seq_eval<-seq(range[1],range[2],length.out =300)
  eval_fd1<-fda::eval.fd(seq_eval,e1)
  eval_fd2<-abs(fda::eval.fd(seq_eval,e2))
  if(dim(eval_fd1)[2]>dim(eval_fd2)[2]) eval_fd2<-matrix(rep(eval_fd2,dim(eval_fd1)[2]),dim(eval_fd1)[1],dim(eval_fd1)[2])
  eval_fde2<-if(dim(eval_fd1)[1]==dim(eval_fd2)[1]&dim(eval_fd1)[2]==dim(eval_fd2)[2])eval_fd1*abs(eval_fd2) else if(dim(eval_fd2)[2]==1)eval_fd1*matrix(eval_fd2,dim(eval_fd1)[1],dim(eval_fd1)[2])
  basis<-fda::create.bspline.basis(range,nbasis = 60)
  fd<-fda::smooth.basis(seq_eval,eval_fde2,basis)$fd
  return(fd)
}
new_mul.fd3<-function (e1,e2) {

  range<-e1$basis$rangeval
  seq_eval<-seq(range[1],range[2],length.out =300)
  eval_fd1<-fda::eval.fd(seq_eval,e1)
  eval_fd2<-abs(fda::eval.fd(seq_eval,e2))
  ind_0<-which(eval_fd2>10^-2)
  if(dim(eval_fd1)[2]>dim(eval_fd2)[2]) eval_fd2<-matrix(rep(eval_fd2,dim(eval_fd1)[2]),dim(eval_fd1)[1],dim(eval_fd1)[2])
  eval_fde2<-eval_fd2
  eval_fde2[ind_0,]<-eval_fd1[ind_0,]*1/matrix(eval_fd2[ind_0,],length(ind_0),dim(eval_fd1)[2])
  eval_fde2[-ind_0,]=10^-6
  fd<-fda::smooth.basis(seq_eval,eval_fde2,e1$basis)$fd
  return(fd)
}
new_sqrt.fd<-function (e1, e2=1/2,eval_fd=NULL,seq_eval=NULL,c=1) {
  range<-e1$basis$rangeval
  seq_eval<-if(is.null(seq_eval))seq(range[1],range[2],length.out = 400) else seq_eval
  eval_fd<-if(is.null(eval_fd))fda::eval.fd(seq_eval,e1) else eval_fd
  if(e2==1/2)eval_fd=abs(eval_fd)
  eval_fde2<-(eval_fd/c)^e2
  fd<-fda::smooth.basis(seq_eval,eval_fde2,e1$basis)$fd
  return(fd)
}
inprod2<-function (fdobj1, fdobj2 = NULL, Lfdobj1 = fda::int2Lfd(0), Lfdobj2 = fda::int2Lfd(0),
                   rng = range1, wtfd = 0)
{
  result1 <- fdchk(fdobj1)
  nrep1 <- result1[[1]]
  fdobj1 <- result1[[2]]
  coef1 <- fdobj1$coefs
  basisobj1 <- fdobj1$basis
  type1 <- basisobj1$type
  range1 <- basisobj1$rangeval
  if (is.null(fdobj2)) {
    tempfd <- fdobj1
    tempbasis <- tempfd$basis
    temptype <- tempbasis$type
    temprng <- tempbasis$rangeval
    if (temptype == "bspline") {
      basis2 <- fda::create.bspline.basis(temprng, 1, 1)
    }
    else {
      if (temptype == "fourier")
        basis2 <- fda::create.fourier.basis(temprng, 1)
      else basis2 <- fda::create.constant.basis(temprng)
    }
    fdobj2 <- fda::fd(1, basis2)
  }
  result2 <- fdchk(fdobj2)
  nrep2 <- result2[[1]]
  fdobj2 <- result2[[2]]
  coef2 <- fdobj2$coefs
  basisobj2 <- fdobj2$basis
  type2 <- basisobj2$type
  range2 <- basisobj2$rangeval
  if (rng[1] < range1[1] || rng[2] > range1[2])
    stop("Limits of integration are inadmissible.")
  if (fda::is.fd(fdobj1) && fda::is.fd(fdobj2) && type1 == "bspline" &&
      type2 == "bspline" && is.eqbasis(basisobj1, basisobj2) &&
      is.integer(Lfdobj1) && is.integer(Lfdobj2) && length(basisobj1$dropind) ==
      0 && length(basisobj1$dropind) == 0 && wtfd == 0 &&
      all(rng == range1)) {
    inprodmat <- fda::inprod.bspline(fdobj1, fdobj2, Lfdobj1$nderiv,
                                     Lfdobj2$nderiv)
    return(inprodmat)
  }
  Lfdobj1 <- fda::int2Lfd(Lfdobj1)
  Lfdobj2 <- fda::int2Lfd(Lfdobj2)
  iter <- 0
  rngvec <- rng
  knotmult <- numeric(0)
  if (type1 == "bspline")
    knotmult <- knotmultchk(basisobj1, knotmult)
  if (type2 == "bspline")
    knotmult <- knotmultchk(basisobj2, knotmult)
  if (length(knotmult) > 0) {
    knotmult <- sort(unique(knotmult))
    knotmult <- knotmult[knotmult > rng[1] && knotmult <
                           rng[2]]
    rngvec <- c(rng[1], knotmult, rng[2])
  }
  if ((all(c(coef1) == 0) || all(c(coef2) == 0)))
    return(matrix(0, nrep1, nrep2))
  JMAX <- 25
  JMIN <- 10
  EPS <- 1e-6
  inprodmat <- matrix(0, nrep1, nrep2)
  nrng <- length(rngvec)
  for (irng in 2:nrng) {
    rngi <- c(rngvec[irng - 1], rngvec[irng])
    if (irng > 2)
      rngi[1] <- rngi[1] + 1e-10
    if (irng < nrng)
      rngi[2] <- rngi[2] - 1e-10
    iter <- 1
    width <- rngi[2] - rngi[1]
    JMAXP <- JMAX + 1
    h <- rep(1, JMAXP)
    h[2] <- 0.25
    s <- array(0, c(JMAXP, nrep1, nrep2))
    sdim <- length(dim(s))
    fx1 <- fda::eval.fd(rngi, fdobj1, Lfdobj1)
    fx2 <- fda::eval.fd(rngi, fdobj2, Lfdobj2)
    if (!is.numeric(wtfd)) {
      wtd <- fda::eval.fd(rngi, wtfd, 0)
      fx2 <- matrix(wtd, dim(wtd)[1], dim(fx2)[2]) * fx2
    }
    s[1, , ] <- width * matrix(crossprod(fx1, fx2), nrep1,
                               nrep2)/2
    tnm <- 0.5
    for (iter in 2:JMAX) {
      tnm <- tnm * 2
      if (iter == 2) {
        x <- base::mean(rngi)
      }
      else {
        del <- width/tnm
        x <- seq(rngi[1] + del/2, rngi[2] - del/2, del)
      }
      fx1 <- fda::eval.fd(x, fdobj1, Lfdobj1)
      fx2 <- fda::eval.fd(x, fdobj2, Lfdobj2)
      if (!is.numeric(wtfd)) {
        wtd <- fda::eval.fd(wtfd, x, 0)
        fx2 <- matrix(wtd, dim(wtd)[1], dim(fx2)[2]) *
          fx2
      }
      chs <- width * matrix(crossprod(fx1, fx2), nrep1,
                            nrep2)/tnm
      s[iter, , ] <- (s[iter - 1, , ] + chs)/2
      if (iter >= 5) {
        ind <- (iter - 4):iter
        ya <- s[ind, , ]
        ya <- array(ya, c(5, nrep1, nrep2))
        xa <- h[ind]
        absxa <- abs(xa)
        absxamin <- min(absxa)
        ns <- min((1:length(absxa))[absxa == absxamin])
        cs <- ya
        ds <- ya
        y <- ya[ns, , ]
        ns <- ns - 1
        for (m in 1:4) {
          for (i in 1:(5 - m)) {
            ho <- xa[i]
            hp <- xa[i + m]
            w <- (cs[i + 1, , ] - ds[i, , ])/(ho - hp)
            ds[i, , ] <- hp * w
            cs[i, , ] <- ho * w
          }
          if (2 * ns < 5 - m) {
            dy <- cs[ns + 1, , ]
          }
          else {
            dy <- ds[ns, , ]
            ns <- ns - 1
          }
          y <- y + dy
        }
        ss <- y
        errval <- max(abs(dy))
        ssqval <- max(abs(ss))
        if (all(ssqval > 0)) {
          crit <- errval/ssqval
        }
        else {
          crit <- errval
        }
        if (crit < EPS && iter >= JMIN)
          break
      }
      s[iter + 1, , ] <- s[iter, , ]
      h[iter + 1] <- 0.25 * h[iter]
      if (iter == JMAX)
        warning("Failure to converge.")
    }
    inprodmat <- inprodmat + ss
  }
  if (length(dim(inprodmat) == 2)) {
    return(as.matrix(inprodmat))
  }
  else {
    return(inprodmat)
  }
}
inprod_ext<-function(x_fd_list_1,sc_mat_1=NULL,x_fd_list_2,sc_mat_2=NULL,weights){
  n_fd_var<-length(x_fd_list_1)
  nscalvar<-if(!is.null(sc_mat_1))dim(sc_mat_1)[2] else 0
  ip_i<-list()
  for(ii in 1:n_fd_var){
    ip_i[[ii]]<-inprod2(x_fd_list_1[[ii]],x_fd_list_2[[ii]])*weights[ii]
  }
  we_sc<-if(!is.null(sc_mat_1))  weights[(n_fd_var+1):(n_fd_var+nscalvar)] else NULL
  if(!is.null(sc_mat_1)){
    if(length(we_sc)==1)
      ip_sc<-sc_mat_1%*%we_sc%*%t(sc_mat_2)
    else
      ip_sc<-sc_mat_1%*%diag(we_sc)%*%t(sc_mat_2)
  }
  ip_fd=do.call("+",ip_i)
  ip<-if(!is.null(sc_mat_1))ip_sc+ip_fd else ip_fd
  return(ip)
}
get_ARL<-function(mod_phaseII,t_out=NULL,min_t=0){
  T2_fd<-mod_phaseII$T2_fd
  SPE_fd<-mod_phaseII$SPE_fd
  CL_T2<-mod_phaseII$CL_T2
  CL_SPE<-mod_phaseII$CL_SPE
  n_obs<-length(T2_fd)
  seq_x<-mod_phaseII$seq_x
  dom_limit<-CL_T2$basis$rangeval
  grid_eval<-seq(min(dom_limit),max(dom_limit),length.out = 500)
  eval_CL_T2<-fda::eval.fd(grid_eval,CL_T2)
  eval_CL_SPE<-fda::eval.fd(grid_eval,CL_SPE)
  eval_CL_T2[eval_CL_T2<0]=0.01
  eval_CL_SPE[eval_CL_SPE<0]=0.01
  FA<-TD<-numeric()
  TTS=matrix(0,n_obs,length(min_t))
  aaa=ind_out_list<-list()
  matrix_out<-matrix(NA,n_obs,500)
  for (ii in 1:n_obs) {
    dom_i<-T2_fd[[ii]]$basis$rangeval
    ind_grid<-which(grid_eval>=dom_i[1]&grid_eval<=dom_i[2])
    grid_eval_i<-grid_eval[ind_grid]
    eval_CL_T2_i<-eval_CL_T2[ind_grid]
    eval_CL_SPE_i<-eval_CL_SPE[ind_grid]
    eval_T2_i<-fda::eval.fd(grid_eval_i,T2_fd[[ii]])
    eval_SPE_i<-fda::eval.fd(grid_eval_i,SPE_fd[[ii]])

    eval_CL_SPE_i[eval_CL_SPE_i<0]=0.01
    eval_T2_i[eval_T2_i<0]=0.01
    ind_out<-as.numeric(eval_T2_i>eval_CL_T2_i|eval_SPE_i>eval_CL_SPE_i)
    matrix_out[ii,ind_grid]<-as.numeric(ind_out==1)
    ind_out_l<-which(ind_out==1)
    if(is.null(t_out[ii])){
      ind_ic<-1:length(grid_eval_i)
      seq_x_n<-grid_eval_i[ind_ic]
      delta_x=seq_x_n[2]-seq_x_n[1]
      if(length(ind_ic)>0)
        FA[ii]<-sum(as.numeric(ind_ic%in%ind_out_l)*delta_x)/(delta_x*length(ind_ic))
      else
        FA[ii]<-NA
      TD[ii]=NA
      TTS[ii]<-NA
    }
    else{
      ind_ic<-which(grid_eval_i<t_out[ii])
      seq_x_n<-grid_eval_i[ind_ic]
      delta_x=seq_x_n[2]-seq_x_n[1]
      if(length(ind_ic)>0)
        FA[ii]<-sum(as.numeric(ind_ic%in%ind_out_l)*delta_x)/(delta_x*length(ind_ic))
      else
        FA[ii]<-NA
      ind_oc<-which(grid_eval_i>=t_out[ii])
      seq_x_n<-grid_eval_i[ind_oc]
      delta_x=seq_x_n[2]-seq_x_n[1]
      if(is.na(delta_x))delta_x=grid_eval_i[ind_oc]-grid_eval_i[ind_oc-1]
      if(length(ind_oc)>0)
        TD[ii]<-sum(as.numeric(ind_oc%in%ind_out_l)*delta_x)/(delta_x*length(ind_oc))
      else
        TD[ii]<-NA
      for(ll in 1:length(min_t)){
        if(sum(ind_out)==0)
        {
          TTS[ii,ll]<-1
        }
        else{
          t_out_ii<-grid_eval_i[ind_out_l]
          diff_out<-t_out_ii-t_out[ii]
          ind<-which(diff_out>=0)
          if(length(ind)==0){
            TTS[ii,ll]<-1
          }
          else{
            t_out_ii=t_out_ii[ind]
            kkkk=0
            for (mm in 1:length(t_out_ii)) {
              est_max<-t_out_ii[mm]+min_t[ll]
              seq_red<-seq_x_n[seq_x_n<=est_max&seq_x_n>=t_out_ii[mm]]
              seq_obs<-t_out_ii[t_out_ii<=est_max&t_out_ii>=t_out_ii[mm]]
              if(length(seq_red)==length(seq_obs)){
                TTS[ii,ll]<-(est_max-t_out[ii])/(max(grid_eval_i)-t_out[ii])
                kkkk=1
                break
              }
            }
            if(kkkk==0) TTS[ii,ll]=1
          }
        }
      }
    }
  }
  TTS_r<-list()
  m_TTS=numeric()
  for (ll in 1:length(min_t)) {
    TTS_r[[ll]]<-TTS[,ll][TTS[,ll]!=Inf&!is.na(TTS[,ll])]
    m_TTS[ll]<-base::mean(TTS_r[[ll]],na.rm=TRUE)
  }
  m_TD<-base::mean(TD,na.rm=TRUE)
  m_FA<-base::mean(FA,na.rm=TRUE)
  m_pFA<-base::mean(apply(matrix_out,2,sum,na.rm=TRUE)/apply(!is.na(matrix_out),2,sum),na.rm=TRUE)
  out<-list(FA=FA,
            TD=TD,
            TTS=TTS,
            m_FA=m_FA,
            m_TD=m_TD,
            m_TTS=m_TTS,
            matrix_out=matrix_out,
            m_pFA=m_pFA)
  return(out)
}
fdchk<-function (fdobj)
{
  if (inherits(fdobj, "fd")) {
    coef <- fdobj$coefs
  }
  else {
    if (inherits(fdobj, "basisfd")) {
      coef <- diag(rep(1, fdobj$nbasis - length(fdobj$dropind)))
      fdobj <- fda::fd(coef, fdobj)
    }
    else {
      stop("FDOBJ is not an FD object.")
    }
  }
  coefd <- dim(as.matrix(coef))
  if (length(coefd) > 2)
    stop("Functional data object must be univariate")
  nrep <- coefd[2]
  basisobj <- fdobj$basis
  return(list(nrep, fdobj))
}
is.eqbasis<-function (basisobj1, basisobj2)
{
  eqwrd <- TRUE
  if (basisobj1$type != basisobj2$type) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  if (any(basisobj1$rangeval != basisobj2$rangeval)) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  if (basisobj1$nbasis != basisobj2$nbasis) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  if (any(basisobj1$params != basisobj2$params)) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  if (any(basisobj1$dropind != basisobj2$dropind)) {
    eqwrd <- FALSE
    return(eqwrd)
  }
  return(eqwrd)
}
knotmultchk<-function (basisobj, knotmult)
{
  type <- basisobj$type
  if (type == "bspline") {
    params <- basisobj$params
    nparams <- length(params)
    norder <- basisobj$nbasis - nparams
    if (norder == 1) {
      knotmult <- c(knotmult, params)
    }
    else {
      if (nparams > 1) {
        for (i in 2:nparams) if (params[i] == params[i -
                                                     1])
          knotmult <- c(knotmult, params[i])
      }
    }
  }
  return(knotmult)
}

