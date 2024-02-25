#' Fits the bivariate joint tail model with Kernel density estimator
#'
#' Fits the bivariate joint tail model with Kernel density estimator (Adapted from Cooley et al. (2019))and provides
#' estimates of a conditional or joint exceedance level curve with a probability corresponding to '\code{pobj}'.
#' Also provides estimates of dependence measures.
#' @param u2 Two column data frame
#' @param pbas joint return period to be modelled with a kde
#' @param pobj objective joint return period modelled with the joint tail model
#' @param beta smoothing parameter for the transition between asymptotic dependent and independent regimes near the axes
#' @param vtau estimate of the rank correlation between the two variables
#' @param devplot additional plots for development (significantly slows the function)
#' @param px Uniform values of the first margin with a mixed distribution (empirical below and gpd above a threshold)
#' @param py Uniform values of the second margin with a mixed distribution (empirical below and gpd above a threshold)
#' @param mar1 Values of the first margin
#' @param mar2 Values of the second margin
#' @param kk uniform margins of the original data
#' @param interh type of hazard interrelation '\code{comb}' for compound and '\code{casc}' for cascade,
#' @export
#' @return a list containing the following:
#' \itemize{
#' \item levelcurve - data frame the objective containing level curve with a return level '\code{pobj}'
#' \item wq0ri - matrix of the base level curve with a return level '\code{pbas}'
#' \item etaJT - threshold dependent extremal dependence measure
#' \item chiJT - threshold dependent coefficient of tail dependence
#' }
#' @seealso \code{\link[texmex]{chi}}
#' @references Tilloy, A., Malamud, B.D., Winter, H. and Joly-Laugel, A., 2020.
#' Evaluating the efficacy of bivariate extreme modelling approaches
#' for multi-hazard scenarios. Natural Hazards and Earth System Sciences, 20(8), pp.2091-2117.
#' @references Cooley, D., Thibaud, E., Castillo, F. and Wehner, M.F., 2019.
#' A nonparametric method for producing isolines of bivariate exceedance probabilities.
#' Extremes, 22(3), pp.373-390.
#' @examples
#' # Inport data
#' data(porto)
#' tr1=0.9
#' tr2=0.9
#' fire01meantemp=na.omit(fire01meantemp)
#' u=fire01meantemp
#'
#' # Compute uniform margins
#' marg=Margins.mod(tr1,tr2,u=fire01meantemp)
#' kk=marg$uvar
#' pp=marg$uvar_ext
#' uu=marg$val_ext
#' upobj=0.001
#' vtau=cor.test(x=u[,1],y=u[,2],method="kendall")$estimate
#' interh="comb"
#' \dontrun{
#' # Fit JT-KDE model
#' jtres<-JT.KDE.ap(u2=u,pbas=0.01,pobj=upobj,beta=100,kk=kk,vtau=vtau,
#' devplot=FALSE,mar1=uu[,1],mar2=uu[,2],px=pp[,1],py=pp[,2],interh=interh)
#' plot(jtres$levelcurve)
#' }
#' @importFrom stats approx cor.test na.omit optim
#'          predict qchisq qnorm quantile rnorm spline uniroot
#' @import ks
#' @import zoo
#' @import lattice
#' @importFrom zoo na.approx
#' @importFrom viridis viridis


JT.KDE.ap<-function(u2,pbas ,pobj,beta,vtau,devplot=F,kk,mar1,mar2,px,py,interh=NA){

  e1<-seq(0,1.2*max(u2[,1]),length.out=200)
  e2<-seq(0,1.2*max(u2[,2]),length.out=200)
  evp<-as.data.frame(cbind(e1,e2))
  bwaa=expand.grid(e1,e2)
  aa<-kcde(u2, gridsize=300,tail.flag = "upper.tail",xmin=c(0,0),xmax=c(1.5*max(u2[,1]),1.5*max(u2[,2])))

  lox<-aa$eval.points[[1]]
  loy<-aa$eval.points[[2]]
  ngx=100000

  godx<-approx(mar1,px, n = ngx, method = "linear",
               yleft = min(px), yright = max(px))
  gody<-approx(mar2,py, n = ngx, method = "linear",
               yleft = min(py), yright = max(py))


  pxe<-approx(godx$x, godx$y, xout = lox, method = "linear",yleft = min(px),yright = max(px), rule = 1)$y
  pye<-approx(gody$x, gody$y, xout = loy, method = "linear",yleft = min(py),yright = max(py), rule = 1)$y

  if(devplot==TRUE){
    plot(aa,cont = c(0.05,0.1),display="filled.contour" ,col=viridis(10))

  }
  az<-aa$estimate
  wq<-contourLines(lox,loy, az, levels = c(0.7,0.5,0.2,0.1,pbas))
  wq0ri<-cbind(wq[[5]]$x,wq[[5]]$y)

  var1<-u2[,1]
  var2<-u2[,2]
  wu<-data.frame(u2)

  ovar1<-var1[order(var1)]
  rx1<-rank(ovar1)
  ovar2<-var2[order(var2)]
  rx2<-rank(ovar2)
  p01=0.01
  p02=0.01
  q0=0.92





  if(length(which(wq0ri[,1]<0))>0)wq0ri<-wq0ri[-which(wq0ri[,1]<0),]
  if(length(which(wq0ri[,2]<0))>0)wq0ri<-wq0ri[-which(wq0ri[,2]<0),]


  wqUnitx<- approx(mar1, px, xout = wq0ri[,1], method = "linear",yleft = min(px),yright = max(px), rule = 1)$y
  wqUnity<- approx(mar2, py, xout = wq0ri[,2], method = "linear",yleft = min(py),yright = max(py), rule = 1)$y


  #Transforming Pbase to Frechet Margins
  wqxFrechet<--1/log(wqUnitx)
  wqyFrechet<--1/log(wqUnity)


  pb<-1-pbas

  #Setting up Pobjective


  s=pbas/pobj


  xFrechet<--1/log(px)
  yFrechet<--1/log(py)
  # ModelHugo_file=paste0(getwd(),"/LF/BveLTDep.R")
  # source (ModelHugo_file)
  if(vtau<0)q0=0.9
  qq<-c()
  cm<-c()
  cq<-c()
  ccd<-c()
  qc<-.95
  rq0<-seq(0.75,0.95,by=0.01)
  for(q0 in rq0){
    estims<-try(Bv.LT.Dep (data= kk,mod.thresh.u = q0,crit.lev.u = qc,sig.lev=0.05,ci.meth='se',marg.inf=TRUE),silent = T)
    qd<-try((estims$par[2]),silent=TRUE)
    cd<-try((estims$chiCIs),silent=TRUE)
    cc<-try((estims$chi),silent=TRUE)

    if(is.numeric(qd)){
      qq<-c(qq,qd)
      ccd<-c(ccd,cc)
    }
    if(is.numeric(cd)){
      cq<-c(cq,cd)
    }
  }
  plot(ccd)
  plot(qq)
  sumd<-0
  for(i in 1:length(diff(qq))){
    sdiff<-diff(qq)[i]
    sumd<-c(sumd,sumd[i]+sdiff)}
  sh<-which(sumd<=-0.02|sumd>=0.02 )[1]
  q0<-rq0[sh-1]
  plot(sumd)
  estims<-try(Bv.LT.Dep (data= kk,mod.thresh.u = q0,crit.lev.u = qc,sig.lev=0.05,ci.meth='se',marg.inf=TRUE),silent = T)
  chat=NA
  etahat=NA
  Chilow=NA
  Chimed=NA
  try(chat<-estims$par[1],silent=TRUE)
  try(etahat<-estims$par[2],silent=TRUE)
  try(Chilow<-estims$chiCIs[1],silent=TRUE)
  try(Chimed<-estims$chi,silent=TRUE)

  #Loop for asymptotic dependence

  if (!is.na(Chilow) & (Chilow<0.05 & etahat<0.75| etahat<0.6)){
    message("Asymptotic Independence")
    bet=beta
    m1= 1- (wqxFrechet/(wqxFrechet+wqyFrechet))^bet
    m2<-1- (wqyFrechet/(wqxFrechet+wqyFrechet))^bet
    eta1<-m1*etahat + (1-m1)
    eta2<-m2*etahat + (1-m2)
    projx<-s^(eta1)*wqxFrechet
    projy<-s^(eta2)*wqyFrechet
  }else{
    projx<-s*wqxFrechet
    projy<-s*wqyFrechet}



  projbackx<-exp(-(1/projx))
  projbacky<-exp(-(1/projy))




  objx<-approx(px,mar1, xout = projbackx, method = "linear",yleft = min(mar1),yright = max(mar1), rule = 1)$y
  objy<-approx(py,mar2, xout = projbacky, method = "linear",yleft = min(mar2),yright = max(mar2), rule = 1)$y

  wqobj<-cbind(objx, objy)

  if(interh=="casc"){
    pobj<-c(seq(0.00000005,0.00005,by=0.0000001),seq(0.00002,pbas/5,by=0.00005))
    s=pbas/pobj

    wqobjf<-c()
    for (sl in 1:length(s)){

      if (etahat<0.85& Chilow<0.02){
        message("Asymptotic Independence")
        beta=200
        m1= 1- (wqxFrechet/(wqxFrechet+wqyFrechet))^beta
        m2<-1- (wqyFrechet/(wqxFrechet+wqyFrechet))^beta
        eta1<-m1*etahat + (1-m1)
        eta2<-m2*etahat + (1-m2)
        projx<-s[sl]^(eta1)*wqxFrechet
        projy<-s[sl]^(eta2)*wqyFrechet
      }else{
        # plot(xFrechet,yFrechet)
        projx<-s[sl]*wqxFrechet
        projy<-s[sl]*wqyFrechet}

      projbackx<-exp(-(1/projx))
      projbacky<-exp(-(1/projy))



      objx<-approx(px,mar1, xout = projbackx, method = "linear",yleft = min(mar1),yright = max(mar1), rule = 1)$y
      objy<-approx(py,mar2, xout = projbacky, method = "linear",yleft = min(mar2),yright = max(mar2), rule = 1)$y

      wqobj<-cbind(objx, objy)
      wqobj<-na.omit(wqobj)
      wqobj<-data.frame(wqobj)
      wqobj[,1]<-jitter(wqobj[,1])
      xlt=seq(min(wqobj[,1]),max(wqobj[,1])-0.1,length.out = 120)
      xlto=seq(min(wq0ri[,1]),max(wq0ri[,1])-0.1,length.out = 120)


      if(length(wqobj[,1])<102){
        repeat{
          mirror<-wqobj[c(length(wqobj[,1]):1),]
          wqobj<-rbind(wqobj,mirror)
          if(length(wqobj[,1])>=102) break
        }
        wqobj<-round(wqobj,8)
        wqobj<-wqobj[order(wqobj[,1],-wqobj[,2]),]
        wqobj[,1]<-jitter(wqobj[,1])
      }

      ltl<-digit.curves.p(start=wqobj[1,], curve=as.matrix(wqobj), nPoints=98, closed = FALSE)
      ltl1<-ltl
      ltl2<-ltl
      if (pobj[sl]>0.0000001){
        gridx<-(seq(min(ltl[,1],na.rm=TRUE),max(mar1),length.out=100))
        gridy<-(seq(min(ltl[,2],na.rm=TRUE),max(mar2),length.out=100))
        ltl1[,1]<-approx(ltl[,1], ltl[,2], xout = gridx, method = "linear", rule = 1)$x
        ltl1[,2]<-approx(ltl[,1], ltl[,2], xout = gridx, method = "linear", rule = 1)$y

        ltl2[,1]<-approx(ltl[,2], ltl[,1], xout = gridy, method = "linear", rule = 1)$y
        ltl2[,2]<-approx(ltl[,2], ltl[,1], xout = gridy, method = "linear", rule = 1)$x

      }
      ltl1<-data.frame(ltl1,rep(pobj[sl],100),rep(sl,100))
      ltl2<-data.frame(ltl2,rep(pobj[sl],100),rep(sl,100))
      wqobjf<-rbind(wqobjf,ltl1,ltl2)

    }


    plot(wqobjf[,1],wqobjf[,2],col=wqobjf[,4])

    tg=50

    gridx<-seq(min(wqobjf[,1],na.rm=TRUE),max(mar1),length.out=tg)
    gridy<-seq(min(wqobjf[,2],na.rm=TRUE),max(mar2),length.out=tg)


    pxg<-approx(mar1, px, xout = gridx, method = "linear",yleft = min(px),yright = max(px), rule = 1)$y
    pyg<-approx(mar2, py, xout = gridy, method = "linear",yleft = min(py),yright = max(py), rule = 1)$y

    matjt<-matrix(ncol=tg,nrow=tg)
    for (k in 1:(length(pxg)-1)){
      colx<-which(wqobjf[,1]>gridx[k] & wqobjf[,1]<=gridx[k+1])
      for (j in 1:(length(pyg)-1)){
        coly<-wqobjf[colx,3][which(wqobjf[colx,2]>gridy[j] & wqobjf[colx,2]<=gridy[j+1])]
        if(length(coly)==0){matjt[j,k]=NA}else{
          matjt[k,j]=mean(coly,na.rm=TRUE)/(1-pxg[k])}
      }
    }

    for (k in 1:(length(pxg)-1)){
      colx<-which(wqobjf[,2]>gridy[k] & wqobjf[,2]<=gridy[k+1])
      for (j in 1:(length(pyg)-1)){
        coly<-wqobjf[colx,3][which(wqobjf[colx,1]>gridx[j] & wqobjf[colx,1]<=gridx[j+1])]
        if(length(coly)==0){matjt[j,k]=matjt[j,k]}else{
          matjt[j,k]=mean(coly,na.rm=TRUE)/(1-pxg[j])}
      }
    }


    grid <- expand.grid(lon=gridx, lat=gridy)


    for (nap in 1: length(pxg)){ matjt[,nap]<-na.approx(matjt[,nap],maxgap = 5,na.rm=FALSE)}

    for (nap in 1: length(pxg)){ matjt[nap,]<-na.approx(matjt[nap,],maxgap = 5,na.rm=FALSE)}

    levelplot(matjt ~ lon * lat, data=grid, cuts=20, pretty=TRUE,contour=TRUE)
    contour(gridx,gridy,matjt,levels=0.001)

    sh<-contourLines(gridx,gridy,matjt,levels=pobj)
    obx<-c()
    oby<-c()
    for (ssh in 1:length(sh)){
      obx<-c(obx,sh[[ssh]]$x)
      oby<-c(oby,sh[[ssh]]$y)
    }
    plot(obx,oby)



    wqobj<-data.frame(obx,oby)
  }

  res<-list(levelcurve=wqobj,etaJT=etahat,chiJT= Chimed,wq0ri=wq0ri)
}

#' Fits the bivariate conditional extremes model
#'
#'  Fits the bivariate conditional extremes model (from Heffernan and Tawn (2004) and texmex R package) and provides
#'  estimates of a conditional or joint exceedance level curve with a probability corresponding to '\code{pobj}'. Also provides estimates of dependence measures.
#'
#' @param u2 Two column data frame
#' @param tr1 extreme threshold for first variable
#' @param tr2 extreme threshold for second variable
#' @param tsim Prediction quantile. The quantile of the conditioning variable above which it will be simulated for importance sampling based prediction (from texmex)
#' @param num.sim The number of simulated observations to be generated for prediction (from texmex)
#' @param pobj objective joint return period modelled with the conditional extremes model
#' @param interh type of hazard interrelation '\code{comb}' for compound (joint exceedance probability) and '\code{casc}' for cascade (conditional porbability)
#' @param px Uniform values of the first margin with a mixed distribution (empirical below and gpd above a threshold)
#' @param py Uniform values of the second margin with a mixed distribution (empirical below and gpd above a threshold)
#' @param mar1 Values of the first margin
#' @param mar2 Values of the second margin
#' @references Tilloy, A., Malamud, B.D., Winter, H. and Joly-Laugel, A., 2020.
#' Evaluating the efficacy of bivariate extreme modelling approaches
#' for multi-hazard scenarios. Natural Hazards and Earth System Sciences, 20(8), pp.2091-2117.
#' @references Heffernan, J.E. and Tawn, J.A., 2004.
#' A conditional approach for multivariate extreme values (with discussion).
#' Journal of the Royal Statistical Society: Series B (Statistical Methodology), 66(3), pp.497-546.
#' @examples
#' # Import data
#' data(porto)
#' tr1=0.9
#' tr2=0.9
#' fire01meantemp=na.omit(fire01meantemp)
#' u=fire01meantemp
#'
#' #Compute uniform margins
#' marg=Margins.mod(tr1,tr2,u=fire01meantemp)
#' kk=marg$uvar
#' pp=marg$uvar_ext
#' uu=marg$val_ext
#'
#' upobj=0.001
#' t.sim=0.98
#' interh="comb"
#'  \dontrun{
#' # Fit conditional extremes model
#' condexres<-Cond.mod.ap(u2=u,tr1,tr2,tsim=t.sim,num.sim=10000,
#' pobj=upobj,mar1=uu[,1],mar2=uu[,2],px=pp[,1],py=pp[,2],interh=interh)
#'
#'  plot(condexres$jline)
#'  }
#' @importFrom texmex mex
#' @export
#' @return a list containing the following:
#' \itemize{
#' \item jline - data frame of the objective level curve with the selected return period '\code{pobj}'
#' \item onlysim - data frame of simulated extreme data for the two variables
#' \item etaHT - threshold dependent extremal dependence measure
#' \item chiHT - threshold dependent coefficient of tail dependence
#' }
#' @seealso \code{\link[texmex]{mex}}

Cond.mod.ap<-function(u2,tr1,tr2,tsim,num.sim,pobj,interh="comb",mar1,mar2,px,py){
  names(u2)= c("V1","V2")
  thresh1 <- tr1
  thresh2 <- tr2

  ext.q=0.95

  mex.fit <- mex(data = u2 , which = 1, mqu = thresh1, dqu = ext.q, margins = "Laplace", constrain = F)
  mex.fit2 <- mex(data = u2, which = 2, mqu =c(thresh1), dqu = ext.q, constrain=FALSE)


  mex.pred <-predict(mex.fit, pqu = tsim, nsim = num.sim,smoothZdistribution=TRUE)
  mex.pred2<-predict(mex.fit2, pqu = tsim, nsim = num.sim,smoothZdistribution=TRUE)

  #Estimation of the H&T chi
  qexp1<-quantile(u2[,2],tsim)
  l1<-length(mex.pred$data$simulated[which(mex.pred$data$simulated[,2]>qexp1),2])
  chilt1<-l1/num.sim
  tault1<-cor.test(x=mex.pred$data$simulated$V1,y=mex.pred$data$simulated$V2,method="kendall")$estimate
  qexp2<-quantile(u2[,1],tsim)
  l2<-length(mex.pred2$data$simulated[which(mex.pred2$data$simulated[,2]>qexp2),2])
  chilt2<-l2/num.sim
  chiht<-mean(chilt1,chilt2)
  tault2<-cor.test(x=mex.pred2$data$simulated$V1,y=mex.pred2$data$simulated$V2,method="kendall")$estimate
  tauht<-mean(tault1,tault2)
  chibarht=2*log(1-tsim)/log(chiht*(1-tsim))-1
  etaht=(chibarht+1)/2


  #Not used here
  q2<-quantile(u2[,2],tsim)
  q1<-quantile(u2[,1],tsim)
  totsim=num.sim*3/2
  #
  sim1<-mex.pred$data$simulated
  #
  sim2<-mex.pred2$data$simulated
  sim2<-data.frame(mex.pred2$data$simulated$V1,mex.pred2$data$simulated$V2)
  names(sim2)<-names(u2)
  names(sim1)<-names(u2)

  sim1<-sim1[which(sim1$V2<(sim1$V1+q2-q1)),]
  sim2<-sim2[which(sim2$V2>(sim2$V1+q2-q1)),]
  # mex.pred2$data$simulated=sim2
  # mex.pred$data$simulated=sim1
  onlysim<-rbind(sim1,sim2)
  u2<-as.data.frame(u2)
  names(onlysim)=names(u2)
  simu<-rbind(u2,onlysim)
  plot(onlysim)


  pobj1=pobj/(1-tsim)

  if(interh=="casc"){
    simv=sim2#Can also condition on y being gretaer than a threshole
    ngx=10000


    godx<-approx(mar1,px, n = ngx, method = "linear",
                 yleft = min(px), yright = max(px))
    cem.dens<-kcde(simv,gridsize=200, tail.flag = "upper.tail",xmin=c(min(simv[,1]),min(simv[,2])),xmax=c(max(godx$x),max(simv[,2])))


    jesus<-cem.dens$estimate
    lesx<-cem.dens$eval.points[[1]]
    lesy<-cem.dens$eval.points[[2]]


    lexp<-approx(godx$x, godx$y, xout = lesx, method = "linear",yleft = min(px),yright = max(px), rule = 1)$y
    ale<-jesus
    for (k in 1:length(lesy)){
      for (j in 1:length(lesx)){
        ale[j,k]=jesus[j,k]/(1-lexp[j])}}

    plot(u2,xlim=c(0,40),ylim=c(0,300))
    contour(lesx,lesy,ale,levels = c(0.1),col=3,ylim=c(0,200),add=TRUE)

    sh2<-contourLines(lesx,lesy,ale,levels=pobj/(1-tsim))
    obx<-c()
    oby<-c()
    for (ssh in 1:length(sh2)){
      obx<-c(obx,sh2[[ssh]]$x)
      oby<-c(oby,sh2[[ssh]]$y)
    }
    plot(obx,oby)


    jline<-data.frame(obx,oby)
  }


  if(interh=="comb"){
    j1 <- texmex::JointExceedanceCurve(mex.pred,pobj,n=100)
    j2 <- texmex::JointExceedanceCurve(mex.pred2,pobj,n=100)
    summary(mex.pred2)

    j1o<-data.frame(j1[[1]],j1[[2]])
    j2o<-data.frame(j2[[1]],j2[[2]])

    names(j1o)=names(j2o)
    jline<-rbind(j1o[which(j1o[,2]<j1o[,1]+q2-q1),],j2o[which(j2o[,2]>j2o[,1]+q2-q1),])
    jline<-jline[order(jline$j2..1..),]
    jj1<-as.matrix(j1o[which(j1o[,2]<j1o[,1]+q2-q1),])
    jj2<-as.matrix(j2o[which(j2o[,2]>j2o[,1]+q2-q1),])
    mj1<-min(jj1[,1],na.rm=TRUE)
    mj2<-max(jj2[,1])
    mj3<-max(jj1[,1])
    if(mj2>mj1){
      t1<- jj1[which(jj1[,1]>mj1&jj1[,1]<mj2),]
      t2<- jj2[which(jj2[,1]>mj1&jj2[,1]<mj2),]
      if(mj3<mj2)mj2=mj3
      xjj<-seq(mj1,mj2,length.out = 50)
      cj1=approx(jj1[,1], jj1[,2], xout = xjj, method = "linear",yleft = max(jj1[,2]),yright = min(jj1[,2]), rule = 1)$y
      cj2=approx(jj2[,1], jj2[,2], xout = xjj, method = "linear",yleft = max(jj2[,2]),yright = min(jj2[,2]), rule = 1)$y
      cjf<-(cj1+cj2)/2
      jj1<-jj1[order(jj1[,2]),]
      jjcrop<-cbind(xjj,cjf)
      jline<-rbind(jj1[1,],jj1[which(jj1[,1]>=(mj2)),],jjcrop,jj2[which(jj2[,1]<mj1),])
      jline<-jline[order(jline[,1]),]
    }

    idm<-which(jline[,2]==min(jline[,2]))
    jline<-jline[-((idm+1):length(jline[,1])),]
  }
  plot(jline)

  res=list(jline=jline,chiHT=chiht,etaht=etaht,onlysim=onlysim)
}
