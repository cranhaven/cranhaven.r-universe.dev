
#' Identifies synthetic datasets analig to input data
#'
#' Automatically identifies analog datasets from the 60 datasets created
#' in Tilloy et al.(2020). The function helps the user to select relevant
#' bivariate model among the six models discussed in Tilloy et al. (2020)
#' See https://nhess.copernicus.org/articles/20/2091/2020/nhess-20-2091-2020.html for more detail.
#'
#' @param u2 Two column data frame
#' @export
#' @importFrom texmex chi
#' @importFrom ggplot2 ggplot
#' @return No return value, called for side effects
#' @examples
#' data(porto)
#' AnalogSel(fire01meantemp)
AnalogSel<-function(u2){
  chiest<-chi(u2,qlim=c(0.75,0.92),nq=100)
  ggplot(chiest)
  plot(chiest$quantile,chiest$chi[,2])
  abline(h=mean(chiest$chi[,2]))
  chiest$chiulb
  etaest=(chiest$chibar+1)/2

  chint<-round(c(min(chiest$chi[,1]),mean(chiest$chi[,2]),max(chiest$chi[,3])),2)
  etint<-round(c(min(etaest[,1]),mean(etaest[,2]),max(etaest[,3])),2)

  etalist<-c(0.25,0.5,0.75,0.9)
  chilist<-c(0.05,0.1,0.3,0.5,0.9)
  et<-c()
  ct=c()
  et<-c(etalist[which(etalist>=etint[1]&etalist<=etint[3])])
  ct<-c(chilist[which(chilist>=chint[1]&chilist<=chint[3])])
  if(chint[1]>0.1)et<-c()
  message(paste(c("analogous datasets to be tested: \n eta = ",et," \n chi = ",ct),collapse= " "))
  AnSel<-list(et=et,ct=ct)
}



#' Compute uniform margins with Generalized Pareto Distribution above threshold
#'
#' @param u Two column data frame
#' @param tr1 extreme threshold for first variable
#' @param tr2 extreme threshold for second variable
#' @importFrom texmex evm
#' @export
#' @return a list of containing the following pseudo observations (uniform margins) with a mixed distribution (empirical below and gpd above a threshold)
#' \itemize{
#' \item uvar - data frame of pseudo observations (uniform margins) of the original data '\code{u}'
#' \item uvar_ext - data frame of pseudo observations (uniform margins) with a mixed distribution (empirical below and gpd above a threshold) and 1000 extrapolated values
#' \item val_est - data frame consiting of mix of original data '\code{u}'and 1000 extrapolated values
#' }
#' @examples
#' data(porto)
#' #set extreme threshold for both variable
#' tr1=0.9
#' tr2=0.9
#' fire01meantemp=na.omit(fire01meantemp)
#' u=fire01meantemp
#' marmod=Margins.mod(tr1,tr2,u=fire01meantemp)

Margins.mod<-function(tr1,tr2,u)
{
  th1=quantile(u[,1],tr1,na.rm = T)
  th2=quantile(u[,2],tr2,na.rm = T)

  idr<-u[which(u[,2]>th2),2]



  gpdN1<-evm(u[,1],family=gpd,qu=tr1,start=c(1,1))

  gpdN2<-evm(u[,2],family=gpd,qu=tr2,start=c(1,1))

  id<-seq(1,length(u[,2]))
  u3<-cbind(u,id)

  parv1n<-gpdN1$par
  parv1n[1]<-exp(parv1n[1])
  parv2n<-gpdN2$par
  parv2n[1]<-exp(parv2n[1])

  pxt1<-texmex::pgpd(u[which(u[,1]>th1+1),1], parv1n[1], xi=parv1n[2], u = as.numeric(th1), lower.tail = T, log.p = F)
  pxt1=pxt1*(1-tr1)+tr1


  pyt1<-texmex::pgpd(u[which(u[,2]>th2+1),2], parv2n[1], xi=parv2n[2], u = as.numeric(th2), lower.tail = T, log.p = F)
  pyt1=pyt1*(1-tr2)+tr2


  epdata <- apply(u, 2, rank, na.last = "keep")
  nasm <- apply(u, 2, function(x) sum(!is.na(x)))
  epdata <-  epdata/rep(nasm + 1, each = nrow(epdata))
  tooLow1 <- which(u[,1] <= min(u[,1]))
  tooLow2 <- which(u[,2] <= min(u[,2]))
  epdata[tooLow1,1] <- 0
  epdata[tooLow2,2] <- 0
  px<-epdata[,1]
  tr1<-approx(u[,1], px, xout = th1, method = "linear",yleft = max(px),yright = min(px), rule = 1)$y
  py<-epdata[,2]
  tr2<-approx(u[,2], py, xout = th2, method = "linear",yleft = max(py),yright = min(py), rule = 1)$y



  interw<-1-(1-tr1)*(1+parv1n[2]*((u[which(u[,1]>th1 & u[,1]<=th1+1),1]-th1)/parv1n[1]))^(-1/parv1n[2])
  interr<-1-(1-tr2)*(1+parv2n[2]*((u[which(u[,2]>th2 & u[,2]<=th2+1),2]-th2)/parv2n[1]))^(-1/parv2n[2])
  ww=seq(0,1,length=length(interw))
  wr=seq(0,1,length=length(interr))

  empix<-px[which(u[,1]>th1 & u[,1]<=th1+1)]
  empiy<-py[which(u[,2]>th2 & u[,2]<=th2+1)]

  interdw<-((1-ww)*empix+ww*interw)
  interdr<-((1-wr)*empiy+wr*interr)

  pxx<-px[-which(u[,1]>th1)]
  pyy<-py[-which(u[,2]>th2)]

  pxf<-as.numeric(c(pxx,interdw,pxt1))
  pyf<-as.numeric(c(pyy,interdr,pyt1))
  plot(pyt1)

  pxp<-c(seq(0.999,0.999999,by=0.000001))

  u1p<-SpatialExtremes::qgpd(pxp, loc=th1, scale=parv1n[1], shape=parv1n[2], lower.tail = T,lambda=tr1)

  pyp<-c(seq(0.999,0.999999,by=0.000001))
  u2p<-SpatialExtremes::qgpd(pyp, loc=th2, scale=parv2n[1], shape=parv2n[2], lower.tail = TRUE,lambda=tr2)

  u1b<-c(u[,1],u1p)
  u2b<-c(u[,2],u2p)


  pxo<-c(u3[which(u3[,1]<=th1),3],u3[which(u3[,1]>th1 & u3[,1]<=th1+1),3],u3[which(u3[,1]>th1+1),3])
  pxf<-pxf[order(pxo)]
  px<-px[order(pxo)]
  plot(pxf)

  pyo<-c(u3[which(u3[,2]<=th2),3],u3[which(u3[,2]>th2 & u3[,2]<=th2+1),3],u3[which(u3[,2]>th2+1),3])
  pyf<-pyf[order(pyo)]
  plot(pyf)

  plot(px[order(px)],pxf[order(pxf)],xlim=c(0.9,1),ylim=c(0.9,1),type="p")
  segments(x0=0,y0=0,x1=1,y1=1,col=2)
  plot(pxf[order(pxf)])


  pxfp<-as.numeric(c(pxf,pxp))
  pyfp<-as.numeric(c(pyf,pyp))

  plot(pxf,u[,1])
  plot(pyf,u[,2])

  plot(pxfp,u1b)
  plot(pyfp,u2b)

  uvar<-data.frame(pxf,pyf)
  uvar_ext<-data.frame(pxfp,pyfp)
  val_ext<-data.frame(u1b,u2b)

  return(list("uvar"=uvar,"uvar_ext"=uvar_ext,val_ext=val_ext))
}



#' Computes the density of level curves for non-parametric models
#'
#' Computes the density along the level curve estimated by JT-KDE or Cond-Ex models.
#' It is based on a kernel density estimation of simulated points for Cond-Ex and extrapolation of the kernel density estimation
#' of the base curve for the joint tail model
#'
#' @param kdetab a table of dimentsion k*k representing the bivariate density of
#' the data estimated with a kernel density estimator
#' @param lines location of the objective level curve for which the deesnity needs to be estimated in the 2D space
#' @param tl indicator which model's density have been estimated in the kdetab, '\code{l}' the joint tail model, '\code{h}' for the conditional extremes model
#' @param lines2 location of the base level curve (only used when tl=l)
#' @export
#' @examples
#'   \dontrun{
#'  data(porto)
#'  fire01meantemp=na.omit(fire01meantemp)
#'  u=fire01meantemp
#'  jt.dens<-kde(u,gridsize = 200)
#' ltl<-densi.curv.em(kdetab=jt.dens,lines2=ltlo, tl="l", lines=ltl)
#' }
#' @return density for each points (couple x,y) along the level curves
densi.curv.em<-function(kdetab,lines,tl,lines2){
  densim<-c()
  aba<-kdetab$estimate/sum(kdetab$estimate)
  for (stro in 1:100){
    pwin<-as.matrix(lines[stro,])
    onarray<-aba[which(abs(kdetab$eval.points[[1]]-pwin[1])==min(abs(kdetab$eval.points[[1]]-pwin[1]))),which(abs(kdetab$eval.points[[2]]-pwin[2])==min(abs(kdetab$eval.points[[2]]-pwin[2])))]
    if(length(onarray)==0)onarray<-aba[which(round(kdetab$eval.points[[1]],1)==round(pwin[1],1)),which(round(kdetab$eval.points[[2]],1)==round(pwin[2],1))]
    if(length(onarray)>1)onarray=mean(onarray)
    densim<-c(densim,onarray)

  }
  dm<-sum(densim)
  densim<-densim/dm
  posoncurve<-seq(1/length(densim),1,length=length(densim))
  plot(posoncurve,densim, type="h")
  if (tl=="l"){
    exit<-data.frame(lines2,densim)}
  if (tl=="h"){
    exit<-data.frame(lines,densim)}
  names(exit)=c("x","y","dens")
  exit
}



#' Computes the density of level curves for copulae models
#'
#' Computes the density along the level curve estimate with a copula.
#' Based on the density function of the selected copula.
#'
#' @param lines location of the objective level curve for which the density needs to be estimated in the 2D space
#' @param copi a copula function with the parameters fitted to the bivariate dataset
#' @param pxf uniform values of the 1st margin
#' @param pyf uniform values of the 1st margin
#' @param u original data
#' @export
#' @return density for each points (couple x,y) along the level curves for copulae
#' @examples
#' data(porto)
#'
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
#' #Copula parameters
#' c1=1.5
#' copu<-copBasic::GHcop
#' upobj=0.001
#' interh="comb"
#'
#' #compute the curve on 3 subdomains
#' cl1<-curve.funct(pxf=pp[,1],pyf=pp[,2],mar1=uu[,1],mar2=uu[,2],pos="l",
#' pobje=upobj,ng=100,inter=interh,coco=copu,c1=c1)
#' cl2<-curve.funct(pxf=pp[,1],pyf=pp[,2],mar1=uu[,1],mar2=uu[,2],pos="m",
#' pobje=upobj,ng=100,inter=interh,coco=copu,c1=c1)
#' cl3<-curve.funct(pxf=pp[,1],pyf=pp[,2],mar1=uu[,1],mar2=uu[,2],pos="r",
#' pobje=upobj,ng=100,inter=interh,coco=copu,c1=c1)
#'
#' cl<-rbind(cl1,cl2,cl3)
#'
#' # Homogenization of the number of points
#' cli<-digit.curves.p(start=c(cl[1,1],cl[1,2]), as.matrix(cl), nPoints=98, closed = FALSE)
#'
#' # Computes the density along the curve
#' co=copula::gumbelCopula(c1,dim=2)
#' cli<-densi.curv.cop(lines=cli,copi=co,pxf=kk[,1],pyf=kk[,2],u=u)
#' @seealso \code{\link[copula]{dcopula}}
#' @importFrom copBasic GHcop
densi.curv.cop<-function(lines, copi,pxf,pyf,u){
  linep<-lines
  linep[,1]<-spline(u[,1],pxf, n = 300, method = "fmm",
                    xmin = min(lines[,1]), xmax = max(lines[,1]), ties = mean,xout=lines[,1])$y
  linep[,2]<-spline(u[,2],pyf, n = 300, method = "fmm",
                    xmin = min(lines[,2]), xmax = max(lines[,2]), ties = mean,xout=lines[,2])$y
  shit<-copula::dCopula(u=as.matrix(data.frame(linep)), copula =copi)
  shma<-sum(shit)
  shit<-shit/shma
  posonshit<-seq(1/length(shit),1,length=length(shit))
  plot(posonshit,shit, type="h")
  shuba<-data.frame(lines,shit)
  names(shuba)=c("x","y","dens")
  shuba
}

utils::globalVariables(c("gpd"))
