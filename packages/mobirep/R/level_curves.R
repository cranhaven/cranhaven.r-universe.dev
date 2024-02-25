#' Creates extreme level curves for copulae
#'
#' The function creates extreme level curves for copulae. The function divides the 2D space
#' into 3 subspaces to model level curves with extreme low probability '\code{pobj}' (<0.001). Two types of level
#' curves can be modeled: conditional or joint exceedance.
#'
#' @param pxf Uniform values of the first margin with a mixed distribution (empirical below and gpd above a threshold)
#' @param pyf Uniform values of the second margin with a mixed distribution (empirical below and gpd above a threshold)
#' @param mar1 Values of the first margin
#' @param mar2 Values of the second margin
#' @param pos part of the curve to be modelled '\code{l}' for the left part, '\code{m}' for the middle part and '\code{r}' for the right part
#' @param pobje probability of the level curve to be modelled
#' @param ng number of points to be interpolated
#' @param coco a copula function from the following: GHcop,NORMcop,FGMcop,GLcop
#' @param c1 the parameter of the copula
#' @param logm log tranformation of the margins '\code{TRUE}'or '\code{FALSE}'
#' @param inter  type of hazard interrelation '\code{comb}' for compound (joint exceedance probability) and '\code{casc}' for cascade (conditional porbability)
#' @export
#' @return two column matrix representing the level curve for a given probability
#' @examples
#' data(porto)
#' tr1=0.9
#' tr2=0.9
#' fire01meantemp=na.omit(fire01meantemp)
#' u=fire01meantemp
#'
#' #Compute uniform margins
#' marg=Margins.mod(tr1,tr2,u=fire01meantemp)
#' pp=marg$uvar_ext
#' uu=marg$val_ext
#'
#' #Copula parameters
#' c1=1.5
#' copu<-copBasic::GHcop
#'
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
#' @importFrom grDevices contourLines
#' @importFrom copBasic surfuncCOP GHcop
#' @importFrom graphics abline contour segments

curve.funct<-function(pxf,pyf,mar1,mar2,pos,pobje,ng=100,inter="comb",coco,c1,logm=FALSE){
  if(logm==TRUE) {
    mar1<-log(mar1)
    mar2<-log(mar2)
  }

  if(pos=="l"){
    xmin=0
    xmax=0.99
    ymin=0.99
    ymax=1
  }
  if(pos=="m"){
    xmin=0.98
    xmax=1
    ymin=0.98
    ymax=1
  }
  if(pos=="r"){
    xmin=0.99
    xmax=1
    ymin=0
    ymax=0.99
  }
  ngx=10000
  godx<-spline(pxf,mar1, n = ngx, method = "natural",
               xmin = xmin, xmax = xmax, ties = mean)

  gody<-spline(pyf,mar2, n = ngx, method = "natural",
               xmin = ymin, xmax = ymax, ties = mean)

  coxi<-approx(godx$y,godx$x, n = ng, method = "linear",
               yleft = 0, yright = 1, ties = mean)

  coyi<-approx(gody$y,gody$x, n = ng, method = "linear",
               yleft = 0, yright = 1, ties = mean)
  plot(coxi$x,coxi$y)
  godx<-coxi$y
  gody<-coyi$y
  coxi<-coxi$x
  coyi<-coyi$x

  repeat{
    idx<-which(diff(coxi)<=0)
    if(length(idx)<1){
      break
    }
    coxi[idx+1]=coxi[idx]+0.001

  }
  repeat{
    idy<-which(diff(coyi)<=0)
    if(length(idy)<1){
      break
    }
    coyi[idy+1]=coyi[idy]+0.001

  }


  acp3<-matrix(NA, nrow = ng, ncol = ng)
  for (k in 1:length(gody)){
    for (j in 1:length(godx)){
      if (inter=="comb"){
        acp3[j,k]=surfuncCOP(godx[j], gody[k], cop=coco, para=c1)}
      if (inter=="casc"){
        acp3[j,k]= surfuncCOP(godx[j], gody[k], cop=coco, para=c1)/(1-godx[j])}

    }
  }
  grid <- expand.grid(lon=godx, lat=gody)
  levelplot(acp3 ~ lon * lat, data=grid, cuts=20, pretty=TRUE)
  cl2<-contourLines(coxi,coyi, acp3, levels = pobje)
  if(length(cl2)>0){
    cl2<-as.matrix(data.frame(cl2[[1]]$x,cl2[[1]]$y))} else{cl2<-NA}

}
