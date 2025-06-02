#' A function rather aimed at developers
#' @import baseline splines
#' @noRd


myBaseline = function(specDat, bsDf=5, BL_method="modpolyfit", RsquareCut=0.2){
  ##### test if we really need baseline, if so, go with baseline correction,
  #####    otherwise skip it, and just return the input data
  #### specDat is a vector of freq domain data, should be phased before using this function
  #### bsDf is the degree of freedom for B-spline, my default is set as 5
  #### BL_method: method for baseline correction of function "baseline", here I set default as "modpolyfit"

  lowTmp=stats::lowess(specDat)

  sp=stats::lm(splines::bs(lowTmp$y,df=bsDf)~lowTmp$x)  ## there are five R quare!!!

  out=summary(sp)
  qs=sapply(out,FUN=function(x){
    x$adj.r.squared
  })

  if(max(qs)>RsquareCut){
    tryBL=baseline::baseline(t(specDat),method=BL_method)
    specDat=baseline::getCorrected(tryBL)
  }
  return(specDat)
}
