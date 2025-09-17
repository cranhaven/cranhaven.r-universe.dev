#' Plot stPredict objects
#'
#' This function prints basic summary info for telefit stPredict objects
#' 
#'
#' @export
#' 
#' @import foreach
#' 
#' @param object Object of class stPredict to summarise
#' @param t timepoint to plot.  Will automatically plot all timepoints and overall
#'  summary if NULL.
#' @param digits Number of digits to pass to signif, if not NULL.
#' @param ... S3 generic/method consistency
#'
#' @examples
#' 
#' data("coprecip.predict")
#' summary(coprecip.predict)
#' 

summary.stPredict = function( object, t=NULL, digits=NULL, ... ) {
  stPredict = object
  if(is.null(t)) {
    cat('Overall fit errors\n\n')
    
    r = data.frame(attributes(stPredict)[-(1:2)])
    colnames(r) = gsub('err\\.', '', colnames(r))
    
    if(!is.null(digits))
      r = signif(r, digits)
    
    print( r, row.names = F )
    
    cat('\n\n')
    
    cat('Fit errors\n\n')
    
    r = foreach(f=stPredict$pred, .combine = 'rbind') %do% {
      data.frame(t=f$yrLab, f$err)
    }
    r = r[order(r$t),]
    
    if(!is.null(digits))
      r = cbind(t=r$t, signif(r[,-1], digits))
    
    print( r, row.names = F )
    
  } else {
    tind = match(t, stPredict$tLabs)
    
    cat('Fit errors\n\n')
    
    r = data.frame(t=t, stPredict$pred[[tind]]$err)
    
    if(!is.null(digits))
      r = cbind(t=r$t, signif(r[,-1], digits))
    
    print( r, row.names = F )
  }
  
  
}