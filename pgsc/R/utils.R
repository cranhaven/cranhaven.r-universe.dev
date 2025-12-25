#####################################################################################
# utils.R
# Provides utility functions: a bootstsrap function and an OLS-conversion function
# Philip Barrett
# Washington, DC, 16sep2018
#####################################################################################

summary.gsc <- function(object, ... ){
  x <- object
  cat( 'Generalized synthetic control estimation, using', x$it, 'iterations \n \n' )
  cat( 'Treatment coefficients:\n' )
  print( x$b )
  cat( '\nMax optimization error = ', x$err )
  cat( '\nMax iteration difference = ', x$diff, '\n' )
}

plot.gsc <- function(x, ... ){
  if(!is.null(x$sig.i)){
    st.names <- if(any(names(x$W)!='' )) names(x$W) else 1:length(x$sig.i)
    barplot( 1/x$sig.i, beside = TRUE, names.arg = st.names, 
             main='Unit-specific weights from first stage', col = 'blue' )
  }
  image(1-t(x$W), col=gray(seq(0,1,length.out=11)) )
      # This needs rotation and 
}

summary.gsc.wald <- function(object, ... ){
  x <- object
  cat( 'Non-linear Wald test of restricted hypothesis, using', length(x$S.boot), 'bootstrapped samples \n \n' )
  cat( 'Restricted coefficients:\n' )
  print( x$b )
  cat( '\nWald statistic = ', x$S )
  cat( '\np-value = ', x$p.val, '\n' )
}

plot.gsc.wald <- function(x, ... ){
  plot( density( x$S.boot ), main='Bootstrap Wald test densities',
        xlab='', ylab='', xlim=range(x$S.boot), lwd=2 )
  abline( v=x$S, col='red', lty=2, lwd=2 )
  legend('topright', c('Bootstrapped Wald density', 'Sample Wald statistic'), 
         lwd=2, lty=1:2, col=c('black','red'), bty='n')
}

# gsc.reg <- function( ols.coeff, dta, start='2005-03-01', stop='2016-12-31', indep.idx=1, dep.idx=NULL, 
#                      indep.var='sec.plm.e.seas', dep.var='rev', gsc.max.it= 100, n.offset=0, lr.test=FALSE,
#                      wt.init=NULL, ret.y=FALSE, ret.d=FALSE ){
# # Applies the gsc method to an OLS regression.  NOT EXPORTED RIGHT NOW
#   n.dep <- length(dep.idx) ; n.indep <- length(indep.idx) ; n.max <- max( n.dep, n.indep )
#       # Number of lags
#   Y <- dep.wide <- acast( subset( dta, as.Date(Date) >= start & as.Date(Date) <=stop ), Unit ~ Date, value.var=dep.var, 
#                           mean, na.rm=TRUE)
#   Y.0 <- Y - Y[,2]
#       # Revenue data.  Scale by second period.
#   NN <- nrow(Y) ; TT <- ncol(Y) ; MM <- n.indep + n.dep
#   # Dimensions
#   sec.wide <- acast( subset( dta, as.Date(Date) >= start & as.Date(Date) <=stop ), Unit ~ Date, 
#                      value.var=indep.var, mean, na.rm=TRUE)
#   # The independent variable
#   D <- array( unlist(lapply( 1:(TT-n.max-n.offset), function(i) 
#     cbind(sec.wide[,i+n.max+1-1:n.indep], Y.0[,i+n.max-1:n.dep]) )), dim=c(NN, MM, TT-n.max-n.offset) )
#   # The "treatment" variable
#   if(ret.y) return(Y.0[,-(1:(n.max+n.offset))])
#   if(ret.d) return(D)
#   # Return Y and D
#   b.init <- ols.coeff[c( indep.idx, dep.idx ) ]
#   if(lr.test){
#     g.i <- function(b) if(n.dep>0) sum(b[indep.idx]) / ( 1 - sum(b[dep.idx]) ) else sum(b[indep.idx])
#     g.i.grad <- function(b) if(n.dep>0) c( rep(1,n.indep) / ( 1 - sum(b[dep.idx]) ), 
#                                            rep( sum(b[indep.idx]), n.dep ) / ( 1 - sum(b[dep.idx]) )^2 ) else rep(1,n.indep)
#     sol.it <- pgsc( Y.0[,-(1:(n.max+n.offset))], D, b.init, 'onestep', max.it=gsc.max.it, 
#                            g.i=g.i, g.i.grad=g.i.grad )
#     sol.2.step.agg <- pgsc( Y.0[,-(1:(n.max+n.offset))], D, b.init, 'twostep.aggte', sol.it, 
#                                    max.it = gsc.max.it, g.i=g.i, g.i.grad=g.i.grad )
#     sol.2.step.indiv <- pgsc( Y.0[,-(1:(n.max+n.offset))], D, b.init, 'twostep.indiv', sol.it, 
#                                      max.it = gsc.max.it, g.i=g.i, g.i.grad=g.i.grad )  
#   }else{
#     sol.it <- pgsc( Y.0[,-(1:(n.max+n.offset))], D, b.init, 'onestep', max.it=gsc.max.it )
#     sol.2.step.agg <- pgsc( Y.0[,-(1:(n.max+n.offset))], D, b.init, 'twostep.aggte', sol.it, max.it = gsc.max.it )
#     sol.2.step.indiv <- pgsc( Y.0[,-(1:(n.max+n.offset))], D, b.init, 'twostep.indiv', sol.it, max.it = gsc.max.it )
#   }
#   # The solutions
#   sol.compare <- rbind(  b.init, sol.it$b, sol.2.step.agg$b, sol.2.step.indiv$b )
#   rownames(sol.compare) <- c( 'Initial guess', 'GSC iter', 'Two-step GSC', 'Optimal two-step GSC')
#   return( list( sol.compare=sol.compare, b.init=b.init, sol.it=sol.it, 
#                 sol.2.step.agg=sol.2.step.agg, sol.2.step.indiv=sol.2.step.indiv ) )
# }
