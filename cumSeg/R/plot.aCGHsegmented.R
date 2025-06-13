# plot.aCGHsegmented <-
# function(x, add=FALSE, y=TRUE, psi.lines=TRUE, ...){
# #se y=TRUE disegna anche le osservazioni e add e'ignorato
#         if(y) add<-FALSE
#         arg<-list(...)
#         if(is.null(arg$col)) arg$col=1
#         if(is.null(arg$lwd)) arg$lwd=1
#         if(is.null(arg$lty)) arg$lty=1
#         if(is.null(arg$xlab)) arg$xlab<-"Genome Location"
#         if(is.null(arg$ylab)) arg$ylab<-"LogRatio"
#         
#         yy<-x$y
#         n<-length(yy)
#         if(x$n.psi<=0) {
#             if(y){
#               plot(yy, ylab=arg$ylab, xlab=arg$xlab, pch=20, col=grey(.7))
#               abline(h=x$est.means, ...)
#               } else {
#                 if(add) {abline(h=x$est.means, ...)} else {
#                   plot(yy, ylab=arg$ylab, xlab=arg$xlab, type="n")
#                   abline(h=x$est.means, ...) }
#                   }
#           return(invisible(NULL))
#           }
#         #coll=arg$col
#         psi<-x$psi
#         est.means<-x$est.means
#         if(y) {
#             plot(yy, ylab=arg$ylab, xlab=arg$xlab, pch=20, col=grey(.7))
#             points(c(1,psi,n), c(est.means,est.means[length(est.means)]),
#               type="s",...)
#               } else {
#         if(!add) {
#           plot(c(1,psi,n), c(est.means,est.means[length(est.means)]),
#               type="s",ylab=arg$ylab, xlab=arg$xlab, col=arg$col, lwd=arg$lwd, lty=arg$lty) 
#               } else {
#           points(c(1,psi,n), c(est.means,est.means[length(est.means)]),
#               type="s",col=arg$col, lwd=arg$lwd, lty=arg$lty)
#               }
#           }
#           if(psi.lines){
#             segments(x0=psi,y0=par()$usr[3],x1=psi,y1=est.means[-1],lty=3 ,col=arg$col)
#             points(psi,rep(par()$usr[3],length(psi)),pch=19 ,col=arg$col)
#             }
#           invisible(NULL)
#             }

plot.aCGHsegmented <- function(x, add=FALSE, y=TRUE, psi.lines=TRUE, typeL="l", what=c("lines","criterion"),...){
#typeL if "s"..        
#y: ignored if add=TRUE
#x: 
                what<-match.arg(what, c("lines","criterion"))
                if(what=="criterion"){
                  if(length(x$criterion)<=0) stop("The object does not include the criterion values (set 'output' to '2' or '3' in jumpoints())")
                  plot(0:(length(x$criterion)-1), x$criterion, type="b", xlab="No. of change points", ylab="Criterion values", lwd=1.5, xaxt="n")
                  axis(1, at=0:(length(x$criterion)-1), cex.axis=.8)
                  points(which.min(x$criterion)-1, min(x$criterion), pch=19)
                  return(invisible(NULL))
                }
                typeL<-match.arg(typeL, c("l","s"))
                arg<-list(...)
                if(is.null(arg$col)) arg$col=1
                if(is.null(arg$lwd)) arg$lwd=1
                if(is.null(arg$lty)) arg$lty=1
                if(is.null(arg$xlab)) arg$xlab<-"Genome Location"
                if(is.null(arg$ylab)) arg$ylab<-"LogRatio"
                
                yy<-x$y
                xx<- if(is.null(x$x)) 1:length(yy) else x$x
                n<-length(yy)
                if(x$n.psi<=0) {
                        if(y){
                                plot(xx, yy, ylab=arg$ylab, xlab=arg$xlab, pch=20, col=grey(.7))
                                abline(h=x$est.means, ...)
                        } else {
                                if(add) {abline(h=x$est.means, ...)} else {
                                        plot(yy, ylab=arg$ylab, xlab=arg$xlab, type="n")
                                        abline(h=x$est.means, ...) }
                        }
                        return(invisible(NULL))
                      }
                #coll=arg$col
                psi<-x$psi
                est.means<-x$est.means
                m<-min(x$rangeX)
                M<-max(x$rangeX)
                Y<-rep(est.means,each=2)
                X<-c(m,rep(psi, each=2) ,M)
                id1<-seq(1,length(X), by=2)
                id2<-seq(2,length(X), by=2)
                if(!add){
                        if(y) plot(xx,yy , ylab=arg$ylab, xlab=arg$xlab, pch=20, col=grey(.7))
                                else  plot(xx,yy , ylab=arg$ylab, xlab=arg$xlab, type="n")
                }
                if(typeL=="s") points(X[id1],Y[id1],X[id2],Y[id2], col=arg$col, lwd=arg$lwd, lty=arg$lty, type="s") 
                        else segments(X[id1],Y[id1],X[id2],Y[id2], col=arg$col, lwd=arg$lwd, lty=arg$lty)
                if(psi.lines){
                        segments(x0=psi,y0=par()$usr[3],x1=psi,y1=apply(matrix(Y[-c(1,length(Y))],ncol=2, byrow=TRUE),1,max),lty=3 ,col=arg$col)
                        points(psi,rep(par()$usr[3],length(psi)),pch=19 ,col=arg$col)
                }
                invisible(NULL)
        }
