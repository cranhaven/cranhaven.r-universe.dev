

#' Plot \code{crs} objects
#'
#' @description Plots results of correction (1st plot: estimated response functions, 2nd plot: coefficient plot. See Appendix A of the reference paper for the 2nd plot).
#' @details Correction results for each respondent are displayed. If either response-style-based clusters or the number of response-style-based clusters are specified, the correction results of response-style-based clusters are displayed.
#' @param x An object of class \code{crs}.
#' @param H An integer indicating the number of response-style-based clusters to display the correction result. If \code{H=NULL} and \code{cls.rs.vec=NULL}, \code{H} is set as \code{H=n}. If \code{H=NULL} but \code{cls.rs.vec!=NULL}, \code{H} is set as \code{H=max(cls.rs.vec)}. The default is \code{H=NULL}.
#' @param cls.rs.vec An integer vector of length n indicating response-style-based clusters for n respondents. If \code{cls.rs.vec=NULL} and \code{H!=NULL}, clusters are determined by k-means clustering of Beta. The default is \code{cls.rs.vec=NULL}.
#' @param \dots Additional arguments passed to \code{\link{plot}}.
#' @seealso \code{\link{ccrs}}
#' @references Takagishi, M., Velden, M. van de & Yadohisa, H. (2019). Clustering preference data in the presence of response style bias, to appear in British Journal of Mathematical and Statistical Psychology.
#' @importFrom colorspace rainbow_hcl
#' @importFrom graphics par matplot text lines plot
#' @importFrom stats kmeans
#' @method plot crs
#' @keywords utility
#' @export
#' @examples
#' ###data setting
#' n <- 30 ; m <- 10 ; H.true <- 2 ; K.true <- 2 ; q <- 5
#' datagene <- generate.rsdata(n=n,m=m,K.true=K.true,H.true=H.true,q=q,clustered.rs = TRUE)
#' ###obtain n x m data matrix
#' X <- datagene$X
#' ccrsdata.list <- create.ccrsdata(X,q=q)
#' crs.list <- correct.rs(ccrsdata.list)
#' ###You can check correction result using this \code{crs.plot} function.
#' plot(crs.list)
#'
#' #####You can also check correction result obtained
#' #####by a simultaneous analysis of correction and content-based clustering.
#' ###CCRS
#' lam <- 0.8 ; K <- 2
#' ccrs.list <- ccrs(ccrsdata.list,K=K,lam=lam)
#' ###check correction result using this \code{crs.plot} function.
#' plot(ccrs.list$crs.list)


plot.crs <- function(x, H=NULL, cls.rs.vec=NULL,...){

  oldpar <- par(ask = TRUE)
  on.exit(par(oldpar))

  MB <- x$MB
  Beta <- x$Beta
  q <- ncol(MB)
  n <- nrow(MB)

  if(is.null(H)) {
    if(is.null(cls.rs.vec)){
      #cat("H is not specified, so the number of response styles H is set to H=n.\n")
      cls.rs.vec <- seq(1,n) ; H <- n
    }else{#if cls.rs.vec is specified while H=NULL
      H <- max(cls.rs.vec)
    }

  }else{#if H is specified
    if(is.null(cls.rs.vec)){
      cat("response-style-based clusters are determined by k-means.\n")
      cls.rs.vec <- kmeans(Beta,H,nstart = 100)$cluster
    }
  }


  #####take mean by response style cluster
  MB.h <- apply(MB,2,function(x)tapply(x,cls.rs.vec,mean))
  Beta.h <- apply(Beta,2,function(x)tapply(x,cls.rs.vec,mean))
  col.vec <- rainbow_hcl(H)

  ####response functions#####
  x.bounds.data <- (c(1:q) - 0.5)/q#ifelse(scaling,q,1)
  matplot(x.bounds.data,t(MB.h),col=col.vec,type="l",lty=1,ylab="response functions",xlab="boundaries"#,ylim=c(0,ylim.max)
          ,main="estimated response functions")

  ####beta plot#######
  xlimm.al <- ylimm.al<- c(-0.001,1)
  xlab.al <- expression(beta[1]) ; ylab.al <- expression(beta[3])
  #betaplot <- function(x){c((x[1]),(x[3]))}
  #textcord<-0.25

  plot(Beta[1,][1],Beta[1,][3],xlab=xlab.al,ylab=ylab.al,type="n"
       ,xlim=xlimm.al,ylim=ylimm.al,main="coefficient plot",...)#,pch=as.character(h))#,cex=cex.h,
       #cex.axis=cex.h-0.3,cex.lab=cex.h) #,main=mainname
  for(h in 1:H){
    beta.h <- Beta[h,]
    text(beta.h[2],beta.h[4],h,col=col.vec[h]) #1st element is coefficient
  }

  lines(c(0,1),c(1,0),lty=2,col=1)# ,lwd=lwd.h)


}
