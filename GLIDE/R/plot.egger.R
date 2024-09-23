plot.egger <- function(x,exposure_coeff,qcutoff=0.2,xlab="Genetic association with the exposure",
                       ylab="Genetic association with the outcome",...)
{
  x=as.data.frame(x)
  
  yy <- x$g_outcome
  idx=match(rownames(x),rownames(exposure_coeff))
  x$g_exposure=exposure_coeff[idx,1]
  xx <- cbind(1,x$g_exposure)
  ww <- diag(1/x$g_outcome_variance)
  bb <- solve(t(xx) %*% ww %*% xx) %*% t(xx) %*% ww %*% yy
  bbcov<- solve(t(xx) %*% ww %*% xx)
  #SNPs with evidence of direct effect by GLIDE based on the selected q-value cutoff
  selected_idx=which(x$q_value<=qcutoff)
  xmax=max(x$g_exposure)
  xmax1=1.2*xmax
  xmin=min(x$g_exposure)
  xmin1=xmin-0.2*xmax
  if (xmin1<0) xmin1=0
  
  ymax=max(x$g_outcome)
  ymax1=1.2*ymax
  ymin=min(x$g_outcome)
  ymin1=ymin-0.3*ymax

  plot(x$g_exposure,x$g_outcome,xlim=c(xmin1,xmax1),
       ylim=c(ymin1,ymax1),xlab=xlab,ylab=ylab,...)
  abline(bb[1],bb[2],lty=2)
  x1=mean(par("usr")[1:2])+0.25*par("usr")[2]
  y1=mean(par("usr")[3:4])+par("usr")[4]*0.2
  text(x1,y1,paste0("y=",format(bb[1],digits=2),"+",format(bb[2],digits=2),"*x"))
  egger_pvalue=format(2*(1-pnorm(abs(bb[1,1])/sqrt(bbcov[1,1]))),digits = 2)
  legend1=paste0("Egger intercept p-value=",egger_pvalue)
  
  if (length(selected_idx)>0)
  {
    points(x$g_exposure[selected_idx],x$g_outcome[selected_idx],
           col=2,pch=16,cex=1.2*par("cex"))
    legend1=c(legend1,"SNPs with evidence of direct effect by GLIDE")
  }
  if (length(legend1)==2)
  {
    legend("bottomright",legend=legend1,ncol=1,col=c("white","red"),pch=c(3,16))
  }else #no snp was found
  {
    legend("bottomright",legend=legend1,col="white",pch=3)
  }
  
}
