plot.glide <- function(x,qcutoff=0.2,xlab="Expected null p-values (log base 10)",
                       ylab="Observed p-values (log base 10)",...)
{
  x=as.data.frame(x)
  
  
  #SNPs with evidence of direct effect by GLIDE based on the selected q-value cutoff
  selected_idx=which(x$q_value<=qcutoff)
  
  plot(-log(x$expected_pvalue[order(x$observed_pvalue)],base=10),
       -log(x$observed_pvalue[order(x$observed_pvalue)],base=10),
       xlab=xlab,ylab=ylab,type="n",...)
  points(-log(x$expected_pvalue[order(x$observed_pvalue)],base=10),
         -log(x$observed_pvalue[order(x$observed_pvalue)],base=10),...)
  
  if (length(selected_idx)>0)
  {
    points(-log(x$expected_pvalue[order(x$observed_pvalue)][1:length(selected_idx)],base=10),
           -log(x$observed_pvalue[order(x$observed_pvalue)][1:length(selected_idx)],base=10),
           col=2,pch=16,cex=1.2*par("cex"))
    legend("bottomright",legend="SNPs with evidence of direct effect by GLIDE",
           col=2,pch=16)
  }
  abline(0,1,lty=2)
}
