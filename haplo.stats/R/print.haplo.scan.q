#$Author: sinnwell $
#$Date: 2005/03/29 14:18:34 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/print.haplo.scan.q,v 1.4 2005/03/29 14:18:34 sinnwell Exp $
#$Locker:  $
#$Log: print.haplo.scan.q,v $
#Revision 1.4  2005/03/29 14:18:34  sinnwell
#fix call again
#
#Revision 1.3  2005/03/28 22:22:11  sinnwell
#print the call to fit width, different in R and Splus
#
#Revision 1.2  2005/03/23 23:13:34  sinnwell
#print max.loc
#
#Revision 1.1  2005/03/23 18:07:08  sinnwell
#Initial revision
#


print.haplo.scan <- function(x, digits=max(options()$digits-2, 5), ...)
  # print a haplo.scan object
{
  scan.df <- as.data.frame(x$scan.df)

  loc.label <- paste("loc", 1:ncol(scan.df), sep='-')
  dimnames(scan.df) <- list(c("stat.obs", "sim.p-val"), loc.label) 
  x$call <- deparse(x$call, width.cutoff=40)
  cat("\n  Call: ", x$call, sep="\n")

  cat("\n")
  
  printBanner("Locus Scan-statistic Simulated P-values")

  print(scan.df[2,,drop=FALSE], digits=digits)

  cat("\n\n")
  cat("      Loci with max scan statistic:  ", x$max.loc, "\n")
  cat(" Max-Stat Simulated Global p-value:  ", signif(x$globalp,digits), "\n",
       "            Number of Simulations:  ", x$nsim, "\n")

  invisible()

}
