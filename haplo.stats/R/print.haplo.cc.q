#$Author: sinnwell $
#$Date: 2005/03/30 16:40:08 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/print.haplo.cc.q,v 1.3 2005/03/30 16:40:08 sinnwell Exp $

print.haplo.cc <- function(x, order.by=c("score","haplotype","freq"),
                           digits=max(options()$digits-2, 5), nlines=NULL, ...)
{

  if (!inherits(x, 'haplo.cc'))
    stop("Not an object of class haplo.cc!")
  
    # Combine haplotypes and results
    # round numeric columns to set length digits
  n.loci <- ncol(x$score.lst$haplotype)
  df.out <- x$cc.df
 
 # print of global score stats:
   printBanner("Global Score Statistics", border= "-")
   cat(paste("global-stat = ",signif(x$score.lst$score.global,digits),", df = ", x$score.lst$df,
             ", p-val = ",signif(x$score.lst$score.global.p,digits),sep=""))

   # print separate section for sim p.vals and the conditions
   # under which they were made
 
  cat("\n\n")
   # print haplo.score simulation information
  if(x$score.lst$simulate) {
    printBanner("Global Simulation p-value Results", border="-")
    cat("Global sim. p-val = ",signif(x$score.lst$score.global.p.sim, digits),"\n")
    cat("Max-Stat sim. p-val = ",signif(x$score.lst$score.max.p.sim, digits), "\n")
    cat("Number of Simulations, Global: ", x$score.lst$n.val.global, ", Max-Stat:", x$score.lst$n.val.haplo)
    cat("\n\n")
  }

   # print counts for the two groups
  printBanner("Counts for Cases and Controls", border = "-")
  print(x$group.count)
  cat("\n\n")
  
  # print a banner for the data frame
  #printBanner(paste("Haplotype Scores, p-values, Hap-Frequencies (hf), and Odds Ratios (",
  #                   round(x$ci.prob*100, 0), "% CI)", sep=""), border = "-")

  # get the order and choose all.haps to print or not
  if(length(order.by)>1) order.by=order.by[1]    
  order.vec <- c("haplotype","score","freq")
  order.int <- pmatch(order.by, order.vec)
  if(all(is.na(order.int))) order.int <- 1
  order.by <- order.vec[order.int]
  switch(order.by,
         score = {
           ord <- (1:nrow(x$cc.df))[order(x$cc.df$"Hap-Score")]
         },
         freq = {
           ord <- (order(x$cc.df$"pool.hf"))[order(nrow(x$cc.df):1)]
         },
         haplotype = {
           ord <- as.numeric(attributes(haplo.hash(df.out[,1:n.loci])$hap.mtx)$row.names)
         })
  nlines <- if(is.null(nlines)) nrow(df.out) else nlines
  print(df.out[ord[1:nlines],], digits=digits, ...)
  invisible(df.out[ord[1:nlines],])   
}
