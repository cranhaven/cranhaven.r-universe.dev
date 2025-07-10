# modified on Jan. 14, 2019
#  (1) based on the R code Zeyu sent to me on Jan. 14, 2019
#
# v2 created on Feb. 25, 2018
#  (1) for the probe with sd=0, we set p-value=0.9999
#
# created on Feb. 13, 2018


wilcoxWrapper=function(mat, grp)
{
  nr=nrow(mat)
  pval=unlist(lapply(1:nr, function(i) {
    xi=mat[i,]
    sdi=sd(xi, na.rm=TRUE)
    if(sdi)
    {
      resi=wilcox.test(xi~grp)
      return(resi$p.value)
    } else {
      return(0.9999)
    }
  }))

  invisible(pval)

}

