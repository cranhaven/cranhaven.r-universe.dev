#$Author: sinnwell $
#$Date: 2005/03/24 22:08:56 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.scan.sim.q,v 1.2 2005/03/24 22:08:56 sinnwell Exp $
#$Locker:  $
#$Log: haplo.scan.sim.q,v $
#Revision 1.2  2005/03/24 22:08:56  sinnwell
#changed from scan.sim
#
#Revision 1.1  2005/03/23 18:07:08  sinnwell
#Initial revision
#
  # find a vector of maximum statistics for each locus
  # use simulated values for y
  # for all locus subsets, use saved h1.sub, h2.sub, post.sub, nrep
  # from scan.obs

haplo.scan.sim <- function(y.reord, save.lst, nloci) {
  
  # save.lst is the information on hapcodes, posteriors, nrep for
  # all subsets which didn't result in 1-haplotype subsets
  # use them for simulations
  df.sim <- numeric(0)
  for(i in 1:length(save.lst)) {
    test <- haplo.chistat(y=y.reord,
                    h1=save.lst[[i]]$h1.sub,
                    h2=save.lst[[i]]$h2.sub,
                    post=save.lst[[i]]$post.sub,
                    nrep=save.lst[[i]]$nrep)
    df.sim <- rbind(df.sim, c(save.lst[[i]]$loci, test) )       
  }

  # for each locus, find the max chistat of those which were
  # calculated on a set of loci (mrow) which included that locus
  svec <- numeric(nloci)

  for(loc in 1:nloci) {
    # find max of chistats which came from subsets containing loc
    mrow <- (df.sim[,1] <= loc) & (loc <= df.sim[,2])
    svec[loc] <- max(df.sim[mrow,3])
  }

  return(svec)

}
