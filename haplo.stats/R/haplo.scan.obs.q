#$Author: sinnwell $
#$Date: 2005/03/28 22:26:58 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.scan.obs.q,v 1.2 2005/03/28 22:26:58 sinnwell Exp $
#$Locker:  $
#$Log: haplo.scan.obs.q,v $
#Revision 1.2  2005/03/28 22:26:58  sinnwell
#changed from scan.obs
#
#Revision 1.1  2005/03/23 18:07:08  sinnwell
#Initial revision
#

haplo.scan.obs <- function(y, em.obj, width) {
  # find a vector of maximum statistics for each locus
  # use observed values for y
  # for all locus subsets, save necessary h1.sub, h2.sub, post.sub, nrep
  # information for simulations
  
  
  # find haplotype probabilities,
  # re-use for needed posteriors on subsets
  id <- em.obj$subj.id
  nloci <- ncol(em.obj$haplotype)

  df <- numeric(0)
  # find all contiguous subsets of size 'width' or less within nloci
  save.lst <- list()
  i <- 1
  for(k in 1:width) {
    start <- 1
    end <- start+k-1
    
    while(end<=nloci) {
      
      sub.loc <- start:end
      haplo.sub <- em.obj$haplotype[, sub.loc, drop=FALSE]

      sub.indx <- as.vector(unclass(factor(apply(haplo.sub, 1, paste, collapse = ","))))
           
    # collapse prob of large haps into smaller
      p.sub <- tapply(em.obj$hap.prob,sub.indx, sum)
      
      # make the equivalent of new hap1code and hap2code
        # for subset of loci
      h1.sub <- sub.indx[em.obj$hap1code]
      h2.sub <- sub.indx[em.obj$hap2code]

      # find new priors for sub-haplotypes
      prior.sub <- p.sub[h1.sub]*p.sub[h2.sub]
      
      # heterozygous could happen two ways--twice the prob
      prior.sub <- ifelse(h1.sub==h2.sub, prior.sub, 2*prior.sub)

      denom <- tapply(prior.sub, id, sum) 
      nrep <- table(id)
      post.sub <- prior.sub/rep(denom, nrep)
      
      if(length(table(c(h1.sub, h2.sub))) > 1) { 
        # perform the test stat
        # don't need to keep anything for subset with only 1 possible hap
        test <- haplo.chistat(h1.sub, h2.sub, post.sub, y, nrep)
        df <- rbind(df, c(start, end, test))
        save.lst[[i]] <- list(loci=c(start,end), h1.sub=h1.sub,h2.sub=h2.sub,post.sub=post.sub,nrep=nrep)
        i <- i+1
      }
      start <- start+1
      end <- end+1
    } # end while()

  }  # end for(width)
 
  # for each locus, find the max chistat of those which were
  # calculated on a set of loci (mrow) which included that locus
  maxstat.vec <- numeric(nloci)

  for(loc in 1:nloci) {
    # find max of chistats which came from subsets containing loc
    mrow <- (df[,1] <= loc) & (loc <= df[,2])
    maxstat.vec[loc] <- max(df[mrow,3])
  }
  
  return(list(svec=maxstat.vec,save.lst=save.lst))
}
