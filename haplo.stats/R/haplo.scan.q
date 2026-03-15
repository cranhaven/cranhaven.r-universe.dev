#$Author: sinnwell $
#$Date: 2011/11/10 15:29:40 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.scan.q,v 1.7 2011/11/10 15:29:40 sinnwell Exp $
#$Locker:  $
#$Log: haplo.scan.q,v $
#Revision 1.7  2011/11/10 15:29:40  sinnwell
#major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
#
#Revision 1.6  2008/12/02 20:23:57  sinnwell
#add code to remove subjects with NA in y.
#
#Revision 1.5  2006/10/25 14:51:32  sinnwell
#remove Matrix library load
#
#Revision 1.4  2006/01/27 16:30:10  sinnwell
#dependency on Matrix library
#
#Revision 1.3  2005/03/28 22:26:31  sinnwell
#change calls to haplo.scan.sim and haplo.scan.obs
#
#Revision 1.2  2005/03/23 23:13:17  sinnwell
#return max.loc
#
#Revision 1.1  2005/03/23 18:07:08  sinnwell
#Initial revision
#

# Jason Sinnwell and Dan Schaid
# Mayo Clinic, Rochester MN USA


haplo.scan <- function(y, geno, width=4, miss.val=c(0,NA),
                     em.control=haplo.em.control(),
                     sim.control=score.sim.control() )

# Given case/control status and unphased genotypes
# For all locus subsets within width of a given locus,
#   Find the maximum chi-square stastistic for case/control status

# 10 locus set, width=3, the max-stat for locus 5 is the max
# for subsets 345, 456, 567, 45, 56, 5

# Perform simulations using permuted case/control status for evaluation of
# significance of the max-stat for each locus, and also for a global-max-stat

{
  
  call <- match.call()
  
  # check dimensions of parameters
  if(!(ncol(geno)%%2==0)) stop("geno has uneven number of columns") 

  if(length(y) != nrow(geno)) stop("y and geno have different number of observations")

  # if any NA in y, rm subject from y and geno
  naY <- is.na(y)
  if(any(naY)) {
    y <- y[!naY]
    geno <- geno[!naY,]
    cat("Removing subjects with NA for y\n")
  }
  
  nloci <- ncol(geno)/2

  # calculate statvec on observed data,
  # while saving information for all locus subsets
  em.obj <- haplo.em(geno, miss.val=miss.val, control=em.control)
  vec.save <- haplo.scan.obs(y=y, em.obj=em.obj, width=width)

  svec <- vec.save$svec
  maxstat.obs <- max(svec)
  maxstat.locus <- (1:nloci)[maxstat.obs==svec]
  # extract the information from sub-locus haplotypes to re-use in simulations
  save <- vec.save$save.lst
  
  # calculate svec on permuted data until:
  # a. simulate at least min.sim times
  # b. aim for p-value accuracy for small pvals (Besag and Clifford, 1991)
  # c. stop at max.sim if accuracy not met by then

  done <- FALSE
  nsim <- 0
  global.rej <- 0
  loc.rej <- numeric(nloci)
  p.threshold <- sim.control$p.threshold

  while(!done) {

    y.reord <- y[order(runif(length(y)))]

    simvec <- haplo.scan.sim(y.reord=y.reord, save.lst=save, nloci)

    nsim <- nsim+1
    maxstat.sim <- max(simvec)
    global.rej <- global.rej + (maxstat.sim > maxstat.obs)
    loc.rej <- loc.rej + (simvec > svec)

      # h is target count of rejection obs to stop sampling
         #   formula from Besag and Clifford 1991
    h.count <- 1/(p.threshold^2 + 1/nsim)
    h.met <- c(global.rej, loc.rej) >= h.count

    # print simulation criteria out to screen
    if(sim.control$verbose)
      cat("h.count:", round(h.count,5), "nsim: ", nsim, "\n glob.rej:", global.rej, " loc.rej:", loc.rej, "\n")

    # done if reach max.sim or enough rej values.  If max is reached,
    # keep results and calculate p-vals at that point.
    ## cat("nsim:", nsim, " h.count:", h.count, " h.met:", h.met, "\n")
    if((nsim >= sim.control$min.sim) & ( (nsim == sim.control$max.sim) | all(h.met) ) )
      done <- TRUE
  }

  locp <- loc.rej/nsim
  globalp <- global.rej/nsim

  scan.obj <- list(call=call, scan.df=rbind(svec, locp),
                max.loc=maxstat.locus, globalp=globalp, nsim=nsim)
  oldClass(scan.obj) <- "haplo.scan"
  return(scan.obj)
  
}


