
##$Author: sinnwell $
##$Date: 2008/04/29 14:36:25 $
##$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.power.cc.ncp.q,v 1.4 2008/04/29 14:36:25 sinnwell Exp $
##$Locker:  $
##$Log: haplo.power.cc.ncp.q,v $
##Revision 1.4  2008/04/29 14:36:25  sinnwell
##T to TRUE
##
##Revision 1.3  2008/03/10 19:00:34  sinnwell
##call get.hapPair instead of re-used code
##
##Revision 1.2  2008/02/28 21:47:10  sinnwell
##*** empty log message ***
##
##Revision 1.1  2008/02/28 15:59:27  sinnwell
##Initial revision
#

haplo.power.cc.ncp <- function(haplo, haplo.freq, base.index, haplo.beta, case.frac, prevalence) {
  
   cont.frac <- 1 - case.frac

   haplo <- as.matrix(haplo)

   n.loci <- ncol(haplo)
   n.haplo  <- nrow(haplo)

   # Error checks

   if(length(base.index) != 1)
     {
       stop("Length of base.index != 1")
     }

   if(length(haplo.beta) != n.haplo)
     {
       stop("Length of haplo.beta != number of haplotypes")
     }
   
   if(n.loci <2)
     {
       stop("Number of loci < 2")
     }
   
   if(length(haplo.freq) != n.haplo)
     {
       stop("Length haplo.freq != number of haplotypes")
     }

   
  # Create matrix of indices for all possible pairs of haplotypes

#   haplo.indx <- expand.grid(1:n.haplo,1:n.haplo)
#   haplo.indx <- cbind(haplo.indx[,2],haplo.indx[,1])
#   haplo.indx <- haplo.indx[haplo.indx[,1] <= haplo.indx[,2],]


 # Set up regression design matrices and beta coeff vectors

#   haplo.reg <- 1:n.haplo
#   x.haplo <- 1*outer(haplo.indx[,1], haplo.reg,"==") +  1*outer(haplo.indx[,2], haplo.reg,"==")

   # Compute prior genotype probs (geno = pair of haplotypes)

#   p.g <-  haplo.freq[haplo.indx[,1]] * haplo.freq[haplo.indx[,2]]
#   p.g <-  p.g * ifelse(haplo.indx[,1] == haplo.indx[,2], 1, 2)
   
   hapPair.lst <- get.hapPair(haplo, haplo.freq, base.index)
  
   x.haplo <- hapPair.lst$x.haplo
   p.g <- hapPair.lst$p.g
   haplo.indx <- hapPair.lst$haplo.indx
   
  # now move base.index to first col of x, and first element of beta

   x.design <- cbind( rep(1,nrow(x.haplo)), x.haplo) #[,-base.index])
   beta <- haplo.beta
   beta <- c(beta[base.index], beta[-base.index])
   
   # Find intercept
   other <- list(x=x.design, p.x=p.g, beta=beta[-1],  prev = prevalence)
   root <- uniroot(f=find.intercept.logistic, interval=c(-50,50),other=other)

   # Put intercept as first item in beta vector
   beta[1] <- root$root

   # function to compute P(Y|g) according to logistic reg model,
   # needed in subsequent steps

   py.g <- function(x,beta){
      root <- exp(x%*%beta)
      p <- root/(1+root)
      return(p)
    }


  # Determine genotype (haplotype pair) frequencies for cases (pg.case) and
  # controls (pg.cont)

   pg.case <- py.g(x.design, beta) * p.g        # P(y=1|g)P(g)
   pg.case <- as.vector(pg.case/sum(pg.case))

   pg.cont <- (1-py.g(x.design, beta)) * p.g    # P(y=0|g)P(g)
   pg.cont <- as.vector(pg.cont/sum(pg.cont))


   #--------------  Complete Haplotype Data ------------------------------------------

   df.complete <-  ncol(x.haplo)

   # E[X|case]
   ex.case.complete <- apply(x.haplo * pg.case, 2, sum)

   # E[X|control]
   ex.cont.complete <- apply(x.haplo * pg.cont, 2, sum)

   delta.complete <- (ex.case.complete - ex.cont.complete)


   # E[Xbar]
   ex.xbar.complete <- (case.frac * ex.case.complete) + (cont.frac * ex.cont.complete)

   # Vx

   xDiff <- x.haplo - matrix(rep(ex.xbar.complete, nrow(x.haplo)),nrow=nrow(x.haplo), byrow=TRUE)
   vx.case <- t(xDiff * pg.case ) %*% (xDiff )
   vx.cont <- t(xDiff * pg.cont ) %*% (xDiff )
   vx.complete <-  (case.frac*vx.case) + (cont.frac * vx.cont)

   ncp.complete <- case.frac*cont.frac*( t(delta.complete) %*% solve(vx.complete) %*% delta.complete)

   
  #--------------  Incomplete Haplotype Data ----------------------------------------


  # Genotype matrix, ignoring haplotype phase information. 

   geno.mat <- NULL
   for(i in 1:n.loci){
     t1 <- haplo[haplo.indx[,1],i]
     t2 <- haplo[haplo.indx[,2],i]
     a1 <- ifelse(t1 < t2, t1, t2)
     a2 <- ifelse(t2 > t1, t2, t1)
     geno.mat <- cbind(geno.mat, a1, a2)
   }

  # Create haplo.group which gives a code for pairs of
  # haplotypes that fall into the same genotype group when phase is unk

   geno.hash <- haplo.hash(geno.mat)

   haplo.group <- geno.hash$hash

  # Create probabilities for haplotype groups 

   p.haplo.group.case <- as.vector(tapply(pg.case, haplo.group, sum))
   p.haplo.group.cont <- as.vector(tapply(pg.cont, haplo.group, sum))


  # Create exemplary data of genotypes for case and controls,
  # with weights that represent their relative frequencies, 
  # such that the weights sum to 1.

   geno.temp <- rbind(geno.hash$hap.mtx, geno.hash$hap.mtx)
   oldClass(geno.temp) <- "model.matrix"
   
   wt <- c(case.frac*p.haplo.group.case, cont.frac*p.haplo.group.cont)

   save.em <- haplo.em(geno=geno.temp, weight=wt, control=haplo.em.control(min.posterior=0))
   
   n.haplo <- nrow(save.em$haplotype)
   hap.indx <- 1:n.haplo
   basehap <-  hap.indx[save.em$hap.prob == max(save.em$hap.prob)][1]

  # Score haplotypes that are not rare (freq > 10e-4) and not the most frequent, which
  # is considered a baseline haplotype

   uhap <- hap.indx[hap.indx!=basehap &( save.em$hap.prob > 10e-4)]

   x <- 1*outer(save.em$hap1code, uhap, "==") + 1*outer(save.em$hap2code, uhap, "==")

   nx <- ncol(x)

   xstar <- NULL
   for(j in 1:nx){
     xstar <-  cbind(xstar, tapply(x[,j] * save.em$post,save.em$subj.id,sum))
   }
   
   df.incomplete <- ncol(xstar)
   
   ncase <- length(p.haplo.group.case)
   case <- 1:ncase
   cont <- (ncase+1):(2*ncase)

  # note that xstar is same for cases and controls, because posteriors are the same for
  # cases and controls when running EM for haplotypes under Ho

   xstar <- xstar[case,]
   
   # E[X|case]
   ex.case.incomplete <- apply(xstar * p.haplo.group.case, 2, sum)

   # E[X|control]
   ex.cont.incomplete <- apply(xstar * p.haplo.group.cont, 2, sum)
   
   delta.incomplete <- ex.case.incomplete - ex.cont.incomplete

   # E[Xbar]
   ex.xbar.incomplete <- (case.frac * ex.case.incomplete) + 
                         (cont.frac * ex.cont.incomplete)

   #  Vx

   xstar.diff <- (xstar  - matrix(rep(ex.xbar.incomplete, nrow(xstar)),nrow=nrow(xstar), byrow=TRUE))

   vx.case <- t(xstar.diff * p.haplo.group.case) %*% (xstar.diff)
   vx.cont <- t(xstar.diff * p.haplo.group.cont) %*% (xstar.diff)

   vx.incomplete <- (case.frac * vx.case) + (cont.frac * vx.cont)

   ncp.incomplete <- case.frac * cont.frac * t(delta.incomplete) %*% solve(vx.incomplete) %*% delta.incomplete

  # By running haplo.em on the unphased genotype data, the haplotype order is changed. To map haplotypes
  # between incomplete and complete haplotype data, we do the following.

   haplo.df.incomplete <- data.frame(save.em$haplotype, haplo.freq.incomp=save.em$hap.prob, 
                                     hap.indx.incomp=hap.indx)
   
   haplo.df.complete   <- data.frame(haplo, haplo.freq, hap.indx.comp=1:nrow(haplo))
   
   haplo.merge <- merge(haplo.df.incomplete, haplo.df.complete, by.x=1:n.loci,by.y=1:n.loci,all=TRUE)

  # Need to subset to haplotypes that are scored for incomplete data (see above for how uhap was created)
   zed <- (hap.indx!=basehap) & ( save.em$hap.prob > 10e-4)

   haplo.merge <- haplo.merge[zed,]
   haplo.merge$x.score <- 1:nrow(haplo.merge)
   ord <- order(haplo.merge$hap.indx.comp)
   haplo.merge <- haplo.merge[ord,]
   ord <- haplo.merge$x.score

   delta.incomplete <-  delta.incomplete[ord]
   ex.case.incomplete <- ex.case.incomplete[ord]
   ex.cont.incomplete <- ex.cont.incomplete[ord]

   tmp <- vx.incomplete[ord,]
   vx.incomplete <- tmp[,ord]

   return(list(ncp.phased.haplo = ncp.complete, 
               df.phased.haplo  = df.complete,
               ex.case.complete = ex.case.complete, 
               ex.cont.complete = ex.cont.complete,
               delta.complete   = delta.complete, 
               vx.complete      = vx.complete, 
               ncp.unphased.haplo = ncp.incomplete, 
               df.unphased.haplo  = df.incomplete,
               ex.case.incomplete  = ex.case.incomplete, 
               ex.cont.incomplete  = ex.cont.incomplete,
               delta.incomplete    = delta.incomplete, 
               vx.incomplete       = vx.incomplete) )

}


find.intercept.logistic <- function(b0,other) {
  
  # function to find intercept for logistic regression when 
  # design matrix is specified, along with
  # population frequencies of rows, beta for non-intercept
  # effects, and prevalence (prev)
  x <- other$x
  p.x <- other$p.x
  beta <- other$beta
  prev <- other$prev
  z <- x %*% c(b0,beta)
  pd <- exp(z)/(1+exp(z))
  return( sum(pd*p.x) - prev )

}
