#$Author: sinnwell $

## package: haplo.stats
## test script: haplo.cc

## settings
verbose=TRUE
options(width=140)
require(haplo.stats)
tmp <- Sys.setlocale("LC_ALL", "C")
tmp <- Sys.getlocale()

data(hla.demo)

# Jason Sinnwell, created 3/2004, updated 9/2014
# Mayo Clinic, Biostatistics

  if(verbose) cat("setting up data...\n")
  
  label <- c("DQB","DRB","B")

  y.bin <- 1*(hla.demo$resp.cat=="low")

  geno <- as.matrix(hla.demo[,c(17,18,21:24)])

  # commented code was to check data that goes into haplo.cc
  # and gets pasted together in the huge data.frame.
  
 # sink(file="results.haplo.cc.txt")  

  seed <- c(17, 53, 1, 40, 37, 0, 62, 56, 5, 52, 12, 1)
 
  if(verbose) cat("hla data \n")
  set.seed(seed)
  cc.hla <- haplo.cc(y.bin, geno, miss.val=0,locus.label=label, 
           control=haplo.glm.control(haplo.min.count=8,  em.c=haplo.em.control()))
  set.seed(seed)
  cc.hla.adj <-  haplo.cc(y.bin, geno, x.adj=hla.demo[,c("male","age")], miss.val=0,locus.label=label, 
           control=haplo.glm.control(haplo.min.count=8,  em.c=haplo.em.control()))

set.seed(seed)
ntest <- 200
geno.test <- cbind(sample(1:2, size=ntest, replace=TRUE), sample(1:2, size=100, replace=TRUE),
           sample(2:3,size=ntest, replace=TRUE), sample(2:3, size=100, replace=TRUE),
           sample(2:4,size=ntest, replace=TRUE, prob=c(.5,.35,.15)), sample(2:4, size=100,
                                            replace=TRUE, prob=c(.5,.35,.15)))
y.test <- sample(1:2,size=ntest, replace=TRUE,prob=c(.6, .4)) - 1
x.test <- cbind(rbinom(nrow(geno.test), 1, prob=.3), round(rnorm(nrow(geno.test), mean=50, sd=4)))
locus.label <- c("A", "B", "C")


if(verbose) cat("small numeric data... \n")
set.seed(seed)
cc.test <- haplo.cc(y.test, geno.test, locus.label=locus.label,
                  ci.prob=.95, control=haplo.glm.control(haplo.min.count=4))
set.seed(seed)
cc.adj <- haplo.cc(y.test, geno.test, x.adj=x.test,locus.label=locus.label,
                  ci.prob=.95, control=haplo.glm.control(haplo.min.count=4))


  locus.label <- c("A", "B", "C")

  geno.char <- ifelse(geno.test==1, 'A',ifelse(geno.test==2, 'T',
                        ifelse(geno.test==3, 'G', 'C')))

  set.seed(seed)
  if(verbose) cat("small char data with simulations... \n")
  cc.char.sim <- haplo.cc(y.test, geno.char, locus.label=locus.label, 
                          ci.prob=.90, simulate=FALSE,
                          control = haplo.glm.control(haplo.min.count=4))
  
  print.haplo.cc(cc.hla, digits=3, order.by="score", nlines=12)
  print(cc.hla.adj, order.by="score",digits=3, nlines=12)
#  print(cc.hla$fit.lst, digits=3)
#  print(cc.hla$score.lst, digits=3)
  print.haplo.cc(cc.test, order.by='score', digits=3)
  print.haplo.cc(cc.test, order.by='haplotype', digits=3)
  print.haplo.cc(cc.test, order.by='freq', digits=3)

  print(cc.adj, order.by="score", digits=3)
  print(cc.adj, order.by="haplotype", digits=3)
  print(cc.adj, order.by="freq", digits=3)
  summary(cc.adj$fit.lst, digits=3)
  print.haplo.cc(cc.char.sim, digits=3)
  print(cc.char.sim$fit.lst, digits=3)
  print(cc.char.sim$score.lst, digits=3)
