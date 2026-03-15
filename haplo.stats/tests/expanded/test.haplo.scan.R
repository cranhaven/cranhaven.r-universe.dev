
########  test.haplo.scan.q: test the haplo.scan function  ##########  
#  Jason Sinnwell   3/2005
#
#   test haplo.scan under various settings
#   sink all of the results into a file
#   check them against known checked results by a diff command (unix)
#
#####################################################################

## package: haplo.stats
## test script: haplo.scan

verbose=TRUE
Sys.setlocale("LC_ALL", "C")
Sys.getlocale()

require(haplo.stats)


# test different options for haplo.scan,
# sink all of the results into a file
# check them against known checked results by a diff command (unix)
 
# use check.haplo.scan.s for splus and check.haplo.scan.r for r versions
  
  
#if(update) sink(goldfile) else sink("sink.haplo.scan.out")

seed <- c(45, 16, 22, 24, 15,  3, 11, 47, 24, 40, 18,  0)

set.seed(seed)
runif(10)


cat("\n Using a random numeric dataset, 10 loci, width=4 sim=100 \n\n")
## regular case, default parameters for 10-locus dataset
set.seed(seed)
tmp <- ifelse(runif(2000)>.3, 1, 2)
geno <- matrix(tmp, ncol=20)
y <- rep(c(0,1),c(50,50))
## show verbose on simulations.

set.seed(seed)
scan1 <- haplo.scan(y = y, geno = geno, miss.val=c(0,NA), width=4,
                    em.control=haplo.em.control(loci.insert.order = NULL, 
                      insert.batch.size = 6, min.posterior = 
                      1e-07, tol = 1e-05, max.iter = 5000,
                      random.start = 0, n.try = 10, iseed = 
                      NULL, max.haps.limit = 2000000, verbose=0),
                    sim.control=score.sim.control(p.threshold = 0.25, min.sim = 50,
                      max.sim = 1000, verbose = FALSE))

print.haplo.scan(scan1,digits=5)

cat("\n Using an all character dataset, 7 loci, width=3 \n\n")
  
## a character allele dataset
geno.char <- matrix(c('a','a','b','b','c','C','d','d','F','f','g','G','h1','h1',
                      'a','A','B','B','c','c','d','d','F','F','g','G','H','h1',
                      'a','a','b','b','c','c','d','d','F','f','g','G','h2','h2',
                      'a','a','B','B','C','c','d','d','f','f','g','G','h2','h1',
                      'a','A','B','B','c','c','d','d','F','F','g','G','H','h2',
                      'a','a','b','B','C','C','D','d','f','f','G','G','h1','h2',
                      'a','a','B','B','c','c','d','d','F','f','G','g','h2','h2',
                      'a','a','z','z','c','c','d','d','F','F','g','G','h1','z',
                      'a','A','B','B','c','z','z','z','F','f','z','z','h1','h1',
                      'a','a','B','B','c','c','d','d','F','f','G','g','h1','h2'),
                    nrow=10,byrow=T)
  
y.char<- c(0,1,0,0,1,0,0,1,0,0)

set.seed(seed)
## use all default parameters except for miss.val
scan.char <- haplo.scan(y=y.char, geno=geno.char, miss.val="z", width=3,
                        em.control=haplo.em.control(loci.insert.order = NULL, 
                          insert.batch.size = 6, min.posterior = 
                          1e-07, tol = 1e-05, max.iter = 5000,
                          random.start = 0, n.try = 10,  
                          max.haps.limit = 2000000., verbose = 0),
                        sim.control=score.sim.control(p.threshold = 0.25,
                          min.sim = 200, max.sim = 1000., verbose = FALSE))
print.haplo.scan(scan.char, digits=5)

cat("\n Using hla dataset, 8 loci, sim=10, width=3 \n\n")  
## use the hla dataset, just the first 8 markers.  slide=3, sim=10

data(hla.demo)

geno.11 <- hla.demo[,-c(1:4)]
y.bin <- 1*(hla.demo$resp.cat=="low")
hla.summary <- summaryGeno(geno.11[,1:16], miss.val=c(0,NA))

## track those subjects with too many possible haplotype pairs ( > 10,000)
many.haps <- (1:length(y.bin))[hla.summary[,4]>10000]

length(many.haps)
many.haps

## For speed, or even just so it will finish, make y.bin and geno.scan 
## for genotypes that don't have too many ambigous haplotypes
geno.scan <- geno.11[-many.haps,]
y.scan <- y.bin[-many.haps]

set.seed(seed)

runif(10)

set.seed(seed)
scan.hla <- haplo.scan(y.scan, geno.scan, width=3,
                       em.control=haplo.em.control(loci.insert.order = NULL, 
                         insert.batch.size = 3, min.posterior = 
                         1e-07, tol = 1e-05, max.iter = 5000,
                         random.start = 0, n.try = 10,  
                         max.haps.limit = 2000000, verbose = 0),
                       sim.control=score.sim.control(p.threshold = 0.25,
                         min.sim = 10, max.sim = 10, verbose = TRUE))

print.haplo.scan(scan.hla, digits=5)




  

