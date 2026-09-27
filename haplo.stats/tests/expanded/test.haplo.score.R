## package: haplo.stats
## test script: haplo.score

## settings

verbose=TRUE
Sys.setlocale("LC_ALL", "C")
Sys.getlocale()

require(haplo.stats)

#library(haplo.stats, lib="/projects/bsi/gentools/R/lib214")


if(verbose)
  cat("prepare two datasets, one with char alleles, the other 3 loci from hla data\n")
  
## make ficticious data set with an intention of some trend in
## haplotypes having H-allele at locus-H with F-allele at locus-F
geno.char <- matrix(c('F','f','g','G','h1','h1',
                      'F','F','g','G','H','h1',
                      'F','f','g','G','h2','h2',
                      'f','f','g','G','h2','h1',
                      'F','F','g','G','H','h2',
                      'f','f','G','G','h1','h2',
                      'F','f','G','g','h2','h2',
                      'F','F','g','G','h1','z',
                      'F','f','z','z','h1','h1',
                      'F','f','G','g','h1','h2',
                      'F','f','G','G','h1','h2',
                      'F','F','g','G','h1','z',
                      'F','f','z','z','h1','h1',
                      'f','f','G','g','h1','h2'), nrow=14,byrow=T)

y.response <- c(2.0,4.5,1.8,2.1,5.2,1.3,3.4,2.5,2.2,1.9,2.1,1.2,3.0,1.9)
y.pheno <- c(0,1,0,0,1,0,0,1,0,0,0,1,1,0)
char.label <- c("F","G","H")

data(hla.demo)

hla.sub <- hla.demo[,c(1,2,3,4,17,18,21:24)]
geno.hla <- hla.sub[,-c(1:4)]
hla.resp <- hla.sub[,1]
hla.respcat <- hla.sub[,2]
hla.sex <- hla.sub[,3]
hla.label=c("DQB","DRB","HLA.B")

seed <- c(33, 10, 39,  6, 16,  0, 40, 24, 12, 60,  7,  1)
set.seed(seed)
runif(10)

if(verbose) cat("character alleles, binary trait, additive and dominant\n")

set.seed(seed)
score.char.pheno.add <- haplo.score(y.pheno, geno.char, trait.type="binomial",
                              locus.label=char.label, miss.val='z',
                              offset = NA, x.adj = NA, min.count=4, simulate=FALSE,
                              haplo.effect="additive", 
                              sim.control = score.sim.control(),
                              em.control = haplo.em.control())

print(score.char.pheno.add)


set.seed(seed)
runif(10)
set.seed(seed)
score.char.pheno.dom <- haplo.score(y.pheno, geno.char, trait.type="binomial",
                        locus.label=char.label, miss.val='z',
                        offset = NA, x.adj=NA, min.count=4, simulate=FALSE,
                        haplo.effect="dom",
                        sim.control = score.sim.control(),
                        em.control = haplo.em.control())

print(score.char.pheno.dom)
  
if(verbose) cat("character alleles, gaussian trait\n")
set.seed(seed)
score.char.gaus <- haplo.score(y.response, geno.char, trait.type="gaussian",
                        skip.haplo=.1,
                        locus.label=char.label, miss.val="z",simulate=TRUE,
                        sim.control=score.sim.control(min.sim=50, p.threshold=.25,
                            max.sim=1000,verbose=FALSE),
                        em.control = haplo.em.control())

print(score.char.gaus)

if(verbose) cat("hla data, gaussian trait and x.adj\n")
set.seed(seed)
score.hla.resp.adj <- haplo.score(hla.resp,geno.hla,trait.type="gaussian",
                              locus.label=hla.label, miss.val=0, x.adj=hla.sex,
                              simulate=FALSE, min.count=5)
                              

print(score.hla.resp.adj)

if(verbose) cat("hla data ordinal trait\n")
set.seed(seed)
score.hla.respcat <- haplo.score(hla.respcat, geno.hla, trait.type="ordinal",
                           locus.label=hla.label, miss.val=0,
                           simulate=FALSE, min.count=5,
                             offset=NA, x.adj=NA)                     

print(score.hla.respcat)

set.seed(seed)
score.hla.respcat.adj <- haplo.score(hla.respcat, geno.hla, trait.type="ordinal",
                           locus.label=hla.label, miss.val=0,
                           simulate=FALSE, min.count=5,
                             offset=NA, x.adj=hla.sex)  

print(score.hla.respcat)

if(verbose) cat("snap SNP data with binary trait to test dominance,recessive\n")
snapDF <- read.table("snapData.csv",header=TRUE, sep=",")
y.bin <- snapDF[,1]-1
geno <- setupGeno(geno=snapDF[,-1])

set.seed(seed)
y.ord <- sample(c("low", "med", "hi"), size=nrow(snapDF), prob=c(.3,.4,.3), replace=TRUE)
#table(y.ord)
geno.rec <- setupGeno(snapDF[,-c(1:9)])
set.seed(seed)
hcount=5
##additive, min.count set to hcount
bin.snap.add <- haplo.score(y=y.bin, geno=geno.rec, trait.type="binomial",
                     min.count=hcount,
                     simulate=TRUE, haplo.effect="add",
                     sim.control=score.sim.control(min.sim=200,max.sim=500))

print(bin.snap.add)

## additive
set.seed(seed)
bin.snap.dom <- haplo.score(y=y.bin, geno=geno.rec, trait.type="binomial",
                 min.count=hcount, simulate=TRUE, haplo.effect="dom",
                 sim.control=score.sim.control(min.sim=200,max.sim=500))

print(bin.snap.dom)

## recessive
set.seed(seed)
bin.snap.rec <- haplo.score(y=y.bin, geno=geno.rec, trait.type="binomial",
                       min.count=hcount, simulate=TRUE,
                       haplo.effect="rec",
                       sim.control=score.sim.control(min.sim=200,max.sim=500))

print(bin.snap.rec)

## ordinal, additive

set.seed(seed)
ord.snap.add <- haplo.score(y=y.ord, geno=geno.rec, trait.type="ordinal",
                    min.count=hcount, simulate=TRUE, haplo.effect="add",
                    sim.control=score.sim.control(min.sim=200,max.sim=500))       

print(ord.snap.add)

set.seed(seed)
ord.snap.add.adj <- haplo.score(y=y.ord, geno=geno.rec, trait.type="ordinal",
                 min.count=hcount, simulate=TRUE, haplo.effect="add", x.adj=hla.sex,
                 sim.control=score.sim.control(min.sim=200,max.sim=500))

print(ord.snap.add.adj)




