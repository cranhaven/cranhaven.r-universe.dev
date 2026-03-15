#$Author: sinnwell $
#$Date: 2011/12/05 20:53:27 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/test/test.haplo.score.slide.R,v 1.1 2011/12/05 20:53:27 sinnwell Exp $
#$Locker:  $
#$Log: test.haplo.score.slide.R,v $
#Revision 1.1  2011/12/05 20:53:27  sinnwell
#changed from .q to .R, to work with R check
#
#Revision 1.1  2011/04/28 20:08:12  sinnwell
#new makefile, pulls R/man files from mgenet, rlocal
#
#Revision 1.6  2004/05/17 16:45:53  sinnwell
#add if() for dump()
#
#Revision 1.5  2004/04/29 21:30:10  sinnwell
#enable updating
#

## package: haplo.stats
## test script: haplo.score.slide

## settings

verbose=TRUE
require(haplo.stats)
Sys.setlocale("LC_ALL", "C")
Sys.getlocale()
  
## Jason Sinnwell 4/2005, updated 1/2007
## Mayo Clinic Div. of Biostatistics


if(verbose) cat("prepare two datasets, one with char alleles, the other 3 loci from hla data\n")
  
 
data(hla.demo)
  
hla.sub <- hla.demo[,c(1,2,3,4,17:24)]
geno.hla <- hla.sub[,-c(1:4)]
hla.resp <- hla.sub[,1]
hla.respcat <- hla.sub[,2]
hla.sex <- hla.sub[,3]
hla.label=c("DQB","DRB","HLA.B")

seed <- c(33, 10, 39,  6, 16,  0, 40, 24, 12, 60,  7,  1)

if(verbose) cat("hla data, gaussian trait and x.adj\n")
set.seed(seed)
hla.slide.resp.adj.add <- haplo.score.slide(hla.resp,geno.hla, n.slide=2,
                           trait.type="gaussian", haplo.effect="additive",
                           locus.label=hla.label, miss.val=0, x.adj=hla.sex,
                           simulate=FALSE, skip.haplo= 5/(2 * nrow(geno.hla)),
                           offset=NA, sim.control=score.sim.control(),
                           em.control = haplo.em.control(insert.batch.size=3,
                              min.posterior=1e-6))

if(verbose) cat("hla data ordinal trait\n")
set.seed(seed)
hla.slide.respcat.dom <- haplo.score.slide(hla.respcat, geno.hla, n.slide=2,
                            trait.type="ordinal", haplo.effect="dominant",
                            locus.label=hla.label, miss.val=0,
                            simulate=FALSE, min.count=5,
                            offset=NA, x.adj=NA,
                            sim.control=score.sim.control(),
                            em.control = haplo.em.control(insert.batch.size = 3,
                              min.posterior = 1e-06))

  
if(verbose) cat("snap SNP data with binary trait to test dominance,recessive\n")

snapDF <- read.table("snapData.csv",header=TRUE, sep=",",stringsAsFactors=FALSE)
y.bin <- snapDF[,1]-1
geno <- setupGeno(geno=snapDF[,-1])

set.seed(seed)
hcount=3
##additive, min.count set to hcount
slide.bin.hla.add3 <- haplo.score.slide(y=y.bin, geno=geno, n.slide=3,
             trait.type="binomial", min.count=hcount, haplo.effect="add",
             simulate=TRUE, sim.control=score.sim.control(min.sim=200,
                              max.sim=500))

## dominant
set.seed(seed)
slide.bin.hla.dom3 <- haplo.score.slide(y=y.bin, geno=geno,
                        trait.type="binomial", n.slide=3, min.count=hcount,
                        haplo.effect="dom", simulate=TRUE,
                    sim.control=score.sim.control(min.sim=200,max.sim=500))

  
## recessive
set.seed(seed)
slide.bin.hla.rec3 <- haplo.score.slide(y=y.bin, geno=geno, n.slide=3,
                         trait.type="binomial", min.count=hcount,
                         haplo.effect="rec", simulate=TRUE,
                         sim.control=score.sim.control(max.sim=1000))

  
print.haplo.score.slide(hla.slide.resp.adj.add, digits=5)
print.haplo.score.slide(hla.slide.respcat.dom, digits=5)

print(slide.bin.hla.add3)
print(slide.bin.hla.dom3)
print(slide.bin.hla.rec3)  
  
