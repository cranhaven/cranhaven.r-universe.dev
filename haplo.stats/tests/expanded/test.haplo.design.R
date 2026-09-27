

## package: haplo.stats
## test script: haplo.glm

## settings
verbose=TRUE

require(haplo.stats)
Sys.setlocale("LC_ALL", "C")
Sys.getlocale()

  
if(verbose) cat("prepare two datasets, one with char alleles, the other 3 loci from hla data\n")
  
  # make ficticious data set with an intention of some trend in
  # haplotypes having H-allele at locus-H with F-allele at locus-F
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

char.label <- c("F","G","H")

data(hla.demo)

hla.sub <- hla.demo[,c(1,2,3,4,17,18,21:24)]
geno.hla <- hla.sub[,-c(1:4)]
hla.label=c("DQB","DRB","HLA.B")

seed <- c(33, 10, 39,  6, 16,  0, 40, 24, 12, 60,  7,  1)

if(verbose) cat("character alleles, min.count=3\n")
set.seed(seed)
em.char <- haplo.em(geno.char, miss.val='z',locus.label=char.label,
                    control = haplo.em.control())

char.design <- haplo.design(em.char, min.count=3)

print(char.design)

if(verbose) cat("hla data, 3 loci\n")
set.seed(seed)
em.hla3 <- haplo.em(geno.hla, locus.label=hla.label, miss.val=0,
                    control = haplo.em.control())

hla3.design <- haplo.design(em.hla3, hapcodes= c(4,99,138))

print(hla3.design)

if(verbose) cat("snap SNP data, options of recessive, dominant, haplo.base=2\n")
snapDF <- read.table("snapData.csv",header=TRUE, sep=",", stringsAsFactors=FALSE)

geno.snap <- setupGeno(geno=snapDF[,-c(1:7)])
set.seed(seed)
em.snap <- haplo.em(geno=geno.snap)

snap.design.rec <- haplo.design(em.snap, haplo.effect="recessive", min.count=4)

snap.design.dom.base2 <- haplo.design(em.snap, haplo.base=2, haplo.effect="dominant", min.count=6)
  

print(snap.design.rec)
print(snap.design.dom.base2)


