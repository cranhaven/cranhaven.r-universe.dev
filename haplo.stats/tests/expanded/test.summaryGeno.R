## package: haplo.stats
## test script: summaryGeno

## settings
#update=FALSE
#goldfile="gold.summaryGeno.out"

verbose=TRUE

require(haplo.stats)

if(verbose)
  cat("test two genotype matrices, one with char alleles,
       the other with 3 loci from hla data\n")
  
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

chartbl <- summaryGeno(geno.char, miss.val="z")

char.label <- c("F","G","H")

data(hla.demo)

hla.sub <- hla.demo[,c(1,2,3,4,17,18,21:24)]
geno.hla <- hla.sub[,-c(1:4)]
hla.label=c("DQB","DRB","HLA.B")


hlatbl <- summaryGeno(geno.hla)


#if(update) {
#  sink(file=goldfile)
#}  else {
#  sink("sink.summaryGeno.out")
#}

print(chartbl)
print(hlatbl)

#sink()

