#'Test the Hardy Weinberg Equilibrium with Chi-square test####
#'@details This function check the Hardy Weinberg Equilibrium from observed and expected distribution with Chi-square test#####
#'@importFrom stats chisq.test
#'@usage HWE.Chisq(G,G0,rescale.p=FALSE,simulate.p.value=TRUE,B=2000)
#'@param G a dataframe of observed genotype frequencies. Each row denotes each genotype; each column denotes each marker. The order of markers follows x; the genotypes are ordered by: from 1:l-th column, the genotypes are homozygous in order as : p1p1, p2p2,p3p3,...,plpl;from ll-th to u-th column, the genotypes are heterozygous in order as:choose(l,2) like: p1p2,p1p3,...,p1pl,p2p3,p2p4,...p2pl,...p(l-1)pl
#'@param G0 a dataframe of expected genotype probabilities;each row denotes each genotype; each column denotes each loci. The order of markers follows x; the genotypes are ordered by: from 1:l-th column, the genotypes are homozygous in order as : p1p1, p2p2,p3p3,...,plpl;from ll-th to u-th column, the genotypes are heterozygous in order as:choose(l,2) like: p1p2,p1p3,...,p1pl,p2p3,p2p4,...p2pl,...p(l-1)pl
#'@param rescale.p a logical scalar; if TRUE then p is rescaled (if necessary) to sum to 1. If rescale.p is FALSE, and p does not sum to 1, an error is given.
#'@param simulate.p.value a logical indicating whether to compute p-values by Monte Carlo simulation.
#'@param B an integer specifying the number of replicates used in the Monte Carlo test.
#'@return a vector of result of p-values for chi-square test; the orders of markers follows x.
#'@export
#'@examples
#'require(mixIndependR)
#'x <- data.frame(STR1=c("11|12","12|13","11|13","13|15"),
#'                STR2=c("12|12","13|14","13|13","14|15"),
#'                SNP1=c("A|T","A|A","T|A","A|T"),
#'                SNP2=c("A|A","T|T","A|T","T|A"))
#'G <- GenotypeFreq(x,expect = FALSE)
#'G0 <- GenotypeFreq(x,expect = TRUE)
#'HWE.Chisq(G,G0,rescale.p=FALSE,simulate.p.value=TRUE,B=2000)

HWE.Chisq <- function(G,G0,rescale.p=FALSE,simulate.p.value=TRUE,B=2000){
  Chi_value <-function(x,x0,simulate.p.value=TRUE,B=2000,rescale.p=FALSE){
    cr<-which(!x0==0)
    output <- chisq.test(x[cr],p=x0[cr],simulate.p.value = simulate.p.value,B=B,rescale.p = rescale.p)
    return(output$p.value)
  }
  return(mapply(Chi_value,G,G0,simulate.p.value=simulate.p.value,B=B,rescale.p=rescale.p))
  }
