#'Generate Comparison Observed and Expected No. of Heterozygous Loci.
#'@details This function generates a dataframe in which the observed and expected heterozygous loci for each sample are included. The observed ones are calculated from the original dataset. However, the expected ones are simulated according to the expected probability with the same sample size as observed sample.
#'@importFrom stats rbinom
#'@usage ComposPare_K(h,Ex,trans)
#'@param h a double made up of "0" and "1" where 1 means heterozygous and 0 means homozygous; Outcome of function "Heterozygous"; Each column denotes each locus and each row denotes each individual.
#'@param Ex a dataframe of expected density, outcome of function "DistHetero", on each possible total number of heterozygous loci.
#'@param trans a logic variable, if True, the outcome is a dataframe of n x 2. n is the number of individuals of original imported database. First column is the observed No. of Heterozygous Loci and the second is the expected one. If False, the dataframe is 2n x 2, where n is the number of individuals of original imported database. The first column is a categorical variable denoting the frequency is observed or expected value; the second column is the frequency of No. of heterozygous loci.
#'@return a dataframe of observed and expected No. of heterozygous loci for each individual.
#'@export
#'@examples
#'h<-matrix(rbinom(20,1,0.5),nrow=5)
#'Ex <- data.frame(K=c(0:5),Density=rnorm(6,mean = 0.5,sd=0.05))
#'ComposPare_K(h,Ex,trans = TRUE)

ComposPare_K <- function(h,Ex,trans)
{
  Obs_hetero<-data.frame(rowSums(h))
  colnames(Obs_hetero)<-c("K_io")
  Exp_hetero <- data.frame(sample(Ex$K,nrow(Obs_hetero),replace = T,prob = Ex$Density))
  colnames(Exp_hetero)<-c("K_ie")
  df<-as.data.frame(c(Obs_hetero,Exp_hetero))
  df_t <- data.frame(OvE=factor(rep(c("Obs", "Exp"), each=nrow(Obs_hetero))),freq=(c(df$K_io,df$K_ie)))
  if(trans){
    return(df)
  }else{
      return(df_t)

  }
}

