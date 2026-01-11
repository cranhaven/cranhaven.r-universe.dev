#'Generate Comparison Observed and Expected No. of Shared Alleles.
#'@details This function generates a dataframe in which the observed and expected shared alleles for each pair of individuals. The observed ones are calculated from the original dataset through "AlleleShare_Table". However, the expected ones are simulated according to the expected probability with the same sample size as the observed sample.
#'@usage ComposPare_X(AS,Ex,trans=TRUE)
#'@param AS a double made up of "0","1" and "2" denoting number of shared alleles; Outcome of function "AlleleShare_Table"; Each column denotes each locus and each row denotes each pair of individuals.
#'@param Ex a dataframe of expected density, outcome of function "DistAlleleShare", on each possible total number of shared Alleles.
#'@param trans a logic variable, if True, the outcome is a dataframe of n x 2. n is the number of individuals of original imported database. First column is the observed No. of Heterozygous Loci and the second is the expected one. If False, the dataframe is 2n x 2, where n is the number of individuals of original imported database. The first column is a categorical variable denoting the frequency is observed or expected value; the second column is the frequency of No. of heterozygous loci.
#'@return a dataframe of observed and expected No. of shared alleles for each pair of individuals.
#'@export
#'@examples
#'AS<-matrix(sample(c(0:2),20,replace=TRUE,prob=c(0.3,0.3,0.4)),nrow=5)
#'Ex <- data.frame(X=c(0:8),Density=rnorm(9,mean = 0.5,sd=0.05))
#'ComposPare_X(AS,Ex,trans = TRUE)

ComposPare_X <- function(AS,Ex,trans=TRUE)
{
  Obs_AS<-data.frame(rowSums(AS))
  colnames(Obs_AS)<-c("X_io")
  Exp_AS <- data.frame(sample(Ex$X,nrow(Obs_AS),replace = T,prob = Ex$Density))
  colnames(Exp_AS)<-c("X_ie")
  df<-as.data.frame(c(Obs_AS,Exp_AS))
  df_t <- data.frame(OvE=factor(rep(c("Obs", "Exp"), each=nrow(Obs_AS))),freq=(c(df$X_io,df$X_ie)))
  if(trans){
    return(df)
  }else{
    return(df_t)
  }
}
