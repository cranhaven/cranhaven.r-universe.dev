


# ekc function from 

# Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to 
# retain in exploratory factor analysis: A comparison of extraction methods under 
# realistic conditions. Psychological Methods, 24(4), 468-491.

# "The Empirical Kaiser Criterion (EKC; Braeken & van Assen, 2017) is an approach 
# that incorporates random sample variations of the eigenvalues in Kaiser's criterion. 
# On a population level, the criterion is equivalent to Kaiser's criterion and extractions 
# all factors with associated eigenvalues of the correlation matrix greater than one. 
# However, on a sample level, the criterion takes the distribution of eigenvalues for 
# normally distributed data into account. Under the null model, the distribution of 
# eigenvalues asymptotically follows a Marc_enko-Pastur distribution (Marcenko & Pastur, 1967). 
# The resulting upper bound of this distribution is the refer- ence value for the 
# first eigenvalue, so

# for N observations and p items. Subsequent eigenvalues are cor- rected by the explained 
# variance, expressed as the eigenvalues of previous factors. The j-th reference eigenvalue is

# such that higher previous eigenvalues lower the reference eigen- value because the 
# proportion of unexplained variance will be lower. In accordance with the original 
# Kaiser criterion, the reference eigenvalue cannot become smaller than one.

# Braeken and van Assen (2017) derived theoretical conditions for scale reliability, 
# number of observations, number of factors, and factor correlation under which the 
# EKC is expected to correctly identify the number of factors. For example, for 
# orthogonal factors, EKC is predicted to work if

# for all 1 <= j >= m and m (overall) underlying factors, Cronbach's alpha in the 
# population, pj the number of items of the respective factor j, and N observations. 
# For correlated factors, the conditions that guarantee a high performance for EKC are 
# more complex, but are also more likely to be fulfilled if alpha and N are high, scales 
# are shorter, and factor correlations are low. Corroborating these as- sumptions, 
# Braeken and van Assen (2017) found that the EKC exhibited high hit rates if these 
# conditions were met (>.90), but low hit rates if they were not (<.50). In particular, 
# the EKC outperformed traditional PA with the 95th percentile as a criterion when factors 
# are correlated and are only measured by few items with very high loadings. Furthermore, 
# the EKC yielded compara- ble results to revised PA and CD in a simulation study with a 
# high number of factors and few observed variables. However, the EKC has not yet been 
# compared with revised PA or CD in a more general simulation study that also included 
# the Hull method.

# Braeken and van Assen (2017) also showed that accuracies were generally high if the 
# conditions under which the EKC is expected to work were met (all accuracies >= .93) 
# and lower if they were violated (all accuracies <= .83). However, the theoretical 
# conditions guaranteeing a high performance of the EKC and other extraction criteria 
# require information that is only available to researchers if a specific common factor 
# structure is assumed. This is potentially undesirable in the context of EFA, which is 
# usually performed to avoid assumptions about the underlying factor structure.

# (Note that assumptions about the underlying factor structure, reliabili- ties, and 
# factor correlation could be used for a power analysis for EFA to determine a sample 
# size under which EKC is expected to work.)

# Braeken, J., & van Assen, M. A. (2017). An empirical Kaiser criterion. Psychological 
# Methods, 22, 450 - 466. http://dx.doi.org/10.1037/ met0000074 


EMPKC <- function (data, corkind='pearson', Ncases=NULL, verbose=TRUE) {
  
  data <- MISSING_DROP(data)
  
  Nvars  <- ncol(data)
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  
  eigenvalues <- eigen(cormat)$values
  
  refs <- rep(0,Nvars)
  for (lupeNvars in 1:Nvars) {
    refs[lupeNvars] <- max( ((1 + sqrt(Nvars / Ncases))^2) * (Nvars - sum(refs)) / 
                              (Nvars - lupeNvars + 1) ,1)
  }
  
  NfactorsEMPKC <- which(eigenvalues <= refs)[1]-1
  
  
  if (verbose ) {
    cat('\n\n\nEMPIRICAL KAISER CRITERION')
    cat('\n\nKind of correlations analyzed: ', ctype,'\n\n')
    evals <- cbind(1:Nvars, eigenvalues, refs)
    rownames(evals) <- rep('',dim(evals)[1])
    colnames(evals) <- c('Nfactors', 'Eigenvalue','Reference Values')
    print(round(evals,3), print.gap=4, row.names=FALSE)
    cat('\nThe number of factors according to the Empirical Kaiser Criterion = ', NfactorsEMPKC,'\n')
  }
  
  empkcOutput <- list(NfactorsEMPKC=NfactorsEMPKC, eigenvalues=eigenvalues, refvalues=refs)
  
  return(invisible(empkcOutput))
  
}




# # Empirical Kaiser:
# EKC <- function(dat) {
# corx <- cor(dat)
# valuesx <- eigen(corx)$values

# refs <- rep(0,dim(dat)[2])
# for (iii in 1:dim(dat)[2]) {
# refs[iii] <- max( ((1 + sqrt(dim(dat)[2]/dim(dat)[1]))^2) * (dim(dat)[2]-sum(refs))/
# (dim(dat)[2]-iii+1) ,1)
# }
# return(which(valuesx<=refs)[1]-1)
# }