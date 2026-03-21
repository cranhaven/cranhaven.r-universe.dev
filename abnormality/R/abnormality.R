#' Measure a Subject's Abnormality with Respect to a Reference Population
#'
#' @param Subj a vector of length n
#' @param Ref an n x p matrix containing the reference population.
#' @param stopping_rule the stopping rule to use when deciding the number of principal components to retain. Options include: c("Kaiser-Guttman", "brStick","TVE").
#' @param dist_measure the aggregate distance measure to use. Options include: c("MAD", "Euclidean", Manhattan","RMSE", "Lk-Norm")
#' @param TVE a numeric value between 0 and 1. The minimum total variance explained for the retained principal components. This will only be used if "TVE" is chosen as the stopping_rule.
#' @param k the value of k if Lk-Norm is chosen as a distance measure
#'
#' @return An unbiased measure of overall abnormality of the subject as compared to the reference population based on the parameters supplied.
#' @export
#' @examples
#' p = 100
#' Subj <- rep(1, p)
#' Reference_Population <- generate_correlated_matrix(100, p, corr = 0.75,constant_cov_matrix = TRUE)
#' overall_abnormality(Subj,Reference_Population)
#' overall_abnormality(Subj,Reference_Population,dist_measure = "Euclidean")
#' overall_abnormality(Subj,Reference_Population,stopping_rule = "TVE", TVE = .90)
#' overall_abnormality(Subj,Reference_Population,dist_measure = "Lk-Norm",k=.5,stopping_rule="brStick")
#'
overall_abnormality <- function(Subj, Ref, stopping_rule = "Kaiser-Guttman", dist_measure = "MAD", TVE = 1, k = 2){
  n_vars <- length(Subj)
  n <- nrow(Ref)

  #Compute the mean and std dev of each variable to use later to center and scale the subject data
  refpop_means <- colMeans(Ref)
  refpop_sd <- apply(Ref,2,stats::sd)
  refpop_sc <- scale(Ref,scale=T,center=T)
  refpop_cov<- stats::cov(refpop_sc)

  #run the principal component analysis on the Ref data
  p <- stats::prcomp(Ref, center=T, scale=T, retx=T)

  #Determine number of PC's to keep
  EigenValues <- p$sdev^2

  if(stopping_rule=="Kaiser-Guttman"){
    numPCs <- sum(EigenValues >= 1)
  }

  if(stopping_rule=="brStick"){
    numPCs <- brStick(EigenValues)
  }

  if(stopping_rule=="TVE"){
    EigenSum <- sum(p$sdev^2)
    PC_VAF <- t(matrix(abs(EigenValues/EigenSum)))
    cum_PC_VAF <- round(cumsum(PC_VAF),5) #rounding is necessary for logical comparison to work.   #https://stackoverflow.com/questions/2769510/numeric-comparison-difficulty-in-r
    numPCs <- min(which(cum_PC_VAF >= TVE))
  }

  #Calculate projections
  refpop_projs<- p$x

  # Get the means of the PCs for the reference groups of normal subjects
  # These should all be near zero.
  PCmeans<- colMeans(refpop_projs)

  # Get the SDs of the Ref PCs. Should = sqrt(Eigenvalues)
  PCsds <- p$sdev

  #scale and center Subj based on the mean and sd of Ref.
  Subj_sc <- (Subj-refpop_means)/refpop_sd

  #Multiply the subject data by the eigenvectors calculated on the normal population. Result is the PC values for the given subject.
  Subj_projs<- Subj_sc%*% p$rotation[,1:numPCs]

  #calculate the distance from the Ref means (the origin) for each dimension
  dist <- (Subj_projs-PCmeans[1:numPCs])/PCsds[1:numPCs]

  #aggregate those raw 'distances' via an aggregate distance measure as chosen by the user via the dist_measure argument
  #Euclidean
  if(dist_measure=="Euclidean"){
    k=2
    dist_squared <- (abs(dist))^k
    abnormality <- sum(dist_squared)^(1/k)
  }

  #MAD
  if(dist_measure=="MAD"){
    dist_abs <- abs(dist)
    abnormality <- sum(dist_abs)/numPCs
  }

  #Manhattan
  if(dist_measure=="Manhattan"){
    k=1
    dist_squared <- (abs(dist))^k
    abnormality <- sum(dist_squared)^(1/k)
  }

  #RMSE
  if(dist_measure=="RMSE"){
    n <- length(dist)
    sum_dist_squared <- sum(dist^2)
    abnormality <- sqrt(sum_dist_squared/n)
  }

  #Lk Norm
  if(dist_measure=="Lk-Norm"){
    dist_squared <- (abs(dist))^k
    abnormality <- sum(dist_squared)^(1/k)
  }

  return(abnormality)

}
