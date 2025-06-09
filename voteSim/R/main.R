# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#   library(devtools)
#   library(roxygen2)
#   Check Package:             'Ctrl + Shift + E'
#   Buid package
#   Test Package:              'Ctrl + Shift + T'
#   Install Package:           'Ctrl + Shift + B'


#' Generates a simulation of voting according to a beta law, returns voters preferences
#' @export
#' @param n_voters integer, represents the number of voters in the election
#' @param n_candidates integer, represents the number of candidates in the election
#' @param beta_a double, parameter of the Beta law (by default 0.5)
#' @param beta_b double, parameter of the Beta law (by default 0.5)
#' @param lambda double, alternative parameter of the Beta law
#' @param min int, the minimum value of the range of possible scores (by default 0)
#' @param max int, the maximum value of the range of possible scores (by default 1)
#' @importFrom stats rbeta
#' @returns scores
#' @examples
#' voting_situation<- generate_beta(n_voters=10, n_candidates=3, beta_a=1, beta_b=5)
#'
generate_beta <- function(n_voters,n_candidates,beta_a = 0.5,beta_b = 0.5, lambda = 0,min = 0,max = 1) {
  #set.seed(2023)
  scores<-matrix(stats::rbeta(n_candidates*n_voters, shape1 = beta_a, shape2 = beta_b, ncp=lambda),c(n_candidates,n_voters))
  scores<-scores*(max-min)+min # on borne les prefs entre min et max
  scores <- rename_rows(scores)
  return(scores)
}

#' Generates a simulation of voting according to a uniform law, returns voters preferences
#' @export
#' @param n_voters integer, represents the number of voters in the election
#' @param n_candidates  integer, represents the number of candidates in the election
#' @param min int, the minimum value of the range of possible scores (by default 0)
#' @param max int, the maximum value of the range of possible scores (by default 1)
#' @importFrom stats runif
#' @returns scores
#' @examples
#' voting_situation<- generate_unif_continuous(n_voters=10, n_candidates=3, min=0, max=10)
generate_unif_continuous <-function(n_voters, n_candidates, min=0, max=1){
  #set.seed(2023)
  scores <- matrix(runif(n_candidates*n_voters, min=min, max=max),c(n_candidates,n_voters))
  scores <- rename_rows(scores)
  return(scores)
}


library(truncnorm)
#' Generate truncated normal scores
#'
#' This function generates truncated normal scores using the 'rtruncnorm' function from the 'truncnorm' package.
#'
#' @param n_candidates The number of candidates to generate scores for.
#' @param n_voters The number of voters to generate scores for.
#' @param min The minimum value of the truncated normal distribution.
#' @param max The maximum value of the truncated normal distribution.
#' @param mean The mean of the truncated normal distribution.
#' @param sd The standard deviation of the truncated normal distribution.
#'
#' @return A matrix of scores with 'n_candidates' rows and 'n_voters' columns.
#' @import truncnorm
#'
#' @export
#' @examples
#' voting_situation<- generate_norm(n_voters=10, n_candidates=3, min=0, max=10, mean=0.7)
generate_norm<-function(n_voters, n_candidates, min=0, max=1, mean=0.5, sd=0.25){
  #set.seed(2023)
  scores<-matrix(truncnorm::rtruncnorm(n_candidates*n_voters, a=min, b=max, mean = mean, sd = sd),c(n_candidates,n_voters))
  scores <- rename_rows(scores)
  return(scores)
}


########################
# DISCRETE SIMULATIONS #
########################


#' Generate uniform discrete scores
#'
#' This function generates uniform discrete scores on a given scale
#'
#' @param n_candidates integer,  The number of candidates to generate scores for.
#' @param n_voters integer, the number of voters to generate scores for.
#' @param min The minimum value of the  distribution, by default 0
#' @param max The maximum value of the  distribution, by default 10
#' @return A matrix of scores with 'n_candidates' rows and 'n_voters' columns.
#' @export
#' @examples
#' voting_situation <- generate_unif_disc(n_voters=10, n_candidates=3, min=0, max=5)
generate_unif_disc<-function(n_voters, n_candidates, min=0, max=10){
  #set.seed(2023)
  scores <- matrix(rep(sample(seq(from=min,to=max), n_voters, replace = TRUE), n_candidates),
                   c(n_candidates,n_voters))
  scores <- rename_rows(t(scores))
  return(scores)
}


#' Generate binomial scores
#'
#' This function generates  discrete scores following a binomial distribution on a given scale
#'
#' @param n_candidates integer,  The number of candidates to generate scores for.
#' @param n_voters integer, the number of voters to generate scores for.
#' @param min The minimum value of the  distribution, by default 0
#' @param max The maximum value of the  distribution, by default 10
#' @param mean The mean value of the  distribution, by default 5
#' @return A matrix of scores with 'n_candidates' rows and 'n_voters' columns.
#' @importFrom stats rbinom
#' @export
#' @examples
#' voting_situation <- generate_binomial(n_voters=10, n_candidates=3, min=0, max=7, mean=5)
generate_binomial<-function(n_voters, n_candidates, min=0, max=10, mean=5){
  #set.seed(2023)
  scores <- matrix(stats::rbinom(n_candidates*n_voters, size= max-min, prob=((mean-min)/(max-min))),c(n_candidates,n_voters))
  scores<-scores+min
  scores <- rename_rows(t(scores))
  return(scores)
}

#' Generate beta-binomial scores
#'
#' This function generates  discrete scores following a beta-binomial distribution on a given scale
#'
#' @param n_candidates integer,  The number of candidates to generate scores for.
#' @param n_voters integer, the number of voters to generate scores for.
#' @param min The minimum value of the  distribution, by default 0
#' @param max The maximum value of the  distribution, by default 10
#' @param alpha The first parameter of the beta-binomial distribution, by default 0.5
#' @param beta The second parameter of the beta-binomial distribution, by default 0.5
#' @return A matrix of scores with 'n_candidates' rows and 'n_voters' columns.
#' @import extraDistr
#' @export
#' @examples
#' voting_situation <- generate_beta_binomial(n_voters=10, n_candidates=3,  max=7)
generate_beta_binomial<-function(n_voters, n_candidates, min=0, max=10, alpha=0.5, beta=0.5){
  #set.seed(2023)
  scores <- matrix(extraDistr::rbbinom(n_candidates*n_voters, size=(max-min), alpha=alpha, beta=beta),c(n_candidates,n_voters))
  scores<-scores+min
  scores <- rename_rows(t(scores))
  return(scores)
}


#############################
# MULTINOMIAL AND DIRICHEL  #
# SIMULATIONS               #
#############################

#' Generate multinomial scores
#'
#' This function generates  discrete scores following a multinomial distribution on a given scale
#'
#' @param n_candidates integer,  The number of candidates to generate scores for.
#' @param n_voters integer, the number of voters to generate scores for.
#' @param max The maximum value of the  distribution, by default 10. It also corresponds to the sum of scores on all the candidates
#' @param probs A vector of size n_candidates corresponding to the parameters of the multinomial distribution. By default all values are equal to 1/n_candidates
#' @return A matrix of scores with 'n_candidates' rows and 'n_voters' columns.
#' @import stats
#' @export
#' @examples
#' voting_situation <- generate_multinom(n_voters=10, n_candidates=3,  max=100, probs=c(0.5, 0.3, 0.2))
generate_multinom<-function(n_voters, n_candidates, max=10, probs=0)
{
  if (length(probs)!=n_candidates){probs<-rep(1/n_candidates, n_candidates)}
  probs<-probs/(sum(probs))
  scores<-stats::rmultinom(n_voters, size= max, prob=probs)
  scores <- rename_rows(t(scores))
  return(scores)

}

#' Generate multinomial scores
#'
#' This function generates  scores following a Dirichlet distribution
#'
#' @param n_candidates integer,  The number of candidates to generate scores for.
#' @param n_voters integer, the number of voters to generate scores for.
#' @param probs A vector of size n_candidates corresponding to the parameters of the Dirichlet distribution. By default all values are equal to 1.
#' @return A matrix of scores with 'n_candidates' rows and 'n_voters' columns.
#' @import extraDistr
#' @export
#' @examples
#' voting_situation <- generate_dirichlet(n_voters=10, n_candidates=3,  probs=c(0.5, 0.3, 0.2))
generate_dirichlet<-function( n_voters, n_candidates, probs=0)
{
  if (length(probs)!=n_candidates){probs<-rep(1/n_candidates, n_candidates)}
  scores<-extraDistr::rdirichlet(n_voters, probs)
  scores <- rename_rows(t(scores))
  return(scores)
}

#############################
# COPULA BASED              #
# SIMULATIONS               #
#############################

#' Discrete Copula based scores
#'
#' This function generates  discrete scores following marginals distributions linked by a copula
#' #'
#' @param n_candidates integer,  The number of candidates to generate scores for.
#' @param n_voters integer, the number of voters to generate scores for.
#' @param min The minimum value of the  distribution, by default 0
#' @param max The maximum value of the  distribution, by default 10
#' @param margins A list of n_candidates cumulative distribution vectors of length (max-min-1) : the last value of the cumulative distribution, 1, should be omitted. By default margin distribution are uniform distributions.
#' @param cor_mat A matrix of correlation coefficients between the n_candidates distributions. By default all correlation coefficients are set up alternatively to 0.5 or -0.5.
#' @return A matrix of scores with 'n_candidates' rows and 'n_voters' columns.
#' @import GenOrd
#' @export
#' @examples
#' # Example for 3 candidates, binomial distributions
#'  min=0
#'  max=7
#' n_candidates<-3
#' distribution<-dbinom(x=(min:max), size=max, prob=0.7)
#' distribution_cumul<-cumsum(distribution)
#' distribution_cumul<-distribution_cumul[-length(distribution_cumul)]
#' margins <- matrix(rep(distribution_cumul, n_candidates), ncol=n_candidates)
#' margins <-as.list(as.data.frame(margins))
#' cor_mat<-matrix(c(1,0.8,0,0.8,1,0, 0,0,1), ncol=n_candidates)
#' voting_situation <- generate_discrete_copula_based(10, 3, max=max, margins=margins, cor_mat=cor_mat)
generate_discrete_copula_based<-function(n_voters, n_candidates, min=0, max=10, margins=list("default"), cor_mat=0)
  {
  score_levels<-min:max
  n_levels<-length(score_levels)
  if(identical(margins,list("default"))){
    distrib<-rep(1/n_levels, nlevels)
    distrib_cumul<-cumsum(distrib)
    distrib_cumul<-distrib_cumul[-length(distrib_cumul)]
    marginales <- matrix(rep(distrib_cumul, n_candidates), ncol=n_candidates)
    margins<- as.list(as.data.frame(marginales))
    }
  test_distrib<-is.list(margins)&(length(margins)==n_candidates)&all(lengths(margins)==(n_levels-1))
  if (test_distrib==FALSE){stop("margin distributions are not consistant")}
  if (identical(cor_mat,0)){
    cor_mat<-matrix(nrow=n_candidates, ncol=n_candidates)
    for (i in 1:n_candidates)
      {for(j in 1:n_candidates)
        {if (i==j){
                cor_mat[i,j]<-1
                  }else{
                  cor_mat[i,j]<-0.5*(-1)^(i+j)
                        }
        }
      }
  }
  support<-list()
  for (i in 1:n_candidates){support[[i]]<-score_levels}
  scores<-GenOrd::ordsample(n_voters, margins, cor_mat, support = support, Spearman = FALSE, cormat = "discrete")
  scores <-scores +min
  scores <- rename_rows(t(scores))
  return(scores)
}



#############################
# SPATIAL                   #
# SIMULATIONS               #
#############################

#' Generate spatial simulation
#'
#' This function generates spatial data consisting of \code{n_voters} voters and \code{n_candidates} candidates. The spatial model is created by placing the candidates on a 2-dimensional plane according to the \code{placement} parameter, and then computing a distance matrix between voters and candidates. The distances are then transformed into scores using the \code{score_method} parameter. Finally, a plot of the candidates and voters is produced.
#'
#' @param n_voters The number of voters.
#' @param n_candidates The number of candidates.
#' @param placement The method used to place the candidates on the 2-dimensional plane. Must be either "uniform" or "beta". Default is "uniform".
#' @param score_method The method used to transform distances into scores. Must be either "linear" or "sigmoide". Default is "linear".
#' @param dim The dimension of the latent space (by default dim =2)
#' @return A matrix of scores.
#' @export
#' @importFrom graphics text
#' @importFrom graphics points
#' @importFrom stats rbeta
#' @importFrom stats runif
#' @examples
#' generate_spatial(n_candidates = 5,n_voters = 100,  placement = "uniform", score_method = "linear")
generate_spatial <- function(n_voters,n_candidates,placement = "uniform",score_method = "linear", dim=2){
  #set.seed(2023)
  if (dim<1){
    warning("dim must be at least 1 - dim has been set up to 2")
    dim <- 2} # constante
  # === placement === #
  if (placement == "uniform"){
    candidates<-matrix(stats::runif(n_candidates*dim), nrow = n_candidates, ncol=dim)
    voters<-matrix(stats::runif(n_voters*dim), nrow = n_voters, ncol=dim)
  }else if(placement == "beta"){
    beta_a= 1.2 # 2 = points centrés
    beta_b= 1.2
    candidates <- matrix(stats::rbeta(n_candidates*dim, shape1 = beta_a, shape2 = beta_b),nrow = n_candidates,ncol = dim)
    voters <- matrix(stats::rbeta(n_voters*dim, shape1 = beta_a, shape2 = beta_b), nrow = n_voters, ncol = dim)
  }else{
    stop("placement must be 'uniform' or 'beta'")  }
  # === distance between voters / candidates} === #
  matrix_distances<-apply(voters,1, function(x) distance(x,candidates))

  # === distance to score === # (linear / sigmoide)
  matrix_scores<-DistToScores(matrix_distances,method = score_method)

  #View(matrix_distances) # test
  #View(matrix_scores) # test

  # === plots === #
  plot(candidates, xlab="dim. 1", ylab="dim. 2", xlim=c(0,1), ylim=c(0,1), col="red", pch=c(17), cex=1.5, main="Spatial model")
  text(candidates[,1]+0.001, candidates[,2]+0.001, labels=1:n_candidates, pos=4, col="red")
  if(n_voters <= 200){
    points(voters)
  }else{
    points(voters[sample(n_voters,200),])
  }

  #pref_rank <- preferences_to_ranks(matrix_scores)
  #View(pref_rank)
  #View(distance_to_pref(pref_rank))
  return(rename_rows(matrix_scores))
}

