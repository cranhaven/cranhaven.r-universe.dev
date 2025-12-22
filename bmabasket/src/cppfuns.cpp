// [[Rcpp::depends(RcppArmadillo)]]

#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
#include <RcppArmadillo.h>


using namespace Rcpp;

//' Compute number of models
//' 
//' Given a basket size and maximal number of distinct
//' response rates, compute the number of possible models
//' 
//' @param K positive integer giving number of baskets
//' @param P positive integer giving maximal number of distinct rates
//' 
//' @return integer giving number of possible models
//' @noRd
// [[Rcpp::export]]
int numModels_cpp( int const& K, int const& P ) {
  double res = 0;
  for ( int p = 1; p <= P; p++ ) {
    double temp = 0;
    for ( int j = 0; j <= P; j++ ) {
      temp += std::pow(-1.0, p-j) * std::pow( (double) j, (double) K) * R::choose(p, j);
    }
    res += std::pow( R::gammafn(p + 1.0) , -1) * temp;
  }
  return( (int) res);
}


//' Log posterior survival probability
//' 
//' Given a vector that gives a partition, computes the posterior probability 
//' that the basket proportion is larger than some specified value
//' 
//' @param pi0        vector whose elements are between 0 and 1 giving threshold for the hypothesis test. If a scalar is provided, assumes same threshold for each basket.
//' @param datMat     matrix of data. The first column gives the number of "successes" and second gives number of "failures" for each basket.
//' @param partition  vector giving a partition for a particular model
//' @param a0         beta prior shape parameter
//' @param b0         beta prior shape parameter
//' @param lbeta_a0b0 scalar giving \code{lbeta(a0, b0)}
//' 
//' @return list giving posterior probability, mean, and numerator for model probability for given model.
//' @noRd
// [[Rcpp::export]]
List logPostProb(
    arma::vec const& pi0,
    arma::mat const& datMat,
    arma::vec const& partition,
    double    const& a0,
    double    const& b0,
    double    const& lbeta_a0b0
) {

  // max index + 1 = size of partition = number of distinct params
  int m = partition.max() + 1;                
  
  // initialize matrix pertaining to model
  arma::mat partMat = arma::mat( m, datMat.n_cols, arma::fill::zeros );
  
  // initialize posterior probabilities for futility and efficacy
  arma::vec pp = pi0;
  arma::vec mn = pi0;
  
  // initialize to be -m * log[Beta(a0, b0)] where m = partition size
  double sumLogBeta = -m * lbeta_a0b0;   
  
  // loop over number of distinct params in partition
  for ( int i = 0; i < m; i++ ) {
    
    // store index vector giving which rows in datMat pertain to partition
    arma::uvec partIndx = find( partition == i );
    
    // sum columns of datMat pertaining the i^th element of the partition: --> (y, n-y) for y in partition
    arma::rowvec partDat_i = arma::sum( datMat.rows( partIndx ) , 0 );
    
    // compute posterior parameters
    double a_jp = a0 + partDat_i(0);
    double b_jp = b0 + partDat_i(1);
    
    // add on log[Beta(a_jp, b_jp)] to sumLogBeta
    sumLogBeta += R::lbeta(a_jp, b_jp);
    
    // compute poterior probability at pi_0k for each basket in the partition
    for ( unsigned int k = 0; k < partIndx.size(); k++ ) {
      pp(partIndx(k)) = R::pbeta( pi0(partIndx(k)), a_jp, b_jp, 0.0, 1.0 );
      mn(partIndx(k)) = log(a_jp) - log(a_jp + b_jp);
    }
  }
  
  // create vector for posterior mean
  
  return List::create(
    _["pp"]         = pp,
    _["mn"]         = mn,
    _["sumLogBeta"] = sumLogBeta
  );
}






//' Bayesian model averaging (C++ function)
//'
//' Computes posterior model probabilities and Bayes model averaged survival function \math{P(\pi_k > pi0 | D)}
//'
//' @param pi0            vector whose elements are between 0 and 1 giving cutoffs for effect size for each basket
//' @param datMat         matrix of data \code{(y, n - y)}
//' @param partitionMat   martrix giving how to partition the data for each model
//' @param mu0            scalar giving prior mean for beta prior
//' @param phi0           scalar giving prior dispersion for beta prior
//' @param logModelPriors vector of length P giving the normalized priors for each model
//'
//' @return a list giving posterior probability and posterior mean
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
Rcpp::List bma_cpp(
    arma::vec const& pi0,
    arma::mat const& datMat,
    arma::mat const& partitionMat,
    double    const& mu0,
    double    const& phi0,
    arma::vec const& logModelPriors
) {
  
  // Store BMA probabilities in empty vector
  arma::vec bmaProbs = arma::vec( pi0.size(), arma::fill::zeros );
  arma::vec bmaMeans  = arma::vec( pi0.size(), arma::fill::zeros );
    
  double modelProbDenominator = 0;
  
  // compute beta prior shape parameters
  double a0         = mu0 * phi0;
  double b0         = (1 - mu0) * phi0;
  double lbeta_a0b0 = R::lbeta(a0, b0);
  
  // loop through models (partitions),
  for ( unsigned int j = 0; j < partitionMat.n_cols; j++ ) {
    
    // Obtain Pr(pi_k > pi_0k | Mj, D) and log unnormalized weight for model Mj
    List postModel      = logPostProb( pi0, datMat, partitionMat.col(j), a0, b0, lbeta_a0b0 );
    
    // increment bmaProbs by unweighted posterior model probability; add to denominator of posterior model probs
    double logModelProbNumerator  = postModel["sumLogBeta"]; // initialize to sum( log(Beta(a_jp, b_jp) - log(B(a0, b0) ) ) )
    logModelProbNumerator        += logModelPriors(j);       // add on prior model probability
    arma::vec postProb            = postModel["pp"];         // log[ Pr(pi_k > pi_0k | Mj, D) ]
    arma::vec postMean            = postModel["mn"];         // log[ a_jp / (a_jp + b_jp) ]
    
    bmaProbs             += arma::exp( logModelProbNumerator + postProb );
    bmaMeans             += arma::exp( logModelProbNumerator + postMean );
    modelProbDenominator += exp( logModelProbNumerator );
  }
  
  return List::create(
    _["bmaProbs"]   = bmaProbs / modelProbDenominator,
    _["bmaMeans"]   = bmaMeans / modelProbDenominator
  );
} 


