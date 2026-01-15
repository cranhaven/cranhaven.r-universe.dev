#include <RcppArmadillo.h>

using namespace Rcpp;



////////////////////
// variable names //
////////////////////

// note that 'i' index is often dropped in comments

// N		number of obs seqs
// T_i		number of obs in obs sequence i - not actually used
// K		number of states
// M		number of features measured at each observation time

// i \in \{1, ..., N\}	 		indexes obs seqs
// t \in \{1, ..., T_i\} 		indexes obs in seq i
// k \in \{1, ..., K\}	 		indexes states
// m \in \{1, ..., M\} 			indexes features

// alpha_i	matrix, [K x T]		scaled forward probs, \propto Pr(X_1=x_1, ..., X_t=x_t, Z_t=k) - not actually used
// lalpha_i	matrix, [K x T]		log alpha_i
// lalphas	list, length N		collects lalpha_i

// beta_i	matrix, [K x T]		scaled backward probs, \propto Pr(X_{t+1}=x_{t+1}, ..., X_T=x_T | Z_t=k) - not actually used
// lbeta_i	matrix, [K x T]		log beta_i
// lbetas	list, length N		collects lbeta_i

// Gamma	matrix, [K x K]		row-stochastic transition probs

// lambda	scalar				ridge-like regularization (added to diagonal of Sigma_i)

// mu		vector, [M x 1]		mean of state distr - not actually used
// mus		matrix, [M x K]		collects state means

// Sigma_i	matrix, [M x M]		cov mat of state distr - not actually used
// Sigmas	tensor, [K x M x M]	collects state cov mats

// Ts		vector, length N	collects T_i

// X_it		vector, length M	features in obs t in obs seq i - not actually used
// X_i		matrix, [T_i x M]	obs seq i - not actually used
// Xs		list, length N		collects X_i - std::vector

// Z_it		scalar				state membership of obs t in obs seq i - not observed
// zeta_i	matrix, [K x T_i]	Pr(Z_it = k) - not actually used
// zetas	list, length N		collects zeta_i - std::vector

const double log2pi = std::log(2.0 * M_PI);

// workhorse multivariate normal density for basic hmm
// allows for partial censoring of features

// [[Rcpp::export]]
arma::vec dmvnorm_cens(
    arma::mat X,  			  // data, [T x M]
    arma::rowvec mu,  	// mean vector, [1 x M]
    arma::mat Sigma, 			// covariance matrix, [M x M]
    arma::uvec missingness_labels_i,  // pattern of missingness features
    std::vector< arma::uvec > nonmissing_features,
    bool logd = false,		// return logarithm?
    double lambda = 0  		// ridge-like regularization, added to Sigma.diag()
) {
  // modified from fast dmvnorm() implementation by Nino Hardt and Dicko Ahmadou
  // http://gallery.rcpp.org/articles/dmvnorm_arma/
  // in turn based on bayesm::dMvn() by Peter Rossi

  arma::uword n = X.n_rows;
  // TODO: check
  //arma::uword M = X.n_cols;
  arma::uword nlabels = nonmissing_features.size();

  arma::vec out(n);
  Sigma.diag() += lambda;  // ridge-like regularization

  std::vector< arma::mat > rooti;
  std::vector<double> rootisum;
  std::vector<double> constants;
  for (arma::uword m=0; m<nlabels; m++){
    if (nonmissing_features[m].n_rows > 0){
      arma::mat Sigma_censor =
        Sigma.submat(nonmissing_features[m], nonmissing_features[m]);
      try {
        rooti.push_back(arma::trans(arma::inv(trimatu(arma::chol(Sigma_censor)))));
      } catch(std::exception &ex) {
        Rcpp::Rcout << "error in missingness mode " << (m+1) << std::endl;
        // // Rcpp::Rcout << "following features observed in this mode: " << nonmissing_features[m] << std::endl;
        Rcpp::Rcout << "failed to invert covariance matrix" << std::endl;
        if (Sigma.n_rows > 5){
          Rcpp::Rcout << "showing Sigma[1:5, 1:5] (after dropping censored features):" << std::endl;
          Rcpp::Rcout << Sigma_censor.submat(1,1,5,5) << std::endl;
        } else {
          Rcpp::Rcout << Sigma_censor << std::endl;
        }
        forward_exception_to_r(ex);
      }
      rootisum.push_back(arma::sum(log(rooti[m].diag())));
      constants.push_back(-(static_cast<double>(nonmissing_features[m].n_rows)/2.0) * log2pi);
    } else {
      rooti.push_back(arma::zeros<arma::vec>(1));
      rootisum.push_back(0);
      constants.push_back(0);
    }
  }

  for (arma::uword t=0; t<n; t++) {
    arma::uword m = missingness_labels_i(t);
    arma::rowvec X_row = X.row(t);
    if (nonmissing_features[m].n_rows > 0){
      arma::vec z = rooti[m] *
        (X_row(nonmissing_features[m]) - mu(nonmissing_features[m]));
      out(t) = constants[m] - 0.5 * arma::sum(z % z) + rootisum[m];
    } else {
      out(t) = 0;
    }
  }

  if (logd == false) {
    out = exp(out);
  }

  return(out);
}



// modified multivariate normal density
// accounts for autocorrelation
// X must include lagged features
// [[Rcpp::export]]
arma::vec dmvnorm_cond(
    arma::mat X,  			  // data, [T x M]
    arma::rowvec mu,  	// mean vector, [1 x M]
    arma::mat Sigma, 			// covariance matrix, [M x M]
    arma::uvec labels_t,  // current features to analyze
    arma::uvec labels_y,  // past features to condition on
    bool logd = false,		// return logarithm?
    double lambda = 0  		// ridge-like regularization, added to Sigma.diag()
) {
  // modified from fast dmvnorm() implementation by Nino Hardt and Dicko Ahmadou
  // http://gallery.rcpp.org/articles/dmvnorm_arma/
  // in turn based on bayesm::dMvn() by Peter Rossi

  arma::uword n = X.n_rows;
  // TODO: check
  // arma::uword M = X.n_cols;

  arma::vec out(n);
  Sigma.diag() += lambda;  // ridge-like regularization

  arma::mat rooti;
  double rootisum;
  double constant;

  // t: current frame (today)
  // y: previous frame (yesterday)
  arma::mat Sigma_tt =
    Sigma.submat(labels_t, labels_t);
  arma::mat Sigma_ty =
    Sigma.submat(labels_t, labels_y);
  arma::mat Sigma_yt =
    Sigma.submat(labels_y, labels_t);
  arma::mat Sigma_yy =
    Sigma.submat(labels_y, labels_y);
  arma::mat Sigma_yy_inv = Sigma_yy.i();

  // Schur complement of Sigma_yy
  arma::mat Sigma_tt_cond =
    Sigma_tt - Sigma_ty * Sigma_yy_inv * Sigma_yt;

  arma::mat mu_cond(labels_t.n_rows, n);
  for (arma::uword t=0; t<n; t++){
    arma::rowvec X_row = X.row(t);
    arma::rowvec X_row_y = X_row.cols(labels_y);
    arma::vec dev_y =
      arma::trans( X_row_y - mu.cols(labels_y) );
    mu_cond.col(t) =
      mu(labels_t) + Sigma_ty * Sigma_yy_inv * dev_y;
  }

  try {
    rooti = arma::trans(arma::inv(trimatu(arma::chol(Sigma_tt_cond))));
  } catch(std::exception &ex) {
    Rcpp::Rcout << "failed to invert covariance matrix" << std::endl;
    if (Sigma.n_rows > 5){
      Rcpp::Rcout << "showing Sigma[1:5, 1:5] (after dropping censored features):" << std::endl;
      Rcpp::Rcout << Sigma_tt_cond.submat(1,1,5,5) << std::endl;
    } else {
      Rcpp::Rcout << Sigma_tt_cond << std::endl;
    }
    forward_exception_to_r(ex);
  }
  rootisum = arma::sum(log(rooti.diag()));
  constant = -(static_cast<double>(labels_t.n_rows/2.0)) * log2pi;

  for (arma::uword t=0; t<n; t++) {
    arma::rowvec X_row = X.row(t);
    arma::rowvec X_row_t = X_row.cols(labels_t);
    arma::vec z = rooti *
      (X_row_t.t() - mu_cond.col(t));
    out(t) = constant - 0.5 * arma::sum(z % z) + rootisum;
  }

  if (logd == false) {
    out = exp(out);
  }
  return(out);
}



//////////////////////////
// helper HMM functions //
//////////////////////////

// forward probs: alpha_t * C = Pr(X_1=x_1, ..., X_t=x_t, Z_t=k)
// [[Rcpp::export]]
arma::mat forward(
    arma::rowvec delta,		  // initial distribution, [1 x K]
    arma::mat Gamma,			  // transition matrix, [K x K]
    arma::mat tstateprobs,	// scaled prob that state k generated obs t: Pr(X_t=x_t | Z_t=k) ] / C_t, [T x K]
    arma::vec scale         // scaling factor C_t for all t
) {
  arma::uword T = tstateprobs.n_cols;		// number of observations
  arma::uword K = Gamma.n_rows;			// number of states

  arma::rowvec alpha_t(K);							// forward probs at t (running product)
  arma::mat lalpha(K, T);								// log forward probs for each t

  arma::vec cumscale = arma::cumsum(scale);

  alpha_t = delta % tstateprobs.col(0).t();			// calculate forward probs at t=1
  lalpha.col(0) = log(alpha_t).t() + cumscale(0);					// store log forward probs for t=1

  double C_t = arma::accu(alpha_t);					// scaling factor to prevent underflow
  double log_C = log(C_t);							// running sum of log scaling factors
  alpha_t = alpha_t / C_t;							// scaled forward probs at t=1

  for (arma::uword t=1; t<T; t++) {
    alpha_t = alpha_t * Gamma % tstateprobs.col(t).t();
    C_t = arma::accu(alpha_t);
    alpha_t = alpha_t / C_t;
    log_C += log(C_t);
    lalpha.col(t) = log(alpha_t).t() + log_C + cumscale(t);
  }

  return lalpha;
}



// backward probs: beta_t * C = Pr(X_{t+1}=x_{t+1}, ..., X_T=x_T | Z_t=k)
// [[Rcpp::export]]
arma::mat backward(
    arma::mat Gamma,		     // transition matrix, [K x K]
    arma::mat tstateprobs,	 // scaled prob that state k generated obs t: Pr(X_t=x_t | Z_t=k) / C_t, [K x T]
    arma::vec scale         // scaling factor C_t for all t
) {

  arma::uword T = tstateprobs.n_cols;		// number of observations
  arma::uword K = Gamma.n_rows;			// number of states

  arma::vec beta_t(K);								// backward probs at t (running product)
  arma::mat lbeta(K, T);								// log backward probs for each t

  beta_t = arma::vec(K, arma::fill::ones);			// set backward probs at t=T
  lbeta.col(T-1) = log(beta_t);						// store log backward probs for t=T

  arma::vec revscale = arma::flipud(scale);
  arma::vec revcumscale = arma::flipud(arma::cumsum(revscale));

  double C_t = arma::accu(beta_t);					// scaling factor to prevent underflow
  double log_C = log(C_t);							// running sum of log scaling factors
  beta_t = beta_t / C_t;

  for (arma::uword t=T-1; t > 0; t--) {
    beta_t = Gamma * (tstateprobs.col(t) % beta_t);
    C_t = arma::accu(beta_t);
    beta_t = beta_t / C_t;
    log_C += log(C_t);
    lbeta.col(t-1) = log(beta_t) + log_C + revcumscale(t);
  }

  return lbeta;
}



// forward probs: alpha_t * C = Pr(X_1=x_1, ..., X_t=x_t, Z_t=k)
// [[Rcpp::export]]
arma::mat forward_upper(
    std::vector<arma::mat> Gammas,			  // list of transition matrices (length T-1), each [K x K]
    arma::mat tstateprobs	// unscaled prob that state k generated obs t: Pr(X_t=x_t | Z_t=k) ] / C_t, [T x K]
) {

  arma::uword T = tstateprobs.n_cols;		            // number of observations
  arma::uword K = Gammas[0].n_rows;			        // number of states

  arma::rowvec alpha_t(K);							// forward probs at t (running product)
  arma::mat lalpha(K, T);								// log forward probs for each t

  alpha_t = tstateprobs.col(0).t();                   // calculate forward probs at t=1
  lalpha.col(0) = log(alpha_t).t();					// store log forward probs for t=1

  double C_t = arma::accu(alpha_t);					// scaling factor to prevent underflow
  double log_C = log(C_t);							// running sum of log scaling factors
  alpha_t = alpha_t / C_t;							// scaled forward probs at t=1

  for (arma::uword t=1; t<T; t++){
    alpha_t = alpha_t * Gammas[t-1] % tstateprobs.col(t).t();
    C_t = arma::accu(alpha_t);
    alpha_t = alpha_t / C_t;
    log_C += log(C_t);
    lalpha.col(t) = log(alpha_t).t() + log_C;
  }

  return lalpha;
}



// backward probs: beta_t * C = Pr(X_{t+1}=x_{t+1}, ..., X_T=x_T | Z_t=k)
// [[Rcpp::export]]
arma::mat backward_upper(
    std::vector<arma::mat> Gammas,			  // list of transition matrices (length T-1), each [K x K]
    arma::mat tstateprobs  // unscaled prob that state k generated obs t: Pr(X_t=x_t | Z_t=k) ] / C_t, [T x K]
) {
  arma::uword T = tstateprobs.n_cols;		            // number of observations
  arma::uword K = Gammas[0].n_rows;       			// number of states

  arma::vec beta_t(K);								// backward probs at t (running product)
  arma::mat lbeta(K, T);								// log backward probs for each t

  beta_t = arma::vec(K, arma::fill::ones);			// set backward probs at t=T
  lbeta.col(T-1) = log(beta_t);						// store log backward probs for t=T

  double C_t = arma::accu(beta_t);					// scaling factor to prevent underflow
  double log_C = log(C_t);							// running sum of log scaling factors
  beta_t = beta_t / C_t;

  for (arma::uword t=T-1; t > 0; t--) {
    beta_t = Gammas[t-1] * (tstateprobs.col(t) % beta_t);
    C_t = arma::accu(beta_t);
    beta_t = beta_t / C_t;
    log_C += log(C_t);
    lbeta.col(t-1) = log(beta_t) + log_C;
  }

  return lbeta;
}


//////////////////////////////////////////////
// train HMM on multiple observation chains //
//////////////////////////////////////////////

// [[Rcpp::export]]
Rcpp::List hmm_cpp(
    std::vector<arma::mat> Xs,		       // list of data, list length N, element i [Ts[i] x M]
    arma::vec weights,
    arma::rowvec delta_init,		         // initial distribution
    arma::mat mus_init,			             // state-specific means: rows=covariates, cols=state
    std::vector<arma::mat> Sigmas_init,  // state-specific vcov matrices: rows/cols=covariates, elements=state
    arma::mat Gamma_init,			           // transition matrix
    std::vector<arma::mat> zetas_init,   // responsibilities (ignored unless supervised == true)
    std::vector< arma::uvec > nonmissing,           // indices of obs with no missingness
    std::vector< arma::uvec > missingness_labels,   // pattern of missingness features
    std::vector< arma::uvec > nonmissing_features,  // details on each missingness type
    double lambda = 0,				           // regularization parameter
    double tol = 1e-6,				           // terminate EM when log-likelihood increase < tol...
    arma::uword maxiter = 100,		       // ... or after maxiter iterations
    double uncollapse = 0,			         // randomly reset collapsing (small-volume) components, i.e. det(Sigma) < uncollapse
    bool verbose = true,				         // status updates
    bool supervised = false
) {

  arma::uword N = Xs.size();				// number of observation sequences
  std::vector<arma::uword> Ts;			// number of observations in each obs sequence
  for (arma::uword i=0; i<N; i++){
    Ts.push_back(Xs[i].n_rows);
  }
  arma::uword M = Xs[0].n_cols;			// number of measured covariates
  arma::uword K = Gamma_init.n_rows;		// number of states

  // for obs seq i: rows=states, cols=time
  std::vector<arma::mat> lalphas;
  std::vector<arma::mat> lbetas;
  std::vector<arma::mat> zetas;

  if (supervised) {
    zetas = zetas_init;
  } else {
    for (arma::uword i=0; i<N; i++) {
      zetas.push_back(arma::mat(K, Ts[i]));		// responsibilities: E[Z_t=k | data]
      zetas[i].col(0) = delta_init.t();		  	// initial state distribution
    }
  }

  // for obs seq i: rows=time, cols=states
  std::vector<arma::mat> lstateprobs;

  for (arma::uword i=0; i<N; i++) {
    lalphas.push_back(arma::mat(K, Ts[i]));		// forward probs: Pr(X_1=x_1, ..., X_t=x_t, Z_t=k)
    lbetas.push_back(arma::mat(K, Ts[i]));		// backward probs: Pr(X_{t+1}=x_{t+1}, ..., X_T=x_T | Z_t=k)
    lstateprobs.push_back(arma::mat(Ts[i], K));	// prob that state k generated obs t: Pr(X_t=x_t | Z_t=k)
  }

  // track log-likelihood
  std::vector<double> llh_seq;
  llh_seq.push_back(-std::numeric_limits<double>::infinity());

  // log-likelihood of each obs seq | parameter estimates in current iter
  arma::vec llhs(N);

  // read in transition probs
  arma::mat Gamma = Gamma_init;

  // read in initial state parameters
  arma::mat mus = mus_init;					// state-specific means: rows=covariates, cols=state
  arma::cube Sigmas(M, M, K);				// state-specific vcov matrices: rows/cols=covariates, slices=state

  for (arma::uword k=0; k<K; k++) {
    Sigmas.slice(k) = Sigmas_init[k];
  }

  // resetting collapsed states
  double log_uncollapse = log(uncollapse);
  int resets = 0;

  // EM algorithm
  for (arma::uword iter=0; iter<maxiter; iter++) {

    // detect collapsed components and reset (assumes that input features have been standardized)
    if (!supervised){
      if (uncollapse > 0){
        for (arma::uword k=0; k<K; k++){
          double volume;
          double sign;
          arma::log_det(volume, sign, Sigmas.slice(k));		// volume of state distribution
          if (volume < log_uncollapse){
            resets += 1;
            if (verbose){
              Rcpp::Rcout << "collapsed state detected; resetting" << std::endl;
            }
            mus.col(k) = arma::randn(M);					        // redraw state mean from std diag multivariate normal
            Sigmas.slice(k) = arma::eye<arma::mat>(M, M);	// reset state cov mat to unit sphere
          }
        }
      }
    }

    if (verbose) {
      Rcpp::Rcout << "iter " << iter + 1 << ": ";
    }

    for (arma::uword i=0; i<N; i++) {

      // state probs Pr[X_t=x | Z_t=k, mus, Sigmas]: loop over clusters, plug X into dmvnorm for each
      for (arma::uword k=0; k<K; k++) {
        // if (verbose){
        //   Rcpp::Rcout << "calculating log probs of obs seq " << (i+1) << " in state " << (k+1) << std::endl;
        // }// debug
        lstateprobs[i].col(k) =
          dmvnorm_cens(Xs[i],
                       mus.col(k).t(), Sigmas.slice(k),
                       missingness_labels[i],
                                         nonmissing_features,
                                         true, lambda);
        // Rcpp::Rcout << lstateprobs[i].col(k) << std::endl; // debug
      }

      arma::vec scale = arma::max(lstateprobs[i], 1); // pseudocode: get max{ log Pr( X_t=x_t | Z_t=k ) : all k }
      arma::mat lstateprobs_scaled = lstateprobs[i];
      for (arma::uword k=0; k<K; k++){
        lstateprobs_scaled.col(k) -= scale;
      }
      arma::mat tstateprobs = exp(lstateprobs_scaled).t();	// t() for faster column access in forward/backward probs

      // convert to working parameters: forward, backward probs
      lalphas[i] = forward(zetas[i].col(0).t(), Gamma, tstateprobs, scale);
      lbetas[i] = backward(Gamma, tstateprobs, scale);

      double log_C_i = max(lalphas[i].col(Ts[i]-1));
      llhs(i) = log(arma::accu(exp(lalphas[i].col(Ts[i]-1) - log_C_i))) + log_C_i;

    }

    double llh = arma::accu(llhs);
    double llh_diff = llh - llh_seq.back();
    llh_seq.push_back(llh);

    if (llh_diff < tol && llh_diff >= 0) {
      if (verbose) {
        Rcpp::Rcout << "log-likelihood increase < tol; stopping" << std::endl;
      }
      break;
    }

    if (verbose) {
      Rprintf("log-likelihood of %f\n", llh);
    }

    arma::mat Gamma_old = Gamma;	// store old transition matrix
    Gamma.zeros();					// reset all elements to zero

    for (arma::uword i=0; i<N; i++) {

      // E-step (states)
      if (!supervised) {
        zetas[i] = exp(lalphas[i] + lbetas[i] - llhs(i)); // zeta[k, t]: E[Z_t=k | data]

        for (arma::uword t=0; t<Ts[i]; t++) {
          double zetas_normalize = arma::accu(zetas[i].col(t));
          zetas[i].col(t) /= zetas_normalize; // normalize state memberships of each obs
        }
      }

      // E-step (transitions)

      arma::mat lstateprobs_dropfirst = lstateprobs[i].submat(arma::span(1, Ts[i]-1), arma::span::all);
      arma::mat lalpha_droplast = lalphas[i].submat(arma::span::all, arma::span(0, Ts[i]-2));
      lalpha_droplast = lalpha_droplast.t();
      arma::mat lbeta_dropfirst = lbetas[i].submat(arma::span::all, arma::span(1, Ts[i]-1));
      lbeta_dropfirst = lbeta_dropfirst.t();

      for (arma::uword k1=0; k1<K; k1++) {
        for (arma::uword k2=0; k2<K; k2++) {
          Gamma(k1, k2) += weights(i) *
            arma::accu(exp(lalpha_droplast.col(k1)
                             + lstateprobs_dropfirst.col(k2)
                             + lbeta_dropfirst.col(k2)
                             - llhs(i)
            ));
        }
      }

    }
    Gamma = Gamma % Gamma_old;

    // normalize transition probs out of each state
    for (arma::uword k=0; k<K; k++){
      Gamma.row(k) /= arma::accu(Gamma.row(k));
    }

    // M-step (mu_k)
    mus.zeros();		// reset all elements to zero
    arma::vec zeta_sums(K, arma::fill::zeros);

    // weighted sum by responsibility (ignore partially censored obs)
    for (arma::uword i=0; i<N; i++){
      mus += weights(i) * (zetas[i].cols(nonmissing[i]) * Xs[i].rows(nonmissing[i])).t();
      zeta_sums += weights(i) * arma::sum(zetas[i].cols(nonmissing[i]), 1);
    }

    // divide by effective number of obs (ignore partially censored obs)
    for (arma::uword k=0; k<K; k++){
      mus.col(k) /= zeta_sums(k);
    }

    // M-step (Sigma_k)
    Sigmas.zeros();		// reset all elements to zero

    // weighted variance by responsibility (ignore partially censored obs)
    for (arma::uword k=0; k<K; k++){
      for (arma::uword i=0; i<N; i++){
        arma::mat X_demean = Xs[i].rows(nonmissing[i]);
        for (arma::uword m=0; m<M; m++){
          X_demean.col(m) -= mus(m, k);
        }
        arma::rowvec zetas_ik = zetas[i].row(k) ;
        Sigmas.slice(k) += weights(i) * X_demean.t() * arma::diagmat(zetas_ik(nonmissing[i])) * X_demean;
      }
      Sigmas.slice(k) /= zeta_sums(k);
    }

    if (verbose){
      if (iter == maxiter - 1){
        Rcpp::Rcout << "maxiter reached; stopping" << std::endl;
      }
    }

  }

  Rcpp::List lstateprobs_out;
  for (arma::uword i=0; i<N; i++){
    lstateprobs_out.push_back(lstateprobs[i]);
  }

  Rcpp::List mus_out;
  Rcpp::List Sigmas_out;
  for (arma::uword k=0; k<K; k++){
    mus_out.push_back(arma::conv_to< std::vector<double> >::from(mus.col(k)));
    Sigmas.slice(k).diag() += lambda;
    Sigmas_out.push_back(Sigmas.slice(k));
  }

  Rcpp::List zetas_out;
  for (arma::uword i=0; i<N; i++){
    zetas_out.push_back(zetas[i]);
  }

  return Rcpp::List::create(
    Rcpp::Named("lstateprobs") = lstateprobs_out,
    Rcpp::Named("llh_seq") = llh_seq,
    Rcpp::Named("llhs") = llhs,
    Rcpp::Named("Gamma") = Gamma,
    Rcpp::Named("mus") = mus_out,
    Rcpp::Named("Sigmas") = Sigmas_out,
    Rcpp::Named("zetas") = zetas_out,
    Rcpp::Named("resets") = resets
  );
}


// [[Rcpp::export]]
Rcpp::List hmm_autocorr_cpp(
    std::vector<arma::mat> Xs,		       // list of data, list length N, element i [Ts[i] x M]
    arma::vec weights,
    arma::rowvec delta_init,		         // initial distribution
    arma::mat mus_init,			             // state-specific means: rows=covariates, cols=state
    std::vector<arma::mat> Sigmas_init,  // state-specific vcov matrices: rows/cols=covariates, elements=state
    arma::mat Gamma_init,			           // transition matrix
    std::vector<arma::mat> zetas_init,   // responsibilities (ignored unless supervised == true)
    arma::uvec labels_t,           // indices of obs with no missingness
    arma::uvec labels_y,   // pattern of missingness features
    double lambda = 0,				           // regularization parameter
    double tol = 1e-6,				           // terminate EM when log-likelihood increase < tol...
    arma::uword maxiter = 100,		       // ... or after maxiter iterations
    double uncollapse = 0,			         // randomly reset collapsing (small-volume) components, i.e. det(Sigma) < uncollapse
    bool verbose = true,				         // status updates
    bool supervised = false
) {
  arma::uword N = Xs.size();				// number of observation sequences
  std::vector<arma::uword> Ts;			// number of observations in each obs sequence

  for (arma::uword i=0; i<N; i++) {
    Ts.push_back(Xs[i].n_rows);
  }

  arma::uword M = Xs[0].n_cols;			// number of measured covariates
  arma::uword K = Gamma_init.n_rows;		// number of states

  // for obs seq i: rows=states, cols=time
  std::vector<arma::mat> lalphas;
  std::vector<arma::mat> lbetas;
  std::vector<arma::mat> zetas;

  if (supervised){
    zetas = zetas_init;
  } else {
    for (arma::uword i=0; i<N; i++) {
      zetas.push_back(arma::mat(K, Ts[i]));		// responsibilities: E[Z_t=k | data]
      zetas[i].col(0) = delta_init.t();		  	// initial state distribution
    }
  }

  // for obs seq i: rows=time, cols=states
  std::vector<arma::mat> lstateprobs;

  for (arma::uword i=0; i<N; i++){
    lalphas.push_back(arma::mat(K, Ts[i]));		// forward probs: Pr(X_1=x_1, ..., X_t=x_t, Z_t=k)
    lbetas.push_back(arma::mat(K, Ts[i]));		// backward probs: Pr(X_{t+1}=x_{t+1}, ..., X_T=x_T | Z_t=k)
    lstateprobs.push_back(arma::mat(Ts[i], K));	// prob that state k generated obs t: Pr(X_t=x_t | Z_t=k)
  }

  // track log-likelihood
  std::vector<double> llh_seq;
  llh_seq.push_back(-std::numeric_limits<double>::infinity());

  // log-likelihood of each obs seq | parameter estimates in current iter
  arma::vec llhs(N);

  // read in transition probs
  arma::mat Gamma = Gamma_init;

  // read in initial state parameters
  arma::mat mus = mus_init;					// state-specific means: rows=covariates, cols=state
  arma::cube Sigmas(M, M, K);				// state-specific vcov matrices: rows/cols=covariates, slices=state

  for (arma::uword k=0; k<K; k++){
    Sigmas.slice(k) = Sigmas_init[k];
  }

  // resetting collapsed states
  double log_uncollapse = log(uncollapse);
  int resets = 0;

  // EM algorithm
  for (arma::uword iter=0; iter<maxiter; iter++){

    // detect collapsed components and reset (assumes that input features have been standardized)
    if (!supervised){
      if (uncollapse > 0){
        for (arma::uword k=0; k<K; k++){
          double volume;
          double sign;
          arma::log_det(volume, sign, Sigmas.slice(k));		// volume of state distribution
          if (volume < log_uncollapse){
            resets += 1;
            if (verbose){
              Rcpp::Rcout << "collapsed state detected; resetting" << std::endl;
            }
            mus.col(k) = arma::randn(M);					        // redraw state mean from std diag multivariate normal
            Sigmas.slice(k) = arma::eye<arma::mat>(M, M);	// reset state cov mat to unit sphere
          }
        }
      }
    }

    if (verbose){
      Rcpp::Rcout << "iter " << iter + 1 << ": ";
    }

    for (arma::uword i=0; i<N; i++){

      // state probs Pr[X_t=x | Z_t=k, mus, Sigmas]: loop over clusters, plug X into dmvnorm for each
      for (arma::uword k=0; k<K; k++){
        lstateprobs[i].col(k) =
          dmvnorm_cond(Xs[i],
                       mus.col(k).t(),
                       Sigmas.slice(k),
                       labels_t,
                       labels_y,
                       true,
                       lambda);
      }

      arma::vec scale = arma::max(lstateprobs[i], 1); // pseudocode: get max{ log Pr( X_t=x_t | Z_t=k ) : all k }
      arma::mat lstateprobs_scaled = lstateprobs[i];
      for (arma::uword k=0; k<K; k++){
        lstateprobs_scaled.col(k) -= scale;
      }
      arma::mat tstateprobs = exp(lstateprobs_scaled).t();	// t() for faster column access in forward/backward probs

      // convert to working parameters: forward, backward probs
      lalphas[i] = forward(zetas[i].col(0).t(), Gamma, tstateprobs, scale);
      lbetas[i] = backward(Gamma, tstateprobs, scale);

      double log_C_i = max(lalphas[i].col(Ts[i]-1));
      llhs(i) = log(arma::accu(exp(lalphas[i].col(Ts[i]-1) - log_C_i))) + log_C_i;

    }

    double llh = arma::accu(llhs);
    double llh_diff = llh - llh_seq.back();
    llh_seq.push_back(llh);

    if (llh_diff < tol && llh_diff >= 0){
      if (verbose){
        Rcpp::Rcout << "log-likelihood increase < tol; stopping" << std::endl;
      }
      break;
    }

    if (verbose){
      Rprintf("log-likelihood of %f\n", llh);
    }

    arma::mat Gamma_old = Gamma;	// store old transition matrix
    Gamma.zeros();					// reset all elements to zero
    for (arma::uword i=0; i<N; i++){

      // E-step (states)

      if (!supervised){
        zetas[i] = exp(lalphas[i] + lbetas[i] - llhs(i)); // zeta[k, t]: E[Z_t=k | data]
        for (arma::uword t=0; t<Ts[i]; t++){
          double zetas_normalize = arma::accu(zetas[i].col(t));
          zetas[i].col(t) /= zetas_normalize; // normalize state memberships of each obs
        }
      }

      // E-step (transitions)

      arma::mat lstateprobs_dropfirst = lstateprobs[i].submat(arma::span(1, Ts[i]-1), arma::span::all);
      arma::mat lalpha_droplast = lalphas[i].submat(arma::span::all, arma::span(0, Ts[i]-2));
      lalpha_droplast = lalpha_droplast.t();
      arma::mat lbeta_dropfirst = lbetas[i].submat(arma::span::all, arma::span(1, Ts[i]-1));
      lbeta_dropfirst = lbeta_dropfirst.t();

      for (arma::uword k1=0; k1<K; k1++){
        for (arma::uword k2=0; k2<K; k2++){
          Gamma(k1, k2) += weights(i) *
            arma::accu(exp(lalpha_droplast.col(k1)
                             + lstateprobs_dropfirst.col(k2)
                             + lbeta_dropfirst.col(k2)
                             - llhs(i)
            ));
        }
      }

    }
    Gamma = Gamma % Gamma_old;

    // normalize transition probs out of each state
    for (arma::uword k=0; k<K; k++){
      Gamma.row(k) /= arma::accu(Gamma.row(k));
    }

    // M-step (mu_k)
    mus.zeros();		// reset all elements to zero
    arma::vec zeta_sums(K, arma::fill::zeros);

    // weighted sum by responsibility (ignore partially censored obs)
    for (arma::uword i=0; i<N; i++){
      mus += weights(i) * (zetas[i] * Xs[i]).t();
      zeta_sums += weights(i) * arma::sum(zetas[i], 1);
    }

    // divide by effective number of obs (ignore partially censored obs)
    for (arma::uword k=0; k<K; k++){
      mus.col(k) /= zeta_sums(k);
    }

    // M-step (Sigma_k)
    Sigmas.zeros();		// reset all elements to zero

    // weighted variance by responsibility (ignore partially censored obs)
    for (arma::uword k=0; k<K; k++){
      for (arma::uword i=0; i<N; i++){
        arma::mat X_demean = Xs[i];
        for (arma::uword m=0; m<M; m++){
          X_demean.col(m) -= mus(m, k);
        }
        arma::rowvec zetas_ik = zetas[i].row(k) ;
        Sigmas.slice(k) += weights(i) * X_demean.t() * arma::diagmat(zetas_ik) * X_demean;
      }
      Sigmas.slice(k) /= zeta_sums(k);
    }

    if (verbose){
      if (iter == maxiter - 1){
        Rcpp::Rcout << "maxiter reached; stopping" << std::endl;
      }
    }

  }

  Rcpp::List lstateprobs_out;
  for (arma::uword i=0; i<N; i++){
    lstateprobs_out.push_back(lstateprobs[i]);
  }

  Rcpp::List mus_out;
  Rcpp::List Sigmas_out;
  for (arma::uword k=0; k<K; k++){
    mus_out.push_back(arma::conv_to< std::vector<double> >::from(mus.col(k)));
    Sigmas.slice(k).diag() += lambda;
    Sigmas_out.push_back(Sigmas.slice(k));
  }

  Rcpp::List zetas_out;
  for (arma::uword i=0; i<N; i++){
    zetas_out.push_back(zetas[i]);
  }

  return Rcpp::List::create(
    Rcpp::Named("lstateprobs") = lstateprobs_out,
    Rcpp::Named("llh_seq") = llh_seq,
    Rcpp::Named("llhs") = llhs,
    Rcpp::Named("Gamma") = Gamma,
    Rcpp::Named("mus") = mus_out,
    Rcpp::Named("Sigmas") = Sigmas_out,
    Rcpp::Named("zetas") = zetas_out,
    Rcpp::Named("resets") = resets
  );
}



////////////////////////////////////////////////
// calculate log-likelihood of new obs chains //
////////////////////////////////////////////////

// [[Rcpp::export]]
Rcpp::List llh_cpp(
    std::vector<arma::mat> Xs,	// rows=time, cols=covariates
    arma::rowvec delta,		      // initial distribution: use stationary distr of Markov chain
    arma::mat mus,			// state-specific means: rows=covariates, cols=state
    std::vector<arma::mat> Sigmas_in,			// state-specific vcov matrices: rows/cols=covariates, elements=state
    arma::mat Gamma,			      // transition matrix
    std::vector< arma::uvec > nonmissing,           // indices of obs with no missingness
    std::vector< arma::uvec > missingness_labels,   // pattern of missingness features
    std::vector< arma::uvec > nonmissing_features,  // details on each missingness type
    double lambda,			          // regularization parameter
    bool verbose = true				         // status updates
){

  arma::uword N = Xs.size();				// number of observation sequences
  std::vector<arma::uword> Ts;			// number of observations in each obs sequence
  for (arma::uword i=0; i<N; i++){
    Ts.push_back(Xs[i].n_rows);
  }
  arma::uword M = Xs[0].n_cols;			// number of measured covariates
  arma::uword K = Gamma.n_rows;			// number of states

  // for obs seq i: rows=states, cols=time
  std::vector<arma::mat> lalphas;

  // for obs seq i: rows=time, cols=states
  std::vector<arma::mat> lstateprobs;

  for (arma::uword i=0; i<N; i++){
    lalphas.push_back(arma::mat(K, Ts[i]));		// forward probs: Pr(X_1=x_1, ..., X_t=x_t, Z_t=k)
    lstateprobs.push_back(arma::mat(Ts[i], K));	// prob that state k generated obs t: Pr(X_t=x_t | Z_t=k)
  }

  // read in initial state parameters
  arma::cube Sigmas(M, M, K);				// state-specific vcov matrices: rows/cols=covariates, slices=state

  for (arma::uword k=0; k<K; k++){
    Sigmas.slice(k) = Sigmas_in[k];
  }

  // compute log likelihoods
  arma::vec llhs(N);
  for (arma::uword i=0; i<N; i++){
    if (verbose){
      Rcpp::Rcout << "obs seq " << i + 1 << std::endl;
    }

    // state probs Pr[X_t=x | Z_t=k, mus, Sigmas]: loop over clusters, plug X into dmvnorm for each
    for (arma::uword k=0; k<K; k++){
      if (verbose){
        Rcpp::Rcout << "  state " << k + 1 << std::endl;
      }
      lstateprobs[i].col(k) =
        dmvnorm_cens(Xs[i],
                     mus.col(k).t(), Sigmas.slice(k),
                     missingness_labels[i],
                                       nonmissing_features,
                                       true, lambda);
    }

    arma::vec scale = arma::max(lstateprobs[i], 1); // pseudocode: get max{ log Pr( X_t=x_t | Z_t=k ) : all k }
    arma::mat lstateprobs_scaled = lstateprobs[i];
    for (arma::uword k=0; k<K; k++){
      lstateprobs_scaled.col(k) -= scale;
    }
    arma::mat tstateprobs = exp(lstateprobs_scaled).t();	// t() for faster column access in forward/backward probs

    // forward probs
    lalphas[i] = forward(delta, Gamma, tstateprobs, scale);

    double log_C_i = max(lalphas[i].col(Ts[i]-1));
    llhs(i) = log(arma::accu(exp(lalphas[i].col(Ts[i]-1) - log_C_i))) + log_C_i;

  }

  return Rcpp::List::create(
    Rcpp::Named("llh_total") = arma::accu(llhs),
    Rcpp::Named("llhs") = Rcpp::wrap(llhs)
  );

}



/////////////////////////////////////////////////////////////////////////
// calculate log-probability of each obs being generated by each state //
/////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
Rcpp::List lstateprobs_cpp(
    std::vector<arma::mat> Xs,	       // rows=time, cols=covariates
    arma::mat mus,			               // state-specific means: rows=covariates, cols=state
    std::vector<arma::mat> Sigmas_in,  // state-specific vcov matrices: rows/cols=covariates, elements=state
    std::vector< arma::uvec > nonmissing,           // indices of obs with no missingness
    std::vector< arma::uvec > missingness_labels,   // pattern of missingness features
    std::vector< arma::uvec > nonmissing_features,  // details on each missingness type
    double lambda			                 // regularization parameter
){

  arma::uword N = Xs.size();				// number of observation sequences
  std::vector<arma::uword> Ts;			// number of observations in each obs sequence
  for (arma::uword i=0; i<N; i++){
    Ts.push_back(Xs[i].n_rows);
  }
  arma::uword M = Xs[0].n_cols;			 // number of measured covariates
  arma::uword K = Sigmas_in.size();  // number of states

  // for obs seq i: rows=time, cols=states
  std::vector<arma::mat> lstateprobs;

  for (arma::uword i=0; i<N; i++){
    lstateprobs.push_back(arma::mat(Ts[i], K));	// prob that state k generated obs t: Pr(X_t=x_t | Z_t=k)
  }

  // read in initial state parameters
  arma::cube Sigmas(M, M, K);				// state-specific vcov matrices: rows/cols=covariates, slices=state

  for (arma::uword k=0; k<K; k++){
    Sigmas.slice(k) = Sigmas_in[k];
  }

  // compute log likelihoods
  arma::vec llhs(N);
  for (arma::uword i=0; i<N; i++){

    // state probs Pr[X_t=x | Z_t=k, mus, Sigmas]: loop over clusters, plug X into dmvnorm for each
    for (arma::uword k=0; k<K; k++){
      lstateprobs[i].col(k) =
        dmvnorm_cens(Xs[i],
                     mus.col(k).t(), Sigmas.slice(k),
                     missingness_labels[i],
                                       nonmissing_features,
                                       true, lambda);
    }

  }

  Rcpp::List lstateprobs_out;
  for (arma::uword i=0; i<N; i++){
    lstateprobs_out.push_back(lstateprobs[i]);
  }

  return lstateprobs_out;

}



/////////////////////////////////////////
// find most likely sequence of states //
/////////////////////////////////////////

// [[Rcpp::export]]
std::vector< std::vector<arma::uword> > viterbi_cpp(
    std::vector<arma::mat> lstateprobs,	// rows=time, cols=covariates
    arma::rowvec delta,		          	  // initial distribution: use stationary distr of Markov chain
    arma::mat Gamma			              	// transition matrix
){

  arma::uword N = lstateprobs.size();				// number of observation sequences
  std::vector<arma::uword> Ts;			// number of observations in each obs sequence
  for (arma::uword i=0; i<N; i++){
    Ts.push_back(lstateprobs[i].n_rows);
  }
  arma::uword K = Gamma.n_rows;			// number of states

  // for obs seq i: rows=states, cols=time
  std::vector<arma::mat> lxis;
  std::vector<arma::mat> tlstateprobs; // transposed for faster column access

  for (arma::uword i=0; i<N; i++){
    lxis.push_back(arma::mat(K, Ts[i]));
    tlstateprobs.push_back(lstateprobs[i].t());
  }

  // log transition probabilities
  arma::mat lGamma = log(Gamma);

  // predicted sequence
  std::vector< std::vector<arma::uword> > seqs;

  // compute log-likelihood of most probable sequence up to t
  // log max_{z_1, ... z_t-1} Pr(Z_1=z_1, ... Z_t=k, X)
  for (arma::uword i=0; i<N; i++){

    lxis[i].col(0) = log(delta.t()) + tlstateprobs[i].col(0);
    for (arma::uword t=1; t<Ts[i]; t++){
      for (arma::uword k=1; k<K; k++){
        lxis[i](k,t) = max(lxis[i].col(t-1) + lGamma.col(k));
      }
      lxis[i].col(t) += tlstateprobs[i].col(t);
    }

  }

  // work backward to decode most probable sequence up to t
  // argmax_{z_1, ... z_T} Pr(Z_1=z_1, ... Z_T=z_T, X)
  for (arma::uword i=0; i<N; i++){

    std::vector<arma::uword> revseq; // reverse order

    arma::uword k_T;
    lxis[i].col(Ts[i]-1).max(k_T); // (state mle @ T | xi) -> k_T
    revseq.push_back(k_T);

    for (arma::uword t=Ts[i]-1; t>0; t--) {
      arma::uword k;
      (lxis[i].col(t-1) + lGamma.col(revseq.back())).max(k); // (state mle @ t-1 | xi, state @ t) -> k
      revseq.push_back(k);
    }

    std::reverse(revseq.begin(), revseq.end());

    for (arma::uword t=0; t<Ts[i]; t++) {
      revseq[t]++; // convert to R indexing
    }

    seqs.push_back(revseq);
  }

  return seqs;
}
