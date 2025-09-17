#include "mcstat.h"

using namespace mcstat;

double mcstat::logit(double x) { return log( x / (1.0 - x) ); }

double mcstat::invlogit(double x) {
	double expX = exp(x);
	return std::isinf(expX)!=0 ? 1 : expX / (1.0 + expX);
}

double mcstat::logdinvgamma_unscaled(double x, double a, double b) {
	return - (a + 1.0) * log(x) - b/x;
}

double mcstat::logdbeta_unscaled(double x, double a, double b) {
	return (a-1) * log(x) + (b-1) * log(1-x);
}

double mcstat::logitProposal(double x, double min_x, double max_x, double sd) {
	double w = max_x - min_x;
	return mcstat::invlogit( mcstat::logit((x - min_x)/w) + R::rnorm(0, sd) ) * w + min_x;
}

double mcstat::logProposal(double x, double sd) {
	return exp( log(x) + R::rnorm(0, sd) );
}

double mcstat::loglogJacobian(double x) { return -log( std::abs(x) ); }
double mcstat::loglogitJacobian(double x) { return -log( std::abs(x*(1.0-x)) ); }

vec mcstat::mvrnorm(const mat & sigma) {
	return chol(sigma, "lower") * randn<vec>(sigma.n_rows, 1);
}

vec mcstat::mvrnorm(const vec & mu, const mat & sigma) {
	return mu + chol(sigma, "lower") * randn<vec>(sigma.n_rows, 1);
}

vec mcstat::mvrnorm(const vec &mu, const mat &sigma, bool precision) {
	if(precision) {
		return solve(chol(sigma, "upper"), randn<vec>(sigma.n_rows, 1)) + mu;
	} else {
		return mu + chol(sigma, "lower") * randn<vec>(sigma.n_rows, 1);
	}
}

mat mcstat::rwishart(const mat &V, int n) {
	// Params:
	//  n - degrees of freedom
	//  V - (symmetric) scale matrix
	
	int p = V.n_rows;
	mat A = mat(p, p, fill::zeros);
	
	// fill diagonal
	for(int i=0; i<p; i++)
		A(i,i) = sqrt(R::rchisq(n-i));
	
	// fill lower-triangular portion of matrix
	for(int i=1; i<p; i++)
		for(int j=0; j<i; j++)
			A(i,j) = R::rnorm(0,1);
	
	mat C = chol(V, "lower") * trimatl(A);
	return C * C.t();
}

mat mcstat::rinvwishart(const mat &V, int n) {
	return inv_sympd( mcstat::rwishart(inv_sympd(V), n) );
}

// evaluate kron(A,B) * C without storing kron(A,B)
mat mcstat::dgemkmm(mat A, mat B, mat C) {
	int m = A.n_rows;
	int n = A.n_cols;
	int p = B.n_rows;
	int q = B.n_cols;
	int r = C.n_cols;
	
	mat res;
	res = mat(m*p, r, fill::zeros);
	
	mat resBlock;
	resBlock.set_size(q, r);
	for(int i=0; i<m; i++) {
		resBlock.zeros();
		
		for(int j=0; j<n; j++)
			resBlock += A.at(i,j) * C.rows( j*q, (j+1)*q-1 );
		
		res.rows( i*p, (i+1)*p-1 ) += B * resBlock;
	}
	
	return res;
}


// evaluate kron(A,B) * C without storing kron(A,B)
mat mcstat::dsemkmm(mat A, mat B, SpMat<double> C) {
	int m = A.n_rows;
	int n = A.n_cols;
	int p = B.n_rows;
	int q = B.n_cols;
	int r = C.n_cols;
	
	mat res;
	res = mat(m*p, r, fill::zeros);
	
	mat resBlock;
	resBlock.set_size(q, r);
	for(int i=0; i<m; i++) {
		resBlock.zeros();
		
		for(int j=0; j<n; j++)
			resBlock += A.at(i,j) * C.rows( j*q, (j+1)*q-1 );
		
		res.rows( i*p, (i+1)*p-1 ) += B * resBlock;
	}
	
	return res;
}



mcstat::RWSampler::RWSampler(double _sd) {
	nSamples = 0;
	accept = 0;
	sd = _sd;
}

double mcstat::RWSampler::getAcceptanceRate() { return accept; }

double mcstat::RWSampler::getSd() { return sd; }

void mcstat::RWSampler::adapt(double adaptScale, double targetRate) {
	sd *= exp( adaptScale * (accept - targetRate) );
}


List mcstat::RWSampler::toList() {
	return List::create(
						_["accept"] = accept,
						_["sd"] = sd
	);
}

double mcstat::RWSampler::sample(double x0) {
	
	double logR = 0;
	double x = x0;
	
	switch(type) {
		case NORMAL:
			x += R::rnorm(0, sd);
			logR = logR_posterior(x);
			break;
			
		case LOG:
			x = logProposal(x0, sd);
			logR = logR_posterior(x) + loglogJacobian(x0) - loglogJacobian(x);
			break;
			
		case LOGIT:
			x = logitProposal(x0, L, U, sd);
			logR = logR_posterior(x) + loglogitJacobian(x0) - loglogitJacobian(x);
			break;
	}
	
	bool accepted = log(R::runif(0,1)) <= std::min(logR, 0.0);
	
	if(accepted) {
		update();
	} else {
		x = x0;
	}
	
	accept += ((accepted ? 1.0 : 0.0) - accept) / (double) (++nSamples);
	
	return x;
}

mcstat::MCMCCheckpoint::MCMCCheckpoint(int _nSamples) {
	nSamples = _nSamples;
	checkPointIt = (int) nSamples * 0.1;
	it = 0;
	start = std::clock();
	lap = start;
}

void mcstat::MCMCCheckpoint::run() {
	if(it++ % checkPointIt == 0) {
		// compute time since last checkpoint
		std::clock_t tmp_clock = lap;
		lap = std::clock();
		double duration = ( lap - tmp_clock ) / (double) CLOCKS_PER_SEC;
		
		// compute percent complete
		double pctComplete = (double) it / (double) nSamples * 100.0;
		
		// compute remaining time
		double total = (lap - start) / (double) CLOCKS_PER_SEC;
		double remaining = (100.0 - pctComplete) * (total / pctComplete) / 60.0;
		
		// output information
		Rcout << round(pctComplete) << "% complete" << " (" <<
		floor(duration * 10.0) / 10.0 << " seconds; " <<
		floor(remaining * 10.0) / 10.0 << " minutes remaining)" <<
		endl;
	}
}

void mcstat::MCMCCheckpoint::finish() {
	// compute final time
	lap = std::clock();
	double duration = (lap - start) / (double) CLOCKS_PER_SEC;
	
	// compute samples per second
	double sampSec = it / duration;
	
	// output information
	Rcout << endl << "Total time (min): " << floor(duration / 60.0 * 10.0) / 10.0 <<
	endl << "Samples per second: " << floor(sampSec * 10.0) / 10.0 << endl;
}

//
// R exports
//


// [[Rcpp::export]]
arma::mat r_dgemkmm(arma::mat A, arma::mat B, arma::mat C) {
	return mcstat::dgemkmm(A, B, C);
}


// [[Rcpp::export]]
arma::mat r_rwishart(arma::mat V, int n) {
	return mcstat::rwishart(V, n);
}


// [[Rcpp::export]]
arma::mat r_rinvwishart(arma::mat V, int n) {
	return mcstat::rinvwishart(V, n);
}
