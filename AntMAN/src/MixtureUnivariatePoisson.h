/*
 *  AntMAN Package
 *
 */


#ifndef ANTMAN_SRC_MIXTUREUNIVARIATEPOISSON_HPP_
#define ANTMAN_SRC_MIXTUREUNIVARIATEPOISSON_HPP_


#include "math_utils.h"
#include "Mixture.h"

class MixtureUnivariatePoisson : public UnivariateIntegerMixture {

	// ParametricPrior
	double _alpha0, _beta0;

	// Tau
	std::vector <double> _theta;

public :
	MixtureUnivariatePoisson (const double alpha0, const double beta0) : _alpha0 (alpha0), _beta0 (beta0) {}

	void get_tau (AntMANLogger& logger) const {
			logger.addlog("theta", _theta);
	}

	void init_tau (const input_t & y, const int M) {
		const double beta0 = _alpha0;
		const double alpha0 = _beta0;
		_theta.resize(M);
		// TODO[CHECK ME] : If alpha0 is lower than 10-3 then instability so warning to user maybe ? std::numeric_limits <double>::min ()
		for(int l=0;l<M;l++){
			_theta[l] = (1. / beta0) * am_rgamma(alpha0, 1); // TODO : everytime you use gamma, no zero but epsilon with a warning!!!
			_theta[l] = (_theta[l] < 0.0000001) ? _theta[l]  : 0.0000001 ; // TODO: this is not small need epsilon
		}
	}


	 allocation_result up_allocated_nonallocated (
			const int K ,const int M ,
			const cluster_indices_t & ci_current , const cluster_indices_t & ci_star  ,
			const double gamma_current, const double U_current,
			const input_t & y ){


		const long   n     = y.size();
		const double alpha0   = _alpha0;
		const double beta0    = _beta0;


		//Allocation_result output ;
		std::vector<double> theta_current(M);

		arma::vec S_current  (M);

		cluster_indices_t ci_reorder(y.size());
		ci_reorder.fill(-1);
		std::vector<int>    nj(K);
		std::map< int, std::vector<int> > clusters_indices;

		for(int i=0;i<n;i++){
			clusters_indices[ci_current[i]].push_back(i);
		}

		for(int local_index=0;local_index<K;local_index++){
			const int key = ci_star[local_index];
			nj[local_index] = clusters_indices[key].size();
			for (auto v : clusters_indices[key]) {
				ci_reorder[v]=local_index;
			}
		}

		for(int l=0; l<K;l++){
			// Find the index of the data in the l-th cluster
			std::vector<int> & which_ind=clusters_indices [ci_star[l]];

			//Prepare the variable that will contain the data in the cluster
			arma::colvec y_l = arma::colvec(nj[l]);
			//Separate the data in each cluster and rename the cluster
			for(int it=0;it<nj[l];it++){
				y_l[it]=y[which_ind[it]];

			}

			// Call the parametric function that update the
			// parameter in each cluster
			// See the boxes 4.c.i and 4.b.i of
			// Figure 1 in the paper

			const int njl = y_l.n_elem; // This is the number of data in the cluster. I hope so


			//Since in our case the full conditionals are in closed form
			//they are Normal-inverse-gamma



			//First compute the posterior parameters

			const double ysum= arma::sum(y_l);

			//double ysum= arma::accu(y); //is there any difference between sun and accu un Armadillo?


			const double alphan = alpha0 + ysum;
			const double scalen  = 1.0/(beta0 + njl);
			theta_current[l] = scalen * am_rgamma (alphan, 1);
			// TODO[CHECK ME] : theta_current must be non-zero

			// TODO[OPTIMIZE ME] : Cannot split or the random value are completly different
			// Update the Jumps of the allocated part of the process
			S_current[l]=am_rgamma(nj[l]+gamma_current,1./(U_current+1.0));

		}

		// Fill non-allocated

		for(int l=K; l<M;l++){
			theta_current[l] = (1.0 / beta0) * am_rgamma (alpha0 , 1) ;
			// TODO[CHECK ME] : theta_current must be non-zero
			S_current[l]=am_rgamma(gamma_current,1./(U_current+1.0));
		}


		this->_theta = theta_current;

		return allocation_result (ci_reorder , nj , S_current);



	}

	cluster_indices_t up_ci(const  input_t & y,
			const long M,
			const arma::vec & S_current) {

			const int n = y.size();

			const std::vector<double>& theta_current = this->_theta;
			arma::vec Log_S_current = arma::log(S_current);
			cluster_indices_t ci_current(n);
			arma::vec random_u   = am_randu(n);

			for (int i=0; i < n; i++) {

				arma::vec pesi(M);
				double max_lpesi=-INFINITY;

				for(int l=0;l<M;l++){
					 double ldensi = - theta_current[l] + y[i] * log (theta_current[l]);
					 //double ldensi = dpois(y[i],theta_current[l],true);

					 pesi[l]=Log_S_current[l]+ldensi;
					 if(max_lpesi<pesi[l]){max_lpesi=pesi[l];}
				}

				// I put the weights in natural scale and re-normalize then
				pesi = exp(pesi - max_lpesi);
				pesi = pesi / sum(pesi);

				const double u = random_u[i];
				double cdf = 0.0;
				unsigned int ii = 0;
				while (u >= cdf) { // This loop assumes (correctly) that runif(0,1) never return 1.
					cdf += pesi[ii++];
				}
				ci_current[i] = ii;
			}

			return  ci_current ;

	}

	input_t sample(const arma::vec & W_current, arma::uword n) {
		input_t result (n);
		long int selected_M = runif_component(W_current);
		for (arma::uword i = 0 ; i < n ; i++) {
			result(i) = am_rpois ( this->_theta[selected_M] );
		}
		return result;
	}

};

#endif 
