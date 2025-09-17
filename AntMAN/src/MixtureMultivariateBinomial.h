/*
 *  AntMAN Package
 *
 */


#ifndef ANTMAN_SRC_MIXTUREMULTIVARIATEBERNOULLI_HPP_
#define ANTMAN_SRC_MIXTUREMULTIVARIATEBERNOULLI_HPP_

#include "math_utils.h"
#include "Mixture.h"
#include "AntMANLogger.h"

class MixtureMultivariateBinomial: public MultivariateIntegerMixture  {

	// Parametric Prior
	arma::vec      _mb;
	arma::vec _a0, _b0;

	//Tau
	arma::mat _theta;

public :
	MixtureMultivariateBinomial (const arma::vec  a0, const arma::vec  b0) :  _mb(a0.size(), arma::fill::ones), _a0 (a0), _b0 (b0) {}
	//Mixture_MultivariateBernoulli (const arma::vec  a0, const arma::vec  b0, const arma::vec  mb) :  _mb(mb), _a0 (a0), _b0 (b0) {}

	void get_tau (AntMANLogger& logger) const {
			logger.addlog("theta", _theta);
	}

	virtual void init_tau (const input_t & y, const int M) {

		VERBOSE_DEBUG(" init_tau (const input_t & y, const int M)");

				const arma::vec& b0 = _b0;
				const arma::vec& a0 = _a0;

				VERBOSE_DEBUG("b0=" << b0 << " a0=" << a0);

				const int d = y.n_cols;
				arma::mat theta(M,d);
				_theta = theta;

				for(int l = 0; l < M; l++){
					for(int e = 0; e < d ; e++){
						_theta(l,e) = am_rbeta(a0[e], b0[e]);
					}
				}

				VERBOSE_DEBUG(" done");
	}

	virtual cluster_indices_t  up_ci(const  input_t & y,
			const long M,
			const arma::vec & S_current) {


		const arma::vec& mb    = _mb;

		const int n = y.n_rows;

		const arma::mat& theta_current = _theta;

		arma::vec Log_S_current = arma::log(S_current);
		cluster_indices_t ci_current(n);
		arma::vec random_u   = am_randu(n);

		for (int i=0; i < n; i++) {

			arma::vec pesi(M);
			double max_lpesi=-INFINITY;

			for(int l=0;l<M;l++){

				arma::rowvec left   =  y.row(i)  % arma::log (    theta_current.row(l)) ;
				arma::rowvec right  = (mb.t() - y.row(i)) % arma::log (1 - theta_current.row(l)) ;
				arma::mat lplusr = left + right;


				double ldensi =  accu(lplusr) ;

				 pesi[l]=Log_S_current[l]+ldensi;
				 if(max_lpesi<pesi[l]){max_lpesi=pesi[l];}
			}

			// I put the weights in natural scale and re-normalize then
			pesi = arma::exp(pesi - max_lpesi);
			pesi = pesi / arma::sum(pesi);

			const double u = random_u[i];
			double cdf = 0.0;
			unsigned int ii = 0;
			while (u >= cdf) { // This loop assumes (correctly) that am_runif(0,1) never return 1.
				cdf += pesi[ii++];
			}
			ci_current[i] = ii;
		}

		return  ci_current ;


	}

	 virtual allocation_result up_allocated_nonallocated (
			const int K ,
			const int M ,
			const cluster_indices_t & ci_current ,
			const cluster_indices_t & ci_star  ,
			const double gamma_current,
			const double U_current,
			const  input_t & y ) {


			const int n = y.n_rows;
			const int d = y.n_cols;

			const arma::vec& a0    = _a0;
			const arma::vec& b0    = _b0;
			const arma::vec& mb    = _mb;


			//Allocation_result output ;
			arma::mat theta_current(M,d);
			arma::vec S_current  (M);

			cluster_indices_t ci_reorder(n);
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
				std::vector <arma::ivec>  y_l (nj[l]);
				//Separate the data in each cluster and rename the cluster
				for(int it=0;it<nj[l];it++){
					y_l[it]=y.row(which_ind[it]).t();

				}


				// Call the parametric function that update the
				// parameter in each cluster
				// See the boxes 4.c.i and 4.b.i of
				// Figure 1 in the paper
				// const int njl = y_l.size(); // This is the number of data in the cluster. I hope so
				const int njl =nj[l];
				arma::vec njvec (d);
				njvec.fill (njl);

				//Since in our case the full conditionals are in closed form
				//they are Normal-inverse-gamma



				//First compute the posterior parameters
				const arma::ivec ysum = y_l.size() ? vectorsum<arma::ivec>(y_l) : arma::zeros<arma::ivec>(d);

				const arma::vec an = a0 + ysum;
				const arma::vec bn = njvec % mb  - ysum + b0; // remove mb
				
				for (int e = 0 ; e < d; e++) {
					theta_current(l,e) = am_rbeta (an[e], bn[e]);
				}
				// TODO[OPTIMIZE ME] : Cannot split or the random value are completly different
				// Update the Jumps of the allocated part of the process
				S_current[l]=am_rgamma(nj[l]+gamma_current,1./(U_current+1.0));

			}

			// Fill non-allocated

			for(int l=K; l<M;l++){
				for (int e = 0 ; e < d; e++) {
					theta_current(l,e) = am_rbeta (a0[e], b0[e]);
				}
				// TODO[CHECK ME] : theta_current must be non-zero
				S_current[l]=am_rgamma(gamma_current,1./(U_current+1.0));
			}


			_theta = theta_current;

			return allocation_result (ci_reorder , nj , S_current);



	 }

	 input_t sample(const arma::vec & W_current,arma::uword  n) {

		 VERBOSE_DEBUG("Run sample");

		 long int selected_M = runif_component(W_current);
		 input_t res = input_t(1,_theta.n_cols);


		 VERBOSE_EXTRA("selected_M = " << selected_M);


		 auto theta0  = _theta.row(selected_M); // take row
		 for (arma::uword idx = 0 ; idx < _theta.n_cols ; idx++) {
			 auto e = theta0[idx];
			 VERBOSE_ASSERT((e <= 1) or (e >= 0), "Condition not checked e in (0,1): Invalid Theta");

			 // sample a value between 0 and 1 , if smaller than e
			 auto sample = am_runif(0,1);
			 res[idx] = sample >= e ? 0 : 1;
		 }


		 // TODO: output is the result.

		 VERBOSE_DEBUG("Return result");
		 return res;


	 }

};




#endif /* ANTMAN_SRC_MIXTUREMULTIVARIATEBERNOULLI_HPP_ */
