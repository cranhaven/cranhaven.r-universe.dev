/*
 *  AntMAN Package
 *
 */


#ifndef ANTMAN_SRC_MIXTUREMULTIVARIATENORMAL_HPP_
#define ANTMAN_SRC_MIXTUREMULTIVARIATENORMAL_HPP_


#include "math_utils.h"
#include "Mixture.h"
#include "utils.h"

class MixtureMultivariateNormal: public MultivariateMixture  {

	// ParametricPrior
	const arma::vec    _mu0;
	const double       _ka0;
	const unsigned int _nu0;
	const arma::mat    _Lam0;

	// Tau
	arma::mat  _mu_current;
	arma::cube _Sig_current;


public :
	MixtureMultivariateNormal (const arma::vec & mu0, const double ka0, const unsigned int nu0, const arma::mat & Lam0) : _mu0 (mu0), _ka0 (ka0), _nu0 (nu0), _Lam0  (Lam0){}

	void get_tau (AntMANLogger& logger) const {
		logger.addlog("mu", _mu_current);
		logger.addlog("Sig", _Sig_current);
	}

	virtual void init_tau (const input_t & y, const int M){

		VERBOSE_DEBUG(" init_tau (const input_t & y, const int M)");

		arma::mat   mu_current  (M,y.n_cols);
		arma::cube  Sig_current (y.n_cols,y.n_cols, M) ;

		 _mu_current = mu_current ;
		 _Sig_current= Sig_current ;

				const arma::colvec mu0  = _mu0;
				const double       ka0  = _ka0;
				const unsigned int nu0  = _nu0;
				const arma::mat    Lam0 = _Lam0;


				VERBOSE_EXTRA("In Alloc: mu0  = " << mu0);
				VERBOSE_EXTRA("In Alloc: ka0  = " << ka0);
				VERBOSE_EXTRA("In Alloc: nu0  = " << nu0);
				VERBOSE_EXTRA("In Alloc: Lam0  = " << Lam0);

				VERBOSE_ASSERT(Lam0.is_sympd(), "In init_tau: rwish requires Lam0 to be symmetric. It is not Lam0 = " << Lam0);
				for(int l=0;l<M;l++){

					const arma::mat res = riwish (nu0, Lam0);

					VERBOSE_EXTRA("In Alloc: riwish (nu0, Lam0) = " << res);

					_Sig_current.slice(l) = res;

					auto tmp1 = res / ka0 ;
					VERBOSE_EXTRA("In Alloc: tmp1 = " << tmp1);
					const arma::colvec tmp = mvrnormArma (mu0, tmp1) ;
					VERBOSE_EXTRA("In Alloc: tmp = " << tmp);

					_mu_current.row(l)   =  tmp.t();

				}

				VERBOSE_DEBUG(" init_tau finished");

		 }

		virtual cluster_indices_t  up_ci(const  input_t & y,
				const long M,
				const arma::vec & S_current){


			VERBOSE_DEBUG("GibbsFramework<Tau_MultivariateNormal>::up_ci");

			const int n = y.n_rows;

			const arma::mat& mu_current = _mu_current;
			const arma::cube& Sig_current = _Sig_current;

			cluster_indices_t ci_current(n);
			arma::vec Log_S_current = arma::log(S_current);
			arma::vec random_u   = am_randu(n);

			VERBOSE_DEBUG("GibbsFramework<Tau_MultivariateNormal>::up_ci: for (int i=0; i < n; i++) {");

			VERBOSE_DEBUG("y  :" << y.n_rows << "x" << y.n_cols);
			VERBOSE_DEBUG("mu_current :" << mu_current.n_rows << "x" << mu_current.n_cols);
			VERBOSE_DEBUG("Sig_current  :"  << Sig_current.n_rows << "x" << Sig_current.n_cols << "x" << Sig_current.n_slices );



			#pragma omp parallel for  if (this->get_parallel())  num_threads(8) schedule(static, 8)
			for (int i=0; i < n; i++) {

				arma::vec pesi(M);
				double max_lpesi=-INFINITY;

				for(int l=0;l<M;l++){

					const arma::vec dmvnorm1_mu  = mu_current.row(l).t();
					const arma::mat dmvnorm1_Sig = Sig_current.slice(l);


					double ldensi = dmvnormZero(y.row(i), dmvnorm1_mu,dmvnorm1_Sig,true);
					 pesi[l]=Log_S_current[l]+ldensi;
					 if(max_lpesi<pesi[l]) {
						 max_lpesi=pesi[l];
					 }
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

				const arma::vec    mu0  = _mu0;
				const double       ka0  = _ka0;
				const unsigned int nu0  = _nu0;
				const arma::mat    Lam0 = _Lam0;


				 arma::mat mu_current (M, d);
				 arma::cube Sig_current (d , d, M);
				 arma::vec S_current  (M);

				cluster_indices_t ci_reorder(y.n_rows);
				ci_reorder.fill(-1);
				std::vector<int>    nj(K);
				std::map< int, std::vector<int> > clusters_indices;

				for(int i=0;i<n;i++){
					clusters_indices[ci_current[i]].push_back(i);
				}

				VERBOSE_DEBUG("clusters_indices is for every cluster the index of data in ci_current.");

				VERBOSE_ASSERT(ci_star.n_rows == (unsigned int) K, "ci_star are unique cluster allocation, must be equal to K");
				VERBOSE_ASSERT(ci_current.n_rows == (unsigned int) n, "ci_current allocate data, should be as big as n, but it is not, ci_current.n_rows = " << ci_current.n_rows << " and n = " << n);

				for(int reorder_cluster_index=0; reorder_cluster_index < K; reorder_cluster_index++){
					const int previous_cluster_index = ci_star[reorder_cluster_index];
					nj[reorder_cluster_index] = clusters_indices[previous_cluster_index].size();
					VERBOSE_ASSERT(nj[reorder_cluster_index] > 0, "It is not possible to have empty cluster, or it would not appear inside ci_star. but this happended reorder_cluster_index = " << reorder_cluster_index);
					for (auto v : clusters_indices[previous_cluster_index]) {
						ci_reorder[v]=reorder_cluster_index;
					}
				}


				for(int l=0; l<K;l++){

					// Find the index of the data in the l-th cluster
					std::vector<int> & which_ind=clusters_indices [ci_star[l]];

					//Prepare the variable that will contain the data in the cluster
					std::vector <arma::vec>  y_l (nj[l]);
					//Separate the data in each cluster and rename the cluster
					for(int it=0;it<nj[l];it++){
						y_l[it]=y.row(which_ind[it]).t();

					}

					const int njl = nj[l]; // This is the number of data in the cluster. I hope so

					//Since in our case the full conditionals are in closed form
					//they are Normal-inverse-gamma


					//Firs compute the posterior parameters

					const arma::vec ysum = y_l.size() ? vectorsum(y_l) : arma::zeros(d);

					VERBOSE_EXTRA("ysum =>" << ysum.n_rows << "x" << ysum.n_cols);
					VERBOSE_EXTRA("++");
					const double ka0nokon = (ka0 * njl ) / (ka0 + njl);
					

					const arma::vec ybar = (ysum / (double) njl) ; // cast?

					VERBOSE_EXTRA("ybar =>" << ybar.n_rows << "x" << ybar.n_cols);
					VERBOSE_EXTRA("mu0  =>" << mu0.n_rows << "x" << mu0.n_cols);
					const arma::vec ybarminusmu0 = (ybar - mu0);
					const arma::rowvec ybarminusmu0t = ybarminusmu0.t();
					const arma::mat  ybarmatmul = ybarminusmu0 * ybarminusmu0t;

					arma::mat S2  = arma::zeros(d, d);
					VERBOSE_EXTRA("S2 = " << S2);
					for (int i = 0 ; i < njl ; i ++) {
						const arma::vec ylmyb = (y_l[i] - ybar);


						const arma::rowvec ylmybt = ylmyb.t();

						const arma::mat matmul = ylmyb * ylmybt;
						VERBOSE_EXTRA("matmul = " << matmul);
						VERBOSE_EXTRA("S2 = " << S2);
						S2 =  S2 + matmul;
					}

					// Then the parameters of the posterior Normal-inverse-gamma a posteriori are
					const double kan = ka0  +  (double) njl; // maybe the cast at double in the sum is not needed
					const unsigned int nun = nu0 + njl;  // maybe the cast at  double in the sum is not needed
					const arma::vec mun = (ysum+mu0*ka0)/kan;
					const arma::mat ka0nokonybarmatmul = ka0nokon * ybarmatmul;
					const arma::mat Lamn = Lam0 + S2 + ka0nokonybarmatmul;


					VERBOSE_EXTRA("In Alloc: ka0  = " << ka0);
					VERBOSE_EXTRA("In Alloc: nun  = " << nun);
					VERBOSE_EXTRA("In Alloc: njl  = " << njl);
					VERBOSE_EXTRA("In Alloc: ka0nokon  = " << ka0nokon);
					VERBOSE_EXTRA("In Alloc: ybar  = " << (ybar));
					VERBOSE_EXTRA("In Alloc: mu0  = " << (mu0));
					VERBOSE_EXTRA("In Alloc: ybarminusmu0t  = " << ybarminusmu0t);
					VERBOSE_EXTRA("In Alloc: ybarminusmu0  = " << ybarminusmu0);
					VERBOSE_EXTRA("In Alloc: ybarmatmul  = " << ybarmatmul);
					VERBOSE_EXTRA("In Alloc: ka0nokonybarmatmul  = " << ka0nokonybarmatmul);
					VERBOSE_EXTRA("In Alloc: S2  = " << S2);
					VERBOSE_EXTRA("In Alloc: Lam0  = " << Lam0);
					VERBOSE_EXTRA("In Alloc: Lamn  = " << Lamn);
					VERBOSE_ASSERT(Lamn.is_sympd(), "In Alloc: rwish requires Lamn to be symmetric. It is not Lamn = " << Lamn);
							
					const arma::mat Sig_l =  riwish (nun, Lamn) ;


					VERBOSE_EXTRA("In Alloc: Sig_l  = " << Sig_l);
					const arma::vec mu_l   = mvrnormArma(mun, Sig_l/kan);
					VERBOSE_EXTRA("In Alloc: mu_l  = " << mu_l);


					Sig_current.slice(l) = Sig_l; // In case 4 we have to update a matrix
					mu_current.row(l)  = mu_l.t();

				}

				for(int l=0; l<K;l++){
					// TODO[CHECK ME] : I split the loop, random generation is not the saame as before, but it should be theoricaly equivalent, isnt it ?
					// Update the Jumps of the allocated part of the process
					S_current[l] = am_rgamma(nj[l]+gamma_current,1./(U_current+1.0));

				}

				VERBOSE_EXTRA("--");
				// Fill non-allocated

				VERBOSE_ASSERT(Lam0.is_sympd(), "In Alloc: rwish requires Lam0 to be symmetric. It is not Lam0 = " << Lam0);
				for(int l=K; l<M;l++){

							const arma::mat res = riwish (nu0, Lam0);

							Sig_current.slice(l) = res; // TODO[CHECK ME] : Raffa study the lam0 - 1
							mu_current.row(l)   = mvrnormArma (mu0, Sig_current.slice(l) / ka0).t() ;
							S_current[l]=am_rgamma(gamma_current,1./(U_current+1.0));
						}

				VERBOSE_EXTRA("End of internal loop ");

			 // TODO[OPTIMIZE ME] : same as initializatrion but initialization can be anything philosophical problem !!!


				this->_Sig_current = Sig_current;
				this->_mu_current = mu_current;

				return allocation_result(ci_reorder , nj , S_current);



		 }
		 input_t sample(const arma::vec & W_current,arma::uword  n) {

			 //
			 //	arma::mat  _mu_current;
			 //	arma::cube _Sig_current;
			 //
			 VERBOSE_EXTRA("sample mvn");
			 unsigned long n_vars = _mu_current.n_cols;

			 arma::uword selected_M = runif_component(W_current);

			 VERBOSE_EXTRA("init res selected_M = " << selected_M << " selected from " << W_current.size() );

			 VERBOSE_EXTRA("init mu0 _mu_current dims = cols:" << _mu_current.n_cols  << " rows:" << _mu_current.n_cols  );
			 VERBOSE_ASSERT(_mu_current.n_rows > selected_M,
					 "_mu_current has less values than the number of component, _mu_current dims are cols:"
					 << _mu_current.n_cols  << " rows:" << _mu_current.n_cols << " while component selected is " << selected_M << " over " << W_current.size() );
			 arma::rowvec mu0  = _mu_current.row(selected_M); // take row
			 VERBOSE_EXTRA("init sig0");

			 VERBOSE_ASSERT(_Sig_current.n_slices > selected_M,
					 "_Sig_current has less values than the number of component, _Sig_current dims are cols:"
					 << _Sig_current.n_cols  << " rows:" << _Sig_current.n_cols  << " slices:" << _Sig_current.n_slices << " while component selected is " << selected_M << " over " << W_current.size() );

			 arma::mat sig0 = _Sig_current.slice(selected_M);


			 VERBOSE_EXTRA("run mvrnormArma");
			 arma::vec res = mvrnormArma (mu0.t(), sig0) ;
			 VERBOSE_EXTRA("res =  " << res);
			 input_t output = input_t(1,n_vars);
			 output.row(0) = res.t();
			 VERBOSE_EXTRA("output =  " << res);
			 return output;
		 }
};






#endif /* ANTMAN_SRC_MIXTUREMULTIVARIATENORMAL_HPP_ */
