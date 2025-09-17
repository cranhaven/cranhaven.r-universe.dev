/*
 *  AntMAN Package
 *
 */

#ifndef ANTMAN_SRC_UTILS_HPP_
#define ANTMAN_SRC_UTILS_HPP_

#include "math_utils.h"
#include "verbose.h"

typedef arma::ivec cluster_indices_t;

template < typename T>
inline T vectorsum (std::vector <T> elems ) {
	T out = elems[0];
	for (unsigned int i = 1 ; i < elems.size() ; i ++) {
		out += elems[i] ;
	}
	return out;
}

inline double  update_lsd ( double lsd, double ln_acp, double iter) {

	VERBOSE_DEBUG("lsd = " << lsd << " ln_acp = " << ln_acp << " iter = " << iter);

	// This is a new parameter to adjust lsd (ADAPTIVE METROPOLIS; the user could be allowed to set a different value in (-1,0) different that -0.7; Even if it is dangerous to change it
	double wg=std::pow(iter,-0.7);
	// This is a new parameter to adjust lsd (ADAPTIVE METROPOLIS; the user could be allowed to set a different value in (0,1) different that 0.234; We should worn however.
	// The adaptive Rejection Metropolis Hasting we are going to use is Algorithm 5 of Griffin Stephens (2003)
	double bartau =0.234;


	// Adaptive metropolis : Algorithm 5 Griffin Sthephens
	//lsd= lsd+wg*(std::exp(std::min(0.0,ln_acp))-bartau);
	/* Andrea suggestions Apparently Griffin uses the multiplicative version
	Per le'algoritmo 5 e parte del 6, anziche' scrivere:
	s2(new) = s2(old) + wg*(alpha - tau)
	scrivi:
	s2(new) = s2(old) * exp( wg*(alpha - tau) )
	*/
	lsd = lsd + (wg*(std::exp(std::min(0.0,ln_acp))-bartau));

	if(lsd<std::pow(10,-50)){
		lsd=std::pow(10,-50);
	}
	if(lsd>std::pow(10,50)){
		lsd=std::pow(10,50);
	}

	VERBOSE_DEBUG("lsd = " << lsd);

	return lsd;

}


/// This is a function to sample one observation from a multivariate
//  Normal with mean vector mu and varcov Sig

inline arma::vec mvrnormArma(arma::colvec mu, arma::mat SigUnchecked) {  // TODO : What to do in case of error ?

	arma::mat Sig = SigUnchecked;

	//if (not Sig.is_sympd()) {
		//VERBOSE_WARNING("mvrnormArma requires Sig to be symmetric. Sig auto-corrected.");
		//Sig  =  arma::symmatu(Sig);
	//}


	//VERBOSE_ASSERT(Sig.is_sympd(), "mvrnormArma requires Sig to be symmetric. It is not Sig = " << std::endl << Sig);


	arma::vec Y = am_randn(Sig.n_cols);
	arma::mat cholres = Sig;

	try {
		cholres =  arma::chol(Sig) ;

	} catch (std::runtime_error& e) {
		VERBOSE_ERROR("cholesky failed....");
		cholres = Sig;

	}


	return mu +  cholres * Y;
}


inline double dmvnormZero(const arma::mat& x, const arma::vec& mu, const arma::mat& S, const bool log_p = false) {

    arma::uword m = x.n_cols;
    double S_det = arma::det(S);
    arma::mat S_inv = arma::inv(S);
    arma::rowvec X(m);
    arma::rowvec Mu = mu.t();
    X = x.row(0) - Mu;
    if ( log_p ) {
        double P = -1.0 * (x.n_cols/2.0) * M_LN_2PI - 0.5 * log(S_det);
        return arma::as_scalar(P - 0.5 * X * S_inv * X.t());
    } else {
    	double P = 1.0 / sqrt(pow(M_2PI, x.n_cols) * S_det);
    	return arma::as_scalar(P * exp(-0.5 * X * S_inv * X.t()));
    }

}

inline arma::mat riwish(const int df, const arma::mat& iSUnchecked) {  // TODO : What to do in case of error ?

	arma::mat iS = iSUnchecked;

		if (not iS.is_sympd()) {
			VERBOSE_WARNING("riwish requires iS to be symmetric. iS auto-corrected.");
			iS  =  arma::symmatu(iS);
		}


		arma::mat iwishrndres = iS;
			try {
				// commented because the inverse returns warnings 
				//iwishrndres =  arma::iwishrnd(iS, df) ;

				//debugging the chol warning
				arma::mat iS_inv = iS.i();
				if (not iS_inv.is_symmetric()){
					iS_inv = arma::symmatu(iS_inv);
				}
				iwishrndres = arma::symmatu(arma::wishrnd(iS_inv, df).i());
				if (not iwishrndres.is_symmetric()){
					iwishrndres = arma::symmatu(iwishrndres);
				}


			} catch (std::runtime_error& e) {
				VERBOSE_ERROR("cholesky failed....");
				iwishrndres = iS;

			}

	return iwishrndres;
}



#endif /* ANTMAN_SRC_UTILS_HPP_ */
