/*
 *  AntMAN Package
 *
 */


#ifndef ANTMAN_SRC_PRIORNEGATIVEBINOMIAL_H_
#define ANTMAN_SRC_PRIORNEGATIVEBINOMIAL_H_


#include "AntMANLogger.h"
#include "math_utils.h"
#include "Prior.h"
#include "utils.h"

// --------------------------------------------------------------------------------------------------------------------

struct negbin_component {
	 double a,b;
	 double LSD;
	 double LSD_g;
	 double value;
	 bool   fixed;
	 negbin_component() : a(0.0), b(0.0) ,  LSD(1.0), LSD_g(1.0) ,  value(0.0), fixed(true) {

	 }
};

inline std::ostream & operator << (std::ostream &out, const negbin_component &c)
{
	if (c.fixed) {out << "(fixed=" << c.value << ")";}
	else {out << "(a=" << c.a << ", b=" << c.b << ", v=" << c.value << ")";}
    return out;
}

class negative_binomial_gamma_q_param_t  : public q_param_t  {
public:
	negbin_component R,P;

	negative_binomial_gamma_q_param_t (negbin_component R, negbin_component P) : R (R), P (P) {}


	inline double compute_lphi (double U_current, double h_param) const {
			double lphi_u =  - h_param * std::log( 1 + U_current ) ;
			return lphi_u;

		}

		inline double compute_lkappa ( double U_current , double h_param, double K , int nij ,  double P_M , double R_M) const {
			double log_kappa = - ( nij + h_param ) * std::log (U_current + 1) +  std::lgamma( h_param + (double) nij) - std::lgamma(h_param) ;

			return log_kappa;
		}

		inline double compute_lPsi ( double U_current , double h_param, double K ,  double P_M , double R_M) const {

			// h is a gamma density here
			double phi_u = std::exp( compute_lphi(U_current, h_param) );

			double lfirst = std::lgamma(R_M + K - 1) - std::lgamma (R_M) + (K - 1) * std::log (P_M) + R_M * std::log (1 - P_M) ;


			double log_num   =  std::log ( phi_u * (R_M -1 ) + K ) ;
			double log_denum = ( K + R_M) * std::log (1 - phi_u * P_M ) ;

			double lpsi = lfirst + log_num - log_denum;

			return lpsi;
		}

	inline double log_full_EPPF( const double h_param, const double K , const std::vector<int> & nj, const double U_current ) const {
		// TODO[CHECK ME] : Take care this is proportionnal to U
		double lPsi = compute_lPsi ( U_current , h_param, K ,  P.value , R.value) ;

		double log_kappa_sum = 0 ;
		for(int j=0;j<K;j++){
			double log_kappa = compute_lkappa (U_current , h_param, K , nj[j] ,  P.value , R.value);
			log_kappa_sum += log_kappa;
		}

		double out = lPsi + log_kappa_sum;

		return out;
	}
	double log_full_gamma( const double Loc_gamma, const int K ,const  std::vector<int> & nj, const  double U_current ,const  double ag,const  double bg) const {

		double out = log_full_EPPF( Loc_gamma, K , nj, U_current);

		out+=(ag-1)*std::log(Loc_gamma)-bg*Loc_gamma;

		return(out);
	}


	void get_values (AntMANLogger& logger) const {
		logger.addlog("R", this->R.value);
		logger.addlog("P", this->P.value);
	}


	void update (const  double U, const  int K, const gamma_h_param_t <negative_binomial_gamma_q_param_t>& h_param) {

			// Metropolis-Hasting for R_M
			if (not this->R.fixed) {

				double R_vecchio = R.value;
				double R_lmedia = std::log(R_vecchio);

				//Propose a new value
				double R_lnuovo=am_rnorm(R_lmedia,R.LSD);
				double R_nuovo=std::exp(R_lnuovo);

				double log_full_r_m_new =  compute_lPsi ( U ,  h_param.gamma,  K ,   P.value ,  R_nuovo) + (R.a-1)*std::log(R_nuovo)-R.b*R_nuovo ;
				double log_full_r_m_vec =  compute_lPsi ( U ,  h_param.gamma,  K ,   P.value ,  R_vecchio) + (R.a-1)*std::log(R_vecchio)-R.b*R_vecchio;

				double R_ln_acp = (log_full_r_m_new - R_lmedia) - (log_full_r_m_vec - R_lnuovo);

				double R_lnu=std::log(am_runif(0.0,1.0));

				this->R.value = R_lnu < R_ln_acp ? R_nuovo : R_vecchio;

				R.LSD = update_lsd (  R.LSD,  R_ln_acp,  R.LSD_g++) ;

			}

			// Metropolis-Hasting for P_M
		

			if (not this->P.fixed) {
				double P_vecchio = P.value;
				double Z_vecchio = std::log(P_vecchio) - std::log(1 - P_vecchio);

				//Propose a new value
				double Z_nuovo=am_rnorm(Z_vecchio,P.LSD);
				double P_nuovo=std::exp(Z_nuovo) / (1 + std::exp (Z_nuovo));

				double log_full_p_m_new = compute_lPsi (U,h_param.gamma,K,P_nuovo,R.value)    + (P.a)*std::log(P_nuovo)  +(P.b )*std::log(1 - P_nuovo) ;
				double log_full_p_m_vec = compute_lPsi (U,h_param.gamma,K,P_vecchio, R.value) + (P.a)*std::log(P_vecchio)+(P.b )*std::log(1 -P_vecchio);

				double P_ln_acp = std::min(0.0, (log_full_p_m_new) - (log_full_p_m_vec));

	   			VERBOSE_DEBUG("P_ln_acp= "<< P_ln_acp<<"P.LSD="<< P.LSD);
				
				double P_lnu=std::log(am_runif(0.0,1.0));

				this->P.value = P_lnu < P_ln_acp ? P_nuovo : P_vecchio;
				P.LSD = update_lsd (  P.LSD,  P_ln_acp,  P.LSD_g++) ;
			}

			return;
		}


};


inline std::ostream & operator << (std::ostream &out, const negative_binomial_gamma_q_param_t &c)
{
    out << "R:" << c.R << " P:" << c.P;
    return out;
}


typedef gamma_h_param_t<negative_binomial_gamma_q_param_t> negative_binomial_gamma_h_param_t ;


// --------------------------------------------------------------------------------------------------------------------

class PriorNegativeBinomial : public TypedPrior < negative_binomial_gamma_h_param_t, negative_binomial_gamma_q_param_t> {

public:

	PriorNegativeBinomial (negative_binomial_gamma_h_param_t gamma_h_param, negative_binomial_gamma_q_param_t negative_binomial_q_param) :
			TypedPrior <negative_binomial_gamma_h_param_t, negative_binomial_gamma_q_param_t> (gamma_h_param,negative_binomial_q_param)  {
			}


int init_M_na(const int K) {

	    VERBOSE_DEBUG("init_M_na (K = " << K << ")");


		const double R_M     = this->q_param.R.value;
		const double P_M     = this->q_param.P.value;
		// TODO[CHECK ME]: According to Rafaelle what we call P is 1-1 in R. and what we call R_M is Size in R.

	    VERBOSE_DEBUG("R_M = " << R_M);
	    VERBOSE_DEBUG("P_M = " << P_M);

	    double M_na =  am_rnbinom(R_M, 1-P_M);

	    VERBOSE_DEBUG("M_na = rnbinom(R_M, 1-P_M) = " << M_na << " = ");

	    return M_na;
	}

int update_M_na(const double U ,  const int KasInt) {
	const double K = KasInt;

    VERBOSE_DEBUG("update_M_na (U = " << U << ",K = " << K << ")");

		const double R_M     = this->q_param.R.value; // TODO : fix that M makes no sense
		const double P_M     = this->q_param.P.value;

	    VERBOSE_DEBUG("R_M = " << R_M);
	    VERBOSE_DEBUG("P_M = " << P_M);
	    VERBOSE_DEBUG("gamma = " << this->h_param.gamma);

		double M_na;

		const double phi_u  = 1 / std::pow(1 + U, this->h_param.gamma);
	    VERBOSE_DEBUG("phi_u = " << " 1 / std::pow(" << 1 + U <<  ", " <<  this->h_param.gamma << ")" << " 1 / " << std::pow(1 + U, this->h_param.gamma) << " = " << " = " << phi_u);

		const double up     = (R_M + K) * phi_u * (P_M) ;
		const double down   =  R_M * phi_u * P_M + K ;
		const double peso   =  up / down;

		const double unif=am_runif(0.0,1.0);
	    VERBOSE_DEBUG("unif = " << unif << " peso = " << peso);

		if(unif < peso){
			M_na=am_rnbinom(R_M + K, 1- phi_u * P_M) + 1;
		    VERBOSE_DEBUG("M_na=rnbinom( " <<  R_M + K <<  ", " <<  phi_u * P_M <<  ") + 1;");
		} else {
			M_na=am_rnbinom(R_M - 1 + K, 1-phi_u * P_M) ;
		    VERBOSE_DEBUG("M_na=rnbinom( " << R_M - 1 + K <<  ", " <<  phi_u * P_M <<  ") ;");
		}

	    VERBOSE_DEBUG("M_na = " << M_na);

		return M_na;

	}

};
// --------------------------------------------------------------------------------------------------------------------

#endif /* ANTMAN_SRC_PRIORNEGATIVEBINOMIAL_H_ */
