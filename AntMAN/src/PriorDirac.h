/*
 *  AntMAN Package
 *
 */


#ifndef ANTMAN_SRC_PRIORDIRAC_H_
#define ANTMAN_SRC_PRIORDIRAC_H_


#include "math_utils.h"
#include "Prior.h"
#include "AntMANLogger.h"

// --------------------------------------------------------------------------------------------------------------------

class dirac_gamma_q_param_t : public q_param_t {
public:
	int Mstar;
	dirac_gamma_q_param_t (int Mstar) :  Mstar (Mstar){}
	void get_values(AntMANLogger& logger) const {
	}



	void update (const  double U, const  int K, const gamma_h_param_t <dirac_gamma_q_param_t>& h_param) {
		return;
	}

	double log_full_gamma ( const double Loc_gamma, const int K , const  std::vector<int> & nj, const  double U_current ,const  double ag,const  double bg) const {

		double out= - ( Loc_gamma  * Mstar ) * std::log (1 + U_current) ;



		for(int j=0;j<K;j++){
			out+=std::lgamma(Loc_gamma+ (double) nj[j])-std::lgamma(Loc_gamma);
		}
		/// When the prior is a gamma

		out+=(ag-1)*std::log(Loc_gamma)-bg*Loc_gamma;

		return(out);
	}



};

typedef gamma_h_param_t<dirac_gamma_q_param_t> dirac_gamma_h_param_t ;



class PriorDirac : public TypedPrior < dirac_gamma_h_param_t, dirac_gamma_q_param_t> {
public:
	PriorDirac (dirac_gamma_h_param_t gamma_h_param, dirac_gamma_q_param_t dirac_q_param) :
		TypedPrior <dirac_gamma_h_param_t, dirac_gamma_q_param_t> (gamma_h_param,dirac_q_param)  {
		}

	int init_M_na(const int K) {
		int M_na = this->q_param.Mstar - K ;
		VERBOSE_ASSERT(M_na >= 0, "Please provide initial clustering with K <= Mstar: " << K << " (K) > " << this->q_param.Mstar << " (M*)");
		return M_na;
	}
	int update_M_na(const double U ,  const int K) {
		int M_na = this->q_param.Mstar - K ;
		VERBOSE_ASSERT(M_na >= 0, "Internal Error, K > Mstar." << this->q_param.Mstar << K << " (K) > " << this->q_param.Mstar << " (M*)");
		return M_na;

	}

};




#endif /* ANTMAN_SRC_PRIORDIRAC_H_ */
