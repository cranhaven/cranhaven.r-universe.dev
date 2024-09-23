#ifndef BHPMBB_POISSON_MC_HIER2_LEV0_H
#define BHPMBB_POISSON_MC_HIER2_LEV0_H

class bhpmBB_poisson_mc_hier2_lev0 : public bhpm1a_poisson_mc_hier2_lev0 {
	public:
		bhpmBB_poisson_mc_hier2_lev0();

		bhpmBB_poisson_mc_hier2_lev0(SEXP sChains, SEXP sBurnin, SEXP sIter,
					SEXP sim_type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma_0, SEXP ptau2_gamma_0, SEXP pmu_theta_0,
					SEXP ptau2_theta_0, SEXP pmu_gamma, SEXP pmu_theta,
					SEXP psigma2_gamma, SEXP psigma2_theta, SEXP pPi, SEXP palpha_pi,
					SEXP pbeta_pi,
					SEXP palgo, SEXP padapt_phase);

		virtual ~bhpmBB_poisson_mc_hier2_lev0();

		virtual void gibbs_sampler();

	protected:
		virtual void release();
		virtual void simulate_MH();
		virtual void simulate_SLICE();

		virtual void sample_pi(int gBurnin, int i, int t);

		virtual double log_f_theta(int c, int i, int b, int j, double theta, int t);
		virtual double log_q_theta(int l, int b, int j, double p, double theta, double mean, int t);

		virtual void sample_theta_MH(int burnin, int iter, int t);
		double cMIN(double a, double b);

		virtual void sample_mu_theta(int burnin, int iter, int t);
		virtual void sample_sigma2_theta(int burnin, int iter, int t);

		virtual void init(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi, SEXP palpha_pi, SEXP pbeta_pi,
					SEXP palgo, SEXP padapt_phase);

		virtual void clear();
		virtual void initGlobalSimParams(SEXP sim_type, SEXP sim_params);
		virtual void initSimParams(SEXP sim_params);
		virtual void initMonitor(SEXP sMonitor);

		virtual void initL2Params(SEXP pmu_gamma_0,
						SEXP ptau2_gamma_0, SEXP pmu_theta_0,
						SEXP ptau2_theta_0, SEXP palpha_gamma,
						SEXP pbeta_gamma, SEXP palpha_theta,
						SEXP pbeta_theta, SEXP palpha_pi, SEXP pbeta_pi);

		virtual void initL2Variables(SEXP pmu_gamma, SEXP pmu_theta,
							SEXP psigma2_gamma, SEXP psigma2_theta, SEXP pPi);
		virtual void releaseL2Variables();

		virtual void initL2Samples();
		virtual void releaseL2Samples();
		virtual void releasePMWeights();

		virtual void initPMWeights(SEXP pm_weights);

	public:

		virtual SEXP getPiSamples();

		virtual void getPiSamples(int *c, int *l, int* b, double* pi);


	protected:

		static const char *sMonitor_pi;
		int iMonitor_pi;

		static const char *sColType;
		static const char *sColParam;
		static const char *sColValue;
		static const char *sColControl;

		static const char* sParam_sigma_MH_gamma;
		static const char* sParam_sigma_MH_theta;
		static const char* sParam_sigma_MH_alpha;
		static const char* sParam_sigma_MH_beta;
		static const char* sParam_w_gamma;
		static const char* sParam_w_theta;
		static const char* sParam_w_alpha;
		static const char* sParam_w_beta;

		static const char* sColPMweight;

		typedef enum {BB2004 = 1, MH, ADAPT, INDEP} eAlgoType;
		typedef enum {eSim_Type_MH = 1, eSim_Type_SLICE} eSimType;

		//eAlgoType gAlgo;
		eSimType gSimType;

		double gDefault_Sigma_MH_gamma;
		double gDefault_Sigma_MH_theta;
		double gDefault_W_gamma;
		double gDefault_W_gamma_control;

		// Point-mass weightings
		//double ***gWp;
		double ****gWp;
		double gMH_weight;


		double alpha_pi;  // Current value of the sampled distribution

		double beta_pi;  // Current value of the sampled distribution

		double**** gPi;
		double***** gPi_samples;
};

#endif


