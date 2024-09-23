#ifndef BHPMBB_POISSON_MC_HIER3_LEV0_H
#define BHPMBB_POISSON_MC_HIER3_LEV0_H

class bhpmBB_poisson_mc_hier3_lev0 : public bhpm1a_poisson_mc_hier3_lev0 {
	public:
		bhpmBB_poisson_mc_hier3_lev0();

		bhpmBB_poisson_mc_hier3_lev0(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sim_type,
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
					SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0,
					SEXP palpha_gamma_0_0, SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0,
					SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma_0, SEXP ptau2_gamma_0, SEXP pmu_theta_0,
					SEXP ptau2_theta_0, SEXP pmu_gamma, SEXP pmu_theta,
					SEXP psigma2_gamma, SEXP psigma2_theta, SEXP pPi, SEXP palpha_pi,
					SEXP pbeta_pi, SEXP plambda_alpha, SEXP plambda_beta,
					SEXP palgo, SEXP padapt_phase);

		virtual ~bhpmBB_poisson_mc_hier3_lev0();

		virtual void gibbs_sampler();

	protected:
		virtual void release();
		virtual void simulate_MH();
		virtual void simulate_SLICE();

		virtual void sample_alpha_pi_MH(int gBurnin, int i, int t);
		virtual void sample_alpha_pi_SLICE(int gBurnin, int i, int t);
		double log_f_alpha_pi(int c, int l, double alpha, int t);

		virtual void sample_beta_pi_SLICE(int gBurnin, int i, int t);
		virtual void sample_beta_pi_MH(int gBurnin, int i, int t);
		virtual double log_f_beta_pi(int c, int l, double alpha, int t);

		virtual void sample_pi(int gBurnin, int i, int t);

		virtual double log_f_theta(int c, int i, int b, int j, double theta, int t);
		virtual double log_q_theta(int l, int b, int j, double p, double theta, double mean, int t);

		virtual void sample_mu_theta(int burnin, int iter, int t);
		virtual void sample_sigma2_theta(int burnin, int iter, int t);
		virtual void sample_theta_MH(int burnin, int iter, int t);
		double cMIN(double a, double b);

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
					SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0,
					SEXP palpha_gamma_0_0, SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0,
					SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi, SEXP palpha_pi, SEXP pbeta_pi, SEXP plambda_alpha,
					SEXP plambda_beta,
					SEXP palgo, SEXP padapt_phase);

		virtual void clear();

		virtual void initGlobalSimParams(SEXP sim_type, SEXP sim_params);

		virtual void initSimParams(SEXP sim_params);

		virtual void initPMWeights(SEXP pm_weights);
		virtual void releasePMWeights();

		virtual void initMonitor(SEXP sMonitor);

		virtual void initL3Params(SEXP pmu_gamma_0_0, SEXP ptau2_gamma_0_0,
					SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0,
					SEXP palpha_gamma_0_0, SEXP pbeta_gamma_0_0,
					SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0,
					SEXP palpha_gamma, SEXP pbeta_gamma,
					SEXP palpha_theta, SEXP pbeta_theta,
					SEXP plambda_alpha, SEXP plambda_beta);

		virtual void initL3Variables(SEXP pmu_gamma_0, SEXP ptau2_gamma_0,
					SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP palpha_pi, SEXP pbeta_pi);
		virtual void releaseL3Variables();

		virtual void initL3Samples();
		virtual void releaseL3Samples();

		virtual void initL2Variables(SEXP pmu_gamma, SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta, SEXP pPi);
		virtual void releaseL2Variables();
		virtual void initL2Samples();
		virtual void releaseL2Samples();

		virtual SEXP getL3Accept(int** &data);
		virtual SEXP getL3Accept(int*** &data);

	public:
		virtual SEXP getPiSamples();
		virtual SEXP getAlphaPiSamples();
		virtual SEXP getBetaPiSamples();
		virtual SEXP getAlphaPiAccept();
		virtual SEXP getBetaPiAccept();

		virtual void getPiSamples(int *c, int *l, int* b, double* pi);
		virtual void getAlphaPiSamples(int *c, int*l, double* alpha_pi);
		virtual void getBetaPiSamples(int *c, int*l, double* beta_pi);
		virtual void getAlphaPiAccept(int *c, int *l, int* t, double* acc);
		virtual void getBetaPiAccept(int *c, int *l, int *t, double* acc);


	protected:
		static const char *sMonitor_pi;
		static const char *sMonitor_alpha_pi;
		static const char *sMonitor_beta_pi;

		int iMonitor_pi;
		int iMonitor_alpha_pi;
		int iMonitor_beta_pi;

		static const char *sColType;
		static const char *sColParam;
		static const char *sColValue;
		static const char *sColControl;

		static const char* sColPMweight;

		static const char* sParam_sigma_MH_gamma;
		static const char* sParam_sigma_MH_theta;
		static const char* sParam_sigma_MH_alpha;
		static const char* sParam_sigma_MH_beta;
		static const char* sParam_w_gamma;
		static const char* sParam_w_theta;
		static const char* sParam_w_alpha;
		static const char* sParam_w_beta;

		static const char* sVariable_alpha;
		static const char* sVariable_beta;

		typedef enum {BB2004 = 1, MH, ADAPT, INDEP} eAlgoType;
		typedef enum {eSim_Type_MH = 1, eSim_Type_SLICE} eSimType;

		eAlgoType gAlgo;
		eSimType gSimType;

		double gDefault_Sigma_MH_theta;

		double gDefault_Sigma_MH_gamma;
		double gDefault_W_gamma;
		double gDefault_W_gamma_control;

		double gDefault_Sigma_MH_alpha;
		double gDefault_Sigma_MH_beta;
		double gDefault_W_alpha;
		double gDefault_W_beta;
		double gDefault_W_alpha_control;
		double gDefault_W_beta_control;

		double **gW_alpha;
		double **gW_beta;
		double **gW_alpha_control;
		double **gW_beta_control;
		double **gSigma_MH_alpha;
		double **gSigma_MH_beta;

		// Point-mass weightings
		double ****gWp;
		double gMH_weight;

		double*** alpha_pi;  // Current value of the sampled distribution
		double**** alpha_pi_samples;
		int*** alpha_pi_acc;

		double*** beta_pi;  // Current value of the sampled distribution
		double**** beta_pi_samples;
		int*** beta_pi_acc;

		double**** gPi;
		double***** gPi_samples;

		double lambda_alpha;
		double lambda_beta;

		// Adapt for some of the MH steps
		int in_apapt_phase;
};

#endif
