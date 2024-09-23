#ifndef BHPM1A_POISSON_MC_HIER2_LEV0_H
#define BHPM1A_POISSON_MC_HIER2_LEV0_H

/*
* hier: 2-level hierarchy
* lev: 0 independent clusters
*/
class bhpm1a_poisson_mc_hier2_lev0 {
	public:
		bhpm1a_poisson_mc_hier2_lev0();

		bhpm1a_poisson_mc_hier2_lev0(SEXP sChains, SEXP sBurnin, SEXP sIter,
					SEXP sim_type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta);

		virtual ~bhpm1a_poisson_mc_hier2_lev0();

		virtual void gibbs_sampler();

	protected:
		virtual void release();
		virtual void clear();
		virtual void simulate_MH();
		virtual void simulate_SLICE();

		virtual void sample_mu_gamma(int burnin, int iter);
		virtual void sample_mu_theta(int burnin, int iter, int treat = 0);
		virtual void sample_sigma2_gamma(int burnin, int iter);
		virtual void sample_sigma2_theta(int burnin, int iter, int treat = 0);
		virtual double log_f_gamma(int c, int i, int b, int j, double gamm);
		virtual void sample_gamma_MH(int burnin, int iter);
		virtual void sample_gamma_SLICE(int burnin, int iter);
		virtual double log_f_theta(int c, int i, int b, int j, double theta, int treat = 0);
		virtual void sample_theta_MH(int burnin, int iter, int treat = 0);
		virtual void sample_theta_SLICE(int burnin, int iter, int treat = 0);
		double cMIN(double a, double b);

		virtual void init(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta);

		virtual void initBaselineVariables(SEXP sChains, SEXP sBurnin, SEXP sIter,
					SEXP sMem_Model, SEXP sNumTreatments, SEXP sNumClusters, SEXP sMaxBs,
					SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE);
		virtual void releaseBaselineVariables();

		virtual void initSimParams(SEXP sim_params);
		virtual void releaseSimParams();

		virtual void initGlobalSimParams(SEXP sSim_Type, SEXP sGlobal_Sim_Param, SEXP sGlobal_Sim_Param_cntrl);
		virtual void releaseGlobalSimParams();

		virtual void initMonitor(SEXP sMonitor);

		virtual void initDataVariables(SEXP pX, SEXP pY, SEXP pC, SEXP pT);
		virtual void releaseDataVariables();

		virtual void initL1Variables(SEXP ptheta, SEXP pgamma);
		virtual void releaseL1Variables();

		virtual void initL2Variables(SEXP pmu_gamma, SEXP pmu_theta,
										SEXP psigma2_gamma, SEXP psigma2_theta);
		virtual void releaseL2Variables();

		virtual void initL2Params(SEXP pmu_gamma_0, SEXP ptau2_gamma_0,
										SEXP pmu_theta_0, SEXP ptau2_theta_0,
										SEXP palpha_gamma, SEXP pbeta_gamma,
										SEXP palpha_theta, SEXP pbeta_theta);
		virtual void releaseL3Variables() {};

		virtual void initL1Samples();
		virtual void releaseL1Samples();

		virtual void initL2Samples();
		virtual void releaseL2Samples();

		// We only create the samples if the memory model is HIGH or it is low and the
		// sample is being monitored
		virtual int retainSamples(int iMonitor) {
					if ((eMemory_Model == HIGH) || (eMemory_Model == LOW && iMonitor))
						return 1;
					else 
						return 0;
				}

		virtual SEXP getL1Samples(double***** &data);
		virtual SEXP getL1Samples(double****** &data);
		virtual SEXP getL2Samples(double**** &data);
		virtual SEXP getL2Samples(double***** &data);

		virtual SEXP getL1Accept(int**** &data);
		virtual SEXP getL1Accept(int***** &data);

	public:
		virtual SEXP getThetaSamples();
		virtual SEXP getGammaSamples();
		virtual SEXP getMuThetaSamples();
		virtual SEXP getMuGammaSamples();
		virtual SEXP getSigma2ThetaSamples();
		virtual SEXP getSigma2GammaSamples();
		virtual SEXP getThetaAccept();
		virtual SEXP getGammaAccept();

		virtual void getThetaSamples(int *c, int *l, int* b, int* j,
						double* theta_samples);
		virtual void getGammaSamples(int *c, int *l, int* b, int* j,
						double* gamma_samples);

		virtual void getMuThetaSamples(int *c, int *l, int* b, double* mu_theta);
		virtual void getMuGammaSamples(int *c, int *l, int* b, double* mu_gamma);
		virtual void getSigma2ThetaSamples(int *c, int *l, int* b, double* sigma2);
		virtual void getSigma2GammaSamples(int *c, int *l, int* b, double* sigma2);
		virtual void getThetaAccept(int *c, int *l, int* b, int* j, double* theta_acc);
		virtual void getGammaAccept(int *c, int *l, int* b, int* j, double* gamma_acc);

	protected:
		int gChains;
		int gBurnin;
		int gIter;
		char* sim_type;

		static const char *sColMonitorVariables;
		static const char *sColMonitorValues;
		static const char *sMonitor_theta;
		static const char *sMonitor_gamma;
		static const char *sMonitor_mu_theta;
		static const char *sMonitor_mu_gamma;
		static const char *sMonitor_sigma2_theta;
		static const char *sMonitor_sigma2_gamma;

		typedef enum {LOW = 1, HIGH} eMemModel;

		eMemModel eMemory_Model;
		int iMonitor_theta;
		int iMonitor_gamma;
		int iMonitor_mu_theta;
		int iMonitor_mu_gamma;
		int iMonitor_sigma2_theta;
		int iMonitor_sigma2_gamma;

		int gNumTreatments;
		int gNumComparators;
		int gNumClusters;
		int gMaxBs;
		int *gNumBodySys;

		int** gNAE;
		int gMaxAEs;

		// Global simulation parameters
		double gSim_Param;
		double gSim_Param_cntrl;

		// Individual simulation parameters
		double ***gW_gamma;
		int ***gW_gamma_control;
		double ***gSigma_MH_gamma;

		double ****gW_theta;
		int ****gW_theta_control;
		double ****gSigma_MH_theta;

		static const char* sColType;
		static const char* sColVariable;
		static const char* sColParam;
		static const char* sColValue;
		static const char* sColControl;
		static const char* sColB;
		static const char* sColj;
		static const char* sColGroup;
		static const char* sColC_index;

		static const char* sParam_w;
		static const char* sParam_sigma_MH;
		static const char* sVariable_gamma;
		static const char* sVariable_theta;


		// Hyper-parameters
		double mu_theta_0;   // Fixed hyper-parameter value
		double mu_gamma_0; // Fixed hyper-parameter value
		double tau2_theta_0; // Fixed hyper-parameter value
		double tau2_gamma_0; // Fixed hyper-parameter value
		double alpha_gamma; // Fixed hype-parameter value
		double beta_gamma; // Fixed hype-parameter value
		double alpha_theta; // Fixed hype-parameter value
		double beta_theta; // Fixed hype-parameter value

		double**** mu_theta;
		double*** mu_gamma;
		double**** sigma2_theta;
		double*** sigma2_gamma;

		double***** gTheta;
		double**** gGamma;
		int***** gTheta_acc;
		int**** gGamma_acc;

		// Data values
		int ***x;
		int ****y;
		double ***C;
		double ****T;

		// Samples
		double****** gTheta_samples;
		double***** gGamma_samples;
		double***** mu_theta_samples;
		double**** mu_gamma_samples;
		double***** sigma2_theta_samples;
		double**** sigma2_gamma_samples;
};

#endif


