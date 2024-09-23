#ifndef BHPM1A_POISSON_MC_HIER3_LEV1_H
#define BHPM1A_POISSON_MC_HIER3_LEV1_H

class bhpm1a_poisson_mc_hier3_lev1 : public bhpm1a_poisson_mc_hier3_lev2 {
	public:
		bhpm1a_poisson_mc_hier3_lev1();

		bhpm1a_poisson_mc_hier3_lev1(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sim_type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatment,
					SEXP sNumClusters,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0, SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta, SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0, SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta);

		virtual ~bhpm1a_poisson_mc_hier3_lev1();

		virtual void gibbs_sampler();

	protected:
		virtual void clear();
		virtual void release();
		virtual void simulate_MH();
		virtual void simulate_SLICE();

		virtual void sample_mu_gamma_0(int burnin, int iter);
		virtual void sample_mu_theta_0(int burnin, int iter, int tr);
		virtual void sample_tau2_gamma_0(int burnin, int iter);
		virtual void sample_tau2_theta_0(int burnin, int iter, int tr);
		virtual void sample_mu_gamma(int burnin, int iter);
		virtual void sample_mu_theta(int burnin, int iter, int tr);
		virtual void sample_sigma2_gamma(int burnin, int iter);
		virtual void sample_sigma2_theta(int burnin, int iter, int tr);
		virtual double log_f_gamma(int c, int i, int b, int j, double gamm);
		virtual void sample_gamma_MH(int burnin, int iter);
		virtual void sample_gamma_SLICE(int burnin, int iter);
		virtual double log_f_theta(int c, int i, int b, int j, double theta, int tr);
		virtual void sample_theta_MH(int burnin, int iter, int tr);
		virtual void sample_theta_SLICE(int burnin, int iter, int r);
		double cMIN(double a, double b);

		virtual void init(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatment,
					SEXP sNumClusters,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0, SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta, SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0, SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta);

		virtual void initL2Variables(SEXP pmu_gamma, SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta);

		virtual void releaseL2Variables();
		virtual void releaseL2Samples();

        virtual void initL2Samples();

		virtual SEXP getL2Samples(double*** &data);
		virtual SEXP getL2Samples(double**** &data);

	public:
		virtual SEXP getMuThetaSamples();
		virtual SEXP getMuGammaSamples();
		virtual SEXP getSigma2ThetaSamples();
		virtual SEXP getSigma2GammaSamples();

		virtual void getMuThetaSamples(int *c, int *l, int* b, double* mu_theta);
		virtual void getMuGammaSamples(int *c, int *l, int* b, double* mu_gamma);
		virtual void getSigma2ThetaSamples(int *c, int *l, int* b, double* sigma2);
		virtual void getSigma2GammaSamples(int *c, int *l, int* b, double* sigma2);

	protected:
		double*** mu_theta;
		double** mu_gamma;
		double*** sigma2_theta;
		double** sigma2_gamma;

		double**** mu_theta_samples;
		double*** mu_gamma_samples;
		double**** sigma2_theta_samples;
		double*** sigma2_gamma_samples;
};

#endif


