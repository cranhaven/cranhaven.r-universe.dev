#ifndef BHPM1A_POISSON_MC_HIER3_LEV0_H
#define BHPM1A_POISSON_MC_HIER3_LEV0_H

/*
* hier: 3-level hierarchy
* lev: 0 independent clusters
*/
class bhpm1a_poisson_mc_hier3_lev0 : public bhpm1a_poisson_mc_hier2_lev0 {
	public:
		bhpm1a_poisson_mc_hier3_lev0();

		bhpm1a_poisson_mc_hier3_lev0(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sim_type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0,
					SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta);

		virtual ~bhpm1a_poisson_mc_hier3_lev0();

		virtual void gibbs_sampler();

	protected:
		virtual void release();
		virtual void clear();
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
					SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0,
					SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta);

		virtual void initMonitor(SEXP sMonitor);

		virtual void initL3Params(SEXP pmu_gamma_0_0, SEXP ptau2_gamma_0_0,
							SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0,
							SEXP palpha_gamma_0_0, SEXP pbeta_gamma_0_0,
							SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0,
							SEXP palpha_gamma, SEXP pbeta_gamma,
							SEXP palpha_theta, SEXP pbeta_theta);

        virtual void initL3Variables(SEXP pmu_gamma_0,
							SEXP ptau2_gamma_0, SEXP pmu_theta_0,
							SEXP ptau2_theta_0);

        virtual void initL3Samples();

        virtual void releaseL3Variables();
        virtual void releaseL3Samples();


		virtual SEXP getL3Samples(double*** &data);
		virtual SEXP getL3Samples(double**** &data);

	public:
		virtual SEXP getMuGamma0Samples();
		virtual SEXP getMuTheta0Samples();
		virtual SEXP getTau2Gamma0Samples();
		virtual SEXP getTau2Theta0Samples();

		virtual void getMuGamma0Samples(int *c, int *l, double* mu);
		virtual void getMuTheta0Samples(int *c, int *l, double* mu);
		virtual void getTau2Gamma0Samples(int *c, int *l, double* tau2);
		virtual void getTau2Theta0Samples(int *c, int *l, double* tau2);

	protected:

		static const char *sMonitor_mu_theta_0;
		static const char *sMonitor_mu_gamma_0;
		static const char *sMonitor_tau2_theta_0;
		static const char *sMonitor_tau2_gamma_0;

		int iMonitor_mu_theta_0;
		int iMonitor_mu_gamma_0;
		int iMonitor_tau2_theta_0;
		int iMonitor_tau2_gamma_0;

		double mu_theta_0_0;   // Fixed hyper-parameter value
		double mu_gamma_0_0; // Fixed hyper-parameter value
		double tau2_theta_0_0; // Fixed hyper-parameter value
		double tau2_gamma_0_0; // Fixed hyper-parameter value
		double alpha_gamma_0_0; // Fixed hype-parameter value
		double beta_gamma_0_0; // Fixed hype-parameter value
		double alpha_theta_0_0; // Fixed hype-parameter value
		double beta_theta_0_0; // Fixed hype-parameter value
		double alpha_gamma; // Fixed hype-parameter value
		double beta_gamma; // Fixed hype-parameter value
		double alpha_theta; // Fixed hype-parameter value
		double beta_theta; // Fixed hype-parameter value

		double **mu_gamma_0;  // Current value of the sampled distribution - updated constantly
		double **tau2_gamma_0;   // Current value of the sampled distribution - updated constantly


		// Samples
		double*** mu_gamma_0_samples;
		double*** tau2_gamma_0_samples;

		double ***mu_theta_0;   // Current value of the sampled distribution - updated constantly
		double ***tau2_theta_0;   // Current value of the sampled distribution - updated constantly

		// Samples
		double**** mu_theta_0_samples;
		double**** tau2_theta_0_samples;
};

#endif


