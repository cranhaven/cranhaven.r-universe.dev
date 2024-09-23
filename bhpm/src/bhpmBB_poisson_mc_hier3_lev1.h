#ifndef BHPMBB_POISSON_MC_HIER3_LEV1_H
#define BHPMBB_POISSON_MC_HIER3_LEV1_H

class bhpmBB_poisson_mc_hier3_lev1 : public bhpmBB_poisson_mc_hier3_lev2 {
	public:
		bhpmBB_poisson_mc_hier3_lev1();

		bhpmBB_poisson_mc_hier3_lev1(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sim_type,
					SEXP sMem_Model, SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatment,
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
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi, SEXP palpha_pi, SEXP pbeta_pi, SEXP plambda_alpha,
					SEXP plambda_beta,
					SEXP palgo, SEXP padapt_phase);

		virtual ~bhpmBB_poisson_mc_hier3_lev1();

		virtual void gibbs_sampler();

	protected:
		virtual void release();
		virtual void simulate_MH();
		virtual void simulate_SLICE();

		virtual void sample_mu_gamma_0(int burnin, int iter);
		virtual void sample_mu_theta_0(int burnin, int iter, int tr);
		virtual void sample_tau2_gamma_0(int burnin, int iter);
		virtual void sample_tau2_theta_0(int burnin, int iter, int tr);

		virtual void sample_alpha_pi_MH(int gBurnin, int i, int tr);
		virtual void sample_alpha_pi_SLICE(int gBurnin, int i, int tr);
		double log_f_alpha_pi(int c, double alpha, int tr);

		virtual void sample_beta_pi_SLICE(int gBurnin, int i, int tr);
		virtual void sample_beta_pi_MH(int gBurnin, int i, int tr);
		virtual double log_f_beta_pi(int c, double alpha, int tr);

		virtual void sample_pi(int gBurnin, int i, int tr);

		virtual double log_f_theta(int c, int i, int b, int j, double theta, int tr);
		virtual double log_q_theta(int l, int b, int j, double p, double theta, double mean, int tr);

		virtual void sample_mu_gamma(int burnin, int iter);
		virtual void sample_mu_theta(int burnin, int iter, int tr);
		virtual void sample_sigma2_gamma(int burnin, int iter);
		virtual void sample_sigma2_theta(int burnin, int iter, int tr);
		virtual double log_f_gamma(int c, int i, int b, int j, double gamm);
		virtual void sample_gamma_MH(int burnin, int iter);
		virtual void sample_gamma_SLICE(int burnin, int iter);
		virtual void sample_theta_MH(int burnin, int iter, int tr);
		double cMIN(double a, double b);

		virtual void init(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatment,
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
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi, SEXP palpha_pi, SEXP pbeta_pi, SEXP plambda_alpha,
					SEXP plambda_beta,
					SEXP palgo, SEXP padapt_phase);

		virtual void clear();
		virtual void initL2Variables(SEXP pmu_gamma, SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta, SEXP pPi);
		virtual void releaseL2Variables();
		virtual void initL2Samples();
		virtual void releaseL2Samples();

		virtual SEXP getL2Samples(double*** &data);
		virtual SEXP getL2Samples(double**** &data);

	public:
		virtual SEXP getMuThetaSamples();
		virtual SEXP getMuGammaSamples();
		virtual SEXP getSigma2ThetaSamples();
		virtual SEXP getSigma2GammaSamples();
		virtual SEXP getPiSamples();

		virtual void getMuThetaSamples(int *c, int *l, int* b, double* mu_theta);
		virtual void getMuGammaSamples(int *c, int *l, int* b, double* mu_gamma);
		virtual void getSigma2ThetaSamples(int *c, int *l, int* b, double* sigma2);
		virtual void getSigma2GammaSamples(int *c, int *l, int* b, double* sigma2);

		virtual void getPiSamples(int *c, int *l, int* b, double* pi);

	protected:
		double*** gPi;
		double**** gPi_samples;

		double*** mu_theta;
		double** mu_gamma;
		double*** sigma2_theta;
		double** sigma2_gamma;

		double**** mu_theta_samples;
		double*** mu_gamma_samples;
		double**** sigma2_theta_samples;
		double*** sigma2_gamma_samples;

		// Adapt for some of the MH steps
		int in_apapt_phase;
};

#endif


