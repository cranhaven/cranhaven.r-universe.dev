#ifndef BHPM_EXEC_H
#define BHPM_EXEC_H

extern "C" {

SEXP bhpm1a_poisson_mc_exec(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_ctrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sLevel,
					SEXP sMaxBs, SEXP sNumBodySys,
					SEXP sMaxAEs, SEXP sNAE,
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

SEXP bhpm1a_cluster_hier2_exec(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_ctrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sLevel,
					SEXP sMaxBs, SEXP sNumBodySys,
					SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta);


SEXP getThetaSamplesClusterAll();
SEXP getGammaSamplesClusterAll();
SEXP getMuThetaSamplesClusterAll();
SEXP getMuGammaSamplesClusterAll();
SEXP getSigma2ThetaSamplesClusterAll();
SEXP getSigma2GammaSamplesClusterAll();
SEXP getThetaAcceptClusterAll();
SEXP getGammaAcceptClusterAll();
SEXP getMuGamma0SamplesClusterAll();
SEXP getMuTheta0SamplesClusterAll();
SEXP getTau2Gamma0SamplesClusterAll();
SEXP getTau2Theta0SamplesClusterAll();

void getThetaSamplesCluster(int *c, int* l, int* b, int* j, double* theta_samples);
void getGammaSamplesCluster(int *c, int* l, int* b, int* j, double* theta_samples);
void getMuThetaSamplesCluster(int *c, int *l, int* b, double* mu_theta);
void getMuGammaSamplesCluster(int *c, int *l, int* b, double* mu_gamma);
void getSigma2ThetaSamplesCluster(int *c, int *l, int* b, double* mu_theta);
void getSigma2GammaSamplesCluster(int *c, int *l, int* b, double* mu_gamma);
void getMuGamma0SamplesCluster(int *c, int *l, double* mu);
void getMuTheta0SamplesCluster(int *c, int *l, double* mu);
void getTau2Gamma0SamplesCluster(int *c, int *l, double* tau2);
void getTau2Theta0SamplesCluster(int *c, int *l, double* tau2);
void getThetaAcceptCluster(int *c, int *l, int* b, int* j, double* theta_acc);
void getGammaAcceptCluster(int *c, int *l, int* b, int* j, double* gamma_acc);
void Release_Cluster();

SEXP bhpmBB_poisson_mc_exec(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model, SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sLevel,
					SEXP sMaxBs, SEXP sNumBodySys,
					SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma, SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0, SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta, SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0, SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi,
					SEXP palpha_pi,
					SEXP pbeta_pi,
					SEXP plambda_alpha,
					SEXP plambda_beta,
					SEXP palgo,
					SEXP padapt_phase);

SEXP bhpmBB_cluster_hier2_exec(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobalSim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sLevel,
					SEXP sMaxBs, SEXP sNumBodySys,
					SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta, SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0, SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi,
					SEXP palpha_pi,
					SEXP pbeta_pi,
					SEXP palgo,
					SEXP padapt_phase);

SEXP getPiSamplesClusterAll();
SEXP getAlphaPiSamplesClusterAll();
SEXP getBetaPiSamplesClusterAll();
SEXP getAlphaPiAcceptClusterAll();
SEXP getBetaPiAcceptClusterAll();

void getPiSamplesCluster(int* c, int* l, int* b, double* pi);
void getAlphaPiSamplesCluster(int *c, int* l, double* alpha_pi);
void getBetaPiSamplesCluster(int *c, int* l, double* beta_pi);
void getAlphaPiAcceptCluster(int *c, int* l, double* acc);
void getBetaPiAcceptCluster(int *c, int* l, double* acc);

}

#endif
