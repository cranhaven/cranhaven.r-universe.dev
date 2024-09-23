#include<cstdio>
#include<cstdlib>

#include<map>

#include <R.h>
#include <Rmath.h>
#include <R_ext/Print.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "bhpm_exec.h"
#include "bhpm1a_poisson_mc_hier2_lev0.h"
#include "bhpm1a_poisson_mc_hier2_lev1.h"
#include "bhpmBB_poisson_mc_hier2_lev0.h"
#include "bhpmBB_poisson_mc_hier2_lev1.h"
#include "bhpm1a_poisson_mc_hier3_lev0.h"
#include "bhpm1a_poisson_mc_hier3_lev2.h"
#include "bhpm1a_poisson_mc_hier3_lev1.h"
#include "bhpmBB_poisson_mc_hier3_lev0.h"
#include "bhpmBB_poisson_mc_hier3_lev2.h"
#include "bhpmBB_poisson_mc_hier3_lev1.h"

using namespace std;

static const char *rcsId = "$Id: bhpm_exec.cpp,v 1.7 2019/04/28 13:51:57 clb13102 Exp clb13102 $";

static bhpm1a_poisson_mc_hier2_lev0* model_cluster = NULL;

SEXP bhpm1a_cluster_hier2_exec(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sLevel,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta)
{
	try {

		if (!model_cluster) {
			delete model_cluster;
			model_cluster = NULL;
		}

		int level = *(INTEGER(sLevel));

		switch (level) {

			case 0:
				model_cluster = new bhpm1a_poisson_mc_hier2_lev0(sChains, sBurnin,
										sIter,
										sSim_Type, sMem_Model, sGlobal_Sim_Param,
										sGlobal_Sim_Param_cntrl,
										sSim_Param,
										sMonitor, sNumTreatments, sNumClusters, sMaxBs,
										sNumBodySys,
										sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,	
										pmu_gamma_0, ptau2_gamma_0,
										pmu_theta_0, ptau2_theta_0,
										palpha_gamma,
										pbeta_gamma,
										palpha_theta, pbeta_theta,
										pmu_gamma, pmu_theta, psigma2_gamma,
										psigma2_theta);
			break;

			case 1:
				model_cluster = new bhpm1a_poisson_mc_hier2_lev1(sChains, sBurnin,
										sIter,
										sSim_Type, sMem_Model, sGlobal_Sim_Param,
										sGlobal_Sim_Param_cntrl,
										sSim_Param,
										sMonitor, sNumTreatments, sNumClusters, sMaxBs,
										sNumBodySys,
										sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,
										pmu_gamma_0, ptau2_gamma_0,
										pmu_theta_0, ptau2_theta_0,
										palpha_gamma, pbeta_gamma,
										palpha_theta, pbeta_theta,
										pmu_gamma, pmu_theta, psigma2_gamma,
										psigma2_theta);
			break;

			default: // level 0
				model_cluster = new bhpm1a_poisson_mc_hier2_lev0(sChains, sBurnin,
										sIter,
										sSim_Type, sMem_Model, sGlobal_Sim_Param,
										sGlobal_Sim_Param_cntrl,
										sSim_Param,
										sMonitor, sNumTreatments, sNumClusters, sMaxBs,
										sNumBodySys,
										sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,
										pmu_gamma_0, ptau2_gamma_0,
										pmu_theta_0, ptau2_theta_0,
										palpha_gamma, pbeta_gamma,
										palpha_theta, pbeta_theta,
										pmu_gamma, pmu_theta, psigma2_gamma,
										psigma2_theta);
			break;
		}

		model_cluster->gibbs_sampler();
		//model_hier2->gibbs_sampler();
	}
	catch(...) {
		Rprintf("Unexpected exception bhpm1a_poisson_mc\n");
	}
	return R_NilValue;
}

// Cluster Analysis Models
SEXP bhpm1a_poisson_mc_exec(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,	
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sLevel,
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
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta)
{
	try {

		if (!model_cluster) {
			delete model_cluster;
			model_cluster = NULL;
		}

		int level = *(INTEGER(sLevel));

		switch (level) {

			case 0:
				model_cluster = new bhpm1a_poisson_mc_hier3_lev0(sChains, sBurnin,
										sIter,
										sSim_Type,
										sMem_Model,
										sGlobal_Sim_Param,
										sGlobal_Sim_Param_cntrl,
										sSim_Param,
										sMonitor,
										sNumTreatments,
										sNumClusters, sMaxBs,
										sNumBodySys,
										sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,	
										pmu_gamma_0_0, ptau2_gamma_0_0,
										pmu_theta_0_0, ptau2_theta_0_0,
										palpha_gamma_0_0,
										pbeta_gamma_0_0,
										palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma,
										pbeta_gamma, palpha_theta,
										pbeta_theta, pmu_gamma_0, ptau2_gamma_0,
										pmu_theta_0,
										ptau2_theta_0,
										pmu_gamma, pmu_theta, psigma2_gamma,
										psigma2_theta);
			break;

			case 1:
				model_cluster = new bhpm1a_poisson_mc_hier3_lev1(sChains, sBurnin, sIter,
										sSim_Type,
										sMem_Model, 
										sGlobal_Sim_Param,
										sGlobal_Sim_Param_cntrl,
										sSim_Param,
										sMonitor,
										sNumTreatments,
										sNumClusters, sMaxBs,
										sNumBodySys,
										sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,
										pmu_gamma_0_0, ptau2_gamma_0_0,
										pmu_theta_0_0, ptau2_theta_0_0,
										palpha_gamma_0_0, pbeta_gamma_0_0,
										palpha_theta_0_0, pbeta_theta_0_0,
										palpha_gamma, pbeta_gamma, palpha_theta,
										pbeta_theta, pmu_gamma_0, ptau2_gamma_0,
										pmu_theta_0, ptau2_theta_0,
										pmu_gamma, pmu_theta, psigma2_gamma,
										psigma2_theta);
			break;

			case 2:
				model_cluster = new bhpm1a_poisson_mc_hier3_lev2(sChains, sBurnin, sIter,
										sSim_Type,
										sMem_Model,
										sGlobal_Sim_Param,
										sGlobal_Sim_Param_cntrl,
										sSim_Param,
										sMonitor,
										sNumTreatments,
										sNumClusters, sMaxBs,
										sNumBodySys,
										sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,
										pmu_gamma_0_0, ptau2_gamma_0_0,
										pmu_theta_0_0, ptau2_theta_0_0,
										palpha_gamma_0_0, pbeta_gamma_0_0,
										palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma,
										pbeta_gamma, palpha_theta,
										pbeta_theta, pmu_gamma_0, ptau2_gamma_0,
										pmu_theta_0, ptau2_theta_0,
										pmu_gamma, pmu_theta, psigma2_gamma,
										psigma2_theta);
			break;

			default: // level 0
				model_cluster = new bhpm1a_poisson_mc_hier3_lev0(sChains, sBurnin,
										sIter,
										sSim_Type,
										sMem_Model,
										sGlobal_Sim_Param,
										sGlobal_Sim_Param_cntrl,
										sSim_Param,
										sMonitor,
										sNumTreatments,
										sNumClusters, sMaxBs,
										sNumBodySys,
										sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,
										pmu_gamma_0_0, ptau2_gamma_0_0,
										pmu_theta_0_0, ptau2_theta_0_0,
										palpha_gamma_0_0, pbeta_gamma_0_0,
										palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma,
										pbeta_gamma, palpha_theta,
										pbeta_theta, pmu_gamma_0, ptau2_gamma_0,
										pmu_theta_0, ptau2_theta_0,
										pmu_gamma, pmu_theta, psigma2_gamma,
										psigma2_theta);
			break;
		}

		model_cluster->gibbs_sampler();
	}
	catch(...) {
		Rprintf("Unexpected exception bhpm1a_poisson_mc\n");
	}
	return R_NilValue;
}

SEXP getThetaSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	if (model_cluster)
		samples = model_cluster->getThetaSamples();

	return samples;
}

SEXP getGammaSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	if (model_cluster)
		samples = model_cluster->getGammaSamples();

	return samples;
}

SEXP getMuThetaSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	if (model_cluster)
		samples = model_cluster->getMuThetaSamples();

	return samples;
}

SEXP getMuGammaSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	if (model_cluster)
		samples = model_cluster->getMuGammaSamples();

	return samples;
}

SEXP getSigma2GammaSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	if (model_cluster)
		samples = model_cluster->getSigma2GammaSamples();

	return samples;
}

SEXP getSigma2ThetaSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	if (model_cluster)
		samples = model_cluster->getSigma2ThetaSamples();

	return samples;
}

SEXP getGammaAcceptClusterAll()
{
	SEXP acc = R_NilValue;

	if (model_cluster)
		acc = model_cluster->getGammaAccept();

	return acc;
}

SEXP getThetaAcceptClusterAll()
{
	SEXP acc = R_NilValue;

	if (model_cluster)
		acc = model_cluster->getThetaAccept();

	return acc;
}

SEXP getMuGamma0SamplesClusterAll()
{
	SEXP samples = R_NilValue;

	bhpm1a_poisson_mc_hier3_lev0* model1a = dynamic_cast<bhpm1a_poisson_mc_hier3_lev0 *>(model_cluster);

	if (model1a)
		samples = model1a->getMuGamma0Samples();

	return samples;
}

SEXP getMuTheta0SamplesClusterAll()
{
	SEXP samples = R_NilValue;

	bhpm1a_poisson_mc_hier3_lev0* model1a = dynamic_cast<bhpm1a_poisson_mc_hier3_lev0 *>(model_cluster);

	if (model1a)
		samples = model1a->getMuTheta0Samples();

	return samples;
}

SEXP getTau2Gamma0SamplesClusterAll()
{
	SEXP samples = R_NilValue;

	bhpm1a_poisson_mc_hier3_lev0* model1a = dynamic_cast<bhpm1a_poisson_mc_hier3_lev0 *>(model_cluster);

	if (model1a)
		samples = model1a->getTau2Gamma0Samples();

	return samples;
}

SEXP getTau2Theta0SamplesClusterAll()
{
	SEXP samples = R_NilValue;

	bhpm1a_poisson_mc_hier3_lev0* model1a = dynamic_cast<bhpm1a_poisson_mc_hier3_lev0 *>(model_cluster);

	if (model1a)
		samples = model1a->getTau2Theta0Samples();

	return samples;
}

void getThetaSamplesCluster(int *c, int* l, int* b, int* j, double* theta_samples)
{
	if (model_cluster)
		model_cluster->getThetaSamples(c, l, b ,j, theta_samples);
}

void getGammaSamplesCluster(int *c, int* l, int* b, int* j, double* gamma_samples)
{
	if (model_cluster)
		model_cluster->getGammaSamples(c, l, b, j, gamma_samples);
}

void getMuThetaSamplesCluster(int *c, int* l, int* b, double* mu_theta)
{
	if (model_cluster)
		model_cluster->getMuThetaSamples(c, l, b, mu_theta);
}

void getMuGammaSamplesCluster(int *c, int* l, int* b, double* mu_gamma)
{
	if (model_cluster)
		model_cluster->getMuGammaSamples(c, l, b, mu_gamma);
}

void getSigma2ThetaSamplesCluster(int *c, int* l, int* b, double* sigma2)
{
	if (model_cluster)
		model_cluster->getSigma2ThetaSamples(c, l, b, sigma2);
}

void getSigma2GammaSamplesCluster(int *c, int* l, int* b, double* sigma2)
{
	if (model_cluster)
		model_cluster->getSigma2GammaSamples(c, l, b, sigma2);
}

void getThetaAcceptCluster(int *c, int* l, int* b, int* j, double* theta_acc)
{
	if (model_cluster)
		model_cluster->getThetaAccept(c, l, b ,j, theta_acc);
}

void getGammaAcceptCluster(int *c, int* l, int* b, int* j, double* gamma_acc)
{
	if (model_cluster)
		model_cluster->getGammaAccept(c, l, b ,j, gamma_acc);
}

void Release_Cluster()
{
	if (model_cluster)
		delete model_cluster;
	model_cluster = NULL;
}

SEXP bhpmBB_poisson_mc_exec(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model, SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sLevel,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
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
					SEXP padapt_phase)
{
	try {

		if (!model_cluster) {
			delete model_cluster;
			model_cluster = NULL;
		}

		int level = *(INTEGER(sLevel));

		switch(level) {
			case 0:
				model_cluster = new bhpmBB_poisson_mc_hier3_lev0(sChains, sBurnin, sIter, sSim_Type,
								sMem_Model, sGlobal_Sim_Params,
								sSim_Params,
								MH_weight,
								pm_weights,
								sMonitor,
								sNumTreatments,
								sNumClusters, sMaxBs, sNumBodySys,
								sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma, pmu_gamma_0_0, ptau2_gamma_0_0,
								pmu_theta_0_0, ptau2_theta_0_0, palpha_gamma_0_0, pbeta_gamma_0_0,
								palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma, pbeta_gamma, palpha_theta,
								pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0,
								pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
								pPi,
								palpha_pi,
								pbeta_pi,
								plambda_alpha,
								plambda_beta,
								palgo,
								padapt_phase);
			break;

			case 1:
				model_cluster = new bhpmBB_poisson_mc_hier3_lev1(sChains, sBurnin, sIter, sSim_Type,
								sMem_Model, sGlobal_Sim_Params,
								sSim_Params,
								MH_weight,
								pm_weights,
								sMonitor,
								sNumTreatments,
								sNumClusters, sMaxBs, sNumBodySys,
								sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma, pmu_gamma_0_0, ptau2_gamma_0_0,
								pmu_theta_0_0, ptau2_theta_0_0, palpha_gamma_0_0, pbeta_gamma_0_0,
								palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma, pbeta_gamma, palpha_theta,
								pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0,
								pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
								pPi,
								palpha_pi,
								pbeta_pi,
								plambda_alpha,
								plambda_beta,
								palgo,
								padapt_phase);
			break;

			case 2:
				model_cluster = new bhpmBB_poisson_mc_hier3_lev2(sChains, sBurnin, sIter, sSim_Type,
								sMem_Model, sGlobal_Sim_Params,
								sSim_Params,
								MH_weight,
								pm_weights,
								sMonitor,
								sNumTreatments,
								sNumClusters, sMaxBs, sNumBodySys,
								sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma, pmu_gamma_0_0, ptau2_gamma_0_0,
								pmu_theta_0_0, ptau2_theta_0_0, palpha_gamma_0_0, pbeta_gamma_0_0,
								palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma, pbeta_gamma, palpha_theta,
								pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0,
								pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
								pPi,
								palpha_pi,
								pbeta_pi,
								plambda_alpha,
								plambda_beta,
								palgo,
								padapt_phase);
			break;

			default:
				model_cluster = new bhpmBB_poisson_mc_hier3_lev0(sChains, sBurnin, sIter, sSim_Type,
								sMem_Model,
								sGlobal_Sim_Params,
								sSim_Params,
								MH_weight,
								pm_weights,
								sMonitor,
								sNumTreatments,
								sNumClusters, sMaxBs, sNumBodySys,
								sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma, pmu_gamma_0_0, ptau2_gamma_0_0,
								pmu_theta_0_0, ptau2_theta_0_0, palpha_gamma_0_0, pbeta_gamma_0_0,
								palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma, pbeta_gamma, palpha_theta,
								pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0,
								pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
								pPi,
								palpha_pi,
								pbeta_pi,
								plambda_alpha,
								plambda_beta,
								palgo,
								padapt_phase);
			break;
		}

		model_cluster->gibbs_sampler();
	}
	catch(...) {
		Rprintf("Unexpected exception bhpm1a_poisson_mc\n");
	}
	return R_NilValue;
}

SEXP bhpmBB_cluster_hier2_exec(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobalSim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sLevel,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi,
					SEXP palpha_pi,
					SEXP pbeta_pi,
					SEXP palgo,
					SEXP padapt_phase)
{
	try {

		if (!model_cluster) {
			delete model_cluster;
			model_cluster = NULL;
		}

		int level = *(INTEGER(sLevel));

		switch(level) {
			case 0:
				model_cluster = new bhpmBB_poisson_mc_hier2_lev0(sChains, sBurnin, sIter, sSim_Type,
								sMem_Model,
								sGlobalSim_Params,
								sSim_Params,
								MH_weight,
								pm_weights,
								sMonitor,
								sNumTreatments,
								sNumClusters, sMaxBs, sNumBodySys,
								sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,
								palpha_gamma, pbeta_gamma, palpha_theta,
								pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0,
								pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
								pPi,
								palpha_pi,
								pbeta_pi,
								palgo,
								padapt_phase);
			break;

			case 1:
				model_cluster = new bhpmBB_poisson_mc_hier2_lev1(sChains, sBurnin, sIter, sSim_Type,
								sMem_Model,
								sGlobalSim_Params,
								sSim_Params,
								MH_weight,
								pm_weights,
								sMonitor,
								sNumTreatments,
								sNumClusters, sMaxBs, sNumBodySys,
								sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,
								palpha_gamma, pbeta_gamma, palpha_theta,
								pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0,
								pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
								pPi,
								palpha_pi,
								pbeta_pi,
								palgo,
								padapt_phase);
			break;

			default:
				model_cluster = new bhpmBB_poisson_mc_hier2_lev0(sChains, sBurnin, sIter, sSim_Type,
								sMem_Model,
								sGlobalSim_Params,
								sSim_Params,
								MH_weight,
								pm_weights,
								sMonitor,
								sNumTreatments,
								sNumClusters, sMaxBs, sNumBodySys,
								sMaxAEs, sNAE, pX, pY, pC, pT, ptheta, pgamma,
								palpha_gamma, pbeta_gamma, palpha_theta,
								pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0,
								pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
								pPi,
								palpha_pi,
								pbeta_pi,
								palgo,
								padapt_phase);
			break;
		}

		model_cluster->gibbs_sampler();
	}
	catch(...) {
		Rprintf("Unexpected exception bhpm1a_poisson_mc\n");
	}
	return R_NilValue;
}

SEXP getPiSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	bhpmBB_poisson_mc_hier2_lev0* modelBB_h2 =
				dynamic_cast<bhpmBB_poisson_mc_hier2_lev0 *>(model_cluster);
	bhpmBB_poisson_mc_hier3_lev0* modelBB_h3 = NULL;

	if (modelBB_h2)
		samples = modelBB_h2->getPiSamples();
	else {
		modelBB_h3 = dynamic_cast<bhpmBB_poisson_mc_hier3_lev0 *>(model_cluster);
		if (modelBB_h3)
			samples = modelBB_h3->getPiSamples();
	}

	return samples;
}

SEXP getAlphaPiSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	bhpmBB_poisson_mc_hier3_lev0* modelBB = dynamic_cast<bhpmBB_poisson_mc_hier3_lev0 *>(model_cluster);

	if (modelBB)
		samples = modelBB->getAlphaPiSamples();

	return samples;
}

SEXP getBetaPiSamplesClusterAll()
{
	SEXP samples = R_NilValue;

	bhpmBB_poisson_mc_hier3_lev0* modelBB = dynamic_cast<bhpmBB_poisson_mc_hier3_lev0 *>(model_cluster);

	if (modelBB)
		samples = modelBB->getBetaPiSamples();

	return samples;
}

SEXP getAlphaPiAcceptClusterAll()
{
	SEXP acc = R_NilValue;

	bhpmBB_poisson_mc_hier3_lev0* modelBB = dynamic_cast<bhpmBB_poisson_mc_hier3_lev0 *>(model_cluster);

	if (modelBB)
		acc = modelBB->getAlphaPiAccept();

	return acc;
}

SEXP getBetaPiAcceptClusterAll()
{
	SEXP acc = R_NilValue;

	bhpmBB_poisson_mc_hier3_lev0* modelBB = dynamic_cast<bhpmBB_poisson_mc_hier3_lev0 *>(model_cluster);

	if (modelBB)
		acc = modelBB->getBetaPiAccept();

	return acc;
}
