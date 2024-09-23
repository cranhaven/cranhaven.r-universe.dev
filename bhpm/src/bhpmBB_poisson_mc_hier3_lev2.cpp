#include<cstdio>
#include<cstdlib>

#include<cstring>
#include<cmath>

#include <R.h>
#include <Rmath.h>
#include <R_ext/Print.h>
#include <Rdefines.h>
#include <Rinternals.h>


#include "bhpm1a_poisson_mc_hier2_lev0.h"
#include "bhpm1a_poisson_mc_hier3_lev0.h"
#include "bhpmBB_poisson_mc_hier3_lev0.h"
#include "bhpmBB_poisson_mc_hier3_lev2.h"

using namespace std;

static const char *rcsId = "$Id: bhpmBB_poisson_mc_hier3_lev2.cpp,v 1.11 2019/05/14 09:29:48 clb13102 Exp clb13102 $";

bhpmBB_poisson_mc_hier3_lev2::bhpmBB_poisson_mc_hier3_lev2()
{
	//Rprintf("bhpmBB_poisson_mc_hier3_lev2::bhpmBB_poisson_mc_hier3_lev2: Default constructor\n");

	mu_theta_0 = NULL;
	mu_gamma_0 = NULL;
	tau2_theta_0 = NULL;
	tau2_gamma_0 = NULL;
	alpha_pi = NULL;
	beta_pi = NULL;

	alpha_pi_acc = NULL;
	beta_pi_acc = NULL;

	mu_theta_0_samples = NULL;
	mu_gamma_0_samples = NULL;
	tau2_theta_0_samples = NULL;
	tau2_gamma_0_samples = NULL;
	alpha_pi_samples = NULL;
	beta_pi_samples = NULL;

	gW_alpha = NULL;
	gW_beta = NULL;
	gW_alpha_control = NULL;
	gW_beta_control = NULL;
	gSigma_MH_alpha = NULL;
	gSigma_MH_beta = NULL;
}

bhpmBB_poisson_mc_hier3_lev2::bhpmBB_poisson_mc_hier3_lev2(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model, SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE, SEXP pX,
					SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma, SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0, SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta, SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0, SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi, SEXP palpha_pi, SEXP pbeta_pi, SEXP plambda_alpha, SEXP plambda_beta,
					SEXP palgo, SEXP padapt_phase)

{
	mu_theta_0 = NULL;
	mu_gamma_0 = NULL;
	tau2_theta_0 = NULL;
	tau2_gamma_0 = NULL;
	alpha_pi = NULL;
	beta_pi = NULL;

	alpha_pi_acc = NULL;
	beta_pi_acc = NULL;

	mu_theta_0_samples = NULL;
	mu_gamma_0_samples = NULL;
	tau2_theta_0_samples = NULL;
	tau2_gamma_0_samples = NULL;
	alpha_pi_samples = NULL;
	beta_pi_samples = NULL;

	gW_alpha = NULL;
	gW_beta = NULL;
	gW_alpha_control = NULL;
	gW_beta_control = NULL;
	gSigma_MH_alpha = NULL;
	gSigma_MH_beta = NULL;

	init(sChains, sBurnin, sIter, sSim_Type, sMem_Model, sGlobal_Sim_Params,
				sSim_Params, MH_weight, pm_weights,
				sMonitor,
				sNumTreatments,
				sNumClusters, sMaxBs, sNumBodySys, sMaxAEs, sNAE,
				pX, pY, pC, pT, ptheta, pgamma, pmu_gamma_0_0, ptau2_gamma_0_0, pmu_theta_0_0, ptau2_theta_0_0,
				palpha_gamma_0_0, pbeta_gamma_0_0, palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma,
				pbeta_gamma, palpha_theta, pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0,
				ptau2_theta_0, pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
				pPi, palpha_pi, pbeta_pi, plambda_alpha, plambda_beta,
				palgo, padapt_phase);
}

void bhpmBB_poisson_mc_hier3_lev2::init(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type, SEXP sMem_Model, SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters,
					SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE,
					SEXP pX, SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0, SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta, SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0, SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta,
					SEXP pPi, SEXP palpha_pi, SEXP pbeta_pi, SEXP plambda_alpha, SEXP plambda_beta,
					SEXP palgo, SEXP padapt_phase)

{
	clear();

	initMonitor(sMonitor);

	initBaselineVariables(sChains, sBurnin, sIter,
				sMem_Model, sNumTreatments, sNumClusters, sMaxBs, sNumBodySys, sMaxAEs, sNAE);

	initDataVariables(pX, pY, pC, pT);

	initL1Variables(ptheta, pgamma);

	initL3Params(pmu_gamma_0_0, ptau2_gamma_0_0,
					pmu_theta_0_0, ptau2_theta_0_0,
					palpha_gamma_0_0, pbeta_gamma_0_0,
					palpha_theta_0_0, pbeta_theta_0_0,
					palpha_gamma, pbeta_gamma,
					palpha_theta, pbeta_theta,
					plambda_alpha, plambda_beta);

	initL3Variables(pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0, palpha_pi, pbeta_pi);

	initL2Variables(pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta, pPi);

	initL3Samples();

	initL2Samples();

	initL1Samples();

	// Global simulation parameters
	initGlobalSimParams(sSim_Type, sGlobal_Sim_Params);

	// Individual simulation parameters
	initSimParams(sSim_Params);

	// MH point-mass weights
	gMH_weight = *(REAL(MH_weight));
	initPMWeights(pm_weights);
}

void bhpmBB_poisson_mc_hier3_lev2::initSimParams(SEXP sSim_Params)
{
	gW_gamma = new double**[gNumClusters];
	gW_gamma_control = new int**[gNumClusters];
	gSigma_MH_gamma = new double**[gNumClusters];
	gSigma_MH_theta = new double ***[gNumComparators];

	gW_alpha = new double[gNumComparators];
	gW_beta = new double[gNumComparators];
	gW_alpha_control = new double[gNumComparators];
	gW_beta_control = new double[gNumComparators];
	gSigma_MH_alpha = new double[gNumComparators];
	gSigma_MH_beta = new double[gNumComparators];

	int i = 0, t = 0, b = 0, j = 0;

	for (i = 0; i < gNumClusters; i++) {

		gW_gamma[i] = new double*[gNumBodySys[i]];
		gW_gamma_control[i] = new int*[gNumBodySys[i]];
		gSigma_MH_gamma[i] = new double*[gNumBodySys[i]];

		for (b = 0; b < gNumBodySys[i]; b++) {

			gW_gamma[i][b] = new double[gNAE[i][b]];
			gW_gamma_control[i][b] = new int[gNAE[i][b]];
			gSigma_MH_gamma[i][b] = new double[gNAE[i][b]];

			for (j = 0; j < gNAE[i][b]; j++) {
				gW_gamma[i][b][j] = gDefault_W_gamma;
				gW_gamma_control[i][b][j] = (int)gDefault_W_gamma_control;
				gSigma_MH_gamma[i][b][j] = gDefault_Sigma_MH_gamma;
			}
		}
	}

	for (t = 0; t < gNumComparators; t++) {
		gSigma_MH_theta[t] = new double**[gNumClusters];

		gW_alpha[t] = gDefault_W_alpha;
		gW_beta[t] = gDefault_W_alpha;
		gW_alpha_control[t] = gDefault_W_alpha_control;
		gW_beta_control[t] = gDefault_W_beta_control;
		gSigma_MH_alpha[t] = gDefault_Sigma_MH_alpha;
		gSigma_MH_beta[t] = gDefault_Sigma_MH_beta;

		for (i = 0; i < gNumClusters; i++) {
			gSigma_MH_theta[t][i] = new double*[gNumBodySys[i]];

			for (b = 0; b < gNumBodySys[i]; b++) {
				gSigma_MH_theta[t][i][b] = new double[gNAE[i][b]];

				for (j = 0; j < gNAE[i][b]; j++) {
					gSigma_MH_theta[t][i][b][j] = gDefault_Sigma_MH_theta;
				}
			}
		}
	}

	int len = Rf_length(sSim_Params);

	if (len && isNewList(sSim_Params)) {

		SEXP sVariables = R_NilValue;
		SEXP sParams = R_NilValue;
		SEXP sValues = R_NilValue;
		SEXP sControl = R_NilValue;
		SEXP sB = R_NilValue;
		SEXP sj = R_NilValue;
		SEXP sC_index = R_NilValue;
		SEXP sGroup = R_NilValue;

		SEXP names = getAttrib(sSim_Params, R_NamesSymbol);

		for (i = 0; i < len; i++) {
			if (strcmp(sColValue, CHAR(STRING_ELT(names, i))) == 0) {
				sValues = VECTOR_ELT(sSim_Params, i);
			}
			if (strcmp(sColParam, CHAR(STRING_ELT(names, i))) == 0) {
				sParams = VECTOR_ELT(sSim_Params, i);
			}
			if (strcmp(sColControl, CHAR(STRING_ELT(names, i))) == 0) {
				sControl = VECTOR_ELT(sSim_Params, i);
			}
			if (strcmp(sColVariable, CHAR(STRING_ELT(names, i))) == 0) {
				sVariables = VECTOR_ELT(sSim_Params, i);
			}
			if (strcmp(sColB, CHAR(STRING_ELT(names, i))) == 0) {
				sB = VECTOR_ELT(sSim_Params, i);
			}
			if (strcmp(sColj, CHAR(STRING_ELT(names, i))) == 0) {
				sj = VECTOR_ELT(sSim_Params, i);
			}
			if (strcmp(sColGroup, CHAR(STRING_ELT(names, i))) == 0) {
				sGroup = VECTOR_ELT(sSim_Params, i);
			}
			if (strcmp(sColC_index, CHAR(STRING_ELT(names, i))) == 0) {
				sC_index = VECTOR_ELT(sSim_Params, i);
			}
		}

		len = Rf_length(sParams);
		
		if (len > 0) {
			double* vals = REAL(sValues);
			double* cntrl = REAL(sControl);
			int* B = INTEGER(sB);
			int* j = INTEGER(sj);
			int* c_index = INTEGER(sC_index);
			int* group = INTEGER(sGroup);

			for (i = 0; i < len; i++) {
				const char *var = CHAR(STRING_ELT(sVariables, i));
				const char *param = CHAR(STRING_ELT(sParams, i));

				int l = c_index[i] - 1;
				int b = B[i] - 1;
				int a = j[i] - 1;
				int t = group[i] - 1;
				if (0 == strcmp(sVariable_gamma, var)) {
					if (0 == strcmp(param, sParam_w_gamma)) {
						gW_gamma[l][b][a] = vals[i];
						gW_gamma_control[l][b][a] = (int)cntrl[i];
					}
					else if (0 == strcmp(param, sParam_sigma_MH_gamma)) {
						gSigma_MH_gamma[l][b][a] = vals[i];
					}
				}
				else if (0 == strcmp(sVariable_theta, var)) {
					if (0 == strcmp(param, sParam_w_theta)) {
						//gW_theta[l][b][a] = vals[i];
						//gW_theta_control[l][b][a] = (int)cntrl[i];
					}
					else if (0 == strcmp(param, sParam_sigma_MH_theta)) {
						gSigma_MH_theta[t][l][b][a] = vals[i];
					}
				}
				else if (0 == strcmp(sVariable_alpha, var)) {
					if (0 == strcmp(param, sParam_w_alpha)) {
						gW_alpha[t] = vals[i];
						gW_alpha_control[t] = (int)cntrl[i];
					}
					else if (0 == strcmp(param, sParam_sigma_MH_alpha)) {
						gSigma_MH_alpha[t] = vals[i];
					}
				}
				else if (0 == strcmp(sVariable_beta, var)) {
					if (0 == strcmp(param, sParam_w_beta)) {
						gW_beta[t] = vals[i];
						gW_beta_control[t] = (int)cntrl[i];
					}
					else if (0 == strcmp(param, sParam_sigma_MH_beta)) {
						gSigma_MH_beta[t] = vals[i];
					}
				}
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::initL3Samples()
{
	int c = 0, t = 0;

	// The samples
	if (retainSamples(iMonitor_mu_gamma_0))
		mu_gamma_0_samples = new double *[gChains];
	if (retainSamples(iMonitor_tau2_gamma_0))
		tau2_gamma_0_samples = new double *[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_mu_gamma_0))
			mu_gamma_0_samples[c] = new double [(gIter - gBurnin)];
		if (retainSamples(iMonitor_tau2_gamma_0))
			tau2_gamma_0_samples[c] =
									new double [(gIter - gBurnin)];
	}

	if (retainSamples(iMonitor_mu_theta_0))
		mu_theta_0_samples = new double **[gChains];
	if (retainSamples(iMonitor_tau2_theta_0))
		tau2_theta_0_samples = new double **[gChains];
	if (retainSamples(iMonitor_alpha_pi))
		alpha_pi_samples = new double **[gChains];
	if (retainSamples(iMonitor_beta_pi))
		beta_pi_samples = new double **[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_mu_theta_0))
			mu_theta_0_samples[c] = new double *[gNumComparators];
		if (retainSamples(iMonitor_tau2_theta_0))
			tau2_theta_0_samples[c] =
									new double *[gNumComparators];
		if (retainSamples(iMonitor_alpha_pi))
			alpha_pi_samples[c] =
									new double *[gNumComparators];
		if (retainSamples(iMonitor_beta_pi))
			beta_pi_samples[c] =
									new double *[gNumComparators];

		for (t = 0; t < gNumComparators; t++) {
			if (retainSamples(iMonitor_mu_theta_0))
				mu_theta_0_samples[c][t] = new double [(gIter - gBurnin)];
			if (retainSamples(iMonitor_tau2_theta_0))
				tau2_theta_0_samples[c][t] =
									new double [(gIter - gBurnin)];
			if (retainSamples(iMonitor_alpha_pi))
				alpha_pi_samples[c][t] =
									new double [(gIter - gBurnin)];
			if (retainSamples(iMonitor_beta_pi))
				beta_pi_samples[c][t] =
									new double [(gIter - gBurnin)];
		}
	}

	alpha_pi_acc = new int*[gChains];
	beta_pi_acc = new int*[gChains];
	for (c = 0; c < gChains; c++) {
		alpha_pi_acc[c] = new int[gNumComparators];
		beta_pi_acc[c] = new int[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			alpha_pi_acc[c][t] = 0;
			beta_pi_acc[c][t] = 0;
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::clear()
{
	release();
	bhpmBB_poisson_mc_hier3_lev0::release();
	bhpm1a_poisson_mc_hier3_lev0::release();
	bhpm1a_poisson_mc_hier2_lev0::release();
}

void bhpmBB_poisson_mc_hier3_lev2::initL3Variables(SEXP pmu_gamma_0, SEXP ptau2_gamma_0,
					SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP palpha_pi, SEXP pbeta_pi)
{
	int c = 0, t = 0;

	mu_gamma_0 = new double[gChains];
	double *vmu_gamma_0 = REAL(pmu_gamma_0);
	for (c = 0; c < gChains; c++) {
		mu_gamma_0[c] = *vmu_gamma_0;
		vmu_gamma_0++;
	}

	mu_theta_0 = new double*[gChains];
	double *vmu_theta_0 = REAL(pmu_theta_0);
	for (c = 0; c < gChains; c++) {
		mu_theta_0[c] = new double[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			mu_theta_0[c][t] = *vmu_theta_0;
			vmu_theta_0++;
		}
	}

	tau2_gamma_0 = new double[gChains];
	double *vtau2_gamma_0 = REAL(ptau2_gamma_0);
	for (c = 0; c < gChains; c++) {
		tau2_gamma_0[c] = *vtau2_gamma_0;
		vtau2_gamma_0++;
	}

	tau2_theta_0 = new double*[gChains];
	double *vtau2_theta_0 = REAL(ptau2_theta_0);
	for (c = 0; c < gChains; c++) {
		tau2_theta_0[c] = new double[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			tau2_theta_0[c][t] = *vtau2_theta_0;
			vtau2_theta_0++;
		}
	}

	alpha_pi = new double*[gChains];
	double *valpha_pi = REAL(palpha_pi);
	for (c = 0; c < gChains; c++) {
		alpha_pi[c] = new double[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			alpha_pi[c][t] = *valpha_pi;
			valpha_pi++;
		}
	}

	beta_pi = new double*[gChains];
	double *vbeta_pi = REAL(pbeta_pi);
	for (c = 0; c < gChains; c++) {
		beta_pi[c] = new double[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			beta_pi[c][t] = *vbeta_pi;
			vbeta_pi++;
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::releaseL3Variables()
{
	int c = 0;

	if (alpha_pi != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] alpha_pi[c];
		}
		delete [] alpha_pi;
		alpha_pi = NULL;
	}

	if (beta_pi != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] beta_pi[c];
		}
		beta_pi = NULL;
	}

	if (mu_theta_0 != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] mu_theta_0[c];
		}
		delete [] mu_theta_0;
		mu_theta_0 = NULL;
	}

	if (mu_gamma_0 != NULL) {
		delete [] mu_gamma_0;
		mu_gamma_0 = NULL;
	}

	if (tau2_theta_0 != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] tau2_theta_0[c];
		}
		delete [] tau2_theta_0;
		tau2_theta_0 = NULL;
	}

	if (tau2_gamma_0 != NULL) {
		delete [] tau2_gamma_0;
		tau2_gamma_0 = NULL;
	}
}

bhpmBB_poisson_mc_hier3_lev2::~bhpmBB_poisson_mc_hier3_lev2()
{
	//Rprintf("bhpmBB_poisson_mc_hier3_lev2::bhpmBB_poisson_mc_hier3_lev2 - destructor\n");
	release();
}

void bhpmBB_poisson_mc_hier3_lev2::gibbs_sampler()
{
	switch(gSimType) {
		case eSim_Type_MH:
			simulate_MH();
		break;

		case eSim_Type_SLICE:
			simulate_SLICE();
		break;

		default:
			simulate_MH();
		break;
	}
		
	return;
}

void bhpmBB_poisson_mc_hier3_lev2::simulate_MH()
{
	int i = 0, t = 0;

	for (i = 0; i < gIter; i++) {
#ifndef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		sample_mu_gamma_0(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_mu_theta_0(gBurnin, i, t);
		sample_tau2_gamma_0(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_tau2_theta_0(gBurnin, i, t);

		for (t = 0; t < gNumComparators; t++)
			sample_alpha_pi_MH(gBurnin, i, t);
		for (t = 0; t < gNumComparators; t++)
			sample_beta_pi_MH(gBurnin, i, t);
		for (t = 0; t < gNumComparators; t++)
			sample_pi(gBurnin, i, t);

		sample_mu_gamma(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_mu_theta(gBurnin, i, t);
		sample_sigma2_gamma(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_sigma2_theta(gBurnin, i, t);
		sample_gamma_MH(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_theta_MH(gBurnin, i, t);
#ifndef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		if ((i + 1)%1000 == 0) {
			Rprintf("%d iterations...\n", i + 1);
		}
	}
	Rprintf("MCMC fitting complete.\n");
}

void bhpmBB_poisson_mc_hier3_lev2::simulate_SLICE()
{
	int i = 0, t = 0;

	for (i = 0; i < gIter; i++) {
#ifndef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		sample_mu_gamma_0(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_mu_theta_0(gBurnin, i, t);
		sample_tau2_gamma_0(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_tau2_theta_0(gBurnin, i, t);

		for (t = 0; t < gNumComparators; t++)
			sample_alpha_pi_SLICE(gBurnin, i, t);
		for (t = 0; t < gNumComparators; t++)
			sample_beta_pi_SLICE(gBurnin, i, t);
		for (t = 0; t < gNumComparators; t++)
			sample_pi(gBurnin, i, t);

		sample_mu_gamma(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_mu_theta(gBurnin, i, t);
		sample_sigma2_gamma(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_sigma2_theta(gBurnin, i, t);
		sample_gamma_SLICE(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_theta_MH(gBurnin, i, t);
#ifndef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		if ((i + 1)%1000 == 0) {
			Rprintf("%d iterations...\n", i + 1);
		}
	}
	Rprintf("MCMC fitting complete.\n");
}

void bhpmBB_poisson_mc_hier3_lev2::sample_mu_gamma_0(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		int sum_B = 0;
		double mu_gamma_tot = 0.0;

		for (l = 0; l < gNumClusters; l++) {
			sum_B = sum_B + gNumBodySys[l];

			int b = 0;
			for (b = 0; b < gNumBodySys[l]; b++) {
				mu_gamma_tot += mu_gamma[c][l][b];
			}
		}

		double denom = tau2_gamma_0[c] + tau2_gamma_0_0 * ((double)sum_B);

		double mean = (tau2_gamma_0[c] * mu_gamma_0_0 + tau2_gamma_0_0 * mu_gamma_tot)/denom;
		double var = (tau2_gamma_0[c] * tau2_gamma_0_0) / denom;

		double sd = sqrt(var);
#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		mu_gamma_0[c] = rnorm(mean, sd);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		if (iter >= burnin && retainSamples(iMonitor_mu_gamma_0)) {
			mu_gamma_0_samples[c][iter - burnin] = mu_gamma_0[c];
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_mu_theta_0 (int burnin, int iter, int tr)
{
	int c = 0, l = 0;


	for (c = 0; c< gChains; c++) {

		int sum_B = 0;
		double mu_theta_tot = 0.0;

		for (l = 0; l < gNumClusters; l++) {
			sum_B = sum_B + gNumBodySys[l];

			int b = 0;

			for (b = 0; b < gNumBodySys[l]; b++) {
				mu_theta_tot += mu_theta[c][tr][l][b];
			}
		}

		double denom = tau2_theta_0[c][tr] + tau2_theta_0_0 * ((double)sum_B);


		double mean = (tau2_theta_0[c][tr] * mu_theta_0_0 + tau2_theta_0_0 * mu_theta_tot)/denom;
		double var = (tau2_theta_0[c][tr] * tau2_theta_0_0) / denom;

		double sd = sqrt(var);

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		mu_theta_0[c][tr] = rnorm(mean, sd);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		if (iter >= burnin && retainSamples(iMonitor_mu_theta_0)) {
			mu_theta_0_samples[c][tr][iter - burnin] = mu_theta_0[c][tr];
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_tau2_gamma_0(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c< gChains; c++) {
		int sum_B = 0;
		double isum = 0.0;
		for (l = 0; l < gNumClusters; l++) {
			sum_B = sum_B + gNumBodySys[l];

			int b = 0;
			for (b = 0; b < gNumBodySys[l]; b++) {
				isum += (pow((mu_gamma[c][l][b] - mu_gamma_0[c]), 2.0));
			}
		}

		double s = alpha_gamma_0_0 + ((double)sum_B)/2.0;
		double r = 0.0;

		r = beta_gamma_0_0 + 0.5*isum;


		// In the C API the gamma distribution is defined to be shape/scale rather than shape/rate
#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		double cand = rgamma(s, 1/r);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		tau2_gamma_0[c] = 1/cand;


		if (iter >= burnin && retainSamples(iMonitor_tau2_gamma_0)) {
			tau2_gamma_0_samples[c][iter - burnin] = tau2_gamma_0[c];
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_tau2_theta_0(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c< gChains; c++) {
		int sum_B = 0;
		double isum = 0.0;
		for (l = 0; l < gNumClusters; l++) {
			sum_B = sum_B + gNumBodySys[l];

			int b = 0;
			for (b = 0; b < gNumBodySys[l]; b++) {
				isum += (pow((mu_theta[c][tr][l][b] - mu_theta_0[c][tr]), 2.0));
			}
		}

		double s = alpha_theta_0_0 + ((double)sum_B)/2.0;
		double r = 0.0;


		r = beta_theta_0_0 + 0.5 * isum;

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		double cand = rgamma(s, 1/r);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		tau2_theta_0[c][tr] = 1/cand;


		if (iter >= burnin && retainSamples(iMonitor_tau2_theta_0)) {
			tau2_theta_0_samples[c][tr][iter - burnin] = tau2_theta_0[c][tr];
		}
	}
}

double bhpmBB_poisson_mc_hier3_lev2::log_f_alpha_pi(int c, double alpha, int tr)
{
	double f = 0.0;
	int l = 0;

	for (l = 0; l < gNumClusters; l++) {
		int b = 0;
		double log_pi_sum = 0.0;
		for (b = 0; b < gNumBodySys[l]; b++) {
			log_pi_sum += log(gPi[c][tr][l][b]);
		}

		f = f + ((double)gNumBodySys[l]) * (lgammafn(alpha + beta_pi[c][tr]) - lgammafn(alpha));

		f = f + (alpha - 1.0)*log_pi_sum;
	}

	f = f - alpha * lambda_alpha;

	return(f);
}

void bhpmBB_poisson_mc_hier3_lev2::sample_alpha_pi_MH(int burnin, int iter, int tr)
{
	int c = 0;

	for (c = 0; c< gChains; c++) {

		double cand = 0;

		// alpha_pi is restricted to being greater than zero
		// This is rejection sampling of a normal distribution truncated at 1.
		// See bhpmBB.cpp
	    while (cand <= 1.0) {
#ifdef INDIVIDUAL_RNG
	        GetRNGstate();
#endif
	        cand = rnorm(alpha_pi[c][tr], gDefault_Sigma_MH_alpha);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif
		}

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		double u = runif(0, 1);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		double f1 = log_f_alpha_pi(c, cand, tr);
		double f2 = log_f_alpha_pi(c, alpha_pi[c][tr], tr);

		double q1 = pnorm((alpha_pi[c][tr] - 1)/gDefault_Sigma_MH_alpha, 0, 1, 1, 0);
		double q2 = pnorm((cand - 1)/gDefault_Sigma_MH_alpha, 0, 1, 1, 0);

		double ratio = (exp(f1 - f2)) * q1/q2;
		ratio = cMIN(ratio, 1);

	    if (u <= ratio) {
	        alpha_pi[c][tr] = cand;
			alpha_pi_acc[c][tr] = alpha_pi_acc[c][tr] + 1;
		}

		if (iter >= burnin && retainSamples(iMonitor_alpha_pi)) {
			alpha_pi_samples[c][tr][iter - burnin] = alpha_pi[c][tr];
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_alpha_pi_SLICE(int burnin, int iter, int tr)
{
	int c = 0;
	int m = gDefault_W_alpha_control, K = 0, J = 0;

	for (c = 0; c < gChains; c++) {

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		J = floor(runif(0,m));
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif
	    K = (m-1) - J;

		double cand = 0.0;
		double l = 0.0, r = 0.0;

		double g = log_f_alpha_pi(c, alpha_pi[c][tr], tr);
		double logy = 0.0;

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		double e = rexp(1);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif
		logy = g - e;

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		double u = runif(0, gDefault_W_alpha);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		l = alpha_pi[c][tr] - u;
		r = alpha_pi[c][tr] + (gDefault_W_alpha - u);

		while (J > 0) {
			if (l <= 1.0)
				break;

			if (logy >= log_f_alpha_pi(c, l, tr)) {
				break;
			}
			l = l - gDefault_W_alpha;

			J--;
		}

		while (K > 0) {
			if (logy >= log_f_alpha_pi(c, r, tr)) {
				break;
			}
			r = r + gDefault_W_alpha;
			K--;
		}

		if (l <= 1.0) {
			l = 1.0;
		}

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		cand = runif(l, r);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		while (logy >= log_f_alpha_pi(c, cand, tr)) {
			if (cand < alpha_pi[c][tr]) {
				l = cand;
			}
			else {
				r = cand;
			}
#ifdef INDIVIDUAL_RNG
			GetRNGstate();
#endif
			cand = runif(l, r);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif
		}

		alpha_pi[c][tr] = cand;

		if (iter >= burnin && retainSamples(iMonitor_alpha_pi)) {
			alpha_pi_samples[c][tr][iter - burnin] = alpha_pi[c][tr];
		}
	}
}

double bhpmBB_poisson_mc_hier3_lev2::log_f_beta_pi(int c, double beta, int tr)
{
	double f = 0.0;
	int l = 0;

	for (l = 0; l < gNumClusters; l++) {
		int b = 0;
		double log_sum = 0.0;
		for (b = 0; b < gNumBodySys[l]; b++) {
			log_sum += log(1 - gPi[c][tr][l][b]);
		}

		f = f + ((double)gNumBodySys[l]) * (lgammafn(alpha_pi[c][tr] + beta) - lgammafn(beta));

		f = f + (beta - 1.0)*log_sum;
	}

	f = f - beta * lambda_alpha;

	return(f);
}

void bhpmBB_poisson_mc_hier3_lev2::sample_beta_pi_MH(int burnin, int iter, int tr)
{
	int c = 0;

	for (c = 0; c < gChains; c++) {

		double cand = 0.0;

		while (cand <= 1.0) {
#ifdef INDIVIDUAL_RNG
			GetRNGstate();
#endif
			cand = rnorm(beta_pi[c][tr], gDefault_Sigma_MH_beta);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif
		}

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		double u = runif(0, 1);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		double f1 = log_f_beta_pi(c, cand, tr);
		double f2 = log_f_beta_pi(c, beta_pi[c][tr], tr);

		double q1 = pnorm((beta_pi[c][tr] - 1)/gDefault_Sigma_MH_beta, 0, 1, 1, 0);
		double q2 = pnorm((cand - 1)/gDefault_Sigma_MH_beta, 0, 1, 1, 0);

		double ratio = (exp(f1 - f2)) * (q1/q2);

		ratio = cMIN(ratio, 1);

		if (u <= ratio) {
			beta_pi[c][tr] = cand;
			beta_pi_acc[c][tr] = beta_pi_acc[c][tr] + 1;
		}

		if (iter >= burnin && retainSamples(iMonitor_beta_pi)) {
			beta_pi_samples[c][tr][iter - burnin] = beta_pi[c][tr];
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_beta_pi_SLICE(int burnin, int iter, int tr)
{
	int c = 0;
	int m = gDefault_W_beta_control, K = 0, J = 0;

	for (c = 0; c < gChains; c++) {

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		J = floor(runif(0,m));
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif
		K = (m-1) - J;

		double l = 0.0, r = 0.0;
		double cand = 0.0;

		double g = log_f_beta_pi(c, beta_pi[c][tr], tr);
		double logy = 0.0;

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		double e = rexp(1);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif
		logy = g - e;

#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		double u = runif(0, gDefault_W_beta);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		l = beta_pi[c][tr] - u;
		r = beta_pi[c][tr] + (gDefault_W_beta - u);


		// beta is retricted to being greater than 1
		// need a do - while loop
		while (J > 0) {
			if (l <= 1.0)
				break;

			if (logy >= log_f_beta_pi(c, l, tr)) {
				break;
			}
			l = l - gDefault_W_beta;
			J--;
		}

		while (K > 0) {
			if (logy >= log_f_beta_pi(c, r, tr)) {
				break;
			}
			r = r + gDefault_W_beta;
			K--;
		}

		if (l <= 1.0) {
			l = 1.0;
		}


#ifdef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		cand = runif(l, r);
#ifdef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		while (logy >= log_f_beta_pi(c, cand, tr)) {
			if (cand < beta_pi[c][tr]) {
				l = cand;
			}
			else {
				r = cand;
			}
#ifdef INDIVIDUAL_RNG
			GetRNGstate();
#endif
			cand = runif(l, r);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif
		}

		beta_pi[c][tr] = cand;

		if (iter >= burnin && retainSamples(iMonitor_beta_pi)) {
			beta_pi_samples[c][tr][iter - burnin] = beta_pi[c][tr];
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_pi(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;
			for (b = 0; b < gNumBodySys[l]; b++) {
				int theta_zero_count = 0;

				int j = 0;
				for (j = 0; j< gNAE[l][b]; j++) {
					if (gTheta[c][tr][l][b][j] == 0.0) {
						theta_zero_count++;
					}
				}

				double shape1 = alpha_pi[c][tr] + (double)theta_zero_count;
				double shape2 = beta_pi[c][tr] + (double)gNAE[l][b] - (double)theta_zero_count;

#ifdef INDIVIDUAL_RNG
				GetRNGstate();
#endif
				gPi[c][tr][l][b] = rbeta(shape1, shape2);
#ifdef INDIVIDUAL_RNG
				PutRNGstate();
#endif

				if (iter >= burnin && retainSamples(iMonitor_pi)) {
					gPi_samples[c][tr][l][b][iter - burnin] = gPi[c][tr][l][b];
				}
			}
		}

	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_mu_gamma(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;

			for (b = 0; b < gNumBodySys[l]; b++) {
				double denom = sigma2_gamma[c][l][b] + ((double)gNAE[l][b])*tau2_gamma_0[c];


				double t = 0.0;
				int j = 0;
				for (j = 0; j < gNAE[l][b]; j++) {
					t += gGamma[c][l][b][j];
				}

				double mean = (sigma2_gamma[c][l][b] * mu_gamma_0[c] + tau2_gamma_0[c] * t)/denom;

				double var = (sigma2_gamma[c][l][b]*tau2_gamma_0[c])/denom;

				double sd = sqrt(var);

#ifdef INDIVIDUAL_RNG
				GetRNGstate();
#endif
				double cand = rnorm(mean, sd);
#ifdef INDIVIDUAL_RNG
				PutRNGstate();
#endif

				mu_gamma[c][l][b] = cand;

				if (iter >= burnin && retainSamples(iMonitor_mu_gamma)) {
					mu_gamma_samples[c][l][b][iter - burnin] = mu_gamma[c][l][b];
				}
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_mu_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;

			for (b = 0; b < gNumBodySys[l]; b++) {

				double t = 0.0;
				int Kb = 0;
				int j = 0;
				for (j = 0; j < gNAE[l][b]; j++) {
					if (gTheta[c][tr][l][b][j] != 0.0) {
						Kb++;
					}
					t += gTheta[c][tr][l][b][j];
				}

				double denom = sigma2_theta[c][tr][l][b] + ((double)Kb)*tau2_theta_0[c][tr];

				double mean = (sigma2_theta[c][tr][l][b] * mu_theta_0[c][tr] + tau2_theta_0[c][tr] * t)/denom;

				double var = (sigma2_theta[c][tr][l][b]*tau2_theta_0[c][tr])/denom;

				double sd = sqrt(var);

#ifdef INDIVIDUAL_RNG
				GetRNGstate();
#endif
				double cand = rnorm(mean, sd);
#ifdef INDIVIDUAL_RNG
				PutRNGstate();
#endif

				mu_theta[c][tr][l][b] = cand;


				if (iter >= burnin && retainSamples(iMonitor_mu_theta)) {
					mu_theta_samples[c][tr][l][b][iter - burnin] = mu_theta[c][tr][l][b];
				}

			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_sigma2_gamma(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;
			for (b = 0; b < gNumBodySys[l]; b++) {

				double s = alpha_gamma + ((double)gNAE[l][b])/2.0;


				double t = 0.0;
				int j = 0;
				for (j = 0; j < gNAE[l][b]; j++) {
					t += (pow(gGamma[c][l][b][j] - mu_gamma[c][l][b],2.0));
				}


				double r = beta_gamma + t/2.0;


#ifdef INDIVIDUAL_RNG
				GetRNGstate();
#endif
				double cand = rgamma(s, 1/r);
#ifdef INDIVIDUAL_RNG
				PutRNGstate();
#endif

				sigma2_gamma[c][l][b] = 1/cand;


				if (iter >= burnin && retainSamples(iMonitor_sigma2_gamma)) {
					sigma2_gamma_samples[c][l][b][iter - burnin] = sigma2_gamma[c][l][b];
				}
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev2::sample_sigma2_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;

			for (b = 0; b < gNumBodySys[l]; b++) {


				double t = 0;
				int j = 0;
				int Kb = 0;
				for (j = 0; j < gNAE[l][b]; j++) {
					if (gTheta[c][tr][l][b][j] != 0.0) {
						Kb++;
						t += (pow((gTheta[c][tr][l][b][j] - mu_theta[c][tr][l][b]),2.0));
					}
				}

				double s = alpha_theta + ((double)Kb)/2.0;
				double r = beta_theta + t/2.0;

#ifdef INDIVIDUAL_RNG
				GetRNGstate();
#endif
				double cand = rgamma(s, 1/r);
#ifdef INDIVIDUAL_RNG
				PutRNGstate();
#endif

				sigma2_theta[c][tr][l][b] = 1/cand;

				if (iter >= burnin && retainSamples(iMonitor_sigma2_theta)) {
					sigma2_theta_samples[c][tr][l][b][iter - burnin] = sigma2_theta[c][tr][l][b];
				}
			}
		}
	}
}

double bhpmBB_poisson_mc_hier3_lev2::cMIN(double a, double b)
{
	if (a < b) {
		return a;
	}
	else {
		return b;
	}
}

void bhpmBB_poisson_mc_hier3_lev2::releaseL3Samples()
{
	int c = 0, t = 0;

	if (alpha_pi_acc != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] alpha_pi_acc[c];
		}
		delete [] alpha_pi_acc;
		alpha_pi_acc = NULL;
	}

	if (beta_pi_acc != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] beta_pi_acc[c];
		}
		delete [] beta_pi_acc;
		beta_pi_acc = NULL;
	}

	if (alpha_pi_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] alpha_pi_samples[c][t];
			}
			delete [] alpha_pi_samples[c];
		}
		delete [] alpha_pi_samples;
		alpha_pi_samples = NULL;
	}

	if (beta_pi_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] beta_pi_samples[c][t];
			}
			delete [] beta_pi_samples[c];
		}
		delete [] beta_pi_samples;
		beta_pi_samples = NULL;
	}

	if (mu_gamma_0_samples != NULL) {
		for (c = 0; c < gChains; c++) {
            free(mu_gamma_0_samples[c]);
		}
		free(mu_gamma_0_samples);
		mu_gamma_0_samples = NULL;
	}

	if (mu_theta_0_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
            	free(mu_theta_0_samples[c][t]);
			}
            free(mu_theta_0_samples[c]);
		}
		free(mu_theta_0_samples);
		mu_theta_0_samples = NULL;
	}

	if (tau2_gamma_0_samples != NULL) {
		for (c = 0; c < gChains; c++) {
            free(tau2_gamma_0_samples[c]);
		}
		free(tau2_gamma_0_samples);
		tau2_gamma_0_samples = NULL;
	}

	if (tau2_theta_0_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				free(tau2_theta_0_samples[c][t]);
			}
            free(tau2_theta_0_samples[c]);
		}
		free(tau2_theta_0_samples);
		tau2_theta_0_samples = NULL;
	}
}

void bhpmBB_poisson_mc_hier3_lev2::release()
{
	releaseL3Variables();

	releaseL3Samples();

	if (gW_alpha != NULL) {
		delete [] gW_alpha;
		gW_alpha = NULL;
	}
    if (gW_beta != NULL) {
		delete [] gW_beta;
		gW_beta = NULL;
	}
    if (gW_alpha_control != NULL) {
		delete [] gW_alpha_control;
		gW_alpha_control = NULL;
	}
    if (gW_beta_control != NULL) {
		delete [] gW_beta_control;
		gW_beta_control = NULL;
	}

    if (gSigma_MH_alpha != NULL) {
		delete [] gSigma_MH_alpha;
		gSigma_MH_alpha = NULL;
	}
    if (gSigma_MH_beta != NULL) {
		delete [] gSigma_MH_beta;
		gSigma_MH_beta = NULL;
	}
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getL3Samples(double** &data)
{
	SEXP samples = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(samples = allocVector(REALSXP, gChains * (gIter - gBurnin)));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		memcpy(REAL(samples) + i, data[c], (gIter - gBurnin)*sizeof(double));

		i += (gIter - gBurnin);
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 2));

	INTEGER(dim)[0] = (gIter - gBurnin);
	INTEGER(dim)[1] = gChains;

	setAttrib(samples, R_DimSymbol, dim);

	UNPROTECT(2);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getL3Samples(double*** &data)
{
	SEXP samples = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(samples = allocVector(REALSXP, gChains * gNumComparators * (gIter - gBurnin)));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int t = 0;
		for (t = 0; t < gNumComparators; t++) {
			memcpy(REAL(samples) + i, data[c][t], (gIter - gBurnin)*sizeof(double));

			i += (gIter - gBurnin);
			delete [] data[c][t];
		}
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 3));

	INTEGER(dim)[0] = (gIter - gBurnin);
	INTEGER(dim)[1] = gNumComparators;
	INTEGER(dim)[2] = gChains;

	setAttrib(samples, R_DimSymbol, dim);

	UNPROTECT(2);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getMuGamma0Samples()
{
	SEXP samples = R_NilValue;

	samples = getL3Samples(mu_gamma_0_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getMuTheta0Samples()
{
	SEXP samples = R_NilValue;

	samples = getL3Samples(mu_theta_0_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getTau2Gamma0Samples()
{
	SEXP samples = R_NilValue;

	samples = getL3Samples(tau2_gamma_0_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getTau2Theta0Samples()
{
	SEXP samples = R_NilValue;

	samples = getL3Samples(tau2_theta_0_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getAlphaPiSamples()
{
	SEXP samples = R_NilValue;

	samples = getL3Samples(alpha_pi_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getBetaPiSamples()
{
	SEXP samples = R_NilValue;

	samples = getL3Samples(beta_pi_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getL3Accept(int* &data)
{
	SEXP acc = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(acc = allocVector(INTSXP, gChains));
	memcpy(INTEGER(acc), data, gChains*sizeof(int));

	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 1));

	INTEGER(dim)[0] = gChains;
	setAttrib(acc, R_DimSymbol, dim);

	UNPROTECT(2);

	return acc;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getL3Accept(int** &data)
{
	SEXP acc = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(acc = allocVector(INTSXP, gChains * gNumComparators));
	int c = 0;
	for (c = 0; c < gChains; c++) {
		memcpy(INTEGER(acc), data, gNumComparators*sizeof(int));
		delete [] data[c];
	}

	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 2));

	INTEGER(dim)[0] = gChains;
	INTEGER(dim)[1] = gNumComparators;
	setAttrib(acc, R_DimSymbol, dim);

	UNPROTECT(2);

	return acc;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getAlphaPiAccept()
{
	SEXP acc = R_NilValue;

	acc = getL3Accept(alpha_pi_acc);

	return acc;
}

SEXP bhpmBB_poisson_mc_hier3_lev2::getBetaPiAccept()
{
	SEXP acc = R_NilValue;

	acc = getL3Accept(beta_pi_acc);

	return acc;
}

void bhpmBB_poisson_mc_hier3_lev2::getMuGamma0Samples(int *c, int *l, double* mu)
{
	int C = (*c) - 1;

	if (mu_gamma_0_samples)
		memcpy(mu, mu_gamma_0_samples[C], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev2::getMuTheta0Samples(int *c, int *l, double* mu)
{
	int C = (*c) - 1;

	if (mu_theta_0_samples)
		memcpy(mu, mu_theta_0_samples[C], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev2::getTau2Gamma0Samples(int *c, int *l, double* tau2)
{
	int C = (*c) - 1;

	if (tau2_gamma_0_samples)
		memcpy(tau2, tau2_gamma_0_samples[C], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev2::getTau2Theta0Samples(int *c, int *l, double* tau2)
{
	int C = (*c) - 1;

	if (tau2_theta_0_samples)
		memcpy(tau2, tau2_theta_0_samples[C], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev2::getAlphaPiSamples(int *c, int *l, double* alpha_pi)
{
	int C = (*c) - 1;

	if (alpha_pi_samples)
		memcpy(alpha_pi, alpha_pi_samples[C], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev2::getBetaPiSamples(int *c, int *l, double* beta_pi)
{
	int C = (*c) - 1;

	if (beta_pi_samples)
		memcpy(beta_pi, beta_pi_samples[C], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev2::getAlphaPiAccept(int *c, int *l, int* t, double* acc)
{
	int C = (*c) - 1;
	int T = (*t) - 1;

	*acc = alpha_pi_acc[C][T];
}

void bhpmBB_poisson_mc_hier3_lev2::getBetaPiAccept(int *c, int* l, int* t,  double* acc)
{
	int C = (*c) - 1;
	int T = (*t) - 1;

	*acc = beta_pi_acc[C][T];
}
