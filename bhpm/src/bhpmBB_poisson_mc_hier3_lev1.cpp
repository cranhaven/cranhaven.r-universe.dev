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
#include "bhpmBB_poisson_mc_hier3_lev1.h"

using namespace std;

static const char *rcsId = "$Id: bhpmBB_poisson_mc_hier3_lev1.cpp,v 1.10 2019/05/14 09:29:48 clb13102 Exp clb13102 $";

bhpmBB_poisson_mc_hier3_lev1::bhpmBB_poisson_mc_hier3_lev1()
{
	//Rprintf("bhpmBB_poisson_mc_hier3_lev1::bhpmBB_poisson_mc_hier3_lev1: Default constructor\n");
	mu_theta = NULL;
	mu_gamma = NULL;
	sigma2_theta = NULL;
	sigma2_gamma = NULL;
	gPi = NULL;

	mu_theta_samples = NULL;
	mu_gamma_samples = NULL;
	sigma2_theta_samples = NULL;
	sigma2_gamma_samples = NULL;
	gPi_samples = NULL;
}

bhpmBB_poisson_mc_hier3_lev1::bhpmBB_poisson_mc_hier3_lev1(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type, SEXP sMem_Model,
					SEXP sGlobal_Sim_Params,
					SEXP sSim_Params,
					SEXP MH_weight,
					SEXP pm_weights,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs,
					SEXP sNAE, SEXP pX,
					SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
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
					SEXP palgo, SEXP padapt_phase)

{
	mu_theta = NULL;
	mu_gamma = NULL;
	sigma2_theta = NULL;
	sigma2_gamma = NULL;
	gPi = NULL;

	mu_theta_samples = NULL;
	mu_gamma_samples = NULL;
	gPi_samples = NULL;
	sigma2_theta_samples = NULL;
	sigma2_gamma_samples = NULL;
 
	init(sChains, sBurnin, sIter, sSim_Type, sMem_Model, sGlobal_Sim_Params,
				sSim_Params, MH_weight, pm_weights,
				sMonitor,
				sNumTreatments,
				sNumClusters, sMaxBs, sNumBodySys, sMaxAEs, sNAE,
				pX, pY, pC, pT, ptheta, pgamma, pmu_gamma_0_0, ptau2_gamma_0_0,
				pmu_theta_0_0, ptau2_theta_0_0,
				palpha_gamma_0_0, pbeta_gamma_0_0, palpha_theta_0_0, pbeta_theta_0_0,
				palpha_gamma,
				pbeta_gamma, palpha_theta, pbeta_theta, pmu_gamma_0, ptau2_gamma_0,
				pmu_theta_0,
				ptau2_theta_0, pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta,
				pPi, palpha_pi, pbeta_pi, plambda_alpha, plambda_beta,
				palgo, padapt_phase);
}

void bhpmBB_poisson_mc_hier3_lev1::init(SEXP sChains, SEXP sBurnin, SEXP sIter,
					SEXP sSim_Type,
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

	// Global Simulation parameters
	initGlobalSimParams(sSim_Type, sGlobal_Sim_Params);

	// Individual simulation parameters
	initSimParams(sSim_Params);

	// MH point-mass weights
	gMH_weight = *(REAL(MH_weight));
	initPMWeights(pm_weights);
}

void bhpmBB_poisson_mc_hier3_lev1::initL2Variables(SEXP pmu_gamma, SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta, SEXP pPi)
{
	int c = 0, t = 0, b = 0;

	double* vmu_gamma = REAL(pmu_gamma);
	mu_gamma = new double*[gChains];
	for (c = 0; c < gChains; c++) {
		mu_gamma[c] = new double[gMaxBs];
		for (b = 0; b < gMaxBs; b++) {
			mu_gamma[c][b] = *vmu_gamma;
			vmu_gamma++;
		}
	}

	double* vmu_theta = REAL(pmu_theta);
	mu_theta = new double**[gChains];
	for (c = 0; c < gChains; c++) {
		mu_theta[c] = new double*[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			mu_theta[c][t] = new double[gMaxBs];
			for (b = 0; b < gMaxBs; b++) {
				mu_theta[c][t][b] = *vmu_theta;
				vmu_theta++;
			}
		}
	}

	double* vsigma2_gamma = REAL(psigma2_gamma);
	sigma2_gamma = new double*[gChains];
	for (c = 0; c < gChains; c++) {
		sigma2_gamma[c] = new double[gMaxBs];
		for (b = 0; b < gMaxBs; b++) {
			sigma2_gamma[c][b] = *vsigma2_gamma;
			vsigma2_gamma++;
		}
	}

	double* vsigma2_theta = REAL(psigma2_theta);
	sigma2_theta = new double**[gChains];
	for (c = 0; c < gChains; c++) {
		sigma2_theta[c] = new double*[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			sigma2_theta[c][t] = new double[gMaxBs];
			for (b = 0; b < gMaxBs; b++) {
				sigma2_theta[c][t][b] = *vsigma2_theta;
				vsigma2_theta++;
			}
		}
	}

	double* vpi = REAL(pPi);
	gPi = new double**[gChains];
	for (c = 0; c < gChains; c++) {
		gPi[c] = new double*[gMaxBs];
		for (t = 0; t < gNumComparators; t++) {
			gPi[c][t] = new double[gMaxBs];
			for (b = 0; b < gMaxBs; b++) {
				gPi[c][t][b] = *vpi;
				vpi++;
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev1::releaseL2Variables()
{
	int c = 0, t = 0;

	if (gPi != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] gPi[c][t];
			}
			delete [] gPi[c];
		}
		delete [] gPi;
		gPi = 0;
	}

	if (mu_gamma != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] mu_gamma[c];
		}
		delete [] mu_gamma;
		mu_gamma = 0;
	}

	if (mu_theta != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] mu_theta[c][t];
			}
			delete [] mu_theta[c];
		}
		delete [] mu_theta;
		mu_theta = 0;
	}

	if (sigma2_gamma != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] sigma2_gamma[c];
		}
		delete [] sigma2_gamma;
		sigma2_gamma = 0;
	}

	if (sigma2_theta != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] sigma2_theta[c][t];
			}
			delete [] sigma2_theta[c];
		}
		delete [] sigma2_theta;
		sigma2_theta = 0;
	}
}

void bhpmBB_poisson_mc_hier3_lev1::initL2Samples()
{
	int c = 0, t = 0, l = 0, b = 0;

	l = 0;
	if (retainSamples(iMonitor_mu_gamma))
		mu_gamma_samples = new double **[gChains];
	if (retainSamples(iMonitor_sigma2_gamma))
		sigma2_gamma_samples = new double **[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_mu_gamma))
			mu_gamma_samples[c] = new double *[gMaxBs];
		if (retainSamples(iMonitor_sigma2_gamma))
			sigma2_gamma_samples[c] = new double *[gMaxBs];

		for (b = 0; b < gNumBodySys[l]; b++) {
			if (retainSamples(iMonitor_mu_gamma))
				mu_gamma_samples[c][b] =
									new double [(gIter - gBurnin)];
			if (retainSamples(iMonitor_sigma2_gamma))
				sigma2_gamma_samples[c][b] =
									new double [(gIter - gBurnin)];
		}
	}

	if (retainSamples(iMonitor_mu_theta))
		mu_theta_samples = new double ***[gChains];
	if (retainSamples(iMonitor_sigma2_theta))
		sigma2_theta_samples = new double ***[gChains];
	if (retainSamples(iMonitor_pi))
		gPi_samples = new double ***[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_mu_theta))
			mu_theta_samples[c] = new double **[gMaxBs];
		if (retainSamples(iMonitor_sigma2_theta))
			sigma2_theta_samples[c] = new double **[gMaxBs];
		if (retainSamples(iMonitor_pi))
			gPi_samples[c] = new double **[gMaxBs];

		for (t = 0; t < gNumComparators; t++) {

		if (retainSamples(iMonitor_mu_theta))
			mu_theta_samples[c][t] = new double *[gMaxBs];
		if (retainSamples(iMonitor_sigma2_theta))
			sigma2_theta_samples[c][t] = new double *[gMaxBs];
		if (retainSamples(iMonitor_pi))
			gPi_samples[c][t] = new double *[gMaxBs];

			for (b = 0; b < gNumBodySys[l]; b++) {
				if (retainSamples(iMonitor_mu_theta))
					mu_theta_samples[c][t][b] =
									new double [(gIter - gBurnin)];
				if (retainSamples(iMonitor_sigma2_theta))
					sigma2_theta_samples[c][t][b] =
									new double [(gIter - gBurnin)];
				if (retainSamples(iMonitor_pi))
					gPi_samples[c][t][b] = new double [(gIter - gBurnin)];
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev1::releaseL2Samples()
{
	int c = 0, t = 0, l = 0, b = 0;

	if (gPi_samples) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; b < gNumComparators; b++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] gPi_samples[c][t][b];
				}
				delete [] gPi_samples[c][t];
			}
			delete [] gPi_samples[c];
		}
		delete [] gPi_samples;
		gPi_samples = NULL;
	}

	l = 0;
	if (mu_theta_samples) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; b < gNumComparators; b++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] mu_theta_samples[c][t][b];
				}
				delete [] mu_theta_samples[c][t];
			}
			delete [] mu_theta_samples[c];
		}
		delete [] mu_theta_samples;
		mu_theta_samples = NULL;
	}

	if (mu_gamma_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (b = 0; b < gNumBodySys[l]; b++) {
				delete [] mu_gamma_samples[c][b];
			}
			delete [] mu_gamma_samples[c];
		}
		delete [] mu_gamma_samples;
		mu_gamma_samples = NULL;
	}

	if (sigma2_theta_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; b < gNumComparators; b++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] sigma2_theta_samples[c][t][b];
				}
				delete [] sigma2_theta_samples[c][t];
			}
			delete [] sigma2_theta_samples[c];
		}
		delete [] sigma2_theta_samples;
		sigma2_theta_samples = NULL;
	}

	if (sigma2_gamma_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (b = 0; b < gNumBodySys[l]; b++) {
				delete [] sigma2_gamma_samples[c][b];
			}
			delete [] sigma2_gamma_samples[c];
		}
		delete [] sigma2_gamma_samples;
		sigma2_gamma_samples = NULL;
	}
}

void bhpmBB_poisson_mc_hier3_lev1::clear()
{
	release();
	bhpmBB_poisson_mc_hier3_lev2::release();
	bhpmBB_poisson_mc_hier3_lev0::release();
	bhpm1a_poisson_mc_hier3_lev0::release();
	bhpm1a_poisson_mc_hier2_lev0::release();
}

bhpmBB_poisson_mc_hier3_lev1::~bhpmBB_poisson_mc_hier3_lev1()
{
	//Rprintf("bhpmBB_poisson_mc_hier3_lev1::bhpmBB_poisson_mc_hier3_lev1 - destructor\n");
	release();
}

void bhpmBB_poisson_mc_hier3_lev1::gibbs_sampler()
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

void bhpmBB_poisson_mc_hier3_lev1::simulate_MH()
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

void bhpmBB_poisson_mc_hier3_lev1::simulate_SLICE()
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

void bhpmBB_poisson_mc_hier3_lev1::sample_mu_gamma_0(int burnin, int iter)
{
	int c = 0;

	for (c = 0; c < gChains; c++) {
		double mu_gamma_tot = 0.0;

		int b = 0;
		for (b = 0; b < gNumBodySys[0]; b++) {
			mu_gamma_tot += mu_gamma[c][b];
		}

		double denom = tau2_gamma_0[c] + tau2_gamma_0_0 * ((double)gNumBodySys[0]);

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

void bhpmBB_poisson_mc_hier3_lev1::sample_mu_theta_0(int burnin, int iter, int tr)
{
	int c = 0;


	for (c = 0; c < gChains; c++) {

		double mu_theta_tot = 0.0;

		int b = 0;

		for (b = 0; b < gNumBodySys[0]; b++) {
			mu_theta_tot += mu_theta[c][tr][b];
		}

		double denom = tau2_theta_0[c][tr] + tau2_theta_0_0 * ((double)gNumBodySys[0]);


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

void bhpmBB_poisson_mc_hier3_lev1::sample_tau2_gamma_0(int burnin, int iter)
{
	int c = 0;

	for (c = 0; c < gChains; c++) {
		double isum = 0.0;


		int b = 0;
		for (b = 0; b < gNumBodySys[0]; b++) {
			isum += (pow((mu_gamma[c][b] - mu_gamma_0[c]), 2.0));
		}

		double s = alpha_gamma_0_0 + ((double)gNumBodySys[0])/2.0;
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

void bhpmBB_poisson_mc_hier3_lev1::sample_tau2_theta_0(int burnin, int iter, int tr)
{
	int c = 0;

	for (c = 0; c < gChains; c++) {

		double isum = 0.0;

		int b = 0;
		for (b = 0; b < gNumBodySys[0]; b++) {
			isum += (pow((mu_theta[c][tr][b] - mu_theta_0[c][tr]), 2.0));
		}

		double s = alpha_theta_0_0 + ((double)gNumBodySys[0])/2.0;
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

double bhpmBB_poisson_mc_hier3_lev1::log_f_alpha_pi(int c, double alpha, int tr)
{
	double f = 0.0;

	int b = 0;
	double log_pi_sum = 0.0;
	for (b = 0; b < gNumBodySys[0]; b++) {
		log_pi_sum += log(gPi[c][tr][b]);
	}

	f = f + ((double)gNumBodySys[0]) * (lgammafn(alpha + beta_pi[c][tr]) - lgammafn(alpha));

	f = f + (alpha - 1.0)*log_pi_sum;

	f = f - alpha * lambda_alpha;

	return(f);
}

void bhpmBB_poisson_mc_hier3_lev1::sample_alpha_pi_MH(int burnin, int iter, int tr)
{
	int c = 0;

	for (c = 0; c< gChains; c++) {

		double cand = 0;

		// alpha_pi is restricted to being greater than zero
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

void bhpmBB_poisson_mc_hier3_lev1::sample_alpha_pi_SLICE(int burnin, int iter, int tr)
{
	int c = 0;
	int m = gDefault_W_alpha_control, K = 0, J = 0;

	for (c = 0; c< gChains; c++) {

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

double bhpmBB_poisson_mc_hier3_lev1::log_f_beta_pi(int c, double beta, int tr)
{
	double f = 0.0;

	int b = 0;
	double log_sum = 0.0;
	for (b = 0; b < gNumBodySys[0]; b++) {
		log_sum += log(1 - gPi[c][tr][b]);
	}

	f = f + ((double)gNumBodySys[0]) * (lgammafn(alpha_pi[c][tr] + beta) - lgammafn(beta));

	f = f + (beta - 1.0)*log_sum;

	f = f - beta * lambda_alpha;

	return(f);
}

void bhpmBB_poisson_mc_hier3_lev1::sample_beta_pi_MH(int burnin, int iter, int tr)
{
	int c = 0;

	for (c = 0; c< gChains; c++) {

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

void bhpmBB_poisson_mc_hier3_lev1::sample_beta_pi_SLICE(int burnin, int iter, int tr)
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
			if (l <= 1.0) {
				break;
			}

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

void bhpmBB_poisson_mc_hier3_lev1::sample_pi(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		int b = 0;
		for (b = 0; b < gNumBodySys[0]; b++) {

			int theta_zero_count = 0;
			int nae = 0;
			for (l = 0; l < gNumClusters; l++) {
				int j = 0;
				for (j = 0; j< gNAE[l][b]; j++) {
					if (gTheta[c][tr][l][b][j] == 0.0) {
						theta_zero_count++;
					}
					nae++;
				}
			}

			double shape1 = alpha_pi[c][tr] + (double)theta_zero_count;
			double shape2 = beta_pi[c][tr] + (double)nae - (double)theta_zero_count;
#ifdef INDIVIDUAL_RNG
			GetRNGstate();
#endif
			gPi[c][tr][b] = rbeta(shape1, shape2);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif

			if (iter >= burnin && retainSamples(iMonitor_pi)) {
				gPi_samples[c][tr][b][iter - burnin] = gPi[c][tr][b];
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev1::sample_mu_gamma(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {

		int b = 0;

		for (b = 0; b < gNumBodySys[0]; b++) {

			int nae = 0;
			for (l = 0; l < gNumClusters; l++) {
				nae = nae + gNAE[l][b];
			}

			double denom = sigma2_gamma[c][b] + ((double)nae)*tau2_gamma_0[c];

			double t = 0.0;
			int j = 0;
			for (l = 0; l < gNumClusters; l++) {
				for (j = 0; j < gNAE[l][b]; j++) {
					t += gGamma[c][l][b][j];
				}
			}

			double mean = (sigma2_gamma[c][b] * mu_gamma_0[c] + tau2_gamma_0[c] * t)/denom;

			double var = (sigma2_gamma[c][b]*tau2_gamma_0[c])/denom;

			double sd = sqrt(var);

#ifdef INDIVIDUAL_RNG
			GetRNGstate();
#endif
			double cand = rnorm(mean, sd);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif

			mu_gamma[c][b] = cand;

			if (iter >= burnin && retainSamples(iMonitor_mu_gamma)) {
				mu_gamma_samples[c][b][iter - burnin] = mu_gamma[c][b];
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev1::sample_mu_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {

		int b = 0;

		for (b = 0; b < gNumBodySys[0]; b++) {

			double t = 0.0;
			int j = 0;
			int Kb = 0;
			for (l = 0; l < gNumClusters; l++) {
				for (j = 0; j < gNAE[l][b]; j++) {
					if (gTheta[c][tr][l][b][j] != 0.0) {
						Kb++;
					}
					t += gTheta[c][tr][l][b][j];
				}
			}

			double denom = sigma2_theta[c][tr][b] + ((double)Kb)*tau2_theta_0[c][tr];

			double mean = (sigma2_theta[c][tr][b] * mu_theta_0[c][tr] + tau2_theta_0[c][tr] * t)/denom;

			double var = (sigma2_theta[c][tr][b]*tau2_theta_0[c][tr])/denom;

			double sd = sqrt(var);


#ifdef INDIVIDUAL_RNG
			GetRNGstate();
#endif
			double cand = rnorm(mean, sd);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif

			mu_theta[c][tr][b] = cand;

			if (iter >= burnin && retainSamples(iMonitor_mu_theta)) {
				mu_theta_samples[c][tr][b][iter - burnin] = mu_theta[c][tr][b];
			}

		}
	}
}

void bhpmBB_poisson_mc_hier3_lev1::sample_sigma2_gamma(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {

		int b = 0;
		for (b = 0; b < gNumBodySys[0]; b++) {

			int nae = 0;
			for (l = 0; l < gNumClusters; l++) {
				nae = nae + gNAE[l][b];
			}

			double s = alpha_gamma + ((double)nae)/2.0;

			double t = 0.0;
			int j = 0;
			for (l = 0; l < gNumClusters; l++) {
				for (j = 0; j < gNAE[l][b]; j++) {
					t += (pow(gGamma[c][l][b][j] - mu_gamma[c][b],2.0));
				}
			}


			double r = beta_gamma + t/2.0;


#ifdef INDIVIDUAL_RNG
			GetRNGstate();
#endif
			double cand = rgamma(s, 1/r);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif

			sigma2_gamma[c][b] = 1/cand;


			if (iter >= burnin && retainSamples(iMonitor_sigma2_gamma)) {
				sigma2_gamma_samples[c][b][iter - burnin] = sigma2_gamma[c][b];
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev1::sample_sigma2_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {

		int b = 0;

		for (b = 0; b < gNumBodySys[0]; b++) {

			int nae = 0;
			for (l = 0; l < gNumClusters; l++) {
				nae = nae + gNAE[l][b];
			}


			double t = 0;
			int Kb = 0;
			int j = 0;
			for (l = 0; l < gNumClusters; l++) {
				for (j = 0; j < gNAE[l][b]; j++) {
					if (gTheta[c][tr][l][b][j] != 0.0) {
						Kb++;
						t += (pow((gTheta[c][tr][l][b][j] - mu_theta[c][tr][b]),2.0));
					}
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

			sigma2_theta[c][tr][b] = 1/cand;


			if (iter >= burnin && retainSamples(iMonitor_sigma2_theta)) {
				sigma2_theta_samples[c][tr][b][iter - burnin] = sigma2_theta[c][tr][b];
			}
		}
	}
}

double bhpmBB_poisson_mc_hier3_lev1::log_f_gamma(int c, int i, int b, int j, double gamm)
{
	double f1 = 0.0, f2 = 0.0, f3 = 0.0, f4 = 0.0, f5 = 0.0;
	int t = 0;

	f1 = ((double)x[i][b][j]) * gamm;
	f2 = -(exp(gamm)) * ((double)C[i][b][j]);

	for (t = 0; t < gNumComparators; t++) {
		f3 += ((double)y[t][i][b][j]) * (gamm + gTheta[c][t][i][b][j]);
		f4 += -(exp(gamm + gTheta[c][t][i][b][j]))*((double)T[t][i][b][j]);
	}

	f5 = -(pow((gamm - mu_gamma[c][b]), 2.0))/(2.0 * sigma2_gamma[c][b]);

	double f = f1 + f2 + f3 + f4 + f5;


	return(f);
}

void bhpmBB_poisson_mc_hier3_lev1::sample_gamma_MH(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0, j = 0;


			for (b = 0; b < gNumBodySys[l]; b++) {
				for (j = 0; j < gNAE[l][b]; j++) {


#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					//double cand = rnorm(gGamma[c][l][b][j], gDefault_Sigma_MH_gamma);
					double cand = rnorm(gGamma[c][l][b][j], gSigma_MH_gamma[l][b][j]);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif

#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					double u = runif(0, 1);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif


					double f1 = log_f_gamma(c, l, b, j , cand);
					double f2 = log_f_gamma(c, l, b, j , gGamma[c][l][b][j]);

					double ratio = exp(f1 - f2);


					ratio = cMIN(ratio, 1.0);

					if (u <= ratio) {
						gGamma[c][l][b][j] = cand;
						gGamma_acc[c][l][b][j] = gGamma_acc[c][l][b][j] + 1;
					}


					if (iter >= burnin && retainSamples(iMonitor_gamma)) {
						gGamma_samples[c][l][b][j][iter - burnin] = gGamma[c][l][b][j];
					}
				}
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev1::sample_gamma_SLICE(int burnin, int iter)
{
	int c = 0, i = 0;
	int K = 0, J = 0;


	for (c = 0; c < gChains; c++) {
		for (i = 0; i < gNumClusters; i++) {

			int b = 0, j = 0;
			double cand = 0.0;

			for (b = 0; b < gNumBodySys[i]; b++) {
				for (j = 0; j < gNAE[i][b]; j++) {

					int m = gW_gamma_control[i][b][j];


#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					J = floor(runif(0,m));
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif
					K = (m-1) - J;


					double l = 0.0, r = 0.0;
					double g = log_f_gamma(c, i, b, j, gGamma[c][i][b][j]);
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
					double u = runif(0, gW_gamma[i][b][j]);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif

					l = gGamma[c][i][b][j] - u;
					r = gGamma[c][i][b][j] + (gW_gamma[i][b][j] - u);

					while (J > 0) {
						if (logy >= log_f_gamma(c, i, b, j, l)) {
							break;
						}
						l = l - gW_gamma[i][b][j];
						J--;
					}

					while (K > 0) {
						if (logy >= log_f_gamma(c, i, b, j, r)) {
							break;
						}
						r = r + gW_gamma[i][b][j];
						K--;
					}
		
#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					cand = runif(l, r);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif

					while (logy >= log_f_gamma(c, i, b, j, cand)) {
						if (cand < gGamma[c][i][b][j]) {
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

					gGamma[c][i][b][j] = cand;

					if (iter >= burnin && retainSamples(iMonitor_gamma)) {
						gGamma_samples[c][i][b][j][iter - burnin] = gGamma[c][i][b][j];
					}
				}
			}
		}
	}
}

double bhpmBB_poisson_mc_hier3_lev1::log_q_theta(int l, int b, int j, double p, double theta, double mean, int t)
{
	double f = 0.0;

	if (theta == 0.0) {
		f = log(p);
	}
	else {
		f = log(1 - p) + log((1.0/(gSigma_MH_theta[t][l][b][j] * sqrt(2.0 * M_PI))))  + (-1.0/(2.0*gSigma_MH_theta[t][l][b][j]*gSigma_MH_theta[t][l][b][j])) * pow((theta - mean), 2.0);
	}

	return f;
}


double bhpmBB_poisson_mc_hier3_lev1::log_f_theta(int c, int i, int b, int j, double theta, int tr)
{
	double f1 = 0.0, f2 = 0.0;

	f1 = (((double)y[tr][i][b][j]) * theta) - (exp(gGamma[c][i][b][j] + theta)) * ((double)T[tr][i][b][j]);

	if (theta == 0.0) {
		f2 = log(gPi[c][tr][b]);
	}
	else {
		f2 = log(1 - gPi[c][tr][b]) + log(1.0/sqrt(2.0 * M_PI*sigma2_theta[c][tr][b]))
				+ ((-1.0/2.0)*(pow(theta -mu_theta[c][tr][b], 2.0))/sigma2_theta[c][tr][b]);
	}

	double f = f1 + f2;

	return(f);
}

/*
* Sample theta using a MH step as detailed in: Gottardo, Raftery - Markov Chain Monte Carlo
* With Mixtures of Mutually Singular Distributions
* Perform an adaption step to get a good candidate density
*/
void bhpmBB_poisson_mc_hier3_lev1::sample_theta_MH(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0, j = 0;
			for (b = 0; b < gNumBodySys[l]; b++) {
				for ( j = 0; j < gNAE[l][b]; j++) {

#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					double u = runif(0, 1);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif

					double cand = 0.0;

					if (u < gWp[tr][l][b][j]) {
						cand = 0.0;
					}
					else {
#ifdef INDIVIDUAL_RNG
						GetRNGstate();
#endif
						cand = rnorm(gTheta[c][tr][l][b][j], gSigma_MH_theta[tr][l][b][j]);
#ifdef INDIVIDUAL_RNG
						PutRNGstate();
#endif
					}


					double f_cand = log_f_theta(c, l, b, j, cand, tr);
					double f_prev = log_f_theta(c, l, b, j, gTheta[c][tr][l][b][j], tr);

					double q_cand = log_q_theta(l, b, j, gWp[tr][l][b][j],
														cand, gTheta[c][tr][l][b][j], tr);
					double q_prev = log_q_theta(l, b, j, gWp[tr][l][b][j],
														gTheta[c][tr][l][b][j], cand, tr);

					double lratio = f_cand - f_prev + q_prev - q_cand;

					double ratio = exp(lratio);

#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					u = runif(0, 1);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif
					if (u <= ratio) {
						gTheta[c][tr][l][b][j] = cand;
						gTheta_acc[c][tr][l][b][j] = gTheta_acc[c][tr][l][b][j] + 1;
					}

					if (iter >= burnin && retainSamples(iMonitor_theta)) {
						gTheta_samples[c][tr][l][b][j][iter - burnin] = gTheta[c][tr][l][b][j];
					}
				}
			}
		}
	}
}

double bhpmBB_poisson_mc_hier3_lev1::cMIN(double a, double b)
{
	if (a < b) {
		return a;
	}
	else {
		return b;
	}
}

void bhpmBB_poisson_mc_hier3_lev1::release()
{
	releaseL2Variables();

	releaseL2Samples();
}

SEXP bhpmBB_poisson_mc_hier3_lev1::getL2Samples(double*** &data)
{
	SEXP samples = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(samples = allocVector(REALSXP, gChains * gMaxBs * (gIter - gBurnin)));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int b = 0;
		for (b = 0; b < gMaxBs; b++) {
			memcpy(REAL(samples) + i, data[c][b],
										(gIter - gBurnin)*sizeof(double));
			i += (gIter - gBurnin);
			delete [] data[c][b];
			data[c][b] = NULL;
		}
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 3));

	INTEGER(dim)[0] = (gIter - gBurnin);
	INTEGER(dim)[1] = gMaxBs;
	INTEGER(dim)[2] = gChains;

	setAttrib(samples, R_DimSymbol, dim);

	UNPROTECT(2);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev1::getL2Samples(double**** &data)
{
	SEXP samples = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(samples = allocVector(REALSXP, gChains * gNumComparators * gMaxBs * (gIter - gBurnin)));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int t = 0;
		for (t = 0; t < gNumComparators; t++) {
			int b = 0;
			for (b = 0; b < gMaxBs; b++) {
				memcpy(REAL(samples) + i, data[c][t][b],
											(gIter - gBurnin)*sizeof(double));
				i += (gIter - gBurnin);
				delete [] data[c][t][b];
				data[c][t][b] = NULL;
			}
			delete [] data[c][t];
			data[c][t] = NULL;
		}

		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 4));

	INTEGER(dim)[0] = (gIter - gBurnin);
	INTEGER(dim)[1] = gMaxBs;
	INTEGER(dim)[2] = gNumComparators;
	INTEGER(dim)[3] = gChains;

	setAttrib(samples, R_DimSymbol, dim);

	UNPROTECT(2);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev1::getMuThetaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(mu_theta_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev1::getMuGammaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(mu_gamma_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev1::getSigma2ThetaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(sigma2_theta_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev1::getSigma2GammaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(sigma2_gamma_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev1::getPiSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(gPi_samples);

	return samples;
}

void bhpmBB_poisson_mc_hier3_lev1::getMuThetaSamples(int *c, int *l, int* b, double* mu_theta)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (mu_theta_samples)
		memcpy(mu_theta, mu_theta_samples[C][B], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev1::getMuGammaSamples(int *c, int *l, int* b, double* mu_gamma)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (mu_gamma_samples)
		memcpy(mu_gamma, mu_gamma_samples[C][B], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev1::getSigma2ThetaSamples(int *c, int *l, int* b, double* sigma2)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (sigma2_theta_samples)
		memcpy(sigma2, sigma2_theta_samples[C][B], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev1::getSigma2GammaSamples(int *c, int *l, int* b, double* sigma2)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (sigma2_gamma_samples)
		memcpy(sigma2, sigma2_gamma_samples[C][B], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev1::getPiSamples(int *c, int *l, int* b, double* pi)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (gPi_samples)
		memcpy(pi, gPi_samples[C][B], (gIter - gBurnin)*sizeof(double));
}
