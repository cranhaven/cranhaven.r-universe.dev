#include<cstdio>
#include<cstdlib>
#include <cstring>
#include <cmath>

#include <R.h>
#include <Rmath.h>
#include <R_ext/Print.h>
#include <Rdefines.h>
#include <Rinternals.h>


#include "bhpm1a_poisson_mc_hier2_lev0.h"
#include "bhpm1a_poisson_mc_hier3_lev0.h"
#include "bhpm1a_poisson_mc_hier3_lev2.h"
#include "bhpm1a_poisson_mc_hier3_lev1.h"

using namespace std;

static const char *rcsId = "$Id: bhpm1a_poisson_mc_hier3_lev1.cpp,v 1.9 2019/05/14 09:29:48 clb13102 Exp clb13102 $";

//
// The way the code is set up means we can't allow the body-systems to change from cluster to
// cluster. This matters in sample_mu_gamma etc because if we wished to allow variation in the
// clusters we need to be able to indentify which clusters a body-system belongs to.
// We can't do this because we use an index b to identify the body-systems, not their names.
// So there is no way of identifying body-system index 3 in cluster 1 as being the same or
// different from body-system index 3 in cluster 2. This means that we can't work backwards
// from the clusters to the body-systems to indentify the cluster containing the body-systems.
//

bhpm1a_poisson_mc_hier3_lev1::bhpm1a_poisson_mc_hier3_lev1()
{
	//Rprintf("bhpm1a_poisson_mc_hier3_lev1::bhpm1a_poisson_mc_hier3_lev1: Default constructor\n");

	mu_theta = NULL;
	mu_gamma = NULL;
	sigma2_theta = NULL;
	sigma2_gamma = NULL;


	mu_theta_samples = NULL;
	mu_gamma_samples = NULL;
	sigma2_theta_samples = NULL;
	sigma2_gamma_samples = NULL;
}

bhpm1a_poisson_mc_hier3_lev1::bhpm1a_poisson_mc_hier3_lev1(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE, SEXP pX,
					SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma, SEXP pmu_gamma_0_0,
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0, SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta, SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0, SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta)

{
	mu_theta = NULL;
	mu_gamma = NULL;
	sigma2_theta = NULL;
	sigma2_gamma = NULL;

	mu_theta_samples = NULL;
	mu_gamma_samples = NULL;
	sigma2_theta_samples = NULL;
	sigma2_gamma_samples = NULL;

	init(sChains, sBurnin, sIter, sSim_Type, sMem_Model, sGlobal_Sim_Param,
				sGlobal_Sim_Param_cntrl,
				sSim_Param,
				sMonitor,
				sNumTreatments,
				sNumClusters, sMaxBs, sNumBodySys, sMaxAEs, sNAE,
				pX, pY, pC, pT, ptheta, pgamma, pmu_gamma_0_0, ptau2_gamma_0_0, pmu_theta_0_0, ptau2_theta_0_0,
				palpha_gamma_0_0, pbeta_gamma_0_0, palpha_theta_0_0, pbeta_theta_0_0, palpha_gamma,
				pbeta_gamma, palpha_theta, pbeta_theta, pmu_gamma_0, ptau2_gamma_0, pmu_theta_0,
				ptau2_theta_0, pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta);

}

void bhpm1a_poisson_mc_hier3_lev1::init(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
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
					SEXP ptau2_gamma_0_0, SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0, SEXP palpha_gamma_0_0,
					SEXP pbeta_gamma_0_0, SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0, SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta, SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0, SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta)
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
					palpha_theta, pbeta_theta);

	initL3Variables(pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0);

	initL2Variables(pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta);

	initL3Samples();

	initL2Samples();

	initL1Samples();

	initGlobalSimParams(sSim_Type, sGlobal_Sim_Param, sGlobal_Sim_Param_cntrl);

	// Individual Simulation Parameters
	initSimParams(sSim_Param);
}

void bhpm1a_poisson_mc_hier3_lev1::initL2Variables(SEXP pmu_gamma, SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta)
{
	int c = 0, b = 0, t = 0;

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
}

void bhpm1a_poisson_mc_hier3_lev1::releaseL2Variables()
{
	int c = 0;

	if (mu_gamma != NULL) {
		for (c = 0; c < gChains; c++) {
			delete [] mu_gamma[c];
		}
		delete [] mu_gamma;
		mu_gamma = 0;
	}

	if (mu_theta != NULL) {
		for (c = 0; c < gChains; c++) {
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
			delete [] sigma2_theta[c];
		}
		delete [] sigma2_theta;
		sigma2_theta = 0;
	}
}

void bhpm1a_poisson_mc_hier3_lev1::initL2Samples()
{
	int c = 0, t = 0, l = 0, b = 0;

	if (retainSamples(iMonitor_mu_gamma))
		mu_gamma_samples = new double**[gChains];
	if (retainSamples(iMonitor_sigma2_gamma))
		sigma2_gamma_samples = new double**[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_mu_gamma))
			mu_gamma_samples[c] = new double*[gMaxBs];
		if (retainSamples(iMonitor_sigma2_gamma))
			sigma2_gamma_samples[c] = new double*[gMaxBs];

		for (b = 0; b < gNumBodySys[l]; b++) {
			if (retainSamples(iMonitor_mu_gamma))
				mu_gamma_samples[c][b] =
									new double[(gIter - gBurnin)];
			if (retainSamples(iMonitor_sigma2_gamma))
				sigma2_gamma_samples[c][b] =
									new double[(gIter - gBurnin)];
		}
	}

	if (retainSamples(iMonitor_mu_theta))
		mu_theta_samples = new double***[gChains];
	if (retainSamples(iMonitor_sigma2_theta))
		sigma2_theta_samples = new double***[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_mu_theta))
			mu_theta_samples[c] = new double**[gNumComparators];
		if (retainSamples(iMonitor_sigma2_theta))
			sigma2_theta_samples[c] = new double**[gNumComparators];

		for (t = 0; t < gNumComparators; t++) {

			if (retainSamples(iMonitor_mu_theta))
				mu_theta_samples[c][t] = new double*[gMaxBs];
			if (retainSamples(iMonitor_sigma2_theta))
				sigma2_theta_samples[c][t] = new double*[gMaxBs];

			for (b = 0; b < gNumBodySys[l]; b++) {
				if (retainSamples(iMonitor_mu_theta))
					mu_theta_samples[c][t][b] =
									new double[(gIter - gBurnin)];
				if (retainSamples(iMonitor_sigma2_theta))
					sigma2_theta_samples[c][t][b] =
									new double[(gIter - gBurnin)];
			}
		}
	}
}

void bhpm1a_poisson_mc_hier3_lev1::releaseL2Samples()
{
	int c = 0, b = 0;

	if (mu_theta_samples) {
		for (c = 0; c < gChains; c++) {
			for (b = 0; b < gNumBodySys[0]; b++) {
				delete [] mu_theta_samples[c][b];
			}
			delete [] mu_theta_samples[c];
		}
		delete [] mu_theta_samples;
		mu_theta_samples = NULL;
	}

	if (mu_gamma_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (b = 0; b < gNumBodySys[0]; b++) {
				delete [] mu_gamma_samples[c][b];
			}
			delete [] mu_gamma_samples[c];
		}
		delete [] mu_gamma_samples;
		mu_gamma_samples = NULL;
	}
	if (sigma2_theta_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (b = 0; b < gNumBodySys[0]; b++) {
				delete [] sigma2_theta_samples[c][b];
			}
			delete [] sigma2_theta_samples[c];
		}
		delete [] sigma2_theta_samples;
		sigma2_theta_samples = NULL;
	}
	if (sigma2_gamma_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (b = 0; b < gNumBodySys[0]; b++) {
				delete [] sigma2_gamma_samples[c][b];
			}
			delete [] sigma2_gamma_samples[c];
		}
		delete [] sigma2_gamma_samples;
		sigma2_gamma_samples = NULL;
	}
}

bhpm1a_poisson_mc_hier3_lev1::~bhpm1a_poisson_mc_hier3_lev1()
{
	//Rprintf("bhpm1a_poisson_mc_hier3_lev1::bhpm1a_poisson_mc_hier3_lev1 - destructor\n");
	release();
}

void bhpm1a_poisson_mc_hier3_lev1::gibbs_sampler()
{
	if (strcmp(sim_type, "MH") == 0) {
		simulate_MH();
	}
	else {
		simulate_SLICE();
	}

	return;
}

void bhpm1a_poisson_mc_hier3_lev1::simulate_MH()
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

void bhpm1a_poisson_mc_hier3_lev1::simulate_SLICE()
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
		sample_mu_gamma(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_mu_theta(gBurnin, i, t);
		sample_sigma2_gamma(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_sigma2_theta(gBurnin, i, t);
		sample_gamma_SLICE(gBurnin, i);
		for (t = 0; t < gNumComparators; t++)
			sample_theta_SLICE(gBurnin, i, t);
#ifndef INDIVIDUAL_RNG
		PutRNGstate();
#endif

		if ((i + 1)%1000 == 0) {
			Rprintf("%d iterations...\n", i + 1);
		}
	}
	Rprintf("MCMC fitting complete.\n");
}

void bhpm1a_poisson_mc_hier3_lev1::sample_mu_gamma_0(int burnin, int iter)
{
	int c = 0;

	for (c = 0; c< gChains; c++) {

		double mu_gamma_tot = 0.0;

		int b = 0;
		for (b = 0; b < gNumBodySys[0]; b++) {
			mu_gamma_tot += mu_gamma[c][b];
		}

		double denom = tau2_gamma_0[c] + tau2_gamma_0_0 * (double)gNumBodySys[0];


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

void bhpm1a_poisson_mc_hier3_lev1::sample_mu_theta_0 (int burnin, int iter, int tr)
{
	int c = 0;

	for (c = 0; c< gChains; c++) {

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

void bhpm1a_poisson_mc_hier3_lev1::sample_tau2_gamma_0(int burnin, int iter)
{
	int c = 0;

	for (c = 0; c< gChains; c++) {
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

void bhpm1a_poisson_mc_hier3_lev1::sample_tau2_theta_0(int burnin, int iter, int tr)
{
	int c = 0;

	for (c = 0; c< gChains; c++) {

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

void bhpm1a_poisson_mc_hier3_lev1::sample_mu_gamma(int burnin, int iter)
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

void bhpm1a_poisson_mc_hier3_lev1::sample_mu_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {

		int b = 0;

		for (b = 0; b < gNumBodySys[0]; b++) {
			int nae = 0;

			for (l = 0; l < gNumClusters; l++) {
				nae = nae + gNAE[l][b];
			}
			double denom = sigma2_theta[c][tr][b] + ((double)nae)*tau2_theta_0[c][tr];

			double t = 0.0;
			int j = 0;
			for (l = 0; l < gNumClusters; l++) {
				for (j = 0; j < gNAE[l][b]; j++) {
					t += gTheta[c][tr][l][b][j];
				}
			}

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

void bhpm1a_poisson_mc_hier3_lev1::sample_sigma2_gamma(int burnin, int iter)
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
			for (l = 0; l < gNumClusters; l++) {
				int j = 0;
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

void bhpm1a_poisson_mc_hier3_lev1::sample_sigma2_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {

		int b = 0;

		for (b = 0; b < gNumBodySys[0]; b++) {

			int nae = 0;
			for (l = 0; l < gNumClusters; l++) {
				nae = nae + gNAE[l][b];
			}

			double s = alpha_theta + ((double)nae)/2.0;

			double t = 0;
			int j = 0;
			for (l = 0; l < gNumClusters; l++) {
				for (j = 0; j < gNAE[l][b]; j++) {
					t += (pow((gTheta[c][tr][l][b][j] - mu_theta[c][tr][b]),2.0));
				}
			}

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

double bhpm1a_poisson_mc_hier3_lev1::log_f_gamma(int c, int i, int b, int j, double gamm)
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

void bhpm1a_poisson_mc_hier3_lev1::sample_gamma_MH(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0, j = 0;


			for (b = 0; b < gNumBodySys[0]; b++) {
				for (j = 0; j < gNAE[l][b]; j++) {

#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
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

void bhpm1a_poisson_mc_hier3_lev1::sample_gamma_SLICE(int burnin, int iter)
{
	int c = 0, i = 0;
	int K = 0, J = 0;

	for (c = 0; c < gChains; c++) {
		for (i = 0; i < gNumClusters; i++) {

			int b = 0, j = 0;
			double cand = 0.0;

			for (b = 0; b < gNumBodySys[0]; b++) {
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

double bhpm1a_poisson_mc_hier3_lev1::log_f_theta(int c, int i, int b, int j, double theta, int t)
{
	double f1 = 0.0, f2 = 0.0, f3 = 0.0;

	f1 = ((double)y[t][i][b][j]) * (gGamma[c][i][b][j] + theta);
	f2 = -(exp(gGamma[c][i][b][j] + theta)) * ((double)T[t][i][b][j]);
	f3 = - ((pow(theta - mu_theta[c][t][b], 2.0)))/(2.0 * sigma2_theta[c][t][b]);

	double f = f1 + f2 + f3;

	return(f);
}

void bhpm1a_poisson_mc_hier3_lev1::sample_theta_MH(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0, j = 0;
			for (b = 0; b < gNumBodySys[0]; b++) {
				for ( j = 0; j < gNAE[l][b]; j++) {

#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					double cand = rnorm(gTheta[c][tr][l][b][j], gSigma_MH_theta[tr][l][b][j]);
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

					double f1 = log_f_theta(c, l, b, j, cand, tr);
					double f2 = log_f_theta(c, l, b, j, gTheta[c][tr][l][b][j], tr);

					double ratio = exp(f1 - f2);

					ratio = cMIN(ratio, 1);

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

void bhpm1a_poisson_mc_hier3_lev1::sample_theta_SLICE(int burnin, int iter, int tr)
{
	int c = 0, i = 0;
	int K = 0, J = 0;

	for (c = 0; c < gChains; c++) {
		for (i = 0; i < gNumClusters; i++) {

			int b = 0, j = 0;
			double cand = 0.0;

			for (b = 0; b < gNumBodySys[0]; b++) {
				for (j = 0; j < gNAE[i][b]; j++) {
					int m = gW_theta_control[tr][i][b][j];

#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					J = floor(runif(0,m));
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif
					K = (m-1) - J;

					double l = 0.0, r = 0.0;
					double g = log_f_theta(c, i, b, j, gTheta[c][tr][i][b][j], tr);
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
					double u = runif(0, gW_theta[tr][i][b][j]);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif

					l = gTheta[c][tr][i][b][j] - u;
					r = gTheta[c][tr][i][b][j] + (gW_theta[tr][i][b][j] - u);

					while (J > 0) {
						if (logy >= log_f_theta(c, i, b, j, l, tr)) {
							break;
						}
						l = l - gW_theta[tr][i][b][j];
						J--;
					}

					while (K > 0) {
						if (logy >= log_f_theta(c, i, b, j, r, tr)) {
							break;
						}
						r = r + gW_theta[tr][i][b][j];
						K--;
					}
			
#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					cand = runif(l, r);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif

					while (logy >= log_f_theta(c, i, b, j, cand, tr)) {
						if (cand < gTheta[c][tr][i][b][j]) {
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

					gTheta[c][tr][i][b][j] = cand;

					if (iter >= burnin && retainSamples(iMonitor_theta)) {
						gTheta_samples[c][tr][i][b][j][iter - burnin] = gTheta[c][tr][i][b][j];
					}
				}
			}
		}
	}
}

double bhpm1a_poisson_mc_hier3_lev1::cMIN(double a, double b)
{
	if (a < b) {
		return a;
	}
	else {
		return b;
	}
}

void bhpm1a_poisson_mc_hier3_lev1::clear()
{
	release();
	bhpm1a_poisson_mc_hier3_lev2::release();
	bhpm1a_poisson_mc_hier3_lev0::release();
	bhpm1a_poisson_mc_hier2_lev0::release();
}

void bhpm1a_poisson_mc_hier3_lev1::release()
{
	releaseL2Variables();

	releaseL2Samples();
}

SEXP bhpm1a_poisson_mc_hier3_lev1::getL2Samples(double*** &data)
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

SEXP bhpm1a_poisson_mc_hier3_lev1::getL2Samples(double**** &data)
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

SEXP bhpm1a_poisson_mc_hier3_lev1::getMuThetaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(mu_theta_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier3_lev1::getMuGammaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(mu_gamma_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier3_lev1::getSigma2ThetaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(sigma2_theta_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier3_lev1::getSigma2GammaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(sigma2_gamma_samples);

	return samples;
}

void bhpm1a_poisson_mc_hier3_lev1::getMuThetaSamples(int *c, int *l, int* b, double* mu_theta)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (mu_theta_samples)
		memcpy(mu_theta, mu_theta_samples[C][B], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier3_lev1::getMuGammaSamples(int *c, int *l, int* b, double* mu_gamma)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (mu_gamma_samples)
		memcpy(mu_gamma, mu_gamma_samples[C][B], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier3_lev1::getSigma2ThetaSamples(int *c, int *l, int* b, double* sigma2)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (sigma2_theta_samples)
		memcpy(sigma2, sigma2_theta_samples[C][B], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier3_lev1::getSigma2GammaSamples(int *c, int *l, int* b, double* sigma2)
{
	int C = (*c) - 1;
	int B = (*b) - 1;

	if (sigma2_gamma_samples)
		memcpy(sigma2, sigma2_gamma_samples[C][B], (gIter - gBurnin)*sizeof(double));
}
