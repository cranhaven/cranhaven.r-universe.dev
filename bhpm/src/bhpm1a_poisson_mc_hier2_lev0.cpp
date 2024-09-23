#include<cstdio>
#include<cstdlib>
#include <cstring>
#include<cmath>

#include <R.h>
#include <Rmath.h>
#include <R_ext/Print.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "bhpm1a_poisson_mc_hier2_lev0.h"

using namespace std;

static const char *rcsId = "$Id: bhpm1a_poisson_mc_hier2_lev0.cpp,v 1.12 2019/06/02 16:31:46 clb13102 Exp clb13102 $";

const char* bhpm1a_poisson_mc_hier2_lev0::sColType = "type";
const char* bhpm1a_poisson_mc_hier2_lev0::sColVariable = "variable";
const char* bhpm1a_poisson_mc_hier2_lev0::sColParam = "param";
const char* bhpm1a_poisson_mc_hier2_lev0::sColValue = "value";
const char* bhpm1a_poisson_mc_hier2_lev0::sColControl = "control";
const char* bhpm1a_poisson_mc_hier2_lev0::sColB = "B";
const char* bhpm1a_poisson_mc_hier2_lev0::sColj = "j";
const char* bhpm1a_poisson_mc_hier2_lev0::sColGroup = "Trt.Grp";
const char* bhpm1a_poisson_mc_hier2_lev0::sColC_index = "C_index";

const char* bhpm1a_poisson_mc_hier2_lev0::sParam_w = "w";
const char* bhpm1a_poisson_mc_hier2_lev0::sParam_sigma_MH = "sigma_MH";
const char* bhpm1a_poisson_mc_hier2_lev0::sVariable_gamma = "gamma";
const char* bhpm1a_poisson_mc_hier2_lev0::sVariable_theta = "theta";

const char* bhpm1a_poisson_mc_hier2_lev0::sColMonitorVariables = "variable";
const char* bhpm1a_poisson_mc_hier2_lev0::sColMonitorValues = "monitor";

const char* bhpm1a_poisson_mc_hier2_lev0::sMonitor_theta = "theta";
const char* bhpm1a_poisson_mc_hier2_lev0::sMonitor_gamma = "gamma";
const char* bhpm1a_poisson_mc_hier2_lev0::sMonitor_mu_theta = "mu.theta";
const char* bhpm1a_poisson_mc_hier2_lev0::sMonitor_mu_gamma = "mu.gamma";
const char* bhpm1a_poisson_mc_hier2_lev0::sMonitor_sigma2_theta = "sigma2.theta";
const char* bhpm1a_poisson_mc_hier2_lev0::sMonitor_sigma2_gamma = "sigma2.gamma";

bhpm1a_poisson_mc_hier2_lev0::bhpm1a_poisson_mc_hier2_lev0()
{
	//Rprintf("bhpm1a_poisson_mc_hier2_lev0::bhpm1a_poisson_mc_hier2_lev0: Default constructor\n");
	gChains = 0;
	gBurnin = 0;
	gIter = 0;
	gMaxBs = 0;
	sim_type = NULL;
	gNAE = NULL;
	gNumTreatments = 0;
	gNumComparators = 0;
	gNumClusters = 0;
	gNumBodySys = NULL;
	gMaxAEs = 0;
	gSim_Param = 0.0;
	gSim_Param_cntrl = 0.0;
	sim_type = NULL;

 	gW_gamma = NULL;
	gW_theta = NULL;
	gW_gamma_control = NULL;
	gW_theta_control = NULL;
	gSigma_MH_gamma = NULL;
	gSigma_MH_theta = NULL;

	eMemory_Model = HIGH;
	iMonitor_theta = 0;
	iMonitor_gamma = 0;
	iMonitor_mu_theta = 0;
	iMonitor_mu_gamma = 0;
	iMonitor_sigma2_theta = 0;
	iMonitor_sigma2_gamma = 0;

	mu_theta_0 = 0.0;
	mu_gamma_0 = 0.0;
	tau2_theta_0 = 0.0;
	tau2_gamma_0 = 0.0;
	alpha_gamma = 0.0;
	beta_gamma = 0.0;
	alpha_theta = 0.0;
	beta_theta = 0.0;

	mu_theta = NULL;
	mu_gamma = NULL;
	sigma2_theta = NULL;
	sigma2_gamma = NULL;

	gTheta = NULL;
	gGamma = NULL;
	gTheta_acc = NULL;
	gGamma_acc = NULL;

	x = NULL;
	y = NULL;
	C = NULL;
	T = NULL;

	gTheta_samples = NULL;
	gGamma_samples = NULL;
	mu_theta_samples = NULL;
	mu_gamma_samples = NULL;
	sigma2_theta_samples = NULL;
	sigma2_gamma_samples = NULL;
}

bhpm1a_poisson_mc_hier2_lev0::bhpm1a_poisson_mc_hier2_lev0(SEXP sChains, SEXP sBurnin,
					SEXP sIter,
					SEXP sSim_Type,
					SEXP sMem_Model,
					SEXP sGlobal_Sim_Param,
					SEXP sGlobal_Sim_Param_cntrl,
					SEXP sSim_Param,
					SEXP sMonitor,
					SEXP sNumTreatments,
					SEXP sNumClusters, SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs,
					SEXP sNAE, SEXP pX,
					SEXP pY, SEXP pC, SEXP pT, SEXP ptheta, SEXP pgamma,
					SEXP pmu_gamma_0,
					SEXP ptau2_gamma_0, SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP palpha_gamma,
					SEXP pbeta_gamma, SEXP palpha_theta, SEXP pbeta_theta,
					SEXP pmu_gamma,
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta)
{
	gChains = 0;
	gBurnin = 0;
	gIter = 0;
	sim_type = NULL;
	gNAE = NULL;
	gNumTreatments = 0;
	gNumComparators = 0;
	gNumClusters = 0;
	gMaxBs = 0;
	gNumBodySys = NULL;
	gMaxAEs = 0;
	gSim_Param = 0.0;
	gSim_Param_cntrl = 0.0;
	sim_type = NULL;

 	gW_gamma = NULL;
	gW_theta = NULL;
	gW_gamma_control = NULL;
	gW_theta_control = NULL;
	gSigma_MH_gamma = NULL;
	gSigma_MH_theta = NULL;

	eMemory_Model = HIGH;
	iMonitor_theta = 0;
	iMonitor_gamma = 0;
	iMonitor_mu_theta = 0;
	iMonitor_mu_gamma = 0;
	iMonitor_sigma2_theta = 0;
	iMonitor_sigma2_gamma = 0;

	mu_theta_0 = 0.0;
	mu_gamma_0 = 0.0;
	tau2_theta_0 = 0.0;
	tau2_gamma_0 = 0.0;
	alpha_gamma = 0.0;
	beta_gamma = 0.0;
	alpha_theta = 0.0;
	beta_theta = 0.0;

	mu_theta = NULL;
	mu_gamma = NULL;
	sigma2_theta = NULL;
	sigma2_gamma = NULL;

	gTheta = NULL;
	gGamma = NULL;
	gTheta_acc = NULL;
	gGamma_acc = NULL;

	x = NULL;
	y = NULL;
	C = NULL;
	T = NULL;

	gTheta_samples = NULL;
	gGamma_samples = NULL;
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
				pX, pY, pC, pT, ptheta, pgamma, pmu_gamma_0, ptau2_gamma_0,
				pmu_theta_0, ptau2_theta_0,
				palpha_gamma, pbeta_gamma, palpha_theta, pbeta_theta,
				pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta);

}

void bhpm1a_poisson_mc_hier2_lev0::init(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
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
					SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta)
{
	release();

	initMonitor(sMonitor);

	initBaselineVariables(sChains, sBurnin, sIter,
				sMem_Model, sNumTreatments, sNumClusters, sMaxBs, sNumBodySys, sMaxAEs, sNAE);

	initDataVariables(pX, pY, pC, pT);

	initL1Variables(ptheta, pgamma);

	initL2Params(pmu_gamma_0, ptau2_gamma_0, pmu_theta_0, ptau2_theta_0,
					palpha_gamma, pbeta_gamma, palpha_theta, pbeta_theta);

	initL2Variables(pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta);

	initL2Samples();

	initL1Samples();

	initGlobalSimParams(sSim_Type, sGlobal_Sim_Param, sGlobal_Sim_Param_cntrl);

	// Individual Simulation Parameters
	initSimParams(sSim_Param);

}

void bhpm1a_poisson_mc_hier2_lev0::initBaselineVariables(SEXP sChains, SEXP sBurnin, SEXP sIter,
                    SEXP sMem_Model, SEXP sNumTreatments, SEXP sNumClusters, SEXP sMaxBs, SEXP sNumBodySys, SEXP sMaxAEs, SEXP sNAE)
{
	int l = 0, b = 0;

	gChains = *(INTEGER(sChains));
	gBurnin = *(INTEGER(sBurnin));
	gIter = *(INTEGER(sIter));
	gNumTreatments = *(INTEGER(sNumTreatments));
	gNumComparators = gNumTreatments - 1;
	gNumClusters = *(INTEGER(sNumClusters));
	gMaxBs = *(INTEGER(sMaxBs));

	// Body-system Data
	gNumBodySys = new int[gNumClusters];
	for (l = 0; l < gNumClusters; l++) {
		gNumBodySys[l] = (INTEGER(sNumBodySys))[l];
	}

	// AE data
	gMaxAEs = *(INTEGER(sMaxAEs));
	gNAE = new int*[gNumClusters];
	for (l = 0; l < gNumClusters; l++) {
		gNAE[l] =  new int[gMaxBs];
	}

	int indx = 0;
	for (l = 0; l < gNumClusters; l++) {
		for (b = 0; b < gMaxBs; b++) {
			gNAE[l][b] = (INTEGER(sNAE))[indx++];
		}
	}

	l = strlen(CHAR(STRING_ELT(sMem_Model, 0)));
	char *mem_model = new char[l+1];
	strcpy(mem_model, CHAR(STRING_ELT(sMem_Model, 0)));
	mem_model[l] = 0;

	Rprintf("Memory Model: %s\n", mem_model);

	if (0 == strcmp("LOW", mem_model)) {
		eMemory_Model = LOW;
	}
	else {
		eMemory_Model = HIGH;
	}

	delete [] mem_model;
	mem_model = NULL;
}

void bhpm1a_poisson_mc_hier2_lev0::releaseBaselineVariables()
{
	int l = 0;

	delete [] gNumBodySys;
	gNumBodySys = NULL;

	if (gNAE != NULL) {
		for (l = 0; l < gNumClusters; l++) {
			delete [] gNAE[l];
		}
		delete [] gNAE;
		gNAE = NULL;
	}
}

void bhpm1a_poisson_mc_hier2_lev0::initDataVariables(SEXP pX, SEXP pY, SEXP pC, SEXP pT)
{
	int t = 0, l = 0, b = 0, a = 0;

	x = new int**[gNumClusters];
	C = new double**[gNumClusters];

	for (l = 0; l < gNumClusters; l++) {
		x[l] = new int*[gMaxBs];
		C[l] = new double*[gMaxBs];
		for (b = 0; b < gMaxBs; b++) {
			x[l][b] = new int[gMaxAEs];
			C[l][b] = new double[gMaxAEs];
		}
	}

	y = new int***[gNumComparators];
	T = new double***[gNumComparators];

	for (t = 0; t < gNumComparators; t++) {
		y[t] = new int**[gNumClusters];
		T[t] = new double**[gNumClusters];
		for (l = 0; l < gNumClusters; l++) {
			y[t][l] = new int*[gMaxBs];
			T[t][l] = new double*[gMaxBs];
			for (b = 0; b < gMaxBs; b++) {
				y[t][l][b] = new int[gMaxAEs];
				T[t][l][b] = new double[gMaxAEs];
			}
		}
	}

	int *vX = INTEGER(pX);
	double *vC = REAL(pC);
	for (l = 0; l < gNumClusters; l++) {
		int b = 0;
		for (b = 0; b < gMaxBs; b++) {
			for (a = 0; a < gMaxAEs; a++) {
				x[l][b][a] = *vX;
				C[l][b][a] = *vC;
				vX++;
				vC++;
			}
		}
	}

	int *vY = INTEGER(pY);
	double *vT = REAL(pT);

	// Multiple comparator treatments
	for (t = 0; t < gNumComparators; t++) {
		for (l = 0; l < gNumClusters; l++) {
			int b = 0;
			for (b = 0; b < gMaxBs; b++) {
				for (a = 0; a < gMaxAEs; a++) {
					y[t][l][b][a] = *vY;
					T[t][l][b][a] = *vT;
					vY++;
					vT++;
				}
			}
		}
	}
}

void bhpm1a_poisson_mc_hier2_lev0::initL2Params(SEXP pmu_gamma_0, SEXP ptau2_gamma_0,
										SEXP pmu_theta_0, SEXP ptau2_theta_0,
										SEXP palpha_gamma, SEXP pbeta_gamma,
										SEXP palpha_theta, SEXP pbeta_theta)
{
	mu_gamma_0 = *(REAL(pmu_gamma_0));
	tau2_gamma_0 = *(REAL(ptau2_gamma_0));
	mu_theta_0 = *(REAL(pmu_theta_0));
	tau2_theta_0 = *(REAL(ptau2_theta_0));
	alpha_gamma = *(REAL(palpha_gamma));
	beta_gamma = *(REAL(pbeta_gamma));
	alpha_theta = *(REAL(palpha_theta));
	beta_theta = *(REAL(pbeta_theta));
}

void bhpm1a_poisson_mc_hier2_lev0::initL2Variables(SEXP pmu_gamma, SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta)
{
	int c = 0, l = 0, b = 0, t = 0;

	double* vmu_gamma = REAL(pmu_gamma);
	mu_gamma = new double**[gChains];
	for (c = 0; c < gChains; c++) {
		mu_gamma[c] = new double*[gNumClusters];
		for (l = 0; l < gNumClusters; l++) {
			mu_gamma[c][l] = new double[gMaxBs];
			for (b = 0; b < gMaxBs; b++) {
				mu_gamma[c][l][b] = *vmu_gamma;
				vmu_gamma++;
			}
		}
	}

	double* vmu_theta = REAL(pmu_theta);
	mu_theta = new double***[gChains];
	for (c = 0; c < gChains; c++) {
		mu_theta[c] = new double**[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			mu_theta[c][t] = new double*[gNumClusters];
			for (l = 0; l < gNumClusters; l++) {
				mu_theta[c][t][l] = new double[gMaxBs];
				for (b = 0; b < gMaxBs; b++) {
					mu_theta[c][t][l][b] = *vmu_theta;
					vmu_theta++;
				}
			}
		}
	}

	double* vsigma2_gamma = REAL(psigma2_gamma);
	sigma2_gamma = new double**[gChains];
	for (c = 0; c < gChains; c++) {
		sigma2_gamma[c] = new double*[gNumClusters];
		for (l = 0; l < gNumClusters; l++) {
			sigma2_gamma[c][l] = new double[gMaxBs];
			for (b = 0; b < gMaxBs; b++) {
				sigma2_gamma[c][l][b] = *vsigma2_gamma;
				vsigma2_gamma++;
			}
		}
	}

	double* vsigma2_theta = REAL(psigma2_theta);
	sigma2_theta = new double***[gChains];
	for (c = 0; c < gChains; c++) {
		sigma2_theta[c] = new double**[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			sigma2_theta[c][t] = new double*[gNumClusters];
			for (l = 0; l < gNumClusters; l++) {
				sigma2_theta[c][t][l] = new double[gMaxBs];
				for (b = 0; b < gMaxBs; b++) {
					sigma2_theta[c][t][l][b] = *vsigma2_theta;
					vsigma2_theta++;
				}
			}
		}
	}
}

void bhpm1a_poisson_mc_hier2_lev0::clear()
{
	release();
}

void bhpm1a_poisson_mc_hier2_lev0::releaseL1Samples()
{
	int c = 0, t = 0, l = 0, b = 0, a = 0;

	if (gTheta_samples) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				for (l = 0; l < gNumClusters; l++) {
					for (b = 0; b < gNumBodySys[l]; b++) {
						for (a = 0; a < gNAE[l][b]; a++) {
							delete [] gTheta_samples[c][t][l][b][a];
						}
						delete [] gTheta_samples[c][t][l][b];
					}
					delete [] gTheta_samples[c][t][l];
				}
				delete [] gTheta_samples[c][t];
			}
			delete [] gTheta_samples[c];
		}
		delete [] gTheta_samples;
		gTheta_samples = NULL;
	}

	if (gGamma_samples) {
		for (c = 0; c < gChains; c++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					for (a = 0; a < gNAE[l][b]; a++) {
						delete [] gGamma_samples[c][l][b][a];
					}
					delete [] gGamma_samples[c][l][b];
				}
				delete [] gGamma_samples[c][l];
			}
			delete [] gGamma_samples[c];
		}
		delete [] gGamma_samples;
		gGamma_samples = NULL;
	}

	if (gTheta_acc != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				for (l = 0; l < gNumClusters; l++) {
					for (b = 0; b < gMaxBs; b++) {
						delete [] gTheta_acc[c][t][l][b];
					}
					delete [] gTheta_acc[c][t][l];
				}
				delete [] gTheta_acc[c][t];
			}
			delete [] gTheta_acc[c];
		}
		delete [] gTheta_acc;
		gTheta_acc = NULL;
	}

	if (gGamma_acc != NULL) {
		for (c = 0; c < gChains; c++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gMaxBs; b++) {
					delete [] gGamma_acc[c][l][b];
				}
				delete [] gGamma_acc[c][l];
			}
			delete [] gGamma_acc[c];
		}
		delete [] gGamma_acc;
		gGamma_acc = NULL;
	}
}

void bhpm1a_poisson_mc_hier2_lev0::initL2Samples()
{
	int c = 0, t = 0, l = 0, b = 0;

	// The samples
	if (retainSamples(iMonitor_mu_gamma))
		mu_gamma_samples = new double ***[gChains];
	if (retainSamples(iMonitor_sigma2_gamma))
		sigma2_gamma_samples = new double ***[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_mu_gamma))
			mu_gamma_samples[c] = new double **[gNumClusters];
		if (retainSamples(iMonitor_sigma2_gamma))
			sigma2_gamma_samples[c] = new double **[gNumClusters];

		for (l = 0; l < gNumClusters; l++) {
			if (retainSamples(iMonitor_mu_gamma))
				mu_gamma_samples[c][l] = new double*[gMaxBs];
			if (retainSamples(iMonitor_sigma2_gamma))
				sigma2_gamma_samples[c][l] = new double*[gMaxBs];

			for (b = 0; b < gNumBodySys[l]; b++) {
				if (retainSamples(iMonitor_mu_gamma))
					mu_gamma_samples[c][l][b] =
						new double[(gIter - gBurnin)];
				if (retainSamples(iMonitor_sigma2_gamma))
					sigma2_gamma_samples[c][l][b] =
						new double[(gIter - gBurnin)];
			}
		}
	}

	if (retainSamples(iMonitor_mu_theta))
		mu_theta_samples = new double ****[gChains];
	if (retainSamples(iMonitor_sigma2_theta))
		sigma2_theta_samples = new double ****[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_mu_theta))
			mu_theta_samples[c] = new double ***[gNumComparators];
		if (retainSamples(iMonitor_sigma2_theta))
			sigma2_theta_samples[c] = new double ***[gNumComparators];

		for (t = 0; t < gNumComparators; t++) {

			if (retainSamples(iMonitor_mu_theta))
				mu_theta_samples[c][t] = new double **[gNumClusters];
			if (retainSamples(iMonitor_sigma2_theta))
				sigma2_theta_samples[c][t] = new double **[gNumClusters];

			for (l = 0; l < gNumClusters; l++) {
				if (retainSamples(iMonitor_mu_theta))
					mu_theta_samples[c][t][l] = new double*[gMaxBs];
				if (retainSamples(iMonitor_sigma2_theta))
					sigma2_theta_samples[c][t][l] = new double*[gMaxBs];

				for (b = 0; b < gNumBodySys[l]; b++) {
					if (retainSamples(iMonitor_mu_theta))
						mu_theta_samples[c][t][l][b] =
							new double[(gIter - gBurnin)];
					if (retainSamples(iMonitor_sigma2_theta))
						sigma2_theta_samples[c][t][l][b] =
							new double[(gIter - gBurnin)];
				}
			}
		}
	}
}

void bhpm1a_poisson_mc_hier2_lev0::releaseL2Samples()
{
	int c = 0, t = 0, l = 0, b = 0;

	if (mu_theta_samples) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				for (l = 0; l < gNumClusters; l++) {
					for (b = 0; b < gNumBodySys[l]; b++) {
						delete [] mu_theta_samples[c][t][l][b];
					}
					delete [] mu_theta_samples[c][t][l];
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
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] mu_gamma_samples[c][l][b];
				}
				delete [] mu_gamma_samples[c][l];
			}
			delete [] mu_gamma_samples[c];
		}
		delete [] mu_gamma_samples;
		mu_gamma_samples = NULL;
	}
	if (sigma2_theta_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumClusters; t++) {
				for (l = 0; l < gNumClusters; l++) {
					for (b = 0; b < gNumBodySys[l]; b++) {
						delete [] sigma2_theta_samples[c][t][l][b];
					}
					delete [] sigma2_theta_samples[c][t][l];
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
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] sigma2_gamma_samples[c][l][b];
				}
				delete [] sigma2_gamma_samples[c][l];
			}
			delete [] sigma2_gamma_samples[c];
		}
		delete [] sigma2_gamma_samples;
		sigma2_gamma_samples = NULL;
	}
}

void bhpm1a_poisson_mc_hier2_lev0::initL1Samples()
{
	int c = 0, t = 0, l = 0, b = 0, a = 0;

	if (retainSamples(iMonitor_gamma))
		gGamma_samples = new double ****[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_gamma))
			gGamma_samples[c] = new double ***[gNumClusters];

		for (l = 0; l < gNumClusters; l++) {
			if (retainSamples(iMonitor_gamma))
				gGamma_samples[c][l] =
							new double**[gNumBodySys[l]];

			for (b = 0; b < gNumBodySys[l]; b++) {
				if (retainSamples(iMonitor_gamma))
					gGamma_samples[c][l][b] =
								new double *[gNAE[l][b]];

				for (a = 0; a < gNAE[l][b]; a++) {
					if (retainSamples(iMonitor_gamma))
						gGamma_samples[c][l][b][a] =
								new double [(gIter - gBurnin)];
				}
			}
		}
	}

	if (retainSamples(iMonitor_theta)) {
		gTheta_samples = new double *****[gChains];

		for (c = 0; c < gChains; c++) {
			gTheta_samples[c] = new double ****[gNumComparators];

			for (t = 0; t < gNumComparators; t++) {

				gTheta_samples[c][t] = new double ***[gNumClusters];

				for (l = 0; l < gNumClusters; l++) {
					gTheta_samples[c][t][l] =
									new double**[gNumBodySys[l]];
	
					for (b = 0; b < gNumBodySys[l]; b++) {
						gTheta_samples[c][t][l][b] =
											new double *[gNAE[l][b]];
	
						for (a = 0; a < gNAE[l][b]; a++) {
							gTheta_samples[c][t][l][b][a] =
										new double [(gIter - gBurnin)];
						}
					}
				}
			}
		}
	}

	gGamma_acc = new int***[gChains];
	for (c = 0; c < gChains; c++) {
		gGamma_acc[c] = new int**[gNumClusters];
		for (l = 0; l < gNumClusters; l++) {
			gGamma_acc[c][l] = new int*[gMaxBs];
			for (b = 0; b < gMaxBs; b++) {
				gGamma_acc[c][l][b] = new int[gMaxAEs];
				for (a = 0; a < gMaxAEs; a++) {
					gGamma_acc[c][l][b][a] = 0;
				}
			}
		}
	}

	gTheta_acc = new int****[gChains];
	for (c = 0; c < gChains; c++) {
		gTheta_acc[c] = new int***[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			gTheta_acc[c][t] = new int**[gNumClusters];
			for (l = 0; l < gNumClusters; l++) {
				gTheta_acc[c][t][l] = new int*[gMaxBs];
				for (b = 0; b < gMaxBs; b++) {
					gTheta_acc[c][t][l][b] = new int[gMaxAEs];
					for (a = 0; a < gMaxAEs; a++) {
						gTheta_acc[c][t][l][b][a] = 0;
					}
				}
			}
		}
	}
}

void bhpm1a_poisson_mc_hier2_lev0::initL1Variables(SEXP ptheta, SEXP pgamma)
{
	int t = 0, c = 0, l = 0, b = 0, a = 0;

	gGamma = new double***[gChains];
	for (c = 0; c < gChains; c++) {
		gGamma[c] = new double**[gNumClusters];
		for (l = 0; l < gNumClusters; l++) {
			gGamma[c][l] = new double*[gMaxBs];
			for (b = 0; b < gMaxBs; b++) {
				gGamma[c][l][b] = new double[gMaxAEs];
			}
		}
	}

	gTheta = new double****[gChains];

	for (c = 0; c < gChains; c++) {
		gTheta[c] = new double***[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			gTheta[c][t] = new double**[gNumClusters];
			for (l = 0; l < gNumClusters; l++) {
				gTheta[c][t][l] = new double*[gMaxBs];
				for (b = 0; b < gMaxBs; b++) {
					gTheta[c][t][l][b] = new double[gMaxAEs];
				}
			}
		}
	}

	double* vgamma = REAL(pgamma);
	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {
			for (b = 0; b < gMaxBs; b++) {
				for (a = 0; a < gMaxAEs; a++) {
					gGamma[c][l][b][a] = *vgamma;
					vgamma++;
				}
			}
		}
	}

	double* vtheta = REAL(ptheta);
	for (c = 0; c < gChains; c++) {
		for (t = 0; t < gNumComparators; t++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gMaxBs; b++) {
					for (a = 0; a < gMaxAEs; a++) {
						gTheta[c][t][l][b][a] = *vtheta;
						vtheta++;
					}
				}
			}
		}
	}
}

void bhpm1a_poisson_mc_hier2_lev0::releaseDataVariables()
{
	int t = 0, l = 0, b = 0;

	if (x != NULL) {
		for (l = 0; l < gNumClusters; l++) {
			for (b = 0; b < gMaxBs; b++) {
				delete [] x[l][b];
			}
			delete [] x[l];
		}
		delete [] x;
		x = NULL;
	}
	if (y != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gMaxBs; b++) {
					delete [] y[t][l][b];
				}
				delete [] y[t][l];
			}
			delete [] y[t];
		}
		delete [] y;
		y = NULL;
	}
	if (C != NULL) {
		for (l = 0; l < gNumClusters; l++) {
			for (b = 0; b < gMaxBs; b++) {
				delete [] C[l][b];
			}
			delete [] C[l];
		}
		delete [] C;
		C = NULL;
	}
	if (T != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gMaxBs; b++) {
					delete [] T[t][l][b];
				}
				delete [] T[t][l];
			}
			delete [] T[t];
		}
		delete [] T;
		T = NULL;
	}
}

void bhpm1a_poisson_mc_hier2_lev0::releaseSimParams()
{
	int l = 0, b = 0, t = 0;

	if (gW_gamma != NULL) {
		for (l = 0; l < gNumClusters; l++) {
			for (b = 0; b < gNumBodySys[l]; b++) {
				delete [] gW_gamma[l][b];
			}
			delete [] gW_gamma[l];
		}
		delete [] gW_gamma;
		gW_gamma = NULL;
	}

	if (gW_theta != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] gW_theta[t][l][b];
				}
				delete [] gW_theta[t][l];
			}
				delete [] gW_theta[t];
		}
		delete [] gW_theta;
		gW_theta = NULL;
	}

	if (gW_gamma_control != NULL) {
		for (l = 0; l < gNumClusters; l++) {
			for (b = 0; b < gNumBodySys[l]; b++) {
				delete [] gW_gamma_control[l][b];
			}
			delete [] gW_gamma_control[l];
		}
		delete [] gW_gamma_control;
		gW_gamma_control = NULL;
	}

	if (gW_theta_control != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] gW_theta_control[t][l][b];
				}
				delete [] gW_theta_control[t][l];
			}
			delete [] gW_theta_control[t];
		}
		delete [] gW_theta_control;
		gW_theta_control = NULL;
	}

	if (gSigma_MH_gamma != NULL) {
		for (l = 0; l < gNumClusters; l++) {
			for (b = 0; b < gNumBodySys[l]; b++) {
				delete [] gSigma_MH_gamma[l][b];
			}
			delete [] gSigma_MH_gamma[l];
		}
		delete [] gSigma_MH_gamma;
		gSigma_MH_gamma = NULL;
	}

	if (gSigma_MH_theta != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] gSigma_MH_theta[t][l][b];
				}
				delete [] gSigma_MH_theta[t][l];
			}
			delete [] gSigma_MH_theta[t];
		}
		delete [] gSigma_MH_theta;
		gSigma_MH_theta = NULL;
	}
}

void bhpm1a_poisson_mc_hier2_lev0::releaseGlobalSimParams()
{
	if (sim_type != NULL) {
		delete [] sim_type;
		sim_type = NULL;
	}
}

void bhpm1a_poisson_mc_hier2_lev0::initGlobalSimParams(SEXP sSim_Type, SEXP sGlobal_Sim_Param, SEXP sGlobal_Sim_Param_cntrl)
{
	int l = 0;

	// Simulations Parameters
	l = strlen(CHAR(STRING_ELT(sSim_Type, 0)));
	sim_type = new char[l + 1];
	strcpy(sim_type, CHAR(STRING_ELT(sSim_Type, 0)));
	sim_type[l] = 0;

	// Global Simulation Parameters
	gSim_Param = *REAL(sGlobal_Sim_Param);
	gSim_Param_cntrl = *REAL(sGlobal_Sim_Param_cntrl);
}

void bhpm1a_poisson_mc_hier2_lev0::initSimParams(SEXP sSim_Params)
{
	gW_gamma = new double **[gNumClusters];
	gW_gamma_control = new int **[gNumClusters];
	gSigma_MH_gamma = new double **[gNumClusters];


	gW_theta = new double***[gNumComparators];
	gW_theta_control = new int ***[gNumComparators];
	gSigma_MH_theta = new double ***[gNumComparators];

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
				gW_gamma[i][b][j] = gSim_Param;
				gW_gamma_control[i][b][j] = (int)gSim_Param_cntrl;
				gSigma_MH_gamma[i][b][j] = gSim_Param;
			}
		}
	}

	for (t = 0; t < gNumComparators; t++) {
		gW_theta[t] = new double**[gNumClusters];
		gW_theta_control[t] = new int **[gNumClusters];
		gSigma_MH_theta[t] = new double **[gNumClusters];

		for (i = 0; i < gNumClusters; i++) {
			gW_theta[t][i] = new double*[gNumBodySys[i]];
			gW_theta_control[t][i] = new int*[gNumBodySys[i]];
			gSigma_MH_theta[t][i] = new double*[gNumBodySys[i]];
			for (b = 0; b < gNumBodySys[i]; b++) {
				gW_theta[t][i][b] = new double[gNAE[i][b]];
				gW_theta_control[t][i][b] = new int[gNAE[i][b]];
				gSigma_MH_theta[t][i][b] = new double[gNAE[i][b]];
	
				for (j = 0; j < gNAE[i][b]; j++) {
					gW_theta[t][i][b][j] = gSim_Param;
					gW_theta_control[t][i][b][j] = (int)gSim_Param_cntrl;
					gSigma_MH_theta[t][i][b][j] = gSim_Param;
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
					if (0 == strcmp(param, sParam_w)) {
						gW_gamma[l][b][a] = vals[i];
						gW_gamma_control[l][b][a] = (int)cntrl[i];
					}
					else if (0 == strcmp(param, sParam_sigma_MH)) {
						gSigma_MH_gamma[l][b][a] = vals[i];
					}
				}
				else if (0 == strcmp(sVariable_theta, var)) {
					if (0 == strcmp(param, sParam_w)) {
						gW_theta[t][l][b][a] = vals[i];
						gW_theta_control[t][l][b][a] = (int)cntrl[i];
					}
					else if (0 == strcmp(param, sParam_sigma_MH)) {
						gSigma_MH_theta[t][l][b][a] = vals[i];
					}
				}
			}
		}
	}
}

void bhpm1a_poisson_mc_hier2_lev0::initMonitor(SEXP sMonitor)
{
    int len = Rf_length(sMonitor);

    SEXP sVariables = R_NilValue;
    SEXP sValues = R_NilValue;

    if (len > 0 && isNewList(sMonitor)) {
        SEXP names = getAttrib(sMonitor, R_NamesSymbol);

        int i = 0;

        for (i = 0; i < len; i++) {
            if (strcmp(sColMonitorVariables, CHAR(STRING_ELT(names, i))) == 0) {
                sVariables = VECTOR_ELT(sMonitor, i);
            }
            if (strcmp(sColMonitorValues, CHAR(STRING_ELT(names, i))) == 0) {
                sValues = VECTOR_ELT(sMonitor, i);
            }
        }
            
        len = Rf_length(sVariables);

		if (len > 0) {

	        int* vals = INTEGER(sValues);
	
	        for (i = 0; i < len; i++) {
				const char *t = CHAR(STRING_ELT(sVariables, i));
	
	            if (0 == strcmp(t, sMonitor_theta)) {
	                iMonitor_theta = vals[i];
	            }
	            if (0 == strcmp(t, sMonitor_gamma)) {
	                iMonitor_gamma = vals[i];
	            }
	            if (0 == strcmp(t, sMonitor_mu_theta)) {
	                iMonitor_mu_theta = vals[i];
	            }
	            if (0 == strcmp(t, sMonitor_mu_gamma)) {
	                iMonitor_mu_gamma = vals[i];
	            }
	            if (0 == strcmp(t, sMonitor_sigma2_theta)) {
	                iMonitor_sigma2_theta = vals[i];
	            }
	            if (0 == strcmp(t, sMonitor_sigma2_gamma)) {
	                iMonitor_sigma2_gamma = vals[i];
	            }
			}
		}
	}
}

bhpm1a_poisson_mc_hier2_lev0::~bhpm1a_poisson_mc_hier2_lev0()
{
	//Rprintf("bhpm1a_poisson_mc_hier2_lev0::bhpm1a_poisson_mc_hier2_lev0 - destructor\n");
	release();
}

void bhpm1a_poisson_mc_hier2_lev0::gibbs_sampler()
{
	if (strcmp(sim_type, "MH") == 0) {
		simulate_MH();
	}
	else {
		simulate_SLICE();
	}

	return;
}

void bhpm1a_poisson_mc_hier2_lev0::simulate_MH()
{
	int i = 0, t = 0;

	for (i = 0; i < gIter; i++) {
#ifndef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		// Keep the original scan order which is in "hierarchy" order
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

void bhpm1a_poisson_mc_hier2_lev0::simulate_SLICE()
{
	int i = 0, t = 0;

	for (i = 0; i < gIter; i++) {
#ifndef INDIVIDUAL_RNG
		GetRNGstate();
#endif
		// Keep the original scan order which is in "hierarchy" order
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

void bhpm1a_poisson_mc_hier2_lev0::sample_mu_gamma(int burnin, int iter)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;

			for (b = 0; b < gNumBodySys[l]; b++) {
				double denom = sigma2_gamma[c][l][b] + ((double)gNAE[l][b])*tau2_gamma_0;

				double t = 0.0;
				int j = 0;
				for (j = 0; j < gNAE[l][b]; j++) {
					t += gGamma[c][l][b][j];
				}

				double mean = (sigma2_gamma[c][l][b] * mu_gamma_0 + tau2_gamma_0 * t)/denom;

				double var = (sigma2_gamma[c][l][b]*tau2_gamma_0)/denom;

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

void bhpm1a_poisson_mc_hier2_lev0::sample_mu_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;

			for (b = 0; b < gNumBodySys[l]; b++) {
				double denom = sigma2_theta[c][tr][l][b] + ((double)gNAE[l][b])*tau2_theta_0;

				double t = 0.0;
				int j = 0;
				for (j = 0; j < gNAE[l][b]; j++) {
					t += gTheta[c][tr][l][b][j];
				}

				double mean = (sigma2_theta[c][tr][l][b] * mu_theta_0 + tau2_theta_0 * t)/denom;

				double var = (sigma2_theta[c][tr][l][b]*tau2_theta_0)/denom;

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

void bhpm1a_poisson_mc_hier2_lev0::sample_sigma2_gamma(int burnin, int iter)
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
					t += (pow(gGamma[c][l][b][j] - mu_gamma[c][l][b], 2.0));
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

void bhpm1a_poisson_mc_hier2_lev0::sample_sigma2_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;

			for (b = 0; b < gNumBodySys[l]; b++) {

				double s = alpha_theta + ((double)gNAE[l][b])/2.0;


				double t = 0;
				int j = 0;
				for (j = 0; j < gNAE[l][b]; j++) {
					t += (pow((gTheta[c][tr][l][b][j] - mu_theta[c][tr][l][b]), 2.0));
				}

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

double bhpm1a_poisson_mc_hier2_lev0::log_f_gamma(int c, int i, int b, int j, double gamm)
{
	double f1 = 0.0, f2 = 0.0, f3 = 0.0, f4 = 0.0, f5 = 0.0;
	int t = 0;

	f1 = ((double)x[i][b][j]) * gamm;
	f2 = -(exp(gamm)) * ((double)C[i][b][j]);
	for (t = 0; t < gNumComparators; t++) {
		f3 += ((double)y[t][i][b][j]) * (gamm + gTheta[c][t][i][b][j]);
		f4 += -(exp(gamm + gTheta[c][t][i][b][j]))*((double)T[t][i][b][j]);
	}
	f5 = -(pow((gamm - mu_gamma[c][i][b]), 2.0))/(2.0 * sigma2_gamma[c][i][b]);

	double f = f1 + f2 + f3 + f4 + f5;

	return(f);
}

void bhpm1a_poisson_mc_hier2_lev0::sample_gamma_MH(int burnin, int iter)
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

void bhpm1a_poisson_mc_hier2_lev0::sample_gamma_SLICE(int burnin, int iter)
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

double bhpm1a_poisson_mc_hier2_lev0::log_f_theta(int c, int i, int b, int j, double theta, int t)
{
	double f1 = 0.0, f2 = 0.0, f3 = 0.0;

	f1 = ((double)y[t][i][b][j]) * (gGamma[c][i][b][j] + theta);
	f2 = -(exp(gGamma[c][i][b][j] + theta)) * ((double)T[t][i][b][j]);
	f3 = - ((pow(theta - mu_theta[c][t][i][b], 2.0)))/(2.0 * sigma2_theta[c][t][i][b]);

	double f = f1 + f2 + f3;

	return(f);
}

void bhpm1a_poisson_mc_hier2_lev0::sample_theta_MH(int burnin, int iter, int t)
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
					double cand = rnorm(gTheta[c][t][l][b][j], gSigma_MH_theta[l][t][b][j]);
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

					double f1 = log_f_theta(c, l, b, j, cand, t);
					double f2 = log_f_theta(c, l, b, j, gTheta[c][t][l][b][j], t);

					double ratio = exp(f1 - f2);

					ratio = cMIN(ratio, 1);

					if (u <= ratio) {
						gTheta[c][t][l][b][j] = cand;
						gTheta_acc[c][t][l][b][j] = gTheta_acc[c][t][l][b][j] + 1;
					}

					if (iter >= burnin && retainSamples(iMonitor_theta)) {
						gTheta_samples[c][t][l][b][j][iter - burnin] = gTheta[c][t][l][b][j];
					}
				}
			}
		}
	}
}

void bhpm1a_poisson_mc_hier2_lev0::sample_theta_SLICE(int burnin, int iter, int t)
{
	int c = 0, i = 0;
	int K = 0, J = 0;

	for (c = 0; c < gChains; c++) {
		for (i = 0; i < gNumClusters; i++) {


			int b = 0, j = 0;
			double cand = 0.0;

			for (b = 0; b < gNumBodySys[i]; b++) {
				for (j = 0; j < gNAE[i][b]; j++) {

					int m = gW_theta_control[t][i][b][j];
#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					J = floor(runif(0,m));
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif
					K = (m-1) - J;

					double l = 0.0, r = 0.0;
					double g = log_f_theta(c, i, b, j, gTheta[c][t][i][b][j], t);
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
					double u = runif(0, gW_theta[t][i][b][j]);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif

					l = gTheta[c][t][i][b][j] - u;
					r = gTheta[c][t][i][b][j] + (gW_theta[t][i][b][j] - u);

					while (J > 0) {
						if (logy >= log_f_theta(c, i, b, j, l, t)) {
							break;
						}
						l = l - gW_theta[t][i][b][j];
						J--;
					}

					while (K > 0) {
						if (logy >= log_f_theta(c, i, b, j, r, t)) {
							break;
						}
						r = r + gW_theta[t][i][b][j];
						K--;
					}
			
#ifdef INDIVIDUAL_RNG
					GetRNGstate();
#endif
					cand = runif(l, r);
#ifdef INDIVIDUAL_RNG
					PutRNGstate();
#endif

					while (logy >= log_f_theta(c, i, b, j, cand, t)) {
						if (cand < gTheta[c][t][i][b][j]) {
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

					gTheta[c][t][i][b][j] = cand;

					if (iter >= burnin && retainSamples(iMonitor_theta)) {
						gTheta_samples[c][t][i][b][j][iter - burnin] = gTheta[c][t][i][b][j];
					}
				}
			}
		}
	}
}

double bhpm1a_poisson_mc_hier2_lev0::cMIN(double a, double b)
{
	if (a < b) {
		return a;
	}
	else {
		return b;
	}
}

void bhpm1a_poisson_mc_hier2_lev0::releaseL1Variables()
{
	int c = 0, l = 0, b = 0, t = 0;

	if (gTheta != NULL) {
        for (c = 0; c < gChains; c++) {
            for (t = 0; t < gNumComparators; t++) {
                for (l = 0; l < gNumClusters; l++) {
                    for (b = 0; b < gMaxBs; b++) {
                        delete [] gTheta[c][t][l][b];
                    }
                    delete [] gTheta[c][t][l];
                }
                delete [] gTheta[c][t];
            }
            delete [] gTheta[c];
        }
        delete [] gTheta;
        gTheta = NULL;
    }

    if (gGamma != NULL) {
        for (c = 0; c < gChains; c++) {
            for (l = 0; l < gNumClusters; l++) {
                for (b = 0; b < gMaxBs; b++) {
                    delete [] gGamma[c][l][b];
                }
                delete [] gGamma[c][l];
            }
            delete [] gGamma[c];
        }
        delete [] gGamma;
        gGamma = NULL;
    }
}

void bhpm1a_poisson_mc_hier2_lev0::releaseL2Variables()
{
	int c = 0, t = 0, l = 0;

	if (mu_gamma != NULL) {
		for (c = 0; c < gChains; c++) {
			for (l = 0; l < gNumClusters; l++) {
				delete [] mu_gamma[c][l];
			}
			delete [] mu_gamma[c];
		}
		delete [] mu_gamma;
		mu_gamma = 0;
	}

	if (mu_theta != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				for (l = 0; l < gNumClusters; l++) {
					delete [] mu_theta[c][t][l];
				}
				delete [] mu_theta[c][t];
			}
			delete [] mu_theta[c];
		}
		delete [] mu_theta;
		mu_theta = 0;
	}

	if (sigma2_gamma != NULL) {
		for (c = 0; c < gChains; c++) {
			for (l = 0; l < gNumClusters; l++) {
				delete [] sigma2_gamma[c][l];
			}
			delete [] sigma2_gamma[c];
		}
		delete [] sigma2_gamma;
		sigma2_gamma = 0;
	}

	if (sigma2_theta != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				for (l = 0; l < gNumClusters; l++) {
					delete [] sigma2_theta[c][t][l];
				}
				delete [] sigma2_theta[c][t];
			}
			delete [] sigma2_theta[c];
		}
		delete [] sigma2_theta;
		sigma2_theta = 0;
	}
}

void bhpm1a_poisson_mc_hier2_lev0::release()
{
	releaseGlobalSimParams();

	releaseDataVariables();

	releaseL1Variables();

	releaseL2Variables();

	releaseL2Samples();

	releaseL1Samples();

	releaseSimParams();

	releaseBaselineVariables();
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getL1Samples(double****** &data)
{
	SEXP samples = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(samples = allocVector(REALSXP, gChains * gNumComparators * gNumClusters * gMaxBs * gMaxAEs * (gIter - gBurnin)));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int t = 0;
		for (t = 0; t < gNumComparators; t++) {
			int l = 0;
			for (l = 0; l < gNumClusters; l++) {
				int b = 0;
				for (b = 0; b < gMaxBs; b++) {
					int j = 0;
					if (b < gNumBodySys[l]) {
						for (j = 0; j < gMaxAEs; j++) {
							if (j < gNAE[l][b]) {
								memcpy(REAL(samples) + i, data[c][t][l][b][j],
															(gIter - gBurnin)*sizeof(double));
								delete [] data[c][t][l][b][j];
								data[c][t][l][b][j] = NULL;
							}
							i += (gIter - gBurnin);
						}
						delete [] data[c][t][l][b];
						data[c][t][l][b] = NULL;
					}
					else {
						i += gMaxAEs*(gIter - gBurnin);
					}
				}
				delete [] data[c][t][l];
				data[c][t][l] = NULL;
			}
			delete [] data[c][t];
			data[c][t] = NULL;
		}
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 6));

	INTEGER(dim)[0] = (gIter - gBurnin);
	INTEGER(dim)[1] = gMaxAEs;
	INTEGER(dim)[2] = gMaxBs;
	INTEGER(dim)[3] = gNumClusters;
	INTEGER(dim)[4] = gNumComparators;
	INTEGER(dim)[5] = gChains;

	setAttrib(samples, R_DimSymbol, dim);

	UNPROTECT(2);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getL1Samples(double***** &data)
{
	SEXP samples = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(samples = allocVector(REALSXP, gChains * gNumClusters * gMaxBs * gMaxAEs * (gIter - gBurnin)));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int l = 0;
		for (l = 0; l < gNumClusters; l++) {
			int b = 0;
			for (b = 0; b < gMaxBs; b++) {
				int j = 0;
				if (b < gNumBodySys[l]) {
					for (j = 0; j < gMaxAEs; j++) {
						if (j < gNAE[l][b]) {
							memcpy(REAL(samples) + i, data[c][l][b][j],
														(gIter - gBurnin)*sizeof(double));
							delete [] data[c][l][b][j];
							data[c][l][b][j] = NULL;
						}
						i += (gIter - gBurnin);
					}
					delete [] data[c][l][b];
					data[c][l][b] = NULL;
				}
				else {
					i += gMaxAEs*(gIter - gBurnin);
				}
			}
			delete [] data[c][l];
			data[c][l] = NULL;
		}
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 5));

	INTEGER(dim)[0] = (gIter - gBurnin);
	INTEGER(dim)[1] = gMaxAEs;
	INTEGER(dim)[2] = gMaxBs;
	INTEGER(dim)[3] = gNumClusters;
	INTEGER(dim)[4] = gChains;

	setAttrib(samples, R_DimSymbol, dim);

	UNPROTECT(2);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getL2Samples(double**** &data)
{
	SEXP samples = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(samples = allocVector(REALSXP, gChains * gNumClusters * gMaxBs * (gIter - gBurnin)));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int l = 0;
		for (l = 0; l < gNumClusters; l++) {
			int b = 0;
			for (b = 0; b < gMaxBs; b++) {
				if (b < gNumBodySys[l]) {
					memcpy(REAL(samples) + i, data[c][l][b],
														(gIter - gBurnin)*sizeof(double));
				}
				i += (gIter - gBurnin);
				delete [] data[c][l][b];
				data[c][l][b] = NULL;
			}
			delete [] data[c][l];
			data[c][l] = NULL;
		}
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 4));

	INTEGER(dim)[0] = (gIter - gBurnin);
	INTEGER(dim)[1] = gMaxBs;
	INTEGER(dim)[2] = gNumClusters;
	INTEGER(dim)[3] = gChains;

	setAttrib(samples, R_DimSymbol, dim);

	UNPROTECT(2);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getL2Samples(double***** &data)
{
	SEXP samples = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(samples = allocVector(REALSXP, gChains * gNumComparators * gNumClusters * gMaxBs * (gIter - gBurnin)));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int t = 0;
		for (t = 0; t < gNumComparators; t++) {
			int l = 0;
			for (l = 0; l < gNumClusters; l++) {
				int b = 0;
				for (b = 0; b < gMaxBs; b++) {
					if (b < gNumBodySys[l]) {
						memcpy(REAL(samples) + i, data[c][t][l][b],
															(gIter - gBurnin)*sizeof(double));
					}
					i += (gIter - gBurnin);
					delete [] data[c][t][l][b];
					data[c][t][l][b] = NULL;
				}
				delete [] data[c][t][l];
				data[c][t][l] = NULL;
			}
			delete [] data[c][t];
			data[c][t] = NULL;
		}
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 5));

	INTEGER(dim)[0] = (gIter - gBurnin);
	INTEGER(dim)[1] = gMaxBs;
	INTEGER(dim)[2] = gNumClusters;
	INTEGER(dim)[3] = gNumComparators;
	INTEGER(dim)[4] = gChains;

	setAttrib(samples, R_DimSymbol, dim);

	UNPROTECT(2);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getThetaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL1Samples(gTheta_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getGammaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL1Samples(gGamma_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getMuThetaSamples()
{
	SEXP samples = R_NilValue;

	//Rprintf("bhpm1a_poisson_mc_hier2_lev0::getMuThetaSamples\n");
	samples = getL2Samples(mu_theta_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getMuGammaSamples()
{
	SEXP samples = R_NilValue;

	//Rprintf("bhpm1a_poisson_mc_hier2_lev0::getMuGammaSamples\n");
	samples = getL2Samples(mu_gamma_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getSigma2ThetaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(sigma2_theta_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getSigma2GammaSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(sigma2_gamma_samples);

	return samples;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getL1Accept(int***** &data)
{
	SEXP acc = R_NilValue;
	SEXP dim = R_NilValue;

   PROTECT(acc = allocVector(INTSXP, gChains * gNumComparators * gNumClusters * gMaxBs * gMaxAEs));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int t = 0;
		for (t = 0; t < gNumComparators; t++) {
			int l = 0;
			for (l = 0; l < gNumClusters; l++) {
				int b = 0;
				for (b = 0; b < gMaxBs; b++) {
					if (b < gNumBodySys[l]) {
						memcpy(INTEGER(acc) + i, data[c][t][l][b], gMaxAEs*sizeof(int));
					}
					i += gMaxAEs;
					delete [] data[c][t][l][b];
					data[c][t][l][b] = NULL;
				}
				delete [] data[c][t][l];
				data[c][t][l] = NULL;
			}
			delete [] data[c][t];
			data[c][t] = NULL;
		}
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 5));

	INTEGER(dim)[0] = gMaxAEs;
	INTEGER(dim)[1] = gMaxBs;
	INTEGER(dim)[2] = gNumClusters;
	INTEGER(dim)[3] = gNumComparators;
	INTEGER(dim)[4] = gChains;

	setAttrib(acc, R_DimSymbol, dim);

	UNPROTECT(2);

	return acc;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getL1Accept(int**** &data)
{
	SEXP acc = R_NilValue;
	SEXP dim = R_NilValue;

   PROTECT(acc = allocVector(INTSXP, gChains * gNumClusters * gMaxBs * gMaxAEs));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		int l = 0;
		for (l = 0; l < gNumClusters; l++) {
			int b = 0;
			for (b = 0; b < gMaxBs; b++) {
				if (b < gNumBodySys[l]) {
					memcpy(INTEGER(acc) + i, data[c][l][b], gMaxAEs*sizeof(int));
				}
				i += gMaxAEs;
				delete [] data[c][l][b];
				data[c][l][b] = NULL;
			}
			delete [] data[c][l];
			data[c][l] = NULL;
		}
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 4));

	INTEGER(dim)[0] = gMaxAEs;
	INTEGER(dim)[1] = gMaxBs;
	INTEGER(dim)[2] = gNumClusters;
	INTEGER(dim)[3] = gChains;

	setAttrib(acc, R_DimSymbol, dim);

	UNPROTECT(2);

	return acc;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getThetaAccept()
{
	SEXP acc = R_NilValue;

	//Rprintf("bhpm1a_poisson_mc_hier2_lev0::getThetaAccept\n");
	acc = getL1Accept(gTheta_acc);

	return acc;
}

SEXP bhpm1a_poisson_mc_hier2_lev0::getGammaAccept()
{
	SEXP acc = R_NilValue;

	//Rprintf("bhpm1a_poisson_mc_hier2_lev0::getGammaAccept\n");
	acc = getL1Accept(gGamma_acc);

	return acc;
}

void bhpm1a_poisson_mc_hier2_lev0::getThetaSamples(int *c, int*l, int* b, int* j, double* theta_samples)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;
	int J = (*j) - 1;

	if (gTheta_samples)
		memcpy(theta_samples,
					gTheta_samples[C][0][L][B][J], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier2_lev0::getGammaSamples(int *c, int *l, int* b, int* j, double* gamma_samples)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;
	int J = (*j) - 1;

	if (gGamma_samples)
		memcpy(gamma_samples,
					gGamma_samples[C][L][B][J], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier2_lev0::getMuThetaSamples(int *c, int *l, int* b, double* mu_theta)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;

	if (mu_theta_samples)
		memcpy(mu_theta, mu_theta_samples[C][0][L][B], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier2_lev0::getMuGammaSamples(int *c, int *l, int* b, double* mu_gamma)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;

	if (mu_gamma_samples)
		memcpy(mu_gamma, mu_gamma_samples[C][L][B], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier2_lev0::getSigma2ThetaSamples(int *c, int *l, int* b, double* sigma2)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;

	if (sigma2_theta_samples)
		memcpy(sigma2, sigma2_theta_samples[C][0][L][B], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier2_lev0::getSigma2GammaSamples(int *c, int *l, int* b, double* sigma2)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;

	if (sigma2_gamma_samples)
		memcpy(sigma2, sigma2_gamma_samples[C][L][B], (gIter - gBurnin)*sizeof(double));
}

void bhpm1a_poisson_mc_hier2_lev0::getThetaAccept(int *c, int *l, int* b, int* j, double* theta_acc)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;
	int J = (*j) - 1;

	*theta_acc = gTheta_acc[C][0][L][B][J];
}

void bhpm1a_poisson_mc_hier2_lev0::getGammaAccept(int *c, int *l, int* b, int* j, double* gamma_acc)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;
	int J = (*j) - 1;

	*gamma_acc = gGamma_acc[C][L][B][J];
}
