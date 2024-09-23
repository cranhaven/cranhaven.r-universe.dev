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
#include "bhpm1a_poisson_mc_hier3_lev0.h"
#include "bhpmBB_poisson_mc_hier3_lev0.h"

using namespace std;

static const char *rcsId = "$Id: bhpmBB_poisson_mc_hier3_lev0.cpp,v 1.9 2019/04/28 13:51:58 clb13102 Exp clb13102 $";

const char* bhpmBB_poisson_mc_hier3_lev0::sMonitor_pi = "pi";
const char* bhpmBB_poisson_mc_hier3_lev0::sMonitor_alpha_pi = "alpha.pi";
const char* bhpmBB_poisson_mc_hier3_lev0::sMonitor_beta_pi = "beta.pi";

const char* bhpmBB_poisson_mc_hier3_lev0::sColType = "type";
const char* bhpmBB_poisson_mc_hier3_lev0::sColParam = "param";
const char* bhpmBB_poisson_mc_hier3_lev0::sColValue = "value";
const char* bhpmBB_poisson_mc_hier3_lev0::sColControl = "control";

const char* bhpmBB_poisson_mc_hier3_lev0::sColPMweight = "weight_pm";

const char* bhpmBB_poisson_mc_hier3_lev0::sParam_sigma_MH_gamma = "sigma_MH_gamma";
const char* bhpmBB_poisson_mc_hier3_lev0::sParam_sigma_MH_theta = "sigma_MH_theta";
const char* bhpmBB_poisson_mc_hier3_lev0::sParam_sigma_MH_alpha = "sigma_MH_alpha";
const char* bhpmBB_poisson_mc_hier3_lev0::sParam_sigma_MH_beta = "sigma_MH_beta";
const char* bhpmBB_poisson_mc_hier3_lev0::sParam_w_gamma = "w_gamma";
const char* bhpmBB_poisson_mc_hier3_lev0::sParam_w_theta = "w_theta";
const char* bhpmBB_poisson_mc_hier3_lev0::sParam_w_alpha = "w_alpha";
const char* bhpmBB_poisson_mc_hier3_lev0::sParam_w_beta = "w_beta";

const char* bhpmBB_poisson_mc_hier3_lev0::sVariable_alpha = "alpha";
const char* bhpmBB_poisson_mc_hier3_lev0::sVariable_beta = "beta";


bhpmBB_poisson_mc_hier3_lev0::bhpmBB_poisson_mc_hier3_lev0()
{
	//Rprintf("bhpmBB_poisson_mc_hier3_lev0::bhpmBB_poisson_mc_hier3_lev0: Default constructor\n");

	gW_alpha = NULL;
	gW_beta = NULL;
	gW_alpha_control = NULL;
	gW_beta_control = NULL;
	gSigma_MH_alpha = NULL;
	gSigma_MH_beta = NULL;

	gWp = NULL;
	gMH_weight = 0.5;

	iMonitor_pi = 0;
	iMonitor_alpha_pi = 0;
	iMonitor_beta_pi = 0;

	gSimType = eSim_Type_SLICE;

	gDefault_Sigma_MH_alpha = 1.0;
	gDefault_Sigma_MH_beta = 1.0;
	gDefault_Sigma_MH_gamma = 1.0;
	gDefault_Sigma_MH_theta = 1.0;
	gDefault_W_alpha = 1.0;
	gDefault_W_beta = 1.0;
	gDefault_W_gamma = 1.0;
	gDefault_W_alpha_control = 6.0;
	gDefault_W_beta_control = 6.0;
	gDefault_W_gamma_control = 6.0;

	lambda_alpha = 0.0;
	lambda_beta = 0.0;

	alpha_pi = NULL;
	beta_pi = NULL;

	gPi = NULL;

	alpha_pi_acc = NULL;
	beta_pi_acc = NULL;

	gPi_samples = NULL;
	alpha_pi_samples = NULL;
	beta_pi_samples = NULL;
}

bhpmBB_poisson_mc_hier3_lev0::bhpmBB_poisson_mc_hier3_lev0(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type,
					SEXP sMem_Model, SEXP sGlobal_Sim_Params,
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
	gW_alpha = NULL;
	gW_beta = NULL;
	gW_alpha_control = NULL;
	gW_beta_control = NULL;
	gSigma_MH_alpha = NULL;
	gSigma_MH_beta = NULL;

	gWp = NULL;
	gMH_weight = 0.5;

	iMonitor_pi = 0;
	iMonitor_alpha_pi = 0;
    iMonitor_beta_pi = 0;

	gSimType = eSim_Type_SLICE;

	gDefault_Sigma_MH_alpha = 1.0;
	gDefault_Sigma_MH_beta = 1.0;
	gDefault_Sigma_MH_gamma = 1.0;
	gDefault_Sigma_MH_theta = 1.0;
	gDefault_W_alpha = 1.0;
	gDefault_W_beta = 1.0;
	gDefault_W_gamma = 1.0;
	gDefault_W_alpha_control = 6.0;
	gDefault_W_beta_control = 6.0;
	gDefault_W_gamma_control = 6.0;

	lambda_alpha = 0.0;
	lambda_beta = 0.0;

	alpha_pi = NULL;
	beta_pi = NULL;

	gPi = NULL;

	alpha_pi_acc = NULL;
	beta_pi_acc = NULL;

	gPi_samples = NULL;
	alpha_pi_samples = NULL;
	beta_pi_samples = NULL;

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

void bhpmBB_poisson_mc_hier3_lev0::initPMWeights(SEXP pm_weights)
{
	gWp = new double ***[gNumComparators];

	int i = 0, t = 0, b = 0, j = 0;
	for (t = 0; t < gNumComparators; t++) {
		gWp[t] = new double**[gNumClusters];

		for (i = 0; i < gNumClusters; i++) {
			gWp[t][i] = new double*[gNumBodySys[i]];
			for (b = 0; b < gNumBodySys[i]; b++) {
				gWp[t][i][b] = new double[gNAE[i][b]];
				for (j = 0; j < gNAE[i][b]; j++) {
					gWp[t][i][b][j] = gMH_weight;
				}
			}
		}
	}

	int len = Rf_length(pm_weights);

	SEXP sPM_Weights = R_NilValue;
	SEXP sC_index = R_NilValue;
	SEXP sB = R_NilValue;
	SEXP sj = R_NilValue;
	SEXP sGroup = R_NilValue;

	if (len && isNewList(pm_weights)) {

		SEXP names = getAttrib(pm_weights, R_NamesSymbol);

		for (i = 0; i < len; i++) {
			if (strcmp(sColPMweight, CHAR(STRING_ELT(names, i))) == 0) {
				sPM_Weights = VECTOR_ELT(pm_weights, i);
			}
			if (strcmp(sColC_index, CHAR(STRING_ELT(names, i))) == 0) {
				sC_index = VECTOR_ELT(pm_weights, i);
			}
			if (strcmp(sColB, CHAR(STRING_ELT(names, i))) == 0) {
				sB = VECTOR_ELT(pm_weights, i);
			}
			if (strcmp(sColj, CHAR(STRING_ELT(names, i))) == 0) {
				sj = VECTOR_ELT(pm_weights, i);
			}
			if (strcmp(sColGroup, CHAR(STRING_ELT(names, i))) == 0) {
				sGroup = VECTOR_ELT(pm_weights, i);
			}
		}

		len = Rf_length(sPM_Weights);

		if (len > 0) {
			double* weights = REAL(sPM_Weights);
			int* L = INTEGER(sC_index);
			int* B = INTEGER(sB);
			int* j = INTEGER(sj);
			int* group = INTEGER(sGroup);

			for (i = 0; i < len; i++) {
				int l = L[i] - 1;
				int b = B[i] - 1;
				int a = j[i] - 1;
				int t = group[i] - 1;
				gWp[t][l][b][a] = weights[i];
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::initSimParams(SEXP sSim_Params)
{
	gW_gamma = new double**[gNumClusters];
	gW_gamma_control = new int**[gNumClusters];
	gSigma_MH_gamma = new double**[gNumClusters];
	gSigma_MH_theta = new double ***[gNumComparators];

	gW_alpha = new double*[gNumComparators];
	gW_beta = new double*[gNumComparators];
	gW_alpha_control = new double*[gNumComparators];
	gW_beta_control = new double*[gNumComparators];
	gSigma_MH_alpha = new double*[gNumComparators];
	gSigma_MH_beta = new double*[gNumComparators];

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

		gW_alpha[t] = new double[gNumClusters];
		gW_beta[t] = new double[gNumClusters];
		gW_alpha_control[t] = new double[gNumClusters];
		gW_beta_control[t] = new double[gNumClusters];
		gSigma_MH_alpha[t] = new double[gNumClusters];
		gSigma_MH_beta[t] = new double[gNumClusters];

		for (i = 0; i < gNumClusters; i++) {

			gW_alpha[t][i] = gDefault_W_alpha;
			gW_beta[t][i] = gDefault_W_alpha;
			gW_alpha_control[t][i] = gDefault_W_alpha_control;
			gW_beta_control[t][i] = gDefault_W_beta_control;
			gSigma_MH_alpha[t][i] = gDefault_Sigma_MH_alpha;
			gSigma_MH_beta[t][i] = gDefault_Sigma_MH_beta;

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
						gW_alpha[t][l] = vals[i];
						gW_alpha_control[t][l] = (int)cntrl[i];
					}
					else if (0 == strcmp(param, sParam_sigma_MH_alpha)) {
						gSigma_MH_alpha[t][l] = vals[i];
					}
				}
				else if (0 == strcmp(sVariable_beta, var)) {
					if (0 == strcmp(param, sParam_w_beta)) {
						gW_beta[t][l] = vals[i];
						gW_beta_control[t][l] = (int)cntrl[i];
					}
					else if (0 == strcmp(param, sParam_sigma_MH_beta)) {
						gSigma_MH_beta[t][l] = vals[i];
					}
				}
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::initGlobalSimParams(SEXP sSim_Type, SEXP sGlobal_Sim_Params)
{
	int len = Rf_length(sGlobal_Sim_Params);

	SEXP sParams = R_NilValue;
	SEXP sValues = R_NilValue;
	SEXP sControl = R_NilValue;

	if (strcmp("MH", CHAR(STRING_ELT(sSim_Type, 0))) == 0) {
		gSimType = eSim_Type_MH;
	}
	else {
		gSimType = eSim_Type_SLICE;
	}

    if (len > 0 && isNewList(sGlobal_Sim_Params)) {

        SEXP names = getAttrib(sGlobal_Sim_Params, R_NamesSymbol);

        int i = 0;

        for (i = 0; i < len; i++) {
            if (strcmp(sColValue, CHAR(STRING_ELT(names, i))) == 0) {
                sValues = VECTOR_ELT(sGlobal_Sim_Params, i);
            }
            if (strcmp(sColParam, CHAR(STRING_ELT(names, i))) == 0) {
                sParams = VECTOR_ELT(sGlobal_Sim_Params, i);
            }
            if (strcmp(sColControl, CHAR(STRING_ELT(names, i))) == 0) {
                sControl = VECTOR_ELT(sGlobal_Sim_Params, i);
            }
        }

        len = Rf_length(sParams);

		if (len > 0) {

			double* vals = REAL(sValues);
			double* cntrl = REAL(sControl);

			for (i = 0; i < len; i++) {
				const char *t = CHAR(STRING_ELT(sParams, i));

				if (0 == strcmp(t, sParam_sigma_MH_gamma)) {
					gDefault_Sigma_MH_gamma = vals[i];
				}
				if (0 == strcmp(t, sParam_sigma_MH_theta)) {
					gDefault_Sigma_MH_theta = vals[i];
				}
				if (0 == strcmp(t, sParam_sigma_MH_alpha)) {
					gDefault_Sigma_MH_alpha = vals[i];
				}
				if (0 == strcmp(t, sParam_sigma_MH_beta)) {
					gDefault_Sigma_MH_beta = vals[i];
				}
				if (0 == strcmp(t, sParam_w_gamma)) {
					gDefault_W_gamma = vals[i];
					gDefault_W_gamma_control = cntrl[i];
				}
				if (0 == strcmp(t, sParam_w_alpha)) {
					gDefault_W_alpha = vals[i];
					gDefault_W_alpha_control = cntrl[i];
				}
				if (0 == strcmp(t, sParam_w_beta)) {
					gDefault_W_beta = vals[i];
					gDefault_W_beta_control = cntrl[i];
				}
			}
    	}
    }
}

void bhpmBB_poisson_mc_hier3_lev0::init(SEXP sChains, SEXP sBurnin, SEXP sIter, SEXP sSim_Type, SEXP sMem_Model, SEXP sGlobal_Sim_Params,
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

	initL3Variables(pmu_gamma_0, ptau2_gamma_0,
					pmu_theta_0, ptau2_theta_0,
					palpha_pi, pbeta_pi);

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

	//Rprintf("Simultion Data: %s (%0.6f) %d, %d, %d\n",
	//							sim_type, gSim_Param, gChains, gBurnin, gIter);
	//
	//Rprintf("Clusters: %d\n", gNumClusters);
	//for (l = 0; l < gNumClusters; l++)
	//	Rprintf("\tCluster %d: Contains %d body-systems\n", l, gNumBodySys[l]);
	//
 	//Rprintf("\tMaxBs: %d\n", gMaxBs);
 	//Rprintf("\tMaxAEs: %d\n", gMaxAEs);
	//
	//for (l = 0; l < gNumClusters; l++) {
	//	for (b = 0; b < gMaxBs; b++) {
	//		Rprintf("\tAEs in Cluster: %d, BS: %d = %d\n", l, b, gNAE[l][b]);
	//	}
	//}
	//
	//Rprintf("Control Count Data:\n");
	//for (l = 0; l < gNumClusters; l++) {
	//	Rprintf("\tCluster: %d\n", l);
	//	int b = 0;
	//	for (b = 0; b < gNumBodySys[l]; b++) {
	//		Rprintf("\t\tBody-system: %d\n", b);
	//		int a = 0;
	//		for (a = 0; a < gNAE[l][b]; a++) {
	//			Rprintf("\t\t\tAE: %d - Count: %d\n", a, x[l][b][a]);
	//		}
	//	}
	//}
	//
	//Rprintf("Treatment Count Data:\n");
	//for (l = 0; l < gNumClusters; l++) {
	//	Rprintf("\tCluster: %d\n", l);
	//	for (b = 0; b < gNumBodySys[l]; b++) {
	//		Rprintf("\t\tBody-system: %d\n", b);
	//		int a = 0;
	//		for (a = 0; a < gNAE[l][b]; a++) {
	//			Rprintf("\tAE: %d - Count: %d\n", a, y[l][b][a]);
	//		}
	//	}
	//}
	//
	//Rprintf("Control Exposure Data:\n");
	//for (l = 0; l < gNumClusters; l++) {
	//	Rprintf("\tCluster: %d\n", l);
	//	for (b = 0; b < gNumBodySys[l]; b++) {
	//		Rprintf("\t\tBody-system: %d\n", b);
	//		int a = 0;
	//		for (a = 0; a < gNAE[l][b]; a++) {
	//			Rprintf("\t\t\tAE: %d - Count: %d\n", a, C[l][b][a]);
	//		}
	//	}
	//}
	//
	//Rprintf("Treatment Exposure Data:\n");
	//for (l = 0; l < gNumClusters; l++) {
	//	Rprintf("\tCluster: %d\n", l);
	//	for (b = 0; b < gNumBodySys[l]; b++) {
	//		Rprintf("\t\tBody-system: %d\n", b);
	//		int a = 0;
	//		for (a = 0; a < gNAE[l][b]; a++) {
	//			Rprintf("\t\t\tAE: %d - Count: %d\n", a, T[l][b][a]);
	//		}
	//	}
	//}
	//
	//Rprintf("Theta initialised values:\n");
	//for (c = 0; c < gChains; c++) {
	//	Rprintf("\tChain: %d\n", c);
	//	for (l = 0; l < gNumClusters; l++) {
	//		Rprintf("\tCluster: %d\n", l);
	//		for (b = 0; b < gNumBodySys[l]; b++) {
	//			Rprintf("\t\tBody-system: %d\n", b);
	//			int a = 0;
	//			for (a = 0; a < gNAE[l][b]; a++) {
	//				Rprintf("\t\t\t\t: %0.6f\n", gTheta[c][l][b][a]);
	//			}
	//		}
	//	}
	//}
	//
	//Rprintf("Gamma initialised values:\n");
	//for (c = 0; c < gChains; c++) {
	//	Rprintf("\tChain: %d\n", c);
	//	for (l = 0; l < gNumClusters; l++) {
	//		Rprintf("\tCluster: %d\n", l);
	//		for (b = 0; b < gNumBodySys[l]; b++) {
	//			Rprintf("\t\tBody-system: %d\n", b);
	//			int a = 0;
	//			for (a = 0; a < gNAE[l][b]; a++) {
	//				Rprintf("\t\t\t\t: %0.6f\n", gGamma[c][l][b][a]);
	//			}
	//		}
	//	}
	//}
	//
	//Rprintf("mu.gamma.0 initialised values:\n");
	//for (c = 0; c < gChains; c++) {
	//	for (l = 0; l < gNumClusters; l++) {
	//		Rprintf("\t%0.6f\n", mu_gamma_0[c][l]);
	//	}
	//}
	//
	//Rprintf("mu.theta.0 initialised values:\n");
	//for (c = 0; c < gChains; c++) {
	//	for (l = 0; l < gNumClusters; l++) {
	//		Rprintf("\t%0.6f\n", mu_theta_0[c][l]);
	//	}
	//}
	//
	//Rprintf("tau2.gamma.0 initialised values:\n");
	//for (c = 0; c < gChains; c++) {
	//	for (l = 0; l < gNumClusters; l++) {
	//		Rprintf("\t%0.6f\n", tau2_gamma_0[c][l]);
	//	}
	//}
	//
	//Rprintf("tau2.theta.0 initialised values:\n");
	//for (c = 0; c < gChains; c++) {
	//	for (l = 0; l < gNumClusters; l++) {
	//		Rprintf("\t%0.6f\n", tau2_theta_0[c][l]);
	//	}
	//}
	//
	//Rprintf("sigma2_gamma initialised values:\n");
	//for (c = 0; c < gChains; c++) {
	//	for (l = 0; l < gNumClusters; l++) {
	//		for (b = 0; b < gNumBodySys[l]; b++) {
	//			Rprintf("\t%d %d %d: %0.6f\n", c, l , b, sigma2_gamma[c][l][b]);
	//		}
	//	}
	//}
	//
	//Rprintf("mu.gamma.0.0: %0.6f\n", mu_gamma_0_0);
	//Rprintf("tau2.gamma.0.0: %0.6f\n", tau2_gamma_0_0);
	//Rprintf("mu.theta.0.0: %0.6f\n", mu_theta_0_0);
	//Rprintf("tau2.theta.0.0: %0.6f\n", tau2_theta_0_0);
	//Rprintf("alpha.gamma.0.0: %0.6f\n", alpha_gamma_0_0);
	//Rprintf("beta.gamma.0.0: %0.6f\n", beta_gamma_0_0);
	//Rprintf("alpha.theta.0.0: %0.6f\n", alpha_theta_0_0);
	//Rprintf("beta.theta.0.0: %0.6f\n", beta_theta_0_0);
	//Rprintf("alpha.gamma: %0.6f\n", alpha_gamma);
	//Rprintf("beta.gamma: %0.6f\n", beta_gamma);
	//Rprintf("alpha.theta: %0.6f\n", alpha_theta);
	//Rprintf("beta.theta: %0.6f\n", beta_theta);
}

void bhpmBB_poisson_mc_hier3_lev0::initL3Params(SEXP pmu_gamma_0_0,
SEXP ptau2_gamma_0_0,
					SEXP pmu_theta_0_0, SEXP ptau2_theta_0_0,
					SEXP palpha_gamma_0_0, SEXP pbeta_gamma_0_0,
					SEXP palpha_theta_0_0, SEXP pbeta_theta_0_0,
					SEXP palpha_gamma, SEXP pbeta_gamma,
					SEXP palpha_theta, SEXP pbeta_theta,
					SEXP plambda_alpha, SEXP plambda_beta)
{
	bhpm1a_poisson_mc_hier3_lev0::initL3Params(pmu_gamma_0_0, ptau2_gamma_0_0,
					pmu_theta_0_0, ptau2_theta_0_0,
					palpha_gamma_0_0, pbeta_gamma_0_0,
					palpha_theta_0_0, pbeta_theta_0_0,
					palpha_gamma, pbeta_gamma,
					palpha_theta, pbeta_theta);

	lambda_alpha = *(REAL(plambda_alpha));
	lambda_beta = *(REAL(plambda_beta));
}

void bhpmBB_poisson_mc_hier3_lev0::initL2Variables(SEXP pmu_gamma, SEXP pmu_theta, SEXP psigma2_gamma, SEXP psigma2_theta, SEXP pPi)
{
	int c = 0, t = 0, l = 0, b = 0;

	bhpm1a_poisson_mc_hier3_lev0::initL2Variables(pmu_gamma, pmu_theta, psigma2_gamma, psigma2_theta);

	double* vpi = REAL(pPi);
	gPi = new double***[gChains];
	for (c = 0; c < gChains; c++) {
		gPi[c] = new double**[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			gPi[c][t] = new double*[gNumClusters];
			for (l = 0; l < gNumClusters; l++) {
				gPi[c][t][l] = new double[gMaxBs];
				for (b = 0; b < gMaxBs; b++) {
					gPi[c][t][l][b] = *vpi;
					vpi++;
				}
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::releaseL2Variables()
{
	int c = 0, t = 0, l = 0;

	bhpm1a_poisson_mc_hier3_lev0::releaseL2Variables();

	if (gPi != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				for (l = 0; l < gNumClusters; l++) {
					delete [] gPi[c][t][l];
				}
				delete [] gPi[c][t];
			}
			delete [] gPi[c];
		}
		delete [] gPi;
		gPi = 0;
	}
}

void bhpmBB_poisson_mc_hier3_lev0::initL3Variables(SEXP pmu_gamma_0, SEXP ptau2_gamma_0,
					SEXP pmu_theta_0, SEXP ptau2_theta_0,
					SEXP palpha_pi, SEXP pbeta_pi)
{
	int c = 0, t = 0, l = 0;

	bhpm1a_poisson_mc_hier3_lev0::initL3Variables(pmu_gamma_0, ptau2_gamma_0,
					pmu_theta_0, ptau2_theta_0);

	alpha_pi = new double**[gChains];
	double *valpha_pi = REAL(palpha_pi);
	for (c = 0; c < gChains; c++) {
		alpha_pi[c] = new double*[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			alpha_pi[c][t] = new double [gNumClusters];
			for (l = 0; l < gNumClusters; l++) {
				alpha_pi[c][t][l] = *valpha_pi;
				valpha_pi++;
			}
		}
	}

	beta_pi = new double**[gChains];
	double *vbeta_pi = REAL(pbeta_pi);
	for (c = 0; c < gChains; c++) {
		beta_pi[c] = new double*[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			beta_pi[c][t] = new double [gNumClusters];
			for (l = 0; l < gNumClusters; l++) {
				beta_pi[c][t][l] = *vbeta_pi;
				vbeta_pi++;
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::releaseL3Variables()
{
	int c = 0, t = 0;

	bhpm1a_poisson_mc_hier3_lev0::releaseL3Variables();

	if (alpha_pi != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] alpha_pi[c][t];
			}
			delete [] alpha_pi[c];
		}
		delete [] alpha_pi;
		alpha_pi = NULL;
	}

	if (beta_pi != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] beta_pi[c][t];
			}
			delete [] beta_pi[c];
		}
		delete [] beta_pi;
		beta_pi = NULL;
	}
}

void bhpmBB_poisson_mc_hier3_lev0::initL2Samples()
{
	int c = 0, t = 0, l = 0, b = 0;

	bhpm1a_poisson_mc_hier3_lev0::initL2Samples();

	if (retainSamples(iMonitor_pi))
		gPi_samples = new double ****[gChains];

	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_pi))
			gPi_samples[c] = new double ***[gNumComparators];

		for (t = 0; t < gNumComparators; t++) {
			if (retainSamples(iMonitor_pi))
				gPi_samples[c][t] = new double **[gNumClusters];

			for (l = 0; l < gNumClusters; l++) {
				if (retainSamples(iMonitor_pi))
					gPi_samples[c][t][l] = new double *[gMaxBs];

				for (b = 0; b < gNumBodySys[l]; b++) {
					if (retainSamples(iMonitor_pi))
						gPi_samples[c][t][l][b] =
								new double [(gIter - gBurnin)];
				}
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::releaseL2Samples()
{
	int c = 0, t = 0, l = 0, b = 0;

	bhpm1a_poisson_mc_hier3_lev0::releaseL2Samples();

	if (gPi_samples) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				for (l = 0; l < gNumClusters; l++) {
					for (b = 0; b < gNumBodySys[l]; b++) {
						delete [] gPi_samples[c][t][l][b];
					}
					delete [] gPi_samples[c][t][l];
				}
				delete [] gPi_samples[c][t];
			}
			delete [] gPi_samples[c];
		}
		delete [] gPi_samples;
		gPi_samples = NULL;
	}
}

void bhpmBB_poisson_mc_hier3_lev0::initL3Samples()
{
	int c = 0, t = 0, l = 0;

	bhpm1a_poisson_mc_hier3_lev0::initL3Samples();

	if (retainSamples(iMonitor_alpha_pi))
		alpha_pi_samples = new double ***[gChains];
	if (retainSamples(iMonitor_beta_pi))
		beta_pi_samples = new double ***[gChains];
	for (c = 0; c < gChains; c++) {
		if (retainSamples(iMonitor_alpha_pi))
			alpha_pi_samples[c] = new double **[gNumComparators];
		if (retainSamples(iMonitor_beta_pi))
			beta_pi_samples[c] = new double **[gNumComparators];

		for (t = 0; t < gNumComparators; t++) {
			if (retainSamples(iMonitor_alpha_pi))
				alpha_pi_samples[c][t] = new double *[gNumClusters];
			if (retainSamples(iMonitor_beta_pi))
				beta_pi_samples[c][t] = new double *[gNumClusters];

			for (l = 0; l < gNumClusters; l++) {
				if (retainSamples(iMonitor_alpha_pi))
					alpha_pi_samples[c][t][l] =
									new double [(gIter - gBurnin)];
				if (retainSamples(iMonitor_beta_pi))
					beta_pi_samples[c][t][l] =
									new double [(gIter - gBurnin)];
			}
		}
	}

	alpha_pi_acc = new int**[gChains];
	beta_pi_acc = new int**[gChains];
	for (c = 0; c < gChains; c++) {
		alpha_pi_acc[c] = new int*[gNumComparators];
		beta_pi_acc[c] = new int*[gNumComparators];
		for (t = 0; t < gNumComparators; t++) {
			alpha_pi_acc[c][t] = new int[gNumClusters];
			beta_pi_acc[c][t] = new int[gNumClusters];
			for (l = 0; l < gNumClusters; l++) {
				alpha_pi_acc[c][t][l] = 0;
				beta_pi_acc[c][t][l] = 0;
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::releaseL3Samples()
{
	int c = 0, t = 0, l = 0;

	bhpm1a_poisson_mc_hier3_lev0::releaseL3Samples();

	if (alpha_pi_acc != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] alpha_pi_acc[c][t];
			}
			delete [] alpha_pi_acc[c];
		}

		delete [] alpha_pi_acc;
		alpha_pi_acc = NULL;
	}

	if (beta_pi_acc != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				delete [] beta_pi_acc[c][t];
			}
			delete [] beta_pi_acc[c];
		}

		delete [] beta_pi_acc;
		beta_pi_acc = NULL;
	}

	if (alpha_pi_samples != NULL) {
		for (c = 0; c < gChains; c++) {
			for (t = 0; t < gNumComparators; t++) {
				for (l = 0; l < gNumClusters; l++) {
					delete [] alpha_pi_samples[c][t][l];
				}
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
				for (l = 0; l < gNumClusters; l++) {
					delete [] beta_pi_samples[c][t][l];
				}
				delete [] beta_pi_samples[c][t];
			}
			delete [] beta_pi_samples[c];
		}
		delete [] beta_pi_samples;
		beta_pi_samples = NULL;
	}
}

void bhpmBB_poisson_mc_hier3_lev0::initMonitor(SEXP sMonitor)
{
    int len = Rf_length(sMonitor);

    SEXP sVariables = R_NilValue;
    SEXP sValues = R_NilValue;

    if (isNewList(sMonitor)) {

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
            if (0 == strcmp(t, sMonitor_mu_theta_0)) {
                iMonitor_mu_theta_0 = vals[i];
            }
            if (0 == strcmp(t, sMonitor_mu_gamma_0)) {
                iMonitor_mu_gamma_0 = vals[i];
            }
            if (0 == strcmp(t, sMonitor_tau2_gamma_0)) {
                iMonitor_tau2_gamma_0 = vals[i];
            }
            if (0 == strcmp(t, sMonitor_tau2_theta_0)) {
                iMonitor_tau2_theta_0 = vals[i];
            }
			if (0 == strcmp(t, sMonitor_pi)) {
				iMonitor_pi = vals[i];
			}
			if (0 == strcmp(t, sMonitor_alpha_pi)) {
				iMonitor_alpha_pi = vals[i];
			}
			if (0 == strcmp(t, sMonitor_beta_pi)) {
				iMonitor_beta_pi = vals[i];
			}
		}
	}
}

bhpmBB_poisson_mc_hier3_lev0::~bhpmBB_poisson_mc_hier3_lev0()
{
	//Rprintf("bhpmBB_poisson_mc_hier3_lev0::bhpmBB_poisson_mc_hier3_lev0 - destructor\n");
	release();
}

void bhpmBB_poisson_mc_hier3_lev0::gibbs_sampler()
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

void bhpmBB_poisson_mc_hier3_lev0::simulate_MH()
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

void bhpmBB_poisson_mc_hier3_lev0::simulate_SLICE()
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

double bhpmBB_poisson_mc_hier3_lev0::log_f_alpha_pi(int c, int l, double alpha, int tr)
{
	double f = 0.0;
	double log_pi_sum = 0.0;

	int b = 0;
	for (b = 0; b < gNumBodySys[l]; b++) {
		log_pi_sum += log(gPi[c][tr][l][b]);
	}

	f = ((double)gNumBodySys[l]) * (lgammafn(alpha + beta_pi[c][tr][l]) - lgammafn(alpha))
							+ (alpha - 1.0) * log_pi_sum - alpha * lambda_alpha; 
	return(f);
}

void bhpmBB_poisson_mc_hier3_lev0::sample_alpha_pi_MH(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c< gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			double cand = 0;

		    // alpha_pi is restricted to being greater than zero
		    while (cand <= 1.0) {
#ifdef INDIVIDUAL_RNG
		        GetRNGstate();
#endif
		        cand = rnorm(alpha_pi[c][tr][l], gSigma_MH_alpha[l][tr]);
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

			double f1 = log_f_alpha_pi(c, l, cand, tr);
			double f2 = log_f_alpha_pi(c, l, alpha_pi[c][tr][l], tr);

			double q1 = pnorm((alpha_pi[c][tr][l] - 1)/gSigma_MH_alpha[l][tr], 0, 1, 1, 0);
			double q2 = pnorm((cand - 1)/gSigma_MH_alpha[l][tr], 0, 1, 1, 0);

			double ratio = (exp(f1 - f2))* q1/q2;
			ratio = cMIN(ratio, 1);

		    if (u <= ratio) {
		        alpha_pi[c][tr][l] = cand;
				alpha_pi_acc[c][tr][l] = alpha_pi_acc[c][tr][l] + 1;
			}

			if (iter >= burnin && retainSamples(iMonitor_alpha_pi)) {
				alpha_pi_samples[c][tr][l][iter - burnin] = alpha_pi[c][tr][l];
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::sample_alpha_pi_SLICE(int burnin, int iter, int tr)
{
	int c = 0, i = 0;
	int K = 0, J = 0;

	for (c = 0; c< gChains; c++) {
		for (i = 0; i < gNumClusters; i++) {

			int m = gW_alpha_control[tr][i];

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

			double g = log_f_alpha_pi(c, i , alpha_pi[c][tr][i], tr);
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
			double u = runif(0, gW_alpha[tr][i]);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif

			l = alpha_pi[c][tr][i] - u;
			r = alpha_pi[c][tr][i] + (gW_alpha[tr][i] - u);

			while (J > 0) {
				if (l <= 1.0)
					break;

				if (logy >= log_f_alpha_pi(c, i, l, tr)) {
					break;
				}
				l = l - gW_alpha[tr][i];

				J--;
			}

			while (K > 0) {
				if (logy >= log_f_alpha_pi(c, i, r, tr)) {
                            break;
				}
				r = r + gW_alpha[tr][i];
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

			while (logy >= log_f_alpha_pi(c, i, cand, tr)) {
				if (cand < alpha_pi[c][tr][i]) {
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

			alpha_pi[c][tr][i] = cand;

			if (iter >= burnin && retainSamples(iMonitor_alpha_pi)) {
				alpha_pi_samples[c][tr][i][iter - burnin] = alpha_pi[c][tr][i];
			}
		}
	}
}

double bhpmBB_poisson_mc_hier3_lev0::log_f_beta_pi(int c, int l, double beta, int tr)
{
    double f = 0.0;
    double log_sum = 0.0;

    int b = 0;
    for (b = 0; b < gNumBodySys[l]; b++) {
        log_sum += log(1.0 - gPi[c][tr][l][b]);
    }

    f = ((double)gNumBodySys[l]) * (lgammafn(alpha_pi[c][tr][l] + beta) - lgammafn(beta)) + (beta - 1.0) * log_sum - beta * lambda_beta; 
    return(f);
}

void bhpmBB_poisson_mc_hier3_lev0::sample_beta_pi_MH(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c< gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			double cand = 0.0;

			while (cand <= 1.0) {
#ifdef INDIVIDUAL_RNG
				GetRNGstate();
#endif
				cand = rnorm(beta_pi[c][tr][l], gSigma_MH_beta[tr][l]);
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

			double f1 = log_f_beta_pi(c, l, cand, tr);
			double f2 = log_f_beta_pi(c, l, beta_pi[c][tr][l], tr);

			double q1 = pnorm((beta_pi[c][tr][l] - 1)/gSigma_MH_beta[tr][l], 0, 1, 1, 0);
			double q2 = pnorm((cand - 1)/gSigma_MH_beta[tr][l], 0, 1, 1, 0);

			double ratio = (exp(f1 - f2)) * (q1/q2);

			ratio = cMIN(ratio, 1);

			if (u <= ratio) {
				beta_pi[c][tr][l] = cand;
				beta_pi_acc[c][tr][l] = beta_pi_acc[c][tr][l] + 1;
			}

			if (iter >= burnin && retainSamples(iMonitor_beta_pi)) {
				beta_pi_samples[c][tr][l][iter - burnin] = beta_pi[c][tr][l];
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::sample_beta_pi_SLICE(int burnin, int iter, int tr)
{
	int c = 0, i = 0;
	int K = 0, J = 0;

	for (c = 0; c< gChains; c++) {
		for (i = 0; i < gNumClusters; i++) {

			int m = gW_beta_control[tr][i];

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

			double g = log_f_beta_pi(c, i, beta_pi[c][tr][i], tr);
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
			double u = runif(0, gW_beta[tr][i]);
#ifdef INDIVIDUAL_RNG
			PutRNGstate();
#endif

			l = beta_pi[c][tr][i] - u;
			r = beta_pi[c][tr][i] + (gW_beta[tr][i] - u);

			// beta is retricted to being greater than 1
			// need a do - while loop
			while (J > 0) {
				if (l <= 1.0) {
					l = 1.0;
					break;
				}

				if (logy >= log_f_beta_pi(c, i, l, tr)) {
					break;
				}
				l = l - gW_beta[tr][i];
				J--;
			}

			while (K > 0) {
				if (logy >= log_f_beta_pi(c, i, r, tr)) {
					break;
				}
				r = r + gW_beta[tr][i];
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

			while (logy >= log_f_beta_pi(c, i, cand, tr)) {
				if (cand < beta_pi[c][tr][i]) {
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

			beta_pi[c][tr][i] = cand;

			if (iter >= burnin && retainSamples(iMonitor_beta_pi)) {
				beta_pi_samples[c][tr][i][iter - burnin] = beta_pi[c][tr][i];
			}
		}
	}
}

void bhpmBB_poisson_mc_hier3_lev0::sample_pi(int burnin, int iter, int tr)
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

				double shape1 = alpha_pi[c][tr][l] + (double)theta_zero_count;
				double shape2 = beta_pi[c][tr][l] + (double)gNAE[l][b] - (double)theta_zero_count;

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

void bhpmBB_poisson_mc_hier3_lev0::sample_mu_theta(int burnin, int iter, int tr)
{
	int c = 0, l = 0;

	for (c = 0; c < gChains; c++) {
		for (l = 0; l < gNumClusters; l++) {

			int b = 0;

			for (b = 0; b < gNumBodySys[l]; b++) {

				double t = 0.0;
				int j = 0;
				int Kb = 0;
				for (j = 0; j < gNAE[l][b]; j++) {
					if (gTheta[c][tr][l][b][j] != 0.0) {
						Kb++;
					}
					t += gTheta[c][tr][l][b][j];
				}

				double denom = sigma2_theta[c][tr][l][b] + ((double)Kb)*tau2_theta_0[c][tr][l];

				double mean = (sigma2_theta[c][tr][l][b] * mu_theta_0[c][tr][l] + tau2_theta_0[c][tr][l] * t)/denom;

				double var = (sigma2_theta[c][tr][l][b]*tau2_theta_0[c][tr][l])/denom;

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

void bhpmBB_poisson_mc_hier3_lev0::sample_sigma2_theta(int burnin, int iter, int tr)
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

double bhpmBB_poisson_mc_hier3_lev0::log_q_theta(int l, int b, int j, double p, double theta, double mean, int t)
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


double bhpmBB_poisson_mc_hier3_lev0::log_f_theta(int c, int i, int b, int j, double theta, int tr)
{
	double f1 = 0.0, f2 = 0.0;

	f1 = (((double)y[tr][i][b][j]) * theta) - (exp(gGamma[c][i][b][j] + theta)) * ((double)T[tr][i][b][j]);

	if (theta == 0) {
		f2 = log(gPi[c][tr][i][b]);
	}
	else {
		f2 = log(1 - gPi[c][tr][i][b]) + log(1.0/sqrt(2.0 * M_PI*sigma2_theta[c][tr][i][b]))
				+ ((-1.0/2.0)*(pow(theta -mu_theta[c][tr][i][b], 2.0))/sigma2_theta[c][tr][i][b]);
	}

	double f = f1 + f2;

	return(f);
}

/*
* Sample theta using a MH step as detailed in: Gottardo, Raftery - Markov Chain Monte Carlo
* With Mixtures of Mutually Singular Distributions
* Perform an adaption step to get a good candidate density
*/
void bhpmBB_poisson_mc_hier3_lev0::sample_theta_MH(int burnin, int iter, int tr)
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

double bhpmBB_poisson_mc_hier3_lev0::cMIN(double a, double b)
{
	if (a < b) {
		return a;
	}
	else {
		return b;
	}
}

void bhpmBB_poisson_mc_hier3_lev0::releasePMWeights()
{
	int t = 0, l = 0, b = 0;

	if (gWp != NULL) {

		for (t = 0; t < gNumComparators; t++) {
			for (l = 0; l < gNumClusters; l++) {
				for (b = 0; b < gNumBodySys[l]; b++) {
					delete [] gWp[t][l][b];
				}
				delete [] gWp[t][l];
			}
			delete [] gWp[t];
		}

		delete [] gWp;
		gWp = NULL;
	}
}

void bhpmBB_poisson_mc_hier3_lev0::clear()
{
	release();
	bhpm1a_poisson_mc_hier3_lev0::release();
	bhpm1a_poisson_mc_hier2_lev0::release();
}

void bhpmBB_poisson_mc_hier3_lev0::release()
{
	int t = 0;

	releaseL3Variables();

	releaseL2Variables();

	releaseL2Samples();

	releasePMWeights();

	if (gW_alpha != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			delete [] gW_alpha[t];
		}
		delete [] gW_alpha;
		gW_alpha = NULL;
	}

	if (gW_beta != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			delete [] gW_beta[t];
		}
		delete [] gW_beta;
		gW_beta = NULL;

	}
	if (gW_alpha_control != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			delete [] gW_alpha_control[t];
		}
		delete [] gW_alpha_control;
		gW_alpha_control = NULL;
	}

	if (gW_beta_control != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			delete [] gW_beta_control[t];
		}
		delete [] gW_beta_control;
		gW_beta_control = NULL;
	}

	if (gSigma_MH_alpha != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			delete [] gSigma_MH_alpha[t];
		}
		delete [] gSigma_MH_alpha;
		gSigma_MH_alpha = NULL;
	}

	if (gSigma_MH_beta != NULL) {
		for (t = 0; t < gNumComparators; t++) {
			delete [] gSigma_MH_beta[t];
		}
		delete [] gSigma_MH_beta;
		gSigma_MH_beta = NULL;
	}
}

SEXP bhpmBB_poisson_mc_hier3_lev0::getPiSamples()
{
	SEXP samples = R_NilValue;

	samples = getL2Samples(gPi_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev0::getAlphaPiSamples()
{
	SEXP samples = R_NilValue;

	samples = getL3Samples(alpha_pi_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev0::getBetaPiSamples()
{
	SEXP samples = R_NilValue;

	samples = getL3Samples(beta_pi_samples);

	return samples;
}

SEXP bhpmBB_poisson_mc_hier3_lev0::getAlphaPiAccept()
{
	SEXP acc = R_NilValue;

	acc = getL3Accept(alpha_pi_acc);

	return acc;
}

SEXP bhpmBB_poisson_mc_hier3_lev0::getL3Accept(int** &data)
{
	SEXP acc = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(acc = allocVector(INTSXP, gChains * gNumClusters));

	int i = 0;
	int c = 0;
	for (c = 0; c < gChains; c++) {
		memcpy(INTEGER(acc) + i, data[c], gNumClusters * sizeof(int));

		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 2));

	INTEGER(dim)[0] = gNumClusters;
	INTEGER(dim)[1] = gChains;

	setAttrib(acc, R_DimSymbol, dim);

	UNPROTECT(2);

	return acc;
}

SEXP bhpmBB_poisson_mc_hier3_lev0::getL3Accept(int*** &data)
{
	SEXP acc = R_NilValue;
	SEXP dim = R_NilValue;

	PROTECT(acc = allocVector(INTSXP, gChains * gNumComparators * gNumClusters));

	int i = 0;
	int c = 0;
	int t = 0;
	for (c = 0; c < gChains; c++) {
		for (t = 0; t < gNumComparators; t++) {
			memcpy(INTEGER(acc) + i, data[c][t], gNumClusters * sizeof(int));
			delete [] data[c][t];
		}
		delete [] data[c];
		data[c] = NULL;
	}
	delete [] data;
	data = NULL;

	PROTECT(dim = allocVector(INTSXP, 3));

	INTEGER(dim)[0] = gNumClusters;
	INTEGER(dim)[1] = gNumComparators;
	INTEGER(dim)[2] = gChains;

	setAttrib(acc, R_DimSymbol, dim);

	UNPROTECT(2);

	return acc;
}

SEXP bhpmBB_poisson_mc_hier3_lev0::getBetaPiAccept()
{
	SEXP acc = R_NilValue;

	acc = getL3Accept(beta_pi_acc);

	return acc;
}

void bhpmBB_poisson_mc_hier3_lev0::getPiSamples(int *c, int *l, int* b, double* pi)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int B = (*b) - 1;

	memcpy(pi, gPi_samples[C][L][B], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev0::getAlphaPiSamples(int *c, int *l, double* alpha_pi)
{
	int C = (*c) - 1;
	int L = (*l) - 1;

	memcpy(alpha_pi, alpha_pi_samples[C][L], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev0::getBetaPiSamples(int *c, int *l, double* beta_pi)
{
	int C = (*c) - 1;
	int L = (*l) - 1;

	memcpy(beta_pi, beta_pi_samples[C][L], (gIter - gBurnin)*sizeof(double));
}

void bhpmBB_poisson_mc_hier3_lev0::getAlphaPiAccept(int *c, int *l, int *t, double* acc)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int T = (*t) - 1;

	*acc = alpha_pi_acc[C][T][L];
}

void bhpmBB_poisson_mc_hier3_lev0::getBetaPiAccept(int *c, int* l, int* t,  double* acc)
{
	int C = (*c) - 1;
	int L = (*l) - 1;
	int T = (*t) - 1;

	*acc = beta_pi_acc[C][T][L];
}
