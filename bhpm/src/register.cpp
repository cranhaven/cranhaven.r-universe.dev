#include<cstdlib>
#include<cstdio>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Print.h>
#include <R_ext/Visibility.h>

#include "register.h"
#include "bhpm_exec.h"

//
// Register native methods with R (entry-points into compiled code)
//

static const char *rcsId = "$Id: register.cpp,v 1.7 2019/04/28 13:51:58 clb13102 Exp clb13102 $";

// .C methods
static R_CMethodDef cMethods[] = {
	{"Release_Cluster", (DL_FUNC)&Release_Cluster, 0, NULL},
	{"getGammaAcceptCluster", (DL_FUNC)&getGammaAcceptCluster, 5, NULL},
	{"getGammaSamplesCluster", (DL_FUNC)&getGammaSamplesCluster, 5, NULL},
	{"getMuGammaSamplesCluster", (DL_FUNC)&getMuGammaSamplesCluster, 4, NULL},
	{"getMuThetaSamplesCluster", (DL_FUNC)&getMuThetaSamplesCluster, 4, NULL},
	{"getSigma2GammaSamplesCluster", (DL_FUNC)&getSigma2GammaSamplesCluster, 4, NULL},
	{"getSigma2ThetaSamplesCluster", (DL_FUNC)&getSigma2ThetaSamplesCluster, 4, NULL},
	{"getThetaAcceptCluster", (DL_FUNC)&getThetaAcceptCluster, 5, NULL},
	{"getThetaSamplesCluster", (DL_FUNC)&getThetaSamplesCluster, 5, NULL},
	{NULL, NULL, 0, NULL},
};

// .Call methods
static R_CallMethodDef callMethods[] = {
	{"bhpm1a_cluster_hier2_exec", (DL_FUNC)&bhpm1a_cluster_hier2_exec, 34},				// 34
	{"bhpm1a_poisson_mc_exec", (DL_FUNC)&bhpm1a_poisson_mc_exec, 42},					// 42
	{"bhpmBB_cluster_hier2_exec", (DL_FUNC)&bhpmBB_cluster_hier2_exec, 40},				// 40
	{"bhpmBB_poisson_mc_exec", (DL_FUNC)&bhpmBB_poisson_mc_exec, 50},					// 50
	{"getAlphaPiAcceptClusterAll", (DL_FUNC)&getAlphaPiAcceptClusterAll, 0},			// 0
	{"getAlphaPiSamplesClusterAll", (DL_FUNC)&getAlphaPiSamplesClusterAll, 0},			// 0
	{"getBetaPiAcceptClusterAll", (DL_FUNC)&getBetaPiAcceptClusterAll, 0},				// 0
	{"getBetaPiSamplesClusterAll", (DL_FUNC)&getBetaPiSamplesClusterAll, 0},			// 0
	{"getGammaAcceptClusterAll", (DL_FUNC)&getGammaAcceptClusterAll, 0},				// 0
	{"getGammaSamplesClusterAll", (DL_FUNC)&getGammaSamplesClusterAll, 0},				// 0
	{"getMuGamma0SamplesClusterAll", (DL_FUNC)&getMuGamma0SamplesClusterAll, 0},		// 0
	{"getMuGammaSamplesClusterAll", (DL_FUNC)&getMuGammaSamplesClusterAll, 0},			// 0
	{"getMuTheta0SamplesClusterAll", (DL_FUNC)&getMuTheta0SamplesClusterAll, 0},		// 0
	{"getMuThetaSamplesClusterAll", (DL_FUNC)&getMuThetaSamplesClusterAll, 0},			// 0
	{"getPiSamplesClusterAll", (DL_FUNC)&getPiSamplesClusterAll, 0},					// 0
	{"getSigma2GammaSamplesClusterAll", (DL_FUNC)&getSigma2GammaSamplesClusterAll, 0},	// 0
	{"getSigma2ThetaSamplesClusterAll", (DL_FUNC)&getSigma2ThetaSamplesClusterAll, 0},	// 0
	{"getTau2Gamma0SamplesClusterAll", (DL_FUNC)&getTau2Gamma0SamplesClusterAll, 0},	// 0
	{"getTau2Theta0SamplesClusterAll", (DL_FUNC)&getTau2Theta0SamplesClusterAll, 0},	// 0
	{"getThetaAcceptClusterAll", (DL_FUNC)&getThetaAcceptClusterAll, 0},				// 0
	{"getThetaSamplesClusterAll", (DL_FUNC)&getThetaSamplesClusterAll, 0},				// 0
    {NULL, NULL, 0}
};

// Register the methods with R
void attribute_visible R_init_bhpm(DllInfo *info)
{
	// Register the entry-points with R
	R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
	// Ensure only registered entry-points can be called by name e.g. .C("Release_Cluster")
	R_useDynamicSymbols(info, FALSE);
}
