#include "gen_lat_func.h"
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
extern "C" {
  #include "FL_vit.h"
}


/*************************************************************************/
/*************************************************************************/
//GENERAL FUNCTIONS.
/*************************************************************************/
/*************************************************************************/

/*************************************************************************/
//Useful for keeping track of estimates over iterations.
/*************************************************************************/
void CopyAtoB(double* pA, double* pB, int nel) {

  for (int i = 0; i < nel; i++) {
    pB[i] = pA[i];
  }

}

/*************************************************************************/
//Mainly used in estimating the Theta matrix.
/*************************************************************************/
void SqColSums(double* pmat, double* pmatSqCS, int nr, int nc) {

  for (int j = 0; j < nc; j++) {
    pmatSqCS[j] = 0.0;
    for (int i = 0; i < nr; i++) {
      pmatSqCS[j] += pmat[i+j*nr]*pmat[i+j*nr];
    }
  }

}

/*************************************************************************/
//Mainly used in estimating the Beta matrix.
/*************************************************************************/
void SqRowSums(double* pmat, double* pmatSqRS, int nr, int nc) {

  for (int i = 0; i < nr; i++) {
    pmatSqRS[i] = 0.0;
    for (int j = 0; j < nc; j++) {
      pmatSqRS[i] += pmat[i+j*nr]*pmat[i+j*nr];
    }
  }

}

/*************************************************************************/
//Used in calculating matrix error and checking for zero matrices.
/*************************************************************************/
double SqTotSum(double* pmat, int nel) {

  double ans = 0.0;
  for (int i = 0; i < nel; i++) {
    ans += pmat[i]*pmat[i];
  }
  return(ans);

}

/*************************************************************************/
//Used for checking convergence over iterations.
/*************************************************************************/
double MatErr(double* pnewMat, double* poldMat, int nel, double thresh) {

  double res, oldSqS = SqTotSum(poldMat,nel),
    newSqS = SqTotSum(pnewMat,nel);

  if ((oldSqS==0.0)&&(newSqS==0.0)) {
    res = 0.0;
  } else if (oldSqS==0.0) {
    res = thresh+1.0;
  } else {
    double den = 0.0, num = 0.0;
    for (int i = 0; i < nel; i++) {
      num += (pnewMat[i]-poldMat[i])*(pnewMat[i]-poldMat[i]);
      den += poldMat[i]*poldMat[i];
    }
    res = std::sqrt(num/den);
  }

  return(res);

}

/*************************************************************************/
//Used in estimating Beta.
/*************************************************************************/
double SoftThresh(double val, double thresh) {
  double t1;
  t1 = sign(val);
  double t2;
  t2 = ((std::fabs(val) - thresh) >= 0) ? std::fabs(val) - thresh : 0.0;
  return(t1*t2);
}


/*************************************************************************/
/*************************************************************************/
//FUNCTIONS SPECIFIC TO LATENT FEATURE MODEL.
/*************************************************************************/
/*************************************************************************/

/*************************************************************************/
//This quantity is required in estimating the Theta matrix.
/*************************************************************************/
void MakeTldY(double* ptldY, double* pY, double* pB, double* pnewT,
	      int j, int S, int L, int J) {
  for (int s = 0; s < S; s++) {
    ptldY[s] = 0.0;
    for (int l = 0; l < L; l++) {
      double term = BTljsSum(pB,pnewT,s,l,j,S,L,J);
      ptldY[s] += pB[l+j*L]*(pY[l+s*L] - term);
    }
  }
}

/*************************************************************************/
//This quantity is required in estimating the Beta matrix.
/*************************************************************************/
void MakeGrvY(double* pgrvY, double* pY, double* pT, double* pnewB,
	      double* pTSqRS, int j, int S, int L, int J) {
  for (int l = 0; l < L; l++) {
    double term1 = 0.0;
    for (int s = 0; s < S; s++) {
      double term2 = BTljsSum(pnewB,pT,s,l,j,S,L,J);
      term1 += pT[j+s*J]*(pY[l+s*L] - term2);
    }
    pgrvY[l] = term1/pTSqRS[j];
  }
}

/*************************************************************************/
//This quantity is used in the MakeTldY() and MakeGrvY() functions.
/*************************************************************************/
double BTljsSum(double* pB, double* pT, int s, int l, int j, int S, int L,
		int J) {
  double res = 0.0;
  for (int k = 0; k < j; k++) {
    res += pB[l+k*L]*pT[k+s*J];
  }
  for (int k = j + 1; k < J; k++) {
    res += pB[l+k*L]*pT[k+s*J];
  }
  return(res);
}

/*************************************************************************/
//The estimated Beta matrix after either convergence or the iteration limit
//is reached.
/*************************************************************************/
void BC(double* pnewB, double* pY, double* pT, double rlam1, double rlam2,
	double rthresh, int imaxiter, int S, int L, int J) {

  //The previous Beta to keep track of changes through iterations.
  double *poldB = new double[L*J];
  CopyAtoB(pnewB,poldB,L*J);

  double *pTSqRS = new double[J];
  SqRowSums(pT,pTSqRS,J,S);

  double err = rthresh+1.0, oldBSqS = SqTotSum(poldB,L*J);
  int niter = 0;
  while ((err>rthresh)&&(niter<imaxiter)&&(oldBSqS!=0.0)) {
    UpdateBC(pnewB,pY,pT,pTSqRS,rlam1,rlam2,S,L,J);
    err = MatErr(pnewB,poldB,L*J,rthresh);
    CopyAtoB(pnewB,poldB,L*J);
    oldBSqS = SqTotSum(poldB,L*J);
    niter++;
  }

  delete [] pTSqRS;
  delete [] poldB;

}

/************************************************************************/
//Updates the Beta matrix for each iteration.  Uses the Viterbi algorithm
//for solving the FLSA developed by Nick Johnson.
/************************************************************************/
void UpdateBC(double* pnewB, double* pY, double* pT, double* pTSqRS,
	      double rlam1, double rlam2, int S, int L, int J) {

  int nProt = 0;

  SEXP grvY = R_NilValue, nlam2 = R_NilValue, maxSegs = R_NilValue;
  PROTECT(grvY = allocVector(REALSXP,L));
  nProt++;
  PROTECT(nlam2 = allocVector(REALSXP,1));
  nProt++;
  PROTECT(maxSegs = allocVector(INTSXP,1));
  nProt++;
  INTEGER(maxSegs)[0] = 1000;
  double *pgrvY = REAL(grvY), *pnlam2 = REAL(nlam2);

  for (int j = 0; j < J; j++) {

    if (pTSqRS[j]==0.0) {
      for(int l = 0; l < L; l++) {
	pnewB[l+j*L] = 0.0;
      }
    } else {
      pnlam2[0] = rlam2/pTSqRS[j];
      MakeGrvY(pgrvY,pY,pT,pnewB,pTSqRS,j,S,L,J);
      SEXP retPath = R_NilValue, segmentVec = R_NilValue,
	noNeed = R_NilValue;
      PROTECT(retPath = allocVector(VECSXP,1));
      PROTECT(segmentVec = allocVector(VECSXP,1));
      PROTECT(noNeed =
	      L2L1VitPath(grvY,nlam2,retPath,maxSegs,segmentVec));
      double *pretPath = REAL(VECTOR_ELT(retPath,0));
      int *psegmentVec = INTEGER(VECTOR_ELT(segmentVec,0));
      int nSegs = ncols(VECTOR_ELT(segmentVec,0));
      for (int seg = 0; seg < nSegs; seg++) {
	for (int l = psegmentVec[2*seg]-1; l < psegmentVec[2*seg+1]; l++) {
	  pnewB[l+j*L] = SoftThresh(pretPath[seg],rlam1/(pTSqRS[j]*2.0));
	}
      }
      UNPROTECT(3);
    }
  }

  UNPROTECT(nProt);

}

/***********************************************************************/
//The estimated Theta matrix after either convergence or the iteration
//limit is reached.
/***********************************************************************/
int TLatL2C(double* pnewT, double* pY, double* pB, double rthresh,
	     int imaxiter, double rsT, int S, int L, int J) {

  //The previous Theta to keep track of changes through iterations.
  double *poldT = new double[J*S];
  CopyAtoB(pnewT,poldT,J*S);

  double *pBSqCS = new double[J];
  SqColSums(pB,pBSqCS,L,J);

  double err = rthresh+1.0;
  int niter = 0;
  while ((err>rthresh)&&(niter<imaxiter)) {
    UpdateTLatL2C(pnewT,pY,pB,pBSqCS,rsT,S,L,J);
    err = MatErr(pnewT,poldT,J*S,rthresh);
    CopyAtoB(pnewT,poldT,J*S);
    niter++;
  }

  delete [] pBSqCS;
  delete [] poldT;

  return(niter);

}

/***********************************************************************/
////Updates the Theta matrix for each iteration.
/***********************************************************************/
void UpdateTLatL2C(double* pnewT, double* pY, double* pB,
		   double* pBSqCS, double rsT, int S, int L, int J) {

  double *ptldY = new double[S];

  for (int j = 0; j < J; j++) {

    MakeTldY(ptldY,pY,pB,pnewT,j,S,L,J);
    double tldYSqS = 0.0, tldYnrm;
    for (int s = 0; s < S; s++) {
      tldYSqS += ptldY[s]*ptldY[s];
    }
    tldYnrm = std::sqrt(tldYSqS);

    if (pBSqCS[j]==0) {
      for (int s = 0; s < S; s++) {
	pnewT[j+J*s] = 0.0;
      }
    } else if (tldYnrm <= std::sqrt(rsT)*(pBSqCS[j])) {
      for (int s = 0; s < S; s++) {
	pnewT[j+J*s] = ptldY[s]/pBSqCS[j];
      }
    } else {
      for (int s = 0; s < S; s++) {
	pnewT[j+J*s] = std::sqrt(rsT)*ptldY[s]/tldYnrm;
      }
    }

  }
  delete [] ptldY;
}

/************************************************************************/
//Mainly used for calculating the PVE.
/************************************************************************/
double LatRSS(double* pY, double* pB, double* pT, int S, int L, int J) {

  double term1 = 0.0;

  for (int lY = 0; lY < L; lY++) {
    for (int sY = 0; sY < S; sY++) {

    double term2 = 0.0;
    for (int j = 0; j < J; j++) {
      term2 += pB[lY+j*L]*pT[j+sY*J];
    }

    term1 += (pY[lY+sY*L] - term2)*(pY[lY+sY*L] - term2);

    }
  }

  return(term1);

}

/************************************************************************/
//Used for finding the optimal values of lambda1 and lambda2.
/************************************************************************/
double LatBIC(double rss, double* pB, int S, int L, int J) {

  int nparms = 0;
  double bic;

  for (int j = 0; j < J; j++) {
    double *pBcol = new double[L];
    for (int l = 0; l < L; l++) {
      pBcol[l] = pB[l+j*L];
    }
    R_rsort(pBcol,L);
    for (int l = 0; l< L; l++) {
      if ((l==0)&&(pBcol[l]!=0)) {
	  nparms++;
      } else if ((pBcol[l]!=0)&&(pBcol[l]!=pBcol[l-1])) {
	  nparms++;
      }
    }
    delete [] pBcol;
  }

  bic = std::log(S*L*1.0)*nparms + S*L*1.0*std::log(rss/(S*L));

  return(bic);

}
