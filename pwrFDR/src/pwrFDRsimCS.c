#include <R.h>
#include <Rmath.h>

typedef struct{
  int index;
  double X;
  double pval;
  int HA;
} XPHindxd;

/* double rbinom(double nin, double pp) */
/* s2.power=v.S/(m.1[k]^2*nsim) */

int cmprXPH(const void *, const void *);

void pwrFDRsimCS(int *pnsim, double *pFDR, int *pcntlFDF, int *pRomano, double *pFDRst,
		 int *pm, double *pr1, int *pn, double *ptheta, double *prho, int *pnWC, double *pdelta,
		 int *pverb, int *pM, int *pR, int *pT, int *pRst, int *pTst, double *pp0ht, int *pRR,
		 int *pTR, double *pX_i, int *pM_i)
{
    int nsim=*pnsim, m=*pm, nWC=*pnWC, icntlFDF=*pcntlFDF, iRomano=*pRomano, m1, ii, j, R_, Rst_, cT_;
    int cTst_, n=*pn, done, h, k, nC, verb;
    double r1=*pr1, r0ht, xN, xn, X_j, FDR=*pFDR, FDRst=*pFDRst, theta=*ptheta;
    double  rtnth, U, v, W, Z, abs_rho, sgn_rho, tau, sig, pv, bhfdrcrit, rmnocrit;
    double smIpvlgthlf, delta=*pdelta;
    XPHindxd *pXPH;

    verb=*pverb;
    abs_rho=fabs(*prho);
    sgn_rho=sign(*prho);

    pXPH = (XPHindxd *)Calloc(m, XPHindxd);

    GetRNGstate();
    xN = (double)m;
    xn = (double)n;
    nC = m/nWC;
    rtnth = pow(xn/2.0, 0.5)*theta;
    tau = pow(abs_rho, 0.5);
    sig = pow(1.0-abs_rho, 0.5);
    if(verb) Rprintf("sig=%g, tau=%g\n", sig, tau);
    // qnorm5(double p, double mu, double sigma, int lower_tail, int log_p)
    // pnorm5(double x, double mu, double sigma, int lower_tail, int log_p)

    for(ii=0;ii<nsim;ii++)
    {
      *(pM+ii) = m1 = (int) rbinom(xN, r1);
      if(ii==0) *pM_i = m1;
      for(h=0;h<nC;h++)
      {
	/* simulate the Z for this cluster */
	U = unif_rand();
	Z = sgn_rho*qnorm5(U, 0.0, tau, 0, 0);
        for(k=0;k<nWC;k++)
        {
	  j = nWC*h + k;
	  (pXPH+j)->index = (j+1);
	  U = unif_rand();
          W = qnorm(U, 0.0, sig, 0, 0);
          if(verb) Rprintf("ii=%d, j=%d, W=%g, Z=%g\n", ii, j, W, Z);
	  X_j = W + Z;
	  if(j < m1) X_j = X_j + rtnth;
          if(ii==0) *(pX_i + j) = X_j;

	  (pXPH+j)->X = X_j;
	  (pXPH+j)->pval = 2.0*pnorm5(fabs(X_j), 0.0, 1.0, 0, 0);
	  (pXPH+j)->HA = ((j+1)<=m1);
	}
      }
      
      qsort(pXPH, m, sizeof(XPHindxd), &cmprXPH);
      
      R_=0;
      cT_=0;
      done=0;
      j=0;
      while(!done && j < m)
      {
	pv = (pXPH+m-j-1)->pval;
	bhfdrcrit = (FDR*((double)(m-j))/xN);
	if(pv <= bhfdrcrit)
	{
	  R_ = m - j;
	  done = 1;
	}
	j++;
      }
      for(j=0;j<R_;j++) cT_+= (pXPH+j)->HA;
      *(pR + ii) = R_;
      *(pT + ii) = cT_;

      if(icntlFDF>=1)
      {
        Rst_=0;
        cTst_=0;
        done=0;
	j=0;
	while(!done && j < m)
        {
          pv = (pXPH+m-j-1)->pval;
	  bhfdrcrit = (FDRst*((double)(m-j))/xN);
	  if(pv <= bhfdrcrit)
	  {
	    Rst_ = m - j;
	    done = 1;
	  }
	  j++;
	}
        for(j=0;j<Rst_;j++) cTst_ += (pXPH+j)->HA;
        *(pRst + ii) = Rst_;
        *(pTst + ii) = cTst_;
      }
      
      if(icntlFDF==2)
      {
	smIpvlgthlf=0.0;
        for(j=0;j<m;j++) smIpvlgthlf+= ((double) ((pXPH+j)->pval > 0.5));
	r0ht = 2.0*smIpvlgthlf/xN;
	*(pp0ht + ii) = r0ht;
      }
      if(iRomano)
      {
        R_=0;
        cT_=0;
        done=0;
	j=0;
	while(!done && j < m)
        {
          pv = (pXPH+j)->pval;
	  rmnocrit = (floor(delta*j) + 1)*FDR/(m + floor(delta*j) + 1 - j);
	  if(pv > rmnocrit)
	  {
	    R_ = j;
	    done = 1;
	  }
	  j++;
	}
        for(j=0;j<R_;j++) cT_ += (pXPH+j)->HA;
        *(pRR + ii) = R_;
        *(pTR + ii) = cT_;
      }
    }
    PutRNGstate();
    Free(pXPH);
}
