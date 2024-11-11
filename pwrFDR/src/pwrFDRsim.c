#include <R.h>
#include <Rmath.h>

typedef struct{
  int opt;
  double p1;
  double p2;
  double p3;
} distpars;

typedef struct{
  int index;
  double X;
  double pval;
  int HA;
} XPHindxd;

double pdist(double x, distpars *par)
{
  double xncp, sig, xdf1, xdf2, ans=0.0;
  if(par->opt == 0)                                         
  {
    xncp=par->p1;
    sig=par->p2;
    ans=pnorm5(x, xncp, sig, 1, 0);
  }                                                         
  if(par->opt == 1)
  {                                                         
    xncp=par->p1;
    xdf1=par->p2;
    if(fabs(xncp)>1e-6) ans=pnt(x,xdf1,xncp,1,0);
    if(fabs(xncp)<=1e-6) ans=pt(x,xdf1,1,0);
  }
  if(par->opt == 2)
  {                                                         
    xncp=par->p1;
    xdf1=par->p2;
    xdf2=par->p3;
    if(fabs(xncp)>1e-6) ans=pnf(x,xdf1,xdf2,xncp,1,0);
    if(fabs(xncp)<=1e-6) ans=pf(x,xdf1,xdf2,1,0);
  }
  return(ans);
}
// double rnorm(double mu, double sigma)
// double rchisq(double df)
// double rnchisq(double df, double lambda)


double rdist(distpars *par)
{
  double xncp, sig, xdf1, xdf2, ans=0.0;
  if(par->opt == 0)                                         
  {
    xncp=par->p1;
    sig=par->p2;
    ans=rnorm(xncp, sig);
  }                                                         
  if(par->opt == 1)
  {                                                         
    xncp=par->p1;
    xdf1=par->p2;
    if(fabs(xncp)>1e-6) ans=rnorm(xncp, 1.0)/pow(rchisq(xdf1)/xdf1, 0.5);
    if(fabs(xncp)<=1e-6) ans=rt(xdf1);
  }
  if(par->opt == 2)
  {                                                         
    xncp=par->p1;
    xdf1=par->p2;
    xdf2=par->p3;
    if(fabs(xncp)>1e-6) ans=rnchisq(xdf1,xncp)/rchisq(xdf2);
    if(fabs(xncp)<=1e-6) ans=rf(xdf1,xdf2);
  }
  return(ans);
}



// qnt(double p, double df, double ncp, int lower_tail, int log_p)
// pnt(double t, double df, double ncp, int lower_tail, int log_p)
// pt(double x, double n, int lower_tail, int log_p)
// qnorm5(double p, double mu, double sigma, int lower_tail, int log_p)
// pnorm5(double x, double mu, double sigma, int lower_tail, int log_p)

int cmprXPH(const void *, const void *);

void pwrFDRsim(int *pnsim, double *pFDR, int *pcntlFDF, int *pRomano, double *pFDRst,
	       int *pm, double *pr1, int *pn, double *ptheta, int *pdistopt, double *pgroups, double *pdelta,
	       int *pverb, int *pM, int *pR, int *pT, int *pRst, int *pTst, double *pp0ht, int *pRR, int *pTR,
	       double *pX_i, int *pM_i)
{
    int nsim=*pnsim, m=*pm, icntlFDF=*pcntlFDF, iRomano=*pRomano, M_i, ii, j, R_, Rst_, cT_;
    int cTst_, n=*pn, done, verb=*pverb, idistopt=*pdistopt;
    double r1=*pr1, r0ht, xm, xn, xgroups=*pgroups, TWO, X_j, FDR=*pFDR, FDRst=*pFDRst;
    double smIpvlgthlf, theta=*ptheta, U, xncp=0.0, v, pv;
    double bhfdrcrit, rmnocrit, delta=*pdelta;
    XPHindxd *pXPH;
    distpars *par0, *par1;

    par0 = (distpars *) Calloc(1, distpars);
    par1 = (distpars *) Calloc(1, distpars);
    pXPH = (XPHindxd *)Calloc(m, XPHindxd);

    GetRNGstate();
    xm = (double)m;
    xn = (double)n;
    
    par0->opt  = par1->opt  = idistopt;
    /* Rprintf("nsim: %d, FDR: %g, m: %d, r1: %g, n: %d, theta: %g, distopt: %d, groups: %g\n",
               *pnsim, *pFDR, *pm, *pr1, *pn, *ptheta, *pdistopt, *pgroups); */
    if(idistopt==0)
    {
      xncp = pow(xn/2.0, 0.5)*theta;
      par0->p1 = par1->p1 = 0.0;
      par0->p2 = par1->p2  = 1.0;
    }
    if(idistopt==1)
    {
      xncp = pow(xn/2.0, 0.5)*theta;
      par0->p1 = par1->p1 = 0.0;
      par0->p2 = par1->p2 = 2.0*xn - 2.0;
    }
    if(idistopt==2)
    {
      xncp = (double) xn/2.0*theta*theta;
      par0->p1 = par1->p1 = 0.0;
      par0->p2 = par1->p2 = xgroups-1.0;
      par0->p3 = par1->p3 = xgroups*(xn - 1.0);
    }
    /* Rprintf("xncp: %g, par0->opt: %d, par0->p1: %g, par0->p2: %g, par1->opt: %d, par1->p1: %g, par1->p2: %g\n",
               xncp, par0->opt, par0->p1, par0->p2,  par1->opt, par1->p1, par1->p2); */

    par0->p1 = par1->p1 = 0.0;
    par1->p1 = xncp;
    /* Rprintf("xncp: %g, par0->opt: %d, par0->p1: %g, par0->p2: %g, par1->opt: %d, par1->p1: %g, par1->p2: %g\n",
	    xncp, par0->opt, par0->p1, par0->p2,  par1->opt, par1->p1, par1->p2);
       Rprintf("U: %g, X_j: %g\n", U, X_j); */
    
    TWO = 2.0;
    if(idistopt==2) TWO = 1.0;
    
    for(ii=0;ii<nsim;ii++)
    {
      *(pM+ii) = M_i = (int) rbinom(xm, r1);
      if(ii==0) *pM_i = M_i;
      for(j=0;j<m;j++)
      {
        par0->p1 = par1->p1 = 0.0;
        if(j < M_i) par1->p1 = xncp;

	X_j = rdist(par1);
	
        if(ii==0) *(pX_i + j) = X_j;

        (pXPH+j)->index = (j+1);
        (pXPH+j)->X = X_j;
        (pXPH+j)->pval = TWO*(1.0 - pdist(fabs(X_j), par0));
        (pXPH+j)->HA = ((j+1)<=M_i);
      }
      qsort(pXPH, m, sizeof(XPHindxd), &cmprXPH);
      
      R_=0;
      cT_=0;
      done=0;
      j=0;
      while(!done && j < m)
      {
	pv = (pXPH+m-j-1)->pval;
	bhfdrcrit = (FDR*((double)(m-j))/xm);
        /* Rprintf("ii=%d, j=%d, pval=%f, crit=%f\n", ii, j, pv, bhfdrcrit); */
	if(pv <= bhfdrcrit)
	{
	  R_ = m - j;
	  done = 1;
	}
	j++;
      }
      for(j=0;j<R_;j++) cT_ += (pXPH+j)->HA;
      *(pR + ii) = R_;
      *(pT + ii) = cT_;
      /*      Rprintf("R=%d, S=%d\n", R_, cT_);*/

      if(icntlFDF>=1)
      {
        Rst_=0;
        cTst_=0;
        done=0;
	j=0;
	while(!done && j < m)
        {
          pv = (pXPH+m-j-1)->pval;
	  bhfdrcrit = (FDRst*((double)(m-j))/xm);
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
        for(j=0;j<m;j++) smIpvlgthlf+= ((double) (1*((pXPH+j)->pval > 0.5)));
	r0ht = 2.0*smIpvlgthlf/xm;
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
    Free(par0);
    Free(par1);
    Free(pXPH);
}

int cmprXPH(const void *x, const void *y)
{
  XPHindxd *xx, *yy;
  xx = (XPHindxd *) x;
  yy = (XPHindxd *) y;

  return(1*((xx->pval) > (yy->pval)) - 1*((xx->pval) < (yy->pval)));
}
