#include<Rmath.h>
#include"oeiinfo.h"
#include"nleqslv.h"

#define CUBE(x) ((x)*(x)*(x))
#define SQR(x) ((x)*(x))
#define SGN(x) ((double)((0.0<x)-(0.0>x)))
#define SQRPI2 2.50662827463
#define LOG6 1.79175946923

double transfun(double x, double upb)
{
  double y = x>0? upb - exp(-x): x+upb-1.0;
  return y;
}
double dtransfun(double x)
{
  double y = x>0? exp(-x): 1.0;
  return y;
}

double oeidkappaSeq(double x, void* param)
{
  parOei *ppar = (parOei*) param;
  int i, p, L;
  double b, iomemu2, barval, dkappa;
  double f1, f2, f3, f4, f1sq, dlmp;
  const double* avec;
  const double* mub2star;
  p = ppar -> p;
  L = ppar -> L;
  b = ppar -> b;
  iomemu2 = ppar -> iomemu2;
  barval = ppar -> barval;
  avec = ppar -> avec;
  mub2star = ppar -> mub2star;
  f1 = 1.0 - 2.0*x*b;
  f1sq = f1*f1;
  dlmp = (double)(L-p);
  dkappa = b*iomemu2/f1+dlmp*b/f1;
  for(i=0; i<p; ++i)
  {
    f2 = 1.0 - 2.0*avec[i]*x;
    f3 = 1.0 - 4.0*avec[i]*b*x*x;
    f4 = f1sq * f2 * f2;
    dkappa += mub2star[i]*f3/f4;
    dkappa += avec[i]/f2;
  }
  return dkappa - barval;
}

double oeidkappa2(double x, void* param)
{
  int i, p, L;
  double b, iomemu2, f1, f2, f3;
  double ap2, bp2, f1p2, f1p3, f2p2, f2p3, xp3;
  double dlmp, dkappa2;
  const double *avec;
  const double *mub2star;
  parOei *ppar = (parOei*) param;

  p = ppar -> p;
  L = ppar -> L;
  b = ppar -> b;
  iomemu2 = ppar -> iomemu2;
  avec = ppar -> avec;
  mub2star = ppar -> mub2star;

  f1 = 1.0 - 2.0*x*b;
  bp2 = b*b;
  f1p2 = SQR(f1);
  f1p3 = f1p2*f1;
  xp3 = CUBE(x);
  dlmp = (double)(L-p);
  dkappa2 = 4.0*bp2/f1p3*iomemu2+2.0*dlmp*bp2/f1p2;
  for(i=0; i<p; ++i)
  {
    ap2 = SQR(avec[i]);
    f2 = 1.0 - 2.0*avec[i]*x;
    f2p2 = SQR(f2);
    f2p3 = f2*f2p2;
    f3 = avec[i]+b-6.0*avec[i]*b*x + 8.0*ap2*bp2*xp3;
    dkappa2 += 2.0*ap2/f2p2;
    dkappa2 += 4.0*mub2star[i]*f3/f1p3/f2p3;
  }
  return dkappa2;
}
void oeidkappadd(double x, void* param,
		 double *fv, double *dfv)
{
  int i, p, L;
  double b, iomemu2, barval;
  double f1, f2, f3, f4;
  double ap2, bp2, f1p2, f1p3, f2p2, f2p3, xp2, xp3;
  double dlmp, dkappa, dkappa2;
  const double *avec;
  const double *mub2star;
  parOei *ppar = (parOei*) param;

  p = ppar -> p;
  L = ppar -> L;
  b = ppar -> b;
  iomemu2 = ppar -> iomemu2;
  barval = ppar -> barval;
  avec = ppar -> avec;
  mub2star = ppar -> mub2star;

  f1 = 1.0 - 2.0*x*b;
  f1p2 = SQR(f1);
  f1p3 = f1p2 * f1;
  bp2 = b*b;
  xp2 = SQR(x);
  xp3 = xp2*x;
  dlmp = (double)(L-p);
  dkappa = b*iomemu2/f1 + dlmp*b/f1;
  dkappa2 = 4.0*bp2/f1p3*iomemu2+2.0*dlmp*bp2/f1p2;
  for(i = 0; i < p; ++i)
  {
    ap2 = SQR(avec[i]);
    f2 = 1.0 - 2.0*avec[i]*x;
    f2p2 = SQR(f2);
    f2p3 = f2*f2p2;
    f3 = 1.0 - 4.0*avec[i]*b*xp2;
    f4 = avec[i]+b-6.0*avec[i]*b*x + 8.0*ap2*bp2*xp3;
    dkappa += avec[i]/f2 + mub2star[i]*f3/f1p2/f2p2;
    dkappa2 += 2.0*ap2/f2p2;
    dkappa2 += 4.0*mub2star[i]*f4/f1p3/f2p3;
  }
  *fv = dkappa - barval;
  *dfv = dkappa2;
}
void oeikappafs(double x, double b, double iomemu2,
		const double* avec, const double* mub2star,
		int p, int L, double* kappa, double* kappa2,
		double *kappa3)
{
  double f1, f2, f3, f4, dlmp;
  double f1p3, f1p4, f2p3, f2p4;
  double bp2, bp3, ap2, ap3;
  double xp2, xp3, xp4;
  double k, k2, k3;
  int i;
  f1 = 1.0 - 2.0*b*x;
  dlmp = (double) (L-p);
  f1p3 = CUBE(f1);
  f1p4 = f1p3 * f1;
  bp2 = SQR(b);
  bp3 = bp2 * b;
  xp2 = SQR(x);
  xp3 = xp2 * x;
  xp4 = xp2 * xp2;
  k = b*x*iomemu2/f1 - 0.5*dlmp*log(f1);
  k2 = 4.0*bp2/f1p3 * iomemu2+2.0*dlmp*bp2/SQR(f1);
  k3 = 24.0*bp3/f1p4*iomemu2 + 8.0*dlmp*bp3/f1p3;
  for(i = 0; i <p; ++i)
  {
    ap2 = SQR(avec[i]);
    ap3 = ap2 * avec[i];
    f2 = 1.0 - 2.0*avec[i]*x;
    f2p3 = CUBE(f2);
    f2p4 = f2p3 * f2;
    f3 = avec[i] + b - 6.0*avec[i]*b*x + 8.0*bp2*ap2*xp3;
    f4 = ap2 + bp2 + avec[i] * b - 8.0*avec[i]*bp2*x - 8.0*ap2*b*x;
    f4 += 16.0*ap2*bp2*xp2 + 16.0*ap3*bp2*xp3 + 16.0*ap2*bp3*xp3;
    f4 -= 48.0 * ap3* bp3 * xp4;
    k += mub2star[i]*x/f1/f2 - 0.5 * log(f2);
    k2 += 2.0 * ap2/SQR(f2);
    k2 += 4.0 * mub2star[i]*f3/f1p3/f2p3;
    k3 += 8.0 * ap3/f2p3;
    k3 += 24.0*mub2star[i]*f4/f2p4/f1p4;
  }
  *kappa = k;
  *kappa2 = k2;
  *kappa3 = k3;
}
double transoeidkappaSeq(double x, void* param)
{
  double xt, upb, ret;
  parOei *ppar = (parOei*) param;
  upb = ppar -> upb;
  xt = transfun(x, upb);
  ret = oeidkappaSeq(xt, param);
  return ret;
}
double transoeidkappa2(double x, void* param)
{
  double xt, dxt, upb, ret;
  parOei *ppar = (parOei*) param;
  upb = ppar -> upb;
  xt = transfun(x,upb);
  dxt = dtransfun(x);
  ret = oeidkappa2(xt, param);
  ret *= dxt;
  return ret;
}
double posapprox(double tval, double zz, double ww,
		 double dk2, double lambda)
{
  double zz2 = zz*zz;
  double ww2 = ww*ww;
  double idx = 0.5*zz2-0.5*ww2;
  double idxp1 = -0.5*ww2+0.5*log(dk2)-log(SQRPI2);
  double pnzz = 1.0-pnorm(zz,0.0,1.0,1,0);
  double idxp2 = idx + log(dk2) + log(pnzz);
  double idxp3 = idx + 0.5*log(dk2);
  double c1 = exp(idxp1)-tval*exp(idxp2);
  double polp3 = pnzz*zz2*(zz2+3.0) - dnorm(zz,0.0,1.0,0)*zz*(zz2+2.0);
  double sgnc2 = SGN(polp3)*SGN(lambda);
  double idxc2 = idxp3 + log(fabs(lambda)) + log(fabs(polp3)) - LOG6;
  double c2 = exp(idxc2)*sgnc2;
  return c1 + c2;

}

double negapprox(double tval, double zz, double ww,
		 double dk2, double lambda, double mumk)
{
  double zz2 = zz*zz;
  double ww2 = ww*ww;
  double idx = 0.5*zz2 - 0.5*ww2;
  double idxp1 = -0.5*ww2+0.5*log(dk2)-log(SQRPI2);
  double pzz = pnorm(zz,0.0,1.0,1,0);
  double idxp2 = idx + log(dk2) + log(pzz);
  double idxp3 = idx + 0.5*log(dk2);
  double c1 = exp(idxp1) + tval*exp(idxp2) + mumk;
  double polp3 = pzz*zz2*(zz2+3.0)+dnorm(zz,0.0,1.0,0)*zz*(zz2+2.0);
  double sgnc2 = SGN(polp3) * SGN(lambda);
  double idxc2 = idxp3 + log(fabs(lambda))+log(fabs(polp3)) - LOG6;
  double c2 = exp(idxc2)*sgnc2;
  return c1 - c2;
}
void oeiinfo(int n, int p, int L, double b, double barval,
	     const double* iomemu2, const double* upb,
	     const double* avec, const double* mub2star,
	     const double* mumk, double *info)
{
  double tval, kp, dk2, dk3, sgnt;
  double zz, lambda, ww;
  int i, j, stat;
  for(i = 0, j = 0; i < n; ++i, j+=p)
  {
    parOei ppar = {p, L, b, iomemu2[i], barval, upb[i], avec+j, mub2star+j};
    stat = nleqslv(0.0, &transoeidkappaSeq, &transoeidkappa2,
		   (void*) &ppar, &tval, 100, 1E-8, 1E-8);
    if(stat != success)
    {
      info[i] = NAN;
      continue;
    }
    tval = transfun(tval,upb[i]);
    oeikappafs(tval, b, iomemu2[i], avec+j, mub2star+j,
	       p, L, &kp, &dk2, &dk3);
    sgnt = SGN(tval);
    zz = tval * sqrt(dk2);
    lambda = dk3/pow(dk2,1.5);
    ww = sgnt * sqrt(2.0*barval*tval-2.0*kp);
    info[i] = tval >0.0? posapprox(tval,zz,ww,dk2,lambda):
      negapprox(tval,zz,ww,dk2,lambda,mumk[i]);
  }
}
void oeiinfo_R(const int* n_, const int* p_, const int* L_,
	       const double *b_, const double *barval_,
	       const double *iomemu2_, const double *upb_,
	       const double *avec_, const double *mub2star_,
	       const double *mumk_, double *info_)
{
  oeiinfo(*n_, *p_, *L_, *b_, *barval_,
	  iomemu2_, upb_, avec_, mub2star_,
	  mumk_, info_);
}
