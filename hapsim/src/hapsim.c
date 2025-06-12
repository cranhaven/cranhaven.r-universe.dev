
#include <Rmath.h>

#define X(_m,_i,_j,_nrow) _m[ _i + _nrow * _j ]
#define PI 3.141592654

double max(double a, double b){
   if (a>b)
        return a;
  else
        return b;
}

double min(double a, double b){
   if (a>b)
        return b;
   else
        return a;
}

/* the signum of a real number */
double sgn(double a){
  if (a>0)
        return 1.;
  else if (a<0)
        return -1.;
  else
        return 0.;
}

/* standard normal density function */
double ndf(double t){
  return 0.398942280401433*exp(-t*t/2);
}

/* standard normal cumulative distribution function */
double nc(double x){
  double result;
  static double a[5] = {0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429};
  if (x<-7.)
        result = ndf(x)/sqrt(1.+x*x);
  else if (x>7.)
        result = 1. - nc(-x);
  else{
    result = 0.2316419;
    result=1./(1+result*fabs(x));
    result=1-ndf(x)*(result*(a[0]+result*(a[1]+result*(a[2]+result*(a[3]+result*a[4])))));
    if (x<=0.) result=1.-result;
   }
return result;
}


/* needed to calculate two dimensional cumulative distribution function */
double fxy(double x, double y, double a, double b, double rho)
{
    double a_s;
    double b_s;
        double result;
    a_s = a / sqrt(2 * (1 - rho * rho));
    b_s = b / sqrt(2 * (1 - rho * rho));
    result = exp(a_s * (2 * x - a_s) + b_s * (2 * y - b_s) + 2 * rho * (x - a_s) * (y - b_s));
return result;
}


/* needed to calculate two dimensional cumulative distribution function
   this equals pnorm2 if a and b and rho are all nonpositive, 
   the generalization for the other cases is pnorm2 below */
double Ntwo(double a, double b, double rho)
{
    static double aij[4]={0.325303,
                          0.4211071,
                          0.1334425,
                          0.006374323};
        static double bij[4]={0.1337764,
                          0.6243247,
                          1.3425378,
                          2.2626645};
    int i;
    int j;
        double result;
    result = 0;
        for(i=0;i<=3;i++) 
                {
                        for(j=0;j<=3;j++)
                        {
                                result+=aij[i] * aij[j] * fxy(bij[i], bij[j], a, b, rho); 
                        }
                }
    result = result * sqrt(1 - rho * rho) / PI;
return result;
}

/* calculates cumulative distribution function for a bivariate normal distribution */
double pnorm2(double a, double b, double rho)
{
    double rho1;
    double rho2;
    double denominator;
        double result;

    if (rho > 0.9999)
        result = nc(min(a, b));
    else if (rho < -0.9999)
        result = max(0, nc(a) - nc(-b));
    else
        {
        if (a * b * rho <= 0) 
                {
            if (a <= 0 && b <= 0 && rho <= 0)
                result = Ntwo(a, b, rho);
            else if (a <= 0 && b * rho >= 0)
                result = nc(a) - Ntwo(a, -b, -rho);
            else if (b <= 0 && rho >= 0)
                result = nc(b) - Ntwo(-a, b, -rho);
            else
                result = nc(a) + nc(b) - 1 + Ntwo(-a, -b, rho);
                }
        else
                {
            denominator = sqrt(a * a - 2 * rho * a * b + b * b);
            rho1 = (rho * a - b) * sgn(a) / denominator;
            rho2 = (rho * b - a) * sgn(b) / denominator;
            result = pnorm2(a, 0, rho1) + pnorm2(b, 0, rho2) - (1 - sgn(a) * sgn(b)) / 4;
        }
        if (result < 0) result = 0;
        }
        return result;
}

/* solves the equation for rho */
double findrho(double x, double y, double c)
{
	double a=-1;
	double b=1;
	double m;
	double precision=0.00001;
	
	while ((b-a)>precision){
		m = (a+b)/2;
		if (pnorm2(x,y,m)==c)
			break;
		if (pnorm2(x,y,m)<c)
			a = m;
		else
			b = m;
	}
	return m;
}


/* computes the required covariance matrix */
void covariance(int *nr, double *cormat, double *probs, double *quants, double *covmat)
{
	int i,j;
	double c;
	   for (i = 0; i < *nr; i++)
		   for (j = i; j < *nr; j++){
			   c = X(cormat, i, j, *nr) * sqrt(probs[i]*(1-probs[i])*probs[j]*(1-probs[j])) + probs[i]*probs[j];
   			   X(covmat, i, j, *nr) = findrho(quants[i], quants[j], c);
			   X(covmat, j, i, *nr) = X(covmat, i, j, *nr);
		   }
    return;
}






#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
   CALLDEF(covariance, 5),
   {NULL, NULL, 0}
};

void R_init_splines(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
}













