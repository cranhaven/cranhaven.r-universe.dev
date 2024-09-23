#include <R.h>
#include <Rmath.h>

#include <time.h>

#define  MIN(x,y) ((x) > (y) ? (y) : (x))
#define  MAX(x,y) ((x) < (y) ? (y) : (x)) 

//marginal empirical cumulative distribution function
double uecdf(double *x, int k, int l, double u)
{
    int i;
    double res = 0.0;
    for (i = k; i < l; i++)
	    res += (x[i] <= u);
    return res/(l-k);
}


//calculation of the  bootstrap sequential empirical copula processes
void bootCn(int n, int k, int l, int d, double *pseudok, double *random, double *u, double Ck, double *Cj, int M, double *val) 
{
	    int ind;
	    double ind1, ind2;
	    for (int m = 0; m < M; m++)
		val[m]=0.;

	    for(int i = k; i < l; i++) 
	    {
	      ind1 = 1.;
	      ind2 = 0.;
	     for(int j = 0; j< d; j++) 
	      {
	       ind   = (pseudok[i + j*n] <= u[j]);
	       ind1 *= ind;
	       ind2 += Cj[j] *  (ind - uecdf(&pseudok[j*n], k, l, u[j]));
	      }
	      double diff = (ind1 - Ck - ind2);
		for (int m = 0; m < M; m++)
	          val[m] += random[i+m * n]*diff;

	     }

}

//Calculation of the empirical copula
double Copemp(int n, int k, int l, int d, double *pseudo, double *u) 
{
	    int i,j,ind,p;
	    p = l-k;
	    double Cp = 0.;
	if(l>0)
	    for(i=k; i< l; i++)
	      {
		ind = 1.;
		for(j=0; j<d; j++)
		  ind *= (pseudo[i+j*n] <= u[j]);
		Cp += ind;
	      }
            if(p!=0)
	    Cp /= p;
	    return(Cp); 
}

//Calculation of the partial derivatives
void Cj(int n, int k, int l, int d, double *pseudo, double *u, double *Cjn)
{
            double h = 1/sqrt(l-k);
	    for(int j = 0; j < d; j++) 
	    {
	      double uj=u[j];
              double uplus = MIN(uj + h, 1);
	      u[j] = uplus;
	      Cjn[j] = Copemp(n, k, l, d, pseudo, u);
	      double umoins = MAX(uj - h, 0);
	      u[j] = umoins;
	      Cjn[j] -= Copemp(n, k, l, d, pseudo, u);
	      Cjn[j] /= uplus- umoins; 
	      u[j] = uj;
	   }
}

//Estimation of the copula break point

void argk(double *X, int *n, int *d, int *m, int *kstar, double *pseudo, double *pseudobk, double *u)

{
  double cop, C1km, Cknm;
  double t,eval;
 
  double *pseudobk2 = Calloc((*n)*(*d), double);
 
  double Snk = 0.0;
  double tempo =0;

  for(int k = 1; k<*n; k++)
    {
		
	for(int i =0; i<(*d)*(*n); i++)
	pseudobk2[i] = pseudobk[i + (k-1)*(*d)*(*n)]; 	
	
      t = (double)k/(*n);

      Snk = 0.0;

      for(int p = 0; p<*n; p++)
	{
	  for(int i = 0; i<*d; i++)	
	    u[i] = pseudo[p + i*(*n)];

	C1km = 0, Cknm=0;
	int j=1;

	while(m[j]<k)
	{
	cop = Copemp(*n, m[j-1], m[j], *d, pseudobk2, u) ;
	C1km +=   cop* (m[j] - m[j-1]);
	j++;
	}
	
	cop = Copemp(*n, m[j-1], k, *d, pseudobk2, u);
	C1km +=  cop * (k - m[j-1]);
	
	cop = Copemp(*n, k, m[j], *d, pseudobk2, u) ;
	Cknm +=   cop* (m[j] - k);
        j++;   
	    
	while(m[j]<=*n)
	{
	cop = Copemp(*n, m[j-1], m[j], *d, pseudobk2, u);
	Cknm +=   cop*(m[j] - m[j-1]) ;
	j++;
	}
	
	eval = t*(1-t)*(C1km/k-Cknm/(*n-k));
	Snk += eval*eval;
	  
     }
     if(tempo <= Snk) 
     {	
	*kstar = k;
        tempo = Snk;
     }
      
  }
  Free(pseudobk2);
}

/////////////////////////////////////////////////////////////

void Snbmult(double *X, int *n, int *d, int *m, double *Snb, double *random, double *pseudo, double *pseudobk, int *M, double *checkDnmk, double *checkDnm, double *u, double *Cbj)

{

  double cop =0;
  double C1km, Cknm;
  double t,eval;

  double *val = Calloc(*M, double);
  double *checkDn = Calloc(*M, double);

  double *pseudobk2 = Calloc((*n)*(*d) ,double);
 
  double Snk = 0.0;

  for(int k = 1; k<*n; k++)
    {
      t = (double)k/(*n);
      for(int beta=0; beta<*M; beta++)
	  checkDnmk[beta] = 0.0;
	
      Snk = 0.0;

      for(int p = 0; p<*n; p++)
	{
	  for(int i = 0; i<*d; i++)	
	    u[i] = pseudo[p + i*(*n)];

//Computation of the empirical copula computed from the sample of size n, k and n-k
	
	for(int i =0; i<(*d)*(*n); i++)
	pseudobk2[i] = pseudobk[i + (k-1)*(*d)*(*n)]; 	

	C1km = 0, Cknm=0;
	for(int beta=0; beta<*M; beta++)
	    checkDn[beta] = 0.0;
	

	int j=1;

	while(m[j]<k)
	{
	cop = Copemp(*n, m[j-1], m[j], *d, pseudobk2, u) ;
	C1km +=   cop* (m[j] - m[j-1]);
	Cj(*n, m[j-1], m[j], *d, pseudobk2, u, Cbj);
	bootCn(*n,m[j-1], m[j], *d, pseudobk2, random, u, cop, Cbj, *M,val) ;	
	for(int beta=0; beta<*M; beta++)
		checkDn[beta] +=  val[beta]*(1-t);
	j++;
	}
	
	cop = Copemp(*n, m[j-1], k, *d, pseudobk2, u);
	C1km +=  cop * (k - m[j-1]);
	Cj(*n, m[j-1], k, *d, pseudobk2, u, Cbj);
	bootCn(*n,m[j-1], k, *d, pseudobk2, random, u, cop, Cbj, *M,val) ;	
	for(int beta=0; beta<*M; beta++)
		checkDn[beta] +=  val[beta]*(1-t);
	
	
	cop = Copemp(*n, k, m[j], *d, pseudobk2, u) ;
	Cknm +=   cop* (m[j] - k);
	Cj(*n, k, m[j], *d, pseudobk2, u, Cbj);
	bootCn(*n,k, m[j], *d, pseudobk2, random, u, cop, Cbj, *M,val) ;	
	for(int beta=0; beta<*M; beta++)
		checkDn[beta] -= val[beta]*t;
	
        j++;   
	    

	while(m[j]<=*n)
	{
	cop = Copemp(*n, m[j-1], m[j], *d, pseudobk2, u);
	Cknm +=   cop*(m[j] - m[j-1]) ;
	Cj(*n, m[j-1], m[j], *d, pseudobk2, u, Cbj);
	bootCn(*n,m[j-1], m[j], *d, pseudobk2, random, u, cop, Cbj, *M,val) ;	
	for(int beta=0; beta<*M; beta++)
		checkDn[beta] -=  val[beta]*t;
	j++;
	}
	
	
	eval = t*(1-t)*(C1km/k-Cknm/(*n-k));
	Snk += eval*eval;
	for(int beta=0; beta<*M; beta++)
	  checkDnmk[beta] += (checkDn[beta]*checkDn[beta]);

	}

      *Snb=MAX(*Snb,Snk);
      for(int beta = 0; beta < *M; beta++)
	{
	  checkDnm[beta] = MAX(checkDnm[beta],checkDnmk[beta]/((*n)*(*n)));
	}

    }

  Free(pseudobk2);
  Free(val);
  Free(checkDn);
}


