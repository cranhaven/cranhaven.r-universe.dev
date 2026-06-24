
#include "array.h"
#include <math.h>

#define MAX_IT      1000      
#define ALPHA       1.0      
#define BETA        0.5      
#define GAMMA       2.0      


double simplex2(double (*func)(int, int, int, double *, double *, double ***, double *, double **, double **, int, int), int n1, int p, int T, double *la, double ***X, double *gamma_k, double **invSk, double **invPsik, double *start, double EPSILON, double scale, int Mu_type, int trans_type)
{
  
  int vs;        	
  int vh;        	
  int vg;        	
  
  int i,j,m,row, n;		
  int itr;	
  int k;	
  
  double **v;           
  double pn,qn;        
  double *f;          
  double fr;            
  double fe;            
  double fc;           
  double *vr;           
  double *ve;          
  double *vc;          
  double *vm;          
  double min;
  
  double fsum,favg,s,cent;


  n = T;  

  MAKE_MATRIX(v, n+1, n);
  MAKE_VECTOR(f, n+1);
  MAKE_VECTOR(vr, n);
  MAKE_VECTOR(ve, n);
  MAKE_VECTOR(vc, n);
  MAKE_VECTOR(vm, n);
  
   
  pn = scale*(sqrt(n+1)-1+n)/(n*sqrt(2));
  qn = scale*(sqrt(n+1)-1)/(n*sqrt(2));
  
  for (i=0;i<n;i++) {
    v[0][i] = start[i];
  }
  
  for (i=1;i<=n;i++) {
    for (j=0;j<n;j++) {
      if (i-1 == j) {
	v[i][j] = pn + start[j];
      }
      else {
	v[i][j] = qn + start[j];
      }
    }
  }
  

  for (j=0;j<=n;j++) {
    f[j] = func(n1, p, T, v[j], la, X, gamma_k, invSk, invPsik, Mu_type, trans_type);
  }
  
  k = n+1;
  
  

  for (itr=1;itr<=MAX_IT;itr++) {     

    vg=0;
    for (j=0;j<=n;j++) {
      if (f[j] > f[vg]) {
	vg = j;
      }
    }
    

    vs=0;
    for (j=0;j<=n;j++) {
      if (f[j] < f[vs]) {
	vs = j;
      }
    }
    

    vh=vs;
    for (j=0;j<=n;j++) {
      if (f[j] > f[vh] && f[j] < f[vg]) {
	vh = j;
      }
    }
    

    for (j=0;j<=n-1;j++) {
      cent=0.0;
      for (m=0;m<=n;m++) {
	if (m!=vg) {
	  cent += v[m][j];
	}
      }
      vm[j] = cent/n;
    }
    

    for (j=0;j<=n-1;j++) {
      vr[j] = (1+ALPHA)*vm[j] - ALPHA*v[vg][j];
    }
    fr = func(n1, p, T, vr, la, X, gamma_k, invSk, invPsik, Mu_type, trans_type);
    k++;
    

    if (fr <= f[vh] && fr > f[vs]) {
      for (j=0;j<=n-1;j++) {
	v[vg][j] = vr[j];
      }
      f[vg] = fr;
    }
    

    if ( fr <=  f[vs]) {
      for (j=0;j<=n-1;j++) {
	ve[j] = GAMMA*vr[j] + (1-GAMMA)*vm[j];
      }
      fe = func(n1, p, T, ve, la, X, gamma_k, invSk, invPsik, Mu_type, trans_type);
      k++;
      
   
      if (fe < fr) {
	for (j=0;j<=n-1;j++) {
	  v[vg][j] = ve[j];
	}
	f[vg] = fe;
      }
      else {
	for (j=0;j<=n-1;j++) {
	  v[vg][j] = vr[j];
	}
	f[vg] = fr;
      }
    }

    if (fr > f[vh]) {
      for (j=0;j<=n-1;j++) {
	vc[j] = BETA*v[vg][j] + (1-BETA)*vm[j];
      }
      fc = func(n1, p, T, vc, la, X, gamma_k, invSk, invPsik, Mu_type, trans_type);
      k++;
      if (fc < f[vg]) {
	for (j=0;j<=n-1;j++) {
	  v[vg][j] = vc[j];
	}
	f[vg] = fc;
      }
       else {
	for (row=0;row<=n;row++) {
	  if (row != vs) {
	    for (j=0;j<=n-1;j++) {
	      v[row][j] = v[vs][j]+(v[row][j]-v[vs][j])/2.0;
	    }
	  }
	}
	f[vg] = func(n1, p, T, v[vg], la, X, gamma_k, invSk, invPsik, Mu_type, trans_type);
	k++;
	f[vh] = func(n1, p, T, v[vh], la, X, gamma_k, invSk, invPsik, Mu_type, trans_type);
	k++;
	
	
      }
    }
    
    

    fsum = 0.0;
    for (j=0;j<=n;j++) {
      fsum += f[j];
    }
    favg = fsum/(n+1);
    s = 0.0;
    for (j=0;j<=n;j++) {
      s += pow((f[j]-favg),2.0)/(n);
    }
    s = sqrt(s);
    if (s < EPSILON) break;
  }

  

  vs=0;
  for (j=0;j<=n;j++) {
    if (f[j] < f[vs]) {
      vs = j;
    }
  }


 
  for (j=0;j<n;j++) {
    start[j] = v[vs][j];
  }
  min=func(n1, p, T, v[vs], la, X, gamma_k, invSk, invPsik, Mu_type, trans_type);
  k++;




  FREE_MATRIX(v);
  FREE_VECTOR(f);
  FREE_VECTOR(vr);
  FREE_VECTOR(ve);
  FREE_VECTOR(vc);
  FREE_VECTOR(vm);
  



  return min;
}
