#include <R.h>
#include <Rmath.h>
#define INLINE

/***********************************************************  
The codes below this line have been checked 
************************************************************/

/*==========================================================
functions for GBD
------------------------------------------------------------*/
int ValidGBD(double l1, double l2, double l3, double l4)
{
  int result=1;
  if(l2<=0 || l3<=-1 || l4<=-1||ISNAN(l1)||ISNAN(l2)||ISNAN(l3)||ISNAN(l4)
     ||!R_finite(l1)||!R_finite(l2)||!R_finite(l3)||!R_finite(l4)
     ){result=0;}
  return(result);
}
double Gbdfx(double x, double l1, double l2, double l3, double l4){
  double x0, Fx;

  x0 = (x-l1)/l2;
  if(x0>=0. && x0<=1.){
    Fx = dbeta(x0, l3+1.,l4+1.,0);
  }else{
    if(x0 < 0.) Fx = 0.;
    if(x0 > 1.) Fx = 0.;
  }
  return(Fx);
}

double GbdFx(double x, double l1, double l2, double l3, double l4){
  double x0, Fx=0.;
  x0 = (x-l1)/l2;
  if(x0 >= 0. && x0 <= 1.){
    Fx = pbeta(x0, l3,l4,1,1);
    Fx = exp(Fx);
  }else{
    if(x0 < 0.) Fx = 0.;
    if(x0 > 1.) Fx = 1.;
  }
  return(Fx);
}

///////////  Functions to be called by R functions //////////////
/*
x: raw data; n: sample size; para: GBD parameters (a vector of length
4)
 */

void Cfitgbd(double x[],int n, double llk[], double b[]){
  int i,imax=100;
  double clogx,clog1x,reltol=1.e-10;
  double dg3,dg4,dg34,tg3,tg4,tg34;
  double f1,f2,c1,c2,c3,c4,b1,b2,b3,b4;
  double s1,s2=0.,dist;

  b1 = b[0];b2=b[1];b3=b[2];b4=b[3];
  clogx=0.0;clog1x=0.0;
  for(i=0;i<n;i++){
    clogx += log(x[i]-b1);
    clog1x += log(b1+b2-x[i]);
  }
  clogx = clogx/n;
  clog1x = clog1x/n;

  i=0;dist=1.;
  while((i<imax)&&(dist>reltol)){
    dg3  = digamma(b3+1.);
    dg4  = digamma(b4+1.);
    dg34 = digamma(b3+b4+2.);
    tg3  = trigamma(b3+1.);
    tg4  = trigamma(b4+1.);
    tg34 = trigamma(b3+b4+2.);
    f1 = dg34-dg3-log(b2)+clogx;
    f2 = dg34-dg4-log(b2)+clog1x;
    c1 = tg34-tg3;
    c2 = tg34;
    c3 = tg34;
    c4 = tg34-tg4;
    s1 = (c2*f2-c4*f1)/(c1*c4-c3*c2);
    s2 = (c1*f2-c3*f1)/(c2*c3-c1*c4);
    b3 += s1; b4 += s2;
    if(b3<= -1.){b3=reltol-1.;}
    if(b4<= -1.){b4=reltol-1.;}
    dist = fabs(s1)+fabs(s2);
    i++;
  }
  b[2] = b3+1.0; b[3] = b4+1.0;
  llk[0] = 0.0;
  for(i=0;i<n;i++){
    llk[0] += dbeta((x[i]-b1)/b2,b3,b4,1);
  }
}

void FitGBD(double *x, int *size, int *type, double *llk0,double *para){
  double reltol=1.e-10;
  double b[4], xstep,xmin,xmax,lmax=-1.e-30;
  int i,j,k,n=size[0];
  xmin = para[0];xmax = para[1];
  b[0] = xmin - reltol; 
  b[1] = xmax - b[0]  + reltol;
  xstep = b[1]/100.;
  b[2] = para[2]; b[3] = para[3];
  switch(type[0]){
  case 0: //ll ul unknown
    for(j=0;j<10;j++){
      b[0] -= xstep;
      for(k=0;k<10;k++){
	b[1] = xmax - b[0] + k*xstep + reltol;
	Cfitgbd(x,n,llk0,b);
	if(llk0[0]>lmax){
	  lmax = llk0[0];
	  for(i=0;i<4;i++) para[i]=b[i];
	}
      }
    }
    break;
  case 1: //ll unkown, ul known
    b[1]=xmax-xmin+reltol;
    for(j=0;j<10;j++){
      b[0] = xmax - b[1] - j*xstep - 2*reltol;
      Cfitgbd(x,n,llk0,b);
      if(llk0[0]>lmax){
	lmax = llk0[0];
	for(i=0;i<4;i++) para[i]=b[i];
      }
    }
    break;
  case 2: //ll known, ul unknown
    for(k=0;k<10;k++){
      b[1] = xmax - b[0] + k*xstep + reltol;
      Cfitgbd(x,n,llk0,b);
      if(llk0[0]>lmax){
	lmax = llk0[0];
	for(i=0;i<4;i++) para[i]=b[i];
      }
    }
    break;
  default:
    Cfitgbd(x,n,llk0,b);
    if(llk0[0]>lmax){
      lmax = llk0[0];
      for(i=0;i<4;i++) para[i]=b[i];
    }
  }
  llk0[0] = lmax;
}

/* --------------------------------------------- 

The following codes are to estimate the GBD parameters via
   moment-matching method.

*/

double ealpha3(double b3, double b4)
{
  double result;
  if(b3>-1 && b4>-1){
    result = (2.*(b4-b3)*sqrt(b3+b4+3.)/((b3+b4+4.)*sqrt((b3+1.)*(b4+1.))));
  }else{
    result=-999.;
  }
  return(result);
}

double ealpha4(double b3, double b4)
{
  double a,b,result;
  if(b3>-1 && b4>-1){
    a = 3.*(b3+b4+3.)*(b3*b4*(b3+b4+2.)+3.*pow(b3,2.)+5.*b3
		       + 3.*pow(b4,2.)+5.*b4+4.);
    b = (b3+b4+4.)*(b3+b4+5.)*(b3+1)*(b4+1);
    result=a/b;
  }else{
    result=-999.;
  }
  return(result);
}


void FitGBDMom(double *x, int *l, double *lmd){
  double mt1,mt2,mt3,mt4;


  int n=l[0], Iter=500; // sample size;
  int Igrid=200,Jgrid=200;
  double Eps = 1.e-16;
  
  double l1,l2,l3,l4;
  int i, j, k;
  
  // To compute the first four sample moments
  j = n/2;
  k = n%2;
  if(k==1) mt1 = x[n-1];
  for(i = 0; i < j; i++) mt1 += x[i]+x[n-i-k-1];
  mt1 /= n;
  if(k==1){
    mt2 = pow(x[n-1]-mt1,2.);
    mt3 = pow(x[n-1]-mt1,3.);
    mt4 = pow(x[n-1]-mt1,4.);
  }
  for(i = 0; i < j; i++) {
    mt2 += pow(x[i]-mt1,2.) + pow(x[n-1-i-k]-mt1,2.);
    mt3 += pow(x[i]-mt1,3.) + pow(x[n-1-i-k]-mt1,3.);
    mt4 += pow(x[i]-mt1,4.) + pow(x[n-1-i-k]-mt1,4.);
  }
  mt2 /= n;
  mt3 = mt3/pow(mt2,1.5)/n;
  mt4 = mt4/pow(mt2,2.)/n;
  //To compute the EDF: empirical distribution function.
  double edf[n],l0;
  k=1;l0=x[0];
  for(i=1;i<n;i++){
    if(x[i]==l0){k += 1;}
    else{
      for(j=1;j<k+1;j++){
	edf[i-j] = (i-(k-1.)/2.)/n;
	l0 = x[i];
	k=1;
      }
    }
  }
  edf[n-1] =1.;

  double gbdmom3 = -.99999,gbdmom4 = -.99999;



  double mmt1=0., mmt2=0., mmt3=0., mmt4=0.;
  double dksa=-999.,ksmin=999.;

  int igrid, jgrid,status=0;
  double f1, f2;
  double df13,df23,df14,df24;
  double fdf;
  double h = 0.001;
  double lstep =1./Igrid;

  for(igrid=0;igrid<Igrid;igrid++){
    gbdmom3 += lstep*3.;
    
    for(jgrid=0;jgrid<Jgrid;jgrid++){
      gbdmom4 += lstep*3.;
     
      //  Fit with GBD MoM; *********
      l3 = gbdmom3; // Searching on (-1,infty)x(-1,infty)
      l4 = gbdmom4;
      //      l3 = 1./runif(0.,1.)-2.; // Searching on (-1,infty)x(-1,infty)
      //      l4 = 1./runif(0.,1.)-2.;
      f1=1.; f2=1.;
      i=0; k=0;
	
      mmt1=mt1; mmt2=mt2; mmt3=mt3; mmt4=mt4;
      while((i < Iter+10) && (fabs(f1) > Eps || fabs(f2) > Eps)){
	i = i+1;
	f1 = ealpha3(l3,l4)-mmt3;
	f2 = ealpha4(l3,l4)-mmt4;
	df13 = ( -ealpha3(l3+2.*h,l4)
		 + 8. * ealpha3(l3+h,l4)
		 - 8. * ealpha3(l3-h,l4)
		 + ealpha3(l3-2.*h,l4))/(12.* h);
	df23 = ( -ealpha4(l3+2.*h,l4)
		 + 8. * ealpha4(l3+h,l4)
		 - 8. * ealpha4(l3-h,l4)
		 + ealpha4(l3-2.*h,l4))/(12.* h);
	df14 = ( -ealpha3(l3,l4+2.*h)
		 + 8. * ealpha3(l3,l4+h)
		 - 8. * ealpha3(l3,l4-h)
		 + ealpha3(l3,l4-2.*h))/(12.* h);
	df24 = ( -ealpha4(l3,l4+2.*h)
		 + 8. * ealpha4(l3,l4+h)
		 - 8. * ealpha4(l3,l4-h)
		 + ealpha4(l3,l4-2.*h))/(12.* h);
	fdf = df13*df24 - df14*df23;
	if(fdf == 0.) {i=Iter+10;}
	else{
	  l3 = l3 - (f1*df24 - f2*df14)/fdf;
	  l4 = l4 + (f1*df23 - f2*df13)/fdf;
	  f1 = ealpha3(l3,l4)-mmt3;
	  f2 = ealpha4(l3,l4)-mmt4;
	}
      }
      l2 = sqrt((mmt2*pow(l3+l4+2.,2.)*(l3+l4+3.))/((l3+1.)*(l4+1.)));
      l1 = mmt1 - l2*(l3+1.)/(l3+l4+2.);
      if(ValidGBD(l1,l2,l3,l4)==1 && i < Iter+10){
	status++;
	dksa=-999.;  // KS test based on CDF
	for(j=0;j<n;j++){
	  l0 = GbdFx(x[j],l1,l2,l3,l4);
	  dksa = fmax(dksa, fabs(l0-edf[j]));
	}
	if(dksa<ksmin&& !ISNAN(dksa)){
	  ksmin = dksa;
	  lmd[0] = l1;
	  lmd[1] = l2;
	  lmd[2] = l3;
	  lmd[3] = l4;
	}
      }
      
      
      l3 = gbdmom3;
      l4 = gbdmom4;
      f1=1.; f2=1.;
      i=0; k=0;
      
      mmt1=mt1; mmt2=mt2; mmt3=mt3; mmt4=mt4;
      while((i < Iter+10) && (fabs(f1) > Eps||fabs(f2) > Eps)){
	i = i+1;
	f1 = ealpha3(l3,l4)-mmt3;
	f2 = ealpha4(l3,l4)-mmt4;
	df13 = ( -ealpha3(l3+2.*h,l4)
		 + 8. * ealpha3(l3+h,l4)
		 - 8. * ealpha3(l3-h,l4)
		 + ealpha3(l3-2.*h,l4))/(12.* h);
	df23 = ( -ealpha4(l3+2.*h,l4)
		 + 8. * ealpha4(l3+h,l4)
		 - 8. * ealpha4(l3-h,l4)
		 + ealpha4(l3-2.*h,l4))/(12.* h);
	df14 = ( -ealpha3(l3,l4+2.*h)
		 + 8. * ealpha3(l3,l4+h)
		 - 8. * ealpha3(l3,l4-h)
		 + ealpha3(l3,l4-2.*h))/(12.* h);
	df24 = ( -ealpha4(l3,l4+2.*h)
		 + 8. * ealpha4(l3,l4+h)
		 - 8. * ealpha4(l3,l4-h)
		 + ealpha4(l3,l4-2.*h))/(12.* h);
	fdf = df13*df24 - df14*df23;
	if(fdf == 0.) {i=Iter+10;}
	else{
	  l3 = l3 - (f1*df24 - f2*df14)/fdf;
	  l4 = l4 + (f1*df23 - f2*df13)/fdf;
	  f1 = ealpha3(l3,l4)-mmt3;
	  f2 = ealpha4(l3,l4)-mmt4;
	}
      }
      l2 = sqrt((mmt2*pow(l3+l4+2.,2.)*(l3+l4+3.))/((l3+1.)*(l4+1.)));
      l1 = mmt1 - l2*(l3+1.)/(l3+l4+2.);
      if(ValidGBD(l1,l2,l3,l4)==1&& i < Iter+10){
	status++;
	dksa=-999.;  // KS test based on CDF
	for(j=0;j<n;j++){
	  l0 = GbdFx(x[j],l1,l2,l3,l4);
	  dksa = fmax(dksa, fabs(l0-edf[j]));
	}
	if(dksa<ksmin&& !ISNAN(dksa)){
	  ksmin = dksa;
	  lmd[0] = l1;
	  lmd[1] = l2;
	  lmd[2] = l3;
	  lmd[3] = l4;
	}
      }
    }     // end of GBD MoM
  }
  l[0]=status;
}
  
