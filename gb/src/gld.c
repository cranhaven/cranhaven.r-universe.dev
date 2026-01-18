/* Compile under R 
 R CMD SHLIB kernel.c -o kernel.so
*/

#include <R.h>
#include <Rmath.h>
#define INLINE

/***********************************************************  
The codes below this line have been checked 
************************************************************/

/*==========================================================
functions for RS-GLD
------------------------------------------------------------*/
int ValidGLD(double l1, double l2, double l3, double l4)
{
  //  The following code is to check the validity of the parameters.
  // result=0: invalid; result=1: valid;
  int result=0;
  if(ISNAN(l1) || ISNAN(l2) ||ISNAN(l3) ||ISNAN(l4)
     || !R_finite(l1)|| !R_finite(l2)|| !R_finite(l3)|| !R_finite(l4)
     || R_NegInf==l1|| R_NegInf==l2|| R_NegInf==l3 || R_NegInf==l4
     || R_PosInf==l1|| R_PosInf==l2|| R_PosInf==l3 || R_PosInf==l4
    )
    result=0;
  else{
    double cond1, cond2;
    cond1 = l4*pow(1.-l3,1.-l3)*pow(l4-1.,l4-1.) + l3*pow(l4-l3,l4-l3);
    cond2 = l3*pow(1.-l4,1.-l4)*pow(l3-1.,l3-1.) + l4*pow(l3-l4,l3-l4);
    if(l2 < 0. && l3 < -1. && l4 > 1.) result = 1;
    if(l2 < 0. && l3 > 1. && l4 < -1.) result = 1;
    if(l2 > 0. && l3 >= 0. && l4 >= 0. && l3+l4 != 0.) result=1;
    if(l2 < 0. && l3 <= 0. && l4 <= 0. && l3+l4 != 0.) result=1;
    if(l2 < 0. && l3 > -1. && l3 < 0. && l4 > 1. && cond1<0.) result=1;
    if(l2 < 0. && l4 > -1. && l4 < 0. && l3 > 1. && cond2<0.) result=1;
  }
  return(result);
}

double KSPvalue(double t){
  double ksp=0.;
  int i;
  for(i=1;i<100;i++){
    ksp += exp(-2.*pow(i*t,2.));
    i++;
    ksp -= exp(-2.*pow(i*t,2.));
  }
  return(2.*ksp);
}


double GldQy(double y, double l1, double l2, double l3, double l4){
  // Compute the GLD quantile function 
  // Q(y) = l1+ (y^l3 - (1-y)^l4)/l2
  double result;
  if(y==0.){
    if(l3>=0.){
      if(l4==0.){ result=l1;}
      else {result=l1-1./l2;}
    } else{
      result=R_NegInf;
    }
  } else {
    if(y==1.) {
      if(l4<0.){
	result=R_PosInf;
      }
      else{
	if(l3==0.){result=l1;}
	else{result=l1+1./l2;}
      }
    } else{
      result = l1 + (pow(y, l3) - pow(1.-y,l4))/l2;
    }
  }
  return(result);
}

double f(double x, double y, double l1, double l2, double l3, double l4){
  return(GldQy(y,l1,l2,l3,l4)-x);
}

double Gldfx(double x, double l1, double l2, double l3, double l4){
  double result;
  int iter=30;
  if(ValidGLD(l1,l2,l3,l4)==0){
    result = 0.;
  }else{
    double y0 = 0.0, g0, y1=1., g1, yk, gk;
  int i=0;
  if(l3<0. && l4<=0.) {
    y0=0.0001;
    g0 = f(x,y0,l1,l2,l3,l4);
    if(g0>0.) {
      result=0.;
      i=iter;
    }
  }
  if(l4<0. && l3<=0.) {
    y1=0.9999;
    g1 = f(x,y1,l1,l2,l3,l4);
    if(g1<0.) {
      result=1.;
      i=iter;
    }
  }
  g0 = f(x,y0,l1,l2,l3,l4);
  g1 = f(x,y1,l1,l2,l3,l4);
  yk = (y0+y1)/2.;
  gk = f(x,yk,l1,l2,l3,l4);
  while((i < iter) && (fabs(gk) > 0.00001)){
    if( g0*gk < 0.){
      y1=yk;
    }
    else{
      y0=yk;
    }
    yk = (y0+y1)/2.;
    g0 = f(x,y0,l1,l2,l3,l4);
    g1 = f(x,y1,l1,l2,l3,l4);
    gk = f(x,yk,l1,l2,l3,l4);
      i = i+1;
  } 
  if(i<iter) result = l2/(l3*pow(yk,l3-1.)+l4*pow(1-yk, l4-1.));
}
return(result);
}

double GldFx(double x, double l1, double l2, double l3, double l4){
  double y0 = 0.0, g0, y1=1., g1, yk, gk;
  int i=0, iter=30;
  double eps=0.0000001;
  if(l3<0. && l4<=0.) {
    y0=0.0000001;
  }
  if(l4<0. && l3<=0.) {
    y1=0.9999999;
  }
  g0 = f(x,y0,l1,l2,l3,l4);
  yk = (y0+y1)/2.;
  if(g0>=0.) {
    yk=0.;i=iter;
  }
  g1 = f(x,y1,l1,l2,l3,l4);
  if(g1<=0.){
    yk = 1.; i=iter;
  }
  gk = f(x,yk,l1,l2,l3,l4);
  while((i < iter) && (fabs(gk) > eps)){
    if( g0*gk < 0.){
      y1=yk;
    }
    else{
      y0=yk;
    }
    yk = (y0+y1)/2.;
    gk = f(x,yk,l1,l2,l3,l4);
    i = i+1;
  } 
  return(yk);
}
///////////  Functions to be called by R functions //////////////
//  This code could be removed
void RIsGld(double *lmd,int *status)
{
  //  The following code is to check the validity of the parameters.
  // to be called by R functions;
  // result=0: invalid; result=1: valid;
  double result=-1.;
  double cond1, cond2;
  cond1 = lmd[3]*pow(1.-lmd[2],1.-lmd[2])*pow(lmd[3]-1.,lmd[3]-1.) 
    + lmd[2]*pow(lmd[3]-lmd[2],lmd[3]-lmd[2]);
  cond2 = lmd[2]*pow(1.-lmd[3],1.-lmd[3])*pow(lmd[2]-1.,lmd[2]-1.)
    + lmd[3]*pow(lmd[2]-lmd[3],lmd[2]-lmd[3]);
  if(lmd[1] < 0. && lmd[2] < -1. && lmd[3] > 1.) result = 1.;
  if(lmd[1] < 0. && lmd[2] > 1. && lmd[3] < -1.) result = 1.;
  if(lmd[1] > 0. && lmd[2] >= 0. && lmd[3] >= 0. && lmd[2]+lmd[3] != 0.) 
    result=1.;
  if(lmd[1] < 0. && lmd[2] <= 0. && lmd[3] <= 0. && lmd[2]+lmd[3] != 0.) 
    result=1.;
  if(lmd[1] < 0. && lmd[2] > -1. && lmd[2] < 0. && lmd[3] > 1. && cond1<0.) 
    result=1.;
  if(lmd[1] < 0. && lmd[3] > -1. && lmd[3] < 0. && lmd[2] > 1. && cond2<0.) 
    result=1.;
  status[0] = result;
}

void RKSPvalue(double *t){
  t[0] = KSPvalue(t[0]);
}

void RGldfx(double *y, double *lambda, int *size){
  int i;
  if(ValidGLD(lambda[0],lambda[1],lambda[2],lambda[3])==1){
    for(i=0;i<size[0];i++){
      y[i] = Gldfx(y[i], lambda[0],lambda[1],lambda[2],lambda[3]);
    }
  }else{
    size[0]=-1;
  }
}

void RGldFx(double *y, double *lambda, int *size){
  int i;
  if(ValidGLD(lambda[0],lambda[1],lambda[2],lambda[3])==1){
    for(i=0;i<size[0];i++){
      y[i] = GldFx(y[i], lambda[0],lambda[1],lambda[2],lambda[3]);
    }
  }else{
    size[0]=-1;
  }
}

void RGldx(double *y, double *lambda, int *size){
  int i;
  if(ValidGLD(lambda[0],lambda[1],lambda[2],lambda[3])==1){
    for(i=0;i<size[0];i++){
      y[i] = GldQy(y[i], lambda[0],lambda[1],lambda[2],lambda[3]);
    }
  }else{
    size[0]=-1;
  }
}

void RLMoM(double *x, int *size,double *lmts){
  double l0=0.,l1=0.,l2=0.,l3=0.; 
  int i, n=size[0];
  l0 = 0.;
  for(i=0;i<n;i++){
    l0 +=  x[i];
  }

  for(i=1;i<n;i++){
    l1 += i * x[i]/n/(n-1.);
  }
  for(i=2;i<n;i++){
    l2 += i*(i-1.) * x[i]/n/(n-1.)/(n-2.);
  }
  for(i=3;i<n;i++){
    l3 += i*(i-1.)*(i-2.) * x[i]/n/(n-1.)/(n-2.)/(n-3.);
  }
  lmts[0] = l0/n;
  lmts[1] = -l0+2.*l1;
  lmts[2] = l0-6.*l1+6.*l2;
  lmts[3] = -l0+12.*l1-30.*l2+20.*l3;
}

/***********************************************************  
Fitting GLD: MoM, MoP and LMoM
************************************************************/
double cA(double l3, double l4)
{
  return(1/(1.+l3) - 1/(1.+l4));
}

double cB(double l3, double l4)
{
  return(1/(1+2.*l3)+1/(1+2.*l4) - 2. * exp(lbeta(1.+l3, 1.+l4)));
}

double cC(double l3, double l4)
{
  return(1/(1.+3.*l3) - 1/(1.+3.*l4) -3* exp(lbeta(1.+2.*l3, 1.+l4))
	 + 3.* exp(lbeta(1.+l3, 1.+2.*l4)));
}
double cD(double l3, double l4)
{
  return(1/(1.+4.*l3) + 1/(1.+4.*l4) -4.* exp(lbeta(1.+3.*l3, 1.+l4))
	 + 6.* exp(lbeta(1.+2.*l3, 1.+2.*l4))- 4.* exp(lbeta(1.+l3, 1.+3.*l4)));
}

double alpha3(double l3, double l4)
{
  double A,B,C;
  A = cA(l3,l4);
  B = cB(l3,l4);
  C = cC(l3,l4);
  return((C-3.0 * A * B+ 2.0 * pow(A,3.0))
	 /pow(fabs(B-pow(A,2.0)),1.5));
}

double alpha4(double l3, double l4)
{
  double A,B,C,D;
  A = cA(l3,l4);
  B = cB(l3,l4);
  C = cC(l3,l4);
  D = cD(l3,l4);
  return((D- 4. * A * C +6. * pow(A,2.) * B  - 3. * pow(A,4.))
	 /pow(fabs(B-pow(A,2.0)),2.));
}


double mratio3(double l3, double l4)
{
  double L3,L2;
  L2 = l3/(l3+1.)/(l3+2.) + l4/(l4+1.)/(l4+2.);
  L3 = l3*(l3-1.)/(l3+1.)/(l3+2.)/(l3+3.) 
    -  l4*(l4-1.)/(l4+1.)/(l4+2.)/(l4+3.);
  return(L3/L2);
}

double mratio4(double l3, double l4)
{
  double L4,L2;
  L2 = l3/(l3+1.)/(l3+2.) + l4/(l4+1.)/(l4+2.);
  L4 = l3*(l3-1.)*(l3-2.)/(l3+1.)/(l3+2.)/(l3+3.)/(l3+4.) 
    +  l4*(l4-1.)*(l4-2.)/(l4+1.)/(l4+2.)/(l4+3.)/(l4+4.);
  return(L4/L2);
}


double rho3(double l3, double l4)
{
  return((pow(.9,l4)-pow(.1,l3)+pow(.5,l3)-pow(.5,l4))
	 /(pow(.9,l3)-pow(.1,l4)+pow(.5,l4)-pow(.5,l3)));
}

double rho4(double l3, double l4)
{
  return((pow(.75,l3)-pow(.25,l4)+pow(.75,l4)-pow(.25,l3))
	 /(pow(.9,l3)-pow(.1,l4)+pow(.9,l4)-pow(.1,l3)));
}



/***********************************************************  
The codes above this line have been checked 
************************************************************/


//  2011/11/15: Write three subroutines to estimate GLD with MoM, LMoM and MoP

void GLDMoM(double *xmts,double *chisq, int *sizes, double *os, double *xbin){
  double Eps = 1.e-9, tol = 6.123234e-17;
  int n=sizes[0], nbin=sizes[1], Iter=100, Igrid=100,Jgrid=100;
  double l1,l2,l3,l4, es[nbin],l0;
  int i, j, igrid, jgrid;
  double mmt1,mmt2,mmt3,mmt4;
  mmt1 = xmts[0]; mmt2 = xmts[1]; mmt3 = xmts[2]; mmt4 = xmts[3];
  
  double gldmom3 = -.24999,gldmom4 = -.24999;
  double gldmom3b = -.0001,gldmom4b = -.0001;
  double lstep =1./Igrid, lstep2 = 0.0002/Igrid;



  //  Fit with GLD MoM: l3 = 1./runif(0.,1.)-1.25; l4 = 1./runif(0.,1.)-1.25;

  double chi,chimin=9999999.;

  double f1, f2;
  double df13,df23,df14,df24;
  double fdf;
  double h = 0.001;

  for(igrid=0;igrid<Igrid;igrid++){
    gldmom3 += lstep;
    gldmom3b += lstep2;
    
    for(jgrid=0;jgrid<Jgrid;jgrid++){
      gldmom4 += lstep;
      gldmom4b += lstep2;
     
      l3 = gldmom3;
      l4 = gldmom4;
      f1=1.; f2=1.;
      i=0; 
      
      while((i < Iter) && (fabs(f1) > Eps||fabs(f2) > Eps)) {
	i ++;
	f1 = alpha3(l3,l4)-mmt3;
	f2 = alpha4(l3,l4)-mmt4;
	df13 = ( -alpha3(l3+2.*h,l4) + 8. * alpha3(l3+h,l4)
		 - 8. * alpha3(l3-h,l4) + alpha3(l3-2.*h,l4))/(12.* h);
	df23 = ( -alpha4(l3+2.*h,l4) + 8. * alpha4(l3+h,l4)
		 - 8. * alpha4(l3-h,l4) + alpha4(l3-2.*h,l4))/(12.* h);
	df14 = ( -alpha3(l3,l4+2.*h) + 8. * alpha3(l3,l4+h)
		 - 8. * alpha3(l3,l4-h) + alpha3(l3,l4-2.*h))/(12.* h);
	df24 = ( -alpha4(l3,l4+2.*h) + 8. * alpha4(l3,l4+h)
		 - 8. * alpha4(l3,l4-h) + alpha4(l3,l4-2.*h))/(12.* h);
	fdf = df13*df24 - df14*df23;
	if(fdf <= tol) {i=Iter;}
	else{
	  l3 = l3 - (f1*df24 - f2*df14)/fdf;
	  l4 = l4 + (f1*df23 - f2*df13)/fdf;
	}
	f1 = alpha3(l3,l4) - mmt3;
	f2 = alpha4(l3,l4) - mmt4;
      }
      l2 = sqrt((cB(l3,l4) - pow(cA(l3,l4),2.))/mmt2);
      if(l3 > -.25 && l4 > -.25 && i<Iter && l2 > tol){
	l1 = mmt1-cA(l3,l4)/l2;
	//l2 could be postive/negative, check the other if one is not valid
	if(ValidGLD(l1,l2,l3,l4)==0){ 
	  l2 = -l2;
	  l1 = mmt1-cA(l3,l4)/l2;
	}
	if(ValidGLD(l1,l2,l3,l4)==1){// better estimates?
	  for(j=0;j<nbin-1;j++){
	    l0 = GldFx(xbin[j+1],l1,l2,l3,l4);
	    es[j] = l0 * n;
	  }
	  es[nbin-1] = n - es[nbin-2];
	  for(j=0;j<nbin-2;j++){
	    es[nbin-j-2] -= es[nbin-j-3];
	  }
	  chi = 0.0;
	  for(j=0;j<nbin;j++){
	    chi += pow(os[j]-es[j],2.0)/es[j];
	  }	  
	  if(chi<chimin && !ISNAN(chi)){
	    chimin = chi;
	    xmts[0] = l1;
	    xmts[1] = l2;
	    xmts[2] = l3;
	    xmts[3] = l4;
	    chisq[0] = chimin;
	  }
	  // end of model selection
	}
      }


      l3 = gldmom3b;
      l4 = gldmom4b;
      f1=1.; f2=1.;
      i=0; 
      
      while((i < Iter) && (fabs(f1) > Eps||fabs(f2) > Eps)) {
	i = i+1;
	f1 = alpha3(l3,l4)-mmt3;
	f2 = alpha4(l3,l4)-mmt4;
	df13 = ( -alpha3(l3+2.*h,l4) + 8. * alpha3(l3+h,l4)
		 - 8. * alpha3(l3-h,l4) + alpha3(l3-2.*h,l4))/(12.* h);
	df23 = ( -alpha4(l3+2.*h,l4) + 8. * alpha4(l3+h,l4)
		 - 8. * alpha4(l3-h,l4) + alpha4(l3-2.*h,l4))/(12.* h);
	df14 = ( -alpha3(l3,l4+2.*h) + 8. * alpha3(l3,l4+h)
		 - 8. * alpha3(l3,l4-h) + alpha3(l3,l4-2.*h))/(12.* h);
	df24 = ( -alpha4(l3,l4+2.*h) + 8. * alpha4(l3,l4+h)
		 - 8. * alpha4(l3,l4-h) + alpha4(l3,l4-2.*h))/(12.* h);
	fdf = df13*df24 - df14*df23;
	if(fdf <= tol) {i=Iter;}
	else{
	  l3 = l3 - (f1*df24 - f2*df14)/fdf;
	  l4 = l4 + (f1*df23 - f2*df13)/fdf;
	}
	f1 = alpha3(l3,l4)-mmt3;
	f2 = alpha4(l3,l4)-mmt4;
      }
      l2 = sqrt((cB(l3,l4) - pow(cA(l3,l4),2.))/mmt2);
      if(l3 > -.25 && l4 > -.25 && i<Iter && l2 > tol){
	l1 = mmt1-cA(l3,l4)/l2;
	if(ValidGLD(l1,l2,l3,l4)==0){
	  l2 = -l2;
	  l1 = mmt1-cA(l3,l4)/l2;
	}
	if(ValidGLD(l1,l2,l3,l4)==1){// better estimates?
	  for(j=0;j<nbin-1;j++){
	    l0 = GldFx(xbin[j+1],l1,l2,l3,l4);
	    es[j] = l0 * n;
	  }
	  es[nbin-1] = n - es[nbin-2];
	  for(j=0;j<nbin-2;j++){
	    es[nbin-j-2] -= es[nbin-j-3];
	  }
	  chi = 0.0;
	  for(j=0;j<nbin;j++){
	    chi += pow(os[j]-es[j],2.0)/es[j];
	  }	  
	  if(chi<chimin && !ISNAN(chi)){
	    chimin = chi;
	    xmts[0] = l1;
	    xmts[1] = l2;
	    xmts[2] = l3;
	    xmts[3] = l4;
	    chisq[0] = chimin;
	  }
	  // end of model selection
	}
      }
    }
  }
  //  for(i=0;i<4;i++) xmts[i] = lmd[i]; 
  // end of GLD MoM

}  


// 2011/11/16:  MoP

void GLDMoP(double *xmts,double *chisq, int *sizes, double *os, double *xbin){
  double Eps = 1.e-9, tol = 6.123234e-17;
  int n=sizes[0], nbin=sizes[1], Iter=100, Igrid=100,Jgrid=100;
  double l1,l2,l3,l4, es[nbin],l0;
  int i, j, k, igrid, jgrid;
  double mmt1,mmt2,mmt3,mmt4;
  mmt1 = xmts[0]; mmt2 = xmts[1]; mmt3 = xmts[2]; mmt4 = xmts[3];
  
  double gldmop3 =0.000000001,gldmop4 =0.000000001;
  double lstep =1./Igrid;

  double chi,chimin=9999999.;

  double f1, f2;
  double df13,df23,df14,df24;
  double fdf;
  double h = 0.0001;

  for(igrid=0;igrid<Igrid;igrid++){
    gldmop3 += lstep;
    
    for(jgrid=0;jgrid<Jgrid;jgrid++){
      gldmop4 += lstep;
     
      l3 = gldmop3;
      l4 = gldmop4;
      f1=1.; f2=1.;
      i=0; k=0;
      
      while((i < Iter) && (fabs(f1) > Eps||fabs(f2) > Eps)) {
	i ++;
	f1 = rho3(l3,l4)-mmt3;
	f2 = rho4(l3,l4)-mmt4;
	df13 = ( -rho3(l3+2.*h,l4)
		 + 8. * rho3(l3+h,l4)
		 - 8. * rho3(l3-h,l4)
		 + rho3(l3-2.*h,l4))/(12.* h);
	df23 = ( -rho4(l3+2.*h,l4)
		 + 8. * rho4(l3+h,l4)
		 - 8. * rho4(l3-h,l4)
		 + rho4(l3-2.*h,l4))/(12.* h);
	df14 = ( -rho3(l3,l4+2.*h)
		 + 8. * rho3(l3,l4+h)
		 - 8. * rho3(l3,l4-h)
		 + rho3(l3,l4-2.*h))/(12.* h);
	df24 = ( -rho4(l3,l4+2.*h)
		 + 8. * rho4(l3,l4+h)
		 - 8. * rho4(l3,l4-h)
		 + rho4(l3,l4-2.*h))/(12.* h);
	fdf = df13*df24 - df14*df23;
	if(fdf < tol) {i=Iter; break;}
	else{
	  l3 = l3 - (f1*df24 - f2*df14)/fdf;
	  l4 = l4 + (f1*df23 - f2*df13)/fdf;
	  f1 = rho3(l3,l4)-mmt3;
	  f2 = rho4(l3,l4)-mmt4;
	}
      }
      l2 = (pow(.9,l3)-pow(.1,l4)+pow(.9,l4)-pow(.1,l3))/mmt2;
      if(fabs(l2) > tol && i<Iter){
	l1 = mmt1 - (pow(.5,l3)-pow(.5,l4))/l2;
	if(ValidGLD(l1,l2,l3,l4)==1){// better estimates?
	  for(j=0;j<nbin-1;j++){
	    l0 = GldFx(xbin[j+1],l1,l2,l3,l4);
	    es[j] = l0 * n;
	  }
	  es[nbin-1] = n - es[nbin-2];
	  for(j=0;j<nbin-2;j++){
	    es[nbin-j-2] -= es[nbin-j-3];
	  }
	  chi = 0.0;
	  for(j=0;j<nbin;j++){
	    chi += pow(os[j]-es[j],2.0)/es[j];
	  }	  
	  if(chi<chimin && !ISNAN(chi)){
	    chimin = chi;
	    xmts[0] = l1;
	    xmts[1] = l2;
	    xmts[2] = l3;
	    xmts[3] = l4;
	    chisq[0] = chimin;
	  }
	  // end of model selection
	}
      }
    }
  }
}  

void GLDLMoM(double *xmts,double *chisq, int *sizes, double *os, double *xbin){
  double Eps = 1.e-9, tol = 6.123234e-17;
  int n=sizes[0], nbin=sizes[1], Iter=100, Igrid=100,Jgrid=100;
  double l1,l2,l3,l4, es[nbin],l0;
  int i, j, igrid, jgrid;
  double mmt1,mmt2,mmt3,mmt4;
  mmt1 = xmts[0]; mmt2 = xmts[1]; mmt3 = xmts[2]; mmt4 = xmts[3];
  
  double gldlmom3 = -.24999,gldlmom4 = -.24999;
  double lstep =1./Igrid;

  double chi,chimin=9999999.;

  double f1, f2;
  double df13,df23,df14,df24;
  double fdf;
  double h = 0.001;

  for(igrid=0;igrid<Igrid;igrid++){
    gldlmom3 += lstep;
    
    for(jgrid=0;jgrid<Jgrid;jgrid++){
      gldlmom4 += lstep;
     
      l3 = gldlmom3;
      l4 = gldlmom4;
      f1=1.; f2=1.;
      i=0; 
      
      while((i < Iter) && (fabs(f1) > Eps||fabs(f2) > Eps)) {
	i ++;
	f1 = mratio3(l3,l4)-mmt3/mmt2;
	f2 = mratio4(l3,l4)-mmt4/mmt2;
	df13 = ( -mratio3(l3+2.*h,l4)
		 + 8. * mratio3(l3+h,l4)
		 - 8. * mratio3(l3-h,l4)
		 + mratio3(l3-2.*h,l4))/(12.* h);
	df23 = ( -mratio4(l3+2.*h,l4)
		 + 8. * mratio4(l3+h,l4)
		 - 8. * mratio4(l3-h,l4)
		 + mratio4(l3-2.*h,l4))/(12.* h);
	df14 = ( -mratio3(l3,l4+2.*h)
		 + 8. * mratio3(l3,l4+h)
		 - 8. * mratio3(l3,l4-h)
		 + mratio3(l3,l4-2.*h))/(12.* h);
	df24 = ( -mratio4(l3,l4+2.*h)
		 + 8. * mratio4(l3,l4+h)
		 - 8. * mratio4(l3,l4-h)
		 + mratio4(l3,l4-2.*h))/(12.* h);
	fdf = df13*df24 - df14*df23;
	if(fdf < tol) {i=Iter;}
	else{
	  l3 = l3 - (f1*df24 - f2*df14)/fdf;
	  l4 = l4 + (f1*df23 - f2*df13)/fdf;
	  f1 = mratio3(l3,l4)-mmt3/mmt2;
	  f2 = mratio4(l3,l4)-mmt4/mmt2;
	}
      }
      //      if(i < Iter && l3 > -.25 && l4 > -.25){
      l2 = (l3/(l3+1.)/(l3+2.) + l4/(l4+1.)/(l4+2.))/mmt2;
      l1 = mmt1 - 1./l2/(l3+1.) + 1./l2/(l4+1.);
      if(ValidGLD(l1,l2,l3,l4)==1){// better estimates?
	for(j=0;j<nbin-1;j++){
	  l0 = GldFx(xbin[j+1],l1,l2,l3,l4);
	  es[j] = l0 * n;
	}
	  es[nbin-1] = n - es[nbin-2];
	  for(j=0;j<nbin-2;j++){
	    es[nbin-j-2] -= es[nbin-j-3];
	  }
	  chi = 0.0;
	  for(j=0;j<nbin;j++){
	    chi += pow(os[j]-es[j],2.0)/es[j];
	  }	  
	  if(chi<chimin && !ISNAN(chi)){
	    chimin = chi;
	    xmts[0] = l1;
	    xmts[1] = l2;
	    xmts[2] = l3;
	    xmts[3] = l4;
	    chisq[0] = chimin;
	  }
	  // end of model selection
	  //	}
      }
    }
  }
}  
