/*
 *  event : A Library of Special Functions for Event Histories
 *  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  SYNOPSIS
 *
 * void ksurvg(double p[],double y[],double x[],int cens[],int *nind,
 *	    int nobs[],int *nbs,int *nccov,int *model,int *dist,int *density,
 *	    int *dep,int *birth,int *tvc,double tvcov[],int *fit,double pred[],
 *	    double rpred[],int *renewal,int *rf,double bb[],
 *	    int *sf,double vv[],double *like)
 *
 *  DESCRIPTION
 *
 *    Function to compute the likelihood function for various distributions
 * inserted in a gamma or Weibull distribution with serial dependence using
 * Kalman-type update for event histories.
 *
 */

#include <math.h>
#include <stddef.h>
#include "R.h"
#include "Rmath.h"

void ksurvg(double p[],double y[],double x[],int cens[],int *nind,int nobs[],
	    int *nbs,int *nccov,int *model,int *dist,int *density,int *dep,
	    int *birth,int *tvc,double tvcov[],int *fit,double pred[],
	    double rpred[],int *renewal,int *rf,double bb[],
	    int *sf,double vv[],double *like){
  int i,j,k,nm,c;
  double a,a1,b,H,delta,lambda,omega,om,beta,bt,l1,yy,ly,plap,tmp,
    intercept;
  
  *like=0;
  nm=0;
  delta=exp(-p[*nccov+*birth+*tvc+1]);
  if(*dep>0)omega=exp(p[*nccov+*birth+*tvc+2])/(1+exp(p[*nccov+*birth+*tvc+2]));
  if(*model>1&&!*sf){
    if(*model<5)lambda=exp(p[*nccov+*birth+*tvc+2+(*dep>0)]);
    else lambda=exp(p[*nccov+*birth+*tvc+2+(*dep>0)]/2);}
  if(*model==4)intercept=exp(p[*nccov+*birth+*tvc+3+(*dep>0)]);
  for(i=0;i<*nind;i++){
    a=b=delta;
    if(!*rf){
      beta=p[0];
      for(k=0;k<*nccov;k++)beta+=p[k+1]*x[i+k**nind];
      if(*model<4){
	if(beta>40) beta=40;
	if(beta<-40)beta=-40;
	beta=exp(beta);}}
    else if(!*tvc)bt=bb[i];
    c=0;
    yy=0;
    for(j=0;j<nobs[i];j++){
      if(*model>1&&*sf)lambda=vv[nm];
      /* store value and check if ties to follow */
      if(y[nm]>0){
	if(*renewal)yy=y[nm];
	else yy+=y[nm];}
      c+=cens[nm];
      if(j>=nobs[i]-1||y[nm+1]!=0){
	if(*model>=5)ly=log(yy);
	a1=a+c;
	/* add in birth and time-varying covariates */
	if(!*rf){
	  if(*tvc){
	    bt=0;
	    for(k=0;k<*tvc;k++)bt+=p[*nccov+*birth+k+1]*tvcov[nm+*nbs*k];
	    if(*model<4){
	      if(bt>40) bt=40;
	      if(bt<-40)bt=-40;
	      bt=exp(bt)*beta;}
	    else bt+=beta;}
	  else bt=beta;
	  if(*birth){
	    if(*model<4)bt*=pow(j+1.,p[*nccov+1]);
	    else bt+=p[*nccov+1]*log(j+1);}}
	else if(*tvc)bt=bb[nm];
	if(!*density){
	  /* intensity models */
	  switch(*model){
	  case 1: /* exponential distribution */
	    H=yy/bt;
	    l1=1/bt;
	    break;
	  case 2: /* Weibull distribution */
	    H=pow(yy/bt,lambda);
	    l1=lambda*pow(yy/bt,lambda-1)/bt;
	    break;
	  case 3: /* gamma distribution */
	    H=-pgamma(yy,lambda,bt,0,1);
	    l1=dgamma(yy,lambda,bt,0)/(1-pgamma(yy,lambda,bt,1,0));
	    break;
	  case 4: /* generalized logistic distribution */
	    H=(yy+log(lambda+intercept*exp(-bt*yy))/bt)/lambda;
	    l1=1/(lambda+intercept*exp(-bt*yy));
	    break;
	  case 5: /* log normal distribution */
	    H=-pnorm(ly,bt,lambda,0,1);
	    l1=dnorm(ly,bt,lambda,0)/yy/(1-pnorm(ly,bt,lambda,1,0));
	    break;
	  case 6: /* log logistic distribution */
	    H=-plogis(ly,bt,lambda,0,1);
	    l1=dlogis(ly,bt,lambda,0)/yy/(1-plogis(ly,bt,lambda,1,0));
	    break;
	  case 7: /* log Cauchy distribution */
	    H=-pcauchy(ly,bt,lambda,0,1);
	    l1=dcauchy(ly,bt,lambda,0)/yy/(1-pcauchy(ly,bt,lambda,1,0));
	    break;
	  case 8: /* log Laplace distribution */
	    tmp=exp(-fabs(ly-bt)/lambda)/2;
	    plap=ly<bt?tmp:1-tmp;
	    H=-log(1-plap);
	    l1=tmp/(lambda*yy*(1-plap));
	    break;}}
	else{
	  /* density models */
	  switch(*model){
	  case 1: /* exponential distribution */
	    H=pexp(yy,bt,1,0);
	    l1=dexp(yy,bt,0);
	    break;
	  case 2: /* Weibull distribution */
	    H=pweibull(yy,lambda,bt,1,0);
	    l1=dweibull(yy,lambda,bt,0);
	    break;
	  case 3: /* gamma distribution */
	    H=pgamma(yy,lambda,bt,1,0);
	    l1=dgamma(yy,lambda,bt,0);
	    break;
	  case 4: /* generalized logistic distribution */
	    H=exp(-yy/lambda)*pow((lambda+intercept)/(lambda+intercept*exp(-bt*yy)),1/(lambda*bt));
	    l1=exp(-yy/lambda)*pow((lambda+intercept)/(lambda+intercept*exp(-bt*yy)),1/(lambda*bt)+1);
	    break;
	  case 5: /* log normal distribution */
	    H=pnorm(ly,bt,lambda,1,0);
	    l1=dnorm(ly,bt,lambda,0)/yy;
	    break;
	  case 6: /* log logistic distribution */
	    H=plogis(ly,bt,lambda,1,0);
	    l1=dlogis(ly,bt,lambda,0)/yy;
	    break;
	  case 7: /* log Cauchy distribution */
	    H=pcauchy(ly,bt,lambda,1,0);
	    l1=dcauchy(ly,bt,lambda,0)/yy;
	    break;
	  case 8: /* log Laplace distribution */
	    tmp=exp(-fabs(ly-bt)/lambda)/2;
	    H=ly<bt?tmp:1-tmp;
	    l1=tmp/lambda/yy;
	    break;}}
	/* calculate likelihood */
	if(l1<1e-20)l1=1e-20;
	if(H<1e-20)H=1e-20;
	if(H>1e20)H=1e20;
	/* is this correct when c>1 ? */
	if(*dist==2){
	  if(c)*like-=c*log(l1)-b*H-lgammafn(a)+log(b)+(a-1)*log(b*H);
	  /*	if(c)*like-=c*log(l1)+log(dgamma(H,a,1/b));*/
	  else *like-=pgamma(H,a,1/b,0,1);}
	else if(*dist==3){
	  if(c)*like-=c*log(l1)-pow(b*H,a)+(a-1)*log(b*H)+log(b)+log(a);
	  else *like+=pow(b*H,a);}
	if(c>1)*like+=lgammafn(c+1);
	if(*fit){
	  pred[nm-c+cens[nm]]=bt;
	  tmp=a/b;
	  if(!*density){
	    switch(*model){
	    case 1: rpred[nm-c+cens[nm]]=bt*tmp; break;
	    case 2: rpred[nm-c+cens[nm]]=bt*pow(tmp,1/lambda); break;
	    case 3: rpred[nm-c+cens[nm]]=qgamma(1-exp(-tmp),lambda,bt,1,0); break;
	    case 5: rpred[nm-c+cens[nm]]=exp(qnorm(1-exp(-tmp),bt,lambda,1,0)); break;
	    case 6: rpred[nm-c+cens[nm]]=exp(qlogis(1-exp(-tmp),bt,lambda,1,0)); break;
	    case 7: rpred[nm-c+cens[nm]]=exp(qcauchy(1-exp(-tmp),bt,lambda,1,0)); break;
	    case 8: rpred[nm-c+cens[nm]]=exp(bt+lambda*log(2*(ly<bt?exp(-tmp):1-exp(-tmp)))); break;}}
	  else{
	    switch(*model){
	    case 1: rpred[nm-c+cens[nm]]=qexp(tmp,bt,1,0); break;
	    case 2: rpred[nm-c+cens[nm]]=qweibull(tmp,lambda,bt,1,0); break;
	    case 3: rpred[nm-c+cens[nm]]=qgamma(tmp,lambda,bt,1,0); break;
	    case 5: rpred[nm-c+cens[nm]]=exp(qnorm(tmp,bt,lambda,1,0)); break;
	    case 6: rpred[nm-c+cens[nm]]=exp(qlogis(tmp,bt,lambda,1,0)); break;
	    case 7: rpred[nm-c+cens[nm]]=exp(qcauchy(tmp,bt,lambda,1,0)); break;
	    case 8: rpred[nm-c+cens[nm]]=exp(bt+lambda*log(2*(ly<bt?tmp:1-tmp))); break;}}}
	/* update parameters */
	switch(*dep){
	case 1:
	case 2: om=pow(omega,yy); a=om*a1+(1-om)*delta; break;
	case 3:
	case 4: a=omega*a1+(1-omega)*(delta-1); break;
	case 5: a=a1; break;
	case 6:
	case 7: a=omega*a1; break;
	default: break;}
	switch(*dep){
	case 1: b=pow(b/(H*delta),om)*delta; break;
	case 2: b=pow(H,-om)*delta; break;
	case 3: b=pow(b/(H*delta),omega)*delta; break;
	case 4: b=pow(H,-omega)*delta; break;
	case 5:
	case 7: b=pow(b/H,omega); break;
	default: break;}
      c=0;}
      nm++;}}
  return;}
