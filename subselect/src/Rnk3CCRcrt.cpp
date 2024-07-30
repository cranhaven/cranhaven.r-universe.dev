#include <cmath>
#include "Sscma.h"
#include "Vsmabo.h"
#include "CCRcrt.h"
#include "NewtonRp.h"
#include "Rnk3CCRcrt.h"

using namespace std;

using namespace newtonrp;

namespace extendedleaps {

double findccr12(double w,double u,double v,double minacpt);	
/* Finds the first canonical correlation (squared), given the values of the three classical statistics
   Returns zero if proven that its value falls below minacpt                                             */

double a,b,c;	// Coeficients of the third degree equation:  x^3 + a x^2 + b x + c = 0	
double lhs(double x);	// Computes the left hand side (lhs) of third degree equation.
double lhsd(double x);  // Computes first derivative of lhs.
double lhsd2(double x); // Computes second derivative of lhs.

partialrnk3ccrdata::partialrnk3ccrdata(vind nvars,vind hrank)
  :	  partialccrdata(nvars,hrank)
{
	lhtmpv.resize(hrank);
}

partialrnk3ccrdata::partialrnk3ccrdata(vind nvars,vind hrank,real r2,real w,real bp,real lh)
  :	  partialccrdata(nvars,hrank,r2,w,bp), lawhotst(lh)
{
	lhtmpv.resize(hrank);
}

rnk3ccrdata::rnk3ccrdata(vind nv,vind tnv,vind nvtopiv,real w,real bp,real lh,real r2) 
  :  ccrdata(nv,tnv,nvtopiv,3,w,bp,r2), lawhotst(lh)  
{ 
	heinv.assign(3,vector<real>(k));
}
   
inline void rnk3ccrdata::pivot(direction,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol)
{ 
	if (mmind.direct()) rnk3pivot(*(mmind.idpm()),vp,t,pdt,fdt,last,reliable,tol); 
	else rnk3pivot(*(mmind.iipm()),vp,t,pdt,fdt,last,reliable,tol); 
}

void  rnk3ccrdata::getpdata(partialdata* pd)  
{ 
	partialrnk3ccrdata *pdasrnk3ccr = static_cast<partialrnk3ccrdata *>(pd);    
	
	/* Attention: pd MUST point to partialrnk3ccrdata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert     */
	
/*	partialrnk3ccrdata *pdasccr = dynamic_cast<partialrnk3ccrdata *>(pd);
	assert(pdasccr);                                                        */

	ccrdata::getpdata(pdasrnk3ccr);
	lawhotst = pdasrnk3ccr->getlawhot();
}

real rnk3ccrdata::updatecrt(direction dir,vind varind,partialdata* newdtpnt,bool& reliable,const double tol,const double rqbound) const 
{  
	partialrnk3ccrdata *newdata = static_cast<partialrnk3ccrdata *>(newdtpnt);    
	
	/* Attention: newdtpnt MUST point to partialrnk3ccrdata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert     */
	
/*	partialrnk3ccrdata *newdata = dynamic_cast<partialrnk3ccrdata *>(pdt);
	assert(newdata);                                                            */

	real newwilksst,newbartpist,newccr12;
	real e1 = (*emat)(varind,varind);
	real *tv = newdata->getlhtmpv(),hei1,*rpl[4];
	real newlawhotst = lawhotst;

	updatest(newwilksst,newbartpist,varind,newdata,reliable,tol);
	for (vind i=0;i<3;i++) {
		hei1 = heinv[i][varind]; 
		rpl[i] = &(tv[i] = hei1 / e1);
		newlawhotst += tv[i]*hei1;  
	}

	#ifdef COUNTING 
	fpcnt1 += 6;
	#endif

	if (reliable) {  
		rpl[3] = &newlawhotst; 
		reliable = errcheck(rpl,tol,4); 
	}  
	
	if (dir==forward) newdata->nvar=nvar+1 ; 
	else newdata->nvar=nvar-1;
	
	if (newdata->nvar == 1) newccr12 = newbartpist; 
 	else if (newdata->nvar == 2)  
		newccr12 = 0.5 * ( newbartpist +  std::sqrt(static_cast<real>(newbartpist*newbartpist -4.*(newbartpist+newwilksst-1.))) );
	else newccr12 = findccr12(newwilksst,newbartpist,newlawhotst,rqbound);
	newdata->setlawhot(newlawhotst);
	newdata->setcrt(newccr12);

	return newccr12;
} 

template<accesstp tp> 
void rnk3ccrdata::rnk3pivot(lagindex<tp>& prtmmit,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,bool& reliable,const double tol)
{
	partialrnk3ccrdata* newpdata = static_cast<partialrnk3ccrdata *>(newpdtpnt);    
	rnk3ccrdata* newfdata = static_cast<rnk3ccrdata *>(newfdtpnt);    
	
	/*  Attention: newpdtpnt and newfdtpnt MUST point to partialrnk3ccrdata and rnk3ccrdata objects !!!
	    For safety, in debug mode use the alternative code with dynamic_cast and assert                */
	
/*	partialrnk3ccrdata* newpdata = dynamic_cast<partialrnk3ccrdata *>(newpdtpnt);
	rnk3ccrdata* newfdata = dynamic_cast<rnk3ccrdata *>(newfdtpnt);
	assert(newpdata && newfdata);                                           */

	ccrdata::pivot(prtmmit,vp,t,newpdata,newfdata,last,reliable,tol);
	for (vind j=0;j<3;j++) 
		vectorpivot(prtmmit,heinv[j],newfdata->heinv[j],*emat,(newpdata->getlhtmpv())[j],vp,t,reliable,tol); 
} 

double findccr12(double w,double u,double v,double minacpt)
{
	double x2,y2;                // coordinates of lhs local minimun.
	double r1_2(0.),frstap;	

	a = -u;                    /* set coeficients of third degree equation   */
	b = 2*u -3 + w*(v+3);
	c = -b + u + w - 1;

	y2 = lhs( x2 = (-a+std::sqrt(static_cast<double>(a*a-3*b)))/3 );
	frstap = x2 + std::sqrt(static_cast<double>(-y2/(3*x2+a)));
  /* aproximate r1_2 by a second order expansion (noting that first derivative of lhs is null at x2)  */
	if (frstap > minacpt) 
		r1_2 = lsrch(frstap,lhs,lhsd,lhsd2,x2,frstap);    /* find r1_2 exact value   */	
	return r1_2;
}

double lhs(double x)
{
	double xsqr = x*x;
	return (x+a)*xsqr + b*x + c;
}

inline double lhsd(double x)
{
	return (3*x+2*a)*x + b;
}

inline double lhsd2(double x)
{
	return 6*x + 2*a;
}

}
