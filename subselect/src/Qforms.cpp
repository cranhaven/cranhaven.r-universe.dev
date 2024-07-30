#include "Sscma.h"
#include "Vsmabo.h"
#include "Qforms.h"

namespace extendedleaps {

#ifdef COUNTING 
extern int fpcnt1;
#endif

partialqfdata::partialqfdata(vind nparcels)
  :   r(nparcels)
{
	tmpv.resize(r);
	for (int i=0;i<r;i++) tmpv[i] = 0.;
}

qfdata::qfdata(vind tnv,vind nvtopiv,vind nparcels)
  :  p(tnv), k(nvtopiv), r(nparcels), unreliable(false), e(0)	
{
	ve.assign(r,vector<real>(k));
	e = new symtwodarray(k);
}

qfdata::~qfdata()
{
	delete e;
}

void qfdata::pivot(direction dir,mindices& mmind,vind vp,vind t,partialqfdata* pdt,qfdata* fdt,bool last,bool& reliable,const double tol)
{ 
	if (mmind.direct()) pivot(*(mmind.idpm()),vp,t,pdt,fdt,last,reliable,tol); 
	else pivot(*(mmind.iipm()),vp,t,pdt,fdt,last,reliable,tol); 
}

void qfdata::pivot(lagindex<d>& prtmmit,vind vp,vind t,partialqfdata* newpdata,qfdata* newfdata,bool last,bool& reliable,const double tol)
{
	symatpivot(prtmmit,newpdata->getpivotval(),*e,*(newfdata->e),vp,t,reliable,tol);
	for (vind j=0;j<r;j++) 
		vectorpivot(prtmmit,ve[j],newfdata->ve[j],*e,(newpdata->gettmpv())[j],vp,t,reliable,tol); 
}

void qfdata::pivot(lagindex<i>& prtmmit,vind vp,vind t,partialqfdata* newpdata,qfdata* newfdata,bool last,bool& reliable,const double tol)
{
	symatpivot(prtmmit,newpdata->getpivotval(),*e,*(newfdata->e),vp,t,reliable,tol);
	for (vind j=0;j<r;j++) 
		vectorpivot(prtmmit,ve[j],newfdata->ve[j],*e,(newpdata->gettmpv())[j],vp,t,reliable,tol); 
}

sqfdata::sqfdata(vind tnv,vind nvtopiv,vind nparcels,real sum)
  :  qfdata(tnv,nvtopiv,nparcels), sum_(sum), rpl(0) 
{  
	rpl = new real *[2*r+2];
}

sqfdata::~sqfdata(void)
{  
	delete[] rpl;
}

real sqfdata::updatesum(mindices& mmind,vind var,partialsqfdata* pdt,bool& reliable,const double tol) const
{ 
	if (mmind.direct()) return updatesum((*(mmind.idpm()))[var-1],pdt,reliable,tol); 
	else return updatesum((*(mmind.iipm()))[var-1],pdt,reliable,tol); 
}

real sqfdata::updatesum(vind varind,partialsqfdata* newdata,bool& reliable,const double tol) const
{
	real *tv = newdata->gettmpv(),ve1;
	real newsum = sum_,e1 = (*e)(varind,varind);

	rpl[0] = &e1;
	for (vind i=0;i<r;i++) {
		rpl[2*i+1] = &(ve1 = ve[i][varind]);
		rpl[2*i+2] = &(tv[i] = ve1 / e1);
		newsum += tv[i]*ve1;
	}
	rpl[2*r+1] = &newsum;
	reliable = errcheck(rpl,tol,2*r+2);

 	#ifdef COUNTING 
	fpcnt1 += 2*r + 4*r;
	#endif

	newdata->setpivotval(e1);
	newdata->setsum(newsum);

	return newsum;
}

real singleqfdata::updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const
{
	partialsingleqfdata *newdata = static_cast<partialsingleqfdata *>(pdt);
	
	// Attention: pdt MUST point to partialsingleqfdata object !!!
	// For safety, in debug mode use the alternative code with dynamic_cast and assert
	
//	partialsingleqfdata *newdata = dynamic_cast<partialsingleqfdata *>(pdt);
//	assert(newdata);
	
	return qf->updatesum(mmind,var,newdata->pqf,reliable,tol);
}


void singleqfdata::pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol) 
{	
	partialsingleqfdata* newpdata = static_cast<partialsingleqfdata *>(pdt);    
	singleqfdata* newfdata = static_cast<singleqfdata *>(fdt);    
	
	/*  Attention: pdt and fdt MUST point to partialsingleqfdata and singleqfdata objects !!!
	    For safety, in debug mode use the alternative code with dynamic_cast and assert         */
	
/*	partialsingleqfdata* newpdata = dynamic_cast<partialsingleqfdata *>(pdt);
	singleqfdata* newfdata = dynamic_cast<singleqfdata *>(fdt);
	assert(newpdata && newfdata);                                */

	qf->pivot(dir,mmind,vp,t,newpdata->pqf,newfdata->qf,last,reliable,tol);
}

}
