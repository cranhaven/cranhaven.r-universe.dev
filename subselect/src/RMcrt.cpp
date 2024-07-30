#include <cmath>
#include "Sscma.h"
#include "Vsmabo.h"
#include "RMcrt.h"

namespace extendedleaps {

#ifdef COUNTING  
extern int fpcnt1;
#endif

partialrmdata::partialrmdata(vind nvariables)
  :   p(nvariables)
{
	tmpv.resize(p);
	for (int i=0;i<p;i++) tmpv[i] = 0.;
}

rmdata::rmdata(vind lastvariab,vind nvtopiv,vind tnv,rmgdata *data,const deque<bool>& active,real criterion)
  :  lastv(lastvariab), p(tnv), k(nvtopiv),  crt(criterion), varin(active), e(0), gdt(data) , rpl(0), unreliable(false)
{
	try {
		if (k > 0) {
			ovct.assign(p,0);
			e = new symtwodarray(k);
			{ for (vind i=0;i<p;i++) {
				if (i+k >= lastv) ovct[i] = new matvectarray(k,e,i-(lastv-k));
				else ovct[i] = new matvectarray(k,0,0);
			} }
		}
		rpl = new real *[2];
	}
	catch (...)   {
		delete e;
		{ for (unsigned i=0;i<ovct.size();i++) delete ovct[i]; }
		delete[] rpl;	
		throw;
	}
}

rmdata::~rmdata()
{
	{ for (unsigned i=0;i<ovct.size();i++) delete ovct[i]; }
	delete e;
	delete[] rpl;
}

real rmdata::updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const
{ 
	if (mmind.direct()) return updatecrt(dir,(*(mmind.idfm())),var,(*(mmind.idpm()))[var-1],pdt,reliable,tol); 
	else return updatecrt(dir,*(mmind.iifm()),var,(*(mmind.iipm()))[var-1],pdt,reliable,tol); 
}

void rmdata::pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol)
{ 
	if (mmind.direct()) pivot(dir,*(mmind.idpm()),*(mmind.idfm()),vp,t,pdt,fdt,last,reliable,tol); 
	else pivot(dir,*(mmind.iipm()),*(mmind.iifm()),vp,t,pdt,fdt,last,reliable,tol); 
}

real rmdata::updatecrt(direction dir,itindex<d>& fmmind,vind var,vind varind,partialdata* newdtpnt,bool& reliable,const double tol) const
{
	partialrmdata *newdata = static_cast<partialrmdata *>(newdtpnt);    
	
	/*  Attention: newdtpnt MUST point to partialrmdata object !!!
	    For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialrmdata *newdata = dynamic_cast<partialrmdata *>(newdtpnt);
	assert(newdata);                                                       */

	real *tv = newdata->gettmpv(),ov1; 
	real newcrt=crt,e1 = (*e)(varind,varind);

	reliable = true;
	if (dir == forward) newcrt -= e1;
	else newcrt -= real(1.)/e1;
	fmmind.reset();
	for (vind i=0;i<p;fmmind++,i++) {
		if (!varin[i] && (i!=var-1) ) {
			ov1 = (*ovct[fmmind()])[varind]; 
			tv[i] = ov1 / e1; 
			if (reliable) { 
				rpl[0] = &ov1; 
				rpl[1] = &tv[i]; 
				reliable = errcheck(rpl,tol,2);
			}
			newcrt -= tv[i] * ov1; 
			#ifdef COUNTING  
			fpcnt1 += 2;
			#endif
		}
	}
	if (reliable) { 
		rpl[0] = &e1; 
		rpl[1] = &newcrt; 
		reliable = errcheck(rpl,tol,2);
	}
	newdata->setpivotval(e1);
	newdata->setcrt(newcrt);
	return newcrt;
}

real rmdata::updatecrt(direction dir,itindex<i>& fmmind,vind var,vind varind,partialdata* newdtpnt,bool& reliable,const double tol) const
{
	partialrmdata *newdata = static_cast<partialrmdata *>(newdtpnt);    
	
	/*  Attention: newdtpnt MUST point to partialrmdata object !!!
	    For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialrmdata *newdata = dynamic_cast<partialrmdata *>(newdtpnt);
	assert(newdata);                                                       */

	real *tv = newdata->gettmpv(),ov1;
	real newcrt=crt,e1 = (*e)(varind,varind);

	reliable = true;
	rpl[0] = &e1;
	if (dir == forward) newcrt -= e1;
	else newcrt -= real(1.)/e1;
	fmmind.reset();
	for (vind i=0;i<p;fmmind++,i++) {
		if (!varin[i] && (i!=var-1) ) {
			ov1 = (*ovct[fmmind()])[varind]; 
			tv[i] = ov1 / e1; 
			if (reliable) { 
				rpl[0] = &ov1; 
				rpl[1] = &tv[i]; 
				reliable = errcheck(rpl,tol,2);
			}
			newcrt -= tv[i] * ov1; 
			#ifdef COUNTING  
			fpcnt1 += 2;
			#endif
		}
	}
	if (reliable) { 
		rpl[0] = &e1; 
		rpl[1] = &newcrt; 
		reliable = errcheck(rpl,tol,2);
	}
	newdata->setpivotval(e1);
	newdata->setcrt(newcrt);
	return newcrt;
}

void rmdata::pivot(direction dir,lagindex<d>& prtmmit,itindex<d>& fmmind,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,
			bool& reliable,const double tol)
{
	partialrmdata* newpdata = static_cast<partialrmdata *>(newpdtpnt);    
	rmdata* newfdata = static_cast<rmdata *>(newfdtpnt);    
	
	/*  Attention: newpdtpnt and newfdttpnt MUST point to partialrmdata and rmdata objects !!!
	    For safety, in debug mode use the alternative code with dynamic_cast and assert           */
	
/*	partialrmdata* newpdata = dynamic_cast<partialrmdata *>(newpdtpnt);
	rmdata* newfdata = dynamic_cast<rmdata *>(newfdtpnt);
	assert(newpdata && newfdata);                                              */

	real pivotval = newpdata->getpivotval();
	real *tv = newpdata->gettmpv();

	{ for (vind i=0;i<p;i++)  
		if (i+1 != vp) newfdata->varin[i] = varin[i]; }
	if (dir == backward) newfdata->varin[vp-1] = false;
	else newfdata->varin[vp-1] = true;
	symatpivot(prtmmit,pivotval,*e,*(newfdata->e),vp,t,reliable,tol);
	fmmind.reset();
	{ for (vind i=0;i<vp;fmmind++,i++)  
		if (i+1 != vp && !newfdata->varin[i])  {
			vectorpivot(prtmmit,*ovct[fmmind()],*newfdata->ovct[i],*e,tv[i],vp,t,reliable,tol);
			newfdata->ovct[i]->switchtoowndata();
	} }
	if (dir == backward) {
		prtmmit.reset(vp);
		for (vind j=vp;j<vp+t;prtmmit++,j++) 
			newfdata->ovct[vp-1]->setvalue(j-vp,-(*ovct[fmmind[vp-1]])[prtmmit()]/pivotval); 
		#ifdef COUNTING  
		fpcnt += t;
		#endif
		newfdata->ovct[vp-1]->switchtoowndata();
	}
	fmmind.reset(vp+t);
	{ for (vind i=vp+t;i<p;fmmind++,i++)  
		if (!newfdata->varin[i])  {
			vectorpivot(prtmmit,*ovct[fmmind()],*newfdata->ovct[i],*e,tv[i],vp,t,reliable,tol);
			newfdata->ovct[i]->switchtoowndata();
	} }
}

void rmdata::pivot(direction dir,lagindex<i>& prtmmit,itindex<i>& fmmind,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,
			bool& reliable,const double tol)
{
	partialrmdata* newpdata = static_cast<partialrmdata *>(newpdtpnt);    
	rmdata* newfdata = static_cast<rmdata *>(newfdtpnt);    
	
	/*  Attention: newpdtpnt and newfdttpnt MUST point to partialrmdata and rmdata objects !!!
	    For safety, in debug mode use the alternative code with dynamic_cast and assert           */
	
/*	partialrmdata* newpdata = dynamic_cast<partialrmdata *>(newpdtpnt);
	rmdata* newfdata = dynamic_cast<rmdata *>(newfdtpnt);
	assert(newpdata && newfdata);                                              */

	real pivotval = newpdata->getpivotval();
	real *tv = newpdata->gettmpv();

	{ for (vind i=0;i<p;i++)  
		if (i+1 != vp) newfdata->varin[i] = varin[i]; }
	if (dir == backward) newfdata->varin[vp-1] = false;
	else newfdata->varin[vp-1] = true;
	symatpivot(prtmmit,pivotval,*e,*(newfdata->e),vp,t,reliable,tol);
	fmmind.reset();
	{ for (vind i=0;i<vp;fmmind++,i++)  
		if (i+1 != vp && !newfdata->varin[i])  {
			vectorpivot(prtmmit,*ovct[fmmind()],*newfdata->ovct[i],*e,tv[i],vp,t,reliable,tol);
			newfdata->ovct[i]->switchtoowndata();
	} }
	if (dir == backward) {
		prtmmit.reset(vp);
		for (vind j=vp;j<vp+t;prtmmit++,j++) 
			newfdata->ovct[vp-1]->setvalue(j-vp,-(*ovct[fmmind[vp-1]])[prtmmit()]/pivotval); 
		#ifdef COUNTING  
		fpcnt += t;
		#endif
		newfdata->ovct[vp-1]->switchtoowndata();
	}
	fmmind.reset(vp+t);
	{ for (vind i=vp+t;i<p;fmmind++,i++)  
		if (!newfdata->varin[i])  {
			vectorpivot(prtmmit,*ovct[fmmind()],*newfdata->ovct[i],*e,tv[i],vp,t,reliable,tol); 
			newfdata->ovct[i]->switchtoowndata();
	} }
}

}

