#include <cmath>
#include "Sscma.h"
#include "Vsmabo.h"
#include "SpecialArrays.h"
#include "Qforms.h"
#include "MStcrt.h"

using namespace std;

namespace extendedleaps {

wilksdata::wilksdata(vind nv,vind tnv,vind nvtopiv,vind hr,real wst)
 :  p(tnv), k(nvtopiv), hrank(hr), nvar(nv), wilksst(wst), unreliable(false), emat(0), tmat(0)
{
	try {
		emat = new symtwodarray(k);
		tmat = new symtwodarray(k);
	}
	catch (...)  {
		delete emat;
		delete tmat;
		throw;
	}
}

wilksdata::~wilksdata(void)
{ 
	delete emat; 
	delete tmat; 
}

const real wilksdata::indice(void)	const	
{
	if (hrank < nvar) return 1. - std::pow(wilksst,1./hrank); 
	else return 1. - std::pow(wilksst,1./nvar); 
} 

void  wilksdata::getpdata(partialdata* pd)  
{ 
	partialwilksdata *pdaswilks = static_cast<partialwilksdata *>(pd);    
	
	/* Attention: pd MUST point to partialwilksdata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialwilksdata *pdaswilks = dynamic_cast<partialwilksdata *>(pd);
	assert(pdaswilks);                                                      */

	wilksst = pdaswilks->getcrt();
	nvar = pdaswilks->nvar;
}

real wilksdata::updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const
{ 
	if (mmind.direct()) return updatecrt(dir,(*(mmind.idpm()))[var-1],pdt,reliable,tol); 
	else return updatecrt(dir,(*(mmind.iipm()))[var-1],pdt,reliable,tol); 
}
   
void wilksdata::pivot(direction,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol)
{ 
	if (mmind.direct()) pivot(*(mmind.idpm()),vp,t,pdt,fdt,last,reliable,tol); 
	else pivot(*(mmind.iipm()),vp,t,pdt,fdt,last,reliable,tol); 
}

real  wilksdata::updatecrt(direction dir,vind varind,partialdata* newdtpnt,bool& reliable,const double tol) const
{  
	real *rpl[3];
	partialwilksdata *newdata = static_cast<partialwilksdata *>(newdtpnt);    

	/* Attention: newdtpnt MUST point to partialwilksdata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialwilksdata *newdata = dynamic_cast<partialwilksdata *>(pdt);
	assert(newdata);                                                       */

	if (dir==forward) newdata->nvar=nvar+1 ; 
	else newdata->nvar=nvar-1; 
	real e1 = (*emat)(varind,varind);
	real t1 = (*tmat)(varind,varind);
	real newwilksst = wilksst * (e1/t1);

	#ifdef COUNTING 
	fpcnt1 += 2;
	#endif

	rpl[0] = &e1;
	rpl[1] = &t1;
	rpl[2] = &newwilksst;
	reliable = errcheck(rpl,tol,3);

	newdata->setepivot(e1);
	newdata->settpivot(t1);
	newdata->setcrt(newwilksst);

	return newwilksst;
} 

void wilksdata::pivot(lagindex<d>& prtmmit,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,bool& reliable,const double tol)
{	
	partialwilksdata* newpdata = static_cast<partialwilksdata *>(newpdtpnt);    
	wilksdata* newfdata = static_cast<wilksdata *>(newfdtpnt);    

	
	/*  Attention: newpdtpnt and newfdtpnt MUST point to partialwilksdata and wilksdata objects !!!
	    For safety, in debug mode use the alternative code with dynamic_cast and assert             */
	
/*	partialwilksdata* newpdata = dynamic_cast<partialwilksdata *>(newpdtpnt);
	wilksdata* newfdata = dynamic_cast<wilksdata *>(newfdtpnt);
	assert(newpdata && newfdata);                                              */

	symatpivot(prtmmit,newpdata->getepivot(),*emat,*(newfdata->emat),vp,t,reliable,tol);
	symatpivot(prtmmit,newpdata->gettpivot(),*tmat,*(newfdata->tmat),vp,t,reliable,tol);
} 

void wilksdata::pivot(lagindex<i>& prtmmit,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,bool& reliable,const double tol)
{	
	partialwilksdata* newpdata = static_cast<partialwilksdata *>(newpdtpnt);    
	wilksdata* newfdata = static_cast<wilksdata *>(newfdtpnt);    
	
	/*  Attention: newpdtpnt and newfdtpnt MUST point to partialwilksdata and wilksdata objects !!!
	    For safety, in debug mode use the alternative code with dynamic_cast and assert             */
	
/*	partialwilksdata* newpdata = dynamic_cast<partialwilksdata *>(newpdtpnt);
	wilksdata* newfdata = dynamic_cast<wilksdata *>(newfdtpnt);
	assert(newpdata && newfdata);                                              */

	symatpivot(prtmmit,newpdata->getepivot(),*emat,*(newfdata->emat),vp,t,reliable,tol);
	symatpivot(prtmmit,newpdata->gettpivot(),*tmat,*(newfdata->tmat),vp,t,reliable,tol);
} 

partialtracedata::partialtracedata(vind nvars,vind hrank)		
	: pqf(0)
{
	nvar = nvars;
	pqf = new partialsqfdata(hrank); 
}

partialtracedata::~partialtracedata(void)
{ 	
	delete pqf;  
}

  const real partialtracedata::getcrt(void) const	
{ 
	return pqf->getsum(); 
}

tracedata::tracedata(vind nv,vind tnv,vind nvtopiv,vind hr,real crt)
	: hrank(hr), sqf(0)
{
	nvar = nv;
	sqf = new sqfdata(tnv,nvtopiv,hr,crt);		
}

tracedata::~tracedata(void) 
{ 
	delete sqf;
}
		
const real tracedata::criterion(void) const	
{ 
	return sqf->qfsum();  
}

void tracedata::setcriterion(real c)			
{
	sqf->setqfsum(c); 
}

void  tracedata::getpdata(partialdata* pd)  
{ 
	partialtracedata *pdastracest = static_cast<partialtracedata *>(pd);    
	
	/* Attention: pd MUST point to partialtracedata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert     */
	
/*	partialtracedata *pdasfgcd = dynamic_cast<partialtracedata *>(pd);
	assert(pdasfgcd);                                                    */

	setcriterion(pdastracest->getcrt());
	nvar = pdastracest->nvar;
}

real tracedata::updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const
{  
	partialtracedata *newdata = static_cast<partialtracedata *>(pdt);    
	
	/* Attention: newdtpnt MUST point to partialtracedata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert     */
	
/*	partialtracedata *newdata = dynamic_cast<partialtracedata *>(pdt);
	assert(newdata);                                                    */

	if (dir==forward) newdata->nvar=nvar+1 ; 
	else newdata->nvar=nvar-1; 
	return sqf->updatesum(mmind,var,newdata->pqf,reliable,tol);
} 

void tracedata::pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol)
{	
	partialtracedata* newpdata = static_cast<partialtracedata *>(pdt);    
	tracedata* newfdata = static_cast<tracedata *>(fdt);    
	
	/* Attention: pdt and fdt MUST point to partialtracedata and gcddata objects !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialtracedata* newpdata = dynamic_cast<partialtracedata *>(pdt);
	tracedata* newfdata = dynamic_cast<tracedata *>(fdt);
	assert(newpdata && newfdata);                                  */

	sqf->pivot(dir,mmind,vp,t,newpdata->pqf,newfdata->sqf,last,reliable,tol);
} 

bool tracedata::nopivot(void) const
{ 
	return sqf->nopivot(); 
}

void tracedata::forbidpivot(void)
{ 
	sqf->forbidpivot(); 
}

void tracedata::allowpivot(void)
{ 
	sqf->allowpivot(); 
}

const real bartpistdata::indice(void)	const	
{
	if (hrank < nvar) return criterion()/hrank; 
	else return criterion()/nvar; 
} 

const real lawlhotstdata::indice(void)	const	
{
	if (hrank < nvar) return criterion()/(criterion()+hrank); 
	else return criterion()/(criterion()+nvar); 
} 

}
