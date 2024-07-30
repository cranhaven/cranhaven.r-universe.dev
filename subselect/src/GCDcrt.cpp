#include <cmath>
#include "Sscma.h"
#include "Vsmabo.h"
#include "Qforms.h"
#include "VSQforms.h"
#include "GCDcrt.h"

using namespace std;

namespace extendedleaps {

inline const real partialgcddata::getcrt(void) const	
{ 
	return pqf->getsum(); 
}

partialfgcddata::partialfgcddata(vind nvars,vind npcs)		
{
	nvar = nvars;
	pqf = new partialsqfdata(npcs); 
}

partialvgcddata::partialvgcddata(vind nvars,vind npcs)		
{
	nvar = nvars;
	pqf = new partialvsqfdata(npcs); 
}
		
inline const real gcddata::criterion(void) const	
{ 
	return sqf->qfsum();  
}

inline void gcddata::setcriterion(real c)			
{
	sqf->setqfsum(c); 
}

fgcddata::fgcddata(vind nv,vind tnv,vind nvtopiv,vind npcs,real crt)
	: q(npcs)
{
	nvar = nv;
	try {
		sqf = new sqfdata(tnv,nvtopiv,npcs,crt);
	}		
	catch (...)  {
		sqf = 0;
		throw;
	}	
}

inline const real fgcddata::indice(void) const	
{
	return criterion()/std::sqrt(static_cast<real>(q*nvar)); 
} 


void  fgcddata::getpdata(partialdata* pd)  
{ 
	partialfgcddata *pdasfgcd = static_cast<partialfgcddata *>(pd);    
	
	/* Attention: pd MUST point to partialfgcddata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert  */
	
/*	partialfgcddata *pdasfgcd = dynamic_cast<partialfgcddata *>(pd); 
	assert(pdasfgcd);                                                    */

	setcriterion(pdasfgcd->getcrt());
	nvar = pdasfgcd->nvar;
}

real fgcddata::updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const
{  
	partialfgcddata *newdata = static_cast<partialfgcddata *>(pdt);    
	
	/* Attention: newdtpnt MUST point to partialfgcddata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert   */
	
/*	partialfgcddata *newdata = dynamic_cast<partialfgcddata *>(pdt); 
	assert(newdata);                                                                    */

	if (dir==forward) newdata->nvar=nvar+1 ; 
	else newdata->nvar=nvar-1; 
 	return sqf->updatesum(mmind,var,newdata->pqf,reliable,tol);
} 

void fgcddata::pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol) 
{	

	partialfgcddata* newpdata = static_cast<partialfgcddata *>(pdt);    
	fgcddata* newfdata = static_cast<fgcddata *>(fdt);    
	
	/* Attention: pdt and fdt MUST point to partialgcddata and gcddata objects !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialgcddata* newpdata = dynamic_cast<partialgcddata *>(pdt);
	gcddata* newfdata = dynamic_cast<gcddata *>(fdt);
	assert(newpdata && newfdata);                                       */

	sqf->pivot(dir,mmind,vp,t,newpdata->pqf,newfdata->sqf,last,reliable,tol); 
} 

vgcddata::vgcddata(vind nv,vind tnv,vind nvtopiv,real vc0,real crt)
{
	nvar = nv;
	try {
		sqf = new vsqfdata(tnv,nvtopiv,tnv,vc0,crt);
	}
	catch (...)  {
		sqf = 0;
		throw;
	}	
}

vgcddata::vgcddata(vind nv,vind tnv,vind nvtopiv,const vector<real>& ovc,real crt)
{
	try {
		sqf = new vsqfdata(tnv,nvtopiv,tnv,ovc,crt);
	}		
	catch (...)  {
		sqf = 0;
		throw;
	}	
}

inline const real vgcddata::indice(void) const	
{
	return criterion()/nvar; 
} 

void  vgcddata::getpdata(partialdata* pd)  
{ 
	partialvgcddata *pdasvgcd = static_cast<partialvgcddata *>(pd);    
	
	/* Attention: pd MUST point to partialvgcddata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert   */
	
/*	partialvgcddata *pdasvgcd = dynamic_cast<partialvgcddata *>(pd);
	assert(pdasvgcd);                                                       */

	setcriterion(pdasvgcd->getcrt());
	nvar = pdasvgcd->nvar;
	getvqfdata()->setvc(pdasvgcd->getpvqfdata()->gettmpvc());
}

real vgcddata::updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const
{  
	partialvgcddata *newdata = static_cast<partialvgcddata *>(pdt);    
	
	/* Attention: newdtpnt MUST point to partialvgcddata object !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialvgcddata *newdata = dynamic_cast<partialvgcddata *>(pdt);
	assert(newdata);                                                    */

	if (dir==forward) newdata->nvar=nvar+1 ; 
	else newdata->nvar=nvar-1; 
	return getvqfdata()->updatesum(dir,mmind,var,nvar,newdata->getpvqfdata(),reliable,tol);	
} 

void vgcddata::pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol)
{	
	partialvgcddata* newpdata = static_cast<partialvgcddata *>(pdt);    
	vgcddata* newfdata = static_cast<vgcddata *>(fdt);    
	
	/* Attention: pdt and fdt MUST point to partialvgcddata and vgcddata objects !!!
	   For safety, in debug mode use the alternative code with dynamic_cast and assert    */
	
/*	partialvgcddata* newpdata = dynamic_cast<partialvgcddata *>(pdt);
	vgcddata* newfdata = dynamic_cast<vgcddata *>(fdt);
	assert(newpdata && newfdata);                                            */

	getvqfdata()->pivot(dir,mmind,vp,t,nvar,newpdata->getpvqfdata(),newfdata->getvqfdata(),last,reliable,tol);
} 

inline const real* vgcddata::getbnds(void)	const	
{ 
	return getvqfdata()->getvc(); 
} 	

inline const real* vgcddata::getsqfparcels(void)	const	
{ 
	return getvqfdata()->getvc(); 
} 	


}
