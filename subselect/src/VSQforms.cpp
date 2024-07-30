#include <vector>
#include "Sscma.h"
#include "Vsmabo.h"
#include "Qforms.h"
#include "VSQforms.h"

namespace extendedleaps {

#ifdef COUNTING  
extern int fpcnt1;
#endif

partialvsqfdata::partialvsqfdata(vind nparcels,real vc0)
  :  partialsqfdata(nparcels)
{
	tmpvc.resize(nparcels);
	tmpvc.assign(nparcels,vc0);
}

vsqfdata::vsqfdata(vind tnv,vind nvtopiv,vind nparcels,real vc0,real sum)
  :  sqfdata(tnv,nvtopiv,nparcels,sum), rpl(0)
{
	vc.resize(nparcels);
	vc.assign(nparcels,vc0);
	rpl = new real *[2*r+2]; 
}

vsqfdata::vsqfdata(vind tnv,vind nvtopiv,vind nparcels,const vector<real>& ovc,real sum)
  :  sqfdata(tnv,nvtopiv,nparcels,sum)
{
	vc.resize(nparcels);
	for (vind j=0;j<nparcels;j++) vc[j] = ovc[j]; 
}

vsqfdata::~vsqfdata()
{  
	delete[] rpl;
}

void  vsqfdata::setvc(real* x,vind nparcels)  
{ 
	for (vind j=0;j<nparcels;j++) vc[j] = x[j];
}

real vsqfdata::updatesum(direction dir,mindices& mmind,vind var,vind dim,partialvsqfdata *pdt,bool& reliable,const double tol) const
{
	if (mmind.direct()) return updatesum(dir,(*(mmind.idpm()))[var-1],dim,pdt,reliable,tol); 
	else return updatesum(dir,(*(mmind.iipm()))[var-1],dim,pdt,reliable,tol); 
}

void vsqfdata::pivot(direction dir,mindices& mmind,vind vp,vind t,vind dim,partialvsqfdata* pdt,vsqfdata* fdt,bool last,bool& reliable,const double tol)
{ 
	if (mmind.direct()) pivot(dir,*(mmind.idpm()),vp,t,dim,pdt,fdt,last,reliable,tol); 
	else pivot(dir,*(mmind.iipm()),vp,t,dim,pdt,fdt,last,reliable,tol); 
}

real vsqfdata::updatesum(direction dir,vind varind,vind dim,partialvsqfdata* newdata,bool& reliable,const double tol) const 
{
	vind maxk=0;
	real *tv = newdata->gettmpv(),*newvc=newdata->gettmpvc(),ve1; 
	real inc,newsum,e1=(*e)(varind,varind); 

	rpl[0] = &e1;	
	switch (dir)  {
		case forward:
			maxk = dim+1;
			if (maxk > r) maxk = r;
			newsum = qfsum() + vc[dim];
			break;
		case backward:
			maxk = dim-1;
			if (maxk > r) maxk = r;
			if (r > dim-1) newsum = qfsum() - vc[dim-1];
			else newsum = qfsum();
			break;
	}
	for (vind j=0;j<maxk;j++) {
		rpl[2*j+1] = &(ve1 = ve[j][varind]); 
		rpl[2*j+2] = &(tv[j] = ve1 / e1);
		newvc[j] = vc[j] + (inc = tv[j]*ve1);	
		newsum += inc;
	} 
	rpl[2*maxk+1] = &newsum;
	reliable = errcheck(rpl,tol,2*maxk+2);

	newdata->setpivotval(e1);
	newdata->setsum(newsum);

	#ifdef COUNTING  
	fpcnt1 += 2*maxk;
	#endif

	return newsum;
}

void vsqfdata::pivot(direction dir,lagindex<d>& prtmmit,vind vp,vind t,vind dim,partialvsqfdata* newpdata,vsqfdata* newfdata,bool last,
			bool& reliable,const double tol)
{
	vind pivotind(prtmmit[vp-1]),newdim(dim),maxk(0);
	real pivotval = newpdata->getpivotval();
	real *tv = newpdata->gettmpv();

	switch (dir)  {
		case forward:
			maxk = (newdim = dim+1) + t;
			if (maxk > r) maxk = r;
			break;
		case backward:
			maxk = newdim = dim-1;
			if (maxk > r) maxk = r;
			break;
	}
	{ for (vind j=newdim;j<maxk;j++) {
		tv[j] = ve[j][pivotind]/pivotval;
		newfdata->vc[j] = vc[j] + tv[j]*ve[j][pivotind];
	} }
	#ifdef COUNTING  
	if (maxk > newdim) fpcnt += 2*(maxk - newfdata->dim);
	#endif

	symatpivot(prtmmit,pivotval,*e,*(newfdata->e),vp,t,reliable,tol);
	for (vind j=0;j<maxk;j++) 
		vectorpivot(prtmmit,ve[j],newfdata->ve[j],*e,tv[j],vp,t,reliable,tol);	
}

void vsqfdata::pivot(direction dir,lagindex<i>& prtmmit,vind vp,vind t,vind dim,partialvsqfdata* newpdata,vsqfdata* newfdata,bool last,
			bool& reliable,const double tol)
{
	vind pivotind(prtmmit[vp-1]),newdim(dim),maxk(0);
	real pivotval = newpdata->getpivotval();
	real *tv = newpdata->gettmpv();

	switch (dir)  {
		case forward:
			maxk = (newdim = dim+1) + t;
			if (maxk > r) maxk = r;
			break;
		case backward:
			maxk = newdim = dim-1;
			if (maxk > r) maxk = r;
			break;
	}
	{ for (vind j=newdim;j<maxk;j++) {
		tv[j] = ve[j][pivotind]/pivotval;
		newfdata->vc[j] = vc[j] + tv[j]*ve[j][pivotind];
	} }
	#ifdef COUNTING  
	if (maxk > newdim) fpcnt += 2*(maxk - newfdata->dim);
	#endif

	symatpivot(prtmmit,pivotval,*e,*(newfdata->e),vp,t,reliable,tol);
	for (vind j=0;j<maxk;j++) 
		vectorpivot(prtmmit,ve[j],newfdata->ve[j],*e,tv[j],vp,t,reliable,tol); 
}



}
