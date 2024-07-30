#ifndef MSTATDATA
#define MSTATDATA

#include "SpecialArrays.h"

namespace extendedleaps {

class symtwodarray; 
class partialsqfdata; 
class sqfdata; 

class partialwilksdata :  public partialdata {     /* Data used in Wilks statistic updates    */
	public:
		partialwilksdata(vind nv,real w)  : nvar(nv), wilksst(w)	{  }
		virtual ~partialwilksdata(void)					{  }
		const real			getepivot(void) const		{ return epivot; }
		void 				setepivot(real pv)		{ epivot = pv; }
		const real			gettpivot(void) const		{ return tpivot; }
		void 				settpivot(real pv)		{ tpivot = pv; }
		virtual const real	getcrt(void)   const			{ return wilksst; }
		virtual void setcrt(real w)					{ wilksst = w; }	
	private:
		vind			nvar;
		real			epivot;
		real			tpivot;
		real			wilksst;
	friend class wilksdata;
};

class wilksdata :  public subsetdata {
	public:
		wilksdata(vind nv,vind tnv,vind nvtopiv,vind hr,real wst);
		virtual ~wilksdata(void);
		virtual bool max(void)  { return false; }
		virtual const real criterion(void) const { return wilksst; }
		virtual void setcriterion(real w)        { wilksst = w; }	
		virtual const real indice(void)	const; 
		virtual void  getpdata(partialdata *pd);  
		virtual real updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol);
/* 
	Note: partialdata and subsetdata pointer must point to partialwilksdata and wilksdata classes
		  or unpredictable behaviour will result  
		  (general partialdata and subsetdata classes were used in order to garantee upward compability)
*/
		virtual subsetdata *crcopy(vind totalnv,vind partialnv)  const
			{  return new wilksdata(nvar,totalnv,partialnv,hrank,wilksst);  }
		virtual const real*	getbnds(void)	const	{ return 0; }	
		void setematcoef(vind i,vind j,real val)   { (*emat)(i,j) = val;  }
		void settmatcoef(vind i,vind j,real val)   { (*tmat)(i,j) = val;  }
		virtual void setorgvarl(vind *) {  }
		virtual bool nopivot(void) const    { return unreliable; }
		virtual void forbidpivot(void)	{ unreliable = true; }	
		virtual void allowpivot(void)   { unreliable = false; }	
	private:
		real updatecrt(direction dir,vind varind,partialdata* newdtpnt,bool& reliable,const double tol) const; 
		void pivot(lagindex<d>& prtmmit,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,bool& reliable,const double tol);
		void pivot(lagindex<i>& prtmmit,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,bool& reliable,const double tol);
		bool nopivot(lagindex<d>& prtmmit,vind vp) const;
		bool nopivot(lagindex<i>& prtmmit,vind vp) const;
		vind		p;
		vind		k;
		vind		hrank;
		vind		nvar;
		real		wilksst;
		bool		unreliable;
		symtwodarray*	emat;
		symtwodarray*	tmat;
};

class partialtracedata :  public partialdata {     /* Data used in trace statistic updates	*/
	public:
		partialtracedata(vind nvars,vind hrank);
		virtual ~partialtracedata(void);
		virtual const real	getcrt(void)	const;
		partialsqfdata*  getpqfdata(void)	const	{ return pqf; }
	protected:
		vind			nvar;
		partialsqfdata*		pqf;
	friend class tracedata;
};

class tracedata :  public subsetdata {
	public:
		tracedata(vind nv,vind tnv,vind nvtopiv,vind hr,real crt);
		virtual ~tracedata(void);
		sqfdata*  getqfdata(void) const	{ return sqf; };
		virtual bool max(void)  { return true; }
		virtual const real criterion(void) const;
		virtual void setcriterion(real c);	
		virtual void setorgvarl(vind *) {  }
		virtual void  getpdata(partialdata *);  
		virtual real updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol);
/* 
	Note: partialdata and subsetdata pointer must point to partialtracedata and tracedata classes
		  or unpredictable behaviour will result  
		  (general partialdata and subsetdata classes were used in order to garantee upward compability)
*/
		virtual bool nopivot(void) const;
		virtual void forbidpivot(void);
		virtual void allowpivot(void);
		virtual const real*	getbnds(void)	const	{ return 0; }	
	protected:
		vind		hrank;
		vind		nvar;
		sqfdata*	sqf;
};

class bartpistdata : public tracedata {
	public:
		bartpistdata(vind nv,vind tnv,vind nvtopiv,vind hr,real crt) 
			:  tracedata(nv,tnv,nvtopiv,hr,crt) {  }
		virtual ~bartpistdata(void) { }
		virtual const real indice(void)	const;
		virtual subsetdata *crcopy(vind totalnv,vind partialnv)  const
			{  return new bartpistdata(nvar,totalnv,partialnv,hrank,criterion());  }
};

class lawlhotstdata : public tracedata {
	public:
		lawlhotstdata(vind nv,vind tnv,vind nvtopiv,vind hr,real crt) 
			:  tracedata(nv,tnv,nvtopiv,hr,crt) {  }
		virtual ~lawlhotstdata(void) { }
		virtual const real indice(void)	const;
		virtual subsetdata *crcopy(vind totalnv,vind partialnv)  const
			{  return new lawlhotstdata(nvar,totalnv,partialnv,hrank,criterion());  }
};

}

#endif

