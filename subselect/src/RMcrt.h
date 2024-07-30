#ifndef RMDATA
#define RMDATA

#include <deque> 
#include "SpecialArrays.h"

using std::deque;

namespace extendedleaps {

class rmgdata : public globaldata {
	public:
		rmgdata(vind nvar):   	p(nvar) { }
		real	trs(void)	{ return trs_; }
		void	settrs(real ts)	{ trs_ = ts;  }
	private:
		vind	p;  
		real	trs_;
};

class partialrmdata :  public partialdata {                 /* Data used in criterion RM updates   */
	public:
		explicit		partialrmdata(vind);
		virtual			~partialrmdata(void)    {  }	
		real*			gettmpv(void)		{ return &tmpv[0]; }
		virtual const real	getcrt(void) const	{ return crt; }
		const real		getpivotval(void) const	{ return pivotval; }
		void 			setcrt(real c) 		{ crt = c; }
		void 			setpivotval(real pv)	{ pivotval = pv; }
	protected:
		vind		p;  
		real		crt;
		real		pivotval;
		vector<real>	tmpv;  	
	friend class rmdata;
};

class rmdata :  public subsetdata {
	public:
		rmdata(vind lastvariab,vind nvtopiv,vind tnv,rmgdata *data,const deque<bool>& active,real criterion=0.);
		virtual ~rmdata(void);
		virtual bool max(void)  { return false; }
		virtual const real criterion(void)	const	{ return crt;  }
		virtual void setcriterion(real c)		{ crt = c; }
		virtual const real indice(void)		const	{ return std::sqrt(1.-crt/gdt->trs()); } 
		virtual real updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol);
/*
	Note: subsetdata pointer must point to rmgdata class or unpredictable behaviour will result 
	(general subsetdata class was used in order to garantee upward compability)
*/
		virtual subsetdata *crcopy(vind lastvariab,vind partialnv) const
			{  return new rmdata(lastvariab,partialnv,p,gdt,varin,crt);  }
		virtual void setorgvarl(vind *) {  }
		virtual const real* getbnds(void)	const	{ return 0; }	
		void setcoefmatel(vind i,vind j,real val)	{ (*e)(i,j) = val;  }
		void setcrt(real val)			  	{ crt = val; }
		rmgdata* getgdata(void)	const		{ return gdt;  }
		virtual bool nopivot(void) const    { return unreliable; }
		virtual void forbidpivot(void)	{ unreliable = true; }
		virtual void allowpivot(void)   { unreliable = false; }	
	private:
		real updatecrt(direction dir,itindex<d>& fmmind,vind var,vind varind,partialdata* newdtpnt,bool& reliable,const double tol) const;
		real updatecrt(direction dir,itindex<i>& fmmind,vind var,vind varind,partialdata* newdtpnt,bool& reliable,const double tol) const;
		void pivot(direction dir,lagindex<d>& prtmmit,itindex<d>& fmmind,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,
				bool last,bool& reliable,const double tol);
		void pivot(direction dir,lagindex<i>& prtmmit,itindex<i>& fmmind,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,
				bool last,bool& reliable,const double tol);
		bool nopivot(lagindex<d>& prtmmit,vind vp) const;
		bool nopivot(lagindex<i>& prtmmit,vind vp) const;
		vind			lastv;
		vind			p;
		vind			k;
		real			crt;
		deque<bool>		varin; 
		symtwodarray*		e;
		vector<matvectarray *>	ovct;
		rmgdata*		gdt;
		real**			rpl;  // Vector of pointers to reals whose accuracy will be monitered
		bool			unreliable;
};

}

#endif
