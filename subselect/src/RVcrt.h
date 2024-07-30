#ifndef RVDATA
#define RVDATA

#include <deque> 
#include "SpecialArrays.h"

using std::deque;

namespace extendedleaps {

typedef vector< vector<real> >  twodarray; 

class rvgdata : public globaldata {
	public:
		rvgdata(vind);
		virtual			~rvgdata(void);
		real			trs2(void)	   const	{ return trs2_; }
		void			settrs2(real ts2)		{ trs2_ = ts2;  }
		void			sets2(vind i,vind j,real val)	{ (*s2)(i,j) = val; }
		real			gets2(vind i,vind j) const	{ return (*s2)(i,j); }
	private:
		vind			p;  
		symtwodarray*		s2;
		real			trs2_;
};

class partialrvdata :  public partialdata {                 /* Data used in criterion RV updates  */
	public:
		explicit		partialrvdata(vind nvariables);
		virtual			~partialrvdata(void)    {  }	
		real*			gettmpv(void)		{ return &tmpv[0]; }
		real*			getcndv(void)		{ return &cndv[0]; }
		twodarray&		getm1t(void)  		{ return m1t; }
		virtual const real	getcrt(void) const		{ return crt; }
		const real		getpivotval(void) const	{ return pivotval; }
		void 			setcrt(real c) 		{ crt = c; }
		void 			setpivotval(real pv)	{ pivotval = pv; }
	protected:
		vind		p;  
		real		crt;
		real		pivotval;
		deque<bool>	vin; 
		vector<real>	tmpv;
		vector<real>	cndv;
		twodarray	m1t;
	friend class rvdata;
};

class rvdata :  public subsetdata {
	public:
		rvdata(vind lastvariab,vind nvtopiv,vind tnv,rvgdata* data,
			const deque<bool>& active,vind* origvarlist,real criterion);
		virtual ~rvdata(void);
		virtual void  getpdata(partialdata* pd);  
		virtual bool max(void)  { return true; }
		virtual const real criterion(void)	const	{ return crt;  }
		virtual void setcriterion(real c)		{ crt = c; }
		virtual const real indice(void)		const	{ return std::sqrt(crt/gdt->trs2()); } 
		virtual real updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol);
/*
	Note: subsetdata pointer must point to rvgdata class or unpredictable behaviour will result 
	(general subsetdata class was used in order to garantee upward compability)
*/
		virtual subsetdata *crcopy(vind lastvariab,vind partialnv) const
			{  return new rvdata(lastvariab,partialnv,p,gdt,varin,orgvar,crt);	}
		virtual void setorgvarl(vind* list)		{ orgvar = list; }
		virtual const real*	getbnds(void)	const	{ return 0; }
		void setcoefmatel(vind i,vind j,real val)	{ (*e)(i,j) = val; }
		void setcrt(real val)				{ crt = val; }
		rvgdata *getgdata(void) const			{ return gdt; }
		void  sets2m1(vind i,vind j,real val)		{ s2m1[i][j] = val; }
		real  gets2m1(vind i,vind j) const		{ return s2m1[i][j]; }
		virtual bool nopivot(void) const	{ return unreliable; }	
		virtual void forbidpivot(void)	{ unreliable = true; }
		virtual void allowpivot(void)   { unreliable = false; }	
	private:
		real updatecrt(direction dir,lagindex<d>& prtmmit,itindex<d>& fmmind,vind var,partialdata* newdtpnt,bool& reliable,const double tol) const;
		real updatecrt(direction dir,lagindex<i>& prtmmit,itindex<i>& fmmind,vind var,partialdata* newdtpnt,bool& reliable,const double tol) const;
		void pivot(direction dir,lagindex<d>& prtmmit,itindex<d>& fmmind,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,
				bool last,bool& reliable,const double tol);
		void pivot(direction dir,lagindex<i>& prtmmit,itindex<i>& fmmind,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,
				bool last,bool& reliable,const double tol);
		bool nopivot(lagindex<d>& prtmmit,vind vp) const;
		bool nopivot(lagindex<i>& prtmmit,vind vp) const;
		void cmpts2sm1(lagindex<d>&,itindex<d>&,partialrvdata* pdata,twodarray& outmat,vind* orgvlst,vind vp,bool* rowlst,bool* collst,
				bool reorder) const;
		void cmpts2sm1(lagindex<i>& prtmmit,itindex<i>& fmmind,partialrvdata* pdata,twodarray& outmat,vind* orgvlst,vind vp,bool* rowlst,
				bool* collst,bool reorder) const;
/*  Computation of the S2*S^1 matrix product for sub-matrices defined by row (rowlst) and column (collst) boolean lists  */
		real frobenius(twodarray& m,bool *inlst) const;
/* Computation of the Frobenius norm for the sub-matrix defined by the boolean list inlst  */
		vind			lastv;
		vind			p;
		vind			k;
		real			crt;
		deque<bool>		varin; 
		vind*			orgvar;
		symtwodarray*		e;
		vector<matvectarray *>	ivct;
		twodarray		s2m1;
		rvgdata*		gdt;
		real**			rpl;  // Vector of pointers to reals whose accuracy will be monitered
		bool			unreliable;
};

}

#endif
