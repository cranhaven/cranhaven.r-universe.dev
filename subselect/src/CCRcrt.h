#ifndef CCRDATA
#define CCRDATA

#include "SpecialArrays.h"

namespace extendedleaps {

using std::vector;

class partialccrdata :  public partialdata {    /* Data used in canonical correlation updates  */
   
	public:
		partialccrdata(vind nvars,vind hrank);
		partialccrdata(vind nvars,vind hrank,real r2,real w,real bp);
		virtual ~partialccrdata(void)				{  }
		real*				getbptmpv(void)		{ return &bptmpv[0]; }
		const real			getepivot(void) const	{ return epivot; }
		void 				setepivot(real pv)	{ epivot = pv; }
		const real			gettpivot(void) const	{ return tpivot; }
		void 				settpivot(real pv)	{ tpivot = pv; }
		const real	getwilks(void)   const			{ return wilksst; }
		void setwilks(real w)					{ wilksst = w; }	
		const real	getbartpi(void)   const			{ return bartpist; }
		void setbartpi(real bp)					{ bartpist = bp; }	
		virtual const real	getcrt(void)   const		{ return ccr12; }
		virtual void setcrt(real r2)				{ ccr12 = r2; }	
	protected:
		vind		nvar;
		real		epivot;
		real		tpivot;
		real		ccr12;
		real		wilksst;
		real		bartpist;
		vector<real>	bptmpv;  	
	friend class ccrdata;
	friend class rnk2ccrdata;
};

class ccrdata :  public subsetdata {
	public:
		ccrdata(vind nv,vind tnv,vind pnv,vind hr,real w,real bp,real r2);
		virtual ~ccrdata(void);
		virtual bool max(void)  { return true; }
		virtual const real criterion(void) const	{ return ccr12; }
		virtual void setcriterion(real r2)		{ ccr12 = r2; }	
		virtual const real indice(void)	const		{ return ccr12; } 
		void setematcoef(vind i,vind j,real val)	{ (*emat)(i,j) = val;  }
		void settmatcoef(vind i,vind j,real val)	{ (*tmat)(i,j) = val;  }
		void sethtinvel(vind i,vind j,real val)		{ htinv[i][j] = val; }
		virtual void  getpdata(partialdata *pd);  
		virtual real updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,
					bool& reliable,const double tol,const double rqbound) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,
					bool last,bool& reliable,const double tol);
/* 
	Note: partialdata and subsetdata pointer must point to partialccrdata and ccrdata classes or unpredictable behaviour will result  
	(general partialdata and subsetdata classes were used in order to garantee upward compability)
*/
		virtual const real*	getbnds(void)	const	{ return 0; }	
		virtual void setorgvarl(vind *) {  }
		virtual bool nopivot(void) const    { return unreliable; }
		virtual void forbidpivot(void)	{ unreliable = true; }
		virtual void allowpivot(void)   { unreliable = false; }
	protected:
		virtual real updatecrt(direction dir,vind varind,partialdata* newdtpnt,
					bool& reliable,const double tol,const double rqbound) const = 0; 
		void updatest(real& newwilksst,real& newbartpist,vind varind,partialccrdata* newdtpnt,bool& reliable,const double tol) const;
		void pivot(lagindex<d>& prtmmit,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,
				bool last,bool& reliable,const double tol);
		void pivot(lagindex<i>& prtmmit,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,
				bool last,bool& reliable,const double tol);
		vind			p;
		vind			k;
		vind			hrank;
		vind			nvar;
		real			ccr12;
		real			wilksst;
		real			bartpist;
		bool			unreliable;
		symtwodarray*		emat;
		symtwodarray*		tmat;
		vector< vector<real> >	htinv;
		private:
		real **rpl;  // Vector of pointers to reals whose accuracy will be monitered 
};

class rnk2ccrdata : public ccrdata {
	public:
		rnk2ccrdata(vind nv,vind tnv,vind pnv,real w,real bp,real r2) 
			:  ccrdata(nv,tnv,pnv,2,w,bp,r2) {  }
		virtual ~rnk2ccrdata(void) { }
		virtual subsetdata *crcopy(vind totalnv,vind partialnv)  const
			{  return new rnk2ccrdata(nvar,totalnv,partialnv,wilksst,bartpist,ccr12);  }
	protected:
		virtual real updatecrt(direction dir,vind varind,partialdata* newdtpnt,
					bool& reliable,const double tol,const double rqbound) const; 
};

}

#endif

