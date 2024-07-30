#ifndef QFDATA1
#define QFDATA1

#include "SpecialArrays.h"

using std::vector;

namespace extendedleaps {


class partialqfdata    {           
	public:
		explicit	partialqfdata(vind nparcels);
		virtual		~partialqfdata(void)    {  }	
		real*		gettmpv(void)			{ return &tmpv[0]; }
		const real	getpivotval(void) const	{ return pivotval; }
		void 		setpivotval(real pv)	{ pivotval = pv; }
	protected:
		real		pivotval;
		vind		r;  	
		vector<real>	tmpv;  	
	friend class qfdata;
};

class partialsqfdata : public partialqfdata    {           
	public:
		explicit partialsqfdata(vind nparcels) : partialqfdata(nparcels) {  }
		virtual		~partialsqfdata(void)   {  }	
		const real	getsum(void) const	{ return sum_; }
		void 		setsum(real s) 		{ sum_ = s; }
	protected:
		real		sum_;
	friend class sqfdata;
};

class partialsingleqfdata :  public partialdata {       /* Class for data used in single quadratic form criteria updates */
	public:
		partialsingleqfdata(void)			{  pqf=0; pqf = new partialsqfdata(1); }
		virtual ~partialsingleqfdata(void)		{ delete pqf; }
		virtual const real	getcrt(void) const	{ return pqf->getsum(); }
	protected:
		partialsqfdata*		pqf;
	friend class singleqfdata;
};

class qfdata   {
	public:
		qfdata(vind tnv,vind nvtopiv,vind nparcels);
		virtual ~qfdata(void);
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialqfdata* pdt,qfdata* fdt,
					bool last,bool& reliable,const double tol);
		void setvectel(vind i,vind j,real val)		{ ve[i][j] = val; }
		void setcoefmatel(vind i,vind j,real val)	{ (*e)(i,j) = val;  }
		virtual bool nopivot(void) const    { return unreliable; }
		virtual void forbidpivot(void)	{ unreliable = true; }	
		virtual void allowpivot(void)   { unreliable = false; }	
	protected:
		vind			p;
		vind			k;
		vind			r;
		bool			unreliable;
		vector< vector<real> >	ve;
		symtwodarray*		e;
	private:
		void pivot(lagindex<d>& prtmmit,vind vp,vind t,partialqfdata* newpdata,qfdata* newfdata,
				bool last,bool& reliable,const double tol); 
		void pivot(lagindex<i>& prtmmit,vind vp,vind t,partialqfdata* newpdata,qfdata* newfdata,
				bool last,bool& reliable,const double tol); 
		bool nopivot(lagindex<d>& prtmmit,vind vp) const;
		bool nopivot(lagindex<i>& prtmmit,vind vp) const;
};

class sqfdata   : public qfdata  {     /* Sum of quadratic forms     */
	public:
		sqfdata(vind tnv,vind nvtopiv,vind nparcels,real sum);	
		virtual ~sqfdata(void);	
		virtual const real qfsum(void) const	{ return sum_;  }
		virtual void setqfsum(real s)		{ sum_ = s; }
		virtual real updatesum(mindices& mmind,vind var,partialsqfdata* pdt,bool& reliable,const double tol) const;
	private:
		real updatesum(vind varind,partialsqfdata* newdata,bool& reliable,const double tol) const;
		real	sum_;
		real **rpl;  // Vector of pointers to reals whose accuracy will be monitered
};

class singleqfdata :  public subsetdata {
	public:
		singleqfdata(vind tnv,vind nvtopiv,real qfval)
			{  qf=0; qf = new sqfdata(tnv,nvtopiv,1,qfval); }
		virtual ~singleqfdata(void)			{ delete qf; }
		sqfdata*  getqfdata(void) const			{ return qf; };
		virtual bool max(void)  { return true; }
		virtual const real criterion(void) const	{ return qf->qfsum(); }
		virtual void setcriterion(real c)		{ qf->setqfsum(c); }
		virtual void setorgvarl(vind *)			{  }
		virtual real updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,
					bool& reliable,const double tol,const double) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,
					bool last,bool& reliable,const double tol);
/* 
	Note: partialdata and subsetdata pointer must point to partialsingleqfdata and singleqfdata classes
		  or unpredictable behaviour will result  
		  (general partialdata and subsetdata classes were used in order to garantee upward compability)
*/
		virtual subsetdata *crcopy(vind totalnv,vind partialnv)  const
			{  return new singleqfdata(totalnv,partialnv,criterion());  }
		virtual const real*	getbnds(void)	const	{ return 0; }	
		virtual bool nopivot(void) const { return qf->nopivot(); }
		virtual void forbidpivot(void)	 { qf->forbidpivot(); }	
		virtual void allowpivot(void)   {  qf->allowpivot(); }
	private:
		sqfdata*	qf;
};

}

#endif

