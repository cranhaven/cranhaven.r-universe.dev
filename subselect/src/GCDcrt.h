#ifndef GCDDATA
#define GCDDATA

namespace extendedleaps {

class sqfdata;		/* forward declaration     */
class partialsqfdata;   /* forward declaration     */
class vsqfdata;		/* forward declaration     */
class partialvsqfdata;  /* forward declaration     */

using std::vector;

class partialgcddata :  public partialdata {       /* Base class for data used in criterion gcd updates   */
	public:
		virtual ~partialgcddata(void) { delete pqf; }
		virtual const real	getcrt(void)   const;
		partialsqfdata*  getpqfdata(void) const { return pqf; }
	protected:
		vind		nvar;
		partialsqfdata*	pqf;
	friend class gcddata;
};

class partialfgcddata :  public partialgcddata {
/* Data used in criterion gcd, with a fixed number of components, updates  */
	public:
		partialfgcddata(vind nvars,vind npcs);
		virtual ~partialfgcddata(void)        {  }
	friend class fgcddata;
};

class partialvgcddata :  public partialgcddata {
/* Data used in criterion gcd, with a variable number of components, updates  */
	public:
		partialvgcddata(vind nvars,vind npcs);
		virtual ~partialvgcddata(void)       {   }
		partialvsqfdata*  getpvqfdata(void)	
			{ return static_cast<partialvsqfdata *>(pqf); }
	friend class vgcddata;
};

class gcddata :  public subsetdata {
	public:
		virtual ~gcddata(void) { delete sqf; }
		sqfdata*  getqfdata(void) const	{ return sqf; };
		virtual bool max(void)  { return true; }
		virtual const real criterion(void) const;
		virtual void setcriterion(real c);	
		virtual void setorgvarl(vind *) {  }
		virtual bool nopivot(void) const	{ return sqf->nopivot(); }
		virtual void forbidpivot(void)	{ sqf->forbidpivot(); }	
		virtual void allowpivot(void)   { sqf->allowpivot(); }
	protected:
		vind		nvar;
		sqfdata*	sqf;
	private:
		bool nopivot(lagindex<d>& prtmmit,vind vp) const;
		bool nopivot(lagindex<i>& prtmmit,vind vp) const;
};

class fgcddata :  public gcddata {
	public:
		fgcddata(vind nv,vind tnv,vind nvtopiv,vind npcs,real crt);
		virtual ~fgcddata(void) { };
		virtual const real indice(void)	const; 
		virtual void  getpdata(partialdata *);  
		virtual real updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,
			bool& reliable,const double tol,const double) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,
					bool last,bool& reliable,const double tol);
/* 
	Note: partialdata and subsetdata pointer must point to partialfgcddata and fgcddata classes
		  or unpredictable behaviour will result  
		  (general partialdata and subsetdata classes were used in order to garantee upward compability)
*/
		virtual subsetdata *crcopy(vind totalnv,vind partialnv)  const
			{  return new fgcddata(nvar,totalnv,partialnv,q,criterion());  }
		virtual const real*	getbnds(void)	const	{ return 0; }	
	private:
		vind  q;		/* Number of spectral decomposition parcels kept in    */
};

class vgcddata :  public gcddata {
	public:
		vgcddata(vind nv,vind tnv,vind nvtopiv,real vc0,real crt);
		vgcddata(vind nv,vind tnv,vind nvtopiv,const vector<real>& ovc,real crt);
		virtual ~vgcddata(void)     { }
		vsqfdata*  getvqfdata(void) const	
			{ return static_cast<vsqfdata *>(sqf); }
		virtual const real indice(void)	const; 
		virtual void  getpdata(partialdata *);  
		virtual real updatecrt(direction dir,mindices& mmind,vind var,partialdata* pdt,bool& reliable,const double tol,const double) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,bool last,bool& reliable,const double tol);
/* 
	Note: partialdata and subsetdata pointer must point  to partialvgcddata and vgcddata classes
		  or unpredictable behaviour will result  
		  (general partialdata and subsetdata classes were used in order to garantee upward compability)
*/
		virtual subsetdata *crcopy(vind totalnv,vind partialnv)  const
			{  return new vgcddata(nvar,totalnv,partialnv,totalnv,criterion());  }
		virtual const real*	getbnds(void) const; 	
		virtual const real*	getsqfparcels(void) const;	
};

}

#endif
