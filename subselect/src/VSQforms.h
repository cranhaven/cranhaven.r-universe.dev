#ifndef VSQFDATA
#define VSQFDATA

using std::vector;

namespace extendedleaps {

class partialvsqfdata : public partialsqfdata    {
/* Data used in updates of sums of quadratic forms with a variable number of parcels  */
	public:
		explicit partialvsqfdata(vind nparcels,real vc0=0.);
		real*   gettmpvc(void)	{ return &tmpvc[0]; }
		virtual	~partialvsqfdata(void)  {  }	
	protected:
		vector<real>	tmpvc;
	friend class vsqfdata;
};

class vsqfdata :  public sqfdata {
/* Subset data for sums of quadratic forms with a variable number of parcels  */
	public:
		vsqfdata(vind tnv,vind nvtopiv,vind nparcels,real vc0,real sum);
		vsqfdata(vind tnv,vind nvtopiv,vind nparcels,const vector<real>& ovc,real sum);
		virtual ~vsqfdata(void);
		real*   getvc(void)	{ return &vc[0]; }
		void  setvc(real* x,vind nparcels);  
		void  setvc(real* x)   { setvc(x,r);  }  
		virtual real updatesum(direction dir,mindices& mmind,vind var,vind dim,partialvsqfdata *pdt,bool& reliable,const double tol) const;
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,vind dim,partialvsqfdata* pdt,vsqfdata* fdt,bool last,
					bool& reliable,const double tol);
	private:
		real updatesum(direction dir,vind varind,vind dim,partialvsqfdata* newdata,bool& reliable,const double tol) const;   
		void pivot(direction dir,lagindex<d>& prtmmit,vind vp,vind t,vind dim,partialvsqfdata* newpdata,vsqfdata* newfdata,bool last,
			bool& reliable,const double tol);
		void pivot(direction dir,lagindex<i>& prtmmit,vind vp,vind t,vind dim,partialvsqfdata* newpdata,vsqfdata* newfdata
			,bool last,bool& reliable,const double tol);
		vector<real>	vc;
		real **rpl;  // Vector of pointers to reals whose accuracy will be monitered  
};

}

#endif
