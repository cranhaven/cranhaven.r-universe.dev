#ifndef R3CCRDATA
#define R3CCRDATA

using std::vector;

namespace extendedleaps {

class partialrnk3ccrdata :  public partialccrdata {     
	public:
		partialrnk3ccrdata(vind nvars,vind hrank);
		partialrnk3ccrdata(vind nvars,vind hrank,real r2,real w,real bp,real lh);
		real*	getlhtmpv(void)	{ return &lhtmpv[0]; }
		const real	getlawhot(void)   const	{ return lawhotst; }
		void setlawhot(real lh)	{ lawhotst = lh; }	
	private:
		real			lawhotst;
		vector<real>		lhtmpv;  	
	friend class rnk3ccrdata;
};

class rnk3ccrdata : public ccrdata {
	public:
		rnk3ccrdata(vind nv,vind tnv,vind nvtopiv,real w,real bp,real lh,real r2);
		virtual ~rnk3ccrdata(void) { }
		virtual void  getpdata(partialdata *pd);  
		void setheinvel(vind i,vind j,real val)	{ heinv[i][j] = val; }
		virtual bool spdcupd(void)  { return true; }
		virtual bool usebounds(void)  { return true; }
		virtual void pivot(direction dir,mindices& mmind,vind vp,vind t,partialdata* pdt,subsetdata* fdt,
					bool last,bool& reliable,const double tol);
		virtual subsetdata *crcopy(vind totalnv,vind partialnv)  const
			{  return new rnk3ccrdata(nvar,totalnv,partialnv,wilksst,bartpist,lawhotst,ccr12);  }
	private:
		virtual real updatecrt(direction dir,vind varind,partialdata* newdtpnt,
					bool& reliable,const double tol,const double rqbound) const;
		template<accesstp tp> 
			void rnk3pivot(lagindex<tp>& prtmmit,vind vp,vind t,partialdata* newpdtpnt,subsetdata* newfdtpnt,bool last,
					bool& reliable,const double tol);
		real			lawhotst;
		vector< vector<real> >	heinv;
};

}

#endif
