#ifndef SETS
#define SETS

#include <set>
#include <vector> 

namespace extendedleaps {

class sbset  {
	public:
		sbset(int p,vind n);
		virtual ~sbset(void);
		vind*  actvar(void)	const	{ return actvar_; }
		vind   nvar(void)	const	{ return nvar_;  }
		real   crt(void)	const	{ return crt_;   }
		real   indice(void)	const	{ return ind_;   }
	private:
		int   	pos;
		vind  	nvar_;
		vind* 	actvar_;
		real	crt_;
		real	ind_;
		sbset(const sbset&);			/* Disallow copy constructur by clients  */
		sbset& operator=(const sbset&);	    	/* Disallow assignment by clients   */
	friend sbset *csbset(vind,vector<vind>&,real,real);
	friend void dsbset(sbset *);
};

class sbstsort {
	public:
		enum cmp_mode  {ascending,descending};
		sbstsort(cmp_mode m) :  mode(m)   {  }
		bool operator() (const sbset* const set1,const sbset* const set2)  const 
		{ return mode == descending ? set1->crt() > set2->crt() : set1->crt() < set2->crt(); }
	private:
		cmp_mode  mode;
};

typedef  sbset*					psbst;
typedef  std::multiset<sbset *,sbstsort>	sbstlist;
typedef  sbstlist*				psbstlist;

}

#endif
