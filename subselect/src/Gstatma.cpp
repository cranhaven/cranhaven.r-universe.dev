// #include <cassert>
#include "Sscma.h"
#include "Subsets.h"
#include "Vsmabo.h"

namespace extendedleaps {

extern vind flsts,lastvar,mindim;  
extern vector<psbstlist> bsts;

sbset *csbset(vind n,vector<vind>& v,real c,real ind);

void savfrst()
{
	subset* fset;

//	assert(flst >= 0 && flst < p+1);
	fset = &SW->subsetat(flst+1);
	for (vind i=lp;i<p-1;i++) {
		actv[i-lp] = fset->getithvar(i) + 1;
//		assert(actv[i-lp] > 0 && actv[i-lp] <= p);
	}
//	assert(lastvar > 0 && lastvar <= p);  
	actv[p-lp-1] = lastvar;
	sbset* st = csbset(fp,actv,fset->getdata().criterion(),fset->getdata().indice());
	bsts[0]->insert(st);
	#ifdef COUNTING
	cntg++;
	#endif
	return;
}

void savfull(void)
{
	subset* wlst; 

//	assert(flst >= 0 && flst <= p);
	wlst = &IW->subsetat(flst+1);
	{ for (vind i=0;i<fp;i++) {
		actv[i] = wlst->getithvar(i)+1;
//		assert(actv[i] > 0 && actv[i] <= p);
	} }
	{ for (vind i=fp+lp;i<p-1;i++) {
		actv[i-lp] = wlst->getithvar(i)+1;
//		assert(actv[i-lp] > 0 && actv[i-lp] <= p);
	} }
//	assert(lastvar > 0 && lastvar <= p);  
	actv[p-lp-1] = lastvar;
	sbset*  st = csbset(p-lp,actv,wlst->getdata().criterion(),wlst->getdata().indice());
	bsts[p-lp-mindim]->insert(st);
	#ifdef COUNTING
	cntg++;
	#endif
	return;
}

}

