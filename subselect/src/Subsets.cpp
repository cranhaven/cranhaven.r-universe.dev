// #include <cassert>
#include "Sscma.h"
#include "Subsets.h"


namespace extendedleaps {

extern int sbsetind,maxsbst;

//extern sbset    **sbsarr;
extern vector<psbst> sbsarr;
sbset::sbset(int p,vind n)
 :  pos(p), nvar_(n), actvar_(0)
{
	actvar_ = new vind[nvar_];
}


sbset::~sbset(void)
{
	delete[] actvar_;
}

sbset *csbset(vind n,vector<vind>& v,real c,real ind)
{
//	assert(sbsetind < maxsbst);
	sbset    *s = sbsarr[sbsetind++];
	s->nvar_ = n; 
	for (vind i=0;i<n;i++) s->actvar_[i] = v[i];
	s->crt_ = c;
	s->ind_ = ind;
	return s;
}

void dsbset(sbset *s)
{
//	assert(sbsetind > 0);
	(sbsarr[s->pos]=sbsarr[--sbsetind])->pos = s->pos;
	(sbsarr[sbsetind]=s)->pos = sbsetind;
	return;
}

}

