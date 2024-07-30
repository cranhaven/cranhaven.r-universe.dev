// #include <cassert>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <limits>
#include <vector> 
#include "Sscma.h"
#include "Subsets.h"
#include "Vsmabo.h"

using std::vector;

namespace extendedleaps {

int  cnt = 0;



vind		p,q,fp,lp,mindim,flst,lastvar;  
real		trs,trs2;
double 		maxtime; 
int		pcrt,sbsetind,maxsbst; 
unsigned long	ms;                      
pcskept		pcsets;
vector<vind> 	actv ;   
vector<double>	lbnd,ubnd;
vector<unsigned long>	sbsetcnt;                      
std::string 	memmsg("\nEleaps error: Unable to find enough memory to run eleaps with so many original variables.\n\n");
#ifdef COUNTING 
int cntg,fpcnt,fpcnt1;	/*  Floating point operations counters    */
#endif    

     /*  Comparison criteria */
 
const int  GCD      = 1;
const int  RV       = 2;
const int  MCB2     = 3;
const int  TAU      = 4;	
const int  XI       = 5;	
const int  ZETA     = 6;	
const int  CCR1     = 7;	
const int  NOTFOUND = 99;

           
subsetdata *idata,*fulldata;
globaldata *gidata,*gfulldata;
vector<partialdata *> pdata;
vind ndim,maxdim;
vector<vind> prvks,cmpl,ivlst,ovlst;      
vind flsts,flsti;
short int	pcrttp;                                         
SRCwrkspace*   SW;   
INVwrkspace*   IW;   
double  c0,v0; 
vector<double> 	vc0;                  				                   
int maxsbqe,maxcmp;
extern vind* dmyv;   
extern vector<vind> Flp;  
extern vector<double> Fl;	
extern bool numericalprob;	

#ifdef COUNTING
int  cntp,fpcnt0;   
#endif

vector<psbst>	sbsarr;                  				                   
vector<psbstlist> bsts;                  				                   

void resetvar(void);
int getpcrt(const char* st,bool fixed);
void initvlist(int *,int *,int *,int,int,int);
void fillres(vind fk,vind nk,int ns,int* bst,int* st,double* bvl,double* vl);	
void saveset(psbstlist,int *,double *l,int,vind);						
extern "C" int trivialcmp(const void *,const void *);
void matasvcttranspose(int m,int n,int* data);
bool asgmemory(void);
void cleanup(void);
void isort(bool reverse);	
void fsort(bool reverse);	

void resetvar()
{
	SW = 0;
	IW = 0;
	dmyv = 0;
	idata = fulldata = 0;
	gidata = gfulldata = 0;
	c0 = v0 = 0.;                   
	sbsetind = 0;
	numericalprob = false;	
	sbsarr.resize(0);
	bsts.resize(0);
	actv.resize(0);
	prvks.resize(0);
	actv.resize(0);
	cmpl.resize(0);
	Flp.resize(0);
	ivlst.resize(0);
	ovlst.resize(0);
	pdata.resize(0);
	lbnd.resize(0);
	ubnd.resize(0);
	vc0.resize(0);
	Fl.resize(0);
	#ifdef COUNTING
	cntp = 0;   
	#endif
}

int getpcrt(const char* st,bool fixed)
{
	if ( !strncmp(st,"TAU_2",5)  )		return TAU;
	else if ( !strncmp(st,"XI_2",4)  )	return XI;
	else if ( !strncmp(st,"ZETA_2",6) )	return ZETA;
	else if ( !strncmp(st,"CCR1_2",6) )	return CCR1;

	else if ( !strncmp(st,"RM",2) )		return MCB2;    
	else if ( !strncmp(st,"RV",2) )		return RV;
	else if ( !strncmp(st,"GCD",3) ) 
	{
		if (fixed) pcsets = given;
		else pcsets = firstk;
		return GCD;  
	}
	else return NOTFOUND;
}

void initvlist(int *ilist,int *olist,int *pcind,int ni,int no,int nind)
{
	if ( ni > 0) ivlst.resize(ni);
	if ( no > 0) ovlst.resize(no);
	if (pcrt != GCD) q = 0;
	else {
		if (pcsets == firstk) q = maxcmp = maxdim;
		else if ( (q=nind) == 0) errmsg("Criterion GCD requires a non-empty list of S eigenvectors\n");
		cmpl.resize(q);
	}

	fp = ni;
	{ for (int i=0;i<ni;i++) ivlst[i] = ilist[i]; }
	lp = no;
	{ for (int i=0;i<no;i++) ovlst[i] = olist[i]; }
	if (q > 0)  {
		{ for (int i=1;i<=(int)q;i++)  {
			if (pcsets == firstk) cmpl[i-1] = i;
			else {
				if (i == 1) maxcmp = cmpl[0] = pcind[0];
				else if ( (cmpl[i-1] = pcind[i-1]) > maxcmp )   maxcmp = cmpl[i-1];
			}
		} }
	}
}

sscmares sscma(bool fullwrksp,bool heuristic,subsetdata *nullsetdt,subsetdata *fullsetdt)
{
	bool searchcompleted;

	try {
		SW = new SRCwrkspace(fullwrksp,p,p,nullsetdt,ivlst,ovlst);
		IW = new INVwrkspace(fullwrksp,p,1,fullsetdt,ivlst,ovlst);
	}
	catch (...)   {
		msg(memmsg);
		return nomemory;
	}

	flst = flsts;
	#ifdef COUNTING  
	fpcnt1 = 0;
	#endif
	if (p > fp+lp+1) fsort(heuristic);
	else lastvar = IW->subsetat(flsti+1).getithvar(p-1)+1;
//	assert(lastvar > 0 && lastvar <= p);
	if (fp > 0 && fp == mindim) savfrst();
	if (maxdim == p-lp) savfull();
	#ifdef COUNTING  
	fpcnt = 0;
	#endif
	if (p > fp+lp+1) {
		if (!heuristic) searchcompleted = Leaps_Search(flst,flst,fp+lp+1,p,fp,p-lp);
		else searchcompleted = Rev_Leaps_Search(flst,flst,fp+lp+1,p,fp,p-lp);
		if (searchcompleted) return optimal;
		else return limsrchbest;
	}
	else return optimal;
}

sscmares sscma(subsetdata *nullsetdt)	// Version of sscma to be employed when only a forward search is performed    
{
	bool searchcompleted,psearch(false);

	try {
		SW = new SRCwrkspace(true,p,p,nullsetdt,ivlst,ovlst);
	}
	catch (...)   {
		msg(memmsg);
		return nomemory;
	}
	flst = flsts;
	#ifdef COUNTING  
	fpcnt1 = 0;
	#endif

	if (mindim*log(static_cast<double>(p/mindim)) > 40)  psearch = true; 
// Things to improve: Make this condition dependent on time limit (study best tradeoff!!)

	if (p > fp+lp+1) isort(psearch);
	if ( fp > 0 && fp == mindim && !SW->subsetat(flst+1).nopivot() ) savfrst();

	#ifdef COUNTING  
	fpcnt = 0;
	#endif
	if (p > fp+lp) {
		if (psearch) searchcompleted = Forward_DepthF_Search(flst,fp+lp+1,p,fp); 
	 	else searchcompleted = Forward_BreadthF_Search(flst,fp+lp+1,p,fp);
		if (searchcompleted) return optimal;
		else return limsrchbest;
	}
	else return optimal;
}

void isort(bool reverse)   
{
	vind var;                  
	vector<vind> sind(p-fp-lp); 
	vind sskipvar=0;
	subset *sl,*slt;

	sl = &SW->subsetat(flsts+1);
	sl->sort(forward,fp+lp+1,p,reverse,false);     	

	for (vind i=1;i<=flsts;i++)  {
		slt = &SW->subsetat(i);
		for (vind j=fp+lp;j<p;j++)  slt->setithvar(j,sl->getithvar(j));
	}

	for (vind i=0;i<p-fp-lp;i++)  {
		var = sl->getithvar(fp+lp+i);
		if (fp == 0 && lp > 0) {
			sskipvar = lp;
			sind[i] = var+1;
		}
		else sind[i] = sl->getvarp(var)-fp-lp+1;
	}
	sl->asgvar(sskipvar,p-fp-lp,sind);

	{  for (int i=1;i<=flsts+1;i++)  {
		slt = &SW->subsetat(i);
		for (int j=fp+lp;j<slt->getp();j++)  slt->setvarp(slt->getithvar(j),j);
	} }
}

void fsort(bool reverse)   
{
	vector<vind> iind(p-fp-lp),sind(p-fp-lp); 
	vind sskipvar=0,iskipvar=0;
	subset *il,*sl,*ilt,*slt;

	il = &IW->subsetat(flsti+1);
	sl = &SW->subsetat(flsts+1);

	il->sort(backward,fp+lp+1,p,reverse,!reverse);     	
	lastvar = il->getithvar(p-1)+1;

	{ if (SW != NULL) for (vind i=1;i<=flsts+1;i++)  {
		slt = &SW->subsetat(i);
		for (vind j=fp+lp;j<p;j++) slt->setithvar(j,il->getithvar(j));
	} }
	{ for (vind i=1;i<=flsti;i++)  {
		ilt = &IW->subsetat(i);
		for (vind j=fp+lp;j<p;j++)  ilt->setithvar(j,il->getithvar(j));
	} }

	for (vind i=0;i<p-fp-lp;i++)  {
		vind var = il->getithvar(fp+lp+i);
		if (lp == 0 && fp > 0) {
			iskipvar = fp;
			iind[i] = var+1;
		}
		else iind[i] = il->getvarp(var)-fp-lp+1;
		if (fp == 0 && lp > 0) {
			sskipvar = lp;
			sind[i] = var+1;
		}
		else sind[i] = sl->getvarp(var)-fp-lp+1;
	}

	il->asgvar(iskipvar,p-fp-lp,iind);
	sl->asgvar(sskipvar,p-fp-lp,sind);

	if (SW != NULL) 
		{	for (int i=1;i<=flsts+1;i++)  {
				slt = &SW->subsetat(i);
				for (int j=fp+lp;j<slt->getp();j++)  slt->setvarp(slt->getithvar(j),j);
			}
	}
	{ for (vind i=1;i<=flsti+1;i++)  {
		ilt = &IW->subsetat(i);
		for (vind j=fp+lp;j<ilt->getp();j++)  ilt->setvarp(ilt->getithvar(j),j);
	} }
}

void fillres(vind fk,vind nk,int ns,int *bst,int* st,double* bvl,double* vl)	
{
	vind i,lk=fk+nk-1;
	int *stmatp;

	for (i=0;i<nk;i++)  {
		stmatp = &(st[i*ns*lk]);
		saveset(bsts[i],stmatp,&(vl[i*ns]),ns,lk);
		matasvcttranspose(ns,lk,stmatp);
		saveset(bsts[i],&(bst[i*lk]),&(bvl[i]),1,lk);
	}
	matasvcttranspose(nk,lk,bst);
	return;
}

void saveset(psbstlist pset,int *bvar,double *bcrtval,int nel,vind dim)	
{
	int i=0,j,*var;

	for (sbstlist::reverse_iterator qep=pset->rbegin();i<nel&&qep!=pset->rend();++qep)  {
		var = &(bvar[(i++)*dim]);
		for (j=0;j<(*qep)->nvar();j++) var[j] = (*qep)->actvar()[j];  
		qsort(static_cast<void *>(var),(*qep)->nvar(),sizeof(*var),trivialcmp);
		for (j=(*qep)->nvar();j<dim;j++) var[j] = 0; 
		*bcrtval++ = (*qep)->indice();

	}
	for (i=pset->size();i<nel;i++)  {
		for (vind j=0;j<dim;j++) bvar[i*dim+j] = 0;
		*bcrtval++ = 0.;
	} 
}

int trivialcmp(const void *a,const void *b)
{
	int ai,bi;

	ai = *(int*)a;
	bi = *(int*)b;
	if (ai > bi)  return  1;
	else if (ai < bi)  return -1;
		 else return 0;
}

void matasvcttranspose(int m,int n,int* data)
{
	int mn=m*n;
	vector<int> tmp(mn); 

	{ for (int i=0;i<m;i++)
		for (int j=0;j<n;j++)  tmp[i+j*m] = data[i*n+j];
	}
	{ for (int i=0;i<mn;i++) data[i] = tmp[i]; }
}

bool asgmemory()
{
 	try  {
		actv.resize(p);	
		Fl.resize(p);
		Flp.resize(p);
		dmyv = new vind[p];
		if (ms > 0) {
			bsts.resize(ndim);
			sbsetcnt.resize(ndim);
			{for (vind i=0;i<ndim;i++) sbsetcnt[i] = 0; }
		}
		if (ndim == p-fp-lp+1) maxsbst = maxsbqe = ms*(ndim-1)+2;
		else maxsbst = maxsbqe = ms*ndim+3;
		sbsarr.resize(maxsbst);
		{ for (int i=0;i<maxsbst;i++) sbsarr[i]=new sbset(i,p);  }

		if ( pcrt == MCB2 || pcrt == TAU) {
			pcrttp = MINIMZ;
			if (ms > 0) 
				{ for (vind i=0;i<ndim;i++) bsts[i] = new sbstlist(sbstsort::descending);  }
				if (ndim == p-fp-lp+1) { 
					ubnd.resize(ndim-1);
					{ for (vind i=0;i<ndim-1;i++) 
						ubnd[i] = std::numeric_limits<float>::infinity();  } 	   
				}
				else {
					ubnd.resize(ndim);
					{ for (vind i=0;i<ndim;i++) 
						ubnd[i] = std::numeric_limits<float>::infinity();  } 	
				}
		}
		else  {
			pcrttp = MAXIMZ;
			if (ms > 0) {
				{  for (vind i=0;i<ndim;i++) bsts[i] = new sbstlist(sbstsort::ascending);  }
				if (ndim == p-fp-lp+1) { 
					lbnd.resize(ndim-1);
					{ for (vind i=0;i<ndim-1;i++) lbnd[i] = 0.;  }
				}
				else {
					lbnd.resize(ndim);
					{ for (vind i=0;i<ndim;i++) lbnd[i] = 0.;  }
				}
			}
		}
		if ( pcrt == GCD && pcsets == firstk)  vc0.resize(q);  
		prvks.resize(p-1);
		vector<double> tmp1,tmp2;
	}
	catch (...)   {
		cleanup();
		msg(memmsg);
		return false;
	}
	return true;
}

void cleanup(void)
{
	delete SW;
	delete IW;
	delete idata;
	delete fulldata;
	delete gidata;
	delete gfulldata;
	delete[] dmyv;
	{ for (unsigned j=0;j<bsts.size();j++)  delete bsts[j]; }
	{ for (unsigned j=0;j<sbsarr.size();j++)  delete sbsarr[j]; }
	{ for (unsigned j=0;j<pdata.size();j++)  delete pdata[j]; } 
}

}


