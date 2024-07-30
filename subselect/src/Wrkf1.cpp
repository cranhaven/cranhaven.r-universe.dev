// #include <cassert>
#include <ctime>
#include "Sscma.h"
#include "Subsets.h"
#include "Vsmabo.h"

namespace extendedleaps {

extern vind *dmyv;   
extern vector<vind> Flp;  
extern vector<double> Fl;	  
extern vector<unsigned long>	sbsetcnt;                      

const int SRC  = 1;
const int INV  = 0;
const double   NOBND = 1E99;
clock_t ctime,newtime; 
double rtime;

sbset *csbset(vind n,vector<vind>& v,real c,real ind);
void pivot(wrkspace *w,vind tree,vind k0,vind k1,vind nv,vind u,vind t,vind minvi,vind maxvi,bool revord);	 
void getactv(wrkspace *,vind,vind,vind);
void actvcnv(vind p,vind p1,vector<vind>& v0,vector<vind>& v1);
bool leap(vind,real,const real *,vind,vind);
real getbounds(vind dir,vind minv,vind maxv);
int cmp(const void *a,const void *b);

#ifdef COUNTING
void showcnt(int,int,vind);
#endif

extern vind maxdim;    
extern vector<vind> prvks;    
extern short int pcrttp;                                         
extern pcskept pcsets;
// extern real   crub,*bndl;
extern vector<psbstlist> bsts;

int cmp(const void *a,const void *b)
{
	vind ai = *(vind *)a - 1;
	vind bi = *(vind *)b - 1;
	if (pcrttp == MAXIMZ)  {
		if (Fl[ai] > Fl[bi]) return 1;
		else if (Fl[ai] < Fl[bi]) return -1;
			else return 0;
	}
	else {
		if (Fl[ai] < Fl[bi]) return 1;
		else if (Fl[ai] > Fl[bi]) return -1;
			else return 0;
	}
}

int revcmp(const void *a,const void *b)	  
{
	return -cmp(a,b);
}

void actvcnv(vind p,vind p1,vector<vind>& v0,vector<vind>& v1)
{
	vind i,j,k;

	for (i=0,j=0,k=1;i<p1;i++,k++)  while (k<v0[i])  v1[j++] = k++;
	while (k<=p) v1[j++] = k++;
}

void getactv(wrkspace *w,vind t,vind k1,vind nv)
{
	subset *wlst = &w->subsetat(k1+1); 

	if (t == INV)  {
		if (wlst->getp() == p)  {  
			actvcnv(p,p-nv,wlst->getvar(),actv);
			{ for (vind i=0;i<nv;i++) actv[i] = wlst->getithvar(actv[i]-1)+1; }
		}
		else { 
			actvcnv(p-1,p-nv,wlst->getvar(),actv);
			{ for (vind i=0;i<nv-1;i++) actv[i] = wlst->getithvar(actv[i]-1)+1; }
			actv[nv-1] = lastvar;
		}
	}
	else
		{ for (vind i=0;i<nv;i++) actv[i] = wlst->getithvar(wlst->getvar(i+1)-1)+1; }
}

void pivot(wrkspace *w,vind tree,vind k0,vind k1,vind nv,vind u,vind t,vind minvi,vind maxvi,bool revord)   
{
	double crt,ind,acptbound(-NOBND);
	bool success;
	vind vi;
	sbset *prevsbset,*st;
	sbstlist::iterator ptprevsbset;
	subsetdata *curdata;

//	assert(k0 > k1);
	if (revord) vi = p-u+1;
	else vi = u;

	curdata = w->subsetat(k1+1).getdatap();
	if (curdata->spdcupd())  acptbound = getbounds(pcrttp,minvi,maxvi);
	else if (pcrttp == MINIMZ ) acptbound *= -1;

	if (k1 == 0)  success = w->pivot(vi,0,k0,0,acptbound);
	else success = w->pivot(vi,t,k0,k1,acptbound);
	if (success) curdata->allowpivot();		
	if (nv < mindim || nv > maxdim || !success) return;
	#ifdef COUNTING
	++cntg;
	#endif
	
	crt = curdata->criterion();
	ind = curdata->indice();

	if (pcrttp == MAXIMZ && crt < lbnd[nv-mindim]) return;	// Check if new subset is better than any of the
	if (pcrttp == MINIMZ && crt > ubnd[nv-mindim]) return;	// sets in current best set list for this dimension
	getactv(w,tree,k1,nv);
	st = csbset(nv,actv,crt,ind);	
	psbstlist curlist = bsts[nv-mindim];
	if (sbsetcnt[nv-mindim] == ms)  {	// Remove and discard worst subset saved
		prevsbset = *(ptprevsbset=curlist->begin());
		curlist->erase(ptprevsbset);
		dsbset(prevsbset);
	}
	else sbsetcnt[nv-mindim]++;
	curlist->insert(st);	// Insert new subset in best sets list
	if (sbsetcnt[nv-mindim] == ms)  { 
		if (pcrttp == MAXIMZ) lbnd[nv-mindim] = (*curlist->begin())->crt();
		else ubnd[nv-mindim] = (*curlist->begin())->crt();
	}
	return;
}

bool Leaps_Search(vind frwind0,vind bckind0,vind fvind,vind lvind,vind nvfrwd,vind nvbckwrd)  
{
	vind nv,minnv,maxnv,minnvfrd,minnvbkrd,maxnvfrd,maxnvbkrd;
	vind u,t,frwind(frwind0);
	real maxstcrt(NOBND);
	const real*	icrtcmpll;
// pointer to list with individual criterion components for sums of qadratic form criteria
// with as many parcels as variables in each subset
	subsetdata* prvdatapt;

	if (lvind-fvind > 10) {
		newtime = clock();
		if (newtime==clock_t(-1))  {
			msg("Eleaps error: time overflow\n");
			return false;
		}
		rtime -= static_cast<double>(newtime-ctime);
		if (rtime < 0.) return false;  // Exit if time limit was exceded
		ctime = newtime; 
	} 

//  Find maximal subset dimensionalities of current tree branches
	
	if ( (maxnvfrd=nvfrwd+bckind0) > maxdim) maxnvfrd = maxdim;
	maxnvbkrd = nvbckwrd-1;

//  Start pivoting variables 

	{ for (vind i=fvind;i<lvind;i++)  {

		u = lvind-i+1; // convert index of increasing order (i) into index of decreasing order (u)
		t = u-2; 

//		Set number of variables in the susbset where the current foward pivot will be performed
		nv = minnvfrd = nvfrwd+1+i-fvind;
		if (maxnvfrd >= mindim && minnvfrd <= maxdim)  {

//			Make a forward pivot	
			if (minnvfrd < mindim) pivot(SW,SRC,frwind,t,nv,u,t,mindim,maxnvfrd,true);
			else if (minnvfrd < maxdim) pivot(SW,SRC,frwind,t,nv,u,t,minnvfrd,maxnvfrd,true);
				 else pivot(SW,SRC,frwind,0,nv,u,t,minnvfrd,maxnvfrd,true);
		}

		if (t > 0) {	//	Keep track of source memory for current forward search results
			prvks[t-1] = frwind; 
			frwind = t;	// Update memory index for forward search
		}

//		Set number of variables in the susbset where the current backward pivot will be performed
		nv = nvbckwrd - 1;
		if ( (minnvbkrd=nvbckwrd-bckind0+i-fvind) < mindim) minnvbkrd = mindim;

		if (maxnvbkrd >= mindim && minnvbkrd <= maxdim)
		{	 
//			Make a backward pivot
			if (maxnvbkrd > maxdim) pivot(IW,INV,bckind0,t,nv,u,t,minnvbkrd,maxdim,true);
			else if (maxnvbkrd > mindim) pivot(IW,INV,bckind0,t,nv,u,t,minnvbkrd,maxnvbkrd,true);

				else pivot(IW,INV,bckind0,0,nv,u,t,minnvbkrd,maxnvbkrd,true);
		}
	} }

//  Process recursevly the subtrees created by the previous cycle

	{ for (vind i=1;i<lvind-fvind;i++)   {
		minnv = nvfrwd+lvind-fvind-i;
		maxnv = nvbckwrd-2;
		if (minnv <= maxdim && maxnv >= mindim) {
			if (minnv < mindim) minnv = mindim;
			if (maxnv > maxdim) maxnv = maxdim;
			if (minnv > maxnv) minnv = maxnv;
			prvdatapt = &IW->subsetat(i+1).getdata();
			if  ( !prvdatapt->nopivot() ) {
				maxstcrt = prvdatapt->criterion();	// Get criterion for maximal subset in the next subtree
				icrtcmpll = prvdatapt->getsqfparcels();
				if (!leap(pcrttp,maxstcrt,icrtcmpll,minnv,maxnv) ) // test if maxstcrt is better than current bounds
	 				if (!Leaps_Search(prvks[i-1],i,fvind,fvind+i,nvfrwd+bckind0-i-1,nvbckwrd-1)) return false;
			}
		}
	} }

	return true;
}

bool Rev_Leaps_Search(vind frwind0,vind bckind0,vind fvind,vind lvind,vind nvfrwd,vind nvbckwrd)
{
	vind prvbckind,bckind(bckind0);  
	vind nv,t,minnv,maxnv,minnvfrd,minnvbkrd,maxnvfrd,maxnvbkrd;  
	real maxstcrt(NOBND);
	const real*	icrtcmpll; 	
// pointer to list with individual criterion components for sums of quadratic form
// criteria with as many parcels as variables in each subset
	subsetdata* prvdatapt;

	if (lvind-fvind > 10) {
		newtime = clock();
		if (newtime==clock_t(-1))  {
			msg("Eleaps error: time overflow\n");
			return false;
		}
		rtime -= static_cast<double>(newtime-ctime);
		if (rtime < 0.) return false;  // Exit if time limit was exceded
		ctime = newtime; 
	} 

//  Find minimal subset dimensionalities of current tree branches
	
	if ( (minnvbkrd=nvbckwrd-frwind0) < mindim) minnvbkrd = mindim;
	minnvfrd = nvfrwd+1;

//  Start pivoting variables

	{ for (vind u=fvind;u<lvind;u++)  {

		t = lvind-u-1; 

//		Get number of variables in the susbset where the current forward pivot will be performed
		nv = nvfrwd + 1;
		if ( (maxnvfrd=nvfrwd+frwind0+fvind-i) > maxdim) maxnvfrd = maxdim;
		if (maxnvfrd >= mindim && minnvfrd <= maxdim)

//			Make a forward pivot 				
			if (minnvfrd < mindim) pivot(SW,SRC,frwind0,t,nv,u,t,mindim,lvind,false);
			else if (minnvfrd < maxdim) pivot(SW,SRC,frwind0,t,nv,u,t,minnvfrd,lvind,false);
				 else pivot(SW,SRC,frwind0,0,nv,u,t,minnvfrd,lvind,false);

//		Get number of variables in the susbset where the current backward pivot will be performed
		nv = maxnvbkrd = nvbckwrd-1+fvind-u;
		if (maxnvbkrd >= mindim && minnvbkrd <= maxdim) {

//			Make a backward pivot 				
			if (maxnvbkrd > maxdim) pivot(IW,INV,bckind,t,nv,u,t,minnvbkrd,maxdim,false);
			else if (maxnvbkrd > mindim) pivot(IW,INV,bckind,t,nv,u,t,minnvbkrd,maxnvbkrd,false);
				 else pivot(IW,INV,bckind,0,nv,u,t,minnvbkrd,maxnvbkrd,false);
		}

		if (t > 0) {	//	Keep track of source memory for current backward search results
				prvks[t-1] = bckind; 
				bckind = t;	// Update memory index for forward search
		}
	} }

//  Process recursevly the subtrees created by the previous cycle

	{ for (vind i=1;i<lvind-fvind;i++)   {
		minnv = nvfrwd+2;
		maxnv = nvfrwd+i+1;
		if (minnv <= maxdim && maxnv >= mindim) {
			if (minnv < mindim) minnv = mindim;
			if (maxnv > maxdim) maxnv = maxdim;
			if (minnv > maxnv) minnv = maxnv;
			prvdatapt = &IW->subsetat((prvbckind=prvks[i-1])+1).getdata();
			if  ( !prvdatapt->nopivot() ) {
				maxstcrt = prvdatapt->criterion();	// Get criterion for maximal subset in the next subtree
				icrtcmpll = prvdatapt->getsqfparcels();
				if (!leap(pcrttp,maxstcrt,icrtcmpll,minnv,maxnv) ) // test if maxstcrt is better than current bounds
					if (!Rev_Leaps_Search(i,prvks[i-1],lvind-i,lvind,nvfrwd+1,nvbckwrd-frwind0+i+1)) return false;
			}
		}
	} }

	return true;
}

// extern int cnt;

bool Forward_BreadthF_Search(vind frwind0,vind fvind,vind lvind,vind nvfrwd)
{
	vind nv,t,maxnv;  

	if (lvind-fvind > 10) {
		newtime = clock();
		if (newtime==clock_t(-1))  {
			msg("Eleaps error: time overflow\n");
			return false;
		}
		rtime -= static_cast<double>(newtime-ctime);
		if (rtime < 0.) return false;  // Exit if time limit was exceded
		ctime = newtime; 
	} 

//  Find maximal subset dimensionaly of current tree branch
	
	if ( (maxnv=nvfrwd+lvind-fvind+1) > maxdim) maxnv = maxdim;

//  Get number of variables in the susbset where the current pivot will be performed
	nv = nvfrwd + 1;

	if (maxnv < mindim || nv > maxdim)  return true; 

//  Start pivoting variables

	{ for (vind u=fvind;u<=lvind;u++)  {
		t = lvind-u; 
		if (nv < mindim) pivot(SW,SRC,frwind0,t,nv,u,t,mindim,lvind,false);
		else pivot(SW,SRC,frwind0,t,nv,u,t,nv,lvind,false);
	} }

//  Process recursevly the subtrees created by the previous cycle

	for (vind i=1;i<=lvind-fvind;i++)   if  ( !(SW->subsetat(i).getdata().nopivot()) )  
		if (!Forward_BreadthF_Search(i,lvind-i+1,lvind,nvfrwd+1)) return false;
	return true;
}

bool  Forward_DepthF_Search(vind frwind0,vind fvind,vind lvind,vind nvfrwd)  
{
	vind nv,minnv,maxnv,minnvfrd,maxnvfrd;
	vind t,frwind(frwind0);
	real maxstcrt(NOBND);
//	subsetdata* prvdatapt;

	if (lvind-fvind > 10) {
		newtime = clock();
		if (newtime==clock_t(-1))  {
			msg("Eleaps error: time overflow\n");
			return false;
		}
		rtime -= static_cast<double>(newtime-ctime);
		if (rtime < 0.) return false;  // Exit if time limit was exceded
		ctime = newtime; 
	} 

//  Find maximal subset dimensionalities of current tree branches
	
	if ( (maxnvfrd=nvfrwd+lvind-fvind+1) > maxdim) maxnvfrd = maxdim;

//  Start pivoting variables 

	{ for (vind u=fvind;u<=lvind;u++)  {

		t = lvind-u; 

//		Set number of variables in the susbset where the current pivot will be performed
		nv = minnvfrd = nvfrwd+u-fvind+1;

		if (maxnvfrd >= mindim && minnvfrd <= maxdim)  {

//			Make a pivot	
			if (minnvfrd < mindim) pivot(SW,SRC,frwind,t,nv,u,t,mindim,maxnvfrd,false);
			else if (minnvfrd < maxdim) pivot(SW,SRC,frwind,t,nv,u,t,minnvfrd,maxnvfrd,false);
				 else pivot(SW,SRC,frwind,0,nv,u,t,minnvfrd,maxnvfrd,false);
		}
		if (t > 0) {	//	Keep track of source memory for current forward search results
			prvks[t-1] = frwind; 
			frwind = t;	// Update memory index 
		}
	} }

//  Process recursevly the subtrees created by the previous cycle

	{ for (vind i=0;i<lvind-fvind;i++)   {
		minnv = nvfrwd+lvind-fvind-i;
		maxnv = nvfrwd+lvind-fvind;

		if (minnv <= maxdim && maxnv >= mindim) 
	 		if (!Forward_DepthF_Search(prvks[i],lvind-i,lvind,minnv-1)) return false;
	} }

	return true;
}


bool leap(vind dir,real crt,const real *crtcrr,vind minv,vind maxv)
{
	vind i;
	bool l;

	for (l=true,i=maxv;l&&i>=minv;i--) {
		if (crtcrr && i < maxv) crt -= crtcrr[i];	
		/* Remove ith+1 parcel when using a quadratic form criterion with a variable number of parcels (ex: GCD) */
		if (dir == MAXIMZ && crt > lbnd[i-mindim]) l = false; 
		else if (dir == MINIMZ && crt < ubnd[i-mindim]) l = false;
	}
	return l;
}

real getbounds(vind dir,vind minv,vind maxv)
{
	real bound;

	if (dir == MAXIMZ) bound = lbnd[minv-mindim];
	else bound = ubnd[minv-mindim];
	for (vind i=minv-mindim+1;i<=maxv-mindim;i++) {
		if (dir == MAXIMZ && lbnd[i] < bound) bound = lbnd[i]; 
		else if (dir == MINIMZ && ubnd[i] > bound) bound = ubnd[i];
	}
	return bound;
}

}

