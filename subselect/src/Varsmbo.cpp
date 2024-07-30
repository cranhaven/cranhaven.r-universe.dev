// #include <cassert>
#include <cstdlib>
#include <vector>
#include <limits>
#include "Sscma.h"
#include "Vsmabo.h"

namespace extendedleaps {

const double INF = std::numeric_limits<double>::infinity(); 
vind *dmyv;  
vector<vind> Flp;  
vector<double> Fl;	

extern std::vector<partialdata *> pdata;
extern vind flsts,flsti;
extern bool numericalprob;

int  cmp(const void *,const void *);
int  revcmp(const void *,const void *);

SRCwrkspace::SRCwrkspace(bool pivotall,vind nv,vind maxdim,subsetdata* data0,vector<vind>& ivlst,vector<vind>& ovlst)
{
	pvall_ = pivotall;
	if (pivotall) flsts = maxdim-lp;
	else flsts = maxdim-lp-1;
	initwrkspace(pivotall,nv,data0,flsts,fp,lp,ivlst,ovlst);
	flsts -= fp;
}

bool SRCwrkspace::pivot(vind vp,vind t,vind li,vind lo,double acpbound)
{
	subset *isi=0,*iso=0;

	pivotinit(isi,iso,vp,li,lo);
	return isi->pivot(forward,vp,t,iso,(lo==0),acpbound);
}

INVwrkspace::INVwrkspace(bool pivotall,vind nv,vind mindim,subsetdata* data0,vector<vind>& ivlst,vector<vind>& ovlst)
{
	pvall_ = pivotall;
	if (pivotall) flsti = nv-mindim-fp+1;
	else flsti = nv-mindim-fp;
	initwrkspace(pivotall,nv,data0,flsti,lp,fp,ovlst,ivlst);
	flsti -= lp;
}

bool INVwrkspace::pivot(vind vp,vind t,vind li,vind lo,double acpbound)
{
	subset *isi=0,*iso=0;

	pivotinit(isi,iso,vp,li,lo);
	return isi->pivot(backward,vp,t,iso,(lo==0),acpbound);
}

void wrkspace::initwrkspace(bool pivotall,vind nv,subsetdata* data0,vind lstind,vind nvattop,vind nvatbot,
				vector<vind>& vattop,vector<vind>& vatbot)
{
	vind lastv;
	vector<vind> tlst; 
	subsetdata* newdata=0;
	double trivbnd(-INF);

	p = nv;
	maxim = data0->max();
	if ( !max() ) trivbnd *= -1;
	wrklst.assign(lstind+1,static_cast<subset *>(0));
	if (pivotall)  {
		lastv = nv;
		nwl = p-fp-lp+1;
	}
	else  {
		lastv = nv-1;
		nwl = p-fp-lp;
	}
	if (fp+lp > 0)  {
		tlst.resize(p);
		frontlsts(vatbot,vattop,nvatbot,nvattop,tlst);
		(wrklst[lstind] = new subset(tlst,p,p,data0,false,p))->reorder(tlst);
	}
	else wrklst[lstind] = new subset(p,p,data0,false,p);

	for (vind j=1;j<=nvattop;j++)  {
		newdata = data0->crcopy(p,p-nvatbot-j);
		try {
			if (fp+lp==0) wrklst[lstind-j] = new subset(p,p-nvatbot-j,newdata,true,p);
			else wrklst[lstind-j] = new subset(tlst,p,p-nvatbot-j,newdata,true,p);
		}
		catch (...)   {
			delete newdata;
			throw;
		}
		if (lstind > j) pivot(nvatbot+j,p-nvatbot-j,lstind+1-j,lstind-j,trivbnd);
		else pivot(nvatbot+j,0,lstind+1-j,0,trivbnd);
		delete wrklst[lstind+1-j];
	}

	for (vind j1=nwl-2;j1>=0;j1--) {
		newdata = data0->crcopy(lastv,j1); 
		try {
			if (fp+lp==0) wrklst[j1] = new subset(lastv,j1,newdata,true,p);
			else wrklst[j1] = new subset(tlst,lastv,j1,newdata,true,p);
		}
		catch (...)   {
			delete newdata;
			throw; 		

		}
	}
}

void wrkspace::frontlsts(vector<vind>& l1,vector<vind>& l2,vind nl1,vind nl2,vector<vind>& ol)
{
	vind tmp;
	vector<vind> elep(p);
	
	{ for (vind i=0;i<p;i++)  ol[i] = elep[i] = i+1; }
	{ for (vind i=0;i<nl1;i++) {
		tmp = ol[i];
		ol[i] = l1[i];
		ol[elep[l1[i]-1]-1] = tmp;
		elep[tmp-1] = elep[l1[i]-1];
		elep[l1[i]-1] = i+1;
	} }
	{ for (vind i=0;i<nl2;i++) {
		tmp = ol[nl1+i];
		ol[nl1+i] = l2[i];
		ol[elep[l2[i]-1]-1] = tmp;
		elep[tmp-1] = elep[l2[i]-1];
		elep[l2[i]-1] = nl1+i+1;
	} }
}

wrkspace::~wrkspace(void)
{
	for (vind j=0;j<nwl;j++)  delete wrklst[j];
}

void wrkspace::pivotinit(subset*& isi,subset*& iso,vind vp,vind li,vind lo)
{
	vind isonvar;
	isi = wrklst[li];
	iso = wrklst[lo];

	isi->copyvar(*iso);
	iso->setnvar(isonvar=isi->getnvar()+1);
	iso->setvar(isonvar,vp);
}

subset::subset(vind nvp,vind pnv,subsetdata *id,bool pdt,vind tnv)
   :  p(tnv), t(pnv), k(0), var(0), frstvarpm(nvp-pnv),  pmemorypos(0), memii(0), data(id), privatedata(pdt)
{
	assgnmem();
	for (vind i=0;i<p;i++)  
		fmemorypos[i] = orgvarind[i] =  orgvarpos[i] = i;
 	if (id) id->setorgvarl(&orgvarind[0]);                    // Attach list of original variable indicators to subsetdata
}

subset::subset(const vector<vind>& ivar,vind nvp,vind pnv,subsetdata *id,bool pdt,vind tnv)
  :  p(tnv), t(pnv), k(0), frstvarpm(nvp-pnv), memii(0), data(id), privatedata(pdt)
{
	assgnmem();
	for (vind i=0;i<p;i++)  {
		orgvarind[i] = ivar[i]-1;
		orgvarpos[orgvarind[i]] = i;
		fmemorypos[i] = i;
	}
	if (id) id->setorgvarl(&orgvarind[0]);          // Attach list of original variable indicators to subsetdata
}

void subset::assgnmem()
{
	pmemorypos.resize(0);
	if (frstvarpm) var.resize(frstvarpm);
	orgvarind.resize(p);
	orgvarpos.resize(p);
	fmemorypos.resize(p);
	memii = new mindices(p,p-frstvarpm,frstvarpm,fmemorypos);
}

subset::~subset()
{
	if (privatedata) delete data;
	delete memii;
}

void subset::copyvar(subset & newsp)
{
	for (vind i=0;i<k;i++)  newsp.var[i] = var[i];  
}

void subset::sort(direction dir,vind fv,vind lv,bool reverse,bool smallestatend)
{
	bool reliable(true);
	double trivbnd(-INF);
	vind ovi;

	if (!data->max())  trivbnd *= -1;
	if (nxtres.empty()) nxtres.resize(p);
	for (vind i=0,oovi=fv-1-p+t;i<=lv-fv;i++,oovi++)  {
		Fl[i] = data->updatecrt(dir,*memii,fv+i,pdata[i+1],reliable,numtol,trivbnd);
		if (!reliable)	Fl[i] = trivbnd; 
 		Flp[orgvarind[fv+i-1]] = i+1;
		dmyv[i] = i+1;
		if (memii->direct()) ovi = (*(memii->idfm()))[oovi];  
		else ovi = (*(memii->iifm()))[oovi];  
		nxtres[ovi].criterion = Fl[i];
		nxtres[ovi].pres = pdata[i+1];
		nxtres[ovi].reliable = reliable;
	}
	if (reverse) qsort((void *)dmyv,lv-fv+1,sizeof(*dmyv),revcmp);
	else qsort((void *)dmyv,lv-fv+1,sizeof(*dmyv),cmp);
	if (smallestatend) {
		int tmp=dmyv[0];
		for (int a=0;a<lv-fv;a++) dmyv[a] = dmyv[a+1];
		dmyv[lv-fv] = tmp;
	}
	{ for (vind i=fv;i<=lv;i++) dmyv[i-fv] = orgvarind[dmyv[i-fv]+fv-2]; }
	{ for (vind i=fv;i<=lv;i++) orgvarind[i-1] = dmyv[i-fv]; }
}

void subset::reorder(vector<vind>& l)
{
	vind lag = p - t;
	bool nopmemorypos(false);

	if (pmemorypos.empty()) {	
		nopmemorypos = true;
		pmemorypos.resize(t);
	}
	for (vind i1=0;i1<p;i1++)  {
		fmemorypos[i1] = l[i1] - 1;
		if (i1 >= lag) pmemorypos[i1-lag] = l[i1]-lag-1;
	}
	if (nopmemorypos) memii->asgnpmmiid(new lagindex<i>(t,frstvarpm,pmemorypos));
}

void subset::asgvar(vind fvar,vind nv,const vector<vind>& list)
{
	vind lag = p - t;
	bool nopmemorypos(false);

	if (pmemorypos.empty()) {
		nopmemorypos = true;
		pmemorypos.resize(t);
	}
	for (vind i1=0;i1<nv;i1++)  {
		pmemorypos[fvar+i1] = list[i1]-1;
		fmemorypos[lag+fvar+i1] = lag+list[i1]-1;
	}
	if (nopmemorypos) memii->asgnpmmiid(new lagindex<i>(t,frstvarpm,pmemorypos));
}

bool subset::pivot(direction dir,vind vp,vind t,subset *newsp,bool last,double acpbound)
{
	bool reliable=true;
	partialdata* pdt;
	vind vporgi;

	if ( nopivot() )  { 
		newsp->forbidpivot(); 	
		numericalprob = true;
		return false;
	}
	if (!nxtres.empty())  { 
		if (memii->direct()) vporgi = (*(memii->idpm()))[vp-1];  
		else vporgi = (*(memii->iipm()))[vp-1];  
		pdt = nxtres[vporgi].pres;
		reliable = nxtres[vporgi].reliable;
	}
	else data->updatecrt(dir,*memii,vp,pdt=pdata[0],reliable,numtol,acpbound);
	if (!reliable) {
		newsp->forbidpivot(); 	
		numericalprob = true;
		return false;
	}
	newsp->getdatap()->getpdata(pdt);
	if (!last) data->pivot(dir,*memii,vp,t,pdt,newsp->data,last,reliable,INF); 
	
	return true;
}

mindices::mindices(vind szfm,vind szpm,vind pmemlag)
   :  idfm_(0), idpm_(0), iifm_(0), iipm_(0) 
{
	try {
		idfm_ = new itindex<d>(szfm);
		idpm_ = new lagindex<d>(szpm,pmemlag);
	}
	catch (...)   {
		delete idfm_;
		delete idpm_;
		throw;
	}
}

mindices::mindices(vind szfm,vind szpm,vind pmemlag,vector<vind>& fmmlst)
   :  idfm_(0), idpm_(0), iifm_(0), iipm_(0) 
{
	try {
		idfm_ = new itindex<d>(szfm);
		idpm_ = new lagindex<d>(szpm,pmemlag);
		iifm_ = new itindex<i>(szfm,fmmlst);
	}
	catch (...)   {
		delete idfm_;
		delete idpm_;
		delete iifm_;
		throw;
	}
}

mindices::mindices(vind szfm,vind szpm,vind pmemlag,vector<vind>& fmmlst,vector<vind>& pmmlst)
   :  idfm_(0), idpm_(0), iifm_(0), iipm_(0) 
{
	try {
		idfm_ = new itindex<d>(szfm);
		idpm_ = new lagindex<d>(szpm,pmemlag);
		iifm_ = new itindex<i>(szfm,fmmlst);
		iipm_ = new lagindex<i>(szpm,pmemlag,pmmlst);
	}
	catch (...)   {
		delete idfm_;
		delete idpm_;
		delete iifm_;
		delete iipm_;
		throw;
	}
}

mindices::~mindices(void)
{
	delete idfm_;
	delete idpm_;
	delete iifm_;
	delete iipm_;
}

}
