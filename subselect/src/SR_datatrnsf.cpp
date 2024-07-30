#include <ctime>
#include <cmath>
#include <deque>
#include <string>
#include "SRI_sscma.h"
#include "Sscma.h"
#include "Subsets.h"
#include "Vsmabo.h"
#include "Qforms.h"
#include "VSQforms.h"
#include "GCDcrt.h"
#include "RMcrt.h"
#include "RVcrt.h"
#include "MStcrt.h"
#include "CCRcrt.h"
#include "Rnk3CCRcrt.h"
 
namespace extendedleaps {

extern vector<double> Fl;	
extern clock_t ctime; 

extern const int  GCD      = 1;
extern const int  RV       = 2;
extern const int  MCB2     = 3;
extern const int  TAU      = 4;	
extern const int  XI       = 5;	
extern const int  ZETA     = 6;	
extern const int  CCR1     = 7;	
extern const int  NOTFOUND = 99;

enum trnsfres {sucess,nomem};

extern int pcrt;    

bool fwrkspace = false;
bool numericalprob = false;	
double timelimit,numtol;	
bool stddata=false,onlyf=false;	
using std::deque;

extern subsetdata *idata,*fulldata;
extern globaldata *gidata,*gfulldata;
extern vector<partialdata *> pdata;
extern int *cmpl,*ivlst,*ovlst;    
extern vector<vind> prvks;    
extern vind ndim,maxdim;      
extern double  c0,v0;                    
extern vector<double> vc0;                    

sscmares sscma(bool fullwrksp,bool heuristic,subsetdata *nullsetdt,subsetdata *fullsetdt);
sscmares sscma(subsetdata *nullsetdt);		  									
trnsfres trnsfdwst(double *S,double *Sinv,double *E,double *Einv,double wstval,int hrank,const bool onlyforward);		  	
trnsfres trnsfdtrst(double *M,double *Minv,double *Hegvct,double *HegvctMinv,double trval,int hrank,const bool onlyforward);	
trnsfres trnsfdccr(double *S,double *Sinv,double *E,double *Einv,
			double *Hegvct,double *HegvctTinv,double *HegvctEinv,
			double ccr12,double wstval,double bartpival,double lawhotval,int hrank,const bool onlyforward);		
trnsfres trnsfdgcd(double *S,double *Sinv,double *Segval,double *Segvct,int npcs,const bool onlyforward);		  	
trnsfres trnsfdrm(double *S,double *Sinv,const bool onlyforward);		  							
trnsfres trnsfdrv(double *S,double *Sinv,double *Ssqr,const bool onlyforward);		  					

void resetvar(void);
int getpcrt(const char* st,bool fixed);
void initvlist(int *,int *,int *,int,int,int);
void fillres(vind fk,vind nk,int ns,int* bst,int* st,double* bvl,double* vl);	
bool asgmemory(void);								
void cleanup(void);

int callsscma(double* S,double* S2,double* Si,double* Segval,double* Segvct,		
	  double* E,double* Ei,double* Hegvct,double* HegvctTinv,double* HegvctEinv,	
	  double wilksval,double bartpival,double lawhotval,double ccr12val,int r,	
	  int kmin,int kmax,int nsol,int* out,int* in,int nout,int nin,			
	  const char* cmpcr,int fixed,int* pcind,int nind,int nvar,double timelimit,			
	  double ntol,bool onlyforward,int* subs,double* subsv,double* bestsv,int* bests,
	  bool printmsg=true)		
{
 	bool heuristic;
	sscmares srchres;
	trnsfres tres;

 	resetvar();
	ctime = clock();
	maxtime = rtime = timelimit*CLOCKS_PER_SEC;
	onlyf = onlyforward;				
	numtol = ntol;					
	p = nvar;
	ms = nsol;
	if (kmin > nin) mindim = kmin;
	else mindim = nin;
	if (kmax < nvar-nout) maxdim = kmax;
	else maxdim = nvar - nout;
	ndim = maxdim-mindim+1;
	if ( (pcrt = getpcrt(cmpcr,static_cast<bool>(fixed))) == NOTFOUND )  {  
		std::string st1("The Comparison criterion suplied, "),st2(cmpcr),st3(", is not supported\n");
		errmsg(st1+st2+st3);
	}
	initvlist(in,out,pcind,nin,nout,nind);
	if (!asgmemory()) return 4;
        switch (pcrt)  {
		case TAU:    
			tres = trnsfdwst(S,Si,E,Ei,wilksval,r,onlyf);									
			break;
		case XI: 
			tres = trnsfdtrst(S,Si,Hegvct,HegvctTinv,bartpival,r,onlyf);							
			break;
		case ZETA: 
			tres = trnsfdtrst(E,Ei,Hegvct,HegvctEinv,lawhotval,r,onlyf);							
			break;
		case CCR1:    
			tres = trnsfdccr(S,Si,E,Ei,Hegvct,HegvctTinv,HegvctEinv,ccr12val,wilksval,bartpival,lawhotval,r,onlyf);	
			break;
		case GCD: 
			tres = trnsfdgcd(S,Si,Segval,Segvct,q,onlyf);									
			break;
		case MCB2: 
			tres = trnsfdrm(S,Si,onlyf);											
			break;
		case RV: 
			tres = trnsfdrv(S,Si,S2,onlyf);										
			break;
	}
	if (tres==nomem)  {
		cleanup();
		msg(memmsg);
		return 4;
	}
	if (fulldata) {													
		if (log(timelimit) < -100+5*p) heuristic = true;
		else heuristic = false;
		srchres = sscma(fwrkspace,heuristic,idata,fulldata);								
	}														
	else srchres = sscma(idata);
	if (srchres== nomemory)  {
		cleanup();
		return 4;
	}
	fillres(mindim,ndim,nsol,bests,subs,bestsv,subsv);							
	if (srchres==limsrchbest &&printmsg) {												
		char timelascstr[10];											
		snprintf(timelascstr,10,"%f",timelimit);									
		std::string st1("\nWarning: An exact search could not be completed in "),st2(timelascstr),st3(" seconds\n");	
		msg(st1+st2+st3);	
	}
	if (numericalprob && printmsg) msg("\nWarning: Because of numerical problems some subsets were excluded from the analysis\n");
	cleanup();

	if (srchres==limsrchbest && !numericalprob) return 1;
	if (srchres==optimal && numericalprob) return 2;
	if (srchres==limsrchbest && numericalprob) return 3;
	if (srchres==optimal && !numericalprob) return 0;

	return 0;  // Too avoid warnings (never reached!!!)
}


trnsfres trnsfdwst(double *S,double *Sinv,double *E,double *Einv,double wstval,int hrank,const bool onlyforward)	
{
	wilksdata  *idataaswilks=0,*fulldataaswilks=0;
			
	try  {
		pdata.resize(p+1);
		{ for (int j=0;j<=p;j++) pdata[j] = 0; }
		for (int j=0;j<=p;j++) pdata[j] = new partialwilksdata(p,0.);
		idataaswilks = static_cast<wilksdata *>(idata = new wilksdata(0,p,p,hrank,1.));
		if (!onlyforward)											
			fulldataaswilks = static_cast<wilksdata *>(fulldata = new wilksdata(p,p,p,hrank,wstval)); 	
	}
	catch (...)   {
		return nomem;
	}
	{ for (int i=0;i<p;i++)   
		for (int j=0;j<=i;j++) {
			idataaswilks->setematcoef(i,j,E[j*p+i]); 
			idataaswilks->settmatcoef(i,j,S[j*p+i]); 
			if (!onlyforward)  {						
				fulldataaswilks->setematcoef(i,j,-Einv[j*p+i]);		
				fulldataaswilks->settmatcoef(i,j,-Sinv[j*p+i]);		
			}									
		}
	}
	return sucess;
}

trnsfres trnsfdtrst(double *M,double *Minv,double *Hegvct,double *HegvctMinv,double trval,int hrank,const bool onlyforward)	
{
	tracedata  *idataastrst=0,*fulldataastrst=0;
			
	try  {
		pdata.resize(p+1);
		{ for (int j=0;j<=p;j++) pdata[j] = 0; }
		for (int j=0;j<=p;j++) pdata[j] = new partialtracedata(p,hrank);
		if (pcrt == XI)  {
			idataastrst = static_cast<tracedata *>(idata = new bartpistdata(0,p,p,hrank,0.));
			if (!onlyforward)											
				fulldataastrst= static_cast<tracedata *>(fulldata = new bartpistdata(p,p,p,hrank,v0=trval));	
		}
		else if (pcrt == ZETA)  {
			idataastrst = static_cast<tracedata *>(idata = new lawlhotstdata(0,p,p,hrank,0.));
			if (!onlyforward)											
				fulldataastrst= static_cast<tracedata *>(fulldata = new lawlhotstdata(p,p,p,hrank,v0=trval));	
		}
	}
	catch (...)   {
		return nomem;
	}
	{ for (int i=0;i<p;i++)   
		for (int j=0;j<=i;j++) {
			idataastrst->getqfdata()->setcoefmatel(i,j,M[j*p+i]); 
			if (!onlyforward)									
				fulldataastrst->getqfdata()->setcoefmatel(i,j,-Minv[j*p+i]);			
		}
	}
	{ for (int i=0;i<hrank;i++)  {
		for (int j=0;j<p;j++) {
			idataastrst->getqfdata()->setvectel(i,j,Hegvct[i*p+j]);  
			if (!onlyforward)										
				fulldataastrst->getqfdata()->setvectel(i,j,-HegvctMinv[i*p+j]);			
		}
	} }
	return sucess;
}


trnsfres trnsfdccr(double *S,double *Sinv,double *E,double *Einv,
			   double *Hegvct,double *HegvctTinv,double *HegvctEinv,
			   double ccr12,double wstval,double bartpival,double lawhotval,int hrank,const bool onlyforward)
{
	singleqfdata  *idataassgqf=0,*fulldataassgqf=0;
	ccrdata  *idataasccr=0,*fulldataasccr=0;
	rnk3ccrdata *idataasrnk3ccr=0,*fulldataasrnk3ccr=0;
		
	try  {
		pdata.resize(p+1);
		{ for (int j=0;j<=p;j++) pdata[j] = 0; }
		if (hrank == 1)  {
			for (int j=0;j<=p;j++) pdata[j] = new partialsingleqfdata();
			idataassgqf = 
				static_cast<singleqfdata *>(idata = new singleqfdata(p,p,0.));
			if (!onlyforward) fulldataassgqf = 							
				static_cast<singleqfdata *>(fulldata = new singleqfdata(p,p,ccr12));
		}
		else if (hrank == 2)  {
			for (int j=0;j<=p;j++) pdata[j] = new partialccrdata(0,hrank);
			idataasccr = 
				static_cast<ccrdata *>(idata = new rnk2ccrdata(0,p,p,1.,0.,0.));
			if (!onlyforward) fulldataasccr = 								
				static_cast<ccrdata *>(fulldata = new rnk2ccrdata(p,p,p,wstval,bartpival,ccr12));
		}
		else if (hrank == 3)  {
			for (int j=0;j<=p;j++) pdata[j] = new partialrnk3ccrdata(0,hrank);
			idataasrnk3ccr = static_cast<rnk3ccrdata *>( idataasccr = 
					static_cast<ccrdata *>(idata = new rnk3ccrdata(0,p,p,1.,0.,0.,0.)) );
			if (!onlyforward) fulldataasrnk3ccr = static_cast<rnk3ccrdata *>( fulldataasccr = 			
				static_cast<ccrdata *>(fulldata = new rnk3ccrdata(p,p,p,wstval,bartpival,lawhotval,ccr12)) );
		}
	}
	catch (...)   {
		return nomem;
	}
	if (hrank == 1) {
		for (int i=0;i<p;i++)  { 
			for (int j=0;j<=i;j++) {
				idataassgqf->getqfdata()->setcoefmatel(i,j,S[j*p+i]); 
				if (!onlyforward) fulldataassgqf->getqfdata()->setcoefmatel(i,j,-Sinv[j*p+i]);		
			}
		}
		for (int j=0;j<p;j++) {
			idataassgqf->getqfdata()->setvectel(0,j,Hegvct[j]); 
			if (!onlyforward) fulldataassgqf->getqfdata()->setvectel(0,j,-HegvctTinv[j]);		
		}
	}
	else  {
		{ for (int i=0;i<p;i++)   
			for (int j=0;j<=i;j++) {
				idataasccr->settmatcoef(i,j,S[j*p+i]); 
				idataasccr->setematcoef(i,j,E[j*p+i]); 
				if (!onlyforward)  {									
					fulldataasccr->settmatcoef(i,j,-Sinv[j*p+i]); 					
					fulldataasccr->setematcoef(i,j,-Einv[j*p+i]);					
				}											
			}
		}
		{ for (int i=0;i<hrank;i++)  {
			for (int j=0;j<p;j++) {
				idataasccr->sethtinvel(i,j,Hegvct[i*p+j]); 
				if (!onlyforward) fulldataasccr->sethtinvel(i,j,-HegvctTinv[i*p+j]); 		
			}
		} }
		if (hrank == 3) { 
			for (int i=0;i<3;i++)  {
				for (int j=0;j<p;j++) {
					idataasrnk3ccr->setheinvel(i,j,Hegvct[i*p+j]); 
					if (!onlyforward) fulldataasrnk3ccr->setheinvel(i,j,-HegvctEinv[i*p+j]); 	
				}
			}
		}
	}
	return sucess;
}

trnsfres trnsfdgcd(double *S,double *Sinv,double *Segval,double *Segvct,int npcs,const bool onlyforward)			
{
	real srtegval;
	gcddata  *idataasgcd=0,*fulldataasgcd=0;
			
	try  {
		pdata.resize(p+1);
		for (int j=0;j<=p;j++) pdata[j] = 0;
		switch (pcsets)  {
			case (given): {
					for (int j=0;j<=p;j++) pdata[j] = new partialfgcddata(p,npcs);
					idataasgcd = static_cast<gcddata *>(idata = new fgcddata(0,p,p,npcs,0.));
					if (!onlyforward)										
						fulldataasgcd = static_cast<gcddata *>(fulldata = new fgcddata(p,p,p,npcs,v0=npcs));	 
				}
				break;
			case (firstk): {
					{ for (int j=0;j<=p;j++) pdata[j] = new partialvgcddata(p,p); }
					idataasgcd = static_cast<gcddata *>(idata = new vgcddata(0,p,p,0.,0.));
					if (!onlyforward)									  
						fulldataasgcd = static_cast<gcddata *>(fulldata = new vgcddata(p,p,p,1.,v0=p));	
					for (int j=0;j<npcs;j++) vc0[j] = 0.;  
				}
				break;
		}
	}
	catch (...)   {
		return nomem;
	}
	{ for (int i=0;i<p;i++)   
		for (int j=0;j<=i;j++) {
			idataasgcd->getqfdata()->setcoefmatel(i,j,S[j*p+i]); 
			if (!onlyforward) fulldataasgcd->getqfdata()->setcoefmatel(i,j,-Sinv[j*p+i]);		
		}
	}
	{ for (int i=0;i<npcs;i++)  {
		srtegval = std::sqrt(static_cast<real>(Segval[i]));
		for (int j=0;j<p;j++) {
			idataasgcd->getqfdata()->setvectel(i,j,srtegval*Segvct[i*p+j]); 
			if (!onlyforward) fulldataasgcd->getqfdata()->setvectel(i,j,-Segvct[i*p+j]/srtegval);		
		}
	} } 
	return sucess;
}

trnsfres trnsfdrm(double *S,double *Sinv,const bool onlyforward)	
{
	deque<bool>	avars(p,false); 
	rmdata *idataasrmdt=0,*fulldataasrmdt=0;

	real trs = S[0];
	{ for (int i=1;i<p;i++) trs += S[i*p+i]; }
	try  {
		pdata.resize(p+1);
		{ for (int j=0;j<=p;j++) pdata[j] = 0; }
		for (int j=0;j<=p;j++) pdata[j] = new partialrmdata(p);
		rmgdata *gdataasrmdt = static_cast<rmgdata *>(gidata = new rmgdata(p));
		idataasrmdt = static_cast<rmdata *>(idata = new rmdata(p,p,p,gdataasrmdt,avars,trs));
		gdataasrmdt->settrs(trs);
		avars.assign(p,true);
		if (!onlyforward) fulldataasrmdt = static_cast<rmdata *>(fulldata = new rmdata(p,p,p,gdataasrmdt,avars,c0=0.));	
	}
	catch (...)   {
		return nomem;
	}
	for (int i=0;i<p;i++)
		for (int j=0;j<=i;j++) {
			idataasrmdt->setcoefmatel(i,j,S[j*p+i]); 
			if (!onlyforward) fulldataasrmdt->setcoefmatel(i,j,-Sinv[j*p+i]);	
		}
	return sucess;
}

trnsfres trnsfdrv(double *S,double *Sinv,double *Ssqr,const bool onlyforward)			
{
	deque<bool>	avars(p,false); 
	rvdata *idataasrvdt=0,*fulldataasrvdt=0;

	real trs2 = Ssqr[0];
	for (int i=1;i<p;i++) trs2 += Ssqr[i*p+i];
	try  {
		pdata.resize(p+1);
		{ for (int j=0;j<=p;j++) pdata[j] = 0; }
		for (int j=0;j<=p;j++) pdata[j] = new partialrvdata(p);
		rvgdata *gdataasrvdt = static_cast<rvgdata *>(gidata = new rvgdata(p));
		{ for (int i=0;i<p;i++)
			for (int j=0;j<=i;j++) gdataasrvdt->sets2(i,j,Ssqr[j*p+i]);  }
		idataasrvdt =  static_cast<rvdata *>(idata = new rvdata(p,p,p,gdataasrvdt,avars,0,0.));
		avars.assign(p,true);
		if (!onlyforward) fulldataasrvdt = static_cast<rvdata *>(fulldata = new rvdata(p,p,p,gdataasrvdt,avars,0,c0=trs2));
		gdataasrvdt->settrs2(trs2);		
	}
	catch (...)   {
		return nomem;
	}
	{ for (int i=0;i<p;i++)
		for (int j=0;j<=i;j++) {
			idataasrvdt->setcoefmatel(i,j,S[j*p+i]); 
			idataasrvdt->sets2m1(i,j,0.); 
			idataasrvdt->sets2m1(j,i,0.); 
			if (!onlyforward)  {
				fulldataasrvdt->setcoefmatel(i,j,-Sinv[j*p+i]);	
				fulldataasrvdt->sets2m1(i,j,S[j*p+i]); 
				fulldataasrvdt->sets2m1(j,i,S[j*p+i]);	
			}
		}
	} 
	return sucess;
}

}

