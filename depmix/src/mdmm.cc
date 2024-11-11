
#include "mdmm.h"

//default empty construction
mdmm::mdmm() { 
	int *mod; mod = new int[1]; mod[0]=1;
	initialize(1,1,mod,0);
	delete [] mod;
}

mdmm::mdmm(const int nst, const int nit, int *mod, int xm) { 
	initialize(nst,nit,mod,xm);
}

mdmm::~mdmm(){
	delete [] modes;
	delete [] respPars;
}

void mdmm::initialize(const int nst, const int nit, int *mod, int xmiss) {
	nstates = nst;
	nitems = nit;
	modes = new int[nitems];
	for(int i=0; i<nitems; i++) modes[i]=mod[i];
	obsMatCols=0;
	for(int i=0; i<nitems; i++) obsMatCols += nppDensity(modes[i]);
	npars = nstates*nstates + nstates*obsMatCols + nstates;
	transPars.reset(nstates,nstates);
	initPars.reset(nstates,1);
	respPars = new matrix[nitems];
	for(int i=0; i<nitems; i++) {
		int nrpar=0;
		nrpar=nppDensity(modes[i]);
		respPars[i].reset(nstates,nrpar);
	}
	datxm.reset(1,nitems);
	resultStates.reset(nstates,1);
	xm=xmiss;
	trans=1;
	obser=2;
	inits=3;
}

void mdmm::reset(const int nst, const int nit, int *mod, int xm) {
	delete [] modes;
	delete [] respPars;
	initialize(nst,nit,mod,xm);
}

void mdmm::setPars(double *pars) {
	int parsUsed=0;
	for(int i=1; i<=nstates; i++) {
		for(int j=1; j<=nstates; j++) {
			parsUsed += 1;
			transPars(i,j) = pars[parsUsed-1];
		}
	}
	for(int i=1; i<=nstates; i++) {
		for(int j=0; j<nitems; j++) {
			int nrpar=0;
			nrpar=nppDensity(modes[j]);
			for(int k=1; k<=nrpar; k++) {
				parsUsed+=1;
				respPars[j](i,k)=pars[parsUsed-1];
			}
		}
	}	
	for(int i=1; i<=nstates; i++) {
		parsUsed += 1;
		initPars(i) = pars[parsUsed-1];
	}
}

void mdmm::rescale(void) {
	matrix trs(nstates,1);
	trs = transPars.rowsums();
	for(int i=1; i<=nstates; i++) {
		for(int j=1; j<=nstates; j++) {
			transPars(i,j) = transPars(i,j)/trs(i);
		}
	}
	double initsum=initPars.msum();
	for(int i=1; i<=nstates; i++) initPars(i) /= initsum;
	//obserpars
	for(int j=0; j<nitems; j++) {
		if(modes[j]>1) {
			matrix rs = respPars[j].rowsums();
			for(int st=1; st<=nstates; st++) {
				for(int k=1; k<=modes[j]; k++) {
					respPars[j](st,k) /= rs(st);
				}
			}
		}
	}
}

//acces functions
int mdmm::getMode(const int it) {
#ifdef MDMMBOUNDS
	if(it>nitems||it<1) error("[mdmm] it out of bounds in call to getMode(it)\n");
#endif
	return(modes[it-1]);
}

matrix mdmm::getDataProb(matrix dat) {// should this matrix be const or not???
#ifdef MDMMBOUNDS
	if(!(rows(dat)==1 && cols(dat)==nitems)) {
		dat.print();
		error("[mdmm] dat incompatible with model in getDataPob.\n");
	}
	for(int it=1; it<=nitems; it++) {
		if(modes[it-1]>1 && (dat(it)<1||dat(it)>modes[it-1]) && ((int) dat(it) !=xm))
			error("[mdmm] datapoint incompatible with item type in call to getDataProb.\n");
	}
#endif
	for(int st=1; st<=nstates; st++) {
		resultStates(st)=1.0;
		for(int it=1; it<=nitems; it++) {
			resultStates(st) *= getProb(dat(it),st,it);
		}
	}
	return(resultStates);
}

double mdmm::getDataProb(const int st, matrix dat) {// should this matrix be const or not???
#ifdef MDMMBOUNDS
	if(!(rows(dat)==1 && cols(dat)==nitems)) {
		dat.print();
		error("[mdmm] dat incompatible with model in getDataPob.\n");
	}
	for(int it=1; it<=nitems; it++) {
		if(modes[it-1]>1 && (dat(it)<1||dat(it)>modes[it-1]) && ((int) dat(it) !=xm))
			error("[mdmm] datapoint incompatible with item type in call to getDataProb.\n");
	}
#endif
	double result=1.0;
	for(int it=1; it<=nitems; it++) {
		result *= getProb(dat(it),st,it);
	}
	return(result);
}

double mdmm::getTrPar(const int st1, const int st2) {
#ifdef MDMMBOUNDS
	if(st1<1||st1>nstates||st2<1||st2>nstates) error("st1 or st2 index out of bounds in call to getTrPar.\n");
#endif
	return(transPars(st1,st2));
}

double mdmm::getInProb(const int st) {
#ifdef MDMMBOUNDS
	if(st<1||st>nstates) error("[mdmm] st out of bounds in call to getInProb.\n");
#endif
	return(initPars(st));
}

//derivatives
double mdmm::derin(const int np, const int st) {
#ifdef MDMMBOUNDS 
	if(np<1 || np>npars) error("[mdmm] np out of bounds in function derin(np,st).\n");
	if(st<1 || st>nstates) error("[mdmm] st out of bounds in function derin(np,st).\n");
#endif
	int pt=partype(np);
	//if np is not an initial parameter deriv=zero
	if(pt!=inits) {
		return(0.0);
	} else {
		int nin=np-npars+nstates;
		if(nin==st) return(1.0);
		else return(0.0);
	}
}

double mdmm::derobs(const int np, const int st, matrix dat) {
#ifdef MDMMBOUNDS 
	if(np<1 || np>npars) error("[mdmm] np out of bounds in function derobs(np,st,dat), np=%d \n", np);
	if(st<1 || st>nstates) error("[mdmm] st out of bounds in function derobs(st,np), st=%d \n",st);
#endif
	int pt=partype(np);
	if(pt==inits || pt==trans) return(0.0);
	else {
		if(nitems==1) return(derobs(np,st,1,dat(1)));
		else {
			double der=0.0;
			//matrix 
			datxm=dat;
			for(int it=1; it<=nitems; it++) {
				datxm(it) = xm;
				double gdp=getDataProb(st,datxm);
				der += gdp*derobs(np,st,it,dat(it));
				datxm(it) = dat(it);
			}
			return(der);
		}
	}
}

double mdmm::dertrans(const int np,const int st1, const int st2) {
	int pt=partype(np);
	if(pt!=trans) return(0.0);
	else {
		int trs1=trstate1(np);
		int trs2=trstate2(np);
		if(trs1==st1 && trs2==st2) return(1.0);
		else return(0.0);
	}
}

//hessian: not complete, nor correct, checking needed
double mdmm::hesobs(const int np1, const int np2, const int st, matrix dat) {
#ifdef MDMMBOUNDS 
	if(np1<1 || np1>npars) error("[mdmm] np1 out of bounds in function hesobs(np1,np2,st,dat), np1=%d \n", np1);
	if(np2<1 || np2>npars) error("[mdmm] np2 out of bounds in function hesobs(np1,np2,st,dat), np2=%d \n", np2);
	if(st<1 || st>nstates) error("[mdmm] st out of bounds in function hesobs(np1,np2,st,dat), st=%d \n",st);
#endif
	int pt1=partype(np1);
	int pt2=partype(np2);
	if(pt1==inits || pt1==trans || pt2==inits || pt2==trans ) return(0.0);
	else {
		if(nitems==1) return(hesobs(np1,np2,st,1,dat(1)));
		else {
			double hes=0.0;
			//matrix 
			datxm=dat;
			for(int it=1; it<=nitems; it++) {
				datxm(it) = xm;
				double gdp=getDataProb(st,datxm);
				hes += gdp*derobs(np1,st,it,dat(it));
				datxm(it) = dat(it);
			}
			return(hes);
		}
	}
	return(0.0);
}

//private functions, auxiliaries

//auxiliary for getDataProb: this is where the actual densities are returned
double mdmm::getProb(const double dat, const int st, const int it) {
	double prob = 0.0;
	if((int) dat==xm && xm!=0) prob=1.0; 
	else {
		// multinomial
		if(getMode(it)>1) prob = respPars[it-1](st, (int) dat);
		else {
			// density functions used from Rmath.h
			switch(getMode(it)) {	
				//normal
				case 1: 
					double mu,sig;
					mu = respPars[it-1](st,1);
					sig = respPars[it-1](st,2);
					prob = dnorm(dat,mu,sig,0);	
					break;
			}
		}
	}
	return(prob);
}

//auxiliary functions for derivatives and hessian (all private)
double mdmm::derobs(const int np, const int st, const int it, double datit) {
	int npst=obsstate(np); 
	if(npst!=st) return(0.0);
	else {
		int npit=itemnr(np); 
		if(npit!=it) return(0.0);
		else {
			int npitpar=itempar(np);
			
			//multinomial item
			if(getMode(it)>1) {
				if(npitpar!=((int) datit)) return(0.0);
				else return(1.0);
			} else {
				switch(getMode(it)) {
					case 1: 
						double mu=respPars[it-1](npst,1);
						double sig=respPars[it-1](npst,2);
						return(dernorm(datit,mu,sig,npitpar,0));
					//fall through for other cases
					error("derivatives of other than normal distribution not available (yet)");
				}
			}
		}
		return(0.0); // to make sure we do not get 'control may reach end of non-void function' warnings
	}
}

double mdmm::hesobs(const int np1, const int np2, const int st, const int it, double datit) {
	// discrete item => hess=0
	if(getMode(it)>1) return(0.0);
	else {
		int npst1=obsstate(np1); 
		int npst2=obsstate(np2); 
		// pars do not belong to the requested state => hess=0
		if(!(npst1==st && npst2==st)) return(0.0);
		else { 
			int npit1=itemnr(np1); 
			int npit2=itemnr(np2); 
			// pars do not belong to the same item => hess=0
			if(npit1!=npit2) return(0.0);
			else { 
				int npitpar1=itempar(np1);
				int npitpar2=itempar(np2);
				switch(getMode(it)) {
					case 1:
						double mu=respPars[it-1](npst1,1);
						double sig=respPars[it-1](npst1,2);
						return(hessnorm(datit,mu,sig,npitpar1,npitpar2,0));
						//fall through for other cases
					error("hessian of other than normal distribution not available (yet)");
				}
			}
		}
		return(0.0); // to make sure we do not get 'control may reach end of non-void function' warnings
	}
}

int mdmm::partype(const int np) {
#ifdef MDMMBOUNDS 
	if(np<1 || np>npars) error("[mdmm] np out of bounds in function partype.\n");
#endif
	int pt=0; 
	int parn;
	if(np<=(nstates*nstates)) pt=trans;
	else {
		parn = np-nstates*nstates;
		if(parn>(nstates*obsMatCols)) pt=inits;
		else pt=obser;
	}
	return(pt); 
}

int mdmm::trstate1(const int np) {
#ifdef MDMMBOUNDS 
	if(np<1 || np>(nstates*nstates)) error("[mdmm] np out of bounds in function trstate1.\n");
#endif
	int st=1;
	if(np>1) st=((np-1)/nstates)+1;
	return(st);
}

int mdmm::trstate2(const int np) {
#ifdef MDMMBOUNDS 
	if(np<1 || np>(nstates*nstates)) error("[mdmm] np out of bounds in function trstate2.\n");
#endif
	int st=(np-1)%nstates+1;
	return(st);
}

int mdmm::obsstate(const int np) {
	int st=1;
	if((np-nstates*nstates)>1) st = (np-nstates*nstates-1)/obsMatCols + 1;
	return(st);
}

int mdmm::itemnr(const int np) {
	int colnr = (np-nstates*nstates-1)%obsMatCols+1;
	int i=0;
	int prev=colnr;
	while(colnr>0) {
		prev=colnr;
		colnr -= nppDensity(modes[i]);
		i++;
	}
	return(i);
}

int mdmm::itempar(const int np) {
	int colnr = (np-nstates*nstates-1)%obsMatCols+1;
	int i=0;
	int prev=colnr;
	while(colnr>0) {
		prev=colnr;
		colnr -= nppDensity(modes[i]);
		i++;
	}
	return(prev);
}

//return the number of parameters for the density of an itemtype=mode
int mdmm::nppDensity(const int mode) {
	int npar=0;
	if(mode > 1) npar = mode;
	else {
		if(mode < -30) npar = 3;
		else npar = 2;
	}
	return(npar);
}

void mdmm::print(void) {
	Rprintf("Nr of parameters: %d\n", getNPars());
	Rprintf("Transition matrix.\n");
	transPars.print();
	Rprintf("Observation parameters.\n");
	for(int it=1; it<=nitems; it++) {
		Rprintf("Item %d parameters\n", it);
		respPars[it-1].print();
	}
	Rprintf("Initial state probabilities.\n");
	initPars.print();
	Rprintf("Missing data code: %d \n", xm);
}
