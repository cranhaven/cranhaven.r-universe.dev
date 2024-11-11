
#include "mgdmm.h"

//default empty constructor
mgdmm::mgdmm(void) { 
	int *ns; ns=new int[1]; ns[0]=1;
	initialize(1,1,ns,1,ns,-9999);
	delete [] ns;
}

//default constructor
mgdmm::mgdmm(const int ng, const int nrc, int *nst, const int nit, int *itt, int xm) {
	initialize(ng,nrc,nst,nit,itt,xm);
}

void mgdmm::initialize(const int ng, const int nrc, int *nst, const int nit, int *itt, int xm) {
	ngroups = ng;
	nrcomp = nrc;
	nstates = new int[nrcomp];
	for(int j=0; j<nrcomp; j++) {
		nstates[j] =nst[j];
	}
	nitems = nit;
	itemtypes = new int[nit];
	for(int j=0; j<nitems; j++) {
		itemtypes[j] = itt[j];
	}
	// create appropriate number of models
	mods = new mdmm[ngroups*nrcomp];
	// reset components to appropriate sizes 
	compsizes.reset(nrcomp,1);
	ncpars = new int[nrcomp];
	for(int ng=0; ng<ngroups; ng++) {
		for(int cp=0; cp<nrcomp; cp++) {
			mods[ng*nrcomp+cp].reset(nstates[cp],nitems,itemtypes,xm);
			compsizes(cp+1)=mods[ng*nrcomp+cp].getNPars();
		}
	}
	for(int cp=0; cp<nrcomp; cp++) ncpars[cp]=mods[cp].getNPars();
	mixprop.reset(ngroups,nrcomp);
	initWorkspace();
}

void mgdmm::initWorkspace(void) {
	fwd1 = new matrix[nrcomp];
	fwdt = new matrix[nrcomp];
	for(int cp=0; cp<nrcomp; cp++) { 
		fwd1[cp].reset(nstates[cp],1); 
		fwdt[cp].reset(nstates[cp],1); 
	}
	psi1 = new matrix[ngroups*nrcomp];
	psit = new matrix[ngroups*nrcomp];
	psitfinal = new matrix[ngroups*nrcomp]; 
	for(int ng=0; ng<ngroups; ng++) {
		for(int cp=0; cp<nrcomp; cp++) {
			psi1[ng*nrcomp+cp].reset(nstates[cp],ncpars[cp]);
			psit[ng*nrcomp+cp].reset(nstates[cp],ncpars[cp]);
			psitfinal[ng*nrcomp+cp].reset(ncpars[cp]);
		}
	}
	// this is quite a big memory chunk used in computing the hessian
	// which is not functional right now ... hence the < 20 condition
	if(nstates[0]<20) {
		omega1 = new matrix*[nrcomp];
		omegat = new matrix*[nrcomp];
		for(int cp=0; cp<nrcomp; cp++) {
			omega1[cp] = new matrix[nstates[cp]];
			omegat[cp] = new matrix[nstates[cp]];
		}
		for(int cp=0; cp<nrcomp;cp++) {
			for(int st=0; st<nstates[cp]; st++) {
				omega1[cp][st].reset(ncpars[cp],ncpars[cp]);
				omegat[cp][st].reset(ncpars[cp],ncpars[cp]);
			}
		}
		omegafinal = new matrix[nrcomp]; 
		for(int cp=0; cp<nrcomp;cp++) omegafinal[cp].reset(ncpars[cp],ncpars[cp]);
	}
}

mgdmm::~mgdmm(void) {
	delete [] itemtypes;
	delete [] mods;
	delete [] ncpars;
	delete [] fwd1;
	delete [] fwdt;
	delete [] psi1;
	delete [] psit;
	delete [] psitfinal;
	if(nstates[0]<20) {
		for(int cp=0; cp<nrcomp; cp++) {
			delete [] omega1[cp];
			delete [] omegat[cp];
		}
		delete [] omega1;
		delete [] omegat;
		delete [] omegafinal;
	}
	delete [] nstates;
}

void mgdmm::reset(const int ngroups, const int nrc, int *nst, const int nit, int *itt, int xm) {
	delete [] itemtypes;
	delete [] mods;
	delete [] ncpars;
	delete [] fwd1;
	delete [] fwdt;
	delete [] psi1;
	delete [] psit;
	delete [] psitfinal;
	if(nstates[0]<20) {
		for(int cp=0; cp<nrcomp; cp++) {
			delete [] omega1[cp];
			delete [] omegat[cp];
		}
		delete [] omega1;
		delete [] omegat;
		delete [] omegafinal;
	}
	delete [] nstates;
	initialize(ngroups,nrc,nst,nit,itt,xm);
}

void mgdmm::setPars(double *pars) {
	int ngrouppars; ngrouppars = ((int) compsizes.msum()) + nrcomp;
	// split pars into appropriate parts and set the values of 
	// component model and group parameters
	for(int i=0; i<ngroups; i++) {
		double *xgrpars; xgrpars = new double[ngrouppars];
		for(int j=0; j<ngrouppars; j++) {
			xgrpars[j] = pars[i*ngrouppars+j];
		}
		for(int j=0; j<nrcomp; j++) {
			mixprop(i+1,j+1) = xgrpars[j];
		}
		int st = nrcomp;
		for(int j=0; j<nrcomp; j++) {
			int cs = (int) compsizes(j+1);
			double *xcomp; xcomp = new double[cs];
			for(int k=0; k<cs; k++) {
				xcomp[k]=xgrpars[st+k];
			}
			st += cs;
			mods[i*nrcomp+j].setPars(xcomp);
			delete [] xcomp;
		}
		delete [] xgrpars;
	}
}

void mgdmm::rescale(void) {
	for(int i=0; i<ngroups; i++) {
		for(int j=0; j<nrcomp; j++) {
			mods[i*nrcomp+j].rescale();
		}
	}
}

void mgdmm::print(void) {
	Rprintf("Matrix of mixing proportions\n");
	mixprop.print();
	for(int i=0; i<ngroups; i++) {
		for(int j=0;j<nrcomp; j++) {
			Rprintf("Parameters for group %d, component model %d \n", i+1, j+1);
			mods[i*nrcomp+j].print();
		}
	}
}
