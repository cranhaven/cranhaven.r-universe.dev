#include "logl.h"

// #include "npsolfit.h"

extern "C" {
	
//this is the actual loglikelihood function for models with covariates
void loglikelihood(double *pars, int *tdcov, double *objf, int *grad, int *hess, double *gr, double *hs, int *grset, int *hsset, int *grInd, double *gradIndividual, double *scale, int *info) {

	if(printlevel>29) Rprintf("Starting to compute likelihood.\n");

	int nrcomp=models.getNrComp();
	int ngroups=models.getNrGroups();
	int *npars = new int[nrcomp];
	int *nstates = new int[nrcomp];
	int totalstates=0;
	
	for(int cp=0; cp<nrcomp; cp++) {
		npars[cp]=models.mods[cp].getNPars();
		nstates[cp]=models.mods[cp].getStates();
		totalstates += nstates[cp];
	}
	
	// (auxiliaries for) gradients from mixing proportions
	matrix *mp1; mp1 = new matrix[ngroups];
	matrix *mpt; mpt = new matrix[ngroups];
	matrix *mptfinal; mptfinal = new matrix[ngroups];
	
	for(int ng=0; ng<ngroups; ng++) { 
		mp1[ng].reset(totalstates,nrcomp);
		mpt[ng].reset(totalstates,nrcomp);
		mptfinal[ng].reset(nrcomp);
	}
	
	hess[0]=0; // hessian not supported
	
	//zero the necessary workspace
	if(grad[0]) {
		for(int ng=0; ng<ngroups; ng++) { 
			for(int cp=0; cp <nrcomp; cp++) {
				for(int np=1; np<=npars[cp]; np++) {
					models.psitfinal[ng*nrcomp+cp](np) = 0.0;
				}
			}
		}
	}
	
	matrix gradIndi(1,1);
	if(grInd[0]==1) {
		if(ngroups==1 && nrcomp==1) {
			gradIndi.reset(ngdat.getIndReal(0),npars[0]+1);
		}
		else Rprintf("Individual case gradients not implemented for multigroup nor multicomponent models.\n");
	}
			
	int totpars=models.getNPars();
	double *tdpars; tdpars=new double[totpars];
	double *covpr; covpr=new double[totpars];
	
	//translate x into model pars
	//split pars into true pars and covariate pars    
	if(tdcov[0]==1) {
		for(int i=0; i<totpars; i++) tdpars[i]=pars[i];
		for(int i=0; i<totpars; i++) covpr[i]=pars[i+totpars];
	} else {
		models.setPars(pars);
	}
	
	double loglike = 0.0;	
	matrix indLogl;
	
	//lystig and hughes likelihood: scaling is done with factors of t-1!!
	for(int ng=0; ng<ngroups; ng++) { 
		indLogl.reset(ngdat.getIndReal(ng),1);
		for(int nr=1; nr<=ngdat.getIndReal(ng); nr++) { 
			double sf1=0.0;
			double sft=0.0;
			int t=1;
			if(tdcov[0]==1) {
				//here the time dependent parameters are put into the model
				for(int i=0; i<totpars; i++) tdpars[i]=pars[i]+covpr[i]*((double) ngcov(ng+1,nr,t));
				models.setPars(tdpars);
			}
			for(int cp=0; cp < nrcomp; cp++) {
				//initial forward vars, ie t=1
				models.fwd1[cp] = (had(models.mods[ng*nrcomp+cp].getInProb(),models.mods[ng*nrcomp+cp].getDataProb(ngdat(ng+1,nr,t))))*models.mixprop(ng+1,cp+1);
			} //component fwd1's computed
			//get their sum
			for(int cp=0; cp < nrcomp; cp++) sf1 += models.fwd1[cp].msum();
			indLogl(nr) += log(sf1);
			//compute gradient vectors for t=1
			if(grad[0]) {
				int prst=0;
				for(int cp=0; cp<nrcomp; cp++) {
					if(cp>0) prst += nstates[cp-1];
					for(int st=1; st<=nstates[cp]; st++) {
						// gradients for mixing proportions
						int inst=st+prst;
						mp1[ng](inst,cp+1) = models.mods[ng*nrcomp+cp].getInProb(st)*models.mods[ng*nrcomp+cp].getDataProb(st,ngdat(ng+1,nr,t));
						// gradients for other pars
						for(int np=1; np<=npars[cp]; np++) {
							models.psi1[ng*nrcomp+cp](st,np) = models.mixprop(ng+1,cp+1)*
								(models.mods[ng*nrcomp+cp].derin(np,st)*models.mods[ng*nrcomp+cp].getDataProb(st,ngdat(ng+1,nr,t))+
								 models.mods[ng*nrcomp+cp].getInProb(st)*models.mods[ng*nrcomp+cp].derobs(np,st,ngdat(ng+1,nr,t)));
						 }
					}
				}
			}
			//compute hessian elements (t=1)
//			if(hess[0]) {
//				for(int cp=0; cp<nrcomp; cp++) {
//					for(int np1=1; np1<=npars[cp]; np1++) {
//						for(int np2=1; np2<=npars[cp]; np2++) {
//							for(int st=1; st<=nstates[cp]; st++) {
//								models.omega1[cp][st-1](np1,np2) = models.mixprop(ng+1,cp+1)*
//									(models.mods[ng*nrcomp+cp].derin(np2,st)*models.mods[ng*nrcomp+cp].derobs(np1,st,ngdat(ng+1,nr,t)) +
//									 models.mods[ng*nrcomp+cp].derin(np1,st)*models.mods[ng*nrcomp+cp].derobs(np2,st,ngdat(ng+1,nr,t)) +
//									 models.mods[ng*nrcomp+cp].getInProb(st)*models.mods[ng*nrcomp+cp].hesobs(np1,np2,st,ngdat(ng+1,nr,t)));
//							}
//						}
//					}
//				}
//			}			
// 			//compute hessian elements (t=1)
// 			if(hess[0]) {
// 				for(int cp=0; cp<nrcomp; cp++) {
// 					for(int np1=1; np1<=npars[cp]; np1++) {
// 						for(int np2=1; np2<=npars[cp]; np2++) {
// 							for(int st=1; st<=nstates[cp]; st++) {
// 								models.omega1[cp][st-1](np1,np2) = models.mixprop(ng+1,cp+1)*
// 									(models.mods[ng*nrcomp+cp].derin(np2,st)*models.mods[ng*nrcomp+cp].derobs(np1,st,ngdat.data[ng].elements[nr-1].rown(t) ) +
// 									 models.mods[ng*nrcomp+cp].derin(np1,st)*models.mods[ng*nrcomp+cp].derobs(np2,st,ngdat.data[ng].elements[nr-1].rown(t)) +
// 									 models.mods[ng*nrcomp+cp].getInProb(st)*models.mods[ng*nrcomp+cp].hesobs(np1,np2,st,ngdat.data[ng].elements[nr-1].rown(t)));
// 							}
// 						}
// 					}
// 				}
// 			}
			//induction of forward vars (t>1)
			if(ngdat.getLength(ng,nr)>1) {
				for (int t=2; t<=ngdat.getLength(ng,nr); t++) {	
					if(tdcov[0]==1) {
						// set the right time dependent pars
						for(int i=0; i<totpars; i++) tdpars[i]=pars[i]+covpr[i]*((double) ngcov(ng+1,nr,t));
						models.setPars(tdpars);
					}
					sft=0.0;
					//compute fwdt per component
					for(int cp=0; cp < nrcomp; cp++) {
						models.fwdt[cp] = (had(transpose(models.mods[ng*nrcomp+cp].getTrProb())*models.fwd1[cp],models.mods[ng*nrcomp+cp].getDataProb(ngdat(ng+1,nr,t))))*(1.0/sf1);
						sft += models.fwdt[cp].msum();
					} 
					indLogl(nr) += log(sft);
					// zero the workspace vars psit and mpt
					if(grad[0]) {
						int prst=0;
						for(int cp1=0; cp1<nrcomp; cp1++) {
							if(cp1>0) prst += nstates[cp1-1];
							for(int st=1; st<=nstates[cp1]; st++) {
								int inst=st+prst;
								for(int cp2=0; cp2<nrcomp; cp2++) {
									mpt[ng](inst,cp2+1) = 0.0; 
								}
								for(int np=1; np<=npars[cp1]; np++) {
									models.psit[ng*nrcomp+cp1](st,np) = 0.0;
								}
							}
						}
					}
					// compute gradient vectors
					// gradient of mixing proporion of single is unneccessary to compute???
					if(grad[0]) {
						int prst=0;
						for(int cp1=0; cp1<nrcomp; cp1++) {
							if(cp1>0) prst += nstates[cp1-1];
							for(int st1=1; st1<=nstates[cp1]; st1++) {
								// mixing prop par gradients
								int inst1=st1+prst;
								for(int cp2=0; cp2<nrcomp; cp2++) {
									if(cp1==cp2) {
										for(int st2=1; st2<=nstates[cp2]; st2++) {
											int inst2=st2+prst;
											mpt[ng](inst1,cp2+1) += (mp1[ng](inst2,cp2+1)*models.mods[ng*nrcomp+cp1].getTrPar(st2,st1)*models.mods[ng*nrcomp+cp1].getDataProb(st1,ngdat(ng+1,nr,t)))/sf1;
										}	
									}
								}
								// other pars gradients
								for(int np=1; np<=npars[cp1]; np++) {
									for(int st2=1; st2<=nstates[cp1]; st2++) {
										models.psit[ng*nrcomp+cp1](st1,np) += (models.psi1[ng*nrcomp+cp1](st2,np)*models.mods[ng*nrcomp+cp1].getTrPar(st2,st1)*models.mods[ng*nrcomp+cp1].getDataProb(st1,ngdat(ng+1,nr,t))+
																			   models.fwd1[cp1](st2)*(models.mods[ng*nrcomp+cp1].dertrans(np,st2,st1)*models.mods[ng*nrcomp+cp1].getDataProb(st1,ngdat(ng+1,nr,t))+
																									models.mods[ng*nrcomp+cp1].getTrPar(st2,st1)*models.mods[ng*nrcomp+cp1].derobs(np,st1,ngdat(ng+1,nr,t))))/sf1;
									}
								}
							}
						}			
					}
					//shift forward & other vars
					for(int cp1=0; cp1 < nrcomp; cp1++) {
						models.fwd1[cp1] = models.fwdt[cp1];
						models.psi1[ng*nrcomp+cp1] = models.psit[ng*nrcomp+cp1]; 
						mp1[ng] = mpt[ng];
					}
					sf1=sft;
				}
			}// end of current realization at t=length
			loglike += ngdat.getWeight(ng,nr)*indLogl(nr);
			// build up the sums of psit over realizations (sum over states psitfinal+=psiT/fwdT) for the current group
			if(grad[0]) {
				for(int cp1=0; cp1<nrcomp; cp1++) {
					for(int np=1; np<=npars[cp1]; np++) {
						double npsum=0.0;
						for(int st=1; st<=nstates[cp1]; st++) {
							npsum += models.psi1[ng*nrcomp+cp1](st,np);
						}
						models.psitfinal[ng*nrcomp+cp1](np) += ngdat.getWeight(ng,nr)*npsum/sf1;
						if(grInd[0]) gradIndi(nr,np+1) = ngdat.getWeight(ng,nr)*npsum/sf1;
					}
					int prst=0;
					double mpsum=0.0;
					for(int cp2=0; cp2<nrcomp; cp2++) {
						if(cp2>0) prst += nstates[cp2-1];
						for(int st=1; st<=nstates[cp2]; st++) {
							int inst=st+prst;
							mpsum += mp1[ng](inst,cp1+1);
						}
					}
					mptfinal[ng](cp1+1) += ngdat.getWeight(ng,nr)*mpsum/sf1;
					if(grInd[0]) gradIndi(nr,1) = ngdat.getWeight(ng,nr)*mpsum/sf1;
				}
			}
			// build up omegafinal terms, ie sum over states, realizations and groups
			// case weighting not applied here yet
			if(hess[0]) {
				for(int cp=0; cp<nrcomp; cp++) {
					for(int np1=1; np1<=npars[cp]; np1++) {
						for(int np2=1; np2<=npars[cp]; np2++) {
							double npsum=0.0;
							for(int st=1; st<=nstates[cp]; st++) {
								npsum += models.omega1[cp][st-1](np1,np2);
							}
							models.omegafinal[cp](np1,np2) += npsum/sf1;
						}
					}
				}
			}
			if(printlevel>29) Rprintf("logLikelihood: %f after case nr %d \n", loglike, nr);
		} // end realizations
	} // end groups, likelihood computed
		
	//set the loglikelihood
	objf[0] = scale[0]*loglike; 
		
	//set the gradients
	if(grad[0]) {
		int pergrouppars=0;
		for(int ng=0; ng<ngroups; ng++) {
			int nptotal=0;
			if(ng>0) nptotal += ng*pergrouppars;
			for(int cp=0; cp<nrcomp; cp++) {
				gr[nptotal] = scale[0]*mptfinal[ng](cp+1);
				grset[nptotal] = 1;
				nptotal += 1;
			}
			for(int cp=0; cp<nrcomp; cp++) {
				for(int np=1; np<=npars[cp]; np++) {
					gr[nptotal] = scale[0]*models.psitfinal[ng*nrcomp+cp](np);
					grset[nptotal] = 1;
					nptotal += 1;
					
				}
			}
			if(ng==0) pergrouppars=nptotal;
		}
	}
	
	int npt=0;
	for(int cp=0; cp<nrcomp; cp++) npt +=  npars[cp];
	npt += nrcomp;
	matrix hesstotal(npt,npt);
	
	//set the hessian
//	if(hess[0]) {
//		int htot=nrcomp;
//		for(int ng=0; ng<ngroups; ng++) { 
//			for(int cp1=0; cp1<nrcomp; cp1++) {
//				for(int np1=1; np1<=npars[cp1]; np1++) {
//					for(int np2=1; np2<=npars[cp1]; np2++) {
//						hesstotal(np1+htot,np2+htot) = scale[0]*(models.omegafinal[cp1](np1,np2)-models.psitfinal[ng*nrcomp+cp1](np1)*models.psitfinal[ng*nrcomp+cp1](np2));
//					}
//				}
//				htot += npars[cp1];
//			}
//		}
//		htot=0;
//		for(int i=1; i<=npt; i++) {
//			for(int j=1; j<=npt; j++) {
//				hs[htot] = hesstotal(i,j);
//				htot += 1;
//			}
//		}
//	}
	
	if(grInd[0]) {
		int k=0;
		for(int nr=1; nr<=ngdat.getIndReal(0); nr++) { 
			for(int np=1; np<=(npars[0]+1); np++) {
				gradIndividual[k] = gradIndi(nr,np);
				k += 1;
			}
		}
	}
	
	//output if requested
	if(printlevel>14) Rprintf("logLikelihood: %f\n", loglike);
	
	if(grad[0]) {
		if(printlevel>19) {
			Rprintf("Gradients\n");
			for(int ng=0; ng<ngroups; ng++) { 
				for(int cp=0; cp<nrcomp; cp++) {
					matrix gr = transpose(models.psitfinal[ng*nrcomp+cp]);
					gr.print();
				}
				mptfinal[ng].print();
			}
		}
	}
	
//	if(hess[0]) {
//		if(printlevel>19) {
//			Rprintf("Hessian\n");
//			hesstotal.print();
//		}
//	}
	
	int insanity=0;
	//put (in)sanity checks here
	if(insanity) info[0]=0;
	//used to stop early using only loglike as criterion (used for bootstrapped p-value)
	if(loglike>crit) info[0]=1;
	
	//closing stuff
	delete [] mp1;
	delete [] mpt;
	delete [] mptfinal;
	
	delete [] tdpars;
	delete [] covpr;
	
} //end lltdcov

/* posterior state sequences */
void posteriors(double *states, double *postdelta, int *comp, double *pars, int *tdcov, int *group, int *indnr) {
	
	// return most likely state sequence and associated probs
	// vars states, postdelta and comp are the return values
	
	//fixed throughout this routine
	int ng=group[0]-1;
	int nrcomp = models.getNrComp();
	int nr=indnr[0];
	
	// auxiliaries=workspace (move to models as with fwd etc ..., maybe even use fwd, which is already allocated??)
	matrix *delta1; delta1 = new matrix[nrcomp];
	matrix *deltat; deltat = new matrix[nrcomp];
	for(int i=0; i<nrcomp;i++) {
		delta1[i].reset(models.mods[ng*nrcomp+i].getStates(),1); 
		deltat[i].reset(models.mods[ng*nrcomp+i].getStates(),1); 
	}
	
	int totpars=models.getNPars();
	double *tdpars; tdpars=new double[totpars];
	double *covpr; covpr=new double[totpars];
	
	//translate x into model pars
	//split pars into true pars and covariate pars
	if(tdcov[0]==1) {
		for(int i=0; i<totpars; i++) tdpars[i]=pars[i];
		for(int i=0; i<totpars; i++) covpr[i]=pars[i+totpars];
	} else {
		models.setPars(pars);
	}
					
//define and set delta
	matrix *delta; delta = new matrix[nrcomp];
	for(int cp=0; cp < nrcomp; cp++) delta[cp].reset(ngdat.getLength(ng,nr), models.mods[ng*nrcomp+cp].getStates());
	//define and set psi
	matrix *psi; psi = new matrix[nrcomp];
	for(int cp=0; cp < nrcomp; cp++) {
		psi[cp].reset(ngdat.getLength(ng,nr), models.mods[ng*nrcomp+cp].getStates());
	}
	//initial delta vars, ie t=1
	//set tdpars
	if(tdcov[0]==1) {
		//here the time dependent parameters are put into the model
		int t=1;
		for(int i=0; i<totpars; i++) tdpars[i]=pars[i]+covpr[i]*((double) ngcov(ng+1,nr,t));
		models.setPars(tdpars);
	}
	for(int cp=0; cp < nrcomp; cp++) {
		int t=1;
		delta1[cp] = had(models.mods[ng*nrcomp+cp].getInProb(), models.mods[ng*nrcomp+cp].getDataProb(ngdat(ng+1,nr,t)));
		delta1[cp] = delta1[cp]*models.mixprop(ng+1,cp+1);
	} //component delta1's computed
	//normalize delta1's to prevent underflow (over all states together, important!!!)
	double sum=0.0;
	for(int cp=0; cp < nrcomp; cp++) {
		for(int st=1; st <= models.mods[ng*nrcomp+cp].getStates(); st++) {
			sum += delta1[cp](st);
		}
	}
	for(int cp=0; cp < nrcomp; cp++) {
		for(int st=1; st <= models.mods[ng*nrcomp+cp].getStates(); st++) {
			delta1[cp](st) /= sum;
		}
	}
	//set overall delta's
	for(int cp=0; cp < nrcomp; cp++) {
		int t=1;
		for(int j=1; j <= models.mods[ng*nrcomp+cp].getStates(); j++) {
			delta[cp](t,j) = delta1[cp](j);
		}
	}
	//compute delta for t>1
	if(ngdat.getLength(ng,nr)>1) {
		for(int t=2; t<=ngdat.getLength(ng,nr); t++) {
			if(tdcov[0]==1) {
				//here the time dependent parameters are put into the model
				for(int i=0; i<totpars; i++) tdpars[i]=pars[i]+covpr[i]*((double) ngcov(ng+1,nr,t));
				models.setPars(tdpars);
			}
			for(int cp=0; cp < nrcomp; cp++) {
				matrix dt(models.mods[ng*nrcomp+cp].getStates(),1);
				for(int j=1; j <= models.mods[ng*nrcomp+cp].getStates(); j++) {
					for(int i=1; i<= models.mods[ng*nrcomp+cp].getStates(); i++) {
						dt(i) = delta1[cp](i)*models.mods[ng*nrcomp+cp].getTrPar(i,j);
					}
					int maxdt = argmax(dt);
					psi[cp](t,j) = (double) maxdt;
					deltat[cp](j) = dt(maxdt)*models.mods[ng*nrcomp+cp].getDataProb(ngdat(ng+1,nr,t))(j);
				}
			}//component deltat's computed
			//normalize deltat's to prevent underflow
			double sum=0.0;
			for(int cp=0; cp < nrcomp; cp++) {
				for(int st=1; st <= models.mods[ng*nrcomp+cp].getStates(); st++) {
					sum += deltat[cp](st);
				}
			}
			for(int cp=0; cp < nrcomp; cp++) {
				for(int st=1; st <= models.mods[ng*nrcomp+cp].getStates(); st++) {
					deltat[cp](st) /= sum;
				}
			}
			//set overall delta's and shift delta1 up
			for(int cp=0; cp < nrcomp; cp++) {
				delta1[cp]=deltat[cp];
				for(int j=1; j <= models.mods[ng*nrcomp+cp].getStates(); j++) {
					delta[cp](t,j) = deltat[cp](j);
				}
			}
		}
	}
	//find max delta at t=T, first for each component
	int endt=ngdat.getLength(ng,nr);
	matrix maxt(nrcomp,1);
	matrix maxstate(nrcomp,1);
	for(int cp=0; cp < nrcomp; cp++) {
		maxt(cp+1) = max(delta[cp].rown(endt));
		maxstate(cp+1) = argmax(delta[cp].rown(endt));
	}
	//... then set component
	int maxcp=argmax(maxt);
	comp[0]=maxcp;
	//... then set final state
	states[endt-1]=maxstate(maxcp);
	int sumst=0;
	for(int cp=0; cp < nrcomp; cp++) sumst += models.mods[ng*nrcomp+cp].getStates();
	//backtracking, find previous states
	if(endt>1) {
		for(int t=endt; t>0; t--) {
			if(t<endt) states[t-1]=psi[maxcp-1](t+1,(int) states[t]);
			int j=0;
			for(int cp=0; cp < nrcomp; cp++) {
				for(int st=1; st <= models.mods[ng*nrcomp+cp].getStates(); st++) {
					j += 1;
					postdelta[(t-1)*sumst-1+j]=delta[cp](t,st);
				}
			}
		}
	}
		
	//finish up
	delete [] delta;
	delete [] delta1;
	delete [] deltat;
	delete [] psi;
	
	delete [] tdpars;
	delete [] covpr;
	
}	

void setCrit(double *criter) {
	crit=criter[0];
	// This will stop optimization when the loglikelihood is better then crit, useful for
	// goodness of fit bootstrapping
	Rprintf("stop crit=%f\n",crit);
}

void mixModelSetUp(int *ngroups, int *nrcomp, int *nstates, int *nitems, int *itemtypes, double *pars, int *xmissing, int *print) {
	
	printlevel=print[0];
	int xm=xmissing[0];
	
	// allocated memory
	// all of this has to be moved to R, not neccessary here
	// allocated memory	by calling initWorkspace()
	for(int cp=0; cp < nrcomp[0]; cp++) {
		//fwd matrices
		totalMemory += 2*nstates[cp]*sizeof(double);
		//psi matrices
		totalMemory += models.ncpars[cp]*nstates[cp]*sizeof(double);
		//omega matrices, used to compute hessian=not implemented yet
// 		totalMemory += nstates[cp]*models.ncpars[cp]*models.ncpars[cp]*sizeof(double);
		//psi + omega final matrices
		totalMemory += (models.ncpars[cp]+1)*models.ncpars[cp]*sizeof(double);
	}
	//other local stuff
	totalMemory += 100*sizeof(double);
	
	if(printlevel>19) Rprintf("Allocated memory has size: %d \n", totalMemory);
	
	//reset models
	models.reset(ngroups[0],nrcomp[0],nstates,nitems[0],itemtypes,xm);
	
	if(printlevel>19) Rprintf("Model reset in to right size.\n");
	
	models.setPars(pars);
	
	//output if requested
	if(printlevel>19) {
		models.print();
		Rprintf("Multigroup mixed model set up finished\n");
	}
}

void covSetUp(int *ngroups, int *nrcomp, int *nstates, int *nitems, int *itemtypes, double *pars, int *xmissing, int *print) {
	
	printlevel=print[0];
	int xm=xmissing[0];
	
	//reset models
	covpars.reset(ngroups[0],nrcomp[0],nstates,nitems[0],itemtypes,xm);
	covpars.setPars(pars);
	
	//output if requested
	if(printlevel>19) {
		covpars.print();
		Rprintf("Covariates model set up finished\n");
	}
}

void multiDataSetUp(int *ng, int *print) {
	printlevel=print[0];
	ngdat.reset(ng[0]);
	if(printlevel>19) ngdat.summary();
}

void ngDataSetUp(int *groupnr, double *data, int *vars, int *modes, int *indReal, int *lengths, double *weights, int *xmiss, int *print) {
	printlevel=print[0];
	int gnr=groupnr[0];
	int xm=xmiss[0];
	if(gnr<1||gnr>ngdat.getNGroups()) error("Wrong groupnr in data set up");
	if(printlevel>19) Rprintf("Setting data for group %d \n", gnr);
	if(printlevel>19) ngdat.data[gnr-1].summary();
	ngdat.data[gnr-1].mtsdata(data,vars[0],modes,indReal[0],lengths,weights,xm);
	if(printlevel>19) ngdat.data[gnr-1].summary();
	if(printlevel>29) ngdat.data[gnr-1].print();
}

void multiCovSetUp(int *ng, int *print) {
	printlevel=print[0];
	ngcov.reset(ng[0]);
	if(printlevel>19) ngcov.summary();
}

void ngCovSetUp(int *groupnr, double *data, int *vars, int *modes, int *indReal, int *lengths, double *weights, int *xmiss, int *print) {
	printlevel=print[0];
	int gnr=groupnr[0];
	int xm=xmiss[0];
	if(gnr<1||gnr>ngcov.getNGroups()) error("Wrong groupnr in covariates set up");
	if(printlevel>19) Rprintf("Setting data for group %d \n", gnr);
	ngcov.data[gnr-1].mtsdata(data,vars[0],modes,indReal[0],lengths,weights,xm);
	if(printlevel>19) ngcov.data[gnr-1].summary();
}


/************************************************************/
/*															*/
/*	NPSOL FUNCTION DEFINITIONS, UNCOMMENT IF YOU HAVE NPSOL	*/
/*															*/
/************************************************************/


// MAIN OPT ROUTINE CALLED FROM R
// This can be used for any problem ... provided that the loglikelihood function is provided
/*
void npsolc(int *n, int *nclin, int *ncnln, int *ldA, int *ldJu, int *ldR, 
			double *A, double *bl, double *bu,
			int *inform, int *iter, int *istate, double *c, double *cJacu, double *clamda, 
			double *objf, double *gradu, double *R, double *optpars,
			int *totMem, int *maxnpcalls, int *optfile,
			int *print, int *deriv, int *tdcov, int *fixedlogical, double *fixedvals, int *nrpars) {
	
	printlevel = print[0];
	derivatives = deriv[0];
	tdc = tdcov[0];
	npars = nrpars[0];
	fixed = new int[npars];
	fixedvalues = new double[npars];
	for(int i=0; i<npars; i++) {
		fixed[i] = fixedlogical[i];
		fixedvalues[i] = fixedvals[i];
	}
		
	totalMemory=0;
	
	//workspace variables
	int extra=100; //overflow workspace in ints/doubles
	// integer workspace for npsol
	int *iw;
	int *leniw; leniw=new int[1];
	int *lenw; lenw=new int[1];
	double *w; 
	leniw[0] = 3*n[0]+nclin[0]+2*ncnln[0]+extra;
	iw = new int[leniw[0]]; 
	//double workspace for npsol
	if(nclin[0]==0 && ncnln[0]==0)
	lenw[0] = 20*n[0] + extra;
	else { 
		if(ncnln[0]==0)  
		lenw[0] = 2*(n[0])*(n[0]) + 20*n[0] + 11*nclin[0] + extra; 
		else 
		lenw[0] = 2*n[0]*n[0] + n[0]*nclin[0] + 20*n[0] + 11*nclin[0] + 21*ncnln[0] + extra;  
	}
	w = new double[lenw[0]];
	
	totalMemory += sizeof(double)*lenw[0]; //double=8 bytes
	totalMemory += sizeof(int)*leniw[0]; //int=4bytes
	
	if(optfile[0]==1) {
		//before calling npsol, options may be changed by calling eg npoptn ('Iterion limit = 100') etc
		int *ioptns; ioptns = new int[1]; ioptns[0] = 33;
		ifstream optionfile;
		optionfile.open("npopt");
		if(optionfile == NULL) {
			Rprintf("Error: could not find/open npsol options file npopt.\n");
			error("This should be in the current working directory.\n");
		}
		optionfile.close();
		F77_CALL (opoptf)();//open the options  file on unit 33 (for no good reason)
		F77_CALL (npfile) (ioptns, inform); //read it
		if(inform[0]==1) error("Could not read npsol options file\n");
		if(inform[0]==2) error("Npsol options file has incorrect format\n");
		F77_CALL (cloptf)(); //close the options file
		delete [] ioptns;
	}	
	
/// NPSOL MAIN CALLING ROUTINE   //////////////////
		
	int *iters; iters = new int[maxnpcalls[0]];
	int *infs; infs = new int[maxnpcalls[0]];
	for(int i=0; i<maxnpcalls[0]; i++) {
		iters[i]=0;
		infs[i]=0;
	}
	
	int nrc=0;
	
	F77_CALL(npsol) (n,nclin,ncnln,ldA,ldJu,ldR,A,bl,bu,funcon,funobj,inform,iter,istate, 
					 c,cJacu,clamda,objf,gradu,R,optpars,iw,leniw,w,lenw);
	
	totMem[0] = totalMemory;
	objf[0]=-objf[0];
	
	// delete workspace etc. 
	delete [] leniw;
	delete [] iw;
	delete [] lenw;
	delete [] w;
	
	delete [] iters;
	delete [] infs;
	
	delete [] fixed;
	delete [] fixedvalues;
	
}

//this function is called by npsol and should return the loglikelihood and the gradient if requested
void funobj(int *mode, int *noptpars, double *optpars, double *objf, double *g, int *nstate) {
	
	//mode options which can save memory and computation time
	//if mode=2 assign both f and g
	//if mode=1 assign only g
	//if mode=0 assign only f
	
	//nstate is ignored, there is no need for initialization of variables or otherwise, this is all done before		
	
	//put zeroes into pars such that it has correct length
	double *pars; pars = new double[npars];
	int k=0;
	for(int i=0; i<npars; i++) {
		if(fixed[i]==0) pars[i]=fixedvalues[i];
		else {
			pars[i]=optpars[k];
			k += 1;
		}
	}
		
	int *grad; grad = new int[1];
	if(mode[0]>0 && derivatives>0) grad[0]=1;
	else grad[0]=0;
	
	// hessian not needed in npsol (and anyway not implemented yet)
	int *hess; hess = new int[1]; hess[0]=0;
	
	double *gr; gr = new double[npars]; //the gradient vector
	double *hs; hs = new double[1]; //hessian is never asked in npsol so no memory needed
	
	int *grset; grset = new int[npars];
	for(int i=0; i<npars; i++) grset[i] = 0;
	int *hsset; hsset = new int[1];
	
	double *scale; scale = new double[1]; scale[0] = -1.0;
	int *inf; inf = new int[1]; inf[0] = 0;
	
	//used to request individual case contributions to the gradient, never used in npsol optimization
	int *grInd; grInd=new int[1]; grInd[0]=0;
	double *gradIndividual; gradIndividual=new double[1]; gradIndividual[0]=0;
	
	loglikelihood(pars,&tdc,objf,grad,hess,gr,hs,grset,hsset,grInd,gradIndividual,scale,inf);
	
	if(inf[0]!=0) {
		mode[0]=-1;
		Rprintf("Npsol optimization terminated due to insane results.\n");
	} else {
		if(grad[0]==1) {
			int k=0;
			for(int i=0; i<npars; i++) {
				if(grset[i]==1 && fixed[i]!=0) {
					g[k] = gr[i];
					k += 1;
				}
			}
		}
	}
}

//derivatives of non-linear contraints of funobj (dummy function because there are no non-linear constraints)
void funcon(int *mode, int *ncnln, int *n, int *ldJ, int *needc, double *x, double *c, double *cJac, int *nstate) { }

*/

// END NPSOL FUNCTIONS

// UNCOMMNENT ABOVE TO USE NPSOL FUNCTIONS

} // end extern "C"
