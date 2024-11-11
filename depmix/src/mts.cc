#include "mts.h"

mts::mts(void) { //default constructor
	vars = 1;
	modes = new int[vars];
	modes[0] = 1;//default construction is a timeseries of length one with a single continuous observation
	indReal = 1;
	lengths = new int[1];
	lengths[0]=1;
	weights = new double[1];
	weights[0] = 1.0;
	elements = new matrix*[indReal];
	for(int i=0; i<indReal; i++) {
		elements[i]=new matrix[lengths[i]];
		for(int j=0; j<lengths[i]; j++) {
			elements[i][j].reset(1,vars);
		}
	}
/* 	elements[0].reset(lengths[0],vars);	 */
	xm=0;
}

mts::mts(const int vm, int *mod, const int ir, int *lts, double *wts, int xm) { //series constructor
	initialize(vm,mod,ir,lts,wts,xm);
}

//empty construction, all elements zero
void mts::initialize(const int vm, int *mod, const int ir, int *lts, double *wts, int xmiss) { //internal constructor
	vars = vm;
	modes = new int[vars];
	for(int i=0; i<vars; i++) modes[i] = mod[i];
	indReal = ir;
	elements = new matrix*[indReal];
	lengths = new int[indReal];
	weights = new double[indReal];
	for(int i=0; i<indReal; i++) {
		lengths[i]=lts[i];
		weights[i]=wts[i];
		elements[i] = new matrix[lengths[i]];
		for(int j=0; j<lengths[i]; j++) {
			elements[i][j].reset(1,vars);
		}
	}
	xm=xmiss;
}

mts::~mts() { //destructor
	delete [] modes;
	delete [] lengths;
	delete [] weights;
	delete [] elements;
}

//reset to new dimensions
void mts::reset(const int vars, int *mod, const int indReal, int *lts, double *wts, int xm) {
	delete [] modes;
	delete [] lengths;
	delete [] weights;
	delete [] elements;
	initialize(vars,mod,indReal,lts,wts,xm);
}

void mts::mtsdata(double *data, const int vars, int *modes, const int indReal, int *lts, double *wts, int xm) {
	reset(vars,modes,indReal,lts,wts,xm);
	int dpt=0;
	for(int ir=1; ir<=indReal; ir++) {
		for(int tp=1; tp<=getLength(ir); tp++) {
			for(int vr=1; vr<=vars; vr++) {
				double dp;
				dp = data[dpt];
				int mod = getMode(vr);
				if(mod>1 && (dp<1 || dp>mod) && ((int)dp)!=xm) {
					error("[MTS] Datapoint out of bounds in reading markovdata at indReal=%d,  timePoint=%d and var=%d.\n",ir,tp,vr);
				}
				if(mod>1 && dp != (int) dp) {
					error("[MTS] Integer value expected in reading markovdata at indReal=%d,  timePoint=%d and var=%d.\n",ir,tp,vr);
				}
				elements[ir-1][tp-1](vr) = dp;
				dpt += 1;
			}
		}
		
	}
}

//access
double mts::operator()(const int ir, int tp, const int vr) {
#ifdef MTSBOUNDS
	if(ir<1 || ir>indReal) {
		Rprintf("Invalid acces point of multivariate timeseries at indReal=%d.\n", ir);
		error("[MTS] Invalid access\n");
	}
	if(tp<1 || tp>lengths[ir-1]) {
		Rprintf("Invalid acces of multivariate timeseries at indReal=%d and timePoint=%d.\n",ir,tp);
		error("[MTS] Invalid access\n");
	}
	if(vr<1 || vr>vars) {
		Rprintf("Invalid acces of multivariate timeseries at indReal=%d and timePoint=%d and varnr=%d.\n",ir,tp,vr);
		error("[MTS] Invalid access\n");
	}
#endif
	return(elements[ir-1][tp-1](vr));
}

//access
matrix& mts::operator()(const int ir, int tp) {
#ifdef MTSBOUNDS
	if(ir<1 || ir>indReal) {
		Rprintf("Invalid acces point of multivariate timeseries at indReal=%d.\n", ir);
		error("[MTS] Invalid access\n");
	}
	if(tp<1 || tp>lengths[ir-1]) {
		Rprintf("Invalid acces of multivariate timeseries at indReal=%d and timePoint=%d.\n",ir,tp);
		error("[MTS] Invalid access\n");
	}
#endif
	return(elements[ir-1][tp-1]);
}

double mts::operator()(const int tp) {
#ifdef MTSBOUNDS
	if(indReal!=1) {
		Rprintf("Invalid acces as univariate single timeseries, indReal not equal to one.\n");
		error("[MTS] Invalid access\n");
	}
	if(vars!=1) {
		Rprintf("Invalid acces as univariate timeseries, vars not equal to one.\n");
		error("[MTS] Invalid access\n");
	}
	if(tp<1 || tp>lengths[0]) {
		Rprintf("Invalid acces of univariate single timeseries at timePoint=%d.\n",tp);
		error("[MTS] Invalid access\n");
	}
#endif
	return(elements[0][tp-1](1));
}

int mts::getLength(const int ir) {
#ifdef MTSBOUNDS
	if(ir<1 || ir>indReal) {
		Rprintf("Invalid acces point of multivariate timeseries, indReal out of bounds (%d).\n", ir);
		error("[MTS] Invalid length access\n");
	}
#endif
	return(lengths[ir-1]);
}

double mts::getWeight(const int ir) {
#ifdef MTSBOUNDS
	if(ir<1 || ir>indReal) {
		Rprintf("Invalid acces point of multivariate timeseries, indReal out of bounds (%d).\n", ir);
		error("[MTS] Invalid weight access\n");
	}
#endif
	return(weights[ir-1]);
}

int mts::getMode(const int vr) {
#ifdef MTSBOUNDS
	if(vr<1 || vr>vars) {
		Rprintf("Invalid acces of multivariate timeseries varnr out of bounds (%d).\n",vr);
		error("[MTS] Invalid modes access\n");
	}
#endif
	return(modes[vr-1]);
}

void mts::summary(void) {
	Rprintf("Item types: ");
	for(int i=1; i<=getVars(); i++)
		Rprintf("%d ", getMode(i));
	Rprintf("\n");	
	int maxdata=5;
	if(indReal<=5) maxdata=indReal;
	Rprintf("Data length(s): ");
	for(int i=1; i<=maxdata; i++)
			Rprintf("%d ", getLength(i));
	if(indReal>5) Rprintf("... \n");
	else Rprintf("\n");
	Rprintf("Case weights: ");
	for(int i=1; i<=maxdata; i++)
			Rprintf("%f ", getWeight(i));
	if(indReal>5) Rprintf("... \n");
	else Rprintf("\n");
	int dp=0;
	Rprintf("Data points: ");
	for(int t=1;t<=getLength(1); t++) {
		for(int i=1; i<=getVars(); i++) {
			Rprintf("%f ", elements[0][t-1](i));
			dp += 1;
			if(dp>8) break;
		}
		if(dp>8) break;
	}
	Rprintf("\n");
}

void mts::print(void) {
	Rprintf("Item types: ");
	for(int i=1; i<=getVars(); i++)
		Rprintf("%d ", getMode(i));
	Rprintf("\n");	
	int maxdata=5;
	if(indReal<=5) maxdata=indReal;
	Rprintf("Data length(s): ");
	for(int i=1; i<=maxdata; i++)
			Rprintf("%d ", getLength(i));
	if(indReal>5) Rprintf("... \n");
	else Rprintf("\n");
	Rprintf("Case weights: ");
	for(int i=1; i<=maxdata; i++)
			Rprintf("%f ", getWeight(i));
	if(indReal>5) Rprintf("... \n");
	else Rprintf("\n");
	Rprintf("Data points: ");
	for(int t=1;t<=getLength(1); t++) {
		for(int i=1; i<=getVars(); i++) {
			Rprintf("%f ", elements[0][t-1](i));
		}
		Rprintf("\n");
	}
	Rprintf("\n");
}
