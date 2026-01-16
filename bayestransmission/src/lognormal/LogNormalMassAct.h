#ifndef ALUN_LOGNORMAL_LOGNORMALMASSACT_H
#define ALUN_LOGNORMAL_LOGNORMALMASSACT_H

#include "LogNormalICP.h"

/*
 Use the LogNormalICP setup to mimic MassActionICP.
*/

class LogNormalMassAct: public LogNormalICP
{
private:

	// value 0 == frequency dependent; 1 == density dependent; 2 == constant;
	int isDensity;

	virtual double logAcquisitionRate(double ncol, double tot);


public:

	LogNormalMassAct(int k, int isDen, int nmet=10);


// Implement LogNormalICP.
	virtual double logProgressionRate(double time, PatientState *p, LocationState *s) override;
	virtual double logProgressionGap(double t0, double t1, LocationState *s) override;
	virtual double logClearanceRate(double time, PatientState *p, LocationState *s) override;
	virtual double logClearanceGap(double t0, double t1, LocationState *s) override;
	virtual double logAcquisitionRate(double time, PatientState *p, LocationState *s) override;
	virtual double logAcquisitionGap(double t0, double t1, LocationState *s) override;
	virtual double* acquisitionRates(double time, PatientState *p, LocationState *s) override;
};
#endif // ALUN_LOGNORMAL_LOGNORMALMASSACT_H
