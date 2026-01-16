#ifndef ALUN_LOGNORMAL_MULTIUNITABXICP_H
#define ALUN_LOGNORMAL_MULTIUNITABXICP_H

#include "LogNormalAbxICP.h"

class MultiUnitAbxICP: public LogNormalAbxICP
{
private:

	double acqRate(int unit, int onabx, double ncolabx, double ncol, double tot);

	Map *units;

	int index(Object *u);

public:

	MultiUnitAbxICP(List *u, int nst, int isDensity, int nmet);
	virtual void setUnit(int i, Object *u, double value, int update, double prival, double priorn, double sig);

// Implement LogNormalICP.
	virtual double logAcquisitionRate(double time, PatientState *p, LocationState *ls) override;
	virtual double logAcquisitionGap(double u, double v, LocationState *ls) override;
	virtual double* acquisitionRates(double time, PatientState *p, LocationState *ls) override;
};
#endif // ALUN_LOGNORMAL_MULTIUNITABXICP_H
