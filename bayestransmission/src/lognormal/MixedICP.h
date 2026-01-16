#ifndef ALUN_LOGNORMAL_MIXEDICP_H
#define ALUN_LOGNORMAL_MIXEDICP_H

#include "LogNormalAbxICP.h"

class MixedICP: public LogNormalAbxICP
{
public:
	MixedICP(int nst, int isDensity, int nmet, int cap=8);

	// Acquisition model mixes constant and mass action terms.
	// Constant parameter is par[0][0]
	// Mixing parameter is par[0][1]
	// log number colonized parameter is par[0][2]
	// log total in-patients parameter is par[0][3]
	// Time parameter is par[0][4].
	// Number abx colonized parameter is par[0][5]
	// Susceptible patient on Abx effect on colonizeation is par[0][6].
	// Susceptible patient ever on Abx effect on colonizeation is par[0][7].

	double acqRate(double time, int onabx, int everabx, double ncolabx, double ncol, double tot);
	virtual double timePar() override;
	virtual double unTransform(int i, int j) override;
	virtual void set(int i, int j, double value, int update, double prival, double priorn) override;
};
#endif // ALUN_LOGNORMAL_MIXEDICP_H
