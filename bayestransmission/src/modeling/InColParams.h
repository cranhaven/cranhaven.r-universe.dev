#ifndef ALUN_MODELING_INCOLPARAMS_H
#define ALUN_MODELING_INCOLPARAMS_H

#include "Parameters.h"

namespace models {

class InColParams : public Parameters
{
protected:

	int nstates;

public:

	InColParams(int nst)
	{
		nstates = nst;
	}

	virtual int getNStates() const override
	{
		return nstates;
	}

	virtual double *acquisitionRates(double time, infect::PatientState *p, infect::LocationState *s) = 0;

	virtual double eventRate(double time, EventCode c, infect::PatientState *p, infect::LocationState *s) = 0;

	virtual double **rateMatrix(double time, infect::PatientState *p, infect::LocationState *u) = 0;
};

} // namespace models

#endif //ALUN_MODELING_INCOLPARAMS_H
