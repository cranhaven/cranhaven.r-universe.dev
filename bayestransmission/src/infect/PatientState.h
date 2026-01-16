// infect/PatientState.h
#ifndef ALUN_INFECT_PATIENTSTATE_H
#define ALUN_INFECT_PATIENTSTATE_H

#include "InfectionCoding.h"
#include "State.h"

class PatientState : public State, public InfectionCoding
{
protected:

	Unit *u;
	InfectionStatus s;
	int n;

public:

    PatientState(Patient *pp);
    PatientState(Patient *pp, int nstates);

	inline Unit *getUnit() const
	{
		return u;
	}

	inline InfectionStatus infectionStatus() const
	{
		return s;
	}

	virtual int onAbx() const;
	virtual int everAbx() const;
	virtual void copy(State *t) override;
	virtual void apply(Event *e) override;
	virtual void unapply(Event *e) override;
	virtual void write(ostream &os) const override;
};
#endif // ALUN_INFECT_PATIENTSTATE_H
