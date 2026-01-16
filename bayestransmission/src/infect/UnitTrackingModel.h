#ifndef ALUN_MODELING_UNITTRACKINGMODEL_H
#define ALUN_MODELING_UNITTRACKINGMODEL_H

#include "Model.h"
#include "SetLocationState.h"
#include "PatientState.h"
#include "UnitEpisodeHistory.h"

class UnitTrackingModel : public Model
{
protected:

	int nstates;
	Map *units;

public:

	UnitTrackingModel(int ns = 2): nstates(ns)
	{
		units = new Map();
	}

	virtual Map *getUnits()
	{
		units->init();
		return units;
	}

	virtual int getNStates() const
	{
		return nstates;
	}

/*
	virtual HistoryLink *makeHistLink(Facility *f, Unit *u, double time, Patient *p, EventCode type, int linked)
	{
		units->put(u,0);

		return new HistoryLink
		(
			new Event(f,u,time,p,type),
			makeSystemState(),
			makeFacilityState(f),
			makeUnitState(u),
			makePatientState(p),
			linked
		);
	}
*/

	virtual string header() const override
	{
		return "";
	}

	virtual void write (ostream &os) override
	{
	}

	virtual int needEventType(EventCode e) override
	{
		return true;
	}

	virtual LocationState *makeSystemState() override
	{
		return 0;
	}

	virtual LocationState *makeFacilityState(Facility *f) override
	{
		return 0;
	}

	virtual LocationState *makeUnitState(Unit *u) override
	{
		return u == 0 ? 0 : new SetLocationState(u,nstates);
		//return u == 0 ? 0 : new SetLocationState(u,nstates);
	}

	virtual PatientState *makePatientState(Patient *p) override
	{
		return p == 0 ? 0 : new PatientState(p,nstates);
	}

	virtual EpisodeHistory *makeEpisodeHistory(HistoryLink *a, HistoryLink *d) override
	{
		return new UnitEpisodeHistory(a,d);
	}

// Dummy methods to satisfy Model requirements.

	virtual double logLikelihood(SystemHistory *h) override
	{
		return 0;
	}

	virtual void forwardSimulate(SystemHistory *h, Random *r) override
	{
	}

	virtual void initEpisodeHistory(EpisodeHistory *h, bool b) override
	{
	}

	virtual void sampleEpisodes(SystemHistory *h, int i, Random *r) override
	{
	}

	virtual void update(SystemHistory *h, Random *r, int i)  override = 0;

};
#endif // ALUN_MODELING_UNITTRACKINGMODEL_H
