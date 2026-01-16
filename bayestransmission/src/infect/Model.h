
// file infect/Model.h:
#ifndef ALUN_INFECT_MODEL_H
#define ALUN_INFECT_MODEL_H

#include "../util/util.h"
#include "EventCoding.h"
#include "InfectionCoding.h"
#include "Facility.h"
#include "Patient.h"

class LocationState;
class PatientState;
class HistoryLink;
class EpisodeHistory;

class SystemHistory;

class Model : public Object, public EventCoding, public InfectionCoding
{
public:

    bool cheating;

	double abxlife;
	double abxdelay;

	virtual string header() const = 0;
	virtual std::string className() const override {return "Model";};

	virtual int needEventType(EventCode e);
	virtual bool isCheating() const;
	virtual LocationState *makeSystemState();
	virtual LocationState *makeFacilityState(Facility *f);
	virtual LocationState *makeUnitState(Unit *u);
	virtual PatientState *makePatientState(Patient *);
	virtual void setAbxLife(double l);
	virtual double getAbxLife() const;
	virtual void setAbxDelay(double l);
	virtual double getAbxDelay() const;
	virtual void handleAbxDoses(HistoryLink *shead);

	virtual EpisodeHistory* makeEpisodeHistory(HistoryLink *a, HistoryLink *d) = 0;
	virtual double logLikelihood(SystemHistory *h) = 0;
	virtual void forwardSimulate(SystemHistory *h, Random *r) = 0;
	virtual void initEpisodeHistory(EpisodeHistory *eh, bool pos) = 0;
	virtual void sampleEpisodes(SystemHistory *h, int max, Random *r) = 0;
	virtual void update(SystemHistory *h, Random *r, int max) = 0;
};
#endif // ALUN_INFECT_MODEL_H
