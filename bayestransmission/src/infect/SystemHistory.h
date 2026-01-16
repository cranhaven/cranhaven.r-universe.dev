// infect/SystemHistory.h
#ifndef ALUN_INFECT_SYSTEMHISTORY_H
#define ALUN_INFECT_SYSTEMHISTORY_H

#include "../util/util.h"
#include "EventCoding.h"
#include "HistoryLink.h"
#include "Model.h"
#include "System.h"

class SystemHistory: public Object, public EventCoding
{
private:

	Map *adm2ep;
	Map *ep2adm;
	Map *ep2dis;

	Map *pheads;
	Map *uheads;
	Map *fheads;


	Map *ep2ephist;

	HistoryLink *shead;

	List *mylinks;

	HistoryLink *makeHistoryLink(Model *mod, Event *e);
	HistoryLink *makeHistoryLink(Model *mod, Facility *f, Unit *u, double t, Patient *p, EventCode c);
	int needEventType(EventCode e);

public:

	~SystemHistory();
	SystemHistory(System *s, Model *m = 0, bool verbose = 0);

	inline Map* getPatientHeads()
	{
	    pheads->init();
	    return pheads;
	}

	inline Map *getUnitHeads()
	{
	    uheads->init();
	    return uheads;
	}

	inline Map *getFacilityHeads()
	{
	    fheads->init();
	    return fheads;
	}

	inline HistoryLink *getSystemHead()
	{
	    return shead;
	}

	inline Map *getEpisodes()
	{
	    ep2ephist->init();
	    return ep2ephist;
	}

	inline Map *getAdmissions()
	{
	    ep2adm->init();
	    return ep2adm;
	}

	inline Map *getDischarges()
	{
	    ep2dis->init();
	    return ep2dis;
	}

	EpisodeHistory** getPatientHistory(Patient *pat, int *n);
	List* getTestLinks();
	Map* positives();
	int sumocc();
	void write(ostream &os) override;
	void write2(ostream &os, int opt);
};

#endif // ALUN_INFECT_SYSTEMHISTORY_H
