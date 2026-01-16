// infect/HistoryLink.h
#ifndef ALUN_INFECT_HISTORYLINK_H
#define ALUN_INFECT_HISTORYLINK_H

#include "LocationState.h"
#include "PatientState.h"

class HistoryLink : public Object
{
private:

	// Pointers to patient's adjacent links.
	HistoryLink* pprev;
	HistoryLink* pnext;

	// Pointers to unit's adjacent links.
	HistoryLink* uprev;
	HistoryLink* unext;

	// Pointers to facility's adjacent links.
	HistoryLink* fprev;
	HistoryLink* fnext;

	// Pointers to whole system's adjacent links.
	HistoryLink* sprev;
	HistoryLink* snext;

	// The system's, facility's, unit's and patient's state after the event.
	LocationState* sstate;
	LocationState* fstate;
	LocationState* ustate;
	PatientState* pstate;

	// Pointers to alternative proposed history links.
	HistoryLink* hprev;
	HistoryLink* hnext;

	// The event occurring at this point.
	Event* e;

	// Indicator for adding event to SystemHistory links.
	int linked;

	// Indicator for hiding event when cross validating.
	int hidden;

public:

	HistoryLink(Event* event, int l = 1);
	HistoryLink(Event* event, LocationState* s, LocationState* f, LocationState* u, PatientState* p, int l = 1);
	~HistoryLink();

	void setStates(LocationState* s, LocationState* f, LocationState* u, PatientState* p);
	void setCopyApply();

	inline void insertAsap(HistoryLink* y)
	{
		HistoryLink* snxt = y;

		for ( ; snxt != 0 && snxt->getEvent()->getTime() < getEvent()->getTime(); snxt = snxt->snext);
		for ( ; snxt != 0 && snxt->sprev != 0 && snxt->sprev->getEvent()->getTime() >= getEvent()->getTime(); snxt = snxt->sprev);

		// if (snxt == 0)
		// 	cerr << "System run off \n";

		insertBeforeS(snxt);

		HistoryLink* xx = 0;

		for (xx = snxt ; xx != 0 && xx->getEvent()->getFacility() != getEvent()->getFacility(); xx = xx->snext);
		// if (xx == 0)
		// 	cerr << "Facility run off \n";
		insertBeforeF(xx);

		for (xx = snxt ; xx != 0 && xx->getEvent()->getUnit() != getEvent()->getUnit(); xx = xx->snext);
		// if (xx == 0)
		// 	cerr << "Unit run off \n";
		insertBeforeU(xx);

		for (xx = snxt ; xx != 0 && xx->getEvent()->getPatient() != getEvent()->getPatient(); xx = xx->snext);
		// if (xx == 0)
		// 	cerr << "Patient run off \n";
		insertBeforeP(xx);
	}

	inline void insertBeforeS(HistoryLink* x)
	{
		snext = x;
		sprev = x->sprev;
		x->sprev = this;
		if (sprev != 0)
			sprev->snext = this;
	}

	inline void insertBeforeF(HistoryLink* x)
	{
		fnext = x;
		fprev = x->fprev;
		x->fprev = this;
		if (fprev != 0)
			fprev->fnext = this;
	}

	inline void insertBeforeU(HistoryLink* x)
	{
		unext = x;
		uprev = x->uprev;
		x->uprev = this;
		if (uprev != 0)
			uprev->unext = this;
	}

	inline void insertBeforeP(HistoryLink* x)
	{
		pnext = x;
		pprev = x->pprev;
		x->pprev = this;
		if (pprev != 0)
			pprev->pnext = this;
	}

	inline void insertAfterP(HistoryLink* x)
	{
		pprev = x;
		pnext = x->pnext;
		x->pnext = this;
		if (pnext != 0)
			pnext->pprev = this;
	}

	inline void removeSystem()
	{
		snext->sprev = sprev;
		sprev->snext = snext;
	}

	inline void removeFacility()
	{
		fnext->fprev = fprev;
		fprev->fnext = fnext;
	}

	inline void removeUnit()
	{
		unext->uprev = uprev;
		uprev->unext = unext;
	}

	inline void removePatient()
	{
		pnext->pprev = pprev;
		pprev->pnext = pnext;
	}

    inline void remove()
    {
        removeSystem();
        removeFacility();
        removeUnit();
        removePatient();
    }


	void write2(ostream &os, int opt) const;
	void write(ostream &os) const override;

// Trivial accessors.

	inline void setLinked(int i)
	{
		linked = i;
	}

	inline int isLinked() const
	{
		return linked;
	}

	inline void setHidden(int i)
	{
		hidden = i;
	}

	inline int isHidden() const
	{
		return hidden;
	}

	inline HistoryLink* hPrev() const
	{
		return hprev;
	}

	inline HistoryLink* hNext() const
	{
		return hnext;
	}

	inline void setHPrev(HistoryLink* l)
	{
		hprev = l;
	}

	inline void setHNext(HistoryLink* l)
	{
		hnext = l;
	}

	inline Event* getEvent()
	{
		return e;
	}

	inline void clearEvent()
	{
		e = 0;
	}

	inline HistoryLink* pPrev() const
	{
		return pprev;
	}

	inline HistoryLink* pNext() const
	{
		return pnext;
	}

	inline HistoryLink* uPrev() const
	{
		return uprev;
	}

	inline HistoryLink* uNext() const
	{
		return unext;
	}

	inline HistoryLink* fPrev() const
	{
		return fprev;
	}

	inline HistoryLink* fNext() const
	{
		return fnext;
	}

	inline HistoryLink* sPrev() const
	{
		return sprev;
	}

	inline HistoryLink* sNext() const
	{
		return snext;
	}

	inline PatientState* getPState() const
	{
		return pstate;
	}

	inline LocationState* getUState() const
	{
		return ustate;
	}

	inline LocationState* getFState() const
	{
		return fstate;
	}

	inline LocationState* getSState() const
	{
		return sstate;
	}
};

#endif // ALUN_INFECT_HISTORYLINK_H
