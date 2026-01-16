// infect/LocationState.h
#ifndef ALUN_INFECT_LOCATIONSTATE_H
#define ALUN_INFECT_LOCATIONSTATE_H

#include "State.h"
#include "Event.h"

class LocationState : public State
{
private:

	int n;

public:

	LocationState(Object *own, int nstates = 0) : State(own), n(nstates){}

	inline int nStates() const {return n;}

	virtual void clear() = 0;
	virtual int getTotal() const = 0;
	virtual int getColonized() const = 0;
	virtual int getLatent() const = 0;
	virtual int getSusceptible() const = 0;
	virtual void copy(State *s) override = 0;
	virtual void apply(Event *e) override = 0;
	virtual void unapply(Event *e) override = 0;


	virtual void write(ostream &os) const override;
};

#endif // ALUN_INFECT_LOCATIONSTATE_H
