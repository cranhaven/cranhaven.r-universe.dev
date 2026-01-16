// infect/CountLocationState.h
#ifndef ALUN_INFECT_COUNTLOCATIONSTATE_H
#define ALUN_INFECT_COUNTLOCATIONSTATE_H

#include "LocationState.h"

class CountLocationState : public LocationState
{
private:

	int tot;
	int inf;
	int lat;

public:

	CountLocationState(Object *own, int nstates = 0);

	virtual void clear() override;
	virtual int getTotal() const override;
	virtual int getColonized() const override;
	virtual int getLatent() const override;
	virtual int getSusceptible() const override;

	virtual void copy(State *s) override;
	virtual void apply(Event *e) override;
	virtual void unapply(Event *e) override;
	virtual void write(ostream &os) const override;
};
#endif // ALUN_INFECT_COUNTLOCATIONSTATE_H
