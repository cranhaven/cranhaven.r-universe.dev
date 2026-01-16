// infect/SetLocationState.h
#ifndef ALUN_INFECT_SETLOCATIONSTATE_H
#define ALUN_INFECT_SETLOCATIONSTATE_H

#include "LocationState.h"
#include "InfectionCoding.h"

class SetLocationState : public LocationState, public InfectionCoding
{
protected:

	Map *pat;
	Map *sus;
	Map *lat;
	Map *col;

	int interSize(Map *a, Map *b);
	int interSize(Map *a, Map *b, int g);
	int subsetSize(Map *a, int g) const;

public:

	SetLocationState(Object *own, int ns = 0);

	~SetLocationState();

	inline void clear() override
	{
		pat->clear();
		sus->clear();
		lat->clear();
		col->clear();
	}

	inline Map *getPatients()
	{
		pat->init();
		return pat;
	}

	virtual int getTotal() const override;
	virtual int getTotal(int g) const;
	virtual int getSusceptible() const override;
	virtual int getSusceptible(int g) const;
	virtual int getLatent() const override;
	virtual int getLatent(int g) const;
	virtual int getColonized() const override;
	virtual int getColonized(int g) const;
	virtual void copy(State *ss) override;
	virtual void apply(Event *e) override;
	virtual void unapply(Event *e) override;
	void write(ostream &os) const override;

};
#endif // ALUN_INFECT_SETLOCATIONSTATE_H
