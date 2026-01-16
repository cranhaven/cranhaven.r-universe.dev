// infect/State.h
#ifndef ALUN_INFECT_STATE_H
#define ALUN_INFECT_STATE_H

#include "Event.h"

class State : public Object, public EventCoding
{
private:
	Object *owner;

protected:

        bool ownerWantsEvent(Event *e);

public:

	virtual void copy(State *s) = 0;
	virtual void apply(Event *e) = 0;
	virtual void unapply(Event *e) = 0;

	State(Object *o):owner(o)
	{
	}

	virtual ~State()
	{
	}

	void write(ostream &os) const override;

	inline Object *getOwner() const
	{
		return owner;
	}
};

#endif // ALUN_INFECT_STATE_H
