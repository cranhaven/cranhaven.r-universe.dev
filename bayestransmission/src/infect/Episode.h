// infect/Episode.h
#ifndef ALUN_INFECT_EPISODE_H
#define ALUN_INFECT_EPISODE_H

#include "../util/util.h"
#include "Event.h"

class Episode : public Object
{
private:

	Event *a;
	Event *d;
	SortedList *s;

public:
	Episode();
	~Episode()
    {
        delete s;
    }

	void write(ostream &os) const override;

	inline void setAdmission(Event *e)
	{
		a = e;
		s->prepend(e);
	}

	inline Event *getAdmission() const
	{
		return a;
	}

	inline bool hasAdmission() const
	{
		return a != nullptr;
	}

	inline void setDischarge(Event *e)
	{
		d = e;
		s->append(e);
	}

	inline Event *getDischarge() const
	{
		return d;
	}

	inline bool hasDischarge() const
	{
		return d != nullptr;
	}

	inline void addEvent(Event *e)
	{
		s->append(e);
	}

	inline SortedList* getEvents()
	{
		if (s != nullptr) {
			s->init();
		}
		return s;
	}

	inline bool hasEvents() const
	{
		return s != nullptr && s->size() > 0;
	}
};
#endif // ALUN_INFECT_EPISODE_H
