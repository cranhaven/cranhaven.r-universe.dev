// infect/Episode.h
#ifndef ALUN_INFECT_EPISODEHISTORY_H
#define ALUN_INFECT_EPISODEHISTORY_H

#include "../util/util.h"
#include "EventCoding.h"
#include "Event.h"
#include "HistoryLink.h"

class EpisodeHistory : public Object, public EventCoding
{
protected:

	virtual void applyInitialEvent(Event *e) = 0;

	virtual void unapplyInitialEvent(Event *e) = 0;

	virtual void applyAndInsert(HistoryLink *l) = 0;

	virtual void removeAndUnapply(HistoryLink *l) = 0;

	HistoryLink *a;
	HistoryLink *d;

	double ta;
	double td;

	HistoryLink *h;
	HistoryLink *t;

	HistoryLink *pt;
	HistoryLink *ph;

public:

	EpisodeHistory(HistoryLink *aa, HistoryLink *dd);
	~EpisodeHistory();

	void removeEvents(List *list);
	EventCode eventPreAdmission() const;
	void proposeSwitch(HistoryLink *l);

	inline HistoryLink *getProposalHead() const
	{
		return ph;
	}

	int countProposedSwitches() const;
	int countSwitches() const;
	int proposalDifferent() const;

	void clearProposal();
	void installProposal();

	void appendLink(HistoryLink *l);

	void apply();

	void unapply();

	inline HistoryLink *admissionLink() const
	{
		return a;
	}

	inline HistoryLink *dischargeLink() const
	{
		return d;
	}

	inline double admissionTime() const
	{
		return ta;
	}

	inline double dischargeTime() const
	{
		return td;
	}

	void write(ostream &os) const override;
};
#endif // ALUN_INFECT_EPISODEHISTORY_H
