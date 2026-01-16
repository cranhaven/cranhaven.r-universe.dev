// infect/FacilityEpisodeHistory.h
#ifndef ALUN_INFECT_FACILITY_EPISODE_HISTORY_H
#define ALUN_INFECT_FACILITY_EPISODE_HISTORY_H

#include "EpisodeHistory.h"

class FacilityEpisodeHistory : public EpisodeHistory
{
protected:

	virtual void applyInitialEvent(Event *e) override
	{
		for (HistoryLink *l = d; ; l = l->fPrev())
		{
			if (l->getPState() != 0)
				l->getPState()->apply(e);

			if (l != d)
			{
				if (l->getUState() != 0)
					l->getUState()->apply(e);
				if (l->getFState() != 0)
					l->getFState()->apply(e);
			}

			if (l == a)
			{
				break;
			}
		}
	}

	virtual void unapplyInitialEvent(Event *e) override
	{
		for (HistoryLink *l = a;  ; l = l->fNext())
		{
			if (l->getPState() != 0)
				l->getPState()->unapply(e);

			if (l != d)
			{
				if (l->getUState() != 0)
					l->getUState()->unapply(e);
				if (l->getFState() != 0)
					l->getFState()->unapply(e);
			}
			else
			{
				break;
			}
		}
	}

	virtual void applyAndInsert(HistoryLink *l) override
	{
		HistoryLink *fnext = d;
		HistoryLink *unext = d;
		HistoryLink *pnext = d;

		for (HistoryLink *ll = d; ll != a; )
		{
			fnext = ll;

			if (ll->getEvent()->getUnit() == l->getEvent()->getUnit())
				unext = ll;

			if (ll->getEvent()->getPatient() == l->getEvent()->getPatient())
				pnext = ll;

			if (ll != d)
			{
				if (ll->getFState() != 0)
					ll->getFState()->apply(l->getEvent());

				if (ll->getUState() != 0)
					ll->getUState()->apply(l->getEvent());
			}

			if (ll->getPState() != 0)
				ll->getPState()->apply(l->getEvent());

			if (l->getEvent()->getTime() < ll->fPrev()->getEvent()->getTime())
			{
				ll = ll->fPrev();
			}
			else
			{
				break;
			}
		}

		l->insertBeforeF(fnext);
		if (l->getFState() != 0)
		{
			l->getFState()->copy(l->fPrev()->getFState());
			l->getFState()->apply(l->getEvent());
		}

		l->insertBeforeU(unext);
		if (l->getUState() != 0)
		{
			l->getUState()->copy(l->uPrev()->getUState());
			l->getUState()->apply(l->getEvent());
		}

		l->insertBeforeP(pnext);
		if (l->getPState() != 0)
		{
			l->getPState()->copy(l->pPrev()->getPState());
			l->getPState()->apply(l->getEvent());
		}
	}

	virtual void removeAndUnapply(HistoryLink *l) override
	{
		l->removePatient();
		l->removeUnit();
		l->removeFacility();

		for (HistoryLink *ll = l->fNext(); ; ll = ll->fNext())
		{
			if (ll->getPState() != 0)
				ll->getPState()->unapply(l->getEvent());

			if (ll != d)
			{
				if (ll->getUState() != 0)
					ll->getUState()->unapply(l->getEvent());

				if (ll->getFState() != 0)
					ll->getFState()->unapply(l->getEvent());
			}
			else
			{
				break;
			}
		}
	}

public:

	FacilityEpisodeHistory(HistoryLink *aa, HistoryLink *dd): EpisodeHistory(aa,dd)
	{
	}
};

#endif // ALUN_INFECT_FACILITY_EPISODE_HISTORY_H
