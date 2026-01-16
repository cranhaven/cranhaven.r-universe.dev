// infect/SystemEpisodeHistory.h
#ifndef ALUN_INFECT_SYSTEMEPISODEHISTORY_H
#define ALUN_INFECT_SYSTEMEPISODEHISTORY_H

#include "EpisodeHistory.h"

class SystemEpisodeHistory : public EpisodeHistory
{
protected:

	virtual void applyInitialEvent(Event *e) override;
	virtual void unapplyInitialEvent(Event *e) override;
	virtual void applyAndInsert(HistoryLink *l) override;
	virtual void removeAndUnapply(HistoryLink *l) override;

public:

	SystemEpisodeHistory(HistoryLink *aa, HistoryLink *dd): EpisodeHistory(aa,dd)
	{
	}

};

#endif // ALUN_INFECT_SYSTEMEPISODEHISTORY_H
