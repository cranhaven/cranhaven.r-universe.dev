#include "infect/infect.h"

namespace infect {

EpisodeHistory::EpisodeHistory(HistoryLink *aa, HistoryLink *dd)
{
    h = 0;
    t = 0;
    ph = 0;
    pt = 0;
    a = aa;
    d = dd;
    ta = a->getEvent()->getTime();
    td = d->getEvent()->getTime();
}

EpisodeHistory::~EpisodeHistory()
{
    for (HistoryLink *l = h; l != 0;)
    {
        HistoryLink *ll = l->hNext();
        delete l->getEvent();
        if (!l->isLinked())
            delete l;
        l = ll;
    }
}

void EpisodeHistory::removeEvents(List *list)
{
    for (HistoryLink *l = h; l != 0;)
    {
        HistoryLink *ll = l->hNext();
        list->remove(l->getEvent());
        l = ll;
    }
}

EventCoding::EventCode EpisodeHistory::eventPreAdmission() const
{
    EventCode c = nullevent;
    for (HistoryLink *l = ph; l != 0; l = l->hNext())
        if (!l->isLinked())
            c = l->getEvent()->getType();
    return c;
}

void EpisodeHistory::proposeSwitch(HistoryLink *l)
{
    if (ph == 0)
    {
        l->setHNext(0);
        l->setHPrev(0);
        ph = l;
        pt = l;
    }
    else
    {
        l->setHNext(0);
        l->setHPrev(pt);
        pt->setHNext(l);
        pt = l;
    }
}

int EpisodeHistory::countProposedSwitches() const
{
    int x = 0;
    for (HistoryLink *l = ph; l != 0; l = l->hNext())
        x += l->isLinked();
    return x;
}

int EpisodeHistory::countSwitches() const
{
    int x = 0;
    for (HistoryLink *l = h; l != 0; l = l->hNext())
        x += l->isLinked();
    return x;
}

int EpisodeHistory::proposalDifferent() const
{
    HistoryLink *l = h;
    HistoryLink *pl = ph;
    while (l != 0 && pl != 0)
    {
        if (l == 0)
            return 1;
        if (pl == 0)
            return 1;
        if (l->getEvent()->getType() != pl->getEvent()->getType())
            return 1;
        if (l->getEvent()->getTime() != pl->getEvent()->getTime())
            return 1;
        l = l->hNext();
        pl = pl->hNext();
    }
    return 0;
}

void EpisodeHistory::clearProposal()
{
    for (HistoryLink *l = ph; l != 0;)
    {
        HistoryLink *ll = l->hNext();
        delete l->getEvent();
        delete l;
        l = ll;
    }
    ph = 0;
    pt = 0;
}

void EpisodeHistory::installProposal()
{
    HistoryLink *l = h;
    h = ph;
    ph = l;
    l = t;
    t = pt;
    pt = l;
}

void EpisodeHistory::appendLink(HistoryLink *l)
{
    unapply();
    installProposal();
    proposeSwitch(l);
    installProposal();
    apply();
}

void EpisodeHistory::apply()
{
    for (HistoryLink *l = h; l != 0; l = l->hNext())
    {
        if (l->isLinked())
            applyAndInsert(l);
        else
            applyInitialEvent(l->getEvent());
    }
}

void EpisodeHistory::unapply()
{
    for (HistoryLink *l = t; l != 0; l = l->hPrev())
    {
        if (l->isLinked())
            removeAndUnapply(l);
        else
            unapplyInitialEvent(l->getEvent());
    }
}

void EpisodeHistory::write(ostream &os) const
{
    for (HistoryLink *l = h; l != 0; l = l->hNext())
    {
        if (!l->isLinked())
            os << "\\t-\\t" << l << "\\n";
        else
            os << "\\t+\\t" << l << "\\n";
    }
    for (HistoryLink *l = a; ; l = l->uNext())
    {
        if (!l->isLinked())
            os << "**\\t-\\t" << l << "\\n";
        else
            os << "**\\t+\\t" << l << "\\n";
        if (l == d)
            break;
    }
}

} // namespace infect
