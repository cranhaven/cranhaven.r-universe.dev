#include "infect/infect.h"

namespace infect {

void SystemEpisodeHistory::applyInitialEvent(Event *e)
{
    for (HistoryLink *l = d; ; l = l->sPrev())
    {
        if (l->getPState() != 0)
            l->getPState()->apply(e);

        if (l != d)
        {
            if (l->getSState() != 0)
                l->getSState()->apply(e);
            if (l->getFState() != 0)
                l->getFState()->apply(e);
            if (l->getUState() != 0)
                l->getUState()->apply(e);
        }

        if (l == a)
        {
            break;
        }
    }
}

void SystemEpisodeHistory::unapplyInitialEvent(Event *e)
{
    for (HistoryLink *l = a;  ; l = l->sNext())
    {
        if (l->getPState() != 0)
            l->getPState()->unapply(e);

        if (l != d)
        {
            if (l->getUState() != 0)
                l->getUState()->unapply(e);
            if (l->getFState() != 0)
                l->getFState()->unapply(e);
            if (l->getSState() != 0)
                l->getSState()->unapply(e);
        }
        else
        {
            break;
        }
    }
}

void SystemEpisodeHistory::applyAndInsert(HistoryLink *l)
{
    HistoryLink *snext = d;
    HistoryLink *fnext = d;
    HistoryLink *unext = d;
    HistoryLink *pnext = d;

    for (HistoryLink *ll = d; ll != a; )
    {
        snext = ll;

        if (ll->getEvent()->getFacility() == l->getEvent()->getFacility())
            fnext = ll;

        if (ll->getEvent()->getUnit() == l->getEvent()->getUnit())
            unext = ll;

        if (ll->getEvent()->getPatient() == l->getEvent()->getPatient())
            pnext = ll;

        if (ll != d)
        {
            if (ll->getSState() != 0)
                ll->getSState()->apply(l->getEvent());

            if (ll->getFState() != 0)
                ll->getFState()->apply(l->getEvent());

            if (ll->getUState() != 0)
                ll->getUState()->apply(l->getEvent());
        }

        if (ll->getPState() != 0)
            ll->getPState()->apply(l->getEvent());

        if (l->getEvent()->getTime() < ll->sPrev()->getEvent()->getTime())
        {
            ll = ll->sPrev();
        }
        else
        {
            break;
        }
    }

    l->insertBeforeS(snext);
    if (l->getSState() != 0)
    {
        l->getSState()->copy(l->sPrev()->getSState());
        l->getSState()->apply(l->getEvent());
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

void SystemEpisodeHistory::removeAndUnapply(HistoryLink *l)
{
    l->removePatient();
    l->removeUnit();
    l->removeFacility();
    l->removeSystem();

    for (HistoryLink *ll = l->sNext(); ; ll = ll->sNext())
    {
        if (ll->getPState() != 0)
            ll->getPState()->unapply(l->getEvent());

        if (ll != d)
        {
            if (ll->getUState() != 0)
                ll->getUState()->unapply(l->getEvent());

            if (ll->getFState() != 0)
                ll->getFState()->unapply(l->getEvent());

            if (ll->getSState() != 0)
                ll->getSState()->unapply(l->getEvent());
        }
        else
        {
            break;
        }
    }
}


} // namespace infect
