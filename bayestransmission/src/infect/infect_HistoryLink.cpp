#include "infect/infect.h"

namespace infect {

HistoryLink::HistoryLink(Event *event, int l)
{
    e = event;
    setStates(0, 0, 0, 0);
    pprev = 0;
    pnext = 0;
    uprev = 0;
    unext = 0;
    fprev = 0;
    fnext = 0;
    sprev = 0;
    snext = 0;
    hprev = 0;
    hnext = 0;
    linked = l;
    hidden = 0;
}

HistoryLink::HistoryLink(Event *event, LocationState *s, LocationState *f, LocationState *u, PatientState *p, int l)
{
    e = event;
    setStates(s, f, u, p);
    pprev = 0;
    pnext = 0;
    uprev = 0;
    unext = 0;
    fprev = 0;
    fnext = 0;
    sprev = 0;
    snext = 0;
    hprev = 0;
    hnext = 0;
    linked = l;
    hidden = 0;
}

HistoryLink::~HistoryLink()
{
    if (sstate != 0)
        delete sstate;
    if (fstate != 0)
        delete fstate;
    if (ustate != 0)
        delete ustate;
    if (pstate != 0)
        delete pstate;
}

void HistoryLink::setStates(LocationState *s, LocationState *f, LocationState *u, PatientState *p)
{
    sstate = s;
    fstate = f;
    ustate = u;
    pstate = p;
}

void HistoryLink::setCopyApply()
{
    if (pstate != 0)
    {
        if (pprev != 0)
            pstate->copy(pprev->pstate);
        pstate->apply(e);
    }
    if (ustate != 0)
    {
        if (uprev != 0)
        {
            ustate->copy(uprev->ustate);
        }
        ustate->apply(e);
    }
    if (fstate != 0)
    {
        if (fprev != 0)
            fstate->copy(fprev->fstate);
        fstate->apply(e);
    }
    if (sstate != 0)
    {
        if (sprev != 0)
            sstate->copy(sprev->sstate);
        sstate->apply(e);
    }
}

void HistoryLink::write2(ostream &os, int opt) const
{
    e->write2(os, opt);
    os << "\\t::\\t";
    os << ustate;
    os << "\\t::\\t";
    os << pstate;
}

void HistoryLink::write(ostream &os) const
{
    write2(os, 0);
}

} // namespace infect
