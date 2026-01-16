#include "infect/infect.h"
#include <Rcpp.h>

namespace infect {

HistoryLink* SystemHistory::makeHistoryLink(Model *mod, Event *e)
{
    if (mod == 0)
    {
        return new HistoryLink(e,0,0,0,0);
    }
    else
    {
        return new HistoryLink
        (
                e,
                mod->makeSystemState(),
                mod->makeFacilityState(e->getFacility()),
                mod->makeUnitState(e->getUnit()),
                mod->makePatientState(e->getPatient())
        );
    }
}

HistoryLink* SystemHistory::makeHistoryLink(Model *mod, Facility *f, Unit *u, double t, Patient *p, EventCode c)
{
    Event *e = new Event(f,u,t,p,c);
    HistoryLink *h = makeHistoryLink(mod,e);
    mylinks->append(h);
    return h;
}

int SystemHistory::needEventType(EventCode e)
{
    switch(e)
    {
    case admission:
    case admission0:
    case admission1:
    case admission2:
    case insitu:
    case insitu0:
    case insitu1:
    case insitu2:
    case discharge:
    case start:
    case stop:
    case marker:
        return 1;

    default:
        return 0;
    }
}

SystemHistory::~SystemHistory()
{
    delete adm2ep;
    delete ep2adm;
    delete ep2dis;
    delete uheads;
    delete fheads;

    if (ep2ephist != 0)
    {
        for (ep2ephist->init(); ep2ephist->hasNext(); )
        {
            //delete ep2ephist->nextValue();
            Episode *ep = (Episode *) ep2ephist->next();
            EpisodeHistory *eh = (EpisodeHistory *) ep2ephist->get(ep);
            eh->removeEvents(ep->getEvents());
            delete eh;
        }
        delete ep2ephist;
    }

    for (pheads->init(); pheads->hasNext(); )
    {
        for (HistoryLink *l = (HistoryLink *)pheads->nextValue(); l != 0; )
        {
            HistoryLink *ll = l;
            l = l->pNext();
            delete ll;
        }
    }
    delete pheads;

    for (mylinks->init(); mylinks->hasNext(); )
    {
        HistoryLink *l = (HistoryLink *) mylinks->next();
        delete l->getEvent();
        delete l;
    }
    delete mylinks;
}

SystemHistory::SystemHistory(System *s, Model *m, bool verbose)
{
    mylinks = new List();

    ep2ephist = 0;

    ep2adm = new Map();
    adm2ep = new Map();
    ep2dis = new Map();

    pheads = new Map();

    uheads = new Map();
    Map *tails = new Map();

    fheads = new Map();

    shead = makeHistoryLink(m,0,0,s->startTime(),0,start);
    HistoryLink *stail = makeHistoryLink(m,0,0,s->endTime(),0,stop);
    shead->insertBeforeS(stail);

    for (IntMap *facs = s->getFacilities().get(); facs->hasNext(); )
    {
        Facility *f = (Facility *) facs->nextValue();

        HistoryLink *fhead = makeHistoryLink(m,f,0,s->startTime(),0,start);
        HistoryLink *ftail = makeHistoryLink(m,f,0,s->endTime(),0,stop);

        fhead->insertBeforeF(ftail);
        fheads->put(f,fhead);
        tails->put(f,ftail);

        // Make map of units to list heads and tails.
        // Initialize to contain just links to start and stop events.

        for (IntMap *i = f->getUnits(); i->hasNext(); )
        {
            Unit *u = (Unit *) i->nextValue();

            HistoryLink *uhead = makeHistoryLink(m,f,u,s->startTime(),0,start);
            HistoryLink *utail = makeHistoryLink(m,f,u,s->endTime(),0,stop);

            uhead->insertBeforeU(utail);
            uheads->put(u,uhead);
            tails->put(u,utail);
        }

    }

    HistoryLink **hx = new HistoryLink*[s->getPatients()->size()];
    int hxn = 0;

    for (IntMap *pat = s->getPatients().get(); pat->hasNext(); )
    {
        HistoryLink *prev = 0;

        Patient *patient = (Patient *) pat->nextValue();

        for (Map *episodes = s->getEpisodes(patient).get(); episodes->hasNext();)
        {
            Episode *ep = (Episode *) episodes->next();

            // Make list of new links and connect the patient pointers.

            for (SortedList *t = ep->getEvents(); t->hasNext(); )
            {
                Event *e = (Event *) t->next();

                HistoryLink *x = makeHistoryLink(m,e);
                
                if (prev == 0)
                {
                    pheads->put(patient,x);
                }
                else
                {
                    x->insertAfterP(prev);
                }

                if (ep2adm->get(ep) == 0)
                {
                    ep2adm->put(ep,x);
                    adm2ep->put(x,ep);
                }
                prev = x;
            }

            ep2dis->put(ep,prev);
        }

        hx[hxn++] = (HistoryLink *) pheads->get(patient);
    }

    for (int i=0; i<hxn; i++)
    {
        for (int j=i; j>0; j--)
        {
            if (hx[j]->getEvent()->getTime() < hx[j-1]->getEvent()->getTime())
            {
                HistoryLink *y = hx[j-1];
                hx[j-1] = hx[j];
                hx[j] = y;
            }
            else
            {
                break;
            }
        }
    }

    for (int i=0; i<hxn; )
    {
        hx[i]->insertBeforeS(stail);
        hx[i]->insertBeforeF((HistoryLink *)tails->get(hx[i]->getEvent()->getFacility()));
        hx[i]->insertBeforeU((HistoryLink *)tails->get(hx[i]->getEvent()->getUnit()));

        hx[i] = hx[i]->pNext();

        if (hx[i] == 0)
        {
            i++;
        }
        else
        {
            for (int j=i+1; j<hxn; j++)
            {
                if (hx[j]->getEvent()->getTime() < hx[j-1]->getEvent()->getTime())
                {
                    HistoryLink *y = hx[j-1];
                    hx[j-1] = hx[j];
                    hx[j] = y;
                }
                else
                {
                    break;
                }
            }
        }
    }

    // Put the sub lists into the full lists.

    for (IntMap *facs = s->getFacilities().get(); facs->hasNext(); )
    {
        Facility *f = (Facility *) facs->nextValue();

        HistoryLink *fhead = (HistoryLink *) fheads->get(f);
        HistoryLink *ftail = (HistoryLink *) tails->get(f);

        ftail->insertBeforeS(stail);
        fhead->insertBeforeS(shead->sNext());

        for (IntMap *i = f->getUnits(); i->hasNext(); )
        {
            Unit *u = (Unit *) i->nextValue();

            HistoryLink *uhead = (HistoryLink *) uheads->get(u);
            HistoryLink *utail = (HistoryLink *) tails->get(u);

            utail->insertBeforeF(ftail);
            uhead->insertBeforeF(fhead->fNext());

            utail->insertBeforeS(ftail);
            uhead->insertBeforeS(fhead->sNext());
        }

    }

    // Filter events model then propagate events.

    if (m != 0)
    {
        for (HistoryLink *l = getSystemHead(); l != 0; )
        {
            if (needEventType(l->getEvent()->getType()) || m->needEventType(l->getEvent()->getType()))
            {
                l = l->sNext();
            }
            else
            {
                if (verbose)
                    Rcpp::Rcerr << "Removing un needed event \t" << l->getEvent() << "\n";

                HistoryLink *ll = l;
                l = l->sNext();
                ll->remove();
                delete ll;
            }
        }
    }

    if (m != 0)
        m->handleAbxDoses(getSystemHead());

    for (HistoryLink *l = getSystemHead(); l != 0; l = l->sNext())
        l->setCopyApply();

    // Make episode histories.

    if (m != 0)
    {
        ep2ephist = new Map();
        for (ep2adm->init(); ep2adm->hasNext(); )
        {
            Episode *ep = (Episode *) ep2adm->next();
            EpisodeHistory *eh = m->makeEpisodeHistory((HistoryLink*)ep2adm->get(ep),(HistoryLink*)ep2dis->get(ep));
            ep2ephist->put(ep,eh);
        }
    }

    // Clean up.

    delete [] hx;
    delete tails;
}

EpisodeHistory** SystemHistory::getPatientHistory(Patient *pat, int *n)
{
    int k = 0;
    for (HistoryLink *l = (HistoryLink *) pheads->get(pat); l != 0; l = l->pNext())
        if (l->getEvent()->isAdmission() || l->getEvent()->isInsitu())
            k++;
    *n = k;

    EpisodeHistory **eps = new EpisodeHistory*[k];
    k = 0;
    for (HistoryLink *l = (HistoryLink *) pheads->get(pat); l != 0; l = l->pNext())
        if (l->getEvent()->isAdmission() || l->getEvent()->isInsitu())
            eps[k++] = (EpisodeHistory *) ep2ephist->get(adm2ep->get(l));

    return eps;
}

List* SystemHistory::getTestLinks()
{
    List *res = new List();

    for (HistoryLink *l = shead ; l != 0; l = l->sNext())
    {
        if (l->getEvent()->isTest())
            res->append(l);
    }

    return res;
}

Map* SystemHistory::positives()
{
    Map *pos = new Map();
    for (HistoryLink *l = getSystemHead(); l != 0; l = l->sNext())
    {
        if (l->getEvent()->isPositiveTest())
            pos->add(l->getEvent()->getPatient());
    }
    return pos;
}

int SystemHistory::sumocc()
{
    int x = 0;

    for (HistoryLink *l = shead; l != 0; l = l->sNext())
        if (l->getUState() != 0)
            x += l->getUState()->getTotal();

    return x;
}

void SystemHistory::write(ostream &os)
{
    write2(os,0);
}

void SystemHistory::write2(ostream &os, int opt)
{
    Map *count = new Map();

    switch(opt)
    {
    case 9: // All events. With count data influencing performance.

        for (uheads->init(); uheads->hasNext(); )
        {
            count->put(uheads->next(), new Integer(0));
        }

        for (HistoryLink *l = shead; l != 0; l = l->sNext())
        {
            l->getEvent()->write(os);

            os << "\t";

            Integer *cc = (Integer *) count->get(l->getEvent()->getUnit());
            if (cc != 0)
                cc->set(l->getUState()->getTotal());

            for (count->init(); count->hasNext(); )
                os << "\t" << count->nextValue();

            os << "\n";
        }
        break;

    case 8: // By unit. All link data.
        for (Map *m = getUnitHeads(); m->hasNext(); )
            for (HistoryLink *l = (HistoryLink *) m->nextValue(); l != 0; l = l->pNext())
            {
                l->write2(os,1);
                os << "\n";
            }
        break;

    case 7: // By unit. All events.
        for (Map *m = getUnitHeads(); m->hasNext(); )
            for (HistoryLink *l = (HistoryLink *) m->nextValue(); l != 0; l = l->pNext())
            {
                l->getEvent()->write2(os,1);
                os << "\n";
            }
        break;

    case 6: // By unit. Observable events only.
        for (Map *m = getUnitHeads(); m->hasNext(); )
            for (HistoryLink *l = (HistoryLink *) m->nextValue(); l != 0; l = l->pNext())
                if (l->getEvent()->isObservable())
                    os << l->getEvent() << "\n";
        break;

    case 5: // By patient. All link data.
        for (Map *m = getPatientHeads(); m->hasNext(); )
            for (HistoryLink *l = (HistoryLink *) m->nextValue(); l != 0; l = l->pNext())
            {
                l->write2(os,1);
                os << "\n";
            }
        break;

    case 4: // By patient. All events.
        for (Map *m = getPatientHeads(); m->hasNext(); )
            for (HistoryLink *l = (HistoryLink *) m->nextValue(); l != 0; l = l->pNext())
            {
                l->getEvent()->write2(os,1);
                os << "\n";
            }
        break;

    case 3: // By patient. Observable events only.
        for (Map *m = getPatientHeads(); m->hasNext(); ){
            for (HistoryLink *l = (HistoryLink *) m->nextValue(); l != 0; l = l->pNext()){
                if (l->getEvent()->isObservable()){
                    os << l->getEvent() << "\n";
                }
            }
        }
        break;

    case 2: // By system, all events and link states. NB: Not all events may be reachable by system links.
        for (HistoryLink *l = shead; l != 0; l = l->sNext())
        {
            l->write2(os,1);
            os << "\n";
        }
        break;

    case 1: // By system. All events. NB: Not all events may be reachable by system links.
        for (HistoryLink *l = shead; l != 0; l = l->sNext())
        {
            l->getEvent()->write2(os,1);
            os << "\n";
        }
        break;

    case 0: // By system, observable events only.
    default:
        for (HistoryLink *l = shead; l != 0; l = l->sNext())
            if (l->getEvent()->isObservable())
                os << l->getEvent() << "\n";
        break;
    }

    delete count;
}

} // namespace infect
