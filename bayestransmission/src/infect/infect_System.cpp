#include "infect/infect.h"

namespace infect {

void System::handleOutOfRangeEvent(Patient *p, int t)
{
    p->setGroup(t);
}

void System::init(RawEventList *l, stringstream &err)
{
    fac = std::make_shared<IntMap>();
    pat = std::make_shared<IntMap>();
    pepis = std::make_shared<Map>();
    start = (int)l->firstTime();
    end = (int) (0.99999999 + l->lastTime());
    makeAllEpisodes(l,err);
    setInsitus();
}

void System::setInsitus()
{
    bool done = false;
    for (pepis->init(); !done && pepis->hasNext(); )
    {
        Patient *p = (Patient *) pepis->next();
        Map *eps = (Map *) pepis->get(p);
        for (eps->init(); !done && eps->hasNext(); )
        {
            Episode *e = (Episode *) eps->next();
            Event *v = e->getAdmission();

            if (abs(v->getTime() - start) < 0.0000001)
            {
                switch(v->getType())
                {
                case admission:
                    v->setType(insitu);
                    break;
                case admission0:
                    v->setType(insitu0);
                    break;
                case admission1:
                    v->setType(insitu1);
                    break;
                case admission2:
                    v->setType(insitu2);
                    break;
                default:
                    break;
                }
            }

            done = true;
        }
    }
}

System::System(RawEventList *l)
{
    init(l,errlog);
}

System::System(RawEventList *l, stringstream &err)
{
    init(l,err);
}

System::System(istream &is, stringstream &err)
{
    RawEventList *l = new RawEventList(is,err);
    init(l,err);
    delete l;
}

System::System(
    std::vector<int> facilities,
    std::vector<int> units,
    std::vector<double> times,
    std::vector<int> patients,
    std::vector<int> types
)
{

    RawEventList *l = new RawEventList(facilities, units, times, patients, types);
    init(l,errlog);
    delete l;
}

System::~System()
{
    // Clean up episodes and their events
    // Only delete contents if we're the only owner (refcount == 1)
    if (pepis != nullptr && pepis.use_count() == 1) {
        for (pepis->init(); pepis->hasNext(); )
        {
            Map *eps = (Map *) pepis->nextValue();
            if (eps != nullptr) {
                for (eps->init(); eps->hasNext(); )
                {
                    Episode *e = (Episode *)eps->next();
                    if (e != nullptr) {
                        for (List *v = e->getEvents(); v->hasNext(); )
                        {
                            if (v->next() != nullptr) {
                                delete v->next();
                            }
                        }
                        delete e;
                    }
                }
                delete eps;
            }
        }
    }
    // pepis shared_ptr will be automatically deleted

    // Clean up patients
    // Only delete contents if we're the only owner (refcount == 1)
    if (pat != nullptr && pat.use_count() == 1) {
        for (pat->init(); pat->hasNext(); ) {
            util::Object *obj = pat->nextValue();
            if (obj != nullptr) {
                delete obj;
            }
        }
    }
    // pat shared_ptr will be automatically deleted

    // Clean up facilities
    // Only delete contents if we're the only owner (refcount == 1)
    if (fac != nullptr && fac.use_count() == 1) {
        for (fac->init(); fac->hasNext(); ) {
            util::Object *obj = fac->nextValue();
            if (obj != nullptr) {
                delete obj;
            }
        }
    }
    // fac shared_ptr will be automatically deleted
}

std::shared_ptr<Map> System::getEpisodes(Patient *p)
{
    // Add null check for patient
    if (p == nullptr) {
        errlog << "Warning: getEpisodes called with null Patient pointer\n";
        return std::make_shared<Map>();  // Return empty map instead of crashing
    }
    
    // The Episodes for a Patient are sorted to give a coherent individual history.
    Map *eps = (Map *) pepis->get(p);
    
    // Add null check for episode map
    if (eps == nullptr) {
        errlog << "Warning: No episodes found for patient " << p->hash() << "\n";
        return std::make_shared<Map>();  // Return empty map
    }
    
    eps->init();
    // Need to wrap raw pointer in shared_ptr with custom deleter that does nothing
    // because this memory is owned by pepis (the System member)
    return std::shared_ptr<Map>(eps, [](Map*){});
}

// void System::write(ostream &os)
// {
//     write2(os,0);
// }
//
// void System::write2(ostream &os,int opt)
// {
//     for (pat->init(); pat->hasNext(); )
//     {
//         for (Map *ep = getEpisodes((Patient*)pat->nextValue()); ep->hasNext(); )
//         {
//             for (List *l = ((Episode *)ep->next())->getEvents(); l->hasNext(); )
//             {
//                 ((Event *)l->next())->write2(os,opt);
//                 os << "\n";
//             }
//         }
//     }
// }

string System::get_log() {
    return errlog.str();
}

double System::timeOfLastKnownEvent(Episode *ep)
{
    double t = 0;
    if (ep->getAdmission() != 0)
        t = ep->getAdmission()->getTime();
    for (List *l = ep->getEvents(); l->hasNext(); )
        t = ((Event *)l->next())->getTime();
    return t;
}

Event* System::makeEvent(Facility *f, Unit *u, double t, Patient *p, EventCode c)
{
    Event *v = new Event(f,u,t,p,c);
    return v;
}

void System::addEpisode(Patient *p, Episode *ep)
{
    Map *eps = (Map *) pepis->get(p);
    if (eps == 0)
    {
        eps = new Map();
        pepis->put(p,eps);
    }
    eps->add(ep);
}

RawEvent* System::getEvent(List *l, EventCode c, int f, int u)
{
    for (l->init(); l->hasNext(); )
    {
        RawEvent *e = (RawEvent *) l->next();
        if (e->getTypeId() == c && (f == 0 || f == e->getFacilityId()) && (u == 0 || u == e->getUnitId()))
            return e;
    }

    return 0;
}

RawEvent* System::getEvent(List *l, int f, int u)
{
    for (l->init(); l->hasNext(); )
    {
        RawEvent *e = (RawEvent *) l->next();

        switch(e->getTypeId())
        {
        case admission:
        case admission0:
        case admission1:
        case admission2:
        case discharge:
            continue;
        default:
            if ((f == 0 || f == e->getFacilityId()) && (u == 0 || u == e->getUnitId()))
                return e;
        }
    }

    return 0;
}

bool System::gotAdmission(List *l, lab *x)
{
    return getEvent(l,admission,x->f,x->u)
    || getEvent(l,admission0,x->f,x->u)
    || getEvent(l,admission1,x->f,x->u)
    || getEvent(l,admission2,x->f,x->u) ;
}

void System::nextUnitId(List *l, int *fid, int *uid)
{
    if (l->size() == 1)
    {
        RawEvent *x = (RawEvent *) l->getFirst();
        *fid = x->getFacilityId();
        *uid = x->getUnitId();
        return;
    }

    Map *m = new Map();
    for (l->init(); l->hasNext(); )
    {
        RawEvent *x = (RawEvent *) l->next();
        m->add(new lab(x->getFacilityId(),x->getUnitId()));
    }

    int fres = -1;
    int res = -1;

    for (m->init(); m->hasNext(); )
    {
        lab *x = (lab *)m->next();
        if (getEvent(l,discharge,x->f,x->u) && !(gotAdmission(l,x)) && (x->f > fres || (x->f == fres && x->u > res)))
        {
            fres = x->f;
            res = x->u;
        }
    }

    if (res == -1)
    {
        for (m->init(); m->hasNext(); )
        {
            lab *x = (lab *)m->next();
            if (getEvent(l,discharge,x->f,x->u) && (x->f > fres || (x->f == fres && x->u > res)))
            {
                fres = x->f;
                res = x->u;
            }
        }
    }

    if (res == -1)
    {
        for (m->init(); m->hasNext(); )
        {
            lab *x = (lab *)m->next();
            if (getEvent(l,x->f,x->u) && !(gotAdmission(l,x)) && (x->f > fres || (x->f == fres && x->u > res)))
            {
                fres = x->f;
                res = x->u;
            }
        }
    }

    if (res == -1)
    {
        for (m->init(); m->hasNext(); )
        {
            lab *x = (lab *)m->next();
            if (getEvent(l,x->f,x->u) && (x->f > fres || (x->f == fres && x->u > res)))
            {
                fres = x->f;
                res = x->u;
            }
        }
    }

    if (res == -1)
    {
        for (m->init(); m->hasNext(); )
        {
            lab *x = (lab *)m->next();
            if ((x->f > fres || (x->f == fres && x->u > res)))
            {
                fres = x->f;
                res = x->u;
            }
        }
    }

    *fid = fres;
    *uid = res;

    for (m->init(); m->hasNext(); )
    {
        delete m->next();
    }
    delete m;
}

void System::makeEvents(List *n, Patient *p, Episode **cur, Facility **f, Unit **u, stringstream &err)
{
    RawEvent *e = 0;

    while (!n->isEmpty())
    {
        if (*u == 0)
        {
            // Deal with the admission event for an episode,
            // creating any implied admission if necesary.

            int fid = 0;
            int uid = 0;
            nextUnitId(n,&fid,&uid);
            getOrMakeFacUnit(fid,uid,f,u);

            e = getEvent(n,admission,(*f)->getId(),(*u)->getId());

            int isimp = -1;

            if (e == 0)
            {
                e = getEvent(n,admission0,(*f)->getId(),(*u)->getId());
                isimp = 0;
            }
            if (e == 0)
            {
                e = getEvent(n,admission1,(*f)->getId(),(*u)->getId());
                isimp = 1;
            }
            if (e == 0)
            {
                e = getEvent(n,admission2,(*f)->getId(),(*u)->getId());
                isimp = 2;
            }

            if (e != 0)
            {
                Episode *ep = new Episode();
                addEpisode(p,ep);
                *cur = ep;
                Event *v = 0;

                switch(isimp)
                {
                case -1: v = makeEvent(*f,*u,e->getTime(),p,admission);
                    break;
                case 0: v = makeEvent(*f,*u,e->getTime(),p,admission0);
                    break;
                case 1: v = makeEvent(*f,*u,e->getTime(),p,admission1);
                    break;
                case 2: v = makeEvent(*f,*u,e->getTime(),p,admission2);
                    break;
                }

                (*cur)->setAdmission(v);
                n->remove(e);
            }
            else
            {
                double mytime = 0;
                EventCode ec = admission;

                if (*cur == 0)
                {
                    mytime = start;
                    err <<  "Adding in situ:\t\t\t";
                }
                else
                {
                    mytime = 0.5 * ((*cur)->getDischarge()->getTime()+((RawEvent*)n->getFirst())->getTime());
                    err <<  "Adding admission:\t\t";
                }

                Episode *ep = new Episode();
                addEpisode(p,ep);
                *cur = ep;
                Event *v = makeEvent(*f,*u,mytime,p,ec);

                (*cur)->setAdmission(v);
                err << (*cur)->getAdmission() << "\n";
            }
        }
        else
        {
            // Deal with internal episode events.

            e = getEvent(n,(*f)->getId(),(*u)->getId());
            if (e != 0)
            {
                Event *v = makeEvent(*f,*u,e->getTime(),p,(EventCode)e->getTypeId());
                (*cur)->addEvent(v);
                n->remove(e);
                continue;
            }

            // Deal with the discharge event for the episode,
            // creating it if necessary.

            e = getEvent(n,discharge,(*f)->getId(),(*u)->getId());
            if (e != 0)
            {
                Event *v = makeEvent(*f,*u,e->getTime(),p,discharge);
                (*cur)->setDischarge(v);
                *u = 0;
                n->remove(e);
                continue;
            }

            e = (RawEvent *)n->getFirst();
            if (e != 0)
            {
                double dischtime = 0.5*(e->getTime() + timeOfLastKnownEvent(*cur));
                Event *v = makeEvent(*f,*u,dischtime,p,discharge);
                (*cur)->setDischarge(v);
                *u = 0;
                err << "Adding discharge:\t\t" << (*cur)->getDischarge() << "\n";
                continue;
            }

            err << "ERROR: Can't get here 3.\n";
        }
    }
}

void System::makePatientEpisodes(List *s, stringstream &err)
{
    // Guard against empty list
    if (s->isEmpty() || s->getFirst() == nullptr) {
        return;  // Nothing to process
    }

    RawEvent *prev = 0;
    RawEvent *curev = 0;
    Facility *f = 0;
    Unit *u = 0;
    Episode *cur = 0;
    List *n = new List();

    Patient *p = getOrMakePatient(((RawEvent *)s->getFirst())->getPatientId());

    for (s->init(); ; )
    {
        curev = (RawEvent *) s->next();

        if (curev != 0 && (curev->getTypeId() < 0 || curev->getTypeId() >= EventCoding::maxeventcode))
        {
            handleOutOfRangeEvent(p,curev->getTypeId());
            continue;
        }

        if (curev == 0 || (prev != 0 && curev->getTime() > prev->getTime()))
        {
            makeEvents(n,p,&cur,&f,&u,err);

            if (curev == 0)
                break;
        }

        n->append(curev);
        prev = curev;
    }

    if (cur != nullptr && cur->getDischarge() == 0)
    {
        Event *v = makeEvent(f,u,end,p,discharge);
        cur->setDischarge(v);
        err << "Adding terminal discharge:\t" << cur->getDischarge() << "\n";
    }

    delete n;
}

void System::makeAllEpisodes(RawEventList *l, stringstream &err)
{
    RawEvent *prev = 0;
    List *s = new List();

    for (l->init(); ; )
    {
        RawEvent *e = (RawEvent *)l->next();

        if (e == 0 || (prev != 0 && prev->getPatientId() != e->getPatientId())) // This is what requires data to be patient sorted.
        {
            makePatientEpisodes(s,err);
            s->clear();
            if (e == 0)
                break;
        }

        s->append(e);
        prev = e;
    }

    delete s;
}

void System::getOrMakeFacUnit(int m, int n, Facility **f, Unit **u)
{
    *f = (Facility *) fac->get(m);
    if (*f == 0)
    {
        *f = new Facility(m);
        fac->put(m,*f);
    }

    *u = (*f)->getUnit(n);
    if (*u == 0)
    {
        *u = new Unit(*f,n);
        (*f)->addUnit(*u);
    }
}

Patient* System::getOrMakePatient(int n)
{
    Patient *p = (Patient*) pat->get(n);
    if (p == 0)
    {
        p = new Patient(n);
        pat->put(n,p);
    }
    return p;
}


} // namespace infect
