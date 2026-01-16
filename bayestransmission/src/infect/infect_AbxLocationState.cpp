
#include "infect/infect.h"

infect::AbxLocationState::AbxLocationState(Object *own, int nstates) : CountLocationState(own, nstates)
{
    abx = new Map();
    abxinf = 0;
    abxlat = 0;
    ever = new Map();
    everinf = 0;
    everlat = 0;
}

infect::AbxLocationState::~AbxLocationState()
{
    delete abx;
    delete ever;
}

void infect::AbxLocationState::clear()
{
    CountLocationState::clear();
    abx->clear();
    abxinf = 0;
    abxlat = 0;
    ever->clear();
    everinf = 0;
    everlat = 0;
}

void infect::AbxLocationState::copy(State *s)
{
    CountLocationState::copy(s);
    AbxLocationState *as = (AbxLocationState *)s;
    abx->clear();
    for (as->abx->init(); as->abx->hasNext();)
        abx->add(as->abx->next());
    abxinf = as->abxinf;
    abxlat = as->abxlat;
    ever->clear();
    for (as->ever->init(); as->ever->hasNext();)
        ever->add(as->ever->next());
    everinf = as->everinf;
    everlat = as->everlat;
}

int infect::AbxLocationState::onAbx(Patient *p) const
{
    return abx->got(p);
}

int infect::AbxLocationState::everAbx(Patient *p) const
{
    return ever->got(p);
}

int infect::AbxLocationState::getAbxTotal() const
{
    return abx->size();
}

int infect::AbxLocationState::getEverAbxTotal() const
{
    return ever->size();
}

int infect::AbxLocationState::getAbxColonized() const
{
    return abxinf;
}

int infect::AbxLocationState::getEverAbxColonized() const
{
    return everinf;
}

int infect::AbxLocationState::getAbxLatent() const
{
    return abxlat;
}

int infect::AbxLocationState::getEverAbxLatent() const
{
    return everlat;
}

int infect::AbxLocationState::getAbxSusceptible() const
{
    return abx->size() - abxinf - abxlat;
}

int infect::AbxLocationState::getEverAbxSusceptible() const
{
    return ever->size() - everinf - everlat;
}

int infect::AbxLocationState::getNoAbxTotal() const
{
    return getTotal() - abx->size();
}

int infect::AbxLocationState::getNeverAbxTotal() const
{
    return getTotal() - ever->size();
}

int infect::AbxLocationState::getNoAbxColonized() const
{
    return getColonized() - abxinf;
}

int infect::AbxLocationState::getNeverAbxColonized() const
{
    return getColonized() - everinf;
}

int infect::AbxLocationState::getNoAbxLatent() const
{
    return getLatent() - abxlat;
}

int infect::AbxLocationState::getNeverAbxLatent() const
{
    return getLatent() - everlat;
}

int infect::AbxLocationState::getNoAbxSusceptible() const
{
    return getSusceptible() - getAbxSusceptible();
}

int infect::AbxLocationState::getNeverAbxSusceptible() const
{
    return getSusceptible() - getEverAbxSusceptible();
}

void infect::AbxLocationState::write(ostream &os) const
{
    CountLocationState::write(os);
    os << "\\t(" << abx->size() << "," << ever->size() << ")";
}

void infect::AbxLocationState::apply(Event *e)
{
    CountLocationState::apply(e);
    if (!ownerWantsEvent(e))
        return;
    Patient *p = e->getPatient();
    int ponabx = sysabx->got(p);
    int pever = syseverabx->got(p);
    switch (e->getType())
    {
    case insitu:
    case insitu0:
    case insitu1:
    case insitu2:
    case admission:
    case admission0:
    case admission1:
    case admission2:
        if (ponabx)
            abx->add(p);
        if (pever)
            ever->add(p);
        break;
    case discharge:
        abx->remove(p);
        ever->remove(p);
        break;
    case abxon:
        ever->add(p);
    case abxoff:
        if (ponabx)
            abx->add(p);
        else
            abx->remove(p);
        break;
    default:
        break;
    }
    if (ever->got(p))
    {
        if (nStates() == 2)
        {
            switch (e->getType())
            {
            case acquisition:
                everinf++;
                break;
            case clearance:
                everinf--;
                break;
            default:
                break;
            }
        }
        if (nStates() == 3)
        {
            switch (e->getType())
            {
            case acquisition:
                everlat++;
                break;
            case progression:
                everlat--;
                everinf++;
                break;
            case clearance:
                everinf--;
                break;
            default:
                break;
            }
        }
    }
    if (abx->got(p))
    {
        if (nStates() == 2)
        {
            switch (e->getType())
            {
            case acquisition:
                abxinf++;
                break;
            case clearance:
                abxinf--;
                break;
            default:
                break;
            }
        }
        if (nStates() == 3)
        {
            switch (e->getType())
            {
            case acquisition:
                abxlat++;
                break;
            case progression:
                abxlat--;
                abxinf++;
                break;
            case clearance:
                abxinf--;
                break;
            default:
                break;
            }
        }
    }
}

void infect::AbxLocationState::unapply(Event *e)
{
    CountLocationState::unapply(e);
    if (!ownerWantsEvent(e))
        return;
    if (ever->got(e->getPatient()))
    {
        if (nStates() == 2)
        {
            switch (e->getType())
            {
            case acquisition:
                everinf--;
                break;
            case clearance:
                everinf++;
                break;
            default:
                break;
            }
        }
        if (nStates() == 3)
        {
            switch (e->getType())
            {
            case acquisition:
                everlat--;
                break;
            case progression:
                everinf--;
                everlat++;
                break;
            case clearance:
                everinf++;
                break;
            default:
                break;
            }
        }
    }
    if (abx->got(e->getPatient()))
    {
        if (nStates() == 2)
        {
            switch (e->getType())
            {
            case acquisition:
                abxinf--;
                break;
            case clearance:
                abxinf++;
                break;
            default:
                break;
            }
        }
        if (nStates() == 3)
        {
            switch (e->getType())
            {
            case acquisition:
                abxlat--;
                break;
            case progression:
                abxinf--;
                abxlat++;
                break;
            case clearance:
                abxinf++;
                break;
            default:
                break;
            }
        }
    }
}
