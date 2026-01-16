#include "infect/infect.h"

namespace infect {

CountLocationState::CountLocationState(Object *own, int nstates) : LocationState(own, nstates)
{
    tot = 0;
    inf = 0;
    lat = 0;
}

void CountLocationState::clear()
{
    tot = 0;
    inf = 0;
    lat = 0;
}

int CountLocationState::getTotal() const
{
    return tot;
}

int CountLocationState::getColonized() const
{
    return inf;
}

int CountLocationState::getLatent() const
{
    return lat;
}

int CountLocationState::getSusceptible() const
{
    return tot - inf - lat;
}


void CountLocationState::copy(State *s)
{
    CountLocationState *cs = (CountLocationState *)s;
    tot = cs->tot;
    lat = cs->lat;
    inf = cs->inf;
}

void CountLocationState::apply(Event *e)
{
    if (!ownerWantsEvent(e))
        return;
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
        tot++;
        break;
    case discharge:
        tot--;
        break;
    default:
        break;
    }
    if (nStates() == 2)
    {
        switch (e->getType())
        {
        case acquisition:
            inf++;
            break;
        case clearance:
            inf--;
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
            lat++;
            break;
        case progression:
            lat--;
            inf++;
            break;
        case clearance:
            inf--;
            break;
        default:
            break;
        }
    }
}

void CountLocationState::unapply(Event *e)
{
    if (!ownerWantsEvent(e))
        return;
    if (nStates() == 2)
    {
        switch (e->getType())
        {
        case acquisition:
            inf--;
            break;
        case clearance:
            inf++;
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
            lat--;
            break;
        case progression:
            inf--;
            lat++;
            break;
        case clearance:
            inf++;
            break;
        default:
            break;
        }
    }
}

void CountLocationState::write(ostream &os) const
{
    os << getOwner();
    os << " (" << getSusceptible() << "+" << getLatent() << "+" << getColonized() << "=" << getTotal() << ")";
}

} // namespace infect
