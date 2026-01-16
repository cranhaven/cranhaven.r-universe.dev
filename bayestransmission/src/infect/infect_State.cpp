#include "infect/infect.h"

namespace infect {

bool State::ownerWantsEvent(Event *e)
{
    Unit *u = dynamic_cast<Unit *>(getOwner());
    if (u)
        return u == e->getUnit();

    Facility *f = dynamic_cast<Facility *>(getOwner());
    if (f)
        return f == e->getFacility();

    if (getOwner() == 0)
        return 1;

    // cerr << "Unknown owner type: " << getOwner() << "\n";
    return 0;
}

void State::write(ostream &os) const
{
    os << owner;
}


} // namespace infect
