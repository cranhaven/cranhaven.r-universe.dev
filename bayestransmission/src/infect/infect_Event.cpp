#include "infect/infect.h"

namespace infect {

Event::Event(Facility *fa, Unit *un, double tm, Patient *pt, EventCode tp)
{
    fac = fa;
    unit = un;
    time = tm;
    pat = pt;
    type = tp;
}

Event::Event() : fac(0), unit(0), time(0), pat(0), type(nullevent) {}

void Event::write(ostream &os) const
{
    write2(os, 0);
}

void Event::write2(ostream &os, int opt) const
{
    int f = fac == 0 ? 0 : fac->getId();
    int u = unit == 0 ? 0 : unit->getId();
    int p = pat == 0 ? 0 : pat->getId();
    EventCode otype = type;
    switch(type)
    {
    case insitu:
        otype = admission;
        break;
    case insitu0:
        otype = ( opt ? admission0 : admission );
        break;
    case insitu1:
        otype = ( opt ? admission1 : admission );
        break;
    case insitu2:
        otype = ( opt ? admission2 : admission );
        break;
    case admission0:
    case admission1:
    case admission2:
        otype = ( opt ? type : admission );
        break;
    default:
        otype = type;
    break;
    }
    os << fixed;
    os << (f == 0 ? 0 : f) << "\\t";
    os << (u == 0 ? 0 : u) << "\\t";
    os << time << "\\t";
    os << (p == 0 ? 0 : p) << "\\t";
    os << otype;
    os.unsetf(ios::fixed | ios::scientific);
}

} // namespace infect
