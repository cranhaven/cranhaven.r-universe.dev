#include "infect/infect.h"

namespace infect {

RawEvent::RawEvent(int f, int u, double t, int p, int tp)
    : time(t), facility(f), unit(u), pat(p), type(tp)
{
}

int RawEvent::compare(Object const *const e) const
{
    RawEvent const *const x = dynamic_cast<RawEvent const *const>(e);
    if (x->pat < pat)
        return 1;
    if (pat < x->pat)
        return -1;
    if (x->time < time)
        return 1;
    if (time < x->time)
        return -1;
    if (x->facility < facility)
        return 1;
    if (facility < x->facility)
        return -1;
    if (x->unit < unit)
        return 1;
    if (unit < x->unit)
        return -1;
    return 0;
}

void RawEvent::write(ostream &os) const
{
    os << facility << "\t";
    os << unit << "\t";
    os << time << "\t";
    os << pat << "\t";
    os << type;
}

} // namespace infect
