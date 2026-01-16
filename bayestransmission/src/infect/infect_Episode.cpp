#include "infect/infect.h"

namespace infect {

Episode::Episode() : Object()
{
    a = nullptr;
    d = nullptr;
    s = new SortedList();
}

void Episode::write(ostream &os) const
{
    os << a << "\\n";
    // for (s->init(); s->hasNext();)
    //     os << s->next() << "\\n";
    s->write(os);
    os << d;
}

} // namespace infect
