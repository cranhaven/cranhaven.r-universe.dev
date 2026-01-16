#include "infect/infect.h"

namespace infect {

Facility::Facility(int id)
{
    number = id;
    unit = new IntMap();
}

Facility::~Facility()
{
    for (unit->init(); unit->hasNext();)
        delete unit->nextValue();
    delete unit;
}

void Facility::write(ostream &os) const
{
    os << number;
}

} // namespace infect
