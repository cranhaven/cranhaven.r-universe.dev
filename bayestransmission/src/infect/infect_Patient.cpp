#include "infect/infect.h"

namespace infect {

Patient::Patient(int id)
{
    name = id;
    group = 0;
}

void Patient::write(ostream &os) const
{
    os << name << "\t" << group;
}

} // namespace infect
