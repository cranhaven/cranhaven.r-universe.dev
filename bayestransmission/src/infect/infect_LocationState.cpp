#include "infect/infect.h"

namespace infect {

void LocationState::write(ostream &os) const
{
    os << getOwner() ;
    os << " (" << getSusceptible() << "+" << getLatent() << "+" << getColonized() << "=" << getTotal() << ")";
}

} // namespace infect
