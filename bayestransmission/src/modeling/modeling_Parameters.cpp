#include "modeling/modeling.h"

namespace models {

void Parameters::update(Random *r)
{
    update(r,0);
}

int Parameters::eventIndex(EventCode e)
{
    switch(e)
    {
    case acquisition:
        return 0;
    case progression:
        return 1;
    case clearance:
        return 2;
    default:
        return -1;
    }
}

int Parameters::stateIndex(InfectionStatus s) const
{
    switch(s)
    {
    case uncolonized:
        return 0;
    case latent:
        return 1;
    case colonized:
        return 2;
    default:
        return -1;
    }
}

int Parameters::testResultIndex(EventCode e) const
{
    switch(e)
    {
    case negtest:
    case negsurvtest:
    case negclintest:
        return 0;

    case postest:
    case possurvtest:
    case posclintest:
        return 1;

    default:
        return -1;
    }
}

} // namespace models
