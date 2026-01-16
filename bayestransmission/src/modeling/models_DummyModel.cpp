#include "modeling/modeling.h"

namespace models {

infect::PatientState* DummyModel::makePatientState(infect::Patient *p)
{
    return p == 0 ? 0 : new infect::AbxPatientState(p,nstates);
}

int DummyModel::needEventType(EventCode e)
{
    switch(e)
    {
    case insitu:
    case insitu0:
    case insitu1:
    case insitu2:
    case admission:
    case admission0:
    case admission1:
    case admission2:
    case discharge:
    case negsurvtest:
    case possurvtest:
    case negclintest:
    case posclintest:
    case abxdose:
    case abxon:
    case abxoff:
        return 1;
    case acquisition:
    case progression:
    case clearance:
        return cheating ? 1 : 0;
    default:
        return 0;
    }
}

} // namespace models
