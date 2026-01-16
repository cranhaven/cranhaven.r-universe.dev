#include "infect/infect.h"

namespace infect {

int Model::needEventType(EventCode e)
{
    return 1;
}

bool Model::isCheating() const
{
    return cheating;
}

LocationState* Model::makeSystemState()
{
    return 0;
}

LocationState* Model::makeFacilityState(Facility *f)
{
    return 0;
}

LocationState* Model::makeUnitState(Unit *u)
{
    return 0;
}

PatientState* Model::makePatientState(Patient *)
{
    return 0;
}

void Model::setAbxLife(double l)
{
    abxlife = l;
}

double Model::getAbxLife() const
{
    return abxlife;
}

void Model::setAbxDelay(double l)
{
    abxdelay = l;
}

double Model::getAbxDelay() const
{
    return abxdelay;
}

void Model::handleAbxDoses(HistoryLink *shead)
{
}

} // namespace infect
