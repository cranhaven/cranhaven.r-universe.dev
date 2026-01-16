#include "infect/infect.h"

infect::AbxPatientState::AbxPatientState(Patient *pp, int nstates) : PatientState(pp, nstates)
{
    abc = 0;
    ever = 0;
}

int infect::AbxPatientState::onAbx() const
{
    return abc;
}

int infect::AbxPatientState::everAbx() const
{
    return ever;
}


void infect::AbxPatientState::copy(State *t)
{
    PatientState::copy(t);
    abc = ((AbxPatientState *)t)->abc;
    ever = ((AbxPatientState *)t)->ever;
}

void infect::AbxPatientState::write(ostream &os) const
{
    PatientState::write(os);
    os << " \\t" << "abx(" << abc << "," << ever << ")";
}

void infect::AbxPatientState::apply(Event *e)
{
    if (getOwner() != e->getPatient())
        return;
    infect::PatientState::apply(e);
    switch (e->getType())
    {
    case abxon:
        abc++;
        ever = 1;
        break;
    case abxoff:
        abc--;
        break;
    default:
        break;
    }
    if (abc < 0)
        abc = 0;
    if (abc)
        sysabx->add(e->getPatient());
    else
        sysabx->remove(e->getPatient());
    if (ever)
        syseverabx->add(e->getPatient());
}
