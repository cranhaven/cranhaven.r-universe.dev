#include "infect/infect.h"

namespace infect{
PatientState::PatientState(Patient *pp) : State(pp), u(nullptr), s(uncolonized), n(0) {}

PatientState::PatientState(Patient *pp, int nstates) : State(pp), u(nullptr), s(uncolonized), n(nstates) {}

int PatientState::onAbx() const
{
    return 0;
}

int PatientState::everAbx() const
{
    return 0;
}

void PatientState::copy(State *t)
{
    PatientState *p = (PatientState *) t;
    u = p->u;
    s = p->s;
}

void PatientState::apply(Event *e)
{
    if (getOwner() != e->getPatient())
        return;

    switch(e->getType())
    {
    case admission:
    case admission0:
    case admission1:
    case admission2:
    case insitu:
    case insitu0:
    case insitu1:
    case insitu2:
        u = e->getUnit();
        break;
    case discharge:
        u = 0;
        break;
    default:
        break;
    }

    if (n == 2)
    {
        switch(e->getType())
        {
        case acquisition:
            s = colonized;
            break;
        case clearance:
            s = uncolonized;
            break;

        default:
            break;
        }
    }

    if (n == 3)
    {
        switch(e->getType())
        {
        case acquisition:
            s = latent;
            break;
        case progression:
            s = colonized;
            break;
        case clearance:
            s = uncolonized;
            break;

        default:
            break;
        }
    }
}

void PatientState::unapply(Event *e)
{
    if (getOwner() != e->getPatient())
        return;

    if (n == 2)
    {
        switch(e->getType())
        {
        case acquisition:
            s = uncolonized;
            break;
        case clearance:
            s = colonized;
            break;

        default:
            break;
        }
    }

    if (n == 3)
    {
        switch(e->getType())
        {
        case acquisition:
            s = uncolonized;
            break;
        case progression:
            s = latent;
            break;
        case clearance:
            s = colonized;
            break;

        default:
            break;
        }
    }
}

void PatientState::write(ostream &os) const
{
    State::write(os);
    os << "\tUnit: ";
    os << ( u == 0 ? 0 : u->getId());
    os << " \t" << codeString(s);
}

}// namespace infect

