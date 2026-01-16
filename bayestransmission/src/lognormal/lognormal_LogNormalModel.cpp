#include "lognormal/lognormal.h"

// protected
namespace lognormal{
LogNormalModel::LogNormalModel(int nst, int fw, int ch) : BasicModel(nst,fw,ch)
{
    abxbyonoff = 0;
    dumpers = new List();
}

// public
LogNormalModel::LogNormalModel(int nst, int abxtest, int nmetro, int fw, int ch) : BasicModel(nst,fw,ch)
{
    abxbyonoff = 0;
    dumpers = new List();

    isp = new InsituParams(nstates);
    survtsp = new TestParamsAbx(nstates,abxtest);
    clintsp = new RandomTestParams(nstates);
    ocp = new OutColParams(nstates,nmetro);
    icp = new LogNormalAbxICP(nst,0,nmetro);
    abxp = new models::AbxParams(nstates);
}

LogNormalModel::LogNormalModel(List *l, int nst, int abxtest, int nmetro, int fw, int ch) : BasicModel(nst,fw,ch)
{
    abxbyonoff = 0;
    dumpers = new List();

    //icp = ( l == 0 ? new LogNormalAbxICP(nst,0,nmetro) :  new MultiUnitAbxICP(l,nst,0,nmetro) );
    icp = new LogNormalAbxICP(nst,0,nmetro);


    isp = new InsituParams(nstates);
    ocp = new OutColParams(nstates,nmetro);
    survtsp = new TestParamsAbx(nstates,abxtest);
    clintsp = new RandomTestParams(nstates);
    abxp = new AbxParams(nstates);
}

LogNormalModel::~LogNormalModel()
{
    if (abxp != 0)
        delete abxp;
    if (ocp != 0)
        delete ocp;
    if (isp != 0)
        delete isp;
    if (clintsp != 0 && clintsp != survtsp)
        delete clintsp;
    if (survtsp != 0)
        delete survtsp;
    if (icp != 0)
        delete icp;

    for (dumpers->init(); dumpers->hasNext(); )
    {
        Object *x = dumpers->next();
        delete x;
    }
    delete dumpers;
}

LogNormalAbxICP* LogNormalModel::getInColParams() const
{
    return (LogNormalAbxICP *)icp;
}

int LogNormalModel::needEventType(EventCode e)
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
        return 1;

    case abxdose:
        return ( abxbyonoff ? 0 : 1 );

    case abxon:
    case abxoff:
        return ( abxbyonoff ? 1 : 0);

    case acquisition:
    case progression:
    case clearance:
        return cheating ? 1 : 0;

    default:
        return 0;
    }
}

PatientState* LogNormalModel::makePatientState(Patient *p)
{
    return p == 0 ? 0 : new AbxPatientState(p,nstates);
}

LocationState* LogNormalModel::makeUnitState(Unit *u)
{
    return u == 0 ? 0 : new AbxLocationState(u,nstates);
}

void LogNormalModel::setAbx(bool onoff, double delay, double life){
    abxbyonoff = onoff;
    if (!abxbyonoff)
    {
        setAbxDelay(delay);
        setAbxLife(life);
    }
}


void LogNormalModel::read(istream &is)
{
    string sdump;

    is >> sdump >> abxbyonoff;
    skipLine(is);

    if (!abxbyonoff)
    {
        double abxd = 0.0;
        is >> sdump >> abxd;
        setAbxDelay(abxd);
        skipLine(is);

        double abxl = 0.0;
        is >> sdump >> abxl;
        setAbxLife(abxl);
        skipLine(is);
    }

    skipLine(is);

    // In situ parameters.
    readInsituParams(getInsituParams(),is);
    skipLine(is);

    // Surveillance test parameters.
    TestParams *stsp = getSurveillanceTestParams();
    readTestParams(stsp,is);
    skipLine(is);

    //  Clinical test parameters.
    RandomTestParams *ctsp = (RandomTestParams *) getClinicalTestParams();
    if (ctsp != 0 && ctsp != stsp)
    {
        readRandomTestParams(ctsp,is);
        skipLine(is);
    }

    // Out of unit infection parameters.
    readOutColParams(getOutColParams(),is);
    skipLine(is);

    // In unit infection parameters.
    readInColParams((LogNormalICP *)getInColParams(),is);
    skipLine(is);

    // Abx parameters.

    AbxParams *abxp = getAbxParams();
    if (abxp != 0)
    {
        readAbxParams(abxp,is);
        skipLine(is);
    }
}

void LogNormalModel::handleAbxDoses(HistoryLink *shead)
{
    if (abxbyonoff)
        return;

    // Loop through all events picking out abxdose events.
    for (HistoryLink *l = shead; l != 0; )
    {
        if (l->getEvent()->getType() != abxdose)
        {
            l = l->sNext();
            continue;
        }

        // Find on and off times corresponding to dose event.
        // Find first events folowing these times.
        double ont = l->getEvent()->getTime();
        ont += getAbxDelay();
        HistoryLink *onpnext = l;
        for ( ; onpnext != 0; onpnext = onpnext->pNext())
            if (onpnext->getEvent()->getTime() >= ont)
                break;

        double offt = l->getEvent()->getTime();
        offt += getAbxLife();
        HistoryLink *offpnext = l;
        for ( ; offpnext != 0; offpnext = offpnext->pNext())
            if (offpnext->getEvent()->getTime() >= offt)
                break;

        // Discard on and off times implied to be out of unit.
        if (offpnext == onpnext && onpnext != 0 && onpnext->getEvent()->isAdmission())
        {
            onpnext = 0;
            offpnext = 0;
        }

        // Create off abx event with fix if it's implied to be out of unit.
        if (offpnext)
        {
            HistoryLink *snext = 0;

            if (offpnext->getEvent()->isAdmission())
            {
                offpnext = offpnext->pPrev();
                offt = offpnext->getEvent()->getTime();
                snext = offpnext;
            }
            else
            {
                for (snext = l; snext != 0; snext = snext->sNext())
                    if (snext->getEvent()->getTime() >= offt)
                        break;
            }

            Event *e = offpnext->getEvent();
            Event *off = new Event(e->getFacility(),e->getUnit(),offt,e->getPatient(),abxoff);
            HistoryLink *loff = new HistoryLink
            (
                    off,
                    makeSystemState(),
                    makeFacilityState(off->getFacility()),
                    makeUnitState(off->getUnit()),
                    makePatientState(off->getPatient())
            );

            loff->insertAsap(snext);
            dumpers->append(off);
        }

        // Crate on abx event with fix if its implied to be out of unit.
        if (onpnext)
        {
            HistoryLink *snext = 0;

            if (onpnext->getEvent()->isAdmission())
            {
                onpnext = onpnext->pPrev();
                ont = onpnext->getEvent()->getTime();
                snext = onpnext;
            }
            else
            {
                for (snext = l; snext != 0; snext = snext->sNext())
                    if (snext->getEvent()->getTime() >= ont)
                        break;
            }

            Event *e = onpnext->getEvent();
            Event *on = new Event(e->getFacility(),e->getUnit(),ont,e->getPatient(),abxon);
            HistoryLink *lon = new HistoryLink
            (
                    on,
                    makeSystemState(),
                    makeFacilityState(on->getFacility()),
                    makeUnitState(on->getUnit()),
                    makePatientState(on->getPatient())
            );

            lon->insertAsap(snext);
            dumpers->append(on);
        }

        // Remove the abx dose event.
        HistoryLink *ll = l;
        l = l->sNext();
        ll->remove();
        delete ll;
    }
}

// Protected
void LogNormalModel::skipLine(istream &is)
{
    char c;
    do
    {
        c = is.get();
    }
    while (c != '\n' && c != EOF);
}

void LogNormalModel::readInColParams(LogNormalICP *icp, istream &is)
{
    string sdump;
    double p;
    double up;

    for (int i=0; i<3; i++)
    {
        if (i != 1)
        {
            for (int j=0; j<icp->nParam2(i); j++)
            {
                is >> sdump >> p >> up;
                skipLine(is);
                icp->set(i,j,p,(up>0),p,up);
            }
        }
        else
        {
            if (icp->getNStates() == 2)
            {
                for (int j=0; j<icp->nParam2(i); j++)
                    icp->set(i,j,0,0,0,0);
            }

            if (icp->getNStates() == 3)
            {
                for (int j=0; j<icp->nParam2(i); j++)
                {
                    is >> sdump >> p >> up;
                    skipLine(is);
                    icp->set(i,j,p,(up>0),p,up);
                }
            }
        }
    }
}

void LogNormalModel::readInsituParams(InsituParams *isp, istream &is)
{
    string sdump;
    double p, q, r;
    double up, uq, ur;

    is >> sdump >> p >> up;
    skipLine(is);

    if (isp->getNStates() == 2)
    {
        q = 0;
        uq = 0;
    }
    if (isp->getNStates() == 3)
    {
        is >> sdump >> q >> uq;
        skipLine(is);
    }

    is >> sdump >> r >> ur;
    skipLine(is);

    p = p+q+r;
    r = r/p;
    q = q/p;
    p = 1-q-r;
    up = ur;

    isp->set(p,q,r);
    isp->setPriors(p*up,q*up,r*ur);
    isp->setUpdate(up,uq,ur);
}

void LogNormalModel::readTestParams(TestParams *stsp, istream &is)
{
    string sdump;
    double up, uq, ur;
    double p, q, r;

    is >> sdump >> p >> up;
    skipLine(is);

    if (stsp->getNStates() == 2)
    {
        q = 0;
        uq = 0;
    }
    if (stsp->getNStates() == 3)
    {
        is >> sdump >> q >> uq;
        skipLine(is);
    }

    is >> sdump >> r >> ur;
    skipLine(is);

    stsp->set(0,p,(up>0),p,up);
    stsp->set(1,q,(uq>0),q,uq);
    stsp->set(2,r,(ur>0),r,ur);
}

void LogNormalModel::readRandomTestParams(RandomTestParams *ctsp, istream &is)
{
    string sdump;
    double up, uq, ur;
    double p, q, r;

    is >> sdump >> p >> up;
    skipLine(is);

    if (ctsp->getNStates() == 2)
    {
        q = 0;
        uq = 0;
    }
    if (ctsp->getNStates() == 3)
    {
        is >> sdump >> q >> uq;
        skipLine(is);
    }

    is >> sdump >> r >> ur;
    skipLine(is);

    ctsp->set(0,p,(up>0),p,up);
    ctsp->set(1,q,(uq>0),q,uq);
    ctsp->set(2,r,(ur>0),r,ur);

    is >> sdump >> p >> up;
    skipLine(is);

    if (ctsp->getNStates() == 2)
    {
        q = 1;
        uq = 0;
    }
    if (ctsp->getNStates() == 3)
    {
        is >> sdump >> q >> uq;
        skipLine(is);
    }

    is >> sdump >> r >> ur;
    skipLine(is);

    ctsp->set(1,0,p,(up>0),p,up);
    ctsp->set(1,1,q,(uq>0),q,uq);
    ctsp->set(1,2,r,(ur>0),r,ur);
}

void LogNormalModel::readOutColParams(OutColParams *ocp, istream &is)
{
    string sdump;
    double up, uq, ur;
    double p, q, r;

    is >> sdump >> p >> up;
    skipLine(is);

    if (ocp->getNStates() == 2)
    {
        q = 0;
        uq = 0;
    }
    if (ocp->getNStates() == 3)
    {
        is >> sdump >> q >> uq;
        skipLine(is);
    }

    is >> sdump >> r >> ur;
    skipLine(is);

    ocp->set(0,p,(up>0),p,up);
    ocp->set(1,q,(uq>0),q,uq);
    ocp->set(2,r,(ur>0),r,ur);
    /*
     if (ocp->getNStates() == 2)
     {
     ocp->set(p,r);
     ocp->setPriors(p,up,r,ur);
     ocp->setUpdate(up,ur);
     }
     if (ocp->getNStates() == 3)
     {
     ocp->set(p,q,r);
     ocp->setPriors(p,up,q,uq,r,ur);
     ocp->setUpdate(up,uq,ur);
     }
     */
}

void LogNormalModel::readAbxParams(AbxParams *abxp, istream &is)
{
    string sdump;
    double up, uq, ur;
    double p, q, r;

    is >> sdump >> p >> up;
    skipLine(is);
    if (abxp->getNStates() == 2)
    {
        q = 0;
        uq = 0;
    }

    if (abxp->getNStates() == 3)
    {
        is >> sdump >> q >> uq;
        skipLine(is);
    }

    is >> sdump >>r >>ur;
    skipLine(is);

    abxp->set(0,p,(up>0),p,up);
    abxp->set(1,q,(uq>0),q,uq);
    abxp->set(2,r,(ur>0),r,ur);

}

//public
void LogNormalModel::write(ostream &os) const
{
    os << isp << "\t\t";
    os << survtsp << "\t\t";
    os << clintsp << "\t\t";
    os << ocp << "\t\t";
    os << icp << "\t\t";
    os << abxp << "\t\t";
}


} // namespace models
