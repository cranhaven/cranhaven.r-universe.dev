#include "modeling/modeling.h"

namespace models {

UnitLinkedModel::UnitLinkedModel(int ns, int fw, int ch)
{
    setAbxLife(1.5);
    setAbxDelay(0.0);
    nstates = ns;
    forwardEnabled = fw;
    cheating = ch;

    isp = 0;
    ocp = 0;
    survtsp = 0;
    clintsp = 0;
    icp = 0;
    abxp = 0;
}

double UnitLinkedModel::logLikelihood(infect::SystemHistory *hist)
{
    // cout << "UnitLinkedModel::logLikelihood(infect::SystemHistory *hist=" << hist << ")\n";
    double xtot = 0;
    for (Map *h = hist->getUnitHeads(); h->hasNext(); )
    {
        // cout << "Map *h = " << h << std::endl;
        double utot = 0;
        for (infect::HistoryLink *l = (infect::HistoryLink *) h->nextValue(); l != 0; l=l->uNext()){
            auto ll = logLikelihood(l);
            // cout << "infect::HistoryLink *l = " << l
            //      << "; logLikelihood(l) = " << ll << std::endl;
            utot += ll;
        }
        xtot += utot;
    }
    return xtot;
}

infect::HistoryLink* UnitLinkedModel::makeHistLink(infect::Facility *f, infect::Unit *u, infect::Patient *p, double time, EventCode type, int linked)
{
    return new infect::HistoryLink
    (
            new infect::Event(f,u,time,p,type),
            makeSystemState(),
            makeFacilityState(f),
            makeUnitState(u),
            makePatientState(p),
            linked
    );
}

string UnitLinkedModel::header() const
{
    stringstream os;
    os << isp->header();
    os << "\t" << survtsp->header();
    if (clintsp && clintsp != survtsp)
        os << "\t" << clintsp->header();
    os << "\t" << ocp->header();
    os << "\t" << icp->header();
    if (abxp)
        os << "\t" << abxp->header();
    return os.str();
}

void UnitLinkedModel::write (ostream &os) const
{
    os << isp;
    os << "\t" << survtsp;
    if (clintsp && clintsp != survtsp)
        os << "\t" << clintsp;
    os << "\t" << ocp;
    os << "\t" << icp;
    if (abxp != 0)
        os << "\t" << abxp;
}

int UnitLinkedModel::needEventType(EventCode e)
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
        //case abxon:
        //case abxoff:
        return 1;
    case acquisition:
    case progression:
    case clearance:
        return cheating;
    default:
        return 0;
    }
}

infect::LocationState* UnitLinkedModel::makeSystemState()
{
    return ( forwardEnabled ? new infect::SetLocationState(0,nstates) : 0 );
}

infect::LocationState* UnitLinkedModel::makeFacilityState(infect::Facility *f)
{
    return 0;
}

infect::LocationState* UnitLinkedModel::makeUnitState(infect::Unit *u)
{
    return u == 0 ? 0 : new infect::CountLocationState(u,nstates);
}

infect::PatientState* UnitLinkedModel::makePatientState(infect::Patient *p)
{
    return p == 0 ? 0 : new infect::PatientState(p,nstates);
}

infect::EpisodeHistory* UnitLinkedModel::makeEpisodeHistory(infect::HistoryLink *a, infect::HistoryLink *d)
{
    if (forwardEnabled)
        return new infect::SystemEpisodeHistory(a,d);
    else
        return new infect::UnitEpisodeHistory(a,d);
}

void UnitLinkedModel::countUnitStats(infect::HistoryLink *l)
{
    infect::HistoryLink *prev = l;

    for (infect::HistoryLink *h = l->uNext() ; h != 0; h = h->uNext())
    {
        icp->countGap(prev,h);
        survtsp->countGap(prev,h);
        if (clintsp && clintsp != survtsp)
            clintsp->countGap(prev,h);
        if (abxp != 0)
            abxp->countGap(prev,h);

        if (h->isHidden())
            break;

        switch(h->getEvent()->getType())
        {
        case insitu:
        case insitu0:
        case insitu1:
        case insitu2:
            isp->count(h);
            break;

        case admission:
        case admission0:
        case admission1:
        case admission2:
            ocp->count(h);
            break;

        case negsurvtest:
        case possurvtest:
            survtsp->count(h);
            break;

        case negclintest:
        case posclintest:
            if (clintsp)
                clintsp->count(h);
            break;

        case acquisition:
        case progression:
        case clearance:
            icp->count(h);
            break;

        case abxon:
            if (abxp != 0)
                abxp->count(h);
            break;

        case abxdose:
        case abxoff:
        case discharge:
        case marker:
        case start:
        case stop:
        case isolon:
        case isoloff:
            break;

        default:
            throw std::runtime_error("Event not handled.");
        // cerr << "Event not handled " << h->getEvent() << "\n";
        break;
        }

        prev = h;
    }
}

double UnitLinkedModel::logLikelihood(infect::EpisodeHistory *h)
{
    // cout << "UnitLinkedModel::logLikelihood(infect::EpisodeHistory *h=" << h << ")\n";
    return logLikelihood(h,0);
}

double UnitLinkedModel::logLikelihood(infect::EpisodeHistory *h, int opt)
{
    // cout << "UnitLinkedModel::logLikelihood(infect::EpisodeHistory *h=" << h << ", int opt=" << opt << ")\n";
    double x = 0;
    for (infect::HistoryLink *l = h->admissionLink(); ; l = l->uNext())
    {
        x += logLikelihood(l);
        if (l == h->dischargeLink())
        {
            if (l->pNext() != 0)
            {
                l = l->pNext();
                x += logLikelihood(l,0);
            }
            break;
        }
    }
    return x;
}

double UnitLinkedModel::logLikelihood(infect::Patient *pat, infect::HistoryLink *h)
{
    return logLikelihood(pat,h,0);
}

double UnitLinkedModel::logLikelihood(infect::Patient *pat, infect::HistoryLink *h, int opt)
{
    // cout << "UnitLinkedModel::logLikelihood(infect::Patient *pat=" << pat << ", infect::HistoryLink *h=" << h << ", int opt=" << opt << ")\n";
    double x = 0;

    for (infect::HistoryLink *l = h; l != 0; )
    {
        infect::Event *e = l->getEvent();
        if (e->getPatient() == pat && (e->isAdmission() || e->isInsitu()))
            x += logLikelihood(l,0);
        else
            x += logLikelihood(l,1);

        if (e->getPatient() == pat && e->getType() == discharge)
            l = l->pNext();
        else
            l = l->uNext();
    }

    return x;
}

double UnitLinkedModel::logLikelihood(infect::HistoryLink *h)
{
    // cout << "UnitLinkedModel::logLikelihood(infect::HistoryLink *h=" << h << ")\n";
    return logLikelihood(h,1);
}

double UnitLinkedModel::logLikelihood(infect::HistoryLink *h, int dogap)
{
    // cout << "UnitLinkedModel::logLikelihood(infect::HistoryLink *h=" << h << ", int dogap=" << dogap << ")\n";
    switch(h->getEvent()->getType())
    {
    case start:
    case stop:
        return 0;
    default:
        break;
    }

    infect::HistoryLink *prev = h->uPrev();
    double x = 0;

    if (dogap)
    {
        x += icp->logProbGap(prev,h);
        // cout << x << "    ";
        x += survtsp->logProbGap(prev,h);
        // cout << x << "    ";
        if (clintsp && clintsp != survtsp){
            x += clintsp->logProbGap(prev,h);
        }
        // cout << x << "    ";
        if (abxp != 0)
            x += abxp->logProbGap(prev,h);
        // cout << x << "    ";
    }

    if (h->isHidden())
        return x;
    switch(h->getEvent()->getType())
    {
    case insitu:
    case insitu0:
    case insitu1:
    case insitu2:
        // cout << "  insitu  ";
        x += isp->logProb(h);
        break;

    case admission:
    case admission0:
    case admission1:
    case admission2:
        // cout << "  admission  ";
        x += ocp->logProb(h);
        break;

    case acquisition:
    case progression:
    case clearance:
        // cout << "  transmission  ";
        x += icp->logProb(h);
        break;

    case negsurvtest:
    case possurvtest:
        // cout << "  survtest  ";
        x += survtsp->logProb(h);
        break;

    case negclintest:
    case posclintest:
        // cout << "  clintest  ";
        if (clintsp)
            x += clintsp->logProb(h);
        break;

    case abxon:
        // cout << "  abxon  ";
        if (abxp != 0)
            x += abxp->logProb(h);
        break;

    case abxdose:
    case discharge:
    case abxoff:
    case marker:
    case start:
    case stop:
    case isolon:
    case isoloff:
        // cout << "  other  ";
        break;

    default:
        throw std::runtime_error("Event not handled.");
    break;
    }

    // cout << x << std::endl;
    return x;
}

void UnitLinkedModel::update(infect::SystemHistory *hist, Random *r)
{
    update(hist,r,0);
}

void UnitLinkedModel::update(infect::SystemHistory *hist, Random *r, int max)
{
    isp->initCounts();
    survtsp->initCounts();
    if (clintsp && clintsp != survtsp)
        clintsp->initCounts();
    icp->initCounts();
    ocp->initCounts();
    if (abxp != 0)
        abxp->initCounts();

    for (Map *h = hist->getUnitHeads(); h->hasNext(); )
        countUnitStats((infect::HistoryLink *)h->nextValue());

    isp->update(r,max);
    icp->update(r,max);
    survtsp->update(r,max);
    if (clintsp && clintsp != survtsp)
        clintsp->update(r,max);
    ocp->update(r,max);
    if (abxp != 0)
        abxp->update(r,max);
}

std::vector<double> UnitLinkedModel::getHistoryLinkLogLikelihoods(infect::SystemHistory *hist)
{
    std::vector<double> lls;
    
    for (Map *h = hist->getUnitHeads(); h->hasNext(); )
    {
        for (infect::HistoryLink *l = (infect::HistoryLink *) h->nextValue(); l != 0; l = l->uNext())
        {
            double ll = logLikelihood(l);
            lls.push_back(ll);
        }
    }
    
    return lls;
}

} // namespace models
