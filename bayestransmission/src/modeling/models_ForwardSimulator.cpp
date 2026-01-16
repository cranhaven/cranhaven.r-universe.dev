#include "modeling/modeling.h"

namespace models {

void ForwardSimulator::forwardSimulate(UnitLinkedModel *mod, infect::SystemHistory *hist, Random *rand)
{
    if (!mod->isForwardEnabled())
    {
        Rcpp::warning("Model is not forward enabled. Cannot simulate.");
        return;
    }

    Map *map = new Map();

    for (Map *x = hist->getAdmissions(); x->hasNext(); )
    {
        infect::Episode *e = (infect::Episode *) x->next();
        infect::HistoryLink *a = (infect::HistoryLink *) x->get(e);
        map->put(a,hist->getEpisodes()->get(e));
    }

    for (infect::HistoryLink *l = hist->getSystemHead(); l != 0; )
    {
        switch(l->getEvent()->getType())
        {
        case insitu:
        case insitu0:
        case insitu1:
        case insitu2:
        case admission:
        case admission0:
        case admission1:
        case admission2:
            randImportState(mod,l,getEpisodeHistory(map,l),rand);
            break;

        case postest:
        case negtest:
        case possurvtest:
        case negsurvtest:
        case posclintest:
        case negclintest:
            randTestResult(mod,l,rand);
            break;

        default:
            break;
        }

        if (l->sNext() == 0)
            break;

        infect::HistoryLink *phl = 0;
        double time = l->sNext()->getEvent()->getTime();

        for (Map *mp = ((infect::SetLocationState *)l->getSState())->getPatients(); mp->hasNext(); )
        {
            infect::Patient *p = (infect::Patient *)mp->next();
            infect::HistoryLink *pl = l;
            for ( ; pl != 0; pl = pl->sPrev() )
                if (pl->getEvent()->getPatient() == p)
                    break;

            if (pl == 0)
                Rcpp::stop("SHOULDN'T GET HERE IN SIMULATE");

            infect::Unit *u = (infect::Unit *) pl->getEvent()->getUnit();
            infect::HistoryLink *ul = l;
            for ( ; ul != 0; ul = ul->sPrev() )
                if (ul->getEvent()->getUnit() == u)
                    break;

            double nexttime = randTimeToEvent(mod,l->getEvent()->getTime(),pl,ul,rand);

            if (nexttime < time)
            {
                time = nexttime;
                phl = pl;
            }
        }

        if (time < l->sNext()->getEvent()->getTime())
        {
            EventCode type = nullevent;

            switch(phl->getPState()->infectionStatus())
            {
            case uncolonized:
                type = acquisition;
                break;

            case latent:
                type = progression;
                break;

            case colonized:
                type = clearance;
                break;

            default:
                break;
            }

            infect::HistoryLink *hl = mod->makeHistLink
            (
                    phl->getEvent()->getFacility(),
                    phl->getEvent()->getUnit(),
                    phl->getEvent()->getPatient(),
                    time,
                    type,
                    1
            );

            getEpisodeHistory(map,phl)->appendLink(hl);
            l = hl;
        }
        else
        {
            l = l->sNext();
        }
    }

    delete map;
}

infect::EpisodeHistory* ForwardSimulator::getEpisodeHistory(Map *map, infect::HistoryLink *h)
{
    for (infect::HistoryLink *l = h; l != 0; l = l->pPrev())
    {
        switch(l->getEvent()->getType())
        {
        case insitu:
        case insitu0:
        case insitu1:
        case insitu2:
        case admission:
        case admission0:
        case admission1:
        case admission2:
            return (infect::EpisodeHistory *) map->get(l);

        default:
            break;
        }
    }
    return 0;
}

void ForwardSimulator::randImportState(UnitLinkedModel *mod, infect::HistoryLink *h, infect::EpisodeHistory *eh, Random *rand)
{
    infect::Facility *f = h->getEvent()->getFacility();
    infect::Unit *u = h->getEvent()->getUnit();
    infect::Patient *p = h->getEvent()->getPatient();
    double atime = h->getEvent()->getTime();

    InfectionStatus prev = nullstatus;
    double time = 0;

    if (h->pPrev() != 0)
    {
        prev = h->pPrev()->getPState()->infectionStatus();
        time = atime - h->pPrev()->getEvent()->getTime();
    }

    if (h->getEvent()->isInsitu())
        h->getEvent()->setType(insitu0);
    if (h->getEvent()->isAdmission())
        h->getEvent()->setType(admission0);

    double U = rand->runif();

    if (mod->getNStates() == 2)
    {
        if (U < mod->getOutColParams()->transitionProb(prev,colonized,time))
        {
            eh->appendLink(mod->makeHistLink(f,u,p,atime,acquisition,0));
            if (h->getEvent()->isInsitu())
                h->getEvent()->setType(insitu2);
            if (h->getEvent()->isAdmission())
                h->getEvent()->setType(admission2);
        }
    }

    if (mod->getNStates() == 3)
    {
        if (U < mod->getOutColParams()->transitionProb(prev,latent,time))
        {
            eh->appendLink(mod->makeHistLink(f,u,p,atime,acquisition,0));
            if (h->getEvent()->isInsitu())
                h->getEvent()->setType(insitu1);
            if (h->getEvent()->isAdmission())
                h->getEvent()->setType(admission1);
        }
        else if (U < mod->getOutColParams()->transitionProb(prev,latent,time) + mod->getOutColParams()->transitionProb(prev,colonized,time))
        {
            eh->appendLink(mod->makeHistLink(f,u,p,atime,acquisition,0));
            eh->appendLink(mod->makeHistLink(f,u,p,atime,progression,0));
            if (h->getEvent()->isInsitu())
                h->getEvent()->setType(insitu2);
            if (h->getEvent()->isAdmission())
                h->getEvent()->setType(admission2);
        }
    }
}

void ForwardSimulator::randTestResult(UnitLinkedModel *mod, infect::HistoryLink *h, Random *rand)
{
    TestParams *tpar = 0;
    EventCode tres = h->getEvent()->getType();

    switch(tres)
    {
    case postest:
    case negtest:
    case possurvtest:
    case negsurvtest:
        tres = negsurvtest;
        tpar = mod->getSurveillanceTestParams();
        break;

    case posclintest:
    case negclintest:
        tres = negclintest;
        tpar = mod->getClinicalTestParams();
        break;

    default:
        break;
    }

    //if (rand->runif() < tpar->eventProb(h->getPState()->infectionStatus(),postest))
    if (rand->runif() < tpar->eventProb(h->getPState()->infectionStatus(),h->getPState()->onAbx(),postest))
    {
        if (tres == negsurvtest)
            tres = possurvtest;
        if (tres == negclintest)
            tres = posclintest;
    }

    h->getEvent()->setType(tres);
}

double ForwardSimulator::randTimeToEvent(UnitLinkedModel *mod, double atime, infect::HistoryLink *ph, infect::HistoryLink *uh, Random *rand)
{
    double time = 0;

    switch(ph->getPState()->infectionStatus())
    {
    case uncolonized:
        time = atime + rand->rexp(mod->getInColParams()->eventRate(atime,acquisition,ph->getPState(),uh->getUState()));
        break;

    case latent:
        time = atime + rand->rexp(mod->getInColParams()->eventRate(atime,progression,ph->getPState(),uh->getUState()));
        break;

    case colonized:
        time = atime + rand->rexp(mod->getInColParams()->eventRate(atime,clearance,ph->getPState(),uh->getUState()));
        break;

    default:
        Rcpp::stop("CAN'T GET HERE");
    return 0;
    break;
    }

    if (time < atime)
    {
        Rcpp::stop("PROBLEM WITH REXP");
    }

    return time ;
}

} // namespace models
