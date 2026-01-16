#include "infect/infect.h"
#include <exception>
namespace infect {

Sampler::Sampler(SystemHistory *h, Model *m, Random *r)
    : hist(h), model(m), rand(r)
{
    initializeEpisodes();
}


void Sampler::sampleModel()
{
    sampleModel(0);
}

void Sampler::sampleModel(int max)
{
    model->update(hist,rand,max);
}

void Sampler::sampleEpisodes()
{
    sampleEpisodes(0);
}


void Sampler::sampleEpisodes(int max)
{
    model->sampleEpisodes(hist,max,rand);
}

void Sampler::initializeEpisodes()
{
    Map *pos = hist->positives();
    for (Map *e = hist->getEpisodes(); e->hasNext();)
    {
        EpisodeHistory *eh = (EpisodeHistory *)e->nextValue();
        Patient *ppp = eh->admissionLink()->getEvent()->getPatient();
        model->initEpisodeHistory(eh, pos->got(ppp));
    }
    if (model->isCheating())
    {
        for (Map *e = hist->getEpisodes(); e->hasNext();)
        {
            EpisodeHistory *eh = (EpisodeHistory *)e->nextValue();
            eh->unapply();
        }
        for (HistoryLink *l = hist->getSystemHead(); l != 0; l = l->sNext())
            l->setCopyApply();
        for (Map *e = hist->getEpisodes(); e->hasNext();)
        {
            EpisodeHistory *eh = (EpisodeHistory *)e->nextValue();
            eh->apply();
        }
    }
    delete pos;
}

} // namespace infect
