#ifndef ALUN_MODELING_FORWARD_SIMULATOR_H
#define ALUN_MODELING_FORWARD_SIMULATOR_H

#include "../infect/infect.h"
#include "UnitLinkedModel.h"
namespace models {

class ForwardSimulator : public Object, infect::EventCoding, infect::InfectionCoding
{
public:
	static void forwardSimulate(UnitLinkedModel *mod, infect::SystemHistory *hist, Random *rand);

protected:
	static infect::EpisodeHistory *getEpisodeHistory(Map *map, infect::HistoryLink *h);
	static void randImportState(UnitLinkedModel *mod, infect::HistoryLink *h, infect::EpisodeHistory *eh, Random *rand);
	static void randTestResult(UnitLinkedModel *mod, infect::HistoryLink *h, Random *rand);
	static double randTimeToEvent(UnitLinkedModel *mod, double atime, infect::HistoryLink *ph, infect::HistoryLink *uh, Random *rand);

};

} // namespace models

#endif // ALUN_MODELING_FORWARD_SIMULATOR_H
