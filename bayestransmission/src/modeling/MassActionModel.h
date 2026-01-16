#ifndef ALUN_MODELING_MASSACTIONMODEL_H
#define ALUN_MODELING_MASSACTIONMODEL_H

#include "BasicModel.h"
#include "MassActionICP.h"
#include "RandomTestParams.h"

namespace models {

class MassActionModel : public BasicModel
{
public:

	MassActionModel(int nst, int nmetro, int isdens, int clin, int fw = 0, int ch = 0);
	~MassActionModel();
};

} // namespace models
#endif // ALUN_MODELING_MASSACTIONMODEL_H
