#include "modeling/modeling.h"

namespace models {

MassActionModel::MassActionModel(int nst, int nmetro, int isdens, int clin, int fw, int ch) : BasicModel(nst,fw,ch)
{
    isp = new InsituParams(nstates);
    ocp = new OutColParams(nstates,nmetro);
    icp = new MassActionICP(nstates,isdens);
    survtsp = new TestParams(nstates);
    clintsp = ( clin ? new RandomTestParams(nstates) : survtsp ) ;
}

MassActionModel::~MassActionModel()
{
    delete ocp;
    delete isp;
    if (clintsp != 0 && clintsp != survtsp)
        delete clintsp;
    delete survtsp;
    delete icp;
}
} // namespace models
