#include "lognormal/lognormal.h"

namespace lognormal{

LinearAbxModel::LinearAbxModel(int nst, int nmetro, int fw, int ch) : LogNormalModel(nst,nmetro,fw,ch)
{
    InColParams *icp = getInColParams();
    delete icp;
    icp = new LinearAbxICP(nst,nmetro);
    setInColParams(icp);
}

} // namespace lognormal
