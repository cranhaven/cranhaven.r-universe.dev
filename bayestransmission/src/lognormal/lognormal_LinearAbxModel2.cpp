#include "lognormal/lognormal.h"

namespace lognormal{

LinearAbxModel2::LinearAbxModel2(int nst, int nmetro, int fw, int ch) : LogNormalModel(nst,nmetro,fw,ch)
{
    // Constructor implementation for LinearAbxModel2
    // LinearAbxModel2 uses LinearAbxICP2 which has different transformation:
    // - Uses log transform for ALL parameters (not logit for mass/freq)
    // - Different acqRate formula (additive not multiplicative)
    InColParams *icp = getInColParams();
    delete icp;
    icp = new LinearAbxICP2(nst,nmetro);
    setInColParams(icp);
}

} // namespace lognormal
