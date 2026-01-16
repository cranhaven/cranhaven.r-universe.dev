#ifndef ALUN_LOGNORMAL_LINEARABXMODEL2_H
#define ALUN_LOGNORMAL_LINEARABXMODEL2_H

#include "LogNormalModel.h"
#include "LinearAbxICP2.h"

class LinearAbxModel2 : public LogNormalModel
{
public:
    LinearAbxModel2(int nst, int nmetro, int fw, int ch);
};

#endif // ALUN_LOGNORMAL_LINEARABXMODEL2_H
