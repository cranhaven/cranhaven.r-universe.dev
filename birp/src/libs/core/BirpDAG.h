#ifndef BIRP_BIRPDAG_H
#define BIRP_BIRPDAG_H

#include "BirpTypes.h"
#include "coretools/Types/TStringHash.h"
#include "stattools/Priors/TPriorDirichlet.h"
#include "stattools/Priors/TPriorExponential.h"
#include "stattools/Priors/TPriorFlat.h"
#include "stattools/Priors/TPriorNormal.h"
#include "stattools/Priors/TPriorUniform.h"

// gamma
using BoxOnGamma = stattools::prior::TFlatFixed<stattools::TParameterBase, TypeGamma, NumDimGamma>;
using SpecGamma  = stattools::ParamSpec<TypeGamma, stattools::Hash<coretools::toHash("gamma")>, BoxOnGamma,
                                        stattools::NumDim<NumDimGamma>>;
// alpha
using BoxOnAlpha = stattools::prior::TDirichletFixed<stattools::TParameterBase, TypeAlpha, NumDimAlpha>;
using SpecAlpha  = stattools::ParamSpec<TypeAlpha, stattools::Hash<coretools::toHash("alpha")>, BoxOnAlpha,
                                        stattools::NumDim<NumDimAlpha>, stattools::SumOne<0>>;

// beta
using BoxOnBeta = stattools::prior::TNormalFixed<stattools::TParameterBase, TypeBeta, NumDimBeta>;
using SpecBeta  = stattools::ParamSpec<TypeBeta, stattools::Hash<coretools::toHash("beta")>, BoxOnBeta,
                                       stattools::NumDim<NumDimBeta>>;

// muOrN
using BoxOnMuOrN = stattools::prior::TUniformFixed<stattools::TParameterBase, TypeMuOrN, NumDimMuOrN>;
using SpecMuOrN  = stattools::ParamSpec<TypeMuOrN, stattools::Hash<coretools::toHash("muOrN")>, BoxOnMuOrN,
                                        stattools::NumDim<NumDimMuOrN>, stattools::SumOne<0>>;

// b
using BoxOnB = stattools::prior::TExponentialFixed<stattools::TParameterBase, TypeB, NumDimB>;
using SpecB  = stattools::ParamSpec<TypeB, stattools::Hash<coretools::toHash("b")>, BoxOnB, stattools::NumDim<NumDimB>>;

// logPhi
using BoxOnLogPhi = stattools::prior::TUniformFixed<stattools::TParameterBase, TypeLogPhi, NumDimLogPhi>;
using SpecLogPhi  = stattools::ParamSpec<TypeLogPhi, stattools::Hash<coretools::toHash("logPhi")>, BoxOnLogPhi,
                                         stattools::NumDim<NumDimLogPhi>>;

// logSigma
using BoxOnLogSigma = stattools::prior::TUniformFixed<stattools::TParameterBase, TypeLogSigma, NumDimLogSigma>;
using SpecLogSigma  = stattools::ParamSpec<TypeLogSigma, stattools::Hash<coretools::toHash("logSigma")>, BoxOnLogSigma,
                                           stattools::NumDim<NumDimLogSigma>>;

// Birp prior
class TBirpPrior; // forward declaration
using BirpBox = TBirpPrior;

// Parameters
using TypeParamGamma    = stattools::TParameter<SpecGamma, BirpBox>;
using TypeParamAlpha    = stattools::TParameter<SpecAlpha, BirpBox>;
using TypeParamBeta     = stattools::TParameter<SpecBeta, BirpBox>;
using TypeParamMuOrN    = stattools::TParameter<SpecMuOrN, BirpBox>;
using TypeParamB        = stattools::TParameter<SpecB, BirpBox>;
using TypeParamLogPhi   = stattools::TParameter<SpecLogPhi, BirpBox>;
using TypeParamLogSigma = stattools::TParameter<SpecLogSigma, BirpBox>;

#endif // BIRP_BIRPDAG_H
