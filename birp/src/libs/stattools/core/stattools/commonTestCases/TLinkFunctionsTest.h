//
// Created by madleina on 20.12.23.
//

#ifndef STATTOOLS_TLINKFUNCTIONSTEST_H
#define STATTOOLS_TLINKFUNCTIONSTEST_H

#include "coretools/Types/TStringHash.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TPriorUniform.h"

namespace stattools {

template<typename TypeBelow, typename TypeParam, template<typename, typename, size_t, typename> class Link>
struct TLinkFunctionsTest {
	constexpr static size_t NumDimBelow = 1;
	constexpr static size_t NumDimParam = 1;

	using BoxOnParam = prior::TUniformFixed<TParameterBase, TypeParam, NumDimParam>;
	using SpecParam  = ParamSpec<TypeParam, Hash<coretools::toHash("param")>, BoxOnParam>;
	using BoxOnBelow = Link<TParameterBase, TypeBelow, NumDimBelow, SpecParam>;
	using SpecBelow  = ParamSpec<TypeBelow, Hash<coretools::toHash("below")>, BoxOnBelow>;

	BoxOnParam boxOnParam;
	TParameter<SpecParam, BoxOnBelow> param;

	BoxOnBelow boxOnBelow;

	TLinkFunctionsTest(std::string_view Filename, std::string_view ParamName)
	    : param(ParamName, &boxOnParam, {Filename}), boxOnBelow(&param) {}

	TLinkFunctionsTest(std::string_view Filename, std::string_view ParamName,
	                   std::function<double(double)> FunctionDown, std::function<double(double)> FunctionUp,
	                   std::string_view Name)
	    : param(ParamName, &boxOnParam, {Filename}), boxOnBelow(&param, FunctionDown, FunctionUp, Name) {}
};

} // namespace stattools

#endif // STATTOOLS_TLINKFUNCTIONSTEST_H
