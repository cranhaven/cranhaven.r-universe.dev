#ifndef TPROBABILITY_H_
#define TPROBABILITY_H_

#include "coretools/Containers/TView.h"
#include "coretools/Types/TSomeProbability.h"
#include "coretools/traits.h"

namespace coretools {

using Probability      = TSomeProbability<ProbabilityType::linear>;
using LogProbability   = TSomeProbability<ProbabilityType::log>;
using Log10Probability = TSomeProbability<ProbabilityType::log10>;
using PhredInt         = TSomeProbability<ProbabilityType::phred>;
using HPPhredInt       = TSomeProbability<ProbabilityType::hpPhred>;

constexpr char toChar(PhredInt ph) noexcept {
	constexpr char min = 33;
	constexpr char max = 126;
	constexpr PhredInt::value_type maxPh = max - min;
	return static_cast<char>(std::min(ph.get(), maxPh) + min);
}

constexpr PhredInt fromChar(char baseQuality) noexcept {
	constexpr char min = 33;
	constexpr char max = 126;
	return PhredInt(tags::NoCheck{}, std::clamp(baseQuality, min, max) - min);
}

template<ProbabilityType Type>
Probability P(const TSomeProbability<Type>& Other) noexcept {
	return Probability{Other};
}

constexpr Probability P(double Value) noexcept(!checkIntervals()) {
	return Probability{Value};
}

constexpr Probability operator""_P(long double Value) noexcept(!checkIntervals()) {
	return P(Value);
}

inline Probability P(std::string_view Value) {
	return Probability(Value);
}

template<ProbabilityType Type>
LogProbability logP(const TSomeProbability<Type>& Other) noexcept {
	return LogProbability{Other};
}

constexpr LogProbability logP(double Value) noexcept(!checkIntervals()) {
	return LogProbability{Value};
}

inline LogProbability logP(std::string_view Value) {
	return LogProbability(Value);
}

template<ProbabilityType Type>
Log10Probability log10P(const TSomeProbability<Type>& Other) noexcept {
	return Log10Probability{Other};
}

constexpr Log10Probability log10P(double Value) noexcept(!checkIntervals()) {
	return Log10Probability{Value};
}

inline Log10Probability log10P(std::string_view Value) {
	return Log10Probability(Value);
}

template<typename... Args>
constexpr double sum(Probability p1, Args... ps) {
	constexpr auto NArgs = sizeof...(Args);
	if constexpr (NArgs == 0) {
		return p1.get();
	} else {
		return p1.get() + sum(ps...);
	}
}

template<typename... Args>
constexpr Probability average(Probability p1, Args... ps) {
	constexpr auto NArgs = sizeof...(Args);
	if constexpr (NArgs == 0) {
		return p1;
	} else {
		return Probability(tags::NoCheck{}, sum(p1, ps...)/(NArgs + 1));
	}
}

inline Probability average(TConstView<Probability> ps) noexcept {
	double tot = 0.;
	for (auto p: ps) tot += p;
	return Probability{tags::NoCheck{}, tot/ps.size()};
}

} // namespace coretools

#endif
