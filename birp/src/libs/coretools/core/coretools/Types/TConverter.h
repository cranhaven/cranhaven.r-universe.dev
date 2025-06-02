#ifndef TCONVERTER_h_
#define TCONVERTER_h_

#include <cmath>
#include <cstdint>

#include "coretools/Math/mathConstants.h"
#include "PhredTables.h"

namespace coretools {

enum class ProbabilityType {linear, log, log10, phred, hpPhred};

template<ProbabilityType From> struct TConverter {};

template<> struct TConverter<ProbabilityType::linear> {
	using value_type = double;

	template<ProbabilityType From, typename T> static value_type from(T t) noexcept {
		if constexpr (From == ProbabilityType::linear) return t;
		else if constexpr (From == ProbabilityType::log) return std::exp(t);
		else if constexpr (From == ProbabilityType::log10) return std::pow(10, t);
		else if constexpr (From == ProbabilityType::phred) {
			//return std::pow(10, t/-10.);
			return phredtables::phred2linear(t);
		} else if constexpr (From == ProbabilityType::hpPhred) {
			//return std::pow(10, t/-1000.);
			return phredtables::hpphred2linear(t);
		}
		else static_assert(From == ProbabilityType::linear); // will not compile
	}
};

template<> struct TConverter<ProbabilityType::log> {
	using value_type = double;

	template<ProbabilityType From, typename T> static value_type from(T t) noexcept {
		if constexpr (From == ProbabilityType::linear) return std::log(t);
		else if constexpr (From == ProbabilityType::log) return t;
		else if constexpr (From == ProbabilityType::log10) return ln10*t;
		else if constexpr (From == ProbabilityType::phred) {
			return -0.1*ln10*t;
		} else if constexpr (From == ProbabilityType::hpPhred) {
			return -0.001*ln10*t;
		}
		else static_assert(From == ProbabilityType::linear); // will not compile
	}
};

template<> struct TConverter<ProbabilityType::log10> {
	using value_type = double;

	template<ProbabilityType From, typename T> value_type static from(T t) noexcept {
		if constexpr (From == ProbabilityType::linear) return std::log10(t);
		else if constexpr (From == ProbabilityType::log) return log10e*t;
		else if constexpr (From == ProbabilityType::log10) return t;
		else if constexpr (From == ProbabilityType::phred) {
			return -0.1*t;
		} else if constexpr (From == ProbabilityType::hpPhred) {
			return -0.001*t;
		}
		else static_assert(From == ProbabilityType::linear); // will not compile
	}
};

template<> struct TConverter<ProbabilityType::phred> {
	// phreded probability = -10 * log_10(probability)
	using value_type = uint8_t;

	template<ProbabilityType From, typename T> value_type static from(T t) noexcept {
		if constexpr (From == ProbabilityType::linear) {
			const double ph = -10 * std::log10(t);
			if (ph > std::numeric_limits<value_type>::max()) return std::numeric_limits<value_type>::max();
			return std::roundl(ph);
		} else if constexpr (From == ProbabilityType::log) {
			const double ph =  -10 * log10e * t;
			if (ph > std::numeric_limits<value_type>::max()) return std::numeric_limits<value_type>::max();
			return std::roundl(ph);
		} else if constexpr (From == ProbabilityType::log10) {
			const double ph =  -10 * t;
			if (ph > std::numeric_limits<value_type>::max()) return std::numeric_limits<value_type>::max();
			return std::roundl(ph);
		} else if constexpr (From == ProbabilityType::phred) {
			return t;
		} else if constexpr (From == ProbabilityType::hpPhred) {
			const double ph =  t/100;
			if (ph > std::numeric_limits<value_type>::max()) return std::numeric_limits<value_type>::max();
			return std::roundl(ph);
		} else
			static_assert(From == ProbabilityType::linear); // will not compile
	}
};

template<> struct TConverter<ProbabilityType::hpPhred> {
    // HighPrecisionPhredIntProbability = -1000 * log10(probability)
	using value_type = uint16_t;

	template<ProbabilityType From, typename T> value_type static from(T t) noexcept {
		if constexpr (From == ProbabilityType::linear) {
			const double ph = -1000 * std::log10(t);
			if (ph > std::numeric_limits<value_type>::max()) return std::numeric_limits<value_type>::max();
			return std::roundl(ph);
		} else if constexpr (From == ProbabilityType::log) {
			const double ph =  -1000 * log10e * t;
			if (ph > std::numeric_limits<value_type>::max()) return std::numeric_limits<value_type>::max();
			return std::roundl(ph);
		} else if constexpr (From == ProbabilityType::log10) {
			const double ph =  -1000 * t;
			if (ph > std::numeric_limits<value_type>::max()) return std::numeric_limits<value_type>::max();
			return std::roundl(ph);
		} else if constexpr (From == ProbabilityType::phred) {
			return t*100;
		} else if constexpr (From == ProbabilityType::hpPhred) {
			return t;
		} else
			static_assert(From == ProbabilityType::linear); // will not compile
	}
};
}


#endif
