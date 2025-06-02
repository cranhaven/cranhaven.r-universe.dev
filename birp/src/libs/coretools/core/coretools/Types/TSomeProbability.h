#ifndef TSOMEPROBABILITY_H_
#define TSOMEPROBABILITY_H_

#include "coretools/Strings/fromString.h"
#include "coretools/Types/intervals.h"
#include "coretools/traits.h"
#include "coretools/Main/TError.h"
#include "coretools/Types/TConverter.h"

namespace coretools {

template<ProbabilityType Type>
class TSomeProbability {
public:
	using Converter  = TConverter<Type>;
	using value_type = typename Converter::value_type;

private:
	value_type _value = min();

	constexpr bool _isValid() const noexcept {
		if constexpr (isLinear()) {
			return (_value >= min() && _value <= max());
		} else {
			return _value <= max();
		}
	}

	constexpr void _check() const noexcept(!checkIntervals()) {
		if constexpr (checkIntervals()) {
			if (!_isValid()) {
				DEVERROR("TLogProbability cannot have value of ", _value, "!");
			}
		}
	}

public:
	constexpr TSomeProbability() = default;
	constexpr explicit TSomeProbability(tags::NoCheck, value_type Value) : _value(Value) {}
	constexpr explicit TSomeProbability(value_type Value) : _value(Value) {_check();}

    template<ProbabilityType From>
	explicit TSomeProbability(const TSomeProbability<From> &Other) : _value(Converter::template from<From>(Other.get())){}

    explicit TSomeProbability(std::string_view Value)
        : _value(str::fromString<value_type, true>(Value)) {
      if (!_isValid())
        UERROR(Value, " is outside of TLogProbability range!");
	}

	// no operator= for, always explicit! Use P(...) functions
	// Do not define these:
	//TSomeProbability &operator=(value_type) = delete;
	//template <ProbabilityType OtherType>
	//TSomeProbability &operator=(const TSomeProbability<OtherType> &) = delete;

	constexpr value_type get() const noexcept { return _value; }
	constexpr /*implicit*/ operator value_type() const noexcept {return get();}
	explicit operator std::string() const { return str::toString<value_type>(_value); };

	constexpr TSomeProbability& operator+=(TSomeProbability rhs) noexcept {
		static_assert(!isLinear());
		_value += rhs.get();
		return *this;
	}

	constexpr TSomeProbability& operator*=(TSomeProbability rhs) noexcept {
		static_assert(isLinear());
		_value *= rhs.get();
		return *this;
	}

	constexpr TSomeProbability& scale(value_type Factor) noexcept(!checkIntervals()) {
		if constexpr (isLinear()) {
			_value /= Factor;
		} else {
			_value -= Factor;
		}
		_check();
		return *this;
	}

	constexpr value_type oddsRatio() const noexcept {
		static_assert(isLinear());
		return _value / (1. - _value);
	}

	constexpr TSomeProbability complement() const noexcept {
		static_assert(isLinear());
		return TSomeProbability{tags::NoCheck{}, 1. - get()};
	}

	constexpr bool moreProbable(TSomeProbability other) const noexcept {
		if constexpr (isPhreded()) return _value < other.get();
		else return _value > other.get();
	}

	static constexpr bool isLinear() noexcept {
		return Type == ProbabilityType::linear;
	}

	static constexpr bool isPhreded() noexcept {
		return Type == ProbabilityType::phred || Type == ProbabilityType::hpPhred;
	}

	static constexpr value_type min() noexcept {
		if constexpr (isLinear() || isPhreded()) return value_type{};
		else return std::numeric_limits<value_type>::lowest();
	}

	static constexpr value_type max() noexcept {
		if constexpr (isLinear()) return 1.;
		else if constexpr (isPhreded()) return std::numeric_limits<value_type>::max();
		else return 0.;
	}

	static constexpr TSomeProbability lowest() noexcept {
		if constexpr (isPhreded()) return TSomeProbability{tags::NoCheck{}, max()};
		else return TSomeProbability{tags::NoCheck{}, min()};
	}

	static constexpr TSomeProbability highest() noexcept {
		if constexpr (isPhreded()) return TSomeProbability{tags::NoCheck{}, min()};
		else return TSomeProbability{tags::NoCheck{}, max()};
	}

	static constexpr auto interval() noexcept {
		if constexpr (isLinear()) return intervals::ZeroOneClosed<value_type>();
		else if constexpr (isPhreded()) return intervals::Positive<value_type>();
		else return intervals::Negative<value_type>();
	}
};

// Arithmetics: allow only with same type
// +
template<ProbabilityType Type>
constexpr auto operator+(TSomeProbability<Type> lhs, TSomeProbability<Type> rhs) noexcept {
	using ThisProbability = TSomeProbability<Type>;
	using value_type      = typename ThisProbability::value_type;
	if constexpr (ThisProbability::isLinear()) {
		return value_type(lhs.get() + rhs.get());
	} else {
		return ThisProbability{tags::NoCheck{}, value_type(lhs.get() + rhs.get())};
	}
}
template<ProbabilityType Lhs, ProbabilityType Rhs>
auto operator+(TSomeProbability<Lhs>, TSomeProbability<Rhs>) = delete;

// -
template<ProbabilityType Type>
constexpr auto operator-(TSomeProbability<Type> lhs, TSomeProbability<Type> rhs) noexcept {
	using ThisProbability = TSomeProbability<Type>;
	using value_type      = typename ThisProbability::value_type;
	return value_type(lhs.get() - rhs.get());
}
template<ProbabilityType Lhs, ProbabilityType Rhs>
auto operator-(TSomeProbability<Lhs>, TSomeProbability<Rhs>) = delete;

// *
template<ProbabilityType Type>
constexpr auto operator*(TSomeProbability<Type> lhs, TSomeProbability<Type> rhs) noexcept {
	using ThisProbability = TSomeProbability<Type>;
	using value_type      = typename ThisProbability::value_type;
	if constexpr (ThisProbability::isLinear()) {
		return ThisProbability{tags::NoCheck{}, lhs.get() * rhs.get()};
	} else {
		return value_type(lhs.get() * rhs.get());
	}
}
template<ProbabilityType Lhs, ProbabilityType Rhs>
auto operator*(TSomeProbability<Lhs>, TSomeProbability<Rhs>) = delete;


// /
template<ProbabilityType Type>
constexpr auto operator/(TSomeProbability<Type> lhs, TSomeProbability<Type> rhs) noexcept {
	using ThisProbability = TSomeProbability<Type>;
	using value_type      = typename ThisProbability::value_type;
	return value_type(lhs.get() / rhs.get());
}
template<ProbabilityType Lhs, ProbabilityType Rhs>
auto operator/(TSomeProbability<Lhs>, TSomeProbability<Rhs>) = delete;

// == 
template<ProbabilityType Type>
constexpr bool operator==(TSomeProbability<Type> lhs, TSomeProbability<Type> rhs) noexcept {
	return lhs.get() == rhs.get();
}
template<ProbabilityType Lhs, ProbabilityType Rhs>
bool operator==(TSomeProbability<Lhs>, TSomeProbability<Rhs>) = delete;

}

#endif
