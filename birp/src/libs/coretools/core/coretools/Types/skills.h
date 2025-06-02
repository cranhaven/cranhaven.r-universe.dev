
#ifndef SKILLS_H_
#define SKILLS_H_

#include <limits>
#include <type_traits>

#include "coretools/traits.h"

namespace coretools::skills {

/**
 *
 * @tparam T Corresponds to TypeWithInterval
 * @tparam crtpType Corresponds to the Skill type, e.g. AddableCheck
 */
template<typename T, template<typename> class crtpType> struct SkillsCRTP {
	constexpr T &underlying() { return static_cast<T &>(*this); };
	constexpr T const &underlying() const { return static_cast<T const &>(*this); };
};

//--------------------------------
// Addition
//--------------------------------

/**
 * Provides + operators with interval checking
 * @tparam T Corresponds to TypeWithInterval
 */
template<typename T> struct AddableCheck : SkillsCRTP<T, AddableCheck> {
public:
	[[nodiscard]] constexpr T operator+(T const &other) const {
		if constexpr (std::is_unsigned_v<typename T::value_type>) {
			check_u_int(this->underlying().get(), other.get());
		}
		return T(this->underlying().get() + other.get());
	};

	constexpr T &operator+=(T const &other) {
		if constexpr (std::is_unsigned_v<typename T::value_type>) {
			check_u_int(this->underlying().get(), other.get());
		}
		this->underlying() = this->underlying().get() + other.get();
		return this->underlying();
	};

	[[nodiscard]] constexpr T operator+() const {
		// no need for special checks if unsigned int
		return T(+this->underlying().get());
	}

	template<typename ValueType> constexpr static void check_u_int(ValueType const &First, ValueType const &Second) {
		// additional check for uint types: check before doing multiplication, as afterwards it might be too late
		constexpr auto nmMax = std::numeric_limits<ValueType>::max();
		if (nmMax - First < Second) {
			DEVERROR("Addition of unsigned integers ", First, " + ", Second,
			         " would result in a value larger than the numeric maximum of that type (", nmMax, ")!");
		}
	}
};

/**
 * Provides + operators without interval checking
 * @tparam T Corresponds to TypeWithInterval
 */
template<typename T> struct AddableNoCheck : SkillsCRTP<T, AddableNoCheck> {
	[[nodiscard]] constexpr T operator+(T const &other) const {
		return T(tags::NoCheck{}, this->underlying().get() + other.get());
	};

	constexpr T &operator+=(T const &other) {
		this->underlying() = operator+(other);
		return this->underlying();
	};

	[[nodiscard]] constexpr T operator+() const { return T(tags::NoCheck{}, +this->underlying().get()); }
};

//--------------------------------
// Subtraction
//--------------------------------

/**
 * Provides - operators with interval checking
 * @tparam T Corresponds to TypeWithInterval
 */
template<typename T> struct SubtractableCheck : SkillsCRTP<T, SubtractableCheck> {
public:
	[[nodiscard]] constexpr T operator-(T const &other) const {
		if constexpr (std::is_unsigned_v<typename T::value_type>) {
			check_u_int(this->underlying().get(), other.get());
		}
		return T(this->underlying().get() - other.get());
	};

	constexpr T &operator-=(T const &other) {
		if constexpr (std::is_unsigned_v<typename T::value_type>) {
			check_u_int(this->underlying().get(), other.get());
		}
		this->underlying() = this->underlying().get() - other.get();
		return this->underlying();
	};

	[[nodiscard]] constexpr auto operator-() const {
		// returns the value_type, otherwise it doesn't decay if e.g. x - a*b where a and b are positive
		return -this->underlying().get();
	}

	template<typename ValueType> constexpr static void check_u_int(ValueType const &First, ValueType const &Second) {
		// additional check for uint types: check before doing subtraction, as afterwards it might be too late
		if (First < Second) {
			DEVERROR("Subtraction of unsigned integers ", First, " - ", Second, " would result in a negative value!");
		}
	}
};

/**
 * Provides - operators without interval checking
 * @tparam T Corresponds to TypeWithInterval
 */
template<typename T> struct SubtractableNoCheck : SkillsCRTP<T, SubtractableNoCheck> {
	[[nodiscard]] constexpr T operator-(T const &other) const {
		return T(tags::NoCheck{}, this->underlying().get() - other.get());
	};

	constexpr T &operator-=(T const &other) {
		this->underlying() = operator-(other);
		return this->underlying();
	};

	[[nodiscard]] constexpr auto operator-() const {
		// returns the value_type, otherwise it doesn't decay if e.g. x - a*b where a and b are positive
		return -this->underlying().get();
	}
};

//--------------------------------
// Multiplication
//--------------------------------

/**
 * Provides * operators with interval checking
 * @tparam T Corresponds to TypeWithInterval
 */
template<typename T> struct MultiplicableCheck : SkillsCRTP<T, MultiplicableCheck> {
public:
	[[nodiscard]] constexpr T operator*(T const &other) const {
		if constexpr (std::is_unsigned_v<typename T::value_type>) {
			check_u_int(this->underlying().get(), other.get());
		}
		return T(this->underlying().get() * other.get());
	};

	constexpr T &operator*=(T const &other) {
		if constexpr (std::is_unsigned_v<typename T::value_type>) {
			check_u_int(this->underlying().get(), other.get());
		}
		this->underlying() = this->underlying().get() * other.get();
		return this->underlying();
	};

	template<typename ValueType> constexpr static void check_u_int(ValueType const &First, ValueType const &Second) {
		// additional check for uint types: check before doing multiplication, as afterwards it might be too late
		constexpr auto nmMax = std::numeric_limits<ValueType>::max();
		if (nmMax / First < Second) {
			DEVERROR("Multiplication of unsigned integers ", First, " * ", Second,
			         " would result in a value larger than the numeric maximum of that type (", nmMax, ")!");
		}
	}
};

/**
 * Provides * operators without interval checking
 * @tparam T Corresponds to TypeWithInterval
 */
template<typename T> struct MultiplicableNoCheck : SkillsCRTP<T, MultiplicableNoCheck> {
	[[nodiscard]] constexpr T operator*(T const &other) const {
		return T(tags::NoCheck{}, this->underlying().get() * other.get());
	};

	constexpr T &operator*=(T const &other) {
		this->underlying() = operator*(other);
		return this->underlying();
	};
};

//--------------------------------
// Division
//--------------------------------

/**
 * Provides / operators with interval checking
 * @tparam T Corresponds to TypeWithInterval
 */
template<typename T> struct DivisibleCheck : SkillsCRTP<T, DivisibleCheck> {
	[[nodiscard]] constexpr T operator/(T const &other) const { return T(this->underlying().get() / other.get()); };

	constexpr T &operator/=(T const &other) {
		this->underlying() = this->underlying().get() / other.get();
		return this->underlying();
	};
};

/**
 * Provides / operators without interval checking
 * @tparam T Corresponds to TypeWithInterval
 */
template<typename T> struct DivisibleNoCheck : SkillsCRTP<T, DivisibleNoCheck> {
	[[nodiscard]] constexpr T operator/(T const &other) const {
		return T(tags::NoCheck{}, this->underlying().get() / other.get());
	};

	constexpr T &operator/=(T const &other) {
		this->underlying() = operator/(other);
		return this->underlying();
	};
};
}

#endif
