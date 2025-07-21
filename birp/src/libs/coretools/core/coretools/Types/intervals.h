
#ifndef INTERVALS_H_
#define INTERVALS_H_

#include <cmath>
#include <limits>
#include <string>

#include "coretools/Strings/toString.h"

namespace coretools::intervals {

template<typename Type> constexpr Type smallestPositiveRepresentableNumber() {
	// note: for types that are strictly positive/negative, we need the smallest positive/negative representable number
	// -> std::numeric_limits<Type>::min() = std::numeric_limits<Type>::lowest() for ints! -> Can not be used, instead
	// return 1
	if constexpr (std::is_integral_v<Type>) {
		return 1;
	} else {
		return std::numeric_limits<Type>::min();
	}
}

//------------------------------------------------
// TWeakTypeIntervals
// - intervals used by WeakTypeWithInterval
// - a valid interval must define min, max and info (string)
// - a valid interval must define two template parameters:
//   1) Type, which is the underlying fundamental type, e.g. double/int...
//   2) Tag, which is a phantom type needed for intervals where min/max can be changed.
//       This is to avoid that different weak types with
//       the same interval share min/max (as min/max are static).
//       Since most intervals defined below do not allow changes in min/max (they are const),
//       Tag has a default (void) for them.
//------------------------------------------------

template<typename Type, typename Tag = void> struct Unbounded {
	static constexpr Type min            = std::numeric_limits<Type>::lowest();
	static constexpr Type max            = std::numeric_limits<Type>::max();
	inline static const std::string info = str::toString("[", min, ", ", max, "]");
};

template<typename Type, typename Tag = void> struct ZeroOneClosed {
	static constexpr Type min              = 0.0;
	static constexpr Type max              = 1.0;
	static constexpr std::string_view info = "[0, 1]";
};

template<typename Type, typename Tag = void> struct ZeroOneOpen {
	static constexpr Type min              = smallestPositiveRepresentableNumber<Type>();
	inline static const Type max           = std::nextafter(1.0, 0.0);
	static constexpr std::string_view info = "(0, 1)";
};

template<typename Type, typename Tag = void> struct ZeroOpenOneClosed {
	static constexpr Type min            = smallestPositiveRepresentableNumber<Type>();
	static constexpr Type max            = 1.0;
	static constexpr std::string_view info = "(0, 1]";
};

template<typename Type, typename Tag = void> struct Positive {
	static constexpr Type min            = 0.0;
	static constexpr Type max            = std::numeric_limits<Type>::max();
	inline static const std::string info = str::toString("[0, ", max, "]");
};

template<typename Type, typename Tag = void> struct StrictlyPositive {
	static constexpr Type min            = smallestPositiveRepresentableNumber<Type>();
	static constexpr Type max            = std::numeric_limits<Type>::max();
	inline static const std::string info = str::toString("(0, ", max, "]");
};

template<typename Type, typename Tag = void> struct Negative {
	static constexpr Type min            = std::numeric_limits<Type>::lowest();
	static constexpr Type max            = 0.0;
	inline static const std::string info = str::toString("[", min, ", 0]");
};

template<typename Type, typename Tag = void> struct StrictlyNegative {
	static constexpr Type min            = std::numeric_limits<Type>::lowest();
	static constexpr Type max            = -smallestPositiveRepresentableNumber<Type>();
	inline static const std::string info = str::toString("[", min, ",  0)");
};

template<typename Type, typename Tag> struct PositiveMaxVariable { // Tag is a phantom type
	// Always positive, but max can be changed (not const)
	// Tag does not have a default to avoid shared static max between different weak types
	static constexpr Type min            = 0.0;
	inline static Type max               = std::numeric_limits<Type>::max(); // not const
	inline static const std::string info = str::toString("[0, ", max, "]");
};

template<typename Type, typename Tag> struct StrictlyPositiveMaxVariable { // Tag is a phantom type
	// Always positive, but max can be changed (not const)
	// Tag does not have a default to avoid shared static max between different weak types
	static constexpr Type min            = smallestPositiveRepresentableNumber<Type>();
	inline static Type max               = std::numeric_limits<Type>::max(); // not const
	inline static const std::string info = str::toString("(0, ", max, "]");
};

template<typename Type, typename Tag> struct MinMaxVariable { // Tag is a phantom type
	// min and max can be changed (not const)
	// Tag does not have a default to avoid shared static max between different weak types
	inline static Type min               = std::numeric_limits<Type>::lowest(); // not const
	inline static Type max               = std::numeric_limits<Type>::max();    // not const
	inline static const std::string info = str::toString("[", min, ", ", max, "]");
};
}

#endif
