//
// Created by madleina on 01.07.21.
//

#ifndef BANGOLIN_TMIRROR_H
#define BANGOLIN_TMIRROR_H

#include <limits>
#include <type_traits>

//-------------------------------------------
// MirrorSigned
//-------------------------------------------

namespace stattools {

template<class Type> class MirrorSigned {
	static_assert(std::is_signed_v<decltype(std::declval<Type>().get())>);

private:
	typedef decltype(std::declval<Type>().get()) UnderlyingType;

	static bool _canComputeValueMinusMin(Type Value, UnderlyingType Min) {
		if constexpr (std::is_floating_point_v<UnderlyingType>) {
			return Value <= Min - std::numeric_limits<Type>::lowest();
		} else {
			// other from floating point, because lowest() of integers is one bigger than max() -> if value = 0 and min
			// = lowest(), 0 - lowest() is one too big in order to be stored as a positive integer
			return Value < Min - std::numeric_limits<Type>::lowest();
		}
	}

	static bool _canComputeMaxMinusValue(Type Value, UnderlyingType Max) {
		return -(UnderlyingType)Value <= std::numeric_limits<UnderlyingType>::max() - Max;
	}

	static bool _crossedMin(Type Value, UnderlyingType Jump, UnderlyingType Min) {
		return -Jump > (UnderlyingType)Value - Min;
	}

	static Type _mirrorAtMin(Type Value, UnderlyingType Jump, UnderlyingType Min) {
		const Type val{Min - Jump - ((UnderlyingType)Value - Min)};
		if constexpr (std::is_integral_v<UnderlyingType>) { return val - 1; }
		return val;
	}

	static bool _crossedMax(Type Value, UnderlyingType Jump, UnderlyingType Max) {
		return Jump > Max - (UnderlyingType)Value;
	}

	static Type _mirrorAtMax(Type Value, UnderlyingType Jump, UnderlyingType Max) {
		const Type val{Max - (Jump - (Max - (UnderlyingType)Value))};
		if constexpr (std::is_integral_v<UnderlyingType>) { return val + 1; }
		return val;
	}

public:
	static Type mirror(Type Value, UnderlyingType Jump, UnderlyingType Min, UnderlyingType Max) {
		if (Value >= 0 && Min < 0) {
			// possible numerical problem calculating value - min (if min is very small, e.g. numeric lower bound)
			if (_canComputeValueMinusMin(Value, Min) && _crossedMin(Value, Jump, Min)) {
				return _mirrorAtMin(Value, Jump, Min);
			} // else value-min > numeric limits of that type -> no need to mirror, as jump can never be this big
			  // (restricted to range/2)
		} else if (_crossedMin(Value, Jump, Min)) {
			// this is now safe to calculate, as Value-min will always be within numeric limits
			return _mirrorAtMin(Value, Jump, Min);
		}

		if (Value < 0 && Max > 0) {
			// possible numerical problem calculating max - Value (if max is very large, e.g. numeric upper bound)
			if (_canComputeMaxMinusValue(Value, Max) && _crossedMax(Value, Jump, Max)) {
				return _mirrorAtMax(Value, Jump, Max);
			} // else _max - Value > numeric limits of that type -> no need to mirror, as jump can never be this big
			  // (restricted to range/2)
		} else if (_crossedMax(Value, Jump, Max)) {
			// this is now safe to calculate, as max-Value will always be within numeric limits
			return _mirrorAtMax(Value, Jump, Max);
		}

		UnderlyingType tmp = Value + Jump;
		if (tmp < Min) { // for numeric reasons, sometimes there can be an overflow at min -> last check in here
			return Type(Min);
		} else {
			return Type(tmp);
		}
	}
};

//-------------------------------------------
// MirrorUnsigned
//-------------------------------------------

template<class Type> class MirrorUnsigned {
	static_assert(std::is_unsigned_v<decltype(std::declval<Type>().get())>);

private:
	typedef decltype(std::declval<Type>().get()) UnderlyingType;

public:
	static Type mirror(Type Value, UnderlyingType Jump, UnderlyingType Shift, UnderlyingType Min, UnderlyingType Max) {
		if (Jump > Shift) {            // go to the right
			UnderlyingType delta = Jump - Shift;
			if (Max - Value < delta) { // mirror
				const auto val = Max - (delta - (Max - Value));
				if constexpr (std::is_integral_v<UnderlyingType>) { return val + 1; }
				return val;
			} else {
				return Value + delta;
			}
		} else {                       // go to the left
			UnderlyingType delta = Shift - Jump;
			if (Value - Min < delta) { // mirror
				const auto val = Min + delta - (Value - Min);
				if constexpr (std::is_integral_v<UnderlyingType>) { return val - 1; }
				return val;
			} else {
				return Value - delta;
			}
		}
	}
};

};     // end namespace stattools

#endif // BANGOLIN_TMIRROR_H
