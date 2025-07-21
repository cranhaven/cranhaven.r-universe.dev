//
// Created by madleina on 21.01.21.
//

#ifndef TVALUE_H
#define TVALUE_H

#include <cassert>

namespace stattools {

template<class Type> class TValueUpdated {
	Type _value    = Type{};
	Type _oldValue = Type{};

public:
	using value_type             = Type;

	constexpr TValueUpdated() = default;
	constexpr TValueUpdated(Type val) noexcept {
		_value    = val;
		_oldValue = val;
	}
	constexpr TValueUpdated(Type val, Type oldValue) noexcept {
		_value    = val;
		_oldValue = oldValue;
	}

	constexpr void initBoth(Type val) noexcept {
		_value    = val;
		_oldValue = val;
	}
	constexpr void setVal(Type val) noexcept { _value = val; }
	constexpr void operator=(Type val) noexcept {
		_oldValue = _value;
		_value    = val;
	}
	constexpr void reset() noexcept { _value = _oldValue; }
	[[nodiscard]] explicit operator Type() const { return _value; }; // cast operator
	constexpr Type oldValue() const noexcept { return _oldValue; }

	constexpr bool operator==(TValueUpdated<Type> Other) const noexcept {
		return (_value == Other._value); // only compare current value
	};
	constexpr bool operator!=(TValueUpdated<Type> Other) const noexcept {
		return (_value != Other._value); // only compare current value
	};
	constexpr bool operator<(TValueUpdated<Type> Other) const noexcept {
		return (_value < Other._value); // only compare current value
	};
	constexpr bool operator>(TValueUpdated<Type> Other) const noexcept {
		return (_value > Other._value); // only compare current value
	};
	constexpr bool operator<=(TValueUpdated<Type> Other) const noexcept {
		return (_value <= Other._value); // only compare current value
	};
	constexpr bool operator>=(TValueUpdated<Type> Other) const noexcept {
		return (_value >= Other._value); // only compare current value
	};
};

template<class Type> using TValueFixed = Type;

template <typename Type>
auto value(Type v) {return v;}

template <typename Underlying>
auto value(TValueUpdated<Underlying> v) {return (Underlying)v;}

}; // end namespace stattools

#endif // TVALUE_H
