#ifndef WEAKTYPE_H_
#define WEAKTYPE_H_

#include "coretools/Main/TError.h"
#include "coretools/Strings/fromString.h"
#include "coretools/Types/skills.h"
#include "coretools/traits.h"
#include <cstddef>
#include <string_view>
#include <ostream>

namespace coretools {

//------------------------------------------------
// Weak type that is bounded
//------------------------------------------------

template<typename Type, template<typename, typename> class Interval, size_t HashInterval = 0,
         template<typename> class... Skills>
class WeakType : public Skills<WeakType<Type, Interval, HashInterval, Skills...>>... {
private:
	using ThisType     = WeakType<Type, Interval, HashInterval, Skills...>;
	using IntervalType = Interval<Type, ThisType>;

protected:
	Type _value;

#ifdef CHECK_INTERVALS
	static constexpr bool _checkIntervals = true;
#else // CHECK_INTERVALS
	static constexpr bool _checkIntervals = false;
#endif

	// interval information
	constexpr void _ensureInterval() const noexcept(!_checkIntervals && noDebug) {
		if constexpr (_checkIntervals) {
			dev_assert(isInsideInterval(), "value ", _value, " outside range ", IntervalType::info, "!");
		} else {
			debug_assert(isInsideInterval(), "value ", _value, " outside range ", IntervalType::info, "!"); // always check in debug-mode
		}
	}

	constexpr void _set(Type Value) noexcept(!_checkIntervals) {
		_value = Value;
		_ensureInterval();
	}

public:
	using value_type = Type;

	constexpr WeakType() : _value(min()) {}
	explicit constexpr WeakType(tags::NoCheck, Type val) : _value(val) {} // no check
	constexpr WeakType(Type val) : _value(val) { _ensureInterval(); }     // not explicit
	explicit constexpr WeakType(tags::NoCheck, std::string_view ValueString)
	    : _value(str::fromString<Type, false>(ValueString)) { // no string conversion check into Type
		_ensureInterval();
	}
	explicit constexpr WeakType(std::string_view ValueString)
	    : _value(str::fromString<Type, true>(ValueString)) { // do string conversion check into Type
		user_assert(isInsideInterval(), "Value ", _value, " is outside range ", IntervalType::info, "!");
	}

	// assign
	constexpr ThisType &operator=(Type Val) {
		_set(Val);
		return *this;
	}
	ThisType &operator=(std::string_view ValueString) {
		_set(str::fromString<Type, true>(ValueString));
		return *this;
	}

	// convert
	[[nodiscard]] constexpr operator Type() const noexcept { return _value; } // not explicit
	[[nodiscard]] explicit operator std::string() const { return str::toString<Type>(_value); }
	[[nodiscard]] constexpr Type get() const noexcept { return _value; }

	// compare
	constexpr bool operator==(ThisType other) const noexcept { return _value == other._value; }
	constexpr bool operator!=(ThisType other) const noexcept { return _value != other._value; }
	constexpr bool operator<(ThisType other) const noexcept { return _value < other._value; }
	constexpr bool operator>(ThisType other) const noexcept { return _value > other._value; }

	template<typename OtherType, typename = std::enable_if_t<isCastable_v<OtherType, Type>>>
	constexpr bool operator==(OtherType other) const noexcept {
		return _value == (Type)other;
	}
	template<typename OtherType, typename = std::enable_if_t<isCastable_v<OtherType, Type>>>
	constexpr bool operator!=(OtherType other) const noexcept {
		return _value != (Type)other;
	}
	template<typename OtherType, typename = std::enable_if_t<isCastable_v<OtherType, Type>>>
	constexpr bool operator<(OtherType other) const noexcept {
		return _value < (Type)other;
	}
	template<typename OtherType, typename = std::enable_if_t<isCastable_v<OtherType, Type>>>
	constexpr bool operator>(OtherType other) const noexcept {
		return _value > (Type)other;
	}

	//---------------
	// calculate with underlying type
	// these functions return the underlying type, no interval checks happen here
	// need to write both left/right combinations out to prevent ambiguous overload errors
	//---------------
	template<typename OtherType, typename = std::enable_if_t<isCastable_v<OtherType, Type>>>
	[[nodiscard]] friend constexpr Type operator*(WeakType First, OtherType Second) {
		if constexpr (std::is_unsigned_v<Type>) {
			skills::MultiplicableCheck<ThisType>::check_u_int((Type)First, (Type)Second);
		}
		return (Type)First * (Type)Second;
	}

	[[nodiscard]] friend constexpr Type operator*(ThisType First, Type Second) {
		if constexpr (std::is_unsigned_v<Type>) {
			skills::MultiplicableCheck<ThisType>::check_u_int((Type)First, Second);
		}
		return (Type)First * Second;
	}

	[[nodiscard]] friend constexpr Type operator*(Type First, ThisType Second) {
		if constexpr (std::is_unsigned_v<Type>) {
			skills::MultiplicableCheck<ThisType>::check_u_int(First, (Type)Second);
		}
		return First * (Type)Second;
	}

	template<typename OtherType, typename = std::enable_if_t<isCastable_v<OtherType, Type>>>
	[[nodiscard]] friend constexpr Type operator/(ThisType First, OtherType Second) {
		return (Type)First / (Type)Second;
	}

	[[nodiscard]] friend constexpr Type operator/(ThisType First, Type Second) noexcept { return (Type)First / Second; }

	[[nodiscard]] friend constexpr Type operator/(Type First, ThisType Second) noexcept { return First / (Type)Second; }

	template<typename OtherType, typename = std::enable_if_t<isCastable_v<OtherType, Type>>>
	[[nodiscard]] friend constexpr Type operator+(ThisType First, OtherType Second) {
		if constexpr (std::is_unsigned_v<Type>) {
			skills::AddableCheck<ThisType>::check_u_int((Type)First, (Type)Second);
		}
		return (Type)First + (Type)Second;
	}

	[[nodiscard]] friend constexpr Type operator+(ThisType First, Type Second) {
		if constexpr (std::is_unsigned_v<Type>) { skills::AddableCheck<ThisType>::check_u_int((Type)First, Second); }
		return (Type)First + Second;
	}

	[[nodiscard]] friend constexpr Type operator+(Type First, ThisType Second) {
		if constexpr (std::is_unsigned_v<Type>) { skills::AddableCheck<ThisType>::check_u_int(First, (Type)Second); }
		return First + (Type)Second;
	}

	template<typename OtherType, typename = std::enable_if_t<isCastable_v<OtherType, Type>>>
	[[nodiscard]] friend constexpr Type operator-(ThisType First, OtherType Second) {
		if constexpr (std::is_unsigned_v<Type>) {
			skills::SubtractableCheck<ThisType>::check_u_int((Type)First, (Type)Second);
		}
		return (Type)First - (Type)Second;
	}

	[[nodiscard]] friend constexpr Type operator-(ThisType First, Type Second) {
		if constexpr (std::is_unsigned_v<Type>) {
			skills::SubtractableCheck<ThisType>::check_u_int((Type)First, Second);
		}
		return (Type)First - Second;
	}

	[[nodiscard]] friend constexpr Type operator-(Type First, ThisType Second) {
		if constexpr (std::is_unsigned_v<Type>) {
			skills::SubtractableCheck<ThisType>::check_u_int(First, (Type)Second);
		}
		return First - (Type)Second;
	}

	// stream
	friend std::ostream &operator<<(std::ostream &os, WeakType Other) {
		if constexpr (std::is_integral_v<Type> && !std::is_signed_v<Type>) {
			// unsigned integers: are printed as ASCII chars by default. Cast in order to print properly!
			os << static_cast<unsigned int>(Other.get());
		} else {
			os << Other.get();
		}
		return os;
	}

	// about interval
	constexpr bool isInsideInterval() const noexcept {
		return _value >= IntervalType::min && _value <= IntervalType::max;
	}
	static constexpr bool isInsideInterval(Type Value) noexcept {
		return Value >= IntervalType::min && Value <= IntervalType::max;
	}

	static constexpr ThisType min() noexcept { return ThisType(IntervalType::min); }
	static constexpr ThisType max() noexcept { return ThisType(IntervalType::max); }
	static constexpr IntervalType interval() noexcept { return IntervalType(); }

	// change interval
	static constexpr void setMin(Type Min) noexcept {
		static_assert(!std::is_const_v<decltype(IntervalType::min)>);
		IntervalType::min = Min;
	}

	static constexpr void setMax(Type Max) noexcept {
		static_assert(!std::is_const_v<decltype(IntervalType::max)>);
		IntervalType::max = Max;
	}
};
} // namespace coretools

#endif
