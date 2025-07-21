/*
 * strongTypes.h
 *
 *  Created on: May 8, 2021
 *      Author: Daniel Wegmann
 */

#ifndef COMMONUTILITIES_CORE_STRONGTYPES_H_
#define COMMONUTILITIES_CORE_STRONGTYPES_H_

#include <ostream>
#include <type_traits>
#include <utility>

#include "coretools/Strings/fromString.h"
#include "coretools/traits.h"

namespace coretools {
namespace StrongTypes {

//------------------------------------------------
// Skills of strong types
// Inspired by https://github.com/joboccara/NamedType
//------------------------------------------------

// base CRTP class to build skills
template<typename T, template<typename> class crtpType> struct SkillsCRTP {
	constexpr T &underlying() { return static_cast<T &>(*this); };

	constexpr T const &underlying() const { return static_cast<T const &>(*this); };
};

template<typename T> struct Incrementable : SkillsCRTP<T, Incrementable> {
	constexpr T &operator++() {
		++this->underlying().get();
		return this->underlying();
	};
};

template<typename T> struct Decrementable : SkillsCRTP<T, Decrementable> {
	constexpr T &operator--() {
		--this->underlying().get();
		return this->underlying();
	};
};

/*
 * The Post-operators are ambiguous. How to solve?
template <typename T>
struct PostIncrementable : SkillsCRTP<T, PostIncrementable> {
    constexpr T operator++(int){
        return T( this->underlying().get()++ );
    };
};

template <typename T>
struct PostDecrementable : SkillsCRTP<T, PostDecrementable> {
    constexpr T operator--(int){
        return T( this->underlying().get()-- );
    };
};
*/

template<typename T> struct Addable : SkillsCRTP<T, Addable> {
	[[nodiscard]] constexpr T operator+(T const &other) const { return T(this->underlying().get() + other.get()); };

	constexpr T &operator+=(T const &other) {
		this->underlying().get() += other.get();
		return this->underlying();
	};

	[[nodiscard]] constexpr T operator+() const { return T(+this->underlying().get()); };
};

template<typename T> struct Subtractable : SkillsCRTP<T, Subtractable> {
	[[nodiscard]] constexpr T operator-(T const &other) const { return T(this->underlying().get() - other.get()); };

	constexpr T &operator-=(T const &other) {
		this->underlying().get() -= other.get();
		return this->underlying();
	};

	[[nodiscard]] constexpr T operator-() const { return T(-this->underlying().get()); };
};

template<typename T> struct Multiplicable : SkillsCRTP<T, Multiplicable> {
	[[nodiscard]] constexpr T operator*(T const &other) const { return T(this->underlying().get() * other.get()); };

	constexpr T &operator*=(T const &other) {
		this->underlying().get() *= other.get();
		return this->underlying();
	};
};

template<typename T> struct Divisible : SkillsCRTP<T, Divisible> {
	[[nodiscard]] constexpr T operator/(T const &other) const { return T(this->underlying().get() / other.get()); };

	constexpr T &operator/=(T const &other) {
		this->underlying().get() /= other.get();
		return this->underlying();
	};
};

template<typename T> struct Printable : SkillsCRTP<T, Printable> {
	static constexpr bool is_printable = true;

	void print(std::ostream &os) const { os << this->underlying().get(); }
};

template<typename T> struct Orderable : SkillsCRTP<T, Orderable> {
	static constexpr bool is_orderable = true;

	constexpr bool operator<(const T &other) const { return this->underlying().get() < other.get(); };

	constexpr bool operator<=(const T &other) const { return !(this->underlying().get() > other.get()); };

	constexpr bool operator>(const T &other) const { return this->underlying().get() > other.get(); };

	constexpr bool operator>=(const T &other) const { return !(this->underlying().get() < other.get()); };
};

//------------------------------------------------
// strongType
// a class to easily create string types with minimal features
//------------------------------------------------

template<typename T, class Tag, template<typename> class... Skills>
class StrongType : public Skills<StrongType<T, Tag, Skills...>>... { // Tag is a phantom type
protected:
	T _value;

public:
	using value_type = T;

	// constructors
	constexpr StrongType() : _value(){};

	explicit constexpr StrongType(const T &value) : _value(value){};

	explicit constexpr StrongType(T &&value) noexcept(std::is_nothrow_move_constructible<T>::value)
	    : _value(static_cast<T &&>(value)) // std::move() might not be constexpr
	      {};

	explicit StrongType(std::string_view value) { _value = str::fromString<T, true>(value); };
	explicit StrongType(tags::NoCheck, std::string_view value) { _value = str::fromString<T>(value); };

	// get
	[[nodiscard]] constexpr T &get() noexcept { return _value; };

	[[nodiscard]] constexpr std::remove_reference_t<T> const &get() const noexcept { return _value; };

	// conversion
	[[nodiscard]] explicit operator T &() noexcept { return _value; };

	[[nodiscard]] explicit constexpr operator const T &() const noexcept { return _value; };

	friend void swap(StrongType &first, StrongType &second) noexcept {
		std::swap(static_cast<T &>(first), static_cast<T &>(second));
	};

	// comparison with itself
	constexpr bool operator==(const StrongType &other) const { return _value == other._value; };

	constexpr bool operator!=(const StrongType &other) const { return _value != other._value; };

	// comparison with underlying type
	constexpr bool operator==(const T &other) const { return _value == other; };

	constexpr bool operator!=(const T &other) const { return _value != other; };
};

//------------------------------------------------
// External skills
//------------------------------------------------
// make underlying printable
template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_printable, std::ostream &>::type
constexpr operator<<(std::ostream &os, StrongType<T, Tag, Skills...> const &object) {
	object.print(os);
	return os;
};

// make underlying comparable (object vs underlying)
template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_orderable, bool>::type
constexpr operator<(StrongType<T, Tag, Skills...> const &object, const T &underlying) {
	return object.get() < underlying;
};

template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_orderable, bool>::type
constexpr operator<=(StrongType<T, Tag, Skills...> const &object, const T &underlying) {
	return object.get() <= underlying;
};

template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_orderable, bool>::type
constexpr operator>(StrongType<T, Tag, Skills...> const &object, const T &underlying) {
	return object.get() > underlying;
};

template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_orderable, bool>::type
constexpr operator>=(StrongType<T, Tag, Skills...> const &object, const T &underlying) {
	return object.get() >= underlying;
};

// make underlying comparable (underlying vs object)
template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_orderable, bool>::type
constexpr operator<(const T &underlying, StrongType<T, Tag, Skills...> const &object) {
	return underlying < object.get();
};

template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_orderable, bool>::type
operator<=(const T &underlying, StrongType<T, Tag, Skills...> const &object) {
	return underlying <= object.get();
};

template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_orderable, bool>::type
constexpr operator>(const T &underlying, StrongType<T, Tag, Skills...> const &object) {
	return underlying > object.get();
};

template<typename T, typename Tag, template<typename> class... Skills>
typename std::enable_if<StrongType<T, Tag, Skills...>::is_orderable, bool>::type
constexpr operator>=(const T &underlying, StrongType<T, Tag, Skills...> const &object) {
	return underlying >= object.get();
};

}; // end namespace StrongTypes
}; // end namespace coretools

#endif /* COMMONUTILITIES_CORE_STRONGTYPES_H_ */
