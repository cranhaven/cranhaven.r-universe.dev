/*
 * traits.h
 *
 *  Created on: Jul 04, 2022
 *      Author: Andreas
 */

#ifndef CORETOOLS_TRAITS_H_
#define CORETOOLS_TRAITS_H_

#include <cstddef>
#include <cstdint>
#include <tuple>
#include <type_traits>

namespace coretools {

constexpr bool checkIntervals() noexcept {
#ifdef CHECK_INTERVALS
	return true;
#else
	return false;
#endif
}


//argumentType
template<size_t I, typename> struct argumentType {};

template<size_t I, typename Result, typename... Args>
class argumentType<I, Result(Args...)> {
	using _args = std::tuple<Args...>;
public:
	using type = std::tuple_element_t<I, _args>;
};

template<size_t I, typename Result, typename TClass, typename... Args>
class argumentType<I, Result(TClass::*)(Args...)> {
	using _args = std::tuple<Args...>;
public:
	using type = std::tuple_element_t<I, _args>;
};

template<size_t I, typename T>
using argumentType_t = typename argumentType<I, T>::type;


// isIterable
template<typename, typename = void> class isIterable : public std::false_type {};

template<typename T>
class isIterable<T, std::void_t<decltype(std::declval<T>().begin()), decltype(std::declval<T>().end())>>
	: public std::true_type {};

template<typename T> constexpr bool isIterable_v = isIterable<T>::value;

//indexType
template<typename T, typename = void> struct indexType {
	using type = std::conditional_t<isIterable_v<T>, size_t, void>;
};

template<typename T>
struct indexType<T, std::void_t<typename T::index_type>> {
	using type = typename T::index_type;
};

template<typename T> using indexType_t = typename indexType<T>::type;

// isView
template<typename, typename = void> class isView : public std::false_type {};

template<typename T>
class isView<T, std::void_t<decltype(std::declval<T>().subview(std::declval<size_t>(), std::declval<size_t>())),
							decltype(std::declval<T>().remove_prefix(std::declval<size_t>())),
							decltype(std::declval<T>().remove_suffix(std::declval<size_t>()))>>
	: public std::true_type {};

template<typename T> constexpr bool isView_v = isView<T>::value;

// isResizeable
template<typename, typename = void> class isResizable : public std::false_type {};

template<typename T>
class isResizable<T, std::void_t<decltype(std::declval<T>().resize(std::declval<size_t>()))>>
	: public std::true_type {};

template<typename T> constexpr bool isResizable_v = isResizable<T>::value;

// isString
template<typename, typename = void> class isString : public std::false_type {};

template<typename T>
class isString<T, std::void_t<decltype(std::declval<T>().substr(0))>>
	: public std::true_type {};

template<typename T> constexpr bool isString_v = isString<T>::value;

// isCastable
template<typename From, typename To, typename = void> class isCastable : public std::false_type {};

template<typename From, typename To>
class isCastable<From, To, std::void_t<decltype(static_cast<To>(std::declval<From>()))>> : public std::true_type {};

template<typename From, typename To> constexpr bool isCastable_v = isCastable<From, To>::value;

// hasValueType
template<typename T, typename = void> struct hasValueType : public std::false_type {};
template<typename T> struct hasValueType<T, std::void_t<typename T::value_type>> : public std::true_type {};
template<typename T> constexpr bool hasValueType_v = hasValueType<T>::value;

// hasGetMember
template<typename T, typename = void> struct hasGetMember : public std::false_type {};
template<typename T> class hasGetMember<T, std::void_t<decltype(std::declval<T>().get())>> : public std::true_type {};
template<typename T> constexpr bool hasGetMember_v = hasGetMember<T>::value;

// isCalculable
template<typename T> struct isCalculable : public std::integral_constant<bool, std::is_arithmetic_v<T> || hasValueType_v<T>> {};
template<typename T> constexpr bool isCalculable_v = isCalculable<T>::value;

// underlying
template<typename Type> auto underlying(Type v) {
	// recursive so we can remove both get and cast
	if constexpr (hasValueType_v<Type>)
		return underlying(static_cast<typename Type::value_type>(v));
	else if constexpr (hasGetMember_v<Type>)
		return underlying(v.get());
	else if constexpr (std::is_enum_v<Type>)
		return static_cast<std::underlying_type_t<Type>>(v);
	else
		return v;
}

template<typename T> struct underlyingType {
	using type = decltype(underlying(std::declval<T>()));
};

template<typename T>
using underlyingType_t = typename underlyingType<T>::type;

template <typename T1, typename T2>
constexpr bool isSameUnderlyingType_v = std::is_same_v<coretools::underlyingType_t<T1>, coretools::underlyingType_t<T2>>;

// isTaggable
template<typename T, typename = void> struct hasIsTaggable : public std::false_type {};
template<typename T> class hasIsTaggable<T, std::void_t<decltype(std::declval<T>().isTaggable())>> : public std::true_type {};
template<typename T> constexpr bool hasIsTaggable_v = hasIsTaggable<T>::value;

namespace impl {
template<typename T> constexpr bool isTaggable() {
	if constexpr (std::is_floating_point_v<T>) {
		return true;
	} else if constexpr (hasIsTaggable_v<T>) {
		return T::isTaggable();
	} else {
		return false;
	}
}
} // namespace impl

	template<typename T> struct isTaggable : public std::integral_constant<bool, impl::isTaggable<T>()> {};
template<typename T> constexpr bool isTaggable_v = isTaggable<T>::value;

// isScopedEnum
	template<typename T> struct isScopedEnum : public std::integral_constant<bool, std::is_enum_v<T> && !std::is_convertible_v<T, underlyingType_t<T>>> {};
template<typename T> constexpr bool isScopedEnum_v = isScopedEnum<T>::value;


// SmallestInteger
template<std::size_t N>
struct SmallestInteger {
	static_assert(N <= 64);
	using type = std::conditional_t<
		N <= 8, std::uint8_t,
		typename std::conditional_t<N <= 16, std::uint16_t,
									typename std::conditional_t<N <= 32, std::uint32_t, std::uint64_t>>>;
};
template<std::size_t N> using SmallestInteger_t = typename SmallestInteger<N>::type;

// Tags for interval check
namespace tags {
struct NoCheck {};
} // namespace tags

} // namespace coretools

#endif
