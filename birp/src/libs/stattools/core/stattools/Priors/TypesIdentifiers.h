//
// Created by caduffm on 7/6/21.
//

#ifndef TYPESIDENTIFIERS_H
#define TYPESIDENTIFIERS_H

#include "coretools/Types/intervals.h"

template<typename... Types> constexpr static bool TypesAreFloatingPoint() {
	return (true && ... && std::is_floating_point_v<typename Types::value_type>);
}

template<typename... Types> constexpr static bool TypesAreInteger() {
	return (true && ... && (std::is_integral_v<typename Types::value_type>));
}

template<typename... Types> constexpr static bool TypesAreUnsignedInteger() {
	return (true && ... &&
	        (std::is_unsigned_v<typename Types::value_type> && std::is_integral_v<typename Types::value_type>));
}

template<typename... Types> constexpr static bool TypesAreBool() {
	return (true && ... && std::is_same_v<bool, typename Types::value_type>);
}

template<typename... Types> inline constexpr static bool TypesAreUnbounded() {
	return (true && ... &&
	        (std::is_same_v<decltype(Types::interval()),
			 coretools::intervals::Unbounded<typename Types::value_type, Types>>));
}

template<typename... Types> inline constexpr static bool TypesArePositive() {
	return (
	    true && ... &&
	    (std::is_same_v<decltype(Types::interval()), coretools::intervals::Positive<typename Types::value_type, Types>>));
}

template<typename... Types> constexpr static bool TypesAreStrictlyPositive() {
	return (true && ... &&
	        (std::is_same_v<decltype(Types::interval()),
	                        coretools::intervals::StrictlyPositive<typename Types::value_type, Types>>));
}

template<typename... Types> inline constexpr static bool TypesAreNegative() {
	return (
	    true && ... &&
	    (std::is_same_v<decltype(Types::interval()), coretools::intervals::Negative<typename Types::value_type, Types>>));
}

template<typename... Types> constexpr static bool TypesAreZeroOneClosed() {
	return (true && ... &&
	        (std::is_same_v<decltype(Types::interval()),
	                        coretools::intervals::ZeroOneClosed<typename Types::value_type>>));
}

template<typename... Types> constexpr static bool TypesAreZeroOneOpen() {
	return (true && ... &&
	        (std::is_same_v<decltype(Types::interval()),
	                        coretools::intervals::ZeroOneOpen<typename Types::value_type, Types>>));
}

template<typename... Types> constexpr static bool TypesAreZeroOpenOneClosed() {
	return (true && ... &&
	        (std::is_same_v<decltype(Types::interval()),
			 coretools::intervals::ZeroOpenOneClosed<typename Types::value_type, Types>>));
}

template<typename... Types> constexpr static bool TypesAreUnboundedFloatingPoints() {
	return (true && ... && (TypesAreUnbounded<Types>() && TypesAreFloatingPoint<Types>()));
}

template<typename... Types> constexpr static bool TypesAreStrictlyPositiveFloatingPoints() {
	return (true && ... && (TypesAreStrictlyPositive<Types>() && TypesAreFloatingPoint<Types>()));
}

template<typename... Types> constexpr static bool TypesArePositiveFloatingPoints() {
	return (true && ... && (TypesArePositive<Types>() && TypesAreFloatingPoint<Types>()));
}

template<typename... Types> constexpr static bool TypesAreNegativeFloatingPoints() {
	return (true && ... && (TypesAreNegative<Types>() && TypesAreFloatingPoint<Types>()));
}

template<typename... Types> constexpr static bool TypesAreZeroOneClosedFloatingPoints() {
	return (true && ... && (TypesAreZeroOneClosed<Types>() && TypesAreFloatingPoint<Types>()));
}

template<typename... Types> constexpr static bool TypesAreZeroOneOpenFloatingPoints() {
	return (true && ... && (TypesAreZeroOneOpen<Types>() && TypesAreFloatingPoint<Types>()));
}

template<typename... Types> constexpr static bool TypesAreZeroOpenOneClosedFloatingPoints() {
	return (true && ... && (TypesAreZeroOpenOneClosed<Types>() && TypesAreFloatingPoint<Types>()));
}

template<typename... Types> constexpr static bool TypesAreUnsignedIntWithVariableMax() {
	return (true && ... &&
	        (TypesAreUnsignedInteger<Types>() &&
	         std::is_same_v<decltype(Types::interval()),
			 coretools::intervals::PositiveMaxVariable<typename Types::value_type, Types>>));
}

template<typename... Types> constexpr static bool TypesAreStrictlyPositiveWithVariableMax() {
	return (
	    true && ... &&
	    (std::is_same_v<decltype(Types::interval()),
	                    coretools::intervals::StrictlyPositiveMaxVariable<typename Types::value_type, Types>>));
}

template<typename... Types> constexpr static bool TypesAreMinMaxVariable() {
	return (true && ... &&
	        (std::is_same_v<decltype(Types::interval()),
	                        coretools::intervals::MinMaxVariable<typename Types::value_type, Types>>));
}

#endif // TYPESIDENTIFIERS_H
