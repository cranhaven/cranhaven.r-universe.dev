//
// Created by madleina on 13.12.23.
//

#ifndef STATTOOLS_BOXTRAITS_H
#define STATTOOLS_BOXTRAITS_H

#include "coretools/Storage/TDimension.h"

namespace stattools::impl {

//--------------------------------
// Helper functions
//--------------------------------

// Figure out if box has a function "doGibbs" that takes a TRange
template<typename, typename, typename = void> class doGibbsTakesRange : public std::false_type {};
template<typename ThisType, typename TypeBoxAround>
class doGibbsTakesRange<ThisType, TypeBoxAround,
						std::void_t<decltype(std::declval<TypeBoxAround>().doGibbs(
							std::declval<ThisType *>(), std::declval<const coretools::TRange &>()))>>
	: public std::true_type {};

template<typename ThisType, typename TypeBoxAround>
static constexpr bool doGibbsTakesRange_v = doGibbsTakesRange<ThisType, TypeBoxAround>::value;

// Figure out if box has a function "doGibbs" that takes a size_t
template<typename, typename, typename = void> class doGibbsTakesSizet : public std::false_type {};
template<typename ThisType, typename TypeBoxAround>
class doGibbsTakesSizet<
	ThisType, TypeBoxAround,
	std::void_t<decltype(std::declval<TypeBoxAround>().doGibbs(std::declval<ThisType *>(), std::declval<size_t>()))>>
	: public std::true_type {};

template<typename ThisType, typename TypeBoxAround>
static constexpr bool doGibbsTakesSizet_v = doGibbsTakesSizet<ThisType, TypeBoxAround>::value;

// Figure out if box has a function "doGibbs" that takes either size_t or TRange
template<typename ThisType, typename TypeBoxAround>
static constexpr bool doGibbs_v =
	(doGibbsTakesRange_v<ThisType, TypeBoxAround> || doGibbsTakesSizet_v<ThisType, TypeBoxAround>);

// Figure out if calculateLLRatio function takes TRange or not
template<typename, typename, typename = void> class calculateLLRatioTakesRange : public std::false_type {};
template<typename ThisType, typename TypeBoxAround>
class calculateLLRatioTakesRange<ThisType, TypeBoxAround,
								 std::void_t<decltype(std::declval<TypeBoxAround>().calculateLLRatio(
									 std::declval<ThisType *>(), std::declval<const coretools::TRange &>()))>>
	: public std::true_type {};

template<typename ThisType, typename TypeBoxAround>
static constexpr bool calculateLLRatioTakesRange_v = calculateLLRatioTakesRange<ThisType, TypeBoxAround>::value;

// Figure out if calculateLLRatio function takes size_t or not
template<typename, typename, typename = void> class calculateLLRatioTakesSizeT : public std::false_type {};
template<typename ThisType, typename TypeBoxAround>
class calculateLLRatioTakesSizeT<ThisType, TypeBoxAround,
								 std::void_t<decltype(std::declval<TypeBoxAround>().calculateLLRatio(
									 std::declval<ThisType *>(), std::declval<size_t>()))>> : public std::true_type {};

template<typename ThisType, typename TypeBoxAround>
static constexpr bool calculateLLRatioTakesSizeT_v = calculateLLRatioTakesSizeT<ThisType, TypeBoxAround>::value;

// Figure out if updateTempVals function takes TRange or not
template<typename, typename, typename = void> class updateTempValsTakesRange : public std::false_type {};
template<typename ThisType, typename TypeBoxAround>
class updateTempValsTakesRange<
	ThisType, TypeBoxAround,
	std::void_t<decltype(std::declval<TypeBoxAround>().updateTempVals(
		std::declval<ThisType *>(), std::declval<const coretools::TRange &>(), std::declval<bool>()))>>
	: public std::true_type {};

template<typename ThisType, typename TypeBoxAround>
static constexpr bool updateTempValsTakesRange_v = updateTempValsTakesRange<ThisType, TypeBoxAround>::value;

// Figure out if updateTempVals function takes size_t or not
template<typename, typename, typename = void> class updateTempValsTakesSizeT : public std::false_type {};
template<typename ThisType, typename TypeBoxAround>
class updateTempValsTakesSizeT<ThisType, TypeBoxAround,
							   std::void_t<decltype(std::declval<TypeBoxAround>().updateTempVals(
								   std::declval<ThisType *>(), std::declval<size_t>(), std::declval<bool>()))>>
	: public std::true_type {};

template<typename ThisType, typename TypeBoxAround>
static constexpr bool updateTempValsTakesSizeT_v = updateTempValsTakesSizeT<ThisType, TypeBoxAround>::value;

} // namespace stattools::impl

#endif // STATTOOLS_BOXTRAITS_H
