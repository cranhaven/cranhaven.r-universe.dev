//
// Created by caduffm on 6/28/21.
//

#ifndef COMMONWEAKTYPES_H
#define COMMONWEAKTYPES_H

#include "coretools/Types/WeakType.h"
#include "coretools/Types/intervals.h"
#include "coretools/Types/skills.h"

namespace coretools {

// Unbounded: [min_double, max_double]
// +-*/ always work
using Unbounded = WeakType<double, intervals::Unbounded, 0, skills::AddableNoCheck, skills::SubtractableNoCheck,
						   skills::MultiplicableNoCheck, skills::DivisibleNoCheck>;

// Integer: [min_int, max_int]
// +-*/ always work
using Integer = WeakType<int, intervals::Unbounded, 0, skills::AddableNoCheck, skills::SubtractableNoCheck,
						 skills::MultiplicableNoCheck, skills::DivisibleNoCheck>;

// Boolean: 0, 1
// +-*/ always work
using Boolean = WeakType<bool, intervals::Unbounded, 0, skills::AddableNoCheck, skills::SubtractableNoCheck,
						 skills::MultiplicableNoCheck, skills::DivisibleNoCheck>;

// Positive: >= 0
// +*/ always work
// - must be checked
using Positive = WeakType<double, intervals::Positive, 0, skills::AddableNoCheck, skills::SubtractableCheck,
						  skills::MultiplicableNoCheck, skills::DivisibleNoCheck>;

// StrictlyPositive: > 0
// +*/ always work
// - must be checked
using StrictlyPositive = WeakType<double, intervals::StrictlyPositive, 0, skills::AddableNoCheck,
								  skills::SubtractableCheck, skills::MultiplicableNoCheck, skills::DivisibleNoCheck>;

// Negative: <= 0
// + always work
// /* never work
// - must be checked
using Negative = WeakType<double, intervals::Negative, 0, skills::AddableNoCheck, skills::SubtractableCheck>;

// Negative: <= 0
// + always work
// /* never work
// - must be checked
using StrictlyNegative =
	WeakType<double, intervals::StrictlyNegative, 0, skills::AddableNoCheck, skills::SubtractableCheck>;

// StrictlyPositive: > 0
// +*/ always work
// - must be checked
using StrictlyPositive = WeakType<double, intervals::StrictlyPositive, 0, skills::AddableNoCheck,
								  skills::SubtractableCheck, skills::MultiplicableNoCheck, skills::DivisibleNoCheck>;

// ZeroOneOpen: (0, 1)
// * always work
// +-/ must be checked
using ZeroOneOpen = WeakType<double, intervals::ZeroOneOpen, 0, skills::AddableCheck, skills::SubtractableCheck,
							 skills::MultiplicableNoCheck, skills::DivisibleCheck>;

// ZeroOneClosed: [0, 1]
// * always work
// +-/ must be checked
using ZeroOneClosed = WeakType<double, intervals::ZeroOneClosed, 0, skills::AddableCheck, skills::SubtractableCheck,
							   skills::MultiplicableNoCheck, skills::DivisibleCheck>;

// ZeroOpenOneClosed: (0, 1]
// * always work
// +-/ must be checked
using ZeroOpenOneClosed = WeakType<double, intervals::ZeroOpenOneClosed, 0, skills::AddableCheck,
								   skills::SubtractableCheck, skills::MultiplicableNoCheck, skills::DivisibleCheck>;

// PositiveWithMax: [0, max]
// +-*/ must be checked
template<size_t Hash>
using PositiveWithMax = WeakType<double, intervals::PositiveMaxVariable, Hash, skills::AddableCheck,
								 skills::SubtractableCheck, skills::MultiplicableCheck, skills::DivisibleCheck>;

// StrictlyPositiveWithMax: (0, max]
// +-*/ must be checked
template<size_t Hash>
using StrictlyPositiveWithMax = WeakType<double, intervals::StrictlyPositiveMaxVariable, Hash, skills::AddableCheck,
										 skills::SubtractableCheck, skills::MultiplicableCheck, skills::DivisibleCheck>;

// MinMaxVariable: (0, max]
// +-*/ must be checked
template<size_t Hash>
using MinMaxVariable = WeakType<double, intervals::MinMaxVariable, Hash, skills::AddableCheck,
								skills::SubtractableCheck, skills::MultiplicableCheck, skills::DivisibleCheck>;

// StrictlyPositiveInt: > 0
// +*/ always work
// - must be checked
using StrictlyPositiveInt = WeakType<int, intervals::StrictlyPositive, 0, skills::AddableNoCheck,
									 skills::SubtractableCheck, skills::MultiplicableNoCheck, skills::DivisibleNoCheck>;

// StrictlyPositiveUInt: > 0
// +*/ always work
// - must be checked
using StrictlyPositiveUInt =
	WeakType<size_t, intervals::StrictlyPositive, 0, skills::AddableNoCheck, skills::SubtractableCheck,
			 skills::MultiplicableNoCheck, skills::DivisibleNoCheck>;

// UnsignedInt8: [0, 255]
// +-*/ must be checked
using UnsignedInt8 = WeakType<uint8_t, intervals::Positive, 0, skills::AddableCheck, skills::SubtractableCheck,
							  skills::MultiplicableCheck, skills::DivisibleCheck>;

// UnsignedInt16: [0, 65535]
// +-*/ must be checked
using UnsignedInt16 = WeakType<uint16_t, intervals::Positive, 0, skills::AddableCheck, skills::SubtractableCheck,
							   skills::MultiplicableCheck, skills::DivisibleCheck>;

// UnsignedInt32: [0, 2^32-1]
// +-*/ must be checked
using UnsignedInt32 = WeakType<uint32_t, intervals::Positive, 0, skills::AddableCheck, skills::SubtractableCheck,
							   skills::MultiplicableCheck, skills::DivisibleCheck>;

// UnsignedInt64: [0,  2^64-1]
// +-*/ must be checked
using UnsignedInt64 = WeakType<uint64_t, intervals::Positive, 0, skills::AddableCheck, skills::SubtractableCheck,
							   skills::MultiplicableCheck, skills::DivisibleCheck>;
// UnsignedInt8WithMax: [0, max]
// +-*/ must be checked
template<size_t Hash>
using UnsignedInt8WithMax = WeakType<uint8_t, intervals::PositiveMaxVariable, Hash, skills::AddableCheck,
									 skills::SubtractableCheck, skills::MultiplicableCheck, skills::DivisibleCheck>;

// UnsignedInt16WithMax: [0, max]
// +-*/ must be checked
template<size_t Hash>
using UnsignedInt16WithMax = WeakType<uint16_t, intervals::PositiveMaxVariable, Hash, skills::AddableCheck,
									  skills::SubtractableCheck, skills::MultiplicableCheck, skills::DivisibleCheck>;

// UnsignedInt64WithMax: [0, max]
// +-*/ must be checked
template<size_t Hash>
using UnsignedInt64WithMax = WeakType<uint64_t, intervals::PositiveMaxVariable, Hash, skills::AddableCheck,
									  skills::SubtractableCheck, skills::MultiplicableCheck, skills::DivisibleCheck>;
} // namespace coretools

#endif // COMMONWEAKTYPES_H
