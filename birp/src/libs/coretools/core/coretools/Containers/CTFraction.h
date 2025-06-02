/*
 * TMassFunction.h
 *
 *  Created on: Apr 19, 2022
 *      Author: andreas
 */

#ifndef CTFRACTION_H_
#define CTFRACTION_H_

#include "coretools/traits.h"

namespace coretools {

template<typename T, int Num, int Denom>
struct CTFraction {
	using U = underlyingType_t<T>;
	static constexpr U value = static_cast<U>(Num)/Denom;
};

template<typename T>
using CT1 = CTFraction<T, 1, 1>;	

template<typename T>
using CT0 = CTFraction<T, 0, 1>;	
}

#endif
