#ifndef TLOGINT_H_
#define TLOGINT_H_

#include "coretools/Main/TError.h"

#include <cmath>
#include <cstdint>

namespace coretools {

class TLogInt {
private:		
	uint8_t _value;

	static uint8_t _linear2log(uint64_t Lin) noexcept(noDebug) {
		DEBUG_ASSERT(Lin > 0);
		// Performance improved using C++20 bit>
		return ceil(std::log2(Lin));
	}

	static constexpr uint64_t _log2linear(uint8_t Log) noexcept(noDebug) {
		DEBUG_ASSERT(Log < 64); // overflow
		return uint64_t(1) << Log;
	}

	constexpr TLogInt(uint8_t Value) : _value(Value) {}

public:
	// No Constructors, these functions are more clear
	static TLogInt fromLinear(uint64_t Lin) noexcept { return TLogInt{_linear2log(Lin)}; }
	static constexpr TLogInt fromLog(uint8_t Log) noexcept(noDebug) {
		DEBUG_ASSERT(Log < 64);
		return TLogInt{Log};
	}
	static constexpr TLogInt min() noexcept { return TLogInt{0}; } // linear = 1 !
	static constexpr TLogInt max() noexcept { return TLogInt{63}; }

	constexpr uint8_t log() const noexcept {return _value;}
	constexpr uint64_t linear() const noexcept {return _log2linear(_value);}

	friend constexpr bool operator==(TLogInt lhs, TLogInt rhs) noexcept {
		return lhs._value == rhs._value;
	}
	friend constexpr bool operator<(TLogInt lhs, TLogInt rhs) noexcept {
		return lhs._value < rhs._value;
	}
};
}

#endif
