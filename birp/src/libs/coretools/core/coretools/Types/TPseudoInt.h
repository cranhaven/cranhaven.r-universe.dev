#ifndef TPSEUDOINT_H_
#define TPSEUDOINT_H_

#include <cmath>
#include <cstdint>

namespace coretools {

class TPseudoInt {
public:
	static constexpr uint64_t maxLin = 255 - 63; // 1 << 63 is highest
private:		
	uint8_t _value;

	static uint8_t _linear2pseudo(uint64_t Lin) noexcept {
		if (Lin <= maxLin) return Lin;

		const auto shifted = Lin - maxLin;

		return maxLin + std::log2(shifted);
	}

	static constexpr uint64_t _pseudo2linear(uint8_t Pse) noexcept {
		if (Pse <= maxLin) return Pse;

		const auto shift = Pse - maxLin;
		return (uint64_t(1) << shift) + Pse;
	}

	constexpr TPseudoInt(uint8_t Value) : _value(Value) {};

public:
	// No Constructors, these functions are more clear
	static TPseudoInt fromLinear(uint64_t Lin) noexcept { return TPseudoInt{_linear2pseudo(Lin)}; }
	static constexpr TPseudoInt fromPseudo(uint8_t Pse) noexcept { return TPseudoInt{Pse}; }

	static constexpr TPseudoInt min() noexcept { return TPseudoInt{0}; }
	static constexpr TPseudoInt max() noexcept { return TPseudoInt{255}; }

	constexpr uint8_t pseudo() const noexcept {return _value;}
	constexpr uint64_t linear() const noexcept {return _pseudo2linear(_value);}

	friend constexpr bool operator==(TPseudoInt lhs, TPseudoInt rhs) noexcept {
		return lhs._value == rhs._value;
	}

	friend constexpr bool operator<(TPseudoInt lhs, TPseudoInt rhs) noexcept {
		return lhs._value < rhs._value;
	}
};
}

#endif
