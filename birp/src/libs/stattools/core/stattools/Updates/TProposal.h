//
// Created by caduffm on 6/28/21.
//

#ifndef BANGOLIN_TPROPOSAL_H
#define BANGOLIN_TPROPOSAL_H

#include "coretools/Main/TError.h"
#include "coretools/Main/TParameters.h"
#include "coretools/Main/TRandomGenerator.h"
#include "stattools/ParametersObservations/TDefinition.h"
#include "stattools/Priors/TypesIdentifiers.h"
#include "stattools/Updates/TMirror.h"

namespace stattools {

//-------------------------------------------
// TPropKernel
//-------------------------------------------

template<class Type, class UnderlyingType> class TPropKernelBase {
private:
protected:
	ProposalKernel::MCMCProposalKernel _name;
	UnderlyingType _range;
	double _idealAccRate = 0.44;

	static UnderlyingType _maxPropKernel(UnderlyingType Range) {
		// prevent overshooting while mirroring: maximal proposal width is range/2
		return static_cast<UnderlyingType>(Range / 2.);
	};

	double _getScaleAdjustProposalWidth(UnderlyingType ProposalWidth, coretools::Probability AcceptanceRate) const {
		// get scaling
		double scale = AcceptanceRate / _idealAccRate;
		// cap at 0.1 and 2.0 to prevent too small/too large scaling
		scale        = std::max(scale, 0.1);
		scale        = std::min(scale, 2.0);

		// make sure we don't cross upper numeric limit of that type when multiplying with scale
		// scale * proposalWidth <= range/2 -> then, everything is ok
		// if scale > range/(2*proposalWidth) -> restrict scale to max
		double maxScale = static_cast<double>(_range) / (2. * ProposalWidth);
		if (scale > maxScale) { scale = maxScale; }

		return scale;
	}

	static UnderlyingType _getRange(UnderlyingType Min, UnderlyingType Max) {
		if constexpr (std::is_same_v<UnderlyingType, bool>) {
			return 1;
		} else {
			if (Min < 0 && Max > 0 && Max >= Min - std::numeric_limits<UnderlyingType>::lowest()) {
				// _max - _min would cause numeric overflow -> restrict to numeric max
				return std::numeric_limits<UnderlyingType>::max();
			} else {
				return Max - Min;
			}
		}
	}

	// keep constructors protected so the base class can not be used
	TPropKernelBase() {
		_name = ProposalKernel::missing;
		setRange();

		// According to Roberts et al 1997 (Weak convergence and optimal scaling of random walk Metropolis algorithms):
		_idealAccRate = coretools::instances::parameters().get("accRate", 0.44);
	};

public:
	virtual ~TPropKernelBase() = default;

	void setRange() { _range = _getRange(Type::min(), Type::max()); }

	virtual Type propose(Type Value, UnderlyingType ProposalWidth) const                                         = 0;
	virtual Type propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth) const = 0;

	virtual UnderlyingType adjustProposalWidth(UnderlyingType ProposalWidth, coretools::Probability AcceptanceRate,
											   std::string_view Name) {
		if constexpr (!std::is_same_v<UnderlyingType, bool>) {
			double scale = _getScaleAdjustProposalWidth(ProposalWidth, AcceptanceRate);
			ProposalWidth *= scale;
		}
		return adjustPropKernelIfTooBig(ProposalWidth, Name);
	}

	virtual UnderlyingType adjustPropKernelIfTooBig(UnderlyingType ProposalWidth, std::string_view Name) {
		if constexpr (!std::is_same_v<UnderlyingType, bool>) {
			if (ProposalWidth < 0) {
				throw coretools::TUserError("Proposal width (", ProposalWidth, ") for parameter '", Name, "' is negative!");
			}
		}

		// restrict to maximum proposal kernel
		ProposalWidth = std::min(ProposalWidth, _maxPropKernel(_range));

		if (ProposalWidth == 0) {
			// if parameter was fixed and you re-start from initVals file, initJumpSize will be 0 -> adjust this
			// automatically
			if constexpr (std::is_integral_v<UnderlyingType>) {
				ProposalWidth = 1;
			} else {
				ProposalWidth = 0.1;
			}
		}

		return ProposalWidth;
	};

	ProposalKernel::MCMCProposalKernel name() const { return _name; };

	virtual double hastingsRatioPropKernel(Type, Type) const {
		// for reversible proposal kernels: proposal kernel cancels out in Hastings ratio
		return 1.0;
	}

	virtual double logHastingsRatioPropKernel(Type, Type) const {
		// for reversible proposal kernels: proposal kernel cancels out in Hastings ratio
		// -> log(1.0) = 0.0
		return 0.0;
	}

	double idealAccRate() const { return _idealAccRate; }
	virtual bool isAdjusted() const   = 0;
	virtual bool isReversible() const = 0;
};

//-------------------------------------------
// TPropKernelNormal
//-------------------------------------------

template<class Type, class UnderlyingType> class TPropKernelNormal : public TPropKernelBase<Type, UnderlyingType> {
	static_assert(std::is_floating_point_v<UnderlyingType>);

private:
	virtual double _getJump(UnderlyingType ProposalWidth) const {
		// virtual for mocking
		return coretools::instances::randomGenerator().getNormalRandom(0, ProposalWidth);
	}

	Type _propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth,
				  UnderlyingType MaxPropKernel) const {
		// random value should always be within [-(max-min)/2, +(max-min)/2] -> prevents issues with overshooting while
		// mirroring & numerical overflow
		double jump = _getJump(ProposalWidth);
		while (jump < -MaxPropKernel || jump > MaxPropKernel) { jump = _getJump(ProposalWidth); }

		// now mirror
		return MirrorSigned<Type>::mirror(Value, jump, Min, Max);
	}

public:
	TPropKernelNormal() : TPropKernelBase<Type, UnderlyingType>() { this->_name = ProposalKernel::normal; }

	~TPropKernelNormal() override = default;

	Type propose(Type Value, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Type::min(), Type::max(), ProposalWidth, this->_maxPropKernel(this->_range));
	};

	Type propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Min, Max, ProposalWidth, this->_maxPropKernel(this->_getRange(Min, Max)));
	};

	bool isAdjusted() const override { return true; };

	bool isReversible() const override { return true; };
};

//-------------------------------------------
// TPropKernelUniform
//-------------------------------------------

template<class Type, class UnderlyingType> class TPropKernelUniform : public TPropKernelBase<Type, UnderlyingType> {
	static_assert(std::is_floating_point_v<UnderlyingType>);

private:
	virtual double _getJump() const { // virtual for mocking
		return coretools::instances::randomGenerator().getRand();
	}

	Type _propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth) const {
		double jump = _getJump() * ProposalWidth - ProposalWidth / 2.0;

		// now mirror
		return MirrorSigned<Type>::mirror(Value, jump, Min, Max);
	};

public:
	TPropKernelUniform() : TPropKernelBase<Type, UnderlyingType>() { this->_name = ProposalKernel::uniform; };

	~TPropKernelUniform() override = default;

	Type propose(Type Value, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Type::min(), Type::max(), ProposalWidth);
	};

	Type propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Min, Max, ProposalWidth);
	};

	bool isAdjusted() const override { return true; };

	bool isReversible() const override { return true; };
};

//-------------------------------------------
// TPropKernelScalingLogNormal
//-------------------------------------------

template<class Type, class UnderlyingType>
class TPropKernelScalingLogNormal : public TPropKernelBase<Type, UnderlyingType> {
	static_assert(TypesArePositiveFloatingPoints<Type>() || TypesAreStrictlyPositiveFloatingPoints<Type>() ||
				  TypesAreNegativeFloatingPoints<Type>());
	/* Proposal width is drawn from log-normal distribution and multiplied to current value (instead of added)
	 * Advantage: scales with current value (similar effect as updating parameter in log)
	 * Attention: not reversible -> need to correct Hastings ratio!
	 */
private:
	constexpr static const double _log0_5 = -0.69314718055994528623; // = log(0.5)

	virtual double _getJump(UnderlyingType ProposalWidth) const {
		// virtual for mocking
		return coretools::instances::randomGenerator().getNormalRandom(0, ProposalWidth);
	}

	Type _propose(Type Value, UnderlyingType ProposalWidth) const {
		// draw random normal number and take exponential
		// (-> corresponds to random log-normal number)
		double jump = exp(_getJump(ProposalWidth));
		while (jump == 0.0) { jump = exp(_getJump(ProposalWidth)); }

		// make sure Value is never exactly 0.0 (-> would stick there forever)
		if (Value > 0.0) {
			// don't mirror:
			// -> jump is always >0, sign change is impossible
			// -> bounded values are prohibited in type declaration
			// reason: proposal kernel is not reversible -> mirroring gets complicated!
			return Value * jump;
		} else {                  // Value == 0.0
			return 10e-06 * jump; // don't take numeric_limits::min(), since jump can be < 1 -> would underflow
		}
	};

public:
	TPropKernelScalingLogNormal() : TPropKernelBase<Type, UnderlyingType>() {
		this->_name = ProposalKernel::scaleLogNormal;
	}

	~TPropKernelScalingLogNormal() override = default;

	Type propose(Type Value, UnderlyingType ProposalWidth) const override { return _propose(Value, ProposalWidth); };

	Type propose(Type Value, UnderlyingType /*Min*/, UnderlyingType /*Max*/,
				 UnderlyingType ProposalWidth) const override {
		// no mirroring: min and max are not used
		return _propose(Value, ProposalWidth);
	};

	bool isAdjusted() const override { return true; };

	bool isReversible() const override { return false; };

	UnderlyingType adjustProposalWidth(UnderlyingType ProposalWidth, coretools::Probability AcceptanceRate,
									   std::string_view Name) override {
		double scale = this->_getScaleAdjustProposalWidth(ProposalWidth, AcceptanceRate);

		double var      = ProposalWidth * ProposalWidth; // = sigma^2
		double exp_var  = exp(var);                      // = exp(sigma^2)
		double exp_2var = exp_var * exp_var;             // = exp(2*sigma^2)

		double varAdjusted = _log0_5 + log(1.0 + sqrt(1.0 + 4.0 * scale * (exp_2var - exp_var)));
		ProposalWidth      = sqrt(varAdjusted);

		// make sure proposal width is not too small (could potentially underflow)
		ProposalWidth = std::max(ProposalWidth, 10e-06);
		return this->adjustPropKernelIfTooBig(ProposalWidth, Name);
	}

	double hastingsRatioPropKernel(Type Value, Type OldValue) const override {
		// not reversible
		return Value / OldValue;
	}

	double logHastingsRatioPropKernel(Type Value, Type OldValue) const override {
		// not reversible
		if (Value == 0.0) { // avoid log(0) = -Inf
			Value = std::numeric_limits<UnderlyingType>::min();
		}
		if (OldValue == 0.0) { // avoid log(0) = -Inf
			OldValue = std::numeric_limits<UnderlyingType>::min();
		}
		return log(Value) - log(OldValue);
	}
};

//-------------------------------------------
// TPropKernelInteger
//-------------------------------------------

template<class Type, class UnderlyingType> class TPropKernelInteger : public TPropKernelBase<Type, UnderlyingType> {
	static_assert(std::is_integral_v<UnderlyingType>);

protected:
	std::vector<double> _cumulProbs;

	void _fillCumulProbabilities(UnderlyingType ProposalWidth) {
		size_t numElements = ProposalWidth * 2 + 1;
		_cumulProbs.resize(numElements);
		for (size_t i = 0; i < numElements; i++) {
			_cumulProbs[i] = static_cast<double>((double)i + 1.) / static_cast<double>(numElements);
		}
	};

	UnderlyingType _getJump(UnderlyingType ProposalWidth) const {
		auto jump = static_cast<UnderlyingType>(coretools::instances::randomGenerator().pickOne(_cumulProbs));
		// -> if jump = 0 -> draw another number, because 0 is just a waste, we don't update the value at all
		while (jump == ProposalWidth) {
			jump = static_cast<UnderlyingType>(coretools::instances::randomGenerator().pickOne(_cumulProbs));
		}
		return jump;
	}

	Type _mirror(Type Value, UnderlyingType Jump, UnderlyingType ProposalWidth, UnderlyingType Min,
				 UnderlyingType Max) const {
		if constexpr (std::is_signed_v<UnderlyingType>) {
			return MirrorSigned<Type>::mirror(Value, Jump - ProposalWidth, Min, Max);
		} else {
			return MirrorUnsigned<Type>::mirror(Value, Jump, ProposalWidth, Min, Max);
		}
	}

	Type _propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth) const {
		// pick one element from -propWidth:propWidth
		// e.g. propWidth = 3 -> get values 0, 1, 2, 3, 4, 5, 6 with equal probabilities from pickOne (given by
		// cumulProbs)
		// -> shift by propWidth to have it symmetric around 0 (-> results in value -3, -2, -1, 0, 1, 2, 3)
		UnderlyingType jump = _getJump(ProposalWidth);

		// mirror
		Type val = _mirror(Value, jump, ProposalWidth, Min, Max);
		while (val == Value) { // by mirroring, it is possible to jump back on previous value -> draw again
			jump = _getJump(ProposalWidth);
			val  = _mirror(Value, jump, ProposalWidth, Min, Max);
		}
		return val;
	};

public:
	TPropKernelInteger() : TPropKernelBase<Type, UnderlyingType>() { this->_name = ProposalKernel::integer; };

	~TPropKernelInteger() override = default;

	UnderlyingType adjustPropKernelIfTooBig(UnderlyingType ProposalWidth, std::string_view Name) override {
		// overridden to fill cumulative probabilities whenever propKernel is changed
		ProposalWidth = TPropKernelBase<Type, UnderlyingType>::adjustPropKernelIfTooBig(ProposalWidth, Name);

		// fill cumulative probabilities
		_fillCumulProbabilities(ProposalWidth);
		return ProposalWidth;
	}

	Type propose(Type Value, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Type::min(), Type::max(), ProposalWidth);
	};

	Type propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Min, Max, ProposalWidth);
	};

	bool isAdjusted() const override { return true; };

	bool isReversible() const override { return true; };
};

//-------------------------------------------
// TPropKernelRandomInteger
//-------------------------------------------

template<class Type, class UnderlyingType>
class TPropKernelRandomInteger : public TPropKernelBase<Type, UnderlyingType> {
	static_assert(std::is_integral_v<UnderlyingType>);

private:
	static Type _propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType /*ProposalWidth*/) {
		// pick one element from entire range
		// pick again if newVal == Value (wasted)
		// no need to mirror
		if (Min == 0 && Max == 1) { return (Type)1 - Value; }
		UnderlyingType newValue =
			coretools::instances::randomGenerator().getRandIntegerClosedMinMax<UnderlyingType>(Min, Max);
		while (newValue == Value) {
			newValue = coretools::instances::randomGenerator().getRandIntegerClosedMinMax<UnderlyingType>(Min, Max);
		}
		return newValue;
	};

public:
	TPropKernelRandomInteger() : TPropKernelBase<Type, UnderlyingType>() {
		this->_name = ProposalKernel::randomInteger;
	};

	~TPropKernelRandomInteger() override = default;

	Type propose(Type Value, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Type::min(), Type::max(), ProposalWidth);
	};

	Type propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Min, Max, ProposalWidth);
	};

	bool isAdjusted() const override { return false; };

	bool isReversible() const override { return true; };
};

//-------------------------------------------
// TPropKernelBool
//-------------------------------------------

template<class Type> class TPropKernelBool : public TPropKernelBase<Type, bool> {
	using UnderlyingType = bool;

private:
	static Type _propose(Type Value, UnderlyingType /*Min*/, UnderlyingType /*Max*/, UnderlyingType /*ProposalWidth*/) {
		return !Value;
	};

public:
	TPropKernelBool() : TPropKernelBase<Type, UnderlyingType>() { this->_name = ProposalKernel::boolean; };

	~TPropKernelBool() override = default;

	Type propose(Type Value, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Type::min(), Type::max(), ProposalWidth);
	};

	Type propose(Type Value, UnderlyingType Min, UnderlyingType Max, UnderlyingType ProposalWidth) const override {
		return _propose(Value, Min, Max, ProposalWidth);
	};

	UnderlyingType adjustProposalWidth(UnderlyingType, coretools::Probability, std::string_view) override {
		// don't adjust proposal width for bools
		return true;
	}

	UnderlyingType adjustPropKernelIfTooBig(UnderlyingType, std::string_view) override {
		// don't adjust proposal width for bools
		return true;
	};

	bool isAdjusted() const override {
		// don't adjust proposal width for bools
		return false;
	};

	bool isReversible() const override { return true; };
};

};     // namespace stattools

#endif // BANGOLIN_TPROPOSAL_H
