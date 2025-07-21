//
// Created by madleina on 24.03.21.
//

#ifndef TLINESEARCH_H
#define TLINESEARCH_H

#include "coretools/Strings/toString.h"
#include "stattools/MLEInference/TReturnCodes.h"
#include <cmath>

namespace stattools {

class TLineSearch {
private:
	TReturnCode<double> _returnObj;
	size_t _counterIterations = 0;

	// dynamically adjust epsilon to terminate?
	bool _dynamicallyAdjustEpsilon       = false;
	double _valueToCompareToleranceTo    = 0.0;
	double _factorDynamicEpsilon         = 0.0;
	size_t _numIterUntilAdjustingEpsilon = 0;

	static bool _turnAround_Max(double f_x, double previous_f_x) {
		// if f_x gets smaller -> missed the peak
		return f_x < previous_f_x;
	}

	static bool _turnAround_Min(double f_x, double previous_f_x) {
		// if f_x gets larger -> missed the peak
		return f_x > previous_f_x;
	}

	static bool _turnAround_Zero(double f_x, double previous_f_x) {
		// if f_x and previous_f_x have different signs -> missed zero
		return f_x * previous_f_x <= 0.;
	}

	double _adjustEpsilon(double Value) {
		// only adjust once, i.e. set bool to false
		_dynamicallyAdjustEpsilon = false;

		// compare current value with _valueToCompareToleranceTo (e.g. the final value of a previous line search run)
		double diff = std::fabs(_valueToCompareToleranceTo - Value);
		// we want to be e.g. 1000x more precise than this difference -> multiply with e.g. 0.001
		return diff * _factorDynamicEpsilon;
	}

	template<class Object, class Function, class TurnAroundFunction, class... AdditionalArguments>
	TReturnCode<double> _lineSearch(Object &Obj, Function &F, TurnAroundFunction &TurnAround, std::string_view What,
									CriticalPoint WhichCriticalPoint, double Start, double InitialStep, double Epsilon,
									size_t MaxIterations, AdditionalArguments &...AdditionalArgs) {
		using namespace coretools::str;
		double x            = Start;
		double f_x          = (Obj.*F)(x, AdditionalArgs...);
		double previous_f_x = f_x;
		double step         = InitialStep;
		double minusExp1    = -exp(1.);

		for (_counterIterations = 0; _counterIterations < MaxIterations; _counterIterations++) {
			// check if we should adjust epsilon
			if (_dynamicallyAdjustEpsilon && _counterIterations == _numIterUntilAdjustingEpsilon) {
				Epsilon = _adjustEpsilon(x);
			}
			// make step
			x += step;
			// calculate f_x
			previous_f_x = f_x;
			f_x          = (Obj.*F)(x, AdditionalArgs...);
			// check if peak was missed -> turn around
			if (TurnAround(f_x, previous_f_x)) { step /= minusExp1; }
			// check for convergence
			if (std::fabs(step) < Epsilon && _counterIterations > _numIterUntilAdjustingEpsilon) {
				_returnObj.setSucceeded(x, WhichCriticalPoint, f_x, _counterIterations,
										toString("Line search: Found ",  What, " within ", _counterIterations,
												 " iterations. Absolute step at end: " + toString(std::fabs(step)), "."));
				return _returnObj;
			}
		}
		_returnObj.setFailed(toString("Line search: Failed to find ", What, ", reached maximum number of iterations (",
									  MaxIterations, +"). Absolute step at end: ", std::fabs(step), "."),
							 reachMaxIterations, x, WhichCriticalPoint, f_x, _counterIterations);
		return _returnObj;
	}

public:
	template<class Object, class Function, class... AdditionalArguments>
	TReturnCode<double> findMax(Object &Obj, Function &F, double Start, double InitialStep, double Epsilon,
								size_t MaxIterations, AdditionalArguments &...AdditionalArgs) {
		return _lineSearch(Obj, F, TLineSearch::_turnAround_Max, "maximum", maximum, Start, InitialStep, Epsilon,
						   MaxIterations, AdditionalArgs...);
	}

	template<class Object, class Function, class... AdditionalArguments>
	TReturnCode<double> findMin(Object &Obj, Function &F, double Start, double InitialStep, double Epsilon,
								size_t MaxIterations, AdditionalArguments &...AdditionalArgs) {
		return _lineSearch(Obj, F, TLineSearch::_turnAround_Min, "minimum", minimum, Start, InitialStep, Epsilon,
						   MaxIterations, AdditionalArgs...);
	}

	template<class Object, class Function, class... AdditionalArguments>
	TReturnCode<double> findZero(Object &Obj, Function &F, double Start, double InitialStep, double Epsilon,
								 size_t MaxIterations, AdditionalArguments &...AdditionalArgs) {
		// ATTENTION: method is not very robust. Only guaranteed to work if function is constantly increasing/decreasing
		// (based on my tests)... first make sure that initial step points to the correct direction
		double step     = InitialStep;
		double f_x      = (Obj.*F)(Start, AdditionalArgs...);
		double f_next_x = (Obj.*F)(Start + step, AdditionalArgs...);
		if (std::fabs(f_next_x) > std::fabs(f_x) && f_x * f_next_x > 0.) {
			// if function gets larger after making the step -> we are walking in the wrong direction, turn around
			step = -step;
		}
		return _lineSearch(Obj, F, TLineSearch::_turnAround_Zero, "root", noCriticalPoint, Start, step, Epsilon,
						   MaxIterations, AdditionalArgs...);
	}

	void dynamicallyAdjustTolerance(double ValueToCompareTo, double FactorPrecision, size_t NumIterUntilAdjustment) {
		// functions that enables Line search to dynamically adjust the epsilon for termination
		// idea: e.g. Line search is used within another algorithm, e.g. EM -> we want to dynamically scale epsilon
		// depending on how "far" we are in the EM, i.e. don't be precise inside NM in the beginning and be precise in
		// the end as next iteration of EM will anyways change values

		// after NumFunctionCallsUntilAdjustment function calls, Line search will compare the current value with
		// ValueToCompareTo (this is typically the function value of the best vertex of the last LS run) if those are
		// very close -> i.e. we are already close to where we were in a previous run of Line search, we want to have a
		// low epsilon, i.e. we want to be very precise if we are far away -> i.e. we already made far jumps compared
		// to where we were before, we don't want to be too precise, and have a relatively high epsilon
		// -> epsilon scales with distance of current Line search to previous Line search
		_dynamicallyAdjustEpsilon     = true;
		_valueToCompareToleranceTo    = ValueToCompareTo;
		_factorDynamicEpsilon         = FactorPrecision;
		_numIterUntilAdjustingEpsilon = NumIterUntilAdjustment;
	}

	[[nodiscard]] size_t getCounterIterations() const { return _counterIterations; }
};

} // end namespace stattools

#endif // TLINESEARCH_H
