//
// Created by madleina on 24.03.21.
//

#ifndef TNEWTONRAPHSON_H
#define TNEWTONRAPHSON_H

#include <cassert>
#include <cmath>
#include "coretools/arma_include.h"

#include "coretools/Containers/TView.h"
#include "coretools/algorithms.h"
#include "stattools/MLEInference/TReturnCodes.h"

namespace stattools {

//-------------------------------
// One-Dimensional Newton-Raphson
//-------------------------------

class TNewtonRaphson {
	// this class implements two Newton-Raphson algorithms
	// the first one (runNewtonRaphson_withBisection) prevents overshooting by bisection
	// the second one (runNewtonRaphson_withBacktracking) prevents overshooting by backtracking
private:
	using Function = std::function<double(double)>;

	// function and derivative
	Function _function;
	Function _derivative;

	// return object
	TReturnCode<double> _returnObj;

	// when to stop?
	double _epsilon           = 10e-10;
	size_t _maxIterations     = 1000;
	size_t _counterIterations = 0;

	// dynamically adjust tolerance to terminate?
	bool _dynamicallyAdjustTolerance = false;
	double _abs_F_ref                = 0.0;
	double _factorDynamicTolerance   = 0.0;

	bool _minMaxIsValid(double F_min, double F_max, double Min, double Max) {
		using namespace coretools::str;
		if (std::isnan(F_min) || !std::isfinite(F_min)) {
			_returnObj.setFailed(
				toString("Newton-Raphson failed: Min (", Min,
						 ") provided as lower boundary results in invalid function values (f(Min) = ", F_min, ")."),
				invalidInput, _counterIterations);
			return false;
		}
		if (std::isnan(F_max) || !std::isfinite(F_max)) {
			_returnObj.setFailed(
				toString("Newton-Raphson failed: Max (", Max,
						 ") provided as upper boundary results in invalid function values (f(Max) = ", F_max, ")."),
				invalidInput, _counterIterations);
			return false;
		}
		if (F_min > 0. && F_max > 0.) {
			_returnObj.setFailed(toString("Newton-Raphson failed: Min (", Min, ") and Max (", Max,
										  ") provided do not bracket zero (interval is ", F_min, ",", F_max, ")."),
								 zeroNotEnclosed, Max,
								 _counterIterations); // derivative > 0 -> peak must be on the left
			return false;
		} else if (F_min < 0. && F_max < 0.) {
			_returnObj.setFailed(toString("Newton-Raphson failed: Min (", Min, ") and Max (", Max,
										  ") provided do not bracket zero (interval is ", F_min, ",", F_max, ")."),
								 zeroNotEnclosed, Min,
								 _counterIterations); // derivative < 0 -> peak must be on the right
			return false;
		}
		return true;
	}

	bool _checkMinMax(double F_min, double F_max, double &Min, double &Max) {
		if (!_minMaxIsValid(F_min, F_max, Min, Max)) { return false; }

		if (F_min == 0.) {
			double d_x         = _derivative(Min);
			auto criticalPoint = _determineCriticalPoint(d_x);
			_returnObj.setSucceeded(
				Min, criticalPoint, F_min, _counterIterations,
				"Newton-Raphson with bisection: Found root within 0 iterations, root is exactly Min.");
			return false;
		}
		if (F_max == 0.) {
			double d_x         = _derivative(Max);
			auto criticalPoint = _determineCriticalPoint(d_x);
			_returnObj.setSucceeded(
				Max, criticalPoint, F_max, _counterIterations,
				"Newton-Raphson with bisection: Found root within 0 iterations, root is exactly Max.");
			return false;
		}

		if (F_min > 0.) {
			// Turn min and max around such that f(Min) < 0
			double tmp = Min;
			Min        = Max;
			Max        = tmp;
		}
		return true;
	}

	static CriticalPoint _determineCriticalPoint(double Cur_Dx) {
		CriticalPoint criticalPoint = noCriticalPoint;
		if (Cur_Dx < 0.) { // only makes sense if D(x) is actually 2nd derivative of some function -> then we can check
						   // if point corresponds to min or max
			criticalPoint = maximum;
		} else if (Cur_Dx > 0.) {
			criticalPoint = minimum;
		}
		return criticalPoint;
	}

	static void _doBisection(double &Step, double &PreviousStep, double &X, double Min, double Max) {
		PreviousStep = Step;
		Step         = 0.5 * (Max - Min);
		X            = Min + Step;
	}

	static double _getStep(double F_x, double D_x) {
		if (D_x == 0.) {
			return 0.0;
		} else {
			return F_x / D_x;
		}
	}

	static void _doNewtonRaphson(double &Step, double &PreviousStep, double &X, double F_x, double D_x) {
		PreviousStep = Step;
		Step         = _getStep(F_x, D_x);
		X -= Step;
	}

	static bool _outOfRange(double X, double Min, double Max, double F_x, double D_x) {
		return ((X - Max) * D_x - F_x) * ((X - Min) * D_x - F_x) > 0.;
	}

	static bool _tooSlow(double F_x, double D_x, double PreviousStep) {
		return (std::fabs(2. * F_x) > std::fabs(PreviousStep * D_x));
	}

	template<bool DoBisectionIfSlow = true>
	static bool _shouldDoBisection(double X, double Min, double Max, double F_x, double D_x, double PreviousStep) {
		if constexpr (DoBisectionIfSlow) {
			// do bisection if out of range or decreasing too slow
			return _outOfRange(X, Min, Max, F_x, D_x) || _tooSlow(F_x, D_x, PreviousStep);
		} else {
			// do bisection only if out of range
			return _outOfRange(X, Min, Max, F_x, D_x);
		}
	}

	static void _maintainBracketZero(double X, double F_x, double &Min, double &Max) {
		if (F_x < 0.) { // maintain bracket around zero
			Min = X;
		} else {
			Max = X;
		}
	}

	bool _terminate(double Cur_X, double Cur_Fx) {
		double abs_Cur_Fx = std::fabs(Cur_Fx);
		// check for convergence
		bool stop         = false;
		if (_dynamicallyAdjustTolerance) {
			if (abs_Cur_Fx < _abs_F_ref / _factorDynamicTolerance) { stop = true; }
		} else {
			if (abs_Cur_Fx < _epsilon) { stop = true; }
		}
		if (stop) {
			double secondDerivative = _derivative(Cur_X);
			auto criticalPoint      = _determineCriticalPoint(secondDerivative);
			_returnObj.setSucceeded(
				Cur_X, criticalPoint, Cur_Fx, _counterIterations,
				coretools::str::toString("Newton-Raphson: Found root within ", _counterIterations,
										 " iterations. Absolute function value at end: ", abs_Cur_Fx, "."));
			return true;
		}
		return false;
	}

public:
	explicit TNewtonRaphson(Function Fun, Function Derivative)
		: _function(std::move(Fun)), _derivative(std::move(Derivative)) {}

	template<class Object, class Func> explicit TNewtonRaphson(Object &Obj, Func &Fun, Func &Der) {
		// constructor that takes a member function of an object
		_function   = [&Obj, &Fun](double Val) { return (Obj.*Fun)(Val); };
		_derivative = [&Obj, &Der](double Val) { return (Obj.*Der)(Val); };
	}

	// set convergence criteria
	void setMaxIterations(size_t MaxIterations) { _maxIterations = MaxIterations; }
	void setEpsilon(double Epsilon) { _epsilon = Epsilon; }

	void dynamicallyAdjustTolerance(double FactorPrecision = 1000.) {
		// functions that enables Newton-Raphson to dynamically adjust the tolerance for termination
		// idea: e.g. Newton-Raphson is used within another algorithm, e.g. EM -> we want to dynamically scale epsilon
		// depending on how "far" we are in the EM, i.e. don't be precise inside NR in the beginning and be precise in
		// the end as next iteration of EM will anyways change values

		// Newton-Raphson will compare the current function value f_x with a reference function value f_ref
		// where f_ref is e.g. the function value of the previous EM iteration
		// if those are very close -> i.e. we are already close to the previous estimate of the root,
		//   we want to have a low tolerance, i.e. we want to be very precise
		// if these are different -> i.e. the current estimate of the root is very different than the previous one
		//   we don't want to be too precise, and have a relatively high tolerance
		_dynamicallyAdjustTolerance = true;
		_factorDynamicTolerance     = FactorPrecision;
	}

	// find root
	template<bool AllowTerminateAfterBisection = true, bool DoBisectionIfSlow = true>
	bool runNewtonRaphson_withBisection(double Start, double Min, double Max) {
		// Returns the root of a function F using a combination of Newton-Raphson and bisection
		// Min and Max = initial boundaries of the function (for bisection)
		// root will be refined until the accuracy is known within +- Epsilon
		// Parameter F is the function where we want to find the root of (for MLE problems: this would be the first
		// derivative) Parameter D is the first derivative of F (for MLE problems: this would be the second derivative)
		// Parameter Obj is the class instance that contains functions F and D
		// Template AllowTerminateAfterBisection: should be set to false if function is known to converge towards zero
		//   -> do not call terminate function after bisection, only allow to terminate after Newton-Raphson step
		//   -> this ensures that we don't get stuck in the flat, almost zero converged part of the function, but
		//      instead   go towards the true zero location

		// if dynamically adjust tolerance: calculate function at _x_ref
		if (_dynamicallyAdjustTolerance) { _abs_F_ref = std::fabs(_function(Start)); }

		// check min and max
		double f_min = _function(Min);
		double f_max = _function(Max);
		if (!_checkMinMax(f_min, f_max, Min, Max)) { return _returnObj.converged(); }
		double x = Start;

		// Initial step size
		double step         = std::fabs(Max - Min);
		double previousStep = step;

		double f_x          = _function(x);
		double d_x          = _derivative(x);
		bool allowTerminate = false;

		for (_counterIterations = 0; _counterIterations < _maxIterations; _counterIterations++) {
			if (_shouldDoBisection<DoBisectionIfSlow>(x, Min, Max, f_x, d_x, previousStep)) {
				_doBisection(step, previousStep, x, Min, Max);
				if constexpr (!AllowTerminateAfterBisection) { allowTerminate = false; }
			} else {
				_doNewtonRaphson(step, previousStep, x, f_x, d_x);
				if constexpr (!AllowTerminateAfterBisection) { allowTerminate = true; }
			}

			// calculate function at new x and decide whether to terminate based on current function value
			f_x = _function(x);
			if constexpr (AllowTerminateAfterBisection) {
				if (_terminate(x, f_x)) { return _returnObj.converged(); }
			} else {
				if (allowTerminate && _terminate(x, f_x)) { return _returnObj.converged(); }
			}

			// calculate derivative at new x
			d_x = _derivative(x);

			_maintainBracketZero(x, f_x, Min, Max);
		}
		auto criticalPoint = _determineCriticalPoint(d_x);
		_returnObj.setFailed(
			coretools::str::toString(
				"Newton-Raphson with bisection: Failed to find root, reached maximum number of iterations (",
				_maxIterations, "). Absolute function value end: ", std::fabs(f_x), "."),
			reachMaxIterations, x, criticalPoint, f_x, _counterIterations);
		return _returnObj.converged();
	}

	bool runNewtonRaphson_withBacktracking(double Start) {
		// Returns the root of a function F using a combination of Newton-Raphson and backtracking
		// root will be refined until the accuracy is known within +- Epsilon
		// Parameter F is the function where we want to find the root of (for MLE problems: this would be the first
		// derivative) Parameter D is the first derivative of F (for MLE problems: this would be the second derivative)
		// Parameter Obj is the class instance that contains functions F and D

		// if dynamically adjust tolerance: calculate function at _x_ref
		if (_dynamicallyAdjustTolerance) { _abs_F_ref = std::fabs(_function(Start)); }

		double x    = Start;
		double oldX = x;
		double f_x  = 0.;
		double d_x  = 0.;

		for (_counterIterations = 0; _counterIterations < _maxIterations; _counterIterations++) {
			f_x         = _function(x);
			d_x         = _derivative(x);
			double step = f_x / d_x;
			oldX        = x;
			x -= step;

			double factor = 0.5;
			while (std::fabs(_function(x)) > std::fabs(f_x)) {
				// backtracking
				x = oldX - step * factor;
				factor /= 2.;
			}

			if (_terminate(x, f_x)) { return _returnObj.converged(); }
		}
		auto criticalPoint = _determineCriticalPoint(d_x);
		_returnObj.setFailed(
			"Newton-Raphson with backtracking: Failed to find root, reached maximum number of iterations (" +
				coretools::str::toString(_maxIterations) +
				"). Absolute function value end: " + coretools::str::toString(std::fabs(f_x)) + ".",
			reachMaxIterations, x, criticalPoint, f_x, _counterIterations);
		return _returnObj.converged();
	}

	// get-functions
	double root() { return _returnObj.result(); }
	const TReturnCode<double> &returnCode() { return _returnObj; }
	[[nodiscard]] size_t getCounterIterations() const { return _counterIterations; }
};

//---------------------------------
// Multi-Dimensional Newton-Raphson
//---------------------------------

template<size_t NDim = 0> class TMultiDimensionalNewtonRaphson {
	// this class implements a multidimensional globally-convergent Newton-Raphson algorithms
	// by preventing overshooting with backtracking
	// code adapted from Numerical Receipes, chapter 9.6 (page 479)
	// NDim = number of parameters to be optimized
	// If unknown at compile time: use NDim = 0
private:
	using VectorType = std::conditional_t<(NDim > 0), std::array<double, NDim>, std::vector<double>>;
	using Function   = std::function<std::pair<VectorType, bool>(coretools::TConstView<double>, size_t)>;
	using Jacobian   = std::function<arma::mat(coretools::TConstView<double>, coretools::TConstView<double>, size_t)>;

	// number of parameters to optimize
	size_t _size = 0;

	// function and jacobian
	Function _function;
	Jacobian _jacobian;

	// return object
	TReturnCode<VectorType> _returnObj;

	// when to stop?
	double _toleranceF                 = 1.e-8;  // convergence criterion on function values
	double _toleranceFMin              = 1.e-12; // criterion for spurious convergence to a minimum of fmin
	double _maxStepSize                = 100.;   // scaled maximum step length allowed in line searches;
	double _convergenceThresholdDeltaX = std::numeric_limits<double>::epsilon(); // convergence criterion on delta x
	size_t _maxIterations              = 200;
	size_t _counterIterations          = 0;
	bool _checkIfCriticalPointIsMin    = false; // when converged: should we check if critical point is indeed min?

	// dynamically adjust tolerance to terminate?
	bool _dynamicallyAdjustTolerance = false;
	double _max_absF_ref             = 0.0;
	double _factorDynamicTolerance   = 0.0;

	// valid intervals of parameters to be optimized
	VectorType _minValues;
	bool _hasDefaultMin = true;
	VectorType _maxValues;
	bool _hasDefaultMax = true;

	void _checkSizeMinMax(size_t Size) {
		if (!_hasDefaultMin && Size != _minValues.size()) {
			DEVERROR("Size of minimal values (", _minValues.size(), ") does not match size of parameters (", Size,
					 ")!");
		}
		if (!_hasDefaultMax && Size != _maxValues.size()) {
			DEVERROR("Size of maximal values (", _maxValues.size(), ") does not match size of parameters (", Size,
					 ")!");
		}
	}

	static void _restrictInitialStepSize(size_t N, arma::vec &Direction, double MaxStepSize) {
		// calculate vector norm of Direction -> sum corresponds to step size in NR
		assert(Direction.size() == N);
		double norm = coretools::vectorNorm(Direction);

		// check if step is too big
		if (norm > MaxStepSize) {
			for (size_t i = 0; i < N; i++) {
				Direction[i] *= MaxStepSize / norm; // scale if attempted step is too big
			}
		}
	}

	bool _calculateSlope(double &Slope, coretools::TConstView<double> Gradient, const arma::vec &Direction) {
		double slope = std::inner_product(Gradient.begin(), Gradient.end(), Direction.begin(), 0.0);
		if (slope >= 0.) {
			_returnObj.setFailed(
				"Multidimensional Newton-Raphson failed: Roundoff problem while backtracking, slope (" +
					coretools::str::toString(slope) + ") is >= 0.",
				numericalIssues, _counterIterations);
			return false;
		}
		Slope = slope;
		return true;
	}

	static double _calculateMinLambda(size_t N, const arma::vec &Direction, coretools::TConstView<double> X_Old) {
		// compute lambda_min
		double test = 0.;
		for (size_t i = 0; i < N; i++) {
			double temp = std::fabs(Direction[i]) / std::max(std::fabs(X_Old[i]), 1.);
			if (temp > test) { test = temp; }
		}

		return std::numeric_limits<double>::epsilon() / test;
	}

	std::pair<double, bool> _calculate_f(coretools::TConstView<double> X, VectorType &ValuesF, size_t Iteration) {
		// Calculates the function value at X as f = 1/2 FF
		// and stores F(x) in ValuesF
		bool valid;
		std::tie(ValuesF, valid) = _function(X, Iteration);
		if (!valid) {
			if constexpr (NDim == 0) { ValuesF.resize(_size, 0.0); }
			return {std::numeric_limits<double>::max(), false};
		}
		return {0.5 * coretools::sumOfSquares(ValuesF), true};
	}

	static void _makeStep(size_t N, VectorType &X_New, coretools::TConstView<double> X_Old, double Lambda,
						  const arma::vec &Direction) {
		for (size_t i = 0; i < N; i++) { X_New[i] = X_Old[i] + Lambda * Direction[i]; }
	}

	static double _calculateLambdaInterval(size_t i, double NewValue_i, coretools::TConstView<double> X_Old,
										   const arma::vec &Direction) {
		return (NewValue_i - X_Old[i]) / Direction[i];
	}

	void _backTrackToInterval(size_t N, VectorType &X_New, coretools::TConstView<double> X_Old, double Lambda,
							  const arma::vec &Direction) {
		if (_hasDefaultMin && _hasDefaultMax) { return; }

		for (size_t i = 0; i < N; i++) {
			if (!_hasDefaultMin && X_New[i] < _minValues[i]) {
				Lambda = std::min(Lambda, _calculateLambdaInterval(i, _minValues[i], X_Old, Direction));
			} else if (!_hasDefaultMax && X_New[i] > _maxValues[i]) {
				Lambda = std::min(Lambda, _calculateLambdaInterval(i, _maxValues[i], X_Old, Direction));
			}
		}
		_makeStep(N, X_New, X_Old, Lambda, Direction);
	}

	static bool _backTracking_convergedOnDeltaX(double Lambda, double MinLambda, VectorType &X_New,
												const VectorType &X_Old, bool &ConvergedOnDeltaX) {
		if (Lambda < MinLambda) {
			// convergence on delta x. For zero finding, the calling program should verify the convergence
			X_New             = X_Old;
			ConvergedOnDeltaX = true;
			return true;
		}
		return false;
	}

	static bool _backTracking_convergedOnFunctionValue(double F_New, double F_Old, double Lambda, double Slope,
													   double DecreaseFactorF) {
		return F_New <= F_Old + DecreaseFactorF * Lambda * Slope;
	}

	static double _backTrack_oneStep(bool Valid, bool PreviousWasValid, double F_New, double F_Old, double Lambda,
									 double Slope, double PreviousF, double PreviousLambda) {
		double tempLambda;
		if (!Valid) { // non-valid parameter values: no smart backtracking
			tempLambda = 0.5 * Lambda;
		} else if (!PreviousWasValid || Lambda == 1.) {
			// now you are valid but before you were not: only do semi-smart backtracking
			// or: first time you do backtracking (if you are valid from the start)
			tempLambda = -Slope / (2. * (F_New - F_Old - Slope));
			tempLambda = std::min(tempLambda, 0.5 * PreviousLambda);
		} else { // subsequent backtracks
			double rhs1 = F_New - F_Old - Lambda * Slope;
			double rhs2 = PreviousF - F_Old - PreviousLambda * Slope;
			double a =
				(rhs1 / (Lambda * Lambda) - rhs2 / (PreviousLambda * PreviousLambda)) / (Lambda - PreviousLambda);
			double b =
				(-PreviousLambda * rhs1 / (Lambda * Lambda) + Lambda * rhs2 / (PreviousLambda * PreviousLambda)) /
				(Lambda - PreviousLambda);
			if (a == 0.) {
				tempLambda = -Slope / (2. * b);
			} else {
				double disc = b * b - 3. * a * Slope;
				if (disc < 0.) {
					tempLambda = 0.5 * Lambda;
				} else if (b <= 0.) {
					tempLambda = (-b + sqrt(disc)) / (3. * a);
				} else {
					tempLambda = -Slope / (b + sqrt(disc));
				}
			}
			if (tempLambda > 0.5 * Lambda) {
				tempLambda = 0.5 * Lambda; // lambda <= 0.5*lambda_1
			}
		}

		if (std::isnan(tempLambda) || !std::isfinite(tempLambda)) { // avoid division by zero issues
			tempLambda = 0.5 * Lambda;
			if (std::isnan(tempLambda) || !std::isfinite(tempLambda)) { return 0.0; }
		}
		tempLambda           = std::max(tempLambda, 0.1 * Lambda); // lambda >= 0.1*lambda_1
		return tempLambda;
	}

	bool _backTrack(const VectorType &X_Old, double F_Old, coretools::TConstView<double> Gradient, arma::vec &Direction,
					VectorType &ValuesF, VectorType &X_New, double &F_New, double MaxStepSize, bool &ConvergedOnDeltaX,
					size_t Iteration) {
		/*
		 * Perform backtracking in the multidimensional Newton-Raphson algorithm
		 *
		 * @param X_Old: A n-dimensional point
		 * @param F_Old: Value of function F at point X_Old
		 * @param Gradient: Gradient at point X_Old
		 * @param Direction: Newton direction
		 *
		 * @param X_New: A new n-dimensional point along the direction p from X_Old where the function F has decreased
		 * sufficiently
		 * @param F_New: Value of function F at point X_New
		 *
		 * @param MaxStepSize: Limit the length of the steps so that you do not try to evaluate the function in regions
		 * where it is undefined or subject to overflow
		 * @param ConvergedOnDeltaX: False on a normal exit. True when X_New is too close to X_Old.
		 *                   In a minimization algorithm, this usually signals convergence and can be ignored.
		 *                   However, in a zero-finding algorithm the calling program should check whether
		 *                   the convergence is spurious.
		 */

		// settings
		const double decreaseFactorF = 1.e-4; // ensures sufficient decrease in function value
		const size_t n               = X_Old.size();
		ConvergedOnDeltaX            = false;

		// restrict initial step size
		_restrictInitialStepSize(n, Direction, MaxStepSize);

		// calculate slope
		double slope;
		if (!_calculateSlope(slope, Gradient, Direction)) { return false; }

		// calculate minimal lambda
		double minLambda = _calculateMinLambda(n, Direction, X_Old);

		double lambda         = 1.; // Always try full Newton step first
		double previousLambda = 0., previousF = 0.;
		bool valid            = false;
		bool previousWasValid = true;
		for (;;) { // start of loop
			// make step and get X_New
			_makeStep(n, X_New, X_Old, lambda, Direction);
			// check if intervals are valid, else backtrack directly to the interval boundary
			_backTrackToInterval(n, X_New, X_Old, lambda, Direction);
			std::tie(F_New, valid) = _calculate_f(X_New, ValuesF, Iteration);

			if (valid && _backTracking_convergedOnDeltaX(lambda, minLambda, X_New, X_Old, ConvergedOnDeltaX)) {
				return true;
			} else if (valid && _backTracking_convergedOnFunctionValue(F_New, F_Old, lambda, slope, decreaseFactorF)) {
				return true; // sufficient function decrease
			} else {         // backtrack
				lambda =
					_backTrack_oneStep(valid, previousWasValid, F_New, F_Old, lambda, slope, previousF, previousLambda);
			}

			previousLambda   = lambda;
			previousF        = F_New;
			previousWasValid = valid;
		} // try again
	}

	static double _adjustMaxStepSize(size_t N, coretools::TConstView<double> X, double MaxStepSize) {
		// Calculate maxStepSize for line search
		return MaxStepSize * std::max(coretools::vectorNorm(X), static_cast<double>(N));
	}

	static void _fillGradient(size_t N, const arma::mat &Jacobian, coretools::TConstView<double> ValuesF,
							  VectorType &Gradient) {
		for (size_t i = 0; i < N; i++) {
			// Compute gradient of f for line search
			double sum = 0.;
			for (size_t j = 0; j < N; j++) { sum += Jacobian(j, i) * ValuesF[j]; }
			Gradient[i] = sum;
		}
	}

	static void _fillDirection(size_t N, coretools::TConstView<double> ValuesF, arma::vec &Direction) {
		for (size_t i = 0; i < N; i++) {
			Direction[i] = -ValuesF[i]; // Right-hand side for linear equations
		}
	}

	static double _getMaxAbsF(size_t N, coretools::TConstView<double> ValuesF) {
		double maxAbsF = 0.;
		for (size_t i = 0; i < N; i++) {
			if (std::fabs(ValuesF[i]) > maxAbsF) { maxAbsF = std::fabs(ValuesF[i]); }
		}
		return maxAbsF;
	}

	[[nodiscard]] CriticalPoint _determineCriticalPoint(const arma::mat &Jacobian) const {
		if (!_checkIfCriticalPointIsMin) { return noCriticalPoint; }

		arma::vec eigval = arma::eig_sym(Jacobian);
		bool allPositive = true;
		bool allNegative = true;
		for (double val : eigval) {
			if (val > 0.) { allNegative = false; }
			if (val < 0.) { allPositive = false; }
		}
		if (allPositive) {
			return minimum;
		} else if (allNegative) {
			return maximum;
		} else {
			return saddlePoint;
		}
	}

	[[nodiscard]] CriticalPoint _determineCriticalPoint(coretools::TConstView<double> X,
														coretools::TConstView<double> ValuesF, size_t Iteration) const {
		if (!_checkIfCriticalPointIsMin) { return minimum; }

		// else: do check if we indeed found minimum (gradient can be zero for min, max or saddle point)
		arma::mat jacobian = _jacobian(X, ValuesF, Iteration);
		return _determineCriticalPoint(jacobian);
	}

	bool _convergedOnFunctionValues(size_t N, const VectorType &X, coretools::TConstView<double> ValuesF,
									double Tolerance, size_t Iteration) {
		// Test for initial guess being a root. Use more stringent test than simply toleranceF
		double maxAbsF = _getMaxAbsF(N, ValuesF);

		bool stop = false;
		if (_dynamicallyAdjustTolerance && maxAbsF < _max_absF_ref / _factorDynamicTolerance) {
			stop = true;
		} else if (!_dynamicallyAdjustTolerance && maxAbsF < Tolerance) {
			stop = true;
		}

		if (stop) {
			auto criticalPoint = _determineCriticalPoint(X, ValuesF, Iteration);
			_returnObj.setSucceeded(
				X, criticalPoint, maxAbsF, _counterIterations,
				coretools::str::toString("Multidimensional Newton-Raphson: Found root within ", _counterIterations,
										 " iterations, terminated based on convergence of function values. Maximum "
										 "absolute function value at end: ",
										 maxAbsF, "."));
			return true;
		}
		return false;
	}

	void _checkForSpuriousConvergence(size_t N, double f, coretools::TConstView<double> Gradient, const VectorType &X,
									  double ToleranceFMin, coretools::TConstView<double> ValuesF, size_t Iteration) {
		// Check for gradient of f zero, i.e., spurious convergence.
		double test = 0.;
		double den  = std::max(f, 0.5 * static_cast<double>(N));
		for (size_t i = 0; i < N; i++) {
			double temp = std::fabs(Gradient[i]) * std::max(std::fabs(X[i]), 1.) / den;
			if (temp > test) { test = temp; }
		}
		double maxAbsF = _getMaxAbsF(N, ValuesF);

		auto criticalPoint = _determineCriticalPoint(X, ValuesF, Iteration);

		if (test < ToleranceFMin) {
			// spurious convergence
			_returnObj.setFailed(coretools::str::toString("Multidimensional Newton-Raphson: Found root within ",
														  _counterIterations,
														  " iterations, terminated based on spurious convergence "
														  "(gradient of f is ~zero). Absolute function value at end: ",
														  maxAbsF, "."),
								 spuriousConvergence, X, criticalPoint, maxAbsF, _counterIterations);
		} else {
			_returnObj.setSucceeded(
				X, criticalPoint, maxAbsF, _counterIterations,
				coretools::str::toString(
					"Multidimensional Newton-Raphson: Found root within ", _counterIterations,
					" iterations, terminated based gradient of f that is ~zero. Absolute function value at end: ",
					maxAbsF, "."));
		}
	}

	bool _convergedOnDeltaX(size_t N, const VectorType &X, coretools::TConstView<double> X_Old,
							double ConvergenceThresholdDeltaX, coretools::TConstView<double> ValuesF,
							size_t Iteration) {
		double test = 0.; // test for convergence on deltaX
		for (size_t i = 0; i < N; i++) {
			double temp = (std::fabs(X[i] - X_Old[i])) / std::max(std::fabs(X[i]), 1.);
			if (temp > test) { test = temp; }
		}
		if (test < ConvergenceThresholdDeltaX) {
			double maxAbsF     = _getMaxAbsF(N, ValuesF);
			auto criticalPoint = _determineCriticalPoint(X, ValuesF, Iteration);
			_returnObj.setSucceeded(
				X, criticalPoint, maxAbsF, _counterIterations,
				coretools::str::toString("Multidimensional Newton-Raphson: Found root within ", _counterIterations,
										 " iterations, terminated based on convergence of deltaX (step size). Absolute "
										 "function value at end: ",
										 maxAbsF, "."));
			return true;
		}
		return false;
	}

public:
	explicit TMultiDimensionalNewtonRaphson(Function Fun, Jacobian Jac)
		// constructor that takes two std::functions
		: _function(std::move(Fun)), _jacobian(std::move(Jac)) {}

	template<class Object, class Func, class Jaco> TMultiDimensionalNewtonRaphson(Object &Obj, Func &Fun, Jaco &Jac) {
		// constructor that takes two member functions of an object
		_function = [&Obj, &Fun](coretools::TConstView<double> Vals, size_t Iter) { return (Obj.*Fun)(Vals, Iter); };
		_jacobian = [&Obj, &Jac](coretools::TConstView<double> Vals, coretools::TConstView<double> ValuesF,
								 size_t Iter) { return (Obj.*Jac)(Vals, ValuesF, Iter); };
	}

	template<class Object, class Func, class JacoObject, class Jaco>
	TMultiDimensionalNewtonRaphson(Object &Obj, Func &Fun, JacoObject &JObj, Jaco &Jac) {
		// constructor that takes two member functions of two objects (one for Fun and one for Jac)
		_function = [&Obj, &Fun](coretools::TConstView<double> Vals, size_t Iter) { return (Obj.*Fun)(Vals, Iter); };
		_jacobian = [&JObj, &Jac](coretools::TConstView<double> Vals, coretools::TConstView<double> ValuesF,
								  size_t Iter) { return (JObj.*Jac)(Vals, ValuesF, Iter); };
	}

	// set convergence criteria
	void doCheckIfCriticalPointIsMin() { _checkIfCriticalPointIsMin = true; }
	void setMaxIterations(size_t MaxIterations) { _maxIterations = MaxIterations; }
	void setToleranceF(double ToleranceF) { _toleranceF = ToleranceF; }
	void setToleranceFMin(double ToleranceFMin) { _toleranceFMin = ToleranceFMin; }

	// set intervals
	void setMinValues(const VectorType &Min) {
		_hasDefaultMin = false;
		_minValues     = Min;
	}

	void setMaxValues(const VectorType &Max) {
		_hasDefaultMax = false;
		_maxValues     = Max;
	}

	void dynamicallyAdjustTolerance(double FactorPrecision = 1000.) {
		// functions that enables Newton-Raphson to dynamically adjust the tolerance for termination
		// idea: e.g. Newton-Raphson is used within another algorithm, e.g. EM -> we want to dynamically scale epsilon
		// depending on how "far" we are in the EM, i.e. don't be precise inside NR in the beginning and be precise in
		// the end as next iteration of EM will anyways change values

		// Newton-Raphson will compare the current function value f_x with a reference function value f_ref
		// where f_ref is e.g. the function value of the previous EM iteration
		// if those are very close -> i.e. we are already close to the previous estimate of the root,
		//   we want to have a low tolerance, i.e. we want to be very precise
		// if these are different -> i.e. the current estimate of the root is very different than the previous one
		//   we don't want to be too precise, and have a relatively high tolerance
		_dynamicallyAdjustTolerance = true;
		_factorDynamicTolerance     = FactorPrecision;
	}

	bool runNewtonRaphson_withBacktracking(coretools::TConstView<double> Start) {
		/*
		 * Run a multidimensional Newton-Raphson algorithm (globally convergent)
		 * @param X: A n-dimensional point, representing an initial guess for a root
		 * @param ConvergedOnDeltaX: False on a normal exit. True if the routine has converged to a local minimum
		 * of the function. In this case try restarting from a different initial guess.
		 */
		_size = Start.size();
		_checkSizeMinMax(Start.size());

		// prepare storage
		size_t n = Start.size();
		arma::mat jacobian(n, n);
		arma::vec direction(n);
		VectorType X;
		if constexpr (NDim == 0) { X.resize(Start.size()); }
		std::transform(Start.cbegin(), Start.cend(), X.begin(), [](auto i) { return i; }); // copy from Start to X
		VectorType gradient;
		VectorType xOld;
		VectorType valuesF;
		if constexpr (NDim == 0) {
			gradient.resize(n);
			xOld.resize(n);
		}

		// Calculate initial f
		auto [f, valid] = _calculate_f(X, valuesF, 0);
		if (_dynamicallyAdjustTolerance) { _max_absF_ref = _getMaxAbsF(n, valuesF); }

		// Test for initial guess being a root; use more stringent test than simply toleranceF
		if (valid && _convergedOnFunctionValues(n, X, valuesF, 0.01 * _toleranceF, 0)) {
			return _returnObj.converged();
		}

		// adjust maxStepSize for line search
		_maxStepSize = _adjustMaxStepSize(n, X, _maxStepSize);

		for (_counterIterations = 0; _counterIterations < _maxIterations; _counterIterations++) {
			// fill Jacobian, gradient and direction
			jacobian = _jacobian(X, valuesF, _counterIterations);
			_fillGradient(n, jacobian, valuesF, gradient);
			_fillDirection(n, valuesF, direction);

			// store x and f
			xOld        = X;
			double fOld = f;

			// solve linear equations
			direction = arma::solve(jacobian, direction);

			// do line search (backtracking) to get new x and f
			bool convergedOnDeltaX = false;
			if (!_backTrack(xOld, fOld, gradient, direction, valuesF, X, f, _maxStepSize, convergedOnDeltaX,
							_counterIterations + 1)) {
				return _returnObj.converged();
			}

			// Check for convergence
			if (_convergedOnFunctionValues(n, X, valuesF, _toleranceF, _counterIterations + 1)) {
				// 1) test for convergence on function values
				return _returnObj.converged();
			} else if (convergedOnDeltaX) {
				// 2) line search reported spurious convergence -> check again and return
				_checkForSpuriousConvergence(n, f, gradient, X, _toleranceFMin, valuesF, _counterIterations + 1);
				return _returnObj.converged();
			} else if (_convergedOnDeltaX(n, X, xOld, _convergenceThresholdDeltaX, valuesF, _counterIterations + 1)) {
				// 3) test for convergence on deltaX
				return _returnObj.converged();
			}
		}

		auto criticalPoint = _determineCriticalPoint(jacobian);
		const auto max_abs_f = _getMaxAbsF(n, valuesF);
		_returnObj.setFailed(
			coretools::str::toString(
				"Multidimensional Newton-Raphson with backtracking: Failed to find root, reached maximum "
				"number of iterations (",
				_maxIterations, "). Absolute function value at end: ", max_abs_f, "."),
			reachMaxIterations, X, criticalPoint, max_abs_f, _counterIterations);
		return _returnObj.converged();
	}

	// getters
	VectorType root() { return _returnObj.result(); }
	const TReturnCode<VectorType> &returnCode() { return _returnObj; }
	[[nodiscard]] size_t getCounterIterations() const { return _counterIterations; }
};

template<size_t NDim = 0> class TApproxJacobian {
	/*
	 * If Jacobian matrix can not be calculated analytically,
	 * this class routine will attempt to compute the necessary partial derivatives of Function F
	 * numerically by finite differences.
	 *
	 * Algorithm adapted Numerical Recipes, Chapter 9.7.2 (Globally Convergent Newton Method) and Chapter 5.7 (Numerical
	 * derivatives)
	 */
private:
	using VectorType = std::conditional_t<(NDim > 0), std::array<double, NDim>, std::vector<double>>;
	using Function   = std::function<std::pair<VectorType, bool>(coretools::TConstView<double>, size_t)>;

	// function and jacobian
	Function _function;

	const double _epsilon = 1.e-8; // Set to approximate square root of the machine precision

public:
	explicit TApproxJacobian(Function Fun) : _function(std::move(Fun)) {
		// constructor that takes std::function
	}

	template<class Object, class Func> TApproxJacobian(Object &Obj, Func &Fun) {
		// constructor that takes a member function of an object
		_function = [&Obj, &Fun](coretools::TConstView<double> Vals, size_t Iter) { return (Obj.*Fun)(Vals, Iter); };
	}

	arma::mat approximateJacobian(coretools::TConstView<double> X, coretools::TConstView<double> ValuesF,
								  size_t Iteration) {
		/* Calculates the Jacobian array by finite differences
		 *
		 * @param X: A n-dimensional point at which the Jacobian is to be evaluated
		 * @param ValuesF: vector of function values at X
		 */
		size_t n = X.size();
		arma::mat jacobian(n, n);
		VectorType xh;
		std::copy(X.begin(), X.end(), std::back_inserter(xh));
		for (size_t j = 0; j < n; j++) {
			double temp = xh[j];
			double h    = _epsilon * std::fabs(temp);
			if (h == 0.) { h = _epsilon; }
			xh[j]           = temp + h; // trick to reduce finite-precision error
			h               = xh[j] - temp;
			auto [f, valid] = _function(xh, Iteration);
			if (!valid) { DEVERROR("Invalid parameter values used to calculate approximateJacobian!"); }
			xh[j] = temp;
			for (size_t i = 0; i < n; i++) { // forward difference formula
				jacobian(i, j) = (f[i] - ValuesF[i]) / h;
			}
		}
		return jacobian;
	}
};
} // end namespace stattools

#endif // TNEWTONRAPHSON_H
