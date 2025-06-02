//
// Created by madleina on 17.02.21.
//

#ifndef TNELDERMEAD_H
#define TNELDERMEAD_H

#include "coretools/Containers/TView.h"
#include "coretools/Strings/toString.h"
#include "stattools/MLEInference/TReturnCodes.h"
#include <array>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <type_traits>
#include <vector>

namespace stattools {

// Normally we know dimension at compile time. N = 0 -> dynamic
template<size_t NDim = 0> class TVertex {
public:
	using VectorType = std::conditional_t<(NDim > 0), std::array<double, NDim>, std::vector<double>>;

private:
	VectorType _vertex{};
	double _value = 0.0;

public:
	TVertex() = default;

	explicit TVertex(size_t Size) : _vertex(Size, 0.) { static_assert(NDim == 0); } // Only makes sence in dynamic mode

	explicit TVertex(coretools::TConstView<double> Vertex) : _vertex(Vertex.begin(), Vertex.end()) {
		if constexpr (NDim) assert(Vertex.size() == NDim);
	}

	void resize(size_t NumDim) {
		static_assert(NDim == 0); // Only makes sence in dynamic mode
		_vertex.resize(NumDim, 0.);
		_value = 0.;
	}

	double &operator[](size_t Dim) noexcept {
		assert(Dim < _vertex.size());
		return _vertex[Dim];
	}

	double operator[](size_t Dim) const noexcept {
		assert(Dim < _vertex.size());
		return _vertex[Dim];
	}

	double &value() noexcept { return _value; }

	[[nodiscard]] double value() const noexcept { return _value; }

	[[nodiscard]] const VectorType &coordinates() const noexcept { return _vertex; }

	[[nodiscard]] size_t numDim() const noexcept { return _vertex.size(); }

	void clear() {
		if constexpr (NDim == 0)
			_vertex.clear();
		else
			_vertex.fill(0.);
		_value = 0.;
	}
};

template<size_t NDim = 0> class TSimplex {
	using Vertex     = TVertex<NDim>;
	using VectorType = std::conditional_t<(NDim > 0), std::array<Vertex, NDim + 1>, std::vector<Vertex>>;

	VectorType _simplex;

public:
	TSimplex() = default;

	TSimplex(coretools::TConstView<double> Init, double Del) {
		if constexpr (NDim == 0) resize(Init.size());
		assert(Init.size() == numDim());

		for (size_t i = 0; i < numDim(); i++) { _simplex.front()[i] = Init[i]; }
		for (size_t vertex = 1; vertex < numVertices(); vertex++) {
			for (size_t i = 0; i < numDim(); i++) {
				_simplex[vertex][i] = Init[i]; // start by setting each vertex to the same values as initialVertex
			}
			_simplex[vertex][vertex - 1] += Del; // displace one value of the vertex
		}
	}

	TSimplex(coretools::TConstView<double> Init, coretools::TConstView<double> Dels) {
		assert(Init.size() == Dels.size());
		if constexpr (NDim == 0) resize(Init.size());
		assert(Init.size() == numDim());

		for (size_t i = 0; i < numDim(); i++) { _simplex.front()[i] = Init[i]; }
		for (size_t vertex = 1; vertex < numVertices(); vertex++) {
			for (size_t i = 0; i < numDim(); i++) {
				_simplex[vertex][i] = Init[i]; // start by setting each vertex to the same values as initialVertex
			}
			_simplex[vertex][vertex - 1] += Dels[vertex - 1]; // displace one value of the vertex
		}
	}

	void resize(size_t NumDim) {
		static_assert(NDim == 0); // Only makes sence in dynamic mode
		_simplex.resize(NumDim + 1, Vertex(NumDim));
	}

	// access simplex coordinates
	Vertex &operator[](size_t Vertex) noexcept {
		assert(Vertex < numVertices());
		return _simplex[Vertex];
	}

	const Vertex &operator[](size_t Vertex) const noexcept {
		assert(Vertex < numVertices());
		return _simplex[Vertex];
	}

	// access simplex function values
	double &value(size_t Vertex) noexcept {
		assert(Vertex < numDim());
		return _simplex[Vertex].value();
	}

	[[nodiscard]] double value(size_t Vertex) const noexcept {
		assert(Vertex < numVertices());
		return _simplex[Vertex].value();
	}

	// get simplex dimensions
	[[nodiscard]] size_t numVertices() const noexcept { return _simplex.size(); }

	[[nodiscard]] size_t numDim() const noexcept { return _simplex.front().numDim(); }

	void clear() {
		for (auto &vertex : _simplex) { vertex.clear(); }
	}
};

template<size_t NDim = 0> class TNelderMead {
	// Multidimensional minimization by the downhill simplex method of Nelder and Mead.
	// code adapted from Numerical Receipes 3rd edition, pp. 504 (Section 10.5 Downhill Simplex Method in
	// Multidimensions)
	using Vertex     = TVertex<NDim>;
	using Simplex    = TSimplex<NDim>;
	using VectorType = typename Vertex::VectorType;
	using Function   = std::function<double(coretools::TConstView<double>)>;

	Function _fun;

	// temporary values
	Simplex _simplex;

	// termination criteria
	double _fractionalConvergenceTolerance = 10e-15;
	size_t _counterFuncEvaluations         = 0;     // the number of function evaluations
	size_t _maxNumFuncEvaluations          = 20000; // maximal number of function evaluations

	// dynamically adjust tolerance to terminate?
	bool _dynamicallyAdjustTolerance                = false;
	double _valueToCompareToleranceTo               = 0.0; // dynamically adjust tolerance
	double _factorDynamicTolerance                  = 0.0;
	size_t _numFunctionCallsUntilAdjustingTolerance = 0;

	// return
	TReturnCode<Vertex> _returnObj;

	// utility functions
	void _sumSimplex_perDim(VectorType &SumSimplex_PerDim) const {
		// returns vector of size N, where each element_i represents the sum of values over all vertices at dimension i
		// used to calculate centroid (in a later step)
		// vec[i] = sum_{v=1}^{N+1} x_{vi}
		for (size_t i = 0; i < _simplex.numDim(); i++) {
			double sum = 0.;
			for (size_t vertex = 0; vertex < _simplex.numVertices(); vertex++) { sum += _simplex[vertex][i]; }
			SumSimplex_PerDim[i] = sum;
		}
	}

	std::tuple<size_t, size_t, size_t> _assignIndices() {
		// determine which point is the highest (worst), next-highest (second-worst), and lowest (best), by looping over
		// the points in the simplex
		size_t IndexLowestVertex, IndexNextHighestVertex, IndexHighestVertex;
		IndexLowestVertex = 0;
		if (_simplex[0].value() > _simplex[1].value()) {
			IndexHighestVertex     = 0;
			IndexNextHighestVertex = 1;
		} else {
			IndexHighestVertex     = 1;
			IndexNextHighestVertex = 0;
		}
		for (size_t vertex = 0; vertex < _simplex.numVertices(); vertex++) {
			if (_simplex[vertex].value() <= _simplex[IndexLowestVertex].value()) { IndexLowestVertex = vertex; }
			if (_simplex[vertex].value() > _simplex[IndexHighestVertex].value()) {
				IndexNextHighestVertex = IndexHighestVertex;
				IndexHighestVertex     = vertex;
			} else if (_simplex[vertex].value() > _simplex[IndexNextHighestVertex].value() &&
			           vertex != IndexHighestVertex) {
				IndexNextHighestVertex = vertex;
			}
		}
		return {IndexLowestVertex, IndexNextHighestVertex, IndexHighestVertex};
	}

	[[nodiscard]] static double _calculateFractionalRange(double Val1, double Val2) {
		return 2.0 * std::fabs(Val1 - Val2) /
		       (std::fabs(Val1) + std::fabs(Val2) + 10e-10); // add 10e-10 to avoid division by zero
	}

	void _adjustTolerance(size_t IndexLowestVertex) {
		// compare current best value with _valueToCompareToleranceTo (this is e.g. the value of the best vertex of a
		// previous Nelder-Mead run)
		const double fractionalRange =
		    _calculateFractionalRange(_valueToCompareToleranceTo, _simplex[IndexLowestVertex].value());
		// we want to be e.g. 1000x more precise than this difference -> multiply with e.g. 0.001
		_fractionalConvergenceTolerance = fractionalRange * _factorDynamicTolerance;

		// only adjust once, i.e. set bool to false
		_dynamicallyAdjustTolerance = false;
	}

	bool _terminate(size_t IndexLowestVertex, size_t IndexHighestVertex) {
		// check if we should adjust tolerance
		if (_dynamicallyAdjustTolerance && _counterFuncEvaluations >= _numFunctionCallsUntilAdjustingTolerance) {
			_adjustTolerance(IndexLowestVertex);
		}
		// calculate fractional range, i.e. difference between best and worst function value
		const double fractionalRange =
		    _calculateFractionalRange(_simplex[IndexHighestVertex].value(), _simplex[IndexLowestVertex].value());
		if (fractionalRange < _fractionalConvergenceTolerance &&
		    _counterFuncEvaluations > _numFunctionCallsUntilAdjustingTolerance) { // terminate!
			const auto best = _getBestVertex(IndexLowestVertex);
			_returnObj.setSucceeded(best, minimum, _simplex[IndexLowestVertex].value(), _counterFuncEvaluations,
			                        "In Nelder-Mead algorithm: Found minimum within " +
			                            coretools::str::toString(_counterFuncEvaluations) + " function evaluations.");
			return true;
		} else { // don't terminate yet
			// check if we reached maximal number of function evaluations
			if (_counterFuncEvaluations >= _maxNumFuncEvaluations) {
				const auto best = _getBestVertex(IndexLowestVertex);
				_returnObj.setFailed(
				    "In Nelder-Mead algorithm: Failed to find minimum. Reached maximum number of "
				    "function evaluations (" +
				        coretools::str::toString(_maxNumFuncEvaluations) +
				        ")! Fractional range at end: " + coretools::str::toString(fractionalRange) + ".",
				    reachMaxIterations, best, minimum, _simplex[IndexLowestVertex].value(), _counterFuncEvaluations);
				return true;
			}
			return false;
		}
	}

	Vertex _getBestVertex(size_t IndexLowestVertex) {
		// Put best point and value in slot 0.
		std::swap(_simplex[0].value(), _simplex[IndexLowestVertex].value());
		for (size_t i = 0; i < _simplex.numDim(); i++) { std::swap(_simplex[0][i], _simplex[IndexLowestVertex][i]); }
		return _simplex[0];
	}

	double _reflect(VectorType &SumSimplex_PerDim, size_t IndexHighestVertex) {
		// First extrapolate by a factor -1 through the face of the simplex across from the high point, i.e. reflect the
		// simplex from the high point.
		return _amotry(SumSimplex_PerDim, IndexHighestVertex, -1.);
	}

	double _expand(VectorType &SumSimplex_PerDim, size_t IndexHighestVertex) {
		// Try an additional extrapolation by a factor 2
		return _amotry(SumSimplex_PerDim, IndexHighestVertex, 2.0);
	}

	double _contract(VectorType &SumSimplex_PerDim, size_t IndexHighestVertex) {
		// do a one-dimensional contraction
		return _amotry(SumSimplex_PerDim, IndexHighestVertex, 0.5);
	}

	void _shrink(VectorType &SumSimplex_PerDim, size_t IndexLowestVertex) {
		// contract around the lowest (best) point
		for (size_t vertex = 0; vertex < _simplex.numVertices(); vertex++) {
			if (vertex != IndexLowestVertex) { // exclude best vertex
				for (size_t i = 0; i < _simplex.numDim(); i++) {
					_simplex[vertex][i] = SumSimplex_PerDim[i] =
					    0.5 * (_simplex[vertex][i] + _simplex[IndexLowestVertex][i]);
				}
				_simplex[vertex].value() = _fun(SumSimplex_PerDim);
			}
		}
	}

	double _amotry(VectorType &SumSimplex_PerDim, size_t IndexHighestVertex, double Factor) {
		// Helper function: Extrapolates by a factor Factor through the face of the simplex across from the high point,
		// tries it, and replaces the high point if the new point is better.
		VectorType newPoint;
		if constexpr (NDim == 0) newPoint.resize(_simplex.numDim());

		// the following calculating is equivalent to
		// x_r = x_o + alpha(x_o - x_{N+1}) from the script if alpha = -Factor
		const double fac1 = (1. - Factor) / (double)_simplex.numDim();
		const double fac2 = fac1 - Factor;
		for (size_t i = 0; i < _simplex.numDim(); i++) {
			newPoint[i] = SumSimplex_PerDim[i] * fac1 - _simplex[IndexHighestVertex][i] * fac2;
		}
		double valueAtNewPoint = _fun(newPoint); // evaluate the function at the trial point
		// avoid issues if value is invalid (nan or infinite): make it huuuuge, should always be worse
		if (std::isnan(valueAtNewPoint) || !std::isfinite(valueAtNewPoint)) {
			valueAtNewPoint = std::numeric_limits<double>::max();
		}
		if (valueAtNewPoint < _simplex[IndexHighestVertex]
		                          .value()) { // if it’s better than the highest (worst point), then replace the highest
			_simplex[IndexHighestVertex].value() = valueAtNewPoint;
			for (size_t i = 0; i < _simplex.numDim(); i++) {
				SumSimplex_PerDim[i] +=
				    newPoint[i] - _simplex[IndexHighestVertex][i]; // update sum (add new and subtract old)
				_simplex[IndexHighestVertex][i] = newPoint[i];
			}
		}
		return valueAtNewPoint;
	}

	void _minimize() {
		// calculate function values at all vertices
		for (size_t vertex = 0; vertex < _simplex.numVertices(); vertex++) {
			_simplex[vertex].value() = _fun(_simplex[vertex].coordinates());
		}

		// initialize temporary values
		VectorType sumSimplex_perDim; // sum of all vertices values per dimension
		if constexpr (NDim == 0) { sumSimplex_perDim.resize(_simplex.numDim()); }

		// start algorithm
		_counterFuncEvaluations = 0;
		_sumSimplex_perDim(sumSimplex_perDim);
		for (;;) { // eternal loop until we break
			// first we must determine which point is the highest (worst), next-highest (second-worst), and lowest
			// (best)
			const auto [indexLowestVertex, indexNextHighestVertex, indexHighestVertex] = _assignIndices();

			// Check if we should terminate: Compute the fractional range from highest to lowest and return if
			// satisfactory.
			if (_terminate(indexLowestVertex, indexHighestVertex)) break;

			// precautionary add two function evaluations for function calls to _reflect() and _expand() (not executed
			// yet)
			_counterFuncEvaluations += 2;

			// first reflect through the face of the simplex across from the high point, i.e. reflect the simplex from
			// the high point
			double valueAtNewPoint = _reflect(sumSimplex_perDim, indexHighestVertex);

			if (valueAtNewPoint <= _simplex[indexLowestVertex].value()) {
				// gives a result better than the best point, so try an additional extrapolation
				_expand(sumSimplex_perDim, indexHighestVertex);
			} else if (valueAtNewPoint >= _simplex[indexNextHighestVertex].value()) {
				// the reflected point is worse than the second-highest, so look for an intermediate lower point, i.e.,
				// do a one-dimensional contraction
				const double valueAtWorstPoint = _simplex[indexHighestVertex].value();
				valueAtNewPoint                = _contract(sumSimplex_perDim, indexHighestVertex);
				if (valueAtNewPoint >= valueAtWorstPoint) {
					// still worse than worst point. Can’t seem to get rid of that high point.
					// better contract around the lowest (best) point
					_shrink(sumSimplex_perDim, indexLowestVertex);

					_sumSimplex_perDim(sumSimplex_perDim);        // recompute sumVerticesPerDimension
					_counterFuncEvaluations += _simplex.numDim(); // keep track of function evaluations: we had to do N
					                                              // extra function calls for shrinking
				}
			} else {
				--_counterFuncEvaluations; // Correct the evaluation count (we didn't expand)
			}
		} // Go back for the test of doneness and the next iteration
	}

public:
	explicit TNelderMead(Function Fun) : _fun(std::move(Fun)) {
		// constructor that takes a std::function
	}

	template<class Object, class Func, class... AdditionalArguments>
	TNelderMead(Object &Obj, Func &Fun, AdditionalArguments &...AdditionalArgs) {
		// constructor that takes a member function of an object
		_fun = [&Obj, &Fun, &AdditionalArgs...](coretools::TConstView<double> Vals) {
			return (Obj.*Fun)(Vals, AdditionalArgs...);
		};
	}

	// set convergence criteria
	void setFractionalConvergenceTolerance(double FractionalConvergenceTolerance) {
		_fractionalConvergenceTolerance = FractionalConvergenceTolerance;
	}

	void setMaxNumFunctionEvaluations(size_t MaxNumFunctionEvaluations) {
		_maxNumFuncEvaluations = MaxNumFunctionEvaluations;
	}

	void dynamicallyAdjustTolerance(double ValueToCompareTo, double FactorPrecision,
	                                size_t NumFunctionCallsUntilAdjustment) {
		// functions that enables Nelder-Mead to dynamically adjust the tolerance for termination
		// idea: e.g. Nelder-Mead is used within another algorithm, e.g. EM -> we want to dynamically scale tolerance
		// depending on how "far" we are in the EM, i.e. don't be precise inside NM in the beginning and be precise in
		// the end as next iteration of EM will anyways change values

		// after NumFunctionCallsUntilAdjustment function calls, Nelder-Mead will compare the current best value with
		// ValueToCompareTo (this is typically the function value of the best vertex of the last NM-Run) if those are
		// very close -> i.e. we are already close to where we were in a previous run of Nelder-Mead, we want to have a
		// low tolerance, i.e. we want to be very precise if we are far away -> i.e. we already made far jumps compared
		// to where we were before, we don't want to be too precise, and have a relatively high tolerance
		// -> tolerance scales with distance of current Nelder-Mead to previous Nelder-Mead
		_dynamicallyAdjustTolerance              = true;
		_valueToCompareToleranceTo               = ValueToCompareTo;
		_factorDynamicTolerance                  = FactorPrecision;
		_numFunctionCallsUntilAdjustingTolerance = NumFunctionCallsUntilAdjustment;
	}

	// minimize functions
	bool minimize(const Simplex &Init) {
		_simplex = Init;
		_minimize();
		return _returnObj.converged();
	}

	bool minimize(coretools::TConstView<double> Init, double Del) { return minimize(Simplex(Init, Del)); }
	bool minimize(coretools::TConstView<double> Init, coretools::TConstView<double> Dels) {
		return minimize(Simplex(Init, Dels));
	}

	// get-functions
	const VectorType &coordinates() { return _returnObj.result().coordinates(); }
	const TReturnCode<Vertex> &returnCode() { return _returnObj; }
	[[nodiscard]] const Simplex &getCurrentSimplex() const { return _simplex; }
	[[nodiscard]] size_t getCounterFunctionEvaluations() const { return _counterFuncEvaluations; }
};

} // end namespace stattools

#endif // TNELDERMEAD_H
