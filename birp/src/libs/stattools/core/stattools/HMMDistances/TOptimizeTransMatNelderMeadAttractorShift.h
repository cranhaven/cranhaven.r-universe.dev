//
// Created by madleina on 02.11.22.
//

#ifndef TOPTIMIZETRANSMATNELDERMEADATTRACTOR_H
#define TOPTIMIZETRANSMATNELDERMEADATTRACTOR_H

#include "stattools/HMMDistances/TOptimizeTransMatNelderMead.h"

namespace stattools {

//---------------------------------------------
// TOptimizeTransMatNelderMeadAttractor
//---------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType, typename TypeTransMat>
class TOptimizeTransMatNelderMeadAttractorShift
	: public TOptimizeTransMatNelderMead<PrecisionType, NumStatesType, LengthType, TypeTransMat> {
private:
	using Base   = TOptimizeTransMatNelderMead<PrecisionType, NumStatesType, LengthType, TypeTransMat>;
	using VecTau = std::vector<arma::mat>;

	using Base::_minusQAtResult;
	using Base::_simplexOfPreviousEMIteration;
	using Base::_tau_dist1;

	bool _fixIxAttractor = false;

	auto _maximizeQFunction_fixAttractor(VecTau &Tau, VecTau &Xi, const std::vector<std::vector<PrecisionType>> &Gamma,
										 bool FirstIteration, bool LastIteration) {
		// run Nelder-Mead for current attractor
		auto values = Base::maximizeQFunction(Tau, Xi, Gamma, FirstIteration, LastIteration);

		// append ix to values
		values.push_back(_tau_dist1.ix());
		return values;
	}

	auto _maximizeQFunction_estimateAttractor(VecTau &Tau, VecTau &Xi,
											  const std::vector<std::vector<PrecisionType>> &Gamma, bool FirstIteration,
											  bool LastIteration) {
		TSimplex initSimplex = _simplexOfPreviousEMIteration;

		// initialize minimum at very high value
		// Note: Nelder-Mead minimizes -> we calculate -Q -> low values are better!
		//       -> we want the attractor that minimizes -Q, as this corresponds to maximizing Q
		std::vector<PrecisionType> bestValues;
		size_t bestIx = 0;
		TSimplex bestSimplex;
		double min = std::numeric_limits<double>::max();

		// loop over all attractors
		for (size_t s = 0; s < this->numStates(); s++) {
			_simplexOfPreviousEMIteration = initSimplex;
			_tau_dist1.setIxAttractor(s);

			// run Nelder-Mead for that attractor
			auto values = Base::maximizeQFunction(Tau, Xi, Gamma, FirstIteration, LastIteration);
			// update minimum
			if (_minusQAtResult < min) {
				bestValues  = values;
				bestIx      = s;
				bestSimplex = _simplexOfPreviousEMIteration;
				min         = _minusQAtResult;
			}
		}

		_simplexOfPreviousEMIteration = bestSimplex;
		_tau_dist1.setIxAttractor(bestIx);
		this->fillProbabilities(bestValues, Tau);

		// append ix to bestValues
		bestValues.push_back(bestIx);
		return bestValues;
	}

public:
	TOptimizeTransMatNelderMeadAttractorShift(size_t NumStates) : Base(NumStates){};
	TOptimizeTransMatNelderMeadAttractorShift(size_t NumStates, PrecisionType Displacement)
		: Base(NumStates, Displacement){};

	~TOptimizeTransMatNelderMeadAttractorShift() override = default;

	void fixAttractorIx() { _fixIxAttractor = true; }

	void setAttractorIx(size_t ix) {
		assert(ix < this->numStates());
		_tau_dist1.setIxAttractor(ix);
	}

	std::vector<PrecisionType> maximizeQFunction(VecTau &Tau, VecTau &Xi,
												 const std::vector<std::vector<PrecisionType>> &Gamma,
												 bool FirstIteration, bool LastIteration) override {
		if (_fixIxAttractor) {
			return _maximizeQFunction_fixAttractor(Tau, Xi, Gamma, FirstIteration, LastIteration);
		} else {
			return _maximizeQFunction_estimateAttractor(Tau, Xi, Gamma, FirstIteration, LastIteration);
		}
	};

	std::vector<PrecisionType> setValuesEM(coretools::TConstView<PrecisionType> InitVertex, VecTau &Tau) override {
		std::vector<PrecisionType> values = Base::setValuesEM(InitVertex, Tau);

		// append index
		values.push_back(_tau_dist1.ix());
		return values;
	};
};

} // namespace stattools

#endif // TOPTIMIZETRANSMATNELDERMEADATTRACTOR_H
