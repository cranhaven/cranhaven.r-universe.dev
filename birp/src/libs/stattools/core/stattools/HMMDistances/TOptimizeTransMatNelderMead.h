//
// Created by caduffm on 10/31/22.
//

#ifndef TOPTIMIZETRANSMATNELDERMEAD_H
#define TOPTIMIZETRANSMATNELDERMEAD_H

#include <cmath>
#include <vector>
#include "coretools/arma_include.h"
#include "coretools/Containers/TView.h"
#include "coretools/Files/TOutputFile.h"
#include "stattools/HMMDistances/TTransitionMatrixDistancesExponential.h"
#include "stattools/MLEInference/TNelderMead.h"

namespace stattools {

//-------------------------------------------
// TOptimizeTransMatNelderMead
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType, typename TypeTransMat>
class TOptimizeTransMatNelderMead
	: public TTransitionMatrixDistancesExponential<PrecisionType, NumStatesType, LengthType, TypeTransMat> {
protected:
	using Base   = TTransitionMatrixDistancesExponential<PrecisionType, NumStatesType, LengthType, TypeTransMat>;
	using VecTau = std::vector<arma::mat>;
	// declare member variables of base class
	using Base::_tau_dist1;

	// settings: run Nelder-Mead in each EM iteration or only in very last one?
	bool _runNelderMeadInEachEMIteration = true;

	// temporary variables for Nelder-Mead
	TSimplex<> _simplexOfPreviousEMIteration   = {};
	PrecisionType _initDisplacement            = 0.1;
	size_t _totalNumberOfFunctionEvaluationsNM = 0;
	size_t _iteration                          = 0;
	double _minusQAtResult                     = 0.0;

	void _setSimplex() {
		auto initial                  = _tau_dist1.getStartValues();
		_simplexOfPreviousEMIteration = TSimplex<>(_tau_dist1.transformToNelderMeadSpace(initial), _initDisplacement);
	}

	PrecisionType _calcSumQTransitionMatrix(const VecTau &Tau, VecTau &Xi) {
		double sum = 0.;
		for (size_t d = 1; d < Tau.size(); d++) {
			// skip first distance group (= initial probability, estimated separately)
			for (NumStatesType i = 0; i < _tau_dist1.numStates(); i++) {     // z_{t-1}
				for (NumStatesType j = 0; j < _tau_dist1.numStates(); j++) { // z_t
					double t = Tau[d](i, j);
					if (t == 0.0) { t = std::numeric_limits<double>::min(); }
					sum += Xi[d](i, j) * log(t);
				}
			}
		}
		return sum;
	}

	PrecisionType _calcQTransitionMatrix_NelderMead(coretools::TConstView<PrecisionType> Values, VecTau &Tau,
													VecTau &Xi) {
		// calculate sum_{i=1}^N sum_{z_i} sum_{z_{i-1}} xi(z_i, z_{i-1}) log P(z_i|z_{i-1})
		// (relevant term for transition probabilities from the Q-function)

		// back-transform values from Nelder-Mead space to normal space
		std::vector<PrecisionType> transformed = _tau_dist1.transformFromNelderMeadSpace(Values);

		// fill transition matrices
		if (!this->fillProbabilities(transformed, Tau)) { return std::numeric_limits<PrecisionType>::max(); }

		// calculate sum and return -sum, because we want to maximize Q, but Nelder-Mead minimizes
		return -_calcSumQTransitionMatrix(Tau, Xi);
	};

	TVertex<> _runNelderMead_OneEMIteration(VecTau &Tau, VecTau &Xi, bool FirstIteration, bool AdjustTolerance,
											bool Report) {
		// start Nelder-Mead with simplex of previous iteration
		auto ptr = &TOptimizeTransMatNelderMead::_calcQTransitionMatrix_NelderMead;
		TNelderMead nelderMead(*this, ptr, Tau, Xi);
		if (FirstIteration) {
			// set low tolerance and maxNumFunctionEvaluations, as this is the first iteration
			// and EM will change values in next iteration anyways
			nelderMead.setFractionalConvergenceTolerance(10e-5);
			nelderMead.setMaxNumFunctionEvaluations(1000);
		} else if (AdjustTolerance) {
			nelderMead.dynamicallyAdjustTolerance(_simplexOfPreviousEMIteration[0].value(), 1e-10, 50);
		}
		nelderMead.minimize(_simplexOfPreviousEMIteration);
		if (this->doReport() && Report) { coretools::instances::logfile().list(nelderMead.returnCode().message()); }

		// store simplex to start next EM iteration with this
		_simplexOfPreviousEMIteration = nelderMead.getCurrentSimplex();
		_totalNumberOfFunctionEvaluationsNM += nelderMead.getCounterFunctionEvaluations();
		_minusQAtResult = nelderMead.returnCode().valueAtResult();

		return nelderMead.returnCode().result();
	};

	std::vector<PrecisionType> _runNelderMead(VecTau &Tau, VecTau &Xi, bool FirstIteration, bool AdjustTolerance,
											  bool Report) {
		// maximize the transition-part of the Q-function numerically with Nelder-Mead
		_iteration++;
		TVertex peak = _runNelderMead_OneEMIteration(Tau, Xi, FirstIteration, AdjustTolerance, Report);

		// back-transform values from Nelder-Mead space to normal space
		return _tau_dist1.transformFromNelderMeadSpace(peak.coordinates());
	};

	std::vector<PrecisionType> _maximizeQFunction_LastIteration(VecTau &Tau, VecTau &Xi) {
		// don't adjust tolerance, we want to be precise now
		auto result = _runNelderMead(Tau, Xi, false, false, true);
		if (this->doReport()) {
			coretools::instances::logfile().list("Ran a total of ", _totalNumberOfFunctionEvaluationsNM,
												  " Nelder-Mead function evaluations over the entire EM algorithm.");
		}
		return result;
	};

	void _printQSurface(std::string_view Filename, VecTau &Tau, VecTau &Xi,
						const std::array<std::vector<PrecisionType>, 2> Grid) {
		std::vector<std::vector<PrecisionType>> QSurface(Grid[0].size(), std::vector<PrecisionType>(Grid[1].size()));
		for (size_t i = 0; i < Grid[0].size(); i++) {
			for (size_t j = 0; j < Grid[1].size(); j++) {
				double v1 = Grid[0][i];
				double v2 = Grid[1][j];

				std::vector<PrecisionType> transformed = _tau_dist1.transformToNelderMeadSpace({v1, v2});
				QSurface[i][j]                         = _calcQTransitionMatrix_NelderMead(transformed, Tau, Xi);
			}
		}

		// print to file
		coretools::TOutputFile file(Filename);
		for (size_t i = 0; i < Grid[0].size(); i++) { file.writeln(QSurface[i]); }
		file.close();
	}

public:
	TOptimizeTransMatNelderMead(size_t NumStates) : Base(NumStates) { _setSimplex(); };
	TOptimizeTransMatNelderMead(TypeTransMat Tau) : Base(Tau) { _setSimplex(); };
	TOptimizeTransMatNelderMead(size_t NumStates, PrecisionType Displacement) : Base(NumStates) {
		_initDisplacement = Displacement;
		_setSimplex();
	};

	~TOptimizeTransMatNelderMead() override = default;

	std::vector<PrecisionType> maximizeQFunction(VecTau &Tau, VecTau &Xi,
												 const std::vector<std::vector<PrecisionType>> &Gamma,
												 bool FirstIteration, bool LastIteration) override {
		if (LastIteration) { return _maximizeQFunction_LastIteration(Tau, Xi); }
		if (_runNelderMeadInEachEMIteration) { return _runNelderMead(Tau, Xi, FirstIteration, true, true); }

		// don't run Nelder-Mead (yet): just estimate transition matrix but not its parameters
		for (size_t d = 1; d < Tau.size(); d++) {
			for (NumStatesType i = 0; i < _tau_dist1.numStates(); i++) {     // z_{t-1}
				for (NumStatesType j = 0; j < _tau_dist1.numStates(); j++) { // z_t
					if (Xi[d](i, j) == 0) {                                  // avoid nan
						Tau[d](i, j) = (Xi[d](i, j) + 1) / (Gamma[d][i] + 1);
					} else {
						Tau[d](i, j) = Xi[d](i, j) / Gamma[d][i];
					}
				}
			}
		}
		return {};
	};

	std::vector<PrecisionType> setValuesEM(coretools::TConstView<PrecisionType> InitVertex, VecTau &Tau) override {
		assert(!InitVertex.empty());

		this->fillProbabilities(InitVertex, Tau);

		// transform initial estimate to Nelder-Mead space
		// idea: there might be certain restrictions on the parameters that Nelder-Mead should optimize,
		//        e.g. they must be positive.
		//        -> transform them (e.g. with log), and let Nelder-Mead operate on that transformed space,
		//           such that values are always valid after back-transformation
		_simplexOfPreviousEMIteration =
			TSimplex<>(_tau_dist1.transformToNelderMeadSpace(InitVertex), _initDisplacement);
		return std::vector<PrecisionType>(InitVertex.begin(), InitVertex.end());
	};

	void setAttractorIx(size_t ix) {
		assert(ix < this->numStates());
		_tau_dist1.setIxAttractor(ix);
	}
};

} // namespace stattools

#endif // TOPTIMIZETRANSMATNELDERMEAD_H
