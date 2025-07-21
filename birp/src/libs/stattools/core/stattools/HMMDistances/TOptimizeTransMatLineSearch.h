//
// Created by caduffm on 10/31/22.
//

#ifndef TOPTIMIZETRANSMATLINESEARCH_H
#define TOPTIMIZETRANSMATLINESEARCH_H

#include <cmath>
#include <vector>
#include "coretools/arma_include.h"
#include "coretools/Files/TOutputFile.h"
#include "stattools/HMMDistances/TTransitionMatrixDistancesExponential.h"
#include "stattools/MLEInference/TLineSearch.h"

namespace stattools {

//-------------------------------------------
// TOptimizeTransMatLineSearch
//-------------------------------------------
template<typename PrecisionType, typename NumStatesType, typename LengthType, typename TypeTransMat>
class TOptimizeTransMatLineSearch
	: virtual public TTransitionMatrixDistancesExponential<PrecisionType, NumStatesType, LengthType, TypeTransMat> {
private:
	using Base   = TTransitionMatrixDistancesExponential<PrecisionType, NumStatesType, LengthType, TypeTransMat>;
	using VecTau = std::vector<arma::mat>;
	using Base::_tau_dist1;

protected:
	// settings for line search tuning
	double _factorPrecision          = 0.001;
	size_t _initMaxIterations        = 100;
	double _initEpsilon              = 10e-5;
	size_t _numIterUntilAdjustingEps = 10;
	const double _maxKappa           = 38;        // largest value still numerically stable
	double _logInitialStepSize       = 0.6931472; // = log(2.0);

	// temporary variables
	size_t _totalNumberOfIterations = 0;
	PrecisionType _previousLogKappa = 0.0;

	void _setLogKappa() {
		auto initialKappa = _tau_dist1.getStartValues();
		assert(initialKappa.size() == 1);
		assert(initialKappa[0] > 0.0);

		const double initialLogKappa = log(initialKappa[0]);
		_previousLogKappa            = initialLogKappa;
	}

	PrecisionType _runLineSearch(VecTau &Tau, VecTau &Xi, bool Report, bool AdjustTolerance) {
		//_printQSurface("Q_" + coretools::str::toString(_previousLogKappa) + ".txt");
		TLineSearch lineSearch;
		if (AdjustTolerance) {
			lineSearch.dynamicallyAdjustTolerance(_previousLogKappa, _factorPrecision, _numIterUntilAdjustingEps);
		}
		auto f_ptr                 = &TOptimizeTransMatLineSearch::_calcQTransitionMatrix;
		TReturnCode<double> result = lineSearch.findMax(*this, f_ptr, _previousLogKappa, _logInitialStepSize,
														_initEpsilon, _initMaxIterations, Tau, Xi);
		if (result.reason() == invalidInput) { throw coretools::TDevError(result.message()); }
		if (this->doReport() && Report) { coretools::instances::logfile().list(result.message()); }

		// store MLE to start next EM iteration with this
		double logKappa = result.converged() ? result.result() : _previousLogKappa;
		if (result.converged()) { _previousLogKappa = logKappa; }
		_totalNumberOfIterations += lineSearch.getCounterIterations();

		return logKappa;
	};

	PrecisionType _calcQTransitionMatrix(PrecisionType LogKappa, VecTau &Tau, VecTau &Xi) {
		// fill transition matrices for current kappa
		const double kappa = exp(LogKappa);

		if (kappa == 0.0 || kappa > _maxKappa || std::isnan(kappa) || !std::isfinite(kappa)) {
			return std::numeric_limits<PrecisionType>::lowest();
		}
		std::vector<PrecisionType> vec = {kappa};
		if (!this->fillProbabilities(vec, Tau)) { return std::numeric_limits<PrecisionType>::lowest(); }

		// calculate sum
		PrecisionType sum = 0.;
		for (size_t d = 1; d < Tau.size(); d++) {
			// skip first distance group (= initial probability, estimated separately)
			for (NumStatesType i = 0; i < _tau_dist1.numStates(); i++) {     // z_{t-1}
				for (NumStatesType j = 0; j < _tau_dist1.numStates(); j++) { // z_t
					double t = Tau[d](i, j);
					if (t <= 0.0) { t = std::numeric_limits<double>::min(); } // numeric issues
					sum += Xi[d](i, j) * log(t);
				}
			}
		}
		return sum;
	};

	void _printQSurface(std::string_view Filename, VecTau &Tau, VecTau &Xi) {
		// debugging
		size_t numPoints = 10000;

		// kappa
		std::vector<PrecisionType> kappa(numPoints);
		PrecisionType start = 1e-06;
		for (size_t i = 0; i < numPoints; i++) { kappa[i] = start + (double)i / 1000.; }

		// calculate Q-function
		std::vector<PrecisionType> f(numPoints, 0.);
		for (size_t i = 0; i < numPoints; i++) { f[i] = _calcQTransitionMatrix(log(kappa[i]), Tau, Xi); }

		// print to file
		coretools::TOutputFile file(Filename);
		std::vector<std::string> header = {"kappa", "f"};
		file.writeHeader(header);

		for (size_t i = 0; i < numPoints; i++) { file << kappa[i] << f[i] << std::endl; }

		file.close();
	};

	void _setFromCommandLine() {
		_factorPrecision    = coretools::instances::parameters().get("factorPrecision", 0.001);
		_initEpsilon        = coretools::instances::parameters().get("initEpsilon", 10e-5);
		_initMaxIterations  = coretools::instances::parameters().get("initMaxIterations", 100);
		_logInitialStepSize = log(coretools::instances::parameters().get("initialStepSize", 2.0));
	}

public:
	TOptimizeTransMatLineSearch(size_t NumStates) : Base(NumStates) {
		_setLogKappa();
		_setFromCommandLine();
	};

	~TOptimizeTransMatLineSearch() override = default;

	std::vector<PrecisionType> setValuesEM(coretools::TConstView<PrecisionType> InitVals, VecTau &Tau) override {
		assert(InitVals.size() == 1);
		this->fillProbabilities(InitVals, Tau);

		if (InitVals[0] <= 0.0) { throw coretools::TDevError("Initial kappa (", InitVals[0], ") must be strictly positive!"); }
		_previousLogKappa = log(InitVals[0]);
		return {InitVals[0]};
	};

	std::vector<PrecisionType> maximizeQFunction(VecTau &Tau, VecTau &Xi,
												 const std::vector<std::vector<PrecisionType>> & /*Gamma*/,
												 bool FirstIteration, bool LastIteration) override {
		// maximize the transition-part of the Q-function numerically with line search
		PrecisionType logResult;
		if (FirstIteration) {
			// don't adjust tolerance, don't report
			logResult = _runLineSearch(Tau, Xi, true, false);
		} else if (LastIteration) {
			// don't adjust tolerance, don't report
			logResult = _runLineSearch(Tau, Xi, false, false);
			if (this->doReport()) {
				coretools::instances::logfile().list("Ran a total of ", _totalNumberOfIterations,
													 " line search iterations over the entire EM algorithm.");
			}
		} else {
			// adjust tolerance, don't report
			logResult = _runLineSearch(Tau, Xi, true, false);
		}

		return {exp(logResult)};
	};

	void setFactorPrecision(double Factor) { _factorPrecision = Factor; };
	void setInitEpsilon(double Epsilon) { _initEpsilon = Epsilon; };
	void setInitMaxIterations(size_t MaxIterations) { _initMaxIterations = MaxIterations; };
};

} // namespace stattools
#endif // TOPTIMIZETRANSMATLINESEARCH_H
