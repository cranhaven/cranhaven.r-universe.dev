//
// Created by caduffm on 10/31/22.
//

#ifndef TTRANSITIONMATRIXDISTANCESEXPONENTIAL_H
#define TTRANSITIONMATRIXDISTANCESEXPONENTIAL_H

#include <cmath>
#include <vector>
#include "coretools/arma_include.h"
#include "stattools/HMMDistances/TTransitionMatrixHMMDistances.h"

namespace stattools {

//-------------------------------------------
// TTransitionMatrixDistancesExponential
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType, typename TypeTransMat>
class TTransitionMatrixDistancesExponential
	: public TTransitionMatrixDistancesOptimizerBase<PrecisionType, NumStatesType, LengthType> {
	// base class for HMMs where the transition matrix depends on a distance between two neighbouring points
	// transition matrix for distance one is calculated based on some specific equation
	// transition matrices for all subsequent distances are simply the square of the previous transition matrix
private:
	using VecTau = std::vector<arma::mat>;

protected:
	// transition matrix for distance 1
	TypeTransMat _tau_dist1;

	[[nodiscard]] bool _tauAreOK(const arma::mat &Tau) const {
		// returns false if any entry in transition matrix is inf or NaN
		for (NumStatesType i = 0; i < Tau.n_rows; i++) {     // rows
			for (NumStatesType j = 0; j < Tau.n_cols; j++) { // cols
				if (std::isnan(Tau(i, j)) || !std::isfinite(Tau(i, j))) { return false; }
			}
		}
		return true;
	}

	bool _fillTauStationary(VecTau &Tau) {
		Tau[0] = _tau_dist1.getStationaryMatrix();
		return _tauAreOK(Tau[0]);
	}

	bool _fillStationaryDistributionFromStartValues(VecTau &Tau) override {
		_tau_dist1.fillStationaryFromStartValues();
		return _fillTauStationary(Tau);
	};

	bool _fillStationaryDistribution(coretools::TConstView<PrecisionType> Values, VecTau &Tau) override {
		bool valid = _tau_dist1.fillStationary(Values);
		if (!valid) { return false; }
		return _fillTauStationary(Tau);
	};

	bool _fillTauTransitionVec(VecTau &Tau) {
		Tau[1] = _tau_dist1.getTransitionMatrix();
		if (!_tauAreOK(Tau[1])) { return false; }

		// fill all other transition matrices by squaring the previous one
		for (size_t i = 2; i < Tau.size(); i++) {
			Tau[i] = Tau[i - 1] * Tau[i - 1];
			if (!_tauAreOK(Tau[i])) { return false; }
		}
		return true;
	}

	bool _fillTransitionProbabilities(coretools::TConstView<PrecisionType> Values, VecTau &Tau) override {
		// Tau[0] contains stationary distribution -> filled in function fillStationaryDistribution
		// fill transition matrix for first distance group (-> specific for each transition matrix class)
		bool valid = _tau_dist1.fillTransitionMatrix(Values);
		if (!valid) { return false; }
		return _fillTauTransitionVec(Tau);
	};

	bool _fillTransitionProbabilitiesFromStartValues(VecTau &Tau) override {
		// Tau[0] contains stationary distribution -> filled in function fillStationaryDistribution
		// fill transition matrix for first distance group (-> specific for each transition matrix class)
		_tau_dist1.fillTransitionMatrixFromStartValues();
		return _fillTauTransitionVec(Tau);
	};

public:
	TTransitionMatrixDistancesExponential(size_t NumStates) : _tau_dist1(NumStates){};
	TTransitionMatrixDistancesExponential(TypeTransMat Tau) : _tau_dist1(std::move(Tau)){};

	~TTransitionMatrixDistancesExponential() override = default;

	void fillHeaderEMReportFile(std::vector<std::string> &Header) override {
		_tau_dist1.fillHeaderEMReportFile(Header);
	}

	[[nodiscard]] size_t numParameters() const override { return _tau_dist1.numParameters(); };
	[[nodiscard]] size_t numStates() const override { return _tau_dist1.numStates(); };
	[[nodiscard]] TypeTransMat &tau() { return _tau_dist1; };
};
} // namespace stattools

#endif // TTRANSITIONMATRIXDISTANCESEXPONENTIAL_H
