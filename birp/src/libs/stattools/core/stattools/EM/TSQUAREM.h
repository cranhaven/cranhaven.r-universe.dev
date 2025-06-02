//
// Created by madleina on 07.03.24.
//

#ifndef TREESWIRL_TSQUAREM_H
#define TREESWIRL_TSQUAREM_H

#include "coretools/algorithms.h"
#include "stattools/EM/TEMPrior.h"
#include "stattools/EM/TLatentVariable.h"

namespace stattools {

template<typename PrecisionType, typename NumStatesType, typename LengthType> struct TThetaSQUAREM {
private:
	std::vector<PrecisionType> _theta_emission;
	std::vector<PrecisionType> _theta_prior;

	PrecisionType _calcNewTheta_i(PrecisionType theta_0_i, PrecisionType alpha, PrecisionType r_i, PrecisionType v_i) {
		return theta_0_i - 2.0 * alpha * r_i + alpha * alpha * v_i;
	}

	std::vector<PrecisionType> _containerDiff(coretools::TConstView<PrecisionType> vs1,
											  coretools::TConstView<PrecisionType> vs2) {
		// calculate element-wise vs1 - vs2
		std::vector<PrecisionType> res(vs1.size());
		for (size_t i = 0; i < vs1.size(); ++i) { res[i] = vs1[i] - vs2[i]; }
		return res;
	}

	std::vector<PrecisionType> _calcR(coretools::TConstView<PrecisionType> Theta_0,
									  coretools::TConstView<PrecisionType> Theta_1) {
		return _containerDiff(Theta_1, Theta_0);
	}

	std::vector<PrecisionType> _calcV(coretools::TConstView<PrecisionType> Theta_1,
									  coretools::TConstView<PrecisionType> Theta_2,
									  coretools::TConstView<PrecisionType> R) {
		return _containerDiff(_containerDiff(Theta_2, Theta_1), R);
	}

	PrecisionType _calcAlpha(coretools::TConstView<PrecisionType> R_emis, coretools::TConstView<PrecisionType> R_prior,
							 coretools::TConstView<PrecisionType> V_emis,
							 coretools::TConstView<PrecisionType> V_prior) {
		const auto sum_r = coretools::sumOfSquares(R_emis) + coretools::sumOfSquares(R_prior);
		const auto sum_v = coretools::sumOfSquares(V_emis) + coretools::sumOfSquares(V_prior);
		const auto alpha = -sqrt(sum_r) / sqrt(sum_v);
		return alpha;
	}

	void _proposeNew(const TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> &Theta_0,
					 const TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> &Theta_1,
					 const TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> &Theta_2) {
		// calculate r: theta_1 - theta_0
		const auto r_emis  = _calcR(Theta_0._theta_emission, Theta_1._theta_emission);
		const auto r_prior = _calcR(Theta_0._theta_prior, Theta_1._theta_prior);

		// calculate v: (theta_2 - theta_1) - r
		const auto v_emis  = _calcV(Theta_1._theta_emission, Theta_2._theta_emission, r_emis);
		const auto v_prior = _calcV(Theta_1._theta_prior, Theta_2._theta_prior, r_prior);

		// calculate alpha
		const auto alpha = _calcAlpha(r_emis, r_prior, v_emis, v_prior);

		// now set new theta
		_theta_emission.resize(Theta_0.size_emission());
		_theta_prior.resize(Theta_0.size_prior());

		for (size_t i = 0; i < Theta_0.size_emission(); ++i) {
			_theta_emission[i] = _calcNewTheta_i(Theta_0._theta_emission[i], alpha, r_emis[i], v_emis[i]);
		}

		for (size_t i = 0; i < Theta_0.size_prior(); ++i) {
			_theta_prior[i] = _calcNewTheta_i(Theta_0._theta_prior[i], alpha, r_prior[i], v_prior[i]);
		}
	}

public:
	TThetaSQUAREM() = default;
	TThetaSQUAREM(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatVar,
				  const TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		initialize(LatVar, EMPrior);
	}

	TThetaSQUAREM(const TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> &Theta_0,
				  const TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> &Theta_1,
				  const TThetaSQUAREM<PrecisionType, NumStatesType, LengthType> &Theta_2) {
		_proposeNew(Theta_0, Theta_1, Theta_2);
	}

	void initialize(const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatVar,
					const TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		_theta_emission = LatVar.getParameters();
		_theta_prior    = EMPrior.getParameters();
	}

	bool update(TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatVar,
				TEMPrior_base<PrecisionType, NumStatesType, LengthType> &EMPrior) {
		if (!LatVar.setParameters(_theta_emission)) { return false; }
		if (!EMPrior.setParameters(_theta_prior)) { return false; }
		return true;
	}

	size_t size_emission() const { return _theta_emission.size(); }
	size_t size_prior() const { return _theta_prior.size(); }

	void print() const {
		coretools::cout << "_theta_emission: " << coretools::str::toString(_theta_emission) << std::endl;
		coretools::cout << "_theta_prior: " << coretools::str::toString(_theta_prior) << std::endl;
	}
};

} // namespace stattools

#endif // TREESWIRL_TSQUAREM_H
