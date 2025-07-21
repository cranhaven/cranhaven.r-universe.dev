//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORUNIFORM_H
#define TPRIORUNIFORM_H

#include "coretools/Main/TRandomGenerator.h"
#include "stattools/Priors/TPriorBase.h"

namespace stattools::prior {

//-------------------------------------------
// TUniformFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
// Note: all types are compatible with uniform prior (floating point / integer / unsigned)
class TUniformFixed : public TStochasticBase<Derived, Type, NumDim> {
private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	double _logDensity = 0.0;
	double _density    = 1.0;

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) {
			if constexpr (std::is_same_v<decltype(std::declval<Type>().get()), bool>) {
				(*Data)[i] = coretools::instances::randomGenerator().pickOneOfTwo();
			} else {
				auto value = coretools::instances::randomGenerator().getRand();
				if (coretools::checkForNumericOverflow_subtraction((double)Type::max(), (double)Type::min())) {
					value = (Type)(value * ((double)Type::max() - (double)Type::min()) + (double)Type::min());
				}
				(*Data)[i] = Type(value);
			}
		}
	};

public:
	~TUniformFixed() override = default;

	std::string name() const override { return "uniform"; };

	void setFixedPriorParameters(std::string_view Params) override {
		coretools::str::convertString(Params, "Uniform distribution does not accept parameters.");

		if (!coretools::checkForNumericOverflow_subtraction((double)Type::max(), (double)Type::min())) {
			_density = 1.;
		} else {
			_density = 1.0 / ((double)Type::max() - (double)Type::min());
		}
		_logDensity = log(_density);
		if (!std::isfinite(_logDensity)) {
			_logDensity = 0.0; // prevent problems with if min,max are very large -> log(1./(inf-(-inf))) = -inf
		}
	};

	double getDensity(const Storage &, size_t) const override { return _density; };

	double getLogDensity(const Storage &, size_t) const override { return _logDensity; };

	double getLogDensityRatio(const UpdatedStorage &, size_t) const override { return 0.0; };
};

}; // namespace stattools::prior

#endif // TPRIORUNIFORM_H
