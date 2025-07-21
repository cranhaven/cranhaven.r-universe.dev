//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORFLAT_H
#define TPRIORFLAT_H

#include "stattools/Priors/TPriorBase.h"

namespace stattools::prior {

//-------------------------------------------
// TFlatFixed
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
// Note: all types are compatible with flat prior (floating point / integer / unsigned)
// flat / improper prior: no information. Can be used if classic scheme of parameter-prior is hacked by developer
class TFlatFixed : public TStochasticBase<Derived, Type, NumDim> {
private:
	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) { (*Data)[i] = Type::min(); }
	};

public:
	~TFlatFixed() override = default;

	std::string name() const override { return "flat"; };

	void setFixedPriorParameters(std::string_view) override{};

	double getDensity(const Storage &, size_t) const override { return 1.0; };

	double getLogDensity(const Storage &, size_t) const override { return 0.0; };

	double getLogDensityRatio(const UpdatedStorage &, size_t) const override { return 0.0; };
};

} // namespace stattools::prior

#endif // TPRIORFLAT_H
