//
// Created by madleina on 14.12.23.
//

#ifndef STATTOOLS_TLINEARMODEL_H
#define STATTOOLS_TLINEARMODEL_H

#include "coretools/arma_include.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

//-------------------------------------------
// TLinearModel
//-------------------------------------------

namespace stattools::det {

template<typename Derived, typename Type, size_t NumDim, typename SpecBeta, typename TypeCovariates, bool Intercept>
class TLinearModel : public prior::TDeterministicBase<Derived, Type, NumDim> {
	static_assert(NumDim == 2);
	static_assert(SpecBeta::numDim == 2);
	static_assert(SpecBeta::constraint::updateType == UpdateTypes::one);

private:
	using typename prior::TDeterministicBase<Derived, Type, NumDim>::Storage;
	using typename prior::TDeterministicBase<Derived, Type, NumDim>::UpdatedStorage;

	using BoxType       = TLinearModel<Derived, Type, NumDim, SpecBeta, TypeCovariates, Intercept>;
	using TypeParamBeta = TParameter<SpecBeta, BoxType>;

	TypeParamBeta *_beta                                       = nullptr;
	coretools::TMultiDimensionalStorage<TypeCovariates, 2> *_x = nullptr;

	size_t _I = 0;
	size_t _J = 0;
	size_t _K = 0;

protected:
	void _checkDim(size_t I_from_X) const {
		if (_I != I_from_X) {
			throw coretools::TUserError(name(), ": Number of rows of Y (", _I, ") must match the number of rows of X (", I_from_X, ")!");
		}
	}

	auto _getDimNamesBeta(const std::shared_ptr<coretools::TNamesEmpty> &DimNames_K) const {
		// add an extra beta at the beginning
		auto newDimNames = std::make_shared<coretools::TNamesStrings>();
		newDimNames->addName({"intercept"});
		for (size_t i = 0; i < DimNames_K->size(); ++i) { newDimNames->addName(DimNames_K->getName(i)); }
		return newDimNames;
	}

	auto _getArmaX() const {
		// add extra row of 1's if there is an intercept
		arma::mat X(_I, _K + Intercept, arma::fill::ones);
		for (size_t i = 0; i < _I; ++i) {
			for (size_t k = 0; k < _K; ++k) { X(i, k + Intercept) = (*_x)(i, k); }
		}
		return X;
	}

	auto _getArmaY(size_t j) const {
		const auto &storage = this->_storageBelow.front();
		arma::vec y(_I);
		for (size_t i = 0; i < _I; ++i) { y(i) = (Type)(*storage)(i, j); }
		return y;
	}

	void _copyToBeta(size_t j, const arma::vec &Beta_j) {
		for (size_t k = 0; k < _K + Intercept; ++k) {
			const size_t ix = _beta->getIndex({k, j});
			_beta->set(ix, Beta_j(k));
		}
	}

public:
	TLinearModel(TypeParamBeta *Beta, coretools::TMultiDimensionalStorage<TypeCovariates, 2> *X) : _beta(Beta), _x(X) {
		this->addPriorParameter(_beta);
	};
	~TLinearModel() override = default;

	std::string name() const override { return "linearModel"; }

	void initialize() override {
		assert(this->_storageBelow.size() == 1);
		assert(this->_parametersBelow.size() == 1);

		// Y = XB
		// Y = matrix of dimensions I times J
		// X = matrix of dimensions I times K
		// B = matrix of dimensions K times J
		const auto &storage = this->_storageBelow.front();
		_I                  = storage->dimensions()[0];
		_J                  = storage->dimensions()[1];
		_K                  = _x->dimensions()[1];

		// check if number of rows (I) of Y and X match
		_checkDim(_x->dimensions()[0]);

		const auto dimNames_J = storage->getDimensionName(1);
		const auto dimNames_K = Intercept ? _getDimNamesBeta(_x->getDimensionName(1)) : _x->getDimensionName(1);

		_beta->initStorage(this, {_K + Intercept, _J}, {dimNames_K, dimNames_J});
	};

	coretools::TRange getYRange(const coretools::TRange &Range) const {
		// Range corresponds to range of elements in beta that were updated
		// this should always be a single element (has static assert above)
		assert(Range.begin + Range.increment >= Range.end);
		const auto &storage = this->_storageBelow.front();

		// one beta_kj update affects an entire column Y_j
		// -> get Range for Y accordingly
		const size_t j                 = _beta->getSubscripts(Range.begin)[1]; // K times J
		const size_t alongDim          = 0;                                    // go over all rows (I)
		const coretools::TRange rangeY = storage->get1DSlice(alongDim, {0, j});
		return rangeY;
	}

	[[nodiscard]] double calculateLLRatio(TypeParamBeta *, const coretools::TRange &Range) const {
		// this function is called from within _beta->updateAllByMyself()
		// translate beta range in Y range
		const auto yRange = getYRange(Range);
		return this->_parametersBelow.front()->calculateLLRatio(yRange);
	}

	void updateTempVals(TypeParamBeta *, const coretools::TRange &Range, bool Accepted) {
		// translate beta range in Y range
		const auto yRange = getYRange(Range);
		return this->_parametersBelow.front()->updateTempVals(yRange, Accepted);
	}

	void guessInitialValues() override {
		// do OLS (per j independently)
		const arma::mat X = _getArmaX();
		for (size_t j = 0; j < _J; ++j) {
			const arma::vec y_j    = _getArmaY(j);
			const arma::vec beta_j = arma::solve(X, y_j).as_col();
			_copyToBeta(j, beta_j);
		}
	}

	double valueForBelow(size_t linear) const override {
		// linear corresponds to linear index in Y
		const auto i_j = this->_storageBelow.front()->getSubscripts(linear);
		const size_t i = i_j[0];
		const size_t j = i_j[1];

		// y_ij = sum_k X_ik * B_kj
		double y_ij = 0.0;
		if constexpr (Intercept) { y_ij += _beta->value({0, j}); }
		size_t kB = Intercept; // beta: without intercept start at 0; with intercept start at 1
		for (size_t k = 0; k < _K; ++k, ++kB) { y_ij += (*_x)(i, k) * _beta->value({kB, j}); }

		return y_ij;
	}
};

} // namespace stattools::det

#endif // STATTOOLS_TLINEARMODEL_H
