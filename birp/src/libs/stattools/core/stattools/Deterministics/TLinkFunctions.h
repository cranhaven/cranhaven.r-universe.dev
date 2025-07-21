//
// Created by madleina on 13.12.23.
//

#ifndef STATTOOLS_TLINKFUNCTIONS_H
#define STATTOOLS_TLINKFUNCTIONS_H

#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"

namespace stattools::det {

//-------------------------------------------
// TLinkFunctions
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename Spec, typename BoxType>
class TLinkFunctionsBase : public prior::TDeterministicBase<Derived, Type, NumDim> {
protected:
	using typename prior::TDeterministicBase<Derived, Type, NumDim>::Storage;
	using typename prior::TDeterministicBase<Derived, Type, NumDim>::UpdatedStorage;

	using TypeParam           = TParameter<Spec, BoxType>;
	using UnderlyingTypeParam = typename Spec::value_type;
	using FunctionDown        = std::function<double(double)>;
	using FunctionUp          = std::function<double(double)>;

	TypeParam *_param = nullptr;
	FunctionDown _funDown; // below = _funDown(param)
	FunctionUp _funUp;     // param = _funUp(below)

	std::string _name;

public:
	TLinkFunctionsBase(TypeParam *Param, FunctionDown FunDown, FunctionUp FunUp, std::string_view Name)
		: _param(Param), _funDown(std::move(FunDown)), _funUp(std::move(FunUp)), _name(Name) {
		this->addPriorParameter(_param);
	};
	~TLinkFunctionsBase() override = default;

	std::string name() const override { return _name; }

	template<typename PtrDerived> void initialize(PtrDerived *Ptr) {
		assert(this->_storageBelow.size() == 1);

		auto dim      = this->_storageBelow.front()->dimensions();
		auto dimNames = this->_storageBelow.front()->getDimensionNames();
		_param->initStorage(Ptr, dim, dimNames);
	};

	[[nodiscard]] double calculateLLRatio(TypeParam *, const coretools::TRange &Range) const {
		// this function is called from within _param->updateAllByMyself()
		assert(this->_parametersBelow.size() == 1);
		return this->_parametersBelow.front()->calculateLLRatio(Range);
	}

	void updateTempVals(TypeParam *, const coretools::TRange &Range, bool Accepted) {
		assert(this->_parametersBelow.size() == 1);
		return this->_parametersBelow.front()->updateTempVals(Range, Accepted);
	}

	void guessInitialValues() override {
		// from bottom to top
		const auto storage = this->_storageBelow.front();
		const auto range   = storage->getFull();
		for (size_t i = range.begin; i < range.end; i += range.increment) {
			using ST = typename Spec::value_type;
			_param->set(i, ST(_funUp((Type)(*storage)[i])));
		}
	}

	double valueForBelow(size_t i) const override {
		assert(i < _param->size());
		return _funDown((UnderlyingTypeParam)_param->value(i));
	}
};

//-------------------------------------------
// TLinkFunctions
// flexible: takes two functionals
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename Spec>
class TLinkFunction
	: public TLinkFunctionsBase<Derived, Type, NumDim, Spec, TLinkFunction<Derived, Type, NumDim, Spec>> {
	// below = FunDown(param)
	// param = FunUp(below)
private:
	using BoxType      = TLinkFunction<Derived, Type, NumDim, Spec>;
	using Base         = TLinkFunctionsBase<Derived, Type, NumDim, Spec, BoxType>;
	using TypeParam    = typename Base::TypeParam;
	using FunctionDown = typename Base::FunctionDown;
	using FunctionUp   = typename Base::FunctionUp;

public:
	TLinkFunction(TypeParam *Param, FunctionDown FunDown, FunctionUp FunUp, std::string_view Name)
		: Base(Param, FunDown, FunUp, Name){};
	~TLinkFunction() override = default;
	void initialize() override { Base::initialize(this); }
};

//-------------------------------------------
// TExp
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename Spec>
class TExp : public TLinkFunctionsBase<Derived, Type, NumDim, Spec, TExp<Derived, Type, NumDim, Spec>> {
	// below = exp(param)
	static_assert(TypesArePositiveFloatingPoints<Type>() ||
				  TypesAreStrictlyPositiveFloatingPoints<Type>()); // below: >= 0
	static_assert(TypesAreUnbounded<typename Spec::value_type>()); // param: [-inf, inf]

private:
	using BoxType   = TExp<Derived, Type, NumDim, Spec>;
	using Base      = TLinkFunctionsBase<Derived, Type, NumDim, Spec, BoxType>;
	using TypeParam = typename Base::TypeParam;

public:
	TExp(TypeParam *Param)
		: Base(Param, [](double x) { return std::exp(x); }, [](double x) { return std::log(x); }, "exp"){};
	~TExp() override = default;
	void initialize() override { Base::initialize(this); }
};

//-------------------------------------------
// TLog
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename Spec>
class TLog : public TLinkFunctionsBase<Derived, Type, NumDim, Spec, TLog<Derived, Type, NumDim, Spec>> {
	// below = log(param)
	static_assert(TypesAreUnbounded<Type>()); // below: [-inf, inf]
	static_assert(TypesArePositiveFloatingPoints<typename Spec::value_type>() ||
				  TypesAreStrictlyPositiveFloatingPoints<typename Spec::value_type>()); // param: >= 0

private:
	using BoxType   = TLog<Derived, Type, NumDim, Spec>;
	using Base      = TLinkFunctionsBase<Derived, Type, NumDim, Spec, BoxType>;
	using TypeParam = typename Base::TypeParam;

public:
	TLog(TypeParam *Param)
		: Base(Param, [](double x) { return std::log(x); }, [](double x) { return std::exp(x); }, "log"){};
	~TLog() override = default;
	void initialize() override { Base::initialize(this); }
};

//-------------------------------------------
// TLogistic
//-------------------------------------------

namespace impl {
inline double logit(double x) noexcept { return log((double)x / (1. - (double)x)); }
inline double logistic(double x) noexcept {
	double tmp = 1. / (1. + exp(-x));
	// make sure tmp is never 0 or 1
	tmp        = std::min(tmp, coretools::ZeroOneOpen::max().get());
	tmp        = std::max(tmp, coretools::ZeroOneOpen::min().get());
	return tmp;
}
} // namespace impl

template<typename Derived, typename Type, size_t NumDim, typename Spec>
class TLogistic : public TLinkFunctionsBase<Derived, Type, NumDim, Spec, TLogistic<Derived, Type, NumDim, Spec>> {
	// below = logistic(param)
	static_assert(TypesAreZeroOneClosed<Type>() || TypesAreZeroOneOpen<Type>() ||
				  TypesAreZeroOpenOneClosed<Type>());              // below: [0,1]
	static_assert(TypesAreUnbounded<typename Spec::value_type>()); // param: [-inf, inf]

private:
	using BoxType   = TLogistic<Derived, Type, NumDim, Spec>;
	using Base      = TLinkFunctionsBase<Derived, Type, NumDim, Spec, BoxType>;
	using TypeParam = typename Base::TypeParam;

public:
	TLogistic(TypeParam *Param)
		: Base(
			  Param, [](double x) { return impl::logistic(x); }, [](double x) { return impl::logit(x); }, "logistic"){};
	~TLogistic() override = default;
	void initialize() override { Base::initialize(this); }
};

//-------------------------------------------
// TLogit
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename Spec>
class TLogit : public TLinkFunctionsBase<Derived, Type, NumDim, Spec, TLogit<Derived, Type, NumDim, Spec>> {
	// below = logit(param)
	static_assert(TypesAreZeroOneClosed<typename Spec::value_type>() ||
				  TypesAreZeroOneOpen<typename Spec::value_type>() ||
				  TypesAreZeroOpenOneClosed<typename Spec::value_type>()); // param: [0,1]
	static_assert(TypesAreUnbounded<Type>());                              // below: [-inf, inf]

private:
	using BoxType   = TLogit<Derived, Type, NumDim, Spec>;
	using Base      = TLinkFunctionsBase<Derived, Type, NumDim, Spec, BoxType>;
	using TypeParam = typename Base::TypeParam;

public:
	TLogit(TypeParam *Param)
		: Base(
			  Param, [](double x) { return impl::logit(x); },
			  [](double x) { return impl::logistic(x); }, "logit"){};
	~TLogit() override = default;
	void initialize() override { Base::initialize(this); }
};

} // namespace stattools::det
#endif // STATTOOLS_TLINKFUNCTIONS_H
