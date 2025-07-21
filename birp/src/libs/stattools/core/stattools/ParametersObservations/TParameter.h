//
// Created by madleina on 01.07.20.
//

#ifndef TPARAMETER_H
#define TPARAMETER_H

#include <string>
#include <type_traits>
#include <vector>

#include "coretools/Counters/TCountDistribution.h"
#include "coretools/Math/TAcceptOddsRation.h"
#include "coretools/Math/TMeanVar.h"
#include "coretools/traits.h"
#include "stattools/DAG/TDAGBuilder.h"
#include "stattools/EM/TLatentVariable.h"
#include "stattools/ParametersObservations/TNodeTyped.h"
#include "stattools/ParametersObservations/boxTraits.h"
#include "stattools/ParametersObservations/spec.h"
#include "stattools/Priors/TypesIdentifiers.h"
#include "stattools/Updates/TLogHCalculator.h"
#include "stattools/Updates/TUpdate.h"
#include "stattools/Updates/indexPickerTraits.h"

#ifdef _OPENMP
#include "omp.h"
#endif

namespace stattools {

//--------------------------------
// TParameter
//--------------------------------

template<typename Spec, typename TypeBoxAround>
class TParameter
    : public TNodeTyped<TParameterBase, typename Spec::value_type, Spec::numDim, typename Spec::typeBoxAbove> {
protected:
	// type alias and constexpr variables
	using Type                                     = typename Spec::value_type;
	using UnderlyingType                           = decltype(std::declval<Type>().get());
	using TypeBoxAbove                             = typename Spec::typeBoxAbove;
	using ThisType                                 = TParameter<Spec, TypeBoxAround>;
	constexpr static size_t NumDim                 = Spec::numDim;
	constexpr static Constraints Constraint        = Spec::constraint::constraint;
	constexpr static UpdateTypes UpdateType        = Spec::constraint::updateType;
	constexpr static size_t NumUpdatesPerIteration = Spec::constraint::numUpdatesPerIteration;
	constexpr static MarkovOrder MOrder            = Spec::markovOrder;
	constexpr static bool Parallelize              = Spec::parallelize;
	constexpr static bool DoRJMCMC                 = Spec::RJMCMC::doRJMCMC;
	using SpecRJMCMCModelParameter   = std::conditional_t<DoRJMCMC, typename Spec::RJMCMC::specModelParameter, Spec>;
	using RJMCMCProposalDistribution = typename Spec::RJMCMC::proposalDistribution;
	using IndexPicker                = decltype(impl::getIndexPickerType<Spec>());
	constexpr static bool IsDeterministic      = TypeBoxAbove::isDeterministic;
	constexpr static bool AllWeightsAreRegular = impl::allWeightsAreRegular<Spec>();

	// type alias from base class
	using Base = TNodeTyped<TParameterBase, Type, NumDim, TypeBoxAbove>;
	using Base::_boxAbove;
	using Base::_storage;

	// ptr to box (prior or deterministic box) around this parameter
	TypeBoxAround *_boxAround;

	// definition
	TParameterDefinition _def;

	// posterior mean and variance
	std::vector<coretools::TMeanVar<double>> _meanVar;
	// for categorical variables: counts per states
	std::vector<coretools::TCountDistribution<uint8_t, uint32_t, false>> _counts;

	// updates
	std::unique_ptr<TUpdateTypedBase<Type>> _updater;

	// index pickers
	// if all dependent / all independent: we only use a single picker
	constexpr static bool KnownNumPickers = MOrder != MarkovOrder::different;
	using ContainerIxPicker = std::conditional_t<KnownNumPickers, std::array<IndexPicker, 1>, std::vector<IndexPicker>>;
	using ContainerNumThreads = std::conditional_t<KnownNumPickers, std::array<size_t, 1>, std::vector<size_t>>;
	ContainerIxPicker _indexPicker;
	ContainerNumThreads _numThreads;
	std::array<size_t, NumDim> _markovOrder;

	// RJ-MCMC
	bool _isRJMCMCModelNode = false; // true if this parameter represents a parameter indicating the model in an RJ-MCMC
	TParameter<SpecRJMCMCModelParameter, TypeBoxAround> *_rjModel; // pointer to such a model node
	double _q_1_To_0     = 0.0;
	double _log_q_1_To_0 = 0.0;
	RJMCMCProposalDistribution _rjProposalDistr;

	void _initMeanVar_InitVals_JumpSizes() {
		_initMeanVar();
		_readInitVals(_def.initVal(), _def.hasDefaultInitVal());
		_initProposalWidths(_createProposalKernel());

		// does parameter have fixed initial value? Or is it the default?
		this->fixInitialization(!_def.hasDefaultInitVal());

		// special case: single element of sumOne or lengthOne -> fix it to one
		if ((Constraint == Constraints::sumOne || Constraint == Constraints::lengthOne) && _storage.size() == 1) {
			_storage[0] = Type(1.0);
			this->fixInitialization();
			setIsUpdated(false);
		}
	}

	[[nodiscard]] constexpr bool _writeStatePos() const {
		return TypesAreBool<Type>() ||
		       (TypesAreUnsignedInteger<Type>() && Type::max() <= std::numeric_limits<uint8_t>::max());
	}

	void _initMeanVar() {
		// store mean and variance
		if (_def.writesFile(MCMCFiles::meanVar)) { _meanVar.assign(_storage.size(), coretools::TMeanVar<double>()); }
		if (_def.writesFile(MCMCFiles::statePosteriors) || _def.writesFile(MCMCFiles::posteriorMode)) {
			assert(_writeStatePos());
			if (Type::max() > std::numeric_limits<uint8_t>::max() - 1) {
				throw coretools::TDevError("Parameter", this->name(), " max (", Type::max(),
				         ") is larger than the maximal value of uint8_t-1 that is used to count the state posteriors.");
			}
			_counts.assign(_storage.size(), coretools::TCountDistribution<uint8_t, uint32_t, false>(
			                                    static_cast<uint8_t>((uint8_t)Type::max() + (uint8_t)1)));
		}
	}

	std::unique_ptr<TPropKernelBase<Type, UnderlyingType>> _createProposalKernel() {
		if constexpr (std::is_same_v<UnderlyingType, bool>) {
			// booleans: only allow proposal kernel for bools
			return std::make_unique<TPropKernelBool<Type>>();
		} else if constexpr (std::is_integral_v<UnderlyingType>) {
			// integers: only allow proposal kernels for integers
			if (_def.propKernel() == ProposalKernel::randomInteger || Type::max() <= 10) {
				return std::make_unique<TPropKernelRandomInteger<Type, UnderlyingType>>();
			} else {
				// default: use adjustable integer proposal kernel
				return std::make_unique<TPropKernelInteger<Type, UnderlyingType>>();
			}
		} else {
			// floating points: choose between normal and uniform proposal kernel
			if (_def.propKernel() == ProposalKernel::normal) {
				return std::make_unique<TPropKernelNormal<Type, UnderlyingType>>();
			} else if (_def.propKernel() == ProposalKernel::uniform) {
				return std::make_unique<TPropKernelUniform<Type, UnderlyingType>>();
			} else if (_def.propKernel() == ProposalKernel::scaleLogNormal) {
				// for positive or negative types: can additionally choose scaleLogNormal proposal kernel
				if constexpr (TypesArePositive<Type>() || TypesAreStrictlyPositive<Type>() ||
				              TypesAreNegative<Type>()) {
					return std::make_unique<TPropKernelScalingLogNormal<Type, UnderlyingType>>();
				} else {
					throw coretools::TDevError("Can not initialize parameter '", this->name(), "': Proposal kernel '",
					         ProposalKernel::proposalKernelToString(_def.propKernel()),
					         "' is only applicable to positive or negative types!");
				}
			} else {
				// none of the proposal kernels matches -> throw
				throw coretools::TDevError("Can not initialize parameter '", this->name(), "': Proposal kernel distribution with name '",
				         ProposalKernel::proposalKernelToString(_def.propKernel()), "' does not exist!");
			}
		}
	}

	void _initProposalWidths(std::unique_ptr<TPropKernelBase<Type, UnderlyingType>> PropKernel) {
		// create proposal kernel
		if (!PropKernel->isAdjusted()) {
			_updater = std::make_unique<TUpdateNoAdjust<Type>>(_def.isUpdated(), std::move(PropKernel), this->name());
		} else if (!std::is_integral_v<UnderlyingType> && (!AllWeightsAreRegular || UpdateType == UpdateTypes::pair ||
		                                                   DoRJMCMC || _def.unequalNumberOfUpdates())) {
			// always use TUpdateUnique where the total counter runs per index
			// because the indices might not be updated equally often
			_updater = std::make_unique<TUpdateUnique<Type, false>>(_storage.size(), _def.isUpdated(),
			                                                        std::move(PropKernel), this->name());
		} else if (_def.oneJumpSizeForAll() || std::is_integral_v<UnderlyingType>) {
			_updater = std::make_unique<TUpdateShared<Type>>(_def.isUpdated(), std::move(PropKernel), this->name());
		} else {
			_updater = std::make_unique<TUpdateUnique<Type, true>>(_storage.size(), _def.isUpdated(),
			                                                       std::move(PropKernel), this->name());
		}

		if (!_def.hasDefaultJumpSizeProposal()) { _updater->setProposalWidth(_def.initJumpSizeProposal()); }
	}

	void _readInitVals(std::string_view InitVal, bool HasDefaultInitVal) {
		if (!HasDefaultInitVal && _storage.size() > 0) {
			TReadInitialValues<Type, NumDim> reader;
			reader.readVals(InitVal, _storage, this->name());
		}
	}

	void _initIndexPicker() {
		if constexpr (MOrder == MarkovOrder::allIndependent || MOrder == MarkovOrder::allDependent) {
			std::fill(_markovOrder.begin(), _markovOrder.end(), 0);
		}

		TMarkovOrder<NumDim> markov(_markovOrder, _storage.dimensions());

		// set number of threads
		_setNumThreads(markov.getNumPickers());

		if constexpr (!KnownNumPickers) { _indexPicker.resize(markov.getNumPickers()); }
		for (size_t i = 0; i < _indexPicker.size(); ++i) {
			if constexpr (UpdateType == UpdateTypes::one) {
				_numThreads[i] = _indexPicker[i].initialize(markov, i, _numThreads[i]);
			} else {
				_numThreads[i] = _indexPicker[i].initialize(_storage.dimensions(), _numThreads[i]);
			}
		}
	}

	void _setNumThreads(size_t NumPickers) {
		if constexpr (!KnownNumPickers) { _numThreads.resize(NumPickers); }

		std::fill(_numThreads.begin(), _numThreads.end(), 1);
#ifdef _OPENMP
		if constexpr (Parallelize) {
			size_t numThreads = coretools::getNumThreads();
			std::fill(_numThreads.begin(), _numThreads.end(), numThreads);
		}
#endif
	}

	void _initRJMCMC() {
		if constexpr (DoRJMCMC) {
			_rjProposalDistr.set(_def.rjProposalDistrParams());
			_q_1_To_0     = _def.q1To0RJMCMC();
			_log_q_1_To_0 = log(_q_1_To_0);
		}
	}

	void _setFromDeterministic(const coretools::TRange &Range) {
		for (size_t i = Range.begin; i < Range.end; i += Range.increment) { set(i, Type(_boxAbove->valueForBelow(i))); }
	}

	void _reSetFromDeterministic(const coretools::TRange &Range, bool Accepted) {
		if (!Accepted) {
			for (size_t i = Range.begin; i < Range.end; i += Range.increment) { reset(i); }
		}
		for (size_t i = Range.begin; i < Range.end; i += Range.increment) { addToMeanVar(i); }
	}

	bool _metropolisHastings(const coretools::TRange &Range) {
		propose(Range);
		const auto LLRatio       = calculateLLRatio(Range);
		const auto logPriorRatio = getLogDensityRatio(Range);
		const double logH        = LLRatio + logPriorRatio;
		return acceptOrReject(logH, Range);
	}

	bool _doGibbs(const coretools::TRange &Range) {
		if constexpr (impl::doGibbsTakesRange_v<ThisType, TypeBoxAround>) {
			// boxAround takes TRange
			_boxAround->doGibbs(this, Range);
		} else {
			// boxAround takes size_t
			if constexpr (UpdateType == UpdateTypes::one) {
				// no need to loop over TRange
				assert(Range.begin + Range.increment >= Range.end); // single element
				_boxAround->doGibbs(this, Range.begin);
			} else {
				for (size_t i = Range.begin; i < Range.end; i += Range.increment) { _boxAround->doGibbs(this, i); }
			}
		}
		return true;
	}

	bool _doRJMCMC(const coretools::TRange &Range) {
		assert(UpdateType == UpdateTypes::one && Range.begin + Range.increment >= Range.end); // single element
		const size_t i = Range.begin;

		if (_rjModel->value(i) == 1) { // we're in 1-model
			if (_rjModel->isUpdated() && coretools::instances::randomGenerator().getRand() < _q_1_To_0) {
				return _updateRJOneToZero(i); // propose model switch
			} else {
				return _metropolisHastings(Range); // update regular
			}
		} else { // we're in 0-model
			if (_rjModel->isUpdated()) {
				return _updateRJZeroToOne(i); // propose model switch
			} else {
				return false; // no updates to be done
			}
		}
	}

	bool _updateRange(const coretools::TRange &Range) {
		if constexpr (DoRJMCMC) {
			return _doRJMCMC(Range);
		} else if constexpr (impl::doGibbs_v<ThisType, TypeBoxAround>) {
			return _doGibbs(Range);
		} else {
			return _metropolisHastings(Range);
		}
	}

	bool _updateRJZeroToOne(size_t i) {
		// RC-MCMC: 0 -> 1 update
		// When switching from null to One-Model: propose new value from proposal distribution
		set(i, proposeNewValueRJMCMC());
		_rjModel->set(i, true);

		// calculate Hastings ratio
		const double log_p_P = _rjProposalDistr.logDensity(this->value(i));
		double logH          = _log_q_1_To_0 + getLogDensity(i) - log_p_P + _rjModel->getLogDensityRatio(i);
		logH += _rjModel->calculateLLRatio(coretools::TRange(i));

		// accept or reject
		bool acc = acceptOrReject(logH, coretools::TRange(i), {}); // nothing was proposed with propkernel -> {}
		if (!acc) { _rjModel->reset(i); }
		_rjModel->addToMeanVar(i);
		return acc;
	}

	bool _updateRJOneToZero(size_t i) {
		// RC-MCMC: 1 -> 0 update
		const double logDensityOld = getLogDensity(i);
		set(i, Type(0.0));
		_rjModel->set(i, false);

		// calculate RJ-MCMC term of Hastings ratio
		double log_p_P = _rjProposalDistr.logDensity(oldValue(i));
		double logH    = log_p_P - _log_q_1_To_0 - logDensityOld + _rjModel->getLogDensityRatio(i);
		logH += _rjModel->calculateLLRatio(coretools::TRange(i));

		bool acc = acceptOrReject(logH, coretools::TRange(i), {}); // nothing was proposed with propkernel -> {}
		if (!acc) { _rjModel->reset(i); }
		_rjModel->addToMeanVar(i);
		return acc;
	}

	void _propose(const coretools::TRange &Range) {
		for (size_t i = Range.begin; i < Range.end; i += Range.increment) {
			assert(i < _storage.size());
			_storage[i] = _updater->propose((Type)_storage[i], i);
		}
	}

	std::pair<size_t, size_t> _getPairIndices(const coretools::TRange &Range) {
		const size_t i = Range.begin;
		const size_t j = Range.begin + Range.increment;
		assert(j + 1 == Range.end); // only two indices
		assert(i < _storage.size() && j < _storage.size());

		return {i, j};
	}

	void _proposePairLengthOne(const coretools::TRange &Range) {
		static_assert(Constraint == Constraints::lengthOne);
		const auto [i, j] = _getPairIndices(Range);

		// update a pair of values (x,y) such that sqrt(x^2 + y^2) does not change -> vector norm remains the same
		const auto x = (Type)_storage[i];
		const auto y = (Type)_storage[j];

		// slope as vector
		const double s_x     = 1.0;
		const double s_y     = -x / y;
		const double norm_s  = sqrt(s_x + s_y * s_y);
		const double slope_x = s_x / norm_s;
		const double slope_y = s_y / norm_s;

		// random step on tangent
		// Note: always take proposal width of ith element (if unique proposal widths)
		const auto alpha   = _updater->propose(0.0, i);
		const auto x_prime = x + alpha * slope_x;
		const auto y_prime = y + alpha * slope_y;

		// adjust ratio and set new values
		const auto ratio = sqrt((x * x + y * y) / (x_prime * x_prime + y_prime * y_prime));
		_storage[i]      = x_prime * ratio;
		_storage[j]      = y_prime * ratio;
	}

	void _proposePairSumOne(const coretools::TRange &Range) {
		static_assert(Constraint == Constraints::sumOne);
		const auto [i, j] = _getPairIndices(Range);

		const auto x = (Type)_storage[i];
		const auto y = (Type)_storage[j];

		// set new maximum
		const auto min = Type::min();
		const auto max = x + y;

		// set proposal width: scaled by length of (x,y) line
		const double oldPropWidth = _updater->proposalWidth(i);
		const double propWidth    = oldPropWidth * sqrt(x * x + y * y);

		// propose new x'
		const auto x_prime = _updater->propose(x, i, min, max, propWidth);

		// set x' and y'
		const auto y_prime = x + y - x_prime;
		_storage[i]        = (Type)x_prime;
		_storage[j]        = (Type)y_prime;
	}

	void _accept(const coretools::TRange &Range) {
		for (size_t i = Range.begin; i < Range.end; i += Range.increment) { addToMeanVar(i); }
	}

	void _reject(const coretools::TRange &RangeUpdated, const coretools::TRange &RangeProposed) {
		// reset storage
		for (size_t i = RangeUpdated.begin; i < RangeUpdated.end; i += RangeUpdated.increment) {
			_storage[i].reset();
			addToMeanVar(i);
		}

		// reject update (for tuning acceptance rate)
		for (size_t i = RangeProposed.begin; i < RangeProposed.end; i += RangeProposed.increment) {
			_updater->reject(i);
		}
	}

	bool _acceptOrReject(double LogH, const coretools::TRange &RangeUpdated, const coretools::TRange &RangeProposed) {
		if (!_updater->isUpdated()) { return false; }
		if (evalLogH(LogH)) {
			_accept(RangeUpdated);
			return true;
		} else {
			_reject(RangeUpdated, RangeProposed);
			return false;
		}
	}

	template<bool ProbsAreLog, bool MustNormalize, typename ContainerType>
	void _sampleDiscrete(size_t ix, ContainerType Posterior) {
		static_assert(TypesAreUnsignedInteger<Type>());

		if constexpr (ProbsAreLog) { coretools::normalizeLogSumExp(Posterior); }
		if constexpr (MustNormalize || ProbsAreLog) {
			// normalize again if ProbsAreInLog since there are cases where sum != 1 due to numerics
			coretools::normalize(Posterior);
		}
		assert(std::fabs(1.0 - coretools::containerSum(Posterior)) < 1e-10);

		_storage[ix] = (Type)coretools::instances::randomGenerator().pickOneRawProbabilities(Posterior);
		addToMeanVar(ix);
	}

	template<bool ProbsAreLog, bool MustNormalize, typename ContainerType>
	void _sampleBinary(size_t ix, ContainerType Posterior) {
		static_assert(TypesAreBool<Type>());

		if constexpr (ProbsAreLog) {
			const double logQ = Posterior[1] - Posterior[0];
			_storage[ix]      = coretools::TAcceptOddsRatio::accept(logQ);
		} else {
			if constexpr (MustNormalize) { coretools::normalize(Posterior); }
			_storage[ix] = (Type)coretools::instances::randomGenerator().pickOneOfTwo(Posterior);
		}
		addToMeanVar(ix);
	}

	[[nodiscard]] size_t _getModeFromCounts(size_t i) const {
		if (_counts.empty()) { throw coretools::TDevError("Counts were not stored for parameter ", this->name(), "!"); }
		if (_counts[i].counts() == 0) { return (Type)_storage[i]; } // has never been updated
		return _counts[i].mode();
	}

	template<typename IndexType = size_t> const coretools::TMeanVar<double> &_getMeanVar(IndexType i) const {
		if (_meanVar.empty()) { throw coretools::TDevError("Mean and var were not stored for parameter ", this->name(), "!"); }
		size_t ix = this->getIndex(i);
		return _meanVar[ix];
	}

	void _writeToMeanVar(coretools::TOutputMaybeRcppFile &File) const {
		for (size_t i = 0; i < this->size(); ++i) {
			File << this->getFullName(i) << mean(i) << var(i) << coretools::endl;
		}
	}

	void _writeToStatePosteriors(coretools::TOutputMaybeRcppFile &File) const {
		for (size_t i = 0; i < this->size(); ++i) {
			File << this->getFullName(i);
			for (size_t s = 0; s < getNumStatesForStatePosterior(); ++s) { File << statePosteriors(i, s); }
			File.endln();
		}
	}

	void _writeToPosteriorMode(coretools::TOutputMaybeRcppFile &File) const {
		for (size_t i = 0; i < this->size(); ++i) {
			File << this->getFullName(i) << posteriorModeName(i) << coretools::endl;
		}
	}

	void _construct(const TParameterDefinition &Def) {
		_def = Def;

		// add to DAGBuilder
		instances::dagBuilder().addToDAG(this);

		// set default output file type in definition
		if (_writeStatePos()) {
			_def.editFile(MCMCFiles::statePosteriors);
			_def.editFile(MCMCFiles::posteriorMode);
		} else {
			_def.editFile(MCMCFiles::meanVar);
		}
	}

public:
	// enabled if DoRJMCMC is false and MarkovOrder is not different (fixed)
	// -> we don't need to know the Markov Order for parallelization
	template<bool R = DoRJMCMC, MarkovOrder M = MOrder,
	         typename std::enable_if_t<(!R && M != MarkovOrder::different), int> = 0>
	TParameter(std::string_view Name, TypeBoxAbove *BoxAbove, const TParameterDefinition &Def)
	    : Base(Name, BoxAbove, Def) {
		_construct(Def);
	}

	// enabled if DoRJMCMC is false and MarkovOrder is different
	// -> we need to know the Markov Order for parallelization
	template<bool R = DoRJMCMC, MarkovOrder M = MOrder,
	         typename std::enable_if_t<(!R && M == MarkovOrder::different), int> = 0>
	TParameter(std::string_view Name, TypeBoxAbove *BoxAbove, const TParameterDefinition &Def,
	           const std::array<size_t, NumDim> &MarkovOrder)
	    : Base(Name, BoxAbove, Def) {
		_markovOrder = MarkovOrder;
		_construct(Def);
	}

	// enabled if DoRJMCMC is true and MarkovOrder is not different (fixed)
	// -> we don't need to know the Markov Order for parallelization, but we need RJ-MCMC stuff
	template<bool R = DoRJMCMC, MarkovOrder M = MOrder,
	         typename std::enable_if_t<(R && M != MarkovOrder::different), int> = 0>
	TParameter(std::string_view Name, TypeBoxAbove *BoxAbove, const TParameterDefinition &Def,
	           TParameter<SpecRJMCMCModelParameter, TypeBoxAround> *Model)
	    : Base(Name, BoxAbove, Def) {
		_rjModel = Model;
		_rjModel->setIsRJMCMCModelNode();
		_construct(Def);
	}

	// enabled if DoRJMCMC is true and MarkovOrder is different
	// -> we need to know the Markov Order for parallelization, and we need RJ-MCMC stuff
	template<bool R = DoRJMCMC, MarkovOrder M = MOrder,
	         typename std::enable_if_t<(R && M == MarkovOrder::different), int> = 0>
	TParameter(std::string_view Name, TypeBoxAbove *BoxAbove, const TParameterDefinition &Def,
	           TParameter<SpecRJMCMCModelParameter, TypeBoxAround> *Model,
	           const std::array<size_t, NumDim> &MarkovOrder)
	    : Base(Name, BoxAbove, Def) {
		_rjModel = Model;
		_rjModel->setIsRJMCMCModelNode();
		_markovOrder = MarkovOrder;
		_construct(Def);
	}

	~TParameter() override = default;

	void initStorage(TypeBoxAround *BoxAround, const std::array<size_t, NumDim> &ExpectedDimensions = {1},
	                 const std::array<std::shared_ptr<coretools::TNamesEmpty>, NumDim> &DimensionNames = {
	                     std::make_shared<coretools::TNamesStrings>(1)}) {
		_boxAround = BoxAround;

		// initialize vector of current and old values
		_storage.resize(ExpectedDimensions);

		// set dimension names
		_storage.setDimensionNames(DimensionNames);

		// initialize all the rest (meanVar / initValues / jumpSizes / fixedInitialValue)
		_initMeanVar_InitVals_JumpSizes();

		// initialize index picker
		_initIndexPicker();

		// initialize RJ-MCMC stuff
		_initRJMCMC();
	}

	void setProposalKernel(std::unique_ptr<TPropKernelBase<Type, UnderlyingType>> PropKernel) {
		// use custom proposal kernel
		_updater.reset();
		_initProposalWidths(std::move(PropKernel));
	}

	void setUpdateWeights(const std::array<std::vector<double>, NumDim> &Statistics,
	                      coretools::Positive FracUpdatesPerIter) {
		// function that takes array of weights (will be combined later)
		for (auto &it : _indexPicker) { it.setWeights(Statistics, _def.argsWeightedUpdates(), FracUpdatesPerIter); }
	}

	void setUpdateWeights(const std::vector<double> &LinearWeights, coretools::Positive FracUpdatesPerIter) {
		// function that takes weights per index (linearized)
		for (auto &it : _indexPicker) { it.setWeights(LinearWeights, FracUpdatesPerIter); }
	}

	void guessInitialValues() override {
		Base::guessInitialValues();

		if (_def.scaleInitPropKernelToValue()) {
			// now that parameter has initial value: scale proposal width accordingly
			if (_updater->proposalWidthIsShared()) {
				// calculate mean value of all parameters
				_updater->scaleProposalWidthsToValue((Type)this->_storage.mean(), 0);
			} else {
				// scale proposal width per parameter
				for (size_t i = 0; i < this->size(); i++) {
					_updater->scaleProposalWidthsToValue((Type)_storage[i], i);
				}
			}
		}
	}

	template<typename IndexType = size_t> Type oldValue(IndexType i = 0) const {
		return _storage[this->getIndex(i)].oldValue();
	}

	//--------------------------------
	// updating

	[[nodiscard]] constexpr bool isDeterministic() const override { return IsDeterministic; }
	void setIsRJMCMCModelNode() { _isRJMCMCModelNode = true; }
	[[nodiscard]] constexpr bool isRJMCMCModel() const override { return _isRJMCMCModelNode; }
	[[nodiscard]] constexpr bool isExcludedFromDAGUpdates() const override { return _def.isExcludedFromDAGUpdates(); }

	[[nodiscard]] bool isUpdated() const override { return _updater->isUpdated(); }
	[[nodiscard]] bool skipThisIter(size_t Iteration) const {
		return _def.doUpdateEveryNthIter() && Iteration % _def.updateEveryNthIter() != 0;
	}

	void propose(const coretools::TRange &Range) {
		if (!isUpdated()) { return; }

		if constexpr (UpdateType == UpdateTypes::pair) {
			if constexpr (Constraint == Constraints::sumOne) {
				_proposePairSumOne(Range);
			} else if constexpr (Constraint == Constraints::lengthOne) {
				_proposePairLengthOne(Range);
			}
		} else { // UpdateTypes::single or UpdateTypes::joint
			_propose(Range);
			if constexpr (Constraint == Constraints::sumOne || Constraint == Constraints::lengthOne) {
				normalize(Range);
			}
		}
	}

	bool acceptOrReject(double LogH, const coretools::TRange &Range) {
		if constexpr (UpdateType == UpdateTypes::pair) {
			return _acceptOrReject(
			    LogH, Range,
			    coretools::TRange(Range.begin)); // only first index of pair was actually used to propose value
		} else {
			return _acceptOrReject(LogH, Range, Range);
		}
	}

	bool acceptOrReject(double LogH, const coretools::TRange &RangeUpdated, const coretools::TRange &RangeProposed) {
		return _acceptOrReject(LogH, RangeUpdated, RangeProposed);
	}

	template<bool ProbsAreLog, bool MustNormalize, typename IndexType = size_t, typename ContainerType>
	bool sampleDiscrete(IndexType i, ContainerType Posterior) {
		if (!_updater->isUpdated()) { return false; }

		size_t ix = this->getIndex(i);
		if constexpr (TypesAreBool<Type>()) {
			_sampleBinary<ProbsAreLog, MustNormalize, ContainerType>(ix, Posterior);
		} else {
			_sampleDiscrete<ProbsAreLog, MustNormalize, ContainerType>(ix, Posterior);
		}
		return true;
	}

	template<class Function, class... AdditionalArguments, class IndexType = size_t>
	bool sample(IndexType i, Function &Func, const AdditionalArguments &...AdditionalArgs) {
		if (!_updater->isUpdated()) { return false; }

		// sample from posterior probabilities
		size_t ix    = this->getIndex(i);
		_storage[ix] = (Type)(coretools::instances::randomGenerator().*Func)(AdditionalArgs...);
		addToMeanVar(ix);
		return true;
	}

	void update(size_t Iteration) override {
		assert(!IsDeterministic);
		if (!isUpdated()) { return; }
		if (skipThisIter(Iteration)) { return; }

		for (size_t u = 0; u < NumUpdatesPerIteration; ++u) { // loop over updates per iteration
			for (size_t p = 0; p < _indexPicker.size(); ++p) {
				_indexPicker[p].prepareIteration();

#ifdef _OPENMP
#pragma omp parallel for num_threads(_numThreads[p]) schedule(static)
#endif
				for (size_t i = 0; i < _indexPicker[p].numUpdates(); ++i) {
#ifdef _OPENMP
					size_t thread = omp_get_thread_num();
#else
					size_t thread = 0;
#endif
					const coretools::TRange range = _indexPicker[p].pick(i, thread);
					update(range);
				}
			}
		}
	}

	void update(const coretools::TRange &Range) {
		// update a specific range
		bool accepted = _updateRange(Range);
		updateTempVals(Range, accepted);
	}

	double calculateLLRatio(const coretools::TRange &Range) override {
		if constexpr (IsDeterministic) { // first set values deterministically
			_setFromDeterministic(Range);
		}

		if constexpr (TypeBoxAround::isDeterministic) {
			// box around is deterministic -> don't call sumLLRatio, loop over storages below is handled in there
			return _boxAround->calculateLLRatio(this, Range);
		} else if constexpr (impl::calculateLLRatioTakesRange_v<ThisType, TypeBoxAround>) {
			// boxAround takes TRange
			return _boxAround->sumLLRatio(_boxAround->calculateLLRatio(this, Range));
		} else if constexpr (impl::calculateLLRatioTakesSizeT_v<ThisType, TypeBoxAround>) {
			// boxAround takes size_t
			if constexpr (UpdateType == UpdateTypes::one && !IsDeterministic) {
				// no need to loop over TRange
				assert(Range.begin + Range.increment >= Range.end); // single element
				return _boxAround->sumLLRatio(_boxAround->calculateLLRatio(this, Range.begin));
			} else {
				double sum = 0.0;
				for (size_t i = Range.begin; i < Range.end; i += Range.increment) {
					sum += _boxAround->sumLLRatio(_boxAround->calculateLLRatio(this, i));
				}
				return sum;
			}
		} else {
			throw coretools::TDevError("Function 'calculateLLRatio' for parameter ", this->name(), " is required but not implemented!");
		}
	}

	void setAllTempVals() override {
		// update all temporary values before starting the MCMC
		// properly follow index picker restrictions to make sure independence assumptions are met
		// (just like when updating)
		if (!isUpdated() || IsDeterministic) { return; }
		for (size_t p = 0; p < _indexPicker.size(); ++p) {
			_indexPicker[p].prepareIteration();
			for (size_t i = 0; i < _indexPicker[p].numUpdates(); ++i) {
				const coretools::TRange range = _indexPicker[p].pick(i, 0); // thread = 0
				updateTempVals(range, true);
			}
		}
	}

	void updateTempVals(const coretools::TRange &Range, bool Accepted) override {
		if constexpr (IsDeterministic) { // first reset values deterministically
			_reSetFromDeterministic(Range, Accepted);
		}

		if constexpr (impl::updateTempValsTakesRange_v<ThisType, TypeBoxAround>) {
			// boxAround takes TRange
			_boxAround->updateTempVals(this, Range, Accepted);
		} else if constexpr (impl::updateTempValsTakesSizeT_v<ThisType, TypeBoxAround>) {
			// boxAround takes size_t -> loop in here
			for (size_t i = Range.begin; i < Range.end; i += Range.increment) {
				_boxAround->updateTempVals(this, i, Accepted);
			}
		} else {
			throw coretools::TDevError("Function 'updateTempVals' for parameter ", this->name(), " is required but not implemented!");
		}
	}

	auto proposeNewValueRJMCMC() const { return _rjProposalDistr.sample(); }

	//--------------------------------
	// prior ratios and densities

	template<typename IndexType = size_t> [[nodiscard]] double getLogDensityRatio(IndexType i = 0) const noexcept {
		assert(!IsDeterministic);

		size_t ix = this->getIndex(i);

		// return log(proposal ratio) + log(prior ratio)
		if (_updater->isUpdated()) {
			double logProposalRatio = _updater->logHastingsRatioPropKernel((Type)_storage[ix], _storage[ix].oldValue());
			double logPriorRatio    = _boxAbove->getLogDensityRatio(this->storage(), ix);
			return logProposalRatio + logPriorRatio;
		}
		return 0.;
	}

	[[nodiscard]] double getLogDensityRatio(const coretools::TRange &Range) const noexcept {
		assert(!IsDeterministic);

		double sum = 0.0;
		for (size_t r = Range.begin; r < Range.end; r += Range.increment) { sum += getLogDensityRatio(r); }
		return sum;
	}

	template<typename IndexType = size_t> [[nodiscard]] double getDensity(IndexType i = 0) const noexcept {
		assert(!IsDeterministic);

		if (_updater->isUpdated()) { return _boxAbove->getDensity(this->storage(), this->getIndex(i)); }
		return 1.;
	}

	template<typename IndexType = size_t> [[nodiscard]] double getLogDensity(IndexType i = 0) const noexcept {
		assert(!IsDeterministic);

		if (_updater->isUpdated()) { return _boxAbove->getLogDensity(this->storage(), this->getIndex(i)); }
		return 1.;
	}

	[[nodiscard]] double getSumLogPriorDensity() const noexcept override {
		if (_updater->isUpdated()) { return Base::getSumLogPriorDensity(); }
		return 0.;
	}

	//--------------------------------
	// EM
	void runEMEstimation(TLatentVariable<double, size_t, size_t> &LatentVariable) {
		_boxAbove->runEMEstimation(LatentVariable);
	}

	void estimateStatePosteriors(TLatentVariable<double, size_t, size_t> &LatentVariable) {
		_boxAbove->estimateStatePosteriors(LatentVariable);
	}

	void switchPriorClassificationAfterEM() { _boxAbove->switchPriorClassificationAfterEM(); }

	//--------------------------------
	// set functions

	void setIsUpdated(bool Update) { _updater->setIsUpdated(Update); }

	using Base::set;

	void set(size_t i, Type val) override {
		assert(i < _storage.size());
		if (this->initializationIsRunning()) { // we are currently initializing the parameters (e.g. MLE)
			if (!this->hasFixedInitialValue()) { _storage[i] = val; } // only set if the initial value is not fixed
		} else {                                                      // we are currently in MCMC
			if (_updater->isUpdated()) { _storage[i] = val; }         // only set if is updated
		}
	}

	void set(const std::array<size_t, NumDim> &coord, Type val) override {
		// overridden to prevent overhead from virtual dispatch
		set(_storage.getIndex(coord), val);
	}

	void setFromString(const std::string &Values) override {
		// set values from comma-separated string
		_readInitVals(Values, false);
	}

	void setProposalWidths(const std::string &Values) override {
		// set jump sizes from comma-separated string
		_updater->setProposalWidth(Values);
	}

	void setProposalWidth(const Type val, size_t index) {
		// set jump sizes from comma-separated string
		_updater->setProposalWidth(val, index);
	}

	template<typename IndexType = size_t> void reset(IndexType i = 0) {
		size_t ix = this->getIndex(i);
		if (_updater->isUpdated()) { _storage[ix].reset(); }
	}

	template<typename IndexType = size_t> void setVal(IndexType i, Type val) {
		size_t ix = this->getIndex(i);
		if (_updater->isUpdated()) { _storage[ix].setVal(val); }
	}

	void setVal(Type val) { setVal(0, val); }

	void normalize(const coretools::TRange &Range) {
		// set all values from Range such that they sum to one
		// use setVal: set only value, don't modify old_value to get proper old value for Hastings ratio
		double s = 1.0;
		if constexpr (Constraint == Constraints::sumOne) {
			s = this->sum(Range);
		} else if constexpr (Constraint == Constraints::lengthOne) {
			s = this->vectorNorm(Range);
		}
		if (s != 1.) { // normalize
			for (size_t i = Range.begin; i < Range.end; i += Range.increment) { setVal(i, (Type)_storage[i] / s); }
		}
	}

	//--------------------------------
	// about posterior mean/var/counts

	template<typename IndexType = size_t> void addToMeanVar(IndexType i = 0) {
		if (!_updater->isUpdated()) { return; }

		size_t ix = this->getIndex(i);
		if (!_meanVar.empty()) { _meanVar[ix].add((Type)_storage[ix]); }
		if (!_counts.empty()) { _counts[ix].add((Type)_storage[ix]); }
	}

	void clearMeanVar() override {
		// re-set mean and var after burnin
		if (!_updater->isUpdated()) { return; }
		for (auto &it : _meanVar) { it.clear(); }
		for (auto &it : _counts) { it.reset(); }
	}

	template<typename IndexType = size_t> uint64_t numUpdates(IndexType i) const {
		auto mV = _getMeanVar(i);
		return (mV.counts());
	}

	template<typename IndexType = size_t> double mean(IndexType i) const {
		auto mV = _getMeanVar(i);
		if (mV.counts() == 0) { return (Type)_storage[this->getIndex(i)]; } // value if parameter has never been updated
		return mV.mean();
	}

	std::string getPosteriorMeans() const override {
		std::string tmp;
		for (size_t i = 0; i < _storage.size(); ++i) {
			if (i > 0) { tmp.append(","); }
			tmp.append(coretools::str::toString(mean(i)));
		}
		return tmp;
	}

	template<typename IndexType = size_t> double var(IndexType i) const { return _getMeanVar(i).variance(); }
	template<typename IndexType = size_t> double sd(IndexType i) const { return _getMeanVar(i).sd(); }

	double fracCounts(size_t i, Type Value) const {
		assert(i < _storage.size());
		if (!_counts.empty()) {
			if (_counts[i].counts() == 0) { return Value == (Type)_storage[i]; } // has never been updated
			return _counts[i].frac(Value);
		}
		throw coretools::TDevError("Counts were not stored for parameter ", this->name(), "!");
	}

	template<typename IndexType = size_t> [[nodiscard]] double statePosteriors(IndexType i, size_t Value) const {
		size_t ix = this->getIndex(i);
		if (_updater->isUpdated()) {
			return fracCounts(ix, Type(coretools::underlyingType_t<Type>(Value)));
		} else {
			if (Value == (Type)_storage[ix]) { return 1.0; }
			return 0.0;
		}
	}

	template<typename IndexType = size_t> [[nodiscard]] size_t posteriorMode(IndexType i) const {
		return _getModeFromCounts(this->getIndex(i));
	}

	template<typename IndexType = size_t> [[nodiscard]] std::string posteriorModeName(IndexType i) const {
		size_t state = _getModeFromCounts(this->getIndex(i));
		if (!_def.getStateNames().empty()) {
			assert(getNumStatesForStatePosterior() == _def.getStateNames().size());
			assert(state < _def.getStateNames().size());
			return _def.getStateNames()[state];
		}
		return coretools::str::toString(state);
	}

	[[nodiscard]] size_t getNumStatesForStatePosterior() const override { return (size_t)Type::max() + 1; }

	//--------------------------------
	// writing

	void writeValsOneString(coretools::TOutputMaybeRcppFile &file) const override {
		// values (comma-separated)
		coretools::TRange range = _storage.getFull();
#ifdef USE_RCPP
		std::string s;
		for (size_t i = range.begin; i < range.end; i += range.increment) {
			s.append(coretools::str::toString((Type)_storage[i]));
			s.append(",");
		}
		file << s;
#else
		for (size_t i = range.begin; i < range.end; i += range.increment) { file.writeNoDelim((Type)_storage[i], ","); }
		file.writeDelim();
#endif
	}

	void writeToSummary(MCMCFiles FileType, coretools::TOutputMaybeRcppFile &File) const override {
		switch (FileType) {
		case MCMCFiles::trace: return this->writeToTrace(File);
		case MCMCFiles::meanVar: return _writeToMeanVar(File);
		case MCMCFiles::statePosteriors: return _writeToStatePosteriors(File);
		case MCMCFiles::posteriorMode: return _writeToPosteriorMode(File);
		case MCMCFiles::simulation: return this->_writeToSimulation(File);
		}
	}

	void writeJumpSizeOneString(coretools::TOutputMaybeRcppFile &file) const override {
		file << _updater->getAllProposalWidthsAsString(",");
	}

	//--------------------------------
	// getters

	TUpdateBase *getPtrToUpdater() override { return _updater.get(); }
	TParameterDefinition &getDefinition() override { return _def; }
};

} // end namespace stattools

#endif // TPARAMETER_H
