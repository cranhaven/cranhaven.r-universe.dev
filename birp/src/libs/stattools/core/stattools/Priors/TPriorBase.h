//
// Created by madleina on 11.06.21.
//

#ifndef TPRIORBASE_H
#define TPRIORBASE_H

#include <functional>

#include "coretools/Main/TError.h"
#include "coretools/Math/TMeanVar.h"
#include "coretools/Storage/TStorage.h"
#include "coretools/Strings/convertString.h"
#include "stattools/DAG/TDAG.h"
#include "stattools/EM/TLatentVariable.h"
#include "stattools/ParametersObservations/TNodeBase.h"
#include "stattools/ParametersObservations/TValue.h"

namespace stattools::prior {

template<typename Derived, typename Type, size_t NumDim, bool IsDeterministic> class TBase {
public:
	static constexpr bool belowIsParameter = std::is_same_v<Derived, TParameterBase>;
	using StorageType                      = std::conditional_t<belowIsParameter, TValueUpdated<Type>, Type>;
	using Storage                          = coretools::TMultiDimensionalStorage<StorageType, NumDim>;
	using UpdatedStorage                   = coretools::TMultiDimensionalStorage<TValueUpdated<Type>, NumDim>;
	using type                             = Type;
	static constexpr size_t numDim         = NumDim;
	constexpr static bool isDeterministic  = IsDeterministic;

private:
	// vector of pointers to parameter/observation on which this box is specified ("below" this box)
	// only used for building the DAG
	std::vector<TNodeBase *> _nodesBelowBox;

	// vector of pointers to prior parameters and observations within the box
	std::vector<TNodeBase *> _nodesWithinBox;

	// functions needed within stattools for assembling DAG
	void _flagPriorParameters() {
		for (auto &node : _nodesWithinBox) { node->setIsPartOfBox(); }
	}

	bool _allNodesBelowExistInDAG(const TDAG &DAG) {
		// go through all nodes below and check if they already exist in DAG
		return std::all_of(_nodesBelowBox.begin(), _nodesBelowBox.end(),
		                   [&DAG](auto node) { return DAG.nodeExists(node); });
	}

	void _addFirstNodeBelowToDAG(TDAG &DAG) {
		// add first parameter of _parameter vector to final DAG
		assert(!_nodesBelowBox.empty());
		DAG.add(_nodesBelowBox[0]);
	}

protected:
	// vector of pointers to parameter/observation on which this prior is specified ("below" this box)
	std::vector<Storage *> _storageBelow;

	// vector of pointers to prior parameters "below" this box
	std::vector<TParameterBase *> _parametersBelow;

	// functions needed within stattools for assembling DAG
	void _addBelow(TNodeBase *Node, Storage *Storage) {
		_nodesBelowBox.push_back(Node);
		_storageBelow.push_back(Storage);
		if constexpr (belowIsParameter) { _parametersBelow.push_back(dynamic_cast<TParameterBase *>(Node)); }
	};

	// functions to by overridden by developer
	virtual void _simulateUnderPrior(Storage *Data) = 0;

public:
	virtual ~TBase() = default;

	// functions needed within stattools
	virtual void addBelow(TNodeBase *Node, Storage *Storage) { _addBelow(Node, Storage); }

	void constructDAG(TDAG &DAG, TDAG &temporaryDAG) {
		if (_allNodesBelowExistInDAG(temporaryDAG)) {
			_addFirstNodeBelowToDAG(DAG);

			// go over all nodes within box
			for (auto &node : _nodesWithinBox) { node->constructDAG(DAG, temporaryDAG); }
		}
	};

	void addPriorParameter(TNodeBase *Param) {
		_nodesWithinBox.push_back(Param);
		_flagPriorParameters();
	}

	void addPriorParameter(const std::vector<TNodeBase *> &Param) {
		_nodesWithinBox.insert(_nodesWithinBox.end(), Param.begin(), Param.end());
		_flagPriorParameters();
	}

	template<typename T, size_t Size> void addPriorParameter(const std::array<T, Size> &Param) {
		_nodesWithinBox.insert(_nodesWithinBox.end(), Param.begin(), Param.end());
		_flagPriorParameters();
	}

	double sumLLRatio(std::function<double(Storage *)> Function) const {
		return std::accumulate(_storageBelow.begin(), _storageBelow.end(), 0.0,
		                       [&Function](double sum, Storage *Data) { return sum + Function(Data); });
	}

	double sumLLRatio(double Val) const { return Val; } // used if calculateLLRatio() function does not return lambda

	// simulate values
	void simulateUnderPrior() {
		for (size_t i = 0; i < _storageBelow.size(); i++) {
			if (!_nodesBelowBox[i]->hasFixedInitialValue()) { _simulateUnderPrior(_storageBelow[i]); }
		}
	};

	//----------------------------------------
	// functions to by overridden by developer
	//----------------------------------------

	[[nodiscard]] virtual std::string name() const = 0;

	virtual void initialize() {
		// only override if there are prior parameters
		if (!_nodesWithinBox.empty()) {
			throw coretools::TUserError("The box '", name(), "' contains ", _nodesWithinBox.size(),
			       " parameters, but the function "
			       "'initialize' is not overridden. Please implement this function.");
		}
	}

	virtual void setFixedPriorParameters(std::string_view /*Parameters*/) {
		// only override if there are no prior parameters
		if (_nodesWithinBox.empty()) {
			throw coretools::TUserError("The box '", name(),
			       "' does not infer any parameters, but the function "
			       "'setFixedPriorParameters' is not overridden. Please implement this function.");
		}
	}

	virtual void guessInitialValues() {
		// only override if there are prior parameters
		if (!_nodesWithinBox.empty()) {
			throw coretools::TUserError("The box '", name(), "' contains ", _nodesWithinBox.size(),
			       " parameters, but the function "
			       "'guessInitialValues' is not overridden. Please implement this function.");
		}
	}

	virtual void burninHasFinished() {
		// communicates to prior that burnin has finished and MCMC chain will start
		// can stay empty
	}

	virtual void MCMCHasFinished() {
		// communicates to prior that MCMC has finished
		// can stay empty
	}

	// functions needed for distributions that use EM for initialization
	virtual void runEMEstimation(TLatentVariable<double, size_t, size_t> &) {
		// function can stay empty if there is no EM
	};
	virtual void estimateStatePosteriors(TLatentVariable<double, size_t, size_t> &) {
		// function can stay empty if there is no EM
	};
	virtual void switchPriorClassificationAfterEM() {
		// function can stay empty if there is no EM
	};

	const std::vector<TNodeBase *> &getAllPriorParameters() const { return _nodesWithinBox; };
};

//-------------------------------------------
// TStochasticBase
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TStochasticBase : public TBase<Derived, Type, NumDim, false> {
private:
public:
	// type aliases
	using Base           = TBase<Derived, Type, NumDim, false>;
	using StorageType    = typename Base::StorageType;
	using Storage        = typename Base::Storage;
	using UpdatedStorage = typename Base::UpdatedStorage;

	//----------------------------------------
	// functions to by overridden by developer
	//----------------------------------------

	virtual double getDensity(const Storage &, size_t) const         = 0;
	virtual double getLogDensityRatio(const UpdatedStorage &, size_t) const = 0;

	virtual double getLogDensity(const Storage &Data, size_t i) const {
		// default function for calculating the log prior density: take log(density)
		// can be overridden in derived classes if there is a more efficient way
		return log(getDensity(Data, i));
	};

	virtual double getSumLogPriorDensity(const Storage &Data) const {
		// default function for calculating the sum over all log prior densities
		// can be overridden in derived classes
		double sum = 0.;
		for (size_t i = 0; i < Data.size(); ++i) { sum += getLogDensity(Data, i); }
		return sum;
	};
};

//-------------------------------------------
// TDeterministicBase
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TDeterministicBase : public TBase<Derived, Type, NumDim, true> {
public:
	// type aliases
	using Base           = TBase<Derived, Type, NumDim, false>;
	using StorageType    = typename Base::StorageType;
	using Storage        = typename Base::Storage;
	using UpdatedStorage = typename Base::UpdatedStorage;

protected:
	void _simulateUnderPrior(Storage *Data) override {
		for (size_t i = 0; i < Data->size(); ++i) { (*Data)[i] = Type(valueForBelow(i)); }
	};

public:
	//----------------------------------------
	// functions needed within stattools
	//----------------------------------------

	void addBelow(TNodeBase *Parameter, Storage *Storage) override {
		if (this->_storageBelow.size() > 0) { // make sure there is only one below for a deterministic box
			throw coretools::TUserError("A deterministic box (", this->name(),
			       ") can not be shared among multiple parameters (adding parameter ", Parameter->name(),
			       " exceeds 1.)");
		}
		this->_addBelow(Parameter, Storage);
	};

	// density functions: needed such that it has same interface as TStochasticBase
	// but should acutally never be used
	double getDensity(const Storage &, size_t) const { return 1.0; }
	double getLogDensityRatio(const UpdatedStorage &, size_t) const { return 0.0; }
	double getLogDensity(const Storage &, size_t) const { return 0.0; };
	double getSumLogPriorDensity(const Storage &) const { return 0.0; };

	//----------------------------------------
	// functions to by overridden by developer
	//----------------------------------------

	[[nodiscard]] virtual double valueForBelow(size_t i) const = 0;
};

//-------------------------------------------
// TBaseLikelihoodPrior
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim>
class TBaseLikelihoodPrior : public TStochasticBase<Derived, Type, NumDim> {
	// class that overrides all prior ratio / density functions and throws if they are called
	// -> used for priors that calculate LL for specific models (e.g. Bangolin, ApproxWF, Inbreeding etc.)
	// where we don't know / don't need to implement all different LL's
public:
	~TBaseLikelihoodPrior() override = default;

	using typename TStochasticBase<Derived, Type, NumDim>::Storage;
	using typename TStochasticBase<Derived, Type, NumDim>::UpdatedStorage;

	double getDensity(const Storage &, size_t) const override { throw coretools::TDevError("not implemented."); };
	double getLogDensityRatio(const UpdatedStorage &, size_t) const override { throw coretools::TDevError("not implemented."); };
};

//-------------------------------------------
// Helper functions
//-------------------------------------------

namespace impl {

template<class ParameterType>
std::vector<ParameterType> readDistributionParams_SameTypeUnknownSize(std::string_view String,
                                                                      std::string_view CorrectFormat) {
	std::vector<ParameterType> result;

	size_t counter = 0;
	coretools::str::TSplitter splitter(String, ',');
	for (auto sp : splitter) {
		ParameterType tmp;
		coretools::str::impl::convertOneParam(sp, CorrectFormat, counter, tmp);
		result.push_back(tmp);
		counter++;
	}
	return result;
}

template<typename T, typename Type> auto calculateMeanVarOfStorages(const std::vector<T> &Storages) {
	// vector of Storages
	coretools::TMeanVar<double> meanVar;
	for (const auto &storage : Storages) {
		for (size_t i = 0; i < storage->size(); ++i) { meanVar.add((Type)(*storage)[i]); }
	}
	return std::make_pair(meanVar.mean(), meanVar.variance());
}

template<typename T, typename Type> double calculateMeanOfStorages(const std::vector<T> &Storages) {
	// vector of Storages
	return calculateMeanVarOfStorages<T, Type>(Storages).first;
}

template<typename T> void checkSameDimStorageBelow(const std::vector<T> &Storages, std::string_view Name) {
	// check if all storages below have the same dimensions
	auto dim = Storages.front()->dimensions();
	for (const auto &storage : Storages) {
		if (storage->dimensions() != dim) {
			throw coretools::TDevError("Mismatch in dimensions of storageBelow in ", Name, ": ", dim, " vs ", storage->dimensions(), "!");
		}
	}
}

}; // namespace impl

} // end namespace stattools::prior

#endif // TPRIORBASE_H
