//
// Created by madleina on 08.02.21.
//

#ifndef TDAG_H
#define TDAG_H

#include "stattools/ParametersObservations/TNodeBase.h"
#include <vector>
#include <functional>

namespace stattools {

class TDAG {
protected:
	std::vector<TNodeBase *> _uniqueNodesBelowBoxes;
	std::vector<TParameterBase *> _nodesToUpdate;

	// vector of functions that are called in each iteration
	// used when update should not run via nodes, but via box
	std::vector<std::function<void()>> _funcUpdates;

public:
	// functions to build DAG
	bool nodeExists(const TNodeBase *Node) const {
		for (auto &node : _uniqueNodesBelowBoxes) {
			if (node->name() == Node->name()) { return true; }
		}
		return false;
	};

	void add(TNodeBase *node) { _uniqueNodesBelowBoxes.push_back(node); };

	void fillNodesToUpdate(const std::vector<TParameterBase *> &AllParameters) {
		for (auto &node : AllParameters) {
			if (!node->isDeterministic() && !node->isRJMCMCModel() && !node->isExcludedFromDAGUpdates()) {
				_nodesToUpdate.push_back(node);
			}
		}
	}

	void addFuncToUpdate(const std::function<void()> &Func) { _funcUpdates.push_back(Func); }

	void initializeStorage() {
		for (auto &node : _uniqueNodesBelowBoxes) { node->tellBoxAboveToInitStorage(); }
	}

	// functions for initializing and updating DAG
	void guessInitialValues(const std::vector<TParameterBase *> &AllParameters) {
		// go left -> right through vector
		for (auto &node : AllParameters) { node->startInitialization(); }
		for (auto &node : _uniqueNodesBelowBoxes) { node->guessInitialValues(); }
		for (auto &node : AllParameters) { node->endInitialization(); }

		// call updateTempVals to make sure all temporary values are set correctly before starting MCMC
		for (auto &node : _nodesToUpdate) { node->setAllTempVals(); }
	}

	void tellBoxAboveThatBurninFinished() {
		for (auto &node : _uniqueNodesBelowBoxes) { node->tellBoxAboveThatBurninFinished(); }
	}

	void tellBoxAboveThatMCMCFinished() {
		for (auto &node : _uniqueNodesBelowBoxes) { node->tellBoxAboveThatMCMCFinished(); }
	}

	void update(size_t Iteration) {
		// go left -> right through vector
		for (auto &node : _nodesToUpdate) { node->update(Iteration); }

		// call extra update functions
		for (auto &func : _funcUpdates) { func(); }
	}

	// functions for simulating
	void simulate(const std::vector<TParameterBase *> &AllParameters) {
		// go right -> left through vector
		for (auto &node : AllParameters) { node->startInitialization(); }
		for (auto node = _uniqueNodesBelowBoxes.rbegin(); node != _uniqueNodesBelowBoxes.rend(); node++) {
			(*node)->simulateUnderPrior();
		}
		for (auto &node : AllParameters) { node->endInitialization(); }
	}

	void clear() {
		_uniqueNodesBelowBoxes.clear();
		_nodesToUpdate.clear();
		_funcUpdates.clear();
	}

	// debug functions
	std::vector<std::string> getNamesOfNodes() {
		std::vector<std::string> names;
		for (auto &node : _uniqueNodesBelowBoxes) { names.push_back(node->name()); }
		return names;
	}
};

}; // end namespace stattools

#endif // TDAG_H
