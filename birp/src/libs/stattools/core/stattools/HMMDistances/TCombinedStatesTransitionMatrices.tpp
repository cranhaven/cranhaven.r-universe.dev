namespace stattools {

//-------------------------------------------
// TCombinedStatesTransitionMatrices
//-------------------------------------------

template<typename PrecisionType, typename NumStatesType, typename LengthType>
TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::TCombinedStatesTransitionMatrices(
	coretools::TDistancesBinnedBase *Distances, const std::vector<std::shared_ptr<TOptimizer>> &Optimizers)
	: TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>() {
	initialize(Distances, Optimizers);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::TCombinedStatesTransitionMatrices(
	coretools::TDistancesBinnedBase *Distances, const std::vector<std::shared_ptr<TOptimizer>> &Optimizers,
	std::string_view Filename, std::vector<std::string> Header)
	: TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>() {
	initialize(Distances, Optimizers, Filename, Header);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::initialize(
	coretools::TDistancesBinnedBase *Distances, const std::vector<std::shared_ptr<TOptimizer>> &Optimizers) {

	_setFixedPriorParameters(Distances, Optimizers);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::initialize(
	coretools::TDistancesBinnedBase *Distances, const std::vector<std::shared_ptr<TOptimizer>> &Optimizers,
	std::string_view Filename, std::vector<std::string> Header) {

	// same as setFixedPriorParameters(), but additionally open file to report EM parameters
	_setFixedPriorParameters(Distances, Optimizers);

	if (Header.empty()) {
		// default header -> ask each tau for its header
		// (problem: same parameter name might appear multiple times if same tau's are used)
		for (const auto &tau : _taus) { tau->fillHeaderEMReportFile(Header); }
	}

	_fileReportEM.open(Filename, Header);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::_setFixedPriorParameters(
	coretools::TDistancesBinnedBase *Distances, const std::vector<std::shared_ptr<TOptimizer>> &Optimizers) {

	// construct transition matrices with distances
	auto numTau = Optimizers.size();
	_taus.resize(numTau);
	_dimensions.resize(numTau);
	_distances = Distances;

	for (size_t i = 0; i < numTau; i++) {
		// set dimensions: numStates per tau
		auto numStates = Optimizers[i]->numStates();
		_dimensions[i] = numStates;
		_taus[i]       = std::make_unique<TTransitionMatrixDistances<PrecisionType, NumStatesType, LengthType>>(
			Distances, std::move(Optimizers[i]));
	}

	// calculate total number of combined states (product of all states)
	_numStates = coretools::containerProduct(_dimensions);
	_index.set(_dimensions);

	this->_init();
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
bool TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::fillProbabilities(
	coretools::TConstView<PrecisionType> Values) {
	// split values in parameters for each tau
	size_t offset = 0;
	bool valid    = true;
	for (auto &tau : _taus) {
		auto numParamsForThisTau = tau->numParameters();
		// slice vector to include only parameters for this tau
		auto subset =
			std::vector<PrecisionType>(Values.begin() + offset, Values.begin() + offset + numParamsForThisTau);
		bool isValid = tau->fillProbabilities(subset);
		if (!isValid) { valid = false; }
		offset += numParamsForThisTau;
	}
	return valid;
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
PrecisionType TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::operator()(
	LengthType Index, NumStatesType From, NumStatesType To) const {
	// From and To are linearized states -> get state per transition matrix
	const auto coordFrom = _index.get(From);
	const auto coordTo   = _index.get(To);

	PrecisionType prob = 1.0;
	for (size_t i = 0; i < _taus.size(); i++) { prob *= (*_taus[i])(Index, coordFrom[i], coordTo[i]); }
	return prob;
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
PrecisionType
TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::stationary(NumStatesType State) const {
	const auto coord = _index.get(State);

	PrecisionType prob = 1.0;
	for (size_t i = 0; i < _taus.size(); i++) { prob *= _taus[i]->stationary(coord[i]); }
	return prob;
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
PrecisionType TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::transitionProbability(
	LengthType Index, NumStatesType From, NumStatesType To) const {
	// same as operator(), just sometimes nicer to read (i.e. for derived classes)
	return operator()(Index, From, To);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
PrecisionType TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::oldTransitionProbability(
	LengthType Index, NumStatesType From, NumStatesType To) const {
	// From and To are linearized states -> get state per transition matrix
	const auto coordFrom = _index.get(From);
	const auto coordTo   = _index.get(To);

	PrecisionType prob = 1.0;
	for (size_t i = 0; i < _taus.size(); i++) {
		prob *= _taus[i]->oldTransitionProbability(Index, coordFrom[i], coordTo[i]);
	}
	return prob;
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
PrecisionType TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::logTransitionProbability(
	LengthType Index, NumStatesType From, NumStatesType To) const {
	const auto v = transitionProbability(Index, From, To);
	return (v == 0.0) ? std::numeric_limits<double>::lowest() : log(v);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
PrecisionType TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::logOldTransitionProbability(
	LengthType Index, NumStatesType From, NumStatesType To) const {
	const auto v = oldTransitionProbability(Index, From, To);
	return (v == 0.0) ? std::numeric_limits<double>::lowest() : log(v);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
PrecisionType TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::dist1_transitionProbability(
	NumStatesType From, NumStatesType To) const {
	// From and To are linearized states -> get state per transition matrix
	const auto coordFrom = _index.get(From);
	const auto coordTo   = _index.get(To);

	PrecisionType prob = 1.0;
	for (size_t i = 0; i < _taus.size(); i++) {
		prob *= _taus[i]->dist1_transitionProbability(coordFrom[i], coordTo[i]);
	}
	return prob;
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::
	handleEMParameterInitializationTransitionProbabilities(LengthType Index, NumStatesType PreviousState,
														   NumStatesType CurrentState) {
	// PreviousState and CurrentState are linearized states -> get state per transition matrix
	const auto coordPrevious = _index.get(PreviousState);
	const auto coordCurrent  = _index.get(CurrentState);

	for (size_t i = 0; i < _taus.size(); i++) {
		_taus[i]->handleEMParameterInitializationTransitionProbabilities(Index, coordPrevious[i], coordCurrent[i]);
	}
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType,
									   LengthType>::finalizeEMParameterInitializationTransitionProbabilities() {
	for (size_t i = 0; i < _taus.size(); i++) { _taus[i]->finalizeEMParameterInitializationTransitionProbabilities(); }
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::fixTransitionMatricesDuringEM(
	const std::vector<bool> &Fix, coretools::TConstView<PrecisionType> Values) {
	assert(Fix.size() == _taus.size());

	size_t offset = 0;
	for (size_t i = 0; i < _taus.size(); i++) {
		auto numParamsForThisTau = _taus[i]->numParameters();
		// slice vector to include only parameters for this tau
		auto subset =
			std::vector<PrecisionType>(Values.begin() + offset, Values.begin() + offset + numParamsForThisTau);
		if (Fix[i]) { _taus[i]->fixTransitionMatricesDuringEM(subset); }
		offset += numParamsForThisTau;
	}
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::setInitialization(
	TypeTransMatInitialization InitType) {
	TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>::setInitialization(InitType);
	for (size_t i = 0; i < _taus.size(); i++) { _taus[i]->setInitialization(InitType); }
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
TypeTransMatInitialization
TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::initializationType() const {
	return TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>::initializationType();
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::initializeEMParameters(
	const TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &LatentVariable,
	const std::vector<LengthType> &ChunkEnds) {

	// write to file before EM starts
	_writeEMReportFile();

	if (this->initializationType() == TypeTransMatInitialization::ML) {
		TTransitionMatrix_base<PrecisionType, NumStatesType, LengthType>::initializeEMParameters(LatentVariable,
																								 ChunkEnds);
	} else if (this->initializationType() == TypeTransMatInitialization::defaultValues) {
		for (size_t i = 0; i < _taus.size(); i++) { _taus[i]->fillProbabilitiesFromStartValues(); }
	}
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::swapNewOldTau() {
	for (auto &tau : _taus) { tau->swapNewOldTau(); }
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::startNewChunk(size_t Chunk) {
	for (auto &tau : _taus) { tau->startNewChunk(Chunk); }
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::noInitializationBeforeEM() {
	for (auto &tau : _taus) { tau->noInitializationBeforeEM(); }
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::setValuesEM(
	coretools::TConstView<PrecisionType> Values) {

	size_t offset = 0;
	for (size_t i = 0; i < _taus.size(); i++) {
		auto numParamsForThisTau = _taus[i]->numParameters();
		// slice vector to include only parameters for this tau
		auto subset =
			std::vector<PrecisionType>(Values.begin() + offset, Values.begin() + offset + numParamsForThisTau);
		_taus[i]->setValuesEM(subset);
		offset += numParamsForThisTau;
	}
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::report() {
	for (auto &tau : _taus) { tau->report(); }
};

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::reportEMParameters() {
	for (auto &tau : _taus) { tau->reportEMParameters(); }
};

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::_runEMEstimationOrStatePosteriors(
	TLatentVariableWithRep<double, size_t, size_t> &latentVariable, bool RunEM) {
	THMM<PrecisionType, NumStatesType, LengthType> hmm(*this, latentVariable, 500, 0.0001, true);
	hmm.report();

	// get chunk ends from distances
	std::vector<LengthType> chunkEnds = _distances->getChunkEnds<LengthType>();

	if (RunEM) {
		hmm.runEM(chunkEnds);
		hmm.estimateStatePosteriors(chunkEnds, false); // initialize z only
	} else {
		hmm.estimateStatePosteriors(chunkEnds, false); // initialize z only
	}
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::runEMEstimation(
	TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &latentVariable) {
	_runEMEstimationOrStatePosteriors(latentVariable, true);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::estimateStatePosteriors(
	TLatentVariableWithRep<PrecisionType, NumStatesType, LengthType> &latentVariable) {
	_runEMEstimationOrStatePosteriors(latentVariable, false);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType,
									   LengthType>::prepareEMParameterEstimationInitial() {
	for (size_t i = 0; i < _taus.size(); i++) { _taus[i]->prepareEMParameterEstimationInitial(); }
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType,
									   LengthType>::prepareEMParameterEstimationOneIterationTransitionProbabilities() {
	for (size_t i = 0; i < _taus.size(); i++) {
		_taus[i]->prepareEMParameterEstimationOneIterationTransitionProbabilities();
	}
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::
	handleEMParameterEstimationOneIterationTransitionProbabilities(
		LengthType Index, const THMMPosteriorXi<PrecisionType, NumStatesType, LengthType> &xi) {
	// create vector of xis, one for each transition matrix
	std::vector<THMMPosteriorXi<PrecisionType, NumStatesType, LengthType>> xis_per_tau(_taus.size());
	for (size_t i = 0; i < _taus.size(); i++) {
		xis_per_tau[i].resize(_dimensions[i]);
		xis_per_tau[i].set(0.0);
	}

	// now sum up xis
	// loop over each entry of the big xi matrix (on the combined states)
	for (size_t i = 0; i < _numStates; i++) {
		for (size_t j = 0; j < _numStates; j++) {
			// for each entry: decide which state transitions are affected per tau
			const auto coordFrom = _index.get(i);
			const auto coordTo   = _index.get(j);

			for (size_t t = 0; t < _taus.size(); t++) { xis_per_tau[t](coordFrom[t], coordTo[t]) += xi(i, j); }
		}
	}

	// hand summed xi's to each tau
	for (size_t i = 0; i < _taus.size(); i++) {
		_taus[i]->handleEMParameterEstimationOneIterationTransitionProbabilities(Index, xis_per_tau[i]);
	}
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType,
									   LengthType>::finalizeEMParameterEstimationOneIterationTransitionProbabilities() {
	for (size_t i = 0; i < _taus.size(); i++) {
		_taus[i]->finalizeEMParameterEstimationOneIterationTransitionProbabilities();
	}
	_writeEMReportFile();
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::finalizeEMParameterEstimationFinal() {
	for (size_t i = 0; i < _taus.size(); i++) { _taus[i]->finalizeEMParameterEstimationFinal(); }
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
std::vector<PrecisionType>
TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::getFinalParameterEstimatesEM() const {
	std::vector<PrecisionType> final;
	for (size_t i = 0; i < _taus.size(); i++) {
		// append to vector
		auto tmp = _taus[i]->getFinalParameterEstimatesEM();
		final.insert(final.end(), tmp.begin(), tmp.end());
	}
	return final;
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::_writeEMReportFile() {
	if (_fileReportEM.isOpen()) { // only write if developer has opened file
		for (size_t i = 0; i < _taus.size(); i++) { _taus[i]->writeToEMReportFile(_fileReportEM); }
		_fileReportEM.endln();
	}
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
std::vector<PrecisionType>
TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::getParameters() const {
	return getFinalParameterEstimatesEM();
};

template<typename PrecisionType, typename NumStatesType, typename LengthType>
bool TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::setParameters(
	coretools::TConstView<PrecisionType> Params) {
	return fillProbabilities(Params);
};

template<typename PrecisionType, typename NumStatesType, typename LengthType>
coretools::TDistancesBinnedBase *
TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::distances() const {
	return _distances;
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
size_t TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::numParameters() const {
	size_t c = 0;
	for (auto &tau : _taus) { c += tau->numParameters(); }
	return c;
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::simulateDistances(const int length) {
	// simulate random distances - only if user has not filled them!
	if (_distances->size() == 0) { _distances->simulate(length); }

	for (size_t i = 0; i < _taus.size(); i++) { _taus[i]->simulateDistances(length); }
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
NumStatesType TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::sampleNextState(
	LengthType Index, NumStatesType State) const {
	const auto coordCurrent = _index.get(State);

	// simulate next state for each tau
	std::vector<size_t> coordNext(_dimensions.size());
	for (size_t i = 0; i < _taus.size(); i++) { coordNext[i] = _taus[i]->sampleNextState(Index, coordCurrent[i]); }

	// linearize
	return coretools::getLinearIndex(coordNext, _dimensions);
}

template<typename PrecisionType, typename NumStatesType, typename LengthType>
void TCombinedStatesTransitionMatrices<PrecisionType, NumStatesType, LengthType>::print() {
	for (size_t i = 0; i < _taus.size(); i++) {
		coretools::cout << "tau_" << i << std::endl;
		_taus[i]->print();
	}
}

}; // end namespace stattools
