//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORMULTIVARIATENORMAL_H
#define TPRIORMULTIVARIATENORMAL_H

#include "coretools/arma_include.h"

#include "stattools/ParametersObservations/TParameter.h"
#include "stattools/Priors/TPriorBase.h"
#include "coretools/Distributions/TMultivariateNormalDistr.h"

namespace stattools::prior {

//-------------------------------------------
// TMultivariateNormal
//-------------------------------------------
namespace impl::mvn {

struct DimMVN {
	size_t D;
	double minusDdiv2Log2Pi;
	double sqrt2piD;

	DimMVN() = default;
	explicit DimMVN(size_t Dim);
};

[[nodiscard]] size_t numberOfElementsInTriangularMatrix_Diagonal0(size_t D);
[[nodiscard]] size_t indexMrs(size_t r, size_t s);

template<typename Storage> DimMVN calculateAndSetD(const std::vector<Storage *> &StorageBelow) {
	// derive D from size of parameter (start with first and then check if all others match dimensions of first)
	size_t D = StorageBelow[0]->dimensions()[1];
	for (const auto &storage : StorageBelow) {
		if (storage->dimensions()[1] != D) {
			DEVERROR("Number of dimensions of multivariate normal prior (=", D,
					 ") and the dimensions of storage below (=", storage->dimensions()[1], ") do not match!");
		}
	}
	return DimMVN(D);
}

std::shared_ptr<coretools::TNamesStrings> generateDimNamesMrs(const std::shared_ptr<coretools::TNamesEmpty> &DimName);

template<typename Storage, typename TypeParamMu, typename TypeParamMrr, typename TypeParamMrs>
double calcDoubleSum(const Storage &Data, size_t n, const TypeParamMu *Mus, const TypeParamMrr *Mrr,
					 const TypeParamMrs *Mrs) {
	const auto row = Data.get1DSlice(1, {n, 0});
	const size_t D = Data.dimensions()[1];
	using Type     = typename Storage::value_type;

	double outerSum = 0;
	for (size_t s = 0; s < D; s++) {
		double innerSum = 0.;
		for (size_t r = s; r < D; r++) {
			// take value for all data elements
			const double value = (Type)Data[row.begin + r * row.increment];
			if (s == r) {
				innerSum += Mrr->value(r) * (value - Mus->value(r));
			} else {
				innerSum += Mrs->value(indexMrs(r, s)) * (value - Mus->value(r));
			}
		}
		outerSum += innerSum * innerSum;
	}

	return outerSum;
}

template<typename TypeParamMrs> double calcSumM_dr_squared(double Mrr, size_t r, const TypeParamMrs *Mrs) {
	double sumM = Mrr * Mrr;
	for (size_t d = 0; d < r; d++) {
		double tmpVal = Mrs->value(indexMrs(r, d));
		sumM += tmpVal * tmpVal;
	}
	return sumM;
}

template<typename T, typename TypeParamMu, typename TypeParamMrr, typename TypeParamMrs>
double calcDoubleSum_updateParam(const T &Data, size_t n, size_t IndexMuThatChanged, size_t Min_r_DMin1,
								 const TypeParamMu *Mus, const TypeParamMrr *Mrr, const TypeParamMrs *Mrs) {
	const auto row = Data.get1DSlice(1, {n, 0});
	const size_t D = Data.dimensions()[1];
	using Type     = typename T::value_type;

	double sum = 0.;
	// go over all s = 1:min(r, D-1)
	for (size_t s = 0; s < Min_r_DMin1; s++) {
		// go over all r = s:D, r != d
		double tmp = 0.;
		for (size_t r = s; r < D; r++) {
			if (r == IndexMuThatChanged)
				continue;
			else if (r == s) {
				tmp += Mrr->value(r) * (value((Type)Data[row.begin + r * row.increment]) - Mus->value(r));
			} else {
				tmp += Mrs->value(indexMrs(r, s)) * (value((Type)Data[row.begin + r * row.increment]) - Mus->value(r));
			}
		}
		if (IndexMuThatChanged == s) {
			sum += tmp * Mrr->value(s);
		} else {
			sum += tmp * Mrs->value(indexMrs(IndexMuThatChanged, s));
		}
	}

	return sum;
}

template<typename Storage, typename TypeParamMu, typename TypeParamMrr, typename TypeParamMrs>
double calcDoubleSum_updateMu(const Storage &Data, const coretools::TRange &Row, size_t IndexMuThatChanged,
							  size_t Min_r_DMin1, const TypeParamMu *Mus, const TypeParamMrr *Mrr,
							  const TypeParamMrs *Mrs) {
	const size_t D = Data.dimensions()[1];
	using Type     = typename Storage::value_type;

	double sum = 0.;
	// go over all s = 1:min(r, D-1)
	for (size_t s = 0; s < Min_r_DMin1; s++) {
		// go over all r = s:D, r != d
		double tmp = 0.;
		for (size_t r = s; r < D; r++) {
			if (r == IndexMuThatChanged)
				continue;
			else if (r == s) {
				tmp += Mrr->value(r) * ((Type)Data[Row.begin + r * Row.increment] - Mus->value(r));
			} else {
				tmp += Mrs->value(indexMrs(r, s)) * ((Type)Data[Row.begin + r * Row.increment] - Mus->value(r));
			}
		}
		if (IndexMuThatChanged == s) {
			sum += tmp * Mrr->value(s);
		} else {
			sum += tmp * Mrs->value(indexMrs(IndexMuThatChanged, s));
		}
	}

	return sum;
}

template<typename TypeParamM>
double calcLLRatioUpdateMu(double OldMu, double NewMu, double SumM, size_t N, double SumParam, double Sum,
						   const TypeParamM *m) {
	return 1. / (2. * m->value() * m->value()) *
		   ((double)N * (OldMu * OldMu - NewMu * NewMu) * SumM + 2. * (NewMu - OldMu) * (SumM * SumParam + Sum));
}

double calcLLRatioUpdateM(size_t N, size_t D, double OldM, double NewM, double Sum);

template<typename TypeParamM>
double calcLLRatioUpdateMrr(size_t N, double OldMrr, double NewMrr, double Sum, double SumParam_nd_minus_mu_d,
							const TypeParamM *m) {
	return (double)N * log(NewMrr / OldMrr) +
		   1. / (2. * m->value() * m->value()) *
			   ((OldMrr * OldMrr - NewMrr * NewMrr) * SumParam_nd_minus_mu_d + 2. * (OldMrr - NewMrr) * Sum);
}

template<typename Storage, typename TypeParamMu, typename TypeParamMrs>
double calcDoubleSum_updateMrr(const Storage &Data, size_t n, size_t IndexMrrThatChanged,
							   double &SumParam_nd_minus_mu_d, const TypeParamMu *Mus, const TypeParamMrs *Mrs) {
	const auto row = Data.get1DSlice(1, {n, 0});
	const size_t D = Data.dimensions()[1];
	using Type     = typename Storage::value_type;

	double sum = 0.;
	for (size_t r = (IndexMrrThatChanged + 1); r < D; r++) {
		sum +=
			Mrs->value(indexMrs(r, IndexMrrThatChanged)) * ((Type)Data[row.begin + r * row.increment] - Mus->value(r));
	}

	double paramND_minus_muD =
		(Type)Data[row.begin + IndexMrrThatChanged * row.increment] - Mus->value(IndexMrrThatChanged);

	SumParam_nd_minus_mu_d += paramND_minus_muD * paramND_minus_muD;
	return paramND_minus_muD * sum;
}

template<typename Storage, typename TypeParamMu, typename TypeParamMrr, typename TypeParamMrs>
void addToSumsUpdateMu(double &Sum, double &SumParam, Storage *Data, size_t n, size_t r, size_t min_r_DMin1,
					   const TypeParamMu *Mus, const TypeParamMrr *Mrr, const TypeParamMrs *Mrs) {
	coretools::TRange row = Data->get1DSlice(1, {n, 0});
	using Type            = typename Storage::value_type;

	Sum += calcDoubleSum_updateMu(*Data, row, r, min_r_DMin1, Mus, Mrr, Mrs);
	SumParam += (Type)(*Data)[row.begin + r * row.increment];
}

template<typename TypeParamM>
double calcLLRatioUpdateMrs(double OldMrs, double NewMrs, double Sum, double SumParam_nd_minus_mu_d,
							const TypeParamM *m) {
	return 1. / (2. * m->value() * m->value()) *
		   ((OldMrs * OldMrs - NewMrs * NewMrs) * SumParam_nd_minus_mu_d + 2. * (OldMrs - NewMrs) * Sum);
}

template<typename Storage, typename TypeParamMu, typename TypeParamMrr, typename TypeParamMrs>
double calcDoubleSum_updateMrs(const Storage &Data, size_t n, size_t d, size_t e, double &SumParam_nd_minus_mu_d,
							   const TypeParamMu *Mus, const TypeParamMrr *Mrr, const TypeParamMrs *Mrs) {
	const auto row = Data.get1DSlice(1, {n, 0});
	const size_t D = Data.dimensions()[1];
	using Type     = typename Storage::value_type;

	double sum = 0.;
	for (size_t r = e; r < D; r++) {
		if (r == d) {
			continue;
		} else if (r == e) {
			sum += Mrr->value(e) * ((Type)Data[row.begin + e * row.increment] - Mus->value(e)); // take mrr
		} else {
			sum += Mrs->value(indexMrs(r, e)) * ((Type)Data[row.begin + r * row.increment] - Mus->value(r));
		}
	}

	double paramND_minus_muD = (Type)Data[row.begin + d * row.increment] - Mus->value(d);

	SumParam_nd_minus_mu_d += paramND_minus_muD * paramND_minus_muD;
	return paramND_minus_muD * sum;
}

template<typename Storage, typename TypeParamMu, typename TypeParamM, typename TypeParamMrr, typename TypeParamMrs>
auto calcPriorDensity(const Storage &Data, size_t n, const TypeParamMu *Mus, const TypeParamM *m,
					  const TypeParamMrr *Mrr, const TypeParamMrs *Mrs, const DimMVN &Dim) {
	double prodMrr   = Mrr->storage().prod();
	double doubleSum = calcDoubleSum(Data, n, Mus, Mrr, Mrs);
	return (1. / pow(m->value(), (double)Dim.D) * prodMrr) / Dim.sqrt2piD *
		   exp(-1. / (2. * m->value() * m->value()) * doubleSum);
}

template<typename Storage, typename TypeParamMu, typename TypeParamM, typename TypeParamMrr, typename TypeParamMrs>
auto calcLogPriorDensity(const Storage &Data, size_t n, const TypeParamMu *Mus, const TypeParamM *m,
						 const TypeParamMrr *Mrr, const TypeParamMrs *Mrs, const DimMVN &Dim) {
	double sumlogMrr = Mrr->storage().sumOfLogs();
	double doubleSum = calcDoubleSum(Data, n, Mus, Mrr, Mrs);
	return -(double)Dim.D * log(m->value()) + sumlogMrr + Dim.minusDdiv2Log2Pi -
		   1. / (2. * m->value() * m->value()) * doubleSum;
}

template<typename UpdatedStorage, typename TypeParamMu, typename TypeParamM, typename TypeParamMrr,
		 typename TypeParamMrs>
auto calcLogPriorRatio(const UpdatedStorage &Data, size_t n, size_t d, const TypeParamMu *Mus, const TypeParamM *m,
					   const TypeParamMrr *Mrr, const TypeParamMrs *Mrs) {
	// store values
	const auto row = Data.get1DSlice(1, {n, 0});
	const size_t D = Data.dimensions()[1];
	using Type     = typename UpdatedStorage::value_type;

	double oldParam = Data[row.begin + d * row.increment].oldValue();
	double newParam = value((Type)Data[row.begin + d * row.increment]);

	// calculate simplified hastings ratio
	double sumM        = calcSumM_dr_squared(Mrr->value(d), d, Mrs);
	size_t min_r_DMin1 = std::min(d + 1, D - 1);
	double sum         = calcDoubleSum_updateParam(Data, n, d, min_r_DMin1, Mus, Mrr, Mrs);
	double tmp = sumM * (oldParam * oldParam - newParam * newParam + 2. * Mus->value(d) * (newParam - oldParam)) +
				 2. * (oldParam - newParam) * sum;

	return 1. / (2. * m->value() * m->value()) * tmp;
}

template<typename Storage> void addToSumMLEMu(std::vector<double> &Sums, size_t n, const Storage &Data) {
	const auto row = Data.get1DSlice(1, {n, 0});
	using Type     = typename Storage::value_type;

	size_t d = 0;
	for (size_t col = row.begin; col < row.end; col += row.increment, d++) { // go over all cols of one row (D)
		Sums[d] += (Type)Data[col];
	}
}

void normalizeSumMLEMu(std::vector<double> &Sums, double TotalNumElements);

template<typename Storage> std::vector<double> calculateMLEMu(const std::vector<Storage *> &StorageBelow) {
	const auto D = StorageBelow[0]->dimensions()[1];

	// calculate MLE of mus
	std::vector<double> sums(D, 0.);
	double totalNumElements = 0.;
	for (auto &storage : StorageBelow) {
		for (size_t n = 0; n < storage->dimensions()[0]; n++) { // go over all rows (N)
			addToSumMLEMu(sums, n, *storage);
		}
		totalNumElements += storage->dimensions()[0];
	}

	// divide
	normalizeSumMLEMu(sums, totalNumElements);

	return sums;
}

template<typename Storage, typename TypeParamMu>
void addToSumMLESigma(arma::mat &Sums, size_t n, const Storage &Data, const TypeParamMu *Mus) {
	using Type     = typename Storage::value_type;
	const auto row = Data.get1DSlice(1, {n, 0});

	size_t j = 0;
	for (size_t col1 = row.begin; col1 < row.end; col1 += row.increment, j++) { // go over all cols of one row (D)
		size_t k = 0;
		for (size_t col2 = row.begin; col2 < row.end; col2 += row.increment, k++) { // go over all cols of one row (D)
			Sums(j, k) += ((Type)Data[col1] - Mus->value(j)) * ((Type)Data[col2] - Mus->value(k));
		}
	}
}

void normalizeSumMLESigma(arma::mat &Sums, double TotalNumElements);

template<typename Storage, typename TypeParamMu>
arma::mat calculateMLESigma(const std::vector<Storage *> &StorageBelow, const TypeParamMu *Mus) {
	const auto D = StorageBelow[0]->dimensions()[1];

	// initialize Parameter for sums (matrix)
	arma::mat sums(D, D, arma::fill::zeros);

	double totalNumElements = 0.;
	for (auto &storage : StorageBelow) {
		for (size_t n = 0; n < storage->dimensions()[0]; n++) { // go over all rows (N)
			addToSumMLESigma(sums, n, *storage, Mus);
		}
		totalNumElements += storage->dimensions()[0];
	}

	// normalize to get MLE of Sigma
	normalizeSumMLESigma(sums, totalNumElements);

	return sums;
}

arma::mat calculateMFromSigma(const arma::mat &Sigma);

template<typename TypeParamMrr, typename TypeParamMrs, typename TypeParamM>
void fillParametersMFromSigma(const arma::mat &Sigma, size_t D, TypeParamMrr *Mrr, TypeParamMrs *Mrs, TypeParamM *m) {
	// calculate M from Sigma
	arma::mat M = calculateMFromSigma(Sigma);

	// set elements of M (m=1)
	if (D == 1) {
		m->set(1. / M(0, 0));
	} else {
		m->set(1.);
		for (size_t s = 0; s < D; s++) {
			for (size_t r = s; r < D; r++) {
				if (s == r) {
					Mrr->set(r, M(r, s));
				} else {
					Mrs->set(indexMrs(r, s), M(r, s));
				}
			}
		}
	}
}

template<typename TypeParamMrr, typename TypeParamMrs, typename TypeParamM>
void fillArmadilloMFromParametersM(arma::mat &M, size_t D, const TypeParamMrr *Mrr, const TypeParamMrs *Mrs,
								   const TypeParamM *m) {
	// fills armadillo matrix from current values of MCMCParameters
	for (size_t s = 0; s < D; s++) {
		for (size_t r = s; r < D; r++) {
			if (r == s) {
				M(r, s) = Mrr->value(r);
			} else {
				M(r, s) = Mrs->value(indexMrs(r, s));
			}
		}
	}
	// divide all entries by m
	M /= m->value();
}

template<typename TypeParamMrr, typename TypeParamMrs, typename TypeParamM>
arma::mat getArmadilloSigmaForSimulation(size_t D, const TypeParamMrr *Mrr, const TypeParamMrs *Mrs,
										 const TypeParamM *m) {
	// first get M and Sigma as armadillo matrices
	arma::mat M(D, D, arma::fill::zeros);
	fillArmadilloMFromParametersM(M, D, Mrr, Mrs, m);
	return arma::inv_sympd(M * M.t());
}

template<typename TypeParamMu, typename TypeParamMrr, typename TypeParamMrs, typename TypeParamM>
auto createMVNForSimulation(size_t D, const TypeParamMu *Mu, const TypeParamMrr *Mrr, const TypeParamMrs *Mrs,
							const TypeParamM *m) {
	std::vector<double> mus(D);
	for (size_t d = 0; d < D; d++) { mus[d] = Mu->value(d); }
	arma::mat Sigma = impl::mvn::getArmadilloSigmaForSimulation(D, Mrr, Mrs, m);
	coretools::probdist::TMultivariateNormalDistr<std::vector<double>> MVN(mus, Sigma);
	return MVN;
}

template<typename Storage>
void simulateMultivariateNormal(Storage &Data, size_t Index,
								const coretools::probdist::TMultivariateNormalDistr<std::vector<double>> &MVN) {
	using Type = typename Storage::value_type;

	std::vector<double> x = MVN.sample();
	for (size_t d = 0; d < x.size(); d++, Index++) { Data[Index] = (Type)x[d]; }
}

std::pair<size_t, size_t> getCoordinatesFromLinearIndex_Mrs(size_t Index);
} // namespace impl::mvn

//-------------------------------------------
// TMultivariateNormalInferred
//-------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecMu, typename SpecM, typename SpecMrr,
		 typename SpecMrs>
class TMultivariateNormalInferred : public TStochasticBase<Derived, Type, NumDim> {
	static_assert(TypesAreUnboundedFloatingPoints<Type, typename SpecMu::value_type, typename SpecMrs::value_type>());
	static_assert(TypesAreStrictlyPositiveFloatingPoints<typename SpecM::value_type, typename SpecMrr::value_type>());
	static_assert(NumDim == 2 && SpecMu::numDim == 1 && SpecM::numDim == 1 && SpecMrr::numDim == 1 &&
				  SpecMrs::numDim == 1);
	static_assert(!SpecMu::parallelize && !SpecM::parallelize && !SpecMrr::parallelize && !SpecMrs::parallelize);

private:
	using BoxType = TMultivariateNormalInferred<Derived, Type, NumDim, SpecMu, SpecM, SpecMrr, SpecMrs>;
	using Base    = TStochasticBase<Derived, Type, NumDim>;

	using typename Base::Storage;
	using typename Base::UpdatedStorage;

	using TypeParamMu  = TParameter<SpecMu, BoxType>;
	using TypeParamM   = TParameter<SpecM, BoxType>;
	using TypeParamMrr = TParameter<SpecMrr, BoxType>;
	using TypeParamMrs = TParameter<SpecMrs, BoxType>;

	TypeParamMu *_mus  = nullptr;
	TypeParamM *_m     = nullptr;
	TypeParamMrr *_Mrr = nullptr;
	TypeParamMrs *_Mrs = nullptr;

	// dimensions and temporary variables
	impl::mvn::DimMVN _dim{};

protected:
	void _setMusToMLE() {
		std::vector<double> means = impl::mvn::calculateMLEMu(this->_storageBelow);

		// set each mus to its MLE
		for (size_t d = 0; d < _dim.D; d++) { _mus->set(d, means[d]); }
	};

	void _setMToMLE() {
		arma::mat Sigma = impl::mvn::calculateMLESigma(this->_storageBelow, _mus);
		impl::mvn::fillParametersMFromSigma(Sigma, _dim.D, _Mrr, _Mrs, _m);
	};

	void _simulateUnderPrior(Storage *Data) override {
		// create re-usable MVN object
		auto MVN = impl::mvn::createMVNForSimulation(_dim.D, _mus, _Mrr, _Mrs, _m);

		// get multivariate normal random numbers
		assert(Data->dimensions()[1] == _dim.D); // number of columns of data must match D

		for (size_t n = 0; n < Data->dimensions()[0]; n++) { // go over all rows
			coretools::TRange row = Data->get1DSlice(1, {n, 0});
			impl::mvn::simulateMultivariateNormal(*Data, row.begin, MVN);
		}
	};

public:
	TMultivariateNormalInferred(TypeParamMu *Mu, TypeParamM *M, TypeParamMrr *Mrr, TypeParamMrs *Mrs)
		: _mus(Mu), _m(M), _Mrr(Mrr), _Mrs(Mrs) {
		this->addPriorParameter({_mus, _m, _Mrr, _Mrs});
	};
	~TMultivariateNormalInferred() override = default;

	[[nodiscard]] std::string name() const override { return "multivariateNormal"; };

	void initialize() override {
		// derive D from dimensionality of parameters below
		_dim = impl::mvn::calculateAndSetD(this->_storageBelow);

		auto dimName = this->_storageBelow[0]->getDimensionName(1);

		_mus->initStorage(this, {_dim.D}, {dimName});
		_m->initStorage(this);
		_Mrr->initStorage(this, {_dim.D}, {dimName});
		if (_dim.D > 1) { // only check if there are >1 dimension (mrs are not defined for D=1)
			auto dimNamesMrs = impl::mvn::generateDimNamesMrs(dimName);
			_Mrs->initStorage(this, {impl::mvn::numberOfElementsInTriangularMatrix_Diagonal0(_dim.D)}, {dimNamesMrs});
		} else {
			auto dimNamesMrs = std::make_shared<coretools::TNamesStrings>(0);
			_Mrs->initStorage(this, {0}, {dimNamesMrs});
		}

		// check: if D=1 (univariate normal distribution), we will only update m and mu, and fix mrr to 1.
		if (_dim.D == 1) {
			_Mrr->set(0, 1.);
			_Mrr->setIsUpdated(false);
			_Mrs->setIsUpdated(false);
		}
	};

	double getDensity(const Storage &Data, size_t Index) const override {
		auto coord = Data.getSubscripts(Index);
		return impl::mvn::calcPriorDensity(Data, coord[0], _mus, _m, _Mrr, _Mrs, _dim);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		auto coord = Data.getSubscripts(Index);
		return impl::mvn::calcLogPriorDensity(Data, coord[0], _mus, _m, _Mrr, _Mrs, _dim);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		auto coord = Data.getSubscripts(Index);
		return impl::mvn::calcLogPriorRatio(Data, coord[0], coord[1], _mus, _m, _Mrr, _Mrs);
	};

	double getSumLogPriorDensity(const Storage &Data) const override {
		double sum = 0.;
		for (size_t n = 0; n < Data.dimensions()[0]; n++) {
			coretools::TRange row = Data.get1DSlice(1, {n, 0});
			sum += getLogDensity(Data, row.begin);
		}
		return sum;
	};

	void guessInitialValues() override {
		_setMusToMLE();
		_setMToMLE();
	};

	[[nodiscard]] auto calculateLLRatio(TypeParamMu *, size_t i) const {
		const auto sumM    = impl::mvn::calcSumM_dr_squared(_Mrr->value(i), i, _Mrs);
		size_t min_r_DMin1 = std::min(i + 1, _dim.D - 1);
		auto f             = [this, i, sumM, min_r_DMin1](Storage *Data) {
			const auto N    = Data->dimensions()[0];
			double sum      = 0.;
			double sumParam = 0.;
			for (size_t n = 0; n < N; n++) {
				impl::mvn::addToSumsUpdateMu(sum, sumParam, Data, n, i, min_r_DMin1, _mus, _Mrr, _Mrs);
			}
			return impl::mvn::calcLLRatioUpdateMu(_mus->oldValue(i), _mus->value(i), sumM, N, sumParam, sum, _m);
		};
		return f;
	}

	void updateTempVals(TypeParamMu *, size_t, bool) { /* empty - no tmp values to be updated */
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamM *, size_t) const {
		auto f = [this](Storage *Data) {
			size_t N   = Data->dimensions()[0];
			double sum = 0.;
			for (size_t n = 0; n < Data->dimensions()[0]; n++) {
				sum += impl::mvn::calcDoubleSum(*Data, n, _mus, _Mrr, _Mrs);
			}
			return impl::mvn::calcLLRatioUpdateM(N, _dim.D, _m->oldValue(), _m->value(), sum);
		};
		return f;
	}

	void updateTempVals(TypeParamM *, size_t, bool) { /* empty - no tmp values to be updated */
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamMrr *, size_t i) const {
		auto f = [this, i](Storage *Data) {
			size_t N    = Data->dimensions()[0];
			double sum1 = 0.;
			double sum2 = 0.;
			for (size_t n = 0; n < N; n++) {
				sum1 += impl::mvn::calcDoubleSum_updateMrr(*Data, n, i, sum2, _mus, _Mrs);
			}
			return impl::mvn::calcLLRatioUpdateMrr(N, _Mrr->oldValue(i), _Mrr->value(i), sum1, sum2, _m);
		};
		return f;
	}

	void updateTempVals(TypeParamMrr *, size_t, bool) { /* empty - no tmp values to be updated */
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamMrs *, size_t i) const {
		const auto p = impl::mvn::getCoordinatesFromLinearIndex_Mrs(i);
		const auto r = p.first;
		const auto s = p.second;
		auto l       = impl::mvn::indexMrs(r, s);

		auto f = [this, r, s, l](Storage *Data) {
			size_t N    = Data->dimensions()[0];
			double sum1 = 0.;
			double sum2 = 0.;
			for (size_t n = 0; n < N; n++) {
				sum1 += impl::mvn::calcDoubleSum_updateMrs(*Data, n, r, s, sum2, _mus, _Mrr, _Mrs);
			}
			return impl::mvn::calcLLRatioUpdateMrs(_Mrs->oldValue(l), _Mrs->value(l), sum1, sum2, _m);
		};
		return f;
	}

	void updateTempVals(TypeParamMrs *, size_t, bool) { /* empty - no tmp values to be updated */
	}
};

} // end namespace stattools::prior

#endif // TPRIORMULTIVARIATENORMAL_H
