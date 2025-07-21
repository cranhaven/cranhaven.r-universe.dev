//
// Created by madleina on 16.03.22.
//

#ifndef TPRIORTWOMULTIVARIATENORMALSMIXEDMODEL_H
#define TPRIORTWOMULTIVARIATENORMALSMIXEDMODEL_H

#include "TPriorMultivariateNormal.h"
#include "stattools/Priors/TPriorMultivariateNormalMixedModel.h"
#include "stattools/Priors/TPriorBase.h"
#include "stattools/ParametersObservations/TParameter.h"
#include "coretools/Main/TLog.h"

namespace stattools::prior {

//-------------------------------------------------
// TVarCovarMatrix
//-------------------------------------------------

class TVarCovarMatrix {
protected:
	// vector of noise which will be added to diagonal of Sigma in case taking the inverse fails
	// corresponds to exp(-18:0) in R
	std::vector<double> _noiseInverseSigma = {1.52299797447126e-08,
											  4.13993771878517e-08,
											  1.12535174719259e-07,
											  3.05902320501826e-07,
											  8.31528719103568e-07,
											  2.26032940698105e-06,
											  6.14421235332821e-06,
											  1.67017007902457e-05,
											  4.53999297624849e-05,
											  0.00012340980408668,
											  0.000335462627902512,
											  0.000911881965554516,
											  0.00247875217666636,
											  0.00673794699908547,
											  0.0183156388887342,
											  0.0497870683678639,
											  0.135335283236613,
											  0.367879441171442,
											  1};

public:
	arma::mat M;
	arma::mat oldM;
	arma::mat Sigma;
	arma::mat oldSigma;

	void init(size_t D) {
		M.eye(D, D); // set diagonal to 1 and rest to 0
		oldM = M;
		calculateSigma();
		oldSigma = Sigma;
	}

	void calculateSigma() {
		oldSigma = Sigma;
		Sigma    = arma::inv_sympd(M * M.t());
	}

	void calculateM() {
		oldM = M;

		arma::mat invSigma;
		if (!arma::inv_sympd(invSigma, Sigma)) {
			// add noise to diagonal
			arma::mat SigmaNoisy = Sigma;
			SigmaNoisy.diag() += _noiseInverseSigma[0];
			size_t counter = 1;
			while (!arma::inv_sympd(invSigma, SigmaNoisy)) {
				if (counter == _noiseInverseSigma.size()) {
					// if still cannot take inverse: just use diagonal matrix (I want to avoid throwing errors in any
					// case!)
					invSigma.eye(Sigma.n_rows, Sigma.n_cols);
					break;
				}
				// add more and more noise
				SigmaNoisy.diag() += _noiseInverseSigma[counter];
				counter++;

				coretools::cout << "Sigma: " << std::endl;
				coretools::cout << Sigma << std::endl;
				coretools::cout << "SigmaNoisy: " << std::endl;
				coretools::cout << SigmaNoisy << std::endl;
			}
		}
		if (!arma::chol(M, invSigma, "lower")) {
			// failed to do Cholesky decomposition
			// I observed that this sometimes happens if Sigma is singular, but armadillo still finds some absurd
			// solution with inv_sympd (results in huge values of invSigma) -> but afterward, Cholesky fails Using
			// arma::solve for the inverse in such cases yields better results (not sure if that is always the case, but
			// in my example it worked) -> try doing this
			invSigma = arma::solve(Sigma, arma::eye(Sigma.n_rows, Sigma.n_cols));
			if (!arma::chol(M, invSigma, "lower")) {
				// if still cannot do Cholesky: just use diagonal matrix (I want to avoid throwing errors in any case!)
				M.eye(Sigma.n_rows, Sigma.n_cols);
			}

			coretools::cout << "Sigma: " << std::endl;
			coretools::cout << Sigma << std::endl;
			coretools::cout << "invSigma: " << std::endl;
			coretools::cout << invSigma << std::endl;
		}
	}

	void reset() {
		M     = oldM;
		Sigma = oldSigma;
	}
};

//-------------------------------------------------
// TTwoMultivariateNormalsMixedModelInferred
//------------------------------------------------

template<typename Derived, typename Type, size_t NumDim, typename SpecZ, typename SpecMus, typename SpecM,
		 typename SpecMrr, typename SpecMrs, typename SpecRho>
class TTwoMultivariateNormalsMixedModelInferred : public TStochasticBase<Derived, Type, NumDim>,
												  public TLatentVariable<double, size_t, size_t> {
	static_assert(TypesAreBool<typename SpecZ::value_type>());
	static_assert(TypesAreUnboundedFloatingPoints<Type, typename SpecMus::value_type, typename SpecMrs::value_type>());
	static_assert(TypesAreStrictlyPositiveFloatingPoints<typename SpecM::value_type, typename SpecMrr::value_type,
														 typename SpecRho::value_type>());
	static_assert(NumDim == 2);
	static_assert(!SpecMus::parallelize && !SpecM::parallelize && !SpecMrr::parallelize && !SpecMrs::parallelize &&
				  !SpecRho::parallelize);

private:
	using BoxType = TTwoMultivariateNormalsMixedModelInferred<Derived, Type, NumDim, SpecZ, SpecMus, SpecM, SpecMrr,
															  SpecMrs, SpecRho>;
	using Base    = TStochasticBase<Derived, Type, NumDim>;

	using typename Base::Storage;
	using typename Base::UpdatedStorage;
	using TypeParamZ   = TParameter<SpecZ, BoxType>;
	using TypeParamMus = TParameter<SpecMus, BoxType>;
	using TypeParamM   = TParameter<SpecM, BoxType>;
	using TypeParamMrr = TParameter<SpecMrr, BoxType>;
	using TypeParamMrs = TParameter<SpecMrs, BoxType>;
	using TypeParamRho = TParameter<SpecRho, BoxType>;

	TypeParamZ *_z      = nullptr;
	TypeParamMus *_mu   = nullptr;
	TypeParamM *_m      = nullptr;
	TypeParamMrr *_Mrr  = nullptr;
	TypeParamMrs *_Mrs  = nullptr;
	TypeParamRho *_rhos = nullptr;

	// dimensions and temporary variables
	impl::mvn::DimMVN _dim{};

	// EM
	std::vector<arma::mat> _EM_update_Sigma_temp;
	std::vector<double> _EM_update_Weights_temp;

protected:
	TVarCovarMatrix _zeroModel;
	TVarCovarMatrix _oneModel;

	[[nodiscard]] arma::vec _stretchEigenVals(const arma::vec &Lambda_0) const {
		arma::vec Lambda_1(_dim.D);
		for (size_t d = 0; d < _dim.D; d++) {
			Lambda_1(d) = Lambda_0(d) * (1. + _rhos->value(d)); // element-wise multiplication
		}
		return Lambda_1;
	};

	void _stretchSigma1() {
		arma::vec Lambda_0;
		arma::mat Q;
		// eigen decomposition
		// -> Lambda_0 will contain eigenvalues (in ascending order, attention: different than in R!)
		// -> Q contains the eigenvectors, sorted corresponding to eigenvalues
		arma::eig_sym(Lambda_0, Q, _zeroModel.Sigma); // directly fills Lambda_0 and Q

		// stretch eigenvalues
		arma::vec Lambda_1 = _stretchEigenVals(Lambda_0);

		// calculate Sigma1
		_oneModel.oldSigma = _oneModel.Sigma;
		_oneModel.Sigma =
			arma::symmatu(Q * arma::diagmat(Lambda_1) *
						  Q.i()); // not always symmetric because of rounding errors in Q -> symmatu makes it symmetric
	};

	void _fill_M0() {
		_zeroModel.oldM = _zeroModel.M;

		// check if m and mrr are too small (if m is very small -> update it -> issues with taking the inverse of
		// M0*M0.t())
		if (_m->value() < 0.0001) { _m->set(1.); }
		for (size_t d = 0; d < _dim.D; d++) {
			if (_Mrr->value(d) < 0.0001) { _Mrr->set(d, 1.); }
		}

		impl::mvn::fillArmadilloMFromParametersM(_zeroModel.M, _dim.D, _Mrr, _Mrs, _m);

		// calculate Sigma0
		_zeroModel.calculateSigma();
	};

	void _fill_M1() {
		// calculate Sigma1
		_stretchSigma1();

		// calculate M1
		_oneModel.calculateM();
	};

	void _fill_M0_afterUpdateMrr(double NewVal, size_t r) {
		_zeroModel.oldM    = _zeroModel.M;
		// update armadillo matrix, always divide by m
		_zeroModel.M(r, r) = NewVal / _m->value();

		// calculate Sigma0
		_zeroModel.calculateSigma();
	};

	void _fill_M0_afterUpdateMrs(double NewVal, size_t r, size_t s) {
		_zeroModel.oldM    = _zeroModel.M;
		// update armadillo matrix, always divide by m
		_zeroModel.M(r, s) = NewVal / _m->value();

		// calculate Sigma0
		_zeroModel.calculateSigma();
	};

	void _fill_M0_afterUpdate_m(double NewVal, double OldVal) {
		_zeroModel.oldM = _zeroModel.M;
		// update armadillo matrix -> first multiply out effect of old m, then divide by new m
		_zeroModel.M *= (OldVal / NewVal);

		// calculate Sigma0
		_zeroModel.calculateSigma();
	};

	double _calcDoubleSum_oldMinNew_M1(const Storage &Data, size_t n) const {
		// calculates sum_s ( (sum_r M1[r,s] * (param[r] - mu[r]))^2 - (sum_r M1[r,s]' * (param_r - mu_r))^2 )
		// used for updates where M1 has changed -> i.e. update of m1, mrr1, mrs1 and rho
		const auto row  = Data.get1DSlice(1, {n, 0});
		double outerSum = 0.;
		for (size_t s = 0; s < _dim.D; s++) {
			double innerSum    = 0.;
			double innerSumOld = 0.;
			for (size_t r = s; r < _dim.D; r++) {
				// take value for all data elements
				double valueMinMu = (Type)Data[row.begin + r * row.increment] - _mu->value(r);
				innerSum += _oneModel.M(r, s) * valueMinMu;
				innerSumOld += _oneModel.oldM(r, s) * valueMinMu;
			}
			outerSum += innerSumOld * innerSumOld - innerSum * innerSum;
		}

		return outerSum;
	};

	void _setMusToMLE() {
		std::vector<double> means = impl::mvn::calculateMLEMu(this->_storageBelow);
		for (size_t d = 0; d < _dim.D; d++) { _mu->set(d, means[d]); }
	};

	void _setMToMLE() {
		// calculate MLE of M (based on classification by z)

		// initialize data for sums: vector of size K with armadillo matrices of dimensions D times D
		std::vector<arma::mat> sums(2, arma::mat(_dim.D, _dim.D, arma::fill::zeros));

		std::vector<double> totalNumElements(2, 0.);
		for (const auto &storage : this->_storageBelow) {
			for (size_t n = 0; n < storage->dimensions()[0]; n++) { // go over all rows (N)
				size_t k = _z->value(n);
				impl::mvn::addToSumMLESigma(sums[k], n, *storage, _mu);
				totalNumElements[k]++;
			}
		}

		// normalize
		for (size_t k = 0; k < 2; k++) { impl::mvn::normalizeSumMLESigma(sums[k], totalNumElements[k]); }

		// fill parameters of M0
		if (!_Mrr->hasFixedInitialValue() && !_Mrs->hasFixedInitialValue() && !_m->hasFixedInitialValue()) {
			impl::mvn::fillParametersMFromSigma(sums[0], _dim.D, _Mrr, _Mrs, _m);
		} // else don't set them, do nothing

		// fill M1 (only if M0 or rho should be initialized)
		if ((!_Mrr->hasFixedInitialValue() && !_Mrs->hasFixedInitialValue() && !_m->hasFixedInitialValue()) ||
			!_rhos->hasFixedInitialValue()) {
			_oneModel.Sigma = sums[1];
			_oneModel.calculateM();
		}
	};

	void _setAllParametersAfterEM() {
		// first fill zero-model armadillo matrices
		if (!_Mrr->hasFixedInitialValue() && !_Mrs->hasFixedInitialValue() && !_m->hasFixedInitialValue()) {
			impl::mvn::fillArmadilloMFromParametersM(_zeroModel.M, _dim.D, _Mrr, _Mrs, _m);
			_zeroModel.calculateSigma();
		} // else don't set them, do nothing

		// do eigendecomposition
		arma::vec Lambda_0;
		arma::mat Q0;
		arma::eig_sym(Lambda_0, Q0, _zeroModel.Sigma);

		arma::vec Lambda_1;
		arma::mat Q1;
		arma::eig_sym(Lambda_1, Q1, _oneModel.Sigma);

		// calculate norm of eigenvalues
		double norm0 = arma::norm(Lambda_0);
		double norm1 = arma::norm(Lambda_1);

		if (norm0 > norm1) { // we have to switch labels
			if (!_Mrr->hasFixedInitialValue() && !_Mrs->hasFixedInitialValue() && !_m->hasFixedInitialValue()) {
				// M0 (and possibly rho) should be initialized
				// switch M0 and Sigma0 with M1 and Sigma1
				arma::mat tempM1     = _oneModel.M;
				arma::mat tempSigma1 = _oneModel.Sigma;
				_oneModel.M          = _zeroModel.M;
				_oneModel.Sigma      = _zeroModel.Sigma;
				_zeroModel.M         = tempM1;
				_zeroModel.Sigma     = tempSigma1;

				// fill new M0 parameters
				impl::mvn::fillParametersMFromSigma(_zeroModel.Sigma, _dim.D, _Mrr, _Mrs, _m);

				// set rho (pass Lambda_0 and Lambda_1 in opposite order as we switched them)
				_setRhoAfterEM(Lambda_1, Lambda_0);

				// switch z
				_switchEMLabels();
			} else if (!_rhos->hasFixedInitialValue()) {
				// M0 are fix, rhos should be initialized
				// -> set rho to a very small value
				for (size_t d = 0; d < _dim.D; d++) { _rhos->set(d, 0.00001); }

				// don't switch z, we didn't change labels
			} else {
				// both M0 and rhos are fix -> should never be changed in EM, and therefore we should never get here!
				throw coretools::TDevError("Should never have gotten here! Both M0 and rho are fix - the condition that M1 must be "
						 "larger than M0 should never be violated!");
			}
		} else {
			// labels are ok -> simply set rho
			_setRhoAfterEM(Lambda_0, Lambda_1);
		}

		// re-calculate M's and Sigma's to make sure everything is initialized
		_fill_M0();
		_fill_M1();
	};

	void _switchEMLabels() {
		// switch z
		if (!_z->hasFixedInitialValue()) {
			for (size_t i = 0; i < _z->size(); i++) {
				_z->set(i, !_z->value(i)); // set to opposite
			}
			// switch prior on z
			_z->switchPriorClassificationAfterEM();
		}
	};

	void _initializeEMParams_basedOnCutoff(const arma::mat &M) {
		// before EM: initialize EM parameters
		// this is the first round: go over all data, calculate log density under overall mean and variance
		// and assign a certain percentage of x that have the highest log density to model 0 and the rest to model 1

		// calculate log density (mean over all parameters)
		// set M1 to M, to simplify calculation.
		_oneModel.M = M;
		std::vector<double> logDens(_z->size(), 0.);
		for (const auto &storage : this->_storageBelow) {
			for (size_t n = 0; n < storage->dimensions()[0]; n++) { // go over all rows (N)
				logDens[n] += _calcLogPriorDensity_M1_vec(*storage, n);
			}
		}
		// normalize
		for (auto &it : logDens) { it /= this->_storageBelow.size(); }

		// define cutoff: 10% of all x will go into model 1 and 90% into model 0
		double cutoffPercentage           = 0.1;
		// sort densities and define cutoff value
		std::vector<double> sortedLogDens = logDens;
		std::sort(sortedLogDens.begin(), sortedLogDens.end());
		size_t cutoffIndex = _z->size() * cutoffPercentage;
		if (cutoffIndex < 1) {
			cutoffIndex = 1; // avoid cases where all have z = 0
		}
		double cutoffValue = sortedLogDens[cutoffIndex];

		// define components

		// go over the data again and assign components
		for (size_t i = 0; i < _z->size(); i++) {
			if (logDens[i] >= cutoffValue) { // z = 0
				_z->set(i, 0);
			} else { // z = 1
				_z->set(i, 1);
			}
		}

		// re-calculate variances for these components
		_setMToMLE();
	};

	void _initializeEMParams_refinement() {
		// before EM: initialize EM parameters
		// this is the second round: go over all data again, calculate log density under overall mean and the two
		// variances we've calculated in the first round assign x that have a higher log density under the 0-model to
		// z=0 and the rest to z=1
		for (size_t i = 0; i < _z->size(); ++i) {
			size_t MLE_state = TLatentVariable<double, size_t, size_t>::getMLEmissionState(i, 2);
			_z->set(i, MLE_state);
		}

		// re-calculate variances for these components
		_setMToMLE();
	};

	void _setRhoAfterEM(const arma::vec &Lambda_0, const arma::vec &Lambda_1) {
		// calculate rho
		arma::vec rho = Lambda_1 / Lambda_0 - 1.;
		for (size_t d = 0; d < _dim.D; d++) {
			if (rho(d) <= 0.) { rho(d) = 0.00001; }
			_rhos->set(d, rho(d));
		}
	};

	std::array<double, 2> _calcDoubleSum_sampleZ(const Storage &Data, size_t n) const {
		// calculates sum_s ( (sum_r M0[r,s]*(param_r - mu0_r)^2 ) as well as sum_s( (sum_r M1[r,s]*(param_r - mu1_r)^2)
		// ) used when sampling z directly fills OuterSum_0Model and OuterSum_1Model
		std::array<double, 2> outerSum = {0.0, 0.0};
		const auto row                 = Data.get1DSlice(1, {n, 0});
		for (size_t s = 0; s < _dim.D; s++) {
			double innerSum_0Model = 0.;
			double innerSum_1Model = 0.;
			for (size_t r = s; r < _dim.D; r++) {
				// take value for all data elements
				double value = (Type)Data[row.begin + r * row.increment] - _mu->value(r);
				innerSum_0Model += _zeroModel.M(r, s) * value;
				innerSum_1Model += _oneModel.M(r, s) * value;
			}
			outerSum[0] += innerSum_0Model * innerSum_0Model;
			outerSum[1] += innerSum_1Model * innerSum_1Model;
		}
		return outerSum;
	};

	[[nodiscard]] std::array<double, 2> _calcProdMrr_sampleZ() const {
		// directly fills Mrr1 and prod_r Mrr0
		// used when sampling z
		std::array<double, 2> prod = {1.0, 1.0};
		for (size_t d = 0; d < _dim.D; d++) {
			prod[0] *= _zeroModel.M(d, d);
			prod[1] *= _oneModel.M(d, d);
		}
		return prod;
	};

	std::array<double, 2> _calculateLogPosterior_sampleZ(size_t n) {
		// calculate likelihoods
		auto logDensities = calcBothLikelihoods_sampleZ(n);
		// add prior probability
		for (size_t i = 0; i < 2; ++i) {
			_z->set(n, i);
			logDensities[i] += _z->getLogDensity(n);
		}

		return logDensities;
	};

	[[nodiscard]] double _calcLLRatioUpdateM1(size_t N, double Sum) const {
		// used in updates that change M1 -> i.e. m, mrr, mrs and rho
		// calculates prior ratio for any update that changes M1

		// calculate sum_r^D( log(mrr1'/mrr1) ) for 1-model
		coretools::TSumLog<> sum;
		for (size_t d = 0; d < _dim.D; d++) { sum.add(_oneModel.M(d, d) / _oneModel.oldM(d, d)); }
		return (double)N * sum.getSum() + 0.5 * Sum;
	};

	[[nodiscard]] double _calcSumM1_dr_squared(size_t r) const {
		// calculates sum_d^r M1(d,r)^2
		// used for updates of param (getLogDensityRatio_vec) and of mu1
		double sumM = 0.;
		for (size_t d = 0; d <= r; d++) { sumM += _oneModel.M(r, d) * _oneModel.M(r, d); }
		return sumM;
	};

	template<typename T>
	double _calcDoubleSum_M1_updateParam(const T &Data, const coretools::TRange &Row, size_t IndexThatChanged,
										 size_t Min_r_DMin1) const {
		// calculates sum_s_0^min(r, D-1) ( M1[d,s] * (sum_r_r=s^D M1[r,s] * (param[r] - mu1[r]))^2 )
		// used in updates of param (getPriorRatio_vec)
		double sum = 0.;
		// go over all s = 1:min(r, D-1)
		for (size_t s = 0; s < Min_r_DMin1; s++) {
			// go over all r = s:D, r != d
			double tmp = 0.;
			for (size_t r = s; r < _dim.D; r++) {
				if (r == IndexThatChanged) continue;
				tmp += _oneModel.M(r, s) * ((Type)Data[Row.begin + r * Row.increment] - _mu->value(r));
			}
			sum += tmp * _oneModel.M(IndexThatChanged, s);
		}

		return sum;
	};

	double _calcDoubleSum_M1_updateMu(const Storage &Data, const coretools::TRange &Row, size_t IndexThatChanged,
									  size_t Min_r_DMin1) const {
		// calculates sum_s_0^min(r, D-1) ( M1[d,s] * (sum_r_r=s^D M1[r,s] * (param[r] - mu1[r]))^2 )
		// used in updates of mu1
		double sum = 0.;
		// go over all s = 1:min(r, D-1)
		for (size_t s = 0; s < Min_r_DMin1; s++) {
			// go over all r = s:D, r != d
			double tmp = 0.;
			for (size_t r = s; r < _dim.D; r++) {
				if (r == IndexThatChanged) continue;
				tmp += _oneModel.M(r, s) * ((Type)Data[Row.begin + r * Row.increment] - _mu->value(r));
			}
			sum += tmp * _oneModel.M(IndexThatChanged, s);
		}

		return sum;
	};

	double _calcDoubleSum_M1(const Storage &Data, size_t n) const {
		// calculates sum_s ( (sum_r M1[r,s] * (param[r] - mu[r]))^2 )
		// used for calculating the log prior density of a given parameter value, for the one-model
		const auto row  = Data.get1DSlice(1, {n, 0});
		double outerSum = 0;
		for (size_t s = 0; s < _dim.D; s++) {
			double innerSum = 0.;
			for (size_t r = s; r < _dim.D; r++) {
				// take value for all data elements
				innerSum += _oneModel.M(r, s) * ((Type)Data[row.begin + r * row.increment] - _mu->value(r));
			}
			outerSum += innerSum * innerSum;
		}

		return outerSum;
	};

	[[nodiscard]] double _calcSumLogMrr1() const {
		// calculates sum_0^D log(M1[d,d])
		// used for calculating the log prior density of a given parameter value or oldValue, for the one-model
		coretools::TSumLog<> sum;
		for (size_t d = 0; d < _dim.D; d++) { sum.add(_oneModel.M(d, d)); }

		return sum.getSum();
	};

	[[nodiscard]] double _calcProdMrr1() const {
		// calculates prod_0^D M1[d,d]
		// used for calculating the prior density of a given parameter value or oldValue, for the one-model
		double prodMrr = 1.0;
		for (size_t d = 0; d < _dim.D; d++) { prodMrr *= _oneModel.M(d, d); }

		return prodMrr;
	};

	double _calcLogPriorDensity_M1_vec(const Storage &Data, size_t n) const {
		// calculates the log prior density of a given parameter value, for the one-model (z = 1)
		double sumlogMrr1 = _calcSumLogMrr1();
		double doubleSum  = _calcDoubleSum_M1(Data, n);
		return sumlogMrr1 + _dim.minusDdiv2Log2Pi - 0.5 * doubleSum;
	};

	double _calcPriorDensity_M1_vec(const Storage &Data, size_t n) const {
		// calculates the prior density of a given parameter value, for the one-model (z = 1)
		double prodMrr1  = _calcProdMrr1();
		double doubleSum = _calcDoubleSum_M1(Data, n);
		return prodMrr1 / _dim.sqrt2piD * exp(-0.5 * doubleSum);
	};

	double _calcLogPriorRatio_M1_vec(const UpdatedStorage &Data, size_t n, size_t d) const {
		const auto row  = Data.get1DSlice(1, {n, 0});
		double oldParam = Data[row.begin + d * row.increment].oldValue();
		double newParam = (Type)Data[row.begin + d * row.increment];

		// calculate simplified hastings ratio
		double sumM        = _calcSumM1_dr_squared(d);
		size_t min_r_DMin1 = std::min(d + 1, _dim.D - 1);
		double sum         = _calcDoubleSum_M1_updateParam(Data, row, d, min_r_DMin1);
		double tmp = sumM * (oldParam * oldParam - newParam * newParam + 2. * _mu->value(d) * (newParam - oldParam)) +
					 2. * (oldParam - newParam) * sum;

		return 0.5 * tmp;
	};

	void _fillBothLikelihoods_sampleZ(Storage *Data, size_t n, std::array<double, 2> &LogDensities) const {
		const auto sum  = _calcDoubleSum_sampleZ(*Data, n);
		const auto prod = _calcProdMrr_sampleZ();
		// calculate log P(z = 0) and log P(z = 1)
		// note: no need to normalize with sqrt(2*pi)^D, since normalization is the same for both 0 and 1 model
		for (size_t i = 0; i < 2; ++i) { LogDensities[i] += log(prod[i]) - 0.5 * sum[i]; }
	};

	void _simulateUnderPrior(Storage *Data) override {
		// first fill M0 from current parameters and calculate M1: might have changed in the meantime
		_fill_M0();
		_fill_M1();

		assert(Data->dimensions()[1] == (size_t)_dim.D); // number of columns of data must match D

		// create re-usable MVN objects
		std::vector<double> mus(_dim.D);
		for (size_t d = 0; d < (size_t)_dim.D; d++) { mus[d] = _mu->value(d); }
		std::vector<coretools::probdist::TMultivariateNormalDistr<std::vector<double>>> MVNs(2);
		MVNs[0].set(mus, _zeroModel.Sigma);
		MVNs[1].set(mus, _oneModel.Sigma);

		for (size_t n = 0; n < Data->dimensions()[0]; n++) { // go over all rows
			coretools::TRange row = Data->get1DSlice(1, {n, 0});
			size_t k              = _z->value(n);
			impl::mvn::simulateMultivariateNormal(*Data, row.begin, MVNs[k]);
		}
	};

public:
	TTwoMultivariateNormalsMixedModelInferred(TypeParamZ *Z, TypeParamMus *Mu, TypeParamM *M, TypeParamMrr *Mrr,
											  TypeParamMrs *Mrs, TypeParamRho *Rho)
		: _z(Z), _mu(Mu), _m(M), _Mrr(Mrr), _Mrs(Mrs), _rhos(Rho) {
		this->addPriorParameter({_z, _mu, _m, _Mrr, _Mrs, _rhos});
	};
	~TTwoMultivariateNormalsMixedModelInferred() override = default;

	[[nodiscard]] std::string name() const override { return "multivariateNormal_mixedModel_stretch"; };

	void initialize() override {
		// derive D from dimensionality of parameters below
		_dim = impl::mvn::calculateAndSetD(this->_storageBelow);

		auto dimNameD       = this->_storageBelow[0]->getDimensionName(1);
		const auto dimNameZ = impl::mvn_mix::getDimensionNameForZ(this->_storageBelow);

		_mu->initStorage(this, {_dim.D}, {dimNameD});
		_m->initStorage(this);
		_Mrr->initStorage(this, {_dim.D}, {dimNameD});
		// check: if D=1 (univariate normal distribution), we will only update m and mu, and fix mrr to 1.
		if (_dim.D == 1) {
			_Mrr->set(0, 1.);
			_Mrr->setIsUpdated(false);
		}
		if (_dim.D > 1) { // only check if there are >1 dimension (mrs are not defined for D=1)
			auto dimNamesMrs = impl::mvn::generateDimNamesMrs(dimNameD);
			_Mrs->initStorage(this, {impl::mvn::numberOfElementsInTriangularMatrix_Diagonal0(_dim.D)}, {dimNamesMrs});
		} else {
			auto dimNamesMrs = std::make_shared<coretools::TNamesStrings>(0);
			_Mrs->initStorage(this, {0}, {dimNamesMrs});
			_Mrs->setIsUpdated(false);
		}
		_z->initStorage(this, {this->_storageBelow[0]->dimensions()[0]}, {dimNameZ});
		_rhos->initStorage(this, {_dim.D}, {dimNameD});

		// check if Mrs is at its default initial value (= numeric limit of double)
		// if yes: change it to 0., because taking the inverse of such values will throw
		if (!_Mrs->hasFixedInitialValue()) {
			coretools::TRange range = _Mrs->getFull();
			for (size_t i = range.begin; i < range.end; i += range.increment) { _Mrs->set(i, 0.); }
		}

		// initialize armadillo matrices
		_zeroModel.init(_dim.D);
		_oneModel.init(_dim.D);

		// fill M0 from current parameters and calculate M1
		_fill_M0();
		_fill_M1();
	};

	double getDensity(const Storage &Data, size_t Index) const override {
		auto subscript_updatedParam = Data.getSubscripts(Index);
		size_t n                    = subscript_updatedParam[0];

		if (_z->value(n) == 1) { // z = 1
			return _calcPriorDensity_M1_vec(Data, n);
		}
		return impl::mvn::calcPriorDensity(Data, n, _mu, _m, _Mrr, _Mrs, _dim);
	};

	double getLogDensity(const Storage &Data, size_t Index) const override {
		auto subscript_updatedParam = Data.getSubscripts(Index);
		size_t n                    = subscript_updatedParam[0];

		if (_z->value(n) == 1) { // z = 1
			return _calcLogPriorDensity_M1_vec(Data, n);
		}
		return impl::mvn::calcLogPriorDensity(Data, n, _mu, _m, _Mrr, _Mrs, _dim);
	};

	double getLogDensityRatio(const UpdatedStorage &Data, size_t Index) const override {
		auto subscript_updatedParam = Data.getSubscripts(Index);
		size_t n                    = subscript_updatedParam[0];
		size_t d                    = subscript_updatedParam[1];

		if (_z->value(n) == 1) { return _calcLogPriorRatio_M1_vec(Data, n, d); }
		return impl::mvn::calcLogPriorRatio(Data, n, d, _mu, _m, _Mrr, _Mrs);
	};

	double getSumLogPriorDensity(const Storage &Data) const override {
		double sum = 0.;
		for (size_t n = 0; n < Data.dimensions()[0]; n++) {
			coretools::TRange row = Data.get1DSlice(1, {n, 0});
			sum += getLogDensity(Data, row.begin);
		}
		return sum;
	};

	[[nodiscard]] auto calculateLLRatio(TypeParamMus *, size_t i) const {
		const auto sumM0   = impl::mvn::calcSumM_dr_squared(_Mrr->value(i), i, _Mrs);
		const auto sumM1   = _calcSumM1_dr_squared(i);
		size_t min_r_DMin1 = std::min(i + 1, (size_t)_dim.D - 1);

		auto f = [this, i, min_r_DMin1, sumM0, sumM1](Storage *Data) {
			const double oldMu          = _mu->oldValue(i);
			const double newMu          = _mu->value(i);
			size_t N                    = Data->dimensions()[0];
			std::array<double, 2> sum_0 = {0.0, 0.0};
			std::array<double, 2> sum_1 = {0.0, 0.0};
			size_t c_z0                 = 0;
			for (size_t n = 0; n < N; n++) {
				coretools::TRange row = Data->get1DSlice(1, {n, 0});
				if (_z->value(n) == 0) {
					sum_0[0] += impl::mvn::calcDoubleSum_updateMu(*Data, row, i, min_r_DMin1, _mu, _Mrr, _Mrs);
					sum_0[1] += (Type)(*Data)[row.begin + i * row.increment];
					++c_z0;
				} else {
					sum_1[0] += _calcDoubleSum_M1_updateMu(*Data, row, i, min_r_DMin1);
					sum_1[1] += (Type)(*Data)[row.begin + i * row.increment];
				}
			}

			size_t c_z1 = N - c_z0;
			return 0.5 * ((double)c_z1 * (oldMu * oldMu - newMu * newMu) * sumM1 +
						  2. * (newMu - oldMu) * (sumM1 * sum_1[1] + sum_1[0])) +
				   1. / (2. * _m->value() * _m->value()) *
					   ((double)c_z0 * (oldMu * oldMu - newMu * newMu) * sumM0 +
						2. * (newMu - oldMu) * (sumM0 * sum_0[1] + sum_0[0]));
		};
		return f;
	}

	void updateTempVals(TypeParamMus *, size_t, bool) { /* emtpy - no tmp values for mu */
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamMrr *, size_t i) {
		// update M0, Sigma0, Sigma1 and M1
		_fill_M0_afterUpdateMrr(_Mrr->value(i), i);
		_fill_M1();

		auto f = [this, i](Storage *Data) {
			size_t N                   = Data->dimensions()[0];
			std::array<double, 2> sums = {0.0, 0.0};
			double sumP                = 0.;
			size_t c_z1                = 0;
			for (size_t n = 0; n < N; n++) {
				if (_z->value(n) == 1) {
					sums[1] += _calcDoubleSum_oldMinNew_M1(*Data, n);
					++c_z1;
				} else {
					sums[0] += impl::mvn::calcDoubleSum_updateMrr(*Data, n, i, sumP, _mu, _Mrs);
				}
			}

			size_t c_z0 = N - c_z1;
			double r0   = impl::mvn::calcLLRatioUpdateMrr(c_z0, _Mrr->oldValue(i), _Mrr->value(i), sums[0], sumP, _m);
			double r1   = _calcLLRatioUpdateM1(c_z1, sums[1]);
			return r0 + r1;
		};
		return f;
	}

	void updateTempVals(TypeParamMrr *, size_t, bool Accepted) {
		if (!Accepted) { // rejected -> reset M0, Sigma0, M1 and Sigma1
			_zeroModel.reset();
			_oneModel.reset();
		}
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamMrs *, size_t i) {
		// update M0, Sigma0, Sigma1 and M1
		auto p       = impl::mvn::getCoordinatesFromLinearIndex_Mrs(i);
		const auto r = p.first;
		const auto s = p.second;

		_fill_M0_afterUpdateMrs(_Mrs->value(i), r, s);
		_fill_M1();

		auto f = [this, r, s, i](Storage *Data) {
			size_t N                   = Data->dimensions()[0];
			std::array<double, 2> sums = {0.0, 0.0};
			double sumP                = 0.;
			size_t c_z1                = 0;
			for (size_t n = 0; n < N; n++) {
				if (_z->value(n) == 1) {
					sums[1] += _calcDoubleSum_oldMinNew_M1(*Data, n);
					++c_z1;
				} else {
					sums[0] += impl::mvn::calcDoubleSum_updateMrs(*Data, n, r, s, sumP, _mu, _Mrr, _Mrs);
				}
			}

			double r0 = impl::mvn::calcLLRatioUpdateMrs(_Mrs->oldValue(i), _Mrs->value(i), sums[0], sumP, _m);
			double r1 = _calcLLRatioUpdateM1(c_z1, sums[1]);
			return r0 + r1;
		};
		return f;
	}

	void updateTempVals(TypeParamMrs *, size_t, bool Accepted) {
		if (!Accepted) { // rejected -> reset M0, Sigma0, M1 and Sigma1
			_zeroModel.reset();
			_oneModel.reset();
		}
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamM *, size_t) {
		// update M0, Sigma0, Sigma1 and M1
		_fill_M0_afterUpdate_m(_m->value(), _m->oldValue());
		_fill_M1();

		auto f = [this](Storage *Data) {
			size_t N                   = Data->dimensions()[0];
			std::array<double, 2> sums = {0.0, 0.0};
			size_t c_z1                = 0;
			for (size_t n = 0; n < N; n++) {
				if (_z->value(n) == 1) {
					sums[1] += _calcDoubleSum_oldMinNew_M1(*Data, n);
					++c_z1;
				} else {
					sums[0] += impl::mvn::calcDoubleSum(*Data, n, _mu, _Mrr, _Mrs);
				}
			}
			size_t c_z0         = N - c_z1;
			double ratio_0Model = impl::mvn::calcLLRatioUpdateM(c_z0, _dim.D, _m->oldValue(), _m->value(), sums[0]);
			double ratio_1Model = _calcLLRatioUpdateM1(c_z1, sums[1]);

			return ratio_0Model + ratio_1Model;
		};
		return f;
	}

	void updateTempVals(TypeParamM *, size_t, bool Accepted) {
		if (!Accepted) {
			// rejected -> reset M0, Sigma0, M1 and Sigma1
			_zeroModel.reset();
			_oneModel.reset();
		}
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamRho *, size_t) {
		_fill_M1();

		auto f = [this](Storage *Data) {
			size_t c          = 0;
			double sum_1Model = 0.0;
			for (size_t n = 0; n < Data->dimensions()[0]; n++) {
				if (_z->value(n) == 1) {
					sum_1Model += _calcDoubleSum_oldMinNew_M1(*Data, n);
					++c;
				}
			}

			return _calcLLRatioUpdateM1(c, sum_1Model);
		};
		return f;
	}

	void updateTempVals(TypeParamRho *, size_t, bool Accepted) {
		if (!Accepted) { // rejected -> reset M1 and Sigma1 (but not M0 and Sigma0, as these didn't change)
			_oneModel.reset();
		}
	}

	[[nodiscard]] auto calculateLLRatio(TypeParamZ *, size_t i) const {
		auto f = [this, i](Storage *Data) {
			std::array<double, 2> logDensities = {0.0, 0.0};
			_fillBothLikelihoods_sampleZ(Data, i, logDensities);
			return logDensities[_z->value(i)] - logDensities[_z->oldValue(i)];
		};
		return f;
	}

	void updateTempVals(TypeParamZ *, size_t, bool) { /* empty - there are no tmp values*/
	}

	[[nodiscard]] std::array<double, 2> calcBothLikelihoods_sampleZ(size_t n) const {
		// compute likelihood, directly filling Dens0 and Dens1
		std::array<double, 2> logDensities = {0.0, 0.0};
		for (const auto &storage : this->_storageBelow) { _fillBothLikelihoods_sampleZ(storage, n, logDensities); }
		return logDensities;
	};

	void doGibbs(TypeParamZ *, size_t i) {
		_z->template sampleDiscrete<true, true>(i, _calculateLogPosterior_sampleZ(i));
	}

	void setMeanAndM() {
		_setMusToMLE();
		_setMToMLE();
	};

	void guessInitialValues() override {
		// run EM to initialize prior parameters, and pretend that the two variances are independent
		impl::mvn_mix::checkForFixedInitialValues(_m, _Mrr, _Mrs, name());
		if (!_z->hasFixedInitialValue()) { // latent variable not known -> run EM
			_z->runEMEstimation(*this);
		} else {
			setMeanAndM();
		}

		// now adjust to model: set larger variance to variance of 1-model
		_setAllParametersAfterEM();
	};

	void initializeEMParameters() override {
		// calculate overall mean and Sigma
		_setMusToMLE();
		arma::mat Sigma = impl::mvn::calculateMLESigma(this->_storageBelow, _mu);
		arma::mat M     = impl::mvn::calculateMFromSigma(Sigma);

		// first round: go over all data and assign a certain percentage to z=0 and the rest to z=1 based on the log
		// density under overall mean and variance
		_initializeEMParams_basedOnCutoff(M);

		// second round: go over all data again and assign z to the model that maximizes the log density (based on
		// the variances we've initialized in the first round)
		_initializeEMParams_refinement();
	};

	[[nodiscard]] size_t getMLEmissionState(size_t Index, size_t) const override { return _z->value(Index); };

	void calculateEmissionProbabilities(size_t Index, TDataVector<double, size_t> &Emission) const override {
		// initialize emissions with zero
		for (size_t k = 0; k < 2; k++) { Emission[k] = 0.; }

		// go over all parameters and over all components and sum emission probability
		// we want emission not in log, but we will use the log to sum and then de-log in the end
		for (const auto &storage : this->_storageBelow) {
			Emission[0] += impl::mvn::calcLogPriorDensity(*storage, Index, _mu, _m, _Mrr, _Mrs, _dim);
			Emission[1] += _calcLogPriorDensity_M1_vec(*storage, Index);
		}

		// get rid of log
		for (size_t k = 0; k < 2; k++) { Emission[k] = exp(Emission[k]); }
	};

	void prepareEMParameterEstimationInitial() override {
		// initialize data: per component, we need to store two sums
		// 2) sum_{i=1}^N weights_ki * sum_{p=1}^P (x_pi - mu_k)(x_pi - mu_k)^T
		// 3) sum_{i=1}^N weights_ki
		_EM_update_Sigma_temp.resize(2, arma::mat(_dim.D, _dim.D, arma::fill::zeros));
		_EM_update_Weights_temp.resize(2);
	};

	void prepareEMParameterEstimationOneIteration() override {
		// fill data with zeros
		for (size_t i = 0; i < 2; i++) {
			_EM_update_Sigma_temp[i].zeros();
			_EM_update_Weights_temp[i] = 0.;
		}
	};

	void handleEMParameterEstimationOneIteration(size_t Index, const TDataVector<double, size_t> &Weights) override {
		// calculate
		// sum_{p=1}^P sum_{i=1}^N (x_pi - mu_k)(x_pi - mu_k)^T

		// initialize data for sums
		std::vector<arma::mat> sumXpiMinusMuSquare(
			2); // 3 dimensions: K times an Armadillo matrix with D times D dimensions
		for (size_t k = 0; k < 2; k++) { sumXpiMinusMuSquare[k] = arma::mat(_dim.D, _dim.D, arma::fill::zeros); }

		for (const auto &storage : this->_storageBelow) {
			coretools::TRange row = storage->get1DSlice(1, {Index, 0}); // one row
			for (size_t d = 0; d < _dim.D; d++) {
				for (size_t k = 0; k < 2; k++) {
					for (size_t e = 0; e < _dim.D; e++) {
						sumXpiMinusMuSquare[k](d, e) +=
							((Type)(*storage)[row.begin + d * row.increment] - _mu->value(d)) *
							((Type)(*storage)[row.begin + e * row.increment] - _mu->value(e));
					}
				}
			}
		}

		// add to sums
		for (size_t k = 0; k < 2; k++) {
			// add to sum (x_pi - mu_k)(x_pi - mu_k)^T * weights_ki
			impl::mvn_mix::handleEMMaximizationOneIteration_updateSigma(k, _dim.D, sumXpiMinusMuSquare, Weights,
																		_EM_update_Sigma_temp[k]);
			// add to sum weights
			_EM_update_Weights_temp[k] += Weights[k];
		}
	};

	void finalizeEMParameterEstimationOneIteration() override {
		// M-step: update parameters

		// 0-model: only if M0 should be initialized
		if (!_Mrr->hasFixedInitialValue() && !_Mrs->hasFixedInitialValue() && !_m->hasFixedInitialValue()) {
			arma::mat Sigma;
			impl::mvn_mix::updateEMParametersOneIteration_Sigma(Sigma, _dim.D, this->_storageBelow.size(),
																_EM_update_Sigma_temp[0], _EM_update_Weights_temp[0]);

			impl::mvn::fillParametersMFromSigma(Sigma, _dim.D, _Mrr, _Mrs, _m);
		} // else don't set them, do nothing

		// 1-model: only if either M0 or rho should be initialized
		if ((!_Mrr->hasFixedInitialValue() && !_Mrs->hasFixedInitialValue() && !_m->hasFixedInitialValue()) ||
			!_rhos->hasFixedInitialValue()) {
			arma::mat Sigma;
			impl::mvn_mix::updateEMParametersOneIteration_Sigma(Sigma, _dim.D, this->_storageBelow.size(),
																_EM_update_Sigma_temp[1], _EM_update_Weights_temp[1]);
			_oneModel.Sigma = Sigma;
			_oneModel.calculateM();
		}
	};

	void finalizeEMParameterEstimationFinal() override {
		// clean memory
		_EM_update_Sigma_temp.clear();
		_EM_update_Weights_temp.clear();
	};

	void handleStatePosteriorEstimation(size_t Index,
										const TDataVector<double, size_t> &StatePosteriorProbabilities) override {
		_z->set(Index, StatePosteriorProbabilities.maxIndex());
	};

	// getters
	[[nodiscard]] const arma::mat &getM0() const { return _zeroModel.M; }
	[[nodiscard]] const arma::mat &getM1() const { return _oneModel.M; }
	[[nodiscard]] const arma::mat &getSigma0() const { return _zeroModel.Sigma; }
	[[nodiscard]] const arma::mat &getSigma1() const { return _oneModel.Sigma; }
};

} // end namespace stattools::prior

#endif // TPRIORTWOMULTIVARIATENORMALSMIXEDMODEL_H
