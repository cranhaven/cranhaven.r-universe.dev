//
// Created by madleina on 15.11.22.
//

#ifndef TKMEANS_H
#define TKMEANS_H

#include "coretools/Math/TMatrix.h"
#include <cmath>
#include <utility>

namespace stattools {

template<typename TypeMatrix, bool WithMissingData = false> class TDataForKMeans {
	// data class used in KMeans algorithm
	// TypeMatrix can be arma::mat or coretools::TMatrix or TStorage or anything else that defines the (i,j) operator
	// if WithMissingData = true: allow for missing data (-> rows) in some replicates -> simply ignored for k-means
private:
	std::vector<TypeMatrix> _data;
	std::vector<std::vector<bool>> _isMissing;

public:
	explicit TDataForKMeans(const std::vector<TypeMatrix> &Data) : _data(Data) {
		// constructor that is used if there are no missing data
		static_assert(!WithMissingData);
	}

	TDataForKMeans(const std::vector<TypeMatrix> &Data, size_t N, std::vector<std::vector<bool>> IsMissing)
		: _data(Data), _isMissing(std::move(IsMissing)) {
		// constructor that is used if there are missing data
		static_assert(WithMissingData);
		// check if sizes match
		if (_data.size() != _isMissing.size()) {
			throw coretools::TDevError("Number of replicates of data (", _data.size(), ") does not match isMissing (", _isMissing.size(),
					 ")!");
		}
		for (const auto &it : _isMissing) {
			if (it.size() != N) { throw coretools::TDevError("Size of isMissing (", it.size(), ") does not match N (", N, ")!"); };
			if (coretools::containerSum(it) ==
				_isMissing.size()) { // at least one data point must not be missing per replicate
				throw coretools::TDevError("All data are missing for a replicate!");
			}
		}
	}

	[[nodiscard]] bool isMissing(size_t r, size_t n) const {
		if constexpr (WithMissingData) {
			return _isMissing[r][n];
		} else {
			return false;
		}
	}

	auto operator()(size_t r, size_t n, size_t m) const {
		assert(!isMissing(r, n));
		return _data[r](n, m);
	}

	size_t size() const { return _data.size(); }
};

template<typename TypeMatrix, bool WithMissingData = false> class TKMeans {
	// Solve for a k-means clustering model
	// TypeMatrix can be arma::mat or coretools::TMatrix or TStorage or anything else that defines the (i,j) operator
	// if WithMissingData = true: not each row (n) has data in all replicates
private:
	// dimensions
	size_t _N = 0; // number of rows of data
	size_t _M = 0; // number of columns of data
	size_t _K = 0; // number of clusters
	size_t _R = 0; // number of replicates (share assignment but have different means)

	TDataForKMeans<TypeMatrix, WithMissingData> _data;
	std::vector<coretools::TMatrix<double>> _means;

	std::vector<size_t> _assignment;
	std::vector<size_t> _count;

	double _calculateSumOfSquaredDistances(size_t n, size_t k) const {
		// calculate sum of squared distances
		double d = 0.0;
		for (size_t r = 0; r < _R; ++r) { // loop over replicates
			if (_data.isMissing(r, n)) { continue; }
			for (size_t m = 0; m < _M; m++) { // loop over columns
				const double v = (double)_data(r, n, m) - _means[r](k, m);
				d += v * v;
			}
		}
		return d;
	}

	std::pair<size_t, double> _findClosestCluster(size_t n, size_t K) {
		size_t kmin = 0;
		double dmin = std::numeric_limits<double>::max();
		for (size_t k = 0; k < K; k++) { // loop over clusters
			const double d = _calculateSumOfSquaredDistances(n, k);
			// decide if cluster is closest so far
			if (d < dmin) {
				dmin = d;
				kmin = k;
			}
		}
		return std::make_pair(kmin, dmin);
	}

	size_t _findClosestCluster(size_t n) { return _findClosestCluster(n, _K).first; }

	void _findInitialClusters() {
		// use the k-means++ algorithm from David Arthur and Sergei Vassilvitskii
		// (http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf)
		using namespace coretools::instances;
		for (size_t r = 0; r < _R; ++r) { _means[r].zeros(_K, _M); }

		// sample a first point at random: can be different for each replicate
		for (size_t r = 0; r < _R; ++r) {
			size_t initial_index = randomGenerator().getRand(0UL, _N);
			// make sure data point actually exists
			while (_data.isMissing(r, initial_index)) { initial_index = randomGenerator().getRand(0UL, _N); }
			for (size_t m = 0; m < _M; m++) { _means[r](0, m) = _data(r, initial_index, m); }
		}

		// loop for K-1 iterations and select the remaining points
		for (size_t k = 1; k < _K; k++) {
			// calculate for each point the distance of the point from its nearest center
			std::vector<double> distances(_N);
			std::vector<size_t> centers(_N);
			for (size_t n = 0; n < _N; n++) { std::tie(centers[n], distances[n]) = _findClosestCluster(n, k); }

			// sample cluster index from this distribution
			coretools::normalize(distances);
			for (size_t r = 0; r < _R; ++r) {
				size_t ix = randomGenerator().pickOneRawProbabilities(distances);
				while (_data.isMissing(r, ix)) { ix = randomGenerator().pickOneRawProbabilities(distances); }
				for (size_t m = 0; m < _M; m++) { _means[r](k, m) = _data(r, ix, m); }
			}
		}
	}

	size_t _eStep() {
		// Assign each data point to the component k whose mean mu_k it is closest to
		size_t counterNumChanges = 0;
		std::fill(_count.begin(), _count.end(), 0);
		for (size_t n = 0; n < _N; n++) {
			size_t kmin = _findClosestCluster(n);
			if (kmin != _assignment[n]) { counterNumChanges++; }
			_assignment[n] = kmin;
			_count[kmin]++;
		}
		return counterNumChanges;
	}

	void _mStep() {
		// For all k, re-estimate the mean mu_k as the average of data points assigned to component k (for each rep)
		for (size_t r = 0; r < _R; ++r) { // loop over replicates
			_means[r].set(0.0);
			for (size_t n = 0; n < _N; n++) { // loop over data points
				if (_data.isMissing(r, n)) { continue; }
				for (size_t m = 0; m < _M; m++) { // loop over columns
					_means[r](_assignment[n], m) += _data(r, n, m);
				}
			}
			for (size_t k = 0; k < _K; k++) { // loop over clusters
				if (_count[k] > 0) {
					for (size_t m = 0; m < _M; m++) { // loop over columns
						_means[r](k, m) /= _count[k];
					}
				}
			}
		}
	}

	double _calculateSumOfSquares() const {
		double SS = 0.0;
		for (size_t n = 0; n < _N; ++n) { SS += _calculateSumOfSquaredDistances(n, _assignment[n]); }
		return SS;
	}

public:
	TKMeans(size_t NumRows, size_t NumCols, size_t K, const TDataForKMeans<TypeMatrix, WithMissingData> &Data)
		: _N(NumRows), _M(NumCols), _K(K), _R(Data.size()), _data(Data), _means(_R),
		  _count(_K, 0){
			  // Data: a vector of an N times M matrix of type TypeMatrix
			  // -> each element in the vector corresponds to a replicate
			  // -> replicates share the cluster assignment but have different cluster means
			  // provide dimensions of Data separately since different templates might use different keywords
		  };

	std::pair<std::vector<coretools::TMatrix<double>>, std::vector<size_t>>
	doKMeans(size_t NumDifferentStartingPositions = 1) {
		double bestSS = std::numeric_limits<double>::max();
		std::vector<coretools::TMatrix<double>> bestMeans;
		std::vector<size_t> bestAssignment;

		for (size_t trial = 0; trial < NumDifferentStartingPositions; ++trial) {
			_assignment.resize(_N, 0);
			// initialize means
			_findInitialClusters();

			// do k-means clustering until e-step does not change the assignment of any data point
			size_t counterNumChanges = _N;
			while (counterNumChanges > 0) {
				counterNumChanges = _eStep();
				_mStep();
			}

			// calculate sum of squared distances
			const double SS = _calculateSumOfSquares();
			if (SS < bestSS) {
				bestSS         = SS;
				bestMeans      = _means;
				bestAssignment = _assignment;
			}
		}

		// return means and an assignment of each data point to one component
		return std::make_pair(bestMeans, bestAssignment);
	}
};
} // end namespace stattools

#endif // TKMEANS_H
