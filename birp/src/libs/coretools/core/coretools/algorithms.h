//
// Created by madleina on 19.04.21.
//

#ifndef BANGOLIN_ALGORITHMS_H
#define BANGOLIN_ALGORITHMS_H

#include "coretools/Main/TError.h"
#include "coretools/Types/probability.h"
#include <numeric>
#include <utility>

namespace coretools {

//--------------------------------------------
// Ranks and sorting
//--------------------------------------------

template<typename Container, typename I>
void rankSort(const Container &input, std::vector<I> &ranks, bool decreasing = false) {
	static_assert(std::is_integral_v<I>);
	// creates a vector of ranks (corresponding to the index of the new sorted
	// vector inside the original vector) e.g. input = {5, 2, 1, 4, 3} will return
	// ranks = {2, 1, 4, 3, 0} corresponds to sort(input, index.return=T)$ix in R
	// stolen from:
	// https://stackoverflow.com/questions/1577475/c-sorting-and-keeping-track-of-indexes

	// initialize original index locations
	ranks.resize(input.size());
	std::iota(ranks.begin(), ranks.end(), 0);

	// sort indexes based on comparing values in v
	// using std::stable_sort instead of std::sort to avoid unnecessary index
	// re-orderings when v contains elements of equal values
	if (decreasing) {
		std::stable_sort(ranks.begin(), ranks.end(), [&input](size_t i1, size_t i2) { return input[i1] > input[i2]; });
	} else {
		std::stable_sort(ranks.begin(), ranks.end(), [&input](size_t i1, size_t i2) { return input[i1] < input[i2]; });
	}
}

template<typename Container> std::vector<size_t> rankSort(const Container &input, bool decreasing = false) {
	std::vector<size_t> ranks;
	rankSort(input, ranks, decreasing);
	return ranks;
}

template<typename ContainerIn, typename I, typename ContainerOut>
void sortContainerByRank(const ContainerIn &input, const std::vector<I> &ranks, ContainerOut &output) {
	assert(input.size() == ranks.size());

	output.resize(input.size());
	std::transform(ranks.begin(), ranks.end(), output.begin(), [&](std::size_t i) { return input[i]; });
}

template<typename ContainerOut, typename ContainerIn, typename I>
ContainerOut sortContainerByRank(const ContainerIn &input, const std::vector<I> &ranks) {
	ContainerOut sorted_vec;
	sortContainerByRank(input, ranks, sorted_vec);
	return sorted_vec;
}

template<typename Container, typename I>
Container sortContainerByRank(const Container &input, const std::vector<I> &ranks) {
	return sortContainerByRank<Container, Container, I>(input, ranks);
}

// Function to find rank 
template<typename Container>
std::vector<double> ranks(const Container &vs) { 
    //get indeces of elements in vs after sorting	
	auto rankIndex = rankSort(vs);
	
	// now calculate ranks. Give average ranks in case of ties
    std::vector<double> theRanks(vs.size());
    size_t lenMinusOne = rankIndex.size() - 1;
	size_t i = 0;
    while(i < rankIndex.size()){ 
        size_t firstIndex = i;
   
        // Get number of elements with equal rank
        while(i < lenMinusOne && vs[rankIndex[i]] == vs[rankIndex[i+1]]){ 
            i++; 
		}

		// calc average rank: add + 1 so that the first rank is 1 (not 0)
		double avgRank = 0.5 * (firstIndex + i) + 1.0;
   
		// set average rank to all those indeces
        for(size_t k = firstIndex; k <= i;++k){ 
            theRanks[rankIndex[k]] = avgRank;
        } 

        // Increment i 
        i++; 
    } 
	return theRanks;
}

//--------------------------------------------
// stats on container
//--------------------------------------------

template<typename Container> size_t numNonZero(const Container &vs) {
	return std::count_if(vs.begin(), vs.end(), [](auto v) { return v != typename Container::value_type{}; });
}

template<typename Container1, typename Container2>
double weightedSum(const Container1 &values, const Container2 &weights) {
	assert(values.size() == weights.size());
	return std::inner_product(values.begin(), values.end(), weights.begin(), 0.);
}

template<class ForwardIterator> double sumOfSquares(ForwardIterator First, ForwardIterator Last) {
	return std::inner_product(First, Last, First, 0.0);
}

template<typename Container> double sumOfSquares(const Container &vs) { return sumOfSquares(vs.begin(), vs.end()); }

template<typename Container1, typename Container2> void pairwiseSum(Container1 &v1, const Container2 &v2) {
	// adds each element of v2 to corresponding element in v1 (pairwise)
	assert(v1.size() == v2.size());
	std::transform(v1.begin(), v1.end(), v2.cbegin(), v1.begin(), std::plus<typename Container1::value_type>());
}

template<typename Container> double vectorNorm(const Container &vs) {
	// calculates euclidean norm (alias length or magnitude) of a euclidean vector
	// norm(x) = sqrt(sum x_i^2)
	return sqrt(sumOfSquares(vs));
}

template<class ForwardIterator> double vectorNorm(ForwardIterator First, ForwardIterator Last) {
	return sqrt(sumOfSquares(First, Last));
}

template<typename Container1, typename Container2>
void fillCumulative(const Container1 &probabilities, Container2 &cumulative) {
	// only works for floating point types
	using Type      = typename Container1::value_type;
	using CumulType = typename Container2::value_type;

	// static_assert(std::is_same_v<Type, CumulType>);
	static_assert(std::is_floating_point_v<Type> || std::is_same_v<Type, Probability>);

	cumulative.clear();
	if (probabilities.empty()) return;

	cumulative.reserve(probabilities.size());

	if (probabilities.size() == 1) {
		cumulative.emplace_back(1.0);
	} else {
		if constexpr (std::is_same_v<CumulType, Probability>) {
			std::vector<double> tmp;
			fillCumulative(probabilities, tmp);
			for (auto t : tmp) cumulative.emplace_back(t);
		} else {
			// faster without this restriction
			std::partial_sum(probabilities.begin(), probabilities.end(), std::back_insert_iterator(cumulative));
			std::transform(cumulative.begin(), cumulative.end(), cumulative.begin(),
			               [sum = cumulative.back()](auto c) { return c / sum; });
		}
	}
}

//--------------------------------------------
// Statistics
//--------------------------------------------
template<typename Container> void reset(Container &vs) {
	std::fill(vs.begin(), vs.end(), typename Container::value_type{});
}

template<typename Container> typename Container::value_type containerSum(const Container &vs) {
	static_assert(!std::is_same_v<typename Container::value_type, bool>); // return type is also bool, will return max 1
	return std::accumulate(vs.begin(), vs.end(), typename Container::value_type{});
}

template<typename Container> typename Container::value_type containerProduct(const Container &vs) {
	return std::accumulate(vs.begin(), vs.end(), 1, std::multiplies<typename Container::value_type>());
}

template<typename Container> double mean(const Container &vs) { return containerSum(vs) / vs.size(); };

template<typename Container> std::pair<double, double> meanVar(const Container &vs) {
	double sum  = 0.0;
	double sum2 = 0.0;
	std::for_each(vs.begin(), vs.end(), [&](auto v) {
		sum += v;
		sum2 += v * v;
	});
	const double mean = sum / vs.size();
	const double var  = sum2 / vs.size() - mean * mean;
	return {mean, var};
}

template<typename Container> double var(const Container &vs) { return meanVar(vs).second; };
template<typename Container> double sd(const Container &vs) { return sqrt(meanVar(vs).second); };

template<typename Container> double sumPairwiseProduct(const Container &first, const Container second) { 
	// returns first[0]*second[0] + first[1]*second[1] + ...
	assert(first.size() == second.size());
	double sum = 0.0;
	 for (size_t i = 0; i < first.size(); ++i){
     	sum += first[i] * second[i];
 	}
	return sum;
}

template<typename Container> double pearsonCorrelation(const Container &first, const Container second) { 
	// return the Pearson correlation
	assert(first.size() == second.size());
	auto meanVarFirst = meanVar(first);
	auto meanVarSecond = meanVar(second);
	double E_XY = sumPairwiseProduct(first, second) / (double) first.size();

	return (E_XY - meanVarFirst.first * meanVarSecond.first) / sqrt(meanVarFirst.second * meanVarSecond.second);
}

template<typename Container> double spearmanCorrelation(const Container &first, const Container second) { 
	// return the Pearson correlation
	assert(first.size() == second.size());
	
	// calculate ranks
	auto ranksFirst = ranks(first);
	auto ranksSecond = ranks(second);

	//return Pearson correlation on ranks
	return pearsonCorrelation(ranksFirst, ranksSecond);
}

// variadic min
template<typename T> T &&min(T &&val) { return std::forward<T>(val); }

template<typename T0, typename T1, typename... Ts> auto min(T0 &&val1, T1 &&val2, Ts &&...vs) {
	return (val1 < val2) ? coretools::min(val1, std::forward<Ts>(vs)...)
	                     : coretools::min(val2, std::forward<Ts>(vs)...);
}

// variadic max
template<typename T> T &&max(T &&val) { return std::forward<T>(val); }

template<typename T0, typename T1, typename... Ts> auto max(T0 &&val1, T1 &&val2, Ts &&...vs) {
	return (val1 > val2) ? coretools::max(val1, std::forward<Ts>(vs)...)
	                     : coretools::max(val2, std::forward<Ts>(vs)...);
}

template<typename Container> typename Container::value_type medianOfUnsorted(Container input) {
	// returns the median of an unsorted container
	// runs in linear time complexity
	// return double (and not T) since median can be between two points if size is odd -> not necessarily same type
	size_t n = input.size();
	if (!(n & 1)) {
		// size is even
		// apply nth_element on N/2 index
		nth_element(input.begin(), input.begin() + n / 2, input.end());
		// apply nth_element on (N-1)/2 index
		nth_element(input.begin(), input.begin() + (n - 1) / 2, input.end());
		// median = average of values at index N/2 and (N-1)/2
		return (input[(n - 1) / 2] + input[n / 2]) / 2;
	} else {
		// size is odd
		// apply nth_element on N/2th index
		nth_element(input.begin(), input.begin() + n / 2, input.end());
		// median = value at index N/2
		return input[n / 2];
	}
}

template<typename T> double percentileOfUnsorted(T input, Probability Prob) {
	// returns the percentile of an unsorted container
	// Note: uses nearest-rank method (no interpolation)
	// -> definition: P-th percentile ( 0 < P ≤ 100 ) of a list of N ordered values (sorted from least to greatest) is
	//                the smallest value in the list such that no more than P percent of the data is strictly less than
	//                the value and at least P percent of the data is less than or equal to that value
	// runs in linear time complexity
	auto nth = input.begin() + std::ceil(Prob * input.size()) - 1;
	std::nth_element(input.begin(), nth, input.end());
	return *nth;
}


//------------------------------------------------
// Normalize
//------------------------------------------------

template<class ForwardIterator>
void normalize(ForwardIterator First, ForwardIterator Last,
               typename std::iterator_traits<ForwardIterator>::value_type tot) {
	const auto tot_inv = 1. / tot;
	std::transform(First, Last, First, [tot_inv](auto v) { return v * tot_inv; });
}

template<class ForwardIterator>
typename std::iterator_traits<ForwardIterator>::value_type normalize(ForwardIterator First, ForwardIterator Last) {
	const auto tot =
	    std::accumulate(First, Last, underlyingType_t<typename std::iterator_traits<ForwardIterator>::value_type>{});
	normalize(First, Last, tot);
	return tot;
}

template<typename Container> void normalize(Container &vs, double tot) {
	using Type     = typename Container::value_type;
	const auto inv = Type(1. / tot);
	for (auto &v : vs) v *= inv;
}

template<typename Container> double normalize(Container &vs) {
	double tot = 0.;
	for (const auto v : vs) tot += v;
	normalize(vs, tot);
	return tot;
}

template<typename ContainerResult, typename ContainerInput>
void fillFromNormalized(ContainerResult &res, const ContainerInput &in) {

	const auto tot = std::accumulate(in.cbegin(), in.cend(), 0.);
	res.clear();
	res.reserve(in.size());

	for (const auto i : in) res.emplace_back(i / tot);
}

template<typename Container> void normalizeLogSumExp(Container &LogVals) {
	// normalize to sum one using the log-sum-exp trick
	// values in LogVals are in log
	// we want to normalize the exp(LogVals) such that they sum to one
	// problem: exp(LogVals[i]) / sum(exp(LogVals)) might over- or underflow
	// solution: subtract maximum value and then take exp -> exact result, but no overflow!
	auto max = *std::max_element(LogVals.begin(), LogVals.end());
	double s = 0.0;
	for (auto v : LogVals) { s += exp(v - max); }
	s = max + log(s);

	for (auto &v : LogVals) { v = exp(v - s); }
}

//------------------------------------------------
// Standardize
//------------------------------------------------

template<typename Container> void standardizeZeroMean(Container &vs) {
	const double m = mean(vs);
	std::transform(vs.begin(), vs.end(), vs.begin(), [m](auto v) { return v - m; });
}

template<typename Container> void standardizeZeroMeanUnitVar(Container &vs) {
	const auto [mean, var] = meanVar(vs);
	const double sd        = sqrt(var);
	std::transform(vs.begin(), vs.end(), vs.begin(), [mean = mean, sd = sd](auto v) { return (v - mean) / sd; });
}

//------------------------------------------------
// Other
//------------------------------------------------


template<typename Container1, typename Container2> double euclideanDistance(const Container1 &x, const Container2 &y) {
	// calculate Euclidean distance: sqrt(sum_{d=1}^D (x_d-y_d)^2)
	assert(x.size() == y.size());
	return sqrt(std::inner_product(x.cbegin(), x.cend(), y.cbegin(), 0., std::plus<>{}, [](auto xi, auto yi) {
		auto d = xi - yi;
		return d * d;
	}));
}

template<class Container> std::pair<bool, size_t> findDuplicate(const Container &Vec) {
	for (size_t i = 0; i < Vec.size() - 1; i++) {
		for (size_t j = i + 1; j < Vec.size(); j++) {
			if (Vec[i] == Vec[j]) { return std::make_pair(true, j); }
		}
	}
	return std::make_pair(false, 0);
}

/*
 * Calculate the simple moving average of Container
 *
 * @tparam Container is the type of container (vector, array, ...)
 * @tparam Round is a boolean denoting if running mean should be rounded to the closest integer
 *
 * @param[in] Values is a Container where moving average should be calculated over
 * @param[in] Bandwidth is the number of elements on each side of the central point
 * 			  such that total window size is 2 * Bandwidth + 1
 *
 * @return A Container of same size of Values
 */
template<bool Round = false, class Container>
Container calculateMovingAverage(const Container &Values, size_t Bandwidth) {
	// if Values is empty or Bandwidth is zero (= no smoothing): just return Values
	if (Values.empty() || Bandwidth == 0) { return Values; }

	// make sure that Bandwidth <= Values.size()
	Bandwidth = std::min(Bandwidth, Values.size());

	// window size = Bandwidth to the left + Bandwidth to the right + central position
	size_t window = 2 * Bandwidth + 1;

	Container mvAvg = Values;
	double sum      = 0.0;
	size_t c        = 0;
	for (size_t i = 0; i < Values.size() + Bandwidth; ++i) {
		if (i < Values.size()) { // if i is within Values: add data point
			sum += (double)Values[i];
			++c;
		}
		if (i >= Bandwidth) {  // we are on the right of the first Bandwidth positions
			if (i >= window) { // we moved the window -> remove data point on the left that is outside of window now
				sum -= (double)Values[i - window];
				--c;
			}
			auto mean = sum / (double)c;
			if constexpr (Round) { mean = std::round(mean); }
			mvAvg[i - Bandwidth] = mean;
		}
	}
	return mvAvg;
}

/*
 * Calculate the moving median of Container
 *
 * @tparam Container is the type of container (vector, array, ...)
 * @tparam Round is a boolean denoting if running mean should be rounded to the closest integer
 *
 * @param[in] Values is a Container where moving median should be calculated over
 * @param[in] Bandwidth is the number of elements on each side of the central point
 * 			  such that total window size is 2 * Bandwidth + 1
 *
 * @return A Container of same size of Values
 */
template<bool Round = false, class Container>
Container calculateMovingMedian(const Container &Values, size_t Bandwidth) {
	// if Values is empty or Bandwidth is zero (= no smoothing): just return Values
	if (Values.empty() || Bandwidth == 0) { return Values; }

	// make sure that Bandwidth <= Values.size()
	Bandwidth = std::min(Bandwidth, Values.size());

	Container mvMed = Values;
	for (size_t i = 0; i < Values.size(); ++i) {
		// fill the window with data points
		size_t start = std::max((int)i - (int)Bandwidth, 0);
		size_t end   = std::min(i + Bandwidth + 1, Values.size());
		std::vector<typename Container::value_type> windowValues(Values.begin() + start, Values.begin() + end);
		auto median = medianOfUnsorted(windowValues);
		if constexpr (Round) { median = std::round(median); }
		mvMed[i] = median;
	}

	return mvMed;
}

/* *
 * Do binary search within a SORTED container and return index of match
 *
 * @tparam ForwardIterator is the type of iterator
 * @tparam Type is the type of the values that are searched for, e.g. std::string/double/int etc.
 *
 * @param[in] First is an iterator pointing to the first element in a vector/array -> NOTE: must be sorted!!
 * @param[in] Last is an iterator pointing to the last element in a vector/array -> NOTE: must be sorted!!
 * @param[in] Value is the value to be searched inside [First, Last)
 *
 * @return index of match
 *         if there are duplicate elements: index of the first match is returned
 *         if there is no match: throw runtime error
 */

template<class ForwardIterator, class Type>
size_t binarySearch_getIndex(ForwardIterator First, ForwardIterator Last, const Type &Value) {
	// do binary search and return index of matching element
	// throws runtime error if there is no match
	// note: if there are duplicates, the index of the first matching element is returned
	auto it = std::lower_bound(First, Last, Value);
	if (it == Last || *it != Value) {
		DEVERROR("In function 'binarySearch_getIndex': Failed to find Value '", Value, "'!");
	} else {
		size_t index = std::distance(First, it);
		return index;
	}
};

/* *
 * Calculate linear index from multi-dimensional coordinates
 * Use a row-major layout (row-by-row are contiguous in memory)
 *
 * @tparam Type is the type of the indices (typically int, size_t or some unsigned int)
 *
 * @param[in] Coord is a vector of coordinates that you want to linearize
 * @param[in] Dimensions is a vector of dimensions of your multi-dimensional array
 *
 * @return linearized index
 */

template<typename ContainerTypeCoord, typename ContainerTypeDimensions,
         typename Type = typename ContainerTypeCoord::value_type>
Type getLinearIndex(const ContainerTypeCoord &Coord, const ContainerTypeDimensions &Dimensions) {
	// check if size matches and if coordinates are within dimensions
	assert(Coord.size() == Dimensions.size());
	assert(([Dimensions = std::as_const(Dimensions), &Coord = std::as_const(Coord)]() constexpr {
		for (size_t i = 0; i < Dimensions.size(); i++) {
			if (Coord[i] >= Dimensions[i]) { return false; }
		}
		return true;
	})());

	// write out 1, 2 and 3 dimensions for speed
	switch (Dimensions.size()) {
	case 1: return Coord[0];
	case 2: return Coord[0] * Dimensions[1] + Coord[1];
	case 3: return (Coord[0] * Dimensions[1] + Coord[1]) * Dimensions[2] + Coord[2];
	default:
		Type index = Coord.back();
		Type prod  = 1;
		for (size_t i = Dimensions.size() - 1; i > 0; i--) {
			prod *= Dimensions[i];
			index += prod * Coord[i - 1];
		}
		return index;
	}
}

/* *
 * Calculate linear index from multi-dimensional coordinates: array version
 * Use a row-major layout (row-by-row are contiguous in memory)
 *
 * @tparam Type is the type of the indices (typically int, size_t or some unsigned int)
 *
 * @param[in] Coord is a std::array of coordinates that you want to linearize
 * @param[in] Dimensions is a std::array of dimensions of your multi-dimensional array
 *
 * @return linearized index
 *
 * Note: did not template getLinearIndex(std::vector, std::vector) because
 * 1) compiler does not accept initializer list (e.g. getLinearIndex({1}, {5})) anymore, because it can not determine
 * the type 2) for arrays, the if-statements can be written with constexpr -> faster
 */

template<typename Type, size_t NumDim>
constexpr Type getLinearIndex(const std::array<Type, NumDim> &Coord, const std::array<Type, NumDim> &Dimensions) {
	// check if size matches and if coordinates are within dimensions
	assert(([Dimensions = std::as_const(Dimensions), &Coord = std::as_const(Coord)]() constexpr {
		for (size_t i = 0; i < Dimensions.size(); i++) {
			if (Coord[i] >= Dimensions[i]) { return false; }
		}
		return true;
	})());

	// write out 1, 2 and 3 dimensions for speed
	if constexpr (NumDim == 1) {
		return Coord[0];
	} else if constexpr (NumDim == 2) {
		return Coord[0] * Dimensions[1] + Coord[1];
	} else if constexpr (NumDim == 3) {
		return (Coord[0] * Dimensions[1] + Coord[1]) * Dimensions[2] + Coord[2];
	} else {
		Type index = Coord.back();
		Type prod  = 1;
		for (size_t i = NumDim - 1; i > 0; i--) {
			prod *= Dimensions[i];
			index += prod * Coord[i - 1];
		}
		return index;
	}
}

/* *
 * Calculate linear index from multi-dimensional coordinates
 * Use a column-major layout (column-by-column are contiguous in memory)
 *
 * @tparam Type is the type of the indices (typically int, size_t or some unsigned int)
 *
 * @param[in] Coord is a vector of coordinates that you want to linearize
 * @param[in] Dimensions is a vector of dimensions of your multi-dimensional array
 *
 * @return linearized index
 */

template<typename Type>
Type getLinearIndexColMajor(const std::vector<Type> &Coord, const std::vector<Type> &Dimensions) {
	// check if size matches and if coordinates are within dimensions
	assert(Coord.size() == Dimensions.size());
	assert(([Dimensions = std::as_const(Dimensions), &Coord = std::as_const(Coord)]() constexpr {
		for (size_t i = 0; i < Dimensions.size(); i++) {
			if (Coord[i] >= Dimensions[i]) { return false; }
		}
		return true;
	})());

	// write out 1, 2 and 3 dimensions for speed
	switch (Dimensions.size()) {
	case 1: return Coord[0];
	case 2: return Coord[1] * Dimensions[0] + Coord[0];
	case 3: return (Coord[2] * Dimensions[1] + Coord[1]) * Dimensions[0] + Coord[0];
	default:
		Type index = Coord[0];
		Type prod  = 1;
		for (size_t i = 1; i < Dimensions.size(); i++) {
			prod *= Dimensions[i - 1];
			index += prod * Coord[i];
		}
		return index;
	}
}

/* *
 * Calculate multi-dimensional coordinates based on a linear index
 * Inverse function of getIndex()
 * Use a row-major layout
 *
 * @tparam Type is the type of the indices (typically int, size_t or some unsigned int)
 *
 * @param[in] LinearIndex is the linearized index
 * @param[in] Dimensions is a vector of dimensions of your multi-dimensional array
 *
 * @return a vector of multi-dimensional coordinates (same size as Dimensions)
 *
 * @assert that the linear index is within the dimensions specified
 */

template<typename Type> std::vector<Type> getSubscripts(Type LinearIndex, const std::vector<Type> &Dimensions) {
	// algorithm from
	// https://stackoverflow.com/questions/46782444/how-to-convert-a-linear-index-to-subscripts-with-support-for-negative-strides

	// check if index is within dimensions
	assert(LinearIndex < containerProduct(Dimensions));

	// write out 1, 2 and 3 dimensions for speed
	switch (Dimensions.size()) {
	case 1: return std::vector<Type>{LinearIndex};
	case 2: {
		const auto coord2 = LinearIndex % Dimensions[1];
		const auto coord1 = (LinearIndex - coord2) / Dimensions[1];
		return std::vector<Type>{coord1, coord2};
	}
	case 3: {
		const auto coord3 = LinearIndex % Dimensions[2];
		LinearIndex -= coord3;
		LinearIndex /= Dimensions[2];
		const auto coord2 = LinearIndex % Dimensions[1];
		const auto coord1 = (LinearIndex - coord2) / Dimensions[1];
		return std::vector<Type>{coord1, coord2, coord3};
	}
	default:
		std::vector<Type> coords(Dimensions.size());

		// go over all dimensions, start at most inner one
		for (int i = Dimensions.size() - 1; i >= 0; i--) {
			// get the index in the current dimension
			const auto indexInCurDimension = LinearIndex % Dimensions[i];
			coords[i]                      = indexInCurDimension;
			// update linear index
			LinearIndex -= indexInCurDimension;
			LinearIndex /= Dimensions[i];
		}
		return coords;
	}
}

/* *
 * Calculate multi-dimensional coordinates based on a linear index
 * Inverse function of getIndex()
 * Use a row-major layout
 *
 * @tparam Type is the type of the indices (typically int, size_t or some unsigned int)
 *
 * @param[in] LinearIndex is the linearized index
 * @param[in] Dimensions is a vector of dimensions of your multi-dimensional array
 *
 * @return a vector of multi-dimensional coordinates (same size as Dimensions)
 *
 * @assert that the linear index is within the dimensions specified
 *
 * Note: did not template getSubscripts(Type, std::vector) because
 * 1) compiler does not accept initializer list (e.g. getSubscripts(1, {5})) anymore, because it can not determine the
 * type 2) for arrays, the if-statements can be written with constexpr -> faster
 */

template<typename Type, size_t NumDim>
std::array<Type, NumDim> getSubscriptsAsArray(Type LinearIndex, const std::array<Type, NumDim> &Dimensions) {
	using RetType = std::array<Type, NumDim>;
	// algorithm from
	// https://stackoverflow.com/questions/46782444/how-to-convert-a-linear-index-to-subscripts-with-support-for-negative-strides

	// check if index is within dimensions
	assert(LinearIndex < containerProduct(Dimensions));

	// write out 1, 2 and 3 dimensions for speed
	if constexpr (NumDim == 1) {
		return RetType{LinearIndex};
	} else if constexpr (NumDim == 2) {
		const auto coord2 = LinearIndex % Dimensions[1];
		const auto coord1 = (LinearIndex - coord2) / Dimensions[1];
		return RetType{coord1, coord2};
	} else if constexpr (NumDim == 3) {
		const auto coord3 = LinearIndex % Dimensions[2];
		LinearIndex -= coord3;
		LinearIndex /= Dimensions[2];
		const auto coord2 = LinearIndex % Dimensions[1];
		const auto coord1 = (LinearIndex - coord2) / Dimensions[1];
		return RetType{coord1, coord2, coord3};
	} else {
		RetType coords;

		// go over all dimensions, start at most inner one
		for (int i = NumDim - 1; i >= 0; i--) {
			// get the index in the current dimension
			const auto indexInCurDimension = LinearIndex % Dimensions[i];
			coords[i]                      = indexInCurDimension;
			// update linear index
			LinearIndex -= indexInCurDimension;
			LinearIndex /= Dimensions[i];
		}
		return coords;
	}
}

/* *
 * Calculate multi-dimensional coordinates based on a linear index
 * Inverse function of getIndex()
 * Use a column-major layout
 *
 * @tparam Type is the type of the indices (typically int, size_t or some unsigned int)
 *
 * @param[in] LinearIndex is the linearized index
 * @param[in] Dimensions is a vector of dimensions of your multi-dimensional array
 *
 * @return a vector of multi-dimensional coordinates (same size as Dimensions)
 *
 * @assert that the linear index is within the dimensions specified
 */

template<typename Type> std::vector<Type> getSubscriptsColMajor(Type LinearIndex, const std::vector<Type> &Dimensions) {
	// check if index is within dimensions
	assert(LinearIndex < containerProduct(Dimensions));

	// write out 1, 2 and 3 dimensions for speed
	switch (Dimensions.size()) {
	case 1: return std::vector<Type>{LinearIndex};
	case 2: {
		const auto coord1 = LinearIndex % Dimensions[0];
		const auto coord2 = (LinearIndex - coord1) / Dimensions[0];
		return std::vector<Type>{coord1, coord2};
	}
	case 3: {
		const auto coord1 = LinearIndex % Dimensions[0];
		LinearIndex -= coord1;
		LinearIndex /= Dimensions[0];
		const auto coord2 = LinearIndex % Dimensions[1];
		const auto coord3 = (LinearIndex - coord2) / Dimensions[1];
		return std::vector<Type>{coord1, coord2, coord3};
	}
	default:
		std::vector<Type> coords(Dimensions.size());

		// go over all dimensions, start at most outer one
		for (size_t i = 0; i < Dimensions.size(); i++) {
			// get the index in the current dimension
			const auto indexInCurDimension = LinearIndex % Dimensions[i];
			coords[i]                      = indexInCurDimension;
			// update linear index
			LinearIndex -= indexInCurDimension;
			LinearIndex /= Dimensions[i];
		}
		return coords;
	}
}

/* *
 * Calculate increments per dimenstion for multi-dimensional coordinates
 * Use a row-major layout (row-by-row are contiguous in memory)
 *
 * @tparam Type is the type of the indices (typically int, size_t or some
 * unsigned int)
 *
 * @param[in] Dimensions is a vector of dimensions of your multi-dimensional
 * array
 *
 * @return a vector of multi-dimensional increments
 */
template<typename Type> std::vector<Type> getIncrements(const std::vector<Type> &Dimensions) {
	// write out 1, 2 and 3 dimensions for speed
	switch (Dimensions.size()) {
	case 1: return std::vector<Type>{1};
	case 2: return std::vector<Type>{Dimensions[1], 1};
	case 3: return std::vector<Type>{Dimensions[2] * Dimensions[1], Dimensions[2], 1};
	default:
		std::vector<Type> incrs(Dimensions.size());
		incrs.back() = 1;
		for (size_t i = Dimensions.size() - 1; i > 0; i--) { incrs[i - 1] = incrs[i] * Dimensions[i]; }
		return incrs;
	}
};

/* *
 * Calculate increments per dimenstion for multi-dimensional coordinates
 * Use a row-major layout (row-by-row are contiguous in memory)
 *
 * @tparam Type is the type of the indices (typically int, size_t or some
 * unsigned int)
 *
 * @param[in] Dimensions is a vector of dimensions of your multi-dimensional
 * array
 *
 * @return a vector of multi-dimensional increments
 */
template<typename Type> std::vector<Type> getIncrementsColMajor(const std::vector<Type> &Dimensions) {
	// write out 1, 2 and 3 dimensions for speed
	switch (Dimensions.size()) {
	case 1: return std::vector<Type>{1};
	case 2: return std::vector<Type>{1, Dimensions[0]};
	case 3: return std::vector<Type>{1, Dimensions[0], Dimensions[0] * Dimensions[1]};
	default:
		std::vector<Type> incrs(Dimensions.size());
		incrs.front() = 1;
		for (size_t i = 0; i < Dimensions.size() - 1; i++) { incrs[i + 1] = incrs[i] * Dimensions[i]; }
		return incrs;
	}
}
}; // namespace coretools

#endif // BANGOLIN_ALGORITHMS_H
