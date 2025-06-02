#ifndef TRandomGenerator_H_
#define TRandomGenerator_H_

#include <random>

#include "coretools/Containers/TView.h"
#include "coretools/Types/commonWeakTypes.h"
#include "coretools/Types/probability.h"
#include "coretools/traits.h"

#ifdef _OPENMP
#include "omp.h"
#endif

namespace coretools {

/* See
https://github.com/CppCon/CppCon2016/blob/master/Presentations/I%20Just%20Wanted%20a%20Random%20Integer!/I%20Just%20Wanted%20a%20Random%20Integer!%20-%20Cheinan%20Marks%20-%20CppCon%202016.pdf
*/

class TRandomGenerator {
private:
	std::mt19937 _integerGen;
	unsigned long _seed; // This should be either a const variable or a function call!
public:
	TRandomGenerator(long addToSeed = 0, bool seedIsFixed = false) { setSeed(addToSeed, seedIsFixed); }

	void setSeed(long addToSeed, bool seedIsFixed = false);

	unsigned long getSeed() const noexcept { return _seed; }

	// uniform on interval [0,1)
	double getRand() { return std::uniform_real_distribution<>()(_integerGen); }

	// uniform on interval [min,max)
	template<typename T> T getRand(const T min, const T max) {
		static_assert(std::is_arithmetic_v<T>);
		if constexpr (std::is_integral_v<T>) {
			return std::uniform_int_distribution<T>(min, max - 1)(_integerGen);
		} else {
			return std::uniform_real_distribution<>(min, max)(_integerGen);
		}
	}

	// uniform on interval [min,max]
	template<typename T> T getRandIntegerClosedMinMax(const T min, const T max) {
		static_assert(std::is_arithmetic_v<T> && std::is_integral_v<T>);
		// useful if e.g. T=uint8_t and we want random values from entire range, i.e. min=0, max = 256
		// -> can not use getRand() as max+1 would overflow
		return std::uniform_int_distribution<T>(min, max)(_integerGen);
	}

	// Legacy
	double getRandNotOne() { return getRand(); }

	template<typename T> void fillRand(T &vec) {
		std::generate(vec.begin(), vec.end(), [this]() { return getRand(); });
	}

	// pick one templates
	template<typename T> T sample(T numElements) {
		assert(numElements > 0);
		return (numElements > 1 ? std::uniform_int_distribution<T>(0, numElements - 1)(_integerGen) : 0);
	}

	bool pickOneOfTwo(Probability prob = P(0.5)) { return getRand() < prob; }

	template<typename T, typename N> [[deprecated("use TView")]] N pickOne(N numElements, T *probsCumulative) {
		assert(numElements > 0);
		if (numElements == 1) return 0;

		double r = getRandNotOne();
		// We can use binary search as probsCumulative is sorted
		return std::distance(probsCumulative, std::upper_bound(probsCumulative, probsCumulative + numElements, r));
	}

	template<class Container, std::enable_if_t<isIterable_v<Container>, bool> = true>
	indexType_t<Container> pickOne(const Container &probsCumulative) {
		using Index = indexType_t<Container>;
		if constexpr (isResizable_v<Container> || isView_v<Container>) {
			assert(probsCumulative.size() > 0);
			const double r = getRand();
			return static_cast<Index>(std::distance(
			    probsCumulative.begin(), std::upper_bound(probsCumulative.begin(), probsCumulative.end(), r)));
		} else {
			// compiler can unloop
			constexpr size_t N = Container().size();
			if constexpr (N == 1) {
				return Index{};
			} else if constexpr (N == 2) {
				const double r = getRand();
				return static_cast<Index>(r > probsCumulative.front());
			} else if constexpr (N <= 20) {
				const double r = getRand();
				Index i{};
				while (r > probsCumulative[i]) ++i;
				return i;
			} else {
				const double r = getRand();
				return static_cast<Index>(std::distance(
				    probsCumulative.begin(), std::upper_bound(probsCumulative.begin(), probsCumulative.end(), r)));
			}
		}
	}

	template<typename Container> uint32_t pickOneRawProbabilities(const Container &probs) {
		assert(probs.size() > 0);
		if (probs.size() == 1) return 0;

		double r    = getRandNotOne();
		size_t i    = 0;
		double prob = probs[0];
		while (r > prob && i < probs.size() - 1) {
			++i;
			prob += probs[i];
		}
		return i;
	}

	template<typename Container, typename IndexType, size_t NumLevels = index(IndexType::max)>
	IndexType sampleIndexOfMaxima(const Container &Data) {
		std::array<IndexType, NumLevels> indices;
		indices.front() = IndexType{};
		size_t nI       = 1;
		for (auto i = IndexType(1); i < IndexType(Data.size()); ++i) {
			const auto max = Data[indices.front()];
			if (Data[i] > max) { // set new maximum
				indices.front() = i;
				nI              = 1;
			}
			if (Data[i] == max) { // add additional index
				indices[nI] = i;
				++nI;
			}
		}
		return indices[sample(nI)];
	}

	char getRandomAlphaNumericCharacter() {
		constexpr char ALPHANUM[] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
		return ALPHANUM[sample(sizeof(ALPHANUM) - 1)];
	}

	std::string getRandomAlphaNumericString(size_t length) {
		std::string rs;
		std::generate_n(std::back_inserter(rs), length, [this]() { return getRandomAlphaNumericCharacter(); });
		return rs;
	}

	template<typename Container> void shuffle(Container &vec) { std::shuffle(vec.begin(), vec.end(), _integerGen); }

	void fillRandomOrderOfIndexes(std::vector<size_t> &vec) {
		// create a vector with a random order of indeces from 0 to length - 1
		vec[0] = 0;
		for (size_t i = 1; i < vec.size(); ++i) {
			size_t j = getRand<size_t>(0, i);
			vec[i]   = vec[j];
			vec[j]   = i;
		}
	}

	std::vector<size_t> randomOrderOfIndexes(const size_t length) {
		// create a vector with a random order of indeces from 0 to length - 1
		std::vector<size_t> vec(length);
		fillRandomOrderOfIndexes(vec);
		return vec;
	}

	// normal
	double getNormalRandom(double dMean, StrictlyPositive dStdDev) {
		return std::normal_distribution<double>(dMean, dStdDev)(_integerGen);
	}

	double getLogNormalRandom(double dMean, StrictlyPositive dStdDev) {
		return std::exp(getNormalRandom(dMean, dStdDev));
	}

	// bernoulli
	bool getBernoulliRand(Probability pp) { return std::bernoulli_distribution(pp)(_integerGen); }

	// binomial
	uint32_t getBinomialRand(Probability pp, uint32_t n) {
		return std::binomial_distribution<uint32_t>(n, pp)(_integerGen);
	}
	// negative binomial
	uint32_t getNegativeBinomialRand(Probability pp, uint32_t n) {
		return std::negative_binomial_distribution<uint32_t>(n, pp)(_integerGen);
	}

	// multinomial
	template<typename T>
	void fillMultinomialRandom(const uint32_t size, TConstView<double> probsCumulative, std::vector<T> &counts) {
		counts.assign(probsCumulative.size(), 0);

		// sample
		for (uint32_t i = 0; i < size; ++i) { ++counts[pickOne(probsCumulative)]; }
	}

	template<typename ContainerIn, typename ContainerOut>
	void fillMultinomialRandomRawProbabilities(uint32_t size, const ContainerIn &probs, ContainerOut &counts) {
		// get cumulative probabilities
		std::vector<double> probsCumulative(probs.size());
		std::partial_sum(probs.begin(), probs.end(), probsCumulative.begin());
		fillMultinomialRandom(size, probsCumulative, counts);
	}

	// gamma
	// ! the b-value of std::gamma_distribution is equal to k (1/beta) as defined on
	// https://en.wikipedia.org/wiki/Gamma_distribution
	double getGammaRand(StrictlyPositive a, StrictlyPositive b = 1) {
		return std::gamma_distribution<double>(a, 1. / b)(_integerGen);
	}

	// chisquared
	double getChisqRand(StrictlyPositive k) { return std::chi_squared_distribution<double>(k)(_integerGen); }

	// beta
	Probability getBetaRandom(StrictlyPositive alpha, StrictlyPositive beta, double a, double b);
	Probability getBetaRandom(StrictlyPositive alpha, StrictlyPositive beta);

	// Betabinomial
	double getBetaBinomialRandom(uint32_t size, StrictlyPositive alpha, StrictlyPositive beta) {
		return getBinomialRand(getBetaRandom(alpha, beta), size);
	}

	// Dirichlet
	void fillDirichletRandom(size_t K, StrictlyPositive *alpha, ZeroOneOpen *res);

	template<typename ContainerIn, typename ContainerOut>
	void fillDirichletRandom(const ContainerIn &alpha, ContainerOut &res) {
		assert(alpha.size() > 0);

		// can not directly fill into res, because type is usually ZeroOneOpen -> violated since normalization happens
		// only afterwards
		std::vector<double> tmp(alpha.size());
		double sum = 0.0;
		for (size_t k = 0; k < alpha.size(); ++k) {
			tmp[k] = getGammaRand(alpha[k]);
			sum += tmp[k];
		}

		res.resize(alpha.size());
		for (size_t k = 0; k < alpha.size(); ++k) { 
			double val = tmp[k] / sum; 
			if(val < ZeroOneOpen::min()){
				val = ZeroOneOpen::min();
			}
			res[k] = tmp[k] / sum;
		}
	}

	// Polya (Dirichlet-Multinomial)
	template<typename ContainerIn, typename ContainerOut>
	void fillPolyaRandom(uint32_t size, const ContainerIn &alpha, ContainerOut &counts) {
		// sample p from dirichlet
		static std::vector<double> dir;
		fillDirichletRandom(alpha, dir);

		// sample from multinomial
		fillMultinomialRandomRawProbabilities(size, dir, counts);
	}

	unsigned int getPoissonRandom(StrictlyPositive lambda) {
		return std::poisson_distribution<unsigned int>(lambda)(_integerGen);
	}

	// exponential
	double getExponentialRandom(StrictlyPositive lambda) {
		return std::exponential_distribution<double>(lambda)(_integerGen);
	}

	// Is this the same?
	// https://en.cppreference.com/w/cpp/numeric/random/extreme_value_distribution
	double getExponentialRandomTruncated(StrictlyPositive lambda, double lowerBound, double upperBound);

	// generalized Pareto
	double getGeneralizedParetoRand(double locationMu, StrictlyPositive scaleSigma, double shapeXi);

	// geometric
	unsigned int getGeometricRandom(Probability p) { return std::geometric_distribution<unsigned int>(p)(_integerGen); }
};

namespace instances {
inline TRandomGenerator &randomGenerator() {
	static thread_local TRandomGenerator rand;
	return rand;
}
} // namespace instances

} // namespace coretools

#endif /* TRandomGenerator_H_ */
