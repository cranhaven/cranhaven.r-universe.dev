/*
 * TCategoricalDistribution.h
 *
 *  Created on: Jul 11, 2022
 *      Author: phaentu
 */

#ifndef CORE_MATH_TCATEGORICALDISTRIBUTION_H_
#define CORE_MATH_TCATEGORICALDISTRIBUTION_H_

#include <memory>
#include <vector>

#include "coretools/Distributions/Distributions.h"

#include "coretools/Main/TRandomPicker.h"
#include "coretools/Math/TNumericRange.h"
#include "coretools/Math/TValueStorage.h"
#include "coretools/Strings/convertString.h"
#include "coretools/Strings/fillContainer.h"
#include "coretools/Strings/stringProperties.h"
#include "coretools/Types/probability.h"
#include "coretools/algorithms.h"
#include "coretools/traits.h"

namespace coretools::probdist {

//-------------------------------------
// TCategoricalDistribution
//-------------------------------------
template<typename valueType> class TCategoricalDistribution {
private:
	std::unique_ptr<TValueStorage_base<valueType>> _values;
	std::vector<Probability> _frequencies;
	std::vector<LogProbability> _logFrequencies;
	TRandomPicker _picker;
	double _mean{};
	static constexpr std::string_view _distNameFixed       = "fixed";
	static constexpr std::string_view _distNameUnif        = "unif";
	static constexpr std::string_view _distNameCategorical = "categorical";

	void _precalculateTmpVars() {
		// pre-calculate log probabilities and cumulative probabilities
		_logFrequencies.resize(_frequencies.size());
		for (size_t i = 0; i < _frequencies.size(); ++i) {
			_logFrequencies[i] = logP(_frequencies[i]);
		}
		_picker.init(_frequencies);

		// pre-calculate mean
		for (size_t i = 0; i < _frequencies.size(); ++i) {
			_mean += (double)underlying(_values->index2value(i)) * _frequencies[i];
		}
	}

	void _initializeEqualFrequencies() {
		// set equal frequencies
		_frequencies.resize(_values->size());
		Probability equal(1.0 / (double)_values->size());
		std::fill(_frequencies.begin(), _frequencies.end(), equal);
		_precalculateTmpVars();
	}

	template<typename... Args>
	static std::string _formatError(std::string_view orig, std::string_view format, std::string_view error,
									Args... errors) {
		auto errorTot = str::toString(error, errors...);
		return str::toString("Unable to understand function string '", orig, "'. ", errorTot, " Use format '", format, "'.");
	}

	template<typename DistrType>
	void _initializeDistribution(std::string_view Param, valueType Min, valueType Max, std::string_view OriginalString,
								 std::string_view Format) {
		try {
			DistrType dist(Param);
			set(dist, Min, Max);
		} catch (err::TDevError &err) { UERROR(_formatError(OriginalString, Format, err.error())); }
	}

	TNumericRange<valueType> _extractRange(std::string_view Orig, std::string_view Range, std::string_view Format) {
		try {
			TNumericRange<valueType> range(Range);
			return range;
		} catch (std::string &error) { UERROR(_formatError(Orig, Format, error)); }
	}

	void _ensureNoRangeIsGiven(std::string_view orig, std::string_view Func, std::string_view format,
							   std::string_view post) {
		if (!post.empty()) {
			if (str::stringContainsAny(post, "[]")) {
				UERROR(_formatError(orig, format, "Function '", Func, "' does not support a range."));
			} else {
				UERROR(_formatError(orig, format, "Unknown part '", post, "'."));
			}
		}
	}

	void _setFixed(std::string_view orig, std::string_view param, std::string_view post) {
		const std::string format = str::toString(_distNameFixed, "(value)");

		if (str::stringContains(param, ',')) {
			UERROR(_formatError(orig, format, "Function '", _distNameFixed, "' only supports a single parameter."));
		}

		_ensureNoRangeIsGiven(orig, _distNameFixed, format, post);

		// seet single value
		set(str::fromString<valueType>(param));
	}

	void _setUnif(std::string_view orig, std::string_view param, std::string_view post) {
		const std::string format = str::toString(_distNameUnif, "()[min,max]");

		if (!param.empty()) {
			UERROR(_formatError(orig, format, "Function '", _distNameUnif, "' can not have parameters."));
		}

		TNumericRange<valueType> range = _extractRange(orig, post, format);

		auto min = underlying(range.min());
		if (!range.minIncluded()) { ++min; }
		auto max = underlying(range.max());
		if (!range.maxIncluded()) { --min; }
		_values.reset(new TValueStorageRange<valueType>(valueType(min), valueType(max)));

		// set equal frequencies
		_initializeEqualFrequencies();
	}

	void _setCategorical(std::string_view orig, std::string_view param, std::string_view post) {
		const std::string format = str::toString(_distNameCategorical, "(val1:freq1,val2:freq2,...)' or '",
												 _distNameCategorical, "(val1,val2,...)");
		_ensureNoRangeIsGiven(orig, _distNameCategorical, format, post);

		// split string by ','
		std::vector<std::string> tmp;
		coretools::str::fillContainerFromString(param, tmp, ',');
		if (tmp.empty()) {
			UERROR("Failed to parse categorical distribution '", orig, "': No parameter values provided!");
		}

		// now parse each bin
		std::vector<valueType> vals(tmp.size());
		std::vector<Positive> freqs(tmp.size());

		bool hasFreq = str::stringContains(tmp[0], ':');

		for (size_t i = 0; i < tmp.size(); ++i) {
			const auto pos = tmp[i].find(':');
			if ((hasFreq && pos == std::string::npos) || (!hasFreq && pos != std::string::npos)) {
				UERROR("Failed to parse categorical distribution '", orig,
					   "': Frequencies provided for some but not all values!");
			}
			str::fromString<true>(tmp[i].substr(0, pos), vals[i]);

			if (hasFreq) { str::fromString(tmp[i].substr(pos + 1), freqs[i]); }
		}

		// now initialize
		if (hasFreq) {
			set(vals, freqs);
		} else {
			set(vals);
		}
	}

public:
	TCategoricalDistribution() = default;
	TCategoricalDistribution(valueType Value) { set(Value); }

	TCategoricalDistribution(std::vector<valueType> &Values) { set(Values); }

	template<typename freqType>
	TCategoricalDistribution(std::vector<valueType> &Values, std::vector<freqType> &Frequencies) {
		set(Values, Frequencies);
	}

	template<typename distrType> TCategoricalDistribution(const distrType &Distribution, valueType Min, valueType Max) {
		set(Distribution, Min, Max);
	}

	TCategoricalDistribution(std::string_view DistributionString) { set(DistributionString); }

	~TCategoricalDistribution() = default;

	// set functions
	//-------------
	// from a single value
	void set(valueType Value) {
		static_assert(std::is_integral_v<underlyingType_t<valueType>>, "Integral type required.");
		// Create value storage and initialize
		_values.reset(new TValueStorageFixed<valueType>(Value));
		_initializeEqualFrequencies();
	}

	// from a vector of values with equal frequencies
	void set(std::vector<valueType> &Values) {
		static_assert(std::is_integral_v<underlyingType_t<valueType>>, "Integral type required.");

		if (Values.size() == 0) {
			DEVERROR("Cannot initialize Categorical Distributions without values!");
		} else if (Values.size() == 1) {
			set(Values.front());
		} else {
			// make sure values is sorted
			sort(Values.begin(), Values.end());

			// check if _values does not contain duplicates
			if (std::adjacent_find(Values.cbegin(), Values.cend()) != Values.cend()) {
				DEVERROR("Values contains duplicate entries!");
			}

			_values.reset(new TValueStorageDispersed<valueType>(Values));

			// set equal frequencies
			_initializeEqualFrequencies();
		}
	}

	// from dispersed values and frequencies
	template<typename freqType>
	void set(const std::vector<valueType> &Values, const std::vector<freqType> &Frequencies) {
		static_assert(std::is_integral_v<underlyingType_t<valueType>>, "Integral type required.");

		// check if sizes match
		if (Values.size() != Frequencies.size()) {
			DEVERROR("Number of probabilities does not match number of values!");
		}

		// initialize
		if (Values.size() == 0) { DEVERROR("Cannot initialize Categorical Distributions without values!"); }
		if (Values.size() == 1) {
			set(Values.front());
		} else {
			// normalize probabilities
			std::vector<Probability> normedFreq;
			fillFromNormalized(normedFreq, Frequencies);

			// make sure vectors are sorted
			auto ranks                          = rankSort(Values);
			std::vector<valueType> sortedValues = sortContainerByRank(Values, ranks);
			sortContainerByRank(normedFreq, ranks, _frequencies);

			// check if _values does not contain duplicates
			if (std::adjacent_find(sortedValues.cbegin(), sortedValues.cend()) != sortedValues.cend()) {
				DEVERROR("Values contain duplicate entries!");
			}

			// Create value storage and
			_values.reset(new TValueStorageDispersed<valueType>(sortedValues));
			_precalculateTmpVars();
		}
	}

	// from distribution
	template<typename DistrType> void set(const DistrType &Distribution, valueType Min, valueType Max) {
		static_assert(std::is_integral_v<underlyingType_t<valueType>>, "Integral type required.");

		// initialize
		if (Min == Max) {
			set(Min);
		} else {
			// initialize values
			_values.reset(new TValueStorageRange<valueType>(Min, Max));

			// calculate per value probabilities
			if (Distribution.isDiscrete()) {
				// discrete distribution: use actual probabilities
				_frequencies.resize(_values->size());
				for (size_t i = 0; i < _frequencies.size(); ++i) {
					_frequencies[i] = P(Distribution.density(underlying(_values->min()) + i));
				}
				normalize(_frequencies);
			} else {
				// Continuous distribution: do histogram using the cumulative density function
				std::vector<Probability> densities(_values->size());
				std::pair supportedRange = Distribution.support();
				auto binMin              = underlying(_values->min()) - 0.5;
				double nextDens          = 0;
				if (binMin < supportedRange.first)
					nextDens = Distribution.cumulativeDensity(supportedRange.first);
				else
					nextDens = Distribution.cumulativeDensity(binMin);
				for (size_t i = 0; i < densities.size(); ++i) {
					const auto prevDens = nextDens;
					auto binMax         = underlying(_values->min()) + i + 0.5;
					if (binMax > supportedRange.second)
						nextDens = Distribution.cumulativeDensity(supportedRange.second);
					else
						nextDens = Distribution.cumulativeDensity(binMax);
					densities[i] = P(nextDens - prevDens);
				}
				fillFromNormalized(_frequencies, densities);
			}

			// fill log and cumulative distribution
			_precalculateTmpVars();
		}
	}

	void set(std::string_view str) {
		auto orig = str::strip(str);
		std::string distributionString{orig};

		// extract function name
		if (!str::stringContains(distributionString, '(')) {
			UERROR("String '", distributionString,
				   "' is not a function string: missing '('! Use format 'function(parameters)[min,max]' or "
				   "'function(parameters)'.");
		}
		if (!str::stringContains(distributionString, ')')) {
			UERROR("String '", distributionString,
				   "' is not a function string: missing ')'! Use format 'function(parameters)[min,max]' or "
				   "'function(parameters)'.");
		}
		std::string func = str::extractBefore(distributionString, '(');

		// extract parameters
		distributionString.erase(0, 1);
		std::string param = str::extractBefore(distributionString, ')');
		distributionString.erase(0, 1);

		// switch by function
		if (func == _distNameFixed) {
			_setFixed(orig, param, distributionString);
		} else if (func == _distNameUnif) {
			_setUnif(orig, param, distributionString);
		} else if (func == _distNameCategorical) {
			_setCategorical(orig, param, distributionString);
		} else {
			std::string format = func + "(parameters)[min,max]";

			// check if there are parameters
			if (param.empty()) {
				UERROR(_formatError(orig,
									std::string{func} + "(parameters)[min,max]' or '" + func +
										"(parameters)' for distributions without ranges.",
									"No parameters provided!"));
			}

			// extract range
			if (distributionString.front() != '[' || distributionString.back() != ']') {
				UERROR(_formatError(orig, format, "Range missing!"));
			}
			distributionString.erase(0, 1);                               // cut away '['
			distributionString.erase(distributionString.length() - 1, 1); // cut away ']'
			valueType min, max;
			try {
				str::convertString(distributionString, "Unable to parse range!", min, max);
			} catch (std::string &error) { UERROR(_formatError(orig, format, error)); }

			// create distribution
			// void _initializeDistribution<distrType>(std::string_view  Param, valueType Min, valueType Max,
			// std::string_view  OriginalString, std::string & Format){
			if (func == TExponentialDistr::name)
				_initializeDistribution<TExponentialDistr>(param, min, max, orig, format);
			else if (func == TNormalDistr::name)
				_initializeDistribution<TNormalDistr>(param, min, max, orig, format);
			else if (func == TPoissonDistr::name)
				_initializeDistribution<TPoissonDistr>(param, min, max, orig, format);
			else if (func == TBinomialDistr::name)
				_initializeDistribution<TBinomialDistr>(param, min, max, orig, format);
			else if (func == TNegativeBinomialDistr::name)
				_initializeDistribution<TNegativeBinomialDistr>(param, min, max, orig, format);
			else if (func == TChiDistr::name)
				_initializeDistribution<TChiDistr>(param, min, max, orig, format);
			else if (func == TChisqDistr::name)
				_initializeDistribution<TChisqDistr>(param, min, max, orig, format);
			else if (func == TParetoDistr::name)
				_initializeDistribution<TParetoDistr>(param, min, max, orig, format);
			else if (func == TGammaDistr::name)
				_initializeDistribution<TGammaDistr>(param, min, max, orig, format);
			else if (func == TGammaModeDistr::name)
				_initializeDistribution<TGammaModeDistr>(param, min, max, orig, format);
			else
				UERROR("Unknown function '", func, "'! Currently supported are ", TExponentialDistr::name, ", ",
					   TNormalDistr::name, ", ", TPoissonDistr::name, ", ", TBinomialDistr::name, ", ",
					   TNegativeBinomialDistr::name, ", ", TChiDistr::name, ", ", TChisqDistr::name, ", ",
					   TParetoDistr::name, ", ", TGammaDistr::name, " and ", TGammaModeDistr::name, ".");
		}
	}

	// describe itself
	std::string functionString() const noexcept {
		if (_values->size() == 1) {
			return "fixed(" + str::toString(_values->front()) + ")";
		} else {
			std::string s = "categorical(" + str::toString(_values->front()) + ":" + str::toString(_frequencies.front());
			for (size_t i = 1; i < _values->size(); ++i) {
				s += "," + str::toString(_values->index2value(i)) + ":" + str::toString(_frequencies[i]);
			}
			return s + ")";
		}
	}

	std::string verbalizedString() const {
		return "categorical distribution with values " + _values->valueString() + " at frequencies " +
			   coretools::str::concatenateString(_frequencies, ", ");
	}

	// functions to get densities
	Probability density(valueType x) const noexcept {
		auto i = _values->value2index(x);
		if (i.first) { return _frequencies[i.second]; }
		return 0.0_P;
	}

	LogProbability logDensity(Positive x) {
		auto i = _values->value2index(x);
		if (i.first) { return _logFrequencies[i.second]; }
		return LogProbability::lowest();
	}

	Probability cumulativeDensity(Positive x) {
		if (x < _values->min()) { return 0.0_P; }
		return _picker.cumul()[_values->value2indexAtOrBefore(x)];
	}

	double mean() const { return _mean; }

	valueType min() const { return _values->min(); }

	valueType max() const { return _values->max(); }

	valueType sample() const { return _values->index2value(_picker(instances::randomGenerator().getRand())); }

	void sample(std::vector<valueType> &vec) const {
		for (auto &it : vec) { it = sample(); }
	}
};

} // namespace coretools::probdist

#endif /* CORE_MATH_TCATEGORICALDISTRIBUTION_H_ */
