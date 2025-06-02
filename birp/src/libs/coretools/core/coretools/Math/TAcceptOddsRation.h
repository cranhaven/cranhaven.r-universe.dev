
#ifndef TACCEPTODDSRATIO_H
#define TACCEPTODDSRATIO_H

#include <array>
#include <cstddef>
#include <cstdint>

namespace coretools {

class TAcceptOddsRatio {
	// accept or reject a log odds ratio logit(q) with minimal use of logarithms (exact)
	// returns true if logit(q) >= logit(random number)
	// algorithm:
	// 1) draw random number
	// 2) get rough estimate of logit(random number) with a lookup table
	// 3) if logit(q) is far away from estimated logit(random number) -> no need to calculate log, just accept/reject
	// 4) else: calculate exact logit(random number) and accept/reject based on this
	// -> exact algorithm, no approximation, but should use less logarithms than naive implementation
	// lookup table constructed with R script "/home/caduffm/ownCloud - Madleina Caduff
	// (unifr.ch)@drive.switch.ch/PhD/stattools/approximateLogOdds.R"
private:
	// size corresponds to lookup table
	static constexpr uint16_t _lengthLookup = 101;
	static constexpr double _min_x          = 1e-10;
	static constexpr double _max_x          = 1.0 - _min_x;
	static constexpr double _inv_delta_x    = (double)(_lengthLookup - 1) / (_max_x - _min_x);

	static constexpr std::array<double, _lengthLookup> _lookup = {
	    -23.02585092984, -4.59511984024, -3.89182029321, -3.47609868661, -3.17805382795, -2.94443897727, -2.75153531148,
	    -2.58668934278,  -2.44234703423, -2.31363492818, -2.19722457645, -2.09074109614, -1.99243016397, -1.90095876054,
	    -1.81528996604,  -1.73460105484, -1.65822807610, -1.58562726327, -1.51634748893, -1.45001017510, -1.38629436074,
	    -1.32492541439,  -1.26566637300, -1.20831120562, -1.15267950965, -1.09861228840, -1.04596855493, -0.99462257491,
	    -0.94446160862,  -0.89538404685, -0.84729786020, -0.80011929993, -0.75377180221, -0.70818505777, -0.66329421727,
	    -0.61903920827,  -0.57536414478, -0.53221681364, -0.48954822522, -0.44731221795, -0.40546510802, -0.36396537713,
	    -0.32277339220,  -0.28185115208, -0.24116205677, -0.20067069542, -0.16034265004, -0.12014431182, -0.08004270766,
	    -0.04000533461,  0.00000000000,  0.04000533461,  0.08004270766,  0.12014431182,  0.16034265004,  0.20067069542,
	    0.24116205677,   0.28185115208,  0.32277339220,  0.36396537713,  0.40546510802,  0.44731221795,  0.48954822522,
	    0.53221681364,   0.57536414478,  0.61903920827,  0.66329421727,  0.70818505777,  0.75377180221,  0.80011929993,
	    0.84729786020,   0.89538404685,  0.94446160862,  0.99462257491,  1.04596855493,  1.09861228840,  1.15267950965,
	    1.20831120562,   1.26566637300,  1.32492541439,  1.38629436074,  1.45001017510,  1.51634748893,  1.58562726327,
	    1.65822807610,   1.73460105484,  1.81528996604,  1.90095876054,  1.99243016397,  2.09074109614,  2.19722457645,
	    2.31363492818,   2.44234703423,  2.58668934278,  2.75153531148,  2.94443897727,  3.17805382795,  3.47609868661,
	    3.89182029321,   4.59511984024,  23.02585084710};

	static size_t _getIndex(double x);
	static bool _evaluate_right(double logitQ, size_t right_bin, double x);

	static bool _evaluate_left(double logitQ, size_t left_bin, double x);

public:
	static bool accept(double logQ);
};
}

#endif
