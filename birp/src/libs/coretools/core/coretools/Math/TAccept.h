//
// Created by caduffm on 5/25/23.
//

#ifndef TACCEPT_H
#define TACCEPT_H

#include <array>
#include <cstddef>
#include <cstdint>

namespace coretools {

class TAccept {
	// accept or reject a log ratio log(q) with minimal use of logarithms (exact)
	// returns true if log(q) >= log(random number)
	// algorithm:
	// 1) draw random number
	// 2) get rough estimate of log(random number) with a lookup table
	// 3) if log(q) is far away from estimated log(random number) -> no need to calculate log, just accept/reject
	// 4) else: calculate exact log(random number) and accept/reject based on this
	// -> exact algorithm, no approximation, but should use less logarithms than naive implementation
	// lookup table constructed with R script "/home/caduffm/ownCloud - Madleina Caduff
	// (unifr.ch)@drive.switch.ch/PhD/stattools/approximateAcceptance.R"
private:
	// size corresponds to lookup table
	static constexpr uint16_t _lengthLookup = 101;
	static constexpr double _xThreshold     = 1e-10;
	static constexpr double _inv_delta_x    = (double)(_lengthLookup - 1) / (1.0 - _xThreshold);

	static constexpr std::array<double, _lengthLookup> _lookup = {
	    -23.02585092994, -4.60517017609, -3.91202300053, -3.50655789409, -3.21887582247, -2.99573227165, -2.81341071519,
	    -2.65926003560,  -2.52572864316, -2.40794560764, -2.30258509209, -2.20727491238, -2.12026353547, -2.04022082786,
	    -1.96611285576,  -1.89711998432, -1.83258146322, -1.77195684144, -1.71479842764, -1.66073120640, -1.60943791203,
	    -1.56064774789,  -1.51412773228, -1.46967596972, -1.42711635532, -1.38629436082, -1.34707364768, -1.30933331971,
	    -1.27296567556,  -1.23787435576, -1.20397280409, -1.17118298128, -1.13943428298, -1.10866262432, -1.07880966118,
	    -1.04982212431,  -1.02165124735, -0.99425227317, -0.96758402610, -0.94160853970, -0.91629073172, -0.89159811914,
	    -0.86750056757,  -0.84397007016, -0.82098055194, -0.79850769610, -0.77652878938, -0.75502258417, -0.73396917497,
	    -0.71334988777,  -0.69314718046, -0.67334455317, -0.65392646731, -0.63487827235, -0.61618613934, -0.59783700067,
	    -0.57981849517,  -0.56211891808, -0.54472717537, -0.52763274201, -0.51082562370, -0.49429632175, -0.47803580088,
	    -0.46203545954,  -0.44628710257, -0.43078291604, -0.41551544391, -0.40047756655, -0.38566248076, -0.37106368135,
	    -0.35667494390,  -0.34249030891, -0.32850406693, -0.31471074480, -0.30110509275, -0.28768207242, -0.27443684567,
	    -0.26136476410,  -0.24846135927, -0.23572233349, -0.22314355129, -0.21072103129, -0.19845093870, -0.18632957817,
	    -0.17435338713,  -0.16251892948, -0.15082288972, -0.13926206732, -0.12783337150, -0.11653381624, -0.10536051565,
	    -0.09431067946,  -0.08338160893, -0.07257069283, -0.06187540371, -0.05129329438, -0.04082199452, -0.03045920748,
	    -0.02020270732,  -0.01005033585, 0.00000000000};

	static size_t _getIndex(double x);
	static bool _evaluate_right(double logQ, size_t right_bin, double x);
	static bool _evaluate_left(double logQ, size_t left_bin, double x);

public:
	static bool accept(double logQ);
};
} // namespace coretools
#endif // TACCEPT_H
