#ifndef CORETOOLS_MAIN_TRANDOMPICKER_H_
#define CORETOOLS_MAIN_TRANDOMPICKER_H_

#include <vector>

#include "coretools/Containers/TView.h"
#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Types/probability.h"

namespace coretools {

class TRandomPicker {
	std::vector<size_t> _index{0};
	std::vector<double> _cumul{1.};

public:
	TRandomPicker() = default;
	TRandomPicker(TConstView<double> Probs, size_t MIndex = 10) {init(Probs, MIndex);}
	TRandomPicker(TConstView<Probability> Probs, size_t MIndex = 10) {init(Probs, MIndex);}

	void init(TConstView<double> Probs, size_t MIndex = 10);
	void init(TConstView<Probability> Probs, size_t MIndex = 10);

	size_t operator()(double r) const noexcept {
		size_t i       = _index[r * _index.size()];
		while (r > _cumul[i]) ++i;
		return i;
	}

	size_t operator()() const {
		return operator()(instances::randomGenerator().getRand());
	}

	const std::vector<double>& cumul() const noexcept {return _cumul;}
};

}

#endif

