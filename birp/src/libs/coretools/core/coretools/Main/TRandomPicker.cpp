#include "TRandomPicker.h"
#include <numeric>

namespace coretools {

namespace impl {

auto createIndex(const std::vector<double> &Cumul, size_t N) noexcept {
	std::vector<size_t> index;
	index.reserve(N);
	const auto step = 1. / N;
	size_t iCumul   = 0;
	for (size_t i = 0; i < N; ++i) {
		const auto pi = step * i;
		while (pi > Cumul[iCumul]) ++iCumul;
		index.push_back(iCumul);
	}
	return index;
}
} // namespace impl

void TRandomPicker::init(TConstView<double> Probs, size_t MIndex) {
	_cumul.resize(Probs.size());
	std::partial_sum(Probs.begin(), Probs.end(), _cumul.begin());
	const auto sum = _cumul.back();

	for (auto it = _cumul.begin(); it < _cumul.end() - 1; ++it) *it /= sum;
	_cumul.back() = 1.;

	_index = impl::createIndex(_cumul, MIndex * Probs.size());
}

void TRandomPicker::init(TConstView<Probability> Probs, size_t MIndex) {
	_cumul.resize(Probs.size());
	_cumul.front() = Probs.front();
	for (size_t i = 1; i < Probs.size(); ++i) {
		_cumul[i] = Probs[i]+_cumul[i-1];
	}
	const auto sum = _cumul.back();

	for (auto it = _cumul.begin(); it < _cumul.end() - 1; ++it) *it /= sum;
	_cumul.back() = 1.;

	_index = impl::createIndex(_cumul, MIndex * Probs.size());
}

} // namespace coretools
