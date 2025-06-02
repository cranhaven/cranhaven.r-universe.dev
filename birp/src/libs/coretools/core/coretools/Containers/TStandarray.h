#ifndef TSTANDARRAY_H_
#define TSTANDARRAY_H_

#include <algorithm>
#include <cmath>
#include <iterator>
#include <type_traits>

#include "coretools/Containers/TStrongArray.h"
#include "coretools/traits.h"
#include "coretools/Containers/CTFraction.h"

namespace coretools {

template<typename Type, typename Index, size_t N = index(Index::max), typename Max=CT1<Type>>
class TStrongStandarray {
private:
	using Underlying = underlyingType_t<Type>;

	static_assert(N > 1);
	static_assert(Max::value > 0);
	static_assert(std::is_floating_point_v<Underlying>);

	static constexpr size_t _I = []() {
		if (N == 2) return 1;
		if (N <= 4) return 2;
		if (N <= 8) return 3;
		if (N <= 16) return 4;
		if (N <= 32) return 5;
		if (N <= 64) return 6;
		return 0;
	}();

	TArray<Underlying, N - 1> _data;

	static constexpr auto _indices() noexcept {
		std::array<std::array<size_t, N>, N> ars{};
		for (size_t maxI = 0; maxI < N; ++maxI) {
			size_t index = 0;
			for (size_t i = 0; i < N; ++i) {
				ars[maxI][i] = index;
				if (maxI != i) ++index;
			}
		}
		return ars;
	}

	static constexpr auto _revIndices() noexcept {
		std::array<std::array<size_t, N - 1>, N> ars{};
		for (size_t maxI = 0; maxI < N; ++maxI) {
			for (size_t i = 0; i < maxI; ++i) {
				ars[maxI][i] = i;
			}
			for (size_t i = maxI + 1; i < N; ++i) {
				ars[maxI][i-1] = i;
			}
		}
		return ars;
	}

	constexpr Type _get(size_t i) const noexcept {
		constexpr auto idx = _indices();
		const auto maxI    = _maxI();

		if (i == maxI) {
			return Type(Max::value);
		} else {
			const auto &idi = idx[maxI];
			return Type(std::abs(_data[idi[i]]));
		}
	}

	constexpr size_t _maxI() const noexcept {
		size_t tag = 0;
		for (size_t i = 0; i < _I; ++i) {
			tag |= std::signbit(_data[(i)]) << i;
		}
		return tag;
	}

	template<typename T>
	constexpr TStrongStandarray(const T* data) noexcept {
		constexpr auto idx   = _revIndices();
		const size_t maxI    = std::distance(data, std::max_element(data, data + N));
		const auto &idi      = idx[maxI];
		const Underlying max = *(data + maxI);

		for (size_t i = 0; i < _I; ++i) {
			if (maxI & (1 << i))
				_data.data()[i] = -data[idi[i]]/max;
			else
			_data.data()[i] = data[idi[i]]/max;
		}
		for (size_t i = _I; i < idi.size(); ++i) {
			_data.data()[i] = data[idi[i]] / max;
		}
	}

public:
	constexpr TStrongStandarray() :_data{Type(Max::value)} {}

	static constexpr TStrongStandarray standardize(const std::array<Type, N> &init) {
		return TStrongStandarray(init.data());
	}

	template<typename OT = Underlying, std::enable_if_t<std::is_floating_point_v<underlyingType_t<OT>>, bool> = true>
	static constexpr TStrongStandarray standardize(const std::array<OT, N> &init) {
		return TStrongStandarray(init.data());
	}

	static constexpr TStrongStandarray standardize(const TStrongArray<Type, Index, N> &init) {
		return TStrongStandarray(init.data());
	}
	template<typename OT = Underlying, std::enable_if_t<std::is_floating_point_v<underlyingType_t<OT>>, bool> = true>
	static constexpr TStrongStandarray standardize(const TStrongArray<OT, Index, N> &init) {
		return TStrongStandarray(init.data());
	}

	constexpr Type operator[](Index i) const noexcept { return _get(index(i)); }

	constexpr Type at(Index i) const {
		if (index(i) >= N) throw std::out_of_range("TStandarrray out of range");
		return _get(index(i));
	}

	constexpr TStrongArray<Type, Index, N> get() const noexcept {
		constexpr auto idx = _revIndices();
		const auto maxI    = _maxI();
		const auto &idi    = idx[maxI];

		TStrongArray<Type, Index, N> ret;
		for (size_t i = 0; i < _I; ++i) {
			ret.data()[idi[i]] = Type(std::abs(_data[i]));
		}
		for (size_t i = _I; i < N - 1; ++i) {
			ret.data()[idi[i]] = Type(_data[i]);
		}
		ret.data()[maxI] = Type(Max::value);
		return ret;
	}

	constexpr Type front() const noexcept { return _get(0); }
	constexpr Type back() const noexcept { return _get(N - 1); }

	constexpr bool empty() const noexcept { return _data.empty(); }
	constexpr size_t size() const noexcept { return N; }
	constexpr size_t max_size() const noexcept { return N; }
};

template<typename Type, size_t N, typename Max=CT1<Type>>
using TStandarray = TStrongStandarray<Type, size_t, N, Max>;

} // namespace coretools

#endif
