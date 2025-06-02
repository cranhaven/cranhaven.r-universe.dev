#ifndef CORETOOLS_CONTAINERS_TMULTIVECTOR_H_
#define CORETOOLS_CONTAINERS_TMULTIVECTOR_H_

#include <initializer_list>
#include <iterator>
#include <utility>
#include <vector>

#include "coretools/Containers/TNestedIterator.h"
#include "coretools/Containers/TView.h"
#include "coretools/Main/TError.h"

namespace coretools {
template<typename Type> class TMultiVector {
private:
	static constexpr double _k = 1.5; // growth-factor
	size_t _nCols              = 1;   // no 0-columns allowed!
	std::vector<Type> _data;

	void _incCapacity() {
		const auto NewSize = size() + 1;
		if (capacity() >= NewSize) return;

		_data.reserve(nCols()*size_t(capacity()*_k + 1));
	}

public:
	using type                   = Type;
	using value_type             = TView<Type>;
	using iterator               = TNestedIterator<TMultiVector>;
	using const_iterator         = TNestedIterator<const TMultiVector>;
	using reverse_iterator       = TNestedIterator<TMultiVector, true>;
	using const_reverse_iterator = TNestedIterator<const TMultiVector, true>;
	using index_type             = size_t;
	using size_type              = size_t;

	TMultiVector() = default;
	TMultiVector(size_t NCols) : _nCols(NCols) {
		if (NCols == 0) DEVERROR("There must be a least one column!");
	}
	TMultiVector(size_t NRows, size_t NCols) : _nCols(NCols), _data(NRows * NCols) {}

	TMultiVector(std::initializer_list<std::initializer_list<Type>> Outer) {
		_nCols = Outer.front().size();
		_data.reserve(Outer.size()*nCols());
		for (const auto& Inner: Outer) {
			if (Inner.size() != nCols()) DEVERROR("Every row must have the same inner dimension ", nCols(), "!");
			for (const auto& v: Inner) {
				_data.push_back(v);
			}
		}
		assert(_data.size() % nCols() == 0);
	}


	void clear() noexcept { _data.clear(); }
	bool empty() const noexcept { return size() == 0; }

	size_t nRows() const noexcept { return _data.size()/nCols(); }
	size_t nCols() const noexcept {return _nCols;}
	size_t size() const noexcept { return nRows(); }
	size_t capacity() const noexcept { return _data.capacity()/nCols(); }

	auto shape() const noexcept {return std::array{size(), nCols()};}
	size_t length() const noexcept { return _data.size(); }

	void reserve(size_t N) {_data.reserve(N*nCols());}
	void resize(size_t N, Type V = {}) {
		assert(nCols() > 0);
		_data.resize(N*_nCols, V);
	}
	void reshape(size_t N) {
		assert(N > 0);
		_nCols = N;
		_data.resize(nCols() * nRows()); // this may remove data!
	}
	void reshape(size_t NRows, size_t NCols, Type V = {}) {
		if (NCols == 0) DEVERROR("There must be a least one column!");
		_nCols = NCols;
		_data.resize(NRows * NCols, V);
	}

	void shrink_to_fit() {
		// swap trick as std::vector::shrink_to_fit may not do anything
		std::vector<Type>(_data).swap(_data);
	}

	void swap(TMultiVector<Type> &other) noexcept(std::is_nothrow_swappable_v<Type>) {
		std::swap(_nCols, other._nCols);
		_data.swap(other._data);
	}

	void push_back(TConstView<Type> View) {
		if (View.size() != nCols()) DEVERROR("Every row must have the same inner dimension ", nCols(), "!");
		_incCapacity();
		if constexpr (std::is_trivially_constructible_v<Type>) {
			const auto oldSize = _data.size();
			_data.resize(oldSize + View.size());
			std::copy(View.begin(), View.end(), _data.begin() + oldSize);
		} else {
			std::copy(View.begin(), View.end(), std::back_inserter(_data));
		}
		assert(_data.size() % nCols() == 0);
	}

	void push_back(const Type& Value) {
		_incCapacity();
		_data.resize(_data.size() + nCols(), Value);
		assert(_data.size() % nCols() == 0);
	}

	template<typename... Args>
	void emplace_back(Args&&... args) {
		_incCapacity();
		_data.resize(_data.size() + nCols(), Type(std::forward<Args>(args)...));
		assert(_data.size() % nCols() == 0);
	}

	TView<Type> get(size_t i) noexcept {
		assert(i < size());
		return TView<Type>(_data.data() + i*nCols(), _data.data() + (i+1)*nCols());
	}
	TView<Type> operator[](size_t i) noexcept { return get(i); }

	TConstView<Type> get(size_t i) const noexcept {
		assert(i < size());
		return TConstView<Type>(_data.data() + i*nCols(), _data.data() + (i+1)*nCols());
	}
	TConstView<Type> operator[](size_t i) const noexcept { return get(i); }

	TView<Type> at(size_t i) {
		if (size() <= i) throw std::out_of_range("TMultiVector out of range");
		return get(i);
	}
	TConstView<Type> at(size_t i) const {
		if (size() <= i ) throw std::out_of_range("TMultiVector out of range");
		return get(i);
	}

	TView<Type> front() noexcept { return get(0); }
	TConstView<Type> front() const noexcept { return get(0); }

	TView<Type> back() noexcept { return get(size() - 1); }
	TConstView<Type> back() const noexcept { return get(size() - 1); }

	Type *data() noexcept { return _data.data(); }
	const Type *data() const noexcept { return _data.data(); }

	iterator begin() noexcept { return iterator(this); }
	iterator end() noexcept { return begin() + size(); }

	const_iterator begin() const noexcept { return const_iterator(this); }
	const_iterator end() const noexcept { return begin() + size(); }

	const_iterator cbegin() const noexcept { return begin(); }
	const_iterator cend() const noexcept { return end(); }

	reverse_iterator rbegin() noexcept { return reverse_iterator(this); }
	reverse_iterator rend() noexcept { return reverse_iterator(this) + size(); }

	const_reverse_iterator rbegin() const noexcept { return const_reverse_iterator(this); }
	const_reverse_iterator rend() const noexcept { return const_reverse_iterator(this) + size(); }

	const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	const_reverse_iterator crend() const noexcept { return crbegin(); }

	friend bool operator==(const TMultiVector<Type>& lhs, const TMultiVector<Type>& rhs) noexcept {
		return lhs._nCols == rhs._nCols && lhs._data == rhs._data;
	}

	friend bool operator!=(const TMultiVector<Type>& lhs, const TMultiVector<Type>& rhs) noexcept {
		return !operator==(lhs, rhs);
	}
};

} // namespace coretools

#endif
