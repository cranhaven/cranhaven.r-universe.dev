#ifndef CORETOOLS_CONTAINERS_TNESTEDVECTOR_H_
#define CORETOOLS_CONTAINERS_TNESTEDVECTOR_H_

#include <initializer_list>
#include <utility>
#include <vector>

#include "coretools/Containers/TNestedIterator.h"
#include "coretools/Containers/TView.h"

namespace coretools {

template<typename NestedVector> class TInner;

template<typename ...Types> class TNestedVector {
private:
	static constexpr size_t _N = sizeof...(Types);
	static constexpr double _k = 1.5; // growth-factor
	static_assert(_N > 0);

	std::vector<size_t> _indices{0};
	std::tuple<std::vector<Types>...> _data;

	template<typename Fun> void _forAll(Fun &&fun) {
		std::apply([fun](auto &&...d) { ((fun(d)), ...); }, _data);
	}

	template<size_t I = 0>
	void _resizeData(const std::tuple<Types...>& Vs) {
		if constexpr (I == _N) return;
		else {
			std::get<I>(_data).resize(_indices.back(), std::get<I>(Vs));
			_resizeData<I+1>(Vs);
		}
	}

	void _resizeData() {
		_forAll([this](auto &d) { d.resize(_indices.back()); });
	}

	void _incCapacity(size_t NewLength) {
		if (capacityLength() >= NewLength) return;

		reserveLength(capacityLength()*_k + 1);
	}

public:
	template<size_t I> using type = std::tuple_element_t<I, std::tuple<Types...>>;
	using iterator                = TNestedIterator<TNestedVector>;
	using const_iterator          = TNestedIterator<const TNestedVector>;
	using reverse_iterator        = TNestedIterator<TNestedVector, true>;
	using const_reverse_iterator  = TNestedIterator<const TNestedVector, true>;
	using index_type              = size_t;
	using size_type               = size_t;

	TNestedVector() = default;
	TNestedVector(size_t N) : _indices(N+1) {}
	TNestedVector(TConstView<size_t> Sizes, const std::tuple<Types...>& Vs = {}) { resize(Sizes, Vs); }

	void clear() noexcept {
		_indices.clear();
		_indices.push_back(0);
		_data.clear();
	}

	bool empty() const noexcept { return size() == 0; }
	size_t size() const noexcept { return _indices.size()  - 1; }
	size_t size(size_t i) const noexcept {
		assert(i < size());
		return _indices[i + 1] - _indices[i];
	}
	size_t length() const noexcept { return std::get<0>(_data).size(); }
	size_t capacity() const noexcept { return _indices.capacity() - 1; }
	size_t capacityLength() const noexcept { return std::get<0>(_data).capacity(); }

	void resize(size_t N) {
		_indices.resize(N+1, _indices.back());
		_resizeData();
	}
	void resize(TConstView<size_t> Sizes, const std::tuple<Types...>& Vs = {}) {
		_indices.clear();
		_indices.reserve(Sizes.size() + 1);
		_indices.push_back(0);
		for (const auto s : Sizes) { _indices.push_back(_indices.back() + s); }
		_resizeData(Vs);
	}

	void reserve(size_t N) noexcept { _indices.reserve(N + 1); }
	void reserveLength(size_t N) {
		_forAll([N](auto &d) { d.reserve(N); });
	}

	void swap(TNestedVector<Types...> &other) noexcept(std::is_nothrow_swappable_v<Types...>) {
		_indices.swap(other._indices);
		_data.swap(other._data);
	}

	template<size_t I = 0> void shrink_to_fit() {
		if constexpr (I == _N)
			return;
		else {
			// swap trick as std::vector::shrink_to_fit may not do anything
			if constexpr (I == 0) {
				std::vector<size_t>(_indices).swap(_indices); // only once
			}
			std::vector<type<I>>(std::get<I>(_data)).swap(std::get<I>(_data));
			shrink_to_fit<I+1>();
		}
	}

	void push_back() {
		// empty vector
		_indices.push_back(_indices.back());
	}

	template<size_t I>
	void push_back(TConstView<type<I>> View) {
		_indices.push_back(_indices.back() + View.size());
		_incCapacity(_indices.back());
		if constexpr (std::is_trivially_constructible_v<type<I>>) {
			const auto oldSize = std::get<I>(_data).size();
			std::get<I>(_data).resize(oldSize + View.size());
			std::copy(View.begin(), View.end(), std::get<I>(_data).begin() + oldSize);
		} else {
			std::copy(View.begin(), View.end(), std::back_inserter(std::get<I>(_data)));
		}
		_resizeData();
	}

	template<size_t I>
	void push_back(const type<I>& Value) {
		if (empty()) DEVERROR("Cannot append value to empty nested vector!");
		++_indices.back();
		std::get<I>(_data).push_back(Value);
		reserveLength(std::get<I>(_data).capacity());
		_resizeData();
	}

	template<size_t I>
	void push_back(type<I>&& Value) {
		if (empty()) DEVERROR("Cannot append value to empty nested vector!");
		++_indices.back();
		std::get<I>(_data).push_back(Value);
		reserveLength(std::get<I>(_data).capacity());
		_resizeData();
	}

	template<size_t I, typename... Args>
	void emplace_back(Args&&... args) {
		if (empty()) DEVERROR("Cannot append value to empty nested vector!");
		++_indices.back();
		std::get<I>(_data).emplace_back(std::forward<Args>(args)...);
		reserveLength(std::get<I>(_data).capacity());
		_resizeData();
	}

	TInner<TNestedVector> get(size_t i) noexcept {
		assert(i < size());
		assert(std::get<0>(_data).size() >= _indices[i+1]);
		return TInner<TNestedVector>(this, i);
	}

	TInner<const TNestedVector> get(size_t i) const noexcept {
		assert(i < size());
		assert(std::get<0>(_data).size() >= _indices[i+1]);
		return TInner<const TNestedVector>(this, i);
	}

	TInner<TNestedVector> operator[](size_t i) noexcept { return get(i); }
	TInner<const TNestedVector> operator[](size_t i) const noexcept { return get(i); }

	template<size_t I>
	TView<type<I>> get(size_t i) noexcept {
		auto& dataI = std::get<I>(_data);
		assert(i < size());
		assert(dataI.size() >= _indices[i+1]);
		return TView<type<I>>(dataI.data() + _indices[i], dataI.data() + _indices[i + 1]);
	}

	template<size_t I>
	TConstView<type<I>> get(size_t i) const noexcept {
		const auto& dataI = std::get<I>(_data);
		assert(i < size());
		assert(dataI.size() >= _indices[i+1]);
		return TConstView<type<I>>(dataI.data() + _indices[i], dataI.data() + _indices[i + 1]);
	}

	template<size_t I>
	TView<type<I>> at(size_t i) {
		if (size() <= i || std::get<I>(_data).size() < _indices[i + 1]) throw std::out_of_range("TNestedVector out of range");
		return get(i);
	}

	template<size_t I>
	TConstView<type<I>> at(size_t i) const {
		if (size() <= i || std::get<I>(_data).size() < _indices[i + 1]) throw std::out_of_range("TNestedVector out of range");
		return get(i);
	}
	TInner<TNestedVector> front() noexcept { return get(0); }
	TInner<const TNestedVector> front() const noexcept { return get(0); }

	TInner<TNestedVector> back() noexcept { return get(size() - 1); }
	TInner<const TNestedVector> back() const noexcept { return get(size() - 1); }

	template<size_t I> TView<type<I>> front() noexcept { return get<I>(0); }
	template<size_t I> TConstView<type<I>> front() const noexcept { return get<I>(0); }

	template<size_t I> TView<type<I>> back() noexcept { return get<I>(size() - 1); }
	template<size_t I> TConstView<type<I>> back() const noexcept { return get<I>(size() - 1); }

	template<size_t I> 
	type<I> *data() noexcept { return std::get<I>(_data).data(); }
	template<size_t I> 
	const type<I> *data() noexcept { return std::get<I>(_data).data(); }

	iterator begin() noexcept { return iterator(this); }
	iterator end() noexcept { return begin() + size(); }

	const_iterator begin() const noexcept { return const_iterator(this); }
	const_iterator end() const noexcept { return begin() + size(); }

	const_iterator cbegin() const noexcept { return begin(); }
	const_iterator cend() const noexcept { return end(); }

	reverse_iterator rbegin() noexcept { return reverse_iterator(this); }
	reverse_iterator rend() noexcept { return rbegin() + size(); }

	const_reverse_iterator rbegin() const noexcept { return reverse_iterator(this); }
	const_reverse_iterator rend() const noexcept { return rbegin() + size(); }

	const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	const_reverse_iterator crend() const noexcept { return rend(); }

	template<size_t I> auto begin() noexcept { return TNestedIterator<TNestedVector, false, I>(this); }
	template<size_t I> auto end() noexcept { return begin<I>() + size(); }

	template<size_t I> auto begin() const noexcept { return TNestedIterator<const TNestedVector, false, I>(this); }
	template<size_t I> auto end() const noexcept { return begin<I>() + size(); }

	template<size_t I> auto cbegin() const noexcept { return begin<I>(); }
	template<size_t I> auto cend() const noexcept { return end<I>(); }

	template<size_t I> auto rbegin() noexcept { return TNestedIterator<TNestedVector, true, I>(this); }
	template<size_t I> auto rend() noexcept { return rbegin<I>() + size(); }

	template<size_t I> auto rbegin() const noexcept { return TNestedIterator<const TNestedVector, true, I>(this); }
	template<size_t I> auto rend() const noexcept { return rbegin<I>() + size(); }

	template<size_t I> auto crbegin() const noexcept { return rbegin<I>(); }
	template<size_t I> auto crend() const noexcept { return rend<I>(); }

	friend bool operator==(const TNestedVector<Types...>& lhs, const TNestedVector<Types...>& rhs) noexcept {
		return lhs._indices == rhs._indices && lhs._data == rhs._data;
	}

	friend bool operator!=(const TNestedVector<Types...>& lhs, const TNestedVector<Types...>& rhs) noexcept {
		return !operator==(lhs, rhs);
	}
};

template<typename NestedVector>
class TInner {
	template<size_t I> using type = typename NestedVector::template type<I>;
	NestedVector *_vector;
	size_t _i;

public:
	TInner(NestedVector *Vector, size_t I = 0) : _vector(Vector), _i(I) {}

	template<size_t I> TView<type<I>> get() noexcept { return _vector->template get<I>(_i); }
	template<size_t I> TConstView<type<I>> get() const noexcept { return _vector->template get<I>(_i); }
	size_t size() const noexcept { return _vector->size(_i); }
};

template<typename Type> class TNestedVector<Type> {
private:
	static constexpr double _k = 1.5; // growth-factor
	std::vector<size_t> _indices{0};
	std::vector<Type> _data;

	void _incCapacity(size_t NewLength) {
		if (_data.capacity() >= NewLength) return;

		reserveLength(_data.capacity()*_k + 1);
	}

public:
	using type                   = Type;
	using iterator               = TNestedIterator<TNestedVector>;
	using const_iterator         = TNestedIterator<const TNestedVector>;
	using reverse_iterator       = TNestedIterator<TNestedVector, true>;
	using const_reverse_iterator = TNestedIterator<const TNestedVector, true>;
	using index_type             = size_t;
	using size_type              = size_t;

	TNestedVector() = default;
	TNestedVector(size_t N) : _indices(N+1) {}
	TNestedVector(TConstView<size_t> Sizes, const Type& V = {}) { resize(Sizes, V); }

	TNestedVector(std::initializer_list<std::initializer_list<Type>> Outer) {
		_indices.reserve(Outer.size() + 1);
		for (const auto& Inner: Outer) {
			_indices.push_back(_indices.back() + Inner.size());
			_data.reserve(_data.size() + Inner.size());
			for (const auto& v: Inner) {
				_data.push_back(v);
			}
		}
	}

	void clear() noexcept {
		_indices.clear();
		_indices.push_back(0);
		_data.clear();
	}

	bool empty() const noexcept { return size() == 0; }
	size_t size() const noexcept { return _indices.size()  - 1; }
	size_t size(size_t i) const noexcept {
		assert(i < size());
		return _indices[i + 1] - _indices[i];
	}
	size_t length() const noexcept { return _data.size(); }
	size_t capacity() const noexcept { return _indices.capacity() - 1; }
	size_t capacityLength() const noexcept { return _data.capacity(); }

	void resize(size_t N) {
		_indices.resize(N+1, _indices.back());
		_data.resize(_indices.back()); // may shrink
	}
	void resize(TConstView<size_t> Sizes, const Type& V = {}) {
		_indices.clear();
		_indices.reserve(Sizes.size() + 1);
		_indices.push_back(0);
		for (const auto s: Sizes) {
			_indices.push_back(_indices.back() + s);
		}
		_data.resize(_indices.back(), V);
	}

	void reserve(size_t N) {_indices.reserve(N + 1);}
	void reserveLength(size_t N) {_data.reserve(N);}

	void swap(TNestedVector<Type> &other) noexcept(std::is_nothrow_swappable_v<Type>) {
		_indices.swap(other._indices);
		_data.swap(other._data);
	}

	void shrink_to_fit() {
		// swap trick as std::vector::shrink_to_fit may not do anything
		std::vector<size_t>(_indices).swap(_indices);
		std::vector<Type>(_data).swap(_data);
	}

	void push_back() {
		// empty vector
		_indices.push_back(_indices.back());
	}

	void push_back(TConstView<Type> View) {
		_indices.push_back(_indices.back() + View.size());
		_incCapacity(_indices.back());
		if constexpr (std::is_trivially_constructible_v<Type>) {
			const auto oldSize = _data.size();
			_data.resize(oldSize + View.size());
			std::copy(View.begin(), View.end(), _data.begin() + oldSize);
		} else {
			std::copy(View.begin(), View.end(), std::back_inserter(_data));
		}
		assert(_indices.back() == _data.size());
	}

	void push_back(const Type& Value) {
		if (empty()) DEVERROR("Cannot append value to empty nested vector!");
		++_indices.back();
		_data.push_back(Value);
		assert(_indices.back() == _data.size());
	}

	void push_back(Type&& Value) {
		if (empty()) DEVERROR("Cannot append value to empty nested vector!");
		++_indices.back();
		_data.push_back(std::move(Value));
		assert(_indices.back() == _data.size());
	}

	template<typename... Args>
	void emplace_back(Args&&... args) {
		if (empty()) DEVERROR("Cannot append value to empty nested vector!");
		++_indices.back();
		_data.emplace_back(std::forward<Args>(args)...);
		assert(_indices.back() == _data.size());
	}

	TView<Type> get(size_t i) noexcept {
		assert(i < size());
		assert(_data.size() >= _indices[i+1]);
		return TView<Type>(_data.data() + _indices[i], _data.data() + _indices[i + 1]);
	}
	TView<Type> operator[](size_t i) noexcept { return get(i); }

	TConstView<Type> get(size_t i) const noexcept {
		assert(i < size());
		assert(_data.size() >= _indices[i+1]);
		return TConstView<Type>(_data.data() + _indices[i], _data.data() + _indices[i + 1]);
	}
	TConstView<Type> operator[](size_t i) const noexcept { return get(i); }

	TView<Type> at(size_t i) {
		if (size() <= i || _data.size() < _indices[i + 1]) throw std::out_of_range("TNestedVector out of range");
		return get(i);
	}
	TConstView<Type> at(size_t i) const {
		if (size() <= i || _data.size() < _indices[i + 1]) throw std::out_of_range("TNestedVector out of range");
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

	friend bool operator==(const TNestedVector<Type>& lhs, const TNestedVector<Type>& rhs) noexcept {
		return lhs._indices == rhs._indices && lhs._data == rhs._data;
	}

	friend bool operator!=(const TNestedVector<Type>& lhs, const TNestedVector<Type>& rhs) noexcept {
		return !operator==(lhs, rhs);
	}
};

} // namespace coretools

#endif
