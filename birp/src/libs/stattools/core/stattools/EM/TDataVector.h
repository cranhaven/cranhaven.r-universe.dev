/*
 * THMMVectors.h
 *
 *  Created on: Dec 15, 2020
 *      Author: phaentu
 */

#ifndef TDATAVECTOR_H_
#define TDATAVECTOR_H_

#include "coretools/Main/TError.h"
#include "coretools/traits.h"
#include "coretools/Main/TLog.h"

namespace stattools {

//-------------------------------------
// TDataVector_base
// a pure abstract class
//-------------------------------------
template<typename PrecisionType, typename SizeType> class TDataVector_base {
protected:
	PrecisionType *_current{};
	SizeType _size{};
	bool _hasStorage{};

	virtual void _init() {
		_current    = nullptr;
		_size       = 0;
		_hasStorage = false;
	}

private:
	void _copyFrom(const TDataVector_base &other) {
		// creates another pointer, but does not copy memory -> hasStorage = false, memory is deleted elsewhere
		_current    = other._current;
		_size       = other._size;
		_hasStorage = false;
	}

	void _moveFrom(TDataVector_base &&other) {
		// transfers memory
		_current    = other._current;
		_size       = other._size;
		_hasStorage = other._hasStorage;

		other._init();
	}

public:
	TDataVector_base() { _init(); }

	virtual ~TDataVector_base() { clear(); }

	TDataVector_base(const TDataVector_base &other) { _copyFrom(other); }

	TDataVector_base(TDataVector_base &&other) { _moveFrom(other); }

	virtual void clear() {
		if (_hasStorage) {
			delete[] _current;
			_current    = nullptr;
			_hasStorage = false;
		}
		_size = 0;
	}

	virtual void resize(SizeType NumStates) = 0;

	virtual SizeType size() const { return _size; }
	bool empty() const { return _size == 0; }
	bool hasStorage() const { return _hasStorage; }

	virtual void set(PrecisionType Value) {
		for (SizeType i = 0; i < _size; ++i) { _current[i] = Value; }
	}

	template<typename ValType> void copyToCurrent(ValType *Values) {
		if constexpr (coretools::isSameUnderlyingType_v<ValType, PrecisionType>) {
			std::memcpy(_current, Values, _size * sizeof(PrecisionType));
		} else {
			for (size_t i = 0; i < _size; ++i) { _current[i] = (PrecisionType)Values[i]; }
		}
	}

	template<typename ValType> void copyFromCurrent(ValType *Dest) {
		if constexpr (coretools::isSameUnderlyingType_v<ValType, PrecisionType>) {
			std::memcpy(Dest, _current, _size * sizeof(PrecisionType));
		} else {
			for (size_t i = 0; i < _size; ++i) { Dest[i] = (ValType)_current[i]; }
		}
	}

	virtual PrecisionType sum() const {
		PrecisionType s = 0;
		for (SizeType i = 0; i < _size; ++i) { s += _current[i]; }
		return s;
	}

	virtual PrecisionType max() const {
		PrecisionType s = _current[0];
		for (SizeType i = 1; i < _size; ++i) {
			if (_current[i] > s) { s = _current[i]; }
		}
		return s;
	}

	virtual SizeType maxIndex() const {
		SizeType index  = 0;
		PrecisionType s = _current[index];
		for (SizeType i = 1; i < _size; ++i) {
			if (_current[i] > s) {
				index = i;
				s     = _current[i];
			}
		}
		return index;
	}

	virtual void normalize(PrecisionType SumToNormalize) {
		for (SizeType i = 0; i < _size; ++i) { _current[i] /= SumToNormalize; }
	}

	virtual void print() const {
		for (SizeType i = 0; i < _size; ++i) {
			if (i > 0) { coretools::cout << ", "; }
			coretools::cout << _current[i];
		}
	}

	PrecisionType *pointerToCurrent() { return _current; }

	// iterator
	struct Iterator {
	public:
		// iterator traits
		using difference_type   = PrecisionType;
		using value_type        = PrecisionType;
		using pointer           = const PrecisionType *;
		using reference         = PrecisionType;
		using iterator_category = std::forward_iterator_tag;

		// constructors
		Iterator(pointer ptr) : _ptr(ptr) {}

		// operators
		reference operator*() const { return *_ptr; }
		pointer operator->() { return _ptr; }
		Iterator operator+(size_t n) {
			_ptr += n;
			return *this;
		}

		Iterator &operator++() {
			_ptr++;
			return *this;
		}
		Iterator operator++(int) {
			Iterator tmp = *this;
			++(*this);
			return tmp;
		}

		friend bool operator==(const Iterator &a, const Iterator &b) { return a._ptr == b._ptr; }
		friend bool operator!=(const Iterator &a, const Iterator &b) { return a._ptr != b._ptr; }

	private:
		pointer _ptr;
	};

	Iterator begin() { return Iterator(&_current[0]); }
	Iterator end() { return Iterator(&_current[_size]); }

	struct Const_Iterator {
	public:
		// iterator traits
		using difference_type   = PrecisionType;
		using value_type        = PrecisionType;
		using pointer           = const PrecisionType *;
		using reference         = PrecisionType;
		using iterator_category = std::forward_iterator_tag;

		// constructors
		Const_Iterator(pointer ptr) : _ptr(ptr) {}

		// operators
		reference operator*() const { return *_ptr; }
		pointer operator->() { return _ptr; }
		Const_Iterator operator+(size_t n) {
			_ptr += n;
			return *this;
		}

		Const_Iterator &operator++() {
			_ptr++;
			return *this;
		}
		Const_Iterator operator++(int) {
			Iterator tmp = *this;
			++(*this);
			return tmp;
		}

		friend bool operator==(const Const_Iterator &a, const Const_Iterator &b) { return a._ptr == b._ptr; }
		friend bool operator!=(const Const_Iterator &a, const Const_Iterator &b) { return a._ptr != b._ptr; }

	private:
		pointer _ptr;
	};

	Const_Iterator cbegin() const { return Const_Iterator(&_current[0]); }
	Const_Iterator cend() const { return Const_Iterator(&_current[_size]); }
};

//-------------------------------------
// TDataVector
//-------------------------------------
template<typename PrecisionType, typename SizeType> class TDataVector : TDataVector_base<PrecisionType, SizeType> {
protected:
	using TDataVector_base<PrecisionType, SizeType>::_current;
	using TDataVector_base<PrecisionType, SizeType>::_size;
	using TDataVector_base<PrecisionType, SizeType>::_hasStorage;

	using TDataVector_base<PrecisionType, SizeType>::_init;

public:
	TDataVector(){}

	TDataVector(SizeType NumStates) {
		_size       = NumStates;
		_current    = new PrecisionType[_size];
		_hasStorage = true;
	}

	TDataVector(PrecisionType *Pointer, SizeType NumStates) {
		// creates another pointer, but does not copy memory -> hasStorage = false, memory is deleted elsewhere
		_current    = Pointer;
		_size       = NumStates;
		_hasStorage = false;
	}

	TDataVector(const TDataVector &other) : TDataVector_base<PrecisionType, SizeType>(other){}

	TDataVector(TDataVector &&other) : TDataVector_base<PrecisionType, SizeType>(other){}

	TDataVector &operator=(const TDataVector &other) {
		TDataVector_base<PrecisionType, SizeType>::_copyFrom(other);
		return *this;
	}

	TDataVector &operator=(TDataVector &&other) {
		TDataVector_base<PrecisionType, SizeType>::_moveFrom(other);
		return *this;
	}

	virtual ~TDataVector() { clear(); }

	virtual void clear() {
		if (_hasStorage) {
			delete[] _current;
			_current    = nullptr;
			_hasStorage = false;
		}
		_size = 0;
	}

	virtual void resize(SizeType NumStates) {
		if (NumStates != _size) {
			clear();
			if (NumStates > 0) {
				_size       = NumStates;
				_current    = new PrecisionType[_size];
				_hasStorage = true;
			}
		}
	}

	PrecisionType operator[](SizeType State) const { return _current[State]; }
	PrecisionType &operator[](SizeType State) { return _current[State]; }

	virtual TDataVector<PrecisionType, SizeType> current() const {
		return TDataVector<PrecisionType, SizeType>(_current, _size);
	}

	using TDataVector_base<PrecisionType, SizeType>::size;
	using TDataVector_base<PrecisionType, SizeType>::empty;
	using TDataVector_base<PrecisionType, SizeType>::hasStorage;
	using TDataVector_base<PrecisionType, SizeType>::set;
	using TDataVector_base<PrecisionType, SizeType>::copyFromCurrent;
	using TDataVector_base<PrecisionType, SizeType>::copyToCurrent;
	using TDataVector_base<PrecisionType, SizeType>::sum;
	using TDataVector_base<PrecisionType, SizeType>::max;
	using TDataVector_base<PrecisionType, SizeType>::maxIndex;
	using TDataVector_base<PrecisionType, SizeType>::normalize;
	using TDataVector_base<PrecisionType, SizeType>::print;
	using TDataVector_base<PrecisionType, SizeType>::pointerToCurrent;

	using TDataVector_base<PrecisionType, SizeType>::begin;
	using TDataVector_base<PrecisionType, SizeType>::end;
	using TDataVector_base<PrecisionType, SizeType>::cbegin;
	using TDataVector_base<PrecisionType, SizeType>::cend;
};

//-------------------------------------
// TDataVectorPair
//-------------------------------------
template<typename PrecisionType, typename SizeType>
class TDataVectorPair : public TDataVector<PrecisionType, SizeType> {
	// manages two pointers: _current and _vec.
	// -> _vec: points to an array of length 2*_size.
	// -> _current: points to an element of _vec.
	//              This is either the first element of _vec (= _vec), or the "middle" element of _vec (= _vec + _size).
	// used for alpha and beta of HMM, where we always need to remember the current and previous/next element,
	// respectively
private:
	using TDataVector<PrecisionType, SizeType>::_current;
	using TDataVector<PrecisionType, SizeType>::_size;
	using TDataVector<PrecisionType, SizeType>::_hasStorage;
	PrecisionType *_vec;

	void _init() {
		TDataVector<PrecisionType, SizeType>::_init();
		_vec        = nullptr;
		_hasStorage = false;
	}

public:
	TDataVectorPair() { _init(); }
	TDataVectorPair(SizeType NumStates) {
		_init();
		resize(NumStates);
	}
	~TDataVectorPair() { clear(); }

	void clear() {
		if (_hasStorage) {
			delete[] _vec;
			_hasStorage = false;
			_vec        = nullptr;
			_current    = nullptr;
		}
		_size = 0;
	}

	void resize(SizeType NumStates) {
		if (NumStates != _size) {
			this->clear();
			if (NumStates > 0) {
				_size       = NumStates;
				_vec        = new PrecisionType[2 * _size];
				_hasStorage = true;
			}
		}
		_current = _vec;
	}

	void swap() {
		if (_current == _vec) {
			_current = _vec + _size;
			// _current now points to "middle" element of _vec
		} else {
			_current = _vec;
			// _current now points to first element in _vec
		}
	}

	TDataVector<PrecisionType, SizeType> other() const {
		if (_current == _vec) {
			// return data vector containing the second part of _vec
			return TDataVector<PrecisionType, SizeType>(_current + _size, _size);
		} else {
			// return data vector containing the first part of _vec
			return TDataVector<PrecisionType, SizeType>(_current - _size, _size);
		}
	}

	using TDataVector_base<PrecisionType, SizeType>::size;
	using TDataVector_base<PrecisionType, SizeType>::empty;
	using TDataVector_base<PrecisionType, SizeType>::set;
	using TDataVector_base<PrecisionType, SizeType>::sum;
	using TDataVector_base<PrecisionType, SizeType>::max;
	using TDataVector_base<PrecisionType, SizeType>::maxIndex;
	using TDataVector_base<PrecisionType, SizeType>::normalize;
	using TDataVector_base<PrecisionType, SizeType>::print;

	using TDataVector_base<PrecisionType, SizeType>::begin;
	using TDataVector_base<PrecisionType, SizeType>::end;
	using TDataVector_base<PrecisionType, SizeType>::cbegin;
	using TDataVector_base<PrecisionType, SizeType>::cend;
};

//-------------------------------------
// TDataVectorMulti
//-------------------------------------
template<typename PrecisionType, typename SizeType, typename LengthType>
class TDataVectorMulti : public TDataVector<PrecisionType, SizeType> {
	// manages three pointers: _vec, _end and _current.
	// -> _vec: points to an array of length _capacity (= _size * _length).
	// -> _end: points to the end of _vec (i.e. to _vec+_size*_length).
	// -> _current: points to an element of _vec.
	//              This is any element whose index is a multiple of _size (e.g. _vec, _vec+_size, _vec+2*_size, ...,
	//              _end-_size).
	// used for alpha and beta of HMM if the entire chain must be stored -> _size would be numStates and _length would
	// be length of chain
protected:
	using TDataVector<PrecisionType, SizeType>::_current;
	using TDataVector<PrecisionType, SizeType>::_size;
	using TDataVector<PrecisionType, SizeType>::_hasStorage;
	PrecisionType *_vec;
	LengthType _length;
	LengthType _capacity;

	// iterator
	PrecisionType *_end;

	void _init() {
		_vec        = nullptr;
		_size       = 0;
		_length     = 0;
		_capacity   = 0;
		_hasStorage = false;

		_current = nullptr;
		_end     = nullptr;
	}

	void _ensureCapacity() {
		if (_size * _length > _capacity) {
			_capacity = _size * _length;
			if (_hasStorage) { delete[] _vec; }
			_vec        = new PrecisionType[_capacity];
			_hasStorage = true;
		}
	}

public:
	TDataVectorMulti() { _init(); }

	TDataVectorMulti(SizeType NumStates, LengthType Length) {
		_init();
		resize(NumStates, Length);
	}

	~TDataVectorMulti() { clear(); }

	void clear() {
		if (_hasStorage) { delete[] _vec; }
		_length     = 0;
		_capacity   = 0;
		_hasStorage = false;
	}

	void resize(SizeType NumStates) { resize(NumStates, _length); }

	void resize(SizeType NumStates, LengthType Length) {
		_size   = NumStates;
		_length = Length;
		_ensureCapacity();
		_end     = &_vec[_length * _size];
		_current = _vec;
	}

	LengthType length() const { return _length; }
	LengthType capacity() const { return _capacity; }

	// loop
	void startForwards() { _current = _vec; }

	bool moveForward() {
		if (_current == _end || _current + _size == _end) { return false; }
		_current += _size;
		return true;
	}

	bool startBackwards() {
		if (_vec == _end) { // start = end -> probably _length not initialized
			return false;
		}
		_current = _end - _size;
		return true;
	}

	bool moveBackward() {
		if (_current == _vec) { return false; }
		_current -= _size;
		return true;
	}

	// access as THMMVector
	TDataVector<PrecisionType, SizeType> previous() const {
		if (_current == _vec) { DEVERROR("already at beginning!"); }
		return TDataVector<PrecisionType, SizeType>(_current - _size, _size);
	}

	TDataVector<PrecisionType, SizeType> current() const {
		if (_current == _end) { DEVERROR("at end!"); }
		return TDataVector<PrecisionType, SizeType>(_current, _size);
	}

	TDataVector<PrecisionType, SizeType> next() const {
		if (_current == _end) { DEVERROR("at end!"); }
		if (_current + _size == _end) { DEVERROR("at end - _size!"); }
		return TDataVector<PrecisionType, SizeType>(_current + _size, _size);
	}

	using TDataVector_base<PrecisionType, SizeType>::size;
	using TDataVector_base<PrecisionType, SizeType>::empty;
	using TDataVector_base<PrecisionType, SizeType>::set;
	using TDataVector_base<PrecisionType, SizeType>::sum;
	using TDataVector_base<PrecisionType, SizeType>::max;
	using TDataVector_base<PrecisionType, SizeType>::maxIndex;
	using TDataVector_base<PrecisionType, SizeType>::normalize;
	using TDataVector_base<PrecisionType, SizeType>::print;

	using TDataVector_base<PrecisionType, SizeType>::begin;
	using TDataVector_base<PrecisionType, SizeType>::end;
	using TDataVector_base<PrecisionType, SizeType>::cbegin;
	using TDataVector_base<PrecisionType, SizeType>::cend;
};

//-------------------------------------
// TDataSquareMatrix
//-------------------------------------
template<typename PrecisionType, typename SizeType>
class TDataSquareMatrix : TDataVector_base<PrecisionType, SizeType> {
	// A linearized square matrix
	// Suitable for data storage but not suitable for matrix operations
	// dimension of matrix: _dim times _dim
	// manages one pointer _current of length _size (where _size = _dim*_dim)
	// used e.g. for storing xi of HMM
protected:
	using TDataVector_base<PrecisionType, SizeType>::_current;
	using TDataVector_base<PrecisionType, SizeType>::_size;
	using TDataVector_base<PrecisionType, SizeType>::_hasStorage;

	SizeType _dim; // _size = _dim*_dim

public:
	TDataSquareMatrix() : TDataVector_base<PrecisionType, SizeType>(){}
	TDataSquareMatrix(SizeType NumStates) : TDataVector_base<PrecisionType, SizeType>() { resize(NumStates); }

	~TDataSquareMatrix() = default;

	void resize(SizeType NumStates) override {
		if (NumStates * NumStates != _size) {
			this->clear();
			if (NumStates > 0) {
				_dim        = NumStates;
				_size       = _dim * _dim;
				_current    = new PrecisionType[_size];
				_hasStorage = true;
			}
		}
	}

	// accessing elements with ()
	PrecisionType &operator()(SizeType From, SizeType To) { return _current[From + _dim * To]; }
	PrecisionType operator()(SizeType From, SizeType To) const { return _current[From + _dim * To]; }

	// size
	SizeType size() const override { return _dim; }
	SizeType linearSize() const { return _size; }

	// functions from base class
	using TDataVector_base<PrecisionType, SizeType>::size;
	using TDataVector_base<PrecisionType, SizeType>::empty;
	using TDataVector_base<PrecisionType, SizeType>::set;
	using TDataVector_base<PrecisionType, SizeType>::sum;
	using TDataVector_base<PrecisionType, SizeType>::max;
	using TDataVector_base<PrecisionType, SizeType>::maxIndex;
	using TDataVector_base<PrecisionType, SizeType>::normalize;
	using TDataVector_base<PrecisionType, SizeType>::print;

	using TDataVector_base<PrecisionType, SizeType>::begin;
	using TDataVector_base<PrecisionType, SizeType>::end;
	using TDataVector_base<PrecisionType, SizeType>::cbegin;
	using TDataVector_base<PrecisionType, SizeType>::cend;
};

} // end namespace stattools

#endif /* TDATAVECTOR_H_ */
