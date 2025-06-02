//
// Created by madleina on 05.08.20.
//

#include "coretools/Main/TRandomGenerator.h"

#include "coretools/algorithms.h"
#include <cstddef>

namespace coretools {

//-------------------------------------------
// TMatrix
//-------------------------------------------

// Parameter Constructors
template<typename T> TMatrix<T>::TMatrix() { clear(); }

template<typename T> TMatrix<T>::TMatrix(int size) {
	_rows        = static_cast<size_t>(size);
	_cols        = static_cast<size_t>(size);
	_initialized = false;
	_initialize(0.);
}

template<typename T> TMatrix<T>::TMatrix(int size, T initial) {
	_rows        = static_cast<size_t>(size);
	_cols        = static_cast<size_t>(size);
	_initialized = false;
	_initialize(initial);
}

template<typename T> TMatrix<T>::TMatrix(int rows, int cols) {
	_rows        = static_cast<size_t>(rows);
	_cols        = static_cast<size_t>(cols);
	_initialized = false;
	_initialize(0);
}

template<typename T> TMatrix<T>::TMatrix(int rows, int cols, T initial) {
	_rows        = static_cast<size_t>(rows);
	_cols        = static_cast<size_t>(cols);
	_initialized = false;
	_initialize(initial);
}

// Copy Constructor
template<typename T> TMatrix<T>::TMatrix(const TMatrix &other) {
	_rows = other.rows();
	_cols = other.cols();
	_mat  = other._mat;

	_initialized = true;
}

template<typename T> TMatrix<T>::TMatrix(const TMatrix &other, T scale) {
	_rows = other.rows();
	_cols = other.cols();
	_mat  = other._mat;
	(*this) *= scale;

	_initialized = true;
}
// Move Constructor
template<typename T> TMatrix<T>::TMatrix(TMatrix &&other) noexcept {
	// call move assignment operator
	*this = std::move(other);
}

template<typename T> TMatrix<T> &TMatrix<T>::operator=(TMatrix<T> &&other) noexcept {
	if (this != &other) {
		clear();
		_rows        = other.rows();
		_cols        = other.cols();
		_mat         = other._mat;
		_initialized = other._initialized;

		other.clear();
	}
	return *this;
}

// Destructor
template<typename T> TMatrix<T>::~TMatrix() = default;

// initialize
template<typename T> void TMatrix<T>::_initialize(T initial) {
	_mat.resize(_rows * _cols);
	set(initial);
	_initialized = true;
}

// resizing
template<typename T> void TMatrix<T>::clear() {
	_mat.clear();
	_rows        = 0;
	_cols        = 0;
	_initialized = false;
}

template<typename T> void TMatrix<T>::resize(int size) { resize(size, size, 0); }

template<typename T> void TMatrix<T>::resize(int size, T initial) { resize(size, size, initial); }

template<typename T> void TMatrix<T>::resize(int rows, int cols) { resize(rows, cols, 0); }

template<typename T> void TMatrix<T>::resize(int rows, int cols, T initial) {
	if (!_initialized || _rows != static_cast<size_t>(rows) || _cols != static_cast<size_t>(cols)) {
		_rows = static_cast<size_t>(rows);
		_cols = static_cast<size_t>(cols);
		_initialize(initial);
	}
}

template<typename T> void TMatrix<T>::zeros(int rows, int cols) { resize(rows, cols, 0); }

template<typename T> void TMatrix<T>::zeros(int rows) { resize(rows, rows, 0); }

// reset all elements to a single value
template<typename T> void TMatrix<T>::set(T value) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) (*this)(i, j) = value;
	}
}

// Assignment Operator
template<typename T> TMatrix<T> &TMatrix<T>::operator=(const TMatrix &other) {
	if (&other == this) return *this;

	_mat         = other._mat;
	_rows        = other._rows;
	_cols        = other._cols;
	_initialized = other._initialized;

	return *this;
}

// Addition of two matrices
template<typename T> TMatrix<T> TMatrix<T>::operator+(const TMatrix &other) {
	TMatrix result(*this);
	result += other;

	return result;
}

// Cumulative addition of this matrix and another
template<typename T> TMatrix<T> &TMatrix<T>::operator+=(const TMatrix &other) {
	assert(_rows == other.rows());
	assert(_cols == other.cols());

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) (*this)(i, j) += other(i, j);
	}

	return *this;
}

// Subtraction of this matrix and another
template<typename T> TMatrix<T> TMatrix<T>::operator-(const TMatrix &other) {
	TMatrix result(*this);
	result -= other;

	return result;
}

// Cumulative subtraction of this matrix and another
template<typename T> TMatrix<T> &TMatrix<T>::operator-=(const TMatrix &other) {
	assert(other.rows() == _rows);
	assert(other.cols() == _cols);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { (*this)(i, j) -= other(i, j); }
	}

	return *this;
}

// Left multiplication of this matrix and another (matrix multiplication, equivalent to this %*% other in R)
template<typename T> TMatrix<T> TMatrix<T>::operator*(const TMatrix &other) const {
	TMatrix result;
	result.fillFromProduct(*this, other);
	return result;
}

// Cumulative left multiplication of this matrix and another (matrix multiplication, equivalent to this %*% other in R)
template<typename T> TMatrix<T> &TMatrix<T>::operator*=(const TMatrix &other) {
	TMatrix result = (*this) * other;
	(*this)        = result;
	return *this;
}

template<typename T> void TMatrix<T>::fillUniformRandom() {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { (*this)(i, j) = instances::randomGenerator().getRand(); }
	}
}

template<typename T> void TMatrix<T>::fillRowsFromVector(const std::vector<T> &Vec, size_t NumRows) {
	_mat.clear();
	_rows        = NumRows;
	_cols        = Vec.size();
	_initialized = true;
	// repeat vector as many times as there are rows
	for (size_t r = 0; r < NumRows; r++) { _mat.insert(_mat.end(), Vec.begin(), Vec.end()); }
}

template<typename T> void TMatrix<T>::fillFromMatrix(const TMatrix &other) { fillFromMatrix(other, 1.); }

template<typename T> void TMatrix<T>::fillFromMatrix(const TMatrix &other, T Scale) {
	resize(static_cast<int>(other.rows()), static_cast<int>(other.cols()));
	for (size_t i = 0; i < _rows; ++i) {
		for (size_t j = 0; j < _cols; ++j) { (*this)(i, j) = other(i, j) * Scale; }
	}
}

template<typename T> void TMatrix<T>::fillFromMatrix(const TBandMatrix<T> &other) { fillFromMatrix(other, 1.); }

template<typename T> void TMatrix<T>::fillFromMatrix(const TBandMatrix<T> &other, T Scale) {
	resize(static_cast<int>(other.rows()), static_cast<int>(other.cols()));
	set(0);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) (*this)(i, j) = other(i, j) * Scale;
	}
}

template<typename T> void TMatrix<T>::fillFromProduct(const TMatrix &first, const TMatrix &second) {
	// left.numCols = right.numRows
	assert(first.cols() == second.rows());

	resize(first.rows(), second.cols(), 0);
	set(0.);
	// now calculate product
	for (size_t i = 0; i < first.rows(); i++) {
		for (size_t j = 0; j < second.cols(); j++) {
			for (size_t k = 0; k < first.cols(); k++) { (*this)(i, j) += first(i, k) * second(k, j); }
		}
	}
}

template<typename T> void TMatrix<T>::fillFromProduct(const TBandMatrix<T> &first, const TBandMatrix<T> &second) {
	TBandMatrix<T> tmp;
	tmp.fillFromProduct(first, second);
	fillFromMatrix(tmp);
}

template<typename T> void TMatrix<T>::fillFromSquare(const TMatrix<T> &other) { fillFromProduct(other, other); }

template<typename T> void TMatrix<T>::fillFromSquare(const TBandMatrix<T> &other) { fillFromProduct(other, other); }

// Calculate a transpose of this matrix
template<typename T> TMatrix<T> TMatrix<T>::transpose() const {
	TMatrix result(_cols, _rows, 0.);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { result(j, i) = (*this)(i, j); }
	}

	return result;
}

// Matrix/scalar addition
template<typename T> TMatrix<T> TMatrix<T>::operator+(T other) {
	TMatrix result(_rows, _cols, 0.0);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { result(i, j) = (*this)(i, j) + other; }
	}

	return result;
}

template<typename T> TMatrix<T> TMatrix<T>::operator+=(T other) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { (*this)(i, j) += other; }
	}

	return *this;
}

// Matrix/scalar subtraction
template<typename T> TMatrix<T> TMatrix<T>::operator-(T other) {
	TMatrix result(_rows, _cols, 0.0);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { result(i, j) = (*this)(i, j) - other; }
	}

	return result;
}

template<typename T> TMatrix<T> TMatrix<T>::operator-=(T other) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { (*this)(i, j) -= other; }
	}

	return *this;
}

// Matrix/scalar multiplication
template<typename T> TMatrix<T> TMatrix<T>::operator*(T other) {
	TMatrix result(_rows, _cols, 0.0);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { result(i, j) = (*this)(i, j) * other; }
	}

	return result;
}

template<typename T> TMatrix<T> TMatrix<T>::operator*=(T other) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { (*this)(i, j) *= other; }
	}

	return *this;
}

// Matrix/scalar division
template<typename T> TMatrix<T> TMatrix<T>::operator/(T other) {
	TMatrix result(_rows, _cols, 0.0);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { result(i, j) = (*this)(i, j) / other; }
	}

	return result;
}

template<typename T> TMatrix<T> TMatrix<T>::operator/=(T other) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { (*this)(i, j) /= other; }
	}

	return *this;
}

// Multiply a matrix with a vector
template<typename T> std::vector<T> TMatrix<T>::operator*(const std::vector<T> &other) {
	assert(_cols == other.size());

	std::vector<T> result(_rows, 0.);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { result[i] += (*this)(i, j) * other[j]; }
	}

	return result;
}

// Compare matrices
template<typename T> int TMatrix<T>::numDiffEntries(const TMatrix &other) {
	assert(other.rows() == _rows);
	assert(other.cols() == _cols);

	int res = 0;
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if ((*this)(i, j) != other(i, j)) res++;
		}
	}
	return res;
}

// Obtain a vector of the diagonal elements
template<typename T> std::vector<T> TMatrix<T>::diag_vec() {
	assert(_rows == _cols);
	std::vector<T> result(this->_rows, 0.0);

	for (size_t i = 0; i < this->_rows; i++) { result[i] = (*this)(i, i); }

	return result;
}

// Add to diagonal
template<typename T> void TMatrix<T>::addToDiag(T val) {
	assert(_rows == _cols);
	for (size_t i = 0; i < this->_rows; i++) { (*this)(i, i) += val; }
}

template<typename T> std::vector<T> TMatrix<T>::row(size_t row) const {
	assert(row < _rows);

	auto begin  = _mat.begin() + getLinearIndex<size_t, 2>({row, 0}, {_rows, _cols});
	auto oneRow = std::vector<T>(begin, begin + _cols);
	return oneRow;
}

template<typename T> std::vector<T> TMatrix<T>::col(size_t col) const {
	assert(col < _cols);
	return this->transpose().row(col);
}

// Access the individual elements
template<typename T> T &TMatrix<T>::operator()(size_t row, size_t col) {
	assert(row < _rows);
	assert(col < _cols);
	return _mat[getLinearIndex<size_t, 2>({row, col}, {_rows, _cols})];
}

// Access the individual elements (const)
template<typename T> T TMatrix<T>::operator()(size_t row, size_t col) const {
	assert(row < _rows);
	assert(col < _cols);
	return _mat[getLinearIndex<size_t, 2>({row, col}, {_rows, _cols})];
}

// calculate row sum
template<typename T> T TMatrix<T>::rowSum(size_t row) const {
	assert(row < _rows);

	T sum      = 0.;
	auto start = getLinearIndex<size_t, 2>({row, 0}, {_rows, _cols});
	for (size_t col = 0; col < this->_cols; col++) { sum += _mat[start + col]; }
	return sum;
}

// calculate column sum
template<typename T> T TMatrix<T>::colSum(size_t col) const {
	assert(col < _cols);

	T sum = 0.;
	for (size_t row = 0; row < this->_rows; row++) { sum += operator()(row, col); }
	return sum;
}

// normalize rows such that they sum to one
template<typename T> void TMatrix<T>::normalize_rows() {
	for (size_t r = 0; r < _rows; ++r) {
		double sum = rowSum(r);
		for (size_t c = 0; c < _cols; ++c) { (*this)(r, c) /= sum; }
	}
}

// normalize columns such that they sum to one
template<typename T> void TMatrix<T>::normalize_cols() {
	for (size_t c = 0; c < _cols; ++c) {
		double sum = colSum(c);
		for (size_t r = 0; r < _rows; ++r) { (*this)(r, c) /= sum; }
	}
}

// Get the number of rows of the matrix
template<typename T> size_t TMatrix<T>::rows() const { return _rows; }

// Get the number of columns of the matrix
template<typename T> size_t TMatrix<T>::cols() const { return _cols; }

// Get the number of columns of the matrix
template<typename T> size_t TMatrix<T>::size() const {
	assert(_rows == _cols);
	return _rows;
}

template<typename T> void TMatrix<T>::fillAsExponential(const TBandMatrix<T> &Q) {
	// We get exp(Q) using a poor man's algorithm:
	//  1) Choose a k
	//  2) Devide all entries by 1/(2^k)
	//  3) Square the matrix Q k times

	// 1) Choose k such that 3 * 2^k > numStates AND 1/2^k < 0.1 (i.e. k>3)
	int k = Q.rows() / 3.0;
	k     = log(k) / log(2) + 10;
	if (k < 10) k = 10;

	// 2) Divide all entries by 1 / (2^k)
	double scale = 1.0 / (double)pow(2, k);
	std::array<TBandMatrix<T>, 2> QScaledBand;
	QScaledBand[0].fillFromMatrix(Q, scale);
	QScaledBand[0].addToDiag(1.0);
	int QScaledBandIndex = 0;

	// 3) Square matrix k times.
	// Change from bandMarix to Matrix once the diagonals are filled
	for (int i = 0; i < k; ++i) {
		QScaledBandIndex = 1 - QScaledBandIndex;
		QScaledBand[QScaledBandIndex].fillFromSquare(QScaledBand[1 - QScaledBandIndex]);
	}
	fillFromMatrix(QScaledBand[QScaledBandIndex]);
}

//-------------------------------------------
// TBandMatrix
//-------------------------------------------

// Constructor
template<typename T> TBandMatrix<T>::TBandMatrix() : TMatrix<T>() {
	_bandwidth = 0;
	_zero      = 0;
	_numDiag   = 0;
}

template<typename T> TBandMatrix<T>::TBandMatrix(size_t size, size_t bandwidth) : TMatrix<T>() {
	_initialize(size, bandwidth, 0);
}

template<typename T> TBandMatrix<T>::TBandMatrix(size_t size, size_t bandwidth, T initial) : TMatrix<T>() {
	_initialize(size, bandwidth, initial);
}

// Copy Constructor
template<typename T> TBandMatrix<T>::TBandMatrix(const TBandMatrix<T> &other) : TMatrix<T>() {
	TBandMatrix::fillFromMatrix(other);
}

template<typename T> TBandMatrix<T>::TBandMatrix(const TBandMatrix &other, T scale) : TMatrix<T>() {
	TBandMatrix::fillFromMatrix(other, scale);
}

template<typename T> void TBandMatrix<T>::_initialize(size_t size, size_t bandwidth, T initial) {
	// set size and bandwidth
	if (bandwidth > size) DEVERROR("bandwidth is > than size!");
	_rows      = size;
	_cols      = size;
	_bandwidth = bandwidth;
	_zero      = 0;

	// now compute dimensions of matrix that is stored
	_numDiag = 2 * _bandwidth + 1; // number of rows = number of non-zero diagonals
	_lengthOfDiags.resize(_numDiag);
	for (size_t i = 0; i < _numDiag; i++) {
		_lengthOfDiags[i] =
		    this->_rows -
		    abs(static_cast<int>(i - _bandwidth)); // number of cols = length of each diagonal (differs between diags)
	}
	_mat.resize(containerSum(_lengthOfDiags));

	// re-set values (resize only stretches/squeezes, but does not re-set already set values)
	set(initial);

	this->_initialized = true;
}

template<typename T> bool TBandMatrix<T>::_onBand(size_t row, size_t col) const {
	if (static_cast<int>(row) < static_cast<int>(col - _bandwidth) || row > (col + _bandwidth)) return false;
	return true;
}

// resizing
template<typename T> void TBandMatrix<T>::resize(size_t size, size_t bandwidth) {
	if (!_initialized || bandwidth != _bandwidth || size != this->_rows) _initialize(size, bandwidth, 0);
}

template<typename T> void TBandMatrix<T>::resize(size_t size, size_t bandwidth, T initial) {
	if (!_initialized || bandwidth != _bandwidth || size != this->_rows) _initialize(size, bandwidth, initial);
}

// reset all elements to a single value
template<typename T> void TBandMatrix<T>::set(T value) { std::fill(_mat.begin(), _mat.end(), value); }

template<typename T> size_t TBandMatrix<T>::_getIndexOffBand(size_t row, size_t col) const {
	size_t diag = col + _bandwidth - row;
	if (row < col) { return _getLinearIndex(diag, row); }
	return _getLinearIndex(diag, col);
}

template<typename T> size_t TBandMatrix<T>::_getLinearIndex(size_t Diag, size_t ElementOnDiag) const {
	size_t index = 0;
	for (size_t r = 0; r < Diag; r++) { index += _lengthOfDiags[r]; }
	return index + ElementOnDiag;
}

// Access the individual elements
template<typename T> T &TBandMatrix<T>::operator()(size_t row, size_t col) {
	assert(row < this->_rows);
	assert(col < this->_cols);

	// re-compute index
	if (!_onBand(row, col)) { return _zero; }
	return _mat[_getIndexOffBand(row, col)];
}

// Access the individual elements (const)
template<typename T> T TBandMatrix<T>::operator()(size_t row, size_t col) const {
	assert(row < this->_rows);
	assert(col < this->_cols);

	// re-compute index
	if (!_onBand(row, col)) { return _zero; }
	return _mat[_getIndexOffBand(row, col)];
}

template<typename T> std::vector<T> TBandMatrix<T>::row(size_t row) const {
	assert(row < this->_rows);

	std::vector<T> result;
	result.resize(this->_cols);
	for (size_t col = 0; col < this->_cols; col++) result[col] = (*this)(row, col);
	return result;
}

template<typename T> T TBandMatrix<T>::rowSum(size_t row) const {
	assert(row < this->_rows);

	T sum = 0.;
	for (size_t col = 0; col < this->_cols; col++) sum += (*this)(row, col);
	return sum;
}

template<typename T> T TBandMatrix<T>::colSum(size_t col) const {
	assert(col < this->_cols);

	T sum = 0.;
	for (size_t row = 0; row < this->_rows; row++) sum += (*this)(row, col);
	return sum;
}

template<typename T> T TBandMatrix<T>::atDiag(size_t diag, size_t index) const {
	assert(diag < _numDiag);
	assert(index < _lengthOfDiags[diag]);

	return this->_mat[_getLinearIndex(diag, index)];
}

// fill
template<typename T> void TBandMatrix<T>::fillUniformRandom() {
	for (auto &element : _mat) { element = instances::randomGenerator().getRand(); }
}

template<typename T> void TBandMatrix<T>::fillFromMatrix(const TBandMatrix<T> &other) { fillFromMatrix(other, 1.); }

template<typename T> void TBandMatrix<T>::fillFromMatrix(const TBandMatrix<T> &other, T Scale) {
	// check dimensions
	resize(other.size(), other.bandwidth());
	// copy data and scale
	for (size_t i = 0; i < _mat.size(); i++) { _mat[i] = other._mat[i] * Scale; }
}

template<typename T> void TBandMatrix<T>::fillFromProduct(const TBandMatrix<T> &first, const TBandMatrix<T> &second) {
	// set dimensions and fill with zeros
	if (first.size() != second.size()) DEVERROR("provided matrices are of different size!");
	resize(first.size(), std::min(first.size(), first.bandwidth() + second.bandwidth()), 0);
	set(0.); // need this, in case we don't resize!

	// Now use an approach to multiply band matrices based on diagonals.
	// Specifically, we profit from the fact that A=a1 + ... + an) and B=b1 + ... + bn), where
	// ai and bi are matrices with all zeros except the ith diagonal, which corresponds to the
	// ith diagonal of matrices A and B, respectively. In this case, A*B = (a1 + ... + an) * (b1 + ... + bn)
	// So we have to multiply each pair of diagonals, which will only add to values on a diagonal again.
	int fillDiag, r, s;
	int rAndS;      // r + s
	int i1, i2, ir; // indexes for first, second and resulting diagonal
	int len;        // length of filling diagonal: last index to fill + 1
	for (size_t d1 = 0; d1 < first.numDiag(); ++d1) {
		for (size_t d2 = 0; d2 < second.numDiag(); ++d2) {
			// set r, s: index of diagonal, counting from longest diagonal. Lower is negative
			r        = static_cast<int>(d1) - static_cast<int>(first.bandwidth());
			s        = static_cast<int>(d2) - static_cast<int>(second.bandwidth());
			rAndS    = r + s;
			fillDiag = rAndS + static_cast<int>(_bandwidth); // in internal numbering
			len      = static_cast<int>(this->_rows) - abs(rAndS);
			if (fillDiag >= 0 && fillDiag < static_cast<int>(_numDiag)) {
				// discriminate cases
				if (r >= 0 && s >= 0) { // r,s >= 0
					i1 = 0;
					i2 = r;
					ir = 0;
				} else if (r <= 0 && s <= 0) { // r,s, <= 0
					i1 = -s;
					i2 = 0;
					ir = 0;
				} else if (r < 0) {
					i1 = 0;
					i2 = 0;
					if (r + s < 0) { // r < 0, s > 0, r+s < 0
						ir = len - (static_cast<int>(this->_rows) + r);
					} else { // r < 0, s > 0, r+s >= 0
						ir = len - (static_cast<int>(this->_rows) - s);
					}
				} else {
					ir = 0;
					if (r + s < 0) { // r > 0, s < 0, r+s < 0
						i1  = abs(rAndS);
						i2  = 0;
						len = static_cast<int>(this->_rows) + s;
					} else { // r > 0, s < 0, r+s >= 0
						i1  = 0;
						i2  = abs(rAndS);
						len = static_cast<int>(this->_rows) - r;
					}
				}
				// now fill diagonal
				for (; ir < len; ++ir, ++i1, ++i2) {
					_mat[_getLinearIndex(fillDiag, ir)] += first.atDiag(d1, i1) * second.atDiag(d2, i2);
				}
			}
		}
	}
}

template<typename T> void TBandMatrix<T>::fillAsExponential(const TBandMatrix<T> &Q) {
	// We get exp(Q) using a poor man's algorithm:
	//  1) Choose a k
	//  2) multiply all entries by 1/(2^k)
	//  3) Square the matrix (I+Q*scale) k times

	// 1) Choose k such that 3 * 2^k > numStates AND 1/2^k < 0.1 (i.e. k>3)
	int k = Q.size() / 3.0;
	k     = log2(k) + 10;
	if (k < 4) k = 4;

	// 2) Divide all entries by 1 / (2^k)
	double scale = 1.0 / (double)pow(2, k);
	std::array<TBandMatrix, 2> QScaledBand;
	QScaledBand[0].fillFromMatrix(Q, scale);
	QScaledBand[0].addToDiag(1.0);
	int QScaledBandIndex = 0;

	// 3) Square matrix k-1 times.
	for (int i = 0; i < (k - 1); ++i) {
		QScaledBandIndex = 1 - QScaledBandIndex;
		QScaledBand[QScaledBandIndex].fillFromSquare(QScaledBand[1 - QScaledBandIndex]);
	}

	// last into this storage
	fillFromSquare(QScaledBand[QScaledBandIndex]);
}

// Calculate a transpose of this matrix
template<typename T> TBandMatrix<T> TBandMatrix<T>::transpose() {
	TBandMatrix<T> result(_cols, _bandwidth);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { result(j, i) = (*this)(i, j); }
	}

	return result;
}

template<typename T> TBandMatrix<T> &TBandMatrix<T>::operator=(const TBandMatrix &other) {
	if (&other == this) return *this;

	fillFromMatrix(other);
	return *this;
}

// Addition of two matrices
template<typename T> TBandMatrix<T> TBandMatrix<T>::operator+(const TBandMatrix &other) {
	TBandMatrix<T> result(*this);
	result += other;
	// result.print();

	return result;
}

// Cumulative addition of this matrix and another
template<typename T> TBandMatrix<T> &TBandMatrix<T>::operator+=(const TBandMatrix &other) {
	assert(_rows == other.rows());
	assert(_cols == other.cols());

	// if bandwidth of other is larger -> we would need to change size of this-matrix
	// easier to add this to other, as this does not require resizing
	if (other.bandwidth() > _bandwidth) {
		TBandMatrix<T> result(other);
		result += *this;
		(*this) = result;
		return *this;
	}

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { (*this)(i, j) += other(i, j); }
	}

	return *this;
}

// Subtraction of this matrix and another
template<typename T> TBandMatrix<T> TBandMatrix<T>::operator-(const TBandMatrix &other) {
	TBandMatrix<T> result(*this);
	result -= other;

	return result;
}

// Cumulative subtraction of this matrix and another
template<typename T> TBandMatrix<T> &TBandMatrix<T>::operator-=(const TBandMatrix &other) {
	assert(other.rows() == _rows);
	assert(other.cols() == _cols);

	// if bandwidth of other is larger -> we would need to change size of this-matrix
	// easier to subtract this to other, as this does not require resizing
	if (other.bandwidth() > _bandwidth) {
		TBandMatrix<T> result(other);
		result -= *this;
		(*this) = result;
		return *this;
	}

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) { (*this)(i, j) -= other(i, j); }
	}

	return *this;
}

// Left multiplication of this matrix and another (matrix multiplication, equivalent to this %*% other in R)
template<typename T> TBandMatrix<T> TBandMatrix<T>::operator*(const TBandMatrix &other) const {
	TBandMatrix<T> result;
	result.fillFromProduct(*this, other);
	return result;
}

// Cumulative left multiplication of this matrix and another (matrix multiplication, equivalent to this %*% other in R)
template<typename T> TBandMatrix<T> &TBandMatrix<T>::operator*=(const TBandMatrix &other) {
	TBandMatrix<T> result = (*this) * other;
	(*this)               = result;
	return *this;
}

// Matrix/scalar addition
template<typename T> TBandMatrix<T> TBandMatrix<T>::operator+(T other) {
	TBandMatrix<T> result(_rows, _bandwidth);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if (_onBand(i, j)) result(i, j) = (*this)(i, j) + other;
		}
	}

	return result;
}

template<typename T> TBandMatrix<T> TBandMatrix<T>::operator+=(T other) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if (_onBand(i, j)) (*this)(i, j) += other;
		}
	}

	return *this;
}

// Matrix/scalar subtraction
template<typename T> TBandMatrix<T> TBandMatrix<T>::operator-(T other) {
	TBandMatrix<T> result(_rows, _bandwidth);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if (_onBand(i, j)) result(i, j) = (*this)(i, j) - other;
		}
	}

	return result;
}

template<typename T> TBandMatrix<T> TBandMatrix<T>::operator-=(T other) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if (_onBand(i, j)) (*this)(i, j) -= other;
		}
	}

	return *this;
}

// Matrix/scalar multiplication
template<typename T> TBandMatrix<T> TBandMatrix<T>::operator*(T other) {
	TBandMatrix<T> result(_rows, _bandwidth);
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if (_onBand(i, j)) result(i, j) = (*this)(i, j) * other;
		}
	}

	return result;
}

template<typename T> TBandMatrix<T> TBandMatrix<T>::operator*=(T other) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if (_onBand(i, j)) (*this)(i, j) *= other;
		}
	}

	return *this;
}

// Matrix/scalar division
template<typename T> TBandMatrix<T> TBandMatrix<T>::operator/(T other) {
	TBandMatrix<T> result(_rows, _bandwidth);

	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if (_onBand(i, j)) result(i, j) = (*this)(i, j) / other;
		}
	}

	return result;
}

template<typename T> TBandMatrix<T> TBandMatrix<T>::operator/=(T other) {
	for (size_t i = 0; i < _rows; i++) {
		for (size_t j = 0; j < _cols; j++) {
			if (_onBand(i, j)) (*this)(i, j) /= other;
		}
	}

	return *this;
}

// Multiply a matrix with a vector
template<typename T> std::vector<T> TBandMatrix<T>::operator*(const std::vector<T> &other) {
	return TMatrix<T>::operator*(other);
}

// getters
template<typename T> size_t TBandMatrix<T>::bandwidth() const { return _bandwidth; }

template<typename T> size_t TBandMatrix<T>::numDiag() const { return _numDiag; }

template<typename T> std::vector<size_t> TBandMatrix<T>::lengthOfDiags() const { return _lengthOfDiags; }

template<typename T> size_t TBandMatrix<T>::size() const { return this->_rows; }

}; // namespace coretools
