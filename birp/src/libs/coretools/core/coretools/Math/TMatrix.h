//
// Created by madleina on 05.08.20.
//

#ifndef TMATRIX_H_
#define TMATRIX_H_

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

namespace coretools {

//-------------------------------------------
// TMatrix
//-------------------------------------------

// forward declaration
template<typename T> class TBandMatrix;

template<typename T> class TMatrix {
	// rectangular matrix
	// certain functions (i.e. diagonals) only make sense for square matrices
	//     -> those use assert to check if rows==cols
protected:
	std::vector<T> _mat;
	size_t _rows;
	size_t _cols;
	bool _initialized;

	void _initialize(T initial);

public:
	TMatrix();
	TMatrix(int size);
	TMatrix(int size, T initial);
	TMatrix(int rows, int cols);
	TMatrix(int rows, int cols, T initial);
	TMatrix(const TMatrix<T> &other);
	TMatrix(const TMatrix<T> &other, T scale);
	TMatrix(TMatrix<T> &&other) noexcept;
	TMatrix<T> &operator=(TMatrix<T> &&other) noexcept;
	virtual ~TMatrix();

	// resizing
	void clear();
	void resize(int size);
	void resize(int size, T initial);
	void resize(int rows, int cols);
	void resize(int rows, int cols, T initial);
	void zeros(int size);
	void zeros(int rows, int cols);

	// reset
	virtual void set(T value);

	// Matrix-Matrix operations
	TMatrix<T> &operator=(const TMatrix<T> &other);
	TMatrix<T> operator+(const TMatrix<T> &other);
	TMatrix<T> &operator+=(const TMatrix<T> &other);
	TMatrix<T> operator-(const TMatrix<T> &other);
	TMatrix<T> &operator-=(const TMatrix<T> &other);
	TMatrix<T> operator*(const TMatrix<T> &other) const;
	TMatrix<T> &operator*=(const TMatrix<T> &other);

	// transpose
	TMatrix<T> transpose() const;

	// fill randomly
	virtual void fillUniformRandom();

	// fill from vector
	void fillRowsFromVector(const std::vector<T> &Vec, size_t NumRows);

	// fill from other TMatrix
	void fillFromMatrix(const TMatrix<T> &other);
	void fillFromMatrix(const TMatrix<T> &other, T Scale);
	void fillFromProduct(const TMatrix<T> &first, const TMatrix<T> &second);
	void fillFromSquare(const TMatrix<T> &other);

	// fill from other TBandMatrix
	virtual void fillFromMatrix(const TBandMatrix<T> &other);
	virtual void fillFromMatrix(const TBandMatrix<T> &other, T Scale);
	virtual void fillFromProduct(const TBandMatrix<T> &first, const TBandMatrix<T> &second);
	virtual void fillFromSquare(const TBandMatrix<T> &other);
	virtual void fillAsExponential(const TBandMatrix<T> &Q);

	// Matrix-scalar operations
	TMatrix<T> operator+(T val);
	TMatrix<T> operator+=(T val);
	TMatrix<T> operator-(T val);
	TMatrix<T> operator-=(T val);
	TMatrix<T> operator*(T val);
	TMatrix<T> operator*=(T val);
	TMatrix<T> operator/(T val);
	TMatrix<T> operator/=(T val);
	void addToDiag(T val);

	// Matrix-vector operations
	virtual std::vector<T> operator*(const std::vector<T> &other);

	// compare
	int numDiffEntries(const TMatrix<T> &other);

	// Access data
	virtual T &operator()(size_t row, size_t col);
	virtual T operator()(size_t row, size_t col) const;
	std::vector<T> diag_vec();
	virtual std::vector<T> row(size_t row) const;
	std::vector<T> col(size_t col) const;

	// operations
	virtual T rowSum(size_t row) const;
	virtual T colSum(size_t col) const;

	// normalize to sum 1
	void normalize_rows();
	void normalize_cols();

	// getters
	size_t rows() const;
	size_t cols() const;
	virtual size_t size() const;
	const size_t &n_rows = _rows; // mimics armadillo interface, read-only
	const size_t &n_cols = _cols; // mimics armadillo interface, read-only
};

//-------------------------------------------
// TBandMatrix
//-------------------------------------------

template<typename T> class TBandMatrix : public TMatrix<T> {
	// a band matrix is a sparse matrix where only the main diagonal (+ possibly more diagonals on either side)
	// have non-zero entries
	// The diagonals are numbered 0:(2*bandwidth) with the bandwidth^th diagonal being the longest one

	// need to re-define all functions that return TMatrix in base class
	//      (this is ugly, but other options are worse:
	//       -> use dynamic_cast to cast to derived class is slow
	//       -> use CRTP is complicated, and I'm not sure if this will be 1) fast and 2) possible for all functions
	//       (https://en.m.wikipedia.org/wiki/Curiously_recurring_template_pattern)
	//       -> use free functions is super hard to read and to debug
	//       (https://stackoverflow.com/questions/41652069/inherited-functions-to-return-derived-class-not-base-class)

protected:
	using TMatrix<T>::_mat;
	using TMatrix<T>::_rows;
	using TMatrix<T>::_cols;
	using TMatrix<T>::_initialized;

	size_t _bandwidth;
	std::vector<size_t> _lengthOfDiags;
	size_t _numDiag;
	T _zero; // needed for const return

	void _initialize(size_t size, size_t bandwidth, T initial);
	bool _onBand(size_t row, size_t col) const;
	size_t _getIndexOffBand(size_t row, size_t col) const;
	size_t _getLinearIndex(size_t Diag, size_t ElementOnDiag) const;

public:
	TBandMatrix();
	TBandMatrix(size_t size, size_t bandwidth);
	TBandMatrix(size_t size, size_t bandwidth, T initial);
	TBandMatrix(const TBandMatrix<T> &other);
	TBandMatrix(const TBandMatrix<T> &other, T scale);
	~TBandMatrix() override = default;

	// resizing
	void resize(size_t size, size_t bandwidth);
	void resize(size_t size, size_t bandwidth, T initial);

	// reset
	void set(T value) override;

	// Matrix-Matrix operations
	TBandMatrix<T> &operator=(const TBandMatrix<T> &other);
	TBandMatrix<T> operator+(const TBandMatrix<T> &other);
	TBandMatrix<T> &operator+=(const TBandMatrix<T> &other);
	TBandMatrix<T> operator-(const TBandMatrix<T> &other);
	TBandMatrix<T> &operator-=(const TBandMatrix<T> &other);
	TBandMatrix<T> operator*(const TBandMatrix<T> &other) const;
	TBandMatrix<T> &operator*=(const TBandMatrix<T> &other);

	// transpose
	TBandMatrix<T> transpose();

	// Matrix-scalar operations
	TBandMatrix<T> operator+(T val);
	TBandMatrix<T> operator+=(T val);
	TBandMatrix<T> operator-(T val);
	TBandMatrix<T> operator-=(T val);
	TBandMatrix<T> operator*(T val);
	TBandMatrix<T> operator*=(T val);
	TBandMatrix<T> operator/(T val);
	TBandMatrix<T> operator/=(T val);

	// Matrix-vector operations
	std::vector<T> operator*(const std::vector<T> &other) override;

	// fill randomly
	void fillUniformRandom() override;

	// fill from other TBandMatrix
	void fillFromMatrix(const TBandMatrix<T> &other) override;
	void fillFromMatrix(const TBandMatrix<T> &other, T Scale) override;
	void fillFromProduct(const TBandMatrix<T> &first, const TBandMatrix<T> &second) override;
	void fillFromSquare(const TBandMatrix<T> &other) override { fillFromProduct(other, other); };
	virtual void fillAsExponential(const TBandMatrix<T> &Q) override;

	// Access the individual elements
	T &operator()(size_t row, size_t col) override;
	T operator()(size_t row, size_t col) const override;
	T atDiag(size_t diag, size_t index) const;
	std::vector<T> row(size_t row) const override;

	// operations
	T rowSum(size_t row) const override;
	T colSum(size_t row) const override;

	// getters
	size_t bandwidth() const;
	size_t numDiag() const;
	std::vector<size_t> lengthOfDiags() const;
	size_t size() const override;
};

}; // namespace coretools

#include "TMatrix.tpp"

#endif // TMATRIX_H_
