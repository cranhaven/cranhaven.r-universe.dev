/*
 * Math functions which will be used in the procedure of generating MED points.
 */

# ifndef MATHFUNCTIONS_H_
# define MATHFUNCTIONS_H_
# include <RcppEigen.h>
# include <unordered_set>
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppEigen;
using namespace Eigen;
using namespace std;

#define minTwo(a, b) (a<b? a:b)
#define MaxTwo(a, b) (a>b? a:b)


# define PI 3.1415926

/*
 * assert a number is prime
 */
bool isPrime(int n);

/*
 * find out the maximum prime number less than given n
 */
int reportMaxPrime(int n);

/*
 * Fast Fourier Transform
 */
ComplexVector FFT(NumericVector& x);

/*
 * Inverse transform of Fast Fourier Transform
 */
ComplexVector IFFT(ComplexVector& x);

/*
 * Omega function
 */
double calOmega(double x);

/*
 * Overloading Omega function
 */
VectorXd calOmega(VectorXd & x);

/*
 * Overloading Omega function
 */
NumericVector calOmega(NumericVector & x);


/*
 * Calculates x ^ a (mod n) using the well known “Russian peasant method”
 */
int POWMOD(int x, int a, int n);

/*
 * 
 */
int findPrimefactors(unordered_set<int> & s, int n);

/*
 * find out all uniqual primes numbers fo factor the number
 */
VectorXd findFactorize(int n);
/*
 * get all irreducible factors of an integer
 */
int generateOrp(int n);

/*
 * return the index (c++, begin with 0) with minimum value
 */
int argMin(VectorXd & x);
/*
 * return the index (c++, begin with 0) with maximum value
 */
int argMax(VectorXd & x);

/*
 * get the element-wise minimum between two vectors
 */
VectorXd compareMin(VectorXd & x, VectorXd & y);

/*
 * compute distances between two point lists
 */
MatrixXd fastPdist(MatrixXd & x, MatrixXd & y);

/*
 * overloading distance function
 */
VectorXd fastPdist(MatrixXd & x, VectorXd & y);

/*
 * compute quantiles of data
 */
VectorXd quantileCPP(VectorXd & x, VectorXd & q);

/*
 * compute the variance-covariance matrix
 */
MatrixXd varCPP(MatrixXd & x);
/*
 * permutation of sequence
 */
VectorXi sampleCPP(int dim);

/*
 * extract sub-columns of matrix
 */
MatrixXd subMatCols(MatrixXd & x, VectorXi & ID);

/*
 * extract sub-rows of matrix
 */
MatrixXd subMatRows(MatrixXd & x, VectorXi ID);

/*
 * re-set diagonal values of matrix
 */
MatrixXd setDiagonal(MatrixXd & x, double y);

/*
 * order function
 */
VectorXi orderCPP(VectorXd & x);
/*
 * rbind matrixs
 */
MatrixXd bindMatByRows(MatrixXd & x, MatrixXd & y);

/*
 * remove one row from matrix
 */
MatrixXd removeMatRow(MatrixXd & x, int RowID);

/*
 * bind matrix and vector
 */
MatrixXd bindMatByRows(MatrixXd & x, VectorXd & y);

template<typename Derived>
MatrixXd bindMatByRows(MatrixXd & x, Eigen::MatrixBase<Derived> & y);

/*
 * remove one column from matrix
 */
MatrixXd removeMatCol(MatrixXd & x, int ColID);

/*
 * remove row and column
 */
MatrixXd removeMatRowCol(MatrixXd & x, int RowID, int ColID);

/*
 * bind matixs by column
 */
MatrixXd bindMatByCols(MatrixXd & x, MatrixXd & y);

/*
 * bind matix and vector by column
 */
MatrixXd bindMatByCols(MatrixXd & x, VectorXd & y);

template<typename Derived>
MatrixXd bindMatByCols(MatrixXd & x, Eigen::MatrixBase<Derived> & y);
/*
 * extract sub-elements of one vector
 */
VectorXd subVectElements(const VectorXd & x, const VectorXi EleID);

/*
 * bind vectors by row
 */
VectorXd bindVectorByRow(const VectorXd & x, const VectorXd & y);
# endif
