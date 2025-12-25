# include "mathfunc.h"

/*
 * assert a number is prime
 */
bool isPrime(int n)
{
  if(n <= 1) return false;
  if(n <= 3) return true;
  if (n%2 == 0 || n%3 == 0) return false;
  for (int i=5; i*i<=n; i=i+6)
    if (n%i == 0 || n%(i+2) == 0)
      return false;
    return true;
}

/*
* find out the maximum prime number less than given n
*/
int reportMaxPrime(int n)
{
  int MaxPrime(0);
  for(int i = 0; i <= n; i++)
  {
    if(isPrime(i))
    {
      MaxPrime = MaxTwo(MaxPrime, i);
    }
  }
  return MaxPrime;
}

/*
* Fast Fourier Transform
*/
ComplexVector FFT(NumericVector& x){
  Environment base("package:stats");
  Function fft = base["fft"];
  ComplexVector res = fft(_["z"] = x);
  return res;
}

/*
* Inverse transform of Fast Fourier Transform
*/
ComplexVector IFFT(ComplexVector& x){
  Environment base("package:stats");
  Function fft = base["fft"];
  ComplexVector res = fft(_["z"] = x, _["inverse"] = true);
  return res;
}

/*
* Omega function
*/
double calOmega(double x){
  return 2.0 * PI * PI * (x * x - x + 1.0 / 6.0);
} 

/*
* Overloading Omega function
*/
VectorXd calOmega(VectorXd & x)
{
  VectorXd res(x.size());
  for(int i = 0; i < x.size(); i++)
  {
    res(i) = calOmega(x(i));
  }
  return res;
}

/*
* Overloading Omega function
*/
NumericVector calOmega(NumericVector & x)
{
  NumericVector res(x.size());
  for(int i = 0; i < x.size(); i++)
  {
    res(i) = calOmega(x(i));
  }
  return res;
}


/*
* Calculates x ^ a (mod n) using the well known “Russian peasant method”
*/
int POWMOD(int x, int a, int n)
{
  int y(1);
  int u(x);
  while(a > 0){
    if(a % 2 == 1){
      y = (y * u) % n;
    }
    u = (u * u) % n;
    a = floor(a / 2.0);
  }
  return y;
}

/*
* 
*/
int findPrimefactors(unordered_set<int> & s, int n)
{
  while(n%2 == 0)
  {
    s.insert(2);
    n = n / 2;
  }
  
  for(int i = 3; i < sqrt(n); i = i + 2)
  {
    while(n % i == 0)
    {
      s.insert(i);
      n = n / i;
    }
  }
  if(n > 2)
    s.insert(n);
  return 1;
}

/*
* find out all uniqual primes numbers fo factor the number
*/
VectorXd findFactorize(int n)
{
  unordered_set<int> s;
  findPrimefactors(s, n);
  VectorXd res(s.size());
  int i(0);
  for(auto it = s.begin(); it != s.end(); it++)
  {
    res(i) = (*it);
    i++;
  }
  return res;
}

/*
* get all irreducible factors of an integer
*/
int generateOrp(int n)
{
  try{
    if(!isPrime(n))
    {
      throw std::invalid_argument("n is not a prime");
    }
    VectorXd primef = findFactorize(n - 1);
    int g(2), i(1);
    while(i <= primef.size())
    {
      if(POWMOD(g, (n - 1) / primef(i - 1), n) == 1)
      {
        g++;
        i = 0;
      }
      i++;
    }
    return g;
  }catch(std::exception &ex){
    forward_exception_to_r(ex);
    return -1;
  }
}

/*
* return the index (c++, begin with 0) with minimum value
*/
int argMin(VectorXd & x)
{
  // std::ptrdiff_t i;
  MatrixXd::Index i;
  x.minCoeff(&i);
  return i;
}
/*
* return the index (c++, begin with 0) with maximum value
*/
int argMax(VectorXd & x)
{
  // std::ptrdiff_t i;
  MatrixXd::Index i;
  x.maxCoeff(&i);
  return i;
}

/*
* get the element-wise minimum between two vectors
*/
VectorXd compareMin(VectorXd & x, VectorXd & y)
{
  VectorXd res(x.size());
  for(int i = 0; i < x.size(); i++)
  {
    res(i) = minTwo(x(i), y(i));
  }
  return res;
}

/*
* compute distances between two point lists
*/
MatrixXd fastPdist(MatrixXd & x, MatrixXd & y)
{
  int n(x.rows()), m(y.rows());
  MatrixXd tempX = x.array().square();
  MatrixXd tempY = y.array().square();
  VectorXd Xn = tempX.rowwise().sum();
  VectorXd Yn = tempY.rowwise().sum();
  MatrixXd Cn = -2.0 * (x * y.transpose());
  for(int i = 0; i < m; i++)
  {
    Cn.col(i) += Xn;
  }
  for(int i = 0; i < n; i++)
  {
    Cn.row(i) += Yn;
  }
  return Cn.array().max(0.0).sqrt();
}


/*
* overloading distance function
*/
VectorXd fastPdist(MatrixXd & x, VectorXd & y)
{
  MatrixXd tempX = x.array().square();
  double Yn = y.transpose() * y;
  VectorXd Xn = tempX.rowwise().sum();
  VectorXd Cn = -2.0 * (x * y);
  Cn = Cn + Xn;
  Cn = Cn.array() + Yn;
  return Cn.array().max(0.0).sqrt();
}

/*
* compute quantiles of data
*/
VectorXd quantileCPP(VectorXd & x, VectorXd & q)
{
  Environment stats("package:stats");
  Function quantile = stats["quantile"];
  // int npr = q.size();
  Map<VectorXd> res(as<Map<VectorXd>>(quantile(x, q)));
  return res;
}

/*
* compute the variance-covariance matrix
*/
MatrixXd varCPP(MatrixXd & x)
{
  VectorXd colMean = x.colwise().mean();
  MatrixXd diffMatrix = x - MatrixXd::Constant(x.rows(), x.cols(), 1.0) * colMean.asDiagonal();
  return diffMatrix.transpose() * diffMatrix / (x.rows() - 1);
}

/*
* permutation of sequence
*/
VectorXi sampleCPP(int dim)
{
  Environment stats("package:base");
  Function sample = stats["sample"];
  Map<VectorXi> res(as<Map<VectorXi>>(sample(dim)));
  return res.array() - 1;
}


/*
* extract sub-columns of matrix
*/
MatrixXd subMatCols(MatrixXd & x, VectorXi & ID)
{
  MatrixXd res(x.rows(), ID.size());
  for(int i = 0; i < ID.size(); i++)
  {
    res.col(i) = x.col(ID(i));
  }
  return res;
}

/*
* extract sub-rows of matrix
*/
MatrixXd subMatRows(MatrixXd & x, VectorXi ID)
{
  MatrixXd res(ID.size(), x.cols());
  for(int i = 0; i < ID.size(); i++)
  {
    res.row(i) = x.row(ID(i));
  }
  return res;
}

/*
* re-set diagonal values of matrix
*/
MatrixXd setDiagonal(MatrixXd & x, double y)
{
  x.diagonal() = ArrayXd::Ones(x.diagonal().size()) * y;
  return x;
}

/*
* order function
*/
VectorXi orderCPP(VectorXd & x)
{
  Environment stats("package:base");
  Function order = stats["order"];
  Map<VectorXi> res(as<Map<VectorXi>>(order(x)));
  return res.array() - 1;
}

/*
* rbind matrixs
*/
MatrixXd bindMatByRows(MatrixXd & x, MatrixXd & y)
{
  MatrixXd tempX = x;
  tempX.conservativeResize(x.rows() + y.rows(), x.cols());
  tempX.bottomRows(y.rows()) = y;
  return tempX;
}

/*
* remove one row from matrix
*/
MatrixXd removeMatRow(MatrixXd & x, int RowID)
{
  if(RowID >= x.rows() || RowID < 0)
  {
    return x;
  }
  MatrixXd res(x.rows() - 1, x.cols());
  res.topRows(RowID) = x.topRows(RowID);
  if(RowID < x.rows() - 1)
  {
    res.bottomRows(x.rows() - RowID - 1) = x.bottomRows(x.rows() - RowID - 1);
  }
  return res;
}

/*
* bind matrix and vector
*/
MatrixXd bindMatByRows(MatrixXd & x, VectorXd & y)
{
  MatrixXd tempX = x;
  tempX.conservativeResize(x.rows() + 1, x.cols());
  tempX.bottomRows(1) = y.transpose();
  return tempX;
}

template<typename Derived>
MatrixXd bindMatByRows(MatrixXd & x, Eigen::MatrixBase<Derived> & y)
{
  MatrixXd tempX = x;
  tempX.conservativeResize(x.rows() + 1, x.cols());
  tempX.bottomRows(1) = y;
  return tempX;
}

/*
* remove one column from matrix
*/
MatrixXd removeMatCol(MatrixXd & x, int ColID)
{
  if(ColID >= x.cols() || ColID < 0)
  {
    return x;
  }
  MatrixXd res(x.rows(), x.cols() - 1);
  res.leftCols(ColID) = x.leftCols(ColID);
  if(ColID < x.cols() - 1)
  {
    res.rightCols(x.cols() - ColID - 1) = x.rightCols(x.cols() - ColID - 1);
  }
  return res;
}

/*
* remove row and column
*/
MatrixXd removeMatRowCol(MatrixXd & x, int RowID, int ColID)
{
  MatrixXd res = removeMatRow(x, RowID);
  return removeMatCol(res, ColID);
}

/*
* bind matixs by column
*/
MatrixXd bindMatByCols(MatrixXd & x, MatrixXd & y)
{
  MatrixXd tempX = x;
  tempX.conservativeResize(x.rows(), x.cols() + y.cols());
  tempX.rightCols(y.cols()) = y;
  return tempX;
}

/*
* bind matix and vector by column
*/
MatrixXd bindMatByCols(MatrixXd & x, VectorXd & y)
{
  MatrixXd tempX = x;
  tempX.conservativeResize(x.rows(), x.cols() + 1);
  tempX.rightCols(1) = y;
  return tempX;
}

template<typename Derived>
MatrixXd bindMatByCols(MatrixXd & x, Eigen::MatrixBase<Derived> & y)
{
  MatrixXd tempX = x;
  tempX.conservativeResize(x.rows(), x.cols() + 1);
  tempX.rightCols(1) = y;
  return tempX;
}

/*
* extract sub-elements of one vector
*/
VectorXd subVectElements(const VectorXd & x, const VectorXi EleID)
{
  VectorXd res(EleID.size());
  for(int i = 0; i < EleID.size(); i++)
  {
    res(i) = x(EleID(i));
  }
  return res;
}

/*
* bind vectors by row
*/
VectorXd bindVectorByRow(const VectorXd & x, const VectorXd & y)
{
  VectorXd res = x;
  res.conservativeResize(x.size() + y.size(), 1);
  res.tail(y.size()) = y;
  return res;
}
