// [[Rcpp::depends(BH)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <iostream>
#include <cmath>
#include <vector>
#include <float.h>
#include <boost/any.hpp>
// #include <omp.h>
#include <time.h>
#include <limits.h>
#include <sstream>
#include <string>
#include <random>
// #ifdef _OPENMP
// #include <omp.h>
// #endif

using namespace Rcpp;
using namespace std;
using namespace arma;

double m_eps = pow(10.0,-10.0);


//--------------------------------------------------------------
//Progress bar
//--------------------------------------------------------------
void flush_console() {
#if !defined(WIN32) && !defined(__WIN32) && !defined(__WIN32__)
  R_FlushConsole();
#endif
}

// [[Rcpp::export]]
void printBar(double prop){
  
  int barWidth = 70;
  
  // std::cout.flush();
  flush_console();
  Rcout << "[";
  int pos = barWidth * prop;
  for (int i = 0; i < barWidth; ++i) {
    if (i < pos) Rcout << "=";
    else if (i == pos) Rcout << ">";
    else Rcout << " ";
  }
  Rcout << "] " << int(prop * 100.0) << " %\r";
  
}



//-------------------------------------------------------------------------------
// Random number generators for different distributions
//-------------------------------------------------------------------------------
namespace sftrabbit {

template <typename RealType = double>
class beta_distribution
{
public:
  typedef RealType result_type;
  
  class param_type
  {
  public:
    typedef beta_distribution distribution_type;
    
    explicit param_type(RealType a = 2.0, RealType b = 2.0)
      : a_param(a), b_param(b) { }
    
    RealType a() const { return a_param; }
    RealType b() const { return b_param; }
    
    bool operator==(const param_type& other) const
    {
      return (a_param == other.a_param &&
              b_param == other.b_param);
    }
    
    bool operator!=(const param_type& other) const
    {
      return !(*this == other);
    }
    
  private:
    RealType a_param, b_param;
  };
  
  explicit beta_distribution(RealType a = 2.0, RealType b = 2.0)
    : a_gamma(a), b_gamma(b) { }
  explicit beta_distribution(const param_type& param)
    : a_gamma(param.a()), b_gamma(param.b()) { }
  
  void reset() { }
  
  param_type param() const
  {
    return param_type(a(), b());
  }
  
  void param(const param_type& param)
  {
    a_gamma = gamma_dist_type(param.a());
    b_gamma = gamma_dist_type(param.b());
  }
  
  template <typename URNG>
  result_type operator()(URNG& engine)
  {
    return generate(engine, a_gamma, b_gamma);
  }
  
  template <typename URNG>
  result_type operator()(URNG& engine, const param_type& param)
  {
    gamma_dist_type a_param_gamma(param.a()),
    b_param_gamma(param.b());
    return generate(engine, a_param_gamma, b_param_gamma); 
  }
  
  result_type min() const { return 0.0; }
  result_type max() const { return 1.0; }
  
  result_type a() const { return a_gamma.alpha(); }
  result_type b() const { return b_gamma.alpha(); }
  
  bool operator==(const beta_distribution<result_type>& other) const
  {
    return (param() == other.param() &&
            a_gamma == other.a_gamma &&
            b_gamma == other.b_gamma);
  }
  
  bool operator!=(const beta_distribution<result_type>& other) const
  {
    return !(*this == other);
  }
  
private:
  typedef std::gamma_distribution<result_type> gamma_dist_type;
  
  gamma_dist_type a_gamma, b_gamma;
  
  template <typename URNG>
  result_type generate(URNG& engine,
                       gamma_dist_type& x_gamma,
                       gamma_dist_type& y_gamma)
  {
    result_type x = x_gamma(engine);
    return x / (x + y_gamma(engine));
  }
};

template <typename CharT, typename RealType>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os,
                                      const beta_distribution<RealType>& beta)
{
  os << "~Beta(" << beta.a() << "," << beta.b() << ")";
  return os;
}

template <typename CharT, typename RealType>
std::basic_istream<CharT>& operator>>(std::basic_istream<CharT>& is,
                                      beta_distribution<RealType>& beta)
{
  std::string str;
  RealType a, b;
  if (std::getline(is, str, '(') && str == "~Beta" &&
      is >> a && is.get() == ',' && is >> b && is.get() == ')') {
    beta = beta_distribution<RealType>(a, b);
  } else {
    is.setstate(std::ios::failbit);
  }
  return is;
}

}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
NumericVector csample_num( NumericVector x,
                           int size,
                           bool replace) {
  //Sampling from vector
  NumericVector prob(x.size());
  for (int i=0;i<x.size();i++){
    prob[i] = 1.0/x.size();
  }
  NumericVector ret = Rcpp::RcppArmadillo::sample(x, size, replace, prob);
  return (ret);
}

//-------------------------------------------------------------------------------
// Functions for computing energy criterion
//-------------------------------------------------------------------------------

double sgn(double val) {
  int ret = (val > 0.0) - (val < 0.0);
  return( (double) ret);
}

// Computing the energy criterion using a Monte-Carlo sample
// [[Rcpp::export]]
double energycrit(NumericMatrix& Rcpp_point, NumericMatrix& Rcpp_des) {
  //Rcpp_point - Approximating points in projected subspace
  //Rcpp_des   - Design in full design space
  //cn         - Combination matrix
  //num_proc   - Number of processors available

  // int num_combn = cn.nrow();
  // NumericVector ret(num_combn);
  double ret = 0.0;

  //Computes the energy criterion under L_pw norm
  int dim_num = Rcpp_point.ncol(); //dimension of projected subspace
  int point_num = Rcpp_point.nrow(); //number of approximating points
  int des_num = Rcpp_des.nrow(); //number of design points

  // #pragma omp parallel for
  // for (int l=0; l<num_combn; l++){
    double runcrit = 0.0;
    for (int i=0; i<des_num; i++){
      for (int j=0; j<point_num; j++){
        double runcrittmp = 0.0;
        for (int k=0; k<dim_num; k++){
          runcrittmp += pow(Rcpp_point(j,k) - Rcpp_des(i,k), 2.0);
        }
        runcrit += sqrt(runcrittmp);
      }
    }
    runcrit = runcrit * (2.0/(des_num*point_num));

    double runcrit2 = 0.0;
    for (int i=0; i<des_num; i++){
      for (int j=0; j<des_num; j++){
        double runcrittmp2 = 0.0;
        for (int k=0; k<dim_num; k++){
          runcrittmp2 += pow(Rcpp_des(i,k) - Rcpp_des(j,k), 2.0);
        }
        runcrit2 += sqrt(runcrittmp2);
      }
    }
    runcrit2 = runcrit2 / ((double)(des_num*des_num));
  // }

  ret = runcrit - runcrit2;
  return(ret);

}

// A faster C++ implementation of L2-discrepancy
// [[Rcpp::export]]
NumericVector starL2cpp(NumericMatrix& D, NumericMatrix& cn, int num_proc) {

  //int dimen = D.ncol();
  int n = D.nrow();
  int dimen = cn.ncol();
  NumericVector ret(cn.nrow());
  // omp_set_num_threads(num_proc);

// #pragma omp parallel for
  for (int l=0; l<cn.nrow(); l++){
    double term1 = pow(3.0,-dimen);
    double runsum = 0.0;
    for (int i=0; i<n; i++){
      double runprod = 1.0;
      for (int j=0; j<dimen; j++){
        runprod = runprod * (1 - pow(D(i,cn(l,j)-1),2) );
      }
      runsum += runprod;
    }
    double term2 = (pow(2.0,1-dimen)/n) * runsum;

    runsum = 0.0;
    for (int i=0; i<n; i++){
      for (int j=0; j<n; j++){
        double runprod = 1.0;
        for (int k=0; k<dimen; k++){
          runprod = runprod * (1-max(D(i,cn(l,k)-1),D(j,cn(l,k)-1)));
        }
        runsum += runprod;
      }
    }
    double term3 = (1/pow(n,2.0))*runsum;

    //             cout << term1 << ", " << term2 << ", " << term3 << endl;

    ret(l) = sqrt(term1-term2+term3);
  }

  return (ret);

}

// A faster C++ implementation of the energy criterion for the standard normal distribution
// [[Rcpp::export]]
NumericVector energy_norm_cpp(NumericMatrix& yMat, NumericMatrix& cn, int num_proc)
{
  /*
  Computes the approximate eigenvalues of $W_infty$ using Monte-Carlo
  Modified from the R package 'energy' - Rizzo (2014)
  */

  //Vectorize yMat
  // int d = yMat.ncol(); //number of data points
  int n = yMat.nrow(); //number of clusters
  int d = cn.ncol();
  NumericVector retvec(cn.nrow());
  // omp_set_num_threads(num_proc);

// #pragma omp parallel for
  for (int l=0; l<cn.nrow(); l++){
    std::vector<double> y(d*n);
    for (int i=0; i<n; i++){
      for (int j=0; j<d; j++){
        y[i*d+j] = yMat(i,cn(l,j)-1);
      }
    }
    //    int    d=(*dim), n=(*nobs);
    int    i, j, k, p, maxterms=2000;
    double D=(double)d;
    double meanyy, meanzz;
    double delta, eps=1.0e-7;
    double normy, yy, dif, sum, sum0, term;
    double lg0, lg1,logak, loggk;

    //Second mean
    lg0 = lgamma(D/2.0);
    lg1 = lgamma((D+1.0)/2.0);
    meanzz = 2.0 * exp(lg1 - lg0);

    //Compting the vector of first mean as series
    arma::vec meanyz(n);
    for (i=0; i<n; i++) {
      yy = 0.0;
      for (j=0; j<d; j++) {
        dif = y[i*d+j] * y[i*d+j];
        yy += dif;
      }
      normy = sqrt(yy);
      delta = 1.0;
      sum = 0.0;
      k = 0;
      while (delta > eps && k < maxterms) {
        sum0 = sum;
        logak = (k+1)*log(yy) - lgamma(k+1) - k*M_LN2 -
          log(2*k+1) - log(2*k+2);
        loggk = lg1 + lgamma(k+1.5) - lgamma(k+D/2+1);
        term = exp(logak + loggk);
        if (k % 2 == 0)
          sum += term;
        else
          sum -= term;
        delta = fabs(sum - sum0);
        k++;
      }
      if (delta < eps)
        meanyz(i) = meanzz/M_SQRT2 + M_SQRT_2dPI * sum;
      else {
        // std::cout << 'I got here!' << std::endl;
        meanyz(i) = normy;
        //Rf_warning('E|y-Z| did not converge, replaced by %f', normy);
      }
    }

    //Computing the entire kernel matrix
    double ret = 0.0;
    arma::vec normyvec(n);
    for (int i=0; i<n; i++){
      for (int j=0; j<n; j++){
        yy = 0.0;
        for (k=0; k<d; k++) {
          dif = pow( y[i*d+k] - y[j*d+k], 2.0);
          yy += dif;
        }
        normy = sqrt(yy);
        //      if (i == j){
        //        normyvec[i] = normy;
        //      }
        ret += meanyz(i) + meanyz(j) - meanzz - normy;
      }
    }
    retvec(l) = ret / pow((double)n,2.0);
  }

  return (retvec);
}

// [[Rcpp::export]]
double obj_qsp(arma::vec& des, arma::mat& distsamp, double q){
  //des - vectorized design
  int NN = distsamp.n_rows;
  int pp = distsamp.n_cols;
  int nn = des.n_elem/pp;
  double runsum = 0.0;
  double runsum2 = 0.0;
  double tmp = 0.0;
  double ret = 0.0;
  
  for (int i=0; i<nn; i++){
    for (int m=0; m<NN; m++){
      tmp = 0.0;
      for (int j=0; j<pp; j++){
        tmp += pow( des(i*pp+j)-distsamp(m,j), 2);
      }
      runsum += pow(tmp, q/2.0);
    }
  }
  runsum = 2.0/( (double) nn * NN ) * runsum;
  // cout << "runsum: " << runsum << endl;
  
  for (int i=0; i<nn; i++){
    for (int k=0; k<nn; k++){
      tmp = 0.0;
      for (int j=0; j<pp; j++){
        tmp += pow( des(i*pp+j)-des(k*pp+j), 2);
      }
      runsum2 += pow(tmp, q/2.0);
    }
  }
  runsum2 = runsum2/( (double) pow(nn,2.0));
  // cout << "runsum2: " << runsum2 << endl;
  
  ret = runsum - runsum2;
  return(ret);
}

// [[Rcpp::export]]
arma::vec grad_qsp(arma::vec& des, arma::mat& distsamp, double q){
  int NN = distsamp.n_rows;
  int pp = distsamp.n_cols;
  int nn = des.n_elem/pp;
  double tmp = 0.0;
  arma::vec ret(nn*pp); //point-by-point
  arma::vec ret2(nn*pp);
  for (int i=0; i<(nn*pp); i++){
    ret(i) = 0.0;
    ret2(i) = 0.0;
  }
  arma::vec tmpvec(pp); //for a single point
  
  for (int i=0; i<nn; i++){
    for (int m=0; m<NN; m++){
      
      //Reset tmpvec
      for (int j=0; j<pp; j++){
        tmpvec(j) = 0.0;
      }
      tmp = 0.0;
      
      for (int j=0; j<pp; j++){
        tmpvec(j) += des(i*pp+j)-distsamp(m,j);
        tmp += pow( tmpvec(j), 2);
      }
      tmpvec = tmpvec / pow(tmp, (2.0-q)/2.0);
      
      //Update ret
      for (int j=0; j<pp; j++){
        ret(i*pp+j) += tmpvec(j);
      }
      
    }
  }
  ret = (2.0 * q)/( (double) nn * NN ) * ret;
  
  for (int i=0; i<nn; i++){
    for (int k=0; k<nn; k++){
      if (k != i){
        //Reset tmpvec
        for (int j=0; j<pp; j++){
          tmpvec(j) = 0.0;
        }
        tmp = 0.0;
        
        for (int j=0; j<pp; j++){
          tmpvec(j) += des(i*pp+j)-des(k*pp+j);
          tmp += pow( tmpvec(j), 2 );
        }
        tmpvec = tmpvec / pow(tmp, (2.0-q)/2.0);
        
        //Update ret
        for (int j=0; j<pp; j++){
          ret2(i*pp+j) += tmpvec(j);
        }
      }
    }
  }
  ret2 = (2.0 * q) / ( (double) nn * nn ) * ret2;
  
  // cout << ret(0) << endl;
  // cout << ret2(0) << endl;
  // cout << tmpvec(0) << endl;
  return(ret-ret2);
}

//-------------------------------------------------------------------------------
// SP and PSP for thinning
//-------------------------------------------------------------------------------

// // [[Rcpp::export]]
// NumericMatrix thincpp(NumericMatrix& Rcpp_point, arma::mat& Rcpp_inides, int num_subsamp,
//              NumericMatrix& bound, int it_max, int inn_it_max, double innertol, double outertol, double epsilon, double rate,
//              int num_proc) 
//   {
//   // Description of Inputs:
//   // Rcpp_point          - Sample data
//   // Rcpp_inides         - Initial design
//   // bound               - Upper and lower bound
//   // it_max              - Maximum number of iterations
//   // tol                 - Tolerance for stopping blockwise updates
//   
//   //Closed-form updates for p == 2
//   
//   int it_num = 0; //keeps track of iteration number
//   int dim_num = Rcpp_point.ncol(); //dimension of data points
//   int point_num = Rcpp_point.nrow(); //number of data points
//   int des_num = Rcpp_inides.n_rows; //number of clusters
//   bool cont = true; //stopping boolean for outside BCD loop
//   bool cont2 = true; //stopping boolean for inside CCCP loop
//   
//   //Containers for optimization
//   std::vector<double> prevdes(des_num*dim_num);
//   std::vector<double> xprime(dim_num);
//   std::vector<double> tmpvec(dim_num);
//   // omp_set_num_threads(num_proc);
//   
//   //Vectorize design and points  
//   std::vector<double> des(des_num*dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       des[j+i*dim_num] = Rcpp_inides(i,j);
//     }
//   }
//   std::vector<double> point(point_num*dim_num);
//   for (int i=0; i<point_num; i++){
//     for (int j=0; j<dim_num; j++){
//       point[j+i*dim_num] = Rcpp_point(i,j);
//     }
//   }
//   
//   //Blockwise coordinate descent
//   while (cont){
//     
//     Rcout << "SP: Iteration " << it_num << "/" << it_max << endl;
//     time_t start = time(0);
//     
//     //Update innertol
//     // innertol = innertol * exp(-(double)rate*it_num);
//     
//     //Update prevdes
//     for (int i=0; i<des_num; i++){
//       for (int j=0; j<dim_num; j++){
//         prevdes[j+i*dim_num] = des[j+i*dim_num];
//       }
//     }
//     
//     //BCD for each point
//     for (int m=0; m<des_num; m++){
//       
//       //        Rcout << "Point: " << m << endl;
//       
//       //Copy current design point as initial point
//       for (int n=0; n<dim_num; n++){
//         xprime[n] = des[n+m*dim_num];
//       }
//       
//       //Reset cont2 flag
//       cont2 = true;
//       int it_num2 = 0;
//       
//       //Concave-convex updates until convergence
//       while( (cont2) && (it_num2 <= inn_it_max) ){
//         
//         //          Rcout << "Inner iteration: " << it_num2 << endl;
//         
//         //Reset xprime
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = 0.0;
//         }
//         
//         //Update using closed-form formula
//         //... for the design
//         for (int o=0; o<des_num; o++){
//           
//           if (o != m){
//             double tmptol = 0.0; //Running total for norm in denominator
//             for (int n=0; n<dim_num; n++){
//               tmpvec[n] = des[n+m*dim_num] - des[n+o*dim_num];
//               tmptol += pow(tmpvec[n],2);
//             }
//             tmptol = sqrt(tmptol+epsilon);
//             
//             for (int n=0; n<dim_num; n++){
//               xprime[n] += tmpvec[n]/tmptol;
//             }
//           }
//           
//         }
//         
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = xprime[n] * (num_subsamp/des_num);
//         }
//         
//         //... for the samples
//         
//         // Subsample:
//         std::random_device rd;
//         std::mt19937 gen(rd());
//         std::uniform_int_distribution<> dis(0, point_num-1);
//         ivec samp(num_subsamp);
//         for (int o=0; o<num_subsamp; o++){
//           samp(o) = dis(gen);
//         }
//         
//         // Compute sample-level 
//         double tmpconst = 0.0; //For the inverse distance sum
//         for (int o=0; o<num_subsamp; o++){
//           
//           double tmptol = 0.0; //Running total for norm in denominator
//           for (int n=0; n<dim_num; n++){
//             tmpvec[n] = pow(point[n+samp(o)*dim_num] - des[n+m*dim_num],2);
//             tmptol += tmpvec[n];
//           }
//           //            Rcout << "tmptol: " << tmptol << endl;
//           tmptol = sqrt(tmptol+epsilon);
//           tmpconst += 1.0/tmptol;
//           
//           for (int n=0; n<dim_num; n++){
//             xprime[n] += point[n+samp(o)*dim_num]/tmptol;
//           }
//           
//         } 
//         
//         //Scale by inverse distances
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = xprime[n]/tmpconst;
//         }    
//         
//         //Update bounds
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = min( max( xprime[n], bound(n,0)), bound(n,1));
//         }
//         
//         //Compute running tolerance
//         double runtol = 0.0;
//         for (int n=0; n<dim_num; n++){
//           runtol += pow(des[n+m*dim_num] - xprime[n],2);
//         }
//         //          Rcout << "runtol: " << runtol << endl;
//         runtol = sqrt(runtol);
//         
//         //Update point
//         for (int n=0; n<dim_num; n++){
//           des[n+m*dim_num] = xprime[n];
//         }
//         
//         //          Rcout << "runtol: " << runtol << endl;; //Temporary
//         
//         //Update cont2 flag
//         if (runtol < innertol){
//           cont2 = false;
//         }
//         it_num2 ++;
//       }
//       
//       //Update design point 
//       for (int n=0; n<dim_num; n++){
//         des[n+m*dim_num] = xprime[n];
//       }
//       
//     }
//     
//     //Output time
//     time_t end = time(0);
//     double sec = difftime(end, start);
//     Rcout << "Iteration time: " << sec * 1000.0 << endl;
//     
//     //Increment and update cont flag 
//     it_num++;
//     //Check maximum iterations
//     if (it_num >= it_max){
//       cont = false;
//     }
//     //Check convergence
//     double rundiff = 0.0;
//     for (int n=0; n<des_num; n++){
//       for (int o=0; o<dim_num; o++){
//         rundiff += abs(des[o+n*dim_num]-prevdes[o+n*dim_num]);
//       }
//     }
//     if (rundiff < outertol){
//       cont = false;
//     }
//     
//     //      cont = false;//////
//   }
//   
//   //Output the final NumericMatrix 
//   NumericMatrix retdes(des_num, dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       retdes(i,j) = des[j+i*dim_num];
//     }
//   }
//   return(retdes);
//   
// }


//-------------------------------------------------------------------------------
// SP and PSP for standard distributions (including asymptotic approximation for large p)
//-------------------------------------------------------------------------------
// // [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
NumericMatrix sp_cpp(int des_num, int dim_num, NumericMatrix& ini,
                             NumericVector& distind, List distparam, 
                             NumericMatrix& distsamp, bool thin, NumericMatrix& bd,
                             int point_num, int it_max, int it_min, double tol, 
                             int num_proc, double n0, NumericVector& wts, bool rnd_flg){
  
  // Description of Inputs:
  // Rcpp_point          - Sample data
  // Rcpp_inides         - Initial design
  // bound               - Upper and lower bound
  // it_max              - Maximum number of iterations
  // tol                 - Tolerance for stopping blockwise updates
  
  int it_num = 0; //keeps track of iteration number
  bool cont = true; //stopping boolean for outside BCD loop
  bool cont2 = true; //stopping boolean for inside CCCP loop
  arma::vec curconst(des_num); //Running total for current inverse distance sum
  curconst.fill(0.0);
  arma::vec runconst(des_num); //Running total for running inverse distance sum
  runconst.fill(0.0);
  arma::vec runconst_up(des_num); //Running total for running inverse distance sum (temporary container)
  runconst_up.fill(0.0);
  // int point_num = subsampfac*des_num;
  // Rcout << wtst << endl;
  
  
  //  Containers for optimization and sampling
  std::vector<double> prevdes(des_num*dim_num);
  // std::vector<double> tmpvec(dim_num);
  std::default_random_engine generator;
  generator.seed(std::random_device{}());
  int distint = 0;
  // omp_set_num_threads(num_proc);
  
  //Vectorize (copy) design and points  
  std::vector<double> des(des_num*dim_num);
  std::vector<double> des_up(des_num*dim_num);
  for (int i=0; i<des_num; i++){
    for (int j=0; j<dim_num; j++){
      des[j+i*dim_num] = ini(i,j);
    }
  }
  
  //CCP
  double nug = 0.0; // nugget
  Rcout << "Optimizing ... " << endl;
  while (cont){
    
    // Rcout << "SP: Iteration " << it_num << "/" << it_max << endl;
    double prop = (double)it_num/(double)it_max;
    if (it_num>0){printBar(prop);}
    
    // time_t start = time(0);
    curconst.fill(0.0);
    bool nanflg = false;
  
    //Update prevdes
    for (int i=0; i<des_num; i++){
      for (int j=0; j<dim_num; j++){
        prevdes[j+i*dim_num] = des[j+i*dim_num];
      }
    }
    
    // Closed-form updates
    
    // Generate sample from F
    arma::mat rnd(point_num,dim_num); //random sample points
    arma::vec rnd_wts(point_num); //weights for sample points (supplied from wts)
    if (thin){
      // std::default_random_engine generator;
      // generator.seed(std::time(0));
      std::uniform_int_distribution<int> uddist(0,distsamp.nrow()-1);
      for (int i=0; i<point_num; i++){
        int ss;
        if (rnd_flg){
          ss = uddist(generator);
        }else{
          // cout << "I got here" << endl;
          ss = i;
        }
        for (int j=0; j<dim_num; j++){
          rnd(i,j) = distsamp(ss,j);
        }
        rnd_wts(i) = wts(ss);
      }
      // Rcout << "I got here!" << endl;
    }else{
      for (int n=0; n<dim_num; n++){
        
        for (int i=0; i<point_num; i++){
          rnd_wts(i) = wts(i);
        }
        
        distint = distind(n);
        switch( distint ){
        case 1:
        {
          SEXP unill = distparam[n];
          NumericVector uniyy(unill);
          std::uniform_real_distribution<double> uni_dist (uniyy(0),uniyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = uni_dist(generator);
          }
          break;
        }
        case 2:
        {
          SEXP normll = distparam[n]; 
          NumericVector normyy(normll);  
          std::normal_distribution<double> norm_dist (normyy(0),normyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = norm_dist(generator);
          }
          break;
        }
        case 3:
        {
          SEXP expll = distparam[n]; 
          NumericVector expyy(expll);  
          std::exponential_distribution<double> exp_dist (expyy(0));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = exp_dist(generator);
          }
          break;
        }
        case 4:
        {
          SEXP gamll = distparam[n]; 
          NumericVector gamyy(gamll);  
          std::gamma_distribution<double> gam_dist (gamyy(0),gamyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = gam_dist(generator);
          }
          break;
        }
        case 5:
        {
          SEXP lnll = distparam[n]; 
          NumericVector lnyy(lnll);  
          std::lognormal_distribution<double> ln_dist (lnyy(0),lnyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = ln_dist(generator)/ ( exp(pow(lnyy(1),2.0)-1.0) * exp(2*lnyy(0)+pow(lnyy(1),2.0)) );
          }
          break;
        }
        case 6:
        {
          SEXP tll = distparam[n]; 
          NumericVector tyy(tll);  
          std::student_t_distribution<double> t_dist (tyy(0));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = t_dist(generator);
          }
          break;
        }
        case 7:
        {
          SEXP wbll = distparam[n]; 
          NumericVector wbyy(wbll);  
          std::weibull_distribution<double> wb_dist (wbyy(0),wbyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = wb_dist(generator);
          }
          break;
        }
        case 8:
        {
          SEXP cauchll = distparam[n]; 
          NumericVector cauchyy(cauchll);  
          std::cauchy_distribution<double> cauch_dist (cauchyy(0),cauchyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = cauch_dist(generator);
          }
          break;
        }
        case 9:
        {
          SEXP betall = distparam[n]; 
          NumericVector betayy(betall);  
          sftrabbit::beta_distribution<> beta_dist (betayy(0),betayy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = beta_dist(generator);
          }
          break;
        }
        }
      }
    }
    
    
    //Parallelize computation
    // Rcout << num_proc << endl;
    // omp_set_num_threads(num_proc);
    // #pragma omp parallel for
    for (int m=0; m<des_num; m++){
      
      arma::vec xprime(dim_num);
      xprime.fill(0.0);
      arma::vec tmpvec(dim_num);
      tmpvec.fill(0.0);
      
      //Summation for design part
      for (int o=0; o<des_num; o++){
        
        if (o != m){
          double tmptol = 0.0; //Running total for norm in denominator
          for (int n=0; n<dim_num; n++){
            tmpvec(n) = prevdes[n+m*dim_num] - prevdes[n+o*dim_num];
            tmptol += pow(tmpvec(n),2.0);
          }
          // if( tmptol <= 1e-300 ){
          //   cout << "design: " << tmptol << endl;
          // }
          tmptol = sqrt(tmptol);
          
          for (int n=0; n<dim_num; n++){
            // xprime(n) += tmpvec(n)/tmptol;
            xprime(n) += tmpvec(n)/(tmptol+nug*DBL_MIN);
          }
        }
      }
      
      for (int n=0; n<dim_num; n++){
        xprime(n) = xprime(n) * ((double)point_num/(double)des_num);
      }
      
      //Summation for sample side
      for (int o=0; o<point_num; o++){
        double tmptol = 0.0; //Running total for norm in denominator
        for (int n=0; n<dim_num; n++){
          // tmpvec(n) = pow(rnd(o,n) - prevdes[n+m*dim_num],2.0);
          // tmptol += tmpvec(n);
          tmptol += pow( rnd(o,n) - prevdes[n+m*dim_num],2.0);
        }
        tmptol = sqrt(tmptol);
        // curconst(m) += ((long double)rnd_wts(o))/tmptol;
        curconst(m) += rnd_wts(o)/(tmptol+(nug*DBL_MIN));
        // if( tmptol <= 1e-14 ){
        //   cout << "tmptol: " << tmptol << endl;
        //   cout << "tmptol+nug: " << (tmptol+(nug*DBL_MIN)) << endl;
        //   cout << "1/(tmptol+nug): " << 1.0/(tmptol+(nug*DBL_MIN)) << endl;
        // }
        
        for (int n=0; n<dim_num; n++){
          // xprime(n) += ((long double)rnd_wts(o)*rnd(o,n))/tmptol;
          xprime(n) += rnd_wts(o)*rnd(o,n)/(tmptol+(nug*DBL_MIN));
        }
      }

      //Scale by inverse distances
      double denom = (1.0-(n0/(it_num+n0))) * runconst(m) + (n0/(it_num+n0)) * curconst(m);
      // double denom = curconst(m);
      for (int n=0; n<dim_num; n++){
        xprime(n) = ( (1.0-(n0/(it_num+n0))) * runconst(m) * prevdes[n+m*dim_num] + (n0/(it_num+n0)) * xprime(n) ) / denom;
        // xprime(n) = xprime(n)/ denom;
      }

      //Update bounds
      for (int n=0; n<dim_num; n++){
        xprime(n) = min( max( xprime(n), bd(n,0)), bd(n,1));
      }

      //Update points, constants, nanflg
      // for (int n=0; n<dim_num; n++){
      //   des[n+m*dim_num] = xprime(n);
      // }
      for (int n=0; n<dim_num; n++){
        des_up[n+m*dim_num] = xprime(n);
        if (isnan(xprime(n))){
          nanflg = true;
        }
      }
      runconst_up(m) = (1-(n0/(it_num+n0)))*runconst(m) + (n0/(it_num+n0))*curconst(m);
      
      // Rcout << curconst(m) << endl;
      // Rcout << nug*DBL_MIN << endl;

    }
    
    //check nanflg
    if (nanflg){
      //increase nugget & reset
      nug += 1.0;
      // it_num = 0;
      // for (int i=0; i<des_num; i++){
      //   for (int j=0; j<dim_num; j++){
      //     des[j+i*dim_num] = ini(i,j);
      //   }
      // }
      runconst.fill(0.0);
      Rcout << endl << "Numerical instablities encountered... resetting optimization" << endl;
    }else{
      des = des_up; // update points & running constant
      runconst = runconst_up;
    }
    // Rcout << "Increasing nugget " << nug << endl;

    // //Output time
    // time_t end = time(0);
    // double sec = difftime(end, start);
    // Rcout << "Iteration time: " << sec * 1000.0 << endl;
    
    //Increment and update cont flag 
    it_num++;
    
    //Check convergence (point with maximum movement)
    double maxdiff = 0.0;
    double rundiff = 0.0;
    for (int n=0; n<des_num; n++){
      rundiff = 0.0;
      for (int o=0; o<dim_num; o++){
        rundiff += std::pow(des[o+n*dim_num]-prevdes[o+n*dim_num],2.0);
      }
      maxdiff = max(maxdiff,rundiff);
    }
    // Rcout << "maxdiff: " << maxdiff << endl;
    if ( (maxdiff < tol) && (it_num >= it_min) && (!nanflg) ){
      cont = false;
      Rcout << endl << "Tolerance level reached: done!" << endl;
    }
    
    //Check maximum iterations
    if ( (it_num >= it_max) && (!nanflg) ){
      cont = false;
      Rcout << endl << "Maximum iterations reached: done!" << endl;
    }
    
  }
  
  //Output the final NumericMatrix 
  // Rcout << "One value: " << des[0] << endl;
  NumericMatrix retdes(des_num, dim_num);
  for (int j=0; j<dim_num; j++){
    // if (distind(j)==5){//Rescale lognormal
    //   SEXP lnll = distparam[j]; 
    //   NumericVector lnyy(lnll);  
    //   for (int i=0; i<des_num; i++){
    //     retdes(i,j) = des[j+i*dim_num] * ( exp(pow(lnyy(1),2.0)-1.0) * exp(2*lnyy(0)+pow(lnyy(1),2.0)) );
    //   }
    // }
    // else{
      for (int i=0; i<des_num; i++){
        retdes(i,j) = des[j+i*dim_num];
      }
    // }
  }
  
  return(retdes);
  
}

// // [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
NumericMatrix sp_seq_cpp(NumericMatrix& cur, int nseq, NumericMatrix& ini,
                         NumericVector& distind, List distparam, 
                         NumericMatrix& distsamp, bool thin, NumericMatrix& bd,
                         int point_num, int it_max, int it_min, double tol, int num_proc){
  
  // Description of Inputs:
  // Rcpp_point          - Sample data
  // Rcpp_inides         - Initial design
  // bound               - Upper and lower bound
  // it_max              - Maximum number of iterations
  // tol                 - Tolerance for stopping blockwise updates
  
  // std::srand ( unsigned ( std::time(0) ) ); // random seed
  
  //  Containers for optimization and sampling
  int dim_num = cur.ncol();
  int nini = cur.nrow();
  
  // std::vector<double> prevdes(des_num*dim_num);
  // std::vector<double> tmpvec(dim_num);
  std::default_random_engine generator;
  // generator.seed(std::time(0));
  int distint = 0;
  // omp_set_num_threads(num_proc);
  
  //Vectorize design and points  
  std::vector<double> des((nini+nseq)*dim_num);
  std::vector<double> des_up((nini+nseq)*dim_num);
  for (int i=0; i<nini; i++){
    for (int j=0; j<dim_num; j++){
      des[j+i*dim_num] = cur(i,j);
    }
  }
  for (int i=nini; i<(nini+nseq); i++){
    for (int j=0; j<dim_num; j++){
      des[j+i*dim_num] = ini(i-nini,j);
    }
  }
  des_up = des;
  // Rcout << "des: " << des[0] << endl;
  // Rcout << "des: " << des[(nini+nseq)*dim_num-1] << endl;
  
  bool cont = true;
  int it_cur = 0;
  Rcout << "Optimizing ... " << endl;
  
  while (cont){
    
    double prop = (double)it_cur/(double)it_max;
    if (it_cur>0){printBar(prop);}
    
    // Generate sample from F
    rst:
      arma::mat rnd(point_num,dim_num);
    if (thin){
      // std::default_random_engine generator;
      // generator.seed(std::time(0));
      std::uniform_int_distribution<int> uddist(0,distsamp.nrow()-1);
      for (int i=0; i<point_num; i++){
        int ss = uddist(generator);
        for (int j=0; j<dim_num; j++){
          rnd(i,j) = distsamp(ss,j);
        }
      }
    }else{
      for (int n=0; n<dim_num; n++){
        distint = distind(n);
        switch( distint ){
        case 1:
        {
          SEXP unill = distparam[n];
          NumericVector uniyy(unill);
          std::uniform_real_distribution<double> uni_dist (uniyy(0),uniyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = uni_dist(generator);
          }
          break;
        }
        case 2:
        {
          SEXP normll = distparam[n]; 
          NumericVector normyy(normll);  
          std::normal_distribution<double> norm_dist (normyy(0),normyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = norm_dist(generator);
          }
          break;
        }
        case 3:
        {
          SEXP expll = distparam[n]; 
          NumericVector expyy(expll);  
          std::exponential_distribution<double> exp_dist (expyy(0));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = exp_dist(generator);
          }
          break;
        }
        case 4:
        {
          SEXP gamll = distparam[n]; 
          NumericVector gamyy(gamll);  
          std::gamma_distribution<double> gam_dist (gamyy(0),gamyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = gam_dist(generator);
          }
          break;
        }
        case 5:
        {
          SEXP lnll = distparam[n]; 
          NumericVector lnyy(lnll);  
          std::lognormal_distribution<double> ln_dist (lnyy(0),lnyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = ln_dist(generator)/ ( exp(pow(lnyy(1),2.0)-1.0) * exp(2*lnyy(0)+pow(lnyy(1),2.0)) );
          }
          break;
        }
        case 6:
        {
          SEXP tll = distparam[n]; 
          NumericVector tyy(tll);  
          std::student_t_distribution<double> t_dist (tyy(0));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = t_dist(generator);
          }
          break;
        }
        case 7:
        {
          SEXP wbll = distparam[n]; 
          NumericVector wbyy(wbll);  
          std::weibull_distribution<double> wb_dist (wbyy(0),wbyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = wb_dist(generator);
          }
          break;
        }
        case 8:
        {
          SEXP cauchll = distparam[n]; 
          NumericVector cauchyy(cauchll);  
          std::cauchy_distribution<double> cauch_dist (cauchyy(0),cauchyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = cauch_dist(generator);
          }
          break;
        }
        case 9:
        {
          SEXP betall = distparam[n]; 
          NumericVector betayy(betall);  
          sftrabbit::beta_distribution<> beta_dist (betayy(0),betayy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = beta_dist(generator);
          }
          break;
        }
        }
      }
    }
    
    //Sequential sampling for nseq samples
    // omp_set_num_threads(num_proc);
    // #pragma omp parallel for
    for (int m=0; m<nseq; m++){
      
      // int it_cur = 0;
      arma::vec xprime(dim_num); //Current opt. vector
      arma::vec prevvec(dim_num); //Previous opt. vector
      for (int n=0; n<dim_num; n++){
        prevvec(n) = des[n+(nini+m)*dim_num];
      }
      xprime.fill(0.0);
      arma::vec tmpvec(dim_num); //temp. container
      tmpvec.fill(0.0);
      
      //Summation for design part
      double tmptol;
      for (int o=0; o<(nini+nseq); o++){
        tmptol = 0.0; //Running total for norm in denominator
        for (int n=0; n<dim_num; n++){
          tmpvec(n) = prevvec(n) - des[n+o*dim_num];
          tmptol += pow(tmpvec(n),2.0);
        }
        tmptol = sqrt(tmptol);
        
        if (tmptol>0.0){
          for (int n=0; n<dim_num; n++){
            xprime(n) += tmpvec(n)/tmptol;
          }
        }
      }
      // Rcout << "xprime: " << xprime << endl;
      
      for (int n=0; n<dim_num; n++){
        // xprime(n) = xprime(n) * (point_num/(m+nini+1));
        xprime(n) = xprime(n) * ((double)point_num/(double)(nini+nseq));
      }
      
      //Summation for sample side
      double tmpconst = 0.0; //Running total for inverse distance sum
      for (int o=0; o<point_num; o++){
        double tmptol = 0.0; //Running total for norm in denominator
        for (int n=0; n<dim_num; n++){
          tmpvec(n) = pow(rnd(o,n) - prevvec(n),2);
          tmptol += tmpvec(n);
        }
        tmptol = sqrt(tmptol);
        tmpconst += 1.0/tmptol;
        
        for (int n=0; n<dim_num; n++){
          xprime(n) += rnd(o,n)/tmptol;
        }
      }
      
      //Scale by inverse distances
      for (int n=0; n<dim_num; n++){
        xprime(n) = xprime(n)/tmpconst;
      }    
      
      //Update bounds
      for (int n=0; n<dim_num; n++){
        xprime(n) = min( max( xprime(n), bd(n,0)), bd(n,1));
      }
      
      //Update point
      for (int n=0; n<dim_num; n++){
        des_up[n+(nini+m)*dim_num] = xprime(n);
      }
      
    }
    
    // //Check tolerance and stop if satisfied
    // double rundiff = 0.0;
    // for (int n=0; n<dim_num; n++){
    //   rundiff += abs(xprime(n)-prevvec(n));
    // }
    // // cout << "rundiff = " << rundiff << endl;
    // // cout << "tol = " << tol << endl;
    // if ((rundiff < tol)&&(it_cur>=it_min)){
    //   cont = false;
    // }else{
    //   it_cur++;
    // }
    
    // //Update prevvec
    // for (int n=0; n<dim_num; n++){
    //   prevvec(n) = xprime(n);
    // }
    
    des = des_up;
    
    //Check maximum iterations
    it_cur++;
    if (it_cur >= it_max){
      cont = false;
    }
  }
  
  //Convert to Rmatrix and output
  NumericMatrix retdes(nini+nseq, dim_num);
  for (int j=0; j<dim_num; j++){
    for (int i=0; i<(nini+nseq); i++){
      // if(i==0){
      //   Rcout << "val: " << des[j+i*dim_num] << endl;
      // }
      retdes(i,j) = des[j+i*dim_num];
    }
  }
  
  return(retdes);
  
}


// [[Rcpp::export]]
double gamma_eval(arma::vec& dd, arma::vec& theta){
  double runtot = 0.0;
  int pp = dd.size();
  for (int i=0; i<pp; i++){
    runtot += theta[i]*pow(dd[i],2.0);
  }
  runtot = exp(-runtot);
  return(runtot);
}

arma::vec asvec(arma::rowvec xx){
  arma::vec ret(xx.size());
  for (int i=0; i<xx.size(); i++){
    ret(i) = xx(i);
  }
  return(ret);
}

// [[Rcpp::export]]
arma::vec omega(arma::vec& theta_vec, arma::vec& gamma_vec, int max_ord){
  arma::vec theta_tr(theta_vec.size()-1);
  int ct;
  int len = std::min((int)theta_vec.size()-1,max_ord);
  arma::vec rec_vec(len+1);
  arma::vec ret(theta_vec.size());
  
  //Repeat for each dimension
  for (int i=0; i<theta_vec.size(); i++){
    ct = 0;
    // cout << "Got here 0" << endl;
    //Remove i-th entry
    for (int j=0; j<theta_vec.size(); j++){
      if (j == i){
      }else{
        theta_tr(ct) = theta_vec(j);
        ct++;
      }
      // cout << "ct: " << ct << ", j:" << j << endl;
    }
    // cout << "Got here 1" << endl;
    //Reset recursion vector
    for (int j=0; j<(len+1); j++){
      if (j==0){
        rec_vec(j) = 1.0;
      }
      else{
        rec_vec(j) = 0.0;
      }
    }
    // cout << "Got here 2" << endl;
    //Compute recursion
    for (int j=0; j<(theta_vec.size()-1); j++){
      //Update first element
      rec_vec(0) = rec_vec(0);
      //Update remaining elements
      rec_vec(span(1,rec_vec.size()-1)) = theta_tr(j)*rec_vec(span(0,rec_vec.size()-2)) + rec_vec(span(1,rec_vec.size()-1));
    }
    // cout << "Got here 3" << endl;
    ret(i) = theta_vec(i) * dot( rec_vec(span(0,max_ord-1)), gamma_vec(span(0,max_ord-1)) );
    // cout << "Got here 4" << endl;
    
  }
  return(ret);
}

// [[Rcpp::export]]
arma::mat opt_hess(arma::vec zz, arma::vec omega_vec){
  int pp = zz.size();
  arma::mat retmtx(pp,pp);
  
  // Computed normalized zz (scaled by omega_vec)
  arma::vec zz_tilde = zz;
  double nm = 0.0;
  for (int i=0; i<pp; i++){
    zz_tilde(i) = zz(i)*omega_vec(i);
    nm += pow(zz(i)*omega_vec(i),2.0);
  }

  // Return matrix
  for (int i=0; i<pp; i++){
    for (int j=0; j<pp; j++){
      retmtx(i,j) = 4.0/exp(1.0)/nm*zz_tilde(i)*zz_tilde(j);
    }
  }
  return(retmtx);
}

// [[Rcpp::export]]
arma::vec psp_mi(arma::vec& xx, arma::mat& omega_mat, arma::mat& samp_mat,
                 std::vector<double>& des_mat, int des_num, int ii){
// arma::vec psp_mi(arma::vec& xx, arma::mat& omega_mat, arma::mat& samp_mat,
//                  arma::mat& des_mat, int ii){

  // int des_num = des_mat.n_rows;
  int nn = des_num;
  int pp = samp_mat.n_cols;
  int NN = samp_mat.n_rows;
  int RR = omega_mat.n_rows;
  // cout << nn << endl;
  // cout << pp << endl;
  // cout << NN << endl;
  // cout << RR << endl;
  arma::vec pt_vec(pp);
  arma::vec omega_vec(pp);
  double gam = 0.0;
  arma::vec runvec(pp);
  runvec.fill(0.0);
  arma::mat runmat(pp,pp);
  runmat.fill(0.0);
  arma::vec tmpvec(pp);
  arma::mat tmpmat(pp,pp);

  // cout << runvec << endl;
  
  // Computing numerator
  for (int r=0; r<RR; r++){
    omega_vec = omega_mat.col(r);
    for (int j=0; j<nn; j++){
      if (!(ii == j)){
        // cout << j << endl;
        // cout << ii << endl;
        // first term
        for (int l=0; l<pp; l++){
          pt_vec(l) = des_mat[l+j*pp];
        }
        // pt_vec = asvec(des_mat.row(j));
        pt_vec = xx - pt_vec;
        gam = gamma_eval(pt_vec,omega_vec);
        for (int l=0; l<pp; l++){
          runvec[l] += (2.0)/(nn*RR)*gam*omega_vec(l)*pt_vec(l);
        }
        // second term
        tmpmat = opt_hess(pt_vec,omega_vec);
        tmpvec = 4.0/(nn*RR)*tmpmat*xx;
        runvec += tmpvec;
      }
    }
  }
  // cout << gam << endl;
  // cout << tmpvec << endl;
  // cout << runvec << endl;
  // cout << "Got here 2" << endl;
  for (int r=0; r<RR; r++){
    omega_vec = omega_mat.col(r);
    for (int m=0; m<NN; m++){
      // cout << "Got here 2.1" << endl;
      pt_vec = asvec(samp_mat.row(m));
      pt_vec = xx - pt_vec;
      // cout << "Got here 2.2" << endl;
      gam = gamma_eval(pt_vec,omega_vec);
      // cout << "Got here 2.3" << endl;
      pt_vec = asvec(samp_mat.row(m));
      for (int l=0; l<pp; l++){
        runvec(l) += (2.0)/(NN*RR)*gam*omega_vec(l)*pt_vec(l);
      }
    }
  }
  // cout << runvec << endl;
  
  // cout << "Got here 3" << endl;
  
  // Computing denominator
  for (int r=0; r<RR; r++){
    omega_vec = omega_mat.col(r);
    for (int j=0; j<nn; j++){
      if (!(ii == j)){
        for (int l=0; l<pp; l++){
          pt_vec(l) = des_mat[l+j*pp];
        }
        // pt_vec = asvec(des_mat.row(j));
        pt_vec = xx - pt_vec;
        tmpmat = opt_hess(pt_vec,omega_vec);
        runmat += 4.0/(nn*RR)*tmpmat;
      }
    }
  }
  // cout << runmat << endl;
  
  // cout << "Got here 4" << endl;
  
  for (int r=0; r<RR; r++){
    omega_vec = omega_mat.col(r);
    for (int m=0; m<NN; m++){
      pt_vec = asvec(samp_mat.row(m));
      pt_vec = xx - pt_vec;
      gam = gamma_eval(pt_vec,omega_vec);
      for (int l=0; l<pp; l++){
        runmat(l,l) += (2.0)/(NN*RR)*gam*omega_vec(l);
      }
    }
  }
  
  // cout << runmat << endl;
  
  // cout << "Got here 5" << endl;
  
  //Inverse and multiply
  tmpmat = inv(runmat);
  tmpvec = tmpmat*runvec;
  return(tmpvec);
}

// [[Rcpp::export]]
NumericMatrix psp_cpp(NumericMatrix& Rcpp_inides, NumericVector& distind, List distparam,
                      NumericVector& gam_param, arma::vec& gamma_vec, int max_ord,
                      NumericMatrix& distsamp, bool thinind,
                      NumericMatrix& gamsamp, bool gamind, NumericMatrix& bd,
                      int point_num, int gam_point_num, int it_max, double tol, int num_proc){
  // Description of Inputs:
  // Rcpp_point          - Sample data
  // Rcpp_inides         - Initial design
  // bound               - Upper and lower bound
  // it_max              - Maximum number of iterations
  // tol                 - Tolerance for stopping blockwise updates
  
  double gam_shape=gam_param[0];
  double gam_rate=gam_param[1];
  
  int des_num = Rcpp_inides.nrow();
  int dim_num = Rcpp_inides.ncol();
  
  int it_num = 0; //keeps track of iteration number
  bool cont = true; //stopping boolean for outside BCD loop
  bool cont2 = true; //stopping boolean for inside CCCP loop
  
  //Containers for optimization
  std::vector<double> prevdes(des_num*dim_num);
  // std::vector<double> xprime(dim_num);
  std::vector<double> tmpvec(dim_num);
  std::default_random_engine generator;
  // generator.seed(std::time(0));
  int distint = 0;
  // omp_set_num_threads(num_proc);
  
  //Vectorize design and points
  std::vector<double> des(des_num*dim_num);
  for (int i=0; i<des_num; i++){
    for (int j=0; j<dim_num; j++){
      des[j+i*dim_num] = Rcpp_inides(i,j);
    }
  }
  
  //CCP
  arma::mat rnd(point_num,dim_num);
  arma::mat theta_mat(gam_point_num,dim_num);
  while (cont){
    
    arma::vec omega_vec(dim_num);
    arma::mat omega_mat(dim_num,gam_point_num);
    arma::vec theta_vec(dim_num);
    
    Rcout << "PSP: Iteration " << it_num << "/" << it_max << endl;
    // time_t start = time(0);
    
    //Update prevdes
    for (int i=0; i<des_num; i++){
      for (int j=0; j<dim_num; j++){
        prevdes[j+i*dim_num] = des[j+i*dim_num];
      }
    }
    
    // Closed-form updates
    
    // Generate sample from F
    if (thinind){
      std::uniform_int_distribution<int> uddist(0,distsamp.nrow()-1);
      // NumericVector sampidx = csample_num(icridx,point_num,true); //without replacement
      for (int i=0; i<point_num; i++){
        int ss = uddist(generator);
        for (int j=0; j<dim_num; j++){
          rnd(i,j) = distsamp(ss,j);
        }
      }
    }else{
      for (int n=0; n<dim_num; n++){
        distint = distind(n);
        switch( distint ){
        case 1:
        {
          SEXP unill = distparam[n];
          NumericVector uniyy(unill);
          std::uniform_real_distribution<double> uni_dist (uniyy(0),uniyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = uni_dist(generator);
          }
          break;
        }
        case 2:
        {
          SEXP normll = distparam[n];
          NumericVector normyy(normll);
          std::normal_distribution<double> norm_dist (normyy(0),normyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = norm_dist(generator);
          }
          break;
        }
        case 3:
        {
          SEXP expll = distparam[n];
          NumericVector expyy(expll);
          std::exponential_distribution<double> exp_dist (expyy(0));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = exp_dist(generator);
          }
          break;
        }
        case 4:
        {
          SEXP gamll = distparam[n];
          NumericVector gamyy(gamll);
          std::gamma_distribution<double> gam_dist (gamyy(0),gamyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = gam_dist(generator);
          }
          break;
        }
        case 5:
        {
          SEXP lnll = distparam[n];
          NumericVector lnyy(lnll);
          std::lognormal_distribution<double> ln_dist (lnyy(0),lnyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = ln_dist(generator)/ ( exp(pow(lnyy(1),2.0)-1.0) * exp(2*lnyy(0)+pow(lnyy(1),2.0)) );
          }
          break;
        }
        case 6:
        {
          SEXP tll = distparam[n];
          NumericVector tyy(tll);
          std::student_t_distribution<double> t_dist (tyy(0));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = t_dist(generator);
          }
          break;
        }
        case 7:
        {
          SEXP wbll = distparam[n];
          NumericVector wbyy(wbll);
          std::weibull_distribution<double> wb_dist (wbyy(0),wbyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = wb_dist(generator);
          }
          break;
        }
        case 8:
        {
          SEXP cauchll = distparam[n];
          NumericVector cauchyy(cauchll);
          std::cauchy_distribution<double> cauch_dist (cauchyy(0),cauchyy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = cauch_dist(generator);
          }
          break;
        }
        case 9:
        {
          SEXP betall = distparam[n];
          NumericVector betayy(betall);
          sftrabbit::beta_distribution<> beta_dist (betayy(0),betayy(1));
          for (int o=0; o<point_num; o++){
            rnd(o,n) = beta_dist(generator);
          }
          break;
        }
        }
      }
    }
    
    // // cout << gamsamp << endl;
    // // cout << theta_mat << endl;
    // // cout << gamma_vec << endl;
    // // cout << omega_mat << endl;
    // 
    // //Generate sample from \theta
    // if (gamind){
    //   std::uniform_int_distribution<int> uddist(0,gamsamp.nrow()-1);
    //   for (int i=0; i<gam_point_num; i++){
    //     int ss = uddist(generator);
    //     for (int j=0; j<dim_num; j++){
    //       theta_mat(i,j) = gamsamp(ss,j);
    //     }
    //   }
    // }
    // else{
    //   std::gamma_distribution<double> dist(gam_shape,1.0/gam_rate);
    //   for (int i=0; i<gam_point_num; i++){
    //     for (int n=0; n<dim_num; n++){
    //       theta_mat(i,n) = dist(generator);
    //     }
    //   }
    // }
    // 
    // //Compute omegas
    // for (int o=0; o<gam_point_num; o++){
    //   for (int n=0; n<dim_num; n++){
    //     theta_vec(n) = theta_mat(o,n);
    //   }
    //   omega_vec = omega(theta_vec,gamma_vec,max_ord);
    //   omega_mat.col(o) = omega_vec;
    // }
    // 
    // // //Randomize optimization order
    // // std::vector<int> desord(des_num);
    // // for (int m=0; m<des_num; m++){
    // //   desord[m] = m;
    // // }
    // // std::random_shuffle( desord.begin(), desord.end() );
    // 
    
    //Parallelize computation
    // #pragma omp parallel for
    for (int m=0; m<des_num; m++){

      //Declare variables
      vec runMat(dim_num);
      vec runVec(dim_num);
      runMat.fill(0.0);
      runVec.fill(0.0);
      arma::vec xprime(dim_num);
      xprime.fill(0.0);
      arma::vec tmpvec(dim_num);
      tmpvec.fill(0.0);
      
      //Reset current point
      for (int n=0; n<dim_num; n++){
        xprime[n] = des[n+m*dim_num];
      }
      
      //Update using closed-form formula:
      xprime = psp_mi(xprime, omega_mat, rnd, des, des_num, m);
      
      //Update bounds
      for (int n=0; n<dim_num; n++){
        xprime[n] = min( max( xprime[n], bd(n,0)), bd(n,1));
      }
      // cout << xprime << endl;
      // cout << bd << endl;
      
      // //Compute running tolerance
      // double runtol = 0.0;
      // for (int n=0; n<dim_num; n++){
      //   runtol += pow(des[n+m*dim_num] - xprime[n],2);
      // }
      // runtol = sqrt(runtol);
      
      //Update design point
      for (int n=0; n<dim_num; n++){
        des[n+m*dim_num] = xprime[n];
        // des[n+desord[m]*dim_num] = xprime[n];
      }
      
    }
    
    // //Output time
    // time_t end = time(0);
    // double sec = difftime(end, start);
    // Rcout << "Iteration time: " << sec * 1000.0 << endl;
    
    //Increment and update cont flag
    it_num++;
    //Check maximum iterations
    if (it_num >= it_max){
      cont = false;
    }
    // //Check convergence (point with maximum movement)
    // double maxdiff = 0.0;
    // double rundiff = 0.0;
    // for (int n=0; n<des_num; n++){
    //   rundiff = 0.0;
    //   for (int o=0; o<dim_num; o++){
    //     rundiff += std::pow(des[o+n*dim_num]-prevdes[o+n*dim_num],2.0);
    //   }
    //   maxdiff = max(maxdiff,rundiff);
    // }
    // // Rcout << "maxdiff: " << maxdiff << endl;
    // // Rcout << "it_num: " << it_num << ", it_max: " << it_max << endl;
    // 
    // if (maxdiff < tol){
    //   cont = false;
    // }
    
  }
  
  stop:
    //Output the final NumericMatrix
    NumericMatrix retdes(des_num, dim_num);
  for (int i=0; i<des_num; i++){
    for (int j=0; j<dim_num; j++){
      retdes(i,j) = des[j+i*dim_num];
    }
  }
  
  // return List::create(Named("retdes")=retdes,Named("theta_mat")=theta_mat);
  return (retdes);
  
}

// [[Rcpp::export]]
NumericMatrix psp_seq_cpp(NumericMatrix& cur, int nseq, NumericMatrix& ini,
                          NumericVector& distind, List distparam, 
                          NumericVector& gam_param, arma::vec& gamma_vec, int max_ord,
                          NumericMatrix& distsamp, bool thin,
                          NumericMatrix& gamsamp, bool gamind, NumericMatrix& bd,
                          int point_num, int gam_point_num, int it_max, double tol, int num_proc){
  
  // Description of Inputs:
  // Rcpp_point          - Sample data
  // Rcpp_inides         - Initial design
  // bound               - Upper and lower bound
  // it_max              - Maximum number of iterations
  // tol                 - Tolerance for stopping blockwise updates
  
  // std::srand ( unsigned ( std::time(0) ) ); // random seed
  
  double gam_shape=gam_param[0];
  double gam_rate=gam_param[1];
  
  //  Containers for optimization and sampling
  int dim_num = cur.ncol();
  int nini = cur.nrow();
  
  // std::vector<double> prevdes(des_num*dim_num);
  // std::vector<double> tmpvec(dim_num);
  std::default_random_engine generator;
  // generator.seed(std::time(0));
  int distint = 0;
  // omp_set_num_threads(num_proc);
  
  //Vectorize design and points  
  std::vector<double> des((nini+nseq)*dim_num);
  for (int i=0; i<nini; i++){
    for (int j=0; j<dim_num; j++){
      des[j+i*dim_num] = cur(i,j);
    }
  }
  for (int i=nini; i<(nini+nseq); i++){
    for (int j=0; j<dim_num; j++){
      des[j+i*dim_num] = ini(i-nini,j);
    }
  }
  
  //Sequential sampling for nseq samples
  for (int m=0; m<nseq; m++){
    
    Rcout << "Sequential PSPs: point " << (nini+m+1) << endl;
    
    int it_cur = 0;
    arma::vec xprime(dim_num); //Current opt. vector
    arma::vec prevvec(dim_num); //Previous opt. vector
    for (int n=0; n<dim_num; n++){
      prevvec(n) = des[n+(nini+m)*dim_num];
    }
    bool cont = true;
    
    while (cont){
      arma::vec omega_vec(dim_num);
      arma::mat omega_mat(dim_num,gam_point_num);
      arma::vec theta_vec(dim_num);
      
      // Generate sample from F
      arma::mat rnd(point_num,dim_num);
      if (thin){
        // std::default_random_engine generator;
        // generator.seed(std::time(0));
        std::uniform_int_distribution<int> uddist(0,distsamp.nrow()-1);
        for (int i=0; i<point_num; i++){
          int ss = uddist(generator);
          for (int j=0; j<dim_num; j++){
            rnd(i,j) = distsamp(ss,j);
          }
        }
      }else{
        for (int n=0; n<dim_num; n++){
          distint = distind(n);
          switch( distint ){
          case 1:
          {
            SEXP unill = distparam[n];
            NumericVector uniyy(unill);
            std::uniform_real_distribution<double> uni_dist (uniyy(0),uniyy(1));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = uni_dist(generator);
            }
            break;
          }
          case 2:
          {
            SEXP normll = distparam[n]; 
            NumericVector normyy(normll);  
            std::normal_distribution<double> norm_dist (normyy(0),normyy(1));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = norm_dist(generator);
            }
            break;
          }
          case 3:
          {
            SEXP expll = distparam[n]; 
            NumericVector expyy(expll);  
            std::exponential_distribution<double> exp_dist (expyy(0));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = exp_dist(generator);
            }
            break;
          }
          case 4:
          {
            SEXP gamll = distparam[n]; 
            NumericVector gamyy(gamll);  
            std::gamma_distribution<double> gam_dist (gamyy(0),gamyy(1));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = gam_dist(generator);
            }
            break;
          }
          case 5:
          {
            SEXP lnll = distparam[n]; 
            NumericVector lnyy(lnll);  
            std::lognormal_distribution<double> ln_dist (lnyy(0),lnyy(1));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = ln_dist(generator)/ ( exp(pow(lnyy(1),2.0)-1.0) * exp(2*lnyy(0)+pow(lnyy(1),2.0)) );
            }
            break;
          }
          case 6:
          {
            SEXP tll = distparam[n]; 
            NumericVector tyy(tll);  
            std::student_t_distribution<double> t_dist (tyy(0));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = t_dist(generator);
            }
            break;
          }
          case 7:
          {
            SEXP wbll = distparam[n]; 
            NumericVector wbyy(wbll);  
            std::weibull_distribution<double> wb_dist (wbyy(0),wbyy(1));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = wb_dist(generator);
            }
            break;
          }
          case 8:
          {
            SEXP cauchll = distparam[n]; 
            NumericVector cauchyy(cauchll);  
            std::cauchy_distribution<double> cauch_dist (cauchyy(0),cauchyy(1));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = cauch_dist(generator);
            }
            break;
          }
          case 9:
          {
            SEXP betall = distparam[n]; 
            NumericVector betayy(betall);  
            sftrabbit::beta_distribution<> beta_dist (betayy(0),betayy(1));
            for (int o=0; o<point_num; o++){
              rnd(o,n) = beta_dist(generator);
            }
            break;
          }
          }
        }
      }
      
      
      //Generate sample from \theta
      arma::mat theta_mat(gam_point_num,dim_num);
      if (gamind){
        // std::default_random_engine generator;
        // generator.seed(std::time(0));
        std::uniform_int_distribution<int> uddist(0,gamsamp.nrow()-1);
        for (int i=0; i<gam_point_num; i++){
          int ss = uddist(generator);
          for (int j=0; j<dim_num; j++){
            theta_mat(i,j) = gamsamp(ss,j);
          }
        }
      }
      else{
        // std::default_random_engine rndgen;
        // rndgen.seed(std::time(0));
        std::gamma_distribution<double> dist(gam_shape,1.0/gam_rate);
        for (int i=0; i<gam_point_num; i++){
          for (int n=0; n<dim_num; n++){
            theta_mat(i,n) = dist(generator);
          }
        }
      }
      
      //Compute omegas
      for (int o=0; o<gam_point_num; o++){
        for (int n=0; n<dim_num; n++){
          theta_vec(n) = theta_mat(o,n);
        }
        omega_vec = omega(theta_vec,gamma_vec,max_ord);
        omega_mat.col(o) = omega_vec;
      }
      
      //Initialize xprime
      for (int n=0; n<dim_num; n++){
        xprime(n) = prevvec(n);
      }
      // arma::vec tmpvec(dim_num); //temp. container
      // tmpvec.fill(0.0);
      
      xprime = psp_mi(xprime, omega_mat, rnd, des, m, m);
      // cout << "xprime: " << xprime[0] << ", " << xprime[1] << endl;
      
      //Update bounds
      for (int n=0; n<dim_num; n++){
        xprime(n) = min( max( xprime(n), bd(n,0)), bd(n,1));
      }
      
      //Check tolerance and stop if satisfied
      double rundiff = 0.0;
      for (int n=0; n<dim_num; n++){
        rundiff += abs(xprime(n)-prevvec(n));
      }
      // cout << "rundiff = " << rundiff << endl;
      // cout << "tol = " << tol << endl;
      if ((rundiff < tol)||(it_cur>=it_max)){
        cont = false;
      }else{
        it_cur++;
      }
      
      //Update prevvec
      for (int n=0; n<dim_num; n++){
        prevvec(n) = xprime(n);
      }
    }
    
    //Update point
    for (int n=0; n<dim_num; n++){
      des[n+(nini+m)*dim_num] = xprime(n);
    }
  }
  
  //Convert to Rmatrix and output
  NumericMatrix retdes(nini+nseq, dim_num);
  for (int j=0; j<dim_num; j++){
    for (int i=0; i<(nini+nseq); i++){
      retdes(i,j) = des[j+i*dim_num];
    }
  }
  
  return(retdes);
  
}



// void std_largep_cpp(arma::mat& Rcpp_inides, 
//                     int it_max, int inn_it_max, double innertol, double outertol, double epsilon, double rate,
//                     int num_proc) {
//   // Description of Inputs:
//   // Rcpp_point          - Sample data
//   // Rcpp_inides         - Initial design
//   // bound               - Upper and lower bound
//   // it_max              - Maximum number of iterations
//   // tol                 - Tolerance for stopping blockwise updates
//   
//   int it_num = 0; //keeps track of iteration number
//   int des_num = Rcpp_inides.n_rows; //number of data points
//   int dim_num = Rcpp_inides.n_cols; //number of clusters
//   bool cont = true; //stopping boolean for outside BCD loop
//   bool cont2 = true; //stopping boolean for inside CCCP loop
//   
//   //Containers for optimization
//   std::vector<double> prevdes(des_num*dim_num);
//   std::vector<double> xprime(dim_num);
//   std::vector<double> tmpvec(dim_num);
//   // omp_set_num_threads(num_proc);
//   
//   //Vectorize design and points  
//   std::vector<double> des(des_num*dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       des[j+i*dim_num] = Rcpp_inides(i,j);
//     }
//   }
//   
//   //Blockwise coordinate descent
//   while (cont){
//     
//     Rcout << "Iteration: " << it_num << endl;
//     time_t start = time(0);
//     
//     //Update innertol
//     innertol = innertol * exp(-(double)rate*it_num);
//     
//     //Update prevdes
//     for (int i=0; i<des_num; i++){
//       for (int j=0; j<dim_num; j++){
//         prevdes[j+i*dim_num] = des[j+i*dim_num];
//       }
//     }
//     
//     //BCD for each point
//     for (int m=0; m<des_num; m++){
//       
//       // Rcout << "Point: " << m << endl;
//       
//       //Copy current design point as initial point
//       for (int n=0; n<dim_num; n++){
//         xprime[n] = des[n+m*dim_num];
//       }
//       
//       //Reset cont2 flag
//       cont2 = true;
//       int it_num2 = 0;
//       
//       //Concave-convex updates until convergence
//       while ( (cont2) && (it_num2<inn_it_max) ){
//         
//         // Rcout << "Inner Iteration: " << it_num2 << endl;
//         
//         //Reset xprime
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = 0.0;
//         }
//         
//         //Update using closed-form formula
//         //... for the design
//         // #pragma omp parallel for
//         for (int o=0; o<des_num; o++){
//           
//           if (o != m){
//             double tmptol = 0.0; //Running total for norm in denominator
//             for (int n=0; n<dim_num; n++){
//               tmpvec[n] = des[n+m*dim_num] - des[n+o*dim_num];
//               tmptol += pow(tmpvec[n],2);
//             }
//             tmptol = sqrt(tmptol+epsilon);
//             
//             for (int n=0; n<dim_num; n++){
//               xprime[n] += tmpvec[n]/tmptol;
//             }
//           }
//           
//         }
//         
//         double coef = 0.0;
//         double dbl_dimnum = (double) dim_num;
//         for (int n=0; n<dim_num; n++){
//           coef += pow(des[n+m*dim_num]-0.5,2.0);
//         }
//         coef = pow(coef + dbl_dimnum/(12.0), 0.5)/des_num;
//         
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = 0.5 + coef*xprime[n];
//         }
//         
//         //Update bounds
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = min( max( xprime[n], 0.0), 1.0);
//         }
//         
//         //Compute running tolerance
//         double runtol = 0.0;
//         for (int n=0; n<dim_num; n++){
//           runtol += pow(des[n+m*dim_num] - xprime[n],2);
//         }
//         runtol = sqrt(runtol);
//         
//         //Update point
//         for (int n=0; n<dim_num; n++){
//           des[n+m*dim_num] = xprime[n];
//         }
//         
//         //Update cont2 flag
//         if (runtol < innertol){
//           cont2 = false;
//         }
//         //          cont2 = false; //////
//         it_num2 ++;
//       }
//       
//       //Update design point 
//       for (int n=0; n<dim_num; n++){
//         des[n+m*dim_num] = xprime[n];
//       }
//       
//     }
//     
//     // //Output time
//     // time_t end = time(0);
//     // double sec = difftime(end, start);
//     // Rcout << "Iteration time: " << sec * 1000.0 << endl;
//     // 
//     //Increment and update cont flag 
//     it_num++;
//     //Check maximum iterations
//     if (it_num >= it_max){
//       cont = false;
//     }
//     //Check convergence
//     double rundiff = 0.0;
//     for (int n=0; n<des_num; n++){
//       for (int o=0; o<dim_num; o++){
//         rundiff += abs(des[o+n*dim_num]-prevdes[o+n*dim_num]);
//       }
//     }
//     if (rundiff < outertol){
//       cont = false;
//     }
//     
//     //      cont = false;//////
//   }
//   
//   //Output the final NumericMatrix 
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       Rcpp_inides(i,j) = des[j+i*dim_num];
//     }
//   }
//   
// }

// // [[Rcpp::export]]
// NumericMatrix thincpp_proj(NumericMatrix& distsamp, arma::mat& Rcpp_inides,
//                            double gam_shape, double gam_rate, arma::vec& gamma_vec, int max_ord,
//                            NumericMatrix& bd, int num_subsamp, int gam_point_num, int it_max, int inn_it_max, 
//                            double innertol, double outertol,double epsilon, double rate, int num_proc){
//   
//   // Description of Inputs:
//   // Rcpp_point          - Sample data
//   // Rcpp_inides         - Initial design
//   // bound               - Upper and lower bound
//   // it_max              - Maximum number of iterations
//   // tol                 - Tolerance for stopping blockwise updates
//   
//   //Closed-form updates for p == 2
//   
//   int it_num = 0; //keeps track of iteration number
//   int dim_num = distsamp.ncol(); //dimension of data points
//   int point_num = distsamp.nrow(); //number of data points
//   int des_num = Rcpp_inides.n_rows; //number of clusters
//   bool cont = true; //stopping boolean for outside BCD loop
//   bool cont2 = true; //stopping boolean for inside CCCP loop
//   
//   //Containers for optimization
//   std::vector<double> prevdes(des_num*dim_num);
//   // std::vector<double> xprime(dim_num);
//   std::vector<double> tmpvec(dim_num);
//   arma::mat rnd(num_subsamp,dim_num);
//   // omp_set_num_threads(num_proc);
//   
//   //Vectorize design and points  
//   std::vector<double> des(des_num*dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       des[j+i*dim_num] = Rcpp_inides(i,j);
//     }
//   }
//   
//   //Blockwise coordinate descent
//   while (cont){
//     
//     Rcout << "PSP: Iteration " << it_num << "/" << it_max << endl;
//     time_t start = time(0);
//     
//     //Update previous design
//     for (int i=0; i<des_num; i++){
//       for (int j=0; j<dim_num; j++){
//         prevdes[j+i*dim_num] = des[j+i*dim_num];
//       }
//     }
//     
//     //CCP for each point
//     for (int m=0; m<des_num; m++){
//       
//       // Rcout << "Point: " << m << endl;
//       // Rcout << "point_num: " << point_num << endl;
//       // Rcout << "num_subsamp: " << num_subsamp << endl;
//       // Rcout << "gam_point_num: " << gam_point_num << endl;
//       
//       //Reset cont2 flag
//       cont2 = true;
//       int it_num2 = 0;
//       
//       //Concave-convex updates until convergence
//       while( (cont2) && (it_num2 <= inn_it_max) ){
//         
//         // Rcout << "Inner Iteration: " << it_num2 << endl;
//         //Declare variables
//         vec runMat(dim_num,fill::zeros);
//         vec runVec(dim_num,fill::zeros);
//         arma::vec omega_vec(dim_num);
//         arma::mat omega_mat(dim_num,gam_point_num);
//         arma::vec theta_vec(dim_num);
//         arma::mat theta_mat(gam_point_num,dim_num);
//         arma::vec xprime(dim_num,fill::zeros);
//         arma::vec tmpvec(dim_num,fill::zeros);
//         vec Delta(dim_num);
//         
//         //Subsample point set
//         std::default_random_engine generator;
//         std::uniform_int_distribution<int> uddist(0,distsamp.nrow()-1);
//         for (int i=0; i<num_subsamp; i++){
//           for (int j=0; j<dim_num; j++){
//             rnd(i,j) = distsamp(uddist(generator),j);
//           }
//         }
//         
//         //Sample gammas
//         std::default_random_engine rndgen;
//         std::gamma_distribution<double> dist(gam_shape,gam_rate);
//         for (int i=0; i<gam_point_num; i++){
//           for (int n=0; n<dim_num; n++){
//             theta_mat(i,n) = dist(rndgen);
//           }
//         }
//         
//         // Rcout << "Step 1" << endl;
//         // if (isnan(runVec(0))||isnan(runMat(0))){
//         //   Rcout << "Hit nan" << endl;
//         //   goto stop;
//         // }
//         // Rcout << "Step 1.5" << endl;
//         
//         //Compute omegas
//         for (int o=0; o<gam_point_num; o++){
//           for (int n=0; n<dim_num; n++){
//             theta_vec(n) = theta_mat(o,n);
//           }
//           omega_vec = omega(theta_vec,gamma_vec,max_ord);
//           omega_mat.col(o) = omega_vec;
//         }
//         
//         //Compute sample-side quantities
//         double run_gamma = 0.0;
//         for (int o=0; o<num_subsamp; o++){
//           
//           for (int q=0; q<gam_point_num; q++){
//             
//             //Compute gamma
//             run_gamma = 0.0;
//             for (int n=0; n<dim_num; n++){
//               run_gamma += -omega_mat(n,q)*abs( des[n+m*dim_num] - rnd(o,n) );
//             }
//             run_gamma = exp(run_gamma);
//             
//             // Rcout << "Step 1.51" << endl;
//             
//             //Compute Delta
//             for (int n=0; n<dim_num; n++){
//               Delta(n) = omega_mat(n,q) / abs( des[n+m*dim_num] - rnd(o,n) + m_eps);
//             }
//             
//             // Rcout << "Step 1.52" << endl;
//             
//             //Update numerator
//             for (int n=0; n<dim_num; n++){
//               runVec(n) += (1.0/num_subsamp)*(1.0/gam_point_num)*run_gamma*Delta(n)*rnd(o,n);
//             }
//             
//             // Rcout << "Step 1.53" << endl;
//             
//             //Update denominator
//             for (int n=0; n<dim_num; n++){
//               runMat(n) += (1.0/num_subsamp)*(1.0/gam_point_num)*run_gamma*Delta(n);
//             }
//             
//             // Rcout << "Step 1.54" << endl;
//             
//           }
//         }
//         
//         // Rcout << "Step 2" << endl;
//         // if (isnan(runVec(0))||isnan(runMat(0))){
//         //   Rcout << "Hit nan" << endl;
//         //   goto stop;
//         // }
//         
//         // Rcout << "Step 3" << endl;
//         
//         //Compute design-side quantities
//         for (int o=0; o<des_num; o++){
//           //...exclude current design point
//           if (o != m){
//             for (int q=0; q<gam_point_num; q++){
//               
//               //Compute gamma
//               double run_gamma = 0.0;
//               for (int n=0; n<dim_num; n++){
//                 run_gamma += -omega_mat(n,q)*abs( des[n+m*dim_num] - des[n+o*dim_num] );
//               }
//               run_gamma = exp(run_gamma);
//               
//               //Update numerator
//               for (int n=0; n<dim_num; n++){
//                 runVec(n) += (1.0/des_num)*(1.0/gam_point_num)*run_gamma * omega_mat(n,q) * sgn(des[n+m*dim_num]-des[n+o*dim_num]);
//               }
//               
//             }
//           }
//         }
//         
//         // Rcout << "Step 3" << endl;
//         // if (isnan(runVec(0))||isnan(runMat(0))){
//         //   Rcout << "Hit nan" << endl;
//         //   goto stop;
//         // }
//         
//         // Rcout << "Step 4" << endl;
//         
//         double theta_const = 0.0;
//         for (int q=0; q<gam_point_num; q++){
//           for (int n=0; n<dim_num; n++){
//             theta_const += pow(omega_mat(n,q),2.0);
//           }
//         }
//         theta_const = ((double)(des_num-1))/(des_num*gam_point_num) * theta_const;
//         
//         for (int n=0; n<dim_num; n++){
//           //Update runVec
//           runVec(n) += theta_const*des[n+m*dim_num];
//           //Update runMat
//           runMat(n) += theta_const; 
//           runMat(n) = 1.0/runMat(n);
//         }
//         
//         // Rcout << "Step 4" << endl;
//         // if (isnan(runVec(0))||isnan(runMat(0))){
//         //   Rcout << "Hit nan" << endl;
//         //   goto stop;
//         // }
//         
//         //Update point
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = runMat(n)*runVec(n);
//         }
//         
//         //Update bounds
//         for (int n=0; n<dim_num; n++){
//           xprime[n] = min( max( xprime[n], bd(n,0)), bd(n,1));
//         }
//         
//         // Rcout << "Step 5" << endl;
//         
//         //Update design point
//         double runtol = 0.0;
//         for (int n=0; n<dim_num; n++){
//           runtol += pow(des[n+m*dim_num] - xprime[n],2);
//         }
//         runtol = sqrt(runtol);
//         for (int n=0; n<dim_num; n++){
//           des[n+m*dim_num] = xprime[n];
//         }
//         
//         //Update cont2 flag
//         if (runtol < innertol){
//           cont2 = false;
//         }
//         it_num2 ++;
//         
//       }
//       
//       // //Update design point 
//       // for (int n=0; n<dim_num; n++){
//       //   des[n+m*dim_num] = xprime[n];
//       // }
//       
//     }
//     
//     //Output time
//     time_t end = time(0);
//     double sec = difftime(end, start);
//     Rcout << "Iteration time: " << sec * 1000.0 << endl;
//     
//     //Increment and update cont flag 
//     it_num++;
//     //Check maximum iterations
//     if (it_num >= it_max){
//       cont = false;
//     }
//     
//     //Check convergence
//     double rundiff = 0.0;
//     for (int n=0; n<des_num; n++){
//       for (int o=0; o<dim_num; o++){
//         rundiff += abs(des[o+n*dim_num]-prevdes[o+n*dim_num]);
//       }
//     }
//     if (rundiff < outertol){
//       cont = false;
//     }
//   }
//   
//   //Output the final NumericMatrix
//   stop:
//     NumericMatrix retdes(des_num, dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       retdes(i,j) = des[j+i*dim_num];
//     }
//   }
//   return(retdes);
//   
// }

// // [[Rcpp::export]]
// NumericMatrix unif_largep(NumericMatrix& Rcpp_inides,
//                           int it_max, int inn_it_max, double innertol, double outertol, double epsilon, double rate,
//                           int num_proc) {
//   int des_num = Rcpp_inides.nrow();
//   int dim_num = Rcpp_inides.ncol();
//   arma::mat inides(des_num, dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       inides(i,j) = Rcpp_inides(i,j);
//     }
//   }
//   
//   //Do SBD
//   unif_largep_cpp(inides, it_max, inn_it_max, innertol, outertol, epsilon, rate, num_proc);
//   
//   //Return matrix
//   NumericMatrix retdes(des_num, dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       retdes(i,j) = inides(i,j);
//     }
//   }
//   return(retdes);
// }



//-------------------------------------------------------------------------------
// Old code
//-------------------------------------------------------------------------------

// // [[Rcpp::export]]
// arma::vec rnd_grad(std::vector<double>& des, arma::mat& point_mat, arma::vec& theta_vec, int K){
//   //Generate features for gradient descent
//   
//   // double runcos = 0.0;//for des
//   // double runcos2 = 0.0;//for points
//   // double runsin = 0.0;//for des
//   
//   // std::vector<double> des(des_tmp.n_rows*des_tmp.n_cols);
//   // for (int i=0; i<des_tmp.n_rows; i++){
//   //   for (int j=0; j<des_tmp.n_cols; j++){
//   //     des[j+i*des_tmp.n_cols] = des_tmp(i,j);
//   //   }
//   // }
//   
//   // std::random_device rs; // random seed
//   std::default_random_engine generator{static_cast<long unsigned int>(time(0))};
//   int num_pts = point_mat.n_rows;
//   int dd = point_mat.n_cols;
//   int num_des = des.size()/dd;
//   
//   arma::mat phii(K,num_des);
//   arma::vec phides(K);
//   arma::vec phipts(K);
//   arma::mat omega_mat(K,dd);
//   arma::vec b_vec(K);
//   phii.fill(0.0);
//   phides.fill(0.0);
//   phipts.fill(0.0);
//   omega_mat.fill(0.0);
//   b_vec.fill(0.0);
//   double inprod = 0.0;
//   double pi = acos(-1.0);
//   
//   //Sample omegas
//   for (int j=0; j<dd; j++){
//     for (int k=0; k<K; k++){
//       std::normal_distribution<double> norm_dist (0.0,sqrt(2.0*theta_vec(j)));
//       omega_mat(k,j) = norm_dist(generator);
//     }
//   }
//   //Sample b
//   std::uniform_real_distribution<double> uni_dist (0.0,2.0*pi);
//   for (int k=0; k<K; k++){
//     b_vec(k) = uni_dist(generator);
//   }
//   
//   //Compute phis for design
//   for (int k=0; k<K; k++){
//     for (int i=0; i<num_des; i++){
//       inprod = 0.0;
//       for (int j=0; j<dd; j++){
//         inprod += omega_mat(k,j)*des[j+i*dd];
//       }
//       phii(k,i) = -sqrt(2.0)*sin(inprod+b_vec(k));
//       phides(k) += sqrt(2.0)*cos(inprod+b_vec(k));
//     }
//   }
//   
//   //Compute phis for points
//   for (int k=0; k<K; k++){
//     for (int i=0; i<num_pts; i++){
//       inprod = 0.0;
//       for (int j=0; j<dd; j++){
//         inprod += omega_mat(k,j)*point_mat(i,j);
//       }
//       phipts(k) += sqrt(2.0)*cos(inprod+b_vec(k));
//     }
//   }
//   
//   //Combine together into gradient
//   arma::vec grd(num_des*dd);
//   grd.fill(0.0);
//   for (int i=0; i<num_des; i++){
//     for (int j=0; j<dd; j++){
//       for (int k=0; k<K; k++){
//         grd(j+i*dd) += -(2.0/(num_des*num_pts*K))*phii(k,i)*phipts(k)*omega_mat(k,j)
//                          + (1.0/(num_des*num_des*K))*phii(k,i)*phides(k)*omega_mat(k,j);
//       }
//     }
//   }
//   
//   return (grd);
// }

// //Stuff taken out from previous psp_cpp:
// 
// //Compute sample-side quantities
// vec Delta(dim_num);
// double run_gamma = 0.0;
// for (int o=0; o<point_num; o++){
//   
//   for (int q=0; q<gam_point_num; q++){
//     
//     //Compute gamma
//     run_gamma = 0.0;
//     for (int n=0; n<dim_num; n++){
//       run_gamma += -omega_mat(n,q)*abs( des[n+m*dim_num] - rnd(o,n) );
//       // if (run_gamma != run_gamma){
//       //   Rcout << "omega: " << omega_mat(n,q) << endl;
//       //   Rcout << "des: " << des[n+m*dim_num] << endl;
//       //   Rcout << "rnd: " << rnd(o,n) << endl;
//       //   goto stop;
//       // }
//     }
//     run_gamma = exp(run_gamma);
//     
//     //Compute Delta
//     for (int n=0; n<dim_num; n++){
//       Delta(n) = omega_mat(n,q) / abs( des[n+m*dim_num] - rnd(o,n) + m_eps);
//     }
//     
//     //Update numerator
//     for (int n=0; n<dim_num; n++){
//       runVec(n) += (1.0/point_num)*(1.0/gam_point_num)*run_gamma*Delta(n)*rnd(o,n);
//     }
//     
//     //Update denominator
//     for (int n=0; n<dim_num; n++){
//       runMat(n) += (1.0/point_num)*(1.0/gam_point_num)*run_gamma*Delta(n);
//       // if (isinf(runMat(n))){
//       //   Rcout << "gam_point_num " << gam_point_num << endl;
//       //   Rcout << "point_num " << point_num << endl;
//       //   Rcout << "run_gamma " << run_gamma << endl;
//       //   Rcout << "omega_mat: " << omega_mat(n,q) << endl;
//       //   Rcout << "denom: " << des[n+m*dim_num] - rnd(o,n) << endl;
//       //   Rcout << "Delta: " << Delta(n) << endl;
//       //   Rcout << (1.0/point_num)*(1.0/gam_point_num)*run_gamma*Delta(n) << endl;
//       //   Rcout << "Part 1" << endl;
//       //   goto stop;
//       // }
//       // if (isinf(runVec(n))){
//       //   Rcout << "Part 1" << endl;
//       //   goto stop;
//       // }
//     }
//   }
// }
// 
// //Compute design-side quantities
// for (int o=0; o<des_num; o++){
//   //...exclude current design point
//   if (o != m){
//     for (int q=0; q<gam_point_num; q++){
//       
//       //Compute gamma
//       double run_gamma = 0.0;
//       for (int n=0; n<dim_num; n++){
//         run_gamma += -omega_mat(n,q)*abs( des[n+m*dim_num] - des[n+o*dim_num] );
//       }
//       run_gamma = exp(run_gamma);
//       
//       //Update numerator
//       for (int n=0; n<dim_num; n++){
//         runVec(n) += (1.0/des_num)*(1.0/gam_point_num)*run_gamma * omega_mat(n,q) * sgn(des[n+m*dim_num]-des[n+o*dim_num]);
//       }
//       
//     }
//   }
// }
// 
// //Add contribution from \theta \theta^T
// // mat tmpMat;
// // tmpMat.zeros(dim_num,dim_num);
// // double tmp = 0.0;
// // for (int q=0; q<gam_point_num; q++){
// //   for (int n=0; n<dim_num; n++){
// //     for (int nn=0; nn<dim_num; nn++){
// //       tmp = ((double)(des_num-1))/(des_num*gam_point_num)*theta[n+q*dim_num]*theta[nn+q*dim_num];
// //       runMat(n,nn) += tmp;
// //       tmpMat(n,nn) += tmp;
// //     }
// //   }
// // }
// double theta_const = 0.0;
// for (int q=0; q<gam_point_num; q++){
//   for (int n=0; n<dim_num; n++){
//     theta_const += pow(omega_mat(n,q),2.0);
//   }
// }
// theta_const = ((double)(des_num-1))/(des_num*gam_point_num) * theta_const;
// 
// for (int n=0; n<dim_num; n++){
//   //Update runVec
//   runVec(n) += theta_const*des[n+m*dim_num];
//   //Update runMat
//   runMat(n) += theta_const;
//   runMat(n) = 1.0/runMat(n);
// }
// 
// //Update point
// for (int n=0; n<dim_num; n++){
//   xprime[n] = runMat(n)*runVec(n);
// }

// // SGD version of psp_cpp
// // [[Rcpp::export]]
// NumericMatrix psp_cpp(NumericMatrix& Rcpp_inides, NumericVector& distind, List distparam,
//                       NumericVector& gam_param, arma::vec& gamma_vec, int max_ord,
//                       NumericMatrix& distsamp, bool thinind,
//                       NumericMatrix& gamsamp, bool gamind, NumericMatrix& bd,
//                       int point_num, int gam_point_num, int K, int it_max, double tol, double eta, int num_proc){
//   // Description of Inputs:
//   // Rcpp_point          - Sample data
//   // Rcpp_inides         - Initial design
//   // bound               - Upper and lower bound
//   // it_max              - Maximum number of iterations
//   // tol                 - Tolerance for stopping blockwise updates
// 
//   double gam_shape=gam_param[0];
//   double gam_scale=gam_param[1];
// 
//   int des_num = Rcpp_inides.nrow();
//   int dim_num = Rcpp_inides.ncol();
// 
//   int it_num = 0; //keeps track of iteration number
//   bool cont = true; //stopping boolean for outside BCD loop
//   // bool cont2 = true; //stopping boolean for inside CCCP loop
// 
//   //Containers for optimization
//   std::vector<double> prevdes(des_num*dim_num);
//   // std::vector<double> xprime(dim_num);
//   std::vector<double> tmpvec(dim_num);
//   std::default_random_engine generator;
//   int distint = 0;
//   // omp_set_num_threads(num_proc);
// 
//   //Vectorize design and points
//   std::vector<double> des(des_num*dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       des[j+i*dim_num] = Rcpp_inides(i,j);
//     }
//   }
//   std::vector<double> grd(des_num*dim_num);
// 
//   //TSGD
//   arma::mat rnd(point_num,dim_num);
//   while (cont){
// 
//     Rcout << "PSP: Iteration " << it_num << "/" << it_max << endl;
//     // time_t start = time(0);
// 
//     //Update prevdes
//     for (int i=0; i<des_num; i++){
//       for (int j=0; j<dim_num; j++){
//         prevdes[j+i*dim_num] = des[j+i*dim_num];
//       }
//     }
// 
//     // Closed-form updates
// 
//     // Generate sample from F
//     if (thinind){
//       std::default_random_engine generator;
//       std::uniform_int_distribution<int> uddist(0,distsamp.nrow()-1);
//       // NumericVector sampidx = csample_num(icridx,point_num,true); //without replacement
//       for (int i=0; i<point_num; i++){
//         for (int j=0; j<dim_num; j++){
//           rnd(i,j) = distsamp(uddist(generator),j);
//         }
//       }
//     }else{
//       for (int n=0; n<dim_num; n++){
//         distint = distind(n);
//         switch( distint ){
//         case 1:
//         {
//           SEXP unill = distparam[n];
//           NumericVector uniyy(unill);
//           std::uniform_real_distribution<double> uni_dist (uniyy(0),uniyy(1));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = uni_dist(generator);
//           }
//           break;
//         }
//         case 2:
//         {
//           SEXP normll = distparam[n];
//           NumericVector normyy(normll);
//           std::normal_distribution<double> norm_dist (normyy(0),normyy(1));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = norm_dist(generator);
//           }
//           break;
//         }
//         case 3:
//         {
//           SEXP expll = distparam[n];
//           NumericVector expyy(expll);
//           std::exponential_distribution<double> exp_dist (expyy(0));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = exp_dist(generator);
//           }
//           break;
//         }
//         case 4:
//         {
//           SEXP gamll = distparam[n];
//           NumericVector gamyy(gamll);
//           std::gamma_distribution<double> gam_dist (gamyy(0),gamyy(1));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = gam_dist(generator);
//           }
//           break;
//         }
//         case 5:
//         {
//           SEXP lnll = distparam[n];
//           NumericVector lnyy(lnll);
//           std::lognormal_distribution<double> ln_dist (lnyy(0),lnyy(1));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = ln_dist(generator)/ ( exp(pow(lnyy(1),2.0)-1.0) * exp(2*lnyy(0)+pow(lnyy(1),2.0)) );
//           }
//           break;
//         }
//         case 6:
//         {
//           SEXP tll = distparam[n];
//           NumericVector tyy(tll);
//           std::student_t_distribution<double> t_dist (tyy(0));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = t_dist(generator);
//           }
//           break;
//         }
//         case 7:
//         {
//           SEXP wbll = distparam[n];
//           NumericVector wbyy(wbll);
//           std::weibull_distribution<double> wb_dist (wbyy(0),wbyy(1));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = wb_dist(generator);
//           }
//           break;
//         }
//         case 8:
//         {
//           SEXP cauchll = distparam[n];
//           NumericVector cauchyy(cauchll);
//           std::cauchy_distribution<double> cauch_dist (cauchyy(0),cauchyy(1));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = cauch_dist(generator);
//           }
//           break;
//         }
//         case 9:
//         {
//           SEXP betall = distparam[n];
//           NumericVector betayy(betall);
//           sftrabbit::beta_distribution<> beta_dist (betayy(0),betayy(1));
//           for (int o=0; o<point_num; o++){
//             rnd(o,n) = beta_dist(generator);
//           }
//           break;
//         }
//         }
//       }
//     }
// 
//     //Generate sample from \theta
//     arma::mat theta_mat(gam_point_num,dim_num);
//     if (gamind){
//       std::default_random_engine generator;
//       std::uniform_int_distribution<int> uddist(0,gamsamp.nrow()-1);
//       for (int i=0; i<gam_point_num; i++){
//         for (int j=0; j<dim_num; j++){
//           theta_mat(i,j) = gamsamp(uddist(generator),j);
//         }
//       }
//     }
//     else{
//       std::default_random_engine rndgen;
//       std::gamma_distribution<double> dist(gam_shape,gam_scale);
//       for (int i=0; i<gam_point_num; i++){
//         for (int n=0; n<dim_num; n++){
//           theta_mat(i,n) = dist(rndgen);
//         }
//       }
//     }
// 
//     //Compute randomized gradient
//     arma::vec tmpgrd(des_num*dim_num);
//     for (int n=0; n<(des_num*dim_num); n++){
//       grd[n] = 0.0;
//     }
//     for (int i=0; i<gam_point_num; i++){
//       arma::vec theta_vec(dim_num);
//       for (int n=0; n<dim_num; n++){
//         theta_vec(n) = theta_mat(i,n);
//       }
//       tmpgrd = rnd_grad(prevdes, rnd, theta_vec, K);
//       for (int n=0; n<(des_num*dim_num); n++){
//         grd[n] += (1.0/gam_point_num)*tmpgrd(n);
//       }
//     }
//     
//     // std::cout << tmpgrd(0) << std::endl;
//     // std::cout << grd[0] << std::endl;
//     // std::cout << eta << std::endl;
//     
//     //One step of SGD
//     for (int m=0; m<des_num; m++){
//       for (int n=0; n<dim_num; n++){
//         // des[n+m*dim_num] = des[n+m*dim_num] - eta*exp(-(double)it_num) * grd[n+m*dim_num];
//         des[n+m*dim_num] = des[n+m*dim_num] - eta/(it_num+1) * grd[n+m*dim_num];
//         // des[n+m*dim_num] = des[n+m*dim_num] - eta * grd[n+m*dim_num];
//       }
//     }
// 
//     //Update bounds
//     for (int m=0; m<des_num; m++){
//       for (int n=0; n<dim_num; n++){
//         des[n+m*dim_num] = min( max( des[n+m*dim_num], bd(n,0)), bd(n,1));
//       }
//     }
// 
//     // //Compute running tolerance
//     // double runtol = 0.0;
//     // for (int n=0; n<dim_num; n++){
//     //   runtol += pow(des[n+m*dim_num] - xprime[n],2);
//     // }
//     // runtol = sqrt(runtol);
// 
//     //Increment and update cont flag
//     it_num++;
//     //Check maximum iterations
//     if (it_num >= it_max){
//       cont = false;
//     }
//     // //Check convergence (point with maximum movement)
//     // double maxdiff = 0.0;
//     // double rundiff = 0.0;
//     // for (int n=0; n<des_num; n++){
//     //   rundiff = 0.0;
//     //   for (int o=0; o<dim_num; o++){
//     //     rundiff += std::pow(des[o+n*dim_num]-prevdes[o+n*dim_num],2.0);
//     //   }
//     //   maxdiff = max(maxdiff,rundiff);
//     // }
//     // // Rcout << "maxdiff: " << maxdiff << endl;
//     // // Rcout << "it_num: " << it_num << ", it_max: " << it_max << endl;
//     // 
//     // if (maxdiff < tol){
//     //   cont = false;
//     // }
// 
//   }
// 
//   stop:
//   //Output the final NumericMatrix
//   NumericMatrix retdes(des_num, dim_num);
//   for (int i=0; i<des_num; i++){
//     for (int j=0; j<dim_num; j++){
//       retdes(i,j) = des[j+i*dim_num];
//     }
//   }
// 
//   // return List::create(Named("retdes")=retdes,Named("rnd")=rnd);
//   return (retdes);
// 
// }

// 
// double delta(arma::vec& omega_vec){
//   double omega_max = 0.0;
//   // double ret = 0.0; 
//   for (int i=0; i<omega_vec.size(); i++){
//     omega_max = max(omega_max,omega_vec(i));
//   }
//   // for (int i=0; i<omega_vec.size(); i++){
//   //   ret += std::pow(omega_vec(i),2.0);
//   // }
//   // ret = sqrt(ret);
//   double ret = omega_max * 4 / exp(1.0);
//   return(ret);
// }
// 
// List psp_mi(std::vector<double>& theta_mat, arma::mat& samp_mat, std::vector<double> des_mat,
//             arma::vec& gamma_vec, int m, int gam_point_num, int des_num, int max_ord){
//   
//   // int gam_point_num = theta_mat.n_rows;
//   int point_num = samp_mat.n_rows;
//   int dim_num = samp_mat.n_cols;
//   // int des_num = des_mat.n_cols;
//   arma::vec theta_vec(dim_num);
//   // arma::vec samp_vec(dim_num);
//   arma::vec omega_vec(dim_num);
//   double del;
//   arma::mat omega_mat(dim_num, gam_point_num);
//   arma::vec delta_vec(gam_point_num);
//   arma::vec runVec(dim_num); //for vector in minimizer
//   // arma::vec runVec1(dim_num); //for vector in minimizer
//   // arma::vec runVec2(dim_num); //for vector in minimizer
//   // arma::vec runVec3(dim_num); //for vector in minimizer
//   runVec.fill(0.0);
//   // runVec1.fill(0.0);
//   // runVec2.fill(0.0);
//   // runVec3.fill(0.0);
//   arma::vec runMat(dim_num); //for matrix in minimizer
//   runMat.fill(0.0);
//   arma::vec ret(dim_num); //to return
//   double run_gamma;
//   
//   // cout << "des_num" << des_num << endl;
//   // cout << "point_num" << point_num << endl;
//   // cout << "gam_point_num" << gam_point_num << endl;
//   // cout << "dim_num" << dim_num << endl;
//   
//   //Compute omegas and deltas
//   // cout << "Computing Omegas and Deltas ... " << endl;
//   for (int q=0; q<gam_point_num; q++){
//     // cout << q << endl;
//     for (int n=0; n<dim_num; n++){
//       theta_vec(n) = theta_mat[n+q*dim_num];
//     }
//     omega_vec = omega(theta_vec,gamma_vec,max_ord);
//     // cout << q << ".1" << endl;
//     del = delta(omega_vec);
//     // cout << q << ".2" << endl;
//     omega_mat.col(q) = omega_vec;
//     delta_vec(q) = del;
//   }
//   
//   //Compute sample-side quantities
//   // cout << "Computing sample-size quantities ... " << endl;
//   for (int o=0; o<point_num; o++){
//     for (int q=0; q<gam_point_num; q++){
//       
//       //Assign vectors
//       // theta_vec = theta_mat.row(q);
//       
//       //Compute kernel
//       run_gamma = 0.0;
//       for (int n=0; n<dim_num; n++){
//         run_gamma += -omega_mat(n,q) * pow(des_mat[n+m*dim_num] - samp_mat(o,n), 2.0);
//       }
//       run_gamma = exp(run_gamma);
//       
//       //Increment runVec and runMat
//       for (int n=0; n<dim_num; n++){
//         runMat(n) += (2.0/(point_num*gam_point_num)) * run_gamma * omega_mat(n,q);
//       }
//       for (int n=0; n<dim_num; n++){
//         runVec(n) += (2.0/(point_num*gam_point_num)) * run_gamma * omega_mat(n,q) * samp_mat(o,n);
//       }
//       
//     }
//   }
//   
//   // cout << "Computing design-size quantities ... " << endl;
//   //Compute design-side quantities
//   for (int o=0; o<des_num; o++){
//     //...exclude current design point
//     if (o != m){
//       for (int q=0; q<gam_point_num; q++){
//         
//         //Compute kernel
//         run_gamma = 0.0;
//         for (int n=0; n<dim_num; n++){
//           run_gamma += -omega_mat(n,q) * pow(des_mat[n+m*dim_num] - des_mat[n+o*dim_num], 2.0);
//         }
//         run_gamma = exp(run_gamma);
//         
//         //Increment runVec
//         for (int n=0; n<dim_num; n++){
//           runVec(n) += (2.0/(des_num*gam_point_num)) * run_gamma * omega_mat(n,q) * (des_mat[n+m*dim_num] - des_mat[n+o*dim_num]);
//         }
//         
//       }
//     }
//   }
//   // cout << "Computing final quantities ... " << endl;
//   for (int q=0; q<gam_point_num; q++){
//     //Increment runVec and runMat
//     for (int n=0; n<dim_num; n++){
//       runMat(n) += ( 4.0*(des_num-1)/(des_num*gam_point_num) ) * delta_vec(q);
//     }
//     for (int n=0; n<dim_num; n++){
//       runVec(n) += ( 4.0*(des_num-1)/(des_num*gam_point_num) ) * delta_vec(q) * des_mat[n+m*dim_num];
//     }
//   }
//   
//   // cout << "Computing closed-form quantities ... " << endl;
//   //Compute closed-form solution
//   for (int n=0; n<dim_num; n++){
//     ret(n) = (1.0/runMat(n)) * runVec(n);
//   }
//   
//   return(List::create(Named("delta")=delta_vec, Named("omega")=omega_mat,
//                       // Named("runVec1")=runVec1, Named("runVec2")=runVec2, Named("runVec3")=runVec3, 
//                       Named("runMat")=runMat, Named("ret")=ret));
//   // return(ret);
//   
// }
// 
// // // [[Rcpp::export]]
// // List psp_mi_test(arma::mat& theta_mat, arma::mat& samp_mat, arma::mat& des_mat,
// //                    arma::vec& gamma_vec, int m, int max_ord){
// //   int des_num = des_mat.n_rows;
// //   int dim_num = des_mat.n_cols;
// //   int point_num = samp_mat.n_rows;
// //   int gam_point_num = theta_mat.n_rows;
// //   
// //   //Transfering to std vectors
// //   std::vector<double> des(des_num*dim_num);
// //   std::vector<double> theta(gam_point_num*dim_num);
// //   for (int i=0; i<des_num; i++){
// //     for (int j=0; j<dim_num; j++){
// //       des[j+i*dim_num] = des_mat(i,j);
// //     }
// //   }
// //   for (int i=0; i<gam_point_num; i++){
// //     for (int j=0; j<dim_num; j++){
// //       theta[j+i*dim_num] = theta_mat(i,j);
// //     }
// //   }
// // 
// //   List ret = psp_mi(theta, samp_mat, des, gamma_vec, m, gam_point_num, des_num, max_ord);
// //   
// //   return(ret);
// // }
// 
