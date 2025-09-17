# ifndef _fourierb
# define _fourierb

#include <RcppArmadillo.h>
#include "function_class.h"
#include <cmath>

using namespace arma;

/*
* Fourier Basis implementation in Armadillo for use with Rcpp and elsewhere
* (c) Niels Olsen, 2016+
*/

/*
* Convention for indexation: First index (index no. 0): intercept. Odd indices: sinus coefficients.
* Even indices: cosinus coefficients
*
*/
class fourierBasis : public functionObject {
  
public:
  const double left_end;
  const double right_end;
  const double length;
  
  const int order;
  
  protected: const double inv_length;
    
    // konstructor
public:
  fourierBasis(double left, double right, int f_order): functionObject(2 * f_order + 1),
  left_end(left), right_end(right), length(right- left), order(f_order),  inv_length(2*M_PI/length)  {
    if (f_order < 1) throw std::invalid_argument("Order must be strictly positive.");
  }
  
  arma::vec eval_coefs(double x) {
    const double z = (x-left_end) * inv_length;
    
    vec ret(n_basis, fill::none);
    ret(0) = 1;
    for (int i=1; i<=order; i++) {
      ret(2*i-1) = sin(z*i);
      ret(2*i) = cos(z*i);
    }
    return ret;
  }
  
  arma::mat eval_coefs(const arma::vec& x) {
    mat ud(x.n_elem , n_basis, fill::none);
    
    for (unsigned int kk = 0; kk < x.n_elem; kk++) {
      
      const double z = (x(kk)-left_end) * inv_length;
      
      rowvec ret(n_basis, fill::none);
      rowvec::row_iterator rit = ret.begin();
      (*rit) = 1;
      
      for (int i=1; i<=order; i++) {
        rit++;
        *rit = sin(z*i);
        rit++;
        *rit = cos(z*i);
      }
      ud.row(kk) = ret;
    }
    return ud;
  }
  
  double eval_fct(double x, const arma::vec& coefs) {
    
    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");
    
    const double z = (x-left_end) * inv_length;
    double ud = coefs(0);
    
    for (int i=1; i<=order; i++) {
      ud += sin(z*i)*coefs(2*i-1);
      ud += cos(z*i)*coefs(2*i);
    }
    
    return ud;
    
  }
  // kan måske gøres bedre.
  arma::vec eval_fct(const arma::vec& x, const arma::vec& coefs) {
    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");
    
    vec ud(x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) {
      ud(kk) = eval_fct(x(kk), coefs);
    }
    return ud;
  }
  
  arma::vec eval_deriv_coefs(double x) {
    const double z = (x-left_end) * inv_length;
    
    vec ret(n_basis, fill::none);
    
    ret(0) = 0;
    for (int i=1; i<=order; i++) {
      ret(2*i-1) = cos(z*i)* inv_length * i;
      ret(2*i) = -sin(z*i)* inv_length * i;
    }
    
    return ret;
  }
  arma::mat eval_deriv_coefs(const arma::vec& x) {
    mat ud(x.n_elem, n_basis, fill::none);
    
    for (unsigned int kk = 0; kk < x.n_elem; kk++) {
      const double z = (x(kk)-left_end) * inv_length;
      rowvec ret(n_basis);
      
      ret(0) = 0;
      for (int i=1; i<=order; i++) {
        ret(2*i-1) = cos(z*i)* inv_length * i;
        ret(2*i) = -sin(z*i)* inv_length * i;
      }
      
      ud.row(kk)  = ret;
    }
    return ud;
  }
  
  double eval_deriv(double x, const arma::vec& coefs) {
    
    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");
    
    const double z = (x-left_end) * inv_length;
    double ud = 0;
    
    for (int i=1; i<=order; i++) {
      ud += cos(z*i)*coefs(2*i-1) * i;
      ud -= sin(z*i)*coefs(2*i) * i;
    }
    
    return inv_length*ud;
  }
  arma::vec eval_deriv(const arma::vec& x, const arma::vec& coefs) {
    
    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");
    
    vec ud = zeros<vec>(x.n_elem);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud(kk) = eval_deriv(x(kk), coefs);
    
    return ud;
  }

   arma::vec eval_d2_coefs(double x) {

    const double z = (x-left_end) * inv_length;
    vec ret(n_basis, fill::none);

    ret(0) = 0;
    for (int i=1; i<=order; i++) {
      ret(2*i-1) = -sin(z*i)* inv_length * inv_length * i * i;
      ret(2*i) = -cos(z*i)* inv_length * inv_length * i * i;
    }
    return ret;
  }

  public: Rcpp::List returnObject() { 
    List ret;
    ret["n_basis"] = (int) n_basis;
    ret["object_type"] = "Fourier basis";
    IntegerVector IV(2);
    IV(0) = left_end;
    IV(1) = right_end;
    ret["endpoints"] = IV;
    ret["harmonics_order"] = (int) order;
    return ret;
  };
};


/* This class uses trigonometrical identities:
 * cos ((k+1)x) =  cos(kx)*cos(x) - sin(kx)*sin(x)
 * sin ((k+1)x) = sin(kx)*cos(x) + cos(kx)*sin(x)
 */

class fourier_basis_trig : public fourierBasis {
  

    // constructor
public:
  fourier_basis_trig(double left, double right, int f_order): 
  fourierBasis(left, right, f_order)  {}
  
  
  arma::vec eval_coefs(double x) {
    const double z = (x-left_end) * inv_length;
    
    vec ret(n_basis, fill::none);
    ret(0) = 1;
    double si = ret(1) = sin(z);
    double co = ret(2) = cos(z);
    if (order > 1) for (int i=1; i < order; i++) {
      ret(2*i+1) = si*ret(2*i) + co*ret(2*i-1); // sinus
      ret(2*i+2) = co*ret(2*i) - si*ret(2*i-1); // cosinus
    }
    return ret;
  };
  
  arma::mat eval_coefs(const arma::vec& x) {
    
    mat ud(x.n_elem, n_basis, fill::none);
    
    for (unsigned int zz = 0; zz < x.n_elem; zz++) {
      double xx = x[zz];
      const double z = (xx-left_end) * inv_length;
      
      ud(zz,0) = 1;
      double si = ud(zz,1) = sin(z);
      double co = ud(zz,2) = cos(z);
      if (order > 1) for (int i=1; i < order; i++) {
        ud(zz,2*i+1) = si*ud(zz,2*i) + co*ud(zz,2*i-1);
        ud(zz,2*i+2) = co*ud(zz,2*i) - si*ud(zz,2*i-1);
      }
    }
    
    return ud;
  };
  
  double eval_fct(double x, const arma::vec& coefs) {
    
    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");
    
    const double z = (x-left_end) * inv_length;
    double ud = coefs(0);
    double si = sin(z);
    double sii = si;
    ud += sii*coefs(1);
    double co = cos(z);
    double coo = co;
    ud += coo*coefs(2);
    
    for (int i=2; i<=order; i++) {
      double si0 = sii;
      sii = si*coo + co*sii;
      ud += sii*coefs(2*i-1);
      coo = coo*co - si*si0;
      ud += coo*coefs(2*i);
    }
    return ud;
    
  };

    arma::vec eval_deriv_coefs(double x) {
    const double z = (x-left_end) * inv_length;

    vec ret(n_basis, fill::none);

    ret(0) = 0;

    double co = ret(1) = cos(z);
    double coo = co;
    ret(1) = inv_length * co;
    double si = sin(z);
    double sii = si;
    ret(2) = - inv_length * si;
    for (int i=2; i <= order; i++) {

      double si0 = sii;
      sii = si*coo + co*sii;
      coo = coo*co - si*si0;


      ret(2*i-1) = coo* inv_length * i;
      ret(2*i) = -sii* inv_length * i;
    }

    return ret;
  };

  arma::mat eval_deriv_coefs(const arma::vec& x) {
    mat ud(n_basis, x.n_elem, fill::none);

    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud.col(kk) =  eval_deriv_coefs(x(kk));
    return ud.t();
  };

  double eval_deriv(double x, const arma::vec& coefs) {

    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");

    const double z = (x-left_end) * inv_length;
    double ud = 0;

    double co = cos(z);
    double coo = co;
    ud += inv_length*coo*coefs(1);
    double si = sin(z);
    double sii = si;
    ud -= inv_length*sii*coefs(2);

    for (int i=2; i<=order; i++) {
      double si0 = sii;
      sii = si*coo + co*sii;
      ud -= i*inv_length*sii*coefs(2*i);
      coo = coo*co - si*si0;
      ud += i*inv_length*coo*coefs(2*i-1);
    }

    return ud;
  }

  arma::vec eval_deriv(const arma::vec& x, const arma::vec& coefs) {
    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");

    vec ud(x.n_elem, fill::none);

    for (unsigned int kk = 0; kk < x.n_elem; kk++) {
     const double z = (x(kk)-left_end) * inv_length;
      vec::const_iterator it = coefs.begin();

      double co = cos(z);
      double coo = co;
      ud[kk] = inv_length*coo*(*(++it));
      double si = sin(z);
      double sii = si;
      ud[kk] -= inv_length*sii*(*(++it));

      for (int i=2; i<=order; i++) {
        double co0 = coo;
        double si0 = sii;

        coo = co0*co - si*sii;
        ud[kk] += i*inv_length*coo*(*(++it));

        sii = si*co0 + co*sii;
        ud[kk] -= i*inv_length*sii*(*(++it));
      }
    }
    return ud;
  };

  
  
};



# endif
