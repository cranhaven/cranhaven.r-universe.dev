#ifndef pol_h
#define pol_h



#include "function_class.h"


class polynomial : public functionObject {
  
public:
  
  const int deg; // grad af polynomium.
  
  // konstruktor
  polynomial(size_t pol_order) : functionObject(pol_order + 1), deg(pol_order)
  {
    if (deg < 1) stop("Order must be strictly positive!"); 
  }
  
  arma::vec eval_coefs(double x) {
    
    vec ret = vec(n_basis, fill::none);
    double x0 = ret(0) = 1.0;
    for (unsigned int i=1; i< n_basis; i++) {
      ret(i) = x0 *= x;
    }
    return ret;
  }
  
  arma::mat eval_coefs(const arma::vec& x) {
    
    mat ud = mat(x.n_elem, n_basis, fill::none);
    double x0;
    rowvec ret = rowvec(n_basis, fill::none);

    for (unsigned int zz = 0; zz < x.n_elem; zz++) {
      const double yy = x(zz);
      x0 = ret(0) = 1.0;
      for (unsigned int i=1; i< n_basis; i++) {
        ret(i) = x0 *= yy;
      }
      ud.row(zz) = ret;
    }
    return ud;
  }
  
  double eval_fct(double x, const arma::vec& coefs) {
    
    double ud = coefs(0);
    double x0 = 1.0;
    for (unsigned int i=1; i< n_basis; i++) {
      x0 *= x;
      ud += x0* coefs(i);
    }
    return ud;
    
  }
  
  arma::vec eval_fct(const arma::vec& x, const arma::vec& coefs) { // kan måske forbedres, brug pointer til element.
    if (n_basis != coefs.n_elem) stop("Coeffienct vector must have same length as number of bases");
    
    vec ud = vec(x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud(kk) = eval_fct(x(kk), coefs);
    
    return ud;
  }
  
  // Evaluerer d/dx B(x)
  arma::vec eval_deriv_coefs(double x)  {
    vec ret = vec(n_basis, fill::none);
    double x0 = 1.0;
    ret(0) = 0.0;
    for (unsigned int i=1; i< n_basis; i++) {
      ret(i) = i*x0;
      x0 *= x;
    }
    return ret;
  }
  
  
  // Evaluaterer d/dx p(x) ganget på koefficienter
  double eval_deriv(double x, const arma::vec& coefs) {
    if (n_basis != coefs.n_elem) stop("Coeffienct vector must have same length as number of bases");
    
    double ud = 0.0;
    double x0 = 1.0;
    
    for (unsigned int i=1; i< n_basis; i++) {
      ud += i*x0* coefs(i);
      x0 *= x;
    }
    return ud;
    
  };
  
  arma::vec eval_deriv(const arma::vec& x, const arma::vec& coefs) {
    if (n_basis != coefs.n_elem) stop("Coeffienct vector must have same length as number of bases");
    
    vec ud = vec(x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud[kk] = eval_deriv(x[kk], coefs);
    
    return ud;
  };
  
  arma::vec eval_d2_coefs(double x) {
    vec ret = vec(n_basis, fill::none);
    double x0 = 1.0;
    ret(0) = ret(1) = 0;
    for (unsigned int i=2; i< n_basis; i++) {
      ret(i) = i*(i-1)*x0;
      x0 *= x;
    }
    return ret;
  };

  public: Rcpp::List returnObject() { 
    List ret;
    ret["n_basis"] = (int) n_basis;
    ret["object_type"] = "Polynomial basis";
    ret["degree"] = (int) deg;
    return ret;
  };
  
};    

//Initializer
//[[Rcpp::export]]
SEXP init_pol_basis(int pol_order) {

  polynomial *pp = new polynomial(pol_order);
  XPtr<polynomial> pp_ptr(pp, true);
  return pp_ptr;
};


#endif

