# ifndef _fct_class
# define _fct_class
#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

/* Functional object
* (c) Niels Olsen, 2016
*
* Base class for functional objects, to be used in FDA or related settings.
* 
* Contains eight virtual functions, of which four must be supplied by subclasses.
*
*
* Examples of subclasses: B-spline, Fourier, polynomial.
* 
*/
class functionObject { // Ny funktionalitet: Opretter og sletter sig selv i databasen.
public:
  const unsigned int n_basis; // Allowed range: 1-32767.
  bool suppressWarnings; // Kan implementeres af sub-klasser.

  // Zeroth derivatives
  //
  virtual arma::vec eval_coefs(double x) = 0;
  
  // Overskriv gerne
  virtual arma::mat eval_coefs(const arma::vec& x) {
    
    mat ud = mat(n_basis, x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud.col(kk) =  eval_coefs(x(kk));
    return ud.t();
  };
  
  virtual double eval_fct(double x, const arma::vec& coefs) {
    return dot(eval_coefs(x), coefs);
  };
  
  // Overskriv gerne
  virtual arma::vec eval_fct(const arma::vec& x, const arma::vec& coefs) {
    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");
    
    vec ud(x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud(kk) = eval_fct(x(kk), coefs);
    return ud;
  };

  // Matrixinput
  virtual arma::mat eval_fct_mat(const arma::vec& x, const arma::mat& coefs) {
    if (n_basis != coefs.n_rows) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");

    mat ud(x.n_elem, coefs.n_cols, fill::none);
    for (unsigned int kk = 0; kk < coefs.n_cols; kk++) ud.col(kk) = eval_fct(x, coefs.col(kk));
    return ud;
  };

  // First derivatives
  //
  virtual arma::vec eval_deriv_coefs(double x) = 0;
  virtual arma::mat eval_deriv_coefs(const arma::vec& x) {
    
    mat ud(n_basis, x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud.col(kk) =  eval_deriv_coefs(x(kk));
    return ud.t();
  };
  
  virtual double eval_deriv(double x, const arma::vec& coefs) = 0;
  virtual arma::vec eval_deriv(const arma::vec& x, const arma::vec& coefs) {
    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");
    
    vec ud(x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud(kk) = eval_deriv(x(kk), coefs);
    
    return ud;
  };
  // Matrixinput
  virtual arma::mat eval_deriv_mat(const arma::vec& x, const arma::vec& coefs) {
    if (n_basis != coefs.n_rows) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");

    mat ud(x.n_elem, coefs.n_cols, fill::none);
    for (unsigned int kk = 0; kk < coefs.n_cols; kk++) ud.col(kk) = eval_deriv(x, coefs.col(kk));
    return ud;
  };

  // Second derivatives
  //
  virtual arma::vec eval_d2_coefs(double x) {
    stop("Not implemented for this functional basis");
  };
  virtual arma::mat eval_d2_coefs(const arma::vec& x) {

    mat ud(n_basis, x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud.col(kk) =  eval_d2_coefs(x(kk));
    return ud.t();
  };

  virtual double eval_d2(double x, const arma::vec& coefs) {
    return arma::dot(eval_d2_coefs(x), coefs);
  };
  virtual arma::vec eval_d2(const arma::vec& x, const arma::vec& coefs) {

    if (n_basis != coefs.n_elem) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");

    vec ud(x.n_elem, fill::none);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud(kk) = eval_d2(x(kk), coefs);

    return ud;
  };
  // Matrixinput
  virtual arma::vec eval_d2_mat(const arma::vec& x, const arma::mat& coefs) {
    if (n_basis != coefs.n_rows) throw std::invalid_argument("Coeffienct vector must have same length as number of bases");

    mat ud(x.n_elem, coefs.n_cols);
    for (unsigned int kk = 0; kk < x.n_elem; kk++) ud.col(kk) = eval_d2(x, coefs.col(kk));
    return ud;
  };
  
  protected: functionObject(size_t basis) : n_basis(basis), suppressWarnings(false) {
    
    if (basis < 1)  // Opdateret 28-05-19. Kan ændres til nul senere.
      throw std::invalid_argument("Number of bases must be strictly positive!");
    medlemmer.insert((size_t) this );
  };
    
    public:
      virtual ~functionObject() {
        
        medlemmer.erase((size_t) this);
      };
      
      // R-del!
      //
      virtual Rcpp::List returnObject() { 
        List ret;
        ret["n_basis"] = (int) n_basis;
        ret["obj"] = "Functional Object. Please overwrite.";
        return ret;
      };
};


# endif
