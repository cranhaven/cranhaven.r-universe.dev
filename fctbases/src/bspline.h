#ifndef _bspline
#define _bspline


#include <RcppArmadillo.h>
#include "function_class.h"
#include <algorithm>
using namespace std;
using namespace arma;
using namespace Rcpp;


inline vec make_tknots(const vec& spline_knots, int deg) {

  if (deg > 0) {
    int n_el = spline_knots.n_elem;
    vec kk(n_el + deg);
    vec::iterator ii = kk.begin();
    for (int i=0; i < n_el; i++) {
      
      (*ii) = spline_knots[i];
      ii++;
    }
    
    double x = spline_knots(n_el -1);
    for ( ; ii != kk.end(); ii++) (*ii) = x;
    return kk;
  }
  
  else return spline_knots;
}


class bspline  : public functionObject {
    
public:
  const int deg;
  const int order;
  const vec knots;
  const vec tknots;
  const double x_min;// = knots(0);
  const double x_max; // = knots(knots.n_elem - 1);
  const int n_intervals; // = length(knots) - 1
  
private:
  const vec diffs;
  
protected:
  
  // Note: ....
    inline int getIndexOf(double x) {
      vec::const_iterator id = upper_bound(knots.begin(), knots.end(), x);
        if (id == knots.end()) return -1;
        else return id-knots.begin(); 
    }
    
    
public:
  // konstructor
  bspline(int spline_order, const vec& spline_knots) : functionObject(spline_knots.n_elem - 1),
  deg(spline_order-1 ), order(spline_order), 
  knots(spline_knots), tknots(make_tknots(spline_knots, deg)),
    x_min(spline_knots(0)),  x_max(spline_knots(spline_knots.n_elem - 1)),
    n_intervals(knots.n_elem -1)
     {
    if (order < 1) throw std::invalid_argument("order must be strictly positive");
    else if (spline_knots.n_elem < 2) throw std::invalid_argument("At least two knots needed.");
    else {
      for (int i = 0; i < n_intervals; i++) if (knots(i) > knots(i+1))
        throw std::invalid_argument("Knots must be increasing");
    }
  };

  arma::vec eval_coefs(double x) {

    vec ret = zeros<vec>(n_basis);
    int i = getIndexOf(x)-1;
    if (i < 0) {
      Rf_warning("Outside of range");
    }
    else {

      ret(i) = 1;

      for (int j=1; j < order; j++) {
        for (int k = i-j; k < i; k++) {

          double dd = tknots(k+j) - tknots(k);

          if (dd) ret(k) = (x- tknots(k))/dd * ret(k) +
            (tknots(k+j+1) - x)/( tknots(k+j+1) - tknots(k+1))* ret(k+1);
          else {
            ret(k) = (tknots(k+j+1) - x)/( tknots(k+j+1) - tknots(k+1))* ret(k+1);
          }
        }
        // afsluttende ..
        ret(i) = (x - tknots(i)) / (tknots(i+j) - tknots(i))* ret(i);
      }
    }
    return ret;
  };
    
    // Evaluates B-spline y at specfified values x
    // If x is outside of the range of y, 0 is returned with a warning.
    arma::mat eval_coefs(const arma::vec& x) {

     mat ud = zeros<mat>(x.n_elem, n_basis);

      for (unsigned int zz = 0; zz < x.n_elem; zz ++) {

      double xx = x[zz];

      int i = getIndexOf(xx)-1;
      if (i < 0) {
        Rf_warning("Outside of range");
      }
      else {

        ud(zz,i) = 1;

          for (int j=1; j < order; j++) {

            for (int k = i-j; k < i; k++) {
              double dd = tknots(k+j) - tknots(k);
              if (dd) ud(zz, k) = (xx- knots(k))/dd * ud(zz,k) +
                ( tknots(k+j+1) - xx)/( tknots(k+j+1) - tknots(k+1))* ud(zz,k+1);
              else {
                ud(zz,k) = (tknots(k+j+1) - xx)/( tknots(k+j+1) - tknots(k+1))* ud(zz,k+1);
              }
            }
            // afsluttende ..
            ud(zz, i) = (xx - tknots(i)) / (tknots(i+j) - tknots(i))* ud(zz,i);
          }
        }
      }
      return ud;
    };
    
    double eval_fct(double x, const arma::vec& coefs) {
      
      if (n_basis != coefs.n_elem) stop("Coeffienct vector must have same length as number of bases");

      
      int i = getIndexOf(x) - 1;
      
      if (i < 0) {
        Rf_warning("Outside of range");
        return 0;
      }
      else {
        vec ret = zeros<vec>(order);
        double ud = 0;
        
        ret(deg) = 1;
        
        if (deg > 0) {
          for (int j=1; j < order; j++) {
            for (int kk = -j; kk < 0; kk++) {
              int k = kk+i;
              
              double dd = tknots(k+j) - tknots(k);
              if (dd) ret(deg+kk) =(x- tknots(k))/ dd * ret(deg+kk) +
                ( tknots(k+j+1) - x)/( tknots(k+j+1) - tknots(k+1))* ret(deg+kk+1);
              else ret(deg+kk) = (tknots(k+j+1) - x)/( tknots(k+j+1) - tknots(k+1))* ret(deg+kk+1);
            }
            //int k = i;
            ret(deg) =(x- tknots(i))/( tknots(i+j) - tknots(i))* ret(deg);
          }
        } 
        for (int j = 0; j < order; j++) ud += ret(deg-j) * coefs(i-j);
      return ud;
      }};
    
    arma::vec eval_fct(const arma::vec& x, const arma::vec& coefs) {
      
      if (n_basis != coefs.n_elem) stop("Coeffienct vector must have same length as number of bases");
      
      vec ud = zeros<vec>(x.n_elem);
      for (unsigned int zz = 0; zz < x.n_elem; zz ++) {
        double xx = x[zz];
      
      
      int i = getIndexOf(xx) - 1;
      
      if (i < 0) {
        Rf_warning("Outside of range");
      }
      else {
        vec ret = zeros<vec>(order);

        ret(deg) = 1;

        if (deg > 0) {
          for (int j=1; j < order; j++) {
            for (int kk = -j; kk < 0; kk++) {
              int k = kk+i;

              double dd = tknots(k+j) - tknots(k);
              if (dd) ret(deg+kk) =(xx - tknots(k))/ dd * ret(deg+kk) +
                ( tknots(k+j+1) - xx)/( tknots(k+j+1) - tknots(k+1))* ret(deg+kk+1);
              else ret(deg+kk) = (tknots(k+j+1) - xx)/( tknots(k+j+1) - tknots(k+1))* ret(deg+kk+1);
            }

            ret(deg) =(xx - tknots(i))/( tknots(i+j) - tknots(i))* ret(deg);
          }
        }
        for (int j = 0; j < order; j++) ud[zz] += ret(deg-j) * coefs(i-j);

      }}
      return ud;
      };
    
    // Evaluerer d/dx B(x)
    arma::vec eval_deriv_coefs(double x)  {
      vec ret = zeros<vec>(n_basis);
      
      int i = getIndexOf(x)-1;
      if (i < 0) {
        Rf_warning("Outside of range");
      }
      
      
      else if (deg > 0) {
        
        ret(i) = 1;
        
          // B-spline af grad = orden-1
          for (int j=1; j < deg; j++) {
            for (int k = i-j; k < i; k++) {

              double dd = tknots(k+j) - tknots(k);
              
              if (dd) ret(k) =(x- tknots(k))/dd * ret(k) +
                ( tknots(k+j+1) - x)/( tknots(k+j+1) - tknots(k+1))* ret(k+1);
              else ret(k) = ( tknots(k+j+1) - x)/( tknots(k+j+1) - tknots(k+1))* ret(k+1);

            }
            
            
            ret(i) = (x - tknots(i)) / (tknots(i+j) - tknots(i))* ret(i);
          }
          
          // Afledte-del:
          
        for (int k = i-deg; k < i; k++) {
          double dd = tknots(k+deg) - tknots(k);
          if (dd) ret(k) = deg * ( ret(k) / dd -
              ret(k+1) / (tknots(k+deg+1) - tknots(k+1))) ;
      
          else ret(k) =  -deg* ret(k+1) / (tknots(k+deg+1) - tknots(k+1));
        }
        ret(i) =  deg * ret(i) / (tknots(i+deg) - tknots(i));
      }
      return ret;
    }
      
    // Evaluaterer d/dx B(x) ganget på koefficienter
    double eval_deriv(double x, const arma::vec& coefs) {
      
      // if spline order is 1, then derivative is zero.
      if (deg > 0) {
        // Find interval
        int i = getIndexOf(x)-1;
        if (i < 0) {
          Rf_warning("Outside of range");
          return 0;
        }
        
        vec ret = zeros<vec>(order);
        ret(deg) = 1;
        
        for (int j=1; j < deg; j++) {
          for (int kk = -j; kk < 0; kk++) {
            int k = kk+i;
              
            double dd = tknots(k+j) - tknots(k);
              
            if (dd) ret(deg + kk) =(x- tknots(k))/dd * ret(deg+kk) +
                (tknots(k+j+1) - x) / (tknots(k+j+1) - tknots(k+1))* ret(deg+kk+1);
            else ret(deg + kk) = 
                (tknots(k+j+1) - x) / (tknots(k+j+1) - tknots(k+1)) * ret(deg+kk+1);
            }
            //int k = i;
            ret(deg) =(x- tknots(i)) / (tknots(i+j) - tknots(i))* ret(deg);
        } 
        
        for (int kk = -deg; kk < 0; kk++) {
          int k = kk+i;
          
          double dd = tknots(k+deg) - tknots(k);
          if (dd) ret(kk+deg) =  deg * ( ret(kk+deg) / dd -
              ret(kk+deg+1) / (tknots(k+deg+1) - tknots(k+1))) ;
          else 
            ret(kk+deg) = -deg * ret(kk+deg+1) / (tknots(k+deg+1) - tknots(k+1)) ;
        }
        ret(deg) =  deg * ret(deg) / (tknots(i+deg) - tknots(i));
        
        // Gang på koefficienter
        double ud = 0;
        for (int j = 0; j < order; j++) ud += ret(deg-j) * coefs(i-j);
        return ud;
      }
      else return 0;
    };

      // Evaluerer d/dx B(x)
  arma::vec eval_d2_coefs(double x)  {

    vec ret = zeros<vec>(n_basis);
    int i = getIndexOf(x)-1;
    if (i < 0) {
      Rf_warning("Outside of range");
    }

    else if (deg > 0) {

      ret(i) = 1;

      // B-spline af grad = orden-2
      for (int j=1; j < deg-1; j++) {
        for (int k = i-j; k < i; k++) {

          double dd = tknots(k+j) - tknots(k);

          if (dd) ret(k) =(x- tknots(k))/dd * ret(k) +
            ( tknots(k+j+1) - x)/( tknots(k+j+1) - tknots(k+1))* ret(k+1);
          else ret(k) = ( tknots(k+j+1) - x)/( tknots(k+j+1) - tknots(k+1))* ret(k+1);

        }
        ret(i) = (x - tknots(i)) / (tknots(i+j) - tknots(i))* ret(i);
      }

      // Fra -2 til -1
      for (int k = i-deg; k < i; k++) {
        double dd = tknots(k+deg) - tknots(k);
        if (dd) ret(k) = deg * (ret(k) / dd - ret(k+1) / (tknots(k+deg+1) - tknots(k+1)));
        else ret(k) =  -deg* ret(k+1) / (tknots(k+deg+1) - tknots(k+1));
      }
      ret(i) =  (deg-1) * ret(i) / (tknots(i+deg-1) - tknots(i));

      // Fra -1 til 0:
      for (int k = i-deg; k < i; k++) {
        double dd = tknots(k+deg) - tknots(k);
        if (dd) ret(k) = deg * ( ret(k) / dd - ret(k+1) / (tknots(k+deg+1) - tknots(k+1))) ;
        else ret(k) =  -deg* ret(k+1) / (tknots(k+deg+1) - tknots(k+1));
      }
      ret(i) =  deg * ret(i) / (tknots(i+deg) - tknots(i));
    }

    return ret;
  };
    
    Rcpp::List returnObject() {
      List ret;
      ret["n_basis"] = (int) n_basis;
      ret["object_type"] = "B-spline";
      ret["order"] = (int) order;
      //    ret["spline_knots"] = knots;
      ret["spline_knots"] = Rcpp::NumericVector(knots.begin(), knots.end());
      return ret;
    };
    
};

class bspline_u4 : public functionObject {

private:  
  
  // Note: order is important!
  const double x_min;// = knots(0);
  const double x_max; // = knots(knots.n_elem - 1);
  const int n_intervals; // = length(knots) - 1
  
  const vec knots;
  
  const double inv_length;
  const double inv_length2;
  const double inv_length3;
  
  const int deg;
  const int order;
  const double diff; // = (x_max - x_min) / n_interval
  
public:
  bspline_u4(const vec& spline_knots) :
    functionObject(spline_knots.n_elem + 2),
    x_min(spline_knots(0)), x_max(spline_knots(spline_knots.n_elem -1)),
    n_intervals(spline_knots.n_elem -1),
    knots(spline_knots),
    inv_length(n_intervals / (x_max - x_min)),
    inv_length2(n_intervals / (x_max - x_min) / 2),
    inv_length3(n_intervals / (x_max - x_min) / 3),
    deg(3), order(4),  diff((x_max - x_min) / n_intervals)
    {
      if (n_intervals < 4) {
        stop("Sorry. At least four intervals needed.");
      }
    };

private:
  // this can surely be speeded up!
  inline int getIndexOf(double x) {
    vec::const_iterator id = upper_bound(knots.begin(), knots.end(), x);
    if (id == knots.end()) return -1;
    else return id-knots.begin(); 
  }
  

public:
  
  arma::vec eval_coefs(double x) {
    vec ud = zeros<vec>(n_basis);

    int i = getIndexOf(x)-1;
    if (i < 0) {
      Rf_warning("Outside of range");
    }
    else {

      // -2 hvis første interval ,-1 hvis næste, 0,1 hvis næstsidsts, 2 hvis sidste.
      int bcase3 = -(i < 2) - (i == 0) + (i == n_intervals-1) + (i > n_intervals - 3);


      ud[i+1] = (x-knots[i])*inv_length;
      ud[i] = (knots[i+1] - x)*inv_length;

      switch(bcase3){
      case -2:
      {
        ud[2] = (x - knots[0])* ud[1]*inv_length2;
        ud[1] = (x - knots[0])* ud[0]*inv_length + (knots[2] - x)*ud[1]*inv_length2;
        ud[0] = (knots[1] - x)*ud[0]*inv_length;
        break;
      }
      case -1:
      case -0:
      case 1:
      {
        ud[i+2] = (x - knots[i])*ud[i+1]*inv_length2;
        ud[i+1] = ((x - knots[i-1])*ud[i]+(knots[i+2] -x)*ud[i+1])*inv_length2;
        ud[i] = (knots[i+1] - x)*ud[i]*inv_length2;
        break;
      }
      case 2:
        ud[i+2] = (x - knots[i])*ud[i+1]*inv_length;
        ud[i+1] = (x - knots[i-1])*ud[i]*inv_length2 +
          (knots[i+1] - x)*ud[i+1]*inv_length;
        ud[i] = (knots[i+1] - x)*ud[i]*inv_length2;
        break;
      }

      switch(bcase3) {
      case -2:
      {

        ud[3] = (x - knots[0]) * ud[2]*inv_length3;
        ud[2] = (x - knots[0]) * ud[1]*inv_length2+(knots[3] - x)*ud[2]*inv_length3;
        ud[1] = (x - knots[0]) * ud[0]*inv_length + (knots[2] - x)*ud[1]*inv_length2;
        ud[0] = (knots[1] - x)*ud[0]*inv_length;
        break;
      }
      case -1:

        ud[4] = (x - knots[1])*ud[3]*inv_length3;
        ud[3] = ((x - knots[0])*ud[2]+(knots[4] - x)*ud[3])*inv_length3;
        ud[2] = (x - knots[0])*ud[1]*inv_length2 + (knots[3] - x)*ud[2]*inv_length3;
        ud[1] = (knots[2] - x)*ud[1]*inv_length2;
        break;
      case 0:

        ud[i+3] = (x - knots[i])*ud[i+2]*inv_length3;
        ud[i+2] = ((x - knots[i-1])*ud[i+1]+(knots[i+3] - x)*ud[i+2])*inv_length3;
        ud[i+1] = ((x - knots[i-2])*ud[i]+(knots[i+2] - x)*ud[i+1])*inv_length3;
        ud[i] = (knots[i+1] - x)*ud[i]*inv_length3;
        break;
      case 1:

        ud[i+3] = (x - knots[i])*ud[i+2]*inv_length2;
        ud[i+2] = (x - knots[i-1])*ud[i+1]*inv_length3 + (knots[i+2] - x)*ud[i+2]*inv_length2;
        ud[i+1] = ((x - knots[i-2])*ud[i]+(knots[i+2] - x)*ud[i+1])*inv_length3;
        ud[i] = (knots[i+1] - x)*ud[i]*inv_length3;
        break;
      case 2:

        ud[i+3] = (x - knots[i])*ud[i+2]*inv_length;
        ud[i+2] =  (x - knots[i-1])*ud[i+1]*inv_length2 + (knots[i+1] - x)*ud[i+2]*inv_length;
        ud[i+1] = (x - knots[i-2])*ud[i]*inv_length3+(knots[i+1] - x)*ud[i+1]*inv_length2;
        ud[i] = (knots[i+1] - x)*ud[i]*inv_length3;
        break;
      }
    }
    return ud;
  };

  double eval_fct(double x, const arma::vec& coefs) {
    
    if (n_basis != coefs.n_elem) stop("Coefficient vector must have same length as number of bases");
    
    vec ret = zeros<vec>(4);
    int i = getIndexOf(x)-1;
    
    
    if (i < 0) {
      Rf_warning("Outside of range");
      return 0;
    }
    else {
      // -1 hvis første interval, +1 hvis sidste interval, 0 ellers.
      int bcase2 = -(i == 0) + (i == n_intervals-1);
      
      // -2,-1,0,1,2.
      int bcase3 = -(i < 2) - (i == 0) + (i == n_intervals-1) + (i > n_intervals - 3);
      
      ret[1] = (x-knots[i])*inv_length;
      ret[0] = (knots[i+1] - x)*inv_length;

      switch(bcase2) {
      case -1: 
        ret(2) = (x - knots[0])*ret(1)*inv_length2;
        ret(1) = (x - knots[0])*ret(0)*inv_length + (knots[2] - x)*ret(1)*inv_length2;
        ret(0) = (knots[1] - x)*ret(0)*inv_length;
        break;
      case 0:
        ret(2) = (x - knots[i])*ret(1)*inv_length2;
        ret(1) = ((x - knots[i-1])*ret(0)+(knots[i+2] - x)*ret(1))*inv_length2;
        ret(0) = (knots[i+1] - x)*ret(0)*inv_length2;
        break;
      case 1: 
        ret(2) = (x - knots[i])*ret(1)*inv_length;
        ret(1) = (x - knots[i-1])*ret(0)*inv_length2 + 
          (knots[i+1] - x)*ret(1)*inv_length;
        ret(0) = (knots[i+1] - x)*ret(0)*inv_length2;
        break;
      } 
      switch(bcase3) {
      case -2: 
        ret(3) = (x - knots[0])*ret(2)*inv_length3;
        ret(2) = (x - knots[0])*ret(1)*inv_length2+(knots[3] - x)*ret(2)*inv_length3;
        ret(1) = (x - knots[0])*ret(0)*inv_length + (knots[2] - x)*ret(1)*inv_length2;
        ret(0) = (knots[1] - x)*ret(0)*inv_length;
        break;
      case -1: 
        ret(3) = (x - knots[1])*ret(2)*inv_length3;
        ret(2) = ((x - knots[0])*ret(1)+(knots[4] - x)*ret(2))*inv_length3;
        ret(1) = (x - knots[0])*ret(0)*inv_length2 + (knots[3] - x)*ret(1)*inv_length3;
        ret(0) = (knots[2] - x)*ret(0)*inv_length2;
        break;
      case 0:
        ret(3) = (x - knots[i])*ret(2)*inv_length3;
        ret(2) = ((x - knots[i-1])*ret(1)+(knots[i+3] - x)*ret(2))*inv_length3;
        ret(1) = ((x - knots[i-2])*ret(0)+(knots[i+2] - x)*ret(1))*inv_length3;
        ret(0) = (knots[i+1] - x)*ret(0)*inv_length3;
        break;
      case 1: 
        ret(3) = (x - knots[i])*ret(2)*inv_length2;
        ret(2) = (x - knots[i-1])*ret(1)*inv_length3 +
          (knots[i+2] - x)*ret(2)*inv_length2;
        ret(1) = ((x - knots[i-2])*ret(0)+(knots[i+2] - x)*ret(1))*inv_length3;
        ret(0) = (knots[i+1] - x)*ret(0)*inv_length3;
        break;
      case 2: 
        ret(3) = (x - knots[i])*ret(2)*inv_length;
        ret(2) =  (x - knots[i-1])*ret(1)*inv_length2 +
          (knots[i+1] - x)*ret(2)*inv_length;
        ret(1) = (x - knots[i-2])*ret(0)*inv_length3+(knots[i+1] - x)*ret(1)*inv_length2;
        ret(0) = (knots[i+1] - x)*ret(0)*inv_length3;
        break;
      }
    // compileren er næsvis!:
    double retur = ret[0]*coefs(i);
      retur += ret[1]*coefs(++i);
      retur += ret[2]*coefs(++i);
      retur += ret[3]*coefs(++i);
    return retur;
    }
  };
  
  
  arma::vec eval_fct(const arma::vec& x, const arma::vec& coefs) {
    
    if (n_basis != coefs.n_elem) stop("Coeffienct vector must have same length as number of bases");
    vec ret = zeros<vec>(4);
    vec ud(x.n_elem, fill::none);

    
    for (unsigned int zz = 0; zz < x.n_elem; zz ++) {
        
      const double xx = x[zz];
      
      int i = getIndexOf(xx)-1;
      
      
      if (i < 0) {
        Rf_warning("Outside of range");
        ud[zz] = 0;
      }
      else {
        // -1 hvis første interval, +1 hvis sidste interval, 0 ellers.
        int bcase2 = -(i == 0) + (i == n_intervals-1);
        
        // -2,-1,0,1,2.
        int bcase3 = -(i < 2) - (i == 0) + (i == n_intervals-1) + (i > n_intervals - 3);
        
        ret[1] = (xx-knots[i]);
        ret[0] = (knots[i+1] - xx);
        
        switch(bcase2) {
        case -1: 
          ret[2] = (xx - knots[0])*ret[1]*inv_length2;
          ret[1] = (xx - knots[0])*ret[0]*inv_length + (knots[2] - xx)*ret[1]*inv_length2;
          ret[0] = (knots[1] - xx)*ret[0]*inv_length;
          break;
        case 0:
          ret[2] = (xx - knots[i])*ret[1]*inv_length2;
          ret[1] = ((xx - knots[i-1])*ret[0]+(knots[i+2] - xx)*ret[1])*inv_length2;
          ret[0] = (knots[i+1] - xx)*ret[0]*inv_length2;
          break;
        case 1: 
          ret[2] = (xx - knots[i])*ret[1]*inv_length;
          ret[1] = (xx - knots[i-1])*ret[0]*inv_length2 +
            (knots[i+1] - xx)*ret[1]*inv_length;
          ret[0] = (knots[i+1] - xx)*ret[0]*inv_length2;
          break;
        }
        switch(bcase3) {
        case -2: 
          ret[3] = (xx - knots[0])*ret[2]*inv_length3;
          ret[2] = (xx - knots[0])*ret[1]*inv_length2+(knots[3] - xx)*ret[2]*inv_length3;
          ret[1] = (xx - knots[0])*ret[0]*inv_length + (knots[2] - xx)*ret[1]*inv_length2;
          ret[0] = (knots[1] - xx)*ret[0]*inv_length;
          break;
        case -1: 
          ret[3] = (xx - knots[1])*ret[2]*inv_length3;
          ret[2] = ((xx - knots[0])*ret[1]+(knots[4] - xx)*ret[2])*inv_length3;
          ret[1] = (xx - knots[0])*ret[0]*inv_length2 + (knots[3] - xx)*ret[1]*inv_length3;
          ret[0] = (knots[2] - xx)*ret[0]*inv_length2;
          break;
        case 0:
          ret[3] = (xx - knots[i])*ret[2]*inv_length3;
          ret[2] = ((xx - knots[i-1])*ret[1]+(knots[i+3] - xx)*ret[2])*inv_length3;
          ret[1] = ((xx - knots[i-2])*ret[0]+(knots[i+2] - xx)*ret[1])*inv_length3;
          ret[0] = (knots[i+1] - xx)*ret[0]*inv_length3;
          break;
        case 1: 
          ret[3] = (xx - knots[i])*ret[2]*inv_length2;
          ret[2] = (xx - knots[i-1])*ret[1]*inv_length3 +
            (knots[i+2] - xx)*ret[2]*inv_length2;
          ret[1] = ((xx - knots[i-2])*ret[0]+(knots[i+2] - xx)*ret[1])*inv_length3;
          ret[0] = (knots[i+1] - xx)*ret[0]*inv_length3;
          break;
        case 2: 
          ret[3] = (xx - knots[i])*ret[2]*inv_length;
          ret[2] =  (xx - knots[i-1])*ret[1]*inv_length2 +
            (knots[i+1] - xx)*ret[2]*inv_length;
          ret[1] = (xx - knots[i-2])*ret[0]*inv_length3+(knots[i+1] - xx)*ret[1]*inv_length2;
          ret[0] = (knots[i+1] - xx)*ret[0]*inv_length3;
          break;
        }
      double retur = ret[0]*coefs(i);
        retur +=  ret[1]*coefs(++i);
        retur += ret[2]*coefs(++i);
        retur += ret[3]*coefs(++i);
      ud[zz] = retur * inv_length;
    }
  }
    return ud;
  };
  
  
  // Evaluerer d/dx B(x)
  arma::vec eval_deriv_coefs(double x)  {
    
    vec ret = zeros<vec>(n_basis);
    
    
    const int i = getIndexOf(x)-1;
    if (i < 0) {
      Rf_warning("Outside of range");
    }
    else {
      // -1 hvis første interval, +1 hvis sidste interval, 0 ellers.
      int bcase2 = -(i == 0) + (i == n_intervals-1);
      
      // -2,-1,0,1,2.
      int bcase3 = -(i < 2) - (i == 0) + (i == n_intervals-1) + (i > n_intervals - 3);
      
      ret[i+1] = (x-knots[i])*inv_length;
      ret[i] = (knots[i+1] - x)*inv_length;
      
      switch(bcase2) {
      case -1: 
        ret(2) = (x - knots[0])*ret(1)*inv_length2;
        ret(1) = (x - knots[0])*ret(0)*inv_length + (knots[2] - x)*ret(1)*inv_length2;
        ret(0) = (knots[1] - x)*ret(0)*inv_length;
        break;
      case 0:
        ret(i+2) = (x - knots[i])*ret(i+1)*inv_length2;
        ret(i+1) = ((x - knots[i-1])*ret(i)+(knots[i+2] - x)*ret(i+1))*inv_length2;
        ret(i) = (knots[i+1] - x)*ret(i)*inv_length2;
        break;
      case 1: 
        ret(i+2) = (x - knots[i])*ret(i+1)*inv_length;
        ret(i+1) = (x - knots[i-1])*ret(i)*inv_length2 + 
          (knots[i+1] - x)*ret(i+1)*inv_length;
        ret(i) = (knots[i+1] - x)*ret(i)*inv_length2;
        break;
      }
      
      
      switch(bcase3) {
      case -2: 
        ret(3) = 3*ret(2)*inv_length3; // Samme som inv_length?
        ret(2) = 3*(ret(1)*inv_length2 - ret(2)*inv_length3);
        ret(1) = 3*(ret(0)*inv_length - ret(1)*inv_length2);
        ret(0) = -3*ret(0)*inv_length;
        break;
      case -1: 
        ret(4) = 3*ret(3)*inv_length3;
        ret(3) = 3*(ret(2) - ret(3))*inv_length3;
        ret(2) = 3*(ret(1)*inv_length2 - ret(2)*inv_length3);
        ret(1) = -3*ret(1)*inv_length2;
        break;
      case 0:
        ret(i+3) = 3*ret(i+2)*inv_length3;
        ret(i+2) = 3*(ret(i+1) - ret(i+2))*inv_length3;
        ret(i+1) = 3*(ret(i) - ret(i+1))*inv_length3;
        ret(i) = -3*ret(i)*inv_length3;
        break;
      case 1: 
        ret(i+3) = 3*ret(i+2)*inv_length2;
        ret(i+2) = 3*(ret(i+1)*inv_length3 - ret(i+2)*inv_length2);
        ret(i+1) = 3*(ret(i) - ret(i+1))*inv_length3;
        ret(i) = -3*ret(i)*inv_length3;
        break;
      case 2: 
        ret(i+3) = 3*ret(i+2)*inv_length;
        ret(i+2) = 3*(ret(i+1)*inv_length2 - ret(i+2)*inv_length);
        ret(i+1) = 3*(ret(i)*inv_length3 - ret(i+1)*inv_length2);
        ret(i) = -3*ret(i)*inv_length3;
        break;
      }
      
    }
    
    
    return ret;
  }
   
   // Evaluerer d/dx B(x) 
  arma::mat eval_deriv_coefs(const arma::vec& x) {
    
    mat ud = zeros<mat>(n_basis, x.n_elem);
    for (unsigned int zz = 0; zz < x.n_elem; zz++) ud.col(zz) =  bspline_u4::eval_deriv_coefs(x(zz));
    return ud.t();
    
  };
  
  
  // Evaluaterer d/dx B(x) ganget på koefficienter
  double eval_deriv(double x, const arma::vec& coefs) {
    
    if (n_basis != coefs.n_elem) stop("Coeffienct vector must have same length as number of bases");
    
    vec ret = zeros<vec>(4);
    int i = getIndexOf(x)-1;
    
    
    if (i < 0) {
      Rf_warning("Outside of range");
      return 0;
    }
    else {
      // -1 hvis første interval, +1 hvis sidste interval, 0 ellers.
      int bcase2 = -(i == 0) + (i == n_intervals-1);
      
      // -2,-1,0,1,2.
      int bcase3 = -(i < 2) - (i == 0) + (i == n_intervals-1) + (i > n_intervals - 3);
      
      ret[1] = (x-knots[i])*inv_length;
      ret[0] = (knots[i+1] - x)*inv_length;
      
      switch(bcase2) {
      case -1: 
        ret(2) = (x - knots[0])*ret(1)*inv_length2;
        ret(1) = (x - knots[0])*ret(0)*inv_length + (knots[2] - x)*ret(1)*inv_length2;
        ret(0) = (knots[1] - x)*ret(0)*inv_length;
        break;
      case 0:
        ret(2) = (x - knots[i])*ret(1)*inv_length2;
        ret(1) = ((x - knots[i-1])*ret(0)+(knots[i+2] - x)*ret(1))*inv_length2;
        ret(0) = (knots[i+1] - x)*ret(0)*inv_length2;
        break;
      case 1: 
        ret(2) = (x - knots[i])*ret(1)*inv_length;
        ret(1) = (x - knots[i-1])*ret(0)*inv_length2 + 
          (knots[i+1] - x)*ret(1)*inv_length;
        ret(0) = (knots[i+1] - x)*ret(0)*inv_length2;
        break;
      }
      
      switch(bcase3) {
      case -2: 
        ret(3) = ret(2)*inv_length3; // Samme som inv_length?
        ret(2) = (ret(1)*inv_length2 - ret(2)*inv_length3);
        ret(1) = (ret(0)*inv_length - ret(1)*inv_length2);
        ret(0) = -ret(0)*inv_length;
        break;
      case -1: 
        ret(3) = ret(2)*inv_length3;
        ret(2) = (ret(1) - ret(2))*inv_length3;
        ret(1) = (ret(0)*inv_length2 - ret(1)*inv_length3);
        ret(0) = -ret(0)*inv_length2;
        break;
      case 0:
        ret(3) = ret(2)*inv_length3;
        ret(2) = (ret(1) - ret(2))*inv_length3;
        ret(1) = (ret(0) - ret(1))*inv_length3;
        ret(0) = -ret(0)*inv_length3;
        break;
      case 1: 
        ret(3) = ret(2)*inv_length2;
        ret(2) = (ret(1)*inv_length3 - ret(2)*inv_length2);
        ret(1) = (ret(0) - ret(1))*inv_length3;
        ret(0) = -ret(0)*inv_length3;
        break;
      case 2: 
        ret(3) = ret(2)*inv_length;
        ret(2) = (ret(1)*inv_length2 - ret(2)*inv_length);
        ret(1) = (ret(0)*inv_length3 - ret(1)*inv_length2);
        ret(0) = -ret(0)*inv_length3;
        break;
      }
    double retur = ret[0]*coefs(i);
      retur += ret[1]*coefs(++i);
      retur += ret[2]*coefs(++i);
      retur += ret[3]*coefs(++i);
    return 3 * retur;
    }
    };
  
  arma::vec eval_d2_coefs(double x)  {

    vec ret = zeros<vec>(n_basis);
    const int i = getIndexOf(x)-1;
    if (i < 0) {
      Rf_warning("Outside of range");
    }
    else {
      // -1 hvis første interval, +1 hvis sidste interval, 0 ellers.
      int bcase2 = -(i == 0) + (i == n_intervals-1);

      // -2,-1,0,1,2.
      int bcase3 = -(i < 2) - (i == 0) + (i == n_intervals-1) + (i > n_intervals - 3);

      ret[i+1] = (x-knots[i])*inv_length;
      ret[i] = (knots[i+1] - x)*inv_length;

      switch(bcase2) {
      case -1:
        ret(2) = 2*ret(1)*inv_length2;
        ret(1) = 2*(ret(0)*inv_length - ret(1)*inv_length2);
        ret(0) = 2*(-ret(0)*inv_length);
        break;
      case 0:
        ret(i+2) = 2*ret(i+1)*inv_length2;
        ret(i+1) = 2*(ret(i) - ret(i+1))*inv_length2;
        ret(i) = 2*(-ret(i)*inv_length2);
        break;
      case 1:
        ret(i+2) = 2*ret(i+1)*inv_length;
        ret(i+1) = 2*(ret(i)*inv_length2 - ret(i+1)*inv_length);
        ret(i) = 2*(-ret(i)*inv_length2);
        break;
      }

      switch(bcase3) {
      case -2:
        ret(3) = 3*ret(2)*inv_length3; // Samme som inv_length?
        ret(2) = 3*(ret(1)*inv_length2 - ret(2)*inv_length3);
        ret(1) = 3*(ret(0)*inv_length - ret(1)*inv_length2);
        ret(0) = -3*ret(0)*inv_length;
        break;
      case -1:
        ret(4) = 3*ret(3)*inv_length3;
        ret(3) = 3*(ret(2) - ret(3))*inv_length3;
        ret(2) = 3*(ret(1)*inv_length2 - ret(2)*inv_length3);
        ret(1) = -3*ret(1)*inv_length2;
        break;
      case 0:
        ret(i+3) = 3*ret(i+2)*inv_length3;
        ret(i+2) = 3*(ret(i+1) - ret(i+2))*inv_length3;
        ret(i+1) = 3*(ret(i) - ret(i+1))*inv_length3;
        ret(i) = -3*ret(i)*inv_length3;
        break;
      case 1:
        ret(i+3) = 3*ret(i+2)*inv_length2;
        ret(i+2) = 3*(ret(i+1)*inv_length3 - ret(i+2)*inv_length2);
        ret(i+1) = 3*(ret(i) - ret(i+1))*inv_length3;
        ret(i) = -3*ret(i)*inv_length3;
        break;
      case 2:
        ret(i+3) = 3*ret(i+2)*inv_length;
        ret(i+2) = 3*(ret(i+1)*inv_length2 - ret(i+2)*inv_length);
        ret(i+1) = 3*(ret(i)*inv_length3 - ret(i+1)*inv_length2);
        ret(i) = -3*ret(i)*inv_length3;
        break;
      }
    }
    return ret;
  }

  double eval_d2(double x, const arma::vec& coefs) {

    if (n_basis != coefs.n_elem) stop("Coeffienct vector must have same length as number of bases");

    vec ret = zeros<vec>(4);
    int i = getIndexOf(x)-1;

    if (i < 0) {
      Rf_warning("Outside of range");
      return 0;
    }
    else {
      // -1 hvis første interval, +1 hvis sidste interval, 0 ellers.
      int bcase2 = -(i == 0) + (i == n_intervals-1);

      // -2,-1,0,1,2.
      int bcase3 = -(i < 2) - (i == 0) + (i == n_intervals-1) + (i > n_intervals - 3);

      ret[1] = (x-knots[i])*inv_length;
      ret[0] = (knots[i+1] - x)*inv_length;

      switch(bcase2) {
      case -1:
        ret(2) = ret(1)*inv_length2;
        ret(1) = ret(0)*inv_length - ret(1)*inv_length2;
        ret(0) = -ret(0)*inv_length;
        break;
      case 0:
        ret(2) = ret(1)*inv_length2;
        ret(1) = (ret(0) - ret(1))*inv_length2;
        ret(0) = -ret(0)*inv_length2;
        break;
      case 1:
        ret(2) = ret(1)*inv_length;
        ret(1) = ret(0)*inv_length2 - ret(1)*inv_length;
        ret(0) = -ret(0)*inv_length2;
        break;
      }

      switch(bcase3) {
      case -2:
        ret(3) = ret(2)*inv_length3; // Samme som inv_length?
        ret(2) = (ret(1)*inv_length2 - ret(2)*inv_length3);
        ret(1) = (ret(0)*inv_length - ret(1)*inv_length2);
        ret(0) = -ret(0)*inv_length;
        break;
      case -1:
        ret(3) = ret(2)*inv_length3;
        ret(2) = (ret(1) - ret(2))*inv_length3;
        ret(1) = (ret(0)*inv_length2 - ret(1)*inv_length3);
        ret(0) = -ret(0)*inv_length2;
        break;
      case 0:
        ret(3) = ret(2)*inv_length3;
        ret(2) = (ret(1) - ret(2))*inv_length3;
        ret(1) = (ret(0) - ret(1))*inv_length3;
        ret(0) = -ret(0)*inv_length3;
        break;
      case 1:
        ret(3) = ret(2)*inv_length2;
        ret(2) = (ret(1)*inv_length3 - ret(2)*inv_length2);
        ret(1) = (ret(0) - ret(1))*inv_length3;
        ret(0) = -ret(0)*inv_length3;
        break;
      case 2:
        ret(3) = ret(2)*inv_length;
        ret(2) = (ret(1)*inv_length2 - ret(2)*inv_length);
        ret(1) = (ret(0)*inv_length3 - ret(1)*inv_length2);
        ret(0) = -ret(0)*inv_length3;
        break;
      }
    double retur = ret[0]*coefs(i);
      retur += ret[1]*coefs(++i);
      retur += ret[2]*coefs(++i);
      retur += ret[3]*coefs(++i);
    return 6 * retur;
        }
    };
};

#endif
