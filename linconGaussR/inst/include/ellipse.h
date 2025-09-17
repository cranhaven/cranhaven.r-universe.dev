#ifndef LINCONGAUSSR_ELLIPSE_H
#define LINCONGAUSSR_ELLIPSE_H

#include <math.h>
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;
using namespace std;
namespace linconGaussR{
class Ellipse{
   /*  Define an ellipse in a D-dimensional space around the origin
        from two vectors as x = a1 * cos(t) + a2 * sin(t)

        :param a1: first vector defining the ellipse, shape (D, 1)
        :param a2: second vector defining the ellipse, shape (D, 1) */
    public:
        arma::vec a1, a2;
        Ellipse() = default;
        Ellipse(arma::vec aa1, arma::vec aa2){
            a1 = aa1;
            a2 = aa2;
        }
        inline arma::vec x(double theta);
};

inline arma::vec Ellipse::x(double theta){
    /* location on ellipse corresponding at angle theta
        :param theta: angle
        :return: location x on ellipse */
    return (a1 * cos(theta) + a2 * sin(theta)) ;
}
}
#endif
