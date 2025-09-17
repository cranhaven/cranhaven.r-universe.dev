// [[Rcpp::depends(RcppArmadillo)]]

#ifndef LINCONGAUSSR_LINEAR_CONSTRAINTS_H
#define LINCONGAUSSR_LINEAR_CONSTRAINTS_H

#include <math.h>
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;
using namespace std;

namespace linconGaussR{

class LinearConstraints
{
public:
    arma::mat A;       // linear constrain's coefficients
    arma::vec b;       // offset of the linear constrains
    int M;             // number of constraints
    int D;             // dimension of normal
    bool intersection; // whether to use intersection of the constrain
    LinearConstraints() = default;
    LinearConstraints(const arma::mat &AA, const arma::vec bb, bool intersection_)
    {
        /* """
        Defines linear functions f(x) = Ax + b.
        The integration domain is defined as the union of where all of these functions are positive if mode='Union'
        or the domain where any of the functions is positive, when mode='Intersection'
        :param A: matrix A with shape (M, D) where M is the number of constraints and D the dimension
        :param b: offset, shape (M, 1)
        """ */
        A = AA;
        b = bb;
        M = AA.n_rows;
        D = AA.n_cols;
        intersection = intersection_;
    }
    arma::mat evaluate(const arma::mat &x)
    {

        /* Evaluate linear functions at N locations x
        :param x: location, shape (D, N)
        :return: Ax + b */
        arma::mat temp = A * x;
        temp.each_col() += b;
        return temp;
    }
    inline arma::Mat<int> indicator_intersection(const arma::mat &x);
    inline arma::Mat<int> indicator_union(const arma::mat &x);
    inline arma::Mat<int> integration_domain(const arma::mat &x);
};

inline arma::Mat<int>
LinearConstraints::indicator_intersection(const arma::mat &x)
{

    /* Intersection of indicator functions taken to be 1 when the linear function is >= 0
        :param x: location, shape (D, N)
        :return: 1 if all linear functions are >= 0, else 0. */

    int N = x.n_cols;
    int temp;
    arma::mat eval = this->evaluate(x);
    Mat<int> res(1, N, fill::ones);
    for (int i = 0; i < N; i++)
    {
        temp = 1;
        for (int j = 0; j < M; j++)
        {
            temp *= (int)((eval(j, i) >= 0));
        }
        res(i) = temp;
    }
    return res;
}

inline arma::Mat<int> LinearConstraints::indicator_union(const arma::mat &x)
{

    /* Union of indicator functions taken to be 1 when the linear function is >= 0
        :param x: location, shape (D, N)
        :return: 1 if any of the linear functions is >= 0, else 0. */

    int N = x.n_cols;
    int temp;
    arma::mat eval = this->evaluate(x);
    Mat<int> res(1, N, fill::ones);
    for (int i = 0; i < N; i++)
    {
        temp = 1;
        for (int j = 0; j < M; j++)
        {
            temp *= (int)((eval(j, i) < 0));
        }
        res(i) = 1 - temp;
    }
    return res;
}

inline arma::Mat<int> LinearConstraints::integration_domain(const arma::mat &x)
{
    /* is 1 if x is in the integration domain, else 0
        :param x: location, shape (D, N)
        :return: either self.indicator_union or self.indicator_intersection, depending on setting of self.mode
         */
    if (intersection)
    {
        return (this->indicator_intersection(x));
    }
    else
    {
        return (this->indicator_union(x));
    }
}

}
#endif
