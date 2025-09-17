#ifndef LINCONGAUSSR_LINCONGAUSSCPP_H
#define LINCONGAUSSR_LINCONGAUSSCPP_H

#include<RcppArmadillo.h>
#include<math.h>

#include "active_intersections.h"
#include "angle_sampler.h"
#include "ellipse.h"
#include "elliptical_slice_sampling.h"
#include "linear_constraints.h"
#include "loop.h"

namespace linconGaussR{

inline arma::mat linconGauss_cpp(int n, 
                            arma::mat A, 
                            arma::vec b, 
                            arma::mat Sigma, 
                            arma::vec mu, 
                            arma::vec x_init,
                            bool intersection=true,
                            int nskp=5){
    arma::mat C = chol(Sigma);
    b += A * mu;
    A = A * C.t();
    LinearConstraints lincon(A,b,intersection);
    x_init = arma::solve(C.t(), x_init-mu);
    
    EllipticalSliceSampler sampler(n,lincon,nskp,x_init);
    sampler.run();
    arma::mat res = sampler.loop_state.samples;
    res = res * C;
    res.each_row() += mu.t();

    return res;
}


}


#endif
