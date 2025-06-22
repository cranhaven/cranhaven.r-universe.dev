/*
 *  BART: Bayesian Additive Regression Trees
 *  Copyright (C) 2017-2018 Robert McCulloch, Rodney Sparapani
 *                          and Charles Spanbauer
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/GPL-2
 */


#ifndef GUARD_rn_h
#define GUARD_rn_h

#include <cmath>
// double log_sum_exp(std::vector<double>& v){
//   double mx=v[0],sm=0.;
//   for(size_t i=0;i<v.size();i++) if(v[i]>mx) mx=v[i];
//   for(size_t i=0;i<v.size();i++){
//     sm += exp(v[i]-mx);
//   }
//   return mx+log(sm);
// }

//pure virtual base class for random numbers
class rn
{
 public:
  rn() {}
  virtual double normal() = 0; //standard normal
  virtual double uniform() = 0; //uniform(0,1)
  virtual double chi_square(double df) = 0; //chi-square
  virtual double exp() = 0; //exponential
  virtual double log_gamma(double shape) = 0; 
  virtual double gamma(double shape, double rate) = 0; 
  virtual double beta(double a, double b) = 0; 
  virtual size_t discrete() = 0; //discrete (categorical) distribution
  virtual size_t geometric(double p) = 0; //geometric distribution
  virtual void set_wts(std::vector<double>& _wts) = 0;
  virtual std::vector<double> log_dirichlet(std::vector<double>& alpha) = 0; 
  virtual ~rn() {}
};

//abstract random number generator based on R/Rcpp
class arn: public rn
{
 public:
  //constructor
 //arn():df(1) {}
 arn() {}
  //virtual
  virtual ~arn() {}
  virtual double normal() {return R::norm_rand();}
  virtual double uniform() { return R::unif_rand();}
  virtual double chi_square(double df) {return R::rchisq(df);}
  virtual double exp() {return R::exp_rand();}
  virtual double log_gamma(double shape) {
    double y=log(R::rgamma(shape+1., 1.)), z=log(this->uniform())/shape;
    return y+z; 
  }
  virtual double gamma(double shape, double rate) {
    if(shape<0.01) return ::exp(this->log_gamma(shape))/rate;
    else return R::rgamma(shape, 1.)/rate; 
  } 
/*
  virtual double gamma_left(double shape, double rate, double a) {
// left truncated gamma: shape must be greater than 1!
// Random Number Generation and Monte Carlo Methods, Second Edition, pp. 180-1
    if(shape<=1.) return -1.;
    double y, x=1., c=2., a_scale=a*rate, shape_shift=shape-1.,
    lambda=0.5*(a_scale-shape+sqrt(pow(a_scale-shape, 2.)+4.*a_scale))/a_scale,
      lambda_shift=1.-lambda, 
      C=1.+log(lambda_shift/shape_shift);
    while(c>x) { // do at least once
      x=R::exp_rand(); 
      y=a_scale+R::exp_rand()/lambda; 
      c=lambda_shift*y-shape_shift*(log(y)+C);
    }
    return y/rate;
  } 
*/
  virtual double beta(double a, double b) {
    double x1=this->gamma(a, 1.), x2=this->gamma(b, 1.);
    return x1/(x1+x2);
  } 
 virtual size_t discrete() {
    size_t p=wts.size(), x=0;
    std::vector<int> vOut (p,0);
    R::rmultinom(1,&wts[0],p,&vOut[0]); 
    if(vOut[0]==0) for(size_t j=1;j<p;j++) x += j*vOut[j]; 
    return x;
  }
/*
    size_t p=wts.size();
    std::vector<int> vOut (p,0); // vector of multionomial output
    std::vector<int> seq (p,0); 
    for(size_t j=0;j<p;j++) seq[j]=j;
    R::rmultinom(1,&wts[0],p,&vOut[0]); // vector vOut contains selected component
    return std::inner_product(vOut.begin(),vOut.end(),seq.begin(),0);
  }
*/
  virtual size_t geometric(double p) {return R::rgeom(p);}
  virtual void set_wts(std::vector<double>& _wts) {
    double smw=0.;
    wts.clear();
    for(size_t j=0;j<_wts.size();j++) smw+=_wts[j];
    for(size_t j=0;j<_wts.size();j++) wts.push_back(_wts[j]/smw);
  }
  virtual std::vector<double> log_dirichlet(std::vector<double>& alpha){
    size_t k=alpha.size();
    std::vector<double> draw(k);
    double lse;
    for(size_t j=0;j<k;j++) draw[j]=this->log_gamma(alpha[j]);
    double mx=draw[0],sm=0.;
    for(size_t i=0;i<draw.size();i++) if(draw[i]>mx) mx=draw[i];
    for(size_t i=0;i<draw.size();i++){
      sm += std::exp(draw[i]-mx);
    }
    lse= mx+log(sm);
    for(size_t j=0;j<k;j++) {
      draw[j] -= lse;
      //draw[j]=::exp(draw[j]);
    }
    return draw;
  }
 private:
  std::vector<double> wts;
  Rcpp::RNGScope RNGstate;
};

#endif 
