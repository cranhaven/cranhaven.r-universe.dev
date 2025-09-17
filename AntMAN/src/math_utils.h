/*
 *  AntMAN Package
 *
 */


#ifndef ANTMAN_MIXTURE_CPP_MATHS_HPP_
#define ANTMAN_MIXTURE_CPP_MATHS_HPP_

#include "verbose.h"

#ifdef NO_RCPP

#include "beta_distribution.h"
#include <armadillo>

#ifndef M_LN_2PI
	#define M_LN_2PI	1.837877066409345483560659472811	/* log(2*pi) */
#endif

#ifndef M_2PI
	#define M_2PI		6.283185307179586476925286766559	/* 2*pi */
#endif

#ifndef M_LN_SQRT_2PI
	#define M_LN_SQRT_2PI	0.918938533204672741780329736406	/* log(sqrt(2*pi)) == log(2*pi)/2 */
#endif

static inline double am_rpois   (double n)                        {
	 static std::default_random_engine generator;
	 std::poisson_distribution<int> distribution(n);
	return (distribution(generator));
}
static inline double am_runif   (double a ,double b)                   {
	 static std::default_random_engine generator;
	 std::uniform_real_distribution<double> distribution(a,b);
	 return (distribution(generator));
}

static inline double am_rnbinom (double a ,double b)                   {
	 static std::default_random_engine generator;
	 std::negative_binomial_distribution<int> distribution(a,b);
	 return (distribution(generator));
}

static inline double am_rbeta (double a ,double b)                   {
	 static std::default_random_engine generator;
	 beta_distribution<double> distribution(a,b);
	 return (distribution(generator));
}
static inline double am_rgamma (double a ,double b)                   {
	 static std::default_random_engine generator;
	 std::gamma_distribution<double> distribution(a,b);
	 return (distribution(generator));
}
static inline double am_rnorm (double a ,double b)                   {
	 static std::default_random_engine generator;
	 std::normal_distribution<double> distribution(a,b);
	 return (distribution(generator));
}

static inline double am_rchisq (double a)                   {
	 static std::default_random_engine generator;
	 std::chi_squared_distribution<double> distribution(a);
	 return (distribution(generator));
}

static inline arma::vec am_randu   (arma::uword n)               {return arma::randu(n);}
static inline arma::vec am_randn   (arma::uword n)               {return arma::randn(n);}

#else
#ifdef HAS_RCPP
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

static inline arma::vec am_randu   (arma::uword n)               {return arma::randu(n);}
static inline arma::vec am_randn   (arma::uword n)               {return arma::randn(n);}
static inline double    am_rpois   (double n)                    {return R::rpois(n);  }
static inline double    am_runif   (double a ,double b)          {return R::runif(a,b);  }
static inline double    am_rnbinom (double a ,double b)          {return R::rnbinom(a,b);}
static inline double    am_rgamma (double a ,double b)           {return R::rgamma(a,b); }
static inline double    am_rbeta  (double a ,double b)           {return R::rbeta(a,b); }
static inline double    am_rnorm  (double a ,double b)           {return R::rnorm(a,b); }
static inline double    am_rchisq (double a)                     {return R::rchisq(a); }
#else
#error "Unsupported Compilation flags, Need NO_RCPP or HAS_RCPP"
#endif
#endif


#endif /* ANTMAN_MIXTURE_CPP_MATHS_HPP_ */
