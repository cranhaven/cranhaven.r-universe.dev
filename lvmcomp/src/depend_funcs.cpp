// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "lvmcomp_omp.h"

double F_theta_y_eta_cpp(double x, arma::vec theta_minus_k, int k, arma::vec y_i,
                         arma::mat inv_sigma, arma::mat A, arma::vec d){
  arma::vec theta = theta_minus_k;
  arma::vec tmp = d;
  theta(k-1) =  x;
  tmp = A * theta + d;
  return -0.5 * arma::accu(theta.t() * inv_sigma * theta) + arma::accu(y_i % tmp - log(1+exp(tmp)));
}
double F_prime_theta_y_eta_cpp(double x, arma::vec theta_minus_k, int k, arma::vec y_i,
                               arma::mat inv_sigma, arma::mat A, arma::vec d){
  arma::vec theta = theta_minus_k;
  arma::vec tmp = d;
  theta(k-1) = x;
  tmp = A * theta + d;
  return -arma::accu(inv_sigma.row(k-1) * theta) + arma::accu( (y_i - 1/(1+exp(-tmp))) % A.col(k-1));
}

arma::vec F_theta_y_eta_cpp1(arma::vec x, arma::vec theta_minus_k, int k, arma::vec y_i,
                             arma::mat inv_sigma, arma::mat A, arma::vec d){
  int nn = x.n_rows;
  int i = 0;
  arma::vec theta = theta_minus_k;
  arma::vec res = arma::zeros(nn);
  arma::vec tmp = d;
  for(i=0;i<nn;++i){
    theta(k-1) = x(i);
    tmp = A * theta + d;
    res(i) = -0.5 * arma::accu(theta.t() * inv_sigma * theta) + arma::accu(y_i % tmp - log(1+exp(tmp)));
  }
  return res;
}

arma::vec F_prime_theta_y_eta_cpp1(arma::vec x, arma::vec theta_minus_k, int k, arma::vec y_i,
                                   arma::mat inv_sigma, arma::mat A, arma::vec d){
  int nn = x.n_rows;
  int i = 0;
  arma::vec theta = theta_minus_k;
  arma::vec res = arma::zeros(nn);
  arma::vec tmp = d;
  for(i=0;i<nn;++i){
    theta(k-1) = x(i);
    tmp = A * theta + d;
    res(i) = -arma::accu(inv_sigma.row(k-1) * theta) + arma::accu( (y_i - 1/(1+exp(-tmp))) % A.col(k-1));
  }
  return res;
}
// [[Rcpp::export]]
double deriv_based_ARS(arma::vec x, arma::vec theta_minus_k, int k, arma::vec y_i,
                       arma::mat inv_sigma, arma::mat A, arma::vec d) {
  int MM = 3;
  arma::vec radom_uniform;
  arma::vec s_a = F_prime_theta_y_eta_cpp1(x, theta_minus_k, k, y_i, inv_sigma, A, d);
  int initial_flag = 0;
  while(initial_flag < 40){
    // arma::uword ind = arma::abs(s_a).index_min();
    if(s_a(0)<=0){
      x(2) = x(1);
      x(1) = x(0);
      x(0) = x(0) - 1;
      s_a(2) = s_a(1);
      s_a(1) = s_a(0);
      s_a(0) = F_prime_theta_y_eta_cpp(x(0), theta_minus_k, k, y_i, inv_sigma, A, d);
      initial_flag += 1;
    }
    else if(s_a(2) >= 0){
      x(0) = x(1);
      x(1) = x(2);
      x(2) = x(2) + 1;
      s_a(0) = s_a(1);
      s_a(1) = s_a(2);
      s_a(2) = F_prime_theta_y_eta_cpp(x(2), theta_minus_k, k, y_i, inv_sigma, A, d);
      initial_flag += 1;
    }
    else{
      break;
    }
  }
  // check initial points; if fails, return
  if(s_a(0)<=0 || s_a(s_a.n_elem-1) >= 0){
    Rprintf("Initial points should on either side of f\n ");
    return 0;
  }
  arma::vec initial_x = x;
  arma::vec log_fn = F_theta_y_eta_cpp1(x, theta_minus_k, k, y_i, inv_sigma, A, d);
  double offset = arma::mean(log_fn);
  log_fn = log_fn - offset;
  arma::vec l_a = arma::diff(log_fn) / arma::diff(x);
  arma::vec z = arma::zeros(s_a.n_elem-1);
  for(int ii=0;ii<z.n_elem;++ii){
    z(ii) = (log_fn(ii) - log_fn(ii+1) + s_a(ii+1)*x(ii+1) - s_a(ii)*x(ii)) / (s_a(ii+1) - s_a(ii));
  }

  int flag = 0;
  double candidate;
  // z is the intercept points of M-1 lines formed by M points (x)
  int choose_region;
  int choose_region_lower;
  arma::vec areas;
  // for(int ii=0;ii<areas.n_elem;++ii){
  //   Rprintf(" %f | ", areas(ii));
  // }
  // Rprintf("\n");
  while(MM < 20){
    // calcu areas
    if(flag == 0){
      areas = arma::zeros(z.n_elem+1);
      areas(0) = std::exp(s_a(0)*(z(0)-x(0))+log_fn(0)) / s_a(0);
      areas(areas.n_elem-1) = -std::exp(s_a(areas.n_elem-1)*(z(z.n_elem-1)-x(areas.n_elem-1))+log_fn(areas.n_elem-1)) / s_a(areas.n_elem-1);
      for(int ii=1;ii<areas.n_elem-1;++ii){
        if(s_a(ii)!=0)
          areas(ii) = 1.0 / s_a(ii) * (exp(log_fn(ii) + s_a(ii)*(z(ii)-x(ii))) - exp(log_fn(ii) + s_a(ii)*(z(ii-1)-x(ii))));
        else
          areas(ii) = std::exp(log_fn(ii)) * (z(ii) - z(ii-1));
      }
    }
    radom_uniform = arma::randu(4);
    arma::vec cum_std_areas = arma::cumsum(areas / sum(areas));
    arma::uvec tmp = arma::find(cum_std_areas <= radom_uniform(0));
    choose_region = tmp.n_elem;
    if(choose_region==0){
      candidate = x(choose_region) +
        1.0/s_a(choose_region)*std::log(radom_uniform(1)*areas(choose_region)*s_a(choose_region)/std::exp(log_fn(choose_region)));
      if(!arma::is_finite(candidate))
        candidate = -20;
    }
    else{
      if(s_a(choose_region)!=0){
        candidate = x(choose_region) +
          1.0/s_a(choose_region)*std::log(radom_uniform(1)*areas(choose_region)*s_a(choose_region)/std::exp(log_fn(choose_region))+
          std::exp(s_a(choose_region)*(z(choose_region-1)-x(choose_region))));
        if(!arma::is_finite(candidate))
          candidate = 20;
      }
      else
        candidate = z(choose_region-1) + radom_uniform(1)*(areas(choose_region)/std::exp(log_fn(choose_region)));
    }
    // squeeze step
    arma::uvec tmp_lower;
    double higher_bound = s_a(choose_region)*(candidate-x(choose_region))+log_fn(choose_region);
    
    if(candidate >= x(0) && candidate <= x(x.n_elem-1)){
      tmp_lower = arma::find(x <= candidate);
      choose_region_lower = tmp_lower.n_elem;
      if(radom_uniform(2) <= std::exp(log_fn(choose_region_lower-1)+l_a(choose_region_lower-1)*(candidate-x(choose_region_lower-1)) -
         higher_bound) ){
        return candidate;
      }
    }
    else if(candidate < x(0)){
      choose_region_lower = 0;
    }
    else{
      choose_region_lower = x.n_elem;
    }
    // squeeze fail calcu new point
    double new_log_fn = F_theta_y_eta_cpp(candidate, theta_minus_k, k, y_i, inv_sigma, A, d) - offset;
    // rejection test
    if(radom_uniform(2) <= std::exp(new_log_fn - higher_bound) ){
      return candidate;
    }
    flag = 1;
    // update step
    if(MM < 20){
      x.insert_rows(choose_region_lower,1);
      x(choose_region_lower) = candidate;
      log_fn.insert_rows(choose_region_lower, 1);
      log_fn(choose_region_lower) = new_log_fn;
      s_a.insert_rows(choose_region_lower, 1);
      s_a(choose_region_lower) = F_prime_theta_y_eta_cpp(candidate, theta_minus_k, k, y_i, inv_sigma, A, d);
      l_a = arma::diff(log_fn) / arma::diff(x);
      z = arma::zeros(s_a.n_elem-1);
      for(int ii=0;ii<z.n_elem;++ii){
        z(ii) = (log_fn(ii) - log_fn(ii+1) + s_a(ii+1)*x(ii+1) - s_a(ii)*x(ii)) / (s_a(ii+1) - s_a(ii));
      }
      flag=0;
      MM++;
    }
    //Rprintf("M=%d\n",M);
  }
  Rprintf("error!\n");
  return candidate;
}
// [[Rcpp::export]]
arma::vec F_prime_theta_y_eta_cpp_partial_credit1(arma::vec x, arma::vec theta_minus_k, int k, arma::vec response_i,
                                                  arma::mat inv_sigma, arma::mat A, arma::mat D){
  int M = D.n_cols;
  int J = response_i.n_rows;
  arma::vec theta_i = theta_minus_k;
  arma::vec res(x.n_elem);
  for(int ii=0;ii<x.n_elem;++ii){
    theta_i(k-1) = x(ii);
    arma::vec thetaA_i = A * theta_i;
    arma::mat tmp = arma::exp(thetaA_i * arma::linspace(0, M-1, M).t() + D);
    arma::vec tmp_res(J);
    tmp_res = response_i - sum( tmp.each_row() % arma::linspace(0, M-1, M).t(), 1 ) / sum( tmp, 1 );
    res(ii) = arma::accu(A.col(k-1) % tmp_res)-arma::accu(inv_sigma.row(k-1) * theta_i);
  }
  
  return res;
}
// [[Rcpp::export]]
double F_prime_theta_y_eta_cpp_partial_credit(double x, arma::vec theta_minus_k, int k, arma::vec response_i,
                                              arma::mat inv_sigma, arma::mat A, arma::mat D){
  int M = D.n_cols;
  int J = response_i.n_elem;
  arma::vec theta_i = theta_minus_k;
  theta_i(k-1) = x;
  arma::vec thetaA_i = A * theta_i;
  arma::mat tmp = arma::exp(thetaA_i * arma::linspace(0, M-1, M).t() + D);
  arma::vec tmp_res(J);
  tmp_res = response_i - sum( tmp.each_row() % arma::linspace(0, M-1, M).t(), 1 ) / sum( tmp, 1 );
  return arma::accu(A.col(k-1) % tmp_res)-arma::accu(inv_sigma.row(k-1) * theta_i);
}
// [[Rcpp::export]]
arma::vec F_theta_y_eta_cpp_partial_credit1(arma::vec x, arma::vec theta_minus_k, int k, arma::vec response_i,
                                            arma::mat inv_sigma, arma::mat A, arma::mat D){
  int M = D.n_cols;
  int J = A.n_rows;
  arma::vec theta_i = theta_minus_k;
  arma::vec res(x.n_elem);
  arma::vec tmp_res(J);
  for(int ii=0;ii<x.n_elem;++ii){
    theta_i(k-1) = x(ii);
    arma::vec thetaA_i = A * theta_i;
    arma::mat tmp = thetaA_i * arma::linspace(0, M-1, M).t() + D;
    for(int jj=0;jj<J;++jj){
      tmp_res(jj) = tmp(jj, response_i(jj)) - std::log(sum(arma::exp(tmp.row(jj))));
    }
    res(ii) = arma::accu(tmp_res) - 0.5 * arma::accu(theta_i.t() * inv_sigma * theta_i);
  }
  
  return res;
}
// [[Rcpp::export]]
double F_theta_y_eta_cpp_partial_credit(double x, arma::vec theta_minus_k, int k, arma::vec response_i,
                                        arma::mat inv_sigma, arma::mat A, arma::mat D){
  int M = D.n_cols;
  int J = A.n_rows;
  arma::vec theta_i = theta_minus_k;
  theta_i(k-1) = x;
  arma::vec thetaA_i = A * theta_i;
  arma::mat tmp = thetaA_i * arma::linspace(0, M-1, M).t() + D;
  arma::vec tmp_res(J);
  for(int i=0;i<J;++i){
    tmp_res(i) = tmp(i, response_i(i)) - std::log(sum(arma::exp(tmp.row(i))));
  }
  return arma::accu(tmp_res) - 0.5 * arma::accu(theta_i.t() * inv_sigma * theta_i);
}
// [[Rcpp::export]]
double deriv_based_ARS_partial_credit(arma::vec x, arma::vec theta_minus_k, int k, arma::vec y_i,
                                      arma::mat inv_sigma, arma::mat A, arma::mat D) {
  int MM = 3;
  arma::vec radom_uniform;
  arma::vec s_a = F_prime_theta_y_eta_cpp_partial_credit1(x, theta_minus_k, k, y_i, inv_sigma, A, D);
  int initial_flag = 0;
  
  while(initial_flag < 40){
    // arma::uword ind = arma::abs(s_a).index_min();
    if(s_a(0)<=0){
      x(2) = x(1);
      x(1) = x(0);
      x(0) = x(0) - 1;
      s_a(2) = s_a(1);
      s_a(1) = s_a(0);
      s_a(0) = F_prime_theta_y_eta_cpp_partial_credit(x(0), theta_minus_k, k, y_i, inv_sigma, A, D);
      initial_flag += 1;
    }
    else if(s_a(2) >= 0){
      x(0) = x(1);
      x(1) = x(2);
      x(2) = x(2) + 1;
      s_a(0) = s_a(1);
      s_a(1) = s_a(2);
      s_a(2) = F_prime_theta_y_eta_cpp_partial_credit(x(2), theta_minus_k, k, y_i, inv_sigma, A, D);
      initial_flag += 1;
    }
    else{
      break;
    }
  }
  if(s_a(0)<=0 || s_a(s_a.n_elem-1) >= 0){
    Rprintf("Initial points should on either side of f\n ");
    return 0;
  }
  arma::vec initial_x = x;
  arma::vec log_fn = F_theta_y_eta_cpp_partial_credit1(x, theta_minus_k, k, y_i, inv_sigma, A, D);
  double offset = arma::mean(log_fn);
  log_fn = log_fn - offset;
  // Rprintf("log fn range: %f\n", max(log_fn) - min(log_fn));
  arma::vec l_a = arma::diff(log_fn) / arma::diff(x);
  arma::vec z = arma::zeros(s_a.n_elem-1);
  for(int ii=0;ii<z.n_elem;++ii){
    z(ii) = (log_fn(ii) - log_fn(ii+1) + s_a(ii+1)*x(ii+1) - s_a(ii)*x(ii)) / (s_a(ii+1) - s_a(ii));
  }
  // Rprintf("s_a = ");
  // for(int ii=0;ii<s_a.n_elem;++ii){
  //   Rprintf(" %f |", s_a(ii));
  // }
  // Rprintf("\n");
  // check initial points; if fails, return
  // Rprintf("hello1\n");
  int flag = 0;
  double candidate;
  // z is the intercept points of MM-1 lines formed by MM points (x)
  int choose_region;
  int choose_region_lower;
  arma::vec areas;
  // for(int ii=0;ii<areas.n_elem;++ii){
  //   Rprintf(" %f | ", areas(ii));
  // }
  // Rprintf("\n");
  // Rprintf("hello2\n");
  while(MM < 20){
    // calcu areas
    if(flag == 0){
      // if(max(log_fn) - min(log_fn) > 100){
      //   Rprintf("range too large, set theta as 0\n");
      //   return 0;
      // }
      areas = arma::zeros(z.n_elem+1);
      areas(0) = std::exp(s_a(0)*(z(0)-x(0))+log_fn(0)) / s_a(0);
      areas(areas.n_elem-1) = -std::exp(s_a(areas.n_elem-1)*(z(z.n_elem-1)-x(areas.n_elem-1))+log_fn(areas.n_elem-1)) / s_a(areas.n_elem-1);
      for(int ii=1;ii<areas.n_elem-1;++ii){
        if(s_a(ii)!=0)
          areas(ii) = 1.0 / s_a(ii) * (exp(log_fn(ii) + s_a(ii)*(z(ii)-x(ii))) - exp(log_fn(ii) + s_a(ii)*(z(ii-1)-x(ii))));
        else
          areas(ii) = std::exp(log_fn(ii)) * (z(ii) - z(ii-1));
      }
    }
    // Rprintf("check1=%f\n",log_fn(1) + s_a(1)*(z(1)-x(1)));
    // Rprintf("check2=%f\n",log_fn(1) + s_a(1)*(z(0)-x(1)));
    // Rprintf("hello3\n");
    radom_uniform = arma::randu(4);
    arma::vec cum_std_areas = arma::cumsum(areas / sum(areas));
    arma::uvec tmp = arma::find(cum_std_areas <= radom_uniform(0));
    // Rprintf("random=%f\n",radom_uniform(0));
    choose_region = tmp.n_elem;
    if(choose_region==0){
      candidate = x(choose_region) +
        1.0/s_a(choose_region)*std::log(radom_uniform(1)*areas(choose_region)*s_a(choose_region)/std::exp(log_fn(choose_region)));
      if(!arma::is_finite(candidate))
        candidate = -20;
      // Rprintf("test=%f\n",log_fn(choose_region));
    }
    else{
      if(s_a(choose_region)!=0){
        candidate = x(choose_region) +
          1.0/s_a(choose_region)*std::log(radom_uniform(1)*areas(choose_region)*s_a(choose_region)/std::exp(log_fn(choose_region))+
          std::exp(s_a(choose_region)*(z(choose_region-1)-x(choose_region))));
        if(!arma::is_finite(candidate))
          candidate = 20;
      }
      else{
        candidate = z(choose_region-1) + radom_uniform(1)*(areas(choose_region)/std::exp(log_fn(choose_region)));
      }
    }
    if(!arma::is_finite(candidate)){
      Rprintf("candidate not finite, return 0\n\n");
      for(int ii=0;ii<initial_x.n_elem;++ii){
        Rprintf("initial_x %d=%f | ", ii, initial_x(ii));
      }
      Rprintf("\n");
      for(int ii=0;ii<x.n_elem;++ii){
        Rprintf("x %d=%f | ", ii, x(ii));
      }
      Rprintf("\n");
      for(int ii=0;ii<log_fn.n_elem;++ii){
        Rprintf("log_fn %d=%f | ", ii, log_fn(ii));
      }
      Rprintf("\n");
      for(int ii=0;ii<s_a.n_elem;++ii){
        Rprintf("s_a %d=%f | ", ii, s_a(ii));
      }
      Rprintf("\n");
      for(int ii=0;ii<z.n_elem;++ii){
        Rprintf("z %d=%f | ", ii, z(ii));
      }
      Rprintf("\n");
      for(int ii=0;ii<areas.n_elem;++ii){
        Rprintf("areas %d=%f | ", ii, areas(ii));
      }
      Rprintf("\n");
      Rprintf("choose_region = %d\n", choose_region);
      Rprintf("cum_std_areas = %f\n",cum_std_areas(cum_std_areas.n_elem-1));
      return 0;
    }
    // Rprintf("candidate=%f\n",candidate);
    // squeeze step
    // Rprintf("hello4\n");
    arma::uvec tmp_lower;
    double higher_bound = s_a(choose_region)*(candidate-x(choose_region))+log_fn(choose_region);
    //Rprintf("hello_test1\n");
    // for(int kkk=0;kkk<log_fn.n_elem;++kkk){
    //   // Rprintf("log_fn_%d=%f | ",kkk,log_fn(kkk));
    // }
    if(candidate >= x(0) && candidate <= x(x.n_elem-1)){
      // Rprintf("hello_test2\n");
      tmp_lower = arma::find(x <= candidate);
      // Rprintf("hello_test3\n");
      choose_region_lower = tmp_lower.n_elem;
      // Rprintf("choose_region_lower=%d\n",choose_region_lower);
      if(radom_uniform(2) <= std::exp(log_fn(choose_region_lower-1)+l_a(choose_region_lower-1)*(candidate-x(choose_region_lower-1)) -
         higher_bound) ){
        return candidate;
      }
      // Rprintf("hello_test4\n");
    }
    else if(candidate < x(0)){
      choose_region_lower = 0;
    }
    else{
      choose_region_lower = x.n_elem;
    }
    // Rprintf("hello5\n");
    // squeeze fail calcu new point
    double new_log_fn = F_theta_y_eta_cpp_partial_credit(candidate, theta_minus_k, k, y_i, inv_sigma, A, D) - offset;
    // rejection test
    if(radom_uniform(2) <= std::exp(new_log_fn - higher_bound) ){
      return candidate;
    }
    flag = 1;
    // Rprintf("hello6\n");
    // update step
    if(MM < 20){
      x.insert_rows(choose_region_lower,1);
      x(choose_region_lower) = candidate;
      log_fn.insert_rows(choose_region_lower, 1);
      log_fn(choose_region_lower) = new_log_fn;
      s_a.insert_rows(choose_region_lower, 1);
      s_a(choose_region_lower) = F_prime_theta_y_eta_cpp_partial_credit(candidate, theta_minus_k, k, y_i, inv_sigma, A, D);
      l_a = arma::diff(log_fn) / arma::diff(x);
      z = arma::zeros(s_a.n_elem-1);
      for(int ii=0;ii<z.n_elem;++ii){
        z(ii) = (log_fn(ii) - log_fn(ii+1) + s_a(ii+1)*x(ii+1) - s_a(ii)*x(ii)) / (s_a(ii+1) - s_a(ii));
      }
      flag=0;
      MM++;
    }
  }
  Rprintf("error!\n");
  return 0;
}
// [[Rcpp::export]]
arma::vec sample_theta_i_myars(arma::vec x, arma::vec theta0_i, arma::vec y_i,
                               arma::mat inv_sigma, arma::mat A, arma::vec d) {
  int K = theta0_i.n_elem;
  for(int k=0;k<K;++k){
    theta0_i(k) = deriv_based_ARS(x, theta0_i, k+1, y_i, inv_sigma, A, d);
  }
  return theta0_i;
}
// [[Rcpp::export]]
arma::vec sample_theta_i_myars_partial_credit(arma::vec x, arma::vec theta0_i, arma::vec y_i,
                                              arma::mat inv_sigma, arma::mat A, arma::mat D) {
  int K = theta0_i.n_elem;
  for(int k=0;k<K;++k){
    theta0_i(k) = deriv_based_ARS_partial_credit(x, theta0_i, k+1, y_i, inv_sigma, A, D);
  }
  return theta0_i;
}
