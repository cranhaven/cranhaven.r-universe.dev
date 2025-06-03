#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;

//' @keywords internal
// [[Rcpp::export]]
NumericVector stl_sort(NumericVector x) {
  
  NumericVector y = clone(x);
  std::sort(y.begin(), y.end());
  return y;
  
}

//' @keywords internal
// [[Rcpp::export]]
NumericVector rcpp_rev(NumericVector x) {
  
  NumericVector revX = clone<NumericVector>(x);
  std::reverse(revX.begin(), revX.end());
  ::Rf_copyMostAttrib(x, revX);
  return revX;
  
}

//' @keywords internal
// [[Rcpp::export]]
NumericMatrix func_coef(NumericMatrix z, int scale){
  
  int n = z.nrow();
  int len = z.ncol();
  int lenw = std::pow(2.0, -scale);
  int t, i, j;
  NumericMatrix coef(n, len-lenw+1);
  NumericVector wave(lenw);
  
  for(j=0; j<lenw/2; j++){
    wave(j) = std::sqrt(std::pow(2.0, scale));
    wave(j+lenw/2) = -wave(j);
  }
  
  for(i=0; i<n; i++){
    for(t=0; t<len-lenw+1; t++){
      for(j=0; j<lenw; j++) coef(i, t) += z(i, t+j)*wave(j);
    }
  }
  
  return(coef);
  
}

//' @keywords internal
// [[Rcpp::export]]
NumericMatrix func_input(NumericMatrix coef, NumericMatrix sgn, bool sq, bool diag){
  int n = coef.nrow();
  int len = coef.ncol();
  int t, i, j, k, sg;
  int d = n*(n+1)/2;
  double avg;
  if(diag==true) d = n;
  NumericMatrix input(len, d);
  
  k = 0;
  for(i=0; i<n; i++){
    for(t=0; t<len; t++) input(t, k) = std::pow(coef(i, t), 2);
    avg = mean(input(_, k));
    for(t=0; t<len; t++){
      input(t, k) /= avg;
      if(sq==true) input(t, k) = std::sqrt(input(t, k));
    }
    k += 1;
    if(diag==false){
      for(j=i+1; j<n; j++){
        sg = sgn(i, j);
        for(t=0; t<len; t++){
          input(t, k) = std::pow(coef(i, t)-sg*coef(j, t), 2);
        }
        avg = mean(input(_, k));
        for(t=0; t<len; t++){
          input(t, k) /= avg;
          if(sq==true) input(t, k) = std::sqrt(input(t, k));
        }
        k += 1;
      }
    }
  }
  
  return(input);
  
}

//' @keywords internal
// [[Rcpp::export]]
List func_dc(NumericMatrix z, double gamma){
  
  int n = z.nrow();
  int nt = n*2;
  int len = z.ncol();
  int i, j;
  NumericMatrix cs(n, len-1);
  NumericMatrix acs(n, len-1);
  NumericVector res(len-1);
  NumericMatrix mat(n, len-1);
  NumericVector ref;
  NumericVector col(n);
  NumericVector iplus(n);
  NumericVector iminus(n);
  double factor;
  
  factor = std::sqrt((double) (len-1))/std::sqrt((double) len);
  iplus = z(_, 0);
  for(j=0; j<n; j++){
    iminus(j) = sum(z(j, _))-z(j, 0);
    cs(j, 0) = factor*(iplus(j)-iminus(j)/(len-1));
    acs(j, 0) = fabs(cs(j, 0));
  }
  if(len-2>0){
    for(i=1; i<len-1; i++){
      factor = std::sqrt((double) (i+1))*std::sqrt((double) (len-i-1))/std::sqrt((double) len);
      for(j=0; j<n; j++){
        iplus(j) = iplus(j)+z(j, i);
        iminus(j) = iminus(j)-z(j, i);
        cs(j, i) = factor*(iplus(j)/(i+1)-iminus(j)/(len-i-1));
        acs(j, i) = fabs(cs(j, i));
      }
    }
  }
  
  for(i=0; i<len-1; i++){
    ref = stl_sort(acs(_, i));
    ref = rcpp_rev(ref);
    
    factor = ((double)(nt-1))/nt; factor = std::pow(factor, gamma); 
    iplus(0) = ref(0);
    iminus(0) = sum(ref)-ref(0);
    col(0) = iplus(0)-iminus(0)/(nt-1);
    col(0) = factor*col(0);
    for(j=1; j<n; j++){
      factor = ((double)(j+1))*(nt-j-1)/nt; factor = std::pow(factor, gamma);
      iplus(j) = iplus(j-1)+ref(j);
      iminus(j) = iminus(j-1)-ref(j);
      col(j) = iplus(j)/(j+1)-iminus(j)/(nt-j-1);
      col(j) = factor*col(j);
    }
    mat(_, i) = col; res(i) = max(col);
  }
  
  return(Rcpp::List::create(Rcpp::Named("cs")=cs, Rcpp::Named("acs")=acs, Rcpp::Named("res")=res, Rcpp::Named("mat")=mat));
  
}

//' @keywords internal
// [[Rcpp::export]]
List func_cusum(NumericMatrix z){
  
  int n = z.nrow();
  int len = z.ncol();
  int i, j;
  NumericMatrix cs(n, len-1);
  NumericMatrix acs(n, len-1);
  NumericVector iplus(n);
  NumericVector iminus(n);
  double factor;
  
  factor = std::sqrt((double) (len-1))/std::sqrt((double) len);
  iplus = z(_, 0);
  for(j=0; j<n; j++){
    iminus(j) = sum(z(j, _))-z(j, 0);
    cs(j, 0) = factor*(iplus(j)-iminus(j)/(len-1));
    acs(j, 0) = fabs(cs(j, 0));
  }
  if(len-2>0){
    for(i=1; i<len-1; i++){
      factor = std::sqrt((double) (i+1))*std::sqrt((double) (len-i-1))/std::sqrt((double) len);
      for(j=0; j<n; j++){
        iplus(j) = iplus(j)+z(j, i);
        iminus(j) = iminus(j)-z(j, i);
        cs(j, i) = factor*(iplus(j)/(i+1)-iminus(j)/(len-i-1));
        acs(j, i) = fabs(cs(j, i));
      }
    }
  }
  
  return(Rcpp::List::create(Rcpp::Named("cs")=cs, Rcpp::Named("acs")=acs));
  
}

//' @keywords internal
// [[Rcpp::export]]
List func_cusum_vec(NumericVector z){
  
  int len = z.size();
  int i;
  NumericVector cs(len-1);
  NumericVector acs(len-1);
  double iplus, iminus, factor;
  
  factor = std::sqrt((double) (len-1))/std::sqrt((double) len);
  iplus = z(0);
  iminus = sum(z)-z(0);
  cs(0) = factor*(iplus-iminus/(len-1));
  acs(0) = fabs(cs(0));
  if(len-2>0){
    for(i=1; i<len-1; i++){
      factor = std::sqrt((double) (i+1))*std::sqrt((double) (len-i-1))/std::sqrt((double) len);
      iplus = iplus + z(i);
      iminus = iminus - z(i);
      cs(i) = factor*(iplus/(i+1) - iminus/(len-i-1));
      acs(i) = fabs(cs(i));
    }
  }
  
  return(Rcpp::List::create(Rcpp::Named("cs")=cs, Rcpp::Named("acs")=acs));
  
}

//' @keywords internal
// [[Rcpp::export]]
List func_dc_by(NumericMatrix z, double gamma, double dmby, double dtby){
  
  int n = z.nrow();
  int nb = ceil(n/dmby);
  int len = z.ncol();
  int lenb = ceil((len-1)/dtby);
  int nt = 2*n;
  int i, j, k;
  int mby = dmby;
  int tby = dtby;
  
  NumericMatrix cs(n, len-1);
  NumericMatrix acs(n, len-1);
  NumericVector res(len-1);
  NumericMatrix mat(n, len-1);
  NumericVector ref;
  NumericVector col(n);
  NumericVector iplus(n);
  NumericVector iminus(n);
  
  double factor, plus, minus;
  
  factor = ((double)(len-1))/len; factor = std::pow(factor, .5);
  iplus = z(_, 0);
  for(j=0; j<n; j++){
    iminus(j) = sum(z(j, _))-z(j, 0);
    cs(j, 0) = factor*(iplus(j)-iminus(j)/(len-1));
    acs(j, 0) = fabs(cs(j, 0));
  }
  if(len-2>0){
    for(i=1; i<len-1; i++){
      factor = ((double)(i+1))*(len-i-1)/len; factor = std::pow(factor, .5);
      for(j=0; j<n; j++){
        iplus(j) = iplus(j)+z(j, i);
        iminus(j) = iminus(j)-z(j, i);
        cs(j, i) = factor*(iplus(j)/(i+1)-iminus(j)/(len-i-1));
        acs(j, i) = fabs(cs(j, i));
      }
    }
  }
  
  for(i=0; i<lenb; i++){
    ref = stl_sort(acs(_, i*tby));
    ref = rcpp_rev(ref);
    
    factor = ((double)(nt-1))/(nt); factor = std::pow(factor, gamma);
    plus = ref(0);
    minus = sum(ref)-ref(0);
    col(0) = plus-minus/(nt-1);
    col(0) = factor*col(0);
    for(k=1; k<mby; k++){
      col(k) = col(0);
      plus += ref(k); minus -= ref(k);
    }
    for(j=1; j<nb; j++){
      factor = ((double)(j*mby+1))*(nt-j*mby-1)/nt; factor = std::pow(factor, gamma);
      plus += ref(j*mby);
      minus -= ref(j*mby);
      col(j*mby) = plus/(j*mby+1)-minus/(nt-j*mby-1);
      col(j*mby) = factor*col(j*mby);
      for(k=1; k<mby; k++){
        if(j*mby+k < n){
          col(j*mby+k) = col(j*mby);
          plus += ref(j*mby+k); minus -= ref(j*mby+k);
        }
      }
    }
    mat(_, i*tby) = col; res(i*tby) = max(col);
    
    for(k=1; k<tby; k++){
      if(i*tby+k < len-1){
        mat(_, i*tby+k) = col; res(i*tby+k) = res(i*tby);
      }
    }
  }
  
  return(Rcpp::List::create(Rcpp::Named("cs")=cs, Rcpp::Named("acs")=acs, Rcpp::Named("res")=res, Rcpp::Named("mat")=mat));
}

//' @keywords internal
// [[Rcpp::export]]
NumericMatrix func_mvt_ar(NumericMatrix ar, NumericMatrix res){
  
  int d = ar.nrow();
  int p = ar.ncol();
  int len = res.ncol();
  int i, j, t;
  NumericMatrix arz(d, len);
  arz = res;
  
  for(i=0; i<d; i++){
    for(t=p; t<len; t++){
      for(j=0; j<p; j++){
        arz(i, t) += arz(i, t-j-1)*ar(i, j);
      }
    }
  }
  
  return(arz);
}


// [[Rcpp::export]]
List func_density(NumericMatrix z, double c){
  
  int n = z.nrow();
  int len = z.ncol();
  int nt = 2*n;
  int i, j;
  double CONST = 2;
  NumericMatrix cs(n, len-1);
  NumericMatrix acs(n, len-1);
  NumericMatrix res(len-1, 6);
  NumericMatrix mat1(n, len-1);
  NumericMatrix mat2(n, len-1);
  NumericMatrix mat3(n, len-1);
  NumericMatrix mat4(n, len-1);
  NumericMatrix mat5(n, len-1);
  NumericVector ref;
  NumericVector col1(n);
  NumericVector col2(n);
  NumericVector col3(n);
  NumericVector col4(n);
  NumericVector col(n);
  NumericVector col5(len-1);
  NumericVector col6(len-1);
  NumericVector iplus(n);
  NumericVector iminus(n);
  double factor;
  
  factor = std::sqrt((double) (len-1))/std::sqrt((double) len);
  iplus = z(_, 0);
  for(j=0; j<n; j++){
    iminus(j) = sum(z(j, _))-z(j, 0);
    cs(j, 0) = factor*(iplus(j)-iminus(j)/(len-1));
    acs(j, 0) = fabs(cs(j, 0));
  }
  if(len-2>0){ 
    for(i=1; i<len-1; i++){
      factor = std::sqrt((double) (i+1))*sqrt((double) (len-i-1))/sqrt((double) len);
      for(j=0; j<n; j++){
        iplus(j) = iplus(j)+z(j, i);
        iminus(j) = iminus(j)-z(j, i);
        cs(j, i) = factor*(iplus(j)/(i+1)-iminus(j)/(len-i-1));
        acs(j, i) = fabs(cs(j, i));
      }
    }
  }
  
  for(i=0; i<len-1; i++){
    col5(i) = max(acs(_, i));
    col6(i) = mean(acs(_, i));
  }	
  res(_, 4) = col5;
  res(_, 5) = col6;
  
  for(i=0; i<len-1; i++){
    ref = stl_sort(acs(_, i));
    ref = rcpp_rev(ref);
    
    factor = std::sqrt((double) (nt-1))/std::sqrt((double) nt);
    iplus(0) = ref(0);
    iminus(0) = sum(ref)-ref(0);
    col1(0) = iplus(0)-iminus(0)/(nt-1);
    col2(0) = factor*col1(0);
    col3(0) = factor*col2(0);
    col4(0) = c*col1(0)+col2(0);
    col(0) = pow(ref(0), 2)-1;
    col(0) /= std::sqrt(CONST);
    for(j=1; j<n; j++){
      factor = std::sqrt((double) (j+1))*sqrt((double) (nt-j-1))/sqrt((double) nt);
      iplus(j) = iplus(j-1)+ref(j);
      iminus(j) = iminus(j-1)-ref(j);
      col1(j) = iplus(j)/(j+1)-iminus(j)/(nt-j-1);
      col2(j) = factor*col1(j);
      col3(j) = factor*col2(j);
      col4(j) = c*col1(j)+col2(j);
      col(j) = col(j-1)*sqrt((double) (2*(j-1)))+pow(ref(j), 2)-1;
      col(j) /= std::sqrt((double) (2*j));
    }
    mat1(_, i) = col1; mat2(_, i) = col2; mat3(_, i) = col3; mat4(_, i) = col4; mat5(_, i) = col;
    res(i, 0) = max(col1); res(i, 1) = max(col2); res(i, 2) = max(col3); res(i, 3) = max(col4);
  }
  
  return(Rcpp::List::create(Rcpp::Named("cs")=cs, Rcpp::Named("acs")=acs, Rcpp::Named("res")=res, Rcpp::Named("mat1")=mat1,
                                        Rcpp::Named("mat2")=mat2, Rcpp::Named("mat3")=mat3, Rcpp::Named("mat4")=mat4, Rcpp::Named("mat5")=mat5, Rcpp::Named("gam")=c));
}

// [[Rcpp::export]]
List func_density_by(NumericMatrix z, double dmby, double dtby, double c){
  
  int n = z.nrow();
  int nb = ceil(n/dmby);
  int len = z.ncol();
  int lenb = ceil((len-1)/dtby);
  int nt = 2*n;
  int i, j, k;
  int mby = dmby;
  int tby = dtby;
  double CONST = 2;
  
  NumericMatrix cs(n, len-1);
  NumericMatrix acs(n, len-1);
  NumericMatrix res(len-1, 6);
  NumericMatrix mat1(n, len-1);
  NumericMatrix mat2(n, len-1);
  NumericMatrix mat3(n, len-1);
  NumericMatrix mat4(n, len-1);
  NumericMatrix mat5(n, len-1);
  NumericVector ref;
  NumericVector col1(n);
  NumericVector col2(n);
  NumericVector col3(n);
  NumericVector col4(n);
  NumericVector col5(len-1);
  NumericVector col6(len-1);
  NumericVector col(n);
  NumericVector iplus(n); 
  NumericVector iminus(n); 
  
  double factor, plus, minus, plussq;
  
  factor = std::sqrt((double) (len-1))/std::sqrt((double) len);
  iplus = z(_, 0);
  for(j=0; j<n; j++){
    iminus(j) = sum(z(j, _))-z(j, 0);
    cs(j, 0) = factor*(iplus(j)-iminus(j)/(len-1));
    acs(j, 0) = fabs(cs(j, 0));
  }
  if(len-2>0){ 
    for(i=1; i<len-1; i++){
      factor = std::sqrt((double) (i+1))*sqrt((double) (len-i-1))/sqrt((double) len);
      for(j=0; j<n; j++){
        iplus(j) = iplus(j)+z(j, i);
        iminus(j) = iminus(j)-z(j, i);
        cs(j, i) = factor*(iplus(j)/(i+1)-iminus(j)/(len-i-1));
        acs(j, i) = fabs(cs(j, i));
      }
    }
  }
  for(i=0; i<len-1; i++){
    col5(i) = max(acs(_, i));
    col6(i) = mean(acs(_, i));
  }	
  res(_, 4) = col5;
  res(_, 5) = col6;
  
  for(i=0; i<lenb; i++){
    ref = stl_sort(acs(_, i*tby));
    ref = rcpp_rev(ref);
    
    factor = std::sqrt((double) (nt-1))/std::sqrt((double) nt);
    plus = ref(0);
    minus = sum(ref)-ref(0);
    plussq = pow(ref(0), 2);
    col1(0) = plus-minus/(nt-1);
    col2(0) = factor*col1(0);
    col3(0) = factor*col2(0);
    col4(0) = c*col1(0)+col2(0);
    col(0) = plussq-1; 
    col(0) /= std::sqrt(CONST);
    for(k=1; k<mby; k++){
      col1(k) = col1(0); col2(k) = col2(0); col3(k) = col3(0); col4(k) = col4(0); col(k) = col(0);
      plus += ref(k); minus -= ref(k);
      plussq += pow(ref(k), 2);
    }
    for(j=1; j<nb; j++){
      factor = std::sqrt((double) (j*mby+1))*std::sqrt((double) (nt-j*mby-1))/std::sqrt((double) nt);
      plus += ref(j*mby);
      minus -= ref(j*mby);
      plussq += pow(ref(j*mby), 2);
      col1(j*mby) = plus/(j*mby+1)-minus/(nt-j*mby-1);
      col2(j*mby) = factor*col1(j*mby);
      col3(j*mby) = factor*col2(j*mby);
      col4(j*mby) = c*col1(j*mby)+col2(j*mby);
      col(j*mby) = col((j-1)*mby)*std::sqrt((double) (2*(j-1)*mby))+plussq-mby; 
      col(j*mby) /= std::sqrt((double) (2*j*mby));
      for(k=1; k<mby; k++){
        if(j*mby+k < n){
          col1(j*mby+k) = col1(j*mby); col2(j*mby+k) = col2(j*mby); col3(j*mby+k) = col3(j*mby); col4(j*mby+k) = col4(j*mby); col(j*mby+k) = col(j*mby);
          plus += ref(j*mby+k); minus -= ref(j*mby+k); plussq += pow(ref(j*mby+k), 2);
        }
      }
    }
    mat1(_, i*tby) = col1; mat2(_, i*tby) = col2; mat3(_, i*tby) = col3; mat4(_, i*tby) = col4; mat5(_, i*tby) = col;
    res(i*tby, 0) = max(col1); res(i*tby, 1) = max(col2); res(i*tby, 2) = max(col3); res(i*tby, 3) = max(col4);
    
    for(k=1; k<tby; k++){
      if(i*tby+k < len-1){
        mat1(_, i*tby+k) = col1; mat2(_, i*tby+k) = col2; mat3(_, i*tby+k) = col3; mat4(_, i*tby+k) = col4; mat5(_, i*tby+k) = col;
        res(i*tby+k, 0) = res(i*tby, 0); res(i*tby+k, 1) = res(i*tby, 1); res(i*tby+k, 2) = res(i*tby, 2); res(i*tby+k, 3) = res(i*tby, 3);
      }
    }
  }
  
  return(Rcpp::List::create(Rcpp::Named("cs")=cs, Rcpp::Named("acs")=acs, Rcpp::Named("res")=res, Rcpp::Named("mat1")=mat1,
                                        Rcpp::Named("mat2")=mat2, Rcpp::Named("mat3")=mat3, Rcpp::Named("mat4")=mat4, Rcpp::Named("mat5")=mat5, Rcpp::Named("gam")=c));
}



// [[Rcpp::export]]
List func_input_off(NumericMatrix coef, NumericMatrix sgn, int log, int sq){
  int n = coef.nrow();
  int len = coef.ncol();
  int t, i, j, k, sg;
  int d = n*(n-1)/2;
  NumericVector avg(d);
  NumericMatrix input(len, d);
  
  k = 0;
  for(i=0; i<n; i++){
    for(j=i+1; j<n; j++){
      sg = sgn(i, j);
      for(t=0; t<len; t++){
        input(t, k) = pow(coef(i, t)-sg*coef(j, t), 2);
        if(log==1) input(t, k) = std::log(input(t, k));
      }
      avg(k) = mean(input(_, k));
      for(t=0; t<len; t++){
        input(t, k) /= avg(k);
        if(sq==1) input(t, k) = std::sqrt(input(t, k));
      }
      k += 1;
    }
  }
  
  return(Rcpp::List::create(Rcpp::Named("input")=input, Rcpp::Named("scale")=avg));
}

// [[Rcpp::export]]
NumericMatrix func_input_on_boot(NumericMatrix coef, int log, int sq, NumericVector avg){
  int n = coef.nrow();
  int len = coef.ncol();
  int t, i;
  NumericMatrix input(len, n);
  
  for(i=0; i<n; i++){
    for(t=0; t<len; t++){
      input(t, i) = pow(coef(i, t), 2);
      if(log==1) input(t, i) = std::log(input(t, i));
    }
    for(t=0; t<len; t++){
      input(t, i) /= avg(i);
      if(sq==1) input(t, i) = std::sqrt(input(t, i));
    }
  }
  
  return(input);
}

// [[Rcpp::export]]
NumericMatrix func_input_off_boot(NumericMatrix coef, NumericMatrix sgn, int log, int sq, NumericVector avg){
  int n = coef.nrow();
  int len = coef.ncol();
  int t, i, j, k, sg;
  int d = n*(n-1)/2;
  NumericMatrix input(len, d);
  
  k = 0;
  for(i=0; i<n; i++){
    for(j=i+1; j<n; j++){
      sg = sgn(i, j);
      for(t=0; t<len; t++){
        input(t, k) = pow(coef(i, t)-sg*coef(j, t), 2);
        if(log==1) input(t, k) = std::log(input(t, k));
      }
      for(t=0; t<len; t++){
        input(t, k) /= avg(k);
        if(sq==1) input(t, k) = std::sqrt(input(t, k));
      }
      k += 1;
    }
  }
  
  return(input);
}

