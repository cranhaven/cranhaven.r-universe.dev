// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
arma::mat dlyap_internal(arma::mat Sch_T, arma::mat Q, arma::mat C,int dim, bool undir) {
  arma::mat X_prime = arma::mat(dim, dim, fill::zeros);
  arma::mat temp_eye = eye(dim, dim);  
  arma::mat temp_eye2 = eye(dim*2, dim*2);  
  arma::mat c_vec = arma::mat(dim*2, 1, fill::zeros);
  arma::mat X_prime_temp = arma::mat(dim, 2, fill::zeros);
  arma::mat temp_b = arma::mat(dim*2, dim*2, fill::zeros);
  arma::mat temp = arma::mat(dim, dim, fill::zeros);
  vec sub = vec(dim, fill::zeros);
  vec sub_1 = vec(dim, fill::zeros);
  vec sub_2 = vec(dim, fill::zeros);
  int i = dim-1;
  
  while(i != -1){
    if(i == dim-1){
      if(Sch_T(i, i-1) == 0){
        
        if(undir){
          X_prime.col(i) = solve(trimatu(Sch_T(i,i)*Sch_T - temp_eye), C.col(i));
        }else{
          X_prime.col(i) = inv(Sch_T(i,i)* Sch_T - temp_eye) * C.col(i);
        }
      
        i = i-1;
      }else{
        temp_b = inv(kron(Sch_T.submat(i-1,i-1, i, i), Sch_T)- temp_eye2);  
        c_vec = vectorise(C.cols(i-1, i));
        X_prime_temp = reshape(temp_b*c_vec, dim, 2);
        X_prime.cols(i-1,i) = X_prime_temp;
        i = i - 2;
      }
      
    }else{
      if(i == 0){
        if(undir){
          temp = solve(trimatu(Sch_T(i,i)*Sch_T-temp_eye), temp_eye);
        }else{
          temp = inv(Sch_T(i,i)* Sch_T - temp_eye);
        }
        
        sub.zeros();
        for(int j = i+1; j<dim; j++){
          sub = sub+ Sch_T(i,j)*X_prime.col(j);
        }
        sub = Sch_T * sub;
        sub = C.col(i)- sub;
        X_prime.col(i) = temp * sub;
        i = i -1;
      }else{
        if(Sch_T(i,i-1) == 0){
          if(undir){
            temp = solve(trimatu(Sch_T(i,i)*Sch_T-temp_eye), temp_eye);
          }else{
            temp = inv(Sch_T(i,i)* Sch_T - temp_eye);
          }
          sub.zeros();
          for(int j = i+1; j<dim; j++){
            sub = sub+ Sch_T(i,j)*X_prime.col(j);
          }
          sub = Sch_T * sub;
          sub = C.col(i)- sub;
          X_prime.col(i) = temp * sub;
          i = i -1;
        }else{
          temp_b = inv(kron(Sch_T.submat(i-1,i-1, i, i), Sch_T)- temp_eye2);
          sub_1.zeros();
          sub_2.zeros();
          for(int j = i+1; j<dim; j++){
            sub_1 = sub_1+ Sch_T(i,j)*X_prime.col(j);
            sub_2 = sub_2+ Sch_T(i-1,j)*X_prime.col(j);
          }
          sub_1 = Sch_T * sub_1;
          sub_2 = Sch_T * sub_2;
          
          vec c_1 = C.col(i) - sub_1;
          vec c_2 = C.col(i-1) - sub_2;
          c_vec = join_cols(c_2, c_1);
          
          X_prime.cols(i-1, i) = reshape(temp_b * c_vec, dim, 2);
          i = i -2;
        }
        
        
      }
      
     
    }
    
  }
  arma::mat X = Q * X_prime * trans(Q); 
  return X;
}


