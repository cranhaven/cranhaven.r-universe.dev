#include <Rcpp.h>
#include <numeric>
#include <iostream>
#include <math.h>

using namespace Rcpp;

// MVP_kronecker_rcpp
// MVP with Kronecker Matrix: Ax, with A = A_list[[1]] otimes ... otimes A_list[[P]] with P leq 4
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::export]]
NumericVector MVP_kronecker_rcpp(List& A_list, NumericVector& x){
  int P=A_list.size();
  std::vector<int> nrow_p(P);
  std::vector<int> ncol_p(P);
  int max_abs=1;
  int length_res=1;

  for(int p=P-1; p>=0; p--){
    const NumericMatrix& A_p=A_list[p];
    nrow_p[p]=A_p.nrow();
    ncol_p[p]=A_p.ncol();
    if(nrow_p[p]>ncol_p[p]){
      max_abs*=nrow_p[p];
    } else{
      max_abs*=ncol_p[p];
    }
    length_res*=nrow_p[p];
  }

  std::vector<double> v1(max_abs),v2(max_abs);

  for(int i=0; i<x.size(); i++){
    v1[i] = x[i];
  }

  for(int p=P-1; p>=0; p--){
    const NumericMatrix& A_p=A_list[p];
    int left=1;
    int right=1;
    for(int i=0; i<p; i++){
      left *= ncol_p[i];
    }
    for(int i=P-1; i>p; i--){
      right *= nrow_p[i];
    }
    int nrow=nrow_p[p];
    int ncol=ncol_p[p];
    int base_col=0;
    int base_row=0;

    v1.swap(v2);

    for(int s=0; s<left; s++){
      for(int j=0; j<right; j++){
        int index_col=base_col+j;
        NumericVector z_in(ncol);
        for(int l=0; l<ncol; l++){
          z_in[l]=v2[index_col];
          index_col += right;
        }
        NumericVector z_out(nrow);
        for(int r=0; r<nrow; r++){
          for(int c=0; c<ncol; c++){
            z_out[r] += A_p(r,c)*z_in[c];
          }
        }
        int index_row = base_row+j;
        for(int l=0; l<nrow; l++){
          v1[index_row] = z_out[l];
          index_row += right;
        }
      }
      base_col += (right*ncol);
      base_row += (right*nrow);
    }
  }

  NumericVector res(length_res);
  for(int i=0; i<res.size(); i++){
    res[i] = v1[i];
  }
  return res;
}

// MVP_khatrirao_rcpp
// MVP with Khatri-Rao Matrix: A%*%x, with A = A_list[[1]] odot ... odot A_list[[P]] with P leq 4 
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::export]]
NumericVector MVP_khatrirao_rcpp(List& A_list, NumericVector& x){
  int P=A_list.size();
  int n=x.size();
  std::vector<int> m_p(P);
  std::vector<NumericMatrix> A;
  for(int p=0; p<P; p++){
    const NumericMatrix& A_p = A_list[p];
    A.push_back(clone(A_p));
    m_p[p]=A_p.nrow();
  }
  int M = std::accumulate(m_p.begin(), m_p.end(), 1, std::multiplies<int>());
  NumericVector res(M);

  for(int i=0; i<n; i++){
    int j=0;

    const auto recursion = [P, i, &A, &m_p, &x](int p, int& j, double product, NumericVector& res) {
      const auto recursion_impl = [P, i, &A, &m_p, &x](int p, int& j, double product, NumericVector& res, const auto& impl) -> void {
        if(p<P-1) {
          for(int k=0; k<m_p[p]; ++k) {
            if(A[p](k,i) != 0.0) {
              impl(p+1, j, product*A[p](k,i), res, impl);
            } else {
              j += std::accumulate(m_p.begin()+p+1, m_p.end(), 1, std::multiplies<int>());
            }
          }
        } else {
          for(int k=0; k<m_p[p]; ++k) {
            res[j++] += product*A[p](k,i)*x[i];
          }
        }
      };
      return recursion_impl(p, j, product, res, recursion_impl);
    };

    recursion(0, j, 1.0, res);
    //product_recursion(0, P, j, i, 1.0, A, m_p, x, res);
  }
  return res;
}

// MVP_khatrirao_trans_rcpp
// MVP with transposed Khatri-Rao Matrix: A^T%*%y, with A = A_list[[1]] odot ... odot A_list[[P]] with P leq 4
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::export]]
NumericVector MVP_khatrirao_trans_rcpp(List& A_list, NumericVector& y){
  int P=A_list.size();
  std::vector<int> m_p(P);
  std::vector<NumericMatrix> A;
  for(int p=0; p<P; p++){
    const NumericMatrix& A_p = A_list[p];
    A.push_back(clone(A_p));
    m_p[p]=A_p.nrow();
  }
  int n = A[0].ncol();
  NumericVector res(n);

  for(int i=0; i<n; i++){
    int j=0;

    const auto recursion = [P, i, &A, &m_p, &y](int p, int& j, double product, NumericVector& res) {
      const auto recursion_impl = [P, i, &A, &m_p, &y](int p, int& j, double product, NumericVector& res, const auto& impl) -> void {
        if(p<P-1) {
          for(int k=0; k<m_p[p]; ++k) {
            if(A[p](k,i) != 0.0) {
              impl(p+1, j, product*A[p](k,i), res, impl);
            } else {
              j += std::accumulate(m_p.begin()+p+1, m_p.end(), 1, std::multiplies<int>());
            }
          }
        } else {
          for(int k=0; k<m_p[p]; ++k) {
            res[i] += product*A[p](k,i)*y[j++];
          }
        }
      };
      return recursion_impl(p, j, product, res, recursion_impl);
    };

    recursion(0, j, 1.0, res);
  }

return res;
}

// diag_kronecker_rcpp
// Diagonal of Kronecker Matrix: diag_A, A = A_list[[1]] otimes ... otimes A_list[[P]] with P leq 44
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::export]]
NumericVector diag_kronecker_rcpp(List& A_list){
  int P = A_list.size();
  std::vector<int> K_p(P);
  std::vector<NumericMatrix> A;
  for(int p=0; p<P; p++){
    const NumericMatrix& A_p = A_list[p];
    A.push_back(clone(A_p));
    K_p[p]=A_p.ncol();
  }
  NumericVector diag(std::accumulate(K_p.begin(), K_p.end(), 1, std::multiplies<int>()));

  int j=0;

  const auto recursion = [P, &A, &K_p](int p, int& j, double product, NumericVector& diag) {
    const auto recursion_impl = [P, &A, &K_p](int p, int& j, double product, NumericVector& diag, const auto& impl) -> void {
      if(p<P-1) {
        for(int k=0; k<K_p[p]; ++k) {
          impl(p+1, j, product*A[p](k,k), diag, impl);
        }
      } else {
        for(int k=0; k<K_p[p]; ++k) {
          diag[j++] = product*A[p](k,k);
        }
      }
    };
    return recursion_impl(p, j, product, diag, recursion_impl);
  };

  recursion(0, j, 1.0, diag);

  return diag;
}

// diag_khatrirao_rcpp
// Diagonal of AA^T with A = A_list[[1]] odot ... odot A_list[[P]] with P leq 4 
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::export]]
NumericVector diag_khatrirao_rcpp(List& A_list){
  int P=A_list.size();
  std::vector<int> K_p(P);
  std::vector<NumericMatrix> A;
  for(int p=0; p<P; p++){
    const NumericMatrix& A_p = A_list[p];
    A.push_back(clone(A_p));
    K_p[p]=A_p.nrow();
  }
  int n=A[0].ncol(); 
  NumericVector diag(std::accumulate(K_p.begin(), K_p.end(), 1, std::multiplies<int>()));

  for(int i=0; i<n; i++){
    int j=0;

    const auto recursion = [P, i, &A, &K_p](int p, int& j, double product, NumericVector& diag) {
      const auto recursion_impl = [P, i, &A, &K_p](int p, int& j, double product, NumericVector& diag, const auto& impl) -> void {
        if(p<P-1) {
          for(int k=0; k<K_p[p]; ++k) {
            if(A[p](k,i) != 0.0) {
              impl(p+1, j, product*A[p](k,i), diag, impl);
            } else {
              j += std::accumulate(K_p.begin()+p+1, K_p.end(), 1, std::multiplies<int>());
            }
          }
        } else {
          for(int k=0; k<K_p[p]; ++k) {
            const double w_ij = product*A[p](k,i);
            diag[j++] += w_ij*w_ij;
          }
        }
      };
      return recursion_impl(p, j, product, diag, recursion_impl);
    };

    recursion(0, j, 1.0, diag);
  }

  return diag;
  
}
 
// MVP_normalfactor_rcpp
// MVP with Normalfactor: B%*%x, with B = I_left otimes A otimes I_right
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::export]]
NumericVector MVP_normalfactor_rcpp(const NumericMatrix& A, int left, int right, const NumericVector& x){
  
  int J = A.nrow();
  //int left = nf_size[1];
  //int right = nf_size[2];
  int base = 0;
  NumericVector v(x.size());
  
  for(int l=0; l<left; l++){
    for(int r=0; r<right; r++){
      
      int index = base+r;
      
      NumericVector z_in(J);
      for(int j=0; j<J; j++){
        z_in[j] = x[index];
        index += right;
      }
      
      NumericVector z_out(J);
      for(int nrow=0; nrow<J; nrow++){
        for(int ncol=0; ncol<J; ncol++){
          z_out[nrow] += A(nrow,ncol)*z_in[ncol];
        }
      }
      
      index = base+r;
      
      for(int j=0; j<J; j++){
        v[index] = z_out[j];
        index += right;
      }
      
    }
    
    base += (right*J);
    
  }
  
  return v;
}
