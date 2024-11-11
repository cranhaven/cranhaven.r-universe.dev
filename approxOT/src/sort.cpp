#include "sort.h"

bool compare (double a, double b) {
  return a < b;
}

std::vector<size_t> sort_indexes( matrix::ConstColXpr & v) {
  
  // initialize original index locations
  std::vector<size_t> idx(v.size());
  std::iota(idx.begin(), idx.end(), 0);
  
  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
            [&v](size_t i1, size_t i2) {return v(i1) < v(i2);});
  return idx;
}

void sort_indexes(const refVecConst & v, vectorI & idx) {
  int P = idx.size();
  // sort indexes based on comparing values in v
  std::sort(idx.data(), idx.data() + P,
            [&v](size_t i1, size_t i2) {return v(i1) < v(i2);});
}

std::vector<size_t> sort_indexes( const vector & v) {
  // Rcpp::Rcout << "using right one to gen new idx\n";
  // initialize original index locations
  // Rcpp::Rcout << v.size()<<" vsize check\n";
  std::vector<size_t> idx(v.size());
  // Rcpp::Rcout << idx[0] << "," <<idx[9]<<" check\n";
  std::iota(idx.begin(), idx.end(), 0);
  // Rcpp::Rcout << idx[0] << "," <<idx[9]<<" check\n";
  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
            [&v](size_t i1, size_t i2) {return v(i1) < v(i2);});
  return idx;
}

void sort_indexes(const matrix::ConstColXpr & v, std::vector<size_t> & idx) {
  
  // initialize original index locations
  std::iota(idx.begin(), idx.end(), 0); //fills with increasing values
  
  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
            [&v](size_t i1, size_t i2) {return v(i1) < v(i2);});
}

void sort_indexes_col(matrix::ColXpr & v, std::vector<size_t> & idx) {
  
  // initialize original index locations
  std::iota(idx.begin(), idx.end(), 0); //fills with increasing values
  
  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
            [&v](size_t i1, size_t i2) {return v(i1) < v(i2);});
}

void sort_indexes( vector & v, std::vector<size_t> & idx) {
  
  // initialize original index locations
  std::iota(idx.begin(), idx.end(), 0); //fills with increasing values
  
  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
            [&v](size_t i1, size_t i2) {return v(i1) < v(i2);});
}

void sort_indexes( const vector & v, std::vector<size_t> & idx) {
  // Rcpp::Rcout << "using sort for const vector\n";
  // initialize original index locations
  std::iota(idx.begin(), idx.end(), 0); //fills with increasing values
  
  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
            [&v](size_t i1, size_t i2) {return v(i1) < v(i2);});
}

void sort_indexes_bycol_Eigenmat(const refMatConst & v, matrixI & idx) { //checked
  
  int N = v.cols();
  int P = v.rows();
  
  for(int n = 0; n < N;  n++) {
    // initialize original index locations
    idx.col(n) = Eigen::VectorXi::LinSpaced(P,0,P-1); //fills with increasing values
    
    // sort indexes based on comparing values in v
    std::sort(idx.col(n).data(), idx.col(n).data() + P,
              [&v,n](size_t i1, size_t i2) {return v(i1,n) < v(i2,n);});
    // for(int i =0; i < idx.size(); i++) idx(i,n) = idx_temp[i];
  }
  
}

void rel_sort_indexes_byrow_Eigenmat(const refMatConst & v, matrixI & idx) { 
  
  int N = v.cols();
  int P = v.rows();
  
  if(idx.rows() != P || idx.cols() != N) 
  {
    idx.resize(N,P);
  }
  
  for(int p = 0; p < P;  p++) {
    // initialize original index locations
    idx.col(p) = Eigen::VectorXi::LinSpaced(N,0,N-1); //fills with increasing values
    
    vector v_row = v.row(p); // get row of v
    
    // sort indexes based on comparing values in v
    std::sort(idx.col(p).data(), idx.col(p).data() + N,
              [&v_row](size_t i1, size_t i2) {return v_row(i1) < v_row(i2);});
    // for(int i =0; i < idx.size(); i++) idx(i,n) = idx_temp[i];
  }
  
}

void sort_indexes_byrow_Eigenmat(const matrix & v, matrixI & idx) { 
  
  int N = v.cols();
  int P = v.rows();
  
  if(idx.rows() != P || idx.cols() != N) 
  {
    idx.resize(N,P);
  }
  
  for(int p = 0; p < P;  p++) {
    // initialize original index locations
    idx.col(p) = Eigen::VectorXi::LinSpaced(N,0,N-1); //fills with increasing values
    
    vector v_row = v.row(p); // get row of v
    
    // sort indexes based on comparing values in v
    std::sort(idx.col(p).data(), idx.col(p).data() + N,
              [&v_row](size_t i1, size_t i2) {return v_row(i1) < v_row(i2);});
    // for(int i =0; i < idx.size(); i++) idx(i,n) = idx_temp[i];
  }
  
}

void sort_indexes_byrow_totalentry(const matrix & v, matrixI & idx) {
  
  int N = v.cols();
  int P = v.rows();
  
  if(idx.rows() != P || idx.cols() != N) 
  {
    idx.resize(P,N);
  }
  
  // idx = Eigen::MatrixXi::LinSpaced(N*P,0,(N*P)-1);
  // Rcpp::Rcout << idx(P-1,0) << "\n";
  for(int p = 0; p < P;  p++) {
    vectorI idx_row = vectorI::LinSpaced(N,0,N-1);
    vector v_row = v.row(p); // get row of v
    // sort indexes based on comparing values in v
    std::sort(idx_row.data(), idx_row.data() + N,
              [&v_row](size_t i1, size_t i2) {return v_row(i1) < v_row(i2);});
    idx.row(p) = (idx_row*P).array() + p;
    // if(p == (P-1)){
    //   Rcpp::Rcout << "Shoudl be row: " << idx_row(0) << "\n";
    // }
  }
  // Rcpp::Rcout << idx(P-1,0) << "\n";
}

void sort_matrix_by_col(refMat v) {
  
  int N = v.cols();
  int P = v.rows();
  
  // for(auto n : v.colwise()) {
  //   std::sort(n.begin(), n.end())
  // } //available in future eigen.
  
  for(int n = 0; n < N;  n++) {
    // sort indexes based on comparing values in v
    std::sort(v.col(n).data(), v.col(n).data() + P);
  }
  
}

void sort_matrix_by_row(refMat v) {
  
  int N = v.cols();
  int P = v.rows();
  
  // for(auto n : v.colwise()) {
  //   std::sort(n.begin(), n.end())
  // } //available in future eigen.
  
  for(int p = 0; p < P;  p++) {
    vector v_row = v.row(p);
    std::sort(v_row.data(), v_row.data() + N);
    v.row(p) = v_row;
  }
  
}

void rank_mat(const matrix & v, matrixI & rank) {
  
  int N = v.cols();
  int P = v.rows();
  
  if(rank.rows() != P) {
    Rcpp::stop("Rows of ranks must match rows of data matrix");
  }
  
  if(rank.cols() != N) {
    Rcpp::stop("Cols of ranks must match cols of data matrix");
  }
  
  
  for(int p = 0; p < P;  p++) {
    // get row that want to sort across (across obs since obs in each col)
    vector v_row = v.row(p);
    
    // initialize original index locations
    vectorI idx = Eigen::VectorXi::LinSpaced(N,0,N-1); //fills with increasing values
    
    // sort indexes based on comparing values in v
    std::sort(idx.data(), idx.data() + N,
              [&v_row](size_t i1, size_t i2) {return v_row(i1) < v_row(i2);});
    
    // assign ranks
    for (int n = 0; n < N; n++) {
      rank(p,idx[n]) = n;
    }
  }
  
}

void rel_sort_matrix_by_col(refMat v, Eigen::DenseBase<Eigen::Matrix<int, -1, -1, 0, -1, -1> >::ColXpr idx) {

  int N = v.cols();
  int P = v.rows();
  matrix v_copy = v;

  // for(auto n : v.colwise()) {
  //   std::sort(n.begin(), n.end())
  // } //available in future eigen.

  for(int n = 0; n < N;  n++) {
    // sort matrix based on idx
    v.col(n) = v_copy.col(idx(n));
  }

}

void rel_sort_matrix_by_col(refMat v, vectorI & idx) {
  
  int N = v.cols();
  int P = v.rows();
  matrix v_copy = v;
  
  // for(auto n : v.colwise()) {
  //   std::sort(n.begin(), n.end())
  // } //available in future eigen.
  
  for(int n = 0; n < N;  n++) {
    // sort matrix based on idx
    v.col(n) = v_copy.col(idx(n));
  }
  
}

// void rel_sort_matrix_by_entry(refMat v, matrixI & idx) {
//   
//   int N = v.cols();
//   int P = v.rows();
//   matrix v_copy = v;
//   
//   // for(auto n : v.colwise()) {
//   //   std::sort(n.begin(), n.end())
//   // } //available in future eigen.
//   
//   
//   for( int p = 0 ; p < P; p++){
//     for(int n = 0; n < N;  n++) {
//       v(p,idx(p,n)) = v_copy(p, n);
//   }// sort matrix entries within row
//   }
//   
// }

void rel_sort_matrix_by_entry(refMat v, 
                              Eigen::DenseBase<Eigen::Matrix<int, -1, -1, 0, -1, -1> >::ColXpr idx) {
  
  int N = v.cols();
  int P = v.rows();
  matrix v_copy = v;
  vecMap v_copy_vec(v_copy.data(), v_copy.size());
  
  // for(auto n : v.colwise()) {
  //   std::sort(n.begin(), n.end())
  // } //available in future eigen.
  // vecMapI idx_vec(idx.data(), idx.size());
  vecMap v_vec(v.data(), v.size());
  
  // Rcpp::Rcout << v.data() <<"\n";
  // Rcpp::Rcout << v_vec.data() <<"\n";
  
  for( int i = 0 ; i < (N*P); i++){
    v_vec(i) = v_copy_vec(idx(i));
  }
  // Rcpp::Rcout << v.data() <<"\n";
  // Rcpp::Rcout << v_vec.data() <<"\n";
  
}

void rel_sort(const std::vector<size_t> & idx, vector & y) {
  vector temp_sort = y;
  std::sort(temp_sort.data(), temp_sort.data() + temp_sort.size());
  for(int i = 0; i < y.size(); i ++) y(idx[i]) = temp_sort(i);
}

void rel_sort(matrixI::ColXpr & idx, vector & y) {
  vector temp_sort = y;
  std::sort(temp_sort.data(), temp_sort.data() + temp_sort.size());
  for(int i = 0; i < y.size(); i ++) y(idx(i)) = temp_sort(i);
}

void rel_sorted_1(const refArrayConstI&  idx, 
                  vector & y, const refArrayConst& yorig) {
  for(int i = 0; i < y.size(); i ++) y(idx(i)) = yorig(i);
}

// vector sort_partial(refMat v, int middle) {
//   vector out = v;
//   std::partial_sort(v.data(), v.data() + middle, v.data() + v.size());
//   return(v);
// }
