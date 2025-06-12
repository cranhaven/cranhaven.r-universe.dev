#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List get_gamma_labels_c(const arma::mat z, const int C)
{
  int n = z.n_rows;
  arma::vec labels = arma::zeros<arma::vec>(n);
  Rcpp::List gamma(C);
  
  int count = 0;
  
  for (arma::uword i = 0; i < z.n_cols; i++)
  {
    arma::uvec temp = find(z.col(i) == 1);
    
    if (temp.n_elem > 0)
    {
      arma::vec fill = arma::vec(temp.n_elem);
      fill.fill(count + 1.0);
      labels(temp) = fill;
      
      for (arma::uword j = 0; j < temp.n_elem; j++)
      {
        temp[j] = temp[j] + 1;
      }
      gamma[count] = temp;
      count += 1;
    }
  }
  
  return Rcpp::List::create(Rcpp::Named("labels") = labels, 
                            Rcpp::Named("gamma") = gamma);
}

arma::uvec resize(const arma::uvec to_resize)
{
  arma::uvec temp = arma::zeros<arma::uvec>(to_resize.n_elem);
  // reset indices
  for (arma::uword k = 0; k < temp.n_elem; k++)
  {
    temp[k] = to_resize[k] - 1;
  }
  
  return(temp);
}
// [[Rcpp::export]]
Rcpp::List step_two_c(const int C, const Rcpp::List gamma, const arma::mat GG)
{
  // initialization of all the stuff
  arma::mat ss_off(C, C, arma::fill::zeros);
  arma::mat nn(C, C, arma::fill::zeros);
  arma::vec ss_diag = arma::zeros<arma::vec>(C);
  arma::mat mean_off(C,C, arma::fill::zeros);
  arma::vec mean_diag = arma::zeros<arma::vec>(C);
  
  for (int i = 0; i < C; i++)
  {
    arma::uvec gamma_i = resize(gamma[i]);
    if (gamma.size() <= i || gamma_i.n_elem <= 1) continue;
    
    for (int j = i; j < C; j++)
    {
      arma::uvec gamma_j = resize(gamma[j]);
      if (gamma.size() <= j || gamma_j.n_elem <= 1 ) continue;

      if (i == j)
      {
        arma::mat GG_1 = GG.submat(gamma_i, gamma_j);
        nn(i,j) = (GG_1.n_cols*GG_1.n_rows - GG_1.n_rows);
        mean_diag[i] = arma::mean(GG_1.diag());
        ss_diag[i] = arma::var(GG_1.diag()); // diagonal variance
        float M = (arma::accu(GG_1) - arma::accu(GG_1.diag()))/nn(i,j); // off diagonals mean
        mean_off(i, j) = M;
        arma::mat temp_GG_M = arma::pow(GG_1 - M,2); // temp for GG_1 - M
        ss_off(i,j) = (arma::accu(temp_GG_M) - arma::accu(temp_GG_M.diag())) / nn(i,j); // offdiagonals variance
      }
      else
      {
        arma::mat GG_2 = GG.submat(gamma_i, gamma_j);
        nn(i,j) = (GG_2.n_cols*GG_2.n_rows);
        float M = arma::accu(GG_2)/nn(i,j);
        mean_off(i,j) = M;
        ss_off(i,j) = arma::accu(arma::pow(GG_2 - M,2)) / nn(i,j);
      }
    }
  }
  
  return Rcpp::List::create(Rcpp::Named("ss_off") = ss_off,
                            Rcpp::Named("ss_diag") = ss_diag,
                            Rcpp::Named("mean_diag") = mean_diag,
                            Rcpp::Named("mean_off") = mean_off);
}

// [[Rcpp::export]]
Rcpp::List step_three_c(const int C, const Rcpp::List gamma, const arma::mat GG, const arma::mat mean_off,
                      const arma::vec mean_diag)
{ 
  // initialization of all the stuff
  arma::mat ss_bound(C, C, arma::fill::zeros);
  arma::mat nn(C, C, arma::fill::zeros);

  for (int i = 0; i < C; i++)
  {
    arma::uvec gamma_i = resize(gamma[i]);
    if (gamma.size() <= i || gamma_i.n_elem <= 1) continue;
    
    arma::mat GG_1 = GG.submat(gamma_i, gamma_i);
    
    for (int j = 0; j < C; j++)
    {
      arma::uvec gamma_j = resize(gamma[j]);
      if (gamma.size() <= j || gamma_j.n_elem <= 1 ) continue;
      
      if (i == j)
      {
        nn(i,j) = (GG_1.n_cols*GG_1.n_rows - GG_1.n_rows);
        float s_homogenous = 0.0;
        
        // loop over all the rows of GG_1
        for (arma::uword k = 0; k < GG_1.n_rows; k++)
        {
          arma::rowvec term_one = GG_1.row(k);
          float term_two = GG_1(k, k) - mean_diag(i);
          
          for (arma::uword l = 0; l < term_one.n_elem; l++)
          {
            term_one[l] = (term_one[l] - mean_off(i,j))*term_two;
          }
   
          float term_three = GG_1(k,k) - mean_off(i,j);

          s_homogenous += arma::accu(term_one) - term_three*term_two;
        }
        ss_bound(i,j) = s_homogenous/nn(i,j);
      }
      else 
      {
        arma::mat GG_2 = GG.submat(gamma_i, gamma_j);
        GG_2 = GG_2 - mean_off(i,j);
        nn(i,j) = (GG_2.n_cols*GG_2.n_rows);
        
        float s_heterogeneous = 0.0;
        
        for (arma::uword k = 0; k < GG_2.n_rows; k++)
        {
          float term_two = GG_1(k, k) - mean_diag(i);
          
          s_heterogeneous += arma::accu(GG_2.row(k) * term_two);
        }
        ss_bound(i,j) = s_heterogeneous/nn(i,j);
      }
    }
  }
  
  return Rcpp::List::create(Rcpp::Named("ss_bound") = ss_bound,
                            Rcpp::Named("blah") = 0.0);
}





// [[Rcpp::export]]
Rcpp::List step_four_c(const int C, const Rcpp::List gamma, const arma::mat GG, const arma::mat mean_off,
                        const arma::vec mean_diag)
{ 

  arma::cube sss(C, C, C, arma::fill::zeros); 
  arma::cube nnn(C, C, C, arma::fill::zeros); 
  // arma::mat GG_1(MAX, MAX, arma::fill::zeros);
  // arma::mat GG_2(MAX, MAX, arma::fill::zeros);

  for (int i = 0; i < C; i++)
  {
    arma::uvec gamma_i = resize(gamma[i]);
    // if (gamma.size() <= i || gamma_i.n_elem <= 1) continue;
    if (gamma_i.n_elem <= 1) continue;
    
      
      for (int j = i; j < C; j++)
      { 
        arma::uvec gamma_j = resize(gamma[j]);
        // if (gamma.size() <= j || gamma_j.n_elem <= 1 ) continue;
        if (gamma_j.n_elem <= 1 ) continue;
        
        
          arma::mat GG_1 = GG.submat(gamma_i, gamma_j);
        
          if (j == i)
          {
            // GG_1 = GG.submat(gamma_i, gamma_i);
            arma::mat term_two = diagmat(GG_1.diag() - mean_diag[i]);
            arma::mat term_three = diagmat(GG_1.diag() - mean_off(i,j));
            GG_1 = (GG_1 - mean_off(i,j)) + term_two - term_three;
          }
          else 
          {
            // GG_1 = GG.submat(gamma_i, gamma_j);
            GG_1 = GG_1 - mean_off(i,j);
          }  
          for (int k = j; k < C; k++)
          { 
            arma::uvec gamma_k = resize(gamma[k]);
            if (gamma_k.n_elem <= 1 ) continue;
              
            arma::mat GG_2 = GG.submat(gamma_i, gamma_k);
            GG_2 = GG_2 - mean_off(i,k);
            
            if (i == j && j == k )
            {
              arma::mat term_two = diagmat(GG_2.diag() - mean_diag[i]);
              arma::mat term_three = diagmat(GG_2.diag() - mean_off(i,k));
              GG_2 = (GG_2 - mean_off(i,k)) + term_two - term_three;

              float S = 0.0;

              nnn(i,j,k) = GG_1.n_rows*GG_1.n_cols*GG_2.n_cols - (GG_1.n_rows*GG_1.n_cols) -
                (GG_2.n_rows*GG_2.n_cols) + GG_1.n_rows;

              for (arma::uword r1 = 0; r1 < GG_1.n_rows; r1++)
              {
                S += arma::accu(kron(GG_1.row(r1),GG_2.row(r1))) - arma::accu(GG_1 % GG_2 ) -
                  GG_1(r1,r1)*accu(GG_2.row(r1)) + GG_1(r1, r1)*GG_1(r1, r1);
                //S = S + sum(kronecker(GG_1[r1,],GG_2[r1,])) - sum(GG_1*GG_2) - sum(GG_1[r1,r1]*GG_2[r1,]) + sum((GG_1[r1,r1])^2)
              }
              sss(i,j,k) = S/nnn(i,j,k);
            }

            if (i == j && j < k)
            {
              float S = 0.0;
              nnn(i,j,k) = GG_1.n_rows*GG_1.n_cols*GG_2.n_cols - GG_2.n_rows*GG_2.n_cols;

              for (arma::uword r1 = 0; r1 < GG_1.n_rows; r1++)
              {
                S += arma::accu(kron(GG_1.row(r1),GG_2.row(r1))) - GG_1(r1,r1)*arma::accu(GG_2.row(r1));
                // S = S + sum(kronecker(GG_1[r1,],GG_2[r1,])) - sum(GG_1[r1,r1]*GG_2[r1,])
              }
              sss(i,j,k) = S/nnn(i,j,k);
            }

            if (i < j && j == k)
            {
              float S = 0.0;
              nnn(i,j,k) = GG_1.n_rows*GG_1.n_cols*GG_2.n_cols - GG_1.n_rows*GG_1.n_cols;

              for (arma::uword r1 = 0; r1 < GG_1.n_rows; r1++)
              {
                arma::rowvec blah = GG_1.row(r1)%GG_2.row(r1);
                S += arma::accu(kron(GG_1.row(r1),GG_2.row(r1))) - arma::accu(blah);
                // S = S + sum(kronecker(GG_1[r1,],GG_2[r1,])) - sum(GG_1[r1,] * GG_2[r1,])
              }
              sss(i,j,k) = S/nnn(i,j,k);
            }

            if (i < j && j < k)
            {
              float S = 0.0;
              nnn(i,j,k) = GG_1.n_rows*GG_1.n_cols*GG_2.n_cols;

              for (arma::uword r1 = 0; r1 < GG_1.n_rows; r1++)
              {
                S += arma::accu(kron(GG_1.row(r1),GG_2.row(r1)));
                // S = S +  sum(kronecker(GG_1[r1,],GG_2[r1,]))
              }

              sss(i,j,k) = S/nnn(i,j,k);
            }
            sss(i,k,j) = sss(j,k,i) = sss(k,j,i) = sss(k,i,j) = sss(j,i,k) = sss(i,j,k);
            // sss(i,k,j) = sss(i,j,k);
            // sss(j,k,i) = sss(i,j,k);
            // sss(k,j,i) = sss(i,j,k);
            // sss(j,i,k) = sss(i,j,k);
          }
      }
  }

  
  return Rcpp::List::create(Rcpp::Named("sss") = sss,
                            Rcpp::Named("blah") = 0);
}

// [[Rcpp::export]]
Rcpp::List step_five_c(const int C, const Rcpp::List gamma, const arma::mat ss_off,
                       const arma::vec ss_diag, arma::vec labels, arma::cube sss, 
                       arma::mat ss_bound,
                       const int N)
{ 
  
  Rcpp::List Cov(C);
  
  for (int c = 0; c < C; c++)
  {
    arma::mat temp(N + 1, N + 1, arma::fill::zeros);
    temp(N, N) = ss_diag[c]; // boundary
    arma::uvec gamma_c = resize(gamma[c]);
    
    for (int i = 0; i < N; i++)
    {
      // use unisgned int for comparison below, linux CRAN checks require it
      unsigned int temp_i = i;
      if (temp_i != gamma_c[0])
      {
        for (int j = i; j < N; j++)
        {
          unsigned int temp_j = j;
          if (temp_j != gamma_c[0])
          {
            if (i == j)
            {
              temp(i,j) = ss_off(c, labels[i] - 1); // diagonals, reset labels 
            }
            else 
            {
              temp(i, j) = sss(c, labels(i)-1, labels(j) - 1); // off diagonals
            }
          }
          temp(j, i) = temp(i, j);
        }
        // temp(N + 1, i) = temp(i, N + 1) = ss_bound(c, labels(i) - 1); //boundary
        temp(N, i) = temp(i, N) = ss_bound(c, labels(i) - 1); //boundary
        
      }
    }
    // covariance on the average column
    
    // first line: Cov[[c]][gamma[[c]][1], 1:N] = colSums(Cov[[c]][1:N, 1:N])/(N - 1)
    arma::rowvec first_line = arma::sum(temp.submat(0, 0, N-1, N-1), 0) / (N - 1);
    temp.submat(gamma_c[0], 0, gamma_c[0], N - 1) = first_line;
    
    // second line: Cov[[c]][1:N, gamma[[c]][1]] = t(Cov[[c]][gamma[[c]][1], 1:N])
    arma::rowvec second_line = temp.submat(gamma_c[0], 0, gamma_c[0], N-1);
    temp.submat(0, gamma_c[0], N-1, gamma_c[0]) = second_line.t();

    // third line: Cov[[c]][gamma[[c]][1], gamma[[c]][1]] = sum(diag(Cov[[c]])[1:N])/(N - 1)
    // float third_line1 = arma::accu(arma::resize(temp.diag(), N));
    arma::vec diag = temp.diag();
    arma::vec diag_1_N = diag.subvec(0, N-1);
    float third_line = arma::accu(diag_1_N)/(N-1);
    temp(gamma_c[0], gamma_c[0]) = third_line;

    // fourth line: Cov[[c]][(N + 1), gamma[[c]][1]] = Cov[[c]][gamma[[c]][1],(N + 1)] = sum(Cov[[c]][1:N,(N + 1)])/(N - 1)
    float fourth_line = arma::accu(temp.submat(0, N, N-1, N)) / (N - 1);
    // temp(N, gamma_c[0]) = fourth_line;
    // temp(gamma_c[0], N) = fourth_line;
    
    temp(N, gamma_c[0]) = temp(gamma_c[0], N) = fourth_line;

    // now assign total to the Cov list
    Cov[c] = temp;
  }
  
  return Rcpp::List::create(Rcpp::Named("Cov") = Cov,
                            Rcpp::Named("blah") = 0);
}


