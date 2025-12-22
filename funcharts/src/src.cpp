#include <RcppArmadillo.h>
#include <string>
#include <cmath>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
using namespace Rcpp;


// [[Rcpp::export]]
arma::mat score(const arma::mat& e,
                const double lambda,
                const arma::mat& k,
                bool huber=true) {


  arma::mat phi = lambda * e;

  // Huber's Score Function
  if (huber) {

    arma::uvec idx1 = find(e > k);
    arma::uvec idx2 = find(e < -k);

    phi.elem(idx1) = e.elem(idx1) - (1 - lambda) * k.elem(idx1);
    phi.elem(idx2) = e.elem(idx2) + (1 - lambda) * k.elem(idx2);

  }
  // bs score function
  else {

    phi = e;
    arma::uvec idx = find(abs(e) <= k);
    phi.elem(idx) = e.elem(idx) * (1 - (1 - lambda) * pow(1 - square(e.elem(idx) / k.elem(idx)), 2));

  }

  return phi;
}



// [[Rcpp::export]]
arma::vec score2(const arma::vec& e,
                 const double lambda,
                 const arma::vec& k,
                 bool huber = true) {


  arma::vec phi = lambda * e;

  // Huber's Score Function
  if (huber) {

    arma::uvec idx1 = find(e > k);
    arma::uvec idx2 = find(e < -k);

    phi.elem(idx1) = e.elem(idx1) - (1 - lambda) * k.elem(idx1);
    phi.elem(idx2) = e.elem(idx2) + (1 - lambda) * k.elem(idx2);

  }
  // bs score function
  else {

    phi = e;
    arma::uvec idx = find(abs(e) <= k);
    phi.elem(idx) = e.elem(idx) * (1 - (1 - lambda) * pow(1 - square(e.elem(idx) / k.elem(idx)), 2));

  }

  return phi;
}


// [[Rcpp::export]]
arma::vec statisticY_EWMA_vec(const arma::vec& X,
                              const arma::vec& Y_previous,
                              double lambda,
                              const arma::vec& k,
                              bool huber) {

  int nvars = X.n_elem;
  arma::vec e = X - Y_previous;
  arma::vec phi = score2(e, lambda, k, huber);
  arma::vec W = phi / e;
  for (int i = 0; i < nvars; ++i) {
    if (e[i] == 0) {
      W[i] = lambda;
    }
  }
  arma::vec Y = (W % X) + ((1 - W) % Y_previous);
  return Y;
}

// [[Rcpp::export]]
arma::mat statisticY_EWMA_cpp(const arma::mat& X,
                              double lambda,
                              const arma::vec& k,
                              bool huber,
                              const arma::vec& idx) {

  int nvars = X.n_cols;
  arma::mat Y(idx.n_elem, nvars); // Matrix to hold all the vectors
  arma::vec Yvec = arma::zeros<arma::vec>(nvars);

  for (arma::uword kk = 0; kk < idx.n_elem; ++kk) {
    Yvec = statisticY_EWMA_vec(X.row(idx(kk) - 1).t(),
                               Yvec,
                               lambda,
                               k,
                               huber);
    Y.row(kk) = Yvec.t();
  }

  return Y;
}


// [[Rcpp::export]]
double calculate_T2(const arma::vec& Y, const arma::mat& Vectors, const arma::vec& Values) {
  // Check dimensions
  if (Y.n_elem != Vectors.n_rows || Values.n_elem != Vectors.n_cols) {
    throw std::runtime_error("Dimension mismatch");
  }

  // Perform the optimized calculation
  arma::vec temp = Vectors.t() * Y; // This is t(Vectors) %*% Y in R
  temp = temp % temp; // Element-wise squaring, equivalent to square each element of the vector
  temp = temp / Values; // Element-wise division by Values, equivalent to each element of the vector divided by corresponding element in Values

  // Sum up the elements for the final result
  double result = sum(temp);

  // Return the result
  return result;
}


// [[Rcpp::export]]
arma::vec calculate_T2_vec(const arma::mat& Y, const arma::mat& Vectors, const arma::vec& Values) {
  // Check dimensions
  if (Y.n_cols != Vectors.n_rows || Values.n_elem != Vectors.n_cols) {
    throw std::runtime_error("Dimension mismatch");
  }

  arma::mat YVsquared = arma::square(Y * Vectors);
  arma::mat temp = YVsquared.each_row() / Values.t();
  arma::vec result = arma::sum(temp, 1);

  return result;
}


// [[Rcpp::export]]
List get_RL_cpp(const arma::mat& X2,
                const arma::mat& X_IC,
                const arma::vec& idx2,
                const arma::vec& idx_IC,
                double lambda,
                const arma::vec& k,
                bool huber,
                double h,
                const arma::vec& Values,
                const arma::mat& Vectors) {

  arma::uword nvars = X2.n_cols;
  arma::uword nmax = idx2.n_elem;
  arma::uword nwarm = idx_IC.n_elem;
  arma::vec T2_IC(nwarm);
  arma::vec T2(nmax);
  int RL = 0;
  arma::vec T2_out;

  arma::vec Y = arma::zeros<arma::vec>(nvars);
  for (arma::uword kk = 0; kk < nwarm; ++kk) {
    arma::vec Xkk = X_IC.row(idx_IC(kk) - 1).t();
    Y = statisticY_EWMA_vec(Xkk, Y, lambda, k, huber);

    T2_IC(kk) = calculate_T2(Y, Vectors, Values);

  }

  for (arma::uword kk = 0; kk < nmax; ++kk) {
    arma::vec Xkk = X2.row(idx2(kk) - 1).t();
    Y = statisticY_EWMA_vec(Xkk, Y, lambda, k, huber);
    T2(kk) = calculate_T2(Y, Vectors, Values);
    if (T2(kk) > h) {
      T2.resize(kk + 1);
      T2_out = T2;
      RL = kk + 1;
      break;
    }
    if ((kk == (nmax - 1)) & (T2(kk) < h)) {
      RL = NA_INTEGER;
    }
  }

  List resultList;
  resultList["RL"] = RL;
  resultList["T2"] = T2;
  resultList["T2_IC"] = T2_IC;

  return resultList;

}





// [[Rcpp::export]]
double der_c(double asn, double smin, double smax, double der_0){

  double out;
  if (asn>=(smin-pow(10,-10)) and	 asn<=(smax+pow(10,-10))){
    out=pow(std::abs(der_0-asn),2);
  } else {
    out=R_PosInf;
  }
  return out;
}

// [[Rcpp::export]]
double loss_c(double z, double y, double z1,double y1,double alpha, double der_h){
  double out= pow( alpha*(z-y),2 )+pow((1-alpha)*(z1*der_h-y1),2);
  return(out);
}
// // [[Rcpp::export]]
// arma::vec eval_fd_c(List fd,arma::vec seq Rcpp::Function eval_fd){
//   Rcpp::Environment base("package:fda");
//   Rcpp::Function eval_fd_c = base["eval.fd"];
//   arma::vec out=Rcpp :: as < arma :: vec >(eval_fd_c(seq,fd));
//   return(out);
//
// }

// // [[Rcpp::export]]
// List DP(int N,int M,arma::vec l, arma::vec u,arma::mat range_x,arma::mat range_tem,arma::mat grid_t, List x_fd_std,
//         List der_x_fd_std,double delta_x1,arma::vec template_eval,arma::vec der_template_eval,
//         double smin,double smax,double alpha,double lambda,double der_0, Rcpp::Function eval_fd_c){
//   // Rcpp::Environment base("package:fda");
//   // Rcpp::Function eval_fd_c = base["eval.fd"];
//   arma::mat D(N,M), P(N,M), L(N,M),x_eval(M,1), der_x_eval(M,1);
//
//   D.fill(arma::datum::inf);
//   P.fill(arma::datum::inf);
//   L.zeros();
//   arma::field<arma::vec> grid_search(N);
//   double delta_t=grid_t(1)-grid_t(0);
//   arma::mat matrepr=arma::repmat(range_x,1,M);
//   if (u(0)!=range_x(0)){
//     grid_search[0]=arma::linspace( l(0), u(0), M );
//   }   else{
//     arma::vec zee(M);
//     zee.zeros();
//     grid_search(0)=zee;
//   }
//   int sta=min(find(grid_t<=(range_tem(0)+delta_x1)));
//   int end=max(find(grid_t<=(range_tem(0)+delta_x1)));
//   arma::mat zeros(end+1,1);
//   zeros.zeros();
//   D.submat(sta,0,end,0)=zeros;
//   L.submat(sta,0,end,0)=zeros;
//
//   arma::mat zeros2(1,M);
//   zeros2.zeros();
//   if(u(0)!=range_x(0)){
//
//     D.row(0)=zeros2;
//   }
//   L.row(0)=zeros2;
//   arma::vec start_jj= arma::conv_to <arma::vec>::from(l==range_x(0));
//   double derr;
//   int ind_min,start_ll,end_ll;
//   List grid_2_list(M);
//   arma::vec grid_search_i,grid_search_im1,grid_search2,div,start_ll_r,end_ll_r;
//   for (int i=1;i<N;++i){
//     grid_search[i]=  arma::linspace<arma::colvec>( l(i), u(i), M );
//     x_eval.col(0)= Rcpp :: as < arma :: mat >(eval_fd_c(grid_search[i],x_fd_std));
//     der_x_eval.col(0)=Rcpp :: as < arma :: mat >(eval_fd_c(grid_search[i],der_x_fd_std));
//     grid_search_i=grid_search(i);
//     grid_search_im1=grid_search(i-1);
//     for (int j=start_jj(i);j<M;++j){
//       grid_search2=(grid_search_im1).elem(arma::find(grid_search_im1<=grid_search_i(j)&&grid_search_im1>=(smax+pow(10,-6))*grid_t(i-1)+grid_search_i(j)-(smax+pow(10,-6))*grid_t(i)));
//       grid_2_list[j]=grid_search2;
//       start_ll_r=arma::conv_to<arma::vec>::from(arma::find(grid_search_im1==grid_search2(0)));
//       end_ll_r=arma::conv_to<arma::vec>::from(arma::find(grid_search_im1==grid_search2(grid_search2.n_rows-1)));
//       start_ll=start_ll_r(0);
//       end_ll=end_ll_r(0);
//       arma::vec cost_ij(grid_search2.n_rows),l_ij(grid_search2.n_rows),derr_ii(grid_search2.n_rows);
//       for (int l=start_ll;l<end_ll+1;++l){
//
//         derr=((grid_search_i(j)-grid_search2(l-start_ll))/(grid_t(i)-grid_t(i-1)))/der_0;
//         derr_ii(l-start_ll)=derr;
//         cost_ij(l-start_ll)=(delta_t*(loss_c(x_eval(j),template_eval(i),der_x_eval(j),der_template_eval(i),alpha,derr))+lambda*delta_t*der_c((grid_search_i(j)-grid_search2(l-start_ll))/(grid_t(i)-grid_t(i-1)),smin,smax,der_0)+D(i-1,l));
//         l_ij(l-start_ll)=L(i-1,l)+delta_t;
//       }
//       div=cost_ij/l_ij;
//       // double min_ij=min(div);
//       ind_min=div.index_min();
//       D(i,j)=cost_ij(ind_min);
//       P(i,j)=grid_search2(ind_min);
//       L(i,j)=l_ij(ind_min);
//     }
//   }
//   List out=List::create(grid_search,D,P,L);
//   return(out);
// }
//



// [[Rcpp::export]]
List DP3(int N,int M,arma::vec l, arma::vec u,arma::mat range_x,arma::mat range_tem,arma::mat grid_t, List x_fd_std,List der_x_fd_std,double delta_x,arma::vec template_eval,arma::vec der_template_eval,double smin,double smax,double alpha,double lambda,double der_0, Rcpp::Function eval_fd_c){
  // Rcpp::Environment base("package:fda");
  // Rcpp::Function eval_fd_c = base["eval.fd"];
  arma::mat D(N,M), P(N,M), L(N,M),x_eval(M,1), der_x_eval(M,1);

  D.fill(arma::datum::inf);
  P.fill(arma::datum::inf);
  L.zeros();
  arma::vec grid_search(N*M);
  arma::field<arma::uvec> ind_f(N);
  arma::field<arma::vec> grid_search_list(N);
  double delta_t=grid_t(1)-grid_t(0);
  arma::mat matrepr=arma::repmat(range_x,1,M);
  arma::uvec ind=arma::linspace<arma::uvec>(0, M-1,M);
  ind_f(0)=ind;
  arma::vec pp=arma::linspace( l(0), u(0), M );
  grid_search.elem(ind)=pp;
  grid_search_list(0)=pp;
  int sta=min(find(grid_t<=(range_tem(0)+delta_x)));
  int end=max(find(grid_t<=(range_tem(0)+delta_x)));
  arma::mat zeros(end+1,1);
  zeros.zeros();
  D.submat(sta,0,end,0)=zeros;
  L.submat(sta,0,end,0)=zeros;
  arma::mat zeros2(1,M);
  zeros2.zeros();
  if(u(0)!=range_x(0)){
    D.row(0)=zeros2;
  }
  L.row(0)=zeros2;
  arma::vec start_jj= arma::conv_to <arma::vec>::from(l==range_x(0));
  double derr;
  int ind_min,start_ll,end_ll;
  arma::field<arma::vec> grid_2_list(M);
  arma::vec grid_search_im1,grid_search2,div,start_ll_r,end_ll_r,pp2,x_eval_i,der_x_eval_i,grid_search_i,pp3;
  arma::uvec ind2,indm1;
  for (int i=1;i<N;++i){
    ind=arma::linspace<arma::uvec>(i*M,(i+1)*M-1,M);
    ind_f(i)=ind;
    arma::vec pp2=arma::linspace<arma::colvec>( l(i), u(i), M );
    grid_search.elem(ind)= pp2 ;
    grid_search_list(i)=pp2;
  }
  arma::vec x_eval_tot= Rcpp :: as < arma :: vec >(eval_fd_c(grid_search,x_fd_std));
  arma::vec der_x_eval_tot=Rcpp :: as < arma :: vec >(eval_fd_c(grid_search,der_x_fd_std));
  for (int i=1;i<N;++i){
    ind2=ind_f(i);
    pp2=grid_search.elem(ind2);
    x_eval_i= x_eval_tot.elem(ind2);
    der_x_eval_i=der_x_eval_tot.elem(ind2);
    grid_search_i=pp2;
    indm1=ind_f(i-1);
    pp3=grid_search.elem(indm1);
    grid_search_im1=pp3;
    for (int j=start_jj(i);j<M;++j){
      grid_search2=(grid_search_im1).elem(arma::find(grid_search_im1<=grid_search_i(j)&&grid_search_im1>=(smax+pow(10,-10))*grid_t(i-1)+grid_search_i(j)-(smax+pow(10,-10))*grid_t(i)));
      grid_2_list[j]=grid_search2;
      start_ll_r=arma::conv_to<arma::vec>::from(arma::find(grid_search_im1==grid_search2(0)));
      end_ll_r=arma::conv_to<arma::vec>::from(arma::find(grid_search_im1==grid_search2(grid_search2.n_rows-1)));
      start_ll=start_ll_r(0);
      end_ll=end_ll_r(0);
      arma::vec cost_ij(end_ll -start_ll+1),l_ij(end_ll -start_ll+1);
      for (int l=start_ll;l<end_ll+1;++l){
        derr=((grid_search_i(j)-grid_search2(l-start_ll))/(grid_t(i)-grid_t(i-1))*pow(der_0,-1));
        cost_ij(l-start_ll)=(delta_t*(loss_c(x_eval_i(j),template_eval(i),der_x_eval_i(j),der_template_eval(i),alpha,derr))+lambda*delta_t*der_c((grid_search_i(j)-grid_search2(l-start_ll))/(grid_t(i)-grid_t(i-1)),smin,smax,der_0)+D(i-1,l));
        l_ij(l-start_ll)=L(i-1,l)+delta_t;
      }
      cost_ij.replace(arma::datum::nan, arma::datum::inf);
      div=cost_ij/l_ij;
      // min_ij=min(div);
      ind_min=div.index_min();
      D(i,j)=cost_ij(ind_min);
      P(i,j)=grid_search2(ind_min);
      L(i,j)=l_ij(ind_min);
    }
  }
  List out=List::create(grid_search_list,D,P,L);
  return(out);
}


// [[Rcpp::export]]
arma::field<arma::mat> get_path_list1(int N,int M,arma::mat range_x,arma::mat range_tem,arma::vec grid_t,arma::vec ind_end1, List grid_search,arma::mat P ){

  arma::field<arma::mat> path_list1(ind_end1.n_rows);
  arma::mat ind(N,2);
  ind.zeros();
  int ind_ind,i,ind_p;
  double P_i,grid_t2,im12;
  arma::vec aa2, aa3,grid_im1,cc,ind_p_r;
  arma::mat aa;
  for (int j=0;j< static_cast<int>(ind_end1.n_rows);++j){
    cc={grid_t(ind_end1(j)-1),range_x(1)};
    ind.row(0)=trans(cc);
    P_i =P(ind_end1(j)-1,M-1);
    arma::vec grid_im1=grid_search(ind_end1(j)-2);
    ind_p_r=arma::conv_to<arma::vec>::from(arma::find(grid_im1==P_i));
    ind_p=ind_p_r(0);
    im12=grid_im1(ind_p);
    grid_t2=grid_t(ind_end1(j)-2);
    cc={grid_t2,im12};
    ind.row(1)=trans(cc);
    i=1;
    ind_ind=i;
    while ((ind(i,0)!=range_tem(0))&&(ind(i,1)!=range_x(0))) {
      i++;

      P_i =P(ind_end1(j)-2-i+2,ind_p);
      arma::vec grid_im1=grid_search(ind_end1(j)-2-i+1);
      ind_p_r=arma::conv_to<arma::vec>::from(arma::find(grid_im1==P_i));
      ind_p=ind_p_r(0);
      im12=grid_im1(ind_p);
      grid_t2=grid_t(ind_end1(j)-2-i+1);
      cc={grid_t2,im12};
      ind.row(i)=trans(cc);
      ind_ind=i;
    }
    aa =ind.rows(0,ind_ind);
    aa2=aa.col(0);
    aa3=aa.col(1);
    aa2=reverse(aa2);
    aa3=reverse(aa3);
    path_list1(j)=join_horiz(aa2,aa3);

  }

  return(path_list1);

}
// [[Rcpp::export]]
arma::field<arma::mat> get_path_list2(int N,int M,arma::mat range_x,arma::mat range_tem,arma::vec grid_t,arma::vec ind_end2, List grid_search,arma::mat P ){

  arma::field<arma::mat> path_list2(ind_end2.n_rows);
  arma::mat ind(N,2);
  ind.zeros();
  int ind_ind,i,ind_p;
  double P_i,grid_t2,im12;
  arma::vec ind_p_r,aa2, aa3,grid_im1,cc;
  arma::mat aa;
  for (int j=0;j<static_cast<int>(ind_end2.n_rows);++j){
    arma::vec grid_im11=grid_search(N-1);
    cc={grid_t(N-1),grid_im11(ind_end2(j)-1)};;
    ind.row(0)=trans(cc);
    P_i =P(N-1,ind_end2(j)-1);
    arma::vec grid_im1=grid_search(N-2);
    ind_p_r=arma::conv_to<arma::vec>::from(arma::find(grid_im1==P_i));
    ind_p=ind_p_r(0);
    im12=grid_im1(ind_p);
    grid_t2=grid_t(N-2);
    cc={grid_t2,im12};
    ind.row(1)=trans(cc);
    i=1;
    ind_ind=i;
    while ((ind(i,0)!=range_tem(0))&&(ind(i,1)!=range_x(0))) {
      i++;
      P_i =P(N-i,ind_p);
      arma::vec grid_im1=grid_search(N-1-i);
      ind_p_r=arma::conv_to<arma::vec>::from(arma::find(grid_im1==P_i));
      ind_p=ind_p_r(0);
      im12=grid_im1(ind_p);
      grid_t2=grid_t(N-1-i);
      cc={grid_t2,im12};
      ind.row(i)=trans(cc);
      ind_ind=i;
    }

    aa =ind.rows(0,ind_ind);
    aa2=aa.col(0);
    aa3=aa.col(1);
    aa2=reverse(aa2);
    aa3=reverse(aa3);
    path_list2(j)=join_horiz(aa2,aa3);
  }
  return(path_list2);
}
// // [[Rcpp::export]]
// List DP2(int N,int M,arma::vec l, arma::vec u,arma::mat range_x,arma::mat range_tem,arma::mat grid_t, List x_fd_std,List der_x_fd_std,
//          double delta_x1,arma::vec template_eval,arma::vec der_template_eval,
//          double smin,double smax,double alpha,double lambda,
//          arma::vec grid_x_new, Rcpp::Function eval_fd_c){
//   // Rcpp::Environment base("package:fda");
//   // Rcpp::Function eval_fd_c = base["eval.fd"];
//   arma::vec x_eval_tot= Rcpp :: as < arma :: colvec >(eval_fd_c(grid_x_new,x_fd_std));
//   arma::vec der_x_eval_tot=Rcpp :: as < arma :: colvec >(eval_fd_c(grid_x_new,der_x_fd_std));
//   arma::vec x_eval,der_x_eval;
//   arma::field<arma::vec> D(N), P(N), L(N), grid_search(N);
//   for (int i=0;i<N;++i){
//
//     arma::vec zeros3(arma::size(arma::find(grid_x_new<=u(i)&&grid_x_new>=l(i))));
//     zeros3.zeros();
//     L(i)=zeros3;
//     zeros3.replace( 0,arma::datum::inf);
//     D(i)=zeros3;
//     P(i)=zeros3;
//   }
//   double delta_t=grid_t(1)-grid_t(0);
//   double der_0=arma::conv_to <double>::from(arma::range(range_x)/arma::range(range_tem));
//   if (u(0)!=range_x(0)){
//     grid_search(0)=arma::linspace( l(0), u(0), D(0).n_rows);
//   }   else{
//     grid_search(0)(0)=0;
//   }
//   int sta=min(find(grid_t<=(range_tem(0)+delta_x1)));
//   int end=max(find(grid_t<=(range_tem(0)+delta_x1)));
//   if(l(0)==range_x(0)){
//     for (int i=sta;i<end+1;++i){
//       D[i](0)=0;
//       L[i](0)=0;
//     }
//   }
//   arma::vec zeros2(arma::size(arma::find(grid_x_new<=u(0)&&grid_x_new>=l(0))));
//   zeros2.zeros();
//   if(u(0)!=range_x(0)){
//     D[0]=zeros2;
//   }
//   L(0)=zeros2;
//
//   arma::vec start_jj= arma::conv_to <arma::vec>::from(l==range_x(0));
//   // double min_ij;
//   int ind_min;
//   double derr;
//   arma::vec grid_search_i;
//   arma::vec  grid_search2;
//   for (int i=1;i<N;++i){
//     arma::umat ind_ii_x=arma::find(grid_x_new<=u(i)&&grid_x_new>=l(i));
//     grid_search_i=(grid_x_new).elem(ind_ii_x);
//     grid_search[i]=grid_search_i;
//     x_eval=x_eval_tot.elem(ind_ii_x);
//     der_x_eval=der_x_eval_tot.elem(ind_ii_x);
//     arma::vec grid_search_im1= grid_search[i-1];
//     for (int j=start_jj(i);j<static_cast<int>(grid_search_i.n_rows);++j){
//       grid_search2=(grid_search_im1).elem(arma::find(grid_search_im1<=grid_search_i(j)));
//       arma::vec cost_ij(grid_search2.n_rows),l_ij(grid_search2.n_rows);
//       for (int l=0;l<static_cast<int>(grid_search2.n_rows);++l){
//         derr=((grid_search_i(j)-grid_search2(l))/(grid_t(i)-grid_t(i-1))*pow(der_0,-1));
//         cost_ij(l)=(delta_t*(loss_c(x_eval(j),template_eval(i),der_x_eval(j),der_template_eval(i),alpha,derr))+lambda*delta_t*der_c((grid_search_i(j)-grid_search2(l))/(grid_t(i)-grid_t(i-1)),smin,smax,der_0)+D[i-1](l));
//         l_ij(l)=L[i-1](l)+delta_t;
//
//       }
//       arma::vec div=cost_ij/l_ij;
//       // min_ij=min(div);
//       ind_min=div.index_min();
//       D[i](j)=cost_ij(ind_min);
//       P[i](j)=grid_search2(ind_min);
//       L[i](j)=l_ij(ind_min);
//
//     }
//
//   }
//   List out=List::create(grid_search,D,P,L,grid_search2);
//   return(out);
// }


// Functions for MFRCC

// [[Rcpp::export]]
Rcpp::List computeSigma(
    const arma::mat& x,
    const arma::mat& y,
    const Rcpp::List& B,
    const arma::mat& z,
    const std::string& model_Sigma,
    int& sing,
    int p,
    int k,
    int n
) {
  Rcpp::List Sigma(k + 1);
  sing = 0;

  if (model_Sigma == "EEE") {
    arma::mat s = arma::zeros(p, p);

    for (int kk = 0; kk < k; ++kk) {
      arma::mat Bk = Rcpp::as<arma::mat>(B[kk]);
      arma::mat yhat = x * Bk;
      arma::mat res = y - yhat;
      s += res.t() * (res.each_col() % z.col(kk));

    }
    s /= n;
    // s = (s.t()+ s) / 2;

    if (arma::cond(s) < 3e-17) {
      sing = 1;
      s.fill(arma::datum::nan);
      Rcpp::Rcout << "\nrcond(Sigma is less than 3e-17!\n";
    }

    for (int kk = 0; kk < k; ++kk) {
      Sigma[kk] = s;
    }

  } else if (model_Sigma == "VVV") {
    for (int kk = 0; kk < k; ++kk) {
      arma::mat Bk = Rcpp::as<arma::mat>(B[kk]);
      arma::mat yhat = x * Bk;
      arma::mat res = y - yhat;
      arma::mat ss = res.t() * (res.each_col() % z.col(kk)) / arma::accu(z.col(kk));

      if (arma::cond(ss) < 3e-17) {
        sing = 1;
        ss.fill(arma::datum::nan);
        Rcpp::Rcout << "\nrcond(Sigma is less than 3e-17!\n";

      }
      Sigma[kk] = ss;
    }

  } else if (model_Sigma == "VII") {
    for (int kk = 0; kk < k; ++kk) {
      arma::mat Bk = Rcpp::as<arma::mat>(B[kk]);
      arma::mat yhat = x * Bk;
      arma::mat res = y - yhat;
      arma::mat ss = arma::trace((res.t() * (res.each_col() % z.col(kk))) / (arma::accu(z.col(kk)) * p)) * arma::eye(p, p);

      if (arma::cond(ss) < 3e-17) {
        sing = 1;
        ss.fill(arma::datum::nan);
        Rcpp::Rcout << "\nrcond(Sigma is less than 3e-17!\n";

      }
      Sigma[kk] = ss;
    }

  } else if (model_Sigma == "EII") {
    arma::mat s = arma::zeros(p, p);

    for (int kk = 0; kk < k; ++kk) {
      arma::mat Bk = Rcpp::as<arma::mat>(B[kk]);
      arma::mat yhat = x * Bk;
      arma::mat res = y - yhat;

      s += res.t() * (res.each_col() % z.col(kk));
    }
    s = arma::trace(s) / (n * p) * arma::eye(p, p);

    if (arma::cond(s) < 3e-17) {
      sing = 1;
      s.fill(arma::datum::nan);
      Rcpp::Rcout << "\nrcond(Sigma is less than 3e-17!\n";
    }

    for (int kk = 0; kk < k; ++kk) {
      Sigma[kk] = s;
    }
  }
  Sigma[k] = sing;
  return Sigma;
}


// [[Rcpp::export]]
arma::vec dmvn(const arma::mat& X, const arma::rowvec& mu, const arma::mat& sigma) {
  int n = X.n_rows;
  int d = X.n_cols;
  arma::vec result(n);

  // Precompute constants
  double logDetSigma;
  double sign;
  arma::log_det(logDetSigma, sign, sigma); // Log determinant and sign
  arma::mat sigmaInv = arma::inv_sympd(sigma); // Inverse of sigma

  double log2pi = std::log(2.0 * M_PI);

  for (int i = 0; i < n; ++i) {
    arma::rowvec diff = X.row(i) - mu;
    double quadForm = arma::as_scalar(diff * sigmaInv * diff.t());
    result[i] = -0.5 * (d * log2pi + logDetSigma + quadForm);
  }

  // Convert to density
  return arma::exp(result);
}

// [[Rcpp::export]]
std::vector<arma::vec> computeComp(
    const arma::mat& x,
    const arma::mat& y,
    const Rcpp::List& B,
    const Rcpp::List& Sigma,
    const arma::vec& prop
) {
  int k = B.size();
  int n = y.n_rows;

  // Result container
  std::vector<arma::vec> comp(k);

  for (int i = 0; i < k; ++i) {
    arma::mat Bk = Rcpp::as<arma::mat>(B[i]);        // Coefficients matrix for component i
    arma::mat yhat = x * Bk;                        // Predicted values
    arma::mat Sigma_k = Rcpp::as<arma::mat>(Sigma[i]); // Covariance matrix for component i

    // Compute densities
    arma::vec lk(n);
    for (int j = 0; j < n; ++j) {
      lk[j] = dmvn(y.row(j), yhat.row(j), Sigma_k)[0];
    }

    // Scale by proportion
    comp[i] = prop[i] * lk;
  }

  return comp;
}

