// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;

//fill in
mat btri_mat(const mat& all_matrices){
  const int p = all_matrices.n_cols;
  const int tp = all_matrices.n_rows;
  const int t = tp/p;
  //make container
  mat all_col = mat(tp, tp, fill::zeros);
  for (int i = 0; i < t; ++i){
    all_col.submat(i*p, i*p, tp - 1, (i + 1)*p - 1) = all_matrices.head_rows((t-i)*p);
  }
  return all_col;
}

//fill in
field<mat> make_A_df(const int& p, const int& t, const mat& dthetas, const mat& lws, const mat& iirws){
  //make containers
  field<mat> output = field<mat>(2);
  mat all_matrices = mat((t + 1)*p, p);
  //build the long block
  const mat diag_p = eye(p, p);
  const mat c_mat = iirws*(dthetas + lws);
  mat tmp_mat = diag_p - c_mat;
  //fill the containers
  all_matrices.rows(0, p - 1) = diag_p;
  all_matrices.rows(p, 2*p - 1) = c_mat - 2*diag_p;
  tmp_mat = tmp_mat * tmp_mat;
  all_matrices.rows(2*p, 3*p - 1) = tmp_mat;
  for (int i = 3; i < t + 1; ++i){
    tmp_mat = c_mat * tmp_mat;
    all_matrices.rows(i*p, (i+1)*p - 1) = tmp_mat;
  }
  const mat mat_proto = btri_mat(all_matrices);
  output(0) = mat_proto.submat(p, 0, (t + 1)*p - 1, t*p - 1);
  output(1) = mat_proto.submat(0, 0, t*p - 1, t*p - 1);
  return output;
}

mat merge_vec(const double& p1, const double& p2, const uvec& th, const int& len){
  vec res = ones(len)*p1;
  res.elem(th) = ones(th.n_elem)*p2;
  return diagmat(res);
}

// [[Rcpp::export]]
Rcpp::RObject msdpd_aqs(const arma::vec& para, 
                        const arma::mat& x_, 
                        const arma::vec& y,
                        const arma::vec& y1, 
                        const arma::mat& w, 
                        const arma::mat& inv_c, 
                        const bool& correction, 
                        const arma::mat& w_lam, 
                        const arma::mat& w_er){
  const int tp = y.size();
  const int p = w.n_cols;
  const int t = tp/p;
  const double rho = para(0);
  const double alp = para(1);
  const double theta = para(2);
  const double lam = para(3);
  // if (std::abs(alp) >= 1||std::abs(theta) + std::abs(rho) + std::abs(lam) >= 1){
  //   return Rcpp::wrap(1e9);
  // }
  const mat diag_p = eye(p, p);
  const mat diag_t = eye(t, t);
  const mat bdinv_c = kron(inv_c, diag_p);
  const mat iaws = diag_p - alp*w_er;
  const mat iaw = kron(diag_t, iaws);
  const mat iiaws = iaws.i();
  const mat iaaw = kron(inv_c, iaws.t()*iaws);
  const mat irws = diag_p - rho*w;
  const mat irw = kron(diag_t,  irws);
  const mat iirws = irws.i();
  const mat lws = lam*w_lam;
  const mat lw = kron(diag_t, lws);
  const vec beta = (x_.t()*iaaw*x_).i()*x_.t()*iaaw*(irw*y - theta*y1 - lw*y1);
  const vec k_ast = irw*y - x_*beta - theta*y1 - lw*y1;
  const double sigs = as_scalar(k_ast.t()*iaaw*k_ast)/tp;
  const mat iirw = kron(diag_t, iirws);
  const mat iiaw = kron(diag_t, iiaws);
  const mat bdw = kron(diag_t, w);
  const mat bdw_lam = kron(diag_t, w_lam);
  const mat bdw_er = kron(diag_t, w_er);
  const rowvec tmp_mat_1 = k_ast.t()*iaaw;
  vec eq = vec(4);
  if (correction) {
    field<mat> A_mats = make_A_df(p, t, theta*eye(p, p), lws, iirws);
    eq(0) = 1/sigs*as_scalar(tmp_mat_1*y1) + trace(bdinv_c*A_mats(1)*iirw);
    eq(1) = 1/sigs*as_scalar(tmp_mat_1*bdw*y) + trace(bdinv_c*A_mats(0)*iirw*bdw);
    eq(2) = 1/sigs*as_scalar(tmp_mat_1*bdw_lam*y1)+ trace(bdinv_c*A_mats(1)*iirw*bdw_lam);
    eq(3) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er.t()*iaws + iaws.t()*w_er)*k_ast) - t * trace(iiaws*w_er);
  } else {
    eq(0) = 1/sigs*as_scalar(tmp_mat_1*y1);
    eq(1) = 1/sigs*as_scalar(tmp_mat_1*bdw*y) - t * trace(iirws*w);
    eq(2) = 1/sigs*as_scalar(tmp_mat_1*bdw_lam*y1);
    eq(3) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er.t()*iaws + iaws.t()*w_er)*k_ast) - t * trace(iiaws*w_er);
  }
  return Rcpp::wrap(sum(square(eq)));
}

// [[Rcpp::export]]
Rcpp::RObject msdpdth_aqs(const arma::vec& para, 
                          const arma::mat& x_,
                          const arma::vec& y,
                          const arma::vec& y1, 
                          const arma::mat& w, 
                          const arma::mat& w_lam,
                          const arma::mat& w_er,
                          const arma::vec& th_e, 
                          const arma::mat& inv_c, 
                          const bool& correction,
                          const int& th_type){
  const int tp = y.n_elem;
  const int p = w.n_cols;
  const int t = tp/p;
  const vec th = th_e.head(p);
  const uvec th_1 = find(th == 1);
  const uvec th_2 = find(th == 0);
  const uvec th_e1 = find(th_e == 1);
  const uvec th_e2 = find(th_e == 0);
  const double rho1 = para(0);
  const double alp1 = para(1);
  const double theta1 = para(2);
  const double lam1 = para(3);
  const double rho2 = para(4);
  const double alp2 = para(5);
  const double theta2 = para(6);
  const double lam2 = para(7);
  const mat diag_p = eye(p, p);
  const mat diag_t = eye(t, t);
  const mat bdinv_c = kron(inv_c, diag_p);
  const mat bdw = kron(diag_t, w);
  const mat bdw_er = kron(diag_t, w_er);
  const mat bdw_lam = kron(diag_t, w_lam);
  vec y_q1 = y, y_q2 = y;
  vec y1_q1 = y1, y1_q2 = y1;
  mat bdw0_q1 = eye(tp,tp), bdw0_q2 = eye(tp,tp);
  mat bdw_q1 = bdw, bdw_q2 = bdw;
  // mat bdw_er_q1 = bdw, bdw_er_q2 = bdw;
  mat bdw_lam_q1 = bdw_lam, bdw_lam_q2 = bdw_lam;
  mat w_q1 = w, w_q2 = w;
  mat w_er_q1 = w_er, w_er_q2 = w_er;
  // mat w_lam_q1 = w_lam, w_lam_q2 = w_lam;
  y_q1.elem(th_e2).fill(0);
  y_q2.elem(th_e1).fill(0);
  y1_q1.elem(th_e2).fill(0);
  y1_q2.elem(th_e1).fill(0);
  mat iaws, irws, lws;
  switch(th_type){
  case 1: {
    w_q1.rows(th_2).fill(0);
    w_q2.rows(th_1).fill(0);
    w_er_q1.rows(th_2).fill(0);
    w_er_q2.rows(th_1).fill(0);
    bdw0_q1.rows(th_e2).fill(0);
    bdw0_q2.rows(th_e1).fill(0);
    bdw_q1.rows(th_e2).fill(0);
    bdw_q2.rows(th_e1).fill(0);
    bdw_lam_q1.rows(th_e2).fill(0);
    bdw_lam_q2.rows(th_e1).fill(0);
    iaws = diag_p - merge_vec(alp1, alp2, th_2, p)*w_er;
    irws = diag_p - merge_vec(rho1, rho2, th_2, p)*w;
    lws = merge_vec(lam1, lam2, th_2, p)*w_lam;
    break;
  }
  case 2: {
    w_q1.cols(th_2).fill(0);
    w_q2.cols(th_1).fill(0);
    w_er_q1.cols(th_2).fill(0);
    w_er_q2.cols(th_1).fill(0);
    bdw0_q1.cols(th_e2).fill(0);
    bdw0_q2.cols(th_e1).fill(0);
    bdw_q1.cols(th_e2).fill(0);
    bdw_q2.cols(th_e1).fill(0);
    bdw_lam_q1.cols(th_e2).fill(0);
    bdw_lam_q2.cols(th_e1).fill(0);
    iaws = diag_p - w_er*merge_vec(alp1, alp2, th_2, p);
    irws = diag_p - w*merge_vec(rho1, rho2, th_2, p);
    lws = w_lam*merge_vec(lam1, lam2, th_2, p);
    break;
  }
  }
  const mat iaw = kron(diag_t, iaws);
  const mat iiaws = iaws.i();
  const mat iaaw = kron(inv_c, iaws.t()*iaws);
  const mat irw = kron(diag_t, irws);
  const mat iirws = irws.i();
  const mat lw = kron(diag_t, lws);
  const mat dthetas = merge_vec(theta1, theta2, th_2, p);
  const mat dtheta = kron(diag_t, dthetas);
  const vec beta = (x_.t()*iaaw*x_).i()*x_.t()*iaaw*(irw*y - (dtheta + lw)*y1);
  const vec k_ast = irw*y - x_*beta - (dtheta + lw)*y1;
  const double sigs = as_scalar(k_ast.t()*iaaw*k_ast)/tp;
  const mat iirw = kron(diag_t, iirws);
  const mat iiaw = kron(diag_t, iiaws);
  const mat tmp_mat_1 = k_ast.t()*iaaw;
  vec eq = vec(8);
  if (correction) {
    field<mat> A_mats = make_A_df(p, t, dthetas, lws, iirws);
    // const vec bias_theta = diagvec(bdinv_c*A_mats(1)*iirw);
    // const vec bias_rho = diagvec(bdinv_c*A_mats(0)*iirw*bdw);
    // const vec bias_lam = diagvec(bdinv_c*A_mats(1)*iirw*bdw_lam);
    // const vec diag_alp = diagvec(iiaws*w_er);
    // eq(0) = 1/sigs*as_scalar(tmp_mat_1*y1_q1) + sum(bias_theta.elem(th_e1));
    // eq(1) = 1/sigs*as_scalar(tmp_mat_1*bdw_q1*y) + sum(bias_rho.elem(th_e1));
    // eq(2) = 1/sigs*as_scalar(tmp_mat_1*bdw_lam_q1*y1)+ sum(bias_lam.elem(th_e1));
    eq(0) = 1/sigs*as_scalar(tmp_mat_1*y1_q1) + trace(bdinv_c*A_mats(1)*iirw*bdw0_q1);
    eq(1) = 1/sigs*as_scalar(tmp_mat_1*bdw_q1*y) + trace(bdinv_c*A_mats(0)*iirw*bdw_q1);
    eq(2) = 1/sigs*as_scalar(tmp_mat_1*bdw_lam_q1*y1)+ trace(bdinv_c*A_mats(1)*iirw*bdw_lam_q1);
    // eq(3) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er_q1.t()*iaws + iaws.t()*w_er_q1)*k_ast) - t * sum(diag_alp.elem(th_1));
    eq(3) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er_q1.t()*iaws + iaws.t()*w_er_q1)*k_ast) - t * trace(iiaws*w_er_q1);
    // eq(4) = 1/sigs*as_scalar(tmp_mat_1*y1_q2) + sum(bias_theta.elem(th_e2));
    // eq(5) = 1/sigs*as_scalar(tmp_mat_1*bdw_q2*y) + sum(bias_rho.elem(th_e2));
    // eq(6) = 1/sigs*as_scalar(tmp_mat_1*bdw_lam_q2*y1)+ sum(bias_lam.elem(th_e2));
    eq(4) = 1/sigs*as_scalar(tmp_mat_1*y1_q2) + trace(bdinv_c*A_mats(1)*iirw*bdw0_q2);
    eq(5) = 1/sigs*as_scalar(tmp_mat_1*bdw_q2*y) + trace(bdinv_c*A_mats(0)*iirw*bdw_q2);
    eq(6) = 1/sigs*as_scalar(tmp_mat_1*bdw_lam_q2*y1)+ trace(bdinv_c*A_mats(1)*iirw*bdw_lam_q2);
    // eq(7) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er_q2.t()*iaws + iaws.t()*w_er_q2)*k_ast) - t * sum(diag_alp.elem(th_2));
    eq(7) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er_q2.t()*iaws + iaws.t()*w_er_q2)*k_ast) - t * trace(iiaws*w_er_q2);
  } else {
    // const vec diag_rho = diagvec(iirws*w);
    // const vec diag_alp = diagvec(iiaws*w);
    eq(0) = 1/sigs*as_scalar(tmp_mat_1*y1_q1);
    eq(1) = 1/sigs*as_scalar(tmp_mat_1*bdw_q1*y) - t * trace(iirws*w_q1);
    eq(2) = 1/sigs*as_scalar(tmp_mat_1*bdw_lam_q1*y1);
    // eq(3) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er_q1.t()*iaws + iaws.t()*w_er_q1)*k_ast) - t * sum(diag_alp.elem(th_1));
    eq(3) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er_q1.t()*iaws + iaws.t()*w_er_q1)*k_ast) - t * trace(iiaws*w_er_q1);
    eq(4) = 1/sigs*as_scalar(tmp_mat_1*y1_q2);
    eq(5) = 1/sigs*as_scalar(tmp_mat_1*bdw_q2*y) - t * trace(iirws*w_q2);
    eq(6) = 1/sigs*as_scalar(tmp_mat_1*bdw_lam_q2*y1);
    // eq(7) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er_q2.t()*iaws + iaws.t()*w_er_q2)*k_ast) - t * sum(diag_alp.elem(th_2));
    eq(7) = 0.5/sigs*as_scalar(k_ast.t()*kron(inv_c, w_er_q2.t()*iaws + iaws.t()*w_er_q2)*k_ast) - t * trace(iiaws*w_er_q2);
  }
  return Rcpp::wrap(sum(square(eq)));
}
