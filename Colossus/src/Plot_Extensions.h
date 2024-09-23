using namespace std;
using namespace Rcpp;

using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::SparseMatrix;
using Eigen::VectorXd;
using Rcpp::as;

List PLOT_SURV(int reqrdnum, MatrixXd& R, MatrixXd& Rd, NumericVector& a_er, NumericMatrix& df_groups, NumericVector& tu , int verbose, bool debugging, int nthreads);

List PLOT_SURV_STRATA(int reqrdnum, MatrixXd& R, MatrixXd& Rd, NumericVector& a_er, NumericMatrix& df_groups, NumericVector& tu, NumericVector& STRATA_vals , int verbose, bool debugging, int nthreads);

List Schoenfeld_Calc( int ntime, int totalnum, const  VectorXd& beta_0, const  MatrixXd& df0, const MatrixXd& R, MatrixXd& Lldd_inv, const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup,IntegerVector& dfc, int verbose, bool debugging, IntegerVector KeepConstant, int nthreads);

List Plot_Omnibus( IntegerVector term_n, StringVector tform, NumericVector a_n,NumericMatrix& x_all,IntegerVector dfc,int fir, int der_iden,string modelform, double abs_max,double dose_abs_max, NumericMatrix& df_groups, NumericVector& tu, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, string ties_method, int nthreads, NumericVector& STRATA_vals, const VectorXd& cens_weight,  int uniq_v, bool strata_bool, bool basic_bool, bool CR_bool, bool Surv_bool, bool Risk_bool, bool Schoenfeld_bool, bool Risk_Sub_bool, const double gmix_theta, const IntegerVector& gmix_term);

List Assign_Events_Pois( IntegerVector term_n, StringVector tform, NumericVector a_n,NumericMatrix& x_all,IntegerVector dfc,const MatrixXd& PyrC, const MatrixXd& dfs,int fir,string modelform, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, int nthreads, const double gmix_theta, const IntegerVector gmix_term, const bool strata_bool);

List Poisson_Residuals(const MatrixXd& PyrC, IntegerVector term_n, StringVector tform, NumericVector a_n,NumericMatrix& x_all,IntegerVector dfc,int fir, int der_iden,string modelform, double abs_max,double dose_abs_max, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, int nthreads, const MatrixXd& dfs, bool strata_bool, const double gmix_theta, const IntegerVector gmix_term, bool Pearson_bool, bool Deviance_bool);
