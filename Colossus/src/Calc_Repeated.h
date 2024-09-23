using namespace std;
using namespace Rcpp;

using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::SparseMatrix;
using Eigen::VectorXd;
using Rcpp::as;

template <typename T> int sign(T val);

void removeRow(Eigen::MatrixXd& matrix, unsigned int rowToRemove);

void removeColumn(Eigen::MatrixXd& matrix, unsigned int colToRemove);

void Make_Groups(const int& ntime, const MatrixXd& df_m, IntegerMatrix& RiskFail, vector<string>& RiskGroup, NumericVector& tu, const int& nthreads, bool debugging );

void Make_Groups_CR(const int& ntime, const MatrixXd& df_m, IntegerMatrix& RiskFail, vector<string>&  RiskGroup,  NumericVector& tu, const VectorXd& cens_weight, const int& nthreads, bool debugging );

void Make_Groups_STRATA(const int& ntime, const MatrixXd& df_m, IntegerMatrix& RiskFail, StringMatrix&  RiskGroup,  NumericVector& tu, const int& nthreads, bool debugging, NumericVector& STRATA_vals);

void Make_Groups_STRATA_CR(const int& ntime, const MatrixXd& df_m, IntegerMatrix& RiskFail, StringMatrix&  RiskGroup,  NumericVector& tu, const int& nthreads, bool debugging, NumericVector& STRATA_vals, const VectorXd& cens_weight);

void Calculate_Sides(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3,const int& nthreads, bool debugging, const IntegerVector& KeepConstant);

void Calculate_Sides_CR(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3, const VectorXd& cens_weight,const int& nthreads, bool debugging, const IntegerVector& KeepConstant);

void Calculate_Sides_CR_SINGLE(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1, const VectorXd& cens_weight,const int& nthreads, bool debugging, const IntegerVector& KeepConstant);

void Calculate_Sides_Single(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1,const int& nthreads, bool debugging);

void Calculate_Sides_STRATA(const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3,const int& nthreads, bool debugging, NumericVector& STRATA_vals, const IntegerVector& KeepConstant);

void Calculate_Sides_STRATA_Single(const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1,const int& nthreads, bool debugging, NumericVector& STRATA_vals, const IntegerVector& KeepConstant);

void Calculate_Sides_STRATA_CR(const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3, const VectorXd& cens_weight,const int& nthreads, bool debugging, NumericVector& STRATA_vals, const IntegerVector& KeepConstant);

void Calculate_Sides_STRATA_Single_CR(const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1, const VectorXd& cens_weight,const int& nthreads, bool debugging, NumericVector& STRATA_vals, const IntegerVector& KeepConstant);

void Calc_LogLik(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR, const MatrixXd& RddR,const MatrixXd& Rls1,const MatrixXd& Rls2,const MatrixXd& Rls3,const MatrixXd& Lls1,const MatrixXd& Lls2,const MatrixXd& Lls3, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging,string ties_method, const IntegerVector& KeepConstant);

void Calc_LogLik_Single(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R,const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, bool debugging,string ties_method);

void Calc_LogLik_Basic_Single(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, bool debugging,string ties_method, const IntegerVector& KeepConstant);

void Calc_LogLik_STRATA(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR, const MatrixXd& RddR,const MatrixXd& Rls1,const MatrixXd& Rls2,const MatrixXd& Rls3,const MatrixXd& Lls1,const MatrixXd& Lls2,const MatrixXd& Lls3, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging,string ties_method, NumericVector& STRATA_vals, const IntegerVector& KeepConstant);

void Calc_LogLik_STRATA_SINGLE(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R,const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, bool debugging,string ties_method, NumericVector& STRATA_vals, const IntegerVector& KeepConstant);

void Calc_LogLik_STRATA_BASIC(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR,const MatrixXd& Rls1,const MatrixXd& Rls2,const MatrixXd& Rls3,const MatrixXd& Lls1,const MatrixXd& Lls2,const MatrixXd& Lls3, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging,string ties_method, NumericVector& STRATA_vals, const IntegerVector& KeepConstant);

void Calc_LogLik_STRATA_BASIC_SINGLE(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R ,const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, bool debugging,string ties_method, NumericVector& STRATA_vals, const IntegerVector& KeepConstant);

void Calc_LogLik_Basic(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR,const MatrixXd& Rls1,const MatrixXd& Rls2,const MatrixXd& Rls3,const MatrixXd& Lls1,const MatrixXd& Lls2,const MatrixXd& Lls3, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging,string ties_method, const IntegerVector& KeepConstant);

void Poisson_LogLik(const int& nthreads, const int& totalnum, const MatrixXd& PyrC, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR, const MatrixXd& RddR, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging, const IntegerVector& KeepConstant);

void Poisson_LogLik_Single(const int& nthreads, const int& totalnum, const MatrixXd& PyrC, const MatrixXd& R, vector<double>& Ll, bool debugging);

void Calculate_Null_Sides(const IntegerMatrix& RiskFail, const vector<string>& RiskGroup, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1,const int& nthreads);

void Calc_Null_LogLik(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>& RiskGroup, const int& ntime, const MatrixXd& R, const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, string ties_method);

void Calculate_Null_Sides_STRATA(const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1, NumericVector& STRATA_vals,const int& nthreads);

void Calc_Null_LogLik_STRATA(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& ntime, const MatrixXd& R, const MatrixXd& Rls1,const MatrixXd& Lls1, NumericVector& STRATA_vals, vector<double>& Ll, string ties_method);
