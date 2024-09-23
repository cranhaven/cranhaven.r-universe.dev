using namespace std;
using namespace Rcpp;

using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::SparseMatrix;
using Eigen::VectorXd;
using Rcpp::as;

template <typename T> int sign(T val);

void Make_subterms(const int& totalnum, const IntegerVector& term_n,const StringVector&  tform, const IntegerVector& dfc, const int& fir, MatrixXd& T0, MatrixXd& Td0, MatrixXd& Tdd0, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm, MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN,const  VectorXd& beta_0,const  MatrixXd& df0,const double& dint, const double& dslp, const int& nthreads, bool debugging, const IntegerVector& KeepConstant);

void Make_subterms_Single(const int& totalnum, const IntegerVector& term_n,const StringVector&  tform, const IntegerVector& dfc, const int& fir, MatrixXd& T0, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm, MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN,const  VectorXd& beta_0,const  MatrixXd& df0, const int& nthreads, bool debugging, const IntegerVector& KeepConstant);

void Make_subterms_Basic(const int& totalnum, const IntegerVector& dfc, MatrixXd& T0, const VectorXd& beta_0,const MatrixXd& df0, const int& nthreads, bool debugging);

void Make_Risks(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const MatrixXd& T0, const MatrixXd& Td0, const MatrixXd& Tdd0, MatrixXd& Te, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, MatrixXd& RdR, MatrixXd& RddR, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, const double gmix_theta, const IntegerVector& gmix_term);

void Make_Risks_Weighted(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const MatrixXd& s_weights, const MatrixXd& T0, const MatrixXd& Td0, const MatrixXd& Tdd0, MatrixXd& Te, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, MatrixXd& RdR, MatrixXd& RddR, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, const double gmix_theta, const IntegerVector& gmix_term);

void Make_Risks_Weighted_Single(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const MatrixXd& s_weights, const MatrixXd& T0, MatrixXd& Te, MatrixXd& R, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, const double gmix_theta, const IntegerVector& gmix_term);

void Make_Risks_Single(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const MatrixXd& T0, MatrixXd& Te, MatrixXd& R, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, const double gmix_theta, const IntegerVector& gmix_term);

void Make_Risks_Basic(const int& totalnum, const MatrixXd& T0, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& RdR, const int& nthreads, bool debugging,const MatrixXd& df0, const IntegerVector& dfc, const IntegerVector& KeepConstant);
