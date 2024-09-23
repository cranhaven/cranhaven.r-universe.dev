#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include "R_Interface.h"
#include "Main_Functions.h"
#include "Plot_Extensions.h"
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <chrono>
#include <random>
#include <ctime>
#include <Eigen/Core>


// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(openmp)]]
using namespace std;
using namespace Rcpp;
using namespace Eigen;
using namespace std::chrono;

using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::SparseMatrix;
using Eigen::VectorXd;
using Rcpp::as;


template <typename T> int sign(T val) {
    return (T(0) < val) - (val < T(0));
}

template<typename Func>
struct lambda_as_visitor_wrapper : Func {
    lambda_as_visitor_wrapper(const Func& f) : Func(f) {}
    template<typename S, typename I>
    void init(const S& v, I i, I j) { return Func::operator()(v, i, j); }
};

template<typename Mat, typename Func>
void visit_lambda(const Mat& m, const Func& f)
{
    lambda_as_visitor_wrapper<Func> visitor(f);
    m.visit(visitor);
}

//' Interface between R code and the Cox PH omnibus regression function
//'
//' \code{cox_ph_Omnibus_transition} Called directly from R, Defines the control variables and calls the regression function
//' @inheritParams CPP_template
//'
//' @return LogLik_Cox_PH output : Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List cox_ph_Omnibus_transition(IntegerVector term_n, StringVector tform, NumericMatrix& a_ns,IntegerVector dfc,NumericMatrix& x_all, int fir, int der_iden,string modelform, List Control, NumericMatrix df_groups, NumericVector tu, IntegerVector KeepConstant, int term_tot, NumericVector STRATA_vals, NumericVector cens_vec, List model_control, NumericMatrix Cons_Mat, NumericVector Cons_Vec){
    bool change_all = Control["change_all"];
    int double_step = Control["double_step"];
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    double lr = Control["lr"];
    NumericVector maxiters = Control["maxiters"];
    int guesses = Control["guesses"];
    int halfmax = Control["halfmax"];
    double epsilon = Control["epsilon"];
    //
    double abs_max = Control["abs_max"];
    double dose_abs_max = Control["dose_abs_max"];
    double deriv_epsilon =Control["deriv_epsilon"];
    string ties_method =Control["ties"];
    int nthreads = Control["ncores"];
    //
	const Map<VectorXd> cens_weight(as<Map<VectorXd> >(cens_vec));
	//
	double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
	//
	const Map<MatrixXd> Lin_Sys(as<Map<MatrixXd> >(Cons_Mat));
	const Map<VectorXd> Lin_Res(as<Map<VectorXd> >(Cons_Vec));
	//
	bool strata_bool = model_control["strata"];
	bool basic_bool  = model_control["basic"];
	bool null_bool   = model_control["null"];
	bool CR_bool     = model_control["cr"];
	bool single_bool = model_control["single"];
	bool constraint_bool = model_control["constraint"];
    //
    // Performs regression
    //----------------------------------------------------------------------------------------------------------------//
    List res = LogLik_Cox_PH_Omnibus(term_n, tform, a_ns, x_all, dfc,fir, der_iden,modelform, lr, maxiters, guesses, halfmax, epsilon, abs_max,dose_abs_max, deriv_epsilon, df_groups, tu, double_step, change_all,verbose, debugging, KeepConstant, term_tot, ties_method, nthreads, STRATA_vals, cens_weight,  strata_bool, basic_bool, null_bool, CR_bool, single_bool, constraint_bool, gmix_theta, gmix_term, Lin_Sys, Lin_Res);
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}

//' Interface between R code and the poisson omnibus regression function
//'
//' \code{pois_Omnibus_transition} Called directly from R, Defines the control variables and calls the regression function
//' @inheritParams CPP_template
//'
//' @return LogLik_Cox_PH output : Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List pois_Omnibus_transition(NumericMatrix dfe, IntegerVector term_n, StringVector tform, NumericMatrix& a_ns,IntegerVector dfc,NumericMatrix& x_all, int fir, int der_iden,string modelform, List Control, IntegerVector KeepConstant, int term_tot, NumericMatrix df0, List model_control, NumericMatrix Cons_Mat, NumericVector Cons_Vec){
    //
    const Map<MatrixXd> PyrC(as<Map<MatrixXd> >(dfe));
    const Map<MatrixXd> dfs(as<Map<MatrixXd> >(df0));
    //
    bool change_all = Control["change_all"];
    int double_step = Control["double_step"];
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    double lr = Control["lr"];
    NumericVector maxiters = Control["maxiters"];
    int guesses = Control["guesses"];
    int halfmax = Control["halfmax"];
    double epsilon = Control["epsilon"];
    //
    double abs_max = Control["abs_max"];
    double dose_abs_max = Control["dose_abs_max"];
    double deriv_epsilon =Control["deriv_epsilon"];
    int nthreads = Control["ncores"];
    //
    double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
	//
	const Map<MatrixXd> Lin_Sys(as<Map<MatrixXd> >(Cons_Mat));
	const Map<VectorXd> Lin_Res(as<Map<VectorXd> >(Cons_Vec));
	//
	bool strata_bool = model_control["strata"];
	bool single_bool = model_control["single"];
	bool constraint_bool = model_control["constraint"];
    //
    // Performs regression
    //----------------------------------------------------------------------------------------------------------------//
    List res = LogLik_Pois_Omnibus(PyrC, term_n, tform, a_ns, x_all, dfc,fir, der_iden,modelform, lr, maxiters, guesses, halfmax, epsilon, abs_max,dose_abs_max, deriv_epsilon, double_step, change_all,verbose, debugging, KeepConstant, term_tot, nthreads, dfs, strata_bool, single_bool, constraint_bool, gmix_theta, gmix_term, Lin_Sys, Lin_Res);
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}

//' Interface between R code and the event assignment omnibus function
//'
//' \code{Assigned_Event_transition} Called directly from R, Defines the control variables and calls the assigning functions
//' @inheritParams CPP_template
//'
//' @return list of assigned/predicted background/excess events
//' @noRd
//'
// [[Rcpp::export]]
List Assigned_Event_Poisson_transition(NumericMatrix dfe, NumericMatrix df0,IntegerVector term_n, StringVector tform, NumericVector a_n,IntegerVector dfc,NumericMatrix& x_all, int fir, int der_iden,string modelform, List Control, IntegerVector KeepConstant, int term_tot, List model_control){
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    string ties_method =Control["ties"];
    bool strata_bool = model_control["strata"];
    int nthreads = Control["ncores"];
    //
	double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
	//
    const Map<MatrixXd> PyrC(as<Map<MatrixXd> >(dfe));
    const Map<MatrixXd> dfs(as<Map<MatrixXd> >(df0));
    //
    // Performs regression
    List res;
    //----------------------------------------------------------------------------------------------------------------//
    res = Assign_Events_Pois( term_n, tform, a_n, x_all, dfc, PyrC, dfs, fir, modelform, verbose, debugging, KeepConstant, term_tot, nthreads, gmix_theta, gmix_term, strata_bool);
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}

//' Interface between R code and the plotting omnibus function
//'
//' \code{Plot_Omnibus_transition} Called directly from R, Defines the control variables and calls the plotting functions
//' @inheritParams CPP_template
//'
//' @return LogLik_Cox_PH output : Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List Plot_Omnibus_transition(IntegerVector term_n, StringVector tform, NumericVector a_n,IntegerVector dfc,NumericMatrix& x_all, int fir, int der_iden,string modelform, List Control, NumericMatrix df_groups, NumericVector tu, IntegerVector KeepConstant, int term_tot, NumericVector STRATA_vals, NumericVector cens_vec, List model_control){
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    double abs_max = Control["abs_max"];
    double dose_abs_max = Control["dose_abs_max"];
    string ties_method =Control["ties"];
    int nthreads = Control["ncores"];
    //
	const Map<VectorXd> cens_weight(as<Map<VectorXd> >(cens_vec));
	//
	double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
	//
	bool strata_bool = model_control["strata"];
	bool basic_bool  = model_control["basic"];
	bool CR_bool     = model_control["cr"];
	//
	bool Surv_bool       = model_control["surv"];
	bool Schoenfeld_bool = model_control["schoenfeld"];
	bool Risk_bool       = model_control["risk"];
	bool Risk_Sub_bool   = model_control["risk_subset"];
	int uniq_v           = model_control["unique_values"];
    //
    // Performs regression
    List res;
    if (uniq_v < 2){
        res = List::create(_["Failed"]="Unique_Values too low, expects atleast 2 values");
        return res;
    }
    //----------------------------------------------------------------------------------------------------------------//
    res = Plot_Omnibus( term_n, tform, a_n,x_all,dfc, fir,  der_iden, modelform, abs_max,dose_abs_max, df_groups, tu, verbose, debugging, KeepConstant, term_tot, ties_method, nthreads, STRATA_vals, cens_weight,  uniq_v, strata_bool, basic_bool, CR_bool, Surv_bool, Risk_bool, Schoenfeld_bool, Risk_Sub_bool, gmix_theta, gmix_term);
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}

//' Generates csv file with time-dependent columns
//'
//' \code{Write_Time_Dep} Called directly from R, Defines a new matrix which interpolates time-dependent values on a grid
//' @inheritParams CPP_template
//'
//' @return saves a dataframe to be used with time-dependent covariate analysis
//' @noRd
//'
// [[Rcpp::export]]
void Write_Time_Dep(const NumericMatrix df0_Times, const NumericMatrix df0_dep, const NumericMatrix df0_const, const NumericVector df0_event,double dt, string filename, StringVector tform_tdep, NumericVector tu, bool iscox, int nthreads){
    const Map<MatrixXd> df_Times(as<Map<MatrixXd> >(df0_Times));
    const Map<MatrixXd> df_dep(as<Map<MatrixXd> >(df0_dep));
    const Map<MatrixXd> df_const(as<Map<MatrixXd> >(df0_const));
    Rcout.precision(10); //forces higher precision numbers printed to terminal
    int tot_covs = ceil(2 + df_dep.cols()/2 + df_const.cols() + 1);
    int max_rows = 0;
    if (iscox){
        max_rows = tu.size();
        dt = tu[1] - tu[0];
        for (int i =0; i<tu.size()-1;i++){
            if (dt > (tu[i+1] - tu[i])){
                dt = tu[i+1] - tu[i];
            }
        }
    } else {
        max_rows = ceil( ((df_Times.col(1).array() - df_Times.col(0).array()).array().abs().maxCoeff()) / dt);
    }
    int True_Rows=0;
    VectorXd row_store = VectorXd::Zero(tot_covs);
    MatrixXd new_row_store = MatrixXd::Zero(max_rows, tot_covs);
    //
    const static IOFormat CSVFormat(FullPrecision, DontAlignCols, ", ", "\n");
    ofstream file(filename);
    //
    int serial_0 = 0;
    int serial_1 = 0;
    for (int i_row=0; i_row<df_Times.rows(); i_row++){
        new_row_store = MatrixXd::Zero(max_rows, tot_covs);
        if (iscox){
            True_Rows=0;
            serial_0 = 0;
            serial_1 = 0;
            for (int i=0;i<tu.size(); i++){
                if (df_Times.coeff(i_row,1) >= tu[i]){
                    serial_1 = i;
                }
                if (df_Times.coeff(i_row,0) > tu[i]){
                    serial_0 = i+1;
                }
            }
            True_Rows = serial_1 - serial_0 + 1;
            #ifdef _OPENMP
            #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
            #endif
            for (int i_inner=serial_0;i_inner<serial_1+1;i_inner++){
                VectorXd dep_temp = VectorXd::Zero(df_dep.cols()/2);
                double t0 = tu[i_inner]- dt/2;
                double t1 = tu[i_inner];
                double ratio = 0;
                if ((df_Times.coeff(i_row,1) - df_Times.coeff(i_row,0)) > 0){
                    ratio = (t1 - df_Times.coeff(i_row,0))/(df_Times.coeff(i_row,1) - df_Times.coeff(i_row,0));
                }
                string func_id = "";
                string delim = "?";
                size_t pos = 0;
                string token = "";
                double temp_tok =0;
                int gather_val=0;
                char tok_char = 'a';
                // step string is either g, l, a ,b for >=, <=, >, <
                for (int i=0; i<dep_temp.size();i++){
                    func_id = as<std::string>(tform_tdep[i]);
                    if (func_id=="lin"){
                        dep_temp[i] = ratio * df_dep.coeff(i_row,2*i+1) + (1 - ratio) * df_dep.coeff(i_row,2*i);
                    } else {
                        pos = func_id.find(delim);
                        token = func_id.substr(0, pos);
                        if (token=="step"){
                            func_id.erase(0, pos + delim.length());
                            gather_val=0;
                            while ((pos = func_id.find(delim)) != std::string::npos) {
                                token = func_id.substr(0, pos);
                                //
                                tok_char = token[token.length()-1];
                                if (tok_char == 'g'){
                                    token.pop_back();
                                    temp_tok = stod(token);
                                    if (t1>temp_tok){
                                        gather_val = gather_val + 1;
                                    }
                                    if (t1==temp_tok){
                                        gather_val = gather_val + 1;
                                    }
                                } else if (tok_char == 'l'){
                                    token.pop_back();
                                    temp_tok = stod(token);
                                    if (t1<temp_tok){
                                        gather_val = gather_val + 1;
                                    }
                                    if (t1==temp_tok){
                                        gather_val = gather_val + 1;
                                    }
                                } else if (tok_char == 'a'){
                                    token.pop_back();
                                    temp_tok = stod(token);
                                    if (t1>temp_tok){
                                        gather_val = gather_val + 1;
                                    }
                                } else if (tok_char == 'b'){
                                    token.pop_back();
                                    temp_tok = stod(token);
                                    if (t1<temp_tok){
                                        gather_val = gather_val + 1;
                                    }
                                } else {
                                    ;
                                }
                                //
                                func_id.erase(0, pos + delim.length());
                            }
                            dep_temp[i] = gather_val;
                        } else {
                            Rcout << "C++ Error: " << func_id << " _:_ " << token << endl;
                            throw invalid_argument( "time dependent identifier isn't implemented" );
                        }
                    }
                }
                int event0 = 0;
                if (i_inner==True_Rows-1){
                    event0 = df0_event[i_row];
                }
                new_row_store.row(i_inner) << t0, t1, dep_temp.transpose(), df_const.row(i_row), event0;
            }
        } else {
            True_Rows = ceil( (df_Times.coeff(i_row,1) - df_Times.coeff(i_row,0))/dt);
            #ifdef _OPENMP
            #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
            #endif
            for (int i_inner=0;i_inner<True_Rows;i_inner++){
                VectorXd dep_temp = VectorXd::Zero(df_dep.cols()/2);
                double ratio = (i_inner+0.5)/True_Rows;
                double t0 = df_Times.coeff(i_row,0) + i_inner * dt;
                double t1 = t0 + dt;
                string func_id = "";
                string delim = "?";
                size_t pos = 0;
                string token = "";
                double temp_tok =0;
                int gather_val=0;
                char tok_char = 'a';
                for (int i=0; i<dep_temp.size();i++){
                    func_id = as<std::string>(tform_tdep[i]);
                    if (func_id=="lin"){
                        dep_temp[i] = ratio * df_dep.coeff(i_row,2*i+1) + (1 - ratio) * df_dep.coeff(i_row,2*i);
                    } else {
                        pos = func_id.find(delim);
                        token = func_id.substr(0, pos);
                        if (token=="step"){
                            func_id.erase(0, pos + delim.length());
                            gather_val=0;
                            while ((pos = func_id.find(delim)) != std::string::npos) {
                                token = func_id.substr(0, pos);
                                //
                                tok_char = token[token.length()-1];
                                if (tok_char == 'u'){
                                    token.pop_back();
                                    temp_tok = stod(token);
                                    if (t0>=temp_tok){
                                        gather_val = gather_val + 1;
                                    }
                                } else if (tok_char == 'l'){
                                    token.pop_back();
                                    temp_tok = stod(token);
                                    if (t1<=temp_tok){
                                        gather_val = gather_val + 1;
                                    }
                                } else {
                                    ;
                                }
                                //
                                func_id.erase(0, pos + delim.length());
                            }
                            dep_temp[i] = gather_val;
                        }
                    }
                }
                int event0 = 0;
                if (i_inner==True_Rows-1){
                    t1 = df_Times.coeff(i_row,1);
                    event0 = df0_event[i_row];
                }
                new_row_store.row(i_inner) << t0 + (t1-t0)*1e-1, t1, dep_temp, df_const.row(i_row), event0;
            }
        }
        if (file.is_open()){
            file << new_row_store.block(0,0,True_Rows,tot_covs).format(CSVFormat);
            file << "\n";
        }
    }
    if (file.is_open()){
        file.close();
    }
}

//' Generates factored columns in parallel
//'
//' \code{Gen_Fac_Par} Called directly from R, returns a matrix with factored columns
//' @inheritParams CPP_template
//'
//' @return saves a dataframe to be used with time-dependent covariate analysis
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix Gen_Fac_Par(const NumericMatrix df0, const NumericVector vals, const NumericVector cols, const int nthreads){
    const Map<MatrixXd> df(as<Map<MatrixXd> >(df0));
    MatrixXd Mat_Fac = MatrixXd::Zero(df.rows(), vals.size());
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<vals.size();ijk++){
        double col_c = cols[ijk];
        double val_c = vals[ijk];
        VectorXi select_ind_all = ((df.col(col_c).array() == val_c)).cast<int>(); //indices at risk
        //
        //
        int th = 1;
        visit_lambda(select_ind_all,
            [&Mat_Fac, ijk, th](double v, int i, int j) {
                if (v==th)
                    Mat_Fac(i,ijk)=1;
            });
        //
    }
    //
    return (wrap(Mat_Fac));
}

//' Interface between R code and the risk check
//'
//' \code{cox_ph_transition_single} Called directly from R, Defines the control variables and calls the function which only calculates the log-likelihood
//' @inheritParams CPP_template
//'
//' @return LogLik_Cox_PH output : Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
bool risk_check_transition(IntegerVector term_n, StringVector tform, NumericVector a_n,IntegerVector dfc,NumericMatrix& x_all, int fir,string modelform, List Control, List model_control, IntegerVector KeepConstant, int term_tot){
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    int nthreads = Control["ncores"];
    double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
    //
    // Performs regression
    //----------------------------------------------------------------------------------------------------------------//
    bool res = Check_Risk(term_n, tform, a_n, x_all, dfc,fir,modelform,verbose, debugging, KeepConstant, term_tot, nthreads, gmix_theta, gmix_term);
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}


//' Generates weightings for stratified poisson regression
//'
//' \code{Gen_Strat_Weight} Called from within c++, assigns vector of weights
//' @inheritParams CPP_template
//'
//' @return assigns weight in place and returns nothing
//' @noRd
//'
// [[Rcpp::export]]
void Gen_Strat_Weight(string modelform, const MatrixXd& dfs, const MatrixXd& PyrC, VectorXd& s_weights, const int nthreads, const StringVector& tform, const IntegerVector& term_n, const int& term_tot){
    ArrayXd Pyrs   = dfs.transpose() * PyrC.col(0);
    ArrayXd Events = dfs.transpose() * PyrC.col(1);
    ArrayXd weight = Events.array() * Pyrs.array().pow(-1).array();
    //
    //
    s_weights = dfs * weight.matrix();
    //
    vector<double> plin_count(term_tot,0);
    vector<double> loglin_count(term_tot,0);
    vector<double> dose_count(term_tot,0);
    #ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
            std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
            initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:dose_count,plin_count,loglin_count)
    #endif
    for (int ij=0;ij<(term_n.size());ij++){
        int tn = term_n[ij];
        if (as< string>(tform[ij])=="loglin") {
            loglin_count[tn]=loglin_count[tn]+1.0;
        } else if (as< string>(tform[ij])=="lin") {
            ;
        } else if (as< string>(tform[ij])=="plin") {
            plin_count[tn]=plin_count[tn]+1.0;
        } else if (as< string>(tform[ij])=="loglin_slope"){
            dose_count[tn]=dose_count[tn]+1.0; 
        } else if (as< string>(tform[ij])=="loglin_top"){
            if (ij==0){
                dose_count[tn]=dose_count[tn]+1.0;
            } else if (tform[ij-1]!="loglin_slope"){
                dose_count[tn]=dose_count[tn]+1.0;
                //
            } else {
                ;
            }
        } else if (as< string>(tform[ij])=="lin_slope"){
            ;
        } else if (as< string>(tform[ij])=="lin_int") {
            ;
        } else if (as< string>(tform[ij])=="quad_slope"){
            ;
        } else if (as< string>(tform[ij])=="step_slope"){
            ;
        } else if (as< string>(tform[ij])=="step_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_quad_slope") {
            ;
        } else if (as< string>(tform[ij])=="lin_quad_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_exp_slope") {
            ;
        } else if (as< string>(tform[ij])=="lin_exp_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_exp_exp_slope") {
            ;
        } else {
            throw invalid_argument( "incorrect subterm type" );
        }
    }
    //
    vector<double> term_val(term_tot,0);
    for (int ijk=0; ijk<term_tot;ijk++){ //combines non-dose terms into a single term
        if (dose_count[ijk]==0){
            dose_count[ijk] = 1.0;
        }
        if (plin_count[ijk]==0){
            plin_count[ijk] = 1.0;
        }
        if (loglin_count[ijk]==0){
            loglin_count[ijk] = 1.0;;//replaces missing data with 1
        }
        term_val[ijk] = dose_count[ijk] * plin_count[ijk] * loglin_count[ijk];
    }
    double default_val=0;
    if (modelform=="A"){
        for (int i=0; i<term_tot; i++){
            default_val += term_val[i];
        }
    }else if (modelform=="PA"){
        for (int i=1; i<term_tot; i++){
            default_val += term_val[i];
        }
        default_val *= term_val[0];
    }else if (modelform=="PAE"){
        for (int i=1; i<term_tot; i++){
            default_val += term_val[i];
        }
        default_val = (1 + default_val) * term_val[0];
    }else if (modelform=="M"){
        default_val=1;
        for (int i=1; i<term_tot; i++){
            default_val *= (1+term_val[i]);
        }
        default_val *= term_val[0];
    } else if (modelform=="GM"){
        throw invalid_argument( "GM isn't implemented" );
    } else {
        throw invalid_argument( "Model isn't implemented" );
    }
    s_weights = s_weights / default_val;
    return;
}

//' Checks the OMP flag
//'
//' \code{OMP_Check} Called directly from R, checks the omp flag and returns if omp is enabled
//'
//' @return boolean: True for OMP allowed
//'
// [[Rcpp::export]]
bool OMP_Check(){
    bool res = false;
    #ifdef _OPENMP
        res = true;
    #endif
    //----------------------------------------------------------------------------------------------------------------//
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}

//' Interface between R code and the Cox PH omnibus bounds regression function
//'
//' \code{cox_ph_Omnibus_Bounds_transition} Called directly from R, Defines the control variables and calls the regression function
//' @inheritParams CPP_template
//'
//' @return LogLik_Cox_PH output : Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List cox_ph_Omnibus_Bounds_transition(IntegerVector term_n, StringVector tform, NumericVector a_n,IntegerVector dfc,NumericMatrix& x_all, int fir, string modelform, List Control, NumericMatrix df_groups, NumericVector tu, IntegerVector KeepConstant, int term_tot, NumericVector STRATA_vals, NumericVector cens_vec, List model_control, NumericMatrix Cons_Mat, NumericVector Cons_Vec){
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    double lr = Control["lr"];
    NumericVector maxiters = Control["maxiters"];
    int guesses = Control["guesses"];
    int halfmax = Control["halfmax"];
    double epsilon = Control["epsilon"];
    //
    double abs_max = Control["abs_max"];
    double dose_abs_max = Control["dose_abs_max"];
    double deriv_epsilon =Control["deriv_epsilon"];
    string ties_method =Control["ties"];
    int nthreads = Control["ncores"];
    //
	const Map<VectorXd> cens_weight(as<Map<VectorXd> >(cens_vec));
	//
	double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
	double mult = model_control["search_mult"];
	//
	const Map<MatrixXd> Lin_Sys(as<Map<MatrixXd> >(Cons_Mat));
	const Map<VectorXd> Lin_Res(as<Map<VectorXd> >(Cons_Vec));
	//
	bool strata_bool = model_control["strata"];
	bool basic_bool  = model_control["basic"];
	bool null_bool   = model_control["null"];
	bool CR_bool     = model_control["cr"];
	bool single_bool = model_control["single"];
	bool constraint_bool = model_control["constraint"];
	//
	double qchi     = model_control["qchi"];
    int para_number = model_control["para_number"];
    //
    int maxstep     = model_control["maxstep"];
    //
    bool manual    = model_control["manual"];
    // Performs regression
    //----------------------------------------------------------------------------------------------------------------//
    List res;
    if (manual){
        res = LogLik_Cox_PH_Omnibus_Log_Bound_Search(term_n, tform, a_n, x_all, dfc,fir,modelform, lr, maxiters, guesses, halfmax, epsilon, abs_max,dose_abs_max, deriv_epsilon, df_groups, tu, verbose, debugging, KeepConstant, term_tot, ties_method, nthreads, STRATA_vals, cens_weight,  strata_bool, basic_bool, null_bool, CR_bool, single_bool, constraint_bool, gmix_theta, gmix_term, Lin_Sys, Lin_Res, qchi, para_number, maxstep, mult);
    } else {
        res = LogLik_Cox_PH_Omnibus_Log_Bound(term_n, tform, a_n, x_all, dfc,fir,modelform, lr, maxiters, guesses, halfmax, epsilon, abs_max,dose_abs_max, deriv_epsilon, df_groups, tu, verbose, debugging, KeepConstant, term_tot, ties_method, nthreads, STRATA_vals, cens_weight,  strata_bool, basic_bool, null_bool, CR_bool, single_bool, constraint_bool, gmix_theta, gmix_term, Lin_Sys, Lin_Res, qchi, para_number, maxstep, mult);
    }
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}

//' Interface between R code and the poisson omnibus bounds regression function
//'
//' \code{pois_Omnibus_Bounds_transition} Called directly from R, Defines the control variables and calls the regression function
//' @inheritParams CPP_template
//'
//' @return LogLik_Cox_PH output : Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List pois_Omnibus_Bounds_transition(NumericMatrix dfe, IntegerVector term_n, StringVector tform, NumericVector a_n,IntegerVector dfc,NumericMatrix& x_all, int fir, string modelform, List Control, IntegerVector KeepConstant, int term_tot, NumericMatrix df0, List model_control, NumericMatrix Cons_Mat, NumericVector Cons_Vec){
    const Map<MatrixXd> PyrC(as<Map<MatrixXd> >(dfe));
    const Map<MatrixXd> dfs(as<Map<MatrixXd> >(df0));
    //
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    double lr = Control["lr"];
    NumericVector maxiters = Control["maxiters"];
    int guesses = Control["guesses"];
    int halfmax = Control["halfmax"];
    double epsilon = Control["epsilon"];
    //
    double abs_max = Control["abs_max"];
    double dose_abs_max = Control["dose_abs_max"];
    double deriv_epsilon =Control["deriv_epsilon"];
    int nthreads = Control["ncores"];
    //
	//
	double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
	double mult = model_control["search_mult"];
	//
	const Map<MatrixXd> Lin_Sys(as<Map<MatrixXd> >(Cons_Mat));
	const Map<VectorXd> Lin_Res(as<Map<VectorXd> >(Cons_Vec));
	//
	bool strata_bool = model_control["strata"];
	bool single_bool = model_control["single"];
	bool constraint_bool = model_control["constraint"];
	//
	double qchi     = model_control["qchi"];
    int para_number = model_control["para_number"];
    //
    int maxstep     = model_control["maxstep"];
    //
    bool manual    = model_control["manual"];
    // Performs regression
    //----------------------------------------------------------------------------------------------------------------//
    List res;
    if (manual){
        res = LogLik_Poisson_Omnibus_Log_Bound_Search(PyrC, dfs, term_n, tform, a_n, x_all, dfc,fir,modelform, lr, maxiters, guesses, halfmax, epsilon, abs_max,dose_abs_max, deriv_epsilon, verbose, debugging, KeepConstant, term_tot, nthreads,  strata_bool, single_bool, constraint_bool, gmix_theta, gmix_term, Lin_Sys, Lin_Res, qchi, para_number, maxstep, mult);
    } else {
        res = LogLik_Poisson_Omnibus_Log_Bound(       PyrC, dfs, term_n, tform, a_n, x_all, dfc,fir,modelform, lr, maxiters, guesses, halfmax, epsilon, abs_max,dose_abs_max, deriv_epsilon, verbose, debugging, KeepConstant, term_tot, nthreads,  strata_bool, single_bool, constraint_bool, gmix_theta, gmix_term, Lin_Sys, Lin_Res, qchi, para_number, maxstep, mult);
    }
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}

//' Interface between R code and the poisson residual calculation function
//'
//' \code{pois_Residual_transition} Called directly from R, Defines the control variables and calls the calculation function
//' @inheritParams CPP_template
//'
//' @return Poisson_Residuals output : list of residuals and sum
//' @noRd
//'
// [[Rcpp::export]]
List pois_Residual_transition(NumericMatrix dfe, IntegerVector term_n, StringVector tform, NumericVector a_n,IntegerVector dfc,NumericMatrix& x_all, int fir, int der_iden,string modelform, List Control, IntegerVector KeepConstant, int term_tot, NumericMatrix df0, List model_control){
    //
    const Map<MatrixXd> PyrC(as<Map<MatrixXd> >(dfe));
    const Map<MatrixXd> dfs(as<Map<MatrixXd> >(df0));
    //
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    double abs_max = Control["abs_max"];
    double dose_abs_max = Control["dose_abs_max"];
    int nthreads = Control["ncores"];
    //
    double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
	//
	//
	bool strata_bool = model_control["strata"];
    bool Pearson_bool = model_control["pearson"];
    bool Deviance_bool = model_control["deviance"];
    //
    // Performs regression
    //----------------------------------------------------------------------------------------------------------------//
    List res = Poisson_Residuals( PyrC, term_n, tform, a_n, x_all, dfc, fir, der_iden, modelform, abs_max, dose_abs_max, verbose, debugging, KeepConstant, term_tot, nthreads, dfs, strata_bool, gmix_theta, gmix_term, Pearson_bool, Deviance_bool);
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}

//' Interface between R code and the Cox PH omnibus regression function
//'
//' \code{cox_ph_multidose_Omnibus_transition} Called directly from R, Defines the control variables and calls the regression function
//' @inheritParams CPP_template
//'
//' @return LogLik_Cox_PH output : Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List cox_ph_multidose_Omnibus_transition(IntegerVector term_n, StringVector tform, NumericVector a_n, IntegerMatrix dose_cols, IntegerVector dose_index,IntegerVector dfc,NumericMatrix& x_all, int fir, int der_iden,string modelform, List Control, NumericMatrix df_groups, NumericVector tu, IntegerVector KeepConstant, int term_tot, NumericVector STRATA_vals, NumericVector cens_vec, List model_control, NumericMatrix Cons_Mat, NumericVector Cons_Vec){
    bool change_all = Control["change_all"];
    int double_step = Control["double_step"];
    int verbose = Control["verbose"];
    bool debugging = FALSE;
    double lr = Control["lr"];
    int maxiter = Control["maxiter"];
    int halfmax = Control["halfmax"];
    double epsilon = Control["epsilon"];
    //
    double abs_max = Control["abs_max"];
    double dose_abs_max = Control["dose_abs_max"];
    double deriv_epsilon =Control["deriv_epsilon"];
    string ties_method =Control["ties"];
    int nthreads = Control["ncores"];
    //
	const Map<VectorXd> cens_weight(as<Map<VectorXd> >(cens_vec));
	//
	double gmix_theta = model_control["gmix_theta"];
	IntegerVector gmix_term = model_control["gmix_term"];
	//
	const Map<MatrixXd> Lin_Sys(as<Map<MatrixXd> >(Cons_Mat));
	const Map<VectorXd> Lin_Res(as<Map<VectorXd> >(Cons_Vec));
	//
	bool strata_bool = model_control["strata"];
	bool basic_bool  = model_control["basic"];
	bool null_bool   = model_control["null"];
	bool CR_bool     = model_control["cr"];
	bool single_bool = model_control["single"];
	bool constraint_bool = model_control["constraint"];
    //
    // Performs regression
    //----------------------------------------------------------------------------------------------------------------//
    List res = LogLik_Cox_PH_Multidose_Omnibus(term_n, tform, a_n, x_all, dose_cols, dose_index, dfc,fir, der_iden,modelform, lr, maxiter, halfmax, epsilon, abs_max,dose_abs_max, deriv_epsilon, df_groups, tu, double_step, change_all,verbose, debugging, KeepConstant, term_tot, ties_method, nthreads, STRATA_vals, cens_weight,  strata_bool, basic_bool, null_bool, CR_bool, single_bool, constraint_bool, gmix_theta, gmix_term, Lin_Sys, Lin_Res);
    //----------------------------------------------------------------------------------------------------------------//
    return res;
}
