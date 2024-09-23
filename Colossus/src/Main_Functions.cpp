#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include "Main_Functions.h"
#include "Omnibus_Pieces.h"
#include "Calc_Repeated.h"
#include "Subterms_Risk.h"
#include "Step_Calc.h"
#include "Colossus_types.h"
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

//' checks if the model is viable
//'
//' \code{Check_Risk} Calculates risks and checks for negative values
//'
//' @inheritParams CPP_template
//'
//' @return True for viable point, False for negative error
//' @noRd
//'
// [[Rcpp::export]]
bool Check_Risk( IntegerVector term_n, StringVector tform, NumericVector a_n,NumericMatrix& x_all,IntegerVector dfc,int fir,string modelform, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, int nthreads, const double gmix_theta, const IntegerVector gmix_term){
    ;
    //
    List temp_list = List::create(_["Status"]="FAILED"); //used as a dummy return value for code checking
    if (verbose>=3){
        Rcout << "C++ Note: START_RISK_CHECK" << endl;
    }
    time_point<system_clock> start_point, end_point;
    start_point = system_clock::now();
    auto start = time_point_cast<microseconds>(start_point).time_since_epoch().count();
    end_point = system_clock::now();
    auto ending = time_point_cast<microseconds>(end_point).time_since_epoch().count(); //The time duration is tracked
    //
    auto gibtime = system_clock::to_time_t(system_clock::now());
    if (verbose>=4){
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    const Map<MatrixXd> df0(as<Map<MatrixXd> >(x_all));
    //
    int totalnum = term_n.size();
    //
    if (verbose>=4){
        Rcout << "C++ Note: Term checked ";
        for (int ij=0;ij<totalnum;ij++){
            Rcout << term_n[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    //
    Rcout.precision(7); //forces higher precision numbers printed to terminal
    //
    //
    if (verbose>=4){
        end_point = system_clock::now();
        ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
        Rcout << "C++ Note: df99," << (ending-start) << ",Starting" <<endl;
        gibtime = system_clock::to_time_t(system_clock::now());
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    MatrixXd T0 = MatrixXd::Zero(df0.rows(), totalnum); //preallocates matrix for Term column
    //
    MatrixXd Te = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for column terms used for temporary storage
    MatrixXd R = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for Risks
    //
    MatrixXd Dose = MatrixXd::Constant(df0.rows(),term_tot,0.0); //Matrix of the total dose term values
    MatrixXd nonDose = MatrixXd::Constant(df0.rows(),term_tot,1.0); //Matrix of the total non-dose term values
    MatrixXd nonDose_LIN = MatrixXd::Constant(df0.rows(),term_tot,0.0); //matrix of Linear subterm values
    MatrixXd nonDose_PLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Loglinear subterm values
    MatrixXd nonDose_LOGLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Product linear subterm values
    MatrixXd TTerm = MatrixXd::Zero(Dose.rows(),Dose.cols()); //matrix of term values
    //
    if (verbose>=4){
        Rcout << "C++ Note: starting subterms " << term_tot << endl;
    }
    // Calculates the subterm and term values
    Make_subterms_Single( totalnum, term_n, tform, dfc, fir, T0, Dose, nonDose, TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN ,beta_0, df0,nthreads, debugging,KeepConstant);
    // ---------------------------------------------------------
    // Prints off a series of calculations to check at what point values are changing
    // ---------------------------------------------------------
    //
    //
    if (verbose>=4){
        Rcout << "C++ Note: values checked ";
        for (int ijk=0;ijk<totalnum;ijk++){
            Rcout << beta_0[ijk] << " ";
        }
        Rcout << " " << endl;
        Rcout << "C++ Note: sums checked ";
        for (int ijk=0;ijk<totalnum;ijk++){
            Rcout << T0.col(ijk).sum() << " ";
        }
        Rcout << " " << endl;
        Rcout << "C++ Note: dose checked ";
        for (int ijk=0;ijk<term_tot;ijk++){
            Rcout << Dose.col(ijk).array().sum() << " ";
        }
        Rcout << " " << endl;
        Rcout << "C++ Note: non-dose checked ";
        for (int ijk=0;ijk<term_tot;ijk++){
            Rcout << nonDose.col(ijk).array().sum() << " ";
        }
        Rcout << " " << endl;
        Rcout << "C++ Note: LIN_non-dose checked ";
        for (int ijk=0;ijk<term_tot;ijk++){
            Rcout << nonDose_LIN.col(ijk).array().sum() << " ";
        }
        Rcout << " " << endl;
        Rcout << "C++ Note: PLIN_non-dose checked ";
        for (int ijk=0;ijk<term_tot;ijk++){
            Rcout << nonDose_PLIN.col(ijk).array().sum() << " ";
        }
        Rcout << " " << endl;
        Rcout << "C++ Note: LOGLIN_non-dose checked ";
        for (int ijk=0;ijk<term_tot;ijk++){
            Rcout << nonDose_LOGLIN.col(ijk).array().sum() << " ";
        }
        Rcout << " " << endl;
    }
    //
    //
    if (verbose>=4){
        end_point = system_clock::now();
        ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
        Rcout << "C++ Note: df99," << (ending-start) << ",Prep_Terms" <<endl;
        gibtime = system_clock::to_time_t(system_clock::now());
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    // Calculates the risk for each row
    Make_Risks_Single(modelform, tform, term_n, totalnum, fir, T0, Te, R, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, nthreads, debugging,KeepConstant, gmix_theta, gmix_term);
    //
    // Removes infinite values
    //
    if ((R.minCoeff()<=0)||(R.hasNaN())){
        if (verbose>=4){
            Rcout << "C++ Error: A non-positive risk was detected: " << R.minCoeff() << endl;
            Rcout << "C++ Warning: final failing values ";
            for (int ijk=0;ijk<totalnum;ijk++){
                Rcout << beta_0[ijk] << " ";
            }
            Rcout << " " << endl;
            //
            Rcout << "C++ Warning: final failing terms ";
            for (int ijk=0;ijk<totalnum;ijk++){
                Rcout << tform[ijk] << " ";
            }
            Rcout << " " << endl;
        }
        return FALSE;
    }
    return TRUE;
}

//' Primary Cox PH regression with multiple starting points and optional combinations of null, stratification, competing risks, multiplicative log-linear model, and no derivative calculation.
//'
//' \code{LogLik_Cox_PH_Omnibus} Performs the calls to calculation functions, Structures the Cox PH regression, With verbose option prints out time stamps and intermediate sums of terms and derivatives
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List LogLik_Cox_PH_Omnibus( IntegerVector term_n, StringVector tform, NumericMatrix& a_ns,NumericMatrix& x_all,IntegerVector dfc,int fir, int der_iden,string modelform, double lr, NumericVector maxiters, int guesses, int halfmax, double epsilon,  double abs_max,double dose_abs_max, double deriv_epsilon, NumericMatrix df_groups, NumericVector tu, int double_step ,bool change_all, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, string ties_method, int nthreads, NumericVector& STRATA_vals, const VectorXd& cens_weight,  bool strata_bool, bool basic_bool, bool null_bool, bool CR_bool, bool single_bool, bool constraint_bool, const double gmix_theta, const IntegerVector gmix_term, const MatrixXd Lin_Sys, const VectorXd Lin_Res){
    ;
    //
    List temp_list = List::create(_["Status"]="TEMP"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_COX_GUESS" << endl;
    }
    time_point<system_clock> start_point, end_point;//, comp_point;
    start_point = system_clock::now();
    auto start = time_point_cast<microseconds>(start_point).time_since_epoch().count();
    end_point = system_clock::now();
    auto ending = time_point_cast<microseconds>(end_point).time_since_epoch().count(); //The time duration is tracked
    // comp_point = system_clock::now();
    // auto comp = time_point_cast<microseconds>(comp_point).time_since_epoch().count();
    //
    auto gibtime = system_clock::to_time_t(system_clock::now());
    if (verbose>=4){
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    // Time durations are measured from this point on in microseconds
    //
    // df0: covariate data
    // ntime: number of event times for Cox PH
    // totalnum: number of terms used
    //
    // ------------------------------------------------------------------------- // initialize
	const Map<MatrixXd> df0(as<Map<MatrixXd> >(x_all));
    int ntime = tu.size();
    int totalnum;
    int reqrdnum;
    // ------------------------------------------------------------------------- // initialize
	if (!null_bool){
		totalnum = term_n.size();
		reqrdnum = totalnum - sum(KeepConstant);
		if (verbose>=4){
            Rcout << "C++ Note: Term checked ";
            for (int ij=0;ij<totalnum;ij++){
                Rcout << term_n[ij] << " ";
            }
            Rcout << " " << endl;
        }
	} else {
		totalnum =1;
		reqrdnum = 1;
	}
    //
    // cout.precision: controls the number of significant digits printed
    // nthreads: number of threads used for parallel operations
    //
    Rcout.precision(7); //forces higher precision numbers printed to terminal
    //
    // Lld_worst: stores the highest magnitude log-likelihood derivative
    //
    //
    double Lld_worst = 0.0; //stores derivative value used to determine if every parameter is near convergence
    //
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    // ------------------------------------------------------------------------- // initialize
    NumericVector a_n = a_ns.row(0);
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    MatrixXd T0;
    MatrixXd Td0;
	MatrixXd Tdd0;
	//
	MatrixXd Te;
	MatrixXd R;
	ColXd Rd;
	ColXd Rdd;
	//
	MatrixXd Dose;
	MatrixXd nonDose;
	MatrixXd nonDose_LIN;
	MatrixXd nonDose_PLIN;
	MatrixXd nonDose_LOGLIN;
	MatrixXd TTerm;
	double dint; //The amount of change used to calculate derivatives in threshold paramters
	double dslp;
    ColXd RdR;
	ColXd RddR;
	// ------------------------------------------------------------------------- // initialize
    if (!null_bool){
		if (verbose>=4){
			end_point = system_clock::now();
			ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
			Rcout << "C++ Note: df99," << (ending-start) << ",Starting" <<endl;
			gibtime = system_clock::to_time_t(system_clock::now());
			Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
		}
		// ---------------------------------------------
		// To Start, needs to seperate the derivative terms
		// ---------------------------------------------
		//
		// comp_point = system_clock::now();
        // comp = time_point_cast<microseconds>(comp_point).time_since_epoch().count();
		Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
		// end_point = system_clock::now();
        // ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
//        Rcout << "C++ Note: refresh term " << (ending-comp) * 1e-6  <<endl;
    } else {
        R = MatrixXd::Constant(df0.rows(),1,1.0);
    }
    // ------------------------------------------------------------------------- // initialize
    StringMatrix RiskGroup_Strata;
    vector<string>  RiskGroup;
    IntegerMatrix RiskFail;
    const Map<MatrixXd> df_m(as<Map<MatrixXd> >(df_groups));
    // FOR BENCHMARKING PURPOSES
    // comp_point = system_clock::now();
    // comp = time_point_cast<microseconds>(comp_point).time_since_epoch().count();
    //
    // ------------------------------------------------------------------------- // initialize
    if (strata_bool){
        RiskGroup_Strata = StringMatrix(ntime,STRATA_vals.size()); //vector of strings detailing the rows
        RiskFail = IntegerMatrix(ntime,2*STRATA_vals.size()); //vector giving the event rows
        //
        if (verbose>=4){
            Rcout << "C++ Note: Grouping Start" << endl;
        }
        // Creates matrices used to identify the event risk groups
        if (CR_bool){
            Make_Groups_STRATA_CR( ntime, df_m, RiskFail, RiskGroup_Strata, tu, nthreads, debugging,STRATA_vals,cens_weight);
        } else {
            Make_Groups_STRATA( ntime, df_m, RiskFail, RiskGroup_Strata, tu, nthreads, debugging,STRATA_vals);
        }
    } else {
        RiskGroup.resize(ntime); //vector of strings detailing the rows
        RiskFail = IntegerMatrix(ntime,2); //vector giving the event rows
        //
        if (verbose>=4){
            Rcout << "C++ Note: Grouping Start" << endl;
        }
        // Creates matrices used to identify the event risk groups
        if (CR_bool){
            Make_Groups_CR( ntime, df_m, RiskFail, RiskGroup, tu,cens_weight, nthreads, debugging);
        } else {
            Make_Groups( ntime, df_m, RiskFail, RiskGroup, tu, nthreads, debugging);
        }
    }
    // FOR BENCHMARKING PURPOSES
    end_point = system_clock::now();
    ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
//    Rcout << "C++ Note: duration time " << (ending-comp) * 1e-6  << endl;
    //
    if (verbose>=4){
        end_point = system_clock::now();
        ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
        Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<-1<< ",Prep_List" <<endl;
        gibtime = system_clock::to_time_t(system_clock::now());
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    // ------------------------------------------------------------------------- // initialize
    MatrixXd Rls1;
	MatrixXd Lls1;
	MatrixXd Rls2;
	MatrixXd Rls3;
	MatrixXd Lls2;
	MatrixXd Lls3;
	vector<double> Ll(reqrdnum,0.0); //Log-likelihood values
	vector<double> Lld(reqrdnum,0.0); //Log-likelihood derivative values
	vector<double> Lldd(pow(reqrdnum,2),0.0);//The second derivative matrix has room for every combination, but only the lower triangle is calculated initially
    // ------------------------------------------------------------------------- // initialize
    if (null_bool){
        if (strata_bool){
            Rls1 =MatrixXd::Zero(ntime, STRATA_vals.size()); //precomputes a series of sums used frequently in the log-liklihood calculations
		    Lls1 =MatrixXd::Zero(ntime, STRATA_vals.size());
		    Calculate_Null_Sides_STRATA( RiskFail, RiskGroup_Strata, ntime, R, Rls1, Lls1, STRATA_vals,nthreads);
		    //
		    //
		    if (verbose>=4){
			    Rcout << "C++ Note: riskr checked ";
			    for (int ijk=0;ijk<reqrdnum;ijk++){
				    Rcout << Rls1.col(0).sum() << " ";
			    }
			    Rcout << " " << endl;
			    //
			    Rcout << "C++ Note: riskl checked ";
			    for (int ijk=0;ijk<reqrdnum;ijk++){
				    Rcout << Lls1.col(0).sum() << " ";
			    }
			    Rcout << " " << endl;
		    }
		    //
		    //
		    Calc_Null_LogLik_STRATA( nthreads, RiskFail, RiskGroup_Strata, ntime, R, Rls1, Lls1, STRATA_vals, Ll, ties_method);
        } else {
		    Rls1 =MatrixXd::Zero(ntime, 1); //precomputes a series of sums used frequently in the log-liklihood calculations
		    Lls1 =MatrixXd::Zero(ntime, 1);
		    //The log-likelihood is calculated in parallel over the risk groups
		    //
		    Calculate_Null_Sides( RiskFail, RiskGroup, ntime, R, Rls1, Lls1,nthreads);
		    //
		    //
		    if (verbose>=4){
			    Rcout << "C++ Note: riskr checked ";
			    for (int ijk=0;ijk<reqrdnum;ijk++){
				    Rcout << Rls1.col(0).sum() << " ";
			    }
			    Rcout << " " << endl;
			    //
			    Rcout << "C++ Note: riskl checked ";
			    for (int ijk=0;ijk<reqrdnum;ijk++){
				    Rcout << Lls1.col(0).sum() << " ";
			    }
			    Rcout << " " << endl;
		    }
		    //
		    //
		    Calc_Null_LogLik( nthreads, RiskFail, RiskGroup, ntime, R, Rls1, Lls1, Ll, ties_method);
	    }
		//
		List res_list = List::create(_["LogLik"]=wrap(Ll[0]),_["AIC"]=-2*Ll[0],_["BIC"]=-2*Ll[0]);
		// returns a list of results
		return res_list;
	}
	// comp_point = system_clock::now();
    // comp = time_point_cast<microseconds>(comp_point).time_since_epoch().count();
	Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
	// end_point = system_clock::now();
    // ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
//    Rcout << "C++ Note: refresh sides " << (ending-comp) * 1e-6  <<endl;
    //The log-likelihood is calculated in parallel over the risk groups
    vector <double> Ll_comp(2,Ll[0]); //vector to compare values
    double abs_max0 = abs_max;
    double dose_abs_max0 = dose_abs_max;
    //
    vector<double> dbeta(totalnum,0.0);
    //
    // --------------------------
    // always starts from initial guess
    // --------------------------
    vector<double> beta_c(totalnum,0.0);
    vector<double> beta_a(totalnum,0.0);
    vector<double> beta_best(totalnum,0.0);
    vector<double> beta_p(totalnum,0.0);
    VectorXd::Map(&beta_p[0], beta_0.size()) = beta_0;// stores previous parameters
    VectorXd::Map(&beta_c[0], beta_0.size()) = beta_0;// stores current parameters
    VectorXd::Map(&beta_a[0], beta_0.size()) = beta_0;// stores a refrence value for parameters
    VectorXd::Map(&beta_best[0], beta_0.size()) = beta_0;// stores the best parameters
    double halves = 0; //number of half-steps taken
    int ind0 = fir; //used for validations
    int iteration=0; //iteration number
    int maxiter=0;
    //
    bool convgd = FALSE;
    int iter_stop =0; //tracks if the iterations should be stopped for convergence
    int iter_check=0; //signal to check for convergence
    //
    NumericMatrix beta_fin(a_ns.rows(), a_ns.cols());
    NumericVector LL_fin(a_ns.rows());
    //
    double Ll_abs_best = 10;
    vector<double> beta_abs_best(totalnum,0.0);
    int guess_abs_best =-1;
    ///
    // Variables that are used for the risk check function shared across cox, poisson, and log bound functions
    bool cox_bool = true;
    double dev = 0.0;
    MatrixXd dev_temp = MatrixXd::Zero(1, 1);
    bool log_bound_bool = false;
    double Lstar = 0.0;
    MatrixXd PyrC = MatrixXd::Zero(1, 1);
    ///
    //
    for (int guess=0; guess <guesses; guess++){
        // comp_point = system_clock::now();
        // comp = time_point_cast<microseconds>(comp_point).time_since_epoch().count();
//        Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
        // end_point = system_clock::now();
        // ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
//        Rcout << "C++ Note: refresh term " << (ending-comp) * 1e-6  <<endl;
        // comp_point = system_clock::now();
        // comp = time_point_cast<microseconds>(comp_point).time_since_epoch().count();
        Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
        // end_point = system_clock::now();
        // ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
//        Rcout << "C++ Note: refresh sides " << (ending-comp) * 1e-6  <<endl;
        fill(Ll.begin(), Ll.end(), 0.0);
        if (!single_bool){
            fill(Lld.begin(), Lld.end(), 0.0);
            fill(Lldd.begin(), Lldd.end(), 0.0);
        }
        beta_p = beta_best;//
        beta_a = beta_best;//
        beta_c = beta_best;//
        abs_max = abs_max0;
        dose_abs_max = dose_abs_max0;
        iter_stop = 0;
        halves=0;
        iteration=0;
        halves = 0; //number of half-steps taken
        ind0 = fir; //used for validations
        iteration=0; //iteration number
        //
        convgd = FALSE;
        iter_stop =0; //tracks if the iterations should be stopped for convergence
        iter_check=0; //signal to check for convergence
        //
        maxiter = maxiters[guess];
        a_n = a_ns.row(guess);
        for (int i=0;i<beta_0.size();i++){
            beta_0[i] = a_n[i];
        }
        if (verbose>=4){
            Rcout << "C++ Note: starting subterms " << term_tot << " in guess " << guess << endl;
        }
        Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
        if ((R.minCoeff()<=0)||(R.hasNaN())){
            if (verbose>=1){
			    Rcout << "C++ Error: A non-positive risk was detected: " << R.minCoeff() << endl;
			    Rcout << "C++ Warning: final failing values ";
			    for (int ijk=0;ijk<totalnum;ijk++){
				    Rcout << beta_0[ijk] << " ";
			    }
			    Rcout << " " << endl;
		    }
			//
			temp_list = List::create(_["beta_0"]=wrap(beta_0) ,_["Deviation"]=R_NaN,_["Status"]="FAILED",_["LogLik"]=R_NaN);
			return temp_list;
		}
        //
        // -------------------------------------------------------------------------------------------
        //
        if (verbose>=4){
            Rcout << "C++ Note: Made Risk Side Lists" << endl;
        }
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        //
        for (int i=0;i<beta_0.size();i++){
            beta_c[i] = beta_0[i];
        }
        while ((iteration < maxiter)&&(iter_stop==0)){
            iteration++;
            beta_p = beta_c;//
            beta_a = beta_c;//
            beta_best = beta_c;//
            //
            // calculates the initial change in parameter
            // comp_point = system_clock::now();
            // comp = time_point_cast<microseconds>(comp_point).time_since_epoch().count();
            if (basic_bool){
                Calc_Change_Basic( double_step, nthreads, totalnum, der_iden, lr, abs_max, Ll, Lld, Lldd, dbeta, change_all, KeepConstant, debugging);
            } else {
                if (constraint_bool){
                    Calc_Change_Cons( Lin_Sys, Lin_Res, beta_0, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant, debugging);
                } else {
                    Calc_Change( double_step, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, change_all, tform, dose_abs_max,abs_max, KeepConstant, debugging);
                }
                Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
            }
            // end_point = system_clock::now();
            // ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
//            Rcout << "C++ Note: step " << (ending-comp) * 1e-6  <<endl;
            if (verbose>=4){
                Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
            }
            //
            //
            if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
                Ll_abs_best = Ll[ind0];
                beta_abs_best = beta_c;
                guess_abs_best=guess;
            }
            //
            if (verbose>=4){
                Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
                Rcout << "C++ Note: df504 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_abs_best[ij] << " ";
                }
                Rcout << " " << endl;
            }
            halves=0;
            while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
//                Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_a[ij] + dbeta[ij];
                    beta_c[ij] = beta_0[ij];
                }
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
                // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

                Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
                //
                Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
            }
            if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
                if (verbose>=4){
                    Rcout << "C++ Note: Changing back to best" <<endl;
                }
                // If it goes through every half step without improvement, then the maximum change needs to be decreased
                abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
                dose_abs_max = dose_abs_max*pow(0.5,halfmax);
                iter_check = 1;
                //
                beta_p = beta_best;//
                beta_a = beta_best;//
                beta_c = beta_best;//
//                Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_best[ij];
                }
                Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
            }
            if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                iter_check=0;
                if (Lld_worst < deriv_epsilon){//ends if the derivatives are low enough
                    iter_stop = 1;
                }
                if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                    iter_stop = 1;
                }
            }
        }
        // -----------------------------------------------
        // Performing Full Calculation to get full second derivative matrix
        // -----------------------------------------------
        fill(Ll.begin(), Ll.end(), 0.0);
        if (!single_bool){
            fill(Lld.begin(), Lld.end(), 0.0);
            fill(Lldd.begin(), Lldd.end(), 0.0);
        }
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        //
        a_n = beta_0;
        if (verbose>=4){
            Rcout << "C++ Note: ending guess " << guess << endl;
        }
        //
        beta_fin(guess, _) = a_n;
        LL_fin[guess] = Ll[0];
        if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
            Ll_abs_best = Ll[ind0];
            beta_abs_best = beta_c;
            guess_abs_best=guess;
        }
        //
        if (verbose>=4){
            Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
            Rcout << "C++ Note: df504 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_abs_best[ij] << " ";
            }
            Rcout << " " << endl;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Refreshing matrices after all guesses" << endl;
    }
    //
//    Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    fill(Ll.begin(), Ll.end(), 0.0);
    if (!single_bool){
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
    }
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    iter_stop = 0;
    halves=0;
    iteration=0;
    halves = 0; //number of half-steps taken
    ind0 = fir; //used for validations
    iteration=0; //iteration number
    //
    convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    iter_check=0; //signal to check for convergence
    //
    int guess_max=guess_abs_best;
    if (verbose>=3){
        Rcout << "C++ Note: Guess Results" << endl;
        Rcout << "Guess number, parameter values, Log-Likelihood" << endl;
        NumericVector beta_temp;
        for (int i=0; i<guesses; i++){
            beta_temp = wrap(beta_fin.row(i));
            if (i==guess_max){
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] << "<-- Best Guess" << endl;
            } else {
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] << endl;
            }
        }
    }
    //
    maxiter = maxiters[guesses];
    a_n = beta_abs_best;
    for (int i=0;i<beta_0.size();i++){
        beta_0[i] = a_n[i];
    }
    for (int i=0;i<beta_0.size();i++){
        beta_c[i] = beta_0[i];
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: starting subterms " << term_tot << " in best guess " << guess_max << endl;
    }
    //
    // Calculates the subterm and term values
    Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    // Calculates the side sum terms used
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
    //
    for (int i=0;i<beta_0.size();i++){
        beta_c[i] = beta_0[i];
    }
    while ((iteration < maxiter)&&(iter_stop==0)){
        iteration++;
        beta_p = beta_c;//
        beta_a = beta_c;//
        beta_best = beta_c;//
        //
        // calculates the initial change in parameter
        if (basic_bool){
            Calc_Change_Basic( double_step, nthreads, totalnum, der_iden, lr, abs_max, Ll, Lld, Lldd, dbeta, change_all, KeepConstant, debugging);
        } else {
            if (constraint_bool){
                Calc_Change_Cons( Lin_Sys, Lin_Res, beta_0, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant, debugging);
            } else {
                Calc_Change( double_step, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, change_all, tform, dose_abs_max,abs_max, KeepConstant, debugging);
            }
            Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
        }
        if (verbose>=4){
            Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
        }
        //
        //
        if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
            Ll_abs_best = Ll[ind0];
            beta_abs_best = beta_c;
        }
        //
        if (verbose>=4){
            Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
            Rcout << "C++ Note: df504 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_abs_best[ij] << " ";
            }
            Rcout << " " << endl;
        }
        halves=0;
        while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
//            Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
            for (int ij=0;ij<totalnum;ij++){
                beta_0[ij] = beta_a[ij] + dbeta[ij];
                beta_c[ij] = beta_0[ij];
            }
            // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
            // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
            // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

            Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
            Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
        }
        if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
            if (verbose>=4){
                Rcout << "C++ Note: Changing back to best" <<endl;
            }
            // If it goes through every half step without improvement, then the maximum change needs to be decreased
            abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
            dose_abs_max = dose_abs_max*pow(0.5,halfmax);
            iter_check = 1;
            //
            beta_p = beta_best;//
            beta_a = beta_best;//
            beta_c = beta_best;//
//            Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
            for (int ij=0;ij<totalnum;ij++){
                beta_0[ij] = beta_best[ij];
            }
            Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
            //
        }
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        Lld_worst=0;
        for (int ij=0;ij<reqrdnum;ij++){
            if (abs(Lld[ij]) > Lld_worst){
                Lld_worst = abs(Lld[ij]);
            }
        }
        if (iteration > reqrdnum){//Doesn't check the first several iterations for convergence
            if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                iter_check=0;
                if (Lld_worst < deriv_epsilon){//ends if the derivatives are low enough
                    iter_stop = 1;
                    convgd = TRUE;
                }
                Ll_comp[1]=Ll[0];
                if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                    iter_stop = 1;
                }
            }
        }
    }
    // -----------------------------------------------
    // Performing Full Calculation to get full second derivative matrix
    // -----------------------------------------------
    fill(Ll.begin(), Ll.end(), 0.0);
    if (!single_bool){
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
    }
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
    //
    if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
        Ll_abs_best = Ll[ind0];
        beta_abs_best = beta_c;
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: Exiting Regression" << endl;
        Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
        Rcout << "C++ Note: df504 ";//prints parameter values
        for (int ij=0;ij<totalnum;ij++){
            Rcout << beta_abs_best[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    List res_list;
    //
    if (single_bool){
        res_list = List::create(_["LogLik"]=wrap(Ll[0]),_["beta_0"]=wrap(beta_0) ,_["AIC"]=2*(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))-2*Ll[0],_["BIC"]=(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))*log(df0.rows())-2*Ll[0]);
        // returns a list of results
        return res_list;
    }
    List para_list;
    if (!basic_bool){
        para_list = List::create(_["term_n"]=term_n,_["tforms"]=tform); //stores the term information
    }
    List control_list = List::create(_["Iteration"]=iteration, _["Maximum Step"]=abs_max, _["Derivative Limiting"]=Lld_worst); //stores the total number of iterations used
    //
    //
    NumericVector Lldd_vec(reqrdnum * reqrdnum); //simplfied information matrix
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    const Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
    //
    MatrixXd Lldd_inv = -1 * Lldd_mat.inverse().matrix(); //uses inverse information matrix to calculate the standard deviation
    VectorXd stdev = VectorXd::Zero(totalnum);
    for (int ij=0;ij<totalnum;ij++){
        if (KeepConstant[ij]==0){
            int pij_ind = ij - sum(head(KeepConstant,ij));
            stdev(ij) = sqrt(Lldd_inv(pij_ind,pij_ind));
        }
    }
    //
    //
    if (basic_bool){
        res_list = List::create(_["LogLik"]=wrap(Ll[0]),_["First_Der"]=wrap(Lld),_["Second_Der"]=Lldd_vec,_["beta_0"]=wrap(beta_0) ,_["Standard_Deviation"]=wrap(stdev),_["AIC"]=2*(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))-2*Ll[0],_["BIC"]=(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))*log(df0.rows())-2*Ll[0],_["Control_List"]=control_list,_["Converged"]=convgd);
    } else {
        res_list = List::create(_["LogLik"]=wrap(Ll[0]),_["First_Der"]=wrap(Lld),_["Second_Der"]=Lldd_vec,_["beta_0"]=wrap(beta_0) ,_["Standard_Deviation"]=wrap(stdev),_["AIC"]=2*(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))-2*Ll[0],_["BIC"]=(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))*log(df0.rows())-2*Ll[0],_["Parameter_Lists"]=para_list,_["Control_List"]=control_list,_["Converged"]=convgd);
    }
    // returns a list of results
    return res_list;
}

//' Primary poisson regression with multiple starting points and optional combinations of stratification and no derivative calculation.
//'
//' \code{LogLik_Pois_Omnibus} Performs the calls to calculation functions, Structures the poisson regression, With verbose option prints out time stamps and intermediate sums of terms and derivatives
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List LogLik_Pois_Omnibus(const MatrixXd& PyrC, IntegerVector term_n, StringVector tform, NumericMatrix& a_ns,NumericMatrix& x_all,IntegerVector dfc,int fir, int der_iden,string modelform, double lr, NumericVector maxiters, int guesses, int halfmax, double epsilon,  double abs_max,double dose_abs_max, double deriv_epsilon, int double_step ,bool change_all, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, int nthreads, const MatrixXd& dfs, bool strata_bool, bool single_bool, bool constraint_bool, const double gmix_theta, const IntegerVector gmix_term, const MatrixXd Lin_Sys, const VectorXd Lin_Res){
    ;
    //
    List temp_list = List::create(_["Status"]="FAILED"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_POIS_GUESS" << endl;
    }
    time_point<system_clock> start_point, end_point;
    start_point = system_clock::now();
    auto start = time_point_cast<microseconds>(start_point).time_since_epoch().count();
    end_point = system_clock::now();
    auto ending = time_point_cast<microseconds>(end_point).time_since_epoch().count(); //The time duration is tracked
    //
    auto gibtime = system_clock::to_time_t(system_clock::now());
    if (verbose>=4){
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    // Time durations are measured from this point on in microseconds
    //
    // df0: covariate data
    // ntime: number of event times for Cox PH
    // totalnum: number of terms used
    //
    // ------------------------------------------------------------------------- // initialize
	const Map<MatrixXd> df0(as<Map<MatrixXd> >(x_all));
    //
    int totalnum = term_n.size();
    int reqrdnum = totalnum - sum(KeepConstant);
    //
    //
    if (verbose>=4){
        Rcout << "C++ Note: Term checked ";
        for (int ij=0;ij<totalnum;ij++){
            Rcout << term_n[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    // cout.precision: controls the number of significant digits printed
    // nthreads: number of threads used for parallel operations
    //
    Rcout.precision(7); //forces higher precision numbers printed to terminal
    // int nthreads = Eigen::nbThreads()-1; //stores how many threads are allocated
    //
    // Lld_worst: stores the highest magnitude log-likelihood derivative
    //
    //
    double Lld_worst = 0.0; //stores derivative value used to determine if every parameter is near convergence
    //
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    // ------------------------------------------------------------------------- // initialize
    NumericVector a_n = a_ns.row(0);
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    MatrixXd T0;
    MatrixXd Td0;
	MatrixXd Tdd0;
	//
	MatrixXd Te;
	MatrixXd R;
	ColXd Rd;
	ColXd Rdd;
	//
	MatrixXd Dose;
	MatrixXd nonDose;
	MatrixXd nonDose_LIN;
	MatrixXd nonDose_PLIN;
	MatrixXd nonDose_LOGLIN;
	MatrixXd TTerm;
	double dint; //The amount of change used to calculate derivatives in threshold paramters
	double dslp;
    ColXd RdR;
	ColXd RddR;
	// ------------------------------------------------------------------------- // initialize
	if (verbose>=4){
		end_point = system_clock::now();
		ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
		Rcout << "C++ Note: df99," << (ending-start) << ",Starting" <<endl;
		gibtime = system_clock::to_time_t(system_clock::now());
		Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
	}
	// ---------------------------------------------
	// To Start, needs to seperate the derivative terms
	// ---------------------------------------------
	//
    T0 = MatrixXd::Zero(df0.rows(), totalnum); //preallocates matrix for Term column
    Te = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for column terms used for temporary storage
	R = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for Risks
	//
	Dose = MatrixXd::Constant(df0.rows(),term_tot,0.0); //Matrix of the total dose term values
	nonDose = MatrixXd::Constant(df0.rows(),term_tot,1.0); //Matrix of the total non-dose term values
	nonDose_LIN = MatrixXd::Constant(df0.rows(),term_tot,0.0); //matrix of Linear subterm values
	nonDose_PLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Loglinear subterm values
	nonDose_LOGLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Product linear subterm values
	TTerm = MatrixXd::Zero(Dose.rows(),Dose.cols()); //matrix of term values
	if (single_bool){
		//
		;
    } else {
		Td0 = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Term derivative columns
		Tdd0 = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Term second derivative columns
		//
		Rd = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Risk derivatives
		Rdd = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Risk second derivatives
		//
		dint = dose_abs_max; //The amount of change used to calculate derivatives in threshold paramters
		dslp = abs_max;
        RdR = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Risk to derivative ratios
		RddR = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Risk to second derivative ratios
    }
    VectorXd s_weights;
    if (strata_bool){
        s_weights = VectorXd::Zero(df0.rows());
        Gen_Strat_Weight(modelform, dfs, PyrC, s_weights, nthreads, tform, term_n, term_tot);
    }
    // ------------------------------------------------------------------------- // initialize
	vector<double> Ll(reqrdnum,0.0); //Log-likelihood values
	vector<double> Lld(reqrdnum,0.0); //Log-likelihood derivative values
	vector<double> Lldd(pow(reqrdnum,2),0.0);//The second derivative matrix has room for every combination, but only the lower triangle is calculated initially
	MatrixXd dev_temp = MatrixXd::Zero(PyrC.rows(),2);
    double dev = 0;
    //The log-likelihood is calculated in parallel over the risk groups
    vector <double> Ll_comp(2,Ll[0]); //vector to compare values
    double abs_max0 = abs_max;
    double dose_abs_max0 = dose_abs_max;
    //
    vector<double> dbeta(totalnum,0.0);
    //
    // --------------------------
    // always starts from initial guess
    // --------------------------
    vector<double> beta_c(totalnum,0.0);
    vector<double> beta_a(totalnum,0.0);
    vector<double> beta_best(totalnum,0.0);
    vector<double> beta_p(totalnum,0.0);
    VectorXd::Map(&beta_p[0], beta_0.size()) = beta_0;// stores previous parameters
    VectorXd::Map(&beta_c[0], beta_0.size()) = beta_0;// stores current parameters
    VectorXd::Map(&beta_a[0], beta_0.size()) = beta_0;// stores a refrence value for parameters
    VectorXd::Map(&beta_best[0], beta_0.size()) = beta_0;// stores the best parameters
    double halves = 0; //number of half-steps taken
    int ind0 = fir; //used for validations
    int iteration=0; //iteration number
    int maxiter=0;
    //
    bool convgd = FALSE;
    int iter_stop =0; //tracks if the iterations should be stopped for convergence
    int iter_check=0; //signal to check for convergence
    //
    NumericMatrix beta_fin(a_ns.rows(), a_ns.cols());
    NumericVector LL_fin(a_ns.rows());
    //
        //
    double Ll_abs_best = 10;
    vector<double> beta_abs_best(totalnum,0.0);
    int guess_abs_best =-1;
    // Variables that are used for the risk check function shared across cox, poisson, and log bound functions
    bool basic_bool = false;
    VectorXd cens_weight(1);
    bool cox_bool = false;
    bool CR_bool = false;
    MatrixXd Lls1 = MatrixXd::Zero(1, 1);
    MatrixXd Lls2 = MatrixXd::Zero(1, 1);
    MatrixXd Lls3 = MatrixXd::Zero(1, 1);
    bool log_bound_bool = false;
    double Lstar = 0.0;
    int ntime = 1.0;
    IntegerMatrix RiskFail(1);
    vector<string> RiskGroup = {"temp"};
    StringMatrix RiskGroup_Strata(1);
    MatrixXd Rls1 = MatrixXd::Zero(1, 1);
    MatrixXd Rls2 = MatrixXd::Zero(1, 1);
    MatrixXd Rls3 = MatrixXd::Zero(1, 1);
    NumericVector STRATA_vals(1);
    string ties_method = "temp";
    ///
    for (int guess=0; guess <guesses; guess++){
        fill(Ll.begin(), Ll.end(), 0.0);
        if (!single_bool){
            fill(Lld.begin(), Lld.end(), 0.0);
            fill(Lldd.begin(), Lldd.end(), 0.0);
        }
        beta_p = beta_best;//
        beta_a = beta_best;//
        beta_c = beta_best;//
        abs_max = abs_max0;
        dose_abs_max = dose_abs_max0;
        iter_check=0;
        iter_stop = 0;
        halves=0;
        iteration=0;
        halves = 0; //number of half-steps taken
        ind0 = fir; //used for validations
        iteration=0; //iteration number
        //
        convgd = FALSE;
        iter_stop =0; //tracks if the iterations should be stopped for convergence
        iter_check=0; //signal to check for convergence
        //
        maxiter = maxiters[guess];
        a_n = a_ns.row(guess);
        for (int i=0;i<beta_0.size();i++){
            beta_0[i] = a_n[i];
        }
        if (verbose>=4){
            Rcout << "C++ Note: starting subterms " << term_tot << " in guess " << guess << endl;
        }
        //
        //
        Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
        if ((R.minCoeff()<=0)||(R.hasNaN())){
            if (verbose>=1){
			    Rcout << "C++ Error: A non-positive risk was detected: " << R.minCoeff() << endl;
			    Rcout << "C++ Warning: final failing values ";
			    for (int ijk=0;ijk<totalnum;ijk++){
				    Rcout << beta_0[ijk] << " ";
			    }
			    Rcout << " " << endl;
			    //
			    Rcout << "C++ Warning: final failing terms ";
			    for (int ijk=0;ijk<totalnum;ijk++){
				    Rcout << tform[ijk] << " ";
			    }
			    Rcout << " " << endl;
		    }
			temp_list = List::create(_["beta_0"]=wrap(beta_0) ,_["Deviation"]=R_NaN,_["Status"]="FAILED",_["LogLik"]=R_NaN);
			return temp_list;
		}
        //
        // -------------------------------------------------------------------------------------------
        //
        
        // Calculates log-likelihood
        Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
        //
        for (int i=0;i<beta_0.size();i++){
            beta_c[i] = beta_0[i];
        }
        while ((iteration < maxiter)&&(iter_stop==0)){
            iteration++;
            beta_p = beta_c;//
            beta_a = beta_c;//
            beta_best = beta_c;//
            //
            // calculates the initial change in parameter
            if (constraint_bool){
                Calc_Change_Cons( Lin_Sys, Lin_Res, beta_0, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant, debugging);
            } else {
                Calc_Change( double_step, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, change_all, tform, dose_abs_max,abs_max, KeepConstant, debugging);
            }
            Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
            if (verbose>=4){
                Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
            }
            //
            //
            if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
                Ll_abs_best = Ll[ind0];
                beta_abs_best = beta_c;
                guess_abs_best=guess;
            }
            //
            if (verbose>=4){
                Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
                Rcout << "C++ Note: df504 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_abs_best[ij] << " ";
                }
                Rcout << " " << endl;
            }
            halves=0;
            while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_a[ij] + dbeta[ij];
                    beta_c[ij] = beta_0[ij];
                }
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
                // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
                Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
                Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
            }
            if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
                if (verbose>=4){
                    Rcout << "C++ Note: Changing back to best" <<endl;
                }
                // If it goes through every half step without improvement, then the maximum change needs to be decreased
                abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
                dose_abs_max = dose_abs_max*pow(0.5,halfmax);
                iter_check = 1;
                //
                beta_p = beta_best;//
                beta_a = beta_best;//
                beta_c = beta_best;//
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_best[ij];
                }
                Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
                // Calculates log-likelihood
                Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
                //
            }
            Lld_worst=0;
            for (int ij=0;ij<reqrdnum;ij++){
                if (abs(Lld[ij]) > Lld_worst){
                    Lld_worst = abs(Lld[ij]);
                }
            }
            if (iteration > reqrdnum){//Doesn't check the first several iterations for convergence
                if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                    iter_check=0;
                    if (Lld_worst < deriv_epsilon){//ends if the derivatives are low enough
                        iter_stop = 1;
                    }
                    Ll_comp[1]=Ll[0];
                    if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                        iter_stop = 1;
                    }
                }
            }
            if (single_bool){
                iter_stop=1;
                if (verbose>=4){
                    end_point = system_clock::now();
                    ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
                    Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
                    gibtime = system_clock::to_time_t(system_clock::now());
                    Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
                    Rcout << "C++ Note: df101 ";//prints the log-likelihoods
                    for (int ij=0;ij<reqrdnum;ij++){
                        Rcout << Ll[ij] << " ";
                    }
                    Rcout << " " << endl;
                    Rcout << "C++ Note: df104 ";//prints parameter values
                    for (int ij=0;ij<totalnum;ij++){
                        Rcout << beta_0[ij] << " ";
                    }
                    Rcout << " " << endl;
                }
            } else {
                if (verbose>=4){
                    end_point = system_clock::now();
                    ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
                    Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
                    gibtime = system_clock::to_time_t(system_clock::now());
                    Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
                    Rcout << "C++ Note: df101 ";//prints the log-likelihoods
                    for (int ij=0;ij<reqrdnum;ij++){
                        Rcout << Ll[ij] << " ";
                    }
                    Rcout << " " << endl;
                    Rcout << "C++ Note: df102 ";//prints the first derivatives
                    for (int ij=0;ij<reqrdnum;ij++){
                        Rcout << Lld[ij] << " ";
                    }
                    Rcout << " " << endl;
                    Rcout << "C++ Note: df103 ";//prints the second derivatives
                    for (int ij=0;ij<reqrdnum;ij++){
                        Rcout << Lldd[ij*reqrdnum+ij] << " ";
                    }
                    Lld_worst=0;
                    for (int ij=0;ij<reqrdnum;ij++){//locates highest magnitude derivative
                        if (abs(Lld[ij]) > Lld_worst){
                            Lld_worst = abs(Lld[ij]);
                        }
                    }
                    Rcout << " " << endl;
                    Rcout << "C++ Note: df104 ";//prints parameter values
                    for (int ij=0;ij<totalnum;ij++){
                        Rcout << beta_0[ij] << " ";
                    }
                    Rcout << " " << endl;
                    Rcout << "C++ Note: Checking Deviance " << dev << endl;
                }
            }
        }
        // -----------------------------------------------
        // Performing Full Calculation to get full second derivative matrix
        // -----------------------------------------------
        fill(Ll.begin(), Ll.end(), 0.0);
        if (!single_bool){
            fill(Lld.begin(), Lld.end(), 0.0);
            fill(Lldd.begin(), Lldd.end(), 0.0);
        }
        // Calculates log-likelihood
        Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
        //
        a_n = beta_0;
        if (verbose>=4){
            Rcout << "C++ Note: ending guess " << guess << endl;
        }
        //
        beta_fin(guess, _) = a_n;
        LL_fin[guess] = Ll[0];
        if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
            Ll_abs_best = Ll[ind0];
            beta_abs_best = beta_c;
            guess_abs_best=guess;
        }
        //
        if (verbose>=4){
            Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
            Rcout << "C++ Note: df504 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_abs_best[ij] << " ";
            }
            Rcout << " " << endl;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Refreshing matrices after all guesses" << endl;
    }
    //
    fill(Ll.begin(), Ll.end(), 0.0);
    if (!single_bool){
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
    }
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    iter_check=0;
    iter_stop = 0;
    halves=0;
    iteration=0;
    halves = 0; //number of half-steps taken
    ind0 = fir; //used for validations
    iteration=0; //iteration number
    //
    convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    iter_check=0; //signal to check for convergence
    //
    int guess_max=guess_abs_best;
    if (verbose>=3){
        Rcout << "C++ Note: Guess Results" << endl;
        Rcout << "Guess number, parameter values, Log-Likelihood" << endl;
        NumericVector beta_temp;
        for (int i=0; i<guesses; i++){
            beta_temp = wrap(beta_fin.row(i));
            if (i==guess_max){
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] << "<-- Best Guess" << endl;
            } else {
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] << endl;
            }
        }
    }
    //
    maxiter = maxiters[guesses];
    a_n = beta_abs_best;
    for (int i=0;i<beta_0.size();i++){
        beta_0[i] = a_n[i];
    }
    for (int i=0;i<beta_0.size();i++){
        beta_c[i] = beta_0[i];
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: starting subterms " << term_tot << " in best guess " << guess_max << endl;
    }
    //
    // Calculates the subterm and term values
    Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);

    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    // Calculates the side sum terms used
    // Calculates log-likelihood
    Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
    //
    for (int i=0;i<beta_0.size();i++){
        beta_c[i] = beta_0[i];
    }
    while ((iteration < maxiter)&&(iter_stop==0)){
        iteration++;
        beta_p = beta_c;//
        beta_a = beta_c;//
        beta_best = beta_c;//
        //
        // calculates the initial change in parameter
        if (constraint_bool){
            Calc_Change_Cons( Lin_Sys, Lin_Res, beta_0, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant, debugging);
        } else {
            Calc_Change( double_step, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, change_all, tform, dose_abs_max,abs_max, KeepConstant, debugging);
        }
        Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
        if (verbose>=4){
            Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
        }
        //
        //
        if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
            Ll_abs_best = Ll[ind0];
            beta_abs_best = beta_c;
        }
        //
        if (verbose>=4){
            Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
            Rcout << "C++ Note: df504 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_abs_best[ij] << " ";
            }
            Rcout << " " << endl;
        }
        halves=0;
        while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
            for (int ij=0;ij<totalnum;ij++){
                beta_0[ij] = beta_a[ij] + dbeta[ij];
                beta_c[ij] = beta_0[ij];
            }
            // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
            // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
            // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

            Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
            Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
        }
        if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
            if (verbose>=4){
                Rcout << "C++ Note: Changing back to best" <<endl;
            }
            // If it goes through every half step without improvement, then the maximum change needs to be decreased
            abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
            dose_abs_max = dose_abs_max*pow(0.5,halfmax);
            iter_check = 1;
            //
            beta_p = beta_best;//
            beta_a = beta_best;//
            beta_c = beta_best;//
            for (int ij=0;ij<totalnum;ij++){
                beta_0[ij] = beta_best[ij];
            }
            Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
            // Calculates log-likelihood
            Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
        }
        Lld_worst=0;
        for (int ij=0;ij<reqrdnum;ij++){
            if (abs(Lld[ij]) > Lld_worst){
                Lld_worst = abs(Lld[ij]);
            }
        }
        if (iteration > reqrdnum){//Doesn't check the first several iterations for convergence
            if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                iter_check=0;
                if (Lld_worst < deriv_epsilon){//ends if the derivatives are low enough
                    iter_stop = 1;
                    convgd = TRUE;
                }
                Ll_comp[1]=Ll[0];
                if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                    iter_stop = 1;
                }
            }
        }
        if (single_bool){
            iter_stop=1;
            if (verbose>=4){
                end_point = system_clock::now();
                ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
                Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
                gibtime = system_clock::to_time_t(system_clock::now());
                Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
                Rcout << "C++ Note: df101 ";//prints the log-likelihoods
                for (int ij=0;ij<reqrdnum;ij++){
                    Rcout << Ll[ij] << " ";
                }
                Rcout << " " << endl;
                Rcout << "C++ Note: df104 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_0[ij] << " ";
                }
                Rcout << " " << endl;
            }
        } else {
            if (verbose>=4){
                end_point = system_clock::now();
                ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
                Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
                gibtime = system_clock::to_time_t(system_clock::now());
                Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
                Rcout << "C++ Note: df101 ";//prints the log-likelihoods
                for (int ij=0;ij<reqrdnum;ij++){
                    Rcout << Ll[ij] << " ";
                }
                Rcout << " " << endl;
                Rcout << "C++ Note: df102 ";//prints the first derivatives
                for (int ij=0;ij<reqrdnum;ij++){
                    Rcout << Lld[ij] << " ";
                }
                Rcout << " " << endl;
                Rcout << "C++ Note: df103 ";//prints the second derivatives
                for (int ij=0;ij<reqrdnum;ij++){
                    Rcout << Lldd[ij*reqrdnum+ij] << " ";
                }
                Lld_worst=0;
                for (int ij=0;ij<reqrdnum;ij++){//locates highest magnitude derivative
                    if (abs(Lld[ij]) > Lld_worst){
                        Lld_worst = abs(Lld[ij]);
                    }
                }
                Rcout << " " << endl;
                Rcout << "C++ Note: df104 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_0[ij] << " ";
                }
                Rcout << " " << endl;
                Rcout << "C++ Note: Checking Deviance " << dev << endl;
            }
        }
    }
    // -----------------------------------------------
    // Performing Full Calculation to get full second derivative matrix
    // -----------------------------------------------
    Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
    //
    if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
        Ll_abs_best = Ll[ind0];
        beta_abs_best = beta_c;
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
        Rcout << "C++ Note: df504 ";//prints parameter values
        for (int ij=0;ij<totalnum;ij++){
            Rcout << beta_abs_best[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    List res_list;
    //
    if (single_bool){
        res_list = List::create(_["LogLik"]=wrap(Ll[0]),_["beta_0"]=wrap(beta_0) ,_["AIC"]=2*(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))+dev,_["BIC"]=(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))*log(df0.rows())-2*Ll[0]);
        // returns a list of results
        return res_list;
    }
    List para_list = List::create(_["term_n"]=term_n,_["tforms"]=tform); //stores the term information
    List control_list = List::create(_["Iteration"]=iteration, _["Maximum Step"]=abs_max, _["Derivative Limiting"]=Lld_worst); //stores the total number of iterations used
    //
    NumericVector Lldd_vec(reqrdnum * reqrdnum);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    const Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
    //
    MatrixXd Lldd_inv = -1 * Lldd_mat.inverse().matrix(); //uses inverse information matrix to calculate the standard deviation
    VectorXd stdev = VectorXd::Zero(totalnum);
    for (int ij=0;ij<totalnum;ij++){
        if (KeepConstant[ij]==0){
            int pij_ind = ij - sum(head(KeepConstant,ij));
            stdev(ij) = sqrt(Lldd_inv(pij_ind,pij_ind));
        }
    }
    //
    //
    res_list = List::create(_["LogLik"]=wrap(Ll[0]),_["First_Der"]=wrap(Lld),_["Second_Der"]=Lldd_vec,_["beta_0"]=wrap(beta_0) ,_["Standard_Deviation"]=wrap(stdev) ,_["AIC"]=2*(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))+dev,_["BIC"]=(totalnum-accumulate(KeepConstant.begin(),KeepConstant.end(), 0.0))*log(df0.rows())-2*Ll[0],_["Deviation"]=dev,_["Parameter_Lists"]=para_list,_["Control_List"]=control_list,_["Converged"]=convgd);
    // returns a list of results
    return res_list;
}

//' Primary Cox PH likelihood bounds calcualtion function.
//'
//' \code{LogLik_Cox_PH_Omnibus_Log_Bound} Performs the calls to calculation functions and log-likeihood profile bounds
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List LogLik_Cox_PH_Omnibus_Log_Bound( IntegerVector term_n, StringVector tform, NumericVector a_n, NumericMatrix& x_all,IntegerVector dfc,int fir,string modelform, double lr, NumericVector maxiters, int guesses, int halfmax, double epsilon,  double abs_max,double dose_abs_max, double deriv_epsilon, NumericMatrix df_groups, NumericVector tu, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, string ties_method, int nthreads, NumericVector& STRATA_vals, const VectorXd& cens_weight,  bool strata_bool, bool basic_bool, bool null_bool, bool CR_bool, bool single_bool, bool constraint_bool, const double gmix_theta, const IntegerVector gmix_term, const MatrixXd Lin_Sys, const VectorXd Lin_Res, double qchi, int para_number, int maxstep, double mult){
    ;
    //
    List temp_list = List::create(_["Status"]="TEMP"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_COX_BOUNDS" << endl;
    }
    time_point<system_clock> start_point, end_point;
    start_point = system_clock::now();
    auto start = time_point_cast<microseconds>(start_point).time_since_epoch().count();
    end_point = system_clock::now();
    auto ending = time_point_cast<microseconds>(end_point).time_since_epoch().count(); //The time duration is tracked

    //
    auto gibtime = system_clock::to_time_t(system_clock::now());
    if (verbose>=4){
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    // Time durations are measured from this point on in microseconds
    //
    // df0: covariate data
    // ntime: number of event times for Cox PH
    // totalnum: number of terms used
    //
    // ------------------------------------------------------------------------- // initialize
	const Map<MatrixXd> df0(as<Map<MatrixXd> >(x_all));
    int ntime = tu.size();
    int totalnum;
    int reqrdnum;
    // ------------------------------------------------------------------------- // initialize
    if (null_bool){
        if (verbose>=1){
            Rcout << "null model is not compatable with log-based bound calculation" << endl;
        }
        temp_list = List::create(_["Status"]="FAILED",_["LogLik"]=R_NaN);
        return temp_list;
    }
    if (single_bool){
        if (verbose>=1){
            Rcout << "non-derivative model calculation is not compatable with log-based bound calculation" << endl;
        }
        temp_list = List::create(_["Status"]="FAILED",_["LogLik"]=R_NaN);
        return temp_list;
    }
	totalnum = term_n.size();
	reqrdnum = totalnum - sum(KeepConstant);
	if (verbose>=4){
        Rcout << "C++ Note: Term checked ";
        for (int ij=0;ij<totalnum;ij++){
            Rcout << term_n[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    // cout.precision: controls the number of significant digits printed
    // nthreads: number of threads used for parallel operations
    //
    Rcout.precision(7); //forces higher precision numbers printed to terminal
    //
    // Lld_worst: stores the highest magnitude log-likelihood derivative
    //
    //
//    double Lld_worst = 0.0; //stores derivative value used to determine if every parameter is near convergence
    //
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    // ------------------------------------------------------------------------- // initialize
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    MatrixXd T0;
    MatrixXd Td0;
	MatrixXd Tdd0;
	//
	MatrixXd Te;
	MatrixXd R;
	ColXd Rd;
	ColXd Rdd;
	//
	MatrixXd Dose;
	MatrixXd nonDose;
	MatrixXd nonDose_LIN;
	MatrixXd nonDose_PLIN;
	MatrixXd nonDose_LOGLIN;
	MatrixXd TTerm;
	double dint; //The amount of change used to calculate derivatives in threshold paramters
	double dslp;
    ColXd RdR;
	ColXd RddR;
	// ------------------------------------------------------------------------- // initialize
	if (verbose>=4){
		end_point = system_clock::now();
		ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
		Rcout << "C++ Note: df99," << (ending-start) << ",Starting" <<endl;
		gibtime = system_clock::to_time_t(system_clock::now());
		Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
	}
	// ---------------------------------------------
	// To Start, needs to seperate the derivative terms
	// ---------------------------------------------
	//
	Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    // ------------------------------------------------------------------------- // initialize
    StringMatrix RiskGroup_Strata;
    vector<string>  RiskGroup;
    IntegerMatrix RiskFail;
    const Map<MatrixXd> df_m(as<Map<MatrixXd> >(df_groups));
    // ------------------------------------------------------------------------- // initialize
    if (strata_bool){
        RiskGroup_Strata = StringMatrix(ntime,STRATA_vals.size()); //vector of strings detailing the rows
        RiskFail = IntegerMatrix(ntime,2*STRATA_vals.size()); //vector giving the event rows
        //
        if (verbose>=4){
            Rcout << "C++ Note: Grouping Start" << endl;
        }
        // Creates matrices used to identify the event risk groups
        if (CR_bool){
            Make_Groups_STRATA_CR( ntime, df_m, RiskFail, RiskGroup_Strata, tu, nthreads, debugging,STRATA_vals,cens_weight);
        } else {
            Make_Groups_STRATA( ntime, df_m, RiskFail, RiskGroup_Strata, tu, nthreads, debugging,STRATA_vals);
        }
    } else {
        RiskGroup.resize(ntime); //vector of strings detailing the rows
        RiskFail = IntegerMatrix(ntime,2); //vector giving the event rows
        //
        if (verbose>=4){
            Rcout << "C++ Note: Grouping Start" << endl;
        }
        // Creates matrices used to identify the event risk groups
        if (CR_bool){
            Make_Groups_CR( ntime, df_m, RiskFail, RiskGroup, tu,cens_weight, nthreads, debugging);
        } else {
            Make_Groups( ntime, df_m, RiskFail, RiskGroup, tu, nthreads, debugging);
        }
    }
    if (verbose>=4){
        end_point = system_clock::now();
        ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
        Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<-1<< ",Prep_List" <<endl;
        gibtime = system_clock::to_time_t(system_clock::now());
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    // ------------------------------------------------------------------------- // initialize
    MatrixXd Rls1;
	MatrixXd Lls1;
	MatrixXd Rls2;
	MatrixXd Rls3;
	MatrixXd Lls2;
	MatrixXd Lls3;
	vector<double> Ll(reqrdnum,0.0); //Log-likelihood values
	vector<double> Lld(reqrdnum,0.0); //Log-likelihood derivative values
	vector<double> Lldd(pow(reqrdnum,2),0.0);//The second derivative matrix has room for every combination, but only the lower triangle is calculated initially
    // ------------------------------------------------------------------------- // initialize
	Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    //The log-likelihood is calculated in parallel over the risk groups
    vector <double> Ll_comp(2,Ll[0]); //vector to compare values
    double abs_max0 = abs_max;
    double dose_abs_max0 = dose_abs_max;
    //
    vector<double> dbeta(totalnum,0.0);
    //
    // --------------------------
    // always starts from initial guess
    // --------------------------
    vector<double> beta_c(totalnum,0.0);
    vector<double> beta_a(totalnum,0.0);
    vector<double> beta_best(totalnum,0.0);
    vector<double> beta_p(totalnum,0.0);
    VectorXd::Map(&beta_p[0], beta_0.size()) = beta_0;// stores previous parameters
    VectorXd::Map(&beta_c[0], beta_0.size()) = beta_0;// stores current parameters
    VectorXd::Map(&beta_a[0], beta_0.size()) = beta_0;// stores a refrence value for parameters
    VectorXd::Map(&beta_best[0], beta_0.size()) = beta_0;// stores the best parameters
    // double halves = 0; //number of half-steps taken
    // int ind0 = fir; //used for validations
//    int iteration=0; //iteration number
    //
//    bool convgd = FALSE;
    int iter_stop =0; //tracks if the iterations should be stopped for convergence
    // int iter_check=0; //signal to check for convergence
    //
    //
    // double Ll_abs_best = 10;
    vector<double> beta_abs_best(totalnum,0.0);
    // int guess_abs_best =-1;
    //
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    fill(Ll.begin(), Ll.end(), 0.0);
    fill(Lld.begin(), Lld.end(), 0.0);
    fill(Lldd.begin(), Lldd.end(), 0.0);
    
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    //
    for (int i=0;i<beta_0.size();i++){
        beta_0[i] = a_n[i];
    }
    Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
    //
    //
    NumericVector Lldd_vec(reqrdnum * reqrdnum);
    NumericVector Lld_vecc(reqrdnum);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
    Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
//    double qchi = 3.841459;
    double Lstar = Ll[0]-qchi;
//    int para_number = 0;
    bool upper = true;
    int half_check = 0;
    bool trouble = false;
    IntegerVector KeepConstant_trouble (totalnum);
    for (int i=0; i<totalnum;i++){
        KeepConstant_trouble[i] = KeepConstant[i];
    }
    KeepConstant_trouble[para_number] = 1;
//    int maxstep=10;
    //
    vector<double> limits(2,0.0);
    vector<bool>   limit_hit(2, FALSE);
    vector<double> ll_final(2,0.0);
    List res_list;
    if (verbose>=4){
        Rcout << "C++ Note: STARTING BOUNDS" << endl;
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: STARTING Upper Bound" << endl;
    }
    upper = true;
    int step = 0;
    bool iter_continue = true;
    double max_change = 100;
    double deriv_max = 100;
    ///
    // variables added for log loop code
    ///
    bool cox_bool = true;
    VectorXd s_weights(1);
    int bound_val = 1;
    //
    while ((step<maxstep)&&(iter_continue)){
        step++;
        //
        trouble = false;
        half_check = 0;
        Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Ll[0], para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, step, dbeta, beta_0, upper, trouble, verbose, mult);
        if (trouble){
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,  abs_max, KeepConstant_trouble, debugging);
        }
        //
        beta_p = beta_c;//
        beta_a = beta_c;//

        Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
        //
        for (int ijk=0;ijk<totalnum;ijk++){
            if (KeepConstant[ijk]==0){
//                int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                //
                if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){ //the threshold values use different maximum deviation values
                    if (abs(dbeta[ijk])>dose_abs_max){
                        dbeta[ijk] = dose_abs_max * sign(dbeta[ijk]);
                    }
                }else{
                    if (abs(dbeta[ijk])>abs_max){
                        dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                    }
                }
            } else {
                dbeta[ijk]=0;
            }
        }
        //
//        Rcout << "Change: ";
        max_change = abs(dbeta[0]);
        for (int ij=0;ij<totalnum;ij++){
            if (ij==para_number){
                // we want to prevent two issues
                // first prevent the parameter estimate from crossing the optimum
                // issue is beta_0[para_number] <= beta_best[para_number]
                if (dbeta[ij] < (beta_best[para_number] - beta_a[ij])/lr){
                    dbeta[ij] = -0.5*dbeta[ij];
                }
                // second issue is making sure that the step is forced away from the optimum when possible
                if (Ll[0] > Lstar){
                    // If the log-likelihood is above the goal, then it must move away from the optimum point
                    // for upper limit, the step is always positive
                    dbeta[ij] = abs(dbeta[ij]);
                }
            }
            if (abs(dbeta[ij])>max_change){
                max_change = abs(dbeta[ij]);
            }
            beta_0[ij] = beta_a[ij] + lr*dbeta[ij];
            beta_c[ij] = beta_0[ij];
//            Rcout << dbeta[ij] << " ";
        }
//        Rcout << " " << endl;
        // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
        // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
        // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

        Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
        bound_val = 1;
        Cox_Pois_Log_Loop(abs_max, basic_bool, beta_0, beta_a, beta_c, bound_val, cox_bool, dbeta, debugging, df0, dfc, dint, Dose, dose_abs_max, dslp, fir, gmix_term, gmix_theta, half_check, halfmax, KeepConstant, limit_hit, lr, modelform, nonDose, nonDose_LIN, nonDose_LOGLIN, nonDose_PLIN, nthreads, R, Rd, Rdd, RddR, RdR, s_weights, single_bool, start, strata_bool, T0, Td0, Tdd0, Te, term_n, term_tot, tform, totalnum, TTerm, verbose);
        if (limit_hit[1]){
            limits[1] = 0;
            ll_final[1] = 0;
            break;
        }
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        //
        if (verbose>=4){
            end_point = system_clock::now();
            ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
            Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
            gibtime = system_clock::to_time_t(system_clock::now());
            Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
            Rcout << "C++ Note: df101 ";//prints the log-likelihoods
            for (int ij=0;ij<reqrdnum;ij++){
                Rcout << Ll[ij] << " ";
            }
            Rcout << " " << endl;
            Rcout << "C++ Note: df104 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_0[ij] << " ";
            }
            Rcout << " " << endl;
        }
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
            Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
            if (ij==jk){
                Lld_vecc[ij] = Lld[ij];
            }
        }
        Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
        Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
        Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
        limits[1] = beta_0[para_number];
        ll_final[1] = Ll[0];
        // if (max_change < epsilon){
        //     iter_continue = false;
        // } else if (deriv_max < deriv_epsilon){
        //     iter_continue = false;
        // }
        if ((max_change < epsilon)&&(deriv_max < deriv_epsilon)){
            iter_continue = false;
        }
        // Rcout << "df505 " << beta_0[para_number] << " " << max_change << " " << deriv_max << endl;
    }
    //
    //
    if (verbose>=4){
        Rcout << "C++ Note: STARTING Lower Bound" << endl;
    }
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    for (int ij=0;ij<totalnum;ij++){
        beta_0[ij] = beta_a[ij];
    }
//    Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    //
    Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Lldd_mat = as<Map<MatrixXd> >(Lldd_vec);
    Lld_vec = as<Map<VectorXd> >(Lld_vecc);
    //
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    //
    upper = false;
    step = 0;
    iter_continue = true;
    while ((step<maxstep)&&(iter_continue)){
        step++;
        trouble = false;
        half_check = 0;
        Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Ll[0], para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, step, dbeta, beta_0, upper, trouble, verbose, mult);
        if (trouble){
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,  abs_max, KeepConstant_trouble, debugging);
        }
        //
        beta_p = beta_c;//
        beta_a = beta_c;//

//        Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
        Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
        //
        for (int ijk=0;ijk<totalnum;ijk++){
            if (KeepConstant[ijk]==0){
//                int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                //
                if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){ //the threshold values use different maximum deviation values
                    if (abs(dbeta[ijk])>dose_abs_max){
                        dbeta[ijk] = dose_abs_max * sign(dbeta[ijk]);
                    }
                }else{
                    if (abs(dbeta[ijk])>abs_max){
                        dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                    }
                }
            } else {
                dbeta[ijk]=0;
            }
        }
        //
        max_change = abs(dbeta[0]);
//        Rcout << "Change: ";
        for (int ij=0;ij<totalnum;ij++){
//            Rcout << dbeta[ij] << " ";
            if (ij==para_number){
                // we want to prevent two issues
                // first prevent the parameter estimate from crossing the optimum
                // issue is beta_0[para_number] <= beta_best[para_number]
                if (dbeta[ij] > (beta_best[para_number] - beta_a[ij])/lr){
                    dbeta[ij] = -0.5*dbeta[ij];
                }
                // second issue is making sure that the step is forced away from the optimum when possible
                if (Ll[0] > Lstar){
                    // If the log-likelihood is above the goal, then it must move away from the optimum point
                    // for lower limit, the step is always positive
                    dbeta[ij] = -1*abs(dbeta[ij]);
                }
            }
            if (abs(dbeta[ij])>max_change){
                max_change = abs(dbeta[ij]);
            }
            beta_0[ij] = beta_a[ij] + lr*dbeta[ij];
            beta_c[ij] = beta_0[ij];
        }
//        Rcout << " " << endl;
        // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
        // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
        // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

        Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
        bound_val = 0;
        Cox_Pois_Log_Loop(abs_max, basic_bool, beta_0, beta_a, beta_c, bound_val, cox_bool, dbeta, debugging, df0, dfc, dint, Dose, dose_abs_max, dslp, fir, gmix_term, gmix_theta, half_check, halfmax, KeepConstant, limit_hit, lr, modelform, nonDose, nonDose_LIN, nonDose_LOGLIN, nonDose_PLIN, nthreads, R, Rd, Rdd, RddR, RdR, s_weights, single_bool, start, strata_bool, T0, Td0, Tdd0, Te, term_n, term_tot, tform, totalnum, TTerm, verbose);
        if (limit_hit[0]){
            limits[0] = 0;
            ll_final[0] = 0;
            break;
        }
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        //
        if (verbose>=4){
            end_point = system_clock::now();
            ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
            Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
            gibtime = system_clock::to_time_t(system_clock::now());
            Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
            Rcout << "C++ Note: df101 ";//prints the log-likelihoods
            for (int ij=0;ij<reqrdnum;ij++){
                Rcout << Ll[ij] << " ";
            }
            Rcout << " " << endl;
            Rcout << "C++ Note: df104 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_0[ij] << " ";
            }
            Rcout << " " << endl;
        }
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
            Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
            if (ij==jk){
                Lld_vecc[ij] = Lld[ij];
            }
        }
        Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
        Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
        Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
        limits[0] = beta_0[para_number];
        ll_final[0] = Ll[0];
        // if (max_change < epsilon){
        //     iter_continue = false;
        // } else if (deriv_max < deriv_epsilon){
        //     iter_continue = false;
        // }
        if ((max_change < epsilon)&&(deriv_max < deriv_epsilon)){
            iter_continue = false;
        }
        // Rcout << "df505 " << beta_0[para_number] << " " << max_change << " " << deriv_max << endl;
    }
    //
    res_list = List::create(_["Parameter_Limits"]=wrap(limits), _["Negative_Limit_Found"]=wrap(limit_hit), _["Likelihood_Boundary"]=wrap(ll_final), _["Likelihood_Goal"]=wrap(Lstar), _["Status"]="PASSED");
    // returns a list of results
    return res_list;
}

//' Primary Cox PH likelihood bounds calcualtion function.
//'
//' \code{LogLik_Cox_PH_Omnibus_Log_Bound_Search} Performs the calls to calculation functions and log-likeihood profile bounds
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List LogLik_Cox_PH_Omnibus_Log_Bound_Search( IntegerVector term_n, StringVector tform, NumericVector a_n, NumericMatrix& x_all,IntegerVector dfc,int fir,string modelform, double lr, NumericVector maxiters, int guesses, int halfmax, double epsilon,  double abs_max,double dose_abs_max, double deriv_epsilon, NumericMatrix df_groups, NumericVector tu, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, string ties_method, int nthreads, NumericVector& STRATA_vals, const VectorXd& cens_weight,  bool strata_bool, bool basic_bool, bool null_bool, bool CR_bool, bool single_bool, bool constraint_bool, const double gmix_theta, const IntegerVector gmix_term, const MatrixXd Lin_Sys, const VectorXd Lin_Res, double qchi, int para_number, int maxstep, double mult){
    ;
    //
    List temp_list = List::create(_["Status"]="TEMP"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_COX_BOUNDS_SEARCH" << endl;
    }
    time_point<system_clock> start_point, end_point;
    start_point = system_clock::now();
    auto start = time_point_cast<microseconds>(start_point).time_since_epoch().count();
    end_point = system_clock::now();
    auto ending = time_point_cast<microseconds>(end_point).time_since_epoch().count(); //The time duration is tracked

    //
    auto gibtime = system_clock::to_time_t(system_clock::now());
    if (verbose>=4){
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    // Time durations are measured from this point on in microseconds
    //
    // df0: covariate data
    // ntime: number of event times for Cox PH
    // totalnum: number of terms used
    //
    // ------------------------------------------------------------------------- // initialize
	const Map<MatrixXd> df0(as<Map<MatrixXd> >(x_all));
    int ntime = tu.size();
    int totalnum;
    int reqrdnum;
    // ------------------------------------------------------------------------- // initialize
    if (null_bool){
        if (verbose>=1){
            Rcout << "null model is not compatable with log-based bound calculation" << endl;
        }
        temp_list = List::create(_["Status"]="FAILED",_["LogLik"]=R_NaN);
        return temp_list;
    }
    if (single_bool){
        if (verbose>=1){
            Rcout << "non-derivative model calculation is not compatable with log-based bound calculation" << endl;
        }
        temp_list = List::create(_["Status"]="FAILED",_["LogLik"]=R_NaN);
        return temp_list;
    }
	totalnum = term_n.size();
	reqrdnum = totalnum - sum(KeepConstant);
	if (verbose>=4){
        Rcout << "C++ Note: Term checked ";
        for (int ij=0;ij<totalnum;ij++){
            Rcout << term_n[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    // cout.precision: controls the number of significant digits printed
    // nthreads: number of threads used for parallel operations
    //
    Rcout.precision(7); //forces higher precision numbers printed to terminal
    //
    // Lld_worst: stores the highest magnitude log-likelihood derivative
    //
    //
//    double Lld_worst = 0.0; //stores derivative value used to determine if every parameter is near convergence
    //
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    // ------------------------------------------------------------------------- // initialize
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    VectorXd beta_max = beta_0;
    MatrixXd T0;
    MatrixXd Td0;
	MatrixXd Tdd0;
	//
	MatrixXd Te;
	MatrixXd R;
	ColXd Rd;
	ColXd Rdd;
	//
	MatrixXd Dose;
	MatrixXd nonDose;
	MatrixXd nonDose_LIN;
	MatrixXd nonDose_PLIN;
	MatrixXd nonDose_LOGLIN;
	MatrixXd TTerm;
	double dint; //The amount of change used to calculate derivatives in threshold paramters
	double dslp;
    ColXd RdR;
	ColXd RddR;
	// ------------------------------------------------------------------------- // initialize
	if (verbose>=4){
		end_point = system_clock::now();
		ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
		Rcout << "C++ Note: df99," << (ending-start) << ",Starting" <<endl;
		gibtime = system_clock::to_time_t(system_clock::now());
		Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
	}
	// ---------------------------------------------
	// To Start, needs to seperate the derivative terms
	// ---------------------------------------------
	//
	Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    // ------------------------------------------------------------------------- // initialize
    StringMatrix RiskGroup_Strata;
    vector<string>  RiskGroup;
    IntegerMatrix RiskFail;
    const Map<MatrixXd> df_m(as<Map<MatrixXd> >(df_groups));
    // ------------------------------------------------------------------------- // initialize
    if (strata_bool){
        RiskGroup_Strata = StringMatrix(ntime,STRATA_vals.size()); //vector of strings detailing the rows
        RiskFail = IntegerMatrix(ntime,2*STRATA_vals.size()); //vector giving the event rows
        //
        if (verbose>=4){
            Rcout << "C++ Note: Grouping Start" << endl;
        }
        // Creates matrices used to identify the event risk groups
        if (CR_bool){
            Make_Groups_STRATA_CR( ntime, df_m, RiskFail, RiskGroup_Strata, tu, nthreads, debugging,STRATA_vals,cens_weight);
        } else {
            Make_Groups_STRATA( ntime, df_m, RiskFail, RiskGroup_Strata, tu, nthreads, debugging,STRATA_vals);
        }
    } else {
        RiskGroup.resize(ntime); //vector of strings detailing the rows
        RiskFail = IntegerMatrix(ntime,2); //vector giving the event rows
        //
        if (verbose>=4){
            Rcout << "C++ Note: Grouping Start" << endl;
        }
        // Creates matrices used to identify the event risk groups
        if (CR_bool){
            Make_Groups_CR( ntime, df_m, RiskFail, RiskGroup, tu,cens_weight, nthreads, debugging);
        } else {
            Make_Groups( ntime, df_m, RiskFail, RiskGroup, tu, nthreads, debugging);
        }
    }
    if (verbose>=4){
        end_point = system_clock::now();
        ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
        Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<-1<< ",Prep_List" <<endl;
        gibtime = system_clock::to_time_t(system_clock::now());
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    // ------------------------------------------------------------------------- // initialize
    MatrixXd Rls1;
	MatrixXd Lls1;
	MatrixXd Rls2;
	MatrixXd Rls3;
	MatrixXd Lls2;
	MatrixXd Lls3;
	vector<double> Ll(reqrdnum,0.0); //Log-likelihood values
	vector<double> Lld(reqrdnum,0.0); //Log-likelihood derivative values
	vector<double> Lldd(pow(reqrdnum,2),0.0);//The second derivative matrix has room for every combination, but only the lower triangle is calculated initially
    // ------------------------------------------------------------------------- // initialize
	Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    //The log-likelihood is calculated in parallel over the risk groups
    vector <double> Ll_comp(2,Ll[0]); //vector to compare values
    double abs_max0 = abs_max;
    double dose_abs_max0 = dose_abs_max;
    //
    vector<double> dbeta(totalnum,0.0);
    vector<double> dbeta_start(totalnum,0.0);
    //
    // --------------------------
    // always starts from initial guess
    // --------------------------
    vector<double> beta_c(totalnum,0.0);
    vector<double> beta_a(totalnum,0.0);
    vector<double> beta_best(totalnum,0.0);
    vector<double> beta_p(totalnum,0.0);
    vector<double> beta_peak(totalnum, 0.0);
    VectorXd::Map(&beta_p[0], beta_0.size()) = beta_0;// stores previous parameters
    VectorXd::Map(&beta_c[0], beta_0.size()) = beta_0;// stores current parameters
    VectorXd::Map(&beta_a[0], beta_0.size()) = beta_0;// stores a refrence value for parameters
    VectorXd::Map(&beta_best[0], beta_0.size()) = beta_0;// stores the best parameters
    VectorXd::Map(&beta_peak[0], beta_0.size()) = beta_0;// stores the peak parameters
    // double halves = 0; //number of half-steps taken
    // int ind0 = fir; //used for validations
//    int iteration=0; //iteration number
    //
//    bool convgd = FALSE;
    int iter_stop =0; //tracks if the iterations should be stopped for convergence
    // int iter_check=0; //signal to check for convergence
    //
    //
    // double Ll_abs_best = 10;
    vector<double> beta_abs_best(totalnum,0.0);
    // int guess_abs_best =-1;
    //
//    Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    fill(Ll.begin(), Ll.end(), 0.0);
    fill(Lld.begin(), Lld.end(), 0.0);
    fill(Lldd.begin(), Lldd.end(), 0.0);
    
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    //
    for (int i=0;i<beta_0.size();i++){
        beta_0[i] = a_n[i];
    }
    Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
    //
    //
    NumericVector Lldd_vec(reqrdnum * reqrdnum);
    NumericVector Lld_vecc(reqrdnum);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
    Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
//    double qchi = 3.841459;
    double Lstar = Ll[0]-qchi;
    double Lmax = Ll[0];
//    int para_number = 0;
    bool upper = true;
    int half_check = 0;
    bool trouble = false;
    double deriv_max = 100;
    //
    // int guesses = 5;
//    double mult = 1.5;
//    int guess_inters = 10;
    //
    IntegerVector KeepConstant_trouble (totalnum);
    for (int i=0; i<totalnum;i++){
        KeepConstant_trouble[i] = KeepConstant[i];
    }
    KeepConstant_trouble[para_number] = 1;
//    int maxstep=10;
    //
    vector<double> limits(2,0.0);
    vector<bool>   limit_hit(2, FALSE);
    vector<double> ll_final(2,0.0);
    List res_list;
    if (verbose>=4){
        Rcout << "C++ Note: STARTING BOUNDS" << endl;
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: STARTING Upper Bound" << endl;
    }
    upper = true;
    // Now define the list of points to check
    trouble = false;
    Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Lmax, para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, 0, dbeta_start, beta_0, upper, trouble, verbose, mult);
    NumericMatrix a_ns(guesses, totalnum);
    // now the dbeta holds the range to check
    // note that currently the guesses are not bounded
    for (int i=0;i<guesses;i++){
        for (int j=0;j<totalnum;j++){
            // use dbeta to assign a_n guess i, parameter j
            // assign such that i=guesses-1 gives mult*dbeta
            a_ns(i, j) = beta_0[j] + dbeta_start[j] * ( i+1)/(guesses);
        }
    }
    if (verbose>=4){
        for (int i=0;i<guesses;i++){
            Rcout << "C++ Note: Initial guess " << i << ": ";
            for (int j=0;j<totalnum;j++){
                Rcout << a_ns(i, j) << " ";
            }
            Rcout << " " << endl;
        }
    }
    // now we have the points to test
    double halves = 0; //number of half-steps taken
    int ind0 = fir; //used for validations
    int iteration=0; //iteration number
    int maxiter=0;
//    double Lld_worst = 0.0;
    //
    bool convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    int iter_check=0; //signal to check for convergence
    //
    NumericMatrix beta_fin(a_ns.rows(), a_ns.cols());
    NumericVector LL_fin(a_ns.rows());
    //
    double Ll_abs_best = 10;
    // vector<double> beta_abs_best(totalnum,0.0);
    int guess_abs_best =-1;
    // Variables that are used for the risk check function shared across cox, poisson, and log bound functions
    bool change_all = false;
    bool cox_bool = true;
    double dev = 0.0;
    MatrixXd dev_temp = MatrixXd::Zero(1, 1);
    bool log_bound_bool = true;
    MatrixXd PyrC = MatrixXd::Zero(1, 1);
    ///
    for (int guess=0; guess <guesses; guess++){
//        Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
        Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
        fill(Ll.begin(), Ll.end(), 0.0);
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
        beta_p = beta_best;//
        beta_a = beta_best;//
        beta_c = beta_best;//
        abs_max = abs_max0;
        dose_abs_max = dose_abs_max0;
        iter_stop = 0;
        halves=0;
        iteration=0;
        halves = 0; //number of half-steps taken
        ind0 = fir; //used for validations
        iteration=0; //iteration number
        //
        convgd = FALSE;
        iter_stop =0; //tracks if the iterations should be stopped for convergence
        iter_check=0; //signal to check for convergence
        //
        maxiter = maxiters[guess];
        a_n = a_ns.row(guess);
        for (int i=0;i<beta_0.size();i++){
            beta_0[i] = a_n[i];
        }
        if (verbose>=4){
            Rcout << "C++ Note: starting subterms " << term_tot << " in guess " << guess << endl;
        }
        Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
        if ((R.minCoeff()<=0)||(R.hasNaN())){
            if (verbose>=4){
			    Rcout << "C++ Error: A non-positive risk was detected: " << R.minCoeff() << endl;
			    Rcout << "C++ Warning: final failing values ";
			    for (int ijk=0;ijk<totalnum;ijk++){
				    Rcout << beta_0[ijk] << " ";
			    }
			}
            iter_stop = 1;
            Ll[0] = 404; // note to consider this point too far
		} else {
            //
            // -------------------------------------------------------------------------------------------
            //
            if (verbose>=4){
                Rcout << "C++ Note: Made Risk Side Lists" << endl;
            }
            Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
            //
            for (int i=0;i<beta_0.size();i++){
                beta_c[i] = beta_0[i];
            }
        }
        while ((iteration < maxiter)&&(iter_stop==0)){
            iteration++;
            beta_p = beta_c;//
            beta_a = beta_c;//
            beta_best = beta_c;//
            //
            // calculates the initial change in parameter
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,  abs_max, KeepConstant_trouble, debugging);
            Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
            if (verbose>=4){
                Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
            }
            //
            //
            if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
                Ll_abs_best = Ll[ind0];
                beta_abs_best = beta_c;
                guess_abs_best=guess;
            }
            //
            if (verbose>=4){
                Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
                Rcout << "C++ Note: df504 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_abs_best[ij] << " ";
                }
                Rcout << " " << endl;
            }
            halves=0;
            while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
//                Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_a[ij] + dbeta[ij];
                    beta_c[ij] = beta_0[ij];
                }
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
                // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

                Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
                Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
            }
            if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
                if (verbose>=4){
                    Rcout << "C++ Note: Changing back to best" <<endl;
                }
                // If it goes through every half step without improvement, then the maximum change needs to be decreased
                abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
                dose_abs_max = dose_abs_max*pow(0.5,halfmax);
                iter_check = 1;
                //
                beta_p = beta_best;//
                beta_a = beta_best;//
                beta_c = beta_best;//
//                Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_best[ij];
                }
                Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
            }
            if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                iter_check=0;
                if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                    iter_stop = 1;
                }
            }
        }
        // -----------------------------------------------
        // Performing Full Calculation to get full second derivative matrix
        // -----------------------------------------------
        fill(Ll.begin(), Ll.end(), 0.0);
        if (!single_bool){
            fill(Lld.begin(), Lld.end(), 0.0);
            fill(Lldd.begin(), Lldd.end(), 0.0);
        }
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        //
        a_n = beta_0;
        if (verbose>=4){
            Rcout << "C++ Note: ending guess " << guess << endl;
        }
        //
        beta_fin(guess, _) = a_n;
        LL_fin[guess] = Ll[0];
        //
    }
    if (verbose>=4){
        Rcout << "C++ Note: Refreshing matrices after all guesses" << endl;
    }
    //
//    Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    fill(Ll.begin(), Ll.end(), 0.0);
    if (!single_bool){
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
    }
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    iter_stop = 0;
    halves=0;
    iteration=0;
    halves = 0; //number of half-steps taken
    ind0 = fir; //used for validations
    iteration=0; //iteration number
    //
    convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    iter_check=0; //signal to check for convergence
    //
//    int guess_max=guess_abs_best;
    // next we need to figure out what point to start at
    int best_guess = -1;
    for (int guess=guesses-1; guess>-1;guess--){
        // we want to find the closest point at which the loglikelihood is below lstar
        if (LL_fin[guess] < Lstar){
            best_guess = guess;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Upper Guess Results" << endl;
        Rcout << "Guess number, parameter values, Log-Likelihood change" << endl;
        NumericVector beta_temp;
        for (int i=0; i<guesses; i++){
            beta_temp = wrap(beta_fin.row(i));
            if (i==best_guess){
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] - Lstar << "<-- Best Guess" << endl;
            } else {
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] - Lstar << endl;
            }
        }
    }
    NumericVector beta_temp;
    NumericVector beta_temp0;
    if (best_guess == 0){
        beta_temp = wrap(beta_fin.row(best_guess)); // the first point was closest, no lower bound
    } else if (best_guess == -1){
        beta_temp = wrap(beta_fin.row(guesses-1)); // the last point was closest, no upper bound
    } else {
        beta_temp = wrap(beta_fin.row(best_guess));
        beta_temp0 = wrap(beta_fin.row(best_guess-1));
        for (int i=0;i<beta_temp.size();i++){
            beta_temp[i] = beta_temp[i] + beta_temp0[i];
            beta_temp[i] = beta_temp[i]/2;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Initial Guess: ";
        for (int i=0;i<beta_0.size();i++){
            Rcout << beta_temp[i] << " ";
        }
        Rcout << " " << endl;
    }
    for (int i=0;i<beta_0.size();i++){
        a_n[i] = beta_temp[i];
        beta_0[i] = a_n[i];
        beta_c[i] = beta_0[i];
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: starting subterms after in best guess" << endl;
    }
    //
//    Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    // Calculates the subterm and term values
    Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    // Calculates the side sum terms used
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Lldd_mat = as<Map<MatrixXd> >(Lldd_vec);
    Lld_vec = as<Map<VectorXd> >(Lld_vecc);
    int step = 1;
    bool iter_continue = true;
    double max_change = 100;
    ///
    // variables added for log loop code
    ///
    VectorXd s_weights(1);
    int bound_val = 1;
    //
    while ((step<maxstep)&&(iter_continue)){
        step++;
         trouble = false;
         Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Ll[0], para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, step, dbeta, beta_0, upper, trouble, verbose, mult);
         if (trouble){
             Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,  abs_max, KeepConstant_trouble, debugging);
         }
         //
         beta_p = beta_c;//
         beta_a = beta_c;//

//         Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
         Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
         //
         for (int ijk=0;ijk<totalnum;ijk++){
             if (KeepConstant[ijk]==0){
//                 int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                 //
                 if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){ //the threshold values use different maximum deviation values
                     if (abs(dbeta[ijk])>dose_abs_max){
                         dbeta[ijk] = dose_abs_max * sign(dbeta[ijk]);
                     }
                 }else{
                     if (abs(dbeta[ijk])>abs_max){
                         dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                     }
                 }
             } else {
                 dbeta[ijk]=0;
             }
         }
         max_change = abs(dbeta[0]);
         //
 //        Rcout << "Change: ";
         for (int ij=0;ij<totalnum;ij++){
//            Rcout << dbeta[ij] << " ";
            if (ij==para_number){
                // we want to prevent two issues
                // first prevent the parameter estimate from crossing the optimum
                // issue is beta_0[para_number] <= beta_best[para_number]
                if (dbeta[ij] < (beta_best[para_number] - beta_a[ij])/lr){
                    dbeta[ij] = -0.5*dbeta[ij];
                }
                // second issue is making sure that the step is forced away from the optimum when possible
                if (Ll[0] > Lstar){
                    // If the log-likelihood is above the goal, then it must move away from the optimum point
                    // for upper limit, the step is always positive
                    dbeta[ij] = abs(dbeta[ij]);
                }
            }
            if (abs(dbeta[ij])>max_change){
                max_change = abs(dbeta[ij]);
            }
            beta_0[ij] = beta_a[ij] + lr*dbeta[ij];
            beta_c[ij] = beta_0[ij];
        }
 //        Rcout << " " << endl;
         // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
         // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
         // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

         Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
         bound_val = 1;
         Cox_Pois_Log_Loop(abs_max, basic_bool, beta_0, beta_a, beta_c, bound_val, cox_bool, dbeta, debugging, df0, dfc, dint, Dose, dose_abs_max, dslp, fir, gmix_term, gmix_theta, half_check, halfmax, KeepConstant, limit_hit, lr, modelform, nonDose, nonDose_LIN, nonDose_LOGLIN, nonDose_PLIN, nthreads, R, Rd, Rdd, RddR, RdR, s_weights, single_bool, start, strata_bool, T0, Td0, Tdd0, Te, term_n, term_tot, tform, totalnum, TTerm, verbose);
         if (limit_hit[1]){
             limits[1] = 0;
             ll_final[1] = 0;
             break;
         }
         Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
         //
         if (verbose>=4){
             end_point = system_clock::now();
             ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
             Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
             gibtime = system_clock::to_time_t(system_clock::now());
             Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
             Rcout << "C++ Note: df101 ";//prints the log-likelihoods
             for (int ij=0;ij<reqrdnum;ij++){
                 Rcout << Ll[ij] << " ";
             }
             Rcout << " " << endl;
             Rcout << "C++ Note: df104 ";//prints parameter values
             for (int ij=0;ij<totalnum;ij++){
                 Rcout << beta_0[ij] << " ";
             }
             Rcout << " " << endl;
         }
         //
         #ifdef _OPENMP
         #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
         #endif
         for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
             int ij = 0;
             int jk = ijk;
             while (jk>ij){
                 ij++;
                 jk-=ij;
             }
             Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
             Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
             if (ij==jk){
                 Lld_vecc[ij] = Lld[ij];
             }
         }
         Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
         Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
         Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
         limits[1] = beta_0[para_number];
         ll_final[1] = Ll[0];
         if ((max_change < epsilon)&&(deriv_max < deriv_epsilon)){
            iter_continue = false;
         }
     }
    // Now refresh matrices back to the maximum point
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: STARTING Lower Bound" << endl;
    }
    beta_p = beta_peak;//
    beta_a = beta_peak;//
    beta_c = beta_peak;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    for (int ij=0;ij<totalnum;ij++){
        beta_0[ij] = beta_a[ij];
    }
//    Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
    //
    upper = false;
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Lldd_mat = as<Map<MatrixXd> >(Lldd_vec);
    Lld_vec = as<Map<VectorXd> >(Lld_vecc);
    //
    trouble = false;
    Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Lmax, para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, 0, dbeta_start, beta_0, upper, trouble, verbose, mult);
    // now the dbeta holds the range to check
    // note that currently the guesses are not bounded
    for (int i=0;i<guesses;i++){
        for (int j=0;j<totalnum;j++){
            // use dbeta to assign a_n guess i, parameter j
            // assign such that i=guesses-1 gives mult*dbeta
            a_ns(i, j) = beta_0[j] + dbeta_start[j] * ( i+1)/(guesses);
        }
    }
    if (verbose>=4){
        for (int i=0;i<guesses;i++){
            Rcout << "C++ Note: Initial guess " << i << ": ";
            for (int j=0;j<totalnum;j++){
                Rcout << a_ns(i, j) << " ";
            }
            Rcout << " " << endl;
        }
    }
    // now we have the points to test
    halves = 0; //number of half-steps taken
    ind0 = fir; //used for validations
    iteration=0; //iteration number
    maxiter=0;
//    double Lld_worst = 0.0;
    //
    convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    iter_check=0; //signal to check for convergence
    //
    Ll_abs_best = 10;
    // vector<double> beta_abs_best(totalnum,0.0);
    guess_abs_best =-1;

    for (int guess=0; guess <guesses; guess++){
//        Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
        Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
        fill(Ll.begin(), Ll.end(), 0.0);
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
        beta_p = beta_best;//
        beta_a = beta_best;//
        beta_c = beta_best;//
        abs_max = abs_max0;
        dose_abs_max = dose_abs_max0;
        iter_stop = 0;
        halves=0;
        iteration=0;
        halves = 0; //number of half-steps taken
        ind0 = fir; //used for validations
        iteration=0; //iteration number
        //
        convgd = FALSE;
        iter_stop =0; //tracks if the iterations should be stopped for convergence
        iter_check=0; //signal to check for convergence
        //
        maxiter = maxiters[guess];
        a_n = a_ns.row(guess);
        for (int i=0;i<beta_0.size();i++){
            beta_0[i] = a_n[i];
        }
        if (verbose>=4){
            Rcout << "C++ Note: starting subterms " << term_tot << " in guess " << guess << endl;
        }
        Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
        if ((R.minCoeff()<=0)||(R.hasNaN())){
            if (verbose>=4){
			    Rcout << "C++ Error: A non-positive risk was detected: " << R.minCoeff() << endl;
			    Rcout << "C++ Warning: final failing values ";
			    for (int ijk=0;ijk<totalnum;ijk++){
				    Rcout << beta_0[ijk] << " ";
			    }
			}
            iter_stop = 1;
            Ll[0] = 404; // note to consider this point too far
		} else {
            //
            // -------------------------------------------------------------------------------------------
            //
            if (verbose>=4){
                Rcout << "C++ Note: Made Risk Side Lists" << endl;
            }
            Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
            //
            for (int i=0;i<beta_0.size();i++){
                beta_c[i] = beta_0[i];
            }
        }
        while ((iteration < maxiter)&&(iter_stop==0)){
            iteration++;
            beta_p = beta_c;//
            beta_a = beta_c;//
            beta_best = beta_c;//
            //
            // calculates the initial change in parameter
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,  abs_max, KeepConstant_trouble, debugging);
            Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
            if (verbose>=4){
                Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
            }
            //
            //
            if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
                Ll_abs_best = Ll[ind0];
                beta_abs_best = beta_c;
                guess_abs_best=guess;
            }
            //
            if (verbose>=4){
                Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
                Rcout << "C++ Note: df504 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_abs_best[ij] << " ";
                }
                Rcout << " " << endl;
            }
            halves=0;
            while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
//                Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_a[ij] + dbeta[ij];
                    beta_c[ij] = beta_0[ij];
                }
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
                // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

                Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
                Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
            }
            if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
                if (verbose>=4){
                    Rcout << "C++ Note: Changing back to best" <<endl;
                }
                // If it goes through every half step without improvement, then the maximum change needs to be decreased
                abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
                dose_abs_max = dose_abs_max*pow(0.5,halfmax);
                iter_check = 1;
                //
                beta_p = beta_best;//
                beta_a = beta_best;//
                beta_c = beta_best;//
//                Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_best[ij];
                }
                Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
            }
            if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                iter_check=0;
                if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                    iter_stop = 1;
                }
            }
        }
        // -----------------------------------------------
        // Performing Full Calculation to get full second derivative matrix
        // -----------------------------------------------
        fill(Ll.begin(), Ll.end(), 0.0);
        if (!single_bool){
            fill(Lld.begin(), Lld.end(), 0.0);
            fill(Lldd.begin(), Lldd.end(), 0.0);
        }
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        //
        a_n = beta_0;
        if (verbose>=4){
            Rcout << "C++ Note: ending guess " << guess << endl;
        }
        //
        beta_fin(guess, _) = a_n;
        LL_fin[guess] = Ll[0];
        //
    }
    if (verbose>=4){
        Rcout << "C++ Note: Refreshing matrices after all guesses" << endl;
    }
    //
//    Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    fill(Ll.begin(), Ll.end(), 0.0);
    if (!single_bool){
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
    }
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    iter_stop = 0;
    halves=0;
    iteration=0;
    halves = 0; //number of half-steps taken
    ind0 = fir; //used for validations
    iteration=0; //iteration number
    //
    convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    iter_check=0; //signal to check for convergence
    //
    // next we need to figure out what point to start at
    best_guess = -1;
    for (int guess=guesses-1; guess>-1;guess--){
        // we want to find the closest point at which the loglikelihood is below lstar
        if (LL_fin[guess] < Lstar){
            best_guess = guess;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Lower Guess Results" << endl;
        Rcout << "Guess number, parameter values, Log-Likelihood change" << endl;
        NumericVector beta_temp;
        for (int i=0; i<guesses; i++){
            beta_temp = wrap(beta_fin.row(i));
            if (i==best_guess){
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] - Lstar << "<-- Best Guess" << endl;
            } else {
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] - Lstar << endl;
            }
        }
    }
    if (best_guess == 0){
        beta_temp = wrap(beta_fin.row(best_guess)); // the first point was closest, no lower bound
    } else if (best_guess == -1){
        beta_temp = wrap(beta_fin.row(guesses-1)); // the last point was closest, no upper bound
    } else {
        beta_temp = wrap(beta_fin.row(best_guess));
        beta_temp0 = wrap(beta_fin.row(best_guess-1));
        for (int i=0;i<beta_temp.size();i++){
            beta_temp[i] = beta_temp[i] + beta_temp0[i];
            beta_temp[i] = beta_temp[i]/2;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Initial Guess: ";
        for (int i=0;i<beta_0.size();i++){
            Rcout << beta_temp[i] << " ";
        }
        Rcout << " " << endl;
    }
    for (int i=0;i<beta_0.size();i++){
        a_n[i] = beta_temp[i];
        beta_0[i] = a_n[i];
        beta_c[i] = beta_0[i];
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: starting subterms after in best guess" << endl;
    }
    //
    // Calculates the subterm and term values
    Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    // Calculates the side sum terms used
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Lldd_mat = as<Map<MatrixXd> >(Lldd_vec);
    Lld_vec = as<Map<VectorXd> >(Lld_vecc);
    step = 1;
    iter_continue = true;
    max_change = 100;
    while ((step<maxstep)&&(iter_continue)){
         step++;
         trouble = false;
         Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Ll[0], para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, step, dbeta, beta_0, upper, trouble, verbose, mult);
         if (trouble){
             Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant_trouble, debugging);
         }
         //
         beta_p = beta_c;//
         beta_a = beta_c;//

//         Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
         Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
         //
         for (int ijk=0;ijk<totalnum;ijk++){
             if (KeepConstant[ijk]==0){
//                 int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                 //
                 if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){ //the threshold values use different maximum deviation values
                     if (abs(dbeta[ijk])>dose_abs_max){
                         dbeta[ijk] = dose_abs_max * sign(dbeta[ijk]);
                     }
                 }else{
                     if (abs(dbeta[ijk])>abs_max){
                         dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                     }
                 }
             } else {
                 dbeta[ijk]=0;
             }
         }
         //
         max_change = abs(dbeta[0]);

 //        Rcout << "Change: ";
         for (int ij=0;ij<totalnum;ij++){
//            Rcout << dbeta[ij] << " ";
            if (ij==para_number){
                // we want to prevent two issues
                // first prevent the parameter estimate from crossing the optimum
                // issue is beta_0[para_number] <= beta_best[para_number]
                if (dbeta[ij] > (beta_best[para_number] - beta_a[ij])/lr){
                    dbeta[ij] = -0.5*dbeta[ij];
                }
                // second issue is making sure that the step is forced away from the optimum when possible
                if (Ll[0] > Lstar){
                    // If the log-likelihood is above the goal, then it must move away from the optimum point
                    // for lower limit, the step is always positive
                    dbeta[ij] = -1*abs(dbeta[ij]);
                }
            }
            if (abs(dbeta[ij])>max_change){
                max_change = abs(dbeta[ij]);
            }
            beta_0[ij] = beta_a[ij] + lr*dbeta[ij];
            beta_c[ij] = beta_0[ij];
        }
 //        Rcout << " " << endl;
         // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
         // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
         // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

         Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
         bound_val = 0;
         Cox_Pois_Log_Loop(abs_max, basic_bool, beta_0, beta_a, beta_c, bound_val, cox_bool, dbeta, debugging, df0, dfc, dint, Dose, dose_abs_max, dslp, fir, gmix_term, gmix_theta, half_check, halfmax, KeepConstant, limit_hit, lr, modelform, nonDose, nonDose_LIN, nonDose_LOGLIN, nonDose_PLIN, nthreads, R, Rd, Rdd, RddR, RdR, s_weights, single_bool, start, strata_bool, T0, Td0, Tdd0, Te, term_n, term_tot, tform, totalnum, TTerm, verbose);
         if (limit_hit[0]){
             limits[0] = 0;
             ll_final[0] = 0;
             break;
         }
         Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
         //
         if (verbose>=4){
             end_point = system_clock::now();
             ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
             Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
             gibtime = system_clock::to_time_t(system_clock::now());
             Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
             Rcout << "C++ Note: df101 ";//prints the log-likelihoods
             for (int ij=0;ij<reqrdnum;ij++){
                 Rcout << Ll[ij] << " ";
             }
             Rcout << " " << endl;
             Rcout << "C++ Note: df104 ";//prints parameter values
             for (int ij=0;ij<totalnum;ij++){
                 Rcout << beta_0[ij] << " ";
             }
             Rcout << " " << endl;
         }
         //
         #ifdef _OPENMP
         #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
         #endif
         for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
             int ij = 0;
             int jk = ijk;
             while (jk>ij){
                 ij++;
                 jk-=ij;
             }
             Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
             Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
             if (ij==jk){
                 Lld_vecc[ij] = Lld[ij];
             }
         }
         Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
         Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
         Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
         limits[0] = beta_0[para_number];
         ll_final[0] = Ll[0];
         if ((max_change < epsilon)&&(deriv_max < deriv_epsilon)){
            iter_continue = false;
        }
     }
    //
    res_list = List::create(_["Parameter_Limits"]=wrap(limits), _["Negative_Limit_Found"]=wrap(limit_hit), _["Likelihood_Boundary"]=wrap(ll_final), _["Likelihood_Goal"]=wrap(Lstar), _["Status"]="PASSED");
    // returns a list of results
    return res_list;
}

//' Primary Poisson likelihood bounds calculation function.
//'
//' \code{LogLik_Poisson_Omnibus_Log_Bound} Performs the calls to calculation functions and log-likeihood profile bounds
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List LogLik_Poisson_Omnibus_Log_Bound( const MatrixXd& PyrC, const MatrixXd& dfs, IntegerVector term_n, StringVector tform, NumericVector a_n, NumericMatrix& x_all,IntegerVector dfc,int fir,string modelform, double lr, NumericVector maxiters, int guesses, int halfmax, double epsilon,  double abs_max,double dose_abs_max, double deriv_epsilon, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, int nthreads,  bool strata_bool, bool single_bool, bool constraint_bool, const double gmix_theta, const IntegerVector gmix_term, const MatrixXd Lin_Sys, const VectorXd Lin_Res, double qchi, int para_number, int maxstep, double mult){
    ;
    //
    List temp_list = List::create(_["Status"]="TEMP"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_POIS_BOUNDS" << endl;
    }
    time_point<system_clock> start_point, end_point;
    start_point = system_clock::now();
    auto start = time_point_cast<microseconds>(start_point).time_since_epoch().count();
    end_point = system_clock::now();
    auto ending = time_point_cast<microseconds>(end_point).time_since_epoch().count(); //The time duration is tracked

    //
    auto gibtime = system_clock::to_time_t(system_clock::now());
    if (verbose>=4){
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    // Time durations are measured from this point on in microseconds
    //
    // df0: covariate data
    // totalnum: number of terms used
    //
    // ------------------------------------------------------------------------- // initialize
	const Map<MatrixXd> df0(as<Map<MatrixXd> >(x_all));
    int totalnum;
    int reqrdnum;
    // ------------------------------------------------------------------------- // initialize
    if (single_bool){
        if (verbose>=1){
            Rcout << "non-derivative model calculation is not compatable with log-based bound calculation" << endl;
        }
        temp_list = List::create(_["Status"]="FAILED",_["LogLik"]=R_NaN);
        return temp_list;
    }
	totalnum = term_n.size();
	reqrdnum = totalnum - sum(KeepConstant);
	if (verbose>=4){
        Rcout << "C++ Note: Term checked ";
        for (int ij=0;ij<totalnum;ij++){
            Rcout << term_n[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    // cout.precision: controls the number of significant digits printed
    // nthreads: number of threads used for parallel operations
    //
    Rcout.precision(7); //forces higher precision numbers printed to terminal
    //
    // Lld_worst: stores the highest magnitude log-likelihood derivative
    //
    //
//    double Lld_worst = 0.0; //stores derivative value used to determine if every parameter is near convergence
    //
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    // ------------------------------------------------------------------------- // initialize
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    MatrixXd T0;
    MatrixXd Td0;
	MatrixXd Tdd0;
	//
	MatrixXd Te;
	MatrixXd R;
	ColXd Rd;
	ColXd Rdd;
	//
	MatrixXd Dose;
	MatrixXd nonDose;
	MatrixXd nonDose_LIN;
	MatrixXd nonDose_PLIN;
	MatrixXd nonDose_LOGLIN;
	MatrixXd TTerm;
	double dint; //The amount of change used to calculate derivatives in threshold paramters
	double dslp;
    ColXd RdR;
	ColXd RddR;
	// ------------------------------------------------------------------------- // initialize
	if (verbose>=4){
		end_point = system_clock::now();
		ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
		Rcout << "C++ Note: df99," << (ending-start) << ",Starting" <<endl;
		gibtime = system_clock::to_time_t(system_clock::now());
		Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
	}
	// ---------------------------------------------
	// To Start, needs to seperate the derivative terms
	// ---------------------------------------------
	//
	T0 = MatrixXd::Zero(df0.rows(), totalnum); //preallocates matrix for Term column
    Te = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for column terms used for temporary storage
	R = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for Risks
	//
	Dose = MatrixXd::Constant(df0.rows(),term_tot,0.0); //Matrix of the total dose term values
	nonDose = MatrixXd::Constant(df0.rows(),term_tot,1.0); //Matrix of the total non-dose term values
	nonDose_LIN = MatrixXd::Constant(df0.rows(),term_tot,0.0); //matrix of Linear subterm values
	nonDose_PLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Loglinear subterm values
	nonDose_LOGLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Product linear subterm values
	TTerm = MatrixXd::Zero(Dose.rows(),Dose.cols()); //matrix of term values
	if (single_bool){
		//
		;
    } else {
		Td0 = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Term derivative columns
		Tdd0 = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Term second derivative columns
		//
		Rd = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Risk derivatives
		Rdd = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Risk second derivatives
		//
		dint = dose_abs_max; //The amount of change used to calculate derivatives in threshold paramters
		dslp = abs_max;
        RdR = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Risk to derivative ratios
		RddR = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Risk to second derivative ratios
    }
    VectorXd s_weights;
    if (strata_bool){
        s_weights = VectorXd::Zero(df0.rows());
        Gen_Strat_Weight(modelform, dfs, PyrC, s_weights, nthreads, tform, term_n, term_tot);
    }
    // ------------------------------------------------------------------------- // initialize
    vector<double> Ll(reqrdnum,0.0); //Log-likelihood values
	vector<double> Lld(reqrdnum,0.0); //Log-likelihood derivative values
	vector<double> Lldd(pow(reqrdnum,2),0.0);//The second derivative matrix has room for every combination, but only the lower triangle is calculated initially
	MatrixXd dev_temp = MatrixXd::Zero(PyrC.rows(),2);
    double dev = 0;
    //The log-likelihood is calculated in parallel over the risk groups
    vector <double> Ll_comp(2,Ll[0]); //vector to compare values
    double abs_max0 = abs_max;
    double dose_abs_max0 = dose_abs_max;
    //
    vector<double> dbeta(totalnum,0.0);
    //
    // --------------------------
    // always starts from initial guess
    // --------------------------
    vector<double> beta_c(totalnum,0.0);
    vector<double> beta_a(totalnum,0.0);
    vector<double> beta_best(totalnum,0.0);
    vector<double> beta_p(totalnum,0.0);
    VectorXd::Map(&beta_p[0], beta_0.size()) = beta_0;// stores previous parameters
    VectorXd::Map(&beta_c[0], beta_0.size()) = beta_0;// stores current parameters
    VectorXd::Map(&beta_a[0], beta_0.size()) = beta_0;// stores a refrence value for parameters
    VectorXd::Map(&beta_best[0], beta_0.size()) = beta_0;// stores the best parameters
    // double halves = 0; //number of half-steps taken
    // int ind0 = fir; //used for validations
//    int iteration=0; //iteration number
    //
//    bool convgd = FALSE;
    int iter_stop =0; //tracks if the iterations should be stopped for convergence
    // int iter_check=0; //signal to check for convergence
    //
    //
    // double Ll_abs_best = 10;
    vector<double> beta_abs_best(totalnum,0.0);
    // int guess_abs_best =-1;
    //
    fill(Ll.begin(), Ll.end(), 0.0);
    fill(Lld.begin(), Lld.end(), 0.0);
    fill(Lldd.begin(), Lldd.end(), 0.0);
    
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    //
    for (int i=0;i<beta_0.size();i++){
        beta_0[i] = a_n[i];
    }
    Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
    //
    //
    NumericVector Lldd_vec(reqrdnum * reqrdnum);
    NumericVector Lld_vecc(reqrdnum);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
    Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
//    double qchi = 3.841459;
    double Lstar = Ll[0]-qchi;
//    int para_number = 0;
    bool upper = true;
    int half_check = 0;
    bool trouble = false;
    IntegerVector KeepConstant_trouble (totalnum);
    for (int i=0; i<totalnum;i++){
        KeepConstant_trouble[i] = KeepConstant[i];
    }
    KeepConstant_trouble[para_number] = 1;
//    int maxstep=10;
    //
    vector<double> limits(2,0.0);
    vector<bool>   limit_hit(2, FALSE);
    vector<double> ll_final(2,0.0);
    List res_list;
    if (verbose>=4){
        Rcout << "C++ Note: STARTING BOUNDS" << endl;
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: STARTING Upper Bound" << endl;
    }
    upper = true;
    int step = 0;
    bool iter_continue = true;
    double max_change = 100;
    double deriv_max = 100;
    ///
    // variables added for log loop code
    ///
    bool basic_bool = false;
    bool cox_bool = true;
    int bound_val = 1;
    //
    while ((step<maxstep)&&(iter_continue)){
        step++;
        //
        trouble = false;
        half_check = 0;
        Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Ll[0], para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, step, dbeta, beta_0, upper, trouble, verbose, mult);
        if (trouble){
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant_trouble, debugging);
        }
        //
        beta_p = beta_c;//
        beta_a = beta_c;//

        //
        for (int ijk=0;ijk<totalnum;ijk++){
            if (KeepConstant[ijk]==0){
//                int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                //
                if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){ //the threshold values use different maximum deviation values
                    if (abs(dbeta[ijk])>dose_abs_max){
                        dbeta[ijk] = dose_abs_max * sign(dbeta[ijk]);
                    }
                }else{
                    if (abs(dbeta[ijk])>abs_max){
                        dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                    }
                }
            } else {
                dbeta[ijk]=0;
            }
        }
        //
//        Rcout << "Change: ";
        max_change = abs(dbeta[0]);
        for (int ij=0;ij<totalnum;ij++){
            if (ij==para_number){
                // we want to prevent two issues
                // first prevent the parameter estimate from crossing the optimum
                // issue is beta_0[para_number] <= beta_best[para_number]
                if (dbeta[ij] < (beta_best[para_number] - beta_a[ij])/lr){
                    dbeta[ij] = -0.5*dbeta[ij];
                }
                // second issue is making sure that the step is forced away from the optimum when possible
                if (Ll[0] > Lstar){
                    // If the log-likelihood is above the goal, then it must move away from the optimum point
                    // for upper limit, the step is always positive
                    dbeta[ij] = abs(dbeta[ij]);
                }
            }
            if (abs(dbeta[ij])>max_change){
                max_change = abs(dbeta[ij]);
            }
            beta_0[ij] = beta_a[ij] + lr*dbeta[ij];
            beta_c[ij] = beta_0[ij];
//            Rcout << dbeta[ij] << " ";
        }
//        Rcout << " " << endl;
        // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
        // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
        // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

        Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
        bound_val = 1;
        Cox_Pois_Log_Loop(abs_max, basic_bool, beta_0, beta_a, beta_c, bound_val, cox_bool, dbeta, debugging, df0, dfc, dint, Dose, dose_abs_max, dslp, fir, gmix_term, gmix_theta, half_check, halfmax, KeepConstant, limit_hit, lr, modelform, nonDose, nonDose_LIN, nonDose_LOGLIN, nonDose_PLIN, nthreads, R, Rd, Rdd, RddR, RdR, s_weights, single_bool, start, strata_bool, T0, Td0, Tdd0, Te, term_n, term_tot, tform, totalnum, TTerm, verbose);
        if (limit_hit[1]){
            limits[1] = 0;
            ll_final[1] = 0;
            break;
        }
        Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
        //
        if (verbose>=4){
            end_point = system_clock::now();
            ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
            Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
            gibtime = system_clock::to_time_t(system_clock::now());
            Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
            Rcout << "C++ Note: df101 ";//prints the log-likelihoods
            for (int ij=0;ij<reqrdnum;ij++){
                Rcout << Ll[ij] << " ";
            }
            Rcout << " " << endl;
            Rcout << "C++ Note: df104 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_0[ij] << " ";
            }
            Rcout << " " << endl;
        }
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
            Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
            if (ij==jk){
                Lld_vecc[ij] = Lld[ij];
            }
        }
        Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
        Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
        Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
        limits[1] = beta_0[para_number];
        ll_final[1] = Ll[0];
        // if (max_change < epsilon){
        //     iter_continue = false;
        // } else if (deriv_max < deriv_epsilon){
        //     iter_continue = false;
        // }
        if ((max_change < epsilon)&&(deriv_max < deriv_epsilon)){
            iter_continue = false;
        }
        // Rcout << "df505 " << beta_0[para_number] << " " << max_change << " " << deriv_max << endl;
    }
    //
    //
    if (verbose>=4){
        Rcout << "C++ Note: STARTING Lower Bound" << endl;
    }
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    for (int ij=0;ij<totalnum;ij++){
        beta_0[ij] = beta_a[ij];
    }
    //
    Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Lldd_mat = as<Map<MatrixXd> >(Lldd_vec);
    Lld_vec = as<Map<VectorXd> >(Lld_vecc);
    //
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    //
    upper = false;
    step = 0;
    iter_continue = true;
    while ((step<maxstep)&&(iter_continue)){
        step++;
        trouble = false;
        half_check = 0;
        Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Ll[0], para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, step, dbeta, beta_0, upper, trouble, verbose, mult);
        if (trouble){
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant_trouble, debugging);
        }
        //
        beta_p = beta_c;//
        beta_a = beta_c;//
        //
        for (int ijk=0;ijk<totalnum;ijk++){
            if (KeepConstant[ijk]==0){
//                int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                //
                if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){ //the threshold values use different maximum deviation values
                    if (abs(dbeta[ijk])>dose_abs_max){
                        dbeta[ijk] = dose_abs_max * sign(dbeta[ijk]);
                    }
                }else{
                    if (abs(dbeta[ijk])>abs_max){
                        dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                    }
                }
            } else {
                dbeta[ijk]=0;
            }
        }
        //
        max_change = abs(dbeta[0]);
//        Rcout << "Change: ";
        for (int ij=0;ij<totalnum;ij++){
//            Rcout << dbeta[ij] << " ";
            if (ij==para_number){
                // we want to prevent two issues
                // first prevent the parameter estimate from crossing the optimum
                // issue is beta_0[para_number] <= beta_best[para_number]
                if (dbeta[ij] > (beta_best[para_number] - beta_a[ij])/lr){
                    dbeta[ij] = -0.5*dbeta[ij];
                }
                // second issue is making sure that the step is forced away from the optimum when possible
                if (Ll[0] > Lstar){
                    // If the log-likelihood is above the goal, then it must move away from the optimum point
                    // for lower limit, the step is always positive
                    dbeta[ij] = -1*abs(dbeta[ij]);
                }
            }
            if (abs(dbeta[ij])>max_change){
                max_change = abs(dbeta[ij]);
            }
            beta_0[ij] = beta_a[ij] + lr*dbeta[ij];
            beta_c[ij] = beta_0[ij];
        }
//        Rcout << " " << endl;
        // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
        // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
        // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

        Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
        bound_val = 0;
        Cox_Pois_Log_Loop(abs_max, basic_bool, beta_0, beta_a, beta_c, bound_val, cox_bool, dbeta, debugging, df0, dfc, dint, Dose, dose_abs_max, dslp, fir, gmix_term, gmix_theta, half_check, halfmax, KeepConstant, limit_hit, lr, modelform, nonDose, nonDose_LIN, nonDose_LOGLIN, nonDose_PLIN, nthreads, R, Rd, Rdd, RddR, RdR, s_weights, single_bool, start, strata_bool, T0, Td0, Tdd0, Te, term_n, term_tot, tform, totalnum, TTerm, verbose);
        if (limit_hit[0]){
            limits[0] = 0;
            ll_final[0] = 0;
            break;
        }
        Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
        //
        if (verbose>=4){
            end_point = system_clock::now();
            ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
            Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
            gibtime = system_clock::to_time_t(system_clock::now());
            Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
            Rcout << "C++ Note: df101 ";//prints the log-likelihoods
            for (int ij=0;ij<reqrdnum;ij++){
                Rcout << Ll[ij] << " ";
            }
            Rcout << " " << endl;
            Rcout << "C++ Note: df104 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_0[ij] << " ";
            }
            Rcout << " " << endl;
        }
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
            Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
            if (ij==jk){
                Lld_vecc[ij] = Lld[ij];
            }
        }
        Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
        Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
        Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
        limits[0] = beta_0[para_number];
        ll_final[0] = Ll[0];
        // if (max_change < epsilon){
        //     iter_continue = false;
        // } else if (deriv_max < deriv_epsilon){
        //     iter_continue = false;
        // }
        if ((max_change < epsilon)&&(deriv_max < deriv_epsilon)){
            iter_continue = false;
        }
        // Rcout << "df505 " << beta_0[para_number] << " " << max_change << " " << deriv_max << endl;
    }
    //
    res_list = List::create(_["Parameter_Limits"]=wrap(limits), _["Negative_Limit_Found"]=wrap(limit_hit), _["Likelihood_Boundary"]=wrap(ll_final), _["Likelihood_Goal"]=wrap(Lstar), _["Status"]="PASSED");
    // returns a list of results
    return res_list;
}

//' Primary Poisson likelihood bounds calculation function.
//'
//' \code{LogLik_Poisson_Omnibus_Log_Bound_Search} Performs the calls to calculation functions and log-likeihood profile bounds
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List LogLik_Poisson_Omnibus_Log_Bound_Search(const MatrixXd& PyrC, const MatrixXd& dfs, IntegerVector term_n, StringVector tform, NumericVector a_n, NumericMatrix& x_all,IntegerVector dfc,int fir,string modelform, double lr, NumericVector maxiters, int guesses, int halfmax, double epsilon,  double abs_max,double dose_abs_max, double deriv_epsilon, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, int nthreads,  bool strata_bool, bool single_bool, bool constraint_bool, const double gmix_theta, const IntegerVector gmix_term, const MatrixXd Lin_Sys, const VectorXd Lin_Res, double qchi, int para_number, int maxstep, double mult){
    ;
    //
    List temp_list = List::create(_["Status"]="TEMP"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_POIS_BOUNDS_SEARCH" << endl;
    }
    time_point<system_clock> start_point, end_point;
    start_point = system_clock::now();
    auto start = time_point_cast<microseconds>(start_point).time_since_epoch().count();
    end_point = system_clock::now();
    auto ending = time_point_cast<microseconds>(end_point).time_since_epoch().count(); //The time duration is tracked

    //
    auto gibtime = system_clock::to_time_t(system_clock::now());
    if (verbose>=4){
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    // Time durations are measured from this point on in microseconds
    //
    // df0: covariate data
    // ntime: number of event times for Cox PH
    // totalnum: number of terms used
    //
    // ------------------------------------------------------------------------- // initialize
	const Map<MatrixXd> df0(as<Map<MatrixXd> >(x_all));
    int totalnum;
    int reqrdnum;
    // ------------------------------------------------------------------------- // initialize
    if (single_bool){
        if (verbose>=1){
            Rcout << "non-derivative model calculation is not compatable with log-based bound calculation" << endl;
        }
        temp_list = List::create(_["Status"]="FAILED",_["LogLik"]=R_NaN);
        return temp_list;
    }
	totalnum = term_n.size();
	reqrdnum = totalnum - sum(KeepConstant);
	if (verbose>=4){
        Rcout << "C++ Note: Term checked ";
        for (int ij=0;ij<totalnum;ij++){
            Rcout << term_n[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    // cout.precision: controls the number of significant digits printed
    // nthreads: number of threads used for parallel operations
    //
    Rcout.precision(7); //forces higher precision numbers printed to terminal
    //
    // Lld_worst: stores the highest magnitude log-likelihood derivative
    //
    //
//    double Lld_worst = 0.0; //stores derivative value used to determine if every parameter is near convergence
    //
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    // ------------------------------------------------------------------------- // initialize
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    VectorXd beta_max = beta_0;
    MatrixXd T0;
    MatrixXd Td0;
	MatrixXd Tdd0;
	//
	MatrixXd Te;
	MatrixXd R;
	ColXd Rd;
	ColXd Rdd;
	//
	MatrixXd Dose;
	MatrixXd nonDose;
	MatrixXd nonDose_LIN;
	MatrixXd nonDose_PLIN;
	MatrixXd nonDose_LOGLIN;
	MatrixXd TTerm;
	double dint; //The amount of change used to calculate derivatives in threshold paramters
	double dslp;
    ColXd RdR;
	ColXd RddR;
	// ------------------------------------------------------------------------- // initialize
	if (verbose>=4){
		end_point = system_clock::now();
		ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
		Rcout << "C++ Note: df99," << (ending-start) << ",Starting" <<endl;
		gibtime = system_clock::to_time_t(system_clock::now());
		Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
	}
	// ---------------------------------------------
	// To Start, needs to seperate the derivative terms
	// ---------------------------------------------
	//
	T0 = MatrixXd::Zero(df0.rows(), totalnum); //preallocates matrix for Term column
    Te = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for column terms used for temporary storage
	R = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for Risks
	//
	Dose = MatrixXd::Constant(df0.rows(),term_tot,0.0); //Matrix of the total dose term values
	nonDose = MatrixXd::Constant(df0.rows(),term_tot,1.0); //Matrix of the total non-dose term values
	nonDose_LIN = MatrixXd::Constant(df0.rows(),term_tot,0.0); //matrix of Linear subterm values
	nonDose_PLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Loglinear subterm values
	nonDose_LOGLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Product linear subterm values
	TTerm = MatrixXd::Zero(Dose.rows(),Dose.cols()); //matrix of term values
	if (single_bool){
		//
		;
    } else {
		Td0 = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Term derivative columns
		Tdd0 = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Term second derivative columns
		//
		Rd = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Risk derivatives
		Rdd = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Risk second derivatives
		//
		dint = dose_abs_max; //The amount of change used to calculate derivatives in threshold paramters
		dslp = abs_max;
        RdR = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Risk to derivative ratios
		RddR = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Risk to second derivative ratios
    }
    VectorXd s_weights;
    if (strata_bool){
        s_weights = VectorXd::Zero(df0.rows());
        Gen_Strat_Weight(modelform, dfs, PyrC, s_weights, nthreads, tform, term_n, term_tot);
    }
    // ------------------------------------------------------------------------- // initialize
    MatrixXd Rls1;
	MatrixXd Lls1;
	MatrixXd Rls2;
	MatrixXd Rls3;
	MatrixXd Lls2;
	MatrixXd Lls3;
	vector<double> Ll(reqrdnum,0.0); //Log-likelihood values
	vector<double> Lld(reqrdnum,0.0); //Log-likelihood derivative values
	vector<double> Lldd(pow(reqrdnum,2),0.0);//The second derivative matrix has room for every combination, but only the lower triangle is calculated initially
    MatrixXd dev_temp = MatrixXd::Zero(PyrC.rows(),2);
    double dev = 0;
    // ------------------------------------------------------------------------- // initialize
    //The log-likelihood is calculated in parallel over the risk groups
    vector <double> Ll_comp(2,Ll[0]); //vector to compare values
    double abs_max0 = abs_max;
    double dose_abs_max0 = dose_abs_max;
    //
    vector<double> dbeta(totalnum,0.0);
    vector<double> dbeta_start(totalnum,0.0);
    //
    // --------------------------
    // always starts from initial guess
    // --------------------------
    vector<double> beta_c(totalnum,0.0);
    vector<double> beta_a(totalnum,0.0);
    vector<double> beta_best(totalnum,0.0);
    vector<double> beta_p(totalnum,0.0);
    vector<double> beta_peak(totalnum, 0.0);
    VectorXd::Map(&beta_p[0], beta_0.size()) = beta_0;// stores previous parameters
    VectorXd::Map(&beta_c[0], beta_0.size()) = beta_0;// stores current parameters
    VectorXd::Map(&beta_a[0], beta_0.size()) = beta_0;// stores a refrence value for parameters
    VectorXd::Map(&beta_best[0], beta_0.size()) = beta_0;// stores the best parameters
    VectorXd::Map(&beta_peak[0], beta_0.size()) = beta_0;// stores the peak parameters
    // double halves = 0; //number of half-steps taken
    // int ind0 = fir; //used for validations
//    int iteration=0; //iteration number
    //
//    bool convgd = FALSE;
    int iter_stop =0; //tracks if the iterations should be stopped for convergence
    // int iter_check=0; //signal to check for convergence
    //
    //
    // double Ll_abs_best = 10;
    vector<double> beta_abs_best(totalnum,0.0);
    // int guess_abs_best =-1;
    //
    fill(Ll.begin(), Ll.end(), 0.0);
    fill(Lld.begin(), Lld.end(), 0.0);
    fill(Lldd.begin(), Lldd.end(), 0.0);
    
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    //
    for (int i=0;i<beta_0.size();i++){
        beta_0[i] = a_n[i];
    }
    Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
    //
    //
    NumericVector Lldd_vec(reqrdnum * reqrdnum);
    NumericVector Lld_vecc(reqrdnum);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
    Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
//    double qchi = 3.841459;
    double Lstar = Ll[0]-qchi;
    double Lmax = Ll[0];
//    int para_number = 0;
    bool upper = true;
    int half_check = 0;
    bool trouble = false;
    double deriv_max = 100;
    //
    // int guesses = 5;
//    double mult = 1.5;
//    int guess_inters = 10;
    //
    IntegerVector KeepConstant_trouble (totalnum);
    for (int i=0; i<totalnum;i++){
        KeepConstant_trouble[i] = KeepConstant[i];
    }
    KeepConstant_trouble[para_number] = 1;
//    int maxstep=10;
    //
    vector<double> limits(2,0.0);
    vector<bool>   limit_hit(2, FALSE);
    vector<double> ll_final(2,0.0);
    List res_list;
    if (verbose>=4){
        Rcout << "C++ Note: STARTING BOUNDS" << endl;
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: STARTING Upper Bound" << endl;
    }
    upper = true;
    // Now define the list of points to check
    trouble = false;
    Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Lmax, para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, 0, dbeta_start, beta_0, upper, trouble, verbose, mult);
    NumericMatrix a_ns(guesses, totalnum);
    // now the dbeta holds the range to check
    // note that currently the guesses are not bounded
    for (int i=0;i<guesses;i++){
        for (int j=0;j<totalnum;j++){
            // use dbeta to assign a_n guess i, parameter j
            // assign such that i=guesses-1 gives mult*dbeta
            a_ns(i, j) = beta_0[j] + dbeta_start[j] * ( i+1)/(guesses);
        }
    }
    if (verbose>=4){
        for (int i=0;i<guesses;i++){
            Rcout << "C++ Note: Initial guess " << i << ": ";
            for (int j=0;j<totalnum;j++){
                Rcout << a_ns(i, j) << " ";
            }
            Rcout << " " << endl;
        }
    }
    // now we have the points to test
    double halves = 0; //number of half-steps taken
    int ind0 = fir; //used for validations
    int iteration=0; //iteration number
    int maxiter=0;
//    double Lld_worst = 0.0;
    //
    bool convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    int iter_check=0; //signal to check for convergence
    //
    NumericMatrix beta_fin(a_ns.rows(), a_ns.cols());
    NumericVector LL_fin(a_ns.rows());
    //
    double Ll_abs_best = 10;
    // vector<double> beta_abs_best(totalnum,0.0);
    int guess_abs_best =-1;
    // Variables that are used for the risk check function shared across cox, poisson, and log bound functions
    bool basic_bool = false;
    VectorXd cens_weight(1);
    bool change_all = false;
    bool cox_bool = false;
    bool CR_bool = false;
//    MatrixXd Lls1 = MatrixXd::Zero(1, 1);
//    MatrixXd Lls2 = MatrixXd::Zero(1, 1);
//    MatrixXd Lls3 = MatrixXd::Zero(1, 1);
    bool log_bound_bool = true;
    int ntime = 1.0;
    IntegerMatrix RiskFail(1);
    vector<string> RiskGroup = {"temp"};
    StringMatrix RiskGroup_Strata(1);
//    MatrixXd Rls1 = MatrixXd::Zero(1, 1);
//    MatrixXd Rls2 = MatrixXd::Zero(1, 1);
//    MatrixXd Rls3 = MatrixXd::Zero(1, 1);
    NumericVector STRATA_vals(1);
    string ties_method = "temp";
    ///
    for (int guess=0; guess <guesses; guess++){
        fill(Ll.begin(), Ll.end(), 0.0);
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
        beta_p = beta_best;//
        beta_a = beta_best;//
        beta_c = beta_best;//
        abs_max = abs_max0;
        dose_abs_max = dose_abs_max0;
        iter_stop = 0;
        halves=0;
        iteration=0;
        halves = 0; //number of half-steps taken
        ind0 = fir; //used for validations
        iteration=0; //iteration number
        //
        convgd = FALSE;
        iter_stop =0; //tracks if the iterations should be stopped for convergence
        iter_check=0; //signal to check for convergence
        //
        maxiter = maxiters[guess];
        a_n = a_ns.row(guess);
        for (int i=0;i<beta_0.size();i++){
            beta_0[i] = a_n[i];
        }
        if (verbose>=4){
            Rcout << "C++ Note: starting subterms " << term_tot << " in guess " << guess << endl;
        }
        Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
        if ((R.minCoeff()<=0)||(R.hasNaN())){
            if (verbose>=4){
			    Rcout << "C++ Error: A non-positive risk was detected: " << R.minCoeff() << endl;
			    Rcout << "C++ Warning: final failing values ";
			    for (int ijk=0;ijk<totalnum;ijk++){
				    Rcout << beta_0[ijk] << " ";
			    }
			}
            iter_stop = 1;
            Ll[0] = 404; // note to consider this point too far
		} else {
            //
            // -------------------------------------------------------------------------------------------
            //
            Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
            //
            for (int i=0;i<beta_0.size();i++){
                beta_c[i] = beta_0[i];
            }
        }
        while ((iteration < maxiter)&&(iter_stop==0)){
            iteration++;
            beta_p = beta_c;//
            beta_a = beta_c;//
            beta_best = beta_c;//
            //
            // calculates the initial change in parameter
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant_trouble, debugging);
            Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
            if (verbose>=4){
                Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
            }
            //
            //
            if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
                Ll_abs_best = Ll[ind0];
                beta_abs_best = beta_c;
                guess_abs_best=guess;
            }
            //
            if (verbose>=4){
                Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
                Rcout << "C++ Note: df504 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_abs_best[ij] << " ";
                }
                Rcout << " " << endl;
            }
            halves=0;
            while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_a[ij] + dbeta[ij];
                    beta_c[ij] = beta_0[ij];
                }
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
                // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

                Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
                Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
            }
            if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
                if (verbose>=4){
                    Rcout << "C++ Note: Changing back to best" <<endl;
                }
                // If it goes through every half step without improvement, then the maximum change needs to be decreased
                abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
                dose_abs_max = dose_abs_max*pow(0.5,halfmax);
                iter_check = 1;
                //
                beta_p = beta_best;//
                beta_a = beta_best;//
                beta_c = beta_best;//
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_best[ij];
                }
                Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
            }
            if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                iter_check=0;
                if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                    iter_stop = 1;
                }
            }
        }
        // -----------------------------------------------
        // Performing Full Calculation to get full second derivative matrix
        // -----------------------------------------------
        fill(Ll.begin(), Ll.end(), 0.0);
        if (!single_bool){
            fill(Lld.begin(), Lld.end(), 0.0);
            fill(Lldd.begin(), Lldd.end(), 0.0);
        }
        Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
        //
        a_n = beta_0;
        if (verbose>=4){
            Rcout << "C++ Note: ending guess " << guess << endl;
        }
        //
        beta_fin(guess, _) = a_n;
        LL_fin[guess] = Ll[0];
        //
    }
    if (verbose>=4){
        Rcout << "C++ Note: Refreshing matrices after all guesses" << endl;
    }
    //
    fill(Ll.begin(), Ll.end(), 0.0);
    if (!single_bool){
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
    }
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    iter_stop = 0;
    halves=0;
    iteration=0;
    halves = 0; //number of half-steps taken
    ind0 = fir; //used for validations
    iteration=0; //iteration number
    //
    convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    iter_check=0; //signal to check for convergence
    //
//    int guess_max=guess_abs_best;
    // next we need to figure out what point to start at
    int best_guess = -1;
    for (int guess=guesses-1; guess>-1;guess--){
        // we want to find the closest point at which the loglikelihood is below lstar
        if (LL_fin[guess] < Lstar){
            best_guess = guess;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Upper Guess Results" << endl;
        Rcout << "Guess number, parameter values, Log-Likelihood change" << endl;
        NumericVector beta_temp;
        for (int i=0; i<guesses; i++){
            beta_temp = wrap(beta_fin.row(i));
            if (i==best_guess){
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] - Lstar << "<-- Best Guess" << endl;
            } else {
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] - Lstar << endl;
            }
        }
    }
    NumericVector beta_temp;
    NumericVector beta_temp0;
    if (best_guess == 0){
        beta_temp = wrap(beta_fin.row(best_guess)); // the first point was closest, no lower bound
    } else if (best_guess == -1){
        beta_temp = wrap(beta_fin.row(guesses-1)); // the last point was closest, no upper bound
    } else {
        beta_temp = wrap(beta_fin.row(best_guess));
        beta_temp0 = wrap(beta_fin.row(best_guess-1));
        for (int i=0;i<beta_temp.size();i++){
            beta_temp[i] = beta_temp[i] + beta_temp0[i];
            beta_temp[i] = beta_temp[i]/2;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Initial Guess: ";
        for (int i=0;i<beta_0.size();i++){
            Rcout << beta_temp[i] << " ";
        }
        Rcout << " " << endl;
    }
    for (int i=0;i<beta_0.size();i++){
        a_n[i] = beta_temp[i];
        beta_0[i] = a_n[i];
        beta_c[i] = beta_0[i];
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: starting subterms after in best guess" << endl;
    }
    //
    // Calculates the subterm and term values
    Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    // Calculates the side sum terms used
    Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Lldd_mat = as<Map<MatrixXd> >(Lldd_vec);
    Lld_vec = as<Map<VectorXd> >(Lld_vecc);
    int step = 1;
    bool iter_continue = true;
    double max_change = 100;
    ///
    // variables added for log loop code
    ///
    int bound_val = 1;
    //
    while ((step<maxstep)&&(iter_continue)){
        step++;
        trouble = false;
        Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Ll[0], para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, step, dbeta, beta_0, upper, trouble, verbose, mult);
        if (trouble){
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant_trouble, debugging);
        }
        //
        beta_p = beta_c;//
        beta_a = beta_c;//
         //
         for (int ijk=0;ijk<totalnum;ijk++){
             if (KeepConstant[ijk]==0){
//                 int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                 //
                 if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){ //the threshold values use different maximum deviation values
                     if (abs(dbeta[ijk])>dose_abs_max){
                         dbeta[ijk] = dose_abs_max * sign(dbeta[ijk]);
                     }
                 }else{
                     if (abs(dbeta[ijk])>abs_max){
                         dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                     }
                 }
             } else {
                 dbeta[ijk]=0;
             }
         }
         max_change = abs(dbeta[0]);
         //
 //        Rcout << "Change: ";
         for (int ij=0;ij<totalnum;ij++){
//            Rcout << dbeta[ij] << " ";
            if (ij==para_number){
                // we want to prevent two issues
                // first prevent the parameter estimate from crossing the optimum
                // issue is beta_0[para_number] <= beta_best[para_number]
                if (dbeta[ij] < (beta_best[para_number] - beta_a[ij])/lr){
                    dbeta[ij] = -0.5*dbeta[ij];
                }
                // second issue is making sure that the step is forced away from the optimum when possible
                if (Ll[0] > Lstar){
                    // If the log-likelihood is above the goal, then it must move away from the optimum point
                    // for upper limit, the step is always positive
                    dbeta[ij] = abs(dbeta[ij]);
                }
            }
            if (abs(dbeta[ij])>max_change){
                max_change = abs(dbeta[ij]);
            }
            beta_0[ij] = beta_a[ij] + lr*dbeta[ij];
            beta_c[ij] = beta_0[ij];
        }
 //        Rcout << " " << endl;
         // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
         // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
         // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

         Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
         bound_val = 1;
         Cox_Pois_Log_Loop(abs_max, basic_bool, beta_0, beta_a, beta_c, bound_val, cox_bool, dbeta, debugging, df0, dfc, dint, Dose, dose_abs_max, dslp, fir, gmix_term, gmix_theta, half_check, halfmax, KeepConstant, limit_hit, lr, modelform, nonDose, nonDose_LIN, nonDose_LOGLIN, nonDose_PLIN, nthreads, R, Rd, Rdd, RddR, RdR, s_weights, single_bool, start, strata_bool, T0, Td0, Tdd0, Te, term_n, term_tot, tform, totalnum, TTerm, verbose);
         if (limit_hit[1]){
             limits[1] = 0;
             ll_final[1] = 0;
             break;
         }
         Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
         //
         if (verbose>=4){
             end_point = system_clock::now();
             ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
             Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
             gibtime = system_clock::to_time_t(system_clock::now());
             Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
             Rcout << "C++ Note: df101 ";//prints the log-likelihoods
             for (int ij=0;ij<reqrdnum;ij++){
                 Rcout << Ll[ij] << " ";
             }
             Rcout << " " << endl;
             Rcout << "C++ Note: df104 ";//prints parameter values
             for (int ij=0;ij<totalnum;ij++){
                 Rcout << beta_0[ij] << " ";
             }
             Rcout << " " << endl;
         }
         //
         #ifdef _OPENMP
         #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
         #endif
         for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
             int ij = 0;
             int jk = ijk;
             while (jk>ij){
                 ij++;
                 jk-=ij;
             }
             Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
             Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
             if (ij==jk){
                 Lld_vecc[ij] = Lld[ij];
             }
         }
         Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
         Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
         Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
         limits[1] = beta_0[para_number];
         ll_final[1] = Ll[0];
         if ((max_change < epsilon)&&(deriv_max < deriv_epsilon)){
            iter_continue = false;
         }
     }
    // Now refresh matrices back to the maximum point
    //
    // -------------------------------------------------------------------------------------------
    //
    if (verbose>=4){
        Rcout << "C++ Note: STARTING Lower Bound" << endl;
    }
    beta_p = beta_peak;//
    beta_a = beta_peak;//
    beta_c = beta_peak;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    for (int ij=0;ij<totalnum;ij++){
        beta_0[ij] = beta_a[ij];
    }
    Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
    //
    upper = false;
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Lldd_mat = as<Map<MatrixXd> >(Lldd_vec);
    Lld_vec = as<Map<VectorXd> >(Lld_vecc);
    //
    trouble = false;
    Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Lmax, para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, 0, dbeta_start, beta_0, upper, trouble, verbose, mult);
    // now the dbeta holds the range to check
    // note that currently the guesses are not bounded
    for (int i=0;i<guesses;i++){
        for (int j=0;j<totalnum;j++){
            // use dbeta to assign a_n guess i, parameter j
            // assign such that i=guesses-1 gives mult*dbeta
            a_ns(i, j) = beta_0[j] + dbeta_start[j] * ( i+1)/(guesses);
        }
    }
    if (verbose>=4){
        for (int i=0;i<guesses;i++){
            Rcout << "C++ Note: Initial guess " << i << ": ";
            for (int j=0;j<totalnum;j++){
                Rcout << a_ns(i, j) << " ";
            }
            Rcout << " " << endl;
        }
    }
    // now we have the points to test
    halves = 0; //number of half-steps taken
    ind0 = fir; //used for validations
    iteration=0; //iteration number
    maxiter=0;
//    double Lld_worst = 0.0;
    //
    convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    iter_check=0; //signal to check for convergence
    //
    Ll_abs_best = 10;
    // vector<double> beta_abs_best(totalnum,0.0);
    guess_abs_best =-1;

    for (int guess=0; guess <guesses; guess++){
        fill(Ll.begin(), Ll.end(), 0.0);
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
        beta_p = beta_best;//
        beta_a = beta_best;//
        beta_c = beta_best;//
        abs_max = abs_max0;
        dose_abs_max = dose_abs_max0;
        iter_stop = 0;
        halves=0;
        iteration=0;
        halves = 0; //number of half-steps taken
        ind0 = fir; //used for validations
        iteration=0; //iteration number
        //
        convgd = FALSE;
        iter_stop =0; //tracks if the iterations should be stopped for convergence
        iter_check=0; //signal to check for convergence
        //
        maxiter = maxiters[guess];
        a_n = a_ns.row(guess);
        for (int i=0;i<beta_0.size();i++){
            beta_0[i] = a_n[i];
        }
        if (verbose>=4){
            Rcout << "C++ Note: starting subterms " << term_tot << " in guess " << guess << endl;
        }
        Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
        if ((R.minCoeff()<=0)||(R.hasNaN())){
            if (verbose>=4){
			    Rcout << "C++ Error: A non-positive risk was detected: " << R.minCoeff() << endl;
			    Rcout << "C++ Warning: final failing values ";
			    for (int ijk=0;ijk<totalnum;ijk++){
				    Rcout << beta_0[ijk] << " ";
			    }
			}
            iter_stop = 1;
            Ll[0] = 404; // note to consider this point too far
		} else {
            //
            // -------------------------------------------------------------------------------------------
            //
            Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
            //
            for (int i=0;i<beta_0.size();i++){
                beta_c[i] = beta_0[i];
            }
        }
        while ((iteration < maxiter)&&(iter_stop==0)){
            iteration++;
            beta_p = beta_c;//
            beta_a = beta_c;//
            beta_best = beta_c;//
            //
            // calculates the initial change in parameter
            Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant_trouble, debugging);
            Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
            if (verbose>=4){
                Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
            }
            //
            //
            if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
                Ll_abs_best = Ll[ind0];
                beta_abs_best = beta_c;
                guess_abs_best=guess;
            }
            //
            if (verbose>=4){
                Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
                Rcout << "C++ Note: df504 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_abs_best[ij] << " ";
                }
                Rcout << " " << endl;
            }
            halves=0;
            while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_a[ij] + dbeta[ij];
                    beta_c[ij] = beta_0[ij];
                }
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
                // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

                Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
                Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
            }
            if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
                if (verbose>=4){
                    Rcout << "C++ Note: Changing back to best" <<endl;
                }
                // If it goes through every half step without improvement, then the maximum change needs to be decreased
                abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
                dose_abs_max = dose_abs_max*pow(0.5,halfmax);
                iter_check = 1;
                //
                beta_p = beta_best;//
                beta_a = beta_best;//
                beta_c = beta_best;//
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_best[ij];
                }
                Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
            }
            if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                iter_check=0;
                if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                    iter_stop = 1;
                }
            }
        }
        // -----------------------------------------------
        // Performing Full Calculation to get full second derivative matrix
        // -----------------------------------------------
        fill(Ll.begin(), Ll.end(), 0.0);
        if (!single_bool){
            fill(Lld.begin(), Lld.end(), 0.0);
            fill(Lldd.begin(), Lldd.end(), 0.0);
        }
        Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
        //
        a_n = beta_0;
        if (verbose>=4){
            Rcout << "C++ Note: ending guess " << guess << endl;
        }
        //
        beta_fin(guess, _) = a_n;
        LL_fin[guess] = Ll[0];
        //
    }
    if (verbose>=4){
        Rcout << "C++ Note: Refreshing matrices after all guesses" << endl;
    }
    //
    fill(Ll.begin(), Ll.end(), 0.0);
    if (!single_bool){
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
    }
    beta_p = beta_best;//
    beta_a = beta_best;//
    beta_c = beta_best;//
    abs_max = abs_max0;
    dose_abs_max = dose_abs_max0;
    iter_stop = 0;
    halves=0;
    iteration=0;
    halves = 0; //number of half-steps taken
    ind0 = fir; //used for validations
    iteration=0; //iteration number
    //
    convgd = FALSE;
    iter_stop =0; //tracks if the iterations should be stopped for convergence
    iter_check=0; //signal to check for convergence
    //
    // next we need to figure out what point to start at
    best_guess = -1;
    for (int guess=guesses-1; guess>-1;guess--){
        // we want to find the closest point at which the loglikelihood is below lstar
        if (LL_fin[guess] < Lstar){
            best_guess = guess;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Lower Guess Results" << endl;
        Rcout << "Guess number, parameter values, Log-Likelihood change" << endl;
        NumericVector beta_temp;
        for (int i=0; i<guesses; i++){
            beta_temp = wrap(beta_fin.row(i));
            if (i==best_guess){
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] - Lstar << "<-- Best Guess" << endl;
            } else {
                Rcout << i << ", " << beta_temp << ", " << LL_fin[i] - Lstar << endl;
            }
        }
    }
    if (best_guess == 0){
        beta_temp = wrap(beta_fin.row(best_guess)); // the first point was closest, no lower bound
    } else if (best_guess == -1){
        beta_temp = wrap(beta_fin.row(guesses-1)); // the last point was closest, no upper bound
    } else {
        beta_temp = wrap(beta_fin.row(best_guess));
        beta_temp0 = wrap(beta_fin.row(best_guess-1));
        for (int i=0;i<beta_temp.size();i++){
            beta_temp[i] = beta_temp[i] + beta_temp0[i];
            beta_temp[i] = beta_temp[i]/2;
        }
    }
    if (verbose>=4){
        Rcout << "C++ Note: Initial Guess: ";
        for (int i=0;i<beta_0.size();i++){
            Rcout << beta_temp[i] << " ";
        }
        Rcout << " " << endl;
    }
    for (int i=0;i<beta_0.size();i++){
        a_n[i] = beta_temp[i];
        beta_0[i] = a_n[i];
        beta_c[i] = beta_0[i];
    }
    //
    if (verbose>=4){
        Rcout << "C++ Note: starting subterms after in best guess" << endl;
    }
    //
    // Calculates the subterm and term values
    Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
    //
    // -------------------------------------------------------------------------------------------
    //
    // Calculates the side sum terms used
    Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
        Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        if (ij==jk){
            Lld_vecc[ij] = Lld[ij];
        }
    }
    Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
    Lldd_mat = as<Map<MatrixXd> >(Lldd_vec);
    Lld_vec = as<Map<VectorXd> >(Lld_vecc);
    step = 1;
    iter_continue = true;
    max_change = 100;
    while ((step<maxstep)&&(iter_continue)){
         step++;
         trouble = false;
         Log_Bound(deriv_max, Lldd_mat, Lld_vec, Lstar, qchi, Ll[0], para_number, nthreads, totalnum, reqrdnum, KeepConstant, term_tot, step, dbeta, beta_0, upper, trouble, verbose, mult);
         if (trouble){
             Calc_Change_trouble( para_number, nthreads, totalnum, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant_trouble, debugging);
         }
         //
         beta_p = beta_c;//
         beta_a = beta_c;//
         //
         for (int ijk=0;ijk<totalnum;ijk++){
             if (KeepConstant[ijk]==0){
//                 int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                 //
                 if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){ //the threshold values use different maximum deviation values
                     if (abs(dbeta[ijk])>dose_abs_max){
                         dbeta[ijk] = dose_abs_max * sign(dbeta[ijk]);
                     }
                 }else{
                     if (abs(dbeta[ijk])>abs_max){
                         dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                     }
                 }
             } else {
                 dbeta[ijk]=0;
             }
         }
         //
         max_change = abs(dbeta[0]);

 //        Rcout << "Change: ";
         for (int ij=0;ij<totalnum;ij++){
//            Rcout << dbeta[ij] << " ";
            if (ij==para_number){
                // we want to prevent two issues
                // first prevent the parameter estimate from crossing the optimum
                // issue is beta_0[para_number] <= beta_best[para_number]
                if (dbeta[ij] > (beta_best[para_number] - beta_a[ij])/lr){
                    dbeta[ij] = -0.5*dbeta[ij];
                }
                // second issue is making sure that the step is forced away from the optimum when possible
                if (Ll[0] > Lstar){
                    // If the log-likelihood is above the goal, then it must move away from the optimum point
                    // for lower limit, the step is always positive
                    dbeta[ij] = -1*abs(dbeta[ij]);
                }
            }
            if (abs(dbeta[ij])>max_change){
                max_change = abs(dbeta[ij]);
            }
            beta_0[ij] = beta_a[ij] + lr*dbeta[ij];
            beta_c[ij] = beta_0[ij];
        }
 //        Rcout << " " << endl;
         // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
         // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
         // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

         Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
         bound_val = 0;
         Cox_Pois_Log_Loop(abs_max, basic_bool, beta_0, beta_a, beta_c, bound_val, cox_bool, dbeta, debugging, df0, dfc, dint, Dose, dose_abs_max, dslp, fir, gmix_term, gmix_theta, half_check, halfmax, KeepConstant, limit_hit, lr, modelform, nonDose, nonDose_LIN, nonDose_LOGLIN, nonDose_PLIN, nthreads, R, Rd, Rdd, RddR, RdR, s_weights, single_bool, start, strata_bool, T0, Td0, Tdd0, Te, term_n, term_tot, tform, totalnum, TTerm, verbose);
         if (limit_hit[0]){
             limits[0] = 0;
             ll_final[0] = 0;
             break;
         }
         Pois_Dev_LL_Calc( reqrdnum, totalnum, fir, R, Rd, Rdd, beta_0 ,  RdR,  RddR, Ll, Lld, Lldd, PyrC, dev_temp, nthreads, debugging, KeepConstant, verbose, single_bool, start, iter_stop, dev);
         //
         if (verbose>=4){
             end_point = system_clock::now();
             ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
             Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<0<< ",Update_Calc" <<endl;//prints the time
             gibtime = system_clock::to_time_t(system_clock::now());
             Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
             Rcout << "C++ Note: df101 ";//prints the log-likelihoods
             for (int ij=0;ij<reqrdnum;ij++){
                 Rcout << Ll[ij] << " ";
             }
             Rcout << " " << endl;
             Rcout << "C++ Note: df104 ";//prints parameter values
             for (int ij=0;ij<totalnum;ij++){
                 Rcout << beta_0[ij] << " ";
             }
             Rcout << " " << endl;
         }
         //
         #ifdef _OPENMP
         #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
         #endif
         for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
             int ij = 0;
             int jk = ijk;
             while (jk>ij){
                 ij++;
                 jk-=ij;
             }
             Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
             Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
             if (ij==jk){
                 Lld_vecc[ij] = Lld[ij];
             }
         }
         Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
         Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
         Map<VectorXd> Lld_vec(as<Map<VectorXd> >(Lld_vecc));
         limits[0] = beta_0[para_number];
         ll_final[0] = Ll[0];
         if ((max_change < epsilon)&&(deriv_max < deriv_epsilon)){
            iter_continue = false;
        }
     }
    //
    res_list = List::create(_["Parameter_Limits"]=wrap(limits), _["Negative_Limit_Found"]=wrap(limit_hit), _["Likelihood_Boundary"]=wrap(ll_final), _["Likelihood_Goal"]=wrap(Lstar), _["Status"]="PASSED");
    // returns a list of results
    return res_list;
}

//' Primary Cox PH regression with multiple distributed dose columns and optional combinations of null, stratification, competing risks, multiplicative log-linear model, and no derivative calculation.
//'
//' \code{LogLik_Cox_PH_Multidose_Omnibus} Performs the calls to calculation functions, Structures the Cox PH regression, With verbose option prints out time stamps and intermediate sums of terms and derivatives
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List LogLik_Cox_PH_Multidose_Omnibus( IntegerVector term_n, StringVector tform, NumericVector a_n,NumericMatrix& x_all, IntegerMatrix dose_cols, IntegerVector dose_index,IntegerVector dfc,int fir, int der_iden,string modelform, double lr, int maxiter, int halfmax, double epsilon,  double abs_max,double dose_abs_max, double deriv_epsilon, NumericMatrix df_groups, NumericVector tu, int double_step ,bool change_all, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, string ties_method, int nthreads, NumericVector& STRATA_vals, const VectorXd& cens_weight,  bool strata_bool, bool basic_bool, bool null_bool, bool CR_bool, bool single_bool, bool constraint_bool, const double gmix_theta, const IntegerVector gmix_term, const MatrixXd Lin_Sys, const VectorXd Lin_Res){
    ;
    //
    List temp_list = List::create(_["Status"]="TEMP"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_COX_GUESS" << endl;
    }
    time_point<system_clock> start_point, end_point;
    start_point = system_clock::now();
    auto start = time_point_cast<microseconds>(start_point).time_since_epoch().count();
    end_point = system_clock::now();
    auto ending = time_point_cast<microseconds>(end_point).time_since_epoch().count(); //The time duration is tracked
    //
    auto gibtime = system_clock::to_time_t(system_clock::now());
    if (verbose>=4){
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    //
    // Time durations are measured from this point on in microseconds
    //
    // df0: covariate data
    // ntime: number of event times for Cox PH
    // totalnum: number of terms used
    //
    // ------------------------------------------------------------------------- // initialize
	const Map<MatrixXd> df0(as<Map<MatrixXd> >(x_all));
    int ntime = tu.size();
    int totalnum;
    int reqrdnum;
    // ------------------------------------------------------------------------- // initialize
	if (null_bool){
        if (verbose>=1){
            Rcout << "null model is not compatable with multi-realization method" << endl;
        }
        temp_list = List::create(_["Status"]="FAILED",_["LogLik"]=R_NaN);
        return temp_list;
    }
    if (single_bool){
        if (verbose>=1){
            Rcout << "non-derivative model calculation is not compatable with multi-realization method" << endl;
        }
        temp_list = List::create(_["Status"]="FAILED",_["LogLik"]=R_NaN);
        return temp_list;
    }
    totalnum = term_n.size();
    reqrdnum = totalnum - sum(KeepConstant);
    if (verbose>=4){
        Rcout << "C++ Note: Term checked ";
        for (int ij=0;ij<totalnum;ij++){
            Rcout << term_n[ij] << " ";
        }
        Rcout << " " << endl;
    }
    //
    // cout.precision: controls the number of significant digits printed
    // nthreads: number of threads used for parallel operations
    //
    Rcout.precision(7); //forces higher precision numbers printed to terminal
    //
    // Lld_worst: stores the highest magnitude log-likelihood derivative
    //
    //
    double Lld_worst = 0.0; //stores derivative value used to determine if every parameter is near convergence
    //
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    // ------------------------------------------------------------------------- // initialize
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    MatrixXd T0;
    MatrixXd Td0;
	MatrixXd Tdd0;
	//
	MatrixXd Te;
	MatrixXd R;
	ColXd Rd;
	ColXd Rdd;
	//
	MatrixXd Dose;
	MatrixXd nonDose;
	MatrixXd nonDose_LIN;
	MatrixXd nonDose_PLIN;
	MatrixXd nonDose_LOGLIN;
	MatrixXd TTerm;
	double dint; //The amount of change used to calculate derivatives in threshold paramters
	double dslp;
    ColXd RdR;
	ColXd RddR;
	// ------------------------------------------------------------------------- // initialize
    if (verbose>=4){
        end_point = system_clock::now();
        ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
        Rcout << "C++ Note: df99," << (ending-start) << ",Starting" <<endl;
        gibtime = system_clock::to_time_t(system_clock::now());
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    // ------------------------------------------------------------------------- // initialize
    StringMatrix RiskGroup_Strata;
    vector<string>  RiskGroup;
    IntegerMatrix RiskFail;
    const Map<MatrixXd> df_m(as<Map<MatrixXd> >(df_groups));
    // ------------------------------------------------------------------------- // initialize
    if (strata_bool){
        RiskGroup_Strata = StringMatrix(ntime,STRATA_vals.size()); //vector of strings detailing the rows
        RiskFail = IntegerMatrix(ntime,2*STRATA_vals.size()); //vector giving the event rows
        //
        if (verbose>=4){
            Rcout << "C++ Note: Grouping Start" << endl;
        }
        // Creates matrices used to identify the event risk groups
        if (CR_bool){
            Make_Groups_STRATA_CR( ntime, df_m, RiskFail, RiskGroup_Strata, tu, nthreads, debugging,STRATA_vals,cens_weight);
        } else {
            Make_Groups_STRATA( ntime, df_m, RiskFail, RiskGroup_Strata, tu, nthreads, debugging,STRATA_vals);
        }
    } else {
        RiskGroup.resize(ntime); //vector of strings detailing the rows
        RiskFail = IntegerMatrix(ntime,2); //vector giving the event rows
        //
        if (verbose>=4){
            Rcout << "C++ Note: Grouping Start" << endl;
        }
        // Creates matrices used to identify the event risk groups
        if (CR_bool){
            Make_Groups_CR( ntime, df_m, RiskFail, RiskGroup, tu,cens_weight, nthreads, debugging);
        } else {
            Make_Groups( ntime, df_m, RiskFail, RiskGroup, tu, nthreads, debugging);
        }
    }
    if (verbose>=4){
        end_point = system_clock::now();
        ending = time_point_cast<microseconds>(end_point).time_since_epoch().count();
        Rcout << "C++ Note: df100 " << (ending-start) << " " <<0<< " " <<0<< " " <<-1<< ",Prep_List" <<endl;
        gibtime = system_clock::to_time_t(system_clock::now());
        Rcout << "C++ Note: Current Time, " << ctime(&gibtime) << endl;
    }
    // ------------------------------------------------------------------------- // initialize
    MatrixXd Rls1;
	MatrixXd Lls1;
	MatrixXd Rls2;
	MatrixXd Rls3;
	MatrixXd Lls2;
	MatrixXd Lls3;
	vector<double> Ll(reqrdnum,0.0); //Log-likelihood values
	vector<double> Lld(reqrdnum,0.0); //Log-likelihood derivative values
	vector<double> Lldd(pow(reqrdnum,2),0.0);//The second derivative matrix has room for every combination, but only the lower triangle is calculated initially
    // ------------------------------------------------------------------------- // initialize
	Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
    //The log-likelihood is calculated in parallel over the risk groups
    vector <double> Ll_comp(2,Ll[0]); //vector to compare values
    double abs_max0 = abs_max;
    double dose_abs_max0 = dose_abs_max;
    //
    vector<double> dbeta(totalnum,0.0);
    //
    // --------------------------
    // always starts from initial guess
    // --------------------------
    vector<double> beta_c(totalnum,0.0);
    vector<double> beta_a(totalnum,0.0);
    vector<double> beta_best(totalnum,0.0);
    vector<double> beta_p(totalnum,0.0);
    VectorXd::Map(&beta_p[0], beta_0.size()) = beta_0;// stores previous parameters
    VectorXd::Map(&beta_c[0], beta_0.size()) = beta_0;// stores current parameters
    VectorXd::Map(&beta_a[0], beta_0.size()) = beta_0;// stores a refrence value for parameters
    VectorXd::Map(&beta_best[0], beta_0.size()) = beta_0;// stores the best parameters
    double halves = 0; //number of half-steps taken
    int ind0 = fir; //used for validations
    int iteration=0; //iteration number
    // int maxiter=0;
    //
    bool convgd = FALSE;
    int iter_stop =0; //tracks if the iterations should be stopped for convergence
    int iter_check=0; //signal to check for convergence
    //
    // Rcout << "dose_cols info " << dose_cols.rows() << ", " << dose_cols.cols() << endl;
    //
    int guesses = dose_cols.cols();
    NumericMatrix beta_fin(dose_cols.cols(), totalnum);
    NumericVector LL_fin(dose_cols.cols());
    NumericMatrix std_fin(dose_cols.cols(), totalnum);
    IntegerVector dfc_0(dfc.length());
    for (int i=0; i<dfc.length();i++){
        dfc_0[i] = dfc[i];
    }
    //
    double Ll_abs_best = 10;
    vector<double> beta_abs_best(totalnum,0.0);
    int guess_abs_best =-1;
    //
    // Variables that are used for the risk check function shared across cox, poisson, and log bound functions
    bool cox_bool = true;
    double dev = 0.0;
    MatrixXd dev_temp = MatrixXd::Zero(1, 1);
    bool log_bound_bool = false;
    double Lstar = 0.0;
    MatrixXd PyrC = MatrixXd::Zero(1, 1);
    ///
    for (int guess=0; guess <guesses; guess++){

//        Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
        Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, single_bool);
        fill(Ll.begin(), Ll.end(), 0.0);
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
        beta_p = beta_best;//
        beta_a = beta_best;//
        beta_c = beta_best;//
        abs_max = abs_max0;
        dose_abs_max = dose_abs_max0;
        iter_stop = 0;
        halves=0;
        iteration=0;
        halves = 0; //number of half-steps taken
        ind0 = fir; //used for validations
        iteration=0; //iteration number
        //
        convgd = FALSE;
        iter_stop =0; //tracks if the iterations should be stopped for convergence
        iter_check=0; //signal to check for convergence
        //
        // maxiter = maxiters[guess];
        // a_n = a_ns.row(guess);
        for (int i=0;i<beta_0.size();i++){
            beta_0[i] = beta_best[i];
        }
        for (int i=0;i<dose_index.size();i++){
            for (int j=0;j<totalnum;j++){
                if (dfc_0[j]==dose_index[i]){
                    dfc[j] = dose_cols(i, guess);
                }
            }
            // dfc[dose_index[i]] = dose_cols(i, guess);
        }
        if (verbose>=4){
            Rcout << "C++ Note: starting subterms " << term_tot << " in guess " << guess << endl;
        }
        Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
        if ((R.minCoeff()<=0)||(R.hasNaN())){
            if (verbose>=1){
			    Rcout << "C++ Error: A non-positive risk was detected: " << R.minCoeff() << endl;
			    Rcout << "C++ Warning: final failing values ";
			    for (int ijk=0;ijk<totalnum;ijk++){
				    Rcout << beta_0[ijk] << " ";
			    }
			    Rcout << " " << endl;
		    }
			//
			temp_list = List::create(_["beta_0"]=wrap(beta_0) ,_["Deviation"]=R_NaN,_["Status"]="FAILED",_["LogLik"]=R_NaN);
			return temp_list;
		}
        //
        // -------------------------------------------------------------------------------------------
        //
        if (verbose>=4){
            Rcout << "C++ Note: Made Risk Side Lists" << endl;
        }
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        //
        for (int i=0;i<beta_0.size();i++){
            beta_c[i] = beta_0[i];
        }
        while ((iteration < maxiter)&&(iter_stop==0)){
            iteration++;
            beta_p = beta_c;//
            beta_a = beta_c;//
            beta_best = beta_c;//
            //
            // calculates the initial change in parameter
            if (basic_bool){
                Calc_Change_Basic( double_step, nthreads, totalnum, der_iden, lr, abs_max, Ll, Lld, Lldd, dbeta, change_all, KeepConstant, debugging);
            } else {
                if (constraint_bool){
                    Calc_Change_Cons( Lin_Sys, Lin_Res, beta_0, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, tform, dose_abs_max,abs_max, KeepConstant, debugging);
                } else {
                    Calc_Change( double_step, nthreads, totalnum, der_iden, dose_abs_max, lr, abs_max, Ll, Lld, Lldd, dbeta, change_all, tform, dose_abs_max,abs_max, KeepConstant, debugging);
                }
                Intercept_Bound(nthreads, totalnum, beta_0, dbeta, dfc, df0, KeepConstant, debugging, tform);
            }
            if (verbose>=4){
                Rcout << "C++ Note: Starting Halves" <<endl;//prints the final changes for validation
            }
            //
            //
            if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
                Ll_abs_best = Ll[ind0];
                beta_abs_best = beta_c;
                guess_abs_best=guess;
            }
            //
            if (verbose>=4){
                Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
                Rcout << "C++ Note: df504 ";//prints parameter values
                for (int ij=0;ij<totalnum;ij++){
                    Rcout << beta_abs_best[ij] << " ";
                }
                Rcout << " " << endl;
            }
            halves=0;
            while ((Ll[ind0] <= Ll_abs_best)&&(halves<halfmax)){ //repeats until half-steps maxed or an improvement
//                Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_a[ij] + dbeta[ij];
                    beta_c[ij] = beta_0[ij];
                }
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
                // The same subterm, risk, sides, and log-likelihood calculations are performed every half-step and iteration
                // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//

                Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
                Cox_Pois_Check_Continue(basic_bool, beta_0, beta_best, beta_c, cens_weight, change_all, cox_bool, CR_bool, dbeta, debugging, dev, dev_temp, fir, halfmax, halves, ind0, iter_stop, KeepConstant, Ll, Ll_abs_best, Lld, Lldd, Lls1, Lls2, Lls3, log_bound_bool, Lstar, nthreads, ntime, PyrC, R, Rd, Rdd, RddR, RdR, reqrdnum, RiskFail, RiskGroup, RiskGroup_Strata, Rls1, Rls2, Rls3, single_bool, start, strata_bool, STRATA_vals, term_n, ties_method, totalnum, TTerm, verbose);
            }
            if (beta_best!=beta_c){//if the risk matrices aren't the optimal values, then they must be recalculated
                if (verbose>=4){
                    Rcout << "C++ Note: Changing back to best" <<endl;
                }
                // If it goes through every half step without improvement, then the maximum change needs to be decreased
                abs_max = abs_max*pow(0.5,halfmax); // reduces the step sizes
                dose_abs_max = dose_abs_max*pow(0.5,halfmax);
                iter_check = 1;
                //
                beta_p = beta_best;//
                beta_a = beta_best;//
                beta_c = beta_best;//
//                Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
                for (int ij=0;ij<totalnum;ij++){
                    beta_0[ij] = beta_best[ij];
                }
                Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dose_abs_max,  abs_max,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start, gmix_theta, gmix_term);
            }
            if ((iteration % (reqrdnum))||(iter_check==1)){//Checks every set number of iterations
                iter_check=0;
                if (Lld_worst < deriv_epsilon){//ends if the derivatives are low enough
                    iter_stop = 1;
                }
                if (abs_max < epsilon/10){//if the maximum change is too low, then it ends
                    iter_stop = 1;
                }
            }
        }
        // -----------------------------------------------
        // Performing Full Calculation to get full second derivative matrix
        // -----------------------------------------------
        fill(Ll.begin(), Ll.end(), 0.0);
        fill(Lld.begin(), Lld.end(), 0.0);
        fill(Lldd.begin(), Lldd.end(), 0.0);
        Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, single_bool, start, iter_stop);
        //
        a_n = beta_0;
        if (verbose>=4){
            Rcout << "C++ Note: ending guess " << guess << endl;
        }
        //
        beta_fin(guess, _) = a_n;
        LL_fin[guess] = Ll[0];
        NumericVector Lldd_vec(reqrdnum * reqrdnum); //simplfied information matrix
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            Lldd_vec[ij * reqrdnum + jk]=Lldd[ij*reqrdnum+jk];
            Lldd_vec[jk * reqrdnum + ij]=Lldd_vec[ij * reqrdnum + jk];
        }
        Lldd_vec.attr("dim") = Dimension(reqrdnum, reqrdnum);
        const Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
        //
        MatrixXd Lldd_inv = -1 * Lldd_mat.inverse().matrix(); //uses inverse information matrix to calculate the standard deviation
        NumericVector stdev(totalnum);
        for (int ij=0;ij<totalnum;ij++){
            if (KeepConstant[ij]==0){
                int pij_ind = ij - sum(head(KeepConstant,ij));
                stdev(ij) = sqrt(Lldd_inv(pij_ind,pij_ind));
            }
        }
        std_fin(guess, _) = stdev;
        // if ((Ll_abs_best>0)||(Ll_abs_best < Ll[ind0])){
        //     Ll_abs_best = Ll[ind0];
        //     beta_abs_best = beta_c;
        //     guess_abs_best=guess;
        // }
        //
        if (verbose>=4){
            Rcout << "C++ Note: df501 " << Ll_abs_best << endl;
            Rcout << "C++ Note: df504 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_abs_best[ij] << " ";
            }
            Rcout << " " << endl;
        }
    }
    //
    List para_list;
    if (!basic_bool){
        para_list = List::create(_["term_n"]=term_n,_["tforms"]=tform); //stores the term information
    }
    List res_list;// = List::create(_["LogLik"]=wrap(LL_fin),_["parameters"]=wrap(beta_fin),_["error"]=wrap(std_fin));
    if (basic_bool){
        res_list = List::create(_["LogLik"]=wrap(LL_fin),_["parameters"]=wrap(beta_fin),_["error"]=wrap(std_fin));
    } else {
        res_list = List::create(_["LogLik"]=wrap(LL_fin),_["parameters"]=wrap(beta_fin),_["error"]=wrap(std_fin),_["Parameter_Lists"]=para_list);
    }
    // returns a list of results
    return res_list;
}
