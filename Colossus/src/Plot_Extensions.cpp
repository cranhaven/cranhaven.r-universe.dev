#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include "Plot_Extensions.h"
#include "Omnibus_Pieces.h"
#include "Calc_Repeated.h"
#include "Subterms_Risk.h"
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



//' Primary Cox PH baseline hazard function with stratification
//'
//' \code{PLOT_SURV_STRATA} Performs the calls to calculation functions, Uses calculated risks and risk groups to approximate the baseline, With verbose option prints out time stamps and intermediate sums of terms and derivatives
//'
//' @inheritParams CPP_template
//'
//' @return List of results: baseline hazard, risk for each row
//' @noRd
//'
// [[Rcpp::export]]
List PLOT_SURV_STRATA(int reqrdnum, MatrixXd& R, MatrixXd& Rd, NumericVector& a_er, NumericMatrix& df_groups, NumericVector& tu, NumericVector& STRATA_vals , int verbose, bool debugging, int nthreads){
    //
    int ntime = tu.size();
    NumericMatrix baseline(ntime, STRATA_vals.size());
    NumericMatrix hazard_error(ntime, STRATA_vals.size());
	#ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum;ijk++){
        Rd.col(ijk) = Rd.col(ijk).array().pow(2).array() * pow(a_er[ijk],2);
    }
    //
    // Iterates through the risk groups and approximates the baseline
    //
    const Map<MatrixXd> df_m(as<Map<MatrixXd> >(df_groups));
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
        for (int ijk=0;ijk<ntime;ijk++){
            double t0 = tu[ijk];
            VectorXi select_ind_end = ((df_m.col(2).array() == 1)&&(df_m.col(1).array()==t0)&&(df_m.col(3).array()==STRATA_vals[s_ij])).cast<int>(); //indices with events
            vector<int> indices_end;
            //
            //
            int th = 1;
            visit_lambda(select_ind_end,
                [&indices_end, th](double v, int i, int j) {
                    if (v==th)
                        indices_end.push_back(i+1);
                });
            //
            vector<int> indices; //generates vector of (start,end) pairs for indices at risk
            if (indices_end.size()>0){
            	int dj = indices_end[indices_end.size()-1] - indices_end[0] + 1;// number of events
                //
                select_ind_end = (((df_m.col(0).array() < t0)||(df_m.col(0).array()==df_m.col(1).array()))&&(df_m.col(1).array()>=t0)&&(df_m.col(3).array()==STRATA_vals[s_ij])).cast<int>(); //indices at risk
                indices_end.clear();
                visit_lambda(select_ind_end,
                    [&indices_end, th](double v, int i, int j) {
                        if (v==th)
                            indices_end.push_back(i+1);
                    });
                for (auto it = begin (indices_end); it != end (indices_end); ++it) {
                    if (indices.size()==0){
                        indices.push_back(*it);
                        indices.push_back(*it);
                    } else if (indices[indices.size()-1]+1<*it){
                        indices.push_back(*it);
                        indices.push_back(*it);
                    } else {
                        indices[indices.size()-1] = *it;
                    }
                }
                double Rs1 = 0; //total risk
				for (vector<double>::size_type i = 0; i < indices.size()-1; i=i+2){
				    Rs1 += R.block(indices[i]-1,0,indices[i+1]-indices[i]+1,1).sum();
				}
				baseline(ijk,s_ij) = dj / Rs1; //approximates the baseline hazard
				hazard_error(ijk,s_ij) = dj / pow(Rs1,2);
            } else {
                baseline(ijk,s_ij) = 0; //approximates the baseline hazard
				hazard_error(ijk,s_ij) = 0;
            }
        }
    }
    //
    NumericVector w_R = wrap(R.col(0));
    // returns the baseline approximates and the risk information
    List res_list = List::create(_["baseline"]=baseline, _["standard_error"]=hazard_error, _["Risks"]=w_R);
    //
    return res_list;
}

//' Primary Cox PH baseline hazard function
//'
//' \code{PLOT_SURV} Performs the calls to calculation functions, Uses calculated risks and risk groups to approximate the baseline, With verbose option prints out time stamps and intermediate sums of terms and derivatives
//'
//' @inheritParams CPP_template
//'
//' @return List of results: baseline hazard, risk for each row
//' @noRd
//'
// [[Rcpp::export]]
List PLOT_SURV(int reqrdnum, MatrixXd& R, MatrixXd& Rd, NumericVector& a_er, NumericMatrix& df_groups, NumericVector& tu , int verbose, bool debugging, int nthreads){
    //
    int ntime = tu.size();
    vector<double> baseline(ntime,0.0);
    vector<double> hazard_error(ntime,0.0);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum;ijk++){
        Rd.col(ijk) = Rd.col(ijk).array().pow(2).array() * pow(a_er[ijk],2);
    }
    //
    // Iterates through the risk groups and approximates the baseline
    //
    const Map<MatrixXd> df_m(as<Map<MatrixXd> >(df_groups));
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<ntime;ijk++){
        double t0 = tu[ijk];
        VectorXi select_ind_all = ((df_m.col(0).array() <= t0)&&(df_m.col(1).array()>=t0)).cast<int>(); //indices at risk
        vector<int> indices_all;
        VectorXi select_ind_end = ((df_m.col(2).array() == 1)&&(df_m.col(1).array()==t0)).cast<int>(); //indices with events
        vector<int> indices_end;
        //
        //
        int th = 1;
        visit_lambda(select_ind_all,
            [&indices_all, th](double v, int i, int j) {
                if (v==th)
                    indices_all.push_back(i+1);
            });
        visit_lambda(select_ind_end,
            [&indices_end, th](double v, int i, int j) {
                if (v==th)
                    indices_end.push_back(i+1);
            });
        //
        vector<int> indices; //generates vector of (start,end) pairs for indices at risk
        for (auto it = begin (indices_all); it != end (indices_all); ++it) {
            if (indices.size()==0){
                indices.push_back(*it);
                indices.push_back(*it);
            } else if (indices[indices.size()-1]+1<*it){
                indices.push_back(*it);
                indices.push_back(*it);
            } else {
                indices[indices.size()-1] = *it;
            }
        }
        int dj = indices_end[indices_end.size()-1] - indices_end[0] + 1;// number of events
        double Rs1 = 0; //total risk
        double Rds1 = 0; //total weighted risk derivative squared
        for (vector<double>::size_type i = 0; i < indices.size()-1; i=i+2){
            Rs1 += R.block(indices[i]-1,0,indices[i+1]-indices[i]+1,1).sum();
            Rds1 += Rd.block(indices[i]-1,0,indices[i+1]-indices[i]+1,reqrdnum).sum();
        }
        baseline[ijk] = dj / Rs1; //approximates the baseline hazard
        hazard_error[ijk] = dj / pow(Rs1,2);
        //
    }
    //
    NumericVector w_base = wrap(baseline);
    NumericVector w_base_er = wrap(hazard_error);
    NumericVector w_R = wrap(R.col(0));
    // returns the baseline approximates and the risk information
    List res_list = List::create(_["baseline"]=w_base, _["standard_error"]=w_base_er, _["Risks"]=w_R);
    //
    return res_list;
}


//' Primary Cox PH schoenfeld residual function
//'
//' \code{Schoenfeld_Calc} Performs the calls to calculation functions, Uses calculated risks and risk groups to calculate the residuals, With verbose option prints out time stamps and intermediate sums of terms and derivatives
//'
//' @inheritParams CPP_template
//'
//' @return List of results: scaled schoenfeld residuals
//' @noRd
//'
// [[Rcpp::export]]
List Schoenfeld_Calc( int ntime, int totalnum, const  VectorXd& beta_0, const  MatrixXd& df0, const MatrixXd& R, MatrixXd& Lldd_inv, const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup,IntegerVector& dfc, int verbose, bool debugging, IntegerVector KeepConstant, int nthreads){
    int reqrdnum = totalnum - sum(KeepConstant);
    if (verbose>=4){
        Rcout << "C++ Note: starting plot data " << endl;
    }
    MatrixXd residuals = MatrixXd::Zero(ntime,reqrdnum);
    MatrixXd res_scale = MatrixXd::Zero(ntime,reqrdnum);
    VectorXd res_df = VectorXd::Zero(ntime);
    //
    VectorXd req_beta = VectorXd::Zero(reqrdnum);
    for (int i=0; i<totalnum; i++){
        if (KeepConstant[i]==0){
            int j = i - sum(head(KeepConstant,i));
            req_beta[j] = beta_0[i];
        }
    }
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int ijk=0;ijk<totalnum;ijk++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            //
            if (KeepConstant[ijk]==0){
                //
                int ij = ijk - sum(head(KeepConstant,ijk));
                int df0_c = dfc[ijk]-1;
                //
                vector<int> InGroup;
                string Groupstr = RiskGroup[j];
                stringstream ss(Groupstr);
                //
                for (int i; ss >> i;) {
                    InGroup.push_back(i);    
                    if (ss.peek() == ',')
                        ss.ignore();
                }
                double t_sum =0;
                double x_expect =0;
                //
                // calculates the total term value
                //
                for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
                    t_sum += R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).sum();
                    x_expect +=  (df0.block(InGroup[i]-1,df0_c,InGroup[i+1]-InGroup[i]+1,1).array() * R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).array()).sum();
                }
                int dj = RiskFail(j,1)-RiskFail(j,0)+1;
                double x_risks = df0.block(RiskFail(j,0),df0_c,dj,1).sum()/dj; //calculate the average covariate value with events
                x_expect = x_expect / t_sum / dj; //calculates the averaged covariate value
                //
                residuals(j,ij) = (x_risks - x_expect);
                if (ij==0){
                    res_df(j) = dj;
                }
            }
        }
    }
    //
    res_scale = ((residuals * Lldd_inv) * ntime).array() + req_beta.transpose().replicate(residuals.rows(),1).array();
    //
    List res_list = List::create(_["residuals"]=wrap(residuals), _["scaled"]=wrap(res_scale), _["df"]=wrap(res_df));
    // returns residuals
    return res_list;
}


//' Primary plotting function.
//'
//' \code{Plot_Omnibus} Performs the calls to calculation functions
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List Plot_Omnibus( IntegerVector term_n, StringVector tform, NumericVector a_n,NumericMatrix& x_all,IntegerVector dfc,int fir, int der_iden,string modelform, double abs_max,double dose_abs_max, NumericMatrix& df_groups, NumericVector& tu, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, string ties_method, int nthreads, NumericVector& STRATA_vals, const VectorXd& cens_weight,  int uniq_v, bool strata_bool, bool basic_bool, bool CR_bool, bool Surv_bool, bool Risk_bool, bool Schoenfeld_bool, bool Risk_Sub_bool, const double gmix_theta, const IntegerVector& gmix_term){
    ;
    //
    List temp_list = List::create(_["Status"]="FAILED"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_PLOT" << endl;
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
    MatrixXd df0;
    int ijk_risk=0;
    vector<float> vv; //stores the covariate values
    if (Risk_bool){
        const Map<MatrixXd> df1(as<Map<MatrixXd> >(x_all));
        float dx = 0;
        if (der_iden >=0){
            ;
        } else {
            throw invalid_argument( "Incorrect parameter to plot by" );
        }
        if (uniq_v > 100){ //selects anything above 100 points to be continuous
            vv.resize(100); //continuous covariates use 100 steps
        } else{
            vv.resize(uniq_v); //factor covariates use the number of factors
        }
        df0 = MatrixXd::Zero(vv.size(), df1.cols()); // stores memory for the derivative term parameters and columns
        df0 = df0.array();
        ijk_risk= dfc[der_iden]-1;
        dx = (df1.col(ijk_risk).maxCoeff() - df1.col(ijk_risk).minCoeff())/(vv.size()-1);//varies from max to minimum
        vv[0] = df1.col(ijk_risk).minCoeff();
        generate(vv.begin(), vv.end(), [n = 0, &dx]() mutable { return n++ * dx; });
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (vector<float>::size_type ij=0;ij<vv.size();ij++){
            df0(ij,ijk_risk)=vv[ij]; //fills the column with varying values
        }
    } else {
	    df0 = as<Map<MatrixXd> >(x_all);
    }
    int ntime = tu.size();
    int totalnum;
    int reqrdnum;
    bool single_bool;
    if ((Risk_Sub_bool)||(Risk_bool)){
        single_bool = TRUE;
    } else {
        single_bool = FALSE;
    }
    // ------------------------------------------------------------------------- // initialize
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
    // ---------------------------------------------
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
	Cox_Refresh_R_TERM(totalnum, reqrdnum, term_tot, dint, dslp, dose_abs_max, abs_max, df0, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, basic_bool, single_bool);
    // ------------------------------------------------------------------------- // initialize
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
    Cox_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR,  nthreads, debugging, KeepConstant, verbose, basic_bool, single_bool, start,gmix_theta, gmix_term);
	//
	List res_list;
	//
	if (Risk_bool){
	    res_list = List::create(_["x"]=wrap(df0.col(ijk_risk)), _["y"]=wrap(R.col(0)));//returns list of covariate values and risk
        return res_list;
	}
	if (Risk_Sub_bool){
	    res_list = List::create(_["Risk"]=wrap(R.col(0)));//returns list of covariate values and risk
        return res_list;
	}
    //
    // -------------------------------------------------------------------------------------------
    //
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
    if (verbose>=4){
        Rcout << "C++ Note: Made Risk Side Lists" << endl;
    }
    Cox_Refresh_R_SIDES(reqrdnum, ntime, Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, STRATA_vals, strata_bool, FALSE);
    fill(Ll.begin(), Ll.end(), 0.0);
    fill(Lld.begin(), Lld.end(), 0.0);
    fill(Lldd.begin(), Lldd.end(), 0.0);
    // Calculates the side sum terms used
    Cox_Side_LL_Calc(reqrdnum, ntime, RiskFail, RiskGroup_Strata, RiskGroup,  totalnum, fir, R, Rd, Rdd,  Rls1, Rls2, Rls3, Lls1, Lls2, Lls3, cens_weight, STRATA_vals, beta_0 , RdR, RddR, Ll, Lld,  Lldd, nthreads, debugging, KeepConstant, ties_method, verbose, strata_bool, CR_bool, basic_bool, FALSE, start, 0);
    int kept_covs = totalnum - sum(KeepConstant); //does !base the standard deviation off of constant parameters
    NumericVector Lldd_vec(kept_covs * kept_covs);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<kept_covs*(kept_covs+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd_vec[ij * kept_covs + jk]=Lldd[ij * kept_covs + jk];
        Lldd_vec[jk * kept_covs + ij]=Lldd_vec[ij * kept_covs + jk];
    }
    Lldd_vec.attr("dim") = Dimension(kept_covs, kept_covs);
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
    NumericVector a_er(wrap(stdev));
    //
    if (Surv_bool){
    	if (strata_bool){
    		res_list = PLOT_SURV_STRATA(reqrdnum, R, Rd, a_er, df_groups, tu, STRATA_vals , verbose, debugging, nthreads);
    	} else {
        	res_list = PLOT_SURV(reqrdnum, R, Rd, a_er, df_groups, tu , verbose, debugging, nthreads);
    	}
        return res_list;
    }
    if (Schoenfeld_bool){
        res_list = Schoenfeld_Calc( ntime, totalnum, beta_0, df0, R, Lldd_inv, RiskFail, RiskGroup, dfc, verbose, debugging, KeepConstant, nthreads);
        return res_list;
    }
    //
    res_list = List::create(_["PASS"]=0);
    // returns a list of results
    return res_list;
}

//' Splits events into background and excess for a poisson regression
//'
//' \code{Assign_Events_Pois} Calculates proportion of events due to background and excess
//'
//' @inheritParams CPP_template
//'
//' @return returns proportion of events due to background and excess for each term
//' @noRd
//'
// [[Rcpp::export]]
List Assign_Events_Pois( IntegerVector term_n, StringVector tform, NumericVector a_n,NumericMatrix& x_all,IntegerVector dfc,const MatrixXd& PyrC, const MatrixXd& dfs,int fir,string modelform, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, int nthreads, const double gmix_theta, const IntegerVector gmix_term, const bool strata_bool){
    ;
    //
    int totalnum = term_n.size();
    List res_list = List::create(_["Status"]="FAILED"); //used as a dummy return value for code checking
    if (verbose>=4){
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
    VectorXd s_weights;
    if (strata_bool){
        s_weights = VectorXd::Zero(df0.rows());
        Gen_Strat_Weight(modelform, dfs, PyrC, s_weights, nthreads, tform, term_n, term_tot);
        Make_Risks_Weighted_Single(modelform, tform, term_n, totalnum, fir, s_weights, T0, Te, R, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, nthreads, debugging,KeepConstant,gmix_theta, gmix_term);
    } else {
        Make_Risks_Single(modelform, tform, term_n, totalnum, fir, T0, Te, R, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, nthreads, debugging,KeepConstant,gmix_theta, gmix_term);
    }
    //
    MatrixXd caused = MatrixXd::Zero(PyrC.rows(),3);
    MatrixXd predict = MatrixXd::Zero(PyrC.rows(),3);
    //
    //
    predict.col(0) = (TTerm.col(fir).array() * PyrC.col(0).array());
    predict.col(1) = (R.col(0).array() * PyrC.col(0).array()).array() - predict.col(0).array();
    predict.col(2) = predict.col(0).array() + predict.col(1).array();
    //
    caused.col(0) = PyrC.col(1).array() * predict.col(0).array() / predict.col(2).array();
    caused.col(1) = PyrC.col(1).array() * predict.col(1).array() / predict.col(2).array();
    caused.col(2) = PyrC.col(1).array();
    //
    res_list = List::create(_["caused"]=wrap(caused),_["predict"]=wrap(predict));
    return res_list;
}

//' Primary plotting function.
//'
//' \code{Poisson_Residuals} Performs the calls to calculation functions
//'
//' @inheritParams CPP_template
//'
//' @return List of final results: Log-likelihood of optimum, first derivative of log-likelihood, second derivative matrix, parameter list, standard deviation estimate, AIC, model information
//' @noRd
//'
// [[Rcpp::export]]
List Poisson_Residuals(const MatrixXd& PyrC, IntegerVector term_n, StringVector tform, NumericVector a_n,NumericMatrix& x_all,IntegerVector dfc,int fir, int der_iden,string modelform, double abs_max,double dose_abs_max, int verbose, bool debugging, IntegerVector KeepConstant, int term_tot, int nthreads, const MatrixXd& dfs, bool strata_bool, const double gmix_theta, const IntegerVector gmix_term, bool Pearson_bool, bool Deviance_bool){
    ;
    //
    List temp_list = List::create(_["Status"]="FAILED"); //used as a dummy return value for code checking
    if (verbose>=4){
        Rcout << "C++ Note: START_RESIDUAL" << endl;
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
    MatrixXd df0;
    df0 = as<Map<MatrixXd> >(x_all);
    int totalnum;
    int reqrdnum;
    bool single_bool = TRUE;
    // ------------------------------------------------------------------------- // initialize
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
    // int nthreads = Eigen::nbThreads()-1; //stores how many threads are allocated
    // Lld_worst: stores the highest magnitude log-likelihood derivative
    // ---------------------------------------------
    // To Start, needs to seperate the derivative terms
    // ---------------------------------------------
    //
    // ------------------------------------------------------------------------- // initialize
    Map<VectorXd> beta_0(as<Map<VectorXd> >(a_n));
    MatrixXd T0 = MatrixXd::Zero(df0.rows(), totalnum); //preallocates matrix for Term column
    MatrixXd Te = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for column terms used for temporary storage
    MatrixXd R = MatrixXd::Zero(df0.rows(), 1); //preallocates matrix for Risks
    //
    MatrixXd Dose = MatrixXd::Constant(df0.rows(),term_tot,0.0); //Matrix of the total dose term values
    MatrixXd nonDose = MatrixXd::Constant(df0.rows(),term_tot,1.0); //Matrix of the total non-dose term values
    MatrixXd nonDose_LIN = MatrixXd::Constant(df0.rows(),term_tot,0.0); //matrix of Linear subterm values
    MatrixXd nonDose_PLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Loglinear subterm values
    MatrixXd nonDose_LOGLIN = MatrixXd::Constant(df0.rows(),term_tot,1.0); //matrix of Product linear subterm values
    MatrixXd TTerm = MatrixXd::Zero(Dose.rows(),Dose.cols()); //matrix of term values
    MatrixXd Td0 = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Term derivative columns
    MatrixXd Tdd0 = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Term second derivative columns
    //
    MatrixXd Rd = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Risk derivatives
    MatrixXd Rdd = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Risk second derivatives
    //
	double dint; //The amount of change used to calculate derivatives in threshold paramters
	double dslp;
    ColXd RdR;
	ColXd RddR;
    dint = dose_abs_max; //The amount of change used to calculate derivatives in threshold paramters
    dslp = abs_max;
    RdR = MatrixXd::Zero(df0.rows(), reqrdnum); //preallocates matrix for Risk to derivative ratios
    RddR = MatrixXd::Zero(df0.rows(), reqrdnum*(reqrdnum+1)/2); //preallocates matrix for Risk to second derivative ratios
    VectorXd s_weights;
    if (strata_bool){
        s_weights = VectorXd::Zero(df0.rows());
        Gen_Strat_Weight(modelform, dfs, PyrC, s_weights, nthreads, tform, term_n, term_tot);
    }
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
    Pois_Term_Risk_Calc(modelform, tform, term_n, totalnum, fir, dfc, term_tot, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, beta_0, df0, dint,  dslp,  TTerm,  nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, s_weights,  nthreads, debugging, KeepConstant, verbose, strata_bool, single_bool, start, gmix_theta, gmix_term);
	List res_list;
	//
    res_list = List::create(_["Risk"]=wrap(R.col(0)),_["Residual_Sum"]=wrap(R.col(0).sum()));//returns list of covariate values and risk
    return res_list;
}
