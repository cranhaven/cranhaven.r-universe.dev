#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif
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

void removeRow(Eigen::MatrixXd& matrix, unsigned int rowToRemove)
{
    unsigned int numRows = matrix.rows()-1;
    unsigned int numCols = matrix.cols();

    if( rowToRemove < numRows )
        matrix.block(rowToRemove,0,numRows-rowToRemove,numCols) = matrix.block(rowToRemove+1,0,numRows-rowToRemove,numCols);

    matrix.conservativeResize(numRows,numCols);
}

void removeColumn(Eigen::MatrixXd& matrix, unsigned int colToRemove)
{
    unsigned int numRows = matrix.rows();
    unsigned int numCols = matrix.cols()-1;

    if( colToRemove < numCols )
        matrix.block(0,colToRemove,numRows,numCols-colToRemove) = matrix.block(0,colToRemove+1,numRows,numCols-colToRemove);

    matrix.conservativeResize(numRows,numCols);
}


//' Utility function to define risk groups
//'
//' \code{Make_Groups} Called to update lists of risk groups, Uses list of event times and row time/event information, Matrices store starting/stopping row indices for each group    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Matrix of event rows for each event time, vectors of strings with rows at risk for each event time
//' @noRd
//'
// [[Rcpp::export]]
void Make_Groups(const int& ntime, const MatrixXd& df_m, IntegerMatrix& RiskFail, vector<string>&  RiskGroup,  NumericVector& tu, const int& nthreads, bool debugging ){
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<ntime;ijk++){
        double t0 = tu[ijk];
        VectorXi select_ind_all = (((df_m.col(0).array() < t0)||(df_m.col(0).array()==df_m.col(1).array()))&&(df_m.col(1).array()>=t0)).cast<int>(); //indices at risk
        vector<int> indices_all;
        //
        int th = 1;
        visit_lambda(select_ind_all,
            [&indices_all, th](double v, int i, int j) {
                if (v==th)
                    indices_all.push_back(i+1);
            });
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
        //
        ostringstream oss;
        copy(indices.begin(), indices.end(),
            std::ostream_iterator<int>(oss, ","));
        RiskGroup[ijk] = oss.str();//stores risk groups in string
        //
        select_ind_all = ((df_m.col(2).array() == 1)&&(df_m.col(1).array()==t0)).cast<int>(); //indices with events
        indices_all.clear();
        visit_lambda(select_ind_all,
            [&indices_all, th](double v, int i, int j) {
                if (v==th)
                    indices_all.push_back(i+1);
            });
        //
        RiskFail(ijk,0)=indices_all[0]-1;//Due to the sorting method, there is a continuous block of event rows
        RiskFail(ijk,1)=indices_all[indices_all.size()-1]-1;
    }
    return;
}

//' Utility function to define risk groups with competing risks
//'
//' \code{Make_Groups_CR} Called to update lists of risk groups, Uses list of event times and row time/event information, Matrices store starting/stopping row indices for each group, adds rows with event=2 past the event time    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Matrix of event rows for each event time, vectors of strings with rows at risk for each event time
//' @noRd
//'
// [[Rcpp::export]]
void Make_Groups_CR(const int& ntime, const MatrixXd& df_m, IntegerMatrix& RiskFail, vector<string>&  RiskGroup,  NumericVector& tu, const VectorXd& cens_weight, const int& nthreads, bool debugging ){
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<ntime;ijk++){
        double t0 = tu[ijk];
        VectorXi select_ind_all = ((((df_m.col(0).array() < t0)||(df_m.col(0).array()==df_m.col(1).array()))&&(df_m.col(1).array()>=t0))||((df_m.col(2).array() == 2)&&(df_m.col(1).array()<=t0))).cast<int>(); //indices at risk
        vector<int> indices_all;
        //
        int th = 1;
		//
		visit_lambda(select_ind_all,
            [&indices_all, th](double v, int i, int j) {
                if (v==th)
					indices_all.push_back(i+1);
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
        ostringstream oss;
        copy(indices.begin(), indices.end(),
            std::ostream_iterator<int>(oss, ","));
        RiskGroup[ijk] = oss.str();//stores risk groups in string
        //
        select_ind_all = ((df_m.col(2).array() == 1)&&(df_m.col(1).array()==t0)).cast<int>(); //indices with events
        indices_all.clear();
        visit_lambda(select_ind_all,
            [&indices_all, th](double v, int i, int j) {
                if (v==th)
					indices_all.push_back(i+1);
            });
        RiskFail(ijk,0)=indices_all[0]-1;//Due to the sorting method, there is a continuous block of event rows
        RiskFail(ijk,1)=indices_all[indices_all.size()-1]-1;
        //
        
    }
    return;
}

//' Utility function to define risk groups with STRATA
//'
//' \code{Make_Groups_STRATA} Called to update lists of risk groups, Uses list of event times and row time/event information, Matrices store starting/stopping row indices for each group    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Matrix of event rows for each event time, vectors of strings with rows at risk for each event time
//' @noRd
//'
// [[Rcpp::export]]
void Make_Groups_STRATA(const int& ntime, const MatrixXd& df_m, IntegerMatrix& RiskFail, StringMatrix&  RiskGroup,  NumericVector& tu, const int& nthreads, bool debugging, NumericVector& STRATA_vals){
    //
    vector<vector<int>> safe_fail(ntime);
    vector<vector<string>> safe_group(ntime);
    for (int i=0;i<ntime;i++){
        safe_fail[i] = vector<int>(RiskFail.cols(),0);
        safe_group[i] = vector<string>(RiskGroup.cols(),"");
    }
    //
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
                safe_fail[ijk][2*s_ij+0] = indices_end[0]-1;//Due to the sorting method, there is a continuous block of event rows
                safe_fail[ijk][2*s_ij+1] = indices_end[indices_end.size()-1]-1;
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
                //
                ostringstream oss;
                copy(indices.begin(), indices.end(),
                    std::ostream_iterator<int>(oss, ","));
                safe_group[ijk][s_ij] = oss.str();//stores risk groups in string
            } else {
                safe_fail[ijk][2*s_ij+0] = -1;
                safe_fail[ijk][2*s_ij+1] = -1;
            }
        }
    }
    for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
        for (int ijk=0;ijk<ntime;ijk++){
            RiskFail(ijk,2*s_ij + 0)= safe_fail[ijk][2*s_ij+0];
            RiskFail(ijk,2*s_ij + 1)= safe_fail[ijk][2*s_ij+1];
            RiskGroup(ijk,s_ij) = safe_group[ijk][s_ij];
        }
    }
    return;
}

//' Utility function to define risk groups with STRATA and competing risks
//'
//' \code{Make_Groups_STRATA_CR} Called to update lists of risk groups, Uses list of event times and row time/event information, Matrices store starting/stopping row indices for each group , adds competing risks  
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Matrix of event rows for each event time, vectors of strings with rows at risk for each event time
//' @noRd
//'
// [[Rcpp::export]]
void Make_Groups_STRATA_CR(const int& ntime, const MatrixXd& df_m, IntegerMatrix& RiskFail, StringMatrix&  RiskGroup,  NumericVector& tu, const int& nthreads, bool debugging, NumericVector& STRATA_vals, const VectorXd& cens_weight){
    //
    vector<vector<int>> safe_fail(ntime);
    vector<vector<string>> safe_group(ntime);
    for (int i=0;i<ntime;i++){
        safe_fail[i] = vector<int>(RiskFail.cols(),0);
        safe_group[i] = vector<string>(RiskGroup.cols(),"");
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
        for (int ijk=0;ijk<ntime;ijk++){
            double t0 = tu[ijk];
            VectorXi select_ind_end = ((df_m.col(2).array() == 1)&&(df_m.col(1).array()==t0)&&(df_m.col(3).array()==STRATA_vals[s_ij])).cast<int>(); //indices with events
            vector<int> indices_end;
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
                safe_fail[ijk][2*s_ij+0] = indices_end[0]-1;//Due to the sorting method, there is a continuous block of event rows
                safe_fail[ijk][2*s_ij+1] = indices_end[indices_end.size()-1]-1;
                //
                select_ind_end = (((((df_m.col(0).array() < t0)||(df_m.col(0).array()==df_m.col(1).array()))&&(df_m.col(1).array()>=t0))||((df_m.col(2).array() == 2)&&(df_m.col(1).array()<=t0)))&&(df_m.col(3).array()==STRATA_vals[s_ij])).cast<int>(); //indices at risk
                indices_end.clear();
                visit_lambda(select_ind_end,
	                [&indices_end, th](double v, int i, int j) {
		                if (v==th)
				                indices_end.push_back(i+1);
	                });
                //
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
                //
                ostringstream oss;
                copy(indices.begin(), indices.end(),
                    std::ostream_iterator<int>(oss, ","));
                safe_group[ijk][s_ij] = oss.str();//stores risk groups in string
            } else {
                safe_fail[ijk][2*s_ij+0] = -1;
                safe_fail[ijk][2*s_ij+1] = -1;
            }
        }
    }
    for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
        for (int ijk=0;ijk<ntime;ijk++){
            RiskFail(ijk,2*s_ij + 0)= safe_fail[ijk][2*s_ij+0];
            RiskFail(ijk,2*s_ij + 1)= safe_fail[ijk][2*s_ij+1];
            RiskGroup(ijk,s_ij) = safe_group[ijk][s_ij];
        }
    }
    return;
}


//' Utility function to calculate repeated values used in Cox Log-Likelihood calculation
//'
//' \code{Calculate_Sides} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Sides(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3,const int& nthreads, bool debugging, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int j=0;j<ntime;j++){
        double Rs1 = 0;
        //
        //
        vector<int> InGroup;
        string Groupstr = RiskGroup[j];
        stringstream ss(Groupstr);
        //
        //
        for (int i; ss >> i;) {
            InGroup.push_back(i);    
            if (ss.peek() == ',')
                ss.ignore();
        }
        //Now has the grouping pairs
        int dj = RiskFail(j,1)-RiskFail(j,0)+1;
        for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
            Rs1 += R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).sum();
        } //precalculates the sums of risk groups
        MatrixXd Ld = MatrixXd::Zero(dj,1);
        Ld << R.block(RiskFail(j,0),0,dj,1);//sum of risks in group
        // only assigns values once
        Rls1(j,0) = Rs1;
        Lls1(j,0) = Ld.col(0).sum();
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int ij=0;ij<reqrdnum;ij++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            double Rs2 = 0;
            //
            vector<int> InGroup;
            string Groupstr = RiskGroup[j];
            stringstream ss(Groupstr);
            //
            //
            //
            for (int i; ss >> i;) {
                InGroup.push_back(i);    
                if (ss.peek() == ',')
                    ss.ignore();
            }
            //Now has the grouping pairs
            int dj = RiskFail(j,1)-RiskFail(j,0)+1;
            for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
                Rs2 += Rd.block(InGroup[i]-1,ij,InGroup[i+1]-InGroup[i]+1,1).sum();
            } //precalculates the sums of risk groups
            MatrixXd Ld = MatrixXd::Zero(dj,1);
            Ld << Rd.block(RiskFail(j,0),ij,dj,1);//sum of risks in group
            // only assigns values once
            Rls2(j,ij) = Rs2;
            Lls2(j,ij) = Ld.col(0).sum();
        }
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            double Rs3 = 0;
            //
            vector<int> InGroup;
            string Groupstr = RiskGroup[j];
            stringstream ss(Groupstr);
            //
            //
            //
            for (int i; ss >> i;) {
                InGroup.push_back(i);    
                if (ss.peek() == ',')
                    ss.ignore();
            }
            //Now has the grouping pairs
            int dj = RiskFail(j,1)-RiskFail(j,0)+1;
            for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
                Rs3 += Rdd.block(InGroup[i]-1,ijk,InGroup[i+1]-InGroup[i]+1,1).sum();
            } //precalculates the sums of risk groups
            MatrixXd Ld = MatrixXd::Zero(dj,1);
            Ld << Rdd.block(RiskFail(j,0),ijk,dj,1);//sum of risks in group
            // only assigns values once
            Rls3(j,ijk) = Rs3;
            Lls3(j,ijk) = Ld.col(0).sum();
        }
    }
    return;
}

//' Utility function to calculate repeated values used in Cox Log-Likelihood calculation
//'
//' \code{Calculate_Sides_CR} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Sides_CR(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3, const VectorXd& cens_weight,const int& nthreads, bool debugging, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int j=0;j<ntime;j++){
        double Rs1 = 0;
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
        //Now has the grouping pairs
        int dj = RiskFail(j,1)-RiskFail(j,0)+1;
		double cens_0 = cens_weight[RiskFail(j,0)];
		VectorXd weighting = VectorXd::Zero(InGroup[1]-InGroup[0]+1);
        for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
            if (weighting.size() != InGroup[i+1]-InGroup[i]+1){
				weighting.resize(InGroup[i+1]-InGroup[i]+1);
			}
			weighting.head(InGroup[i+1]-InGroup[i]+1) << cens_weight.segment(InGroup[i]-1,InGroup[i+1]-InGroup[i]+1);
			weighting = weighting / cens_0;
			weighting = (weighting.array()<1).select(weighting,1);
			//
            Rs1 += (R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).array() * weighting.head(InGroup[i+1]-InGroup[i]+1).array()).sum();
        } //precalculates the sums of risk groups
        MatrixXd Ld = MatrixXd::Zero(dj,1);
        Ld << R.block(RiskFail(j,0),0,dj,1);//sum of risks in group
        // only assigns values once
        Rls1(j,0) = Rs1;
        Lls1(j,0) = Ld.col(0).sum();
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int ij=0;ij<reqrdnum;ij++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            double Rs2 = 0;
            //
            vector<int> InGroup;
            string Groupstr = RiskGroup[j];
            stringstream ss(Groupstr);
            //
            //
            //
            for (int i; ss >> i;) {
                InGroup.push_back(i);    
                if (ss.peek() == ',')
                    ss.ignore();
            }
            //Now has the grouping pairs
            int dj = RiskFail(j,1)-RiskFail(j,0)+1;
			double cens_0 = cens_weight[RiskFail(j,0)];
			VectorXd weighting = VectorXd::Zero(InGroup[1]-InGroup[0]+1);
            for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
				if (weighting.size() != InGroup[i+1]-InGroup[i]+1){
					weighting.resize(InGroup[i+1]-InGroup[i]+1);
				}
				weighting.head(InGroup[i+1]-InGroup[i]+1) << cens_weight.segment(InGroup[i]-1,InGroup[i+1]-InGroup[i]+1);
				weighting = weighting / cens_0;
				weighting = (weighting.array()<1).select(weighting,1);
				//
                Rs2 += (Rd.block(InGroup[i]-1,ij,InGroup[i+1]-InGroup[i]+1,1).array() * weighting.head(InGroup[i+1]-InGroup[i]+1).array()).sum();
            } //precalculates the sums of risk groups
            MatrixXd Ld = MatrixXd::Zero(dj,1);
            Ld << Rd.block(RiskFail(j,0),ij,dj,1);//sum of risks in group
            // only assigns values once
            Rls2(j,ij) = Rs2;
            Lls2(j,ij) = Ld.col(0).sum();
        }
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            double Rs3 = 0;
            //
            vector<int> InGroup;
            string Groupstr = RiskGroup[j];
            stringstream ss(Groupstr);
            //
            //
            for (int i; ss >> i;) {
                InGroup.push_back(i);    
                if (ss.peek() == ',')
                    ss.ignore();
            }
            //Now has the grouping pairs
            int dj = RiskFail(j,1)-RiskFail(j,0)+1;
			double cens_0 = cens_weight[RiskFail(j,0)];
			VectorXd weighting = VectorXd::Zero(InGroup[1]-InGroup[0]+1);
            for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
				if (weighting.size() != InGroup[i+1]-InGroup[i]+1){
					weighting.resize(InGroup[i+1]-InGroup[i]+1);
				}
				weighting.head(InGroup[i+1]-InGroup[i]+1) << cens_weight.segment(InGroup[i]-1,InGroup[i+1]-InGroup[i]+1);
				weighting = weighting / cens_0;
				weighting = (weighting.array()<1).select(weighting,1);
				//
                Rs3 += (Rdd.block(InGroup[i]-1,ijk,InGroup[i+1]-InGroup[i]+1,1).array() * weighting.head(InGroup[i+1]-InGroup[i]+1).array()).sum();
            } //precalculates the sums of risk groups
            MatrixXd Ld = MatrixXd::Zero(dj,1);
            Ld << Rdd.block(RiskFail(j,0),ijk,dj,1);//sum of risks in group
            // only assigns values once
            Rls3(j,ijk) = Rs3;
            Lls3(j,ijk) = Ld.col(0).sum();
        }
    }
    return;
}

//' Utility function to calculate repeated values used in Cox Log-Likelihood calculation
//'
//' \code{Calculate_Sides_CR_SINGLE} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Sides_CR_SINGLE(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1, const VectorXd& cens_weight,const int& nthreads, bool debugging, const IntegerVector& KeepConstant){
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int j=0;j<ntime;j++){
        double Rs1 = 0;
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
        //Now has the grouping pairs
        int dj = RiskFail(j,1)-RiskFail(j,0)+1;
		double cens_0 = cens_weight[RiskFail(j,0)];
		VectorXd weighting = VectorXd::Zero(InGroup[1]-InGroup[0]+1);
        for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
            if (weighting.size() != InGroup[i+1]-InGroup[i]+1){
				weighting.resize(InGroup[i+1]-InGroup[i]+1);
			}
			weighting.head(InGroup[i+1]-InGroup[i]+1) << cens_weight.segment(InGroup[i]-1,InGroup[i+1]-InGroup[i]+1);
			weighting = weighting / cens_0;
			weighting = (weighting.array()<1).select(weighting,1);
			//
            Rs1 += (R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).array() * weighting.head(InGroup[i+1]-InGroup[i]+1).array()).sum();
        } //precalculates the sums of risk groups
        MatrixXd Ld = MatrixXd::Zero(dj,1);
        Ld << R.block(RiskFail(j,0),0,dj,1);//sum of risks in group
        // only assigns values once
        Rls1(j,0) = Rs1;
        Lls1(j,0) = Ld.col(0).sum();
    }
    return;
}

//' Utility function to calculate repeated values used in Cox Log-Likelihood calculation. but not derivatives
//'
//' \code{Calculate_Sides_Single} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Sides_Single(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1,const int& nthreads, bool debugging){
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int j=0;j<ntime;j++){
        double Rs1 = 0;
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
        //Now has the grouping pairs
        int dj = RiskFail(j,1)-RiskFail(j,0)+1;
        for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
            Rs1 += R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).sum();
        } //precalculates the sums of risk groups
        MatrixXd Ld = MatrixXd::Zero(dj,1);
        Ld << R.block(RiskFail(j,0),0,dj,1);//sum of risks in group
        Rls1(j,0) = Rs1;
        Lls1(j,0) = Ld.col(0).sum();
    }
    return;
}

//' Utility function to calculate repeated values used in Cox Log-Likelihood calculation with STRATA
//'
//' \code{Calculate_Sides_STRATA} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Sides_STRATA(const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3,const int& nthreads, bool debugging, NumericVector& STRATA_vals, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int j=0;j<ntime;j++){
        for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
            double Rs1 = 0;
            //
            vector<int> InGroup;
            //Now has the grouping pairs
            if (RiskFail(j,2*s_ij + 1)>-1){
                string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                stringstream ss(Groupstr);
                for (int i; ss >> i;) {
                    InGroup.push_back(i);    
                    if (ss.peek() == ',')
                        ss.ignore();
                }
                int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
                for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
					//
                    Rs1 += R.block(  InGroup[i]-1, 0,  InGroup[i+1]-InGroup[i]+1,1).sum();
                } //precalculates the sums of risk groups
                MatrixXd Ld = MatrixXd::Zero(dj,1);
                Ld << R.block(RiskFail(j,2*s_ij),0,dj,1);//sum of risks in group
                // only assigns values once
                Rls1(j,s_ij) = Rs1;
                Lls1(j,s_ij) = Ld.col(0).sum();
            }
        }
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(3)
    #endif
    for (int ij=0;ij<reqrdnum;ij++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                double Rs2 = 0;
                //
                vector<int> InGroup;
                //Now has the grouping pairs
                if (RiskFail(j,2*s_ij + 1)>-1){
                    string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                    stringstream ss(Groupstr);
                    //
                    for (int i; ss >> i;) {
                        InGroup.push_back(i);    
                        if (ss.peek() == ',')
                            ss.ignore();
                    }
                    int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
                    for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
                        Rs2 += Rd.block( InGroup[i]-1, ij, InGroup[i+1]-InGroup[i]+1,1).sum();
                    } //precalculates the sums of risk groups
                    MatrixXd Ld = MatrixXd::Zero(dj,1);
                    Ld << Rd.block(RiskFail(j,2*s_ij),ij,dj,1);//sum of risks in group
                    // only assigns values once
                    Rls2(j,ij*STRATA_vals.size() + s_ij) = Rs2;
                    Lls2(j,ij*STRATA_vals.size() + s_ij) = Ld.col(0).sum();
                }
            }
        }
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(3)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                int ij = 0;
                int jk = ijk;
                while (jk>ij){
                    ij++;
                    jk-=ij;
                }
                double Rs3 = 0;
                //
                vector<int> InGroup;
                //Now has the grouping pairs
                if (RiskFail(j,2*s_ij + 1)>-1){
                    string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                    stringstream ss(Groupstr);
                    //
                    for (int i; ss >> i;) {
                        InGroup.push_back(i);    
                        if (ss.peek() == ',')
                            ss.ignore();
                    }
                    int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
                    for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
                        Rs3 += Rdd.block(InGroup[i]-1, ijk,InGroup[i+1]-InGroup[i]+1,1).sum();
                    } //precalculates the sums of risk groups
                    MatrixXd Ld = MatrixXd::Zero(dj,1);
                    Ld << Rdd.block(RiskFail(j,2*s_ij),ijk,dj,1);//sum of risks in group
                    // only assigns values once
                    Rls3(j,ijk*STRATA_vals.size() + s_ij) = Rs3;
                    Lls3(j,ijk*STRATA_vals.size() + s_ij) = Ld.col(0).sum();
                }
            }
        }
    }
    return;
}

//' Utility function to calculate repeated values used in Cox Log-Likelihood calculation with STRATA and without derivative
//'
//' \code{Calculate_Sides_STRATA_Single} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group but not derivatives
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Sides_STRATA_Single(const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1,const int& nthreads, bool debugging, NumericVector& STRATA_vals, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(3)
    #endif
    for (int ij=0;ij<reqrdnum;ij++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                
                double Rs1 = 0;
                //
                vector<int> InGroup;
                //Now has the grouping pairs
                if (RiskFail(j,2*s_ij + 1)>-1){
                    string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                    stringstream ss(Groupstr);
                    //
                    for (int i; ss >> i;) {
                        InGroup.push_back(i);    
                        if (ss.peek() == ',')
                            ss.ignore();
                    }
                    int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
                    for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
                        Rs1 += R.block(  InGroup[i]-1, 0,  InGroup[i+1]-InGroup[i]+1,1).sum();
                    } //precalculates the sums of risk groups
                    MatrixXd Ld = MatrixXd::Zero(dj,1);
                    Ld << R.block(RiskFail(j,2*s_ij),0,dj,1);//sum of risks in group
                    // only assigns values once
					Rls1(j,s_ij) = Rs1;
					Lls1(j,s_ij) = Ld.col(0).sum();
                }
            }
        }
    }
    return;
}

//' Utility function to calculate repeated values used in Cox Log-Likelihood calculation with STRATA and competing risks
//'
//' \code{Calculate_Sides_STRATA_CR} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group and competing risks
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Sides_STRATA_CR(const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, MatrixXd& Rls1, MatrixXd& Rls2, MatrixXd& Rls3, MatrixXd& Lls1, MatrixXd& Lls2, MatrixXd& Lls3, const VectorXd& cens_weight,const int& nthreads, bool debugging, NumericVector& STRATA_vals, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int j=0;j<ntime;j++){
        for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
            double Rs1 = 0;
            //
            //
            vector<int> InGroup;
            //Now has the grouping pairs
            if (RiskFail(j,2*s_ij + 1)>-1){
                string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                stringstream ss(Groupstr);
            //
                for (int i; ss >> i;) {
                    InGroup.push_back(i);    
                    if (ss.peek() == ',')
                        ss.ignore();
                }
                int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
				double cens_0 = cens_weight[RiskFail(j,2*s_ij)];
				VectorXd weighting = VectorXd::Zero(InGroup[1]-InGroup[0]+1);
                for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
					if (weighting.size() < InGroup[i+1]-InGroup[i]+1){
						weighting.resize(InGroup[i+1]-InGroup[i]+1);
					}
					weighting.head(InGroup[i+1]-InGroup[i]+1) << cens_weight.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1);
					weighting = weighting / cens_0;
					weighting = (weighting.array()<1).select(weighting,1);
					//
                    Rs1 += (R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).array() * weighting.head(InGroup[i+1]-InGroup[i]+1).array()).sum();
                } //precalculates the sums of risk groups
                MatrixXd Ld = MatrixXd::Zero(dj,1);
                Ld << R.block(RiskFail(j,2*s_ij),0,dj,1);//sum of risks in group
                // only assigns values once
                Rls1(j,s_ij) = Rs1;
                Lls1(j,s_ij) = Ld.col(0).sum();
            }
        }
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(3)
    #endif
    for (int ij=0;ij<reqrdnum;ij++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                double Rs2 = 0;
                //
                vector<int> InGroup;
                //Now has the grouping pairs
                if (RiskFail(j,2*s_ij + 1)>-1){
                    string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                    stringstream ss(Groupstr);
                    //
                    for (int i; ss >> i;) {
                        InGroup.push_back(i);    
                        if (ss.peek() == ',')
                            ss.ignore();
                    }
                    int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
					double cens_0 = cens_weight[RiskFail(j,2*s_ij)];
					VectorXd weighting = VectorXd::Zero(InGroup[1]-InGroup[0]+1);
                    for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
						if (weighting.size() < InGroup[i+1]-InGroup[i]+1){
							weighting.resize(InGroup[i+1]-InGroup[i]+1);
						}
						weighting.head(InGroup[i+1]-InGroup[i]+1) << cens_weight.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1);
						weighting = weighting / cens_0;
						weighting = (weighting.array()<1).select(weighting,1);
						//
						Rs2 += (Rd.block(InGroup[i]-1,ij,InGroup[i+1]-InGroup[i]+1,1).array() * weighting.head(InGroup[i+1]-InGroup[i]+1).array()).sum();
                    } //precalculates the sums of risk groups
                    MatrixXd Ld = MatrixXd::Zero(dj,1);
                    Ld << Rd.block(RiskFail(j,2*s_ij),ij,dj,1);//sum of risks in group
                    // only assigns values once
                    Rls2(j,ij*STRATA_vals.size() + s_ij) = Rs2;
                    Lls2(j,ij*STRATA_vals.size() + s_ij) = Ld.col(0).sum();
                }
            }
        }
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(3)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                int ij = 0;
                int jk = ijk;
                while (jk>ij){
                    ij++;
                    jk-=ij;
                }
                double Rs3 = 0;
                //
                vector<int> InGroup;
                //Now has the grouping pairs
                if (RiskFail(j,2*s_ij + 1)>-1){
                    string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                    stringstream ss(Groupstr);
                    //
                    for (int i; ss >> i;) {
                        InGroup.push_back(i);    
                        if (ss.peek() == ',')
                            ss.ignore();
                    }
                    int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
					double cens_0 = cens_weight[RiskFail(j,2*s_ij)];
					VectorXd weighting = VectorXd::Zero(InGroup[1]-InGroup[0]+1);
                    for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
						if (weighting.size() < InGroup[i+1]-InGroup[i]+1){
							weighting.resize(InGroup[i+1]-InGroup[i]+1);
						}
						weighting.head(InGroup[i+1]-InGroup[i]+1) << cens_weight.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1);
						weighting = weighting / cens_0;
						weighting = (weighting.array()<1).select(weighting,1);
						//
						Rs3 += (Rdd.block(InGroup[i]-1,ijk,InGroup[i+1]-InGroup[i]+1,1).array() * weighting.head(InGroup[i+1]-InGroup[i]+1).array()).sum();
                    } //precalculates the sums of risk groups
                    MatrixXd Ld = MatrixXd::Zero(dj,1);
                    Ld << Rdd.block(RiskFail(j,2*s_ij),ijk,dj,1);//sum of risks in group
                    // only assigns values once
                    Rls3(j,ijk*STRATA_vals.size() + s_ij) = Rs3;
                    Lls3(j,ijk*STRATA_vals.size() + s_ij) = Ld.col(0).sum();
                }
            }
        }
    }
    return;
}

//' Utility function to calculate repeated values used in Cox Log-Likelihood calculation with STRATA and without derivative and with competing risks
//'
//' \code{Calculate_Sides_STRATA_Single_CR} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group but not derivatives but with competing risks
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Sides_STRATA_Single_CR(const IntegerMatrix& RiskFail, const StringMatrix&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1, const VectorXd& cens_weight,const int& nthreads, bool debugging, NumericVector& STRATA_vals, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(3)
    #endif
    for (int ij=0;ij<reqrdnum;ij++){//totalnum*(totalnum+1)/2
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                double Rs1 = 0;
                //
                vector<int> InGroup;
                //Now has the grouping pairs
                if (RiskFail(j,2*s_ij + 1)>-1){
                    string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                    stringstream ss(Groupstr);
                    //
                    for (int i; ss >> i;) {
                        InGroup.push_back(i);    
                        if (ss.peek() == ',')
                            ss.ignore();
                    }
                    int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
					double cens_0 = cens_weight[RiskFail(j,2*s_ij)];
					VectorXd weighting = VectorXd::Zero(InGroup[1]-InGroup[0]+1);
                    for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
                        if (weighting.size() < InGroup[i+1]-InGroup[i]+1){
							weighting.resize(InGroup[i+1]-InGroup[i]+1);
						}
						weighting.head(InGroup[i+1]-InGroup[i]+1) << cens_weight.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1);
						weighting = weighting / cens_0;
						weighting = (weighting.array()<1).select(weighting,1);
						//
                        Rs1 += (R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).array() * weighting.head(InGroup[i+1]-InGroup[i]+1).array()).sum();
                    } //precalculates the sums of risk groups
                    MatrixXd Ld = MatrixXd::Zero(dj,1);
                    Ld << R.block(RiskFail(j,2*s_ij),0,dj,1);//sum of risks in group
                    // only assigns values once
					Rls1(j,s_ij) = Rs1;
					Lls1(j,s_ij) = Ld.col(0).sum();
                }
            }
        }
    }
    return;
}

//' Utility function to calculate Cox Log-Likelihood and derivatives
//'
//' \code{Calc_LogLik} Called to update log-likelihoods, Uses list of event rows, risk matrices, and repeated sums, Sums the log-likelihood contribution from each event time
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_LogLik(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR, const MatrixXd& RddR,const MatrixXd& Rls1,const MatrixXd& Rls2,const MatrixXd& Rls3,const MatrixXd& Lls1,const MatrixXd& Lls2,const MatrixXd& Lls3, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging,string ties_method, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll,Lld,Lldd) collapse(2)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//performs log-likelihood calculations for every derivative combination and risk group
        for (int j=0;j<ntime;j++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            double Rs1 = Rls1(j,0);
            double Rs2 = Rls2(j,ij);
            double Rs2t = Rls2(j,jk);
            double Rs3 = Rls3(j,ijk);
            //
            int dj = RiskFail(j,1)-RiskFail(j,0)+1;
            MatrixXd Ld = MatrixXd::Zero(dj,4);
            Ld << R.block(RiskFail(j,0),0,dj,1), RdR.block(RiskFail(j,0),ij,dj,1), RdR.block(RiskFail(j,0),jk,dj,1) ,RddR.block(RiskFail(j,0),ijk,dj,1);//rows with events
            //
            MatrixXd Ldm = MatrixXd::Zero(dj,4);
            Vector4d Ldcs;
            if (ties_method=="efron"){
                Ldcs << Lls1(j,0), Lls2(j,ij), Lls2(j,jk), Lls3(j,ijk);
                for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                    Ldm.row(i) = (-double(i) / double(dj)) *Ldcs.array();
                }
            }
            Ldm.col(0) = Ldm.col(0).array() + Rs1;
            Ldm.col(1) = Ldm.col(1).array() + Rs2;
            Ldm.col(2) = Ldm.col(2).array() + Rs2t;
            Ldm.col(3) = Ldm.col(3).array() + Rs3;
            // Calculates the left-hand side terms
            //
            double Ld1;
            double Ld2;
            double Ld3;
            //
            MatrixXd temp1 = MatrixXd::Zero(Ld.rows(),1);
            MatrixXd temp2 = MatrixXd::Zero(Ld.rows(),1);
            if (ij==jk){
                temp1 = Ld.col(0).array().log();
                Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
            }
            temp1 = Ld.col(1).array();
            temp2 = Ld.col(2).array();
            if (ij==jk){
                Ld2 = (temp1.array().isFinite()).select(temp1,0).sum();
            }
            temp1 = Ld.col(3).array() - (temp1.array() * temp2.array());
            Ld3 = (temp1.array().isFinite()).select(temp1,0).sum();
            // calculates the right-hand side terms
            if (ij==jk){
                temp1 = Ldm.col(0).array().log();
                Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
            }
            temp1 = Ldm.col(1).array() * (Ldm.col(0).array().pow(-1).array());
            temp2 = Ldm.col(2).array() * (Ldm.col(0).array().pow(-1).array());
            if (ij==jk){
                Rs2 = (temp1.array().isFinite()).select(temp1,0).sum();
            }
            temp1 = Ldm.col(3).array() * (Ldm.col(0).array().pow(-1).array()) - temp1.array() * temp2.array();
            Rs3 = (temp1.array().isFinite()).select(temp1,0).sum();
            //
            if (ij==jk){
                Ll[ij] += Ld1 - Rs1;
                Lld[ij] += Ld2 - Rs2;
            }
            Lldd[ij*reqrdnum+jk] += Ld3 - Rs3; //sums the log-likelihood and derivatives
        }
    }
    double LogLik = 0;
    for (int i=0;i<reqrdnum;i++){
        if (Ll[i]!=0){
            LogLik=Ll[i];
            break;
        }
    }
    fill(Ll.begin(), Ll.end(), LogLik);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//fills second-derivative matrix
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd[jk*reqrdnum+ij] = Lldd[ij*reqrdnum+jk];
    }
    return;
}

//' Utility function to calculate Cox Log-Likelihood and derivatives, basic model
//'
//' \code{Calc_LogLik_Basic} Basic model, Called to update log-likelihoods, Uses list of event rows, risk matrices, and repeated sums, Sums the log-likelihood contribution from each event time
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_LogLik_Basic(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR,const MatrixXd& Rls1,const MatrixXd& Rls2,const MatrixXd& Rls3,const MatrixXd& Lls1,const MatrixXd& Lls2,const MatrixXd& Lls3, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging,string ties_method, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll,Lld,Lldd) collapse(2)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//performs log-likelihood calculations for every derivative combination and risk group
        for (int j=0;j<ntime;j++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            double Rs1 = Rls1(j,0);
            double Rs2 = Rls2(j,ij);
            double Rs2t = Rls2(j,jk);
            double Rs3 = Rls3(j,ijk);
            //
            int dj = RiskFail(j,1)-RiskFail(j,0)+1;
            MatrixXd Ld = MatrixXd::Zero(dj,2);
            Ld << R.block(RiskFail(j,0),0,dj,1), RdR.block(RiskFail(j,0),ij,dj,1);//rows with events
            //
            MatrixXd Ldm = MatrixXd::Zero(dj,4);
            Vector4d Ldcs;
            if (ties_method=="efron"){
                Ldcs << Lls1(j,0), Lls2(j,ij), Lls2(j,jk), Lls3(j,ijk);
                for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                    Ldm.row(i) = (-double(i) / double(dj)) *Ldcs.array();
                }
            }
            Ldm.col(0) = Ldm.col(0).array() + Rs1;
            Ldm.col(1) = Ldm.col(1).array() + Rs2;
            Ldm.col(2) = Ldm.col(2).array() + Rs2t;
            Ldm.col(3) = Ldm.col(3).array() + Rs3;
            // Calculates the left-hand side terms
            //
            double Ld1;
            double Ld2;
            //
            MatrixXd temp1 = MatrixXd::Zero(Ld.rows(),1);
            MatrixXd temp2 = MatrixXd::Zero(Ld.rows(),1);
            if (ij==jk){
                temp1 = Ld.col(0).array().log();
                Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                temp1 = Ld.col(1).array();
                Ld2 = (temp1.array().isFinite()).select(temp1,0).sum();
            }
            // calculates the right-hand side terms
            if (ij==jk){
                temp1 = Ldm.col(0).array().log();
                Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
            }
            temp1 = Ldm.col(1).array() * (Ldm.col(0).array().pow(-1).array());
            temp2 = Ldm.col(2).array() * (Ldm.col(0).array().pow(-1).array());
            if (ij==jk){
                Rs2 = (temp1.array().isFinite()).select(temp1,0).sum();
            }
            temp1 = Ldm.col(3).array() * (Ldm.col(0).array().pow(-1).array()) - temp1.array() * temp2.array();
            Rs3 = (temp1.array().isFinite()).select(temp1,0).sum();
            //
            if (ij==jk){
                Ll[ij] += Ld1 - Rs1;
                Lld[ij] += Ld2 - Rs2;
            }
            Lldd[ij*reqrdnum+jk] += 0 - Rs3; //sums the log-likelihood and derivatives
        }
    }
    double LogLik = 0;
    for (int i=0;i<reqrdnum;i++){
        if (Ll[i]!=0){
            LogLik=Ll[i];
            break;
        }
    }
    fill(Ll.begin(), Ll.end(), LogLik);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//fills second-derivative matrix
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd[jk*reqrdnum+ij] = Lldd[ij*reqrdnum+jk];
    }
    return;
}

//' Utility function to calculate Cox Log-Likelihood, basic model
//'
//' \code{Calc_LogLik_Basic_Single} Basic model, Called to update log-likelihoods, Uses list of event rows, risk matrices, and repeated sums, Sums the log-likelihood contribution from each event time
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_LogLik_Basic_Single(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, bool debugging,string ties_method, const IntegerVector& KeepConstant){
	#ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll)
    #endif
	for (int j=0;j<ntime;j++){
		double Rs1 = Rls1(j,0);
		//
		int dj = RiskFail(j,1)-RiskFail(j,0)+1;
		MatrixXd Ld = MatrixXd::Zero(dj,1);
		Ld << R.block(RiskFail(j,0),0,dj,1);//rows with events
		//
		MatrixXd Ldm = MatrixXd::Zero(dj,1);
		double Ldcs;
		if (ties_method=="efron"){
			Ldcs = Lls1(j,0);
			for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
				Ldm(i,0) = (-double(i) / double(dj)) *Ldcs;
			}
		}
		
		Ldm.col(0) = Ldm.col(0).array() + Rs1;
        // Calculates the left-hand side terms
        MatrixXd temp1 = MatrixXd::Zero(Ld.rows(),1);
        temp1 = Ld.col(0).array().log();
        double Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
        // calculates the right-hand side terms
        temp1 = Ldm.col(0).array().log();
        Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
        //
        Ll[0] += Ld1 - Rs1;
		
	}
    double LogLik = Ll[0];
    fill(Ll.begin(), Ll.end(), LogLik);
    return;
}

//' Utility function to calculate Cox Log-Likelihood
//'
//' \code{Calc_LogLik_Single} Called to update log-likelihoods, Uses list of event rows, risk matrices, and repeated sums, Sums the log-likelihood contribution from each event time
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_LogLik_Single(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R,const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, bool debugging,string ties_method){
	#ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll)
    #endif
    for (int j=0;j<ntime;j++){
        double Rs1 = Rls1(j,0);
        //
        int dj = RiskFail(j,1)-RiskFail(j,0)+1;
        MatrixXd Ld = MatrixXd::Zero(dj,1);
        Ld << R.block(RiskFail(j,0),0,dj,1);//rows with events
        //
        MatrixXd Ldm = MatrixXd::Zero(dj,1);
        double Ldcs;
        if (ties_method=="efron"){
            Ldcs = Lls1(j,0);
            for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                Ldm(i,0) = (-double(i) / double(dj)) * Ldcs;
            }
        }
        Ldm.col(0) = Ldm.col(0).array() + Rs1;
        // Calculates the left-hand side terms
        MatrixXd temp1 = MatrixXd::Zero(Ld.rows(),1);
        temp1 = Ld.col(0).array().log();
        double Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
        // calculates the right-hand side terms
        temp1 = Ldm.col(0).array().log();
        Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
        //
        Ll[0] += Ld1 - Rs1;
    }
    return;
}

//' Utility function to calculate Cox Log-Likelihood and derivatives with STRATA
//'
//' \code{Calc_LogLik_STRATA} Called to update log-likelihoods, Uses list of event rows, risk matrices, and repeated sums, Sums the log-likelihood contribution from each event time
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_LogLik_STRATA(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR, const MatrixXd& RddR,const MatrixXd& Rls1,const MatrixXd& Rls2,const MatrixXd& Rls3,const MatrixXd& Lls1,const MatrixXd& Lls2,const MatrixXd& Lls3, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging,string ties_method, NumericVector& STRATA_vals, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll,Lld,Lldd) collapse(3)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//performs log-likelihood calculations for every derivative combination and risk group
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                int ij = 0;
                int jk = ijk;
                while (jk>ij){
                    ij++;
                    jk-=ij;
                }
                double Rs1 =  Rls1(j,s_ij);
                double Rs2 =  Rls2(j,ij*STRATA_vals.size() + s_ij);
                double Rs2t = Rls2(j,jk*STRATA_vals.size() + s_ij);
                double Rs3 =  Rls3(j,ijk*STRATA_vals.size() + s_ij);
                //
                int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
                if (RiskFail(j,2*s_ij + 1)>-1){
                    MatrixXd Ld = MatrixXd::Zero(dj,4);
                    Ld << R.block(RiskFail(j,2*s_ij),0,dj,1), RdR.block(RiskFail(j,2*s_ij),ij,dj,1), RdR.block(RiskFail(j,2*s_ij),jk,dj,1) ,RddR.block(RiskFail(j,2*s_ij),ijk,dj,1);//rows with events
                    //
                    MatrixXd Ldm = MatrixXd::Zero(dj,4);
                    Vector4d Ldcs;
                    if (ties_method=="efron"){
                        Ldcs << Lls1(j,s_ij), Lls2(j,ij*STRATA_vals.size() + s_ij), Lls2(j,jk*STRATA_vals.size() + s_ij), Lls3(j,ijk*STRATA_vals.size() + s_ij);
                        for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                            Ldm.row(i) = (-double(i) / double(dj)) *Ldcs.array();
                        }
                    }
                    Ldm.col(0) = Ldm.col(0).array() + Rs1;
                    Ldm.col(1) = Ldm.col(1).array() + Rs2;
                    Ldm.col(2) = Ldm.col(2).array() + Rs2t;
                    Ldm.col(3) = Ldm.col(3).array() + Rs3;
                    // Calculates the left-hand side terms
                    //
                    double Ld1;
                    double Ld2;
                    double Ld3;
                    //
                    MatrixXd temp1 = MatrixXd::Zero(Ld.rows(),1);
                    MatrixXd temp2 = MatrixXd::Zero(Ld.rows(),1);
                    if (ij==jk){
                        temp1 = Ld.col(0).array().log();
                        Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                    }
                    temp1 = Ld.col(1).array();
                    temp2 = Ld.col(2).array();
                    if (ij==jk){
                        Ld2 = (temp1.array().isFinite()).select(temp1,0).sum();
                    }
                    temp1 = Ld.col(3).array() - (temp1.array() * temp2.array());
                    Ld3 = (temp1.array().isFinite()).select(temp1,0).sum();
                    // calculates the right-hand side terms
                    if (ij==jk){
                        temp1 = Ldm.col(0).array().log();
                        Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                    }
                    temp1 = Ldm.col(1).array() * (Ldm.col(0).array().pow(-1).array());
                    temp2 = Ldm.col(2).array() * (Ldm.col(0).array().pow(-1).array());
                    if (ij==jk){
                        Rs2 = (temp1.array().isFinite()).select(temp1,0).sum();
                    }
                    temp1 = Ldm.col(3).array() * (Ldm.col(0).array().pow(-1).array()) - temp1.array() * temp2.array();
                    Rs3 = (temp1.array().isFinite()).select(temp1,0).sum();
                    //
                    if (ij==jk){
                        Ll[ij] += Ld1 - Rs1;
                        Lld[ij] += Ld2 - Rs2;
                    }
                    Lldd[ij*reqrdnum+jk] += Ld3 - Rs3; //sums the log-likelihood and derivatives
                }
            }
        }
    }
    double LogLik = 0;
    for (int i=0;i<reqrdnum;i++){
        if (Ll[i]!=0){
            LogLik=Ll[i];
            break;
        }
    }
    fill(Ll.begin(), Ll.end(), LogLik);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//fills second-derivative matrix
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd[jk*reqrdnum+ij] = Lldd[ij*reqrdnum+jk];
    }
    return;
}

//' Utility function to calculate just Cox Log-Likelihood with STRATA
//'
//' \code{Calc_LogLik_STRATA_SINGLE} Called to update log-likelihoods, Uses list of event rows, risk matrices, and repeated sums, Sums the log-likelihood contribution from each event time and strata
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_LogLik_STRATA_SINGLE(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R,const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, bool debugging,string ties_method, NumericVector& STRATA_vals, const IntegerVector& KeepConstant){
//    int reqrdnum = totalnum - sum(KeepConstant);
	#ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll) collapse(2)
    #endif
    for (int j=0;j<ntime;j++){
        for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
            double Rs1 =  Rls1(j,s_ij);
            //
            int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
            if (RiskFail(j,2*s_ij + 1)>-1){
                MatrixXd Ld = MatrixXd::Zero(dj,1);
                Ld << R.block(RiskFail(j,2*s_ij),0,dj,1);//rows with events
                //
                MatrixXd Ldm = MatrixXd::Zero(dj,1);
                double Ldcs;
                if (ties_method=="efron"){
                    Ldcs = Lls1(j,s_ij);
                    for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                        Ldm(i,0) = (-double(i) / double(dj)) *Ldcs;
                    }
                }
                Ldm.col(0) = Ldm.col(0).array() + Rs1;
                // Calculates the left-hand side terms
                //
                double Ld1;
                //
                MatrixXd temp1 = MatrixXd::Zero(Ld.rows(),1);
                temp1 = Ld.col(0).array().log();
                Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                //
                temp1 = Ldm.col(0).array().log();
                Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                Ll[0] += Ld1 - Rs1;
            }
        }
    }
    fill(Ll.begin(), Ll.end(), Ll[0]);
    return;
}

//' Utility function to calculate Cox Log-Likelihood and derivatives with STRATA, basic model
//'
//' \code{Calc_LogLik_STRATA_BASIC} Called to update log-likelihoods, Uses list of event rows, risk matrices, and repeated sums, Sums the log-likelihood contribution from each event time, basic model
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_LogLik_STRATA_BASIC(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR,const MatrixXd& Rls1,const MatrixXd& Rls2,const MatrixXd& Rls3,const MatrixXd& Lls1,const MatrixXd& Lls2,const MatrixXd& Lls3, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging,string ties_method, NumericVector& STRATA_vals, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll,Lld,Lldd) collapse(3)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//performs log-likelihood calculations for every derivative combination and risk group
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                int ij = 0;
                int jk = ijk;
                while (jk>ij){
                    ij++;
                    jk-=ij;
                }
                if (RiskFail(j,2*s_ij + 1)>-1){
                    double Rs1 = Rls1(j,s_ij);
                    double Rs2 =  Rls2(j,ij*STRATA_vals.size() + s_ij);
                    double Rs2t = Rls2(j,jk*STRATA_vals.size() + s_ij);
                    double Rs3 = Rls3(j,ijk*STRATA_vals.size() + s_ij);
                    //
                    int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
                    MatrixXd Ld = MatrixXd::Zero(dj,2);
                    Ld << R.block(RiskFail(j,2*s_ij),0,dj,1), RdR.block(RiskFail(j,2*s_ij),ij,dj,1);//rows with events
                    //
                    MatrixXd Ldm = MatrixXd::Zero(dj,4);
                    Vector4d Ldcs;
                    if (ties_method=="efron"){
                        Ldcs << Lls1(j,s_ij), Lls2(j,ij*STRATA_vals.size() + s_ij), Lls2(j,jk*STRATA_vals.size() + s_ij), Lls3(j,ijk*STRATA_vals.size() + s_ij);
                        for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                            Ldm.row(i) = (-double(i) / double(dj)) *Ldcs.array();
                        }
                    }
                    Ldm.col(0) = Ldm.col(0).array() + Rs1;
                    Ldm.col(1) = Ldm.col(1).array() + Rs2;
                    Ldm.col(2) = Ldm.col(2).array() + Rs2t;
                    Ldm.col(3) = Ldm.col(3).array() + Rs3;
                    // Calculates the left-hand side terms
                    //
                    double Ld1;
                    double Ld2;
                    //
                    MatrixXd temp1 = MatrixXd::Zero(Ld.rows(),1);
                    MatrixXd temp2 = MatrixXd::Zero(Ld.rows(),1);
                    if (ij==jk){
                        temp1 = Ld.col(0).array().log();
                        Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                        temp1 = Ld.col(1).array();
                        Ld2 = (temp1.array().isFinite()).select(temp1,0).sum();
                    }
                    // calculates the right-hand side terms
                    if (ij==jk){
                        temp1 = Ldm.col(0).array().log();
                        Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                    }
                    temp1 = Ldm.col(1).array() * (Ldm.col(0).array().pow(-1).array());
                    temp2 = Ldm.col(2).array() * (Ldm.col(0).array().pow(-1).array());
                    if (ij==jk){
                        Rs2 = (temp1.array().isFinite()).select(temp1,0).sum();
                    }
                    temp1 = Ldm.col(3).array() * (Ldm.col(0).array().pow(-1).array()) - temp1.array() * temp2.array();
                    Rs3 = (temp1.array().isFinite()).select(temp1,0).sum();
                    //
                    if (ij==jk){
                        Ll[ij] += Ld1 - Rs1;
                        Lld[ij] += Ld2 - Rs2;
                    }
                    Lldd[ij*reqrdnum+jk] += 0 - Rs3; //sums the log-likelihood and derivatives
                }
            }
        }
    }
    double LogLik = 0;
    for (int i=0;i<reqrdnum;i++){
        if (Ll[i]!=0){
            LogLik=Ll[i];
            break;
        }
    }
    fill(Ll.begin(), Ll.end(), LogLik);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//fills second-derivative matrix
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        Lldd[jk*reqrdnum+ij] = Lldd[ij*reqrdnum+jk];
    }
    return;
}

//' Utility function to calculate Cox Log-Likelihood and derivatives with STRATA, basic model, no derivatives
//'
//' \code{Calc_LogLik_STRATA_BASIC_SINGLE} Called to update log-likelihoods, Uses list of event rows, risk matrices, and repeated sums, Sums the log-likelihood contribution from each event time, basic model
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_LogLik_STRATA_BASIC_SINGLE(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& totalnum, const int& ntime, const MatrixXd& R ,const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, bool debugging,string ties_method, NumericVector& STRATA_vals, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    #ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll) collapse(3)
    #endif
    for (int ij=0;ij<reqrdnum;ij++){//performs log-likelihood calculations for every derivative combination and risk group
        for (int j=0;j<ntime;j++){
            for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
                if (RiskFail(j,2*s_ij + 1)>-1){
                    double Rs1 = Rls1(j,s_ij);
                    //
                    int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
                    MatrixXd Ld = MatrixXd::Zero(dj,1);
                    Ld << R.block(RiskFail(j,2*s_ij),0,dj,1);//rows with events
                    //
                    MatrixXd Ldm = MatrixXd::Zero(dj,1);
                    double Ldcs;
                    if (ties_method=="efron"){
                        Ldcs = Lls1(j,s_ij);
                        for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                            Ldm(i,0) = (-double(i) / double(dj)) *Ldcs;
                        }
                    }
                    Ldm.col(0) = Ldm.col(0).array() + Rs1;
                    // Calculates the left-hand side terms
                    //
                    double Ld1;
                    //
                    MatrixXd temp1 = MatrixXd::Zero(Ld.rows(),1);
                    //
                    temp1 = Ld.col(0).array().log();
                    Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                    // calculates the right-hand side terms
                    temp1 = Ldm.col(0).array().log();
                    Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                    //
                    Ll[ij] += Ld1 - Rs1;
                }
            }
        }
    }
    double LogLik = 0;
    for (int i=0;i<reqrdnum;i++){
        if (Ll[i]!=0){
            LogLik=Ll[i];
            break;
        }
    }
    fill(Ll.begin(), Ll.end(), LogLik);
    return;
}

//' Utility function to calculate poisson log-likelihood and derivatives
//'
//' \code{Poisson_LogLik} Called to update log-likelihoods, Uses list risk matrices and person-years, Sums the log-likelihood contribution from each row
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Poisson_LogLik(const int& nthreads, const int& totalnum, const MatrixXd& PyrC, const MatrixXd& R, const MatrixXd& Rd, const MatrixXd& Rdd, const MatrixXd& RdR, const MatrixXd& RddR, vector<double>& Ll, vector<double>& Lld, vector<double>& Lldd, bool debugging, const IntegerVector& KeepConstant){
    int reqrdnum = totalnum - sum(KeepConstant);
    MatrixXd temp(Rd.rows(),Rd.cols());
    VectorXd CoL=VectorXd::Zero(Rd.rows());
    
    temp = (PyrC.col(1).array() * (PyrC.col(0).array() * R.col(0).array()).array().log()).array() - (PyrC.col(0).array() * R.col(0).array());
    fill(Ll.begin(), Ll.end(), (temp.array().isFinite()).select(temp,0).sum());
    
    CoL = PyrC.col(1).array() * R.col(0).array().pow(-1).array();
	#ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<reqrdnum*(reqrdnum+1)/2;ijk++){//totalnum*(totalnum+1)/2
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        VectorXd temp(Rdd.rows(),1);
        temp = Rdd.col(ijk).array() * ( CoL.array() - PyrC.col(0).array()) - PyrC.col(1).array() * RdR.col(ij).array() * RdR.col(jk).array();
        Lldd[ij*reqrdnum+jk] = (temp.array().isFinite()).select(temp,0).sum();
        if (ij!=jk){
            Lldd[jk*reqrdnum+ij] = (temp.array().isFinite()).select(temp,0).sum();
        } else{
            temp = Rd.col(ij).array() * ( CoL.array() - PyrC.col(0).array());
            Lld[ij] = (temp.array().isFinite()).select(temp,0).sum();
        }
    }
    return;
}

//' Utility function to calculate poisson log-likelihood
//'
//' \code{Poisson_LogLik_Single} Called to update log-likelihoods, Uses list risk matrices and person-years, Sums the log-likelihood contribution from each row
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Poisson_LogLik_Single(const int& nthreads, const int& totalnum, const MatrixXd& PyrC, const MatrixXd& R, vector<double>& Ll, bool debugging){
    int reqrdnum = Ll.size();
    MatrixXd temp(R.rows(),reqrdnum);
    temp = (PyrC.col(1).array() * (PyrC.col(0).array() * R.col(0).array()).array().log()).array() - (PyrC.col(0).array() * R.col(0).array());
    fill(Ll.begin(), Ll.end(), (temp.array().isFinite()).select(temp,0).sum());
    return;
}

//' Utility function to perform null model equivalent of Calculate_Sides
//'
//' \code{Calculate_Null_Sides} Called to update repeated sum calculations, Uses list of event rows, Performs calculation of counts in each group
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Null_Sides(const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1,const int& nthreads){
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int j=0;j<ntime;j++){
        double Rs1 = 0;
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
        //Now has the grouping pairs
        int dj = RiskFail(j,1)-RiskFail(j,0)+1;
        for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
            Rs1 += R.block(InGroup[i]-1,0,InGroup[i+1]-InGroup[i]+1,1).sum();
        } //precalculates the sums of risk groups
        MatrixXd Ld = MatrixXd::Zero(dj,1);
        Ld << R.block(RiskFail(j,0),0,dj,1);//sum of risks in group
        // only assigns values once
        Rls1(j,0) = Rs1;
        Lls1(j,0) = Ld.col(0).sum();
    }
    return;
}


//' Utility function to perform null model equivalent of Calc_LogLik
//'
//' \code{Calc_Null_LogLik} Called to update log-likelihoods, Uses list of event rows and repeated sums, Sums the log-likelihood contribution from each event time
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_Null_LogLik(const int& nthreads,const IntegerMatrix& RiskFail, const vector<string>&  RiskGroup, const int& ntime, const MatrixXd& R, const MatrixXd& Rls1,const MatrixXd& Lls1, vector<double>& Ll, string ties_method){
	#ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll)
    #endif
    for (int j=0;j<ntime;j++){
        double Rs1 = Rls1(j,0);
        int dj = RiskFail(j,1)-RiskFail(j,0)+1;
        //
        MatrixXd Ld = MatrixXd::Zero(dj,1);
        Ld << R.block(RiskFail(j,0),0,dj,1);//rows with events
        //
        MatrixXd Ldm = MatrixXd::Zero(dj,1);
        Vector4d Ldcs;
        if (ties_method=="efron"){
            Ldcs << Lls1(j,0);
            for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                Ldm.row(i) = (-double(i) / double(dj)) *Ldcs.array();
            }
        }
        Ldm.col(0) = Ldm.col(0).array() + Rs1;
        // Calculates the left-hand side terms
        MatrixXd temp1 = Ld.col(0).array().log();
        double Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
        // calculates the right-hand side terms
        temp1 = Ldm.col(0).array().log();
        Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
        //
        Ll[0] += Ld1 - Rs1;
    }
    return;
}

//' Utility function to perform null model equivalent of Calculate_Sides with strata
//'
//' \code{Calculate_Null_Sides_STRATA} Called to update repeated sum calculations, Uses list of event rows, Performs calculation of counts in each group
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Calculate_Null_Sides_STRATA(const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& ntime, const MatrixXd& R, MatrixXd& Rls1, MatrixXd& Lls1, NumericVector& STRATA_vals,const int& nthreads){
	#ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) collapse(2)
    #endif
    for (int j=0;j<ntime;j++){
        for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
            double Rs1 = 0;
            //
            //
            vector<int> InGroup;
            //Now has the grouping pairs
            if (RiskFail(j,2*s_ij + 1)>-1){
                string Groupstr = as<std::string>(RiskGroup(j,s_ij));
                stringstream ss(Groupstr);
                for (int i; ss >> i;) {
                    InGroup.push_back(i);    
                    if (ss.peek() == ',')
                        ss.ignore();
                }
                int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
                for (vector<double>::size_type i = 0; i < InGroup.size()-1; i=i+2){
					//
                    Rs1 += InGroup[i+1]-InGroup[i]+1;
                } //precalculates the sums of risk groups
                // only assigns values once
                Rls1(j,s_ij) = Rs1;
                Lls1(j,s_ij) = dj;
            }
        }
    }
    return;
}




//' Utility function to perform null model equivalent of Calc_LogLik
//'
//' \code{Calc_Null_LogLik_STRATA} Called to update log-likelihoods, Uses list of event rows and repeated sums, Sums the log-likelihood contribution from each event time
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Log-likelihood vectors/matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_Null_LogLik_STRATA(const int& nthreads,const IntegerMatrix& RiskFail, const StringMatrix& RiskGroup, const int& ntime, const MatrixXd& R, const MatrixXd& Rls1,const MatrixXd& Lls1, NumericVector& STRATA_vals, vector<double>& Ll, string ties_method){
	#ifdef _OPENMP
    #pragma omp declare reduction(vec_double_plus : std::vector<double> : \
        std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<double>())) \
        initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(vec_double_plus:Ll) collapse(2)
    #endif
    for (int s_ij=0;s_ij<STRATA_vals.size();s_ij++){
        for (int j=0;j<ntime;j++){
            double Rs1 =  Rls1(j,s_ij);
            int dj = RiskFail(j,2*s_ij + 1)-RiskFail(j,2*s_ij + 0)+1;
            if (RiskFail(j,2*s_ij + 1)>-1){
                //
                MatrixXd Ld = MatrixXd::Constant(dj,1,1.0);
                //
                MatrixXd Ldm = MatrixXd::Zero(dj,1);
                double Ldcs;
                if (ties_method=="efron"){
                    Ldcs = Lls1(j,s_ij);
                    for (int i = 0; i < dj; i++){ //adds in the efron approximation terms
                        Ldm(i,0) = (-double(i) / double(dj)) *Ldcs;
                    }
                }
                Ldm.col(0) = Ldm.col(0).array() + Rs1;
                // Calculates the left-hand side terms
                MatrixXd temp1 = Ld.col(0).array().log();
                double Ld1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                // calculates the right-hand side terms
                temp1 = Ldm.col(0).array().log();
                Rs1 =  (temp1.array().isFinite()).select(temp1,0).sum();
                //
                Ll[0] += Ld1 - Rs1;
            }
        }
    }
    return;
}
