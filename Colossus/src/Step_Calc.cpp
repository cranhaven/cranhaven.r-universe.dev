#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include "Step_Calc.h"
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

//' Utility function to keep intercept parameters within the range of possible values
//'
//' \code{Intercept_Bound} Called to update the parameter list in the event that intercepts leave the bounds of possible values
//' @inheritParams CPP_template
//' 
//' @return Updates vector in place: parameter vector
//' @noRd
//'
// [[Rcpp::export]]
void Intercept_Bound(const int& nthreads, const int& totalnum, const VectorXd& beta_0, vector<double>& dbeta, const IntegerVector& dfc, const  MatrixXd& df0, const IntegerVector& KeepConstant, bool debugging,const StringVector&  tform){
    set<string> Dose_Iden; //List of dose subterms
    Dose_Iden.insert( "lin_int");
    Dose_Iden.insert("step_int");
    Dose_Iden.insert("lin_quad_int");
    Dose_Iden.insert("lin_exp_int");
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ij=0;ij<totalnum;ij++){
        if ((Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end())&&(KeepConstant[ij]==0)){
            int df0_c = dfc[ij]-1;
            double pmin = (df0.col(df0_c)).array().minCoeff();
            double pmax = (df0.col(df0_c)).array().maxCoeff();
            double db_temp = beta_0[ij] + dbeta[ij];
            if (db_temp<pmin){
                dbeta[ij] = pmin-beta_0[ij];
            } else if (db_temp>pmax){
                dbeta[ij] = pmax-beta_0[ij];
            }
        }
    }
    return;
}

//' Utility function to calculate the change to make each iteration, applying linear constraints
//'
//' \code{Calc_Change_Cons} Called to update the parameter changes, Uses log-likelihoods and control parameters, Applies newton steps and change limitations with a system of constraints    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: parameter change matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_Change_Cons(const MatrixXd& Lin_Sys, const VectorXd& Lin_Res, const  VectorXd& beta_0, const int& nthreads, const int& totalnum, const int& der_iden, const double& dose_abs_max, const double& lr, const double& abs_max, const vector<double>& Ll, const vector<double>& Lld, const vector<double>& Lldd, vector<double>& dbeta,const StringVector&   tform, const double& dint, const double& dslp, IntegerVector KeepConstant, bool debugging){
    //
    int kept_covs = totalnum - sum(KeepConstant);
    //
    VectorXd beta_1(kept_covs);
    for (int ij=0;ij<totalnum;ij++){
        if (KeepConstant[ij]==0){
            int pij_ind = ij - sum(head(KeepConstant,ij));
            beta_1(pij_ind) = beta_0(ij);
        }
    }
    VectorXd Lin_Dif = Lin_Sys * beta_1 - Lin_Res;
    //
    int total_covs = kept_covs + Lin_Sys.rows();
    //
    NumericVector Lldd_vec(total_covs*total_covs);
    NumericVector Lld_vec(total_covs);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<total_covs*(total_covs+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        if (ij < kept_covs){
            Lldd_vec[jk * total_covs + ij]=Lldd[jk * kept_covs + ij];
            if (ij==jk){
                Lld_vec[ij]=Lld[ij];
            } else {
                Lldd_vec[ij * total_covs + jk]=Lldd_vec[jk * kept_covs + ij];
            }
        } else {
            if (jk < kept_covs) {
                Lldd_vec[jk * total_covs + ij]=Lin_Sys(ij-kept_covs,jk);
            } else {
                Lldd_vec[jk * total_covs + ij]=0.0;
            }
            if (ij==jk){
                Lld_vec[ij]=Lin_Dif(ij-kept_covs);
            } else {
                Lldd_vec[ij * total_covs + jk]=Lldd_vec[jk * total_covs + ij];
            }
        }
    }
    //
    //
    Lldd_vec.attr("dim") = Dimension(total_covs, total_covs);
    const Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
    const Map<VectorXd> Lld_mat(as<Map<VectorXd> >(Lld_vec));
    //
    //
    VectorXd Lldd_solve0 = Lldd_mat.colPivHouseholderQr().solve(-1*Lld_mat);
    VectorXd Lldd_solve = VectorXd::Zero(totalnum);
    for (int ij=0;ij<totalnum;ij++){
        if (KeepConstant[ij]==0){
            int pij_ind = ij - sum(head(KeepConstant,ij));
            Lldd_solve(ij) = Lldd_solve0(pij_ind);
        }
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<totalnum;ijk++){
        if (KeepConstant[ijk]==0){
            int pjk_ind = ijk - sum(head(KeepConstant,ijk));
            if (isnan(Lldd_solve(ijk))){
                if (Lldd[pjk_ind*kept_covs+pjk_ind] != 0 ){
                    dbeta[ijk] = -lr * Lld[pjk_ind] / Lldd[pjk_ind*kept_covs+pjk_ind];
                } else {
                    dbeta[ijk] = 0;
                }
            } else {
                dbeta[ijk] = lr * Lldd_solve(ijk);//-lr * Lld[ijk] / Lldd[ijk*totalnum+ijk];
            }
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
    return;
}

//' Utility function to calculate the change to make each iteration
//'
//' \code{Calc_Change} Called to update the parameter changes, Uses log-likelihoods and control parameters, Applies newton steps and change limitations    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: parameter change matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_Change(const int& double_step, const int& nthreads, const int& totalnum, const int& der_iden, const double& dose_abs_max, const double& lr, const double& abs_max, const vector<double>& Ll, const vector<double>& Lld, const vector<double>& Lldd, vector<double>& dbeta, const bool change_all,const StringVector&   tform, const double& dint, const double& dslp, IntegerVector KeepConstant, bool debugging){
    if (double_step==1){
        int kept_covs = totalnum - sum(KeepConstant);
        NumericVector Lldd_vec(kept_covs * kept_covs);
        NumericVector Lld_vec(kept_covs);
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
            Lldd_vec[jk * kept_covs + ij]=Lldd[jk * kept_covs + ij];
            if (ij==jk){
                Lld_vec[ij]=Lld[ij];
            } else {
                Lldd_vec[ij * kept_covs + jk]=Lldd_vec[jk * kept_covs + ij];
            }
        }
        //
        Lldd_vec.attr("dim") = Dimension(kept_covs, kept_covs);
        const Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
        const Map<VectorXd> Lld_mat(as<Map<VectorXd> >(Lld_vec));
        VectorXd Lldd_solve0 = Lldd_mat.colPivHouseholderQr().solve(-1*Lld_mat);
        VectorXd Lldd_solve = VectorXd::Zero(totalnum);
        for (int ij=0;ij<totalnum;ij++){
            if (KeepConstant[ij]==0){
                int pij_ind = ij - sum(head(KeepConstant,ij));
                Lldd_solve(ij) = Lldd_solve0(pij_ind);
            }
        }
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<totalnum;ijk++){
            if (change_all){
                if (KeepConstant[ijk]==0){
                    int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                    if (isnan(Lldd_solve(ijk))){
                        if (Lldd[pjk_ind*kept_covs+pjk_ind] != 0 ){
                            dbeta[ijk] = -lr * Lld[pjk_ind] / Lldd[pjk_ind*kept_covs+pjk_ind];
                        } else {
                            dbeta[ijk] = 0;
                        }
                    } else {
                        dbeta[ijk] = lr * Lldd_solve(ijk);//-lr * Lld[ijk] / Lldd[ijk*totalnum+ijk];
                    }
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
            }else{
                if (ijk!=der_iden){//Validation requires controlled changes
                    dbeta[ijk] = 0.0;
                } else {
                    if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){
                        dbeta[ijk] = dint;
                    } else if ((tform[ijk]=="loglin")||(tform[ijk]=="lin")||(tform[ijk]=="plin")){
                        dbeta[ijk] = 0.001;
                    } else {
                        dbeta[ijk] = dslp;
                    }
                }
            }
        }
    } else {
        int kept_covs = totalnum - sum(KeepConstant);
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<totalnum;ijk++){
            if (change_all){
                if (KeepConstant[ijk]==0){
                    int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                    if (Lldd[pjk_ind*kept_covs+pjk_ind] != 0 ){
                        dbeta[ijk] = -lr * Lld[pjk_ind] / Lldd[pjk_ind*kept_covs+pjk_ind];
                    } else {
                        dbeta[ijk] = 0;
                    }
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
            }else{
                if (ijk!=der_iden){//Validation requires controlled changes
                    dbeta[ijk] = 0.0;
                } else {
                    if ((tform[ijk]=="lin_quad_int")||(tform[ijk]=="lin_exp_int")||(tform[ijk]=="step_int")||(tform[ijk]=="lin_int")){
                        dbeta[ijk] = dint;
                    } else if ((tform[ijk]=="loglin")||(tform[ijk]=="lin")||(tform[ijk]=="plin")){
                        dbeta[ijk] = abs_max;
                    } else {
                        dbeta[ijk] = dslp;
                    }
                }
            }
        }
    }
    return;
}

//' Utility function to calculate the change to make each iteration, with basic model
//'
//' \code{Calc_Change_Basic} Called to update the parameter changes, Uses log-likelihoods and control parameters, Applies newton steps and change limitations    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: parameter change matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_Change_Basic(const int& double_step, const int& nthreads, const int& totalnum, const int& der_iden, const double& lr, const double& abs_max, const vector<double>& Ll, const vector<double>& Lld, const vector<double>& Lldd, vector<double>& dbeta, const bool change_all, IntegerVector KeepConstant, bool debugging){
    if (double_step==1){
        //
        int kept_covs = totalnum - sum(KeepConstant);
        NumericVector Lldd_vec(kept_covs * kept_covs);
        NumericVector Lld_vec(kept_covs);
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
            Lldd_vec[jk * kept_covs + ij]=Lldd[jk * kept_covs + ij];
            if (ij==jk){
                Lld_vec[ij]=Lld[ij];
            } else {
                Lldd_vec[ij * kept_covs + jk]=Lldd_vec[jk * kept_covs + ij];
            }
        }
        Lldd_vec.attr("dim") = Dimension(kept_covs, kept_covs);
        const Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
        const Map<VectorXd> Lld_mat(as<Map<VectorXd> >(Lld_vec));
        VectorXd Lldd_solve0 = Lldd_mat.colPivHouseholderQr().solve(-1*Lld_mat);
        VectorXd Lldd_solve = VectorXd::Zero(totalnum);
        for (int ij=0;ij<totalnum;ij++){
            if (KeepConstant[ij]==0){
                int pij_ind = ij - sum(head(KeepConstant,ij));
                Lldd_solve(ij) = Lldd_solve0(pij_ind);
            }
        }
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<totalnum;ijk++){
            if (change_all){
                if (KeepConstant[ijk]==0){
                    //
                    int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                    if (isnan(Lldd_solve(ijk))){
                        if (Lldd[pjk_ind*kept_covs+pjk_ind] != 0 ){
                            dbeta[ijk] = -lr * Lld[pjk_ind] / Lldd[pjk_ind*kept_covs+pjk_ind];
                        } else {
                            dbeta[ijk] = 0;
                        }
                    } else {
                        dbeta[ijk] = lr * Lldd_solve(ijk);//-lr * Lld[ijk] / Lldd[ijk*totalnum+ijk];
                    }
                    //
                    //
                    if (abs(dbeta[ijk])>abs_max){
                        dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                    }
                } else {
                    dbeta[ijk]=0;
                }
            }else{
                if (ijk!=der_iden){//Validation requires controlled changes
                    dbeta[ijk] = 0.0;
                } else {
                    dbeta[ijk] = abs_max;
                }
            }
        }
    } else {
        int kept_covs = totalnum - sum(KeepConstant);
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<totalnum;ijk++){
            if (change_all){
                if (KeepConstant[ijk]==0){
                    int pjk_ind = ijk - sum(head(KeepConstant,ijk));
                    if (Lldd[pjk_ind*kept_covs+pjk_ind] != 0 ){
                        dbeta[ijk] = -lr * Lld[pjk_ind] / Lldd[pjk_ind*kept_covs+pjk_ind];
                    } else {
                        dbeta[ijk] = 0;
                    }
                    //
                    double dbeta_max;
                    if (Lld[ijk]!=0){
                        dbeta_max = abs(Ll[ijk]/Lld[ijk]);//uses newtonian step for zero log-likelihood as a limit
                    }else{
                        dbeta_max = 0;
                    }
                    if (abs(dbeta[ijk])>dbeta_max){
                        dbeta[ijk] = dbeta_max * sign(dbeta[ijk]);
                    }
                    if (abs(dbeta[ijk])>abs_max){
                        dbeta[ijk] = abs_max * sign(dbeta[ijk]);
                    }
                } else {
                    dbeta[ijk]=0;
                }
            }else{
                if (ijk!=der_iden){//Validation requires controlled changes
                    dbeta[ijk] = 0.0;
                } else {
                    dbeta[ijk] = abs_max;
                }
            }
        }
    }
    return;
}

//' Utility function to calculate steps for a likelihood based bound
//'
//' \code{Log_Bound} Called to perform likelihood bound steps
//' @inheritParams CPP_template
//' @param Lstar likelihood goal
//' @param L0 current likelihood
//'
//' @return Updates matrices in place: risk storage matrices
//' @noRd
//'
// [[Rcpp::export]]
void Log_Bound(double& deriv_max, const MatrixXd& Lldd_mat, const VectorXd& Lld_vec, const double& Lstar, const double& qchi, const double& L0, const int& para_number, const int& nthreads, const int& totalnum, const int& reqrdnum, IntegerVector KeepConstant, const int& term_tot, const int& step, vector<double>& dbeta, const VectorXd& beta_0, bool upper, bool& trouble, int verbose, double mult){
    // starts with solved likelihoods and derivatives
    // store the second derivative as D0
    MatrixXd D0 = Lldd_mat;
    deriv_max = 100;
    if (step==0){
        if (verbose>=4){
            Rcout << "C++ Note: df201 " << L0 << " " << Lstar << " " << endl;
            Rcout << "C++ Note: df204 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_0[ij] << " ";
            }
            Rcout << " " << endl;
        }
        //Initial step, calculate dom/dbet and h
        MatrixXd dOmdBeta = Lldd_mat.col(para_number).matrix();
        removeRow(D0, para_number);
        removeColumn(D0, para_number);
        removeRow(dOmdBeta, para_number);
        D0 = D0.inverse().matrix();
        dOmdBeta = -1 * D0 * dOmdBeta;
        //
        MatrixXd dLdBdO = Lldd_mat.row(para_number).matrix();
        removeColumn(dLdBdO, para_number);
        double h = Lldd_mat(para_number, para_number) - (dLdBdO.matrix() * D0 * dLdBdO.matrix().transpose().matrix())(0,0);
        h = mult * pow(qchi/(-1*h),0.5);
        if (upper){
            h = abs(h)/2;
        } else {
            h = abs(h)/-2;
        }
        // calculate first step
        int j=0;
        for (int ij=0;ij<totalnum;ij++){
            if (KeepConstant[ij]==0){
                int pij_ind = ij - sum(head(KeepConstant,ij));
                if (pij_ind == para_number){
                    dbeta[ij] = h;
                } else {
                    dbeta[ij] = h * dOmdBeta(j);
                    j=j+1;
                }
            }
        }
    } else {
        //Td0 = MatrixXd::Zero(df0.rows(), reqrdnum);
        MatrixXd G = MatrixXd::Zero(reqrdnum, reqrdnum);
        VectorXd v = VectorXd::Zero(reqrdnum);
        v[para_number] = L0 - Lstar;
        G.row(para_number) = Lld_vec;
        for (int j=0; j<reqrdnum;j++){
            if (j!=para_number){
                G.row(j) = D0.row(j);
                v[j] = Lld_vec[j];
            }
        }
        // At this point, we have the standard newton-raphson equation defined
        if (verbose>=4){
            Rcout << "C++ Note: df201 " << L0 << " " << Lstar << " " << endl;
            Rcout << "C++ Note: df202 ";//prints the first derivatives
            for (int ij=0;ij<reqrdnum;ij++){
                Rcout << v[ij] << " ";
            }
            Rcout << " " << endl;
            Rcout << "C++ Note: df203 ";//prints the second derivatives
            for (int ij=0;ij<reqrdnum;ij++){
                Rcout << G(ij, ij) << " ";
            }
            Rcout << " " << endl;
            Rcout << "C++ Note: df204 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << beta_0[ij] << " ";
            }
            Rcout << " " << endl;
            Rcout << "C++ Note: Second Derivative Determinant: " << G.determinant() << endl;
        }
        deriv_max = abs(v[0]);
        for (int ij=0;ij<reqrdnum;ij++){
            if (abs(v[ij])>deriv_max){
                deriv_max = abs(v[ij]);
            }
        }
        //
        if (abs(G.determinant()) < 1e-6){
            // The inverted matrix does not exist
            for (int ij=0;ij<totalnum;ij++){
                if (KeepConstant[ij]==0){
                    int pij_ind = ij - sum(head(KeepConstant,ij));
                    dbeta[ij] = -v[pij_ind]/G(pij_ind,pij_ind);
                }
            }
        } else {
            G = G.inverse().matrix();
            v = G.matrix() * v.matrix();
    //        Rcout << "reached G" << endl;
            VectorXd g1 = G.col(para_number);
            // we now must solve for the roots
            double as2 = g1.matrix().transpose() * D0 * g1.matrix();
            double bs1 = 2*v.matrix().transpose() *D0 * g1.matrix() - 2;
            double cs0 = v.matrix().transpose() * D0 * v.matrix();
//            Rcout << as2 << "s^2 + " << bs1 << "s + " << cs0 << endl;
            //
            if (pow(bs1,2)-4*as2*cs0 >= 0){
                double s0 = pow(bs1,2)-4*as2*cs0;
                double s1 = (-bs1 - pow(s0, 0.5))/(2*as2);
                s0 = (-bs1 + pow(s0, 0.5))/(2*as2);
                // check which is closer
                double s00 = (v + s0*g1).matrix().transpose() * D0 * (v + s0*g1).matrix();
                double s11 = (v + s1*g1).matrix().transpose() * D0 * (v + s1*g1).matrix();
                //
                if (abs(s00)<abs(s11)){
                    //s1 is further away
                    for (int ij=0;ij<totalnum;ij++){
                        if (KeepConstant[ij]==0){
                            int pij_ind = ij - sum(head(KeepConstant,ij));
                            dbeta[ij] = -v[pij_ind] - g1[pij_ind]*s0;
                        }
                    }
                } else {
                    //s0 is further away
                    for (int ij=0;ij<totalnum;ij++){
                        if (KeepConstant[ij]==0){
                            int pij_ind = ij - sum(head(KeepConstant,ij));
                            dbeta[ij] = -v[pij_ind] - g1[pij_ind]*s1;
                        }
                    }
                }
            } else {
                // there are no real solutions, needs a more conservative step?
                // currently will take a step to optimize with constant beta
                trouble = true;
                double s0 = -bs1/2/as2;
                for (int ij=0;ij<totalnum;ij++){
                    if (KeepConstant[ij]==0){
                        int pij_ind = ij - sum(head(KeepConstant,ij));
                        dbeta[ij] = -v[pij_ind] - g1[pij_ind]*s0;
                    }
                }
            }
        }
        if (verbose>=4){
            Rcout << "C++ Note: df205 ";//prints parameter values
            for (int ij=0;ij<totalnum;ij++){
                Rcout << dbeta[ij] << " ";
            }
            Rcout << " " << endl;
        }
    }
    return;
}

//' Utility function to calculate the change to make each iteration
//'
//' \code{Calc_Change_trouble} Called to update the parameter changes, Uses log-likelihoods and control parameters, Applies newton steps and change limitations    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: parameter change matrix
//' @noRd
//'
// [[Rcpp::export]]
void Calc_Change_trouble(const int& para_number, const int& nthreads, const int& totalnum, const double& dose_abs_max, const double& lr, const double& abs_max, const vector<double>& Ll, const vector<double>& Lld, const vector<double>& Lldd, vector<double>& dbeta,const StringVector&   tform, const double& dint, const double& dslp, IntegerVector KeepConstant_trouble, bool debugging){
    int kept_covs = totalnum - sum(KeepConstant_trouble);
    NumericVector Lldd_vec(kept_covs * kept_covs);
    NumericVector Lld_vec(kept_covs);
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
        int ij0 = ij;
        int jk0 = jk;
        if (ij >= para_number){
            ij0++;
        }
        if (jk >= para_number){
            jk0++;
        }
        Lldd_vec[jk * kept_covs + ij]=Lldd[jk0 * (kept_covs+1) + ij0];
        if (ij==jk){
            Lld_vec[ij]=Lld[ij0];
        } else {
            Lldd_vec[ij * kept_covs + jk]=Lldd_vec[jk0 * (kept_covs+1) + ij0];
        }
    }
    //
    //
    Lldd_vec.attr("dim") = Dimension(kept_covs, kept_covs);
    const Map<MatrixXd> Lldd_mat(as<Map<MatrixXd> >(Lldd_vec));
    const Map<VectorXd> Lld_mat(as<Map<VectorXd> >(Lld_vec));
    VectorXd Lldd_solve0 = Lldd_mat.colPivHouseholderQr().solve(-1*Lld_mat);
    VectorXd Lldd_solve = VectorXd::Zero(totalnum);
    for (int ij=0;ij<totalnum;ij++){
        if (KeepConstant_trouble[ij]==0){
            int pij_ind = ij - sum(head(KeepConstant_trouble,ij));
            Lldd_solve(ij) = Lldd_solve0(pij_ind);
        }
    }
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<totalnum;ijk++){
        if (KeepConstant_trouble[ijk]==0){
            int pjk_ind = ijk - sum(head(KeepConstant_trouble,ijk));
            if (isnan(Lldd_solve(ijk))){
                if (Lldd[pjk_ind*kept_covs+pjk_ind] != 0 ){
                    dbeta[ijk] = -lr * Lld[pjk_ind] / Lldd[pjk_ind*kept_covs+pjk_ind];
                } else {
                    dbeta[ijk] = 0;
                }
            } else {
                dbeta[ijk] = lr * Lldd_solve(ijk);//-lr * Lld[ijk] / Lldd[ijk*totalnum+ijk];
            }
//            //
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
    return;
}
