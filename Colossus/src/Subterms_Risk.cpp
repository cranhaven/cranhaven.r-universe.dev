#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif
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

//' Utility function to calculate the term and subterm values
//'
//' \code{Make_subterms} Called to update term matrices, Uses lists of term numbers and types to apply formulas
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: subterm matrices, Term matrices
//' @noRd
// [[Rcpp::export]]
void Make_subterms(const int& totalnum, const IntegerVector& term_n,const StringVector&  tform, const IntegerVector& dfc, const int& fir, MatrixXd& T0, MatrixXd& Td0, MatrixXd& Tdd0, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm, MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN,const  VectorXd& beta_0,const  MatrixXd& df0,const double& dint, const double& dslp, const int& nthreads, bool debugging, const IntegerVector& KeepConstant){
    //
    // Calculates the sub term values
    //
    // reset the subterm counts
    Dose = MatrixXd::Constant(T0.rows(),Dose.cols(),0.0); //Matrix of the total dose term values
	nonDose_LIN = MatrixXd::Constant(T0.rows(),Dose.cols(),0.0); //matrix of Linear subterm values
	nonDose_PLIN = MatrixXd::Constant(T0.rows(),Dose.cols(),1.0); //matrix of Loglinear subterm values
	nonDose_LOGLIN = MatrixXd::Constant(T0.rows(),Dose.cols(),1.0); //matrix of Product linear subterm values
    //
    vector<int> lin_count(nonDose.cols(),0);
    vector<int> dose_count(nonDose.cols(),0);
    #ifdef _OPENMP
    #pragma omp declare reduction (eig_plus: MatrixXd: omp_out=omp_out.array() + omp_in.array()) initializer(omp_priv=MatrixXd::Constant(omp_orig.rows(),omp_orig.cols(),0.0))
    #pragma omp declare reduction (eig_mult: MatrixXd: omp_out=omp_out.array() * omp_in.array()) initializer(omp_priv=MatrixXd::Constant(omp_orig.rows(),omp_orig.cols(),1.0))
    #pragma omp declare reduction(vec_int_plus : std::vector<int> : \
            std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<int>())) \
            initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(eig_plus:Dose,nonDose_LIN,nonDose_PLIN) reduction(eig_mult:nonDose_LOGLIN) reduction(vec_int_plus:lin_count,dose_count)
    #endif
    for (int ij=0;ij<(totalnum);ij++){
        int df0_c = dfc[ij]-1;
        int tn = term_n[ij];
        if (as< string>(tform[ij])=="loglin") {
            T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]).matrix();
            T0.col(ij) = T0.col(ij).array().exp();;
            nonDose_LOGLIN.col(tn) = nonDose_LOGLIN.col(tn).array() * T0.col(ij).array();

        } else if (as< string>(tform[ij])=="lin") {
            T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]).matrix();
            nonDose_LIN.col(tn) = nonDose_LIN.col(tn).array() + T0.col(ij).array();
            lin_count[tn]=lin_count[tn]+1;

        } else if (as< string>(tform[ij])=="plin") {
            T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]).matrix();
            nonDose_PLIN.col(tn) = nonDose_PLIN.col(tn).array() + T0.col(ij).array();

        } else if (as< string>(tform[ij])=="loglin_slope"){
            //
            T0.col(ij) = beta_0[ij] * (beta_0[ij+1] * df0.col(df0_c)).array().exp();
            T0.col(ij+1) = T0.col(ij);
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
            
        } else if (as< string>(tform[ij])=="loglin_top"){
            if (ij==0){
                T0.col(ij) = (beta_0[ij] * df0.col(df0_c)).array().exp();
                Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
                dose_count[tn]=dose_count[tn]+1;

            } else if (tform[ij-1]!="loglin_slope"){
                T0.col(ij) = (beta_0[ij] * df0.col(df0_c)).array().exp();
                Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
                dose_count[tn]=dose_count[tn]+1;
                //

            } else {
                ;
            }
        } else if (as< string>(tform[ij])=="lin_slope"){
            T0.col(ij) = (df0.col(df0_c).array() - beta_0[ij+1]);
            //
            T0.col(ij) = (T0.col(ij).array() < 0).select(0, T0.col(ij));
            //
            T0.col(ij) = beta_0[ij] * T0.col(ij);
            T0.col(ij+1) = T0.col(ij);
            //
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;

        } else if (as< string>(tform[ij])=="lin_int") {
            ;
        } else if (as< string>(tform[ij])=="quad_slope"){
            //
            T0.col(ij) = beta_0[ij] * df0.col(df0_c).array().square();
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
        } else if (as< string>(tform[ij])=="step_slope"){
            T0.col(ij) = (df0.col(df0_c).array() - beta_0[ij+1]);
            //
            T0.col(ij) = (T0.col(ij).array() < 0).select(0.0, MatrixXd::Zero(T0.rows(),1).array()+1.0);
            //
            T0.col(ij) = beta_0[ij] * T0.col(ij);
            T0.col(ij+1) = T0.col(ij);
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
        } else if (as< string>(tform[ij])=="step_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_quad_slope") {
            ArrayXd temp = (df0.col(df0_c).array() - beta_0[ij+1]);
            T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]);
            T0.col(ij+1) = (df0.col(df0_c).array().pow(2).array() * beta_0[ij] /2 / beta_0[ij+1] + beta_0[ij] /2 * beta_0[ij+1]);
            //
            temp = (temp.array() < 0).select(T0.col(ij), T0.col(ij+1));
            //
            T0.col(ij) = temp.array();
            T0.col(ij+1) = T0.col(ij).array();
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
        } else if (as< string>(tform[ij])=="lin_quad_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_exp_slope") {
            T0.col(ij) = (df0.col(df0_c).array() - beta_0[ij+1]);
            double c1;
            double a1;
            if (beta_0[ij]<0){
                c1 = log(-1*beta_0[ij]/beta_0[ij+2]) + beta_0[ij+1] * beta_0[ij+2];
                a1 = -1*beta_0[ij] * beta_0[ij+1] + exp(c1 - beta_0[ij+2] * beta_0[ij+1]);
                T0.col(ij+2) = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
            } else {
                c1 = log(beta_0[ij]/beta_0[ij+2]) + beta_0[ij+1] * beta_0[ij+2];
                a1 = beta_0[ij] * beta_0[ij+1] + exp(c1 - beta_0[ij+2] * beta_0[ij+1]);
                T0.col(ij+2) = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
            }
            //
            T0.col(ij+1) = (df0.col(df0_c).array() * beta_0[ij]);
            T0.col(ij) = (T0.col(ij).array() < 0).select(T0.col(ij+1), T0.col(ij+2));
            //
            T0.col(ij+1) = T0.col(ij);
            T0.col(ij+2) = T0.col(ij);
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
        } else if (as< string>(tform[ij])=="lin_exp_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_exp_exp_slope") {
            ;
        } else {
            throw invalid_argument( "incorrect subterm type" );
        }
    }
    //
    // Calculates the terms and derivatives
    //
    for (int ijk=0; ijk<nonDose.cols();ijk++){ //combines non-dose terms into a single term
        if (dose_count[ijk]==0){
            Dose.col(ijk) = Dose.col(ijk).array() * 0.0 + 1;
        }
        if (lin_count[ijk]==0){
            nonDose_LIN.col(ijk) = nonDose_LIN.col(ijk).array() * 0.0 + 1;//replaces missing data with 1
        }
        nonDose.col(ijk) = nonDose_LIN.col(ijk).array()  * nonDose_PLIN.col(ijk).array()  * nonDose_LOGLIN.col(ijk).array() ;
    }
    TTerm << Dose.array() * nonDose.array();
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ij=0;ij<(totalnum);ij++){
        int df0_c = dfc[ij]-1;
        int tn = term_n[ij];
        if (KeepConstant[ij]==0){
            int jk = ij - sum(head(KeepConstant,ij));
            if (as< string>(tform[ij])=="loglin") {
                T0.col(ij) = nonDose_LOGLIN.col(tn);
                Td0.col(jk) = df0.col(df0_c).array() * T0.col(ij).array();
                Tdd0.col((jk)*(jk+1)/2+jk) = df0.col(df0_c).array() * Td0.col(jk).array();
            } else if (as< string>(tform[ij])=="plin") {
                T0.col(ij) = nonDose_PLIN.col(tn);
                Td0.col(jk) = df0.col(df0_c);
            } else if (as< string>(tform[ij])=="loglin_slope"){
                T0.col(ij) = (beta_0[ij+1] * df0.col(df0_c)).array().exp();
                //
                Td0.col(jk) = T0.col(ij).array();
                Td0.col(jk+1) = beta_0[ij] * T0.col(ij).array() * df0.col(df0_c).array();
                Tdd0.col((jk+1)*(jk+2)/2+jk) = T0.col(ij).array() * df0.col(df0_c).array();
                Tdd0.col((jk+1)*(jk+2)/2+jk+1) =beta_0[ij] * T0.col(ij).array() * df0.col(df0_c).array().square().array();
                //
                T0.col(ij)   = Dose.col(tn);
                T0.col(ij+1) = Dose.col(tn);
                
            } else if (as< string>(tform[ij])=="loglin_top"){
                if (ij==0){
                    T0.col(ij) = (beta_0[ij] * df0.col(df0_c)).array().exp();
                    Td0.col(jk) = T0.col(ij).array() * df0.col(df0_c).array();
                    Tdd0.col(jk * (jk+1)/2 + jk) = T0.col(ij).array() * df0.col(df0_c).array().square().array();
                    //
                    T0.col(ij) = Dose.col(tn);

                } else if (tform[ij-1]!="loglin_slope"){
                    T0.col(ij) = (beta_0[ij] * df0.col(df0_c)).array().exp();
                    Td0.col(jk) = T0.col(ij).array() * df0.col(df0_c).array();
                    Tdd0.col(jk * (jk+1)/2 + jk) = T0.col(ij).array() * df0.col(df0_c).array().square().array();
                    //
                    //
                    T0.col(ij) = Dose.col(tn);
                } else if ((tform[ij-1]=="loglin_slope")&&(KeepConstant[ij-1]==1)){
                    T0.col(ij) = beta_0[ij-1] * (beta_0[ij] * df0.col(df0_c)).array().exp();
                    Td0.col(jk) = T0.col(ij).array() * df0.col(df0_c).array();
                    Tdd0.col(jk * (jk+1)/2 + jk) = T0.col(ij).array() * df0.col(df0_c).array().square().array();
                    //
                    //
                    T0.col(ij) = Dose.col(tn);
                    T0.col(ij-1) = Dose.col(tn);
                }
            } else if (as< string>(tform[ij])=="lin_slope"){
                if (KeepConstant[ij+1]==0){
                    Td0.col(jk)  = (df0.col(df0_c).array() - beta_0[ij+1]);
                    T0.col(ij)   = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                    T0.col(ij+1) = (df0.col(df0_c).array() - beta_0[ij+1]-dint);
                    //
                    Td0.col(jk)  = (Td0.col(jk).array()  < 0).select(0, Td0.col(jk));
                    T0.col(ij)   = (T0.col(ij).array()   < 0).select(0, T0.col(ij));
                    T0.col(ij+1) = (T0.col(ij+1).array() < 0).select(0, T0.col(ij+1));
                    //
                    Td0.col(jk+1) = beta_0[ij] * (T0.col(ij+1).array()-T0.col(ij).array())/2.0/dint;
                    //
                    Tdd0.col((jk+1)*(jk+2)/2+jk)   = (T0.col(ij+1).array()-T0.col(ij).array())/2.0/dint;
                    Tdd0.col((jk+1)*(jk+2)/2+jk+1) = beta_0[ij] * (T0.col(ij+1).array()-2.0*Td0.col(jk).array()+T0.col(ij).array()) / pow(dint,2);
                    //
                    T0.col(ij)   = Dose.col(tn);
                    T0.col(ij+1) = Dose.col(tn);
                } else { // Special case with a fixed intercept, but not a fixed slope
                    Td0.col(jk)  = (df0.col(df0_c).array() - beta_0[ij+1]);
                    Td0.col(jk)  = (Td0.col(jk).array()  < 0).select(0, Td0.col(jk));
                    //
                    T0.col(ij)   = Dose.col(tn);
                    T0.col(ij+1) = Dose.col(tn);
                }
            } else if (as< string>(tform[ij])=="quad_slope"){
                Td0.col(jk) = df0.col(df0_c).array().square();
                //
                T0.col(ij) = Dose.col(tn);
            } else if (as< string>(tform[ij])=="step_slope"){
                if (KeepConstant[ij+1]==0){
                    Td0.col(jk) = (df0.col(df0_c).array() - beta_0[ij+1]);
                    T0.col(ij) = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                    T0.col(ij+1) = (df0.col(df0_c).array() - beta_0[ij+1]-dint);
                    //
                    Td0.col(jk)  = (Td0.col(jk).array() < 0).select(0.0, MatrixXd::Zero(Td0.rows(),1).array()+1.0);
                    T0.col(ij)   = (T0.col(ij).array() < 0).select(0.0, MatrixXd::Zero(Td0.rows(),1).array()+1.0);
                    T0.col(ij+1) = (T0.col(ij+1).array() < 0).select(0.0, MatrixXd::Zero(Td0.rows(),1).array()+1.0);
                    //
                    Td0.col(jk+1) = beta_0[ij] * (T0.col(ij+1).array()-T0.col(ij).array()) / 2.0/dint;
                    //
                    Tdd0.col((jk+1)*(jk+2)/2+jk) = (T0.col(ij+1).array()-T0.col(ij).array()) / 2.0/dint;
                    Tdd0.col((jk+1)*(jk+2)/2+jk+1) = beta_0[ij] * (T0.col(ij+1).array()-2.0*Td0.col(jk).array()+T0.col(ij).array()) / pow(dint,2);
                    //
                    T0.col(ij) = Dose.col(tn);
                    T0.col(ij+1) = Dose.col(tn);
                } else { // Special case with a fixed intercept, but not a fixed slope
                    Td0.col(jk)  = (df0.col(df0_c).array() - beta_0[ij+1]);
                    Td0.col(jk)  = (Td0.col(jk).array() < 0).select(0.0, MatrixXd::Zero(Td0.rows(),1).array()+1.0);
                    //
                    T0.col(ij)   = Dose.col(tn);
                    T0.col(ij+1) = Dose.col(tn);
                }

            } else if (as< string>(tform[ij])=="lin_quad_slope") {
                ArrayXd temp = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                ArrayXd temp0 = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                ArrayXd temp1 = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                double a1 = 0;
                double b1 = 0;
                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                a1 = (beta_0[ij] - dslp) /2.0 / (beta_0[ij+1]-dint);
                b1 = (beta_0[ij] - dslp) /2.0 * (beta_0[ij+1]-dint);
                temp0 = (df0.col(df0_c).array() * (beta_0[ij] - dslp));
                temp1 = (df0.col(df0_c).array().pow(2).array() * a1 + b1);
                //
                T0.col(ij) = (temp.array() < 0).select(temp0, temp1);
                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]-dint);
                a1 = (beta_0[ij]+dslp) /2.0 / (beta_0[ij+1]+dint);
                b1 = (beta_0[ij]+dslp) /2.0 * (beta_0[ij+1]+dint);
                temp0 = (df0.col(df0_c).array() * (beta_0[ij] + dslp));
                temp1 = (df0.col(df0_c).array().pow(2).array() * a1 + b1);
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);
                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]);
                a1 = (beta_0[ij]) /2.0 / (beta_0[ij+1]);
                b1 = (beta_0[ij]) /2.0 * (beta_0[ij+1]);
                temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                temp1 = (df0.col(df0_c).array().pow(2).array() * a1 + b1);
                //
                ArrayXd temp11 = (temp.array() < 0).select(temp0, temp1);
                //

                Tdd0.col((jk+1)*(jk+2)/2+jk+0) = (T0.col(ij+1).array()-2.0*temp11.array()+T0.col(ij).array()) / (pow(dint,2)+pow(dslp,2));


                //
                a1 = (beta_0[ij]+dslp) /2 / (beta_0[ij+1]);
                b1 = (beta_0[ij]+dslp) /2 * (beta_0[ij+1]);
                temp0 = (df0.col(df0_c).array() * (beta_0[ij]+dslp));
                temp1 = (df0.col(df0_c).array().pow(2).array() * a1 + b1);
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);

                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]);
                a1 = (beta_0[ij] - dslp) /2 / (beta_0[ij+1]);
                b1 = (beta_0[ij] - dslp) /2 * (beta_0[ij+1]);
                temp0 = (df0.col(df0_c).array() * (beta_0[ij] - dslp));
                temp1 = (df0.col(df0_c).array().pow(2).array() * a1 + b1);
                //
                T0.col(ij) = (temp.array() < 0).select(temp0, temp1);



                Td0.col(jk)   = (T0.col(ij+1).array()-T0.col(ij).array()) / 2/dslp;
                Tdd0.col((jk+0)*(jk+1)/2+jk+0) = (T0.col(ij+1).array()-2*temp11.array()+T0.col(ij).array()) / pow(dslp,2);

                temp = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                a1 = beta_0[ij] /2 / (beta_0[ij+1]-dint);
                b1 = beta_0[ij] /2 * (beta_0[ij+1]-dint);
                temp0 = (df0.col(df0_c).array() * beta_0[ij]);
                temp1 = (df0.col(df0_c).array().pow(2).array() * a1 + b1);
                //
                T0.col(ij) = (temp.array() < 0).select(temp0, temp1);

                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]-dint);
                //
                a1 = (beta_0[ij]) /2 / (beta_0[ij+1]+dint);
                b1 = (beta_0[ij]) /2 * (beta_0[ij+1]+dint);
                temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                temp1 = (df0.col(df0_c).array().pow(2).array() * a1 + b1);
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);
                //


                Td0.col(jk+1) = (T0.col(ij+1).array()-T0.col(ij).array()) / 2/dint;
                Tdd0.col((jk+1)*(jk+2)/2+jk+1) = (T0.col(ij+1).array()-2*temp11.array()+T0.col(ij).array()) / pow(dint,2);
                //
                T0.col(ij) = Dose.col(tn);
                T0.col(ij+1) = Dose.col(tn);
                //
            } else if (as< string>(tform[ij])=="lin_exp_slope") {
                // the exp_exp_slope term must be greater than zero
                double eeslp = dslp;
                if (eeslp >= beta_0[ij+2]){
                    eeslp = beta_0[ij+2]*0.9;
                }
                double c1;
                double a1;
                //
                ArrayXd temp = (df0.col(df0_c).array() - beta_0[ij+1]);
                if (beta_0[ij]<0){
                    c1 = log((-1*beta_0[ij])/(beta_0[ij+2])) + (beta_0[ij+1]) * (beta_0[ij+2]);
                    a1 = -1*(beta_0[ij]) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]));
                } else {
                    c1 = log((beta_0[ij])/(beta_0[ij+2])) + (beta_0[ij+1]) * (beta_0[ij+2]);
                    a1 = (beta_0[ij]) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]));
                }
                ArrayXd temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                ArrayXd temp1 = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                if (beta_0[ij]<0){
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij) = (temp.array() < 0).select(temp0, temp1);
                //
                if (beta_0[ij]+dslp<0){
                    c1 = log(-1*(beta_0[ij]+dslp)/(beta_0[ij+2])) + (beta_0[ij+1]) * (beta_0[ij+2]);
                    a1 = -1*(beta_0[ij]+dslp) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]+dslp));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij]+dslp)/(beta_0[ij+2])) + (beta_0[ij+1]) * (beta_0[ij+2]);
                    a1 = (beta_0[ij]+dslp) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]+dslp));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);
                //
                if (beta_0[ij]-dslp<0){
                    c1 = log(-1*(beta_0[ij]-dslp)/(beta_0[ij+2])) + (beta_0[ij+1]) * (beta_0[ij+2]);
                    a1 = -1*(beta_0[ij]-dslp) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]-dslp));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij]-dslp)/(beta_0[ij+2])) + (beta_0[ij+1]) * (beta_0[ij+2]);
                    a1 = (beta_0[ij]-dslp) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]-dslp));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+2) = (temp.array() < 0).select(temp0, temp1);
                //
                Td0.col(jk)   = (T0.col(ij+1).array()-T0.col(ij+2).array()) / 2/dslp;
                Tdd0.col((jk+0)*(jk+1)/2+jk+0) = (T0.col(ij+1).array()-2*T0.col(ij).array()+T0.col(ij+2).array()) / pow(dslp,2);
                //
                if (beta_0[ij]<0){
                    c1 = log(-1*(beta_0[ij])/(beta_0[ij+2]-eeslp)) + (beta_0[ij+1]) * (beta_0[ij+2]-eeslp);
                    a1 = -1*(beta_0[ij]) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]-eeslp) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]-eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij])/(beta_0[ij+2]-eeslp)) + (beta_0[ij+1]) * (beta_0[ij+2]-eeslp);
                    a1 = (beta_0[ij]) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]-eeslp) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]-eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);
                //
                if (beta_0[ij]<0){
                    c1 = log(-1*(beta_0[ij])/(beta_0[ij+2]+eeslp)) + (beta_0[ij+1]) * (beta_0[ij+2]+eeslp);
                    a1 = -1*(beta_0[ij]) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]+eeslp) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]+eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij])/(beta_0[ij+2]+eeslp)) + (beta_0[ij+1]) * (beta_0[ij+2]+eeslp);
                    a1 = (beta_0[ij]) * (beta_0[ij+1]) + exp(c1 - (beta_0[ij+2]+eeslp) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]+eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+2) = (temp.array() < 0).select(temp0, temp1);
                //
                Td0.col(jk+2) = (T0.col(ij+2).array()-T0.col(ij+1).array()) / 2/eeslp;
                Tdd0.col((jk+2)*(jk+3)/2+jk+2) = (T0.col(ij+2).array()-2*T0.col(ij).array()+T0.col(ij+1).array()) / pow(eeslp,2);
                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                if (beta_0[ij]<0){
                    c1 = log(-1*(beta_0[ij])/(beta_0[ij+2])) + (beta_0[ij+1]-dint) * (beta_0[ij+2]);
                    a1 = -1*(beta_0[ij]) * (beta_0[ij+1]-dint) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]-dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij])/(beta_0[ij+2])) + (beta_0[ij+1]-dint) * (beta_0[ij+2]);
                    a1 = (beta_0[ij]) * (beta_0[ij+1]-dint) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]-dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);
                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]-dint);
                if (beta_0[ij]<0){
                    c1 = log(-1*(beta_0[ij])/(beta_0[ij+2])) + (beta_0[ij+1]+dint) * (beta_0[ij+2]);
                    a1 = -1*(beta_0[ij]) * (beta_0[ij+1]+dint) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]+dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij])/(beta_0[ij+2])) + (beta_0[ij+1]+dint) * (beta_0[ij+2]);
                    a1 = (beta_0[ij]) * (beta_0[ij+1]+dint) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]+dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+2) = (temp.array() < 0).select(temp0, temp1);
                //
                Td0.col(jk+1) = (T0.col(ij+2).array()-T0.col(ij+1).array()) / 2/dint;
                Tdd0.col((jk+1)*(jk+2)/2+jk+1) = (T0.col(ij+2).array()-2*T0.col(ij).array()+T0.col(ij+1).array()) / pow(dint,2);
                //
                if (beta_0[ij]+dslp<0){
                    c1 = log(-1*(beta_0[ij]+dslp)/(beta_0[ij+2])) + (beta_0[ij+1]+dint) * (beta_0[ij+2]);
                    a1 = -1*(beta_0[ij]+dslp) * (beta_0[ij+1]+dint) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]+dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]+dslp));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij]+dslp)/(beta_0[ij+2])) + (beta_0[ij+1]+dint) * (beta_0[ij+2]);
                    a1 = (beta_0[ij]+dslp) * (beta_0[ij+1]+dint) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]+dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]+dslp));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+2) = (temp.array() < 0).select(temp0, temp1);
                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                //
                if (beta_0[ij]-dslp<0){
                    c1 = log(-1*(beta_0[ij]-dslp)/(beta_0[ij+2])) + (beta_0[ij+1]-dint) * (beta_0[ij+2]);
                    a1 = -1*(beta_0[ij]-dslp) * (beta_0[ij+1]-dint) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]-dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]-dslp));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij]-dslp)/(beta_0[ij+2])) + (beta_0[ij+1]-dint) * (beta_0[ij+2]);
                    a1 = (beta_0[ij]-dslp) * (beta_0[ij+1]-dint) + exp(c1 - (beta_0[ij+2]) * (beta_0[ij+1]-dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]-dslp));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);
                Tdd0.col((jk+1)*(jk+2)/2+jk+0) = (T0.col(ij+2).array()-2*T0.col(ij).array()+T0.col(ij+1).array()) / (pow(dint,2)+pow(dslp,2));
                //
                if (beta_0[ij]-dslp<0){
                    c1 = log(-1*(beta_0[ij]-dslp)/(beta_0[ij+2]-eeslp)) + (beta_0[ij+1]) * (beta_0[ij+2]-eeslp);
                    a1 = -1*(beta_0[ij]-dslp) * (beta_0[ij+1]-dslp) + exp(c1 - (beta_0[ij+2]-eeslp) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]-dslp));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]-eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij]-dslp)/(beta_0[ij+2]-eeslp)) + (beta_0[ij+1]) * (beta_0[ij+2]-eeslp);
                    a1 = (beta_0[ij]-dslp) * (beta_0[ij+1]-dslp) + exp(c1 - (beta_0[ij+2]-eeslp) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]-dslp));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]-eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);
                //
                if (beta_0[ij]+dslp<0){
                    c1 = log(-1*(beta_0[ij]+dslp)/(beta_0[ij+2]+eeslp)) + (beta_0[ij+1]) * (beta_0[ij+2]+eeslp);
                    a1 = -1*(beta_0[ij]+dslp) * (beta_0[ij+1]+eeslp) + exp(c1 - (beta_0[ij+2]+eeslp) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]+dslp));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]+eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij]+dslp)/(beta_0[ij+2]+eeslp)) + (beta_0[ij+1]) * (beta_0[ij+2]+eeslp);
                    a1 = (beta_0[ij]+dslp) * (beta_0[ij+1]+eeslp) + exp(c1 - (beta_0[ij+2]+eeslp) * (beta_0[ij+1]));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]+dslp));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]+eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+2) = (temp.array() < 0).select(temp0, temp1);
                //
                Tdd0.col((jk+2)*(jk+3)/2+jk+0) = (T0.col(ij+2).array()-2*T0.col(ij).array()+T0.col(ij+1).array()) / (pow(dslp,2)+pow(eeslp,2));
                temp = (df0.col(df0_c).array() - beta_0[ij+1]+dint);
                //
                if (beta_0[ij]<0){
                    c1 = log(-1*(beta_0[ij])/(beta_0[ij+2]-eeslp)) + (beta_0[ij+1]-dint) * (beta_0[ij+2]-eeslp);
                    a1 = -1*(beta_0[ij]) * (beta_0[ij+1]-dint) + exp(c1 - (beta_0[ij+2]-eeslp) * (beta_0[ij+1]-dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]-eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij])/(beta_0[ij+2]-eeslp)) + (beta_0[ij+1]-dint) * (beta_0[ij+2]-eeslp);
                    a1 = (beta_0[ij]) * (beta_0[ij+1]-dint) + exp(c1 - (beta_0[ij+2]-eeslp) * (beta_0[ij+1]-dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]-eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+1) = (temp.array() < 0).select(temp0, temp1);
                //
                temp = (df0.col(df0_c).array() - beta_0[ij+1]-dint);
                if (beta_0[ij]<0){
                    c1 = log(-1*(beta_0[ij])/(beta_0[ij+2]+eeslp)) + (beta_0[ij+1]+dint) * (beta_0[ij+2]+eeslp);
                    a1 = -1*(beta_0[ij]) * (beta_0[ij+1]+dint) + exp(c1 - (beta_0[ij+2]+eeslp) * (beta_0[ij+1]+dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = -1*(a1 - (c1 - (beta_0[ij+2]+eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                } else {
                    c1 = log((beta_0[ij])/(beta_0[ij+2]+eeslp)) + (beta_0[ij+1]+dint) * (beta_0[ij+2]+eeslp);
                    a1 = (beta_0[ij]) * (beta_0[ij+1]+dint) + exp(c1 - (beta_0[ij+2]+eeslp) * (beta_0[ij+1]+dint));
                    temp0 = (df0.col(df0_c).array() * (beta_0[ij]));
                    temp1 = (a1 - (c1 - (beta_0[ij+2]+eeslp) * df0.col(df0_c).array()).array().exp().array()).array();
                }
                //
                T0.col(ij+2) = (temp.array() < 0).select(temp0, temp1);
                //
                Tdd0.col((jk+2)*(jk+3)/2+jk+1) = (T0.col(ij+2).array()-2*T0.col(ij).array()+T0.col(ij+1).array()) / (pow(dint,2)+pow(eeslp,2));
                //
                T0.col(ij)   = Dose.col(tn);
                T0.col(ij+1) = Dose.col(tn);
                T0.col(ij+2) = Dose.col(tn);
            } else if (as< string>(tform[ij])=="lin") {
                T0.col(ij) = nonDose_LIN.col(tn);
                Td0.col(jk) = df0.col(df0_c);
            } else {
                ;
            }
        }
    }
    //
    // Adds in possible log-linear subterm second derivatives between DIFFERENT covariates
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<totalnum*(totalnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        int tij = term_n[ij];
        int tjk = term_n[jk];
        int df0_ij = dfc[ij]-1;
        int df0_jk = dfc[jk]-1;
        //
        if (KeepConstant[ij]+KeepConstant[jk]==0){
            int ij_ind = ij - sum(head(KeepConstant,ij));
            int jk_ind = jk - sum(head(KeepConstant,jk));
            if (tij==tjk){
                if (as< string>(tform[ij])=="loglin") {
                    if (ij==jk){
                        Tdd0.col((ij_ind)*(ij_ind+1)/2+ij_ind) = df0.col(df0_ij).array().pow(2).array() * nonDose_LOGLIN.col(tij).array();
                    } else if (as< string>(tform[jk])=="loglin") {
                        Tdd0.col((ij_ind)*(ij_ind+1)/2+jk_ind) = df0.col(df0_ij).array() * df0.col(df0_jk).array() * nonDose_LOGLIN.col(tij).array();
                    }
                }
            }
        }
    }
    return;
}

//' Utility function to calculate the term and subterm values, but not derivatives
//'
//' \code{Make_subterms_Single} Called to update term matrices, Uses lists of term numbers and types to apply formulas
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: subterm matrices, Term matrices
//' @noRd
// [[Rcpp::export]]
void Make_subterms_Single(const int& totalnum, const IntegerVector& term_n,const StringVector&  tform, const IntegerVector& dfc, const int& fir, MatrixXd& T0, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm, MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN,const  VectorXd& beta_0,const  MatrixXd& df0, const int& nthreads, bool debugging, const IntegerVector& KeepConstant){
    //
    // Calculates the sub term values
    //
    // reset the subterm counts
    Dose = MatrixXd::Constant(T0.rows(),Dose.cols(),0.0); //Matrix of the total dose term values
	nonDose_LIN = MatrixXd::Constant(T0.rows(),Dose.cols(),0.0); //matrix of Linear subterm values
	nonDose_PLIN = MatrixXd::Constant(T0.rows(),Dose.cols(),1.0); //matrix of Loglinear subterm values
	nonDose_LOGLIN = MatrixXd::Constant(T0.rows(),Dose.cols(),1.0); //matrix of Product linear subterm values
    //
    vector<int> lin_count(nonDose.cols(),0);
    vector<int> dose_count(nonDose.cols(),0);
    #ifdef _OPENMP
    #pragma omp declare reduction (eig_plus: MatrixXd: omp_out=omp_out.array() + omp_in.array()) initializer(omp_priv=MatrixXd::Constant(omp_orig.rows(),omp_orig.cols(),0.0))
    #pragma omp declare reduction (eig_mult: MatrixXd: omp_out=omp_out.array() * omp_in.array()) initializer(omp_priv=MatrixXd::Constant(omp_orig.rows(),omp_orig.cols(),1.0))
    #pragma omp declare reduction(vec_int_plus : std::vector<int> : \
            std::transform(omp_out.begin(), omp_out.end(), omp_in.begin(), omp_out.begin(), std::plus<int>())) \
            initializer(omp_priv = omp_orig)
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads) reduction(eig_plus:Dose,nonDose_LIN,nonDose_PLIN) reduction(eig_mult:nonDose_LOGLIN) reduction(vec_int_plus:lin_count,dose_count)
    #endif
    for (int ij=0;ij<(totalnum);ij++){
        int df0_c = dfc[ij]-1;
        int tn = term_n[ij];
        if (as< string>(tform[ij])=="loglin") {
            T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]).matrix();
            T0.col(ij) = T0.col(ij).array().exp();;
            nonDose_LOGLIN.col(tn) = nonDose_LOGLIN.col(tn).array() * T0.col(ij).array();

        } else if (as< string>(tform[ij])=="lin") {
            T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]).matrix();
            nonDose_LIN.col(tn) = nonDose_LIN.col(tn).array() + T0.col(ij).array();
            lin_count[tn]=lin_count[tn]+1;

        } else if (as< string>(tform[ij])=="plin") {
            T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]).matrix();
            nonDose_PLIN.col(tn) = nonDose_PLIN.col(tn).array() + T0.col(ij).array();

        } else if (as< string>(tform[ij])=="loglin_slope"){
            T0.col(ij) =beta_0[ij] * (beta_0[ij+1] * df0.col(df0_c)).array().exp();
            //
            //
            T0.col(ij+1) = T0.col(ij);
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
            
        } else if (as< string>(tform[ij])=="loglin_top"){
            if (ij==0){
                T0.col(ij) = (beta_0[ij] * df0.col(df0_c)).array().exp();
                Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
                dose_count[tn]=dose_count[tn]+1;

            } else if (tform[ij-1]!="loglin_slope"){
                T0.col(ij) = (beta_0[ij] * df0.col(df0_c)).array().exp();
                Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
                dose_count[tn]=dose_count[tn]+1;
                //

            } else {
                ;
            }
        } else if (as< string>(tform[ij])=="lin_slope"){
            T0.col(ij) = (df0.col(df0_c).array() - beta_0[ij+1]);
            //
            T0.col(ij) = (T0.col(ij).array() < 0).select(0, T0.col(ij));
            //
            T0.col(ij) = beta_0[ij] *T0.col(ij);
            T0.col(ij+1) = T0.col(ij);
            //
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;

        } else if (as< string>(tform[ij])=="lin_int") {
            ;
        } else if (as< string>(tform[ij])=="quad_slope"){
            //
            T0.col(ij) = beta_0[ij] * df0.col(df0_c).array().square();
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
        } else if (as< string>(tform[ij])=="step_slope"){
            T0.col(ij) = (df0.col(df0_c).array() - beta_0[ij+1]);
            //
            T0.col(ij) = (T0.col(ij).array() < 0).select(0.0, MatrixXd::Zero(T0.rows(),1).array()+1.0);
            //
            T0.col(ij) = beta_0[ij] * T0.col(ij);
            T0.col(ij+1) = T0.col(ij);
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
        } else if (as< string>(tform[ij])=="step_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_quad_slope") {
            ArrayXd temp = (df0.col(df0_c).array() - beta_0[ij+1]);
            T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]);
            T0.col(ij+1) = (df0.col(df0_c).array().pow(2).array() * beta_0[ij] /2 / beta_0[ij+1] + beta_0[ij] /2 * beta_0[ij+1]);
            //
            temp = (temp.array() < 0).select(T0.col(ij), T0.col(ij+1));
            //
            T0.col(ij) = temp.array();
            T0.col(ij+1) = temp.array();
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
        } else if (as< string>(tform[ij])=="lin_quad_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_exp_slope") {
            T0.col(ij) = (df0.col(df0_c).array() - beta_0[ij+1]);
            double c1;
            double a1;
            if (beta_0[ij]<0){
                c1 = log(-1*beta_0[ij]/beta_0[ij+2]) + beta_0[ij+1] * beta_0[ij+2];
                a1 = -1*beta_0[ij] * beta_0[ij+1] + exp(c1 - beta_0[ij+2] * beta_0[ij+1]);
                T0.col(ij+1) = (df0.col(df0_c).array() * beta_0[ij]);
                T0.col(ij+2) = -1*(a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
            } else {
                c1 = log(beta_0[ij]/beta_0[ij+2]) + beta_0[ij+1] * beta_0[ij+2];
                a1 = beta_0[ij] * beta_0[ij+1] + exp(c1 - beta_0[ij+2] * beta_0[ij+1]);
                T0.col(ij+1) = (df0.col(df0_c).array() * beta_0[ij]);
                T0.col(ij+2) = (a1 - (c1 - (beta_0[ij+2]) * df0.col(df0_c).array()).array().exp().array()).array();
            }
            //
            T0.col(ij) = (T0.col(ij).array() < 0).select(T0.col(ij+1), T0.col(ij+2));
            //
            T0.col(ij+1) = T0.col(ij);
            T0.col(ij+2) = T0.col(ij);
            Dose.col(tn) = Dose.col(tn).array() + T0.col(ij).array();
            dose_count[tn]=dose_count[tn]+1;
        } else if (as< string>(tform[ij])=="lin_exp_int") {
            ;
        } else if (as< string>(tform[ij])=="lin_exp_exp_slope") {
            ;
        } else {
            throw invalid_argument( "incorrect subterm type" );
        }
    }
    //
    // Calculates the terms and derivatives
    //
    for (int ijk=0; ijk<nonDose.cols();ijk++){ //combines non-dose terms into a single term
        if (dose_count[ijk]==0){
            Dose.col(ijk) = Dose.col(ijk).array() * 0.0 + 1;
        }
        if (lin_count[ijk]==0){
            nonDose_LIN.col(ijk) = nonDose_LIN.col(ijk).array() * 0.0 + 1;//replaces missing data with 1
        }
        nonDose.col(ijk) = nonDose_LIN.col(ijk).array()  * nonDose_PLIN.col(ijk).array()  * nonDose_LOGLIN.col(ijk).array() ;
    }
    TTerm << Dose.array() * nonDose.array();
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ij=0;ij<(totalnum);ij++){
        int tn = term_n[ij];
        if (as< string>(tform[ij])=="loglin") {
            T0.col(ij) = nonDose_LOGLIN.col(tn);
        } else if (as< string>(tform[ij])=="lin") {
            T0.col(ij) = nonDose_LIN.col(tn);

        } else if (as< string>(tform[ij])=="plin") {
            T0.col(ij) = nonDose_PLIN.col(tn);
        } else if (as< string>(tform[ij])=="loglin_slope"){
            //
            T0.col(ij) = Dose.col(tn);
            T0.col(ij+1) = Dose.col(tn);
            
        } else if (as< string>(tform[ij])=="loglin_top"){
            if (ij==0){
                T0.col(ij) = Dose.col(tn);

            } else if (tform[ij-1]!="loglin_slope"){
                T0.col(ij) = Dose.col(tn);
                //
            } else {
                ;
            }
        } else if (as< string>(tform[ij])=="lin_slope"){
            //
            T0.col(ij) = Dose.col(tn);
            T0.col(ij+1) = Dose.col(tn);

        } else if (as< string>(tform[ij])=="quad_slope"){
            //
            T0.col(ij) = Dose.col(tn);
        } else if (as< string>(tform[ij])=="step_slope"){
            //
            T0.col(ij) = Dose.col(tn);
            T0.col(ij+1) = Dose.col(tn);
        } else if (as< string>(tform[ij])=="lin_quad_slope") {
            //
            T0.col(ij) = Dose.col(tn);
            T0.col(ij+1) = Dose.col(tn);
            //
        } else if (as< string>(tform[ij])=="lin_exp_slope") {
            //
            T0.col(ij) = Dose.col(tn);
            T0.col(ij+1) = Dose.col(tn);
            T0.col(ij+2) = Dose.col(tn);
            //
        } else {
            ;
        }
    }
    return;
}

//' Utility function to calculate the term and subterm values with the basic model
//'
//' \code{Make_subterms_Basic} Called to update term matrices, Uses lists of term numbers and types to apply formulas
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: subterm matrices, Term matrices
//' @noRd
// [[Rcpp::export]]
void Make_subterms_Basic(const int& totalnum, const IntegerVector& dfc, MatrixXd& T0, const VectorXd& beta_0,const MatrixXd& df0, const int& nthreads, bool debugging){
    //
    // Calculates the sub term values
    //
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ij=0;ij<(totalnum);ij++){
        int df0_c = dfc[ij]-1;
        T0.col(ij) = (df0.col(df0_c).array() * beta_0[ij]).matrix();
        T0.col(ij) = T0.col(ij).array().exp();
    }
    return;
}

//' Utility function to calculate the risk and risk ratios
//'
//' \code{Make_Risks} Called to update risk matrices, Splits into cases based on model form, Uses lists of term numbers and types to apply different derivative formulas    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Risk, Risk ratios
//' @noRd
// [[Rcpp::export]]
void Make_Risks(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const MatrixXd& T0, const MatrixXd& Td0, const MatrixXd& Tdd0, MatrixXd& Te, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, MatrixXd& RdR, MatrixXd& RddR, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, const double gmix_theta, const IntegerVector& gmix_term){
    set<string> Dose_Iden; //List of dose subterms
    Dose_Iden.insert("loglin_top");
    Dose_Iden.insert("loglin_slope");
    Dose_Iden.insert("lin_slope");
    Dose_Iden.insert("lin_int");
    Dose_Iden.insert("quad_slope");
    Dose_Iden.insert("step_slope");
    Dose_Iden.insert("step_int");
    Dose_Iden.insert("lin_quad_slope");
    Dose_Iden.insert("lin_quad_int");
    Dose_Iden.insert("lin_exp_slope");
    Dose_Iden.insert("lin_exp_int");
    Dose_Iden.insert("lin_exp_exp_slope");
    //
    //
    MatrixXd Tterm_ratio = MatrixXd::Constant(Td0.rows(),Td0.cols(), 1.0);
    int reqrdnum = totalnum - sum(KeepConstant);
    //
    if (((modelform=="A")||(modelform=="PA")||(modelform=="PAE"))&&(TTerm.cols()>1)){ //same process used for all of the additive type models
        Te = TTerm.array().rowwise().sum().array();
        // computes initial risk and derivatives
        if (modelform=="A"){
            R << Te.array();
            //
            #ifdef _OPENMP
            #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
            #endif
            for (int ijk=0;ijk<totalnum*(totalnum+1)/2;ijk++){
                int ij = 0;
                int jk = ijk;
                while (jk>ij){
                    ij++;
                    jk-=ij;
                }
                int tij = term_n[ij];
                int tjk = term_n[jk];
                if (KeepConstant[ij]+KeepConstant[jk]==0){
                    //
                    ij = ij - sum(head(KeepConstant,ij));
                    jk = jk - sum(head(KeepConstant,jk));
                    int p_ijk = ij*(ij+1)/2 + jk;
                    //
                    if (ij==jk){
                        if (tform[ij]=="loglin") {
                            Rd.col(ij) =   TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                            Rdd.col(p_ijk) = TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Tdd0.col(p_ijk).array();
                        } else if (Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end()){
                            Rd.col(ij) =   nonDose.col(tij).array() *   Td0.col(ij).array();
                            Rdd.col(p_ijk) = nonDose.col(tij).array() *   Tdd0.col(p_ijk).array();
                        } else if (tform[ij]=="lin") {
                            Rd.col(ij) =   nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() *   Td0.col(ij).array();
                        } else if (tform[ij]=="plin") {
                            Rd.col(ij) =   nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() *   Td0.col(ij).array();
                        }
                    } else if (tij==tjk){
                        if (Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end()){
                            if (tform[jk]=="loglin") {
                                Rdd.col(p_ijk) = nonDose.col(tij).array() * Td0.col(ij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(jk).array();
                            } else if (Dose_Iden.find(as< string>(tform[jk])) != Dose_Iden.end()){
                                Rdd.col(p_ijk) = nonDose.col(tij).array() * Tdd0.col(p_ijk).array();
                            } else if (tform[jk]=="lin") {
                                Rdd.col(p_ijk) = nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            } else if (tform[jk]=="plin") {
                                Rdd.col(p_ijk) = nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            }
                        } else if (Dose_Iden.find(as< string>(tform[jk])) != Dose_Iden.end()){
                            if (tform[ij]=="loglin") {
                                Rdd.col(p_ijk) = nonDose.col(tij).array() * Td0.col(jk).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                            } else if (tform[ij]=="lin") {
                                Rdd.col(p_ijk) = nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            } else if (tform[ij]=="plin") {
                                Rdd.col(p_ijk) = nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            }
                        } else if (tform[ij]=="loglin") {
                            if (tform[jk]=="loglin") {
                                Rdd.col(p_ijk) = TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Tdd0.col(p_ijk).array();
                            } else if( tform[jk]=="lin") {
                                Rdd.col(p_ijk) = nonDose_PLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            } else if (tform[jk]=="plin") {
                                Rdd.col(p_ijk) = nonDose_LIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            }
                        } else if (tform[jk]=="loglin") {
                            if( tform[ij]=="lin") {
                                Rdd.col(p_ijk) = nonDose_PLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            } else if (tform[ij]=="plin") {
                                Rdd.col(p_ijk) = nonDose_LIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            }
                        } else if (tform[ij]=="lin") {
                            if( tform[jk]=="lin") {
                                ;
                            } else if (tform[jk]=="plin") {
                                Rdd.col(p_ijk) = nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                            }
                        } else if (tform[jk]=="lin") {
                            if (tform[ij]=="plin") {
                                Rdd.col(p_ijk) = nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array();
                                Rdd.col(p_ijk) = TTerm.col(tij).array() * Rdd.col(p_ijk).array();
                                Rdd.col(p_ijk) = Rdd.col(p_ijk).array() * Td0.col(jk).array() *  Td0.col(ij).array();
                            }
                        } else {
                            ;
                        }
                    }
                }
            }
        } else if ((modelform=="PAE")||(modelform=="PA")){
            Te = Te.array() - TTerm.col(fir).array();
            if (modelform=="PAE"){
                Te = Te.array() + 1;
            }
            R << TTerm.col(fir).array() * Te.array();
            #ifdef _OPENMP
            #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
            #endif
            for (int ijk=0;ijk<totalnum*(totalnum+1)/2;ijk++){
                int ij = 0;
                int jk = ijk;
                while (jk>ij){
                    ij++;
                    jk-=ij;
                }
                int tij = term_n[ij];
                int tjk = term_n[jk];
                if (KeepConstant[ij]+KeepConstant[jk]==0){
                    //
                    ij = ij - sum(head(KeepConstant,ij));
                    jk = jk - sum(head(KeepConstant,jk));
                    int p_ijk = ij*(ij+1)/2 + jk;
                    //
                    if (ij==jk){
                        if (tij==fir){
                            if (Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end()){
                                Rd.col(ij) =   R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                Rdd.col(p_ijk) =  R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Tdd0.col(p_ijk).array();
                            } else if (tform[ij]=="lin") {
                                Rd.col(ij) =   R.col(0).array() * nonDose_LIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                Rdd.col(p_ijk) =  R.col(0).array() * nonDose_LIN.col(tij).array().pow(-1).array() * Tdd0.col(p_ijk).array();
                            } else if (tform[ij]=="plin") {
                                Rd.col(ij) =   R.col(0).array() * nonDose_PLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                Rdd.col(p_ijk) =  R.col(0).array() * nonDose_PLIN.col(tij).array().pow(-1).array() * Tdd0.col(p_ijk).array();
                            } else if (tform[ij]=="loglin") {
                                Rd.col(ij) =   R.col(0).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                Rdd.col(p_ijk) =  R.col(0).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Tdd0.col(p_ijk).array();
                            }
                        } else {
                            if (Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end()){
                                Rd.col(ij) =   TTerm.col(fir).array() * nonDose.col(tij).array() * Td0.col(ij).array();
                                Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose.col(tij).array() * Tdd0.col(p_ijk).array();
                            } else if (tform[ij]=="lin") {
                                Rd.col(ij) =   TTerm.col(fir).array() * nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array();
                                Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Tdd0.col(p_ijk).array();
                            } else if (tform[ij]=="plin") {
                                Rd.col(ij) =   TTerm.col(fir).array() * nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array();
                                Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Tdd0.col(p_ijk).array();
                            } else if (tform[ij]=="loglin") {
                                Rd.col(ij) =   TTerm.col(fir).array() * TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                Rdd.col(p_ijk) = TTerm.col(fir).array() * TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Tdd0.col(p_ijk).array();
                            }
                        }
                    } else {
                        if (tij==tjk){
                            if (tij==fir){
                                if (Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end()){
                                    if (Dose_Iden.find(as< string>(tform[jk])) != Dose_Iden.end()){
                                        Rdd.col(p_ijk) = R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Tdd0.col(p_ijk).array();
                                    } else if (tform[jk]=="lin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_LIN.col(tij).array().pow(-1).array()    * Td0.col(jk).array();
                                    } else if (tform[jk]=="plin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_PLIN.col(tij).array().pow(-1).array()   * Td0.col(jk).array();
                                    } else if (tform[jk]=="loglin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(jk).array();
                                    }
                                } else if (Dose_Iden.find(as< string>(tform[jk])) != Dose_Iden.end()){
                                    if (tform[ij]=="lin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Td0.col(jk).array() * nonDose_LIN.col(tij).array().pow(-1).array()    * Td0.col(ij).array();
                                    } else if (tform[ij]=="plin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Td0.col(jk).array() * nonDose_PLIN.col(tij).array().pow(-1).array()   * Td0.col(ij).array();
                                    } else if (tform[ij]=="loglin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * Dose.col(tij).array().pow(-1).array() * Td0.col(jk).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                    }
                                } else if (tform[ij]=="loglin") {
                                    if( tform[jk]=="lin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_LIN.col(tij).array().pow(-1).array() * Td0.col(jk).array();
                                    } else if (tform[jk]=="plin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_PLIN.col(tij).array().pow(-1).array() * Td0.col(jk).array();
                                    } else if (tform[jk]=="loglin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_LOGLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array();
                                    }
                                } else if (tform[jk]=="loglin") {
                                    if( tform[ij]=="lin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * nonDose_LOGLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array() * nonDose_LIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                    } else if (tform[ij]=="plin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * nonDose_LOGLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array() * nonDose_PLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                    }
                                } else if (tform[ij]=="lin") {
                                    if( tform[jk]=="lin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * nonDose_LIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_LIN.col(tij).array().pow(-1).array() * Td0.col(jk).array();
                                    } else if (tform[jk]=="plin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * nonDose_LIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_PLIN.col(tij).array().pow(-1).array() * Td0.col(jk).array();
                                    }
                                } else if (tform[jk]=="lin") {
                                    if (tform[ij]=="plin") {
                                        Rdd.col(p_ijk) = R.col(0).array() * nonDose_LIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array() * nonDose_PLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                    }
                                } else {
                                    Rdd.col(p_ijk) = R.col(0).array() * nonDose_PLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array() * nonDose_PLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                }
                            } else {
                                if (Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end()){
                                    if (Dose_Iden.find(as< string>(tform[jk])) != Dose_Iden.end()){
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose.col(tij).array() * Tdd0.col(p_ijk).array();
                                    } else if (tform[jk]=="lin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    } else if (tform[jk]=="plin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    } else if (tform[jk]=="loglin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose.col(tij).array() * Td0.col(ij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(jk).array();
                                    }
                                } else if (Dose_Iden.find(as< string>(tform[jk])) != Dose_Iden.end()){
                                    if (tform[ij]=="lin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    } else if (tform[ij]=="plin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    } else if (tform[ij]=="loglin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose.col(tij).array() * Td0.col(jk).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                    }
                                } else if (tform[ij]=="loglin") {
                                    if( tform[jk]=="lin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_PLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    } else if (tform[jk]=="plin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_PLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    } else if (tform[jk]=="loglin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_LOGLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array();
                                    }
                                } else if (tform[jk]=="loglin") {
                                    if( tform[ij]=="lin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_PLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    } else if (tform[ij]=="plin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_LIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    }
                                } else if (tform[ij]=="lin") {
                                    if( tform[jk]=="lin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    } else if (tform[jk]=="plin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    }
                                } else if (tform[jk]=="lin") {
                                    if (tform[ij]=="plin") {
                                        Rdd.col(p_ijk) = TTerm.col(fir).array() * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                    }
                                } else {
                                    ;
                                }
                            }  
                        } else if ((tij==fir)||(tjk==fir)){
                            if (Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end()){
                                if (Dose_Iden.find(as< string>(tform[jk])) != Dose_Iden.end()){
                                    Rdd.col(p_ijk) = TTerm.col(tjk).array() * nonDose.col(tij).array() * Tdd0.col(p_ijk).array();
                                } else if (tform[jk]=="lin") {
                                    Rdd.col(p_ijk) = nonDose_PLIN.col(tjk).array()  * nonDose_LOGLIN.col(tjk).array() * Dose.col(tjk).array() * nonDose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                } else if (tform[jk]=="plin") {
                                    Rdd.col(p_ijk) = nonDose_LIN.col(tjk).array()  * nonDose_LOGLIN.col(tjk).array() * Dose.col(tjk).array() * nonDose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                } else if (tform[jk]=="loglin") {
                                    Rdd.col(p_ijk) = TTerm.col(tjk).array() * nonDose.col(tij).array() * Td0.col(ij).array() * nonDose_LOGLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array();
                                }
                            } else if (Dose_Iden.find(as< string>(tform[jk])) != Dose_Iden.end()){
                                if (tform[ij]=="lin") {
                                    Rdd.col(p_ijk) = nonDose.col(tjk).array() * nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                                } else if (tform[ij]=="plin") {
                                    Rdd.col(p_ijk) = nonDose.col(tjk).array() * nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                                } else if (tform[ij]=="loglin") {
                                    Rdd.col(p_ijk) = TTerm.col(tij).array() * nonDose.col(tjk).array() * Td0.col(jk).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array();
                                }
                            } else if (tform[ij]=="loglin") {
                                if( tform[jk]=="lin") {
                                    Rdd.col(p_ijk) = nonDose_PLIN.col(tjk).array()  * nonDose_LOGLIN.col(tjk).array() * Dose.col(tjk).array() * TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * Td0.col(jk).array();
                                } else if (tform[jk]=="plin") {
                                    Rdd.col(p_ijk) = nonDose_LIN.col(tjk).array()  * nonDose_LOGLIN.col(tjk).array() * Dose.col(tjk).array() * TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * Td0.col(jk).array();
                                } else if (tform[jk]=="loglin") {
                                    Rdd.col(p_ijk) = TTerm.col(tjk).array() * TTerm.col(tij).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() * Td0.col(ij).array() * nonDose_LOGLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array();
                                }
                            } else if (tform[jk]=="loglin") {
                                if( tform[ij]=="lin") {
                                    Rdd.col(p_ijk) = nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * TTerm.col(tjk).array() * nonDose_LOGLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                } else if (tform[ij]=="plin") {
                                    Rdd.col(p_ijk) = nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * TTerm.col(tjk).array() * nonDose_LOGLIN.col(tjk).array().pow(-1).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                }
                            } else if (tform[ij]=="lin") {
                                if( tform[jk]=="lin") {
                                    Rdd.col(p_ijk) = nonDose_PLIN.col(tjk).array()  * nonDose_LOGLIN.col(tjk).array() * Dose.col(tjk).array() * nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                                } else if (tform[jk]=="plin") {
                                    Rdd.col(p_ijk) = nonDose_LIN.col(tjk).array()  * nonDose_LOGLIN.col(tjk).array() * Dose.col(tjk).array() * nonDose_PLIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(ij).array() * Td0.col(jk).array();
                                }
                            } else if (tform[jk]=="lin") {
                                if (tform[ij]=="plin") {
                                    Rdd.col(p_ijk) = nonDose_PLIN.col(tjk).array()  * nonDose_LOGLIN.col(tjk).array() * Dose.col(tjk).array() * nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                                }
                            } else {
                                Rdd.col(p_ijk) = nonDose_LIN.col(tjk).array()  * nonDose_LOGLIN.col(tjk).array() * Dose.col(tjk).array() * nonDose_LIN.col(tij).array()  * nonDose_LOGLIN.col(tij).array() * Dose.col(tij).array() * Td0.col(jk).array() * Td0.col(ij).array();
                            }
                        }
                    }
                }
            }
        }
    }else if ((modelform=="M")||(((modelform=="A")||(modelform=="PA")||(modelform=="PAE"))&&(TTerm.cols()==1))){
        //
        MatrixXd TTerm_p = MatrixXd::Zero(TTerm.rows(),TTerm.cols());
        TTerm_p << TTerm.array() + 1.0;
        TTerm_p.col(fir) = TTerm.col(fir).array();
        Te = TTerm_p.array().rowwise().prod().array();
        R << Te.array();
        //
        Rd = Td0.array();
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ij=0;ij<totalnum;ij++){
            int tij = term_n[ij];
            if (KeepConstant[ij]==0){
                int ijk = ij - sum(head(KeepConstant,ij));
                if (tij != fir){
                    Tterm_ratio.col(ijk) = TTerm.col(tij).array() * TTerm_p.col(tij).array().pow(-1).array();
                }
                if (tform[ij]=="loglin") {
                    Tterm_ratio.col(ijk) = Tterm_ratio.col(ijk).array() * nonDose_LOGLIN.col(tij).array().pow(-1).array() ;
                } else if (Dose_Iden.find(as< string>(tform[ij])) != Dose_Iden.end()){
                    Tterm_ratio.col(ijk) = Tterm_ratio.col(ijk).array() * Dose.col(tij).array().pow(-1).array();
                } else if (tform[ij]=="lin") {
                    Tterm_ratio.col(ijk) = Tterm_ratio.col(ijk).array() * nonDose_LIN.col(tij).array().pow(-1).array();
                } else if (tform[ij]=="plin") {
                    Tterm_ratio.col(ijk) = Tterm_ratio.col(ijk).array() * nonDose_PLIN.col(tij).array().pow(-1).array();
                }
                Rd.col(ijk) = R.col(0).array() * Td0.array().col(ijk).array() * Tterm_ratio.col(ijk).array();
            }
        }
        R = (R.array().isFinite()).select(R,-1);
        Rd = (Rd.array().isFinite()).select(Rd,0);
        //
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<totalnum*(totalnum+1)/2;ijk++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            int tij = term_n[ij];
            int tjk = term_n[jk];
            if (KeepConstant[ij]+KeepConstant[jk]==0){
                //
                ij = ij - sum(head(KeepConstant,ij));
                jk = jk - sum(head(KeepConstant,jk));
                int p_ijk = ij*(ij+1)/2 + jk;
                //
                if (tij==tjk){
                    Rdd.col(p_ijk) = R.col(0).array() * Tterm_ratio.col(ij).array() * Tdd0.array().col(p_ijk).array();
                } else {
                    Rdd.col(p_ijk) = R.col(0).array() * Tterm_ratio.col(ij).array() * Tterm_ratio.col(jk).array() * Td0.array().col(ij).array() * Td0.array().col(jk).array();
                    //
                }
            }
        }
    } else if (modelform=="GMIX"){
        VectorXd A_vec(TTerm.rows(),1);
        VectorXd B_vec(TTerm.rows(),1);
        //
        for (int ij=0;ij<TTerm.cols();ij++){
            if (ij==fir){
                ;
            } else if (gmix_term[ij]==1){
                TTerm.col(ij) = TTerm.col(ij).array() + 1;
            }
        }
        //
        A_vec = TTerm.block(0,1,TTerm.rows(),TTerm.cols()-1).array().rowwise().prod().array();
        B_vec = TTerm.rightCols(TTerm.cols()-1).array().rowwise().sum().array() - TTerm.cols() + 2;
        R << TTerm.col(0).array() * A_vec.array().pow(gmix_theta).array() * B_vec.array().pow(1-gmix_theta).array();
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ij=0;ij<totalnum;ij++){
            int tij = term_n[ij];
            if (KeepConstant[ij]==0){
                int ijk = ij - sum(head(KeepConstant,ij));
                if (tij != fir){
                    Rd.col(ijk) = R.col(0).array() * Td0.col(ijk).array() * ((1-gmix_theta) * B_vec.array().pow(-1).array() + gmix_theta * TTerm.col(tij).array().pow(-1).array());
                } else {
                    Rd.col(ijk) = R.col(0).array() * Td0.col(ijk).array() *  TTerm.col(tij).array().pow(-1).array();
                }
            }
        }
        //
        #ifdef _OPENMP
        #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
        #endif
        for (int ijk=0;ijk<totalnum*(totalnum+1)/2;ijk++){
            int ij = 0;
            int jk = ijk;
            while (jk>ij){
                ij++;
                jk-=ij;
            }
            int tij = term_n[ij];
            int tjk = term_n[jk];
            if (KeepConstant[ij]+KeepConstant[jk]==0){
                //
                ij = ij - sum(head(KeepConstant,ij));
                jk = jk - sum(head(KeepConstant,jk));
                int p_ijk = ij*(ij+1)/2 + jk;
                //
                if (tij==tjk){
                    if (tij==fir){
                        Rdd.col(p_ijk) = Tdd0.array().col(p_ijk).array() * A_vec.array().pow(gmix_theta).array() * B_vec.array().pow(1-gmix_theta).array();
                    } else {
                        VectorXd C_vec = Rd.col(ij).array() * Td0.col(jk).array().pow(-1).array();
                        Rdd.col(p_ijk) = Td0.col(ij).array() * (R.col(0).array() * Td0.col(jk).array() * ((gmix_theta-1) * B_vec.array().pow(-2).array() - gmix_theta * TTerm.col(tij).array().pow(-2).array()) + Rd.col(jk).array() * C_vec.array() * R.col(0).array().pow(-1).array()) + Tdd0.array().col(p_ijk).array() * C_vec.array();
                    }
                } else {
                    if ((tij==fir) or (tjk==fir)){
                        Rdd.col(p_ijk) = Td0.col(ij).array() * TTerm.col(tij).array().pow(-1).array() * Rd.col(jk).array();
                    } else {
                        Rdd.col(p_ijk) = Td0.col(ij).array() * Td0.col(jk).array() * ((gmix_theta - 1) * R.col(0).array() * B_vec.array().pow(-2).array() + ((1-gmix_theta) * B_vec.array().pow(-1).array() + gmix_theta * TTerm.col(tij).array().pow(-1).array()) * ((1-gmix_theta) * B_vec.array().pow(-1).array() + gmix_theta * TTerm.col(tjk).array().pow(-1).array()));
                    }
                }
            }
        }
    } else if (modelform=="GM"){
        throw invalid_argument( "GM isn't implemented" );
    } else {
        Rcout << "C++ Note: " << modelform << ", " << TTerm.cols() << endl;
        throw invalid_argument( "Model isn't implemented" );
    }
    //
    R =   (R.array().isFinite()).select(R,-1);
    Rd =  (Rd.array().isFinite()).select(Rd,0);
    Rdd = (Rdd.array().isFinite()).select(Rdd,0);
    //
    for (int ijk=0;ijk<(reqrdnum*(reqrdnum+1)/2);ijk++){//Calculates ratios
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        if (ij==jk){
            RdR.col(ij)=R.col(0).array().pow(-1).array() * Rd.col(jk).array();
        }
        RddR.col(ijk)=R.col(0).array().pow(-1).array() * Rdd.col(ijk).array();
    }
    return;
}

//' Utility function to calculate the risk and risk ratios with a weighting applied
//'
//' \code{Make_Risks_Weighted} Called to update weighted risk matrices, Splits into cases based on model form, Uses lists of term numbers and types to apply different derivative formulas  
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Risk, Risk ratios
//' @noRd
// [[Rcpp::export]]
void Make_Risks_Weighted(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const MatrixXd& s_weights, const MatrixXd& T0, const MatrixXd& Td0, const MatrixXd& Tdd0, MatrixXd& Te, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, MatrixXd& RdR, MatrixXd& RddR, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, const double gmix_theta, const IntegerVector& gmix_term){
    //
    Make_Risks(modelform, tform, term_n, totalnum, fir, T0, Td0, Tdd0, Te, R, Rd, Rdd, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, RdR, RddR, nthreads, debugging,KeepConstant,gmix_theta, gmix_term);
    //
    int reqrdnum = totalnum - sum(KeepConstant);
    R = R.array() * s_weights.array();
    //
    R =   (R.array().isFinite()).select(R,-1);
    Rd =  (Rd.array().isFinite()).select(Rd,0);
    Rdd = (Rdd.array().isFinite()).select(Rdd,0);
    //
    for (int ijk=0;ijk<(reqrdnum*(reqrdnum+1)/2);ijk++){//Calculates ratios
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        if (ij==jk){
            Rd.col(jk) = Rd.col(jk).array() * s_weights.array();
            RdR.col(ij)=R.col(0).array().pow(-1).array() * Rd.col(jk).array();
        }
        Rdd.col(ijk) = Rdd.col(ijk).array() * s_weights.array();
        RddR.col(ijk)=R.col(0).array().pow(-1).array() * Rdd.col(ijk).array();
    }
    return;
}


//' Utility function to calculate the risk with a weighting applied and no derivatives calculated
//'
//' \code{Make_Risks_Weighted_Single} Called to update weighted risk matrices, Splits into cases based on model form, Uses lists of term numbers and types to apply different derivative formulas  
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Risk, Risk ratios
//' @noRd
// [[Rcpp::export]]
void Make_Risks_Weighted_Single(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const MatrixXd& s_weights, const MatrixXd& T0, MatrixXd& Te, MatrixXd& R, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, const double gmix_theta, const IntegerVector& gmix_term){
    //
    Make_Risks_Single(modelform, tform, term_n, totalnum, fir, T0, Te, R, Dose, nonDose, TTerm, nonDose_LIN, nonDose_PLIN, nonDose_LOGLIN, nthreads, debugging,KeepConstant,gmix_theta, gmix_term);
    //
    R = R.array() * s_weights.array();
    //
    R =   (R.array().isFinite()).select(R,-1);
    //
    return;
}

//' Utility function to calculate the risk, but not derivatives
//'
//' \code{Make_Risks_Single} Called to update risk matrices, Splits into cases based on model form   
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Risk, Risk ratios
//' @noRd
// [[Rcpp::export]]
void Make_Risks_Single(string modelform, const StringVector& tform, const IntegerVector& term_n, const int& totalnum, const int& fir, const MatrixXd& T0, MatrixXd& Te, MatrixXd& R, MatrixXd& Dose, MatrixXd& nonDose,  MatrixXd& TTerm,  MatrixXd& nonDose_LIN, MatrixXd& nonDose_PLIN, MatrixXd& nonDose_LOGLIN, const int& nthreads, bool debugging, const IntegerVector& KeepConstant, const double gmix_theta, const IntegerVector& gmix_term){
    set<string> Dose_Iden; //List of dose subterms
    Dose_Iden.insert("loglin_top");
    Dose_Iden.insert("loglin_slope");
    Dose_Iden.insert("lin_slope");
    Dose_Iden.insert( "lin_int");
    Dose_Iden.insert("quad_slope");
    Dose_Iden.insert("step_slope");
    Dose_Iden.insert("step_int");
    Dose_Iden.insert("lin_quad_slope");
    Dose_Iden.insert("lin_quad_int");
    Dose_Iden.insert("lin_exp_slope");
    Dose_Iden.insert("lin_exp_int");
    Dose_Iden.insert("lin_exp_exp_slope");
    //
    if (((modelform=="A")||(modelform=="PA")||(modelform=="PAE"))&&(TTerm.cols()>1)){ //same process used for all of the additive type models
        Te = TTerm.array().rowwise().sum().array();
        // computes initial risk and derivatives
        if (modelform=="A"){
            R << Te.array();
        } else if ((modelform=="PAE")||(modelform=="PA")){
            Te = Te.array() - TTerm.col(fir).array();
            if (modelform=="PAE"){
                Te = Te.array() + 1;
            }
            R << TTerm.col(fir).array() * Te.array();
        }
    }else if ((modelform=="M")||(((modelform=="A")||(modelform=="PA")||(modelform=="PAE"))&&(TTerm.cols()==1))){
        //
        MatrixXd TTerm_p = MatrixXd::Zero(TTerm.rows(),TTerm.cols());
        TTerm_p << TTerm.array() + 1.0;
        TTerm_p.col(fir) = TTerm.col(fir).array();
        Te = TTerm_p.array().rowwise().prod().array();
        R << Te.array();
        //
        R = (R.array().isFinite()).select(R,-1);
    } else if (modelform=="GMIX"){
        VectorXd A_vec(TTerm.rows(),1);
        VectorXd B_vec(TTerm.rows(),1);
        for (int ij=0;ij<TTerm.cols();ij++){
            if (ij==fir){
                ;
            } else if (gmix_term[ij]==1){
                TTerm.col(ij) = TTerm.col(ij).array() + 1;
            }
        }
        //
        A_vec = TTerm.block(0,1,TTerm.rows(),TTerm.cols()-1).array().rowwise().prod().array();
        B_vec = TTerm.rightCols(TTerm.cols()-1).array().rowwise().sum().array() - TTerm.cols() + 2;
        R << TTerm.col(0).array() * A_vec.array().pow(gmix_theta).array() * B_vec.array().pow(1-gmix_theta).array();
        R = (R.array().isFinite()).select(R,-1);
    } else if (modelform=="GM"){
        throw invalid_argument( "GM isn't implemented" );
    } else {
        throw invalid_argument( "Model isn't implemented" );
    }
    //
    //
    R = (R.array().isFinite()).select(R,-1);
    return;
}

//' Utility function to calculate the risk and risk ratios for the basic model
//'
//' \code{Make_Risks_Basic} Called to update risk matrices, Splits into cases based on model form, Uses lists of term numbers and types to apply different derivative formulas    
//' @inheritParams CPP_template
//'
//' @return Updates matrices in place: Risk, Risk ratios
//' @noRd
// [[Rcpp::export]]
void Make_Risks_Basic(const int& totalnum, const MatrixXd& T0, MatrixXd& R, MatrixXd& Rd, MatrixXd& Rdd, MatrixXd& RdR, const int& nthreads, bool debugging,const MatrixXd& df0, const IntegerVector& dfc, const IntegerVector& KeepConstant){
    //
    R.col(0) = T0.rowwise().prod();
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ij=0;ij<totalnum;ij++){
        int df0_c = dfc[ij]-1;
        if (KeepConstant[ij]==0){
            int ijk = ij - sum(head(KeepConstant,ij));
            Rd.col(ijk) = R.col(0).array() * df0.col(df0_c).array() ;
        }
    }
    R = (R.array().isFinite()).select(R,-1);
    Rd = (Rd.array().isFinite()).select(Rd,0);
    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic) num_threads(nthreads)
    #endif
    for (int ijk=0;ijk<totalnum*(totalnum+1)/2;ijk++){
        int ij = 0;
        int jk = ijk;
        while (jk>ij){
            ij++;
            jk-=ij;
        }
        int df0_c = dfc[ij]-1;
        if (KeepConstant[ij]+KeepConstant[jk]==0){
            //
            ij = ij - sum(head(KeepConstant,ij));
            jk = jk - sum(head(KeepConstant,jk));
            int p_ijk = ij*(ij+1)/2 + jk;
            //
            Rdd.col(p_ijk) = Rd.col(jk).array() * df0.col(df0_c).array();
        }
    }
    //
    Rdd = (Rdd.array().isFinite()).select(Rdd,0);
    //
    for (int ij=0;ij<totalnum;ij++){//Calculates ratios
        int df0_ij = dfc[ij]-1;
        if (KeepConstant[ij]==0){
        	RdR.col(ij)=df0.col(df0_ij).array();
    	}
    }
    return;
}
