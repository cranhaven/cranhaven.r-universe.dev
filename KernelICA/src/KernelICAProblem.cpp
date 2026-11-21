/*
 * KernelICAProblem.cpp
 *
 * We provide a class and module of the KernelICA problem which inherits an abstract class from the ManifoldOptim package.
 * This way no R code needs to be called during the optimization process in the Stiefel manifold.
 * The StieBrockett standalone example from ManifoldOptim was taken as a reference.
 *
 *  Created on: Apr 07, 2020
 *      Author: Christoph Koesner
 */

// [[Rcpp::depends(RcppArmadillo,RcppEigen, ManifoldOptim)]]

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <ManifoldOptim.h>

#include "IncompleteCholesky.h"
#include "KernelGauss.h"
#include "KernelHermite.h"
#include <memory>
#include "Debugging.h"


// needed for RcppModule
using namespace Rcpp;

class KernelICAProblem : public ManifoldOptimProblem{
    private:
        Eigen::MatrixXd m_data;
        double m_eps;
        double m_kappa;
        char m_type;
        std::unique_ptr<Kernel> m_ker;
        
    public:
        /*
        * In general, R can not distinguish between different constructors, 
        * when the same number of arguments are given. Luckily, with the Hermite Kernel, 
        * we also need to give the rank of the Hermite polynomial. 
        */
        
        /*
         * Constructor for Kernel ICA with Gauss Kernel
         */
        KernelICAProblem(Eigen::MatrixXd data, double eps, double kappa, char type, double sigma):
        m_data(data), m_eps(eps), m_kappa(kappa), m_type(type),
        m_ker(new KernelGauss(sigma))
        {}
        /*
         * Constructor for Kernel ICA with Hermite Kernel. 
         */
        KernelICAProblem(Eigen::MatrixXd data, double eps, double kappa, char type, double sigma, int d):
        m_data(data), m_eps(eps), m_kappa(kappa), m_type(type),
        m_ker(new KernelHermite(sigma, d))
        {}

        KernelICAProblem(const KernelICAProblem&) = delete;
        KernelICAProblem& operator=(const KernelICAProblem&) = delete;

        /*
         * The following three methods overwrite the virtual methods from the
         * ManifoldOptimProblem class
         */
        double objFun(const arma::vec& x) const{
            // Rcout << "Object function called. \n";
            
            arma::mat X = x;
            X.reshape(m_data.cols(), m_data.cols());
            Eigen::MatrixXd X_eigen = castArmaEigen(X);
            return(contrast(m_data * X_eigen));
        }
        
        /*
         * We have not yet implemented the gradient. This can however
         * give a large performance boost to the method
         */
        arma::vec gradFun(const arma::vec& x) const{
            return ManifoldOptimProblem::gradFun(x);
        }
        
        arma::vec hessEtaFun(const arma::vec& x, const arma::vec& eta) const
        {
            return ManifoldOptimProblem::hessEtaFun(x, eta);
        }
        
        /*
         * Since we have written the contrast function and the incomplete Cholesky decomposition in Eigen
         * and the overwritten methods from ManifoldOptimProblem only accept Armadillo matrices as input,
         * we need a cast function.
         */
        Eigen::MatrixXd castArmaEigen(arma::mat arma_A) const{
            Eigen::MatrixXd eigen_B = Eigen::Map<Eigen::MatrixXd>(arma_A.memptr(),
                                                                  arma_A.n_rows,
                                                                  arma_A.n_cols);
            return eigen_B;
        }
        
        double contrast(const Eigen::MatrixXd& x) const{
            // Rcout << "contrast begin \n";
            
            int i = 0;
            int j = 0;
            int n = x.rows(); //number of samples
            int m = x.cols(); //number of input
            int n2 = 0;
            int cut = 0;
            double res = 0;
            
            Eigen::VectorXd x_cur(n);
            choleskyReturnVal chol_ret{};
            
            std::vector<Eigen::MatrixXd> u_svds{};
            std::vector<Eigen::VectorXd> d_svds{};
            
            u_svds.reserve(m);
            d_svds.reserve(m);
            
            std::vector<int> indeces(m+1);
            std::vector<int> sizes(m);
            
            indeces[0] = 0;
            
            // Rcout << "dimensions\n";
            for(i = 0; i < m; i++){
                x_cur = x.block(0,i,n,1);
                // low rank cholesky approximation
                chol_ret = choleskyLowRank(x_cur, *m_ker, n*m_eps);
                Eigen::MatrixXd chol_res = chol_ret.perm * chol_ret.L;
                chol_res.rowwise() -= chol_res.colwise().mean();
                n2 = chol_res.cols();

                // singular value decomposition
                Eigen::JacobiSVD<Eigen::MatrixXd> L_svd(chol_res, Eigen::ComputeThinU);
                Eigen::ArrayXd val = L_svd.singularValues().array().square();

                cut = (val < n*m_eps).matrix().count();
                sizes[i] = n2-cut;
                indeces[i+1] = indeces[i]+sizes[i]; // last index only used for full size of Rkappa matrix

                u_svds.push_back(L_svd.matrixU().block(0,0,n,n2-cut));
                Eigen::ArrayXd val_red = val.block(0,0,n2-cut,1);
                d_svds.push_back((val_red/ (val_red+ (n * m_kappa / 2) )).matrix());
            }

            Eigen::MatrixXd mat = Eigen::MatrixXd::Identity(indeces[m], indeces[m]);
            // 
            if(m_type == 'c'){ // kcca case
              // only the lower part of the matrix needs to be calculated because of the symmetric property.
              // the selfadjoint eigensolver does not need the upper part.
                for(i = 1; i < m; i++){
                    for(j = 0; j < i; j++){
                        Eigen::MatrixXd tmp = d_svds[i].asDiagonal() * u_svds[i].transpose() * u_svds[j] * d_svds[j].asDiagonal();
                        mat.block(indeces[i], indeces[j], sizes[i], sizes[j]) = tmp;
                    }
                }

                Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(indeces[m]);
                es.compute(mat);
                res = es.eigenvalues().minCoeff();
                if(std::isnan(res)){
                  Rcout << mat << "\n";
                }
                ASSERT(!std::isnan(res), "kcca, nan value before logarithm");
                ASSERT(res > 0, "kcca, non-positive input for logarithm");
                res = -0.5 * std::log(res);
            }else{ // kgv case
                for(i = 1; i < m; i++){
                    for(j = 0; j < i; j++){
                        Eigen::MatrixXd tmp = d_svds[i].asDiagonal() * u_svds[i].transpose() * u_svds[j] * d_svds[j].asDiagonal();
                        mat.block(indeces[i], indeces[j], sizes[i], sizes[j]) = tmp;
                        if(i != j){
                            mat.block(indeces[j], indeces[i], sizes[j], sizes[i]) = tmp.transpose();
                        }
                    }
                }

                res = mat.determinant();
                ASSERT(!std::isnan(res), "kgv, nan value before logarithm");
                ASSERT(res > 0, "kgv: non-positive input for logarithm");
                res = -0.5 * std::log(res);
            }
            ASSERT(!std::isnan(res), "nan value after logarithm");
            return res; 
        }
};
        
RCPP_MODULE(KernelICA_module) {
        class_<KernelICAProblem>("KernelICAProblem")
        .constructor<Eigen::MatrixXd, double, double, char, double>()
        .constructor<Eigen::MatrixXd, double, double, char, double, int>()
        .method("objFun", &KernelICAProblem::objFun)
        .method("contrast", &KernelICAProblem::contrast)
        ;
}
