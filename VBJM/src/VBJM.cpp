#include <math.h>
#include <RcppArmadillo.h>
#include <RcppEnsmallen.h>
using namespace Rcpp;

// [[Rcpp::depends("RcppEnsmallen")]]
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(cpp11)]]

// maximum exponent to avoid numerical instability //
const double MAX_EXP = 15;

// matrix inverse //
arma::mat myinvCpp(const arma::mat& A){
    bool invflag = false;
    arma::mat B = A;
    invflag = arma::inv_sympd(B , A, arma::inv_opts::allow_approx);
    if(!invflag){
        //Rcout << "inv_sympd failed, try inv\n";
        invflag = arma::inv( B, A, arma::inv_opts::allow_approx);
        if(!invflag){
            //Rcout << "inv failed, try pinv\n";
            invflag = arma::pinv( B,A);
            if(!invflag){
                //Rcout << "all inv methods failed!\n";
                throw std::runtime_error("error");
            }
        }
    }
    return B;
}


// Cholesky decomposition //
arma::mat myCholCpp(arma::mat A){
    bool flag = false;
    arma::mat B( arma::size(A), arma::fill::zeros);
    flag = arma::chol(B , A, "lower");
    if(!flag){
        arma::vec avec = A.diag();
        arma::vec tmp = avec.elem(arma::find(avec));
        double val = 0.01 * arma::mean(arma::abs(tmp));
        A.diag() += val;
        flag = arma::chol(B , A,"lower");
        if(!flag){
            B.diag().fill(val);
        }
    }
    return B;
}


//' Main function to run LME
//'
//'@noRd
//'
// [[Rcpp::export]]
List init_LME(arma::field<arma::vec> Y, arma::field<arma::mat> X,
              arma::field<arma::mat> Z,
              const int maxiter=100, const double eps=1e-4){

    const int n = Y.n_elem;
    const int px = X(0).n_cols;
    const int pz = Z(0).n_cols;
    const int npara = pz*pz + px + 1;

    arma::field<arma::vec> mu(n);
    arma::field<arma::mat> V(n);
    arma::vec beta(px);
    double sig2 = 1;
    arma::mat Sigma(pz,pz);
    Sigma.diag().fill(1.0);
    arma::mat Sigma_inv = Sigma;

    int N=0;
    arma::mat XXinv(px,px, arma::fill::zeros);
    arma::vec XY(px, arma::fill::zeros);
    arma::field<arma::mat> ZZ(n);
    for(int i=0; i<n; i++){
        XXinv += X(i).t() * X(i);
        XY += X(i).t() * Y(i);
        N += Y(i).n_elem;
        ZZ(i) = Z(i).t() * Z(i);
    }
    XXinv = myinvCpp(XXinv);
    beta = XXinv*XY;

    arma::mat Sigma_tmp(pz, pz, arma::fill::zeros);
    int iter = 0;
    arma::vec beta_00 = beta;
    arma::mat Sigma_00 = Sigma;
    double sig2_00 = sig2;

    for(iter=0; iter < maxiter; iter++){

        beta_00 = beta;
        Sigma_00 = Sigma;
        sig2_00 = sig2;
        // update mu_i and V_i
        Sigma_tmp.zeros();
        XY.zeros();
        XXinv.zeros();
        for(int i=0; i<n; i++){
            V(i) =  myinvCpp(ZZ(i) + sig2 * Sigma_inv);
            mu(i) =  V(i) * Z(i).t() *(Y(i) - X(i)* beta);
            V(i) *=  sig2;
            Sigma_tmp += mu(i)*mu(i).t() + V(i);

            arma::mat ZVZ = - Z(i) *V(i)*Z(i).t() / sig2/ sig2;
            ZVZ.diag() += 1/ sig2;

            XXinv += X(i).t() * ZVZ * X(i);
            XY += X(i).t() * ZVZ * Y(i);
        }

        //beta = XXinv*XY;
        beta = solve(XXinv, XY);
        Sigma = Sigma_tmp/n;
        Sigma_inv = myinvCpp(Sigma);

        // update sig2;
        sig2 = 0;
        for(int i=0; i<n; i++){
            sig2 += arma::trace(V(i) * ZZ(i)) +
                arma::accu(arma::square(
                        Y(i) - X(i)*beta - Z(i)*mu(i)
                ));
        }
        sig2 /= N;

        if(iter > 1){

            double err_para = arma::accu( arma::square(beta_00-beta)) +
                arma::accu( arma::square(Sigma_00-Sigma))+
                (sig2_00-sig2)*(sig2_00-sig2);

            err_para = std::sqrt(err_para/npara);
            //Rcout << iter << " err_para=" << err_para <<"\n";

            if(err_para<eps){
                break;
            }

        }
    }

    return List::create(
        _["sig2"] = sig2,
        _["Sigma"] = Sigma,
        _["beta"] = beta,
        _["mu"] = mu,
        _["V"] = V
    );
}


//// below are functions for VBJM /////


// matrix lower-triangular matrix
arma::mat makeLowTriMat(const arma::mat& V,
                        const arma::vec& Lvec){
    arma::uvec lower_indices = arma::trimatl_ind( arma::size(V) );
    arma::mat L( arma::size(V), arma::fill::zeros);
    L(lower_indices) = Lvec;
    return L;
}

// extract lower-triangular elements of matrix
arma::vec LowTriVec(const arma::mat& V){
    arma::uvec lower_indices = arma::trimatl_ind( arma::size(V) );
    arma::vec Lvec = V(lower_indices);
    return Lvec;
}


// reshape field of vec to n \times K
void field_reshape_vec(const arma::field<arma::vec>& Y_tmp,
                       arma::field<arma::vec>& Y, int n, int K ){
    int iter = 0;
    for(int k=0; k<K;k++){
        for(int i=0; i<n; i++){
            Y(i,k) = Y_tmp(iter);
            iter++;
        }
    }
}

// reshape field of mat to n \times K
void field_reshape_mat(const arma::field<arma::mat>& Y_tmp,
                       arma::field<arma::mat>& Y, int n, int K ){
    int iter = 0;
    for(int k=0; k<K;k++){
        for(int i=0; i<n; i++){
            Y(i,k) = Y_tmp(iter);
            iter++;
        }
    }
}

// convert field of vec to vec
arma::vec field_to_alpha_vec(const arma::field<arma::vec>& X_T,
                             const arma::vec& alpha,
                             int i_now, arma::uvec p_x_vec){

    int p_x = arma::accu(p_x_vec);
    arma::vec X_ia(p_x);

    int start = 0;
    for(int k=0; k<alpha.n_elem; k++){
        X_ia.subvec(start,start+p_x_vec(k)-1) = X_T(i_now, k) * alpha(k);
        start = start+p_x_vec(k);
    }
    return X_ia;
}

// convert field of mat to mat
arma::mat field_to_alpha_mat(const arma::field<arma::mat>& X_t,
                             const arma::vec& alpha,
                             int i_now, const arma::uvec& p_x_vec){

    int p_x = arma::accu(p_x_vec);
    arma::mat X_ia(X_t(i_now,0).n_rows , p_x);

    int start = 0;
    for(int k=0; k<alpha.n_elem; k++){
        X_ia.cols(start,start+p_x_vec(k)-1) = X_t(i_now, k) * alpha(k);
        start = start+p_x_vec(k);
    }
    return X_ia;
}


// convert field of mat to field of diagonal matrix
arma::field<arma::mat> field_to_field_Dmat(
        const arma::field<arma::mat>& X_t, const arma::uvec& p_x_vec){

    int p_x = arma::accu(p_x_vec);
    int K = p_x_vec.n_elem;
    int nt = X_t(0).n_rows;

    arma::field<arma::mat> X_D = arma::field<arma::mat>(nt);

    for(int j = 0; j<nt; j++){

        int start = 0, end=0;
        X_D(j) = arma::mat(p_x,K,arma::fill::zeros);
        for(int k=0; k<K; k++){
            end = start+p_x_vec(k)-1;
            X_D(j)(arma::span(start,end), k)= X_t(k).row(j).t();
            start = end +1;
        }
    }

    return X_D;
}

arma::mat field_to_Dmat(const arma::field<arma::vec>& X_T,
                        const arma::uvec& p_x_vec){

    int p_x = arma::accu(p_x_vec);
    int K = p_x_vec.n_elem;

    arma::mat X_D = arma::mat(p_x,K,arma::fill::zeros);
    int start = 0, end=0;

    for(int k=0; k<K; k++){
        end = start+p_x_vec(k)-1;
        X_D(arma::span(start,end), k) = X_T(k);
        start = end +1;
    }
    return X_D;
}


// convert vec to field of vec
arma::field<arma::vec> vec_to_field(const arma::vec& mu,
                                    const arma::uvec& p_z_vec){

    arma::field<arma::vec> mu_f(p_z_vec.n_elem);

    int start = 0;
    for(int k=0; k<p_z_vec.n_elem; k++){
        mu_f(k) = mu.subvec(start,start+p_z_vec(k)-1);
        start = start+p_z_vec(k);
    }

    return mu_f;
}


// convert  field of vec  to vec
arma::vec field_to_vec(const arma::field<arma::vec>& mu,
                       const arma::uvec& p_z_vec){

    int p_z = arma::accu(p_z_vec);
    arma::vec mu_vec(p_z);

    int start = 0;
    for(int k=0; k<p_z_vec.n_elem; k++){
        mu_vec.subvec(start,start+p_z_vec(k)-1) = mu(k);
        start = start+p_z_vec(k);
    }

    return mu_vec;
}

// data struct
struct VBJM_data_t{
    // data part //
    arma::field<arma::vec> Y; // n \times K vectors
    arma::field<arma::mat> X; // n \times K mat
    arma::field<arma::mat> Z; // n \times K mat
    arma::field<arma::vec> X_T; // n \times K vec
    arma::field<arma::vec> Z_T; // n \times K vec
    arma::field<arma::mat> X_t; // n \times K mat
    arma::field<arma::mat> Z_t; // n \times K mat
    arma::mat W; //n \times p_w  baseline covariates in survival model
    arma::field<arma::vec> GQ_w; // n \times 1 vec, Gauss-quadrature weights
    arma::field<arma::vec> GQ_t; // n \times 1 vec, Gauss-quadrature nodes

    arma::vec ftime; // n \times 1 vec
    arma::vec fstat; //  n \times 1 vec

    int K; //total number of biomarkers
    int n; //total number of subjects
    int p_x; //total number of fixed-effects
    int p_z; //total number of random-effects
    arma::uvec p_x_vec; //number of fixed-effects for each biomarker
    arma::uvec p_z_vec; //number of random-effects for each biomarker


    // initialization function //
    VBJM_data_t(const List& datalist)
    {
        ftime = as<arma::vec> (datalist["ftime"]);
        fstat = as<arma::vec> (datalist["fstat"]);
        n = ftime.n_elem;

        W = as<arma::mat>(datalist["W"]);
        arma::field<arma::vec> GQ_w_tmp = datalist["GQ_w"];
        GQ_w = GQ_w_tmp;
        GQ_w_tmp.clear();
        arma::field<arma::vec> GQ_t_tmp = datalist["GQ_t"];
        GQ_t = GQ_t_tmp;
        GQ_t_tmp.clear();

        arma::field<arma::vec> Y_tmp = datalist["Y"];
        K = Y_tmp.n_elem / n;
        Y = arma::field<arma::vec>(n,K);
        field_reshape_vec(Y_tmp, Y,  n, K);
        Y_tmp.clear();

        arma::field<arma::mat> X_tmp = datalist["X"];
        X = arma::field<arma::mat>(n,K);
        field_reshape_mat(X_tmp, X,  n, K);
        X_tmp.clear();

        p_x_vec = arma::uvec(K);
        for(int k=0; k<K; k++){
            p_x_vec(k) = X(0,k).n_cols;
        }
        p_x = arma::accu(p_x_vec);

        arma::field<arma::vec> X_T_tmp = datalist["X_T"];
        X_T = arma::field<arma::vec>(n,K);
        field_reshape_vec(X_T_tmp, X_T,  n, K);
        X_T_tmp.clear();

        arma::field<arma::mat> X_t_tmp = datalist["X_t"];
        X_t = arma::field<arma::mat>(n,K);
        field_reshape_mat(X_t_tmp, X_t,  n, K);
        X_t_tmp.clear();

        arma::field<arma::mat> Z_tmp = datalist["Z"];
        Z = arma::field<arma::mat>(n,K);
        field_reshape_mat(Z_tmp, Z,  n, K);
        Z_tmp.clear();

        p_z_vec = arma::uvec(K);
        for(int k=0; k<K; k++){
            p_z_vec(k) = Z(0,k).n_cols;
        }
        p_z = arma::accu(p_z_vec);

        arma::field<arma::vec> Z_T_tmp = datalist["Z_T"];
        Z_T = arma::field<arma::vec>(n,K);
        field_reshape_vec(Z_T_tmp, Z_T,  n, K);
        Z_T_tmp.clear();

        arma::field<arma::mat> Z_t_tmp = datalist["Z_t"];
        Z_t = arma::field<arma::mat>(n,K);
        field_reshape_mat(Z_t_tmp, Z_t,  n, K);
        Z_t_tmp.clear();
    }

};

// parameter struct

struct VBJM_para_t{
    // para part //
    arma::field<arma::vec> mu; // n \times K vec
    arma::field<arma::mat> V; // n \times 1 mat
    arma::field<arma::vec> Lvec; // n \times 1 vec: Lvec*Lvec.t() = V
    arma::field<arma::vec> beta; // K \times 1 vec
    arma::vec sig2; // dim = K
    arma::mat Sigma; // dim = (q \times K)
    arma::mat invSigma; // inverse of Sigma
    arma::vec gamma; // dim = p_w
    arma::vec alpha; // dim = K
    arma::vec weib; // dim = 2, first is shape, second is scale

    arma::uvec npara_vec; // num. of parameters in beta, gamma, alpha, weib
    // initialization function //
    VBJM_para_t(const List& paralist)
    {
        sig2 = as<arma::vec>(paralist["sig2"]);
        Sigma = as<arma::mat>(paralist["Sigma"]);
        gamma = as<arma::vec>(paralist["gamma"]);
        alpha = as<arma::vec>(paralist["alpha"]);
        weib = as<arma::vec>(paralist["weib"]);

        int K = sig2.n_elem;
        arma::field<arma::mat> V_tmp = paralist["V"];
        V = V_tmp;
        V_tmp.clear();
        int n = V.n_elem;
        Lvec = arma::field<arma::vec>(n);
        for(int i=0; i<n; i++){
            // Cholesky decomposition
            //arma::mat Ltmp = arma::chol(V(i),"lower");
            arma::mat Ltmp = myCholCpp(V(i));
            arma::uvec lower_indices = arma::trimatl_ind(arma::size(Ltmp));
            Lvec(i) = Ltmp(lower_indices);
        }

        arma::field<arma::vec> mu_tmp = paralist["mu"];
        mu = arma::field<arma::vec>(n,K);
        field_reshape_vec(mu_tmp, mu,  n, K);
        mu_tmp.clear();

        arma::field<arma::vec> beta_tmp = paralist["beta"];
        beta = beta_tmp;
        beta_tmp.clear();

        invSigma = myinvCpp(Sigma);

        npara_vec = arma::uvec(4, arma::fill::zeros);
        for(int k=0; k<beta.n_elem; k++){
            npara_vec(0) += beta(k).n_elem;
        }
        npara_vec(1) = gamma.n_elem;
        npara_vec(2) = alpha.n_elem;
        npara_vec(3) = weib.n_elem;
    }

    void updateInvSigma(){
        invSigma = myinvCpp(Sigma);
    }

};



// update variational parameters mu_i and V_i //
class updateMuVFun{
public:
    const VBJM_data_t& data;
    const VBJM_para_t& para;
    arma::field<arma::vec> Ytmp; // Y - X*beta
    arma::vec Z_ia_T;
    arma::mat Z_ia_t;
    arma::mat ZOZ;
    arma::vec h_t;
    int i_now=0;

    updateMuVFun(const VBJM_data_t& data,
                 const VBJM_para_t& para) :
        data(data), para(para){
        Ytmp = arma::field<arma::vec>(data.K);
    }

    void updateIntermediate(){
        for(int k=0; k<data.K; k++){
            Ytmp(k) = data.Y(i_now, k) - data.X(i_now,k)*para.beta(k);
        }

        Z_ia_T = field_to_alpha_vec(data.Z_T, para.alpha,
                                    i_now, data.p_z_vec);
        Z_ia_t = field_to_alpha_mat(data.Z_t, para.alpha,
                                    i_now, data.p_z_vec);

        double lambda = para.weib(0);
        double theta = para.weib(1);
        h_t = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i_now)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i_now) * para.gamma);

        ZOZ = para.invSigma;
        int start = 0;

        for(int k=0; k<data.K; k++){
            h_t += data.X_t(i_now,k)*para.beta(k) *para.alpha(k);
            ZOZ.submat(start, start,
                       start+data.p_z_vec(k)-1,
                       start+data.p_z_vec(k)-1) +=
                           data.Z(i_now,k).t()*data.Z(i_now,k)/para.sig2(k);

            start = start+data.p_z_vec(k);
        }

        // for(int j=0;j<h_t.size(); j++){
        //     h_t(j) +=  0.5 * arma::as_scalar(
        //         Z_ia_t.row(j) * para.V(i_now) * Z_ia_t.row(j).t()
        //     );
        // }

    }

    // Return the objective function with gradient.
    double EvaluateWithGradient(const arma::mat& muV, arma::mat& g)
    {
        arma::vec mu = muV(arma::span(0,data.p_z-1), 0);
        arma::vec Lvec = muV(arma::span(data.p_z, muV.n_rows-1), 0);

        arma::field<arma::vec> mu_f = vec_to_field(mu, data.p_z_vec);
        arma::mat L =  makeLowTriMat( para.V(i_now),  Lvec);
        arma::mat V = L*L.t();

        double val;
        double sign;


        /// fun value
        double fval = 0.0;
        arma::vec h_it = h_t + Z_ia_t*mu;
        for(int j=0;j<h_it.n_elem; j++){
            h_it(j) +=  0.5 * arma::as_scalar(
                Z_ia_t.row(j) * V * Z_ia_t.row(j).t()
            );
        }
        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        h_it = arma::exp(h_it);

        arma::log_det(val, sign, V);

        fval += -0.5*arma::trace(ZOZ*V) +  0.5 * val;

        arma::field<arma::vec> Ytmp2 = Ytmp;
        for(int k=0; k< Ytmp.n_elem; k++){
            Ytmp2(k) -= data.Z(i_now, k) * mu_f(k);
            fval += -0.5 * arma::accu(arma::square(Ytmp2(k)))/para.sig2(k);
        }
        fval += data.fstat(i_now)* arma::accu(Z_ia_T % mu) -
            arma::accu(data.GQ_w(i_now) % h_it) -
            0.5 * arma::as_scalar(mu.t() * para.invSigma *mu);


        /// gradient

        arma::mat grad_V = -1*ZOZ*L + arma::trans(arma::inv( arma::trimatl(L)));
        grad_V -= Z_ia_t.t()*arma::diagmat((data.GQ_w(i_now) % h_it)) *
            Z_ia_t * L;


        arma::vec grad_mu(mu.n_rows,arma::fill::zeros);
        int start = 0;
        for(int k=0; k<data.p_z_vec.n_elem; k++){
            grad_mu.subvec(start,start+data.p_z_vec(k)-1) =
                data.Z(i_now, k).t() * Ytmp2(k) / para.sig2(k);
            start = start+data.p_z_vec(k);
        }
        grad_mu += data.fstat(i_now)* Z_ia_T -
            Z_ia_t.t() * (data.GQ_w(i_now) % h_it) -
            para.invSigma * mu;

        fval = -1*fval;

        g(arma::span(0,data.p_z-1),0) = -grad_mu;

        g(arma::span(data.p_z,muV.n_rows-1),0) = -LowTriVec(grad_V);

        //Rcout << fval << "\n";

        return fval;
    }

};

// to put the new updates into para //
void storeMuV(const VBJM_data_t& data, VBJM_para_t& para,
              const arma::vec& mu,  const arma::vec& Lvec,
              const int& i){

    para.Lvec(i) = Lvec;
    arma::mat L =  makeLowTriMat( para.V(i),  Lvec);
    para.V(i) = L*L.t();


    arma::field<arma::vec> mu_f = vec_to_field(mu, data.p_z_vec);
    for(int k=0; k<data.K; k++){
        para.mu(i,k) = mu_f(k);
    }
}


// update MAP parameters mu_i for LP and QPL //
class updateMuFun{
public:
    const VBJM_data_t& data;
    const VBJM_para_t& para;
    arma::field<arma::vec> Ytmp; // Y - X*beta
    arma::vec Z_ia_T;
    arma::mat Z_ia_t;
    arma::mat ZOZ;
    arma::vec h_t;
    int i_now=0;

    updateMuFun(const VBJM_data_t& data,
                const VBJM_para_t& para) :
        data(data), para(para){
        Ytmp = arma::field<arma::vec>(data.K);
    }

    void updateIntermediate(){
        for(int k=0; k<data.K; k++){
            Ytmp(k) = data.Y(i_now, k) - data.X(i_now,k)*para.beta(k);
        }

        Z_ia_T = field_to_alpha_vec(data.Z_T, para.alpha,
                                    i_now, data.p_z_vec);
        Z_ia_t = field_to_alpha_mat(data.Z_t, para.alpha,
                                    i_now, data.p_z_vec);

        double lambda = para.weib(0);
        double theta = para.weib(1);
        h_t = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i_now)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i_now) * para.gamma);

        ZOZ = para.invSigma;
        int start = 0;

        for(int k=0; k<data.K; k++){
            h_t += data.X_t(i_now,k)*para.beta(k) *para.alpha(k);
            ZOZ.submat(start, start,
                       start+data.p_z_vec(k)-1,
                       start+data.p_z_vec(k)-1) +=
                           data.Z(i_now,k).t()*data.Z(i_now,k)/para.sig2(k);

            start = start+data.p_z_vec(k);
        }

        // for(int j=0;j<h_t.size(); j++){
        //     h_t(j) +=  0.5 * arma::as_scalar(
        //         Z_ia_t.row(j) * para.V(i_now) * Z_ia_t.row(j).t()
        //     );
        // }

    }

    // Return the objective function with gradient.
    double EvaluateWithGradient(const arma::mat& mu_new, arma::mat& g)
    {
        arma::vec mu = mu_new.col(0);
        //arma::vec Lvec = muV(arma::span(data.p_z, muV.n_rows-1), 0);

        arma::field<arma::vec> mu_f = vec_to_field(mu, data.p_z_vec);
        //arma::mat L =  makeLowTriMat( para.V(i_now),  Lvec);
        //arma::mat V = L*L.t();

        //double val;
        //double sign;


        /// fun value
        double fval = 0.0;
        arma::vec h_it = h_t + Z_ia_t*mu;
        // for(int j=0;j<h_it.n_elem; j++){
        //     h_it(j) +=  0.5 * arma::as_scalar(
        //         Z_ia_t.row(j) * V * Z_ia_t.row(j).t()
        //     );
        // }
        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        h_it = arma::exp(h_it);

        //arma::log_det(val, sign, V);

        //fval += -0.5*arma::trace(ZOZ*V) +  0.5 * val;

        arma::field<arma::vec> Ytmp2 = Ytmp;
        for(int k=0; k< Ytmp.n_elem; k++){
            Ytmp2(k) -= data.Z(i_now, k) * mu_f(k);
            fval += -0.5 * arma::accu(arma::square(Ytmp2(k)))/para.sig2(k);
        }
        fval += data.fstat(i_now)* arma::accu(Z_ia_T % mu) -
            arma::accu(data.GQ_w(i_now) % h_it) -
            0.5 * arma::as_scalar(mu.t() * para.invSigma *mu);


        /// gradient

        //arma::mat grad_V = -1*ZOZ*L + arma::trans(arma::inv( arma::trimatl(L)));
        //grad_V -= Z_ia_t.t()*arma::diagmat((data.GQ_w(i_now) % h_it)) *
         //   Z_ia_t * L;


        arma::vec grad_mu(mu.n_rows,arma::fill::zeros);
        int start = 0;
        for(int k=0; k<data.p_z_vec.n_elem; k++){
            grad_mu.subvec(start,start+data.p_z_vec(k)-1) =
                data.Z(i_now, k).t() * Ytmp2(k) / para.sig2(k);
            start = start+data.p_z_vec(k);
        }
        grad_mu += data.fstat(i_now)* Z_ia_T -
            Z_ia_t.t() * (data.GQ_w(i_now) % h_it) -
            para.invSigma * mu;

        fval = -1*fval;

        g.col(0) = -grad_mu;

        //g(arma::span(data.p_z,muV.n_rows-1),0) = -LowTriVec(grad_V);
        //Rcout << fval << "\n";

        return fval;
    }

};


// to put the new updates of MAP mu into para //
void storeMu(const VBJM_data_t& data, VBJM_para_t& para,
              const arma::vec& mu,
              const int& i){

    // para.Lvec(i) = Lvec;
    // arma::mat L =  makeLowTriMat( para.V(i),  Lvec);
    // para.V(i) = L*L.t();
    arma::field<arma::vec> mu_f = vec_to_field(mu, data.p_z_vec);
    for(int k=0; k<data.K; k++){
        para.mu(i,k) = mu_f(k);
    }
}


// update sig2 and Sigma //
// the same for LP algorithm //
// make sure the ''V'' is the inverse of V//
void updateSig(const VBJM_data_t& data,
               VBJM_para_t& para){

    arma::mat Sigma_tmp(arma::size(para.Sigma), arma::fill::zeros);
    arma::vec sig2_tmp(data.K, arma::fill::zeros);
    arma::uvec N(data.K, arma::fill::zeros);
    int start=0, end = 0;

    for(int i=0; i< data.n; i++){
        arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);
        Sigma_tmp += mu * mu.t() + para.V(i);
        start = 0;
        for(int k=0; k<data.K; k++){
            N(k) += data.Y(i,k).n_elem;
            end = start+data.p_z_vec(k)-1;
            sig2_tmp(k) +=  arma::accu(arma::square(
                data.Y(i,k) - data.X(i,k)*para.beta(k) - data.Z(i,k) * para.mu(i,k)
            ))+ arma::trace(
                    data.Z(i,k) * para.V(i).submat(start, start, end, end) * data.Z(i,k).t()
            );
            start = end + 1;
        }
    }

    sig2_tmp = sig2_tmp/N;
    Sigma_tmp /= data.n;

    para.sig2 = sig2_tmp;
    para.Sigma = Sigma_tmp;
    para.invSigma = myinvCpp(Sigma_tmp);
}

// update V for QPL //
void updateVPQL(const VBJM_data_t& data,
                VBJM_para_t& para){

    int start=0;// end = 0;
    double lambda = para.weib(0);
    double theta = para.weib(1);

    for(int i=0; i< data.n; i++){

        // update V
        arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i) * para.gamma);

        arma::mat Z_ia_t = field_to_alpha_mat(data.Z_t, para.alpha,
                                              i, data.p_z_vec);
        for(int k=0; k<data.K; k++){
            h_it += data.X_t(i,k)*para.beta(k)*para.alpha(k) +
                data.Z_t(i,k)*para.mu(i,k)*para.alpha(k);
        }


        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        h_it = arma::exp(h_it);

        arma::mat ZOZ = para.invSigma;
        start = 0;
        for(int k=0; k<data.K; k++){
            ZOZ.submat(start, start,
                       start+data.p_z_vec(k)-1,
                       start+data.p_z_vec(k)-1) +=
                           data.Z(i,k).t()*data.Z(i,k)/para.sig2(k);
            start = start+data.p_z_vec(k);
        }
        para.V(i) = ZOZ + Z_ia_t.t()*arma::diagmat((data.GQ_w(i) % h_it)) * Z_ia_t;
        para.V(i) = myinvCpp(para.V(i));
    }
}


// update sig2 and Sigma for QPL //
void updateSigQPL(const VBJM_data_t& data,
                  VBJM_para_t& para){

    arma::mat Sigma_tmp(arma::size(para.Sigma), arma::fill::zeros);
    arma::vec sig2_tmp(data.K, arma::fill::zeros);
    arma::uvec N(data.K, arma::fill::zeros);
    int start=0, end = 0;
    //double lambda = para.weib(0);
    //double theta = para.weib(1);

    updateVPQL(data,  para);
    //Rcout << para.sig2(0) << "\n";
    //update sig2
    for(int i=0; i< data.n; i++){

        // update sig2 and Sigma
        //arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);
        //Sigma_tmp += mu * mu.t() + para.V(i);
        //Sigma_tmp += mu * mu.t();
        start = 0;
        for(int k=0; k<data.K; k++){
            N(k) += data.Y(i,k).n_elem;
            end = start+data.p_z_vec(k)-1;
            sig2_tmp(k) +=  arma::accu(arma::square(
                data.Y(i,k) - data.X(i,k)*para.beta(k) - data.Z(i,k) * para.mu(i,k)
            )) +  arma::trace(
                     data.Z(i,k) * para.V(i).submat(start, start, end, end) * data.Z(i,k).t()
             );
            start = end + 1;
        }
    }
    sig2_tmp = sig2_tmp/N;
    para.sig2 = sig2_tmp;

    //Rcout << para.sig2(0) << "\n";

    //updateVPQL(data,  para);

    // update  Sigma
    for(int i=0; i< data.n; i++){
        // update Sigma
        arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);
        Sigma_tmp += mu * mu.t() + para.V(i);
        //Sigma_tmp += mu * mu.t();
    }


    Sigma_tmp /= data.n;
    para.Sigma = Sigma_tmp;
    para.invSigma = myinvCpp(Sigma_tmp);

    //Rcout << para.Sigma.max() << "\n";
    //Rcout << para.invSigma.max() << "\n";
}

// combine all parameters into vector //
// beta, gamma, alpha, weib //
arma::vec combinePara(const VBJM_data_t& data,
                      const VBJM_para_t& para){

    // create beta vector
    arma::vec beta_vec = field_to_vec(para.beta, data.p_x_vec);

    // create field of parameter vector
    arma::field<arma::vec> para_field(para.npara_vec.n_elem);
    para_field(0) = beta_vec;
    para_field(1) = para.gamma;
    para_field(2) = para.alpha;
    // work on log-scale of weibull parameter to ensure nonnegativitity
    // remember to exp it after optimization //
    para_field(3) = arma::log(para.weib);

    // concatenate all parameter into one vector
    arma::vec para_all = field_to_vec(para_field, para.npara_vec);

    return para_all;
}

// to put the new updates into para //
void storePara(const arma::vec& para_all, const VBJM_data_t& data,
               VBJM_para_t& para){

    arma::field<arma::vec> para_field = vec_to_field(para_all, para.npara_vec);
    arma::field<arma::vec> beta = vec_to_field(para_field(0), data.p_x_vec);
    para.beta = beta;
    para.gamma = para_field(1);
    para.alpha = para_field(2);
    para_field(3) = arma::clamp(para_field(3), -MAX_EXP, MAX_EXP);
    para.weib = arma::exp(para_field(3));
}

// update parameters other than sig2 and Sigma//

class updateParaFun{
public:
    const VBJM_data_t& data;
    const VBJM_para_t& para;

    updateParaFun(const VBJM_data_t& data,
                  const VBJM_para_t& para):
        data(data), para(para){
    }

    void updateIntermediate(){

    }

    // Return the objective function with gradient.
    double EvaluateWithGradient(const arma::mat& para_all, arma::mat& g)
    {
        arma::field<arma::vec> para_field = vec_to_field(para_all, para.npara_vec);

        arma::field<arma::vec> beta = vec_to_field(para_field(0), data.p_x_vec);
        arma::vec gamma = para_field(1);
        arma::vec alpha = para_field(2);
        arma::vec weib = arma::clamp(para_field(3), -MAX_EXP, MAX_EXP);
        //double c = weib(0);
        double d = weib(1);
        weib = arma::exp(weib);
        double lambda = weib(0);
        double theta = weib(1);

        arma::vec ELBO(data.n, arma::fill::zeros);
        arma::mat grad_beta(para.npara_vec(0),data.n, arma::fill::zeros);
        arma::mat grad_gamma(para.npara_vec(1),data.n, arma::fill::zeros);
        arma::mat grad_alpha(para.npara_vec(2),data.n, arma::fill::zeros);
        arma::mat grad_weib(para.npara_vec(3),data.n, arma::fill::zeros);

        for(int i=0; i< data.n; i++){

            // calculate function value
            //Rcout << "111\n";
            int start = 0, end=0;
            for(int k=0; k<data.K; k++){
                end = start+data.p_z_vec(k)-1;
                ELBO(i) -= 0.5*arma::accu(arma::square(
                    data.Y(i,k) - data.X(i,k)*beta(k) - data.Z(i,k) * para.mu(i,k)
                ))/para.sig2(k);
                start = end + 1;
            }
            //Rcout << "112\n";

            if(data.fstat(i) == 1){
                ELBO(i) += std::log(lambda) + (lambda-1)*std::log(data.ftime(i)) -
                    lambda*std::log(theta) +
                    arma::as_scalar(data.W.row(i) * gamma);

                for(int k=0; k<data.K; k++){
                    ELBO(i) += arma::accu(data.X_T(i,k)%beta(k)) *alpha(k) +
                        arma::accu(data.Z_T(i,k)%para.mu(i,k)) *alpha(k);
                }
            }

            //Rcout << "113\n";
            arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * gamma);

            arma::mat Z_ia_t = field_to_alpha_mat(data.Z_t, alpha,
                                                  i, data.p_z_vec);
            //Rcout << "114\n";
            for(int k=0; k<data.K; k++){
                h_it += data.X_t(i,k)*beta(k)*alpha(k) +
                    data.Z_t(i,k)*para.mu(i,k)*alpha(k);
            }

            for(int j=0; j< h_it.n_elem; j++){
                h_it(j) += 0.5 * arma::as_scalar(
                    Z_ia_t.row(j) * para.V(i) * Z_ia_t.row(j).t()
                );
            }

            h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
            h_it = arma::exp(h_it);

            ELBO(i) -=  arma::accu(data.GQ_w(i) % h_it);

            //Rcout << "115\n";

            // gradient of beta //
            arma::vec grad_beta_tmp(data.p_x, arma::fill::zeros);
            start = 0; end=0;
            for(int k=0; k<data.K; k++){
                end = start+data.p_x_vec(k)-1;
                grad_beta_tmp.subvec(start, end) = data.X(i,k).t() * (
                    data.Y(i,k) - data.X(i,k)*beta(k) - data.Z(i,k) * para.mu(i,k)
                )/para.sig2(k);
                if(data.fstat(i)==1){
                    grad_beta_tmp.subvec(start, end) += alpha(k) * data.X_T(i,k);
                }
                grad_beta_tmp.subvec(start, end) -=
                    data.X_t(i,k).t() * (data.GQ_w(i) % h_it) * alpha(k);
                start = end + 1;
            }

            grad_beta.col(i) = grad_beta_tmp;

            //Rcout << "116\n";
            // gradient of gamma //
            grad_gamma.col(i) =  (data.fstat(i) -
                arma::accu(data.GQ_w(i) % h_it)) *
                data.W.row(i).t();

            //Rcout << "117\n";
            // gradient of alpha //
            arma::vec grad_alpha_tmp(alpha.n_elem, arma::fill::zeros);
            start = 0; end=0;
            for(int k=0; k < alpha.n_elem;k++){
                end = start+data.p_z_vec(k)-1;
                if(data.fstat(i)==1){
                    grad_alpha_tmp(k) += arma::accu(data.X_T(i,k)%beta(k))+
                        arma::accu(data.Z_T(i,k)%para.mu(i,k));
                }
                arma::vec XBZmu = data.X_t(i,k)*beta(k) +
                    data.Z_t(i,k)*para.mu(i,k);

                for(int j=0; j< XBZmu.n_elem; j++){
                    XBZmu(j) +=  arma::as_scalar(
                        data.Z_t(i,k).row(j) * para.V(i).rows(start,end) *
                            Z_ia_t.row(j).t()
                    );
                }

                grad_alpha_tmp(k) -= arma::accu(data.GQ_w(i) % h_it % XBZmu);
                start = end + 1;
            }

            grad_alpha.col(i) = grad_alpha_tmp;

            //Rcout << "118\n";
            // gradient of weibull parameters w.r.t. c and d //
            double grad_c=0, grad_d=0;
            if(data.fstat(i)==1){
                grad_c = 1 + lambda*(std::log(data.ftime(i)) -d);
                grad_d = - lambda;
            }
            grad_c -= arma::accu(
                (1+lambda*(arma::log(data.GQ_t(i)) - d)) % data.GQ_w(i) % h_it
            );
            grad_d += lambda * arma::accu(data.GQ_w(i) % h_it);
            grad_weib(0,i) = grad_c;
            grad_weib(1,i) = grad_d;
        }

        double fval= -1*arma::accu(ELBO)/data.n;

        arma::field<arma::vec> grad_field(4);
        grad_field(0) = arma::sum(grad_beta,1)/data.n;
        grad_field(1) = arma::sum(grad_gamma,1)/data.n;
        grad_field(2) = arma::sum(grad_alpha,1)/data.n;
        grad_field(3) = arma::sum(grad_weib,1)/data.n;

        g.col(0) = -1* field_to_vec(grad_field, para.npara_vec);

        return fval;
    }

};



// update parameters other than sig2 and Sigma//
// for Laplace approx //
class updateParaFunLP{
public:
    const VBJM_data_t& data;
    VBJM_para_t& para;

    updateParaFunLP(const VBJM_data_t& data,
                    VBJM_para_t& para):
        data(data), para(para){
    }

    void updateIntermediate(){

    }

    // Return the objective function with gradient.
    double EvaluateWithGradient(const arma::mat& para_all, arma::mat& g)
    {
        arma::field<arma::vec> para_field = vec_to_field(para_all, para.npara_vec);

        arma::field<arma::vec> beta = vec_to_field(para_field(0), data.p_x_vec);
        arma::vec gamma = para_field(1);
        arma::vec alpha = para_field(2);
        arma::vec weib = arma::clamp(para_field(3), -MAX_EXP, MAX_EXP);
        //double c = weib(0);
        double d = weib(1);
        weib = arma::exp(weib);
        double lambda = weib(0);
        double theta = weib(1);

        arma::vec ELBO(data.n, arma::fill::zeros);
        arma::mat grad_beta(para.npara_vec(0),data.n, arma::fill::zeros);
        arma::mat grad_gamma(para.npara_vec(1),data.n, arma::fill::zeros);
        arma::mat grad_alpha(para.npara_vec(2),data.n, arma::fill::zeros);
        arma::mat grad_weib(para.npara_vec(3),data.n, arma::fill::zeros);

        double val,sign;

        for(int i=0; i< data.n; i++){

            // calculate function value
            //Rcout << "111\n";
            int start = 0, end=0;
            for(int k=0; k<data.K; k++){
                end = start+data.p_z_vec(k)-1;
                ELBO(i) -= 0.5*arma::accu(arma::square(
                    data.Y(i,k) - data.X(i,k)*beta(k) - data.Z(i,k) * para.mu(i,k)
                ))/para.sig2(k);
                start = end + 1;
            }
            //Rcout << "112\n";

            if(data.fstat(i) == 1){
                ELBO(i) += std::log(lambda) + (lambda-1)*std::log(data.ftime(i)) -
                    lambda*std::log(theta) +
                    arma::as_scalar(data.W.row(i) * gamma);

                for(int k=0; k<data.K; k++){
                    ELBO(i) += arma::accu(data.X_T(i,k)%beta(k)) *alpha(k) +
                        arma::accu(data.Z_T(i,k)%para.mu(i,k)) *alpha(k);
                }
            }

            //Rcout << "113\n";
            arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * gamma);

            arma::mat Z_ia_t = field_to_alpha_mat(data.Z_t, alpha,
                                                  i, data.p_z_vec);
            //Rcout << "114\n";
            for(int k=0; k<data.K; k++){
                h_it += data.X_t(i,k)*beta(k)*alpha(k) +
                    data.Z_t(i,k)*para.mu(i,k)*alpha(k);
            }

            // for(int j=0; j< h_it.n_elem; j++){
            //     h_it(j) += 0.5 * arma::as_scalar(
            //         Z_ia_t.row(j) * para.V(i) * Z_ia_t.row(j).t()
            //     );
            // }

            h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
            h_it = arma::exp(h_it);

            ELBO(i) -=  arma::accu(data.GQ_w(i) % h_it);

            ///////////////////////////////////////////////////
            ///// additional log-det term of LP algorithm /////
            arma::mat ZOZ = para.invSigma;
            start = 0;
            for(int k=0; k<data.K; k++){
                ZOZ.submat(start, start,
                           start+data.p_z_vec(k)-1,
                           start+data.p_z_vec(k)-1) +=
                               data.Z(i,k).t()*data.Z(i,k)/para.sig2(k);
                start = start+data.p_z_vec(k);
            }
            para.V(i) = ZOZ + Z_ia_t.t()*arma::diagmat((data.GQ_w(i) % h_it)) * Z_ia_t;
            para.V(i) = myinvCpp(para.V(i));

            arma::log_det(val, sign, para.V(i));

            ELBO(i) += 0.5 * val;
            //////////////////////////////////////////////////
            arma::vec tr_VZZ = arma::mat(Z_ia_t * para.V(i) * Z_ia_t.t()).diag();

            //Rcout << "115\n";

            // gradient of beta //
            arma::vec grad_beta_tmp(data.p_x, arma::fill::zeros);
            start = 0; end=0;
            for(int k=0; k<data.K; k++){
                end = start+data.p_x_vec(k)-1;
                grad_beta_tmp.subvec(start, end) = data.X(i,k).t() * (
                    data.Y(i,k) - data.X(i,k)*beta(k) - data.Z(i,k) * para.mu(i,k)
                )/para.sig2(k);
                if(data.fstat(i)==1){
                    grad_beta_tmp.subvec(start, end) += alpha(k) * data.X_T(i,k);
                }
                grad_beta_tmp.subvec(start, end) -=
                    data.X_t(i,k).t() * (data.GQ_w(i) % h_it) * alpha(k);

                ////// additional /////
                grad_beta_tmp.subvec(start, end) -=
                    0.5* data.X_t(i,k).t() * (data.GQ_w(i) % h_it % tr_VZZ) * alpha(k);
                ///////////////////////
                start = end + 1;
            }

            grad_beta.col(i) = grad_beta_tmp;

            //Rcout << "116\n";
            // gradient of gamma //
            grad_gamma.col(i) =  (data.fstat(i) -
                arma::accu(data.GQ_w(i) % h_it)) *
                data.W.row(i).t();

            ////// additional /////
            grad_gamma.col(i) -=
                0.5* arma::accu(data.GQ_w(i) % h_it % tr_VZZ) * data.W.row(i).t();
            ///////////////////////


            //Rcout << "117\n";
            // gradient of alpha //
            arma::vec grad_alpha_tmp(alpha.n_elem, arma::fill::zeros);
            start = 0; end=0;
            for(int k=0; k < alpha.n_elem;k++){
                end = start+data.p_z_vec(k)-1;
                if(data.fstat(i)==1){
                    grad_alpha_tmp(k) += arma::accu(data.X_T(i,k)%beta(k))+
                        arma::accu(data.Z_T(i,k)%para.mu(i,k));
                }
                arma::vec XBZmu = data.X_t(i,k)*beta(k) +
                    data.Z_t(i,k)*para.mu(i,k);

                // for(int j=0; j< XBZmu.n_elem; j++){
                //     XBZmu(j) +=  arma::as_scalar(
                //         data.Z_t(i,k).row(j) * para.V(i).rows(start,end) *
                //             Z_ia_t.row(j).t()
                //     );
                // }

                grad_alpha_tmp(k) -= arma::accu(data.GQ_w(i) % h_it % XBZmu);

                ////// additional /////
                grad_alpha_tmp(k) -= 0.5 * arma::accu(data.GQ_w(i) % h_it % XBZmu % tr_VZZ);

                for(int j=0; j< XBZmu.n_elem; j++){
                    XBZmu(j) =  arma::as_scalar(
                        data.Z_t(i,k).row(j) * para.V(i).rows(start,end) *
                            Z_ia_t.row(j).t()
                    );
                }
                grad_alpha_tmp(k) -= arma::accu(data.GQ_w(i) % h_it % XBZmu);
                ///////////////////////

                start = end + 1;
            }

            grad_alpha.col(i) = grad_alpha_tmp;

            //Rcout << "118\n";
            // gradient of weibull parameters w.r.t. c and d //
            double grad_c=0, grad_d=0;
            if(data.fstat(i)==1){
                grad_c = 1 + lambda*(std::log(data.ftime(i)) -d);
                grad_d = - lambda;
            }
            grad_c -= arma::accu(
                (1+lambda*(arma::log(data.GQ_t(i)) - d)) % data.GQ_w(i) % h_it
            );
            grad_d += lambda * arma::accu(data.GQ_w(i) % h_it);

            ////// additional /////
            grad_c -= 0.5 * arma::accu(
                (1+lambda*(arma::log(data.GQ_t(i)) - d)) % data.GQ_w(i) % h_it % tr_VZZ
            );
            grad_d += 0.5 * lambda * arma::accu(data.GQ_w(i) % h_it % tr_VZZ);
            ///////////////////////

            grad_weib(0,i) = grad_c;
            grad_weib(1,i) = grad_d;
        }

        double fval= -1*arma::accu(ELBO)/data.n;

        arma::field<arma::vec> grad_field(4);
        grad_field(0) = arma::sum(grad_beta,1)/data.n;
        grad_field(1) = arma::sum(grad_gamma,1)/data.n;
        grad_field(2) = arma::sum(grad_alpha,1)/data.n;
        grad_field(3) = arma::sum(grad_weib,1)/data.n;

        g.col(0) = -1* field_to_vec(grad_field, para.npara_vec);

        return fval;
    }

};

// update parameters other than sig2 and Sigma//
// for Quasi-Penalized Likelihood approx //
class updateParaFunQPL{
public:
    const VBJM_data_t& data;
    VBJM_para_t& para;

    updateParaFunQPL(const VBJM_data_t& data,
                     VBJM_para_t& para):
        data(data), para(para){
    }

    void updateIntermediate(){

    }

    // Return the objective function with gradient.
    double EvaluateWithGradient(const arma::mat& para_all, arma::mat& g)
    {
        arma::field<arma::vec> para_field = vec_to_field(para_all, para.npara_vec);

        arma::field<arma::vec> beta = vec_to_field(para_field(0), data.p_x_vec);
        arma::vec gamma = para_field(1);
        arma::vec alpha = para_field(2);
        arma::vec weib = arma::clamp(para_field(3), -MAX_EXP, MAX_EXP);
        //double c = weib(0);
        double d = weib(1);
        weib = arma::exp(weib);
        double lambda = weib(0);
        double theta = weib(1);

        arma::vec ELBO(data.n, arma::fill::zeros);
        arma::mat grad_beta(para.npara_vec(0),data.n, arma::fill::zeros);
        arma::mat grad_gamma(para.npara_vec(1),data.n, arma::fill::zeros);
        arma::mat grad_alpha(para.npara_vec(2),data.n, arma::fill::zeros);
        arma::mat grad_weib(para.npara_vec(3),data.n, arma::fill::zeros);

        // double val,sign;

        for(int i=0; i< data.n; i++){

            // calculate function value
            //Rcout << "111\n";
            int start = 0, end=0;
            for(int k=0; k<data.K; k++){
                end = start+data.p_z_vec(k)-1;
                ELBO(i) -= 0.5*arma::accu(arma::square(
                    data.Y(i,k) - data.X(i,k)*beta(k) - data.Z(i,k) * para.mu(i,k)
                ))/para.sig2(k);
                start = end + 1;
            }
            //Rcout << "112\n";

            if(data.fstat(i) == 1){
                ELBO(i) += std::log(lambda) + (lambda-1)*std::log(data.ftime(i)) -
                    lambda*std::log(theta) +
                    arma::as_scalar(data.W.row(i) * gamma);

                for(int k=0; k<data.K; k++){
                    ELBO(i) += arma::accu(data.X_T(i,k)%beta(k)) *alpha(k) +
                        arma::accu(data.Z_T(i,k)%para.mu(i,k)) *alpha(k);
                }
            }

            //Rcout << "113\n";
            arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * gamma);

            arma::mat Z_ia_t = field_to_alpha_mat(data.Z_t, alpha,
                                                  i, data.p_z_vec);
            //Rcout << "114\n";
            for(int k=0; k<data.K; k++){
                h_it += data.X_t(i,k)*beta(k)*alpha(k) +
                    data.Z_t(i,k)*para.mu(i,k)*alpha(k);
            }

            // for(int j=0; j< h_it.n_elem; j++){
            //     h_it(j) += 0.5 * arma::as_scalar(
            //         Z_ia_t.row(j) * para.V(i) * Z_ia_t.row(j).t()
            //     );
            // }

            h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
            h_it = arma::exp(h_it);

            ELBO(i) -=  arma::accu(data.GQ_w(i) % h_it);

            ///////////////////////////////////////////////////
            ///// additional log-det term of LP algorithm /////
            // arma::mat ZOZ = para.invSigma;
            // start = 0;
            // for(int k=0; k<data.K; k++){
            //     ZOZ.submat(start, start,
            //                start+data.p_z_vec(k)-1,
            //                start+data.p_z_vec(k)-1) +=
            //                    data.Z(i,k).t()*data.Z(i,k)/para.sig2(k);
            //     start = start+data.p_z_vec(k);
            // }
            // para.V(i) = ZOZ + Z_ia_t.t()*arma::diagmat((data.GQ_w(i) % h_it)) * Z_ia_t;
            // para.V(i) = myinvCpp(para.V(i));
            //
            // arma::log_det(val, sign, para.V(i));
            //
            // ELBO(i) += 0.5 * val;
            //////////////////////////////////////////////////
            // arma::vec tr_VZZ = arma::mat(Z_ia_t * para.V(i) * Z_ia_t.t()).diag();

            //Rcout << "115\n";

            // gradient of beta //
            arma::vec grad_beta_tmp(data.p_x, arma::fill::zeros);
            start = 0; end=0;
            for(int k=0; k<data.K; k++){
                end = start+data.p_x_vec(k)-1;
                grad_beta_tmp.subvec(start, end) = data.X(i,k).t() * (
                    data.Y(i,k) - data.X(i,k)*beta(k) - data.Z(i,k) * para.mu(i,k)
                )/para.sig2(k);
                if(data.fstat(i)==1){
                    grad_beta_tmp.subvec(start, end) += alpha(k) * data.X_T(i,k);
                }
                grad_beta_tmp.subvec(start, end) -=
                    data.X_t(i,k).t() * (data.GQ_w(i) % h_it) * alpha(k);

                ////// additional /////
                // grad_beta_tmp.subvec(start, end) -=
                //     0.5* data.X_t(i,k).t() * (data.GQ_w(i) % h_it % tr_VZZ) * alpha(k);
                // ///////////////////////
                start = end + 1;
            }

            grad_beta.col(i) = grad_beta_tmp;

            //Rcout << "116\n";
            // gradient of gamma //
            grad_gamma.col(i) =  (data.fstat(i) -
                arma::accu(data.GQ_w(i) % h_it)) *
                data.W.row(i).t();

            ////// additional /////
            // grad_gamma.col(i) -=
            //     0.5* arma::accu(data.GQ_w(i) % h_it % tr_VZZ) * data.W.row(i).t();
            ///////////////////////


            //Rcout << "117\n";
            // gradient of alpha //
            arma::vec grad_alpha_tmp(alpha.n_elem, arma::fill::zeros);
            start = 0; end=0;
            for(int k=0; k < alpha.n_elem;k++){
                end = start+data.p_z_vec(k)-1;
                if(data.fstat(i)==1){
                    grad_alpha_tmp(k) += arma::accu(data.X_T(i,k)%beta(k))+
                        arma::accu(data.Z_T(i,k)%para.mu(i,k));
                }
                arma::vec XBZmu = data.X_t(i,k)*beta(k) +
                    data.Z_t(i,k)*para.mu(i,k);

                // for(int j=0; j< XBZmu.n_elem; j++){
                //     XBZmu(j) +=  arma::as_scalar(
                //         data.Z_t(i,k).row(j) * para.V(i).rows(start,end) *
                //             Z_ia_t.row(j).t()
                //     );
                // }

                grad_alpha_tmp(k) -= arma::accu(data.GQ_w(i) % h_it % XBZmu);

                ////// additional /////
                // grad_alpha_tmp(k) -= 0.5 * arma::accu(data.GQ_w(i) % h_it % XBZmu % tr_VZZ);
                //
                // for(int j=0; j< XBZmu.n_elem; j++){
                //     XBZmu(j) =  arma::as_scalar(
                //         data.Z_t(i,k).row(j) * para.V(i).rows(start,end) *
                //             Z_ia_t.row(j).t()
                //     );
                // }
                // grad_alpha_tmp(k) -= arma::accu(data.GQ_w(i) % h_it % XBZmu);
                ///////////////////////

                start = end + 1;
            }

            grad_alpha.col(i) = grad_alpha_tmp;

            //Rcout << "118\n";
            // gradient of weibull parameters w.r.t. c and d //
            double grad_c=0, grad_d=0;
            if(data.fstat(i)==1){
                grad_c = 1 + lambda*(std::log(data.ftime(i)) -d);
                grad_d = - lambda;
            }
            grad_c -= arma::accu(
                (1+lambda*(arma::log(data.GQ_t(i)) - d)) % data.GQ_w(i) % h_it
            );
            grad_d += lambda * arma::accu(data.GQ_w(i) % h_it);

            ////// additional /////
            // grad_c -= 0.5 * arma::accu(
            //     (1+lambda*(arma::log(data.GQ_t(i)) - d)) % data.GQ_w(i) % h_it % tr_VZZ
            // );
            // grad_d += 0.5 * lambda * arma::accu(data.GQ_w(i) % h_it % tr_VZZ);
            ///////////////////////

            grad_weib(0,i) = grad_c;
            grad_weib(1,i) = grad_d;
        }

        double fval= -1*arma::accu(ELBO)/data.n;

        arma::field<arma::vec> grad_field(4);
        grad_field(0) = arma::sum(grad_beta,1)/data.n;
        grad_field(1) = arma::sum(grad_gamma,1)/data.n;
        grad_field(2) = arma::sum(grad_alpha,1)/data.n;
        grad_field(3) = arma::sum(grad_weib,1)/data.n;

        g.col(0) = -1* field_to_vec(grad_field, para.npara_vec);

        return fval;
    }

};


// calculate ELBO //
double calcELBO(const VBJM_data_t& data,
                const VBJM_para_t& para){

    //double ELBO=0;
    arma::vec ELBO(data.n,arma::fill::zeros);

    const double lambda = para.weib(0);
    const double theta = para.weib(1);
    arma::vec logsig2 = arma::log(para.sig2);

    for(int i=0; i< data.n; i++){

        //int n_i = data.Y(i,0).n_elem;
        //ELBO(i) -= 0.5 * arma::accu(arma::log(para.sig2))*n_i;

        int start = 0, end=0;
        for(int k=0; k<data.K; k++){
            ELBO(i) -= 0.5 * logsig2(k) * data.Y(i,k).n_elem;

            end = start+data.p_z_vec(k)-1;
            ELBO(i) -= 0.5*arma::accu(arma::square(
                data.Y(i,k) - data.X(i,k)*para.beta(k) - data.Z(i,k) * para.mu(i,k)
            ))/para.sig2(k) +  0.5*arma::trace(
                    data.Z(i,k) * para.V(i).submat(start, start, end, end) * data.Z(i,k).t()
            )/para.sig2(k);
            start = end + 1;
        }

        if(data.fstat(i) == 1){
            ELBO(i) += std::log(lambda) + (lambda-1)*std::log(data.ftime(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * para.gamma);

            for(int k=0; k<data.K; k++){
                ELBO(i) += arma::accu(data.X_T(i,k)%para.beta(k)) *para.alpha(k) +
                    arma::accu(data.Z_T(i,k)%para.mu(i,k)) *para.alpha(k);
            }
        }

        arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i) * para.gamma);

        arma::mat Z_ia_t = field_to_alpha_mat(data.Z_t, para.alpha,
                                              i, data.p_z_vec);

        for(int k=0; k<data.K; k++){
            h_it += data.X_t(i,k)*para.beta(k)*para.alpha(k) +
                data.Z_t(i,k)*para.mu(i,k)*para.alpha(k);

        }

        for(int j=0; j< h_it.n_elem; j++){
            h_it(j) += 0.5 * arma::as_scalar(
                Z_ia_t.row(j) * para.V(i) * Z_ia_t.row(j).t()
            );
        }

        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        h_it = arma::exp(h_it);

        ELBO(i) -=  arma::accu(data.GQ_w(i) % h_it);

        double val, sign;
        arma::log_det(val, sign, para.Sigma);
        ELBO(i) -= 0.5 * val;

        arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);

        ELBO(i) -= 0.5 * arma::as_scalar(mu.t() * para.invSigma *mu);

        ELBO(i) -= 0.5 * arma::trace(para.invSigma * para.V(i));

        arma::log_det(val, sign, para.V(i));
        ELBO(i) += 0.5 * val;

    }

    return arma::accu(ELBO);
}



// calculate Log-likelihhod //
double calcLogLP(const VBJM_data_t& data,
                 const VBJM_para_t& para){

    //double ELBO=0;
    arma::vec ELBO(data.n,arma::fill::zeros);

    const double lambda = para.weib(0);
    const double theta = para.weib(1);
    arma::vec logsig2 = arma::log(para.sig2);

    for(int i=0; i< data.n; i++){

        //int n_i = data.Y(i,0).n_elem;
        //ELBO(i) -= 0.5 * arma::accu(arma::log(para.sig2))*n_i;

        int start = 0, end=0;
        for(int k=0; k<data.K; k++){
            ELBO(i) -= 0.5 * logsig2(k) * data.Y(i,k).n_elem;

            end = start+data.p_z_vec(k)-1;
            ELBO(i) -= 0.5*arma::accu(arma::square(
                data.Y(i,k) - data.X(i,k)*para.beta(k) - data.Z(i,k) * para.mu(i,k)
            ))/para.sig2(k) ;

            // +  0.5*arma::trace(
            //         data.Z(i,k) * para.V(i).submat(start, start, end, end) * data.Z(i,k).t()
            // )/para.sig2(k);
            start = end + 1;
        }

        if(data.fstat(i) == 1){
            ELBO(i) += std::log(lambda) + (lambda-1)*std::log(data.ftime(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * para.gamma);

            for(int k=0; k<data.K; k++){
                ELBO(i) += arma::accu(data.X_T(i,k)%para.beta(k)) *para.alpha(k) +
                    arma::accu(data.Z_T(i,k)%para.mu(i,k)) *para.alpha(k);
            }
        }

        arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i) * para.gamma);

        arma::mat Z_ia_t = field_to_alpha_mat(data.Z_t, para.alpha,
                                              i, data.p_z_vec);

        for(int k=0; k<data.K; k++){
            h_it += data.X_t(i,k)*para.beta(k)*para.alpha(k) +
                data.Z_t(i,k)*para.mu(i,k)*para.alpha(k);

        }

        // for(int j=0; j< h_it.n_elem; j++){
        //     h_it(j) += 0.5 * arma::as_scalar(
        //         Z_ia_t.row(j) * para.V(i) * Z_ia_t.row(j).t()
        //     );
        // }

        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        h_it = arma::exp(h_it);

        ELBO(i) -=  arma::accu(data.GQ_w(i) % h_it);

        double val, sign;
        arma::log_det(val, sign, para.Sigma);
        ELBO(i) -= 0.5 * val;

        arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);

        ELBO(i) -= 0.5 * arma::as_scalar(mu.t() * para.invSigma *mu);

        //ELBO(i) -= 0.5 * arma::trace(para.invSigma * para.V(i));

        arma::log_det(val, sign, para.V(i));
        ELBO(i) += 0.5 * val;

    }

    return arma::accu(ELBO);
}


// calculate Log-likelihhod //
double calcLogQPL(const VBJM_data_t& data,
                 const VBJM_para_t& para){

    //double ELBO=0;
    arma::vec ELBO(data.n,arma::fill::zeros);

    const double lambda = para.weib(0);
    const double theta = para.weib(1);
    arma::vec logsig2 = arma::log(para.sig2);

    for(int i=0; i< data.n; i++){

        //int n_i = data.Y(i,0).n_elem;
        //ELBO(i) -= 0.5 * arma::accu(arma::log(para.sig2))*n_i;

        int start = 0, end=0;
        for(int k=0; k<data.K; k++){
            ELBO(i) -= 0.5 * logsig2(k) * data.Y(i,k).n_elem;

            end = start+data.p_z_vec(k)-1;
            ELBO(i) -= 0.5*arma::accu(arma::square(
                data.Y(i,k) - data.X(i,k)*para.beta(k) - data.Z(i,k) * para.mu(i,k)
            ))/para.sig2(k) ;

            // +  0.5*arma::trace(
            //         data.Z(i,k) * para.V(i).submat(start, start, end, end) * data.Z(i,k).t()
            // )/para.sig2(k);
            start = end + 1;
        }

        if(data.fstat(i) == 1){
            ELBO(i) += std::log(lambda) + (lambda-1)*std::log(data.ftime(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * para.gamma);

            for(int k=0; k<data.K; k++){
                ELBO(i) += arma::accu(data.X_T(i,k)%para.beta(k)) *para.alpha(k) +
                    arma::accu(data.Z_T(i,k)%para.mu(i,k)) *para.alpha(k);
            }
        }

        arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i) * para.gamma);

        arma::mat Z_ia_t = field_to_alpha_mat(data.Z_t, para.alpha,
                                              i, data.p_z_vec);

        for(int k=0; k<data.K; k++){
            h_it += data.X_t(i,k)*para.beta(k)*para.alpha(k) +
                data.Z_t(i,k)*para.mu(i,k)*para.alpha(k);

        }

        // for(int j=0; j< h_it.n_elem; j++){
        //     h_it(j) += 0.5 * arma::as_scalar(
        //         Z_ia_t.row(j) * para.V(i) * Z_ia_t.row(j).t()
        //     );
        // }

        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        h_it = arma::exp(h_it);

        ELBO(i) -=  arma::accu(data.GQ_w(i) % h_it);

        double val, sign;
        arma::log_det(val, sign, para.Sigma);
        ELBO(i) -= 0.5 * val;

        arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);

        ELBO(i) -= 0.5 * arma::as_scalar(mu.t() * para.invSigma *mu);

        //ELBO(i) -= 0.5 * arma::trace(para.invSigma * para.V(i));

        // arma::log_det(val, sign, para.V(i));
        // ELBO(i) += 0.5 * val;

    }

    return arma::accu(ELBO);
}


// calculate hessian //
// D2(log(det|Sigma^-1|))(vech(Sigma^-1))
arma::mat D2logdet_Sigma(const arma::mat& Sigma){
    //// hessian on vech(invSigma) ////
    ///  needs info on Sigma ////
    int start = 0, p_mu = Sigma.n_cols,
        p_Sigma = (p_mu+1)*p_mu/2;

    arma::mat D_Sigma_Sigma(p_Sigma, p_Sigma, arma::fill::zeros);

    for(int j=0; j<p_mu; j++){ // column
        for(int i=j; i<p_mu; i++){ // row

            arma::mat tmp = Sigma.col(i) * Sigma.row(j);
            if(i!=j){
                tmp = tmp + tmp.t();
            }
            arma::vec Dtmp = tmp.diag();
            tmp *= -2.0;
            tmp.diag() += Dtmp;
            D_Sigma_Sigma.col(start) = LowTriVec(tmp);
            start ++;
        }
    }
    return D_Sigma_Sigma;
}

// D1(tr(SX))(vech(S))
arma::vec D1trace(const arma::mat& X){
    arma::mat tmp = X + X.t();
    tmp.diag() -= X.diag();
    return LowTriVec(tmp);
}

// D2(tr(Sigma*V))(vech(Sigma^-1)vech(V))
arma::mat D2_Sigma_V_fun(const arma::mat& Sigma){
    //// hessian on vech(invSigma) ////
    ///  needs info on Sigma ////
    int start = 0, p_mu = Sigma.n_cols,
        p_Sigma = (p_mu+1)*p_mu/2;

    arma::mat D_Sigma_V(p_Sigma, p_Sigma, arma::fill::zeros);
    D_Sigma_V.diag() -= 1.0;
    for(int j=0; j<p_mu; j++){ // column
        for(int i=j; i<p_mu; i++){ // row
            if(i==j){
                D_Sigma_V(start,start) = -0.5;
            }
            start ++;
        }
    }
    return D_Sigma_V;
}

// calcHessian
arma::mat calcHessian(const VBJM_data_t& data,
                      const VBJM_para_t& para){

    int p_beta = data.p_x;
    arma::uvec p_beta_vec = data.p_x_vec;
    int p_gamma = para.gamma.n_elem;
    int p_alpha = data.K;
    int p_weib = 2;
    int p_Sigma = (1+para.invSigma.n_cols)*para.invSigma.n_cols/2;
    int p_sig2 = data.K;
    int p_mu = data.p_z;
    arma::uvec p_mu_vec = data.p_z_vec;
    int p_V = (1+p_mu)*p_mu/2;

    const double lambda = para.weib(0);
    const double theta = para.weib(1);

    // only interested in covariance of these parameters,
    // not interested in mu and V
    int p_all = p_beta+p_gamma+p_alpha+p_weib+p_Sigma+p_sig2;
    arma::uvec p_all_vec(8);
    p_all_vec(0) = p_beta; p_all_vec(1) = p_gamma; p_all_vec(2) = p_alpha;
    p_all_vec(3) = p_weib; p_all_vec(4) = p_Sigma; p_all_vec(5) = p_sig2;
    p_all_vec(6) = p_mu; p_all_vec(7) = p_V;

    arma::umat var_pos(p_all_vec.n_elem,2);
    int start = 0, end=0;
    int start_z = 0, end_z=0;
    for(int j=0; j<p_all_vec.n_elem;j++){
        end = start + p_all_vec(j)-1;
        var_pos(j,0) = start;
        var_pos(j,1) = end;
        start = end+1;
    }

    //Rcout << var_pos << "\n";

    arma::mat H_11(p_all,p_all,arma::fill::zeros);

    //// on invSigma ////
    arma::mat D_Sigma_Sigma(p_Sigma, p_Sigma, arma::fill::zeros);

    D_Sigma_Sigma = D2logdet_Sigma(para.Sigma);
    D_Sigma_Sigma *= (data.n/2.0);
    H_11(arma::span(var_pos(4,0), var_pos(4,1)) ,
         arma::span(var_pos(4,0), var_pos(4,1))) = D_Sigma_Sigma;

    // D_Sigma_V is the same for all V_i //
    arma::mat D_Sigma_V(p_Sigma, p_V, arma::fill::zeros);
    D_Sigma_V = D2_Sigma_V_fun(para.Sigma);

    //arma::mat H_12_all(p_all,p_mu+p_V,arma::fill::zeros);
    //arma::mat H_22_all(p_mu+p_V,p_mu+p_V,arma::fill::zeros);
    //Rcout << "1\n";

    for(int i=0; i< data.n; i++){

        //Rcout << "11\n";
        arma::mat H_12(p_all,p_mu+p_V,arma::fill::zeros);
        arma::mat H_22(p_mu+p_V,p_mu+p_V,arma::fill::zeros);

        arma::vec h_0t = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
            lambda*std::log(theta);

        arma::vec h_it(h_0t.n_elem,arma::fill::zeros);
        h_it +=  arma::as_scalar(data.W.row(i) * para.gamma);

        arma::mat Z_ia_t = field_to_alpha_mat(data.Z_t, para.alpha,
                                              i, data.p_z_vec);

        for(int k=0; k<data.K; k++){
            h_it += data.X_t(i,k)*para.beta(k)*para.alpha(k) +
                data.Z_t(i,k)*para.mu(i,k)*para.alpha(k);
        }

        for(int j=0; j< h_it.n_elem; j++){
            h_it(j) += 0.5 * arma::as_scalar(
                Z_ia_t.row(j) * para.V(i) * Z_ia_t.row(j).t()
            );
        }

        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        arma::vec e_it = arma::exp(h_it);
        h_it = h_0t+h_it;
        h_it = arma::exp(h_it);


        //Rcout << "12\n";
        //Rcout << "beta\n";
        ///// beta part /////
        arma::mat D_beta_beta(p_beta,p_beta,arma::fill::zeros);
        arma::mat X_ia_t = field_to_alpha_mat(data.X_t, para.alpha,
                                              i, data.p_x_vec);
        start = 0, end=0;
        for(int k=0;k<data.K;k++){
            end = start+data.p_x_vec(k)-1;
            D_beta_beta.submat(start,start,end,end) -=
                data.X(i,k).t() * data.X(i,k) / para.sig2(k);
            start = end + 1;
        }
        for(int j=0; j<h_it.n_elem; j++){
            D_beta_beta -=
                h_it(j) * data.GQ_w(i)(j)* X_ia_t.row(j).t() * X_ia_t.row(j);
        }

        H_11(arma::span(var_pos(0,0), var_pos(0,1)) ,
             arma::span(var_pos(0,0), var_pos(0,1))) += D_beta_beta;

        //Rcout << "13\n";
        // beta_gamma
        arma::mat D_beta_gamma(p_beta,p_gamma,arma::fill::zeros);
        D_beta_gamma -= X_ia_t.t() * (data.GQ_w(i) % h_it) * data.W.row(i);

        H_11(arma::span(var_pos(0,0), var_pos(0,1)) ,
             arma::span(var_pos(1,0), var_pos(1,1))) += D_beta_gamma;

        //Rcout << "14\n";
        // beta_alpha
        arma::mat D_beta_alpha(p_beta,p_alpha, arma::fill::zeros);

        arma::mat VZa = para.V(i) * Z_ia_t.t();
        arma::field<arma::vec> D1h_alpha(data.K);

        //Rcout << "141\n";
        //Rcout << data.p_x_vec << "\n";
        //Rcout << VZa << "\n";

        start = 0, end=0;
        start_z=0, end_z=0;
        for(int k=0;k<data.K;k++){
            end = start+data.p_x_vec(k)-1;
            end_z = start_z+data.p_z_vec(k)-1;

            //Rcout << "142\n";
            if(data.fstat(i)==1){
                D_beta_alpha(arma::span(start,end), k) +=
                    data.X_T(i,k);
            }
            D1h_alpha(k) =
                data.X_t(i,k)*para.beta(k) + data.Z_t(i,k)*para.mu(i,k);
            //Rcout << "143\n";
            for(int j=0; j<D1h_alpha(k).n_elem; j++){
                D1h_alpha(k)(j) += arma::as_scalar(
                    data.Z_t(i,k).row(j) * VZa(arma::span(start_z,end_z), j)
                );
            }
            //Rcout << "144\n";
            D_beta_alpha.col(k) -= X_ia_t.t() * (data.GQ_w(i) % h_it%D1h_alpha(k));

            //Rcout << "145\n";
            D_beta_alpha(arma::span(start,end), k) -=
                data.X_t(i,k).t() * (data.GQ_w(i) % h_it);

            start = end + 1;
            start_z = end_z + 1;
        }
        //Rcout << "142\n";
        H_11(arma::span(var_pos(0,0), var_pos(0,1)) ,
             arma::span(var_pos(2,0), var_pos(2,1))) += D_beta_alpha;

        //Rcout << "15\n";
        // beta_weib
        arma::mat D_beta_weib(p_beta,p_weib, arma::fill::zeros);

        arma::vec D1h0_lambda(e_it.n_elem, arma::fill::zeros);
        arma::vec D1h0_theta(e_it.n_elem, arma::fill::zeros);

        D1h0_lambda = arma::pow(data.GQ_t(i)/theta, lambda)/data.GQ_t(i) %
            ( 1 + lambda*(arma::log(data.GQ_t(i))- std::log(theta)));

        D1h0_theta -= arma::pow(data.GQ_t(i)/theta, lambda)/data.GQ_t(i) * (lambda*
            lambda/theta);

        D_beta_weib.col(0) -= X_ia_t.t() * (data.GQ_w(i) % e_it % D1h0_lambda);
        D_beta_weib.col(1) -= X_ia_t.t() * (data.GQ_w(i) % e_it % D1h0_theta);


        H_11(arma::span(var_pos(0,0), var_pos(0,1)) ,
             arma::span(var_pos(3,0), var_pos(3,1))) += D_beta_weib;

        // beta_Sigma =0
        //Rcout << "16\n";
        // beta_sig2
        arma::mat D_beta_sig2(p_beta, p_sig2, arma::fill::zeros);
        start = 0, end=0;
        for(int k=0;k<data.K;k++){
            end = start+data.p_x_vec(k)-1;
            arma::vec Ytmp = data.Y(i,k) - data.X(i,k)*para.beta(k) -
                data.Z(i,k)*para.mu(i,k);
            D_beta_sig2(arma::span(start,end), k) -= data.X(i,k).t() * Ytmp /
                para.sig2(k)/para.sig2(k);
            start = end + 1;
        }

        H_11(arma::span(var_pos(0,0), var_pos(0,1)) ,
             arma::span(var_pos(5,0), var_pos(5,1))) += D_beta_sig2;

        //Rcout << "17\n";
        // beta_mu
        arma::mat D_beta_mu(p_beta, p_mu, arma::fill::zeros);
        int start_z = 0, end_z=0;
        start = 0, end=0;
        for(int k=0;k<data.K;k++){
            end = start+data.p_x_vec(k)-1;
            end_z = start_z+data.p_z_vec(k)-1;
            D_beta_mu.submat(start,start_z,end,end_z) -=
                data.X(i,k).t() * data.Z(i,k)/ para.sig2(k);
            start = end + 1;
            start_z = end_z + 1;
        }

        for(int j=0; j<h_it.n_elem; j++){
            D_beta_mu -= h_it(j) * data.GQ_w(i)(j)* X_ia_t.row(j).t() *
                Z_ia_t.row(j);
        }
        //Rcout << "18\n";
        // beta_V
        arma::mat D_beta_V(p_beta, p_V, arma::fill::zeros);
        for(int j=0; j<h_it.n_elem; j++){
            D_beta_V -= h_it(j) * data.GQ_w(i)(j)* X_ia_t.row(j).t() *
                D1trace( 0.5* Z_ia_t.row(j).t() *  Z_ia_t.row(j)).t() ;
        }

        H_12(arma::span(var_pos(0,0), var_pos(0,1)) ,
             arma::span(0, p_mu-1) ) += D_beta_mu;
        H_12(arma::span(var_pos(0,0), var_pos(0,1)) ,
             arma::span(p_mu, p_V+p_mu-1) ) += D_beta_V;


        //Rcout << "gamma\n";
        //Rcout << "19\n";
        ///// gamma part /////
        arma::mat D_gamma_gamma(p_gamma, p_gamma, arma::fill::zeros);
        D_gamma_gamma -= arma::accu(data.GQ_w(i) % h_it) *
            data.W.row(i).t() * data.W.row(i);

        H_11(arma::span(var_pos(1,0), var_pos(1,1)) ,
             arma::span(var_pos(1,0), var_pos(1,1))) += D_gamma_gamma;


        // gamma_alpha
        arma::mat D_gamma_alpha(p_gamma, p_alpha, arma::fill::zeros);

        for(int k=0;k<data.K;k++){
            D_gamma_alpha.col(k) -= data.W.row(i).t() *
                arma::accu(data.GQ_w(i) % h_it%D1h_alpha(k));
        }
        H_11(arma::span(var_pos(1,0), var_pos(1,1)) ,
             arma::span(var_pos(2,0), var_pos(2,1))) += D_gamma_alpha;


        // gamma_weib
        arma::mat D_gamma_weib(p_gamma, p_weib, arma::fill::zeros);

        D_gamma_weib.col(0) -= data.W.row(i).t() *
            arma::accu(data.GQ_w(i) % e_it % D1h0_lambda);
        D_gamma_weib.col(1) -= data.W.row(i).t() *
            arma::accu(data.GQ_w(i) % e_it % D1h0_theta);

        H_11(arma::span(var_pos(1,0), var_pos(1,1)) ,
             arma::span(var_pos(3,0), var_pos(3,1))) += D_gamma_weib;

        // gamma_Sigma =0
        // gamma_sig2 =0

        // gamma_mu
        arma::mat D_gamma_mu(p_gamma, p_mu, arma::fill::zeros);
        for(int j=0; j<h_it.n_elem; j++){
            D_gamma_mu -= h_it(j) * data.GQ_w(i)(j)* data.W.row(i).t() *
                Z_ia_t.row(j);
        }

        // gamma_V
        arma::mat D_gamma_V(p_gamma, p_V, arma::fill::zeros);
        for(int j=0; j<h_it.n_elem; j++){
            D_gamma_V -= h_it(j) * data.GQ_w(i)(j)* data.W.row(i).t() *
                D1trace( 0.5* Z_ia_t.row(j).t() *  Z_ia_t.row(j)).t() ;
        }

        H_12(arma::span(var_pos(1,0), var_pos(1,1)) ,
             arma::span(0, p_mu-1) ) += D_gamma_mu;
        H_12(arma::span(var_pos(1,0), var_pos(1,1)) ,
             arma::span(p_mu, p_V+p_mu-1) ) += D_gamma_V;


        //Rcout << "alpha\n";
        //// alpha part ////

        arma::mat D_alpha_alpha(p_alpha, p_alpha, arma::fill::zeros);
        arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);
        arma::vec beta = field_to_vec(para.beta, data.p_x_vec);
        arma::field<arma::mat> X_iD = field_to_field_Dmat(
            data.X_t.row(i), data.p_x_vec);
        arma::field<arma::mat> Z_iD = field_to_field_Dmat(
            data.Z_t.row(i), data.p_z_vec);

        for(int j=0; j< X_iD.n_elem; j++){
            arma::vec D1_h_alpha_vec = X_iD(j).t()* beta +
                Z_iD(j).t()*mu +  Z_iD(j).t()*para.V(i)* Z_ia_t.row(j).t();
            D_alpha_alpha -=  h_it(j) * data.GQ_w(i)(j)*
                ( D1_h_alpha_vec*D1_h_alpha_vec.t() +
                Z_iD(j).t()*para.V(i)*Z_iD(j));
        }


        H_11(arma::span(var_pos(2,0), var_pos(2,1)) ,
             arma::span(var_pos(2,0), var_pos(2,1))) += D_alpha_alpha;

        // alpha_weib
        arma::mat D_alpha_weib(p_alpha, p_weib, arma::fill::zeros);
        for(int k=0; k<data.K;k++){
            D_alpha_weib(k,0) -=
                arma::accu(data.GQ_w(i) % e_it % D1h0_lambda % D1h_alpha(k));
            D_alpha_weib(k,1) -=
                arma::accu(data.GQ_w(i) % e_it % D1h0_theta % D1h_alpha(k));
        }

        H_11(arma::span(var_pos(2,0), var_pos(2,1)) ,
             arma::span(var_pos(3,0), var_pos(3,1))) += D_alpha_weib;

        // alpha_Sigma=0
        // alpha_sig2=0

        // alpha_mu

        arma::mat D_alpha_mu(p_alpha, p_mu, arma::fill::zeros);
        arma::mat Z_TD = field_to_Dmat(data.Z_T.row(i),  data.p_z_vec);
        if(data.fstat(i)==1){
            D_alpha_mu += Z_TD.t();
        }

        for(int j=0; j< X_iD.n_elem; j++){
            arma::vec D1_h_alpha_vec = X_iD(j).t()* beta +
                Z_iD(j).t()*mu +  Z_iD(j).t()*para.V(i)* Z_ia_t.row(j).t();
            D_alpha_mu -=  h_it(j) * data.GQ_w(i)(j)*
                ( Z_iD(j).t()  + D1_h_alpha_vec * Z_ia_t.row(j));
        }


        // alpha_V
        arma::mat D_alpha_V(p_alpha, p_V, arma::fill::zeros);
        for(int j=0; j< X_iD.n_elem; j++){
            arma::vec D1_h_alpha_vec = X_iD(j).t()* beta +
                Z_iD(j).t()*mu +  Z_iD(j).t()*para.V(i)* Z_ia_t.row(j).t();
            D_alpha_V -= h_it(j) * data.GQ_w(i)(j)*
                (D1_h_alpha_vec *
                D1trace( 0.5* Z_ia_t.row(j).t()*Z_ia_t.row(j) ).t() );

            for(int k=0; k<data.K; k++){
                D_alpha_V.row(k) -=  h_it(j) * data.GQ_w(i)(j)*(
                    D1trace ( Z_ia_t.row(j).t() * Z_iD(j).col(k).t() ).t()
                );
            }
        }

        H_12(arma::span(var_pos(2,0), var_pos(2,1)) ,
             arma::span(0, p_mu-1) ) += D_alpha_mu;
        H_12(arma::span(var_pos(2,0), var_pos(2,1)) ,
             arma::span(p_mu, p_V+p_mu-1) ) += D_alpha_V;

        //Rcout << "Weibull\n";
        ///// Weibull part /////

        arma::mat D_weib_weib(p_weib, p_weib, arma::fill::zeros);
        if(data.fstat(i)==1){
            D_weib_weib(0,0) -= 1.0/lambda/lambda;
            D_weib_weib(0,1) -= 1.0/theta;
            D_weib_weib(1,1) += lambda/theta/theta;
        }

        arma::vec h_2_lam2 = arma::pow(data.GQ_t(i)/theta, lambda)/data.GQ_t(i) %(
            2*(arma::log(data.GQ_t(i)/theta)) +
                lambda* arma::square( arma::log(data.GQ_t(i)/theta) )
        );

        arma::vec h_2_lam_theta = -arma::pow(data.GQ_t(i)/theta, lambda)/data.GQ_t(i) %(
            2 + lambda*(arma::log(data.GQ_t(i)/theta)) ) * lambda/theta;

        arma::vec h_2_theta_theta = arma::pow(data.GQ_t(i)/theta, lambda)/data.GQ_t(i) *
            (lambda+1) * std::pow( lambda/theta,2.0);

        D_weib_weib(0,0) -= arma::accu(data.GQ_w(i) % e_it %h_2_lam2);
        D_weib_weib(0,1) -= arma::accu(data.GQ_w(i) % e_it %h_2_lam_theta);
        D_weib_weib(1,0) = D_weib_weib(0,1);
        D_weib_weib(1,1) -= arma::accu(data.GQ_w(i) % e_it %h_2_theta_theta);

        H_11(arma::span(var_pos(3,0), var_pos(3,1)) ,
             arma::span(var_pos(3,0), var_pos(3,1))) += D_weib_weib;


        // D_weib_mu
        arma::mat D_weib_mu(p_weib, p_mu, arma::fill::zeros);
        D_weib_mu.row(0) -=  (data.GQ_w(i) % e_it % D1h0_lambda).t() * Z_ia_t;
        D_weib_mu.row(1) -=  (data.GQ_w(i) % e_it % D1h0_theta).t() * Z_ia_t;


        // D_weib_V
        arma::mat D_weib_V(p_weib, p_V, arma::fill::zeros);
        for(int j=0; j< e_it.n_elem; j++){
            D_weib_V.row(0) -= e_it(j) * data.GQ_w(i)(j)*  D1h0_lambda(j)*
                D1trace( 0.5* Z_ia_t.row(j).t() *  Z_ia_t.row(j)).t();
            D_weib_V.row(1) -= e_it(j) * data.GQ_w(i)(j)*  D1h0_theta(j)*
                D1trace( 0.5* Z_ia_t.row(j).t() *  Z_ia_t.row(j)).t();
        }

        H_12(arma::span(var_pos(3,0), var_pos(3,1)) ,
             arma::span(0, p_mu-1) ) += D_weib_mu;
        H_12(arma::span(var_pos(3,0), var_pos(3,1)) ,
             arma::span(p_mu, p_V+p_mu-1) ) += D_weib_V;

        //Rcout << "Sigma\n";
        ///// Sigma part /////

        // D_Sigma_mu
        arma::mat D_Sigma_mu(p_Sigma, p_mu, arma::fill::zeros);

        for(int j=0; j<p_mu; j++){
            arma::mat mu0(p_mu,p_mu, arma::fill::zeros);
            mu0.row(j) += mu.t();
            mu0.col(j) += mu;
            arma::vec Dtmp = mu0.diag();
            mu0 *= 2.0;
            mu0.diag() -= Dtmp;
            D_Sigma_mu.col(j) = -0.5 * LowTriVec(mu0);
        }

        // D_Sigma_V
        H_12(arma::span(var_pos(4,0), var_pos(4,1)) ,
             arma::span(0, p_mu-1) ) += D_Sigma_mu;
        H_12(arma::span(var_pos(4,0), var_pos(4,1)) ,
             arma::span(p_mu, p_V+p_mu-1) ) += D_Sigma_V;


        //Rcout << "sig2\n";
        ///// sig2 part /////
        arma::mat D_sig2_sig2(p_sig2, p_sig2, arma::fill::zeros);
        arma::mat D_sig2_mu(p_sig2, p_mu, arma::fill::zeros);
        arma::mat D_sig2_V(p_sig2, p_V, arma::fill::zeros);

        start = 0;
        for(int k=0; k < data.K; k++){
            end = start + data.p_z_vec(k)-1;
            arma::vec Ytmp = data.Y(i,k) - data.X(i,k)*para.beta(k) -
                data.Z(i,k) * para.mu(i,k);
            double zvz = arma::trace(
                data.Z(i,k) * para.V(i).submat(start, start, end, end) *
                    data.Z(i,k).t() );
            D_sig2_sig2(k,k) = Ytmp.n_elem*0.5/para.sig2(k)/para.sig2(k) -
                (arma::accu(arma::square(Ytmp))+zvz) / std::pow(para.sig2(k),3);

            D_sig2_mu(k, arma::span(start, end)) -=
                Ytmp.t() * data.Z(i,k) / std::pow(para.sig2(k),2);

            arma::mat ZZ(p_mu, p_mu, arma::fill::zeros);
            ZZ.submat(start, start, end, end) =  data.Z(i,k).t()* data.Z(i,k);

            D_sig2_V.row(k) = 0.5/std::pow(para.sig2(k),2)*D1trace(ZZ).t();

            start = end + 1;
        }

        H_11(arma::span(var_pos(5,0), var_pos(5,1)) ,
             arma::span(var_pos(5,0), var_pos(5,1))) += D_sig2_sig2;
        H_12(arma::span(var_pos(5,0), var_pos(5,1)) ,
             arma::span(0, p_mu-1) ) += D_sig2_mu;
        H_12(arma::span(var_pos(5,0), var_pos(5,1)) ,
             arma::span(p_mu, p_V+p_mu-1) ) += D_sig2_V;

        //Rcout << "muV\n";
        ///// mu and V part /////

        // Rcout << "1\n";
        arma::mat D_mu_mu(p_mu, p_mu, arma::fill::zeros);
        arma::mat D_mu_V(p_mu, p_V, arma::fill::zeros);
        arma::mat D_V_V(p_V, p_V, arma::fill::zeros);

        start = 0;
        for(int k=0; k<data.K; k++){
            end = start + data.p_z_vec(k) - 1;
            D_mu_mu.submat(start, start, end,end) -=
                data.Z(i, k).t() *  data.Z(i, k)/ para.sig2(k);
            start = end+1;
        }

        // Rcout << "2\n";
        for(int j=0; j< h_it.n_elem; j++){
            D_mu_mu -= h_it(j) * data.GQ_w(i)(j)* Z_ia_t.row(j).t()*
                Z_ia_t.row(j);
            D_mu_V -= h_it(j) * data.GQ_w(i)(j)* Z_ia_t.row(j).t()*
                D1trace( 0.5* Z_ia_t.row(j).t() *  Z_ia_t.row(j)).t();

            D_V_V -= h_it(j) * data.GQ_w(i)(j)*
                D1trace( 0.5* Z_ia_t.row(j).t() *  Z_ia_t.row(j))*
                D1trace( 0.5* Z_ia_t.row(j).t() *  Z_ia_t.row(j)).t();
        }
        D_mu_mu -= para.invSigma;

        arma::mat invV = myinvCpp(para.V(i));
        D_V_V += 0.5 * D2logdet_Sigma(invV);


        H_22(arma::span(0, p_mu-1) ,
             arma::span(0, p_mu-1)) += D_mu_mu;
        H_22(arma::span(0, p_mu-1),
             arma::span(p_mu, p_V+p_mu-1)) += D_mu_V;
        H_22(arma::span(p_mu, p_V+p_mu-1) ,
             arma::span(p_mu, p_V+p_mu-1)) += D_V_V;

        H_22 = arma::symmatu(H_22);
        H_11 = arma::symmatu(H_11);

        H_22 = arma::inv(H_22);
        H_11-= H_12 * H_22* H_12.t();
        //break;
    }

    return H_11;
    // return List::create(
    //     _["H11"] =H_11,
    //     _["H12"] = H_12_all,
    //     _["H22"]=H_22_all
    // );
}



//' Main function to run VBJM
//'
//'@noRd
//'
// [[Rcpp::export]]
List VBJM(const List& datalist, const List& paralist,
          int maxiter = 100, double eps=1e-4){

    //Rcout << "1\n";
    VBJM_data_t data(datalist);

    //Rcout << "2\n";
    VBJM_para_t para(paralist);

    //Rcout << "3\n";
    // arma::field<arma::mat> res = field_to_field_Dmat(
    //     data.X_t.row(0),data.p_x_vec);

    ens::L_BFGS lbfgs;

    lbfgs.MinGradientNorm() = 1e-8;
    // lbfgs.MinGradientNorm() = eps;
    //lbfgs.MaxIterations() = 5;
    updateMuVFun MuV_fun(data, para);
    updateParaFun Para_fun(data,  para);

    double ELBO = calcELBO(data, para);
    arma::vec ELBO_vec(maxiter);
    int iter;

    //double err_ELBO=0;
    double err_para;

    for(iter=0; iter < maxiter; iter++){
        // Rcout << ELBO << "\n";
        // update V and mu -- variational para
        //Rcout << "31\n";
        for(int i=0; i < data.n; i++){
            MuV_fun.i_now = i;
            MuV_fun.updateIntermediate();

            arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);
            arma::vec Lvec = para.Lvec(i);

            arma::vec muV(Lvec.n_elem + mu.n_elem);
            muV.subvec(0, data.p_z-1) = mu;
            muV.subvec(data.p_z, muV.n_elem-1) = Lvec;

            lbfgs.Optimize(MuV_fun,muV);

            mu = muV.subvec(0, data.p_z-1);
            Lvec = muV.subvec(data.p_z, muV.n_elem-1);
            storeMuV(data, para,  mu, Lvec, i);

            //ELBO = calcELBO(data, para);
            //Rcout << ELBO << "\n";

        }

        //ELBO = calcELBO(data, para);
        //Rcout << ELBO << "\n";

        // update paramters other than sig2 and Sigma
        //Rcout << "32\n";
        arma::vec para_all = combinePara(data, para);
        arma::vec para_all_prev = para_all;
        //Rcout << "321\n";
        lbfgs.Optimize(Para_fun, para_all);
        //Rcout << "322\n";
        storePara( para_all, data, para);

        //ELBO = calcELBO(data, para);
        //Rcout << ELBO << "\n";


        // update sig2 and Sigma
        //Rcout << "33\n";
        updateSig(data,  para);
        ELBO = calcELBO(data, para);
        //Rcout << ELBO << "\n";

        ELBO_vec(iter) = ELBO;
        //Rcout << "iter="<< iter << "; EBLO=" << ELBO <<"\n";

        if(iter >= 0){
            // if(iter>0){
            //     err_ELBO = std::fabs(ELBO_vec(iter)-ELBO_vec(iter-1))/data.n/data.K;
            // }
            //double err_ELBO = (ELBO_vec(iter)-ELBO_vec(iter-1))/ELBO_vec(iter-1);
            err_para = std::sqrt(
                arma::accu(arma::square(para_all-para_all_prev))/para_all.n_elem
                                     );
            // err_ELBO < eps or
            if( err_para<eps){
                break;
            }

        }

    }

    //Rcout <<"iters:" <<  iter << "; err_ELBO="<< err_ELBO << "; err_para=" << err_para <<"\n";
    //Rcout << "4\n";
    arma::mat H = calcHessian( data,  para);

    // arma::vec sigma_vec = LowTriVec(para.invSigma);
    //arma::mat test_mat = call_KD_hessian_in_C(para.beta(0),  data,  para);
    return List::create(
        _["sig2"] = para.sig2,
        _["Sigma"] = para.Sigma,
        _["alpha"] = para.alpha,
        _["beta"] = para.beta,
        _["weib"] = para.weib,
        _["gamma"] = para.gamma,
        _["mu"] = para.mu,
        _["V"] = para.V,
        _["ELBO"] = ELBO_vec,
        _["iter"] = iter,
        _["H"] = H
    );
}


//' Main function to predict random effects in VBJM
//'
//' @noRd
// [[Rcpp::export]]
List VBJM_raneff(const List& datalist, const List& paralist, double eps=1e-4){

    //Rcout << "1\n";
    VBJM_data_t data(datalist);

    //Rcout << "2\n";
    VBJM_para_t para(paralist);

    //
    ens::L_BFGS lbfgs;
    lbfgs.MinGradientNorm() = 1e-8;
    //lbfgs.MaxIterations() = 5;
    updateMuVFun MuV_fun(data, para);

    for(int i=0; i < data.n; i++){
        MuV_fun.i_now = i;
        MuV_fun.updateIntermediate();

        arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);
        arma::vec Lvec = para.Lvec(i);

        arma::vec muV(Lvec.n_elem + mu.n_elem);
        muV.subvec(0, data.p_z-1) = mu;
        muV.subvec(data.p_z, muV.n_elem-1) = Lvec;

        lbfgs.Optimize(MuV_fun,muV);

        mu = muV.subvec(0, data.p_z-1);
        Lvec = muV.subvec(data.p_z, muV.n_elem-1);
        storeMuV(data, para,  mu, Lvec, i);
    }


    return List::create(
        _["sig2"] = para.sig2,
        _["Sigma"] = para.Sigma,
        _["alpha"] = para.alpha,
        _["beta"] = para.beta,
        _["weib"] = para.weib,
        _["gamma"] = para.gamma,
        _["mu"] = para.mu,
        _["V"] = para.V
    );
}


//' Main function to get hessian in VBJM
//'
//'@noRd
//'
// [[Rcpp::export]]
double VBJM_numH(const arma::vec para_all, const List& datalist,
                 List& paralist, double eps=1e-4){

    //Rcout << "1\n";
    VBJM_data_t data(datalist);

    //Rcout << "2\n";
    VBJM_para_t para(paralist);

    //
    int p_beta = data.p_x;
    arma::uvec p_beta_vec = data.p_x_vec;
    int p_gamma = para.gamma.n_elem;
    int p_alpha = data.K;
    int p_weib = 2;
    int p_Sigma = (1+para.invSigma.n_cols)*para.invSigma.n_cols/2;
    int p_sig2 = data.K;

    //const double lambda = para.weib(0);
    //const double theta = para.weib(1);

    // only interested in covariance of these parameters,
    // not interested in mu and V
    //int p_all = p_beta+p_gamma+p_alpha+p_weib+p_Sigma+p_sig2;
    arma::uvec p_all_vec(6);
    p_all_vec(0) = p_beta; p_all_vec(1) = p_gamma; p_all_vec(2) = p_alpha;
    p_all_vec(3) = p_weib; p_all_vec(4) = p_Sigma; p_all_vec(5) = p_sig2;

    arma::field<arma::vec> para_field = vec_to_field(para_all, p_all_vec);
    arma::field<arma::vec> beta = vec_to_field(para_field(0), data.p_x_vec);
    para.beta = beta;
    para.gamma = para_field(1);
    para.alpha = para_field(2);
    //para_field(3) = arma::clamp(para_field(3), -MAX_EXP, MAX_EXP);
    //para.weib = arma::exp(para_field(3));
    para.weib = para_field(3);

    arma::mat L =  makeLowTriMat( para.Sigma,  para_field(4));
    para.Sigma = L*L.t();
    para.invSigma = myinvCpp(para.Sigma);
    para.sig2 = para_field(5);

    //
    ens::L_BFGS lbfgs;
    lbfgs.MinGradientNorm() = 1e-8;
    //lbfgs.MaxIterations() = 5;
    updateMuVFun MuV_fun(data, para);

    for(int i=0; i < data.n; i++){
        MuV_fun.i_now = i;
        MuV_fun.updateIntermediate();

        arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);
        arma::vec Lvec = para.Lvec(i);

        arma::vec muV(Lvec.n_elem + mu.n_elem);
        muV.subvec(0, data.p_z-1) = mu;
        muV.subvec(data.p_z, muV.n_elem-1) = Lvec;

        lbfgs.Optimize(MuV_fun,muV);

        mu = muV.subvec(0, data.p_z-1);
        Lvec = muV.subvec(data.p_z, muV.n_elem-1);
        storeMuV(data, para,  mu, Lvec, i);
    }

    double ELBO = calcELBO(data, para);
    return ELBO;
}


//' Main function to get sub-hessian in VBJM
//'
//' @noRd
//'
// [[Rcpp::export]]
double VBJM_numH_sub(const arma::vec para_all, const List& datalist,
                     List& paralist, double eps=1e-4){

    //Rcout << "1\n";
    VBJM_data_t data(datalist);

    //Rcout << "2\n";
    VBJM_para_t para(paralist);

    //
    int p_beta = data.p_x;
    arma::uvec p_beta_vec = data.p_x_vec;
    int p_gamma = para.gamma.n_elem;
    int p_alpha = data.K;
    int p_weib = 2;
    // int p_Sigma = (1+para.invSigma.n_cols)*para.invSigma.n_cols/2;
    // int p_sig2 = data.K;

    //const double lambda = para.weib(0);
    //const double theta = para.weib(1);

    // only interested in covariance of these parameters,
    // not interested in mu and V
    //int p_all = p_beta+p_gamma+p_alpha+p_weib+p_Sigma+p_sig2;
    arma::uvec p_all_vec(4);
    p_all_vec(0) = p_beta; p_all_vec(1) = p_gamma; p_all_vec(2) = p_alpha;
    p_all_vec(3) = p_weib;
    // p_all_vec(4) = p_Sigma; p_all_vec(5) = p_sig2;

    arma::field<arma::vec> para_field = vec_to_field(para_all, p_all_vec);
    arma::field<arma::vec> beta = vec_to_field(para_field(0), data.p_x_vec);
    para.beta = beta;
    para.gamma = para_field(1);
    para.alpha = para_field(2);
    //para_field(3) = arma::clamp(para_field(3), -MAX_EXP, MAX_EXP);
    //para.weib = arma::exp(para_field(3));
    para.weib = para_field(3);

    //arma::mat L =  makeLowTriMat( para.Sigma,  para_field(4));
    //para.Sigma = L*L.t();
    //para.invSigma = myinvCpp(para.Sigma);
    //para.sig2 = para_field(5);

    //
    ens::L_BFGS lbfgs;
    lbfgs.MinGradientNorm() = 1e-8;
    //lbfgs.MaxIterations() = 5;
    updateMuVFun MuV_fun(data, para);

    const int maxiter = 100;
    const int p_len = para.Sigma.n_elem + para.sig2.n_elem;

    double err_para = 1;
    arma::mat Sigma_prev = para.Sigma;
    arma::vec sig2_prev = para.sig2;

    for(int iter=0; iter < maxiter; iter++){

        Sigma_prev = para.Sigma;
        sig2_prev = para.sig2;

        for(int i=0; i < data.n; i++){
            MuV_fun.i_now = i;
            MuV_fun.updateIntermediate();

            arma::vec mu = field_to_vec(para.mu.row(i), data.p_z_vec);
            arma::vec Lvec = para.Lvec(i);

            arma::vec muV(Lvec.n_elem + mu.n_elem);
            muV.subvec(0, data.p_z-1) = mu;
            muV.subvec(data.p_z, muV.n_elem-1) = Lvec;

            lbfgs.Optimize(MuV_fun,muV);

            mu = muV.subvec(0, data.p_z-1);
            Lvec = muV.subvec(data.p_z, muV.n_elem-1);
            storeMuV(data, para,  mu, Lvec, i);
        }

        updateSig(data,  para);

        if(iter >= 0){
            err_para = std::sqrt(
                arma::accu( arma::square(sig2_prev - para.sig2))/p_len +
                    arma::accu(arma::square(Sigma_prev - para.Sigma))/p_len
                    );
            // err_ELBO < eps or
            if( err_para < eps){
                // Rcout << iter << "\n";
                break;
            }
        }
    }

    double ELBO = calcELBO(data, para);
    return ELBO;
}


