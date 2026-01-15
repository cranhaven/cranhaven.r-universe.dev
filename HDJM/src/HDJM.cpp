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
    invflag = arma::inv_sympd(B , A);
    if(!invflag){
        //Rcout << "inv_sympd failed, try inv\n";
        invflag = arma::inv( B, A);
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

//' Main function to run LME
//' @noRd
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
                             int i_now, arma::uvec p_x_vec,
                             arma::uvec idx){

    int p_x = arma::accu(p_x_vec.elem(idx));
    arma::vec X_ia(p_x);

    int start = 0, k;
    for(int j=0; j<idx.n_elem; j++){
        k = idx(j);
        X_ia.subvec(start,start+p_x_vec(k)-1) = X_T(i_now, k) * alpha(k);
        start = start+p_x_vec(k);
    }
    return X_ia;
}

// convert field of mat to mat
arma::mat field_to_alpha_mat(const arma::field<arma::mat>& X_t,
                             const arma::vec& alpha,
                             int i_now, const arma::uvec& p_x_vec,
                             arma::uvec idx){

    int p_x = arma::accu(p_x_vec.elem(idx));
    arma::mat X_ia(X_t(i_now,0).n_rows , p_x);

    int start = 0,k;
    for(int j=0; j<idx.n_elem; j++){
        k = idx(j);
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

// convert vec to field of vec
arma::field<arma::vec> vec_to_field_L(const arma::vec& L,
                                       const arma::uvec& p_z_vec){

    arma::field<arma::vec> L_f(p_z_vec.n_elem);

    int start = 0,step;
    for(int k=0; k<p_z_vec.n_elem; k++){
        step = p_z_vec(k)*(p_z_vec(k)+1)/2;
        L_f(k) = L.subvec(start,start+step-1);
        start = start + step ;
    }
    return L_f;
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

// extract lower-triangular elements of field of matrices
arma::vec LowTriVec_field(const arma::field<arma::mat>& V){
    arma::uvec p_vec(V.n_elem);
    arma::field<arma::vec> Lvec_f(V.n_elem);

    for(int j =0; j< V.n_elem; j++){
        arma::uvec lower_indices = arma::trimatl_ind( arma::size(V(j)) );
        Lvec_f(j) = V(j)(lower_indices);
        p_vec(j) = Lvec_f(j).n_elem;
    }

    return field_to_vec(Lvec_f, p_vec);
}


// data struct
struct HDJM_data_t{
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
    HDJM_data_t(const List& datalist)
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

struct HDJM_para_t{
    // para part //
    arma::field<arma::vec> mu; // n \times K vec
    arma::field<arma::mat> V; // n \times K mat
    arma::field<arma::vec> Lvec; // n \times K vec: Lvec*Lvec.t() = V
    arma::field<arma::vec> beta; // K \times 1 vec
    arma::vec sig2; // dim = K
    arma::field<arma::mat> Sigma; // K \times 1 mat
    arma::field<arma::mat> invSigma; // inverse of Sigma
    arma::vec gamma; // dim = p_w
    arma::vec alpha; // dim = K
    arma::vec weib; // dim = 2, first is shape, second is scale

    arma::uvec alpha_idx; // index for nonzero alphas
    int p_x_alpha; // total number of fixed-effects for nonzero alphas
    int p_z_alpha; // total number of random-effects for nonzero alphas
    int p_zz_alpha; // total length of Lvec for nonzero alphas
    arma::uvec p_x_vec_alpha; //number of fixed-effects for each nonzero alpha
    arma::uvec p_z_vec_alpha; //number of random-effects for each nonzero alpha
    arma::uvec p_zz_vec_alpha; //length of Lvec for each nonzero alpha

    arma::uvec npara_vec; // num. of parameters in beta, gamma, alpha, weib
    // initialization function //
    HDJM_para_t(const List& paralist)
    {
        sig2 = as<arma::vec>(paralist["sig2"]);
        //Sigma = as<arma::mat>(paralist["Sigma"]);
        gamma = as<arma::vec>(paralist["gamma"]);
        alpha = as<arma::vec>(paralist["alpha"]);
        weib = as<arma::vec>(paralist["weib"]);

        int K = sig2.n_elem;
        arma::field<arma::mat> V_tmp = paralist["V"];
        int n = V_tmp.n_elem/K;

        V = arma::field<arma::mat>(n,K);
        field_reshape_mat(V_tmp, V,  n, K);
        V_tmp.clear();

        Lvec = arma::field<arma::vec>(n,K);
        for(int i=0; i<n; i++){
            // Cholesky decomposition
            for(int k=0; k<K; k++){
                arma::mat Ltmp = arma::chol(V(i,k),"lower");
                arma::uvec lower_indices = arma::trimatl_ind(arma::size(Ltmp));
                Lvec(i,k) = Ltmp(lower_indices);
            }
        }

        arma::field<arma::vec> mu_tmp = paralist["mu"];
        mu = arma::field<arma::vec>(n,K);
        field_reshape_vec(mu_tmp, mu,  n, K);
        mu_tmp.clear();

        arma::field<arma::vec> beta_tmp = paralist["beta"];
        beta = beta_tmp;
        beta_tmp.clear();

        arma::field<arma::mat> Sigma_tmp = paralist["Sigma"];
        Sigma = Sigma_tmp;
        Sigma_tmp.clear();

        invSigma = arma::field<arma::mat>(K);
        for(int k=0; k<K; k++){
            invSigma(k) = myinvCpp(Sigma(k));
        }


        npara_vec = arma::uvec(3, arma::fill::zeros);
        for(int k=0; k<beta.n_elem; k++){
            npara_vec(0) += beta(k).n_elem;
        }
        npara_vec(1) = gamma.n_elem;
        //npara_vec(2) = alpha.n_elem;
        npara_vec(2) = weib.n_elem;
    }


    void NonZeroAlpha(){
        alpha_idx = arma::find(alpha);
        if(alpha_idx.n_elem > 0){
            p_z_vec_alpha = arma::uvec(alpha_idx.n_elem);
            p_zz_vec_alpha = arma::uvec(alpha_idx.n_elem);
            p_x_vec_alpha = arma::uvec(alpha_idx.n_elem);
            for(int j=0; j<alpha_idx.n_elem; j++){
                p_z_vec_alpha(j) = mu(0,alpha_idx(j)).n_elem;
                p_zz_vec_alpha(j) = p_z_vec_alpha(j)*(p_z_vec_alpha(j)+1)/2;
                p_x_vec_alpha(j) = beta(alpha_idx(j)).n_elem;
            }
            p_z_alpha = arma::accu(p_z_vec_alpha);
            p_zz_alpha = arma::accu(p_zz_vec_alpha);
            p_x_alpha = arma::accu(p_x_vec_alpha);

            npara_vec(0) = p_x_alpha;
        }
    }
};


// update variational parameters mu_i and V_i //
// only update mu_ik and V_ik for nonzero alpha_k //
class updateMuVFun{
public:
    const HDJM_data_t& data;
    const HDJM_para_t& para;
    arma::field<arma::vec> Ytmp; // Y - X*beta
    arma::vec Z_ia_T;
    arma::mat Z_ia_t;
    arma::field<arma::mat> ZOZ;
    arma::vec h_t;
    int i_now=0;

    updateMuVFun(const HDJM_data_t& data,
                 const HDJM_para_t& para) :
        data(data), para(para){
        //Ytmp = arma::field<arma::vec>(data.K);
    }

    void initiate(){
        if(para.alpha_idx.n_elem > 0){
            Ytmp = arma::field<arma::vec>(para.alpha_idx.n_elem);
            ZOZ = arma::field<arma::mat>(para.alpha_idx.n_elem);
        }
    }

    void updateIntermediate(){
        double lambda = para.weib(0);
        double theta = para.weib(1);
        h_t = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i_now)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i_now) * para.gamma);

        int k = 0;
        for(int j=0; j<para.alpha_idx.n_elem; j++){
            k = para.alpha_idx(j);
            Ytmp(j) = data.Y(i_now, k) - data.X(i_now,k)*para.beta(k);
            ZOZ(j) = data.Z(i_now,k).t()*data.Z(i_now,k)/para.sig2(k) + para.invSigma(k);
            h_t += data.X_t(i_now,k)*para.beta(k) *para.alpha(k);
        }

        Z_ia_T = field_to_alpha_vec(data.Z_T, para.alpha,
                                    i_now, data.p_z_vec, para.alpha_idx);
        Z_ia_t = field_to_alpha_mat(data.Z_t, para.alpha,
                                    i_now, data.p_z_vec, para.alpha_idx);

    }

    // Return the objective function with gradient.
    double EvaluateWithGradient(const arma::mat& muV, arma::mat& g)
    {
        arma::vec mu = muV(arma::span(0,para.p_z_alpha-1), 0);
        arma::vec Lvec = muV(arma::span(para.p_z_alpha, muV.n_rows-1), 0);

        arma::field<arma::vec> mu_f = vec_to_field(mu, para.p_z_vec_alpha);
        arma::field<arma::vec> Lvec_f = vec_to_field_L(Lvec, para.p_z_vec_alpha);

        int k=0;
        arma::field<arma::mat> V_f(para.alpha_idx.n_elem);
        arma::field<arma::mat> L_f(para.alpha_idx.n_elem);

        for(int j=0; j< para.alpha_idx.n_elem;j++){
            k = para.alpha_idx(j);
            L_f(j) = makeLowTriMat(para.V(i_now,k),  Lvec_f(j));
            V_f(j) = L_f(j)*L_f(j).t();
        }

        double val;
        double sign;

        /// fun value
        double fval = 0.0;
        arma::vec h_it = h_t + Z_ia_t*mu;
        for(int jj=0;jj<h_it.n_elem; jj++){
            for(int j=0; j<para.alpha_idx.n_elem;j++){
                k = para.alpha_idx(j);
                h_it(jj) +=  0.5 * para.alpha(k)*para.alpha(k) * arma::as_scalar(
                    data.Z_t(i_now, k).row(jj) *  V_f(j)*
                        data.Z_t(i_now, k).row(jj).t()
                );
            }
        }
        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        h_it = arma::exp(h_it);

        arma::field<arma::vec> Ytmp2 = Ytmp;
        for(int j=0; j<para.alpha_idx.n_elem;j++){
            k = para.alpha_idx(j);
            Ytmp2(j) -= data.Z(i_now, k) * mu_f(j);
            fval += -0.5 * arma::accu(arma::square(Ytmp2(j)))/para.sig2(k) -
                0.5 *  arma::as_scalar(mu_f(j).t() * para.invSigma(k) *mu_f(j));
            arma::log_det(val, sign, V_f(j));
            fval += -0.5*arma::trace(ZOZ(j)*V_f(j)) +  0.5 * val;
        }
        fval += data.fstat(i_now)* arma::accu(Z_ia_T % mu) -
            arma::accu(data.GQ_w(i_now) % h_it);


        /// gradient

        //arma::mat grad_V = -1*ZOZ*L + arma::trans(arma::inv( arma::trimatl(L)));
        // grad_V -= Z_ia_t.t()*arma::diagmat((data.GQ_w(i_now) % h_it)) *
        //     Z_ia_t * L;

        arma::field<arma::mat> grad_V(para.alpha_idx.n_elem);
        for(int j=0; j<para.alpha_idx.n_elem;j++){
            k = para.alpha_idx(j);
            grad_V(j) = -ZOZ(j)*L_f(j) + arma::trans(arma::inv( arma::trimatl(L_f(j)))) -
                para.alpha(k)*para.alpha(k) *  data.Z_t(i_now, k).t() *
                arma::diagmat((data.GQ_w(i_now) % h_it)) *
                            data.Z_t(i_now, k) * L_f(j) ;
        }

        arma::vec grad_mu(mu.n_rows,arma::fill::zeros);
        int start = 0;
        for(int j=0; j<para.alpha_idx.n_elem; j++){
            k = para.alpha_idx(j);
            grad_mu.subvec(start, start+para.p_z_vec_alpha(j)-1) =
                data.Z(i_now, k).t() * Ytmp2(j) / para.sig2(k) -
                para.invSigma(k) * mu_f(j);
            start = start + para.p_z_vec_alpha(j);
        }
        grad_mu += data.fstat(i_now) * Z_ia_T -
            Z_ia_t.t() * (data.GQ_w(i_now) % h_it);

        fval = -1*fval;

        g(arma::span(0,para.p_z_alpha-1), 0) = -grad_mu;
        g(arma::span(para.p_z_alpha, muV.n_rows-1), 0) = -LowTriVec_field(grad_V);

        return fval;
    }

};


// combine parameters into vector //
// mu_i, V_i //
arma::vec combineMuV(const HDJM_data_t& data, const HDJM_para_t& para,
                     const int& i){

    arma::field<arma::vec> mu_f(para.alpha_idx.n_elem);
    arma::field<arma::vec> Lvec_f(para.alpha_idx.n_elem);

    int k;
    for(int j=0; j<para.alpha_idx.n_elem; j++){
        k = para.alpha_idx(j);
        mu_f(j) = para.mu(i,k);
        Lvec_f(j) = para.Lvec(i,k);
    }

    arma::vec mu = field_to_vec(mu_f, para.p_z_vec_alpha);
    arma::vec Lvec =field_to_vec(Lvec_f, para.p_zz_vec_alpha);

    arma::vec muV(Lvec.n_elem + mu.n_elem);
    muV.subvec(0, mu.n_elem - 1) = mu;
    muV.subvec(mu.n_elem, muV.n_elem-1) = Lvec;
    return muV;
}

// to put the new updates into para //
void storeMuV(const HDJM_data_t& data, HDJM_para_t& para,
              const arma::vec& muV,
              const int& i){

    arma::vec mu =  muV.subvec(0, para.p_z_alpha - 1);
    arma::vec Lvec = muV.subvec(mu.n_elem, muV.n_elem-1);

    arma::field<arma::vec> mu_f = vec_to_field(mu, para.p_z_vec_alpha);
    arma::field<arma::vec> Lvec_f = vec_to_field_L(Lvec, para.p_z_vec_alpha);

    int k=0;
    for(int j=0; j< para.alpha_idx.n_elem;j++){
        k = para.alpha_idx(j);
        para.mu(i,k) = mu_f(j);
        para.Lvec(i,k) = Lvec_f(j);
        arma::mat L = makeLowTriMat(para.V(i,k),  Lvec_f(j));
        para.V(i,k) = L*L.t();
    }
}

// combine parameters into vector //
// beta, gamma, weib //
arma::vec combinePara(const HDJM_data_t& data,
                      const HDJM_para_t& para){

    // create beta vector
    arma::field<arma::vec> beta_f(para.alpha_idx.n_elem);
    int k=0;
    for(int j=0; j< para.alpha_idx.n_elem;j++){
        k = para.alpha_idx(j);
        beta_f(j) = para.beta(k);
    }
    arma::vec beta_vec = field_to_vec(beta_f, para.p_x_vec_alpha);

    // create field of parameter vector
    arma::field<arma::vec> para_field(para.npara_vec.n_elem);
    para_field(0) = beta_vec;
    para_field(1) = para.gamma;
   // para_field(2) = para.alpha;
    // work on log-scale of weibull parameter to ensure nonnegativitity
    // remember to exp it after optimization //
    para_field(2) = arma::log(para.weib);

    // concatenate all parameter into one vector
    arma::vec para_all = field_to_vec(para_field, para.npara_vec);

    return para_all;
}

// to put the new updates into para //
void storePara(const arma::vec& para_all, const HDJM_data_t& data,
               HDJM_para_t& para){

    arma::field<arma::vec> para_field = vec_to_field(para_all, para.npara_vec);
    arma::field<arma::vec> beta = vec_to_field(para_field(0), para.p_x_vec_alpha);
    //para.beta = beta;
    int k=0;
    for(int j=0; j< para.alpha_idx.n_elem;j++){
        k = para.alpha_idx(j);
        para.beta(k) = beta(j);
    }

    para.gamma = para_field(1);
    //para.alpha = para_field(2);
    para_field(2) = arma::clamp(para_field(2), -MAX_EXP, MAX_EXP);
    para.weib = arma::exp(para_field(2));
}

// update parameters other than sig2, Sigma, alpha//

class updateParaFun{
public:
    const HDJM_data_t& data;
    const HDJM_para_t& para;

    updateParaFun(const HDJM_data_t& data,
                  const HDJM_para_t& para):
        data(data), para(para){
    }

    // Return the objective function with gradient.
    double EvaluateWithGradient(const arma::mat& para_all, arma::mat& g)
    {
        arma::field<arma::vec> para_field = vec_to_field(para_all, para.npara_vec);
        arma::field<arma::vec> beta_f = vec_to_field(para_field(0), para.p_x_vec_alpha);
        arma::vec gamma = para_field(1);
        arma::vec weib = arma::clamp(para_field(2), -MAX_EXP, MAX_EXP);

        //double c = weib(0);
        double d = weib(1);
        weib = arma::exp(weib);
        double lambda = weib(0);
        double theta = weib(1);

        arma::vec ELBO(data.n, arma::fill::zeros);
        arma::mat grad_beta(para.npara_vec(0), data.n, arma::fill::zeros);
        arma::mat grad_gamma(para.npara_vec(1), data.n, arma::fill::zeros);
        arma::mat grad_weib(para.npara_vec(2), data.n, arma::fill::zeros);

        int k =0;
        for(int i=0; i< data.n; i++){

            // function value
            for(int j=0; j< para.alpha_idx.n_elem;j++){
                k = para.alpha_idx(j);
                ELBO(i) -= 0.5*arma::accu(arma::square(
                    data.Y(i,k) - data.X(i,k)*beta_f(j) - data.Z(i,k) * para.mu(i,k)
                ))/para.sig2(k);
            }

            if(data.fstat(i) == 1){
                ELBO(i) += std::log(lambda) + (lambda-1)*std::log(data.ftime(i)) -
                    lambda*std::log(theta) +
                    arma::as_scalar(data.W.row(i) * gamma);

                for(int j=0; j< para.alpha_idx.n_elem;j++){
                    k = para.alpha_idx(j);
                    ELBO(i) += arma::accu(data.X_T(i,k)%beta_f(j)) *para.alpha(k);
                }
            }

            //Rcout << "113\n";
            arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * gamma);

            for(int j=0; j< para.alpha_idx.n_elem; j++){
                k = para.alpha_idx(j);
                h_it += data.X_t(i,k)*beta_f(j)*para.alpha(k) +
                    data.Z_t(i,k)*para.mu(i,k)*para.alpha(k);
            }

            for(int jj=0; jj < h_it.n_elem; jj++){
                for(int j=0; j<para.alpha_idx.n_elem; j++){
                    k = para.alpha_idx(j);
                    h_it(jj) +=  0.5 * para.alpha(k)*para.alpha(k) * arma::as_scalar(
                        data.Z_t(i, k).row(jj) *  para.V(i,k)*
                            data.Z_t(i, k).row(jj).t()
                    );
                }
            }
            h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
            h_it = arma::exp(h_it);

            ELBO(i) -=  arma::accu(data.GQ_w(i) % h_it);

            // gradient of beta //
            arma::field<arma::vec> grad_beta_f(beta_f.n_elem);
            for(int j=0; j< para.alpha_idx.n_elem; j++){
                k = para.alpha_idx(j);
                grad_beta_f(j) = data.X(i,k).t() * (
                    data.Y(i,k) - data.X(i,k)*beta_f(j) - data.Z(i,k) * para.mu(i,k)
                )/para.sig2(k);

                if(data.fstat(i)==1){
                    grad_beta_f(j) += para.alpha(k) * data.X_T(i,k);
                }
                grad_beta_f(j) -=
                    data.X_t(i,k).t() * (data.GQ_w(i) % h_it) * para.alpha(k);
            }

            grad_beta.col(i) = field_to_vec(grad_beta_f, para.p_x_vec_alpha);

            // gradient of gamma //
            grad_gamma.col(i) =  (data.fstat(i) -
                arma::accu(data.GQ_w(i) % h_it)) *
                data.W.row(i).t();

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

        double fval = -1*arma::accu(ELBO);

        arma::field<arma::vec> grad_field(3);
        grad_field(0) = arma::sum(grad_beta,1);
        grad_field(1) = arma::sum(grad_gamma,1);
        //grad_field(2) = arma::sum(grad_alpha,1);
        grad_field(2) = arma::sum(grad_weib,1);

        g.col(0) = -1* field_to_vec(grad_field, para.npara_vec);

        return fval;
    }

};

double get_lammax(const HDJM_data_t& data,  const HDJM_para_t& para,
                  const arma::vec& gvec){

    arma::mat h_t = arma::mat(data.GQ_t(0).n_elem,data.n, arma::fill::zeros);
    arma::mat XBZmu_T = arma::mat(data.n, data.K, arma::fill::zeros);
    arma::field<arma::vec> XBZmu_t = arma::field<arma::vec>(data.n, data.K);
    arma::field<arma::vec> ZVZ = arma::field<arma::vec>(data.n, data.K);
    arma::vec tmp(data.GQ_t(0).n_elem, arma::fill::zeros);

    for(int i=0; i< data.n; i++){
        for(int k=0; k<data.K;k++){
            XBZmu_T(i,k) = arma::accu(data.X_T(i,k)%para.beta(k))  +
                arma::accu(data.Z_T(i,k)%para.mu(i,k)) ;
            XBZmu_t(i,k) = data.X_t(i,k)*para.beta(k) +
                data.Z_t(i,k)*para.mu(i,k);
            for(int jj=0; jj<tmp.n_elem; jj++){

                tmp(jj) = arma::as_scalar(
                    data.Z_t(i, k).row(jj) *  para.V(i,k)*
                        data.Z_t(i, k).row(jj).t()
                );
            }
            ZVZ(i,k) = tmp;
        }
    }

    double lambda = para.weib(0);
    double theta = para.weib(1);
    int k=0;

    for(int i=0; i< data.n; i++){
        arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i) * para.gamma);
        for(int j=0; j< para.alpha_idx.n_elem; j++){
            k = para.alpha_idx(j);
            h_it += XBZmu_t(i,k) * para.alpha(k) +
                0.5 * para.alpha(k)*para.alpha(k) * ZVZ(i,k);
        }
        h_t.col(i) = h_it;
    }


    arma::vec grad(data.K,arma::fill::zeros);
    for(int k=0;k<data.K;k++){
        for(int i=0; i<data.n; i++){
            if(data.fstat(i)==1){
                grad(k) += XBZmu_T(i,k);
            }
            arma::vec h_it = h_t.col(i);
            h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
            h_it = arma::exp(h_it);
            grad(k) -= arma::accu(data.GQ_w(i) % h_it % XBZmu_t(i,k));
        }
    }

    return  (grad/gvec).max()*1.5;
}

// update alpha//
class updateAlphaFun{
public:
    const HDJM_data_t& data;
    const HDJM_para_t& para;
    arma::mat h_t;
    arma::mat XBZmu_T;
    arma::field<arma::vec> XBZmu_t;
    arma::field<arma::vec> ZVZ;
    int k_now = 0;

    double lam;
    double ridge;
    const arma::vec& gvec;

    updateAlphaFun(const HDJM_data_t& data,
                   const HDJM_para_t& para,
                   const arma::vec& gvec):
        data(data), para(para), gvec(gvec){

        h_t = arma::mat(data.GQ_t(0).n_elem,data.n, arma::fill::zeros);
        XBZmu_T = arma::mat(data.n, data.K, arma::fill::zeros);
        XBZmu_t = arma::field<arma::vec>(data.n, data.K);
        ZVZ = arma::field<arma::vec>(data.n, data.K);
        arma::vec tmp(data.GQ_t(0).n_elem, arma::fill::zeros);

        for(int i=0; i< data.n; i++){
            for(int k=0; k<data.K;k++){
                XBZmu_T(i,k) = arma::accu(data.X_T(i,k)%para.beta(k))  +
                    arma::accu(data.Z_T(i,k)%para.mu(i,k)) ;
                XBZmu_t(i,k) = data.X_t(i,k)*para.beta(k) +
                    data.Z_t(i,k)*para.mu(i,k);
                for(int jj=0; jj<tmp.n_elem; jj++){
                    tmp(jj) = arma::as_scalar(
                        data.Z_t(i, k).row(jj) *  para.V(i,k)*
                            data.Z_t(i, k).row(jj).t()
                    );
                }
                ZVZ(i,k) = tmp;
            }
        }
    }

    void initiate(){

        int k=0;
        if(para.alpha_idx.n_elem>0){
            arma::vec tmp(data.GQ_t(0).n_elem, arma::fill::zeros);
            for(int i=0; i<data.n; i++){
                for(int j=0; j< para.alpha_idx.n_elem; j++){
                    k = para.alpha_idx(j);
                    XBZmu_T(i,k) = arma::accu(data.X_T(i,k)%para.beta(k))  +
                        arma::accu(data.Z_T(i,k)%para.mu(i,k)) ;
                    XBZmu_t(i,k) = data.X_t(i,k)*para.beta(k) +
                        data.Z_t(i,k)*para.mu(i,k);
                    for(int jj=0; jj<tmp.n_elem; jj++){
                        tmp(jj) = arma::as_scalar(
                            data.Z_t(i, k).row(jj) *  para.V(i,k)*
                                data.Z_t(i, k).row(jj).t()
                        );
                    }
                    ZVZ(i,k) = tmp;
                }
            }
        }

        double lambda = para.weib(0);
        double theta = para.weib(1);
        //int k=0;

        for(int i=0; i< data.n; i++){

            arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * para.gamma);

            for(int j=0; j< para.alpha_idx.n_elem; j++){
                k = para.alpha_idx(j);
                h_it += XBZmu_t(i,k) * para.alpha(k) +
                    0.5 * para.alpha(k)*para.alpha(k) * ZVZ(i,k);
            }

            h_t.col(i) = h_it;
        }
    }

    void RemoveAdd(bool remove=true){

        for(int i=0; i< data.n; i++){
            arma::vec h_it = para.alpha(k_now)*XBZmu_t(i,k_now) +
                0.5 * para.alpha(k_now)*para.alpha(k_now) * ZVZ(i,k_now);

            if(remove){
                h_t.col(i) -= h_it;
            }else{
                h_t.col(i) += h_it;
            }
        }
    }

    // calculate the gradient for all k's
    // to determine the largest lasso lambda
    arma::vec gradKKT_all(){
        arma::vec grad(data.K,arma::fill::zeros);
        for(int k=0;k<data.K;k++){
            for(int i=0; i<data.n; i++){
                if(data.fstat(i)==1){
                    grad(k) += XBZmu_T(i,k);
                }
                arma::vec h_it = h_t.col(i);
                h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
                h_it = arma::exp(h_it);
                grad(k) -= arma::accu(data.GQ_w(i) % h_it % XBZmu_t(i,k));
            }
        }
        return grad;
    }
    // calculate the gradient for k_now
    double gradKKT(){
        double grad=0;
        for(int i=0; i<data.n; i++){
            if(data.fstat(i)==1){
                grad += XBZmu_T(i,k_now);
            }
            arma::vec h_it = h_t.col(i);
            h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
            h_it = arma::exp(h_it);
            grad -= arma::accu(data.GQ_w(i) %h_it % XBZmu_t(i,k_now));
        }
        return grad;
    }

    // Return the objective function with gradient.
    double EvaluateWithGradient(const arma::mat& para, arma::mat& g)
    {
        double alpha_k = para(0,0);

        double fval = 0;
        double grad = 0;
        for(int i=0; i<data.n; i++){
            if(data.fstat(i)==1){
                fval += XBZmu_T(i,k_now)*alpha_k;
                grad += XBZmu_T(i,k_now);
            }
            arma::vec h_it = h_t.col(i);
            h_it += alpha_k * XBZmu_t(i,k_now) +
                0.5*alpha_k*alpha_k*  ZVZ(i, k_now);
            h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
            h_it = arma::exp(h_it);

            arma::vec tmp = XBZmu_t(i,k_now) + alpha_k * ZVZ(i, k_now);

            fval -= arma::accu(data.GQ_w(i) % h_it);
            grad -= arma::accu(data.GQ_w(i) % h_it%tmp);
        }

        // penalty part
        if(alpha_k>0){
            fval -= lam*gvec(k_now)*alpha_k + 0.5*ridge*alpha_k*alpha_k;
            grad -= lam*gvec(k_now) + alpha_k*ridge;
        }else{
            fval -= -lam*gvec(k_now)*alpha_k + 0.5*ridge*alpha_k*alpha_k;
            grad -= -lam*gvec(k_now) + alpha_k*ridge;
        }

        fval = -fval;
        g(0,0) = -grad;

        return fval;
    }

};

// update sig2 and Sigma //
void updateSig(const HDJM_data_t& data,
               HDJM_para_t& para){

    int k =0,N=0;
    if(para.alpha_idx.n_elem > 0){
        for(int j=0; j< para.alpha_idx.n_elem;j++){
            k = para.alpha_idx(j);
            para.sig2(k) = 0;
            para.Sigma(k).zeros();
            N = 0;
            for(int i=0; i< data.n; i++){

                N += data.Y(i,k).n_elem;
                para.Sigma(k) += para.mu(i,k) *   para.mu(i,k).t() + para.V(i,k);
                para.sig2(k) +=  arma::accu(arma::square(
                    data.Y(i,k) - data.X(i,k)*para.beta(k) - data.Z(i,k) * para.mu(i,k)
                )) +  arma::trace(
                        data.Z(i,k) * para.V(i,k) * data.Z(i,k).t()
                );
            }
            para.sig2(k) /= N;
            para.Sigma(k) /= data.n;
            para.invSigma(k) = myinvCpp(para.Sigma(k));
        }
    }
}


// calculate baseline ELBO for biomarkers with zero alphas //
arma::mat calcELBO_baseLME(const HDJM_data_t& data,
                           const HDJM_para_t& para){

    //double ELBO=0;
    arma::mat ELBO(data.n, data.K, arma::fill::zeros);

    //const double lambda = para.weib(0);
    //const double theta = para.weib(1);
    //arma::vec N(data.K, arma::fill::zeros);
    arma::vec logsig2 = arma::log(para.sig2);

    for(int i=0; i< data.n; i++){
        for(int k=0; k<data.K; k++){
            //N(k) += data.Y(i,k).n_elem;
            //int n_i = data.Y(i,k).n_elem;
            ELBO(i,k) -= 0.5*data.Y(i,k).n_elem * logsig2(k);
            //end = start+data.p_z_vec(k)-1;
            ELBO(i,k) -= 0.5*arma::accu(arma::square(
                data.Y(i,k) - data.X(i,k)*para.beta(k) - data.Z(i,k) * para.mu(i,k)
            ))/para.sig2(k) +  0.5*arma::trace(
                    data.Z(i,k) * para.V(i,k) * data.Z(i,k).t()
            )/para.sig2(k);

        }

        double val, sign;
        for(int k=0; k< data.K; k++){

            arma::log_det(val, sign, para.Sigma(k));
            ELBO(i,k) -= 0.5 * val;

            ELBO(i,k) -= 0.5 * arma::as_scalar(para.mu(i,k).t()*para.invSigma(k)*
                para.mu(i,k));
            ELBO(i,k) -= 0.5 * arma::trace(para.invSigma(k) * para.V(i,k));

            arma::log_det(val, sign, para.V(i,k));
            ELBO(i,k) += 0.5 * val;
        }
    }

    return ELBO;
}


// calculate ELBO //
double calcELBO(const HDJM_data_t& data, const HDJM_para_t& para,
                const arma::mat& baseELBO,
                const arma::vec&  gvec, double lam, double ridge){

    //double ELBO=0;
    arma::mat ELBO = baseELBO;
    arma::vec ELBO_surv(data.n,arma::fill::zeros);

    const double lambda = para.weib(0);
    const double theta = para.weib(1);
    int k =0;
    double val, sign;

    // reset the columns for nonzero alpha_k
    for(int j=0; j< para.alpha_idx.n_elem; j++){
        k = para.alpha_idx(j);
        ELBO.col(k).zeros();
    }
    arma::vec logsig2 = arma::log(para.sig2);

    for(int i=0; i< data.n; i++){

        // int n_i = data.Y(i,0).n_elem;
        // int start = 0, end=0;
        // Longitudinal part
        for(int j=0; j< para.alpha_idx.n_elem; j++){
            k = para.alpha_idx(j);
            //end = start+data.p_z_vec(k)-1;
            ELBO(i,k) -= 0.5 * logsig2(k) * data.Y(i,k).n_elem;
            ELBO(i,k) -= 0.5*arma::accu(arma::square(
                data.Y(i,k) - data.X(i,k)*para.beta(k) - data.Z(i,k) * para.mu(i,k)
            ))/para.sig2(k) +  0.5*arma::trace(
                    data.Z(i,k) * para.V(i,k) * data.Z(i,k).t()
            )/para.sig2(k);
            //start = end + 1;

            arma::log_det(val, sign, para.Sigma(k));
            ELBO(i,k) -= 0.5 * val;

            ELBO(i,k) -= 0.5 * arma::as_scalar(para.mu(i,k).t()*para.invSigma(k)*
                para.mu(i,k));
            ELBO(i,k) -= 0.5 * arma::trace(para.invSigma(k) * para.V(i,k));

            arma::log_det(val, sign, para.V(i,k));
            ELBO(i,k) += 0.5 * val;
        }

        // Survival part
        if(data.fstat(i) == 1){
            ELBO_surv(i) += std::log(lambda) + (lambda-1)*std::log(data.ftime(i)) -
                lambda*std::log(theta) +
                arma::as_scalar(data.W.row(i) * para.gamma);

            for(int j=0; j< para.alpha_idx.n_elem; j++){
                k = para.alpha_idx(j);
                ELBO_surv(i) += arma::accu(data.X_T(i,k)%para.beta(k)) *para.alpha(k) +
                    arma::accu(data.Z_T(i,k)%para.mu(i,k)) *para.alpha(k);
            }
        }

        arma::vec h_it = std::log(lambda) + (lambda-1)*arma::log(data.GQ_t(i)) -
            lambda*std::log(theta) +
            arma::as_scalar(data.W.row(i) * para.gamma);

        for(int j=0; j< para.alpha_idx.n_elem; j++){
            k = para.alpha_idx(j);
            h_it += data.X_t(i,k)*para.beta(k)*para.alpha(k) +
                data.Z_t(i,k)*para.mu(i,k)*para.alpha(k);
        }

        for(int jj=0; jj < h_it.n_elem; jj++){
            for(int j=0; j<para.alpha_idx.n_elem; j++){
                k = para.alpha_idx(j);
                h_it(jj) +=  0.5 * para.alpha(k)*para.alpha(k) * arma::as_scalar(
                    data.Z_t(i, k).row(jj) *  para.V(i,k)*
                        data.Z_t(i, k).row(jj).t()
                );
            }
        }

        h_it = arma::clamp(h_it, -MAX_EXP, MAX_EXP);
        h_it = arma::exp(h_it);

        ELBO_surv(i) -=  arma::accu(data.GQ_w(i) % h_it);
    }

    double res = arma::accu(ELBO) + arma::accu(ELBO_surv) -
        lam*arma::accu(gvec%arma::abs(para.alpha)) -
        0.5*ridge*arma::accu(arma::square(para.alpha));
    //res = res/data.n/data.K;

    return res;
    // return arma::accu(ELBO) + arma::accu(ELBO_surv) -
    //     lam*arma::accu(gvec%arma::abs(para.alpha)) -
    //     0.5*ridge*arma::accu(arma::square(para.alpha));
}


// combine parameters into vector //
// beta, gamma, weib, alpha //
arma::vec combinePara_all(const HDJM_data_t& data,
                          const HDJM_para_t& para){

    // create beta vector
    //arma::field<arma::vec> beta_f(para.alpha_idx.n_elem);
    // int k=0;
    //for(int j=0; j< para.alpha_idx.n_elem;j++){
    //    k = para.alpha_idx(j);
    //    beta_f(j) = para.beta(k);
    //}
    //arma::vec beta_vec = field_to_vec(beta_f, para.p_x_vec_alpha);
    arma::vec beta_vec = field_to_vec(para.beta, data.p_x_vec);

    // create field of parameter vector
    //arma::field<arma::vec> para_field(para.npara_vec.n_elem);
    //para_field(0) = beta_vec;
    //para_field(1) = para.gamma;
    // para_field(2) = para.alpha;
    // work on log-scale of weibull parameter to ensure nonnegativitity
    // remember to exp it after optimization //
    arma::vec log_weib = arma::log(para.weib);

    // concatenate all parameter into one vector
    //arma::vec para_all = field_to_vec(para_field, para.npara_vec);

    arma::vec para_all_2  = arma::join_cols(beta_vec, para.gamma,para.alpha,
                                            log_weib);

    return para_all_2;
}

//' Main function to run HDJM given one lasso penalty
//' @noRd
//'
// [[Rcpp::export]]
List HDJM(const List& datalist, const List& paralist,
          const arma::vec& gvec, double lam, double ridge,
          int maxiter = 100, double eps=1e-4){

    //Rcout << "1\n";
    HDJM_data_t data(datalist);
    HDJM_para_t para(paralist);
    para.NonZeroAlpha();

    ens::L_BFGS lbfgs;
    //lbfgs.MinGradientNorm() = eps;
    //lbfgs.MaxIterations() = 5;

    updateMuVFun MuV_fun(data, para);
    updateParaFun Para_fun(data,  para);
    updateAlphaFun Alpha_fun(data,  para, gvec);
    Alpha_fun.lam = lam;
    Alpha_fun.ridge = ridge;
    Alpha_fun.initiate();

    // arma::vec grad_all  = Alpha_fun.gradKKT_all();
    // double lam_max = (grad_all/gvec).max();
    // Rcout << "lam_max: " << lam_max << "\n";

    arma::mat ELBO_base = calcELBO_baseLME(data, para);
    double ELBO = calcELBO(data, para, ELBO_base, gvec, lam, ridge);
    arma::vec ELBO_vec(maxiter);
    int iter;
    //Rcout << ELBO << "\n";

    double grd = 0;
    arma::mat alpha_k(1,1);
    arma::vec alpha_vec = para.alpha;

    arma::vec para_pre = combinePara_all(data, para);
    arma::vec para_aft = combinePara_all(data, para);

    for(iter=0;iter<maxiter;iter++){

        para_pre = combinePara_all(data, para);

        // update alpha's
        Alpha_fun.initiate();
        for(int j = 0;j<maxiter; j++){
            alpha_vec = para.alpha;
            for(int k=0; k<data.K; k++){
                Alpha_fun.k_now = k;
                if(para.alpha(k) !=0){
                    Alpha_fun.RemoveAdd(true);
                }
                grd = Alpha_fun.gradKKT();
                if(std::fabs(grd) < lam*gvec(k)){
                    alpha_k(0,0) = 0;
                }else{
                    alpha_k(0,0) = para.alpha(k);
                    lbfgs.Optimize(Alpha_fun,alpha_k);
                }
                para.alpha(k) = alpha_k(0,0);

                if(para.alpha(k) !=0){
                    Alpha_fun.RemoveAdd(false);
                }
            }
            double err_alpha = std::sqrt(arma::accu(arma::square(alpha_vec-para.alpha)));
            if(err_alpha<eps){
                break;
            }
        }
        para.NonZeroAlpha();
        //ELBO = calcELBO(data, para, ELBO_base, gvec, lam, ridge);
        //Rcout << ELBO << "\n";

        // update other parameters
        if(para.alpha_idx.n_elem>0){
            // update paramters other than sig2 and Sigma
            arma::vec para_all = combinePara(data, para);
            //arma::vec para_all_prev = para_all;
            lbfgs.Optimize(Para_fun, para_all);
            storePara( para_all, data, para);

            //ELBO = calcELBO(data, para, ELBO_base, gvec, lam, ridge);
            //Rcout << ELBO << "\n";

            // update sig2 and Sigma
            updateSig(data,  para);
            //ELBO = calcELBO(data, para, ELBO_base, gvec, lam, ridge);
            //Rcout << ELBO << "\n";

            // update V and mu -- variational para
            MuV_fun.initiate();
            for(int i=0; i < data.n; i++){
                MuV_fun.i_now = i;
                MuV_fun.updateIntermediate();
                arma::vec muV = combineMuV(data, para, i);
                lbfgs.Optimize(MuV_fun,muV);
                storeMuV(data, para,  muV, i);
            }
        }

        para_aft = combinePara_all(data, para);
        //ELBO = calcELBO(data, para, ELBO_base, gvec, lam, ridge);
        //Rcout << ELBO << "\n";
        //ELBO_vec(iter) = ELBO;
        //double err_ELBO = (ELBO_vec(iter)-ELBO_vec(iter-1))/ELBO_vec(iter-1);
        //double err_ELBO = (ELBO_vec(iter)-ELBO_vec(iter-1))/data.n/data.K;

        double err_para = std::sqrt(arma::accu(arma::square(para_aft-
                                    para_pre))/para_pre.n_elem);
        if(err_para < eps){
            break;
        }
    }

    //ELBO = calcELBO(data, para, ELBO_base, gvec, 0, 0);
    //Rcout << ELBO << "\n";

    return List::create(
        _["sig2"] = para.sig2,
        _["Sigma"] = para.Sigma,
        _["alpha"] = para.alpha,
        _["beta"] = para.beta,
        _["weib"] = para.weib,
        _["gamma"] = para.gamma,
        _["mu"] = para.mu,
        _["V"] = para.V,
        _["ELBO_vec"] = ELBO_vec,
        _["iter"] = iter,
        _["ELBO"] = ELBO
    );
}


// base function for running a sequence of lambdas //
double HDJM_base(const HDJM_data_t& data, HDJM_para_t& para,
                 const arma::mat& ELBO_base,
                 const arma::vec& gvec, double lam, double ridge,
                 int maxiter = 100, double eps=1e-4){

    para.NonZeroAlpha();

    ens::L_BFGS lbfgs;
    //lbfgs.MinGradientNorm() = eps;

    updateMuVFun MuV_fun(data, para);
    updateParaFun Para_fun(data,  para);
    updateAlphaFun Alpha_fun(data,  para, gvec);
    Alpha_fun.lam = lam;
    Alpha_fun.ridge = ridge;
    Alpha_fun.initiate();

    double ELBO;
    //arma::vec ELBO_vec(maxiter);
    int iter;
    double grd = 0;
    arma::mat alpha_k(1,1);
    arma::vec alpha_vec = para.alpha;

    arma::vec para_pre = combinePara_all(data, para);
    arma::vec para_aft = combinePara_all(data, para);

    for(iter=0;iter<maxiter;iter++){

        para_pre = combinePara_all(data, para);

        // update alpha
        Alpha_fun.initiate();

        for(int j = 0;j<maxiter; j++){
            alpha_vec = para.alpha;
            for(int k=0; k<data.K; k++){
                Alpha_fun.k_now = k;
                if(para.alpha(k) !=0){
                    Alpha_fun.RemoveAdd(true);
                }
                grd = Alpha_fun.gradKKT();
                if(std::fabs(grd) < lam*gvec(k)){
                    alpha_k(0,0) = 0;
                }else{
                    alpha_k(0,0) = para.alpha(k);
                    lbfgs.Optimize(Alpha_fun,alpha_k);
                }
                para.alpha(k) = alpha_k(0,0);

                if(para.alpha(k) !=0){
                    Alpha_fun.RemoveAdd(false);
                }
            }
            double err_alpha = std::sqrt(arma::accu(arma::square(alpha_vec-para.alpha)));
            if(err_alpha<eps){
                break;
            }
        }
        para.NonZeroAlpha();
        //ELBO = calcELBO(data, para, ELBO_base, gvec, lam, ridge);
        //Rcout << ELBO << "\n";

        // update other parameters
        if(para.alpha_idx.n_elem > 0){
            //Rcout << "6\n";
            // update paramters other than sig2 and Sigma
            arma::vec para_all = combinePara(data, para);
            //arma::vec para_all_prev = para_all;
            lbfgs.Optimize(Para_fun, para_all);
            storePara( para_all, data, para);

            //ELBO = calcELBO(data, para, ELBO_base, gvec, lam, ridge);
            //Rcout << ELBO << "\n";

            //Rcout << "7\n";
            // update sig2 and Sigma
            updateSig(data,  para);
            //ELBO = calcELBO(data, para, ELBO_base, gvec, lam, ridge);
            //Rcout << ELBO << "\n";

            // update V and mu -- variational para
            //Rcout << "8\n";

            MuV_fun.initiate();
            for(int i=0; i < data.n; i++){
                MuV_fun.i_now = i;
                MuV_fun.updateIntermediate();
                arma::vec muV = combineMuV(data, para, i);
                lbfgs.Optimize(MuV_fun,muV);
                storeMuV(data, para,  muV, i);
            }
        }

        para_aft = combinePara_all(data, para);

        double err_para = std::sqrt(arma::accu(arma::square(para_aft-
                                    para_pre))/para_pre.n_elem);
        if(err_para < eps){
            break;
        }
    }

    ELBO = calcELBO(data, para, ELBO_base, gvec, 0, 0);
    //Rcout <<"#iter = "<<iter << "; ELBO = "<< ELBO << "\n";

    return ELBO;
}

//' Main function to run HDJM for a sequence of lasso penalties
//' @noRd
//'
// [[Rcpp::export]]
List HDJM_seq(const List& datalist, const List& paralist,
              const arma::vec& gvec, int nlam, double ridge, int pmax,
              const double min_ratio=0.001, const int maxiter=100,
              const double eps=1e-4, const bool UseSurvN=true){

    //Rcout << "1\n";
    HDJM_data_t data(datalist);
    HDJM_para_t para(paralist);
    para.NonZeroAlpha();

    //Rcout << "2\n";
    double lam_max = get_lammax(data, para, gvec);
    //Rcout << "3\n";
    //Rcout << "lam_max: " << lam_max << "\n";
    arma::vec lam_seq = arma::exp(arma::linspace(
        std::log(lam_max), std::log(lam_max*min_ratio), nlam ));

    //Rcout << "lam_seq: " << lam_seq << "\n";

    arma::mat ELBO_base = calcELBO_baseLME(data, para);

    // object to store the results
    arma::mat alpha_mat(data.K, nlam, arma::fill::zeros);
    arma::vec ELBO_vec(nlam, arma::fill::zeros);
    arma::vec BIC(nlam, arma::fill::zeros);

    arma::field<arma::vec> mu_f(para.mu.n_rows, para.mu.n_cols,nlam); // n \times K vec
    arma::field<arma::mat> V_f(para.V.n_rows, para.V.n_cols, nlam); // n \times K mat
    arma::field<arma::vec> beta_f(para.beta.n_rows, nlam); // K \times 1 vec
    arma::mat sig2_mat(para.sig2.n_elem, nlam); // dim = K
    arma::field<arma::mat> Sigma_f(para.Sigma.n_rows, nlam); // K \times 1 mat
    arma::mat gamma_mat(para.gamma.n_elem, nlam); // dim = p_w
    arma::mat weib_mat(para.weib.n_elem, nlam);


    // run the algorithm
    int j=0;
    double eff_sam;
    if(UseSurvN){
        eff_sam = arma::accu(data.fstat);
    }else{
        eff_sam = data.ftime.n_elem*1.0;
    }
    //double eff_sam = arma::accu(data.fstat);
    //double eff_sam = data.ftime.n_elem*1.0;

    for(j=0; j<nlam; j++){
        double lam = lam_seq(j);
        //Rcout << j+1 << "th lam=" << lam <<"\n";
        ELBO_vec(j) = HDJM_base(data,  para, ELBO_base, gvec,  lam,  ridge,
                  maxiter, eps);

        alpha_mat.col(j) = para.alpha;
        gamma_mat.col(j) = para.gamma;
        weib_mat.col(j) = para.weib;
        sig2_mat.col(j) = para.sig2;
        mu_f.slice(j) = para.mu;
        V_f.slice(j) = para.V;
        beta_f.col(j) = para.beta;
        Sigma_f.col(j) = para.Sigma;

        BIC(j) = -2*ELBO_vec(j) + std::log(eff_sam) * para.alpha_idx.n_elem;

        if(para.alpha_idx.n_elem > pmax){
            break;
        }
    }

    if(j+1 < nlam){
        alpha_mat.shed_cols(j+1, nlam-1);
        //gamma_mat.shed_cols(j+1, nlam-1);
        //weib_mat.shed_cols(j+1, nlam-1);
        //sig2_mat.shed_cols(j+1, nlam-1);
        ELBO_vec.shed_rows(j+1, nlam-1);
        BIC.shed_rows(j+1, nlam-1);
        lam_seq.shed_rows(j+1, nlam-1);
    }

    arma::uword ind_min = BIC.index_min();

    para.mu = mu_f.slice(ind_min);
    para.V = V_f.slice(ind_min);
    para.Sigma = Sigma_f.col(ind_min);
    para.sig2 = sig2_mat.col(ind_min);
    para.alpha = alpha_mat.col(ind_min);
    para.beta =  beta_f.col(ind_min);
    para.weib = weib_mat.col(ind_min);
    para.gamma = gamma_mat.col(ind_min);

    return List::create(
        _["alpha_mat"] = alpha_mat,
        _["ELBO"] = ELBO_vec,
        _["BIC"] = BIC,
        _["lam_seq"] = lam_seq,
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

//' Main function to predict random effects in HDJM
//' @noRd
//'
// [[Rcpp::export]]
List HDJM_raneff(const List& datalist, const List& paralist, double eps=1e-4){


    HDJM_data_t data(datalist);
    HDJM_para_t para(paralist);
    para.NonZeroAlpha();

    if(para.alpha_idx.n_elem > 0){

        ens::L_BFGS lbfgs;
        //lbfgs.MinGradientNorm() = eps;
        updateMuVFun MuV_fun(data, para);
        MuV_fun.initiate();
        for(int i=0; i < data.n; i++){
            MuV_fun.i_now = i;
            MuV_fun.updateIntermediate();
            arma::vec muV = combineMuV(data, para, i);
            lbfgs.Optimize(MuV_fun,muV);
            storeMuV(data, para,  muV, i);
        }

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

// // [[Rcpp::export]]
// void test_size(arma::vec bb, arma::mat AA, arma::field<arma::vec> KK){
//     Rcout << bb.size() << "\n";
//     Rcout << bb.n_elem << "\n";
//     Rcout << arma::size(bb) << "\n";
//     Rcout << arma::size(AA) << "\n";
//     Rcout << AA.size() << "\n";
//     Rcout << AA.n_elem << "\n";
//     Rcout << KK.size() << "\n";
//     Rcout << KK.n_elem << "\n";
// }



