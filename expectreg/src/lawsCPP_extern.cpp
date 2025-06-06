#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using namespace Eigen;
using namespace Rcpp;
using namespace std;


// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
Eigen::MatrixXd AAt(const Eigen::MatrixXd& A) {
    using   Eigen::LLT;
    using   Eigen::Lower;
    using   Eigen::Map;
    using   Eigen::MatrixXd;
    using   Eigen::MatrixXi;
    using   Eigen::Upper;
    using   Eigen::VectorXd;
    using   Eigen::VectorXi;
    //typedef Map<MatrixXd>  MapMatd;
    //typedef Map<MatrixXi>  MapMati;
    //typedef Map<Eigen::VectorXd>  MapVecd;
    //typedef Map<VectorXi>  MapVeci;
    
    int    n(A.cols());
    return   MatrixXd(n,n).setZero().selfadjointView<Lower>()
                          .rankUpdate(A);
}

// [[Rcpp::export]]
Eigen::MatrixXd AtA(const Eigen::MatrixXd& A) {
    using   Eigen::LLT;
    using   Eigen::Lower;
    using   Eigen::Map;
    using   Eigen::MatrixXd;
    using   Eigen::MatrixXi;
    using   Eigen::Upper;
    using   Eigen::VectorXd;
    using   Eigen::VectorXi;
    //typedef Map<MatrixXd>  MapMatd;
    //typedef Map<MatrixXi>  MapMati;
    //typedef Map<Eigen::VectorXd>  MapVecd;
    //typedef Map<VectorXi>  MapVeci;
    
    int    n(A.cols());
    return   MatrixXd(n,n).setZero().selfadjointView<Lower>()
                          .rankUpdate(A.adjoint());
}

// [[Rcpp::export]]
List lltLS(const    Eigen::Map<Eigen::VectorXd>&    y,
           const   Eigen::MatrixXd&   B,
                  const   double&     tau,
                  const   Eigen::Map<Eigen::VectorXd>&    lambdashort_glatt,
                  const   Eigen::Map<Eigen::VectorXd>&    lambdashort_orig,
                  Eigen::MatrixXd    DD,
                  const   Eigen::Map<Eigen::VectorXi>&    NB,
                  const   Eigen::Map<Eigen::VectorXi>&    glatterms) {
    
    using   Eigen::LLT;
    using   Eigen::Lower;
    using   Eigen::Map;
    using   Eigen::MatrixXd;
    using   Eigen::MatrixXi;
    using   Eigen::Upper;
    using   Eigen::VectorXd;
    using   Eigen::VectorXi;
    //typedef Map<MatrixXd>  MapMatd;
    //typedef Map<MatrixXi>  MapMati;
    //typedef Map<Eigen::VectorXd>  MapVecd;
    //typedef Map<VectorXi>  MapVeci;
    
    VectorXd lambdalong(NB.sum()+1);
    lambdalong.fill(1);
    int lli = 0;
    for(int li=0;li<B.cols()-NB.sum();li++){
        lambdalong[li] = 0;
        lli++;
    }
    VectorXd lambdashort = lambdashort_orig;
    
    for(int k=0; k < glatterms.size(); k++) {
        if(NB(glatterms(k)-1) != 0) {
            lambdashort(glatterms(k)-1) = lambdashort_glatt(k);
        } 
    }
    for(int nbi=0;nbi<NB.size();nbi++){
        for(int li=0;li<NB[nbi];li++){
            lambdalong[lli] = lambdashort[nbi];
            lli++;
        }
    }
    
    for(int i=0; i<DD.cols();i++)
        DD.col(i) *= sqrt(lambdalong[i]);
    
    MatrixXd X (B.rows()+DD.rows(),B.cols());
    X << B,DD;

    // Start Merle
    VectorXd yExtended = VectorXd::Zero(X.rows());

    for (int i = 0; i < y.size(); i++) {
        yExtended[i] = y[i];
    }
    // end

    const int n(B.rows());
    
    LLT<MatrixXd> llt(AtA(X));
    
    VectorXd  betahat(llt.solve(X.adjoint() * yExtended)); //Merle
    
    VectorXd  fitted(X * betahat);
    
    VectorXd weight(X.rows());
    
    weight.fill(1);
    
    
    for(int i=0; i < n; i++)
        if(yExtended[i] < fitted[i]) //Merle
            weight[i] = 1 - tau;
        else
            weight[i] = tau;
        
        //return List::create(Named("weights") =weight);
        
        //double dw = 1;
        int iter = 1;
        VectorXd wold = weight;
        wold[1]++;
        while((wold - weight).norm() != 0 && iter < 50)
        {
            checkUserInterrupt();
            
            llt.compute(X.adjoint() * weight.asDiagonal() * X);
            betahat = llt.solve(X.adjoint() * weight.asDiagonal() * yExtended); //Merle
            
            iter++;
            
            wold = weight;
            fitted = X*betahat;
            
            for(int i=0;i<n;i++)
                if(yExtended[i] < fitted[i]) //Merle
                    weight[i] = 1 - tau;
                else
                    weight[i] = tau;
                
        }
        
        return List::create(Named("a") =betahat,Named("weight") =weight.head(n),Named("fitted") = fitted.head(n),Named("diag.hat.ma") = (X*llt.solve(X.adjoint() * weight.asDiagonal())).diagonal().head(n));
}


// [[Rcpp::export]]
double smoothCPPP(const    Eigen::Map<Eigen::VectorXd>&    y,
                  const   Eigen::MatrixXd&   B,
                  const   double&     tau,
                  const   Eigen::Map<Eigen::VectorXd>&    lambdashort_glatt,
                  const   Eigen::Map<Eigen::VectorXd>&    lambdashort_orig,
                  Eigen::MatrixXd    DD,
                  const   Eigen::Map<Eigen::VectorXi>&    NB,
                  const   Eigen::Map<Eigen::VectorXi>&    glatterms, 
                  std::string smoothtype){
    using   Eigen::LLT;
    using   Eigen::Lower;
    using   Eigen::Map;
    using   Eigen::MatrixXd;
    using   Eigen::MatrixXi;
    using   Eigen::Upper;
    using   Eigen::VectorXd;
    using   Eigen::VectorXi;
    //typedef Map<MatrixXd>  MapMatd;
    //typedef Map<MatrixXi>  MapMati;
    //typedef Map<Eigen::VectorXd>  MapVecd;
    //typedef Map<VectorXi>  MapVeci;
    
    VectorXd lambdalong(NB.sum()+1);
    lambdalong.fill(1);
    int lli = 0;
    for(int li=0;li<B.cols()-NB.sum();li++){
        lambdalong[li] = 0;
        lli++;
    }
    VectorXd lambdashort = lambdashort_orig;
    
    for(int k=0; k < glatterms.size(); k++) {
        if(NB(glatterms(k)-1) != 0) {
            lambdashort(glatterms(k)-1) = lambdashort_glatt(k);
        } 
    }
    for(int nbi=0;nbi<NB.size();nbi++){
        for(int li=0;li<NB[nbi];li++){
            lambdalong[lli] = lambdashort[nbi];
            lli++;
        }
    }
    /*Rcout << lambdalong << std::endl;
     Rcout << lambdashort << std::endl;
     Rcout << std::endl;*/
    
    for(int i=0; i<DD.cols();i++)
        DD.col(i) *= sqrt(lambdalong[i]);
    
    MatrixXd X (B.rows()+DD.rows(),B.cols());
    X << B,DD;

    // Start Merle
    VectorXd yExtended = VectorXd::Zero(X.rows());

    for (int i = 0; i < y.size(); i++) {
        yExtended[i] = y[i];
    }
    // end

    const int n(B.rows());
    
    LLT<MatrixXd> llt(AtA(X));
    
    VectorXd  betahat(llt.solve(X.adjoint() * yExtended)); //Merle
    
    VectorXd  fitted(X * betahat);
    
    VectorXd weight(X.rows());
    
    weight.fill(1);
    
    
    for(int i=0;i<n;i++)
        if(yExtended[i] < fitted[i]) //Merle
            weight[i] = 1 - tau;
        else
            weight[i] = tau;
        
        //return List::create(Named("weights") =weight);
        
        //double dw = 1;
        int iter = 1;
        VectorXd wold = weight;
        wold[1]++;
        while((wold - weight).norm() != 0 && iter < 50)
        {
            checkUserInterrupt();
            
            llt.compute(X.adjoint() * weight.asDiagonal() * X);
            betahat = llt.solve(X.adjoint() * weight.asDiagonal() * yExtended); //Merle
            
            iter++;
            
            wold = weight;
            fitted = X*betahat;
            
            for(int i=0; i < n; i++)
                if(yExtended[i] < fitted[i]) //Merle
                    weight[i] = 1 - tau;
                else
                    weight[i] = tau;
                
        }
        
        
        double score = 0;
        /*double tempscore1 = ((weight.head(n).array())*((pow((((y.head(n) - fitted.head(n)).array())),2)).array())).sum();
        double tempscore2 = log((((weight.head(n).array())*((pow((((y.head(n) - fitted.head(n)).array())),2)).array())).sum())/n)*n;
        double tempscore3 = 2*((X*llt.solve(X.adjoint() * weight.asDiagonal())).diagonal().head(n).sum());
        Rcout << tempscore1 << tempscore2 << tempscore3 << std::endl;*/
        if(smoothtype == "aic") {
            score = log((((weight.head(n).array())*((pow((((yExtended.head(n) - fitted.head(n)).array())),2)).array())).sum())/n)*n + 2*((X*llt.solve(X.adjoint() * weight.asDiagonal())).diagonal().head(n).sum()); //Merle
        }
        if(smoothtype == "bic") {
            score = log((((weight.head(n).array())*((pow((((yExtended.head(n) - fitted.head(n)).array())),2)).array())).sum())/n)*n + log(n)*((X*llt.solve(X.adjoint() * weight.asDiagonal())).diagonal().head(n).sum()); //Merle
        }
        if(smoothtype == "ocv") {
            score = ( (((weight.head(n).array())*((pow((((yExtended.head(n) - fitted.head(n)).array())),2)).array()))).array() /  //Merle
                          (pow(((1-((X*llt.solve(X.adjoint() * weight.asDiagonal())).diagonal().head(n)).array()).array()),2)).array() ).mean();
        }
        if(smoothtype == "gcv") {
            score = (((weight.head(n).array())*((pow((((yExtended.head(n) - fitted.head(n)).array())),2)).array())).mean()) * 1/pow(1-(1+(X*llt.solve(X.adjoint() * weight.asDiagonal())).diagonal().head(n).sum())/n,2); //Merle
        }
        
        if(!(score != score) && !(score==score)) {
            score = 1e50;
        }
        
        return (score);
}


