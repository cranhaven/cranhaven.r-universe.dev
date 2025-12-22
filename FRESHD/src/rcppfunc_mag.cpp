// //// [[Rcpp::depends(RcppArmadillo)]]
// #include <RcppArmadillo.h>
// #include "auxfunc.h"
//
// using namespace std;
// using namespace arma;
//
//
//
// /////////////////////////////////////////////////////////////////////////////////////////
// /////////////////////////////////// magging implementation /////////////////////////////
// /////////////////////////////////////////////////////////////////////////////////////////
// //[[Rcpp::export]]
// Rcpp::List maglas(arma::mat dims, int usex,
//                   arma::mat B, arma::mat Phi1, arma::mat Phi2, arma::mat Phi3,
//                   Rcpp::NumericVector resp,
//                   std::string penalty,
//                   double kappa,
//                   int usekappa,
//                   arma::vec lambda,
//                   int nlambda,
//                   int makelamb,
//                   double lambdaminratio,
//                   arma::mat penaltyfactor,
//                   int steps){
//
//   Rcpp::List output;
//   Rcpp::NumericVector vecresp(resp);
//   Rcpp::IntegerVector respDim = vecresp.attr("dim");
//   const arma::cube Y(vecresp.begin(), respDim[0], respDim[1], respDim[2], false);
//
//
//   int Tf=2,stopsparse = 0,
//     endmodelno = nlambda,
//     n1 = dims(0,0), n2 = dims(1,0), n3 = dims(2,0),
//     Nog = Y.n_slices, ng = n1 * n2 * n3,
//     p1 = dims(0,1), p2 = dims(1,1), p3 = dims(2,1),
//     p = p1 * p2 * p3, n=n1*n2*n3  ;
//
//   double  ascad=0.1, sparsity= 0;
//
//   arma::vec df(nlambda),lambdamaxs(Nog)     ;
//
//   arma::mat  stB, Q, Betas(p, nlambda), Penaltyfactor(p1* p2 * p3, Nog), //stBs(Nog * p1* p2 * p3, nlambda),
//   dpen(p1, p2 * p3),
//   Gamma(p1, p2 * p3),          wGamma(p1, p2 * p3), posdefscale;
//
//
//   ////fill variables
//   Betas.fill(42);
//   posdefscale.eye(Nog,Nog);
//   posdefscale = posdefscale * 0.000001;
//   ////precompute ///// compute group ols estimates!!!!
//   for(int j = 0; j < Nog; j++){
//     Penaltyfactor.col(j)= vectorise(penaltyfactor);
//     arma::mat tmp;
//     if(usex == 1){
//       tmp = RHmat(Phi3.t(), RHmat(Phi2.t(), RHmat(Phi1.t(), Y.slice(j), n2, n3), n3, p1), p1, p2);
//       B.col(j) = vectorise(tmp);
//     }else{
//       tmp = reshape(B.col(j), p1, p2*p3);
//     }
//     if(makelamb == 1){
//       arma::mat   absgradzeroall = abs(tmp) % penaltyfactor;
//       arma::mat absgradzeropencoef = absgradzeroall % (penaltyfactor > 0);
//       arma::mat penaltyfactorpencoef = (penaltyfactor == 0) * 1 + penaltyfactor;
//       lambdamaxs(j) = as_scalar(max(max(absgradzeropencoef / penaltyfactorpencoef)));
//     }
//
//   }
//
//   ////set up quad  problem
//   arma::mat AE(Nog, 1), AI(Nog, Nog), ce(1, 1), ci(Nog, 1);
//   AE.ones();
//   Eigen::MatrixXd eigen_AE = eigen_cast(AE);
//   ce.fill(-1);
//   Eigen::VectorXd eigen_ce = eigen_cast(ce);
//   AI.eye();
//   Eigen::MatrixXd eigen_AI = eigen_cast(AI);
//   ci.zeros();
//   Eigen::VectorXd eigen_ci = eigen_cast(ci);
//   Eigen::MatrixXd eigen_Q;
//
//   ////make lambda sequence
//   if(makelamb == 1){
//
//     arma::mat Ze = zeros<mat>(n1, n2 * n3);
//     double lambdamax = max(lambdamaxs);
//     double m = log(lambdaminratio);
//     double M = 0;
//     double difflamb = abs(M - m) / (nlambda - 1);
//     double l = 0;
//
//     for(int i = 0; i < nlambda ; i++){
//
//       lambda(i) = lambdamax * exp(l);
//       l = l - difflamb;
//
//     }
//
//   }
//
//   //else{std::sort(lambda.begin(), lambda.end(), std::greater<int>());}
//
//   if(usekappa == 1){
//
//
//
//
//   }
//
//   ///////////start lambda loop
//   for (int m = 0; m < nlambda; m++){
//     Gamma = Penaltyfactor * lambda(m);
//
//     //start MSA loop
//     for (int s = 0; s < steps; s++){
//
//       if(s == 0){
//
//         if(penalty != "lasso"){wGamma = Gamma / lambda(m);}else{wGamma = Gamma;}
//
//       }else{
//
//         if(penalty == "scad"){//can we use this ???!?!? scad is multi lasso and each lasso is ok...
//
//           //          HOW DOES THIS WORK?! ITERATION HHOW?!?!
//
//           // arma::mat absBeta = abs(Beta);
//           // arma::mat pospart = ((ascad * Gamma - absBeta) + (ascad * Gamma - absBeta)) / 2;
//           // arma::mat dpen = sign(Beta) % Gamma % ((absBeta <= Gamma) + pospart / (ascad - 1) % (absBeta > Gamma));
//           // wGamma = abs(dpen) % Gamma / lambda(m) % (Beta != 0) + lambda(m) * (Beta == 0);
//
//         }
//
//       }
//       ////this computation can be halvfed by symmetry but maybe thats done on by arma already??
//       stB = st(B, wGamma);
//       // stBs.col(m) = vectorise(stB);
//       Q = stB.t() * stB + posdefscale;
//       eigen_Q = eigen_cast(Q);
//
//       // Betas.col(m) = magging(stB, eigen_Q,eigen_AE, eigen_ce, eigen_AI, eigen_ci);
//       arma::mat tmp = magging(stB, eigen_Q, eigen_AE, eigen_ce, eigen_AI, eigen_ci);
//       sparsity = accu(tmp == 0);
//       Betas.col(m)=tmp;
//       df(m)= p - sparsity;
//
//     }//end MSA loop
//     endmodelno = m + 1;
//
//     if(kappa <  sparsity / (p * 1.0) //|| Stopmaxiter == 1
//     ){
//       stopsparse=1;
//       break;
//
//     }
//
//
//   }//end lambda loop
//
//   output = Rcpp::List::create(Rcpp::Named("Beta") = Betas,
//                               Rcpp::Named("stBs") = stopsparse,
//                               Rcpp::Named("B") = B,
//                               Rcpp::Named("df") = df,
//                               Rcpp::Named("endmodelno") = endmodelno,
//                               Rcpp::Named("lambda") = lambda
//   );
//
//   return output;
//
// }
