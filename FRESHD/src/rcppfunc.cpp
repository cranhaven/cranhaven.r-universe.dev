#include "auxfunc.h"

using namespace std;
using namespace arma;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// maximin implementation /////////////////////
////////////////////////////////////////////////////////////////////////////////
//[[Rcpp::export]]
Rcpp::List solveMMP(arma::mat dims,
                    arma::mat Phi1, arma::mat Phi2, arma::mat Phi3,
                    Rcpp::NumericVector resp,
                    std::string penalty,
                    double kappa,
                    arma::vec lambda, int nlambda, int makelamb, double lambdaminratio,
                    arma::mat penaltyfactor,
                    double tol,
                    int maxiter,
                    std::string alg,
                    std::string stopcond,
                    double orthval,
                    double gamma0,
                    double gmh,
                    double gmg,
                    double minval,
                    double epsiloncor,
                    int Tf,
                    std::string wf,
                    int J,
                    int dim,
                    double tauk,
                    double gamk,
                    double eta){
Rcpp::List output;
Rcpp::NumericVector vecresp(resp);
Rcpp::IntegerVector respDim = vecresp.attr("dim");
const arma::cube Y(vecresp.begin(), respDim[0], respDim[1], respDim[2], false);

double *g = nullptr, *h = nullptr;
int Lwave;
if(wf != "not used"){
  Lwave = get_L(wf);
  g = new double[Lwave];
  h = new double[Lwave];
  wave_filter(wf, g, h, Lwave);
}

int endmodelno = nlambda, //n1 = dims(0,0),
    n2 = dims(1,0), n3 = dims(2, 0),
  Nog = Y.n_slices, p1 = dims(0, 1), p2 = dims(1, 1), p3 = dims(2, 1), p = p1 * p2 * p3,
  stopsparse = 0, stopmaxiter = 0
;

double delta,
dres_norm, min_eig,//eta, //gamk,
lambdamax,// lamb = 1,
  lip,pres_norm, relobjerr,
  sparsity = 0//, tauk
  ;

arma::vec aradmm_est(2), df(nlambda), dres(maxiter), eig1, eig2, eig3, Iter(nlambda), gms(maxiter),
lambdamaxs(Nog), obj(maxiter), pres(maxiter), Stops(3) ,tauks(maxiter), tols(maxiter);

arma::mat Ax(p1, p2 * p3), Ax0(p1, p2 * p3),BZ(p1, p2 * p3), BZ1(p1, p2 * p3),
          BZ0(p1, p2 * p3), eig_val, fpr(nlambda, maxiter), Lambda(p1, p2 * p3), L(p1, p2 * p3),
          L1(p1, p2 * p3), Lhat(p1, p2 * p3), L0(p1, p2 * p3), Lhat0(p1, p2 * p3),
          Phi1tPhi1, Phi2tPhi2, Phi3tPhi3, reldiff(maxiter,nlambda), res,
          Q, stX, sumB(p1, p2 * p3), U(p1, p2 * p3),
          X(p1, p2 * p3), Xnorm(maxiter,nlambda), Xnormprev(maxiter,nlambda),
          Xf(p1, p2 * p3), Xg(p1, p2 * p3), Xh(p1, p2 * p3),
          Xtilde(p1, p2 * p3), Xs(p, nlambda),
          Z(p1, p2 * p3),Z1(p1, p2 * p3),Z0(p1, p2 * p3);

arma::mat PhitY(p1* p2 * p3, Nog);

////fill variables
Xnorm.fill(42);
Xnormprev.fill(42);
reldiff.fill(42);
Xs.fill(42);
Iter.fill(0);
fpr.fill(42);

////get XtY Gxn matrix
if(makelamb == 1){sumB.fill(0);}
for(int j = 0; j < Nog; j++){
arma::mat tmp(p1, p2 * p3);
if(wf == "not used"){//here the check should be on wf!
tmp = RHmat(Phi3.t(), RHmat(Phi2.t(), RHmat(Phi1.t(), Y.slice(j), n2, n3), n3, p1), p1, p2);
}else{
tmp  = wt(Y.slice(j), dim, Lwave, h, g, J, p1, p2, p3, tmp);
}

PhitY.col(j) = vectorise(tmp);

//get lambda
if(makelamb == 1){//remove nonpenalized coeffs from lambdamax computation
arma::mat absgradzeroall = abs(tmp) % penaltyfactor;
arma::mat absgradzeropencoef = absgradzeroall % (penaltyfactor > 0);
arma::mat penaltyfactorpencoef = (penaltyfactor == 0) * 1 + penaltyfactor;
lambdamaxs(j) = as_scalar(max(max(absgradzeropencoef / penaltyfactorpencoef)));
sumB = sumB + tmp;
}

}

if(makelamb == 1){
lambdamax = max(max(abs(sumB / Nog))); //soft maximin lambmax
if(alg == "aradmm" || alg == "admm"
   //wf != "not used"
     ){//go from 0 towards lambmax sigmoid for aradmm and admm.... todo: change wf to alg in if statement
double m = -10;
double M = 10;
double difflamb = abs(M - m) / (nlambda - 1);
double l = m;
for(int i = 0; i < nlambda ; i++){
lambda(i) = lambdamax / (1 + exp(l));
l = l + difflamb;
}
}else{// non wavlet but could be ortho....
double m = log(lambdaminratio);
double M = 0;
double difflamb = abs(M - m) / (nlambda - 1);
double l = 0;
for(int i = 0; i < nlambda ; i++){
lambda(i) = lambdamax * exp(l);
l = l - difflamb;
}
}
}else{std::sort(lambda.begin(), lambda.end(), std::greater<int>());}

//alwasy let lamb go min to max for admm...
if(alg == "aradmm" || alg == "admm"){std::sort(lambda.begin(), lambda.end());}//increasing ie dense to sparse

////set up projection problem
arma::mat AE(Nog, 1), AI(Nog, Nog), ce(1, 1), ci(Nog, 1);
AE.ones();
Eigen::MatrixXd eigen_AE = eigen_cast(AE);
ce.fill(-1);
Eigen::VectorXd eigen_ce = eigen_cast(ce);
AI.eye();
Eigen::MatrixXd eigen_AI = eigen_cast(AI);
ci.zeros();
Eigen::VectorXd eigen_ci = eigen_cast(ci);

arma::mat posdefscale;
posdefscale.eye(Nog,Nog);
posdefscale = posdefscale * 0.00000; //NEEDED??

Q = PhitY.t() * PhitY + posdefscale; //  consisten with B=PhitY???
Eigen::MatrixXd eigen_Q = eigen_cast(Q);

Phi1tPhi1 = Phi1.t() * Phi1;
Phi2tPhi2 = Phi2.t() * Phi2;
Phi3tPhi3 = Phi3.t() * Phi3;

//eigenvalue of C operator =lip const = 1/beta used for delta with tosacc.
//only calulate if tauk=0 oterwise use delta=1/tauk and tos.
//tauk>0 should be used in conjunction with tos for big design  without kronecker structure as lip is time consuming to compute
//tauk= 0 and tosacc without kronecker is possible but might be very slow as eig val comp for big design is slow
//gamk in (0, (4/lip − delta)/(2/lip)) and delta=1/tauk in (0,2/lip) so tauk in (lip/2,infty)
if(alg == "tosacc"
     //&& (alg != "admm" && alg != "aradmm")
     ){
eig1 = arma::eig_sym(Phi1tPhi1);
eig2 = arma::eig_sym(Phi2tPhi2);
eig3 = arma::eig_sym(Phi3tPhi3);
eig_val = kron(eig1, kron(eig2, eig3));
lip = as_scalar(max(eig_val));
min_eig = as_scalar(min(eig_val)) * 0.99;
}
////initialize
if(alg == "aradmm" || alg == "admm"){//orthogonal
//tauk = 1; //step size
//gamk = 1; //relax
if(alg == "aradmm"){
tauk = 0.1;
gamk = 1; //relax
}

X = reshape(PhitY.col(1), p1, p2 * p3); //initial guess
stX = st(X, Lambda);
Z = reshape(PhitY.col(2), p1, p2 * p3); //initial guess
L = X + Z;
Z1 = Z;
L1 = L;
Z0 = Z;
L0 = L;
Lhat0 = Lhat;
BZ1 = fB(Z);
}else{//general
//eta = 0.1; //from davis
if(alg == "tosacc"){
  delta = 1.99 / lip * (1 - eta); //in (0, 2β(1 − η)) //davis yin thm 1.2 for acc
}else{
  delta = 1 / tauk;
}
Z.fill(0);
Xf.fill(0);
U.fill(0);
//Xg = proxg_tos(Xf, delta, -PhitY, eigen_Q, eigen_AE, eigen_ce, eigen_AI, eigen_ci, p1, p2, p3);
//Xg.fill(0);
//U = (1 / delta) * (Xf - Xg);
}

///////////start lambda loop
for (int m = 0; m < nlambda; m++){
Lambda = penaltyfactor * lambda(m);
//delta = 1 / lip * (1 - eta); //in (0, 2β(1 − η)) //davis yin #should i treset?

/////////////////admm/radmm/aradmm from xu ,TOS algorithm from davis yin
for (int k = 0; k < maxiter; k++){

if(alg == "aradmm" || alg == "admm"){
X = solvh(Z1, L1, tauk, Lambda); //# x-update, prox operator for f
Ax = fA(X);
Xtilde = gamk * Ax + (1.0 - gamk) * (-BZ1);//#%relax
Z = solvg(Xtilde, L1, tauk, PhitY, eigen_Q, eigen_AE, eigen_ce, eigen_AI,
          eigen_ci, p1, p2, p3 );//#proxg(u-l/t, 1/t) //# z-update, projection onto to C
BZ = fB(Z);
L = L1 + tauk * (-Xtilde - BZ); //#% u-update
pres(k) = ip(-Ax - BZ, -Ax - BZ);//#%primal residual
dres(k) = tauk * ip(fAt(BZ - BZ1), fAt(BZ - BZ1)); //#%dual residual
// mres(k) = tauk*pres(k)^2+tauk*norm(Bz-Bz1, type="2")^2; #%monotone residual

//stop criterion
Iter(m) = k + 1;
pres_norm = pres(k) / max(ip(Ax , Ax), ip(BZ , BZ));
dres_norm = dres(k) / ip(fAt(L), fAt(L));
tols(k) = max(pres_norm, dres_norm);
if (tols(k) < tol){break;}///ADMM STOPS HERE

if(alg == "aradmm"){// adaptive stepsize ie tau and gamma
if (k == 0){ //record at first iteration
L0 = L;
Lhat0 = L1 + tauk * (-Ax - BZ1);
BZ0 = BZ;
Ax0 = Ax;
}else if(k % Tf == 0){
Lhat = L1 + tauk * (-Ax - BZ1);
aradmm_est = aradmm_estimate(k, tauk, gamk, Ax, Ax0, Lhat, Lhat0, BZ, BZ0,
                                 L, L0, orthval, minval, gmh, gmg, gamma0);
tauk = aradmm_est(0);
gamk = aradmm_est(1);
 //record for next estimation
L0 = L;
Lhat0 = Lhat;
BZ0 = BZ;
Ax0 = Ax;
}
}

tauks(k) = tauk;
gms(k) = gamk;
BZ1 = BZ;
Z1 = Z;
L1 = L;

}else{ //general non orthogonal problem
if(alg == "tos"){
Xg = proxg_tos(Z, delta, -PhitY, eigen_Q, eigen_AE, eigen_ce, eigen_AI, eigen_ci,
               p1, p2, p3);
Xh = RHmat(Phi3tPhi3, RHmat(Phi2tPhi2, RHmat(Phi1tPhi1, Xg, p2, p3), p3, p1), p1, p2);
Xf = proxf(2 * Xg - Z - delta * Xh, delta * Lambda);
Z = Z + gamk * (Xf - Xg);
}
if(alg == "tosacc"){
Xg = proxg_tos(Xf + delta * U, delta, -PhitY, eigen_Q, eigen_AE, eigen_ce,
               eigen_AI, eigen_ci, p1, p2, p3);
U = (1 / delta) * (Xf + delta * U - Xg);
delta = (-2 * pow(delta, 2) * min_eig * eta
           + sqrt(pow(2 * pow(delta, 2) * min_eig * eta, 2) + 4 * pow(delta, 2))) / 2;
Xh = RHmat(Phi3tPhi3, RHmat(Phi2tPhi2, RHmat(Phi1tPhi1, Xg, p2, p3), p3, p1), p1, p2);
Xf = proxf(Xg - delta * (U + Xh), delta * Lambda);
}

Iter(m) = k + 1;
if(stopcond == "fpr"){
fpr(m, k) = (1 / delta) * ip(Xf - Xg, Xf - Xg) / (1 + ip(Z, Z));
if(fpr(m, k) < tol and k > 1 ){
  break;}
}
if(stopcond == "obj"){//todo not in use!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
obj(k) = 42; //NOTE its eval on xf=xA
relobjerr = abs(obj(k) - obj(k - 1)) / (1 + abs(obj(k - 1)));
if(relobjerr < tol and k > 1 ){break;}
}

}//end ortho-nonortho choice

}//end k- loop

endmodelno = m + 1;//endmodelno = m;
if(alg == "aradmm" || alg == "admm"//wf != "not used"
     ){//todo choise on alg??
res = vectorise(st(X, Lambda));
}else{
obj.fill(0);
res = vectorise(Xf);
}

Xs.col(m) = res;
sparsity = accu(res == 0) ;
df(m) = p - sparsity;

if(Iter(m) > maxiter - 1){//end as last lambda didnt converge
  stopmaxiter = 1;
  break;}
if(alg == "aradmm" || alg == "admm"
     //wf != "not used"
     ){
  if(kappa < sparsity / (p * 1.0) or df(m) == 0){
    stopsparse = 1;
    break;}
  }

}//end lambda loop

Stops(0) = stopmaxiter;
Stops(1) = stopsparse;
output = Rcpp::List::create(Rcpp::Named("coefs") = Xs,
                            Rcpp::Named("df") = df,
                            Rcpp::Named("PhitY") = PhitY,
                            Rcpp::Named("stopcond") = stopcond,
                            Rcpp::Named("Iter") = Iter,
                            Rcpp::Named("endmodelno") = endmodelno,
                            Rcpp::Named("lambda") = lambda,
                            Rcpp::Named("lambdamax") = lambdamaxs,
                            Rcpp::Named("lip") = lip,
                            Rcpp::Named("eig") = eig_val,
                            Rcpp::Named("xh") = Xh,
                            Rcpp::Named("Stops") = Stops);

return output;

}

/////////////////////////////////// magging implementation /////////////////////
//[[Rcpp::export]]
Rcpp::List solveMag(arma::mat & B){
int Nog = B.n_cols;
arma::mat AE(Nog, 1), AI(Nog, Nog), ce(1, 1), ci(Nog, 1);
AE.ones();
Eigen::MatrixXd eigen_AE = eigen_cast(AE);
ce.fill(-1);
Eigen::VectorXd eigen_ce = eigen_cast(ce);
AI.eye();
Eigen::MatrixXd eigen_AI = eigen_cast(AI);
ci.zeros();
Eigen::VectorXd eigen_ci = eigen_cast(ci);

arma::mat posdefscale;
posdefscale.eye(Nog,Nog);
posdefscale = posdefscale * 0.000001; //needed?
arma::mat Q = B.t() * B + posdefscale;
Eigen::MatrixXd eigen_Q = eigen_cast(Q);

arma::mat z(B.n_rows, 1);
z.fill(0);
arma::mat out = projB(z, B, eigen_Q, eigen_AE, eigen_ce, eigen_AI, eigen_ci);
Rcpp::List output = Rcpp::List::create(Rcpp::Named("out") = out);
return output;
}

/////////////////////////////////// wavelet transforms /////////////////////
//[[Rcpp::export]]
arma::mat WT(arma::mat x, int dim, std::string wf, int J, int p1, int p2, int p3){
  arma::mat out(p1, p2 * p3);
  int L = get_L(wf);
  double *g = nullptr, *h = nullptr;
  g = new double[L];
  h = new double[L];
  wave_filter(wf, g, h, L);
  out = wt(x, dim, L, h, g, J, p1, p2, p3, out);
  return out;
}

//[[Rcpp::export]]
arma::mat IWT(arma::mat x, int dim, std::string wf, int J, int p1, int p2, int p3){
  arma::mat out(p1, p2 * p3);
  int L = get_L(wf);
  double *g = nullptr, *h = nullptr;
  g = new double[L];
  h = new double[L];
  wave_filter(wf, g, h, L);
  out =   iwt(x, dim, L,  h, g,   J,   p1,   p2,   p3, out);
  return out;

}

