#ifdef _OPENMP
#include <omp.h>
#endif

#include "RcppArmadillo.h" //for arma namespace?

#include "auxfunc.h"
#include "wave_filter.cpp"

using namespace std;
using namespace arma;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// algorithm //////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//[[Rcpp::export]]
Rcpp::List pga(Rcpp::List phi,
               Rcpp::List resp,
               std::string penalty,
               arma::vec zeta,
               double c,
               arma::vec lambda,
               int nlambda,
               int makelamb,
               double lambdaminratio,
               arma::mat penaltyfactor,
               double reltol,
               int maxiter,
               int steps,
               int btmax,
               int mem,
               double tau,
               double nu,
               int alg,
               int array,
               int ll,
               double Lmin,
               int nthreads,
               int wave,
               int J,
               int dim,
               std::string wf){

Rcpp::List output, Resp(resp), Phi(phi);
int G = Resp.size();
int openmp = 0;
if(array == 1){//npg and fista for array data------

arma::mat Phi1 = Rcpp::as<arma::mat>(Phi[0]);
arma::mat Phi2 = Rcpp::as<arma::mat>(Phi[1]);
arma::mat Phi3 = Rcpp::as<arma::mat>(Phi[2]);

double *g = nullptr, *h = nullptr;

int Lwave, n1, n2, n3, p1, p2, p3;
if(wave == 1){
  Lwave = get_L(wf);
  g = new double[Lwave];
  h = new double[Lwave];
  wave_filter(wf, g, h, Lwave);
n1 = Phi1(0, 0), n2 = Phi2(0, 0), n3 = Phi3(0, 0), p1 = n1, p2 = n2, p3 = n3;
}else{
n1 = Phi1.n_rows, n2 = Phi2.n_rows, n3 = Phi3.n_rows,
p1 = Phi1.n_cols, p2 = Phi2.n_cols, p3 = Phi3.n_cols;
}

int  ng = n1 * n2 * n3,     p = p1 * p2 * p3,    nzeta = zeta.n_elem;

double alphamax, delta, deltamax, L;

arma::vec Btenter(nzeta),  Btiter(nzeta),  eig1, eig2, eig3,  EndMod(nzeta);

arma::mat DF(nlambda, nzeta), ITER(nlambda, nzeta), Lamb(nlambda, nzeta),
          Phi1tPhi1, Phi2tPhi2, Phi3tPhi3, Stops(3, nzeta);

arma::cube Coef(p, nlambda, nzeta), OBJ(maxiter, nlambda, nzeta),
           PhitZ(p1, p2 * p3, G),    Z(n1, n2 * n3, G);

for(int i = 0; i < G; i++){Z.slice(i) = Rcpp::as<arma::mat>(Resp[i]);}

if(wave == 1){alphamax = 1;}else{
Phi1tPhi1 = Phi1.t() * Phi1;
Phi2tPhi2 = Phi2.t() * Phi2;
Phi3tPhi3 = Phi3.t() * Phi3;
eig1 = arma::eig_sym(Phi1tPhi1);
eig2 = arma::eig_sym(Phi2tPhi2);
eig3 = arma::eig_sym(Phi3tPhi3);
alphamax = as_scalar(max(kron(eig1, kron(eig2, eig3))));
}
for(int j = 0; j < G; j++){
if(wave == 1){
PhitZ.slice(j) = wt(Z.slice(j), dim, Lwave, h, g, J, p1, p2, p3, PhitZ.slice(j));
}else{
PhitZ.slice(j) = RHmat(Phi3.t(), RHmat(Phi2.t(),
RHmat(Phi1.t(), Z.slice(j), n2, n3), n3, p1), p1, p2);
}
}

mat A(G, G);
mat PhitZi;
A.fill(0);
for(int i = 0; i < G; i++){
PhitZi = PhitZ.slice(i);
for(int j = i + 1; j < G; j++){
A(i,j) = sum_square(PhitZi- PhitZ.slice(j));
}
}

L = 4 / pow(ng, 2) * (max(max(A)) + alphamax * ng / 2); //upper bound on Lipschitz constant
delta = nu * 1.9 / L; //stepsize scaled up by nu
deltamax = 1.99 / L; //maximum theoretically allowed stepsize
if(Lmin == 0){Lmin = (1 / nu) * L;}

////make lambda sequence, Beta is ZERO here so lambdamax identical across zeta
if(makelamb == 1){

arma::mat absgradzeroall(p1, p2 * p2);
absgradzeroall = abs(gradloss(PhitZ, arma::zeros<mat>(p1, p2 * p3),
                              -eev(arma::zeros<mat>(n1, n2 * n3), Z, ng), ng, 42, ll));
arma::mat absgradzeropencoef = absgradzeroall % (penaltyfactor > 0);
arma::mat penaltyfactorpencoef = (penaltyfactor == 0) * 1 + penaltyfactor;
double lambdamax = as_scalar(max(max(absgradzeropencoef / penaltyfactorpencoef)));
double m = log(lambdaminratio);
double M = 0;
double difflamb = abs(M - m) / (nlambda - 1);
double l = 0;

for(int i = 0; i < nlambda ; i++){

lambda(i) = lambdamax * exp(l);
l = l - difflamb;

}

}else{std::sort(lambda.begin(), lambda.end(), std::greater<int>());}

#ifdef _OPENMP
openmp = 1;
#pragma omp parallel num_threads(nthreads)
{
#endif
//zeta loop-----
#ifdef _OPENMP
#pragma omp for
#endif
for(int z = 0; z < nzeta ; z++){

int ascent, ascentmax, bt, btenter = 0, btiter = 0, endmodelno = nlambda,
Stopconv = 0, Stopmaxiter = 0, Stopbt = 0;

double ascad = 3.7, lossBeta, lossProp = 0, lossX, penProp = 0, relobj, val;

arma::vec df(nlambda), Iter(nlambda), Pen(maxiter),
obj(maxiter + 1),  eevBeta, eevProp, eevX;

arma::mat absBeta(p1, p2 * p3), Beta(p1, p2 * p3), Betaprev(p1, p2 * p3),
Betas(p, nlambda), BT(nlambda, maxiter), Delta(maxiter, nlambda),
dpen(p1, p2 * p3), Gamma(p1, p2 * p3),
GradlossX(p1, p2 * p3), GradlossXprev(p1, p2 * p3), GradlossX2(p1, p2 * p3),
Obj(maxiter, nlambda),
PhitPhiBeta, PhitPhiX, pospart(p1, p2 * p3),
Prop(p1, p2 * p3), PhiBeta(n1, n2 * n3), PhiProp(n1, n2 * n3), PhiX(n1, n2 * n3),
wGamma(p1, p2 * p3), R, S,  X(p1, p2 * p3), Xprev, Zi;

////fill variables
ascentmax = 4;
double scale = 0.9;

obj.fill(NA_REAL);
Betas.fill(42);
Iter.fill(0);
GradlossXprev.fill(0);
BT.fill(-1);

Delta.fill(NA_REAL);
Obj.fill(NA_REAL);
Pen.fill(0);

////initialize
Betaprev.fill(0);
Beta = Betaprev;
X = Beta; //npg only uses X
Xprev = Betaprev; //npg only uses X

if(wave == 1){//idwt!!
PhiBeta = iwt(Beta, dim, Lwave, h, g, J, p1, p2, p3, PhiBeta);
}else{
PhiBeta = RHmat(Phi3, RHmat(Phi2, RHmat(Phi1, Beta, p2, p3), p3, n1), n1, n2);
}
PhiX = PhiBeta; //npg only uses X
if(wave == 1){//identity!!
PhitPhiBeta = Beta;
}else{
PhitPhiBeta = RHmat(Phi3tPhi3,
RHmat(Phi2tPhi2,
RHmat(Phi1tPhi1, Beta, p2, p3), p3, p1), p1, p2);
}

PhitPhiX = PhitPhiBeta; //npg only uses X
eevBeta = -eev(PhiBeta, Z, ng);
eevX = eevBeta;

lossBeta = softmaxloss(eevBeta, zeta(z), ll);
lossX = lossBeta; //npg only uses X

///////////start lambda loop
for (int j = 0; j < nlambda; j++){

Gamma = penaltyfactor * lambda(j);

ascent = 0;

//start MSA loop
for (int s = 0; s < steps; s++){

if(s == 0){

if(penalty != "lasso"){wGamma = Gamma / lambda(j);}else{wGamma = Gamma;}

}else{

if(penalty == "scad"){

absBeta = abs(Beta);
pospart = ((ascad * Gamma - absBeta) + (ascad * Gamma - absBeta)) / 2;
dpen = sign(Beta) % Gamma % ((absBeta <= Gamma) + pospart / (ascad - 1) % (absBeta > Gamma));
wGamma = abs(dpen) % Gamma / lambda(j) % (Beta != 0) + lambda(j) * (Beta == 0);

}

}
if(alg == 1){/////////////////ARRAY NPG algorithm from chen2016

for (int k = 0; k < maxiter; k++){

if(k == 0){

Xprev = X;
obj(k) = lossX + l1penalty(wGamma, X);
Obj(k, j) = obj(k);
Delta(k, j) = delta;

}else{//if not the first iteration

if(wave == 1){//identity!!
PhitPhiX = X;
}else{
PhitPhiX = RHmat(Phi3tPhi3,
RHmat(Phi2tPhi2,
RHmat(Phi1tPhi1, X, p2, p3), p3, p1), p1, p2);
}

GradlossX = gradloss(PhitZ, PhitPhiX, eevX, ng, zeta(z), ll);
lossX = softmaxloss(eevX, zeta(z), ll);

if(k > 1){//why this exception ?? todo
S = X - Xprev;
R = GradlossX - GradlossXprev;
double tmp = as_scalar(accu(S % R ) / sum_square(S)); //is this corrert???
tmp = std::max(tmp, Lmin);
delta = 1 / std::min(tmp, pow(10,8));
}else{
delta = 1;
}

////proximal backtracking from chen2016
BT(j, k) = 0;
while(BT(j, k) < btmax){

Prop = prox_l1(X - delta * GradlossX, delta * wGamma);
if(wave == 1){//idwt!!
PhiProp = iwt(Prop, dim, Lwave, h, g, J, p1, p2, p3, PhiProp);
}else{
PhiProp = RHmat(Phi3, RHmat(Phi2, RHmat(Phi1, Prop, p2, p3), p3, n1), n1, n2);
}

eevProp = -eev(PhiProp, Z, ng);
lossProp = softmaxloss(eevProp, zeta(z), ll);

val = as_scalar(max(obj(span(std::max(0, k - mem), k - 1))) - c / 2 * sum_square(Prop - Xprev));
penProp = l1penalty(wGamma, Prop);

if (lossProp + penProp <= val + 0.0000001){

break;

}else{

delta = delta / tau; //tau>1, scaling delta down instead of L up...
BT(j, k) = BT(j, k) + 1;

}

}//end line search

////check if maximum number of proximal backtraking step is reached
if(BT(j, k) == btmax){Stopbt = 1;}

Xprev = X;
GradlossXprev = GradlossX;
X = Prop;
PhiX = PhiProp;
eevX = eevProp;
obj(k) = lossProp + penProp;
Obj(k, j) = obj(k);
Iter(j) = k;
Delta(k, j) = delta;

////proximal convergence check
relobj = abs(obj(k) - obj(k - 1)) / (reltol + abs(obj(k - 1)));

if(k < maxiter - 1 && relobj < reltol){//todo is this necessary?? put outside k-loop

df(j) = p - accu((X == 0));
Betas.col(j) = vectorise(X);
obj.fill(NA_REAL);
Stopconv = 1;
break;

}else if(k == maxiter - 1){ //todo is this necessary?? put outside k-loop

df(j) = p - accu((X == 0));
Betas.col(j) = vectorise(X);
obj.fill(NA_REAL);
Stopmaxiter = 1;
break;

}

}

////break proximal loop if maximum number of proximal backtraking step is reached
if(Stopbt == 1 || Stopmaxiter == 1){

Betas.col(j) = vectorise(X);
break;

}

}//end proximal loop

}else{////FISTA

for (int k = 0; k < maxiter; k++){

if(k == 0){

Betaprev = Beta;
X = Beta;
obj(k) = lossBeta + l1penalty(wGamma, Beta);
Obj(k, j) = obj(k);
BT(j, k) = 1; //force initial backtracking
Delta(k, j) = delta;

}else{

X = Beta + (k - 2) / (k + 1) * (Beta - Betaprev);

if(wave == 1){
PhiX = iwt(X, dim, Lwave, h, g, J, p1, p2, p3, PhiX);
}else{
PhiX = RHmat(Phi3, RHmat(Phi2, RHmat(Phi1, X, p2, p3), p3, n1), n1, n2);
}

eevX = -eev(PhiX, Z, ng);
if(wave == 1){
PhitPhiX = X;
}else{
PhitPhiX = RHmat(Phi3tPhi3, RHmat(Phi2tPhi2, RHmat(Phi1tPhi1, X, p2, p3), p3, p1), p1, p2);
}

GradlossX = gradloss(PhitZ, PhitPhiX, eevX, ng, zeta(z), ll);

////check if proximal backtracking occurred last iteration
if(BT(j, k - 1) > 0){bt = 1;}else{bt = 0;}

////check for divergence
if(ascent > ascentmax){bt= 1;}

if((bt == 1 && deltamax < delta) || nu > 1){//backtrack

lossX = softmaxloss(eevX, zeta(z), ll);
BT(j, k) = 0;

while(BT(j, k) < btmax){//start backtracking

Prop = prox_l1(X - delta * GradlossX, delta * wGamma);

if(wave == 1){
PhiProp = iwt(Prop, dim, Lwave, h, g, J, p1, p2, p3, PhiProp);
}else{
PhiProp = RHmat(Phi3, RHmat(Phi2, RHmat(Phi1, Prop, p2, p3), p3, n1), n1, n2);
}

lossProp = softmaxloss(-eev(PhiProp, Z, ng), zeta(z), ll);

val = as_scalar(lossX + accu(GradlossX % (Prop - X))
+ 1 / (2 * delta) * sum_square(Prop - X));

if(lossProp <= val + 0.0000001){ //need to add a little due to numerical issues

break;

}else{

delta = scale * delta;
BT(j, k) = BT(j, k) + 1;

//if(delta < deltamax){delta = deltamax;}

}

}//end backtracking
////check if maximum number of proximal backtraking step is reached
if(BT(j, k) == btmax){Stopbt = 1;}

}else{//no backtracking

Prop = prox_l1(X - delta * GradlossX, delta * wGamma);
if(wave == 1){
PhiProp = iwt(Prop, dim, Lwave, h, g, J, p1, p2, p3, PhiProp);
}else{
PhiProp = RHmat(Phi3, RHmat(Phi2, RHmat(Phi1, Prop, p2, p3), p3, n1), n1, n2);
}

lossProp = softmaxloss(-eev(PhiProp, Z, ng), zeta(z), ll);

}

Betaprev = Beta;
Beta = Prop;
lossBeta = lossProp;
obj(k) = lossBeta + l1penalty(wGamma, Beta);
Iter(j) = k;
Delta(k, j) = delta;
Obj(k, j) = obj(k);

////proximal divergence check
if(obj(k) > obj(k - 1)){ascent = ascent + 1;}else{ascent = 0;}

relobj = abs(obj(k) - obj(k - 1)) / (reltol + abs(obj(k - 1)));

if(k < maxiter - 1 && relobj < reltol){

df(j) = p - accu((Beta == 0));
Betas.col(j) = vectorise(Beta);
obj.fill(NA_REAL);
Stopconv = 1;
break;

}else if(k == maxiter - 1){

df(j) = p - accu((Beta == 0));
Betas.col(j) = vectorise(Beta);
obj.fill(NA_REAL);
Stopmaxiter = 1;
break;

}

}

////break proximal loop if maximum number of proximal backtraking step is reached
if(Stopbt == 1){

Betas.col(j) = vectorise(Beta);
break;

}

}//end proximal loop

}

//Stop msa loop if maximum number of backtracking steps or maxiter is reached
if(Stopbt == 1 || Stopmaxiter == 1){

endmodelno = j;
break;

}

}//end MSA loop

//Stop lambda loop if maximum number of backtracking steps or maxiter is reached
if(Stopbt == 1 || Stopmaxiter == 1){

endmodelno = j;
break;

}

}//end lambda loop

Stops(0, z) = Stopconv;
Stops(1, z) = Stopmaxiter;
Stops(2, z) = Stopbt;
btenter = accu((BT > -1));
btiter = accu((BT > 0) % BT);

//save to shared
Coef.slice(z) = Betas;
DF.col(z) = df;
Btenter(z) = btenter;
Btiter(z) = btiter;
OBJ.slice(z) = Obj;
ITER.col(z) = Iter;
EndMod(z) = endmodelno;
Lamb.col(z) = lambda;

}//end zeta loop
#ifdef _OPENMP
}
#endif
output = Rcpp::List::create(Rcpp::Named("Beta") = Coef,
Rcpp::Named("df") = DF,
Rcpp::Named("btenter") = Btenter,
Rcpp::Named("btiter") = Btiter,
Rcpp::Named("Obj") = OBJ,
Rcpp::Named("Iter") = ITER,
Rcpp::Named("endmodelno") = EndMod,
Rcpp::Named("lambda") = Lamb,
Rcpp::Named("Stops") = Stops,
Rcpp::Named("openMP") = openmp
);

}else{//non array

int p = Rcpp::as<arma::mat>(Phi[0]).n_cols;
field<mat> RESP(G, 1), PHI(G, 1);
arma::cube PHItPHI(p, p, G);
arma::mat PHItRESP(p, G);
arma::vec n(G);

for(int i = 0; i < G; i++){

RESP(i, 0) = Rcpp::as<arma::mat>(Resp[i]); //ngx1 matrices
PHI(i, 0) = Rcpp::as<arma::mat>(Phi[i]); //ngxp matrices
PHItPHI.slice(i) = PHI(i, 0).t() * PHI(i, 0);  // pxp * G store in cube!!
PHItRESP.col(i) = PHI(i, 0).t() * RESP(i, 0); //eta...//px1 * G stor in matrix
n(i) = PHI(i, 0).n_rows;

}

int nzeta = zeta.n_elem;
arma::vec Btiter(nzeta),  EndMod(nzeta);
arma::mat DF(nlambda, nzeta), ITER(nlambda, nzeta), Lamb(nlambda, nzeta), Stops(3, nzeta);
cube Coef(p, nlambda, nzeta), OBJ(maxiter, nlambda, nzeta);

//make lambda sequence
if(makelamb == 1){

arma::mat absgradzeroall(p, 1);

absgradzeroall = abs(gradloss_f(PHItRESP, cube_mult(PHItPHI, arma::zeros<mat>(p, 1)),
-eev_f(field_mult(PHI, arma::zeros<mat>(p, 1)), RESP, n), n, 42, ll));

arma::mat absgradzeropencoef = absgradzeroall % (penaltyfactor > 0);
arma::mat penaltyfactorpencoef = (penaltyfactor == 0) * 1 + penaltyfactor;
double lambdamax = as_scalar(max(max(absgradzeropencoef / penaltyfactorpencoef)));
double m = log(lambdaminratio);
double M = 0;
double difflamb = abs(M - m) / (nlambda - 1);
double l = 0;

for(int i = 0; i < nlambda ; i++){

lambda(i) = lambdamax * exp(l);
l = l - difflamb;

}

}else{std::sort(lambda.begin(), lambda.end(), std::greater<int>());}

#ifdef _OPENMP
openmp = 1;
#pragma omp parallel num_threads(nthreads) //valgrind doesnt like this!!
{
#endif

#ifdef _OPENMP
#pragma omp for
#endif
for(int z = 0; z < nzeta ; z++){//zeta loop-----

int btiter = 0, endmodelno = nlambda, Stopconv = 0, Stopmaxiter = 0, Stopbt = 0;

double ascad = 3.7, delta, lossProp = 0, lossX, penProp = 0, relobj, val;

arma::vec df(nlambda), eevBeta, eevProp, eevX, Iter(nlambda), obj(maxiter + 1),
Pen(maxiter);

arma::mat absX(p, 1), Betas(p, nlambda), BT(nlambda, maxiter),
Delta(maxiter, nlambda), dpen(p, 1), Gamma(p, 1), GradlossX(p, 1),
GradlossXprev(p, 1), GradlossX2(p, 1), Obj(maxiter, nlambda), PHItPHIX,
pospart(p, 1), Prop(p, 1),
wGamma(p, 1), R,S,X(p, 1), Xprev(p, 1);

field<mat> PHIX, PHIProp;

////fill variables
obj.fill(NA_REAL);
Betas.fill(42);
Iter.fill(0);
GradlossXprev.fill(0);
BT.fill(-1);

Delta.fill(NA_REAL);//todo
Obj.fill(NA_REAL);//todo
Pen.fill(0);//todo

////initialize at zero which is optimal for lambmax pr construction
Xprev.fill(0);
X = Xprev;
PHIX = field_mult(PHI, X); // G * n_g field
PHItPHIX = cube_mult(PHItPHI, X); // p x G matrix
eevX = -eev_f(PHIX, RESP, n);
lossX = softmaxloss(eevX, zeta(z), ll);

//make lambda sequence
if(makelamb == 1){

arma::mat absgradzeroall(p, 1);

absgradzeroall = abs(gradloss_f(PHItRESP, PHItPHIX, eevX, n, zeta(z), ll)); //todo gradloss

arma::mat absgradzeropencoef = absgradzeroall % (penaltyfactor > 0);
arma::mat penaltyfactorpencoef = (penaltyfactor == 0) * 1 + penaltyfactor;
double lambdamax = as_scalar(max(max(absgradzeropencoef / penaltyfactorpencoef)));
double m = log(lambdaminratio);
double M = 0;
double difflamb = abs(M - m) / (nlambda - 1);
double l = 0;

for(int i = 0; i < nlambda ; i++){

lambda(i) = lambdamax * exp(l);
l = l - difflamb;

}

}else{std::sort(lambda.begin(), lambda.end(), std::greater<int>());}

///////start lambda loop
for (int j = 0; j < nlambda; j++){

Gamma = penaltyfactor * lambda(j);

//start MSA loop
for (int s = 0; s < steps; s++){

if(s == 0){

if(penalty != "lasso"){wGamma = Gamma / lambda(j);}else{wGamma = Gamma;}

}else{

if(penalty == "scad"){

absX = abs(X);
pospart = ((ascad * Gamma - absX) + (ascad * Gamma - absX)) / 2;
dpen = sign(X) % Gamma % ((absX <= Gamma) + pospart / (ascad - 1) % (absX > Gamma));
wGamma = abs(dpen) % Gamma / lambda(j) % (X != 0) + lambda(j) * (X == 0);

}

}

///start proximal loop
for (int k = 0; k < maxiter; k++){

if(k == 0){

Xprev = X;
obj(k) = lossX + l1penalty(wGamma, X);
Obj(k, j) = obj(k);

}else{//if not the first iteration

PHItPHIX = cube_mult(PHItPHI, X);
GradlossX = gradloss_f(PHItRESP, PHItPHIX, eevX, n, zeta(z), ll);
lossX = softmaxloss(eevX, zeta(z), ll);

if(k > 1){//why this exception ?? todo
S = X - Xprev;
R = GradlossX - GradlossXprev;
double tmp = as_scalar(accu(S % R ) / sum_square(S)); //is this corrert???
tmp = std::max(tmp, Lmin);
delta = 1 / std::min(tmp, pow(10,8));
}else{
delta = 1;
}

////proximal backtracking from chen2016
BT(j, k) = 0;
while(BT(j, k) < btmax){

Prop = prox_l1(X - delta * GradlossX, delta * wGamma);
PHIProp = field_mult(PHI, Prop); //eta return field of length G
eevProp = -eev_f(PHIProp, RESP, n); //
lossProp = softmaxloss(eevProp, zeta(z), ll);

val = as_scalar(max(obj(span(std::max(0, k - mem), k - 1))) - c / 2 * sum_square(Prop - Xprev));
penProp = l1penalty(wGamma, Prop);

if (lossProp + penProp <= val + 0.0000001){

break;

}else{

delta = delta / tau; //tau>1, scaling delta down instead of L up...
BT(j, k) = BT(j, k) + 1;

}

}//end line search

////check if maximum number of proximal backtraking step is reached
if(BT(j, k) == btmax){Stopbt = 1;}

Xprev = X;
GradlossXprev = GradlossX;
X = Prop;
PHIX = PHIProp;
eevX = eevProp;
obj(k) = lossProp + penProp;
Iter(j) = k;

////proximal convergence check
relobj = abs(obj(k) - obj(k - 1)) / (reltol + abs(obj(k - 1)));

if(k < maxiter - 1 && relobj < reltol){//todo is this necessary?? put outside k-loop

df(j) = p - accu((X == 0));
Betas.col(j) = vectorise(X);
obj.fill(NA_REAL);
Stopconv = 1;
break;

}else if(k == maxiter - 1){ //todo is this necessary?? put outside k-loop

df(j) = p - accu((X == 0));
Betas.col(j) = vectorise(X);
obj.fill(NA_REAL);
Stopmaxiter = 1;
break;

}

}

////break proximal loop if maximum number of proximal backtraking step is reached
if(Stopbt == 1 || Stopmaxiter == 1){

Betas.col(j) = vectorise(X);
break;

}

}//end proximal loop

//Stop msa loop if maximum number of backtracking steps or maxiter is reached
if(Stopbt == 1 || Stopmaxiter == 1){

endmodelno = j;
break;

}

}//end MSA loop

//Stop lambda loop if maximum number of backtracking steps or maxiter is reached
if(Stopbt == 1 || Stopmaxiter == 1){

endmodelno = j;
break;

}

}//end lambda loop

Stops(0, z) = Stopconv;
Stops(1, z) = Stopmaxiter;
Stops(2, z) = Stopbt;
btiter = accu((BT > 0) % BT);

Coef.slice(z) = Betas;
DF.col(z) = df;
//Btenter(z) = btenter;
Btiter(z) = btiter;
OBJ.slice(z) = Obj;
ITER.col(z) = Iter;
EndMod(z) = endmodelno;
Lamb.col(z) = lambda;

}//end zeta loop

#ifdef _OPENMP
}
#endif

output = Rcpp::List::create(
Rcpp::Named("Beta") = Coef,
Rcpp::Named("df") = DF,
//  Rcpp::Named("btenter") = Btenter,
Rcpp::Named("btiter") = Btiter,
//  Rcpp::Named("Obj") = OBJ,
Rcpp::Named("Iter") = ITER,
Rcpp::Named("endmodelno") = EndMod,
Rcpp::Named("lambda") = Lamb,
//  Rcpp::Named("BT") = BT,
Rcpp::Named("Stops") = Stops,
Rcpp::Named("openMP") = openmp
);
}

return output;

}

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
