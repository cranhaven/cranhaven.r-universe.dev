#include "dwt/dwt.h"
#include "dwt/dwt3.h"
#include "dwt/dwt2.h"
#include <math.h>
using namespace std;
//using namespace arma;

///new
#include <RcppArmadillo.h>
#define NDEBUG 1 //to avoid assert

#include <RcppEigen.h>

#include <iostream>
#include <sstream>
#include <string>

#include <Eigen/Dense>

#include "qprog/eiquadprog.h"

using namespace Rcpp;
using namespace Eigen;

////////////////////////////////// Auxiliary functions /////////////////////////

//////////////////// Direct RH-transform of a flat 3d array (matrix) M by a matrix X
arma::mat RHmat(arma::mat const& X, arma::mat const& M,int col, int sli){

int rowx = X.n_rows;

////matrix multiply
arma::mat XM = X * M;

////make matrix into rotated (!) cube (on matrix form)
arma::mat Mnew(col, sli * rowx);
for (int s = 0; s < sli; s++) {

for (int c = 0; c < col; c++) {

for (int r = 0; r < rowx; r++) {

Mnew(c, s + r * sli) = XM(r, c + s * col);

}

}

}

return Mnew;

}

//////////////////// The weighted (gam = penaltyfactor * lambda) l1-penalty function
double l1penalty(arma::mat const& gam, arma::mat const& zv){return accu(gam % abs(zv));}

////////////////////inner product
double ip(arma::mat x, arma::mat y){return accu(x % y);}

//////////////////// soft threshold operator on matrix
arma::mat st(arma::mat const& zv, arma::mat const& gam){

return (zv >= gam) % (zv - gam) + (zv <= -gam) % (zv + gam);

}

//////////////////// convert eigen to arma
arma::mat arma_cast(Eigen::MatrixXd eigen_A) {
arma::mat arma_B = arma::mat(eigen_A.data(), eigen_A.rows(), eigen_A.cols(),
true,   // changed from false to true. bit tricky
false);
return arma_B;
}

//////////////////// convert arma to eigen
Eigen::MatrixXd eigen_cast(arma::mat arma_A) {

Eigen::MatrixXd eigen_B = Eigen::Map<Eigen::MatrixXd>(arma_A.memptr(),
arma_A.n_rows,
arma_A.n_cols);
return eigen_B;
}

//////////////////// prox h for ortho
arma::mat oproxh(arma::mat const& z, double del, arma::mat const& lamb){//SLOW???
arma::mat  tmp;
tmp.zeros(z.n_rows, z.n_cols);
tmp = tmp + (z < -lamb) % ((z -  lamb * del) / (1 + del)); //coordinat wise div
tmp = tmp + (z > lamb) % ((z +  lamb * del) / (1 + del)); //coordinat wise div
tmp = tmp + (abs(z) <= lamb) % z;

return tmp;
}

arma::mat solvh(arma::mat v, arma::mat l, double t, arma::mat  lam){
return oproxh(v + l / t, 1 / t, lam);
}

arma::mat oproxg(arma::mat const& B,
arma::mat const& z,
Eigen::MatrixXd & Q,
const Eigen::MatrixXd & AE,
const Eigen::VectorXd & ce,
const Eigen::MatrixXd & AI,
const Eigen::VectorXd & ci,
int p1, int p2, int p3){

arma::vec  zvec=vectorise(z);
Eigen::VectorXd q = eigen_cast(-B.t() * zvec);
Eigen::VectorXd w(B.n_cols);
solve_quadprog(Q, q, AE, ce, AI, ci, w);

return reshape(B * arma_cast(w), p1, p2 * p3);

}

//////////////for admm orthogonal
arma::mat solvg(arma::mat u, arma::mat l, double t,
arma::mat B,
Eigen::MatrixXd & Q,
const Eigen::MatrixXd & AE,
const Eigen::VectorXd & ce,
const Eigen::MatrixXd & AI,
const Eigen::VectorXd & ci,
int p1, int p2, int p3){
return  oproxg(B,u-l/t, Q, AE, ce, AI, ci, p1, p2, p3);
}

//////////////for tos, wrapper of soft threshold
arma::mat proxf(arma::mat v,  arma::mat  lam){return st(v,  lam);}

////projection for tos and magging
arma::mat projB( arma::mat & z,
arma::mat & B,
Eigen::MatrixXd & Q,
const Eigen::MatrixXd & AE,
const Eigen::VectorXd & ce,
const Eigen::MatrixXd & AI,
const Eigen::VectorXd & ci){

arma::vec zvec = vectorise(z);
Eigen::VectorXd q = eigen_cast(-B.t() * zvec);
Eigen::VectorXd w(B.n_cols);
solve_quadprog(Q, q, AE, ce, AI, ci, w);

return B * arma_cast(w);

}

////////////for tos, prox of support function
arma::mat proxg_tos(arma::mat x, double delta,
arma::mat B, //matrix containng the support points of the convex hull
Eigen::MatrixXd & Q, //BtB
const Eigen::MatrixXd & AE,
const Eigen::VectorXd & ce,
const Eigen::MatrixXd & AI,
const Eigen::VectorXd & ci,
int p1, int p2, int p3){
arma::mat tmp = x / delta;
arma::mat tmp2 =  projB(tmp, B, Q, AE, ce, AI, ci);

return  x - delta * reshape(tmp2, p1, p2 * p3);

}

////////////////////aradmm
double curv_adaptive_BB(double al_h, double de_h){ //#%adapive BB, reference: FASTA paper
double tau_h, tmph = de_h/al_h;// #%correlation
if (tmph > .5){  tau_h = de_h;}else{tau_h = al_h - 0.5 * de_h;}
return tau_h;
}

arma::vec aradmm_estimate(int iter, double tau, double gamma,
arma::mat Au, arma::mat Au0, arma::mat l_hat,
arma::mat l_hat0, arma::mat Bv, arma::mat Bv0,
arma::mat l, arma::mat l0, double orthval,
double minval, double gmh, double gmg, double gamma0){
arma::vec out(2);
double bb_h,bb_g;
//inner products
arma::mat Au_Au0 = Au - Au0, l_hat_l_hat0 = l_hat - l_hat0, Bv_Bv0 = Bv - Bv0, l_l0 = l - l0;
double ul_hat = ip(Au_Au0, l_hat_l_hat0); //  arma::mat tmp =  (Au-Au0)*(l_hat-l_hat0) ;
double vl = ip(Bv_Bv0, l_l0); //tmp =  ((Bv-Bv0)*(l-l0));

//norm of lambda, lambda_hat
double  dl_hat = sqrt(ip(l_hat_l_hat0, l_hat_l_hat0));//tmp = l_hat-l_hat0;//norm(tmp, type ="2"   );
double  dl = sqrt(ip(l_l0, l_l0));// //tmp = l-l0;norm(tmp, type ="2"  );

//norm of gradient change
double  du = sqrt(ip(Au_Au0, Au_Au0));//tmp = Au-Au0; norm(tmp  , type ="2" );
double dv = sqrt(ip(Bv_Bv0, Bv_Bv0));//   tmp = Bv-Bv0;//norm(tmp , type ="2"  );

//#%flag to indicate whether the curvature can be estimated
bool hflag = false;
bool gflag = false;

//#%estimate curvature, only if it can be estimated
//#%use correlation/othogonal to test whether can be estimated
//#%use the previous stepsize when curvature cannot be estimated
if (ul_hat > orthval * du * dl_hat + minval){
hflag = true;
double  al_h = pow(dl_hat, 2) / ul_hat;
double  de_h = ul_hat / pow(du, 2);
bb_h = curv_adaptive_BB(al_h, de_h);
}
if(vl > orthval * dv * dl + minval){
gflag = true;
double al_g = pow(dl,2)/vl;
double  de_g = vl/pow(dv,2);
bb_g = curv_adaptive_BB(al_g, de_g);
}

if (hflag && gflag){
double ss_h = sqrt(bb_h);
double ss_g = sqrt(bb_g);
gamma = min(1 + 2 / (ss_h / ss_g + ss_g / ss_h), gamma0);
tau = ss_h*ss_g;
}else if(hflag){
gamma = gmh;//# %1.9;
tau = bb_h;
}else if (gflag){
gamma = gmg; //#%1.1;
tau = bb_g;
}else{
gamma = gamma0; //#%1.5;
//#%tau = tau;
}

out(0) = tau;
out(1) = gamma;
return out;
}

//#####
arma::mat fA(arma::mat x){return x;}
arma::mat fAt(arma::mat x){return x;}
arma::mat fB(arma::mat x){return -x;}

//////////////////   wavelet transform for 1d,2d,3d
arma::mat wt(arma::mat x, int dim, int L, double *h, double *g,
int J, int p1, int p2, int p3, arma::mat inout){
int nx = p1, ny = p2, nz = p3, size, endind = 0;

double* dat = nullptr; //= new double[2];

if(dim == 3){
size = nx * ny * nz / 8;
double *LLL = new double[size], *HLL = new double[size], *LHL = new double[size],
*LLH = new double[size], *HHL = new double[size], *HLH = new double[size],
*LHH = new double[size], *HHH = new double[size];
for(int j = 0; j < J; j++) {
if(j == 0){
three_D_dwt(x.memptr(), &nx, &ny, &nz, &L, h, g, LLL, HLL, LHL, LLH,
HHL, HLH, LHH, HHH);
}else{
three_D_dwt(dat, &nx, &ny, &nz, &L, h, g, LLL, HLL, LHL, LLH, HHL,
HLH, LHH, HHH);
}

if(j < J - 1) {
delete [] dat;
dat = new double[size];
std::copy(LLL, LLL + size, dat);
std::copy(HLL, HLL + size, inout.memptr() + endind);
std::copy(LHL, LHL + size, inout.memptr() + endind + size);
std::copy(LLH, LLH + size, inout.memptr() + endind + size * 2);
std::copy(HHL, HHL + size, inout.memptr() + endind + size * 3);
std::copy(HLH, HLH + size, inout.memptr() + endind + size * 4);
std::copy(LHH, LHH + size, inout.memptr() + endind + size * 5);
std::copy(HHH, HHH + size, inout.memptr() + endind + size * 6);
endind = endind + 7 * size;

delete [] LLL;
delete [] HLL;
delete [] LHL;
delete [] LLH;
delete [] HHL;
delete [] HLH;
delete [] LHH;
delete [] HHH;

nx = nx / 2, ny = ny / 2, nz = nz / 2;
size = nx * ny * nz / 8;
LLL = new double[size], HLL = new double[size], LHL = new double[size],
LLH = new double[size], HHL = new double[size], HLH = new double[size],
LHH = new double[size], HHH = new double[size];

}else{
std::copy(HLL, HLL + size, inout.memptr() + endind);
std::copy(LHL, LHL + size, inout.memptr() + endind + size);
std::copy(LLH, LLH + size, inout.memptr() + endind + size * 2);
std::copy(HHL, HHL + size, inout.memptr() + endind + size * 3);
std::copy(HLH, HLH + size, inout.memptr() + endind + size * 4);
std::copy(LHH, LHH + size, inout.memptr() + endind + size * 5);
std::copy(HHH, HHH + size, inout.memptr() + endind + size * 6);
std::copy(LLL, LLL + size, inout.memptr() + endind + size * 7);
endind = endind + 7 * size;

delete [] LLL;
delete [] HLL;
delete [] LHL;
delete [] LLH;
delete [] HHL;
delete [] HLH;
delete [] LHH;
delete [] HHH;
delete [] dat;

}
}

}else if(dim == 2){
size = nx * ny  / 4;
double *LL = new double[size], *HL = new double[size], *LH = new double[size],
*HH = new double[size];
for(int j = 0; j < J; j++) {
if(j == 0){
two_D_dwt(x.memptr(), &nx, &ny, &L, h, g, LL, LH, HL, HH);
}else{
two_D_dwt(dat, &nx, &ny, &L, h, g, LL, LH, HL, HH);
}

if(j < J - 1) {

delete [] dat;
dat = new double[size];
std::copy(LL, LL + size, dat);
std::copy(LH, LH + size, inout.memptr() + endind);
std::copy(HL, HL + size, inout.memptr() + endind + size);
std::copy(HH, HH + size, inout.memptr() + endind + size * 2);
endind = endind + 3 * size;

delete [] LL;
delete [] LH;
delete [] HL;
delete [] HH;

nx = nx / 2, ny = ny / 2;
size = nx * ny  / 4;
LL = new double[size], HL = new double[size], LH = new double[size],
HH = new double[size];

}else{

std::copy(LH, LH + size, inout.memptr() + endind);
std::copy(HL, HL + size, inout.memptr() + endind + size);
std::copy(HH, HH + size, inout.memptr() + endind + size * 2);
std::copy(LL, LL + size, inout.memptr() + endind + size * 3);
endind = endind + 3 * size;

delete [] LL;
delete [] LH;
delete [] HL;
delete [] HH;
delete [] dat;

}
}

}else{//1d
size = nx / 2;
double *V = new double[size], *W = new double[size];

for(int j = 0; j < J; j++) {

if(j == 0){
dwt(x.memptr(), &nx, &L, h, g, W, V);
}else{
dwt(dat, &nx, &L, h, g, W, V);
}

if(j < J - 1) {
delete [] dat;
dat = new double[size];
std::copy(V, V + size, dat);
std::copy(W, W + size, inout.memptr() + endind);
endind = endind + size;
delete [] V;
delete [] W;
nx = nx / 2;
size = nx / 2;
V = new double[size], W = new double[size];
}else{
std::copy(W, W + size, inout.memptr() + endind);
std::copy(V, V + size, inout.memptr() + endind + size);
delete [] V;
delete [] W;
delete [] dat;

}
}

}
return  inout;

}

////////////////// inverse wavelet transform for 1d,2d,3d
arma::mat iwt(arma::mat x, int dim, int L, double *h, double *g,
int J, int p1, int p2, int p3 ,
arma::mat inout){
int nx = p1 / pow(2, J), ny = p2 / pow(2, J), nz = p3 / pow(2, J);
int p = p1 * p2 * p3;
int size, startind;
double *im = nullptr;

if(dim == 3){
double *LLL  = nullptr , *HLL  = nullptr , *LHL = nullptr,*LLH  = nullptr,
*HHL  = nullptr, *HLH  = nullptr, *LHH  = nullptr, *HHH  = nullptr;

for(int j = 0; j < J ; j++) {
size = nx * ny * nz;
LLL = new double[size], HLL = new double[size], LHL = new double[size],
LLH = new double[size], HHL = new double[size], HLH = new double[size],
                      LHH = new double[size], HHH = new double[size];
if(j == 0){
startind = p - 8 * size;
std::copy(x.memptr() + startind + 7 * size, x.memptr() + startind + 8 * size, LLL);
}else{
startind = startind - 7 * size;
std::copy(im, im +  size, LLL);
delete [] im;
}
//todo this copy seems inefficient...can be done with pointers?
std::copy(x.memptr() + startind, x.memptr() + startind + size, HLL);
std::copy(x.memptr() + startind + size, x.memptr() + startind + 2 * size, LHL);
std::copy(x.memptr() + startind + 2 * size, x.memptr() + startind + 3 * size, LLH);
std::copy(x.memptr() + startind + 3 * size, x.memptr() + startind + 4 * size, HHL);
std::copy(x.memptr() + startind + 4 * size, x.memptr() + startind + 5 * size, HLH);
std::copy(x.memptr() + startind + 5 * size, x.memptr() + startind + 6 * size, LHH);
std::copy(x.memptr() + startind + 6 * size, x.memptr() + startind + 7 * size, HHH);
im = new double[8 * size];

three_D_idwt(LLL, HLL, LHL, LLH, HHL, HLH, LHH, HHH, &nx, &ny, &nz, &L, h, g, im);
nx = nx * 2, ny = ny * 2, nz = nz * 2;
delete [] LLL;
delete [] HLL;
delete [] LHL;
delete [] LLH;
delete [] HHL;
delete [] HLH;
delete [] LHH;
delete [] HHH;

}

}else if(dim == 2){//2d

double *LL  = nullptr , *HL  = nullptr , *LH = nullptr, *HH  = nullptr;

for(int j = 0; j < J ; j++) {
size = nx * ny;
LL = new double[size], HL = new double[size], LH = new double[size],
HH = new double[size];
if(j == 0){
startind = p - 4 * size;
std::copy(x.memptr() + startind + 3 * size, x.memptr() + startind + 4 * size, LL);
}else{
startind = startind - 3 * size;
std::copy(im, im +  size, LL);
delete [] im;
}
//todo this copy seems inefficient...can be done with pointers?
std::copy(x.memptr() + startind, x.memptr() + startind + size, LH);
std::copy(x.memptr() + startind + size, x.memptr() + startind + 2 * size, HL);
std::copy(x.memptr() + startind + 2 * size, x.memptr() + startind + 3 * size, HH);
im = new double[4 * size];
two_D_idwt(LL, LH, HL, HH, &nx, &ny, &L, h, g, im);
nx = nx * 2, ny = ny * 2;
delete [] LL;
delete [] HL;
delete [] LH;
delete [] HH;

}

}else{//1d
double *V = nullptr, *W  = nullptr;

for(int j = 0; j < J ; j++) {
size = nx ;
V = new double[size], W = new double[size] ;
if(j == 0){
startind = p - 2 * size;
std::copy(x.memptr() + startind + size, x.memptr() + startind + 2 * size , V);
}else{
startind = startind - size;
std::copy(im, im + size, V);
}
//todo this copy seems inefficient...can be done with pointers?
std::copy(x.memptr() + startind, x.memptr() + startind + size , W);
delete [] im;
im = new double[2 * size];
idwt(W, V, &nx,   &L, h,  g, im);
nx = nx * 2 ;
delete [] V;
delete [] W;

}
}

std::copy(im, im + p1 * p2 * p3, inout.memptr());
delete [] im;
return  inout;

}


void qmf(int L, double* g, double* h){
for(int i = 0; i < L; i++){
h[i] = pow(-1, i) * g[L - 1 - i]; // ^(0:(L - 1)) * rev(g)
}
}
int get_L(std::string wf){
int Lwave;
if(wf == "haar"){Lwave = 2;}
if(wf == "d4"){Lwave = 4;}
if(wf == "??"){Lwave = 4;}
if(wf == "mb4"){Lwave = 4;}
if(wf == "fk4"){Lwave = 4;}
if(wf == "d6"){Lwave = 6;}
if(wf == "fk6"){Lwave= 6;}
if(wf == "d8"){Lwave =8;}
if(wf == "fk8"){Lwave =8;}
if(wf == "la8"){Lwave =8;}
if(wf == "mb8"){Lwave =8;}
if(wf == "bl14"){Lwave =14;}
if(wf == "fk14"){Lwave =14;}
if(wf == "d16"){Lwave =16;}
if(wf == "la16"){Lwave =16;}
if(wf == "mb16"){Lwave =16;}
if(wf == "la20"){Lwave =20;}
if(wf == "bl20"){Lwave =20;}
if(wf == "fk22"){Lwave =22;}
if(wf == "mb24"){Lwave =24;}
return Lwave;
}

void wave_filter(std::string filt,  double * g, double * h, int L){
if(filt == "haar"){
//int L = 2;
double tmp[2] = {0.7071067811865475, 0.7071067811865475};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "d4"){
//int L = 4;
double tmp[4] = {0.4829629131445341, 0.8365163037378077, 0.2241438680420134,
-0.1294095225512603};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "e"){
//int L = 4;
double tmp[4] = {-0.125,  0.375,  0.375, -0.125};
std::copy(tmp, tmp + L , g);
double tmp2[4] = {-0.125,  0.375, -0.375,  0.125};
std::copy(tmp2, tmp2 + L , h);
}
if(filt == "mb4"){
//int L = 4;
double tmp[4] = {4.801755e-01, 8.372545e-01, 2.269312e-01, -1.301477e-01};
std::copy(tmp, tmp +  L, g);
qmf(L, g, h);
}
if(filt == "fk4"){
//int L = 4;
double tmp[4] = {.6539275555697651, .7532724928394872, 0.05317922877905981,
-0.0461657148152177};
std::copy(tmp, tmp + L, g);
qmf(L, g, h);
}
if(filt == "d6"){
//int L = 6;
double tmp[6] = {0.3326705529500827, 0.8068915093110928, 0.4598775021184915,
-0.1350110200102546, -0.0854412738820267, 0.0352262918857096};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "fk6"){
int L=6;
double tmp[6] = {.4279150324223103, .8129196431369074, .3563695110701871,
-.1464386812725773, -.7717775740697006e-1, .4062581442323794e-1};
std::copy(tmp, tmp + L, g);
qmf(L, g, h);
}
if(filt == "d8"){
//int L = 8;
double tmp[8] = {0.2303778133074431, 0.7148465705484058, 0.6308807679358788,
-0.0279837694166834, -0.1870348117179132, 0.0308413818353661,
0.0328830116666778, -0.0105974017850021};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "fk8"){
//int L = 8;
double tmp[8] = {.3492381118637999, .7826836203840648, .4752651350794712,
-.9968332845057319e-1, -.1599780974340301, .4310666810651625e-1,
.4258163167758178e-1, -.1900017885373592e-1};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "la8"){
//int L = 8;
double tmp[8] = {-0.07576571478935668, -0.02963552764596039, 0.49761866763256290,
0.80373875180538600, 0.29785779560560505, -0.09921954357695636,
-0.01260396726226383, 0.03222310060407815};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "mb8"){
//int L = 8;
double tmp[8] = {0.064363450,  0.007106015, -0.110867300,  0.294785500,  0.735133100,
0.572577100,  0.018477510, -0.167361900};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "bl14"){
//int L = 14;
double tmp[14] = { 0.0120154192834842, 0.0172133762994439, -0.0649080035533744,
-0.0641312898189170, 0.3602184608985549, 0.7819215932965554,
0.4836109156937821, -0.0568044768822707, -0.1010109208664125,
0.0447423494687405, 0.0204642075778225, -0.0181266051311065,
-0.0032832978473081, 0.0022918339541009};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "fk14"){
//int L = 14;
double tmp[14] = {.2603717692913964, .6868914772395985, .6115546539595115,
.5142165414211914e-1, -.2456139281621916, -.4857533908585527e-1,
.1242825609215128, .2222673962246313e-1, -.6399737303914167e-1,
-.5074372549972850e-2, .2977971159037902e-1, -.3297479152708717e-2,
-.9270613374448239e-2, .3514100970435962e-2};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "d16"){
//int L = 16;
double tmp[16] = {0.0544158422431049, 0.3128715909143031, 0.6756307362972904,
0.5853546836541907, -0.0158291052563816, -0.2840155429615702,
0.0004724845739124, 0.1287474266204837, -0.0173693010018083,
-0.0440882539307952, 0.0139810279173995, 0.0087460940474061,
-0.0048703529934518, -0.0003917403733770, 0.0006754494064506,
-0.0001174767841248};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "la16"){
//int L = 16;
double tmp[16] = {-0.0033824159513594, -0.0005421323316355, 0.0316950878103452,
0.0076074873252848, -0.1432942383510542, -0.0612733590679088,
0.4813596512592012, 0.7771857516997478, 0.3644418948359564,
-0.0519458381078751, -0.0272190299168137, 0.0491371796734768,
0.0038087520140601, -0.0149522583367926, -0.0003029205145516,
0.0018899503329007};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "mb16"){
//int L = 16;
double tmp[16] = {0.005765899,  0.009620427, -0.049846980, -0.024838760,  0.054746280,
-0.019879860, -0.056566570,  0.234534200,  0.670164600,  0.634922800,  0.118872500, -0.227835900,
-0.057765700,  0.113611600,  0.021736770, -0.013027700};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "la20"){
//int L = 20;
double tmp[20] = {0.0007701598091030, 0.0000956326707837, -0.0086412992759401,
-0.0014653825833465, 0.0459272392237649, 0.0116098939129724,
-0.1594942788575307, -0.0708805358108615, 0.4716906668426588,
0.7695100370143388, 0.3838267612253823, -0.0355367403054689,
-0.0319900568281631, 0.0499949720791560, 0.0057649120455518,
-0.0203549398039460, -0.0008043589345370, 0.0045931735836703,
0.0000570360843390, -0.0004593294205481};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "bl20"){
//int L = 20;
double tmp[20] = {0.0008625782242896, 0.0007154205305517, -0.0070567640909701,
0.0005956827305406, 0.0496861265075979, 0.0262403647054251,
-0.1215521061578162, -0.0150192395413644, 0.5137098728334054,
0.7669548365010849, 0.3402160135110789, -0.0878787107378667,
-0.0670899071680668, 0.0338423550064691, -0.0008687519578684,
-0.0230054612862905, -0.0011404297773324, 0.0050716491945793,
0.0003401492622332, -0.0004101159165852};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "fk22"){
//int L = 22;
double tmp[22] = {.1938961077599566, .5894521909294277, .6700849629420265,
.2156298491347700, -.2280288557715772, -.1644657152688429,
.1115491437220700, .1101552649340661, -.6608451679377920e-1,
-.7184168192312605e-1, .4354236762555708e-1, .4477521218440976e-1,
-.2974288074927414e-1, -.2597087308902119e-1, .2028448606667798e-1,
.1296424941108978e-1, -.1288599056244363e-1, -.4838432636440189e-2,
.7173803165271690e-2, .3612855622194901e-3, -.2676991638581043e-2,
.8805773686384639e-3};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
if(filt == "mb24"){
//int L = 24;
double tmp[24] = {5.361301e-05,  1.193006e-03, -2.062335e-03, -1.644859e-02,
1.027154e-02,  1.479342e-01, -4.575448e-03, -3.622424e-01, -3.091111e-01,
2.556731e-01,  6.176385e-01, 4.581101e-01,  1.949147e-01,  1.243531e-01,
1.689456e-01,  1.019512e-01, -6.559513e-03, -2.658282e-03,  4.199576e-02,
-1.482995e-03, -4.879053e-03,  7.456041e-04,  4.745736e-04, -2.132706e-05};
std::copy(tmp, tmp + L , g);
qmf(L, g, h);
}
}


/*   select.bs3.1 <- function() {
L <- 4
g <- {0.1767767, 0.5303301, 0.5303301, 0.1767767)
h <- qmf(g)
gd <- {0.3535534, 1.06066, -1.06066, -0.3535534)
hd <- qmf(g)
return(list(length = L, hpf = h, lpf = g, dhpf = hd, dlpf = gd))
} */






