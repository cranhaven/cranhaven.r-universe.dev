#include<iostream>
#include<fstream>
//#include<assert.h>
#include<math.h>
#include <cmath>
#include "vemema.h"
#include <string.h>
//#include <windows.h>
//#include <ctype.h>
//#include <Rmath.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
# define USE_RINTERNALS


/**
	Exported function for calling from R-code
*/

//extern "C" __declspec(dllexport) 
extern"C" {
    
double tol; /// convergence criterium.
int numiter;
int k;
int N;
//int iterat;
   vector<vector<double> > mysample;
   vector<vector<double> > lambda;
   
   vector<vector<double> > var;

    vector<double> prob;
    
    vector<double> corr;
    vector<double> indexx;
vector<double> lik;

vector<vector<double> > res;
vector<double>resi;
vector<double> gl;

  
     
////1.Wrapper f?r VEM univariate Daten

SEXP vem_uni(SEXP ra, SEXP klass,SEXP num,SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
double tol=REAL(acc)[0];
double* x=REAL(ra);
vector<double> a(x, x + Rf_length (ra));
mysample.clear();
lik.clear();
 gl.clear();
 res.clear();
mysample.push_back(a);
N = mysample.at(0).size();
  vector<vector<double> > res = vemema.do_vem(k,tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);  
  for(int i=0; i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}   

////2.Wrapper f?r VEM (kurz) bivariate Daten

SEXP vem_bi_sh(SEXP ra, SEXP rn, SEXP klass,SEXP num, SEXP acc){
VEMEMA vemema;
    k = INTEGER(klass)[0];
    numiter = INTEGER(num)[0];
    tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
mysample.clear();
lik.clear();
mysample.push_back(a);
mysample.push_back(n);
   gl.clear();
   res.clear();
N = mysample.at(0).size();
  res = vemema.f1();  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
  for(int i=0; i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
} 


////3.Wrapper f?r VEM (lang) bivariate Daten

SEXP vem_bi(SEXP ra, SEXP rn,SEXP klass,SEXP num,SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
mysample.clear();
 gl.clear();
 res.clear();
 lik.clear();
mysample.push_back(a);
mysample.push_back(n);
N = mysample.at(0).size();
//Rprintf("N =%d \n",N);
 res = vemema.vem_bivariate(k,tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
   
  for(int i=0; i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
} 

SEXP vem_bi_grad(SEXP ra, SEXP rn,SEXP klass,SEXP num,SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
mysample.clear();
mysample.push_back(a);
mysample.push_back(n);
   gl.clear();
   res.clear();
N = mysample.at(0).size();
//Rprintf("N =%d \n",N);
 res = vemema.vem_bivariate_grad(k,tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
  
  for(int i=0; i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}
////4.Wrapper f?r EM  Indikator f?r univariaten Daten 
SEXP ema_ind_uni(SEXP ra,SEXP klass,SEXP num,SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
vector<double> a(x, x + Rf_length (ra));
mysample.clear();
resi.clear();
mysample.push_back(a);
N = mysample.at(0).size();
 resi= vemema.ema_ind_uni(tol);  
  SEXP rres;
  PROTECT(rres=Rf_allocVector(REALSXP, resi.size()));
double* res0=REAL(rres);
for (int i=0; i<(int)resi.size(); i++){
res0[i]=resi.at(i);}
UNPROTECT(1);
return rres;
}   

////5.Wrapper f?r EM f?r univariaten Daten 
SEXP ema_uni(SEXP ra, SEXP klass,SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
vector<double> a(x, x + Rf_length (ra));
mysample.clear();
mysample.push_back(a);
 gl.clear();
 res.clear();
N = mysample.at(0).size();
  res = vemema.ema_uni(tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
  for(int i=0; i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}   


////6. Wrapper f?r EM (lang) Indikator f?r biivariate Daten 
SEXP ema_ind(SEXP ra, SEXP rn, SEXP klass,SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
mysample.clear();
resi.clear();
mysample.push_back(a);
mysample.push_back(n);
N = mysample.at(0).size();
   resi= vemema.ema_ind(tol);  
  SEXP rres;
  PROTECT(rres=Rf_allocVector(REALSXP, resi.size()));
double* res0=REAL(rres);
for (int i=0; i<(int)resi.size(); i++){
res0[i]=resi.at(i);}
UNPROTECT(1);
return rres;
}   

////7. Wrapper f?r EM (kurz) Indikator f?r biivariate Daten 
SEXP ema_ind_sh(SEXP ra, SEXP rn, SEXP klass,SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
mysample.clear();
resi.clear();
mysample.push_back(a);
mysample.push_back(n);
N = mysample.at(0).size();
   resi = vemema.ema_ind_sh(tol);  
  SEXP rres;
  PROTECT(rres=Rf_allocVector(REALSXP, resi.size()));
double* res0=REAL(rres);
for (int i=0; i<(int)resi.size(); i++){
res0[i]=resi.at(i);}
UNPROTECT(1);
return rres;
}

////8.Wrapper f?r EM (kurz) f?r bivariate Daten und univariate Verteilung
SEXP ema_univariat(SEXP ra, SEXP rn, SEXP klass, SEXP num, SEXP acc){
VEMEMA vemema;
numiter = INTEGER(num)[0];
k = INTEGER(klass)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
mysample.clear();
 gl.clear();
 res.clear();
mysample.push_back(a);
mysample.push_back(n);
N = mysample.at(0).size();
   res = vemema.ema_univariat(tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
   
  for(int i=0; i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
} 

////9.Wrapper f?r EM  Indikator f?r bivariate Daten mit Startwerte
SEXP ema_ind_st(SEXP ra, SEXP rn, SEXP lam1, SEXP lam2, SEXP pr,SEXP num, SEXP acc){
VEMEMA vemema;
//k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
//double* d=REAL(rv1);
//double* z=REAL(rv2);
//double* c=REAL(cor);
double* e=REAL(lam1);
double* f=REAL(lam2);
double* p=REAL(pr);

vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> l1(e, e + Rf_length (lam1));
vector<double> l2(f, f + Rf_length (lam2));
vector<double> pro(p, p + Rf_length (pr));
mysample.clear();
//var.clear();
lambda.clear();
prob.clear();
resi.clear();
mysample.push_back(a);
mysample.push_back(n);
//var.push_back(v1);
//var.push_back(v2);
lambda.push_back(l1);
lambda.push_back(l2);
prob=pro;
//corr=co;
N = mysample.at(0).size();
  resi = vemema.ema_ind_start(tol);  
  
  SEXP rres;
  PROTECT(rres=Rf_allocVector(REALSXP, resi.size()));
double* res0=REAL(rres);
for (int i=0; i<(int)resi.size(); i++){
res0[i]=resi.at(i);}
UNPROTECT(1);
return rres;
} 


// PD am 10.4.2023: Auskommentiert, da nicht in CAMAN.R exportiert
// ////10.Wrapper f?r EM (lang) f?r bivariate Daten 
// SEXP ema_versh(SEXP ra, SEXP rn, SEXP klass, SEXP num, SEXP acc){
// VEMEMA vemema;
// k = INTEGER(klass)[0];
// numiter = INTEGER(num)[0];
// tol=REAL(acc)[0];
// double* x=REAL(ra);
// double* y=REAL(rn);
// vector<double> a(x, x + Rf_length (ra));
// vector<double> n(y, y + Rf_length (rn));
// mysample.clear();
//    gl.clear();
//    res.clear();
// mysample.push_back(a);
// mysample.push_back(n);
// N = mysample.at(0).size();
//    res = vemema.ema_versh(tol);  
//   SEXP rres;
//    PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
//    double* res0=REAL(rres);
//   for(int i=0; i<(int)res.size(); ++i){
//   for(int j=0; j<(int)res.at(0).size(); ++j){
//     gl.push_back(res.at(i).at(j));
// }
// }
// for (int i=0; i<(int)gl.size(); i++){
//        
// res0[i]=gl.at(i);}
// UNPROTECT(1);
// return rres;
// }  

////11.Wrapper f?r EM (kurz) f?r bivariate Daten 
SEXP ema_versh_sh(SEXP ra, SEXP rn, SEXP klass, SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
mysample.clear();
mysample.push_back(a);
mysample.push_back(n);
lik.clear();
 gl.clear();
 res.clear();
N = mysample.at(0).size();
  res = vemema.ema_versh_sh(tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
  for(int i=0; i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}  


////12.Wrapper f?r EM (kurz) f?r bivariate Daten mit Startwerte
SEXP ema_versh_st(SEXP ra, SEXP rn, SEXP lam1, SEXP lam2, SEXP pr, SEXP num, SEXP acc){
VEMEMA vemema;
//k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
//double* d=REAL(rv1);
//double* z=REAL(rv2);
//double* c=REAL(cor);
double* e=REAL(lam1);
double* f=REAL(lam2);
double* p=REAL(pr);

vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
//vector<double> v1(d, d + Rf_length (rv1));
//vector<double> v2(z, z + Rf_length (rv2));
//vector<double> co(c, c + Rf_length (cor));
vector<double> l1(e, e + Rf_length (lam1));
vector<double> l2(f, f + Rf_length (lam2));
vector<double> pro(p, p + Rf_length (pr));
mysample.clear();
lik.clear();
lambda.clear();
prob.clear();
 gl.clear();
 res.clear();
//corr.clear();
mysample.push_back(a);
mysample.push_back(n);
//var.push_back(v1);
//var.push_back(v2);
lambda.push_back(l1);
lambda.push_back(l2);
prob=pro;
//corr=co;
N = mysample.at(0).size();
  res = vemema.ema_versh_start(tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
  for(int i=0;i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}      



////13.Wrapper f?r VEM (lang) meta Daten 
SEXP vem_versh_meta(SEXP ra, SEXP rn, SEXP rv1, SEXP rv2, SEXP klass, SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
double* d=REAL(rv1);
double* z=REAL(rv2);


vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> v1(d, d + Rf_length (rv1));
vector<double> v2(z, z + Rf_length (rv2));


mysample.clear();
var.clear();
lik.clear();
   gl.clear();
   res.clear();
mysample.push_back(a);
mysample.push_back(n);
var.push_back(v1);
var.push_back(v2);
N = mysample.at(0).size();
  res = vemema.vem_bivariate_meta(k,tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
  for(int i=0;i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}

////14.Wrapper f?r VEM (kurz) meta Daten 
SEXP vem_versh_meta_sh(SEXP ra, SEXP rn, SEXP rv1, SEXP rv2, SEXP klass, SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
double* d=REAL(rv1);
double* z=REAL(rv2);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> v1(d, d + Rf_length (rv1));
vector<double> v2(z, z + Rf_length (rv2));
mysample.clear();
var.clear();
lik.clear();
res.clear();
   gl.clear();
mysample.push_back(a);
mysample.push_back(n);
var.push_back(v1);
var.push_back(v2);


N = mysample.at(0).size();
  res = vemema.f1_meta();  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
  for(int i=0;i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}

////15.Wrapper f?r EM (lang) Indikator meta Daten 
SEXP ema_ind_meta(SEXP ra, SEXP rn, SEXP rv1, SEXP rv2, SEXP klass,SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
double* d=REAL(rv1);
double* z=REAL(rv2);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> v1(d, d + Rf_length (rv1));
vector<double> v2(z, z + Rf_length (rv2));

mysample.clear();
var.clear();
lambda.clear();
prob.clear();
resi.clear();
mysample.push_back(a);
mysample.push_back(n);
var.push_back(v1);
var.push_back(v2);

N = mysample.at(0).size();
  resi = vemema.ema_ind_meta(tol);  
  SEXP rres;
  PROTECT(rres=Rf_allocVector(REALSXP, resi.size()));
double* res0=REAL(rres);
for (int i=0; i<(int)resi.size(); i++){
res0[i]=resi.at(i);}
UNPROTECT(1);
return rres;
} 

////16.Wrapper f?r EM (kurz) Indikator meta Daten 
SEXP ema_ind_meta_sh(SEXP ra, SEXP rn, SEXP rv1, SEXP rv2, SEXP klass, SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
double* d=REAL(rv1);
double* z=REAL(rv2);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> v1(d, d + Rf_length (rv1));
vector<double> v2(z, z + Rf_length (rv2));

mysample.clear();
var.clear();
lambda.clear();
prob.clear();
resi.clear();
mysample.push_back(a);
mysample.push_back(n);
var.push_back(v1);
var.push_back(v2);

N = mysample.at(0).size();
 resi = vemema.ema_ind_meta_sh(tol);  
  SEXP rres;
  PROTECT(rres=Rf_allocVector(REALSXP, resi.size()));
double* res0=REAL(rres);
for (int i=0; i<(int)resi.size(); i++){
res0[i]=resi.at(i);}
UNPROTECT(1);
return rres;
} 




////17.Wrapper f?r EM  f?r meta Daten mit Startwerte
SEXP ema_meta_st(SEXP ra, SEXP rn, SEXP rv1, SEXP rv2, SEXP lam1, SEXP lam2, SEXP pr, SEXP num, SEXP acc){
VEMEMA vemema;
//k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
double* d=REAL(rv1);
double* z=REAL(rv2);

double* e=REAL(lam1);
double* f=REAL(lam2);
double* p=REAL(pr);

vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> v1(d, d + Rf_length (rv1));
vector<double> v2(z, z + Rf_length (rv2));

vector<double> l1(e, e + Rf_length (lam1));
vector<double> l2(f, f + Rf_length (lam2));
vector<double> pro(p, p + Rf_length (pr));
mysample.clear();
var.clear();
lambda.clear();
prob.clear();
lik.clear();
mysample.push_back(a);
mysample.push_back(n);
var.push_back(v1);
var.push_back(v2);
lambda.push_back(l1);
lambda.push_back(l2);
gl.clear();
res.clear();
prob=pro;
N = mysample.at(0).size();
  res = vemema.ema_versh_meta(tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
   
  for(int i=0;i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}

UNPROTECT(1);
return rres;
}


////18.Wrapper f?r EM  Indikator f?r meta Daten mit varianzen und Startwerten
SEXP ema_ind_meta_st(SEXP ra, SEXP rn, SEXP rv1, SEXP rv2, SEXP lam1, SEXP lam2, SEXP pr, SEXP num, SEXP acc){
VEMEMA vemema;
//k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
double* d=REAL(rv1);
double* z=REAL(rv2);

double* e=REAL(lam1);
double* f=REAL(lam2);
double* p=REAL(pr);

vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> v1(d, d + Rf_length (rv1));
vector<double> v2(z, z + Rf_length (rv2));

vector<double> l1(e, e + Rf_length (lam1));
vector<double> l2(f, f + Rf_length (lam2));
vector<double> pro(p, p + Rf_length (pr));
mysample.clear();
var.clear();
lambda.clear();
prob.clear();
resi.clear();
mysample.push_back(a);
mysample.push_back(n);
var.push_back(v1);
var.push_back(v2);
lambda.push_back(l1);
lambda.push_back(l2);
prob=pro;
N = mysample.at(0).size();
resi = vemema.ema_ind_meta_start(tol);  
  SEXP rres;
  PROTECT(rres=Rf_allocVector(REALSXP, resi.size()));
double* res0=REAL(rres);
for (int i=0; i<(int)resi.size(); i++){
res0[i]=resi.at(i);}
UNPROTECT(1);
return rres;
} 

////19.Wrapper f?r EM (lang) f?r meta Daten 
SEXP ema_meta(SEXP ra, SEXP rn, SEXP rv1, SEXP rv2, SEXP klass, SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
double* d=REAL(rv1);
double* z=REAL(rv2);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> v1(d, d + Rf_length (rv1));
vector<double> v2(z, z + Rf_length (rv2));

mysample.clear();
var.clear();
mysample.push_back(a);
mysample.push_back(n);
var.push_back(v1);
var.push_back(v2);
lik.clear();
res.clear();
   gl.clear();
N = mysample.at(0).size();
 res = vemema.ema_meta(tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
   
  for(int i=0;i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}


////20.Wrapper f?r EM (kurz) f?r meta Daten 
SEXP ema_meta_sh(SEXP ra, SEXP rn, SEXP rv1, SEXP rv2, SEXP klass, SEXP num, SEXP acc){
VEMEMA vemema;
k = INTEGER(klass)[0];
numiter = INTEGER(num)[0];
tol=REAL(acc)[0];
double* x=REAL(ra);
double* y=REAL(rn);
double* d=REAL(rv1);
double* z=REAL(rv2);
vector<double> a(x, x + Rf_length (ra));
vector<double> n(y, y + Rf_length (rn));
vector<double> v1(d, d + Rf_length (rv1));
vector<double> v2(z, z + Rf_length (rv2));

mysample.clear();
var.clear();
lik.clear();
  gl.clear();
  res.clear();
mysample.push_back(a);
mysample.push_back(n);
var.push_back(v1);
var.push_back(v2);

N = mysample.at(0).size();
   res = vemema.ema_meta_sh(tol);  
  SEXP rres;
   PROTECT(rres=Rf_allocVector(REALSXP,(res.size()*res.at(0).size())));
   double* res0=REAL(rres);
  
  for(int i=0;i<(int)res.size(); ++i){
  for(int j=0; j<(int)res.at(0).size(); ++j){
    gl.push_back(res.at(i).at(j));
}
}
for (int i=0; i<(int)gl.size(); i++){
       
res0[i]=gl.at(i);}
UNPROTECT(1);
return rres;
}

void VEMEMA::initialize(const char*name)
{
	ifstream in(name);
	//assert(in.good());

	mysample.clear();
	mysample.resize(2);
double s1, s2;
	while(in >> s1 >> s2)
	{
		mysample.at(0).push_back(s1);
		mysample.at(1).push_back(s2);
	}




	N = mysample.at(0).size();
Rprintf("%s \n", "BERECHNUNG STARTET"); 
//return mysample;
//printf("ok", s1);
}



double VEMEMA::mean(vector<double>& v)
{
	int i;
	double m = 0;
	int n = v.size();
	for (i=0; i<n; ++i)
	{
		m += v.at(i)/ N;
	}

	return m;
}








/// Funktion zur Berechnung der Varianz mit Empirischem Mittelwert
/// Engabe: v - Vektor der Beobachtungen
/// Ausgabe: var - Varianz des v
double VEMEMA::variance(vector<double>& v)
{
	double var = 0;

	double m = mean(v);

	int i;
	int n = v.size();
	for (i=0; i<n; ++i)
		var +=(v.at(i)-m)*(v.at(i)-m)/(n-1);

	return var;
}




/// Funktion zur Berechnung der Varianz mit unbekanntem Mittelwert
/// Engabe: v - Vektor der Beobachtungen
///         m -  Mittelwert
/// Ausgabe: var - Varianz des v
double VEMEMA::variance(vector<double>& v, double m)
{
	double var = 0;

	int n = v.size();
	int i;
	for (i=0; i<n; ++i)
		var +=(v.at(i)-m)*(v.at(i)-m)/(n-1);

	return var;
}





/// Funktion zur Berechnung den Korrelationskoeffizient f?r bivariaten Daten
/// Engabe: vec1 - ein Vektor der Beobachtungen aus X1, 
///         vec2 - ein Vektor der Beobachtungen aus X2, 
///         m1 -  Mittelwert des X1
///         m2 -  Mittelwert des X2
///         v1 -  Varianz des X1
///         v2 -  Varianz des X2
/// Ausgabe: Korrelationskoeffizient
double VEMEMA::correlation(vector<double>& vec1, vector<double>& vec2, double m1, double m2, double v1, double v2)
{
	int s = vec1.size();
	//assert(s == (int)vec2.size());

	int i;
	double c = 0.;

	for(i=0; i<s; ++i)
		c += (vec1.at(i) - m1) * (vec2.at(i) - m2)/s;

	return c/(sqrt(v1*v2));

}




/// Funktion f?r Definition einer Matrix mit Varianzen
/// Ausgabe: Matrix var.at(i).at(j)
/// lambda - Mittelwert der Subpopulationen
/// i - Nummer der Lambdas
/// j - Nummer der Komponenten (Klassen)
void VEMEMA::get_variance()
{
	int i, j;

	var.clear();
	var.resize(mysample.size());

	for(i=0; i<(int)lambda.size(); ++i){
		for(j=0; j<(int)lambda.at(i).size(); ++j){
			var.at(i).push_back(variance(mysample.at(i), lambda.at(i).at(j)));
		} /// end j-loop
	}
}




/// Funktion f?r Definition eines Vektors mit Korrelationskoefizienten 
/// Ausgabe: Vektor corr.at(j)
/// lambda - Mittelwert der Subpopulationen
/// j - Nummer der Komponenten
/// mysample.at(0) - Beobachtungen aus X1
/// mysample.at(1) - Beobachtungen aus X2
/// lamdba.at(0).at(j) - Mittelwerte des X1
/// lambda.at(1).at(j) - Mittelwerte des X2
/// var.at(0).at(j) - Varianz des X1
/// var.at(1).at(j) - Varianz des X2
void VEMEMA::get_corr()
{
	int j;

	corr.clear();
	for(j=0; j<(int)lambda.at(0).size(); ++j)
		corr.push_back(correlation(mysample.at(0), mysample.at(1), lambda.at(0).at(j), lambda.at(1).at(j), var.at(0).at(j), var.at(1).at(j)));
}




/// Funktion zur Berechnung der Dichte der Normalverteilung
/// Eingabe: x - Wert der Beobachtung
///          l - Mittelwerte
///          v - Varianzen
/// Ausgabe: Normalverteilungsdichte
double VEMEMA::normal(double k, double l, double v)
{
	//assert(v>0);
	return exp((l-k)*(k-l)/(2*v)) / sqrt(2*PI*v);
}




/// Funktion zur Berechnung der Dichte der bivariaten Normalverteilung
/// Eingabe: x1 - Werte mit Beobachtungen aus X1
///          x2 - Werte mit Beobachtungen aus X2
///          m1 -  Mittelwert des X1
///          m2 -  Mittelwert des X2
///          v1 -  Varianz des X1
///          v2 -  Varianz des X2
///          cor - Korrelationskoeffizient 
/// Ausgabe: bivariate Normalverteilungsdichte
double VEMEMA::normal_mult(double x1, double x2, double m1, double m2, double v1, double v2, double cor)
{

//	if(cor*cor>=1 || v1<=0 || v2<=0)
    // cout << "corr: " << cor << ", v1: " << v1 << ", v2: " << v2 << endl;
//	assert(cor*cor<1 && v1>0 && v2>0);

	double s1 = x1-m1;
	double s2 = x2-m2;

	double e = (s1*s1/v1 + s2*s2/v2 - 2*cor*s1*s2/sqrt(v1*v2)) / (2*(cor*cor-1));

	return exp(e) / (2*3.14159265*sqrt((1-cor*cor)*v1*v2));

}




/// Funktion zur Aufruf eines Funktions mit bivariater Normalverteilungsdichte
/// Eingabe: i - Nummer der Beobachtungen
///          j - Nummer der Komponenten (Klass)
/// Ausgabe: d - Wert der bivariate Normalverteilungsdichte
/// lambda - Mittelwert der Subpopulationen
/// mysample.at(0) - Beobachtungen aus X1
/// mysample.at(1) - Beobachtungen aus X2
/// lamdba.at(0).at(j) - Mittelwerte des X1
/// lambda.at(1).at(j) - Mittelwerte des X2
/// var.at(0).at(j) - Varianz des X1
/// var.at(1).at(j) - Varianz des X2
/// corr.at(j) - Korrelationskoeffizient
double VEMEMA::density(int i, int j)
{
	double d;

	d = normal_mult(mysample.at(0).at(i), mysample.at(1).at(i), lambda.at(0).at(j), lambda.at(1).at(j), var.at(0).at(j), var.at(1).at(j), corr.at(j));

	return d;

}
double VEMEMA::density_meta(int i, int j)
{
	double d;

	d = normal_mult(mysample.at(0).at(i), mysample.at(1).at(i), lambda.at(0).at(j), lambda.at(1).at(j), var.at(0).at(i), var.at(1).at(i), 0);

	return d;

}

double VEMEMA::density_uni(int i, int j)
{
  double d;

  d = normal(mysample.at(0).at(i), lambda.at(0).at(j), var.at(0).at(j));

  return d;

}

/// Funktion f?r Definition einer Matrix mit Werten der univariate Normalverteilungsdichte f?r bivariate Daten
/// Eingabe: x - ein Vektor mit Beobachtungen 
///          l - ein Vektor mit Mittelwerte
///          v - Varianz
///          d - Ergebnismatrix
void VEMEMA::get_dens(vector<double>& s, vector<double>& l, vector<vector<double> >& d)
{
	int n = s.size();
	int k = l.size();

	int i, j;

	double v = variance(s);

	d.clear();
	d.resize(n);

	for(i=0; i<n; ++i)
		for(j=0; j<k; ++j)
			d.at(i).push_back(normal(s.at(i),l.at(j),v));
}





/// Funktion f?r Definition einer Varianz, Korrelationskoeffizient,  Matrix mit Werten der bivariate Normalverteilungsdichte f?r bivariate Daten 
/// f?r verschiedene Varianzen und Korrelationskoeffizienten
/// Eingabe: l - Matrix mit Lambdas (Mittelwerte)
///          d - Ergebnissmatrix

void VEMEMA::get_dens(vector<vector<double> >& l, vector<vector<double> >& d)
{
	int i,j;


	vector<vector<double> > v; /// Matrix mit Varianzen
	v.clear();
	v.resize(l.size());
	for(i=0; i<(int)l.size(); ++i)
		for(j=0; j<(int)l.at(i).size(); ++j)
			v.at(i).push_back(variance(mysample.at(i), l.at(i).at(j)));

	vector<double> c; /// Vektor mit Korrelationskoeffizienten
	c.clear();
	for(j=0; j<(int)l.at(0).size(); ++j)
		c.push_back(correlation(mysample.at(0), mysample.at(1), l.at(0).at(j), l.at(1).at(j), v.at(0).at(j), v.at(1).at(j)));

	d.clear();
	d.resize(N);
	for(i=0; i<N; ++i)
		for(j=0; j<(int)l.at(0).size(); ++j)
			d.at(i).push_back(normal_mult(mysample.at(0).at(i), mysample.at(1).at(i), l.at(0).at(j), l.at(1).at(j), v.at(0).at(j), v.at(1).at(j), c.at(j)));


}



/// Funktion f?r Definition einer Varianz, Korrelationskoeffizient,  Matrix mit Werten der bivariate Normalverteilungsdichte f?r bivariate Daten 
/// f?r empirische Varianzen und Korrelationskoeffizienten
/// Eingabe: l - Matrix mit Lambdas (Mittelwerte)
///          d - Ergebnissmatrix
void VEMEMA::get_dens1(vector<vector<double> >& l, vector<vector<double> >& d)
{
	int i,j;

	double v0 = variance(mysample.at(0)); /// Empirische Varianz der erste Spalte der Beobachtungen
	double v1 = variance(mysample.at(1)); /// Empirische Varianz der zweite Spalte der Beobachtungen

	double l0 = mean(mysample.at(0)); /// Empirischer Mitterwert der erste Spalte der Beobachtungen
	double l1 = mean(mysample.at(1)); /// Empirischer Varianz der zweite Spalte der Beobachtungen
	//cout<<l0<<"   "<< v0;
	//cout<<endl;

	double c = correlation(mysample.at(0),mysample.at(1), l0, l1, v0, v1); ///Empirischer Korrelationskoefizient
	//cout<<c;

	d.clear();
	d.resize(N);
	for(i=0; i<N; ++i)
		for(j=0; j<(int)l.at(0).size(); ++j)
			d.at(i).push_back(normal_mult(mysample.at(0).at(i), mysample.at(1).at(i), l.at(0).at(j), l.at(1).at(j), v0, v1, c));


}

void VEMEMA::get_dens_meta(vector<vector<double> >& l, vector<vector<double> >& d)
{
	int i,j;

//	double v0 = variance(mysample.at(0)); /// Empirische Varianz der erste Spalte der Beobachtungen
//	double v1 = variance(mysample.at(1)); /// Empirische Varianz der zweite Spalte der Beobachtungen
//
//	double l0 = mean(mysample.at(0)); /// Empirischer Mitterwert der erste Spalte der Beobachtungen
//	double l1 = mean(mysample.at(1)); /// Empirischer Varianz der zweite Spalte der Beobachtungen
//	//cout<<l0<<"   "<< v0;
//	//cout<<endl;

//	double c = correlation(mysample.at(0),mysample.at(1), l0, l1, v0, v1); ///Empirischer Korrelationskoefizient
	//cout<<c;

	d.clear();
	d.resize(N);
	for(i=0; i<N; ++i)
		for(j=0; j<(int)l.at(0).size(); ++j)
			d.at(i).push_back(normal_mult(mysample.at(0).at(i), mysample.at(1).at(i), l.at(0).at(j), l.at(1).at(j),  var.at(0).at(i), var.at(1).at(i), 0));


}

/// Funktion zur Berechnung der Mischungsdichte der bivariate Normalverteilung 
/// Eingabe: i - Nummer der Beobachtung
/// Ausgabe: m_d - Wert der Mischungsdichte

double VEMEMA::mix_den(int i)
{

	int j;
	double m_d = 0;
	//Rprintf("Rf_length %d\n",(int)lambda.at(0).size());
	for(j=0; j<(int)lambda.at(0).size(); ++j)
		m_d += prob.at(j)*density(i,j);

	return m_d;
}

double VEMEMA::mix_den_meta(int i)
{

	int j;
	double m_d = 0;
	for(j=0; j<(int)lambda.at(0).size(); ++j)
		m_d += prob.at(j)*density_meta(i,j);

	return m_d;
}

double VEMEMA::mix_den_uni(int i)
{

  int j;
  double m_d = 0;
  for(j=0; j<(int)lambda.at(0).size(); ++j)
    m_d += prob.at(j)*density_uni(i,j);
  return m_d;

}
/// Funktion zur Berechnung der Mischungsdichte der univariate Normalverteilung 
/// Eingabe: i - Nummer der Beobachtung
///          p - Vektor der Mischungsgewichten
///          dens - Matrix mit univariate Normalverteiltedichte
/// Ausgabe: m_d - Wert der Mischungsdichte
double VEMEMA::mix_den(int i, vector<double>& p, vector<vector<double> >& dens)
{

	int j;
	double m_d = 0;
	for(j=0; j<(int)dens.at(i).size(); ++j)
		m_d += p.at(j)*dens.at(i).at(j);

	return m_d;
}




///Funktion zur Berechnung der Likelihood
/// Ausgabe : Wert der Likelihood 
/// i - Nummer der Beobachtungen

double VEMEMA::likelihood()
{
  int i;
  double l_k,logm;
 //Rprintf("mix_den %f \n",mix_den(0));
 //l_k = log(mix_den(0));


   l_k = 0.;

  for(i=0; i<N; ++i)
   {
	logm=mix_den(i);  
   if(logm > 1.E-12) l_k+= log(logm);
  }
//Rprintf("likelihood %f \n", l_k);
  return l_k;
}

double VEMEMA::likelihood_meta()
{
  int i;
  double l_k = 0.;

  for(i=0; i<N; ++i){
  		 
    l_k += log(mix_den_meta(i));
    
}
  return l_k;
}

double VEMEMA::likelihood_uni()
{
  int i;
  double l_k = 0.;

  for(i=0; i<N; ++i){
  		   
    l_k += log(mix_den_uni(i));

}
  return l_k;
}
/// Funktion zur Berechnung von Gradient
/// Eingabe: dens - Matrix mit univariate Normalverteiltedichte
///          p - Vektor der Mischungsgewichten
///          g - Vektor der Gradienten

void VEMEMA::gradient(vector<vector<double> >& dens, vector<double>& p, vector<double>& g)
{
	int i, j;
	int k = dens.at(0).size();

	g.clear();
	g.resize(k,0.);
//	Rprintf("N= ",N," k= ",k,"\n");
//	cin>>i;
	for(i=0; i<N; ++i)
	{
		double m_d = mix_den(i, p, dens);
		for(j=0; j<k; ++j)
			g.at(j) += dens.at(i).at(j)/(m_d*N);
	}
}





// void VEMEMA::sort(vector<double>& v, vector<double>& s_v)
// {

//   int i, j;

//   double tmp;

//   s_v = v;

//   for(i=0; i<N; ++i)
//     for(j=0; j<N-i-1; ++j)
//       if(s_v.at(j) > s_v.at(j+1))
// 	{
// 	  tmp       = s_v.at(j);
// 	  s_v.at(j)   = s_v.at(j+1);
// 	  s_v.at(j+1) = tmp;
// 	}

// }


void VEMEMA::get_min(vector<double>& v, double& min, int& i_min)
{
	// assert(v.size()>0);

	min = 1.;
	for(int i=0; i<(int)v.size(); ++i)
		if(v.at(i)<min && v.at(i)>0.01)
		{
			min = v.at(i);
			i_min = i;
		}
}


/// Funktion zur Berechnung der Positionen  der max. und  min. der Gradienten
/// Eingabe: v - Vektor der Gradienten
///          p - Vektor der Mischungsgewichten
///          i_max - Position der groessten Komponente des Gradients
///          i_min - Position der kleinsten Komponente des Gradients
///          v_max - Wert der Maximum
void VEMEMA::get_max_min(vector<double>& v, vector<double>& p, int& i_max, int& i_min, double& v_max)
{
	int i;

	i_max = 0;
	i_min = 0;

	double max = 1.e-7;
	double min = 1.e+7;

	for(i=0; i<(int)v.size(); ++i)
	{
		if(v.at(i) > max) { max = v.at(i); i_max = i; }
		if(v.at(i) < min && p.at(i)>0) { min = v.at(i); i_min = i; }
	}

	v_max = max;

}



/// Funktion zur Berechnung der min., max. der Gradienten
/// Eingabe: v - Vektor der Gradienten
///          max - Wert der Maximum
///          min - Wert der Minimum

void VEMEMA::get_max_min(vector<double>& v, double& max, double& min)
{
	int i;
	max = v.at(0);
	min = v.at(0);
	for(i=1; i<(int)v.size(); ++i)
	{
		if(v.at(i)>max) max = v.at(i);
		if(v.at(i)<min) min = v.at(i);
	}


}



/// F?nktion f?r Definition des Subpopulations der Mittelwerten und Mischungsgewichten f?r univariate NV
/// Eingabe: start_nr_cl - Nummer f?r Spalte von Beobachtungen
///          s - ein Vektor der Beobachtungen
///          l - ein Vektor der Mittelwerten
///          p - ein Vektor der Mischungsgewichten

void VEMEMA::get_start_values(int start_nr_cl, vector<double>& s, vector<double>& l, vector<double>& p)
{
	int i;

	//   int n = (N-2)/(start_nr_cl-2);

	//   vector<double> sort_mysample;
	//   sort(s, sort_mysample);

	//   l.clear();
	//   for(i=0; i<N; i+=n)
	//     l.push_back(sort_mysample.at(i));
	//   if(i>N) l.push_back(sort_mysample.at(N-1));

	//   p = vector<double>(start_nr_cl,1./start_nr_cl);

	double max, min;

	get_max_min(s,max,min);

	double stepRf_length = (max-min)/(start_nr_cl-1);

	l.clear();

	for(i=0; i<start_nr_cl; ++i)
	{
	 // cout<<"min= "<<min<<"step  "<<min+i*stepRf_length<<endl;
		l.push_back(min+i*stepRf_length);
	}
//cout<<"start"<<start_ nr_cl<<endl;
//cin>>i;
	p = vector<double>(start_nr_cl,1./start_nr_cl);

}




/// Funktion f?r Definition des Subpopulations der Mittelwerten und Mischungsgewichten f?r bivariate NV
/// Eingabe: start_nr_cl - Nummer f?r Spalte von Beobachtungen
///          s - ein Vektor der Beobachtungen
///          L - Matrix der Mittelwerten
///          p - ein Vektor der Mischungsgewichten
void VEMEMA::get_start_values(int start_nr_cl, vector<vector<double> >& L, vector<double>& p)
{
	int i;

//	double max, min;

	vector<double> s, l;

	vector<vector<double> > L_temp;
	L_temp.clear();

	for(i=0; i<(int)mysample.size(); ++i)
	{
		s = mysample.at(i);
		get_start_values(start_nr_cl, s, l, p);
		L_temp.push_back(l);
	}
//cout<<"grid ok"<<endl;
//cin>>i;
	grid(L_temp, L);

	p = vector<double>(L.at(0).size(),1./L.at(0).size());


}




/// Funktion f?r Definition eines Vektors mit Differenzen der Dichte und Mischungsdichte
/// Eingabe: i -  Position der groessten Komponente des Gradients
///          j -  Position der kleinsten Komponente des Gradients
///          dens - Matrix mit univariate Normalverteiltedichte
///          p - Vektor der Mischungsgewichten
///          ht - Vektor der Differenzen
void VEMEMA::get_ht(int i, int j, vector<vector<double> >& dens, vector<double>& p, vector<double>& ht)
{
	int ii;

	ht.clear();

	for(ii=0; ii<N; ++ii)
		ht.push_back((dens.at(ii).at(i) - dens.at(ii).at(j))*p.at(j));
}







double VEMEMA::stepsize(vector<double>& ht, vector<vector<double> >& dens, vector<double>& p)
 {
int i,j;

double s0,s11,s21, s2, sl,a,step,grad10, grad11,oldstep,grad1s,grad2s,b;
s0=0.;
s11=0.;
s21=0;
sl=0.;
 s2=0; 
grad10=0;
grad11=0;
vector<double>s1;

 s1.clear();
 s1.resize(N);
 vector<double>gradi;

 gradi.clear();
 gradi.resize(N);
  for(i=0; i<N; ++i)
      {
	s1[i] = mix_den(i,p,dens);
    }
   
   for(i=0;i<N;i++)
   {
   gradi[i]=0;
   if(s1[i] >1.E-12)
   gradi[i]=ht[i]/(s1[i]);
  // b=1.+a;
   if(abs(gradi[i]+1) >1.E-12)  
   sl=gradi[i]/(1+gradi[i]);
   grad10=grad10+gradi[i];
   if(abs(s1[i]+ht[i]) > 1.E-12) 
   grad11=grad11+ht[i]/(s1[i]+ht[i]);
   s11=s11+sl;//* freq.at(i);
   s0=s0+gradi[i];//freq.at(i)*ht[i];
   s2=s2-(gradi[i]*gradi[i]);
   s21=s21-sl*sl;
 }

 ////   Optimal step-Rf_length procedure

	step=0.;
        oldstep=0.;
//cccc    begin of iteration

//// Erste Methode
//        
//        
////ccc     computation of derivatives
//if((abs(grad11-grad10))>1.E-7) step=-grad10/(grad11-grad10);
//if(step > 1) step=1;
//if (step < 1.E-7) step=0;








///Zweite Mathode
 	
     
//step=s0/N*(s0+N-1);
//if(s2>1.E-8)
//step=-s0/s2;
//if(step > 1) step=1;
//if (step < 1.E-7) step=0;





//Dritte Methode

    	for(j=0;j<50;j++)
         {
          grad1s=0.;
          grad2s=0.;

	for (i=0;i<N;i++)
        {
	a=s1[i]+step*ht[i];
      //ut<<"a="<<a<<w[i]<<"=w"<<" s1= "<<s1[i]<<" ht="<<ht[i]<<endl;
	 if (abs(a)>1.E-12)
	 {
           b=ht[i]/a;
	   grad1s=grad1s+b;//freq.at(i)*b;
	   grad2s=grad2s-(b*b);
         }
        }

	if( abs(grad2s) > 1.E-12) step=step-grad1s/grad2s;
	if (step < 1.E-12) step=0;
       //	cout<<"step= "<<step<<" grad1s= "<<grad1s<<" grad2s "<<grad2s<<endl;
         if ( (oldstep>1.00) && (step>oldstep) 	)
	{

       // cout<<"step="<<step<<"oldstep="<<oldstep<<endl;
        step=1.;
        break;
        }
        
	if(grad1s <1.E-12)  break;


	oldstep=step;
	
     }

	if (step >1.0)
	 step=1.;

      	//ut<<"final step="<<step<<endl;




        return step;

 }


/// Funktion f?r Definition einer Matrix mit alle m?glichen Kombinationen der Lambdas (Mittelwerten)
/// Eingabe: v_in - Matrix mit (Lambdas) Mittelwerten 
///          v_out - Ergebnissmatrix 
void VEMEMA::grid(vector<vector<double> >& v_in, vector<vector<double> >& v_out)
{
	// assert(v_in.size()==2);

	int i,j;

	v_out.clear();
	v_out.resize(2);

	for(i=0; i<(int)v_in.at(0).size(); ++i)
		for(j=0; j<(int)v_in.at(1).size(); ++j)
		{
			v_out.at(0).push_back(v_in.at(0).at(i));
			v_out.at(1).push_back(v_in.at(1).at(j));
		}

}




/// Funktion f?r Berechnung des VEM-Algorithmus der univariate NV
/// Eingabe: col - Nummer der Spalte der Beobachtungen
///          start_nr_cl - Anzahl der Gridpunkte
///          l_out - Ergebnissvektor der Mittelwerten
///          p_out - Ergebnissvektor der Mischungsgewichten

void VEMEMA::vem(int col, int start_nr_cl, double tol, vector<double>& l_out, vector<double>& p_out)
{
	//assert(start_nr_cl <= N);
	//assert(col==0 || col==1);

	int i_min, i_max;
	double step;
	double grad_max=0;
	vector<vector<double> > dens;
	vector<double> grad;
	vector<double> s = mysample.at(col);
	vector<double> ht;
	vector<double> l;
	vector<double> p;

	get_start_values(start_nr_cl, s, l, p);
	get_dens(s, l, dens);
	gradient(dens, p, grad);
	get_max_min(grad, p, i_max, i_min, grad_max);

	int it=1;
	while(abs(grad_max-1.) > tol && it < numiter)
	{
		//cout << endl << "iteration: " << it << endl;
		//cout << "grad_max: " << grad_max << ", i_min: " << i_min << ", i_max: " << i_max <<endl;


		get_ht(i_max, i_min, dens, p, ht);

		step = stepsize(ht, dens, p);

		double h = p.at(i_min);
		p.at(i_min) -= step*h;
		p.at(i_max) += step*h;

		gradient(dens, p, grad);
		get_max_min(grad, p, i_max, i_min, grad_max);

		++it;
	}

	//cout << "nr. of iterations: " << --it << endl;

	l_out.clear();
	p_out.clear();
	for(int ii=0; ii<(int)p.size(); ++ii)
		if(p.at(ii) != 0)
		{
			//cout << p.at(ii) << "\t" << l.at(ii) << endl;
			l_out.push_back(l.at(ii));
			p_out.push_back(p.at(ii));
		}

}



///// Funktion f?r Berechnung des VEM-Algorithmus der bivariate NV
///// Eingabe: start_nr_col - Anzahl der Gridpunkte
/////          l_out - Ergebnissmatrix der Mittelwerten
/////          p_out - Ergebnissvektor der Mischungsgewichten
//void VEMEMA::vem_bivariate(int start_nr_cl, double tol)
//{
//  assert(start_nr_cl <= N);
//
//  //cout << endl << "### vertex-exchange-method ###" << endl;
//
//  int i,i_min, i_max;
//  double step;
//  double grad_max;
//
//  vector<vector<double> > l;
//  vector<double> p;
//  get_start_values(start_nr_cl, l, p);
//
//  vector<vector<double> > dens;
//  //get_dens(l, dens);
//  get_dens1(l, dens);
//
//  vector<double> grad;
//  gradient(dens, p, grad);
//
//  get_max_min(grad, p, i_max, i_min, grad_max);
//
//  int it=1;
// while(abs(grad_max-1.) > tol && it < ){
//
//  //while((grad_max-1.0) > tol)
//  
////       cout << endl << "iteration: " << it << endl;
////      cout << "grad_max: " << grad_max << ", i_min: " << i_min << ", i_max: " << i_max <<endl;
//
//      vector<double> ht;
//      get_ht(i_max, i_min, dens, p, ht);
//
//      step = stepsize(ht, dens, p);
//if(step<0) step=0;
//      double h = p.at(i_min);
//      p.at(i_min) -= step*h;
//      p.at(i_max) += step*h;
//
//      gradient(dens, p, grad);
//      get_max_min(grad, p, i_max, i_min, grad_max);
//
//      ++it;
//    //   cout << endl << "iteration: " << it << endl;
//   // cout << "grad_max: " << grad_max << ", i_min: " << i_min << ", i_max: " << i_max <<endl;
//    
//}
//  cout << "nr. of iterations: " << --it << endl;
//
//
//  double min;
//
//  for(i=0; i<(int)p.size(); ++i)
//    if(p.at(i)>0 && p.at(i)<0.01)
//      {
//	get_min(p,min,i_min);
//	p.at(i_min) += p.at(i);
//	p.at(i) = 0.;
//	l.at(0).at(i_min) = (l.at(0).at(i_min)+l.at(0).at(i))/2.;
//	l.at(1).at(i_min) = (l.at(1).at(i_min)+l.at(1).at(i))/2.;
//      }
//
//  lambda.clear();
//  lambda.resize(mysample.size());
//  prob.clear();
//
//  for(i=0; i<(int)p.size(); ++i)
//    if(p.at(i) != 0)
//      {
//	lambda.at(0).push_back(l.at(0).at(i));
//	lambda.at(1).push_back(l.at(1).at(i));
//	prob.push_back(p.at(i));
//      }
//
//
//  // print results *********************************
// // for(i=0; i<lambda.at(0).size(); ++i)
////    {
////      for(j=0; j<lambda.size(); ++j)
////	cout <<   "lambda(" << i << "," << j << "): " << lambda.at(j).at(i) << "\t";
////      cout << "prob: " << prob.at(i) << endl;
////    }
//
//  // **********************************************
//}


vector<vector<double> >  VEMEMA::vem_bivariate(int start_nr_cl, double tol)
{
	//assert(start_nr_cl <= N);

	int i,  i_min, i_max;
	double step;
	double grad_max=0.;

	vector<vector<double> > l;
	vector<double> p;
	l.clear();
	p.clear();

	get_start_values(start_nr_cl, l, p);
	
	//int j;

	
    vector<vector<double> > result;
	vector<vector<double> > dens;
	result.clear();
	dens.clear();
	//get_dens(l, dens);
	get_dens1(l, dens);

	vector<double> grad;
	grad.clear();
	gradient(dens, p, grad);

	get_max_min(grad, p, i_max, i_min, grad_max);

	int it=1;

	while(abs(grad_max-1.) > tol && it < numiter)
	{
		//cout << endl << "iteration: " << it << endl;
		//cout << "grad_max: " << grad_max << ", i_min: " << i_min << ", i_max: " << i_max <<endl;

		vector<double> ht;
		get_ht(i_max, i_min, dens, p, ht);

		step = stepsize(ht, dens, p);

		double h = p.at(i_min);
		p.at(i_min) -= step*h;
		p.at(i_max) += step*h;

		gradient(dens, p, grad);
		get_max_min(grad, p, i_max, i_min, grad_max);

		++it;
	}

	//cout << "nr. of iterations: " << --it << endl;
//ofstream out;
//out.open("grad.dat");

//out<<"p "<<"lambda1 "<<" lambda2 "<<" gradient"<<endl;
	double min;
	it=0;

	for(i=0;i<(int)p.size(); ++i)
		if(p.at(i)>0 && p.at(i)<0.01)
		{
			get_min(p,min,i_min);
			p.at(i_min) += p.at(i);
			p.at(i) = 0.;
			l.at(0).at(i_min) = (l.at(0).at(i_min)+l.at(0).at(i))/2.;
			l.at(1).at(i_min) = (l.at(1).at(i_min)+l.at(1).at(i))/2.;
		}

		lambda.clear();
		lambda.resize(mysample.size());
		prob.clear();
	//	Rprintf("test \n");
	//	ll=0;
//		for(int ii=0;ii<5;ii++)
	//	{
//ll=ll+mix_den(ii);
//Rprintf("ll= %f\n",ll);
//
//		}
	//	ll=likelihood();
//Rprintf("ll= %f\n",ll);		
		for(i=0; i<(int)p.size(); ++i)
			if(p.at(i) != 0)
			{
			//	out << p.at(i) << "\t" << l.at(0).at(i) << "\t" << l.at(1).at(i) <<"\t"<<grad.at(i)<<endl;
			  //Rprintf("p= %f lambda1= %f lambda2= %f\n", p.at(i), l.at(0).at(i),l.at(1).at(i));
			  //<<"\t"<<grad.at(i)<<endl;
				lambda.at(0).push_back(l.at(0).at(i));
				lambda.at(1).push_back(l.at(1).at(i));
				prob.push_back(p.at(i));
			}
			//cin.get();
			//   lambda = l_out;
			//   prob = p_out;
				

//out.close();
//Rprintf("test %s \n");
//ll=mix_den(0);
//Rprintf("ll= %f\n",ll);
			for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));

result.push_back(prob);
//Rprintf("jetzt \n");
//ll=likelihood();
//Rprintf("logl =%f\n",ll);


//for(i=0; i<(int)lambda.at(0).size(); ++i){
  //lik.push_back(ll);}
//result.push_back(lik);
//result.push_back(ll);
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}
vector<vector<double> >  VEMEMA::vem_bivariate_grad(int start_nr_cl, double tol)
{
  // assert(start_nr_cl <= N);

 // cout << endl << "### vertex-exchange-method ###" << endl;

  int i, i_min, i_max;
  double step;
  double grad_max=0.;
vector<vector<double> > result;
  vector<vector<double> > l;
  vector<double> p;
  get_start_values(start_nr_cl, l, p);
  
  vector<vector<double> > dens;
  //get_dens(l, dens);
  get_dens1(l, dens);

  vector<double> grad;
  grad.clear();
  gradient(dens, p, grad);

  get_max_min(grad, p, i_max, i_min, grad_max);

  int it=1;
 while(abs(grad_max-1.) > tol && it < numiter){

  //while((grad_max-1.0) > tol)
  
     // cout << endl << "iteration: " << it << endl;
    // cout << "grad_max: " << grad_max << ", i_min: " << i_min << ", i_max: " << i_max <<endl;

      vector<double> ht;
      get_ht(i_max, i_min, dens, p, ht);

      step = stepsize(ht, dens, p);

    //if(step<0) step=0;

      double h = p.at(i_min);
      p.at(i_min) -= step*h;
      p.at(i_max) += step*h;

      gradient(dens, p, grad);
      get_max_min(grad, p, i_max, i_min, grad_max);

      ++it;
    //  cout << endl << "iteration: " << it << endl;
    //cout << "grad_max: " << grad_max << ", i_min: " << i_min << ", i_max: " << i_max <<", step: "<<step <<", p_max: "<<p.at(i_max)<<endl;
    
 //   for (i=0; i<p.size();++i)
// cout<<"p: "<<p.at(i)<<endl; 
    
}
  //cout << "nr. of iterations: " << --it << endl;


  double min;

  for(i=0; i<(int)p.size(); ++i)
  //  if(p.at(i)>0 && p.at(i)<0.01)
      {
	get_min(p,min,i_min);
	p.at(i_min) += p.at(i);
	p.at(i) = 0.;
	l.at(0).at(i_min) = (l.at(0).at(i_min)+l.at(0).at(i))/2.;
	l.at(1).at(i_min) = (l.at(1).at(i_min)+l.at(1).at(i))/2.;
      }

  lambda.clear();
  lambda.resize(mysample.size());
  prob.clear();

  for(i=0; i<(int)p.size(); ++i)
    //if(p.at(i) != 0)
      {
	lambda.at(0).push_back(l.at(0).at(i));
	lambda.at(1).push_back(l.at(1).at(i));
	prob.push_back(p.at(i));
      }

//ll = likelihood();
  //cout<<"LL"<<ll<<endl;
  // print results *********************************
			for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));

result.push_back(prob);
result.push_back(grad);
//Rprintf("jetzt \n");
//ll=likelihood();
//Rprintf("logl =%f\n",ll);


//for(i=0; i<(int)lambda.at(0).size(); ++i){
  //lik.push_back(ll);}
//result.push_back(lik);
//result.push_back(ll);
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}

vector<vector<double> >  VEMEMA::vem_bivariate_meta(int start_nr_cl, double tol)
{
  // assert(start_nr_cl <= N);

  //cout << endl << "### vertex-exchange-method ###" << endl;

  int i,i_min, i_max;
  double step;
  double grad_max=0.;
  double ll;

  vector<vector<double> > l;
  vector<double> p;
  get_start_values(start_nr_cl, l, p);
vector<vector<double> > result;
result.clear();
  vector<vector<double> > dens;
  //get_dens(l, dens);
  get_dens_meta(l, dens);

  vector<double> grad;
  gradient(dens, p, grad);

  get_max_min(grad, p, i_max, i_min, grad_max);

  int it=1;
 while(abs(grad_max-1.) > tol && it < numiter){

  //while((grad_max-1.0) > tol)
  
//       cout << endl << "iteration: " << it << endl;
//      cout << "grad_max: " << grad_max << ", i_min: " << i_min << ", i_max: " << i_max <<endl;

      vector<double> ht;
      get_ht(i_max, i_min, dens, p, ht);

      step = stepsize(ht, dens, p);
if(step<0) step=0;
      double h = p.at(i_min);
      p.at(i_min) -= step*h;
      p.at(i_max) += step*h;

      gradient(dens, p, grad);
      get_max_min(grad, p, i_max, i_min, grad_max);

      ++it;
    //   cout << endl << "iteration: " << it << endl;
   // cout << "grad_max: " << grad_max << ", i_min: " << i_min << ", i_max: " << i_max <<endl;
    
}
 // cout << "nr. of iterations: " << --it << endl;


  double min;

  for(i=0; i<(int)p.size(); ++i)
    if(p.at(i)>0 && p.at(i)<0.01)
      {
	get_min(p,min,i_min);
	p.at(i_min) += p.at(i);
	p.at(i) = 0.;
	l.at(0).at(i_min) = (l.at(0).at(i_min)+l.at(0).at(i))/2.;
	l.at(1).at(i_min) = (l.at(1).at(i_min)+l.at(1).at(i))/2.;
      }

  lambda.clear();
  lambda.resize(mysample.size());
  prob.clear();

  for(i=0; i<(int)p.size(); ++i)
    if(p.at(i) != 0)
      {
	lambda.at(0).push_back(l.at(0).at(i));
	lambda.at(1).push_back(l.at(1).at(i));
	prob.push_back(p.at(i));
      }
ll = likelihood_meta();
			//cin.get();
			//   lambda = l_out;
			//   prob = p_out;
				for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(ll);}
  
			for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));

result.push_back(prob);
result.push_back(lik);

//Rprintf("%s \n", "VEM der bivariate NV"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
  // print results *********************************
 // for(i=0; i<lambda.at(0).size(); ++i)
//    {
//      for(j=0; j<lambda.size(); ++j)
//	cout <<   "lambda(" << i << "," << j << "): " << lambda.at(j).at(i) << "\t";
//      cout << "prob: " << prob.at(i) << endl;
//    }

  // **********************************************
}



vector<vector<double> >  VEMEMA::f1()
{
  int count, i;
  double ref0, ref1,ll;
  vem_bivariate(k,tol);
    get_variance();

  get_corr();
  vector<double>::iterator i0 = lambda.at(0).begin();
  vector<double>::iterator i1 = lambda.at(1).begin();
  vector<double>::iterator pr = prob.begin();
  vector<double>::iterator j0, j1, p;
vector<vector<double> > result;
  while(i0 != lambda.at(0).end())
    {
      j0 = i0+1;
      j1 = i1+1;
      p  = pr+1;

      count = 1;

      ref0 = *i0;
      ref1 = *i1;

      while(j0 != lambda.at(0).end())
	{
	  if(*j0==ref0 || *j1==ref1){
	    *i0 += *j0;
	    *i1 += *j1;
	    *pr += *p;
	    ++count;
	    j0 = lambda.at(0).erase(j0);
	    j1 = lambda.at(1).erase(j1);
	    p  = prob.erase(p);
	  }
	  else{
	    ++j0;
	    ++j1;
	    ++p ;
	  }
	}

      *i0 /= count;
      *i1 /= count;

      ++i0;
      ++i1;
      ++pr;
    }
ll = likelihood();
  				for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(ll);}
  // print results *********************************
  //cout << endl;
//  for(i=0; i<lambda.at(0).size(); ++i)
//    {
//      for(j=0; j<lambda.size(); ++j)
//	cout <<   "lambda(" << i << "," << j << "): " << lambda.at(j).at(i) << "\t";
//      cout << "prob: " << prob.at(i) << endl;
//    }


  // **********************************************
			for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));

result.push_back(prob);
result.push_back(lik);

//Rprintf("%s \n", "VEM der bivariate NV"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
 // }
//cin.get();
}

vector<vector<double> >  VEMEMA::f1_meta()
{
  int count, i;
  double ref0, ref1,ll;
  vem_bivariate_meta(k,tol);
  vector<double>::iterator i0 = lambda.at(0).begin();
  vector<double>::iterator i1 = lambda.at(1).begin();
  vector<double>::iterator pr = prob.begin();
  vector<double>::iterator j0, j1, p;

vector<vector<double> > result;
  while(i0 != lambda.at(0).end())
    {
      j0 = i0+1;
      j1 = i1+1;
      p  = pr+1;

      count = 1;

      ref0 = *i0;
      ref1 = *i1;

      while(j0 != lambda.at(0).end())
	{
	  if(*j0==ref0 || *j1==ref1){
	    *i0 += *j0;
	    *i1 += *j1;
	    *pr += *p;
	    ++count;
	    j0 = lambda.at(0).erase(j0);
	    j1 = lambda.at(1).erase(j1);
	    p  = prob.erase(p);
	  }
	  else{
	    ++j0;
	    ++j1;
	    ++p ;
	  }
	}

      *i0 /= count;
      *i1 /= count;

      ++i0;
      ++i1;
      ++pr;
    }
ll=likelihood_meta();
  				for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(ll);}
  // print results *********************************
  //cout << endl;
//  for(i=0; i<lambda.at(0).size(); ++i)
//    {
//      for(j=0; j<lambda.size(); ++j)
//	cout <<   "lambda(" << i << "," << j << "): " << lambda.at(j).at(i) << "\t";
//      cout << "prob: " << prob.at(i) << endl;
//    }
  // **********************************************
			for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));

result.push_back(prob);
result.push_back(lik);
//Rprintf("%s \n", "VEM der bivariate NV"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
 // }
//cin.get();
}

vector<vector<double> > VEMEMA::do_vem(int  start_nr_cl, double tol)
{
	int i, j, n;
double lh;
	vector<vector<double> > lam;
	vector<vector<double> > pr;
    vector<vector<double> > result;
	lam.clear();
	pr.clear();
   result.clear();
	for(i=0; i<(int)mysample.size(); ++i)
	{
		//cout << endl << "### VEM for " << i+1 << ". column ###" << endl;
		vector<double> l, p;
		vem(i, start_nr_cl, tol, l, p);
		lam.push_back(l);
		pr.push_back(p);
	}
	int m = lam.at(0).size();/// Anzahl beobachtungen in lambda
	for(i=1; i<(int)lam.size(); ++i) /// Anzahl lambdas
		if((int)lam.at(i).size()<m) m=lam.at(i).size();

	for(i=0; i<(int)lam.size(); ++i)
	{
		if((int)lam.at(i).size() > m)
		{
			n = lam.at(i).size()-m+1;
			lam.at(i).at(m-1) /= n;
			for(j=m; j<(int)lam.at(i).size(); ++j)
			{
				lam.at(i).at(m-1) += lam.at(i).at(j)/n;
				lam.at(i).at(j) = 0.;
				pr.at(i).at(m-1) += pr.at(i).at(j);
				pr.at(i).at(j) = 0.;
			}
		}
	}


	//cout << endl;
	lambda.clear();
	prob.clear();
	lambda.resize(lam.size());
	prob.resize(m,0.);
	for(i=0; i<(int)lam.size(); ++i)
		for(j=0; j<m; ++j)
		{
			lambda.at(i).push_back(lam.at(i).at(j));
			prob.at(j) += pr.at(i).at(j)/lam.size();
		}
		
		
	 get_variance();
    for(i=0; i<(int)mysample.at(0).size(); ++i){
      	for(j=0; j<(int)lambda.at(0).size(); ++j){
	density_uni(i,j);
	 mix_den_uni(i);}}
    lh = likelihood_uni();
       				for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(lh);}
  
  
  
for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));

result.push_back(prob);
result.push_back(lik);

		//print results ****************
//		for(i=0; i<lambda.size(); ++i)
//		{
//			cout << "lambda" << i << ": ";
//			for(j=0; j<lambda.at(i).size(); ++j)
//				cout << lambda.at(i).at(j) << " ";
//			cout << endl;
//		}
//
//		cout << "prob: ";
//		for(i=0; i<prob.size(); ++i)
//		{
//			cout << prob.at(i) << " ";
//		}
//		cout << endl;
		// ***************************
//Rprintf("%s \n", "VEM der univariate NV"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;

}



vector<double>VEMEMA::ema_ind(double tol)
{
  int i, j, l;
  int j_max=0;
  int it=1;
  //f1();
vem_bivariate(k, tol);
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h;
  //double lh, lh_new;

  get_variance();

  get_corr();
int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;

  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  vector<vector<double> > var_new;
  vector<double> corr_new;
  vector<double> prob_new;
vector<double> result;

//  cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
 while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
 dens.clear();
    double max;

    indexx.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den(i);

	e.at(i).clear();
	max = 0;
	for(j=0; j<k; ++j){
	  h = prob.at(j)*density(i,j)/den;
	  e.at(i).push_back(h);
	  if(h>max) { max = h; j_max = j; }
	}
	indexx.push_back(j_max);
      }
 vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    vector<vector<double> > var_new(dim,vector<double>(k,0.));
    vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();

    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      for(l=0; l<dim; ++l)
		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);

	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
    for(j=0; j<k; ++j)
      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));

  //  lh = likelihood();

	get_max_min(grad, prob_new, i_max, i_min, grad_max);


    lambda  = lambda_new;
    prob    = prob_new;
    var     = var_new;
    corr    = corr_new;

    //get_corr();

  //  lh_new = likelihood();

    //diff = abs(lh-lh_new);

    ++it;

  }

      for(i=0; i<N; ++i)
      result.push_back(indexx.at(i));
  return result;

}

vector<double>VEMEMA::ema_ind_sh(double tol)
{
  int i, j, l;
  int j_max=0;
  int it=1;
  f1();
  int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
//vem_bivariate(k, tol);
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h;
//  double lh, lh_new;

  get_variance();

  get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  vector<vector<double> > var_new;
  vector<double> corr_new;
  vector<double> prob_new;
vector<double> result;

 // cout << endl << "### expectation-maximization algorithm ###" << endl;

//  double diff = 100;

 while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
 dens.clear();
    double max;

    indexx.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den(i);

	e.at(i).clear();
	max = 0;
	for(j=0; j<k; ++j){
	  h = prob.at(j)*density(i,j)/den;
	  e.at(i).push_back(h);
	  if(h>max) { max = h; j_max = j; }
	}
	indexx.push_back(j_max);
      }
 vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    vector<vector<double> > var_new(dim,vector<double>(k,0.));
    vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();

    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      for(l=0; l<dim; ++l)
		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);

	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
    for(j=0; j<k; ++j)
      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));
get_dens1(lambda_new, dens);
	gradient(dens, prob_new, grad);

	get_max_min(grad, prob_new, i_max, i_min, grad_max);

    //lh = likelihood();

    lambda  = lambda_new;
    prob    = prob_new;
    var     = var_new;
    corr    = corr_new;

    //get_corr();

  //  lh_new = likelihood();

   // diff = abs(lh-lh_new);

    ++it;

  }

      for(i=0; i<N; ++i)
      result.push_back(indexx.at(i));
  return result;

}

vector<double>VEMEMA::ema_ind_uni(double tol)
{
  int i, j, l;
  int j_max=0;
  int it=1;
  do_vem(k,tol);
  
int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
//vem_bivariate(k, tol);
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h;
//  double lh, lh_new;

  get_variance();

  //get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  vector<vector<double> > var_new;
  vector<double> corr_new;
  vector<double> prob_new;
vector<double> result;

 // cout << endl << "### expectation-maximization algorithm ###" << endl;

 // double diff = 100;
 while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
dens.clear();
    double max;

    indexx.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den_uni(i);

	e.at(i).clear();
	max = 0;
	for(j=0; j<k; ++j){
	  h = prob.at(j)*density_uni(i,j)/den;
	  e.at(i).push_back(h);
	  if(h>max) { max = h; j_max = j; }
	}
	indexx.push_back(j_max);
      }
 vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    vector<vector<double> > var_new(dim,vector<double>(k,0.));
   // vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();

    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      for(l=0; l<dim; ++l)
		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);

	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	     // corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
    //for(j=0; j<k; ++j)
    //  corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));
get_dens(mysample.at(0), lambda_new.at(0), dens);
	gradient(dens, prob_new, grad);
	get_max_min(grad, prob_new, i_max, i_min, grad_max);
   // lh = likelihood_uni();

    lambda  = lambda_new;
    prob    = prob_new;
    var     = var_new;
    //corr    = corr_new;

    //get_corr();

  //  lh_new = likelihood_uni();

  //  diff = abs(lh-lh_new);

    ++it;

  }

      for(i=0; i<N; ++i)
      result.push_back(indexx.at(i));
  return result;

}
vector<double>VEMEMA::ema_ind_start(double tol)
{
  int i, j, l;
  int j_max=0;
  int it=1;
  //f1();
//vem_bivariate(k, tol);
int  i_min, i_max;
 double grad_max=0.;
 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h;
  //double lh, lh_new;

  get_variance();

  get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  vector<vector<double> > var_new;
  vector<double> corr_new;
  vector<double> prob_new;
vector<double> result;

  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
  
 while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
dens.clear();
    double max;

    indexx.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den(i);

	e.at(i).clear();
	max = 0;
	for(j=0; j<k; ++j){
	  h = prob.at(j)*density(i,j)/den;
	  e.at(i).push_back(h);
	  if(h>max) { max = h; j_max = j; }
	}
	indexx.push_back(j_max);
      }
 vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    vector<vector<double> > var_new(dim,vector<double>(k,0.));
    vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();

    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      for(l=0; l<dim; ++l)
		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);

	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
    for(j=0; j<k; ++j)
      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));

	get_dens1(lambda_new, dens);
	gradient(dens, prob_new, grad);

	get_max_min(grad, prob_new, i_max, i_min, grad_max);
	
   // lh = likelihood();

    lambda  = lambda_new;
    prob    = prob_new;
    var     = var_new;
    corr    = corr_new;

    //get_corr();

    //lh_new = likelihood();

    //diff = abs(lh-lh_new);

    ++it;

  }

      for(i=0; i<N; ++i)
      result.push_back(indexx.at(i));
  return result;

}
vector<double>VEMEMA::ema_ind_meta_start(double tol)
{
  int i, j, l;
  int j_max=0;
  int it=1;
  
  int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
  //f1_meta();
//vem_bivariate_meta(10, tol);
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h;
  //double lh, lh_new;

  //get_variance();

 // get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
vector<double> result;

  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;

 while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
       dens.clear();
    double max;

    indexx.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den_meta(i);

	e.at(i).clear();
	max = 0;
	for(j=0; j<k; ++j){
	  h = prob.at(j)*density_meta(i,j)/den;
	  e.at(i).push_back(h);
	  if(h>max) { max = h; j_max = j; }
	}
	indexx.push_back(j_max);
      }
 vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

  //  vector<vector<double> > var_new(dim,vector<double>(k,0.));
    //vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();

    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      //for(l=0; l<dim; ++l)
//		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
//		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);
//
//	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
//	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	  //  var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
  //  for(j=0; j<k; ++j)
//      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));
   get_dens_meta(lambda_new, dens);

	  gradient(dens, prob_new, grad);
	     get_max_min(grad, prob_new, i_max, i_min, grad_max);
   // lh = likelihood_meta();

    lambda  = lambda_new;
    prob    = prob_new;
 //   var     = var_new;
//    corr    = corr_new;

    //get_corr();

//    lh_new = likelihood_meta();

   // diff = abs(lh-lh_new);

    ++it;

  }

      for(i=0; i<N; ++i)
      result.push_back(indexx.at(i));
  return result;

}


vector<double>VEMEMA::ema_ind_meta_sh(double tol)
{
  int i, j, l;
  int j_max=0;
  int it=1;
 f1_meta();
 
int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;

//vem_bivariate_meta(k, tol);
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h;
  //double lh, lh_new;

  //get_variance();

 // get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
vector<double> result;

 // cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
 while(abs(grad_max-1.) > tol && it < numiter)

  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
    
     dens.clear();
    double max;

    indexx.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den_meta(i);

	e.at(i).clear();
	max = 0;
	for(j=0; j<k; ++j){
	  h = prob.at(j)*density_meta(i,j)/den;
	  e.at(i).push_back(h);
	  if(h>max) { max = h; j_max = j; }
	}
	indexx.push_back(j_max);
      }
 vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

  //  vector<vector<double> > var_new(dim,vector<double>(k,0.));
    //vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();

    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      //for(l=0; l<dim; ++l)
//		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
//		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);
//
//	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
//	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	  //  var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
  //  for(j=0; j<k; ++j)
//      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));
get_dens_meta(lambda_new, dens);

	  gradient(dens, prob_new, grad);
	     get_max_min(grad, prob_new, i_max, i_min, grad_max);
   // lh = likelihood_meta();

    lambda  = lambda_new;
    prob    = prob_new;
 //   var     = var_new;
//    corr    = corr_new;

    //get_corr();

   // lh_new = likelihood_meta();

  //  diff = abs(lh-lh_new);

    ++it;

  }

      for(i=0; i<N; ++i)
      result.push_back(indexx.at(i));
  return result;

}

vector<double>VEMEMA::ema_ind_meta(double tol)
{
  int i, j, l;
  int j_max=0;
  int it=1;
 //f1_meta();
vem_bivariate_meta(k, tol);

int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h;
  //double lh, lh_new;

  //get_variance();

 // get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
vector<double> result;

  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
  while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
      dens.clear();
    double max;

    indexx.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den_meta(i);

	e.at(i).clear();
	max = 0;
	for(j=0; j<k; ++j){
	  h = prob.at(j)*density_meta(i,j)/den;
	  e.at(i).push_back(h);
	  if(h>max) { max = h; j_max = j; }
	}
	indexx.push_back(j_max);
      }
 vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

  //  vector<vector<double> > var_new(dim,vector<double>(k,0.));
    //vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();

    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      //for(l=0; l<dim; ++l)
//		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
//		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);
//
//	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
//	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	  //  var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
  //  for(j=0; j<k; ++j)
//      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));

   // lh = likelihood_meta();
      get_dens_meta(lambda_new, dens);

	  gradient(dens, prob_new, grad);
	     get_max_min(grad, prob_new, i_max, i_min, grad_max);

    lambda  = lambda_new;
    prob    = prob_new;
 //   var     = var_new;
//    corr    = corr_new;

    //get_corr();

  //  lh_new = likelihood_meta();

  //  diff = abs(lh-lh_new);

    ++it;

  }

      for(i=0; i<N; ++i)
      result.push_back(indexx.at(i));
  return result;

}

/// Funktion f?r EM- Algorithmus

vector<vector<double> >  VEMEMA::ema_uni(double tol)
{
  int i, j, l;
  int it=1;
  //cout<<"k= "<<k<<endl;
  //k=30;
  do_vem(k,tol);
   int dim = lambda.size();
  int k   = lambda.at(0).size();
int  i_min, i_max;
 double grad_max=0.;
 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
  //f1();
//vem_bivariate(10, tol);

  double den, P, h, lh_new;

  
  get_variance();
  
   

  //get_corr();

//cout<<var.at(0).at(2);
//cin.get();
  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
 // vector<vector<double> > var_new;
  vector<double> corr_new;
  vector<double> prob_new;
   vector<vector<double> > result;
// cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;

 while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
  dens.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den_uni(i);

	e.at(i).clear();
	for(j=0; j<k; ++j){
	  e.at(i).push_back(prob.at(j)*density_uni(i,j)/den);
	  
       }
       // cout<<den<<endl;
 
      }
//cin.get();
    //vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();
result.clear();
    vector<vector<double> > var_new(dim,vector<double>(k,0.));
    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      for(l=0; l<dim; ++l)
		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);
	     // c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);

	  }

	for(l=0; l<dim; ++l){
	  lambda_new.at(l).push_back(en.at(l)/den.at(l));
 var_new.at(l).at(j) /= den.at(l);}
      }

    //var_new.clear();
    ///for(l=0; l<dim; ++l)
     /// var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

    //corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));

//    lh = likelihood_uni();
get_dens(mysample.at(0), lambda_new.at(0), dens);
	gradient(dens, prob_new, grad);
	get_max_min(grad, prob_new, i_max, i_min, grad_max);
    lambda  = lambda_new;
    prob    = prob_new;
    var     = var_new;
    //corr    = corr_new;

    //get_corr();

    lh_new = likelihood_uni();

  //  diff = abs(lh-lh_new);

    ++it;

 
}
for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(lh_new);}
	
	for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));
    //for(i=0; i<lambda.size(); ++i)
  // result.push_back(var.at(i));
    result.push_back(prob);
    //result.push_back(corr);
  for(i=0; i<(int)lambda.size(); ++i)
   result.push_back(var.at(i));
     result.push_back(lik);
     //cout<<"uni vem  k="<<k<<endl;
  
	//for(j=0; j<k; ++j)
	//{
	//	for(l=0; l<dim; ++l)
	//	{
	//		cout << "lambda_new(" << j << "," << l << "): " << lambda.at(l).at(j) << "   "<<endl;
	//	}
	//	for(l=0; l<dim; ++l)
	//		cout << "var_new: " << var.at(l).at(j) << "   "<<endl;
//
//
//		cout << "prob_new: " << prob.at(j) << endl;
//		cout<<endl;
//	}


//	cout << "corr_new: " << corr.at(0) << " ";

//Rprintf("%s \n", "EM der bivariate NV"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}



vector<vector<double> > VEMEMA::ema_univariat(double tol)
{
  int i, j, l;
  int it=1;
  do_vem(k,tol);
  int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
 //f1();
 //vem_bivariate(k,tol);
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h;
 // double lh, lh_new;

  get_variance();

  get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
  vector<vector<double> > result;
  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
  while(abs(grad_max-1.) > tol && it < numiter)
  {
//    cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
  dens.clear();
    for(i=0; i<N; ++i)
      {
	den = mix_den(i);

	e.at(i).clear();
	for(j=0; j<k; ++j)
	  e.at(i).push_back(prob.at(j)*density(i,j)/den);
      }

    vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    vector<vector<double> > var_new(dim,vector<double>(k,0.));
    vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();
    result.clear();
    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      for(l=0; l<dim; ++l)
		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);

	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
    for(j=0; j<k; ++j)
    corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));
	get_dens(mysample.at(0), lambda_new.at(0), dens);
	gradient(dens, prob_new, grad);
	get_max_min(grad, prob_new, i_max, i_min, grad_max);

  //  lh = likelihood();

    lambda  = lambda_new;
    prob    = prob_new;
    var     = var_new;
    corr    = corr_new;

    //get_corr();

  //  lh_new = likelihood();

  //  diff = abs(lh-lh_new);

    ++it;

  }
	for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));
  
    result.push_back(prob);
      for(i=0; i<(int)lambda.size(); ++i)
   result.push_back(var.at(i));
    result.push_back(corr);

//Rprintf("%s \n", "EM der bivariate NV mit vershiedenen Varianzen"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}

// PD am 10.4.2023: Auskommentiert, da nicht in CAMAN.R aufgerufen
// 
// vector<vector<double> > VEMEMA::ema_versh(double tol)
// {
//   int i, j, l;
//   int it=1;
//   
//   double den, P, h, lh, lh_new,oldgrad;
//  //f1();
//  vem_bivariate(k,tol);
//  
//  int dim = lambda.size();
//   int k   = lambda.at(0).size();
//  
//  int  i_min, i_max;
//  double grad_max=0.;
//  vector<double> lik;
//  vector<double> grad;
//   grad.clear();
// //cin>>i;
//   vector<double> max_grad;
//    max_grad.clear();
// 
// vector<vector<double> > dens;
// 
// 
//   get_variance();
// 
//   get_corr();
// 
// 
//   
// //Rprintf("erstes mal k=%d dim=%d\n",k,dim);
//   //  lh = likelihood();
//     
//    // Rprintf("Vem logl %f\n",lh);
//   vector<vector<double> > e;
// 
//   vector<vector<double> > lambda_new;
//   //vector<vector<double> > var_new;
//   //vector<double> corr_new;
//   vector<double> prob_new;
//   vector<vector<double> > result;
//  // cout << endl << "### expectation-maximization algorithm ###" << endl;
// oldgrad=100;
//   //double diff = 100;
//   Rprintf("here we go\n");
//     lh = likelihood();
//     
//     Rprintf("Vem logl %f\n",lh);
//   while(abs(grad_max-1.) > tol && it < numiter)
//   {
//     //cout << "iteration: " << it << "grad "<<grad_max<<"oldgrad"<<endl;
// 
// 
//     e.clear();
//     e.resize(N);
// dens.clear();
//     for(i=0; i<N; ++i)
//       {
// 	den = mix_den(i);
// 
// 	e.at(i).clear();
// 	for(j=0; j<k; ++j)
// 	  e.at(i).push_back(prob.at(j)*density(i,j)/den);
//       }
// 
//     vector<double> v(dim,0.); // variance
//     //double c = 0.;            // correlation
// 
//     vector<vector<double> > var_new(dim,vector<double>(k,0.));
//     vector<double> corr_new(k,0.);
// 
//     lambda_new.clear();
//     lambda_new.resize(dim);
//     prob_new.clear();
//     result.clear();
//     for(j=0; j<k; ++j)
//       {
// 	P = 0;
// 	vector<double> en(dim,0); // enumerator
// 	vector<double> den(dim,0); // denominator
// 	for(i=0; i<N; ++i)
// 	  {
// 	    P   += e.at(i).at(j);
// 	    for(l=0; l<dim; ++l)
// 	      den.at(l) += e.at(i).at(j);
// 	  }
// 
// 	prob_new.push_back(P/N);
// 
// 	for(i=0; i<N; ++i)
// 	  {
// 	    for(l=0; l<dim; ++l)
// 	      {
// 		h = mysample.at(l).at(i)*e.at(i).at(j);
// 
// 		en.at(l) += h;
// 	      }
// 
// 	      for(l=0; l<dim; ++l)
// 		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
// 		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);
// 
// 	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
// 	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
// 	  }
// 
// 	for(l=0; l<dim; ++l)
// 	  {
// 	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
// 	    var_new.at(l).at(j) /= den.at(l);
// 	  }
// 
//       }
// 
// //     var_new.clear();
// //     for(l=0; l<dim; ++l)
// //       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????
// 
// //    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
//     for(j=0; j<k; ++j)
//     corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));
// 
//    // lh = likelihood();
// 	get_dens1(lambda_new, dens);
// 	gradient(dens, prob_new, grad);
// 
// 	get_max_min(grad, prob_new, i_max, i_min, grad_max);
//     lambda  = lambda_new;
//     prob    = prob_new;
//     var     = var_new;
//     corr    = corr_new;
//    if(abs(oldgrad-grad_max)<1.E-7) break;
//     oldgrad=grad_max;
// 
//     //get_corr();
// 
//     //lh_new = likelihood();
// 
//   //  diff = abs(lh-lh_new);
// 
//     ++it;
// 
//   }
//   
//   
// 	for(i=0; i<(int)lambda.at(0).size(); ++i){
//   lik.push_back(lh_new);}
//   
// 	for(i=0; i<(int)lambda.size(); ++i)
//     result.push_back(lambda.at(i));
// 
// 
//     result.push_back(prob);
//       for(i=0; i<(int)lambda.size(); ++i)
//    result.push_back(var.at(i));
//     result.push_back(corr);
//      result.push_back(lik);
// 
// //Rprintf("%s \n", "EM der bivariate NV mit vershiedenen Varianzen"); 
// //Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
// return result;
// }


vector<vector<double> > VEMEMA::ema_versh_sh(double tol)
{
  int i, j, l;
  int it=1;
 f1();
 int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();
 vector<double> lik;
 // lik.clear();
  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
 //vem_bivariate(k,tol);
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h, lh_new;

  get_variance();

  get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
  vector<vector<double> > result;
  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
 while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
dens.clear();
    for(i=0; i<N; ++i)
      {
	den = mix_den(i);

	e.at(i).clear();
	for(j=0; j<k; ++j)
	  e.at(i).push_back(prob.at(j)*density(i,j)/den);
      }

    vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    vector<vector<double> > var_new(dim,vector<double>(k,0.));
    vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();
    result.clear();
    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      for(l=0; l<dim; ++l)
		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);

	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
    for(j=0; j<k; ++j)
    corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));



    lambda  = lambda_new;
    prob    = prob_new;
    var     = var_new;
    corr    = corr_new;

    //get_corr();
get_dens1(lambda_new, dens);
	gradient(dens, prob_new, grad);

	get_max_min(grad, prob_new, i_max, i_min, grad_max);
    lh_new = likelihood();

  //  diff = abs(lh-lh_new);

    ++it;

  }
  	for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(lh_new);}
	for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));
  
    result.push_back(prob);
      for(i=0; i<(int)lambda.size(); ++i)
   result.push_back(var.at(i));
    result.push_back(corr);
    result.push_back(lik);

//Rprintf("%s \n", "EM der bivariate NV mit vershiedenen Varianzen"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}

vector<vector<double> > VEMEMA::ema_versh_meta(double tol)
{
  int i, j, l;
  int it=1;
// f1_meta();
int  i_min, i_max;
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h,  lh_new;
 double grad_max=0.;
  //get_variance();

//  get_corr();
 vector<double> grad;
  grad.clear();
  vector<double> max_grad;
  //lik.clear();
    max_grad.clear();
  vector<vector<double> > e;
 vector<vector<double> > dens;
  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
  vector<vector<double> > result;
  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
    while(abs(grad_max-1.) > tol && it < numiter)
  //while(diff > tol)
  {
   // cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);

    for(i=0; i<N; ++i)
      {
	den = mix_den_meta(i);

	e.at(i).clear();
	for(j=0; j<k; ++j)
	  e.at(i).push_back(prob.at(j)*density_meta(i,j)/den);
      }

    vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    //vector<vector<double> > var_new(dim,vector<double>(k,0.));
    //vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();
       dens.clear();

    result.clear();
    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	    //  for(l=0; l<dim; ++l)
//		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
//		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);
//
//	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
//	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
 }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    //var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
  //  for(j=0; j<k; ++j)
//      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));

   // lh = likelihood_meta();
      get_dens_meta(lambda_new, dens);
//cout<<dens.at(0).at(0);
	  gradient(dens, prob_new, grad);
	     get_max_min(grad, prob_new, i_max, i_min, grad_max);

    lambda  = lambda_new;
    prob    = prob_new;
    //var     = var_new;
    //corr    = corr_new;

    //get_corr();

    lh_new = likelihood_meta();

   // diff = abs(lh-lh_new);

    ++it;

  }

	for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(lh_new);}
  for(i=0; i<(int)lambda.at(0).size(); ++i){
  max_grad.push_back(grad_max);}
  
	for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));
   // for(i=0; i<lambda.size(); ++i)
   //result.push_back(var.at(i));
    result.push_back(prob);
    
   result.push_back(lik);

 result.push_back(max_grad);
//Rprintf("%s \n", "EM der bivariate NV mit vershiedenen Varianzen"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}

vector<vector<double> > VEMEMA::ema_meta(double tol)
{
  int i, j, l;
  int it=1;
  vem_bivariate_meta(k,tol);
  int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
//f1_meta();
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h, lh_new;

  //get_variance();

//  get_corr();

  //lik.clear();
  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
  vector<vector<double> > result;
  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
while(abs(grad_max-1.) > tol && it < numiter)
  {
    //cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
  dens.clear();
    for(i=0; i<N; ++i)
      {
	den = mix_den_meta(i);

	e.at(i).clear();
	for(j=0; j<k; ++j)
	  e.at(i).push_back(prob.at(j)*density_meta(i,j)/den);
      }

    vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    //vector<vector<double> > var_new(dim,vector<double>(k,0.));
    //vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();
    result.clear();
    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	    //  for(l=0; l<dim; ++l)
//		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
//		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);
//
//	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
//	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
 }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    //var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
  //  for(j=0; j<k; ++j)
//      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));

    //lh = likelihood_meta();

    lambda  = lambda_new;
    prob    = prob_new;
    //var     = var_new;
    //corr    = corr_new;

    //get_corr();
get_dens1(lambda_new, dens);
	gradient(dens, prob_new, grad);

	get_max_min(grad, prob_new, i_max, i_min, grad_max);
    lh_new = likelihood_meta();

  //  diff = abs(lh-lh_new);

    ++it;

  }

	for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(lh_new);}
  
for(i=0; i<(int)lambda.at(0).size(); ++i){
  max_grad.push_back(grad_max);}

	for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));
   // for(i=0; i<lambda.size(); ++i)
   //result.push_back(var.at(i));
    result.push_back(prob);
    
   result.push_back(lik);

result.push_back(max_grad);
//Rprintf("%s \n", "EM der bivariate NV mit vershiedenen Varianzen"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}



vector<vector<double> > VEMEMA::ema_meta_sh(double tol)
{
  int i, j, l;
  int it=1;
//  vem_bivariate_meta(k,tol);
f1_meta();

int  i_min, i_max;
 double grad_max=0.;

 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h,  lh_new;

  //get_variance();

//  get_corr();


  lik.clear();
  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
  vector<vector<double> > result;
  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;
 while(abs(grad_max-1.) > tol && it < numiter)
  {
   // cout << "iteration: " << it << endl;


    e.clear();
    e.resize(N);
 dens.clear();

    for(i=0; i<N; ++i)
      {
	den = mix_den_meta(i);

	e.at(i).clear();
	for(j=0; j<k; ++j)
	  e.at(i).push_back(prob.at(j)*density_meta(i,j)/den);
      }

    vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    //vector<vector<double> > var_new(dim,vector<double>(k,0.));
    //vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();
    result.clear();
    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	    //  for(l=0; l<dim; ++l)
//		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
//		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);
//
//	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
//	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
 }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    //var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
  //  for(j=0; j<k; ++j)
//      corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));

 //   lh = likelihood_meta();
      get_dens_meta(lambda_new, dens);

	  gradient(dens, prob_new, grad);
	     get_max_min(grad, prob_new, i_max, i_min, grad_max);

    lambda  = lambda_new;
    prob    = prob_new;
    //var     = var_new;
    //corr    = corr_new;

    //get_corr();

    lh_new = likelihood_meta();

  //  diff = abs(lh-lh_new);

    ++it;

  }

	for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(lh_new);}
  
  for(i=0; i<(int)lambda.at(0).size(); ++i){
  max_grad.push_back(grad_max);}
	for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));
   // for(i=0; i<lambda.size(); ++i)
   //result.push_back(var.at(i));
    result.push_back(prob);
    
   result.push_back(lik);
result.push_back(max_grad);

//Rprintf("%s \n", "EM der bivariate NV mit vershiedenen Varianzen"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}

vector<vector<double> > VEMEMA::ema_versh_start(double tol)
{
  int i, j, l;
  int it=1;
 //f1();
 //vem_bivariate(k,tol);
 int  i_min, i_max;
 double grad_max=0.;
//vector<double> lik;
 vector<double> grad;
  grad.clear();

  vector<double> max_grad;
   max_grad.clear();

vector<vector<double> > dens;
  int dim = lambda.size();
  int k   = lambda.at(0).size();

  double den, P, h,lh_new;

  get_variance();

  get_corr();


  vector<vector<double> > e;

  vector<vector<double> > lambda_new;
  //vector<vector<double> > var_new;
  //vector<double> corr_new;
  vector<double> prob_new;
  vector<vector<double> > result;
  //cout << endl << "### expectation-maximization algorithm ###" << endl;

  //double diff = 100;

 while(abs(grad_max-1.) > tol && it < numiter)
  {
   // cout << "iteration: " << it <<"grad"<<grad_max<<endl;


    e.clear();
    e.resize(N);
  dens.clear();
    for(i=0; i<N; ++i)
      {
	den = mix_den(i);

	e.at(i).clear();
	for(j=0; j<k; ++j)
	  e.at(i).push_back(prob.at(j)*density(i,j)/den);
      }

    vector<double> v(dim,0.); // variance
    //double c = 0.;            // correlation

    vector<vector<double> > var_new(dim,vector<double>(k,0.));
    vector<double> corr_new(k,0.);

    lambda_new.clear();
    lambda_new.resize(dim);
    prob_new.clear();
    result.clear();
    for(j=0; j<k; ++j)
      {
	P = 0;
	vector<double> en(dim,0); // enumerator
	vector<double> den(dim,0); // denominator
	for(i=0; i<N; ++i)
	  {
	    P   += e.at(i).at(j);
	    for(l=0; l<dim; ++l)
	      den.at(l) += e.at(i).at(j);
	  }

	prob_new.push_back(P/N);

	for(i=0; i<N; ++i)
	  {
	    for(l=0; l<dim; ++l)
	      {
		h = mysample.at(l).at(i)*e.at(i).at(j);

		en.at(l) += h;
	      }

	      for(l=0; l<dim; ++l)
		//v.at(l) += e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2)/(N-1);
		var_new.at(l).at(j) +=  e.at(i).at(j)*pow(mysample.at(l).at(i)-lambda.at(l).at(j),2);

	      //c += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	      corr_new.at(j) += e.at(i).at(j)*(mysample.at(0).at(i)-lambda.at(0).at(j))*(mysample.at(1).at(i)-lambda.at(1).at(j))/(N-1);
	  }

	for(l=0; l<dim; ++l)
	  {
	    lambda_new.at(l).push_back(en.at(l)/den.at(l));
	    var_new.at(l).at(j) /= den.at(l);
	  }

      }

//     var_new.clear();
//     for(l=0; l<dim; ++l)
//       var_new.push_back(vector<double>(k,v.at(l))); //???????????????????????????????

//    corr_new = vector<double>(k,c/sqrt(v.at(0)*v.at(1)));
    for(j=0; j<k; ++j)
    corr_new.at(j) /= sqrt(var_new.at(0).at(j)*var_new.at(1).at(j));

  //  lh = likelihood();
	get_dens1(lambda_new, dens);
	gradient(dens, prob_new, grad);

	get_max_min(grad, prob_new, i_max, i_min, grad_max);

    lambda  = lambda_new;
    prob    = prob_new;
    var     = var_new;
    corr    = corr_new;

    //get_corr();

    lh_new = likelihood();

  //  diff = abs(lh-lh_new);

    ++it;

  }
  for(i=0; i<(int)lambda.at(0).size(); ++i){
  lik.push_back(lh_new);}
	for(i=0; i<(int)lambda.size(); ++i)
    result.push_back(lambda.at(i));
  
    result.push_back(prob);
      for(i=0; i<(int)lambda.size(); ++i)
   result.push_back(var.at(i));
    result.push_back(corr);
     result.push_back(lik);



//Rprintf("%s \n", "EM der bivariate NV mit vershiedenen Varianzen"); 
//Rprintf("%s \n", "     lambda1_1 lambda1_2 lambda2_1 lambda2_2 prob1 prob2 ");
return result;
}

//void
//R_init_mylib(DllInfo *info)
//{
///* Register routines, allocate resources. */
//}
//void
//R_unload_mylib(DllInfo *info)
//{
///* Release resources. */
//}


}

