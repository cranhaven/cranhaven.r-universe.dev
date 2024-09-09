
//qtlmt.cc

#include "qtlmt.h"

/********************************************************
 C++ functions
 ********************************************************/
// general R functions
//extern "C"{
   void runifc(double* x,int& n,long *seed){
      runif(x,n,seed);
   }
   void svdc(double *a, int& m, int& n, double* w, double *v){
      double** a0=new double*[m]; for(int i=0;i<m;i++) a0[i]=a+i*n;
      double** v0=new double*[n]; for(int i=0;i<n;i++) v0[i]=v+i*n;
      svd(a0, m, n, w, v0);
      delete[] a0; delete[] v0;
   }
       void cholc(double* A, int& n, double* p){
      double** a=new double*[n]; for(int i=0;i<n;i++) a[i]=A+i*n;
      chol(a,n,p);
      delete[] a;
       }
   void cholsolve(double* A,int& n,double* b,double* x){
      double** a=new double*[n]; for(int i=0;i<n;i++) a[i]=A+i*n;
      cholsl(a,n,b,x);
      delete[] a;
   }
   void lusolve(double* A,int& n,double* b,double* x){
      double** a=new double*[n]; for(int i=0;i<n;i++) a[i]=A+i*n;
      solve(a,n,b,x);
      delete[] a;
   }
//}

// functions for QTL mapping
extern "C"{//must have in c++
//SURE models
   void sureEstc(double* y,int& n,int& p,double* x,int& m,int* nqs,int* qs,
      double* b,double* sigma,double& loglik,int& ini_sigma,int& iter,double& tol){
      double** y0=new double*[n];for(int i=0;i<n;i++) y0[i]=y+i*p;
      double** x0=new double*[n];for(int i=0;i<n;i++) x0[i]=x+i*m;
      double** sigma0=new double*[p];for(int i=0;i<p;i++) sigma0[i]=sigma+i*p;
      loglik = sureEst(y0,n,p,x0,m,nqs,qs,b,sigma0,ini_sigma,iter,tol);
      delete[] y0; delete[] x0; delete[] sigma0;
   }
/*   void sureAdd1c(double* y,int& n,int& p,double* x, int& m,int* nqs,int* qs,
      int* nupper,int* upper,double* sigma,int* which, bool& add,
      int& iter, double& tol){
      double** y0=new double*[n];for(int i=0;i<n;i++) y0[i]=y+i*p;
      double** x0=new double*[n];for(int i=0;i<n;i++) x0[i]=x+i*m;
      double** sigma0=new double*[p];for(int i=0;i<p;i++) sigma0[i]=sigma+i*p;
      add = sureAdd1(y0,n,p,x0,m,nqs,qs,nupper,upper,sigma0,which,iter,tol);
      delete[] y0; delete[] x0; delete[] sigma0;
   }
*/
   void sureStepc(double* y,int& n,int& p,double* x,int& m,
      int* nlower,int* lower,int* nupper,int* upper,double& k,int& direction,
      int* vin,double* rec,int& max_terms,int& steps,int& iter, double& tol){
      double** y0=new double*[n];for(int i=0;i<n;i++) y0[i]=y+i*p;
      double** x0=new double*[n];for(int i=0;i<n;i++) x0[i]=x+i*m;
      int** vin0=new int*[p];for(int i=0;i<p;i++) vin0[i]=vin+i*m;
      sureStep(y0,n,p,x0,m,nlower,lower,nupper,upper,k,direction,vin0,rec,
         max_terms,steps,iter,tol);
      delete[] y0; delete[] x0; delete[] vin0;
   }
//single-trait composite multiple-interval mapping
   void mimEstc(double* y,double* P,double* G,double* W,int& n,int& m,int& k,int& l,
      double*a,double* b,double& sigma,double& loglik,
      int& init,int& iter,double& tol){
      double** P0=new double*[n]; for(int i=0;i<n;i++) P0[i]=P+i*m;
      double** G0=new double*[m]; for(int i=0;i<m;i++) G0[i]=G+i*k;
      double** W0=new double*[n]; for(int i=0;i<n;i++) W0[i]=W+i*l;
      loglik=mimEst(y,P0,G0,W0,n,m,k,l,a,b,sigma,init,iter,tol);
          delete[] P0; delete[] G0; delete[] W0;
   }
//multiple-trait composite multiple-interval mapping
   void mtcmimEstc(double* y,int& n,int& p,double* P,int& np,
      double* G,int& nG,int* ngs,int* gs,double* W,int& nW,int* nws,int* ws,
      double* a,double* b,double* sigma,double& loglik,
      int& init,int& iter,double& tol){
      double** y0=new double*[n]; for(int i=0;i<n;i++) y0[i]=y+i*p;
      double** P0=new double*[n]; for(int i=0;i<n;i++) P0[i]=P+i*np;
      double** G0=new double*[np]; for(int i=0;i<np;i++) G0[i]=G+i*nG;
      double** W0=new double*[n]; for(int i=0;i<n;i++) W0[i]=W+i*nW;
      double** sigma0=new double*[p]; for(int i=0;i<p;i++) sigma0[i]=sigma+i*p;
      loglik=mtcmimEst(y0,n,p,P0,np,G0,ngs,gs,W0,nws,ws,a,b,sigma0,
         init,iter,tol);
      delete[] y0; delete[] P0; delete[] G0; delete[] W0; delete[] sigma0;
   }
   void fPc(int* A,int& nP,int& nQ,int* mdat,int& n,int& nm,
      double* mpos,int* dists_ch,int* dists_mid,double* dists_d,int* mid,int& nmid,
      double* P,int& pp){
      int** A0=new int*[nP];for(int i=0;i<nP;i++) A0[i]=A+i*nQ;
      int** mdat0=new int*[n];for(int i=0;i<n;i++) mdat0[i]=mdat+i*nm;
      double** mpos0=new double*[nm];for(int i=0;i<nm;i++) mpos0[i]=mpos+i*4;
      double** P0=new double*[n];for(int i=0;i<n;i++) P0[i]=P+i*nP;
      fP(A0,nP,nQ,mdat0,n,nm,mpos0,dists_ch,dists_mid,dists_d,mid,nmid,P0,pp);
      delete[] A0; delete[] mdat0; delete[] mpos0; delete[] P0;
   }

static const R_CMethodDef cMethods[] = {
//    {"runifc",       (DL_FUNC) &runifc,          3},
//    {"svdc",         (DL_FUNC) &svdc,            5},
//    {"cholc",        (DL_FUNC) &cholc,           3},
//    {"cholsolve",    (DL_FUNC) &cholsolve,       4},
//    {"lusolve",      (DL_FUNC) &lusolve,         4},
    {"sureEstc",     (DL_FUNC) &sureEstc,       13},
    {"sureStepc",    (DL_FUNC) &sureStepc,      17},
    {"mimEstc",      (DL_FUNC) &mimEstc,        15}, //no need for now
    {"mtcmimEstc",   (DL_FUNC) &mtcmimEstc,     20},
    {"fPc",          (DL_FUNC) &fPc,            14},
    {NULL, NULL, 0}
};

void R_init_qtlmt(DllInfo *dll)
{
    R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
//    R_forceSymbols(dll, TRUE);
}
}

/********************************************************
 basic functions
 ********************************************************/

/*----------------------------------
 transpose of m by n array arr
 ----------------------------------*/
void arr_t(double** arr,int m, int n,double** result){
   for(int i=0;i<m;i++){
      for(int j=0;j<n;j++){
         result[j][i]=arr[i][j];
      }
   }
}

/*---------------------------------------------------
 Cholesky decomposition A = L*LT (positive definite)
 A: input (only the upper triangle is needed,
       which is not modified), L is returned by the 
       lower triangle of A and p
 p: vector of length n, contains the diagonal of L      
 --------------------------------------------------*/
void chol(double** A, int n, double* p){
   int i,j,k;
   double sum;
   for (i=0;i<n;i++) {
      for (j=i;j<n;j++) {
         for (sum=A[i][j],k=i-1;k>=0;k--) sum -= A[i][k]*A[j][k];
         if (i == j) {
            if (sum <= 0.0){
               error(_("Cholesky decomposition failed...\n"));
            }
            p[i]=sqrt(sum);
         } else A[j][i]=sum/p[i];
      }
   }
}

/*--------------------------------------------
 Solve A *x = b, where A is positive definite    
 ---------------------------------------------*/
void cholsl(double **A, int n, double b[], double x[]){
   double* p = new double[n];
   chol(A,n,p);
   
   int i,k;
   double sum;
   for (i=0;i<n;i++) {
      for (sum=b[i],k=i-1;k>=0;k--) sum -= A[i][k]*x[k];
      x[i]=sum/p[i];
   }
   for (i=n-1;i>=0;i--) {
      for (sum=x[i],k=i+1;k<n;k++) sum -= A[k][i]*x[k];
      x[i]=sum/p[i];
   }
   delete[] p;
}

/*-------------------------------------
 dnorm: normal density function
 defualt: mean=0 and sd=1
 --------------------------------------*/
double dnorm(double x, double mean, double sd){
   # define PI 3.141592653589793
   double pdf;
   double y;

   y = ( x - mean ) / sd;
   pdf = exp ( - 0.5*y*y ) / ( sd * sqrt ( 2.0 * PI ) );

   return pdf;
   # undef PI
}

/*------------------------
 create a file name 
 ------------------------*/
void fchar(char *path,char *file,int num,char *type,char *buff){
   if(num==0){
      strcpy(buff,path);
      strcat(buff,file);
      strcat(buff,type);
   }
   else{
      char* p=new char[100];
      strcpy(buff,path);
      strcat(buff,file);
      i_to_a(num,p);
      strcat(buff,p);
      strcat(buff,type);
      delete[] p;
   }
}

/*--------------------------------------------------
 Determinant of n by n matrix A by LU decomposition
 A: n by n matrix, not change
 --------------------------------------------------*/
double det(double** A, int n){
   double** a=new double*[n]; for(int i=0;i<n;i++) a[i]=new double[n];
   int* indx=new int[n];
   double d;
   for(int i=0;i<n;i++){
      for(int j=0;j<n;j++){
         a[i][j] = A[i][j];
      }
   }
   lud(a,n,indx,&d);
   for(int j=0;j<n;j++) d *= a[j][j];
   
   for(int i=0;i<n;i++) delete[] a[i];
   delete[]a; delete[] indx;
   
   return d;
}

/*--------------------------------
 Generalized inverse of A by SVD
 A: m by n matrix
 ---------------------------------*/
void ginv(double** A, int m, int n, double** ginvA){
   double s;
   double** u=new double*[m]; for(int i=0;i<m;i++) u[i]=new double[n];
   double* w=new double[n];
   double** v=new double*[n]; for(int i=0;i<n;i++) v[i]=new double[n];
   double eps = numeric_limits<double>::epsilon();

   for(int i=0;i<m;i++){
      for(int j=0;j<n;j++){
         u[i][j] = A[i][j];
      }
   }
   svd(u, m, n, w, v);
   for (int j=0;j<n;j++) {
      if(abs(w[j])>sqrt(eps)) s = 1/w[j];
         else s = 0.0;
      for(int i=0;i<n;i++){
         v[i][j] *= s;
      }
   }
   for(int i=0;i<n;i++){
      for (int j=0;j<m;j++) {
         ginvA[i][j] = 0.0;
         for(int k=0;k<n;k++){
            ginvA[i][j] += v[i][k]*u[j][k];
         }
      }
   }

   for(int i=0;i<m;i++) delete[] u[i];
   for(int i=0;i<n;i++) delete[] v[i];
   delete[] u; delete[] w; delete[] v;
}

/*-----------------------------------------------
 Inverse of n by n matrix A by LU decomposition
 A: n by n matrix, not change
 -----------------------------------------------*/
void inv(double** A, int n, double** invA){
   double** a=new double*[n]; for(int i=0;i<n;i++) a[i]=new double[n];
   int* indx=new int[n];
   double* col=new double[n];
   double d;
   for(int i=0;i<n;i++){
      for(int j=0;j<n;j++){
         a[i][j] = A[i][j];
      }
   }
   lud(a,n,indx,&d);
   for(int j=0;j<n;j++) {
      for(int i=0;i<n;i++) col[i]=0.0;
      col[j]=1.0;
      lubksb(a,n,indx,col);
      for(int i=0;i<n;i++) invA[i][j]=col[i];
   }

   for(int i=0;i<n;i++) delete[] a[i];
   delete[]a; delete[] indx; delete[] col;
}

/*--------------------------------------------------------------
 Inverse and determinant of n by n matrix A by LU decomposition
 A: n by n matrix, not change
 --------------------------------------------------------------*/
double inv_det(double** A, int n, double** invA){
   double** a=new double*[n]; for(int i=0;i<n;i++) a[i]=new double[n];
   int* indx=new int[n];
   double* col=new double[n];
   double d;
   for(int i=0;i<n;i++){
      for(int j=0;j<n;j++){
         a[i][j] = A[i][j];
      }
   }
   lud(a,n,indx,&d);
   for(int j=0;j<n;j++) {
      d *= a[j][j];
      for(int i=0;i<n;i++) col[i]=0.0;
      col[j]=1.0;
      lubksb(a,n,indx,col);
      for(int i=0;i<n;i++) invA[i][j]=col[i];
   }

   for(int i=0;i<n;i++) delete[] a[i];
   delete[]a; delete[] indx; delete[] col;

   return d;
}

/*----------------------------------------------
 i_to_a: convert an integer into a string
 default: base=10
 -----------------------------------------------*/
void i_to_a(int i,char buff[],int base){
   int len=100;
   char* tmp=new char[len];
   int k,l=i;
   int indx=0;
   while(l){
      k=l%base;
      tmp[indx]=48+k;
      l=(l-k)/base;
      indx++;
   }
   for(int n=0;n<indx;n++){
      buff[n]=tmp[indx-n-1];
   }
   buff[indx]='\0';
   if(indx>len){
      error(_("Array provided is too short!\n"));
   }
   delete[] tmp;
}

/*-----------------------------------------------------
 LU decomposition: A = L*U (non-singluar)
 a: n by n matrix, will store L and U
 indx: vector of length n, records the row permutation
    effected by the partial pivoting (0...n-1)
 d: output as ±1 depending on whether the number of row
   interchanges was even or odd, respectively
 -----------------------------------------------------*/
void lud(double **a, int n, int *indx, double *d){
   #define TINY 1.0e-38
   int i,imax,j,k;
   double big,dum,sum,temp;
   double *vv;

   vv=new double[n];
   *d=1.0;
   for (i=0;i<n;i++) {
      big=0.0;
      for (j=0;j<n;j++){
         temp=abs(a[i][j]);
         if (temp > big) big=temp;
      }
      if (big == 0.0){
         error(_("Singular matrix in routine ludcmp...\n"));
      }
      vv[i]=1.0/big;
   }
   for (j=0;j<n;j++) {
      for (i=0;i<j;i++) {
         sum=a[i][j];
         for (k=0;k<i;k++) sum -= a[i][k]*a[k][j];
         a[i][j]=sum;
      }
      big=0.0;
      imax=j;
      for (i=j;i<n;i++) {
         sum=a[i][j];
         for (k=0;k<j;k++)
            sum -= a[i][k]*a[k][j];
         a[i][j]=sum;
         if ( (dum=vv[i]*abs(sum)) >= big) {
            big=dum;
            imax=i;
         }
      }
      if (j != imax) {
         for (k=0;k<n;k++) {
            dum=a[imax][k];
            a[imax][k]=a[j][k];
            a[j][k]=dum;
         }
         *d = -(*d);
         vv[imax]=vv[j];
      }
      indx[j]=imax;
      if (a[j][j] == 0.0) a[j][j]=TINY;
      if (j != n-1) {
         dum=1.0/(a[j][j]);
         for (i=j+1;i<n;i++) a[i][j] *= dum;
      }
   }
   delete[] vv;
}

/*-------------------------------
 Generate n uniform number (0,1)
 returned by x
 ------------------------------*/
double runif0(long *seed){
   #define IM1 2147483563
   #define IM2 2147483399
   #define AM (1.0/IM1)
   #define IMM1 (IM1-1)
   #define IA1 40014
   #define IA2 40692
   #define IQ1 53668
   #define IQ2 52774
   #define IR1 12211
   #define IR2 3791
   #define NTAB 32
   #define NDIV (1+IMM1/NTAB)
   #define EPS 1.2e-7
   #define RNMX (1.0-EPS)

   int j;
   long k;
   static long seed2=123456789;
   static long iy=0;
   static long iv[NTAB];
   double temp;
   if (*seed <= 0) {
      if (-(*seed) < 1) *seed=1;
         else *seed = -(*seed);
   }
   seed2=(*seed);
   for (j=NTAB+7;j>=0;j--) {
      k=(*seed)/IQ1;
      *seed=IA1*(*seed-k*IQ1)-k*IR1;
      if (*seed < 0) *seed += IM1;
      if (j < NTAB) iv[j] = *seed;
   }
   iy=iv[0];
   k=(*seed)/IQ1;
   *seed=IA1*(*seed-k*IQ1)-k*IR1;
   if (*seed < 0) *seed += IM1;
   k=seed2/IQ2;
   seed2=IA2*(seed2-k*IQ2)-k*IR2;
   if (seed2 < 0) seed2 += IM2;
   j=iy/NDIV;
   iy=iv[j]-seed2;
   iv[j] = *seed;
   if (iy < 1) iy += IMM1;
   if ((temp=AM*iy) > RNMX) return RNMX;
      else return temp;
}

void runif(double* x,int n,long *seed){
   if(*seed==0){
      *seed = (long int)time(0);
   }
   for(int i=0;i<n;i++){
      x[i] = runif0(seed);
   }
}

/*----------------------------------------------------------
 Solve Ax=b for x returned by b
 A: n by n matrix from LU decomposition, not change
 indx: vector of length n from LU decomposition, not change
 ----------------------------------------------------------*/
void lubksb(double **a, int n, int *indx, double b[]){
   int i,ii=0,ip,j;
   double sum;

   for (i=0;i<n;i++) {
      ip=indx[i];
      sum=b[ip];
      b[ip]=b[i];
      if (ii)
         for (j=ii-1;j<=i-1;j++) sum -= a[i][j]*b[j];
      else if (sum) ii=i+1;
      b[i]=sum;
   }
   for (i=n-1;i>=0;i--) {
      sum=b[i];
      for (j=i+1;j<n;j++) sum -= a[i][j]*b[j];
      b[i]=sum/a[i][i];
   }
}

void solve(double** A, int n, double b[], double x[]){
   double** a=new double*[n];for(int i=0;i<n;i++) a[i]=new double[n];
   int* indx=new int[n];
   double d;
   for(int i=0;i<n;i++){
      for(int j=0;j<n;j++){
         a[i][j] = A[i][j];
      }
      x[i] = b[i];
   }
   lud(a,n,indx,&d);
   lubksb(a,n,indx,x);

   for(int i=0;i<n;i++) delete[] a[i];
   delete[] a; delete[] indx;
}

/*--------------------------------
 Solve Ax=b for x returned by SVD
 A: m by n matrix (m >= n)
 Note: much slower than LU
 ---------------------------------*/
void solve(double** A, int m, int n, double b[], double x[]){
   int jj,j,i;
   double s,*tmp=new double[n];
   double** u=new double*[m];for(int i=0;i<m;i++) u[i]=new double[n];
   double* w=new double[n];
   double** v=new double*[n];for(int i=0;i<n;i++) v[i]=new double[n];
   double eps = numeric_limits<double>::epsilon();

   for(int i=0;i<m;i++){
      for(int j=0;j<n;j++){
         u[i][j] = A[i][j];
      }
   }
   svd(u, m, n, w, v);
   for (j=0;j<n;j++) {
      s=0.0;
      if (abs(w[j])>sqrt(eps)) {
         for (i=0;i<m;i++) s += u[i][j]*b[i];
         s /= w[j];
      }
      tmp[j]=s;
   }
   for (j=0;j<n;j++) {
      s=0.0;
      for (jj=0;jj<n;jj++) s += v[j][jj]*tmp[jj];
      x[j]=s;
   }

   for(int i=0;i<m;i++) delete[] u[i];
   for(int i=0;i<n;i++) delete[] v[i];
   delete[] tmp; delete[] u; delete[] w; delete[] v;
}

/*--------------------------------------
 SVD decomposition: A = U*W*VT
 a: m by n matrix
 U: replace a on output
 W: vector of length n; singular values
 V: n by n matrix
 Note: not accurate if m<n
 --------------------------------------*/
double pythag(double a, double b){ 
   #define SQR(a) (a == 0.0 ? 0.0 : a*a)
   double absa,absb; 
   absa=abs(a); 
   absb=abs(b); 
   if (absa > absb) return absa*sqrt(1.0+SQR(absb/absa)); 
   else return (absb == 0.0 ? 0.0 : absb*sqrt(1.0+SQR(absa/absb))); 
} 

void svd(double **a, int m, int n, double w[], double **v){ 
   #define SQR(a) (a == 0.0 ? 0.0 : a*a)
   #define FMAX(a,b) (a > b ? a : b)
   #define FMIN(a,b) (a < b ? a : b)
   #define SIGN(a,b) (b >= 0.0 ? abs(a) : -abs(a))
   #define eps numeric_limits<double>::epsilon()

   int flag,i,its,j,jj,k,l,nm;
   double anorm,c,f,g,h,s,scale,x,y,z,*rv1; 

   rv1=new double[n];
   g=scale=anorm=0.0; l=0;
   for (i=0;i<n;i++) {
      l=i+1;
      rv1[i]=scale*g;
      g=s=scale=0.0;
      if (i< m) {
         for (k=i;k<m;k++) scale += abs(a[k][i]);
         if (scale) {
            for (k=i;k<m;k++) {
               a[k][i] /= scale;
               s += a[k][i]*a[k][i];
            }
            f=a[i][i];
            g = -SIGN(sqrt(s),f);
            h=f*g-s;
            a[i][i]=f-g;
            for (j=l;j<n;j++) {
               for (s=0.0,k=i;k<m;k++) s += a[k][i]*a[k][j];
               f=s/h;
               for (k=i;k<m;k++) a[k][j] += f*a[k][i];
            }
            for (k=i;k<m;k++) a[k][i] *= scale;
         }
      }
      w[i]=scale *g;
      g=s=scale=0.0;
      if (i< m && i!= n-1) {
         for (k=l;k<n;k++) scale += abs(a[i][k]);
         if (scale) {
            for (k=l;k<n;k++) {
               a[i][k] /= scale;
               s += a[i][k]*a[i][k];
            }
            f=a[i][l];
            g = -SIGN(sqrt(s),f);
            h=f*g-s;
            a[i][l]=f-g;
            for (k=l;k<n;k++) rv1[k]=a[i][k]/h;
            for (j=l;j<m;j++) {
               for (s=0.0,k=l;k<n;k++) s += a[j][k]*a[i][k]; 
               for (k=l;k<n;k++) a[j][k] += s*rv1[k]; 
            } 
            for (k=l;k<n;k++) a[i][k] *= scale; 
         }
      }
      anorm=FMAX(anorm,(abs(w[i])+abs(rv1[i])));
   } 
   for (i=n-1;i>=0;i--) {
      if (i< n-1) { 
         if(g) { 
            for (j=l;j<n;j++)
            v[j][i]=(a[i][j]/a[i][l])/g; 
            for (j=l;j<n;j++) { 
               for (s=0.0,k=l;k<n;k++) s += a[i][k]*v[k][j]; 
               for (k=l;k<n;k++) v[k][j] += s*v[k][i]; 
            }
         }
         for (j=l;j<n;j++) v[i][j]=v[j][i]=0.0;
      }
      v[i][i]=1.0;
      g=rv1[i];
      l=i;
   } 
   for (i=FMIN(m,n)-1;i>=0;i--) {
      l=i+1; 
      g=w[i]; 
      for (j=l;j<n;j++) a[i][j]=0.0; 
      if (g) { 
         g=1.0/g; 
         for (j=l;j<n;j++) {
            for (s=0.0,k=l;k<m;k++) s += a[k][i]*a[k][j];
            f=(s/a[i][i])*g;
            for (k=i;k<m;k++) a[k][j] += f*a[k][i];
         } 
         for (j=i;j<m;j++) a[j][i] *= g;
      } else for (j=i;j<m;j++) a[j][i]=0.0;
      ++a[i][i];
   } 
   for (k=n-1;k>=0;k--) {
      for (its=1;its<=250;its++) {
         flag=1; 
         for (l=k;l>=0;l--) {
            nm=l-1;
            if (abs(rv1[l]) < eps) { 
               flag=0; 
               break; 
            } 
            if (abs(w[nm]) < eps) break;
         }
         if (flag) {
            c=0.0;
            s=1.0;
            for (i=l;i<=k;i++) {
               f=s*rv1[i];
               rv1[i]=c*rv1[i];
               if (abs(f) < eps) break;
               g=w[i];
               h=pythag(f,g);
               w[i]=h;
               h=1.0/h;
               c=g*h;
               s = -f*h;
               for (j=0;j<m;j++) {
                  y=a[j][nm];
                  z=a[j][i];
                  a[j][nm]=y*c+z*s;
                  a[j][i]=z*c-y*s;
               } 
            } 
         } 
         z=w[k]; 
         if(l == k) {
            if (z < 0.0) {
               w[k] = -z;
               for (j=0;j<n;j++) v[j][k] = -v[j][k];
            } 
            break;
         }
         if (its == 250){
            Rf_warning("svd: convergence might have failed in 250 iterations.\n"); 
         }
         x=w[l];
         nm=k-1; 
         y=w[nm]; 
         g=rv1[nm]; 
         h=rv1[k]; 
         f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y); 
         g=pythag(f,1.0); 
         f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x; 
         c=s=1.0;
         for (j=l;j<=nm;j++) { 
            i=j+1;
            g=rv1[i];
            y=w[i];
            h=s*g;
            g=c*g;
            z=pythag(f,h);
            rv1[j]=z;
            c=f/z;
            s=h/z;
            f=x*c+g*s;
            g = g*c-x*s;
            h=y*s;
            y*=c;
            for (jj=0;jj<n;jj++) {
               x=v[jj][j];
               z=v[jj][i];
               v[jj][j]=x*c+z*s;
               v[jj][i]=z*c-x*s;
            }
            z=pythag(f,h);
            w[j]=z;
            if (z) {
               z=1.0/z;
               c=f*z;
               s=h*z;
            }
            f=c*g+s*y;
            x=c*y-s*g;
            for (jj=0;jj<m;jj++) {
               y=a[jj][j];
               z=a[jj][i];
               a[jj][j]=y*c+z*s;
               a[jj][i]=z*c-y*s;
            }
         }
         rv1[l]=0.0;
         rv1[k]=f;
         w[k]=x;
      }
   }

   delete[] rv1;
} 

/*--------------------------------------------
 print vetor arr into m rows and n columns
 default: width=12
 --------------------------------------------*/
template <class T>
void arr_print(T* arr,int m,int n,int width){
   Rprintf("\n");
   for(int i=0;i<m;i++){
      for(int j=0;j<n;j++){
         cout<<setw(width)<<arr[i*n+j];
      }
      Rprintf("\n");
   }
   Rprintf("\n");
}

/*--------------------------------------------
 print m by n arr into n columns
 default: width=12
 --------------------------------------------*/
template <class T>
void arr_print(T** arr,int m,int n,int width){
   Rprintf("\n");
   for(int i=0;i<m;i++){
      for(int j=0;j<n;j++){
         cout<<setw(width)<<arr[i][j];
      }
      Rprintf("\n");
   }
   Rprintf("\n");
}

/*--------------------------------------------------
 copy array1 into array (of length n)
 --------------------------------------------------*/
template <class T>
void arr_copy(T* arr1,int n,T* arr){
   for(int i=0;i<n;i++){
      arr[i]=arr1[i];
   }
}

/*--------------------------------------------------
 copy m by n array1 into m by n array
 --------------------------------------------------*/
template <class T>
void arr_copy(T** arr1,int m,int n,T** arr){
   for(int i=0;i<m;i++){
      for(int j=0;j<n;j++){
         arr[i][j]=arr1[i][j];
      }
   }
}

/*--------------------------------------------
 kronecker product of two arrays
 arr1: nr1 by nc1; arr2: nr2 by nc2
 arr: nr1*nr2 by nc1*nc2 to be returned
 --------------------------------------------*/
template <class T>
void arr_kronecker(T** arr1,int nr1,int nc1,T** arr2,int nr2,int nc2,T** arr){
   int i; int j;
   for(int i1=0;i1<nr1;i1++){
      for(int j1=0;j1<nc1;j1++){
         i=i1*nr2;
         for(int i2=0;i2<nr2;i2++){
            j=j1*nc2;
            for(int j2=0;j2<nc2;j2++){
               arr[i][j] = arr1[i1][j1]*arr2[i2][j2];
               j++;
            }
            i++;
         }
      }
   }
}

/*----------------------------------
 arr1: m by n; arr2: n by 1
 arr: m by 1 to be returned
 ----------------------------------*/
template <class T>
void arr_prod(T** arr1,T* arr2,int m,int n,T* arr){
   for(int i=0;i<m;i++){
      arr[i]=0;
      for(int l=0;l<n;l++)
         arr[i] += arr1[i][l]*arr2[l];
   }
}

/*----------------------------------
 arr1: m by k; arr2: k by n
 arr: m by n to be returned
 ----------------------------------*/
template <class T>
void arr_prod(T** arr1,T** arr2, int m,int k,int n,T** arr){
   for(int i=0;i<m;i++){
      for(int j=0;j<n;j++){
         arr[i][j]=0;
         for(int l=0;l<k;l++){
            arr[i][j] += arr1[i][l]*arr2[l][j];
         }
      }
   }
}

/*----------------------------------
 arr1: n by n; arr2: n by n
 arr: n by n to be returned
 ----------------------------------*/
template <class T>
void arr_prod(T** arr1,T** arr2, int n,T** arr){
   arr_prod(arr1, arr2, n, n, n, arr);
}

/*------------------------------------------------------------
 cumsum: cumulative sumation of an array of length n
 ------------------------------------------------------------*/
template <class T>
void cumsum(T* x,int n,T* arr){
   T s=0;
   for(int i=0;i<n;i++){
      s+=x[i];
      arr[i]=s;
   }
}

/*-----------------------------------------------
 largest: obtain largest k values
 from an array x of length n
 returned by arr (k<=n)
 choice: indices of elements being chosen
 default choice: NULL
 -----------------------------------------------*/
template <class T>
void largest(T *x,int n,T *arr,int k,int *which){
   if(k>n){
      error(_("largest: cann't get more numbers than given...\n"));
   }
   int* p=new int[n];
   order(x,n,p,false);
   if(which==NULL){
      for(int i=0;i<k;i++){
         arr[i]=x[p[i]];
      }
   }
   else{
      for(int i=0;i<k;i++){
         arr[i]=x[p[i]]; which[i]=p[i];
      }
   }
   delete[] p;
}

/*---------------------------------------------------------
 max: obtain the maximum of an array x of length n
 ---------------------------------------------------------*/
template <class T>
T max(T* x,int n,int* which){
   T m=x[0];
   if(which==NULL){
      for(int i=0;i<n;i++){
         if(x[i]>m)m=x[i];
      }
   }
   else{
      *which=0;
      for(int i=0;i<n;i++){
         if(x[i]>m){m=x[i]; *which=i;}
      }
   }
   return(m);
}

/*---------------------------------------------------------
 meam: obtain the mean of an array x of length n
 ---------------------------------------------------------*/
template <class T>
double mean(T* x,int n){
   double m;
   m=static_cast<double>(sum(x,n))/n;
   return m;
}

/*---------------------------------------------------------
 min: obtain the minimum of an array x of length n
 ---------------------------------------------------------*/
template <class T>
T min(T* x,int n,int* which){
   T m=x[0];
   if(which==NULL){
      for(int i=0;i<n;i++){
         if(x[i]<m)m=x[i];
      }
   }
   else{
      *which=0;
      for(int i=0;i<n;i++){
         if(x[i]<m){m=x[i]; *which=i;}
      }
   }
   return(m);
}

/*----------------------------------------------------------
 order: obtain the order of an array x of length n
 returned by integeral arr of length n
 default: increasing=true
 ----------------------------------------------------------*/
template <class T>
void order(T* x, int n, int* arr, bool increasing){
   T tmp;
   int k;
   T* p=new T[n];

   for(int i=0;i<n;i++){
      p[i]=x[i];
      arr[i]=i;
   }
   if(increasing==true){
      for(int i=0;i<n-1;i++){
         for(int j=i+1;j<n;j++){
            if(p[j]<p[i]){
               tmp=p[i];
               p[i]=p[j];
               p[j]=tmp;
               k=arr[i];
               arr[i]=arr[j];
               arr[j]=k;
            }
         }
      }
   }
   else if(increasing==false){
      for(int i=0;i<n-1;i++){
         for(int j=i+1;j<n;j++){
            if(p[j]>p[i]){
               tmp=p[i];
               p[i]=p[j];
               p[j]=tmp;
               k=arr[i];
               arr[i]=arr[j];
               arr[j]=k;
            }
         }
      }
   }
   delete[] p;
}

/*--------------------------------------------------------------
 quantile: obtain p-quantile of an array x of length n
 --------------------------------------------------------------*/
template <class T>
double quantile(T* x, int n, double p){
   if(p<0||p>1){
      error(_("misspecified p!\n"));
   }

   T* ptr=new T[n];
   sort(x,n,ptr);
   T q;
   int indx;
   double y1,y2;

   if(p<=1.0/n) q=ptr[0];
   else{
      for(int i=1;i<n;i++){
         if(p>(i+0.0)/n&&p<=(i+1.0)/n){
            indx=i; 
            break;
         }
      }
      y1=static_cast<double>(ptr[indx-1]); 
      y2=static_cast<double>(ptr[indx]);
      q=y1+(y2-y1)/((indx+1.0)/n-(indx+0.0)/n)*(p-(indx+0.0)/n);
   }

   delete[] ptr;
   return(q);
}

/*----------------------------------------------------
 read a distk file -- infile into an array x
 ----------------------------------------------------*/
template <class T>
void read_T(T* x,char infile[]){
   T* p=x;
   ifstream inf(infile);
   while(inf){
      inf>>*p;
      p++;
   }
   inf.close();
}

/*--------------------------------------------------------------
 rep: genarate an array of length n with same element x
 returned by arr of length n
 ---------------------------------------------------------------*/
template <class T>
void rep(T x,int n,T* arr){
   for(int i=0;i<n;i++){
      arr[i]=x;
   }
}

/*-------------------------------------------
 round: round a number x to n-th place
 default: n=0
 -------------------------------------------*/
template <class T>
double round(T x,int n){
   double y;
   double rd;
   rd=static_cast<double>(n);
   y=static_cast<double>(x);
   y=floor(y*pow(10,rd)+0.5)/pow(10,rd);
   return y;
}

/*-----------------------------------------------------------
 Sample: sample k observations from array x of n
 to array arr of k
 default: replace=false, seed=0 (time-dependent)
 Note: a permutation when k=n and replace=false
 ------------------------------------------------------------*/
template <class T>
void sample(T* x,int n,T* arr,int k,long int *seed,bool replace){
   if(replace==false){
      if(n<k){
         error(_("Cannot sample from a smaller group without replacement...\n"));
      }
      double *rs=new double[n];
      runif(rs,n,seed);

      int* ord=new int[n];
      order(rs,n,ord);
      for(int i=0;i<k;i++){
         arr[i]=x[ord[i]];
      }
      delete[] rs;
      delete[] ord;
   }
   else if(replace==true){
      int irs;
      double tmp;
      for(int i=0;i<k;i++){
         runif(&tmp,1,seed);
         irs=static_cast<int>(floor(tmp));
         arr[i]=x[irs];
      }
   }
}

/*---------------------------------------------------
 standard deviation of an array of length n
 ---------------------------------------------------*/
template <class T>
double sd(T* x,int n){
   double sdv;

   sdv=sqrt(var(x,n));
   return sdv;
}

/*-----------------------------------------------
 smallest: obtain smallest k values
 from an array x of length n
 returned by arr (k<=n)
 choice: indices of elements being chosen
 default choice: NULL
 -----------------------------------------------*/
template <class T>
void smallest(T *x,int n,int k,T *arr,int *choice){
   if(k>n){
      error(_("largest: cann't get more numbers than given...\n"));
   }
   int* p=new int[n];
   order(x,n,p,true);
   if(choice==NULL){
      for(int i=0;i<k;i++){
         arr[i]=x[p[i]];
      }
   }
   else{
      for(int i=0;i<k;i++){
         arr[i]=x[p[i]]; choice[i]=p[i];
      }
   }
   delete[] p;
}

/*----------------------------------------
 sort: sort an array x of length n
 returned by arr of length n
 default: increasing=true
 ----------------------------------------*/
template <class T>
void sort(T* x,int n,T* arr,bool increasing){
   T tmp;

   for(int i=0;i<n;i++){
      arr[i]=x[i];
   }
   if(increasing==true){
      for(int i=0;i<n-1;i++){
         for(int j=i+1;j<n;j++){
            if(arr[j]<arr[i]){
               tmp=arr[i];
               arr[i]=arr[j];
               arr[j]=tmp;
            }
         }
      }
   }
   else if(increasing==false){
      for(int i=0;i<n-1;i++){
         for(int j=i+1;j<n;j++){
            if(arr[j]>arr[i]){
               tmp=arr[i];
               arr[i]=arr[j];
               arr[j]=tmp;
            }
         }
      }
   }
}

/*-------------------------------------------
 subset: select a subset of k elements
 by choice from an array x of length n
 returned by arr of length k
 -------------------------------------------*/
template <class T>
void subset(T* x,bool* choice,int n,T* arr){
   int indx=0;
   for(int i=0;i<n;i++){
      if(choice[i]==true){
         arr[indx]=x[i];
         indx++;
      }
   }
}

/*----------------------------------
   sum of an array of length n
 -----------------------------------*/
template <class T>
T sum(T* x,int n){
   T s=0;
   for(int i=0;i<n;i++){
      s+=x[i];
   }
   return s;
}

int sum(bool* x,int n){
   int s=0;
   for(int i=0;i<n;i++){
      if(x[i]==true) s++;
   }
   return(s);
}

/*-----------------------------------------
 variance of an array of length n
 -----------------------------------------*/
template <class T>
double var(T* x,int n){
   if(n<2){
      error(_("var not exits!\n"));
   }
   double m, v=0;

   m=mean(x,n);
   for(int i=0;i<n;i++){
      v += (x[i]-m)*(x[i]-m)/(n-1);
   }
   return v;
}

/*---------------------------------------------------
 write an array of length n 
 into a distk file (outfile) of ncol columns 
 ---------------------------------------------------*/
template <class T>
void write_T(T* x,int n,int ncol,char outfile[]){
   T* p=x;
   ofstream outf(outfile);
   int indx=1;
   for(int i=0;i<n;i++,indx++,p++){
      outf<<*p;
      if(indx%ncol==0){
         outf<<'\n';
      }
      else outf<<' ';
   }
   outf<<'\n';
   outf.close();
}

/********************************************************
 sure functions
 ********************************************************/

/*------------------------------------------------------------
 ML estimate b (of length kk+p) and sigma (p by p) by SURE
 y: n by p data matrix
 x: n by m data matrix (predictors)
 nqs (of length p): how many predictors to fit for y_j
 qs (of length kk): which predictors to fit for y_j
 ini_sigma: 1 if sigma is initial, 0 if not
 Return: log-likelihood
 ------------------------------------------------------------*/
double sureEst(double** y,int n,int p,double** x,int m,int* nqs,int* qs,
   double* b,double** sigma,int ini_sigma,int iter,double tol){
   #define INF 1e+308
   #define PI 3.141592653589793
   const double CONST = -n*p/2.0*(log(2.0*PI)+1.0);
   double lik, lik1, lik2;
   double la, la1;

   int kk = sum(nqs,p);
   int its = 2;
   int idx;
     double mx;
     double tmp;
   double* ybar=new double[p];
   double** e=new double*[n]; for(int i=0;i<n;i++) e[i]=new double[p];
   double* b0=new double[m+1];
   double* b1=new double[m+1];
   double* bb=new double[kk+p];
   double** sigma0=new double*[p]; for(int i=0;i<p;i++) sigma0[i]=new double[p];
   double** sinv=new double*[p]; for(int i=0;i<p;i++) sinv[i]=new double[p];
   double** den=new double*[m+1]; for(int i=0;i<m+1;i++) den[i]=new double[m+1];
   for(int j=0;j<p;j++){
      ybar[j] = 0.0;
      for(int i=0;i<n;i++){
         ybar[j] += y[i][j];
      }
      ybar[j] /= n;
   }
   for(int i=0;i<n;i++){
      for(int j=0;j<p;j++){
         e[i][j] = y[i][j]-ybar[j];
      }
   }
   if(ini_sigma==0){
      for(int j=0;j<p;j++){
         for(int k=j;k<p;k++){
            sigma[j][k] = 0.0;
            for(int i=0;i<n;i++){
               sigma[j][k] += e[i][j]*e[i][k];
            }
            sigma[j][k] /= n;
         }
         for(int k=0;k<j;k++){
            sigma[j][k] = sigma[k][j];
         }
      }
   }else if(ini_sigma==1){
      inv(sigma,p,sinv);
      while(its>0){
         idx = 0;
         for(int k=0;k<kk+p;k++){
            bb[k] = b[k];
         }
         for(int k=0;k<p;k++){
            den[0][0] = n*sinv[k][k];
            for(int s=1;s<=nqs[k];s++){
               den[0][s] = 0.0;
               for(int i=0;i<n;i++){
                  den[0][s] += x[i][qs[idx+s-1]-1];
               }
               den[0][s] *= sinv[k][k];
            }
            for(int r=1;r<=nqs[k];r++){
               for(int s=r;s<=nqs[k];s++){
                  den[r][s] = 0.0;
                  for(int i=0;i<n;i++){
                     den[r][s] += x[i][qs[idx+r-1]-1]*x[i][qs[idx+s-1]-1];
                  }
                  den[r][s] *= sinv[k][k];
               }
               for(int s=0;s<r;s++){
                  den[r][s] = den[s][r];
               }
            }
            for(int r=0;r<nqs[k]+1;r++){
               b0[r] = 0.0;
               for(int i=0;i<n;i++){
                  tmp = 0.0;
                  for(int j=0;j<p;j++){
                     if(j!=k) tmp += sinv[k][j]*e[i][j];
                     else tmp += sinv[k][j]*y[i][j];
                  }
                  if(r==0) b0[r] += tmp;
                  else b0[r] += tmp*x[i][qs[idx+r-1]-1];
               }
            }
            cholsl(den,nqs[k]+1,b0,b1);
            //solve(den,nqs[k]+1,nqs[k]+1,b0,b1);
            for(int r=0;r<nqs[k]+1;r++){
               b[idx+k+r] = b1[r];
            }
            for(int i=0;i<n;i++){
               e[i][k] = y[i][k]-b[idx+k];
               for(int r=0;r<nqs[k];r++){
                  e[i][k] -= x[i][qs[idx+r]-1]*b[idx+k+r+1];
               }
            }
            idx += nqs[k];
         }
         mx = 0.0;
         for(int k=0;k<kk+p;k++){
            tmp = abs(b[k]-bb[k]);
            if(mx<tmp) mx = tmp;
         }
         its--;
      }
   }else{
       Rprintf("ini_sigma = %d\n",ini_sigma);
      error(_("sureEst: wrong ini_sigma ...\n"));
   }
   for(int j=0;j<p;j++){
      for(int k=0;k<p;k++){
         sigma0[j][k] = sigma[j][k];
      }
   }
   lik = CONST - n/2.0*log(det(sigma,p));
   lik1 = -INF;
   lik2 = -INF;
   la = INF;
   do{
      idx = 0;
      inv(sigma,p,sinv);
      for(int k=0;k<p;k++){
         den[0][0] = n*sinv[k][k];
         for(int s=1;s<=nqs[k];s++){
            den[0][s] = 0.0;
            for(int i=0;i<n;i++){
               den[0][s] += x[i][qs[idx+s-1]-1];
            }
            den[0][s] *= sinv[k][k];
         }
         for(int r=1;r<=nqs[k];r++){
            for(int s=r;s<=nqs[k];s++){
               den[r][s] = 0.0;
               for(int i=0;i<n;i++){
                  den[r][s] += x[i][qs[idx+r-1]-1]*x[i][qs[idx+s-1]-1];
               }
               den[r][s] *= sinv[k][k];
            }
            for(int s=0;s<r;s++){
               den[r][s] = den[s][r];
            }
         }

         for(int r=0;r<nqs[k]+1;r++){
            b0[r] = 0.0;
            for(int i=0;i<n;i++){
               tmp = 0.0;
               for(int j=0;j<p;j++){
                  if(j!=k) tmp += sinv[k][j]*e[i][j];
                  else tmp += sinv[k][j]*y[i][j];
               }
               if(r==0) b0[r] += tmp;
               else b0[r] += tmp*x[i][qs[idx+r-1]-1];
            }
         }
         cholsl(den,nqs[k]+1,b0,b1);
         //solve(den,nqs[k]+1,nqs[k]+1,b0,b1);
         for(int r=0;r<nqs[k]+1;r++){
            b[idx+k+r] = b1[r];
         }
         for(int i=0;i<n;i++){
            e[i][k] = y[i][k]-b[idx+k];
            for(int r=0;r<nqs[k];r++){
               e[i][k] -= x[i][qs[idx+r]-1]*b[idx+k+r+1];
            }
         }
         idx += nqs[k];
      }
      for(int j=0;j<p;j++){
         for(int k=j;k<p;k++){
            sigma[j][k] = 0.0;
            for(int i=0;i<n;i++){
               sigma[j][k] += e[i][j]*e[i][k];
            }
            sigma[j][k] /= n;
         }
         for(int k=0;k<j;k++){
            sigma[j][k] = sigma[k][j];
         }
      }
      lik2 = lik1;
      lik1 = lik;
      lik = CONST - n/2.0*log(det(sigma,p));
      if(lik==lik1) break;
      la1 = la;
      la = lik1+(lik-lik1)/(1-(lik-lik1)/(lik1-lik2));
      mx = abs(la-la1);
      iter--;
      if(iter<0){
         Rprintf("sureEst: convergence failed...\n");
         break;
      }
   }while(mx>tol);
    
   for(int i=0;i<n;i++) delete[] e[i];
   for(int i=0;i<p;i++) delete[] sigma0[i];
   for(int i=0;i<p;i++) delete[] sinv[i];
   for(int i=0;i<m+1;i++) delete[] den[i];
   delete[] ybar; delete[] e; delete[] b0; delete[] b1; delete[] bb;
   delete[] sigma0; delete[] sinv; delete[] den;

   return lik;
}

/*------------------------------------------------------------------
 Select the predictor  to add to the SURE model by ML
 y: n by p data matrix
 x: n by m data matrix (predictors)
 nqs (of length p): how many predictors to fit for y_j
 qs (of length kk): which predictors to fit for y_j
 nupper (of length p): how many predictors to choose to fit for y_j
 upper: which predictors to choose from to fit for y_j
 sigma: will change
 which[2]: which predictor to be selected
     which[0]--which y_i, which[1]--which x_j
 ------------------------------------------------------------------*/
bool sureAdd1(double** y,int n,int p,double** x, int m,int* nqs,int* qs,
   int* nupper,int* upper,double** sigma,int* which, int iter=100, double tol=1e-8){
   int idx1=0;
   int idx2=0;
   bool no=true;
   for(int i=0;i<p;i++){//check if out of upper
      for(int j=idx1;j<idx1+nqs[i];j++){
         for(int k=idx2;k<idx2+nupper[i];k++){
            if(qs[j]==upper[k]) {no = false; break;}
         }
         if(no){
            error(_("sure add1: out of upper...\n"));
         }
      }
      idx1 += nqs[i];
      idx2 += nupper[i];
   }
   
   #define PI 3.141592653589793
   #define INF 1e+308
//   const double CONST = -n*p/2.0*(log(2.0*PI)+1.0);
   bool add = false;
   int kk=sum(nqs,p); //total number of predictors in model
   
   int idx;
   int nl;
   int* nqs1=new int[p];
   int* qs1=new int[kk+1];
   double* b1=new double[kk+p+1];
   double** sigma1=new double*[p]; for(int i=0;i<p;i++) sigma1[i]=new double[p];
   int* vl = new int[m]; 
   for(int r=0;r<p;r++){
      for(int c=0;c<p;c++){
         sigma1[r][c] = sigma[r][c];
      }
   }
   double lik;
   double likold = -INF;
   idx1=0; idx2=0;
   for(int i=0;i<p;i++){
      nl = nupper[i]-nqs[i]; //number of predictors left
      if(nl>0){
         add = true;
         for(int j=0;j<p;j++){
            if(j==i) nqs1[j] = nqs[j]+1;
            else nqs1[j] = nqs[j];
         }
         idx = 0;
         for(int k=0;k<nupper[i];k++){
            no = true;
            for(int j=0;j<nqs[i];j++){
               if(upper[idx2+k]==qs[idx1+j]){no = false; break;}
            }
            if(no){vl[idx] = upper[idx2+k]; idx++;}
         }
         for(int j=0;j<idx1+nqs[i];j++){
            qs1[j] = qs[j];
         }
         for(int j=idx1+nqs[i]+1;j<kk+1;j++){
            qs1[j] = qs[j-1];
         }
         for(int k=0;k<nl;k++){
            qs1[idx1+nqs[i]] = vl[k];
            lik = sureEst(y,n,p,x,m,nqs1,qs1,b1,sigma1,1,iter,tol);
//            lik = CONST - n/2.0*log(det(sigma1,p));
            if(lik>likold+1e-8){
               which[0] = i;
               which[1] = vl[k];
               likold = lik;
               arr_copy(sigma1,p,p,sigma);
            }
         }
      }
      idx1 += nqs[i];
      idx2 += nupper[i];
   }

   for(int i=0;i<p;i++) delete[] sigma1[i];
    delete[] nqs1; delete[] qs1; delete[] b1; delete[] sigma1;
   delete[] vl;
   return add;
}
   
/*------------------------------------------------------------------
 Select the predictor  to drop from the SURE model by ML
 y: n by p data matrix
 x: n by m data matrix (predictors)
 nqs (of length p): how many predictors to fit for y_j
 qs (of length kk): which predictors to fit for y_j
 sigma: will change
 which[2]: which predictor to be selected
     which[0]--which y_i, which[1]--which x_j
 ------------------------------------------------------------------*/
bool sureDrop1(double** y,int n,int p,double** x, int m,int* nqs,int* qs,
   int* nlower,int* lower,double** sigma,int* which,int iter=100, double tol=1e-8){
   int idx1=0;
   int idx2=0;
   bool no=true;
   for(int i=0;i<p;i++){//check if include lower
      for(int j=idx1;j<idx1+nlower[i];j++){
         for(int k=idx2;k<idx2+nqs[i];k++){
            if(lower[j]==qs[k]) {no = false; break;}
         }
         if(no){
            error(_("sure drop1: not incude lower...\n"));
         }
      }
      idx1 += nlower[i];
      idx2 += nqs[i];
   }
   
   #define PI 3.141592653589793
   #define INF 1e+308
   bool drop = false;
//   const double CONST = -n*p/2.0*(log(2.0*PI)+1.0);
   int kk=sum(nqs,p); //total number of predictors in model
   if(kk<1) return drop;
   
   int* nqs1=new int[p];
   int* qs1=new int[kk-1];
   double* b1=new double[kk+p-1];
   double** sigma1=new double*[p]; for(int i=0;i<p;i++) sigma1[i]=new double[p];
   for(int r=0;r<p;r++){
      for(int c=0;c<p;c++){
         sigma1[r][c] = sigma[r][c];
      }
   }
   double lik;
   double likold = -INF;
   int idx;
   int nr;
   int* vr = new int[m]; 
   idx1=0; idx2=0;
   for(int i=0;i<p;i++){
      nr = nqs[i]-nlower[i];
      if(nr>0){
         drop = true;
         for(int j=0;j<p;j++){
            if(j==i) nqs1[j] = nqs[j]-1;
            else nqs1[j] = nqs[j];
         }
         idx = 0;
         for(int k=0;k<nqs[i];k++){
            no = true;
            for(int j=0;j<nlower[i];j++){
               if(qs[idx2+k]==lower[idx1+j]){no = false; break;}
            }
            if(no){vr[idx] = k; idx++;}
         }
         for(int j=0;j<nr;j++){
            for(int k=0;k<idx2+vr[j];k++){
               qs1[k] = qs[k];
            }
            for(int k=idx2+vr[j]+1;k<kk;k++){
               qs1[k-1] = qs[k];
            }
            lik = sureEst(y,n,p,x,m,nqs1,qs1,b1,sigma1,1,iter,tol);
//            lik = CONST - n/2.0*log(det(sigma1,p));
            if(lik>likold+1e-8){
               which[0] = i;
               which[1] = qs[idx2+vr[j]];
               likold = lik;
               arr_copy(sigma1,p,p,sigma);
            }
         }
      }
      idx1 += nlower[i];
      idx2 += nqs[i];
   }
   
   for(int i=0;i<p;i++) delete[] sigma1[i];
    delete[] nqs1; delete[] qs1; delete[] b1; delete[] sigma1; delete[] vr;
   return drop;
}

/*------------------------------------------------------------------
 Model selection for the SURE model by ML
 y: n by p data matrix
 x: n by m data matrix (m predictors)
 nupper (of length p): how many predictors to fit for y_j
 upper (of length kk): which predictors to fit for y_j
 k: penalty for each parameter
 direction: 0 if backward, 1 if forward, 2 if both
 vin: p by m matrix (0/1); 1 if selected, 0 if not selected
    input initial model and return the final model
 record: return y_j for which to add (+) or drop (-)
    x_j to add or drop, and loglikelihood
 max_terms: forward possible only if number of terms < max_terms
 steps: max number of steps
 iter: max number of iterations in estimation
 tol: accuracy of parameter estimates
 ------------------------------------------------------------------*/
void myf1(int** vin,int p,int m,int* nxs){
   for(int i=0;i<p;i++){
      nxs[i] = 0;
      for(int j=0;j<m;j++){
         if(vin[i][j]==1) nxs[i]++;
         else if(vin[i][j]!=0){
            error(_("vin in sureStep: wrong info...\n"));
         }
      }
   }
}
void myf2(int** vin,int* nxs,int p,int m,int* xs){
   int idx=0;
   for(int i=0;i<p;i++){
      for(int j=0;j<m;j++){
         if(vin[i][j]==1) {xs[idx] = j+1; idx++;}
      }
   }
   if(idx!=sum(nxs,p)){
      error(_("Number of predictors: something was wrong...\n"));
   }
}

void sureStep(double** y,int n,int p,double** x, int m,int* nlower,int* lower,
   int* nupper,int* upper,double k,int direction,int** vin,double* record,
   int max_terms,int steps,int iter, double tol){
   bool backwd=true;
   bool forwd=true;
   if(direction == 0){backwd = true; forwd = false;}
   else if(direction == 1){backwd = false; forwd = true;}
   else if(direction == 2){backwd = true; forwd = true;}
   else {
      error(_("sureStep: wrong direction...\n"));
   }

   int* nxs=new int[p]; myf1(vin,p,m,nxs);
   int kk = sum(nxs,p);
   int* xs=new int[sum(nupper,p)+1]; myf2(vin,nxs,p,m,xs);
   
   const double CONST = -n*p/2.0*(log(2.0*PI)+1.0);
   double* b=new double[kk+p];
   double** sigma=new double*[p]; for(int i=0;i<p;i++) sigma[i]=new double[p];
   double** sigma1=new double*[p]; for(int i=0;i<p;i++) sigma1[i]=new double[p];
   sureEst(y,n,p,x,m,nxs,xs,b,sigma,0,iter,tol);

   double lik;
   double lik1;
   double aic;
   double aic1;
   int idx = 0;
   lik = CONST - n/2.0*log(det(sigma,p));
   aic = -2*lik+k*(kk+p+p*(p+1)/2);
   record[idx] = 0; idx++;
   record[idx] = 0; idx++;
   record[idx] = lik; idx++;
   bool go;
   bool yes;
   int* which=new int[2];
   while(steps>0){
      go = false;
      if(backwd){
         yes = false;
         arr_copy(sigma,p,p,sigma1);
         yes = sureDrop1(y,n,p,x,m,nxs,xs,nlower,lower,sigma1,which,iter,tol);
         if(yes){
            lik1 = CONST - n/2.0*log(det(sigma1,p));
            aic1 = -2*lik1+k*(kk+p-1+p*(p+1)/2);
            if(aic1<aic-1e-8){
               vin[which[0]][which[1]-1] = 0;
               nxs[which[0]]--;
               myf2(vin,nxs,p,m,xs); 
               arr_copy(sigma1,p,p,sigma);
               lik = lik1;
               aic = aic1;
               kk--;
               go = true;
               record[idx] = -(which[0]+1); idx++; //"-" if drop
               record[idx] = which[1]; idx++;
               record[idx] = lik; idx++;
            }
         }
      }
      if(forwd && kk<max_terms){
         yes = false;
         arr_copy(sigma,p,p,sigma1);
         yes = sureAdd1(y,n,p,x,m,nxs,xs,nupper,upper,sigma1,which,iter,tol);
         if(yes){
            lik1 = CONST - n/2.0*log(det(sigma1,p));
            aic1 = -2*lik1+k*(kk+1+p+p*(p+1)/2);
            if(aic1<aic-1e-8){
               vin[which[0]][which[1]-1] = 1;
               nxs[which[0]]++;
               myf2(vin,nxs,p,m,xs);
               arr_copy(sigma1,p,p,sigma);
               lik = lik1; 
               aic = aic1;
               kk++;
               go = true;
               record[idx] = +(which[0]+1); idx++; //"+" if add
               record[idx] = which[1]; idx++;
               record[idx] = lik; idx++;
            }
         }
      }
      if(!go) break;
      steps--;
   }
   record[idx] = 9999; idx++; //end with 9999
   record[idx] = 9999; idx++;
   record[idx] = 9999; idx++;

    for(int i=0;i<p;i++){delete[] sigma[i]; delete[] sigma1[i];}
   delete[] nxs; delete[] xs; delete[] which;
   delete[] b; delete[] sigma; delete[] sigma1;
}

/********************************************************
 mappingBasic functions
 ********************************************************/

/*--------------------------------------------
 haldane: convert recombination rate c
 to distance d (M)
 -------------------------------------------*/
double haldane(double c){
   double d;
   d=-0.5*log(1-2*c);

   return d;
}

/*------------------------------------------
 haldane_inv: convert distance d (M)
 to recombination rate c
 ------------------------------------------*/
double haldane_inv(double d){
   double c;
   c=(1-exp(-2*d))/2;

   return c;
}

/*---------------------------------------------------------
  compute recombination rate ("a-b-c")
  no interference assumed
-----------------------------------------------------------*/
double rate_ac(double r_ab, double r_bc){
  double r_ac;
  r_ac=r_ab+r_bc-2*r_ab*r_bc;

  return r_ac;
}

double rate_bc(double r_ab,double r_ac){
  double r_bc;
  r_bc=(r_ac-r_ab)/(1-2*r_ab);

  return r_bc;
}

/*------------------------------------------------
  cal condl prob of QTL(QQ)
  m1 and m2: flanking markers that take 0 or 1
  r:m1-m2 and r1:m1-qtl
-------------------------------------------------*/
double getprob(int m1,int m2,double r1,double r){
   double p;
   double r2;
   r2=rate_bc(r1,r); p=0.0;
   if(m1==1&&m2==1){
      p=(1-r1)*(1-r2)/(1-r);
   }else if(m1==1&&m2==0){
      p=(1-r1)*r2/r;
   }else if(m1==0&&m2==1){
      p=r1*(1-r2)/r;
   }else if(m1==0&&m2==0){
      p=r1*r2/(1-r);
   }else{
      error(_("wrong marker information!\n"));
   }
   return p;
}

/*--------------------------------------------------------------
 cal condl prob of QTL(QQ)
 mdat: nrow by ncol matrix (0/1)
 mid: which column of mdata (or marker id)
 d1: distance to mid
 d: length of marker interval with left flanking marker id mid
 p: array of length nrow
---------------------------------------------------------------*/
double getp_1(int mL,int mR,double d1,double d){
   double r1,r;
   double p;
   r1=haldane_inv(d1/100);
   r=haldane_inv(d/100);
   p=getprob(mL,mR,r1,r);

   return p;
}

void getp(int** mdat,int nrow,int ncol,int mid,double d1,double d,double* p){
   for(int n=0;n<nrow;n++){
      p[n]=getp_1(mdat[n][mid-1],mdat[n][mid],d1,d);
   }
}

/*--------------------------------------------------------
 i:vector of marker interval index
 nmark:vector of marker numbers on chromosomes
 returned by array m of length i_len
---------------------------------------------------------*/
void itom(int *i,int i_len,int *nmark,int nm_len,int* m){
   int* ninterval=new int[nm_len];
   for(int j=0;j<nm_len;j++){
      ninterval[j]=nmark[j]-1;
   }
   int* cumsuminterval=new int[nm_len];
   cumsum(ninterval,nm_len,cumsuminterval);
   for(int j=0;j<i_len;j++){
      int indx=0;
      for(int k=0;k<nm_len;k++){
         if(i[j]<=cumsuminterval[k])break;
         indx++;
      }
      m[j]=i[j]+indx;
   }
   delete[] ninterval;
   delete[] cumsuminterval;
}

/*----------------------------------------------------------
 m:vector of marker index
 nmark:vector of marker numbers on chromosomes
 returned by array i of length m_len
-----------------------------------------------------------*/
void mtoi(int* m,int m_len,int* nmark,int nm_len,int* i){
   int* cumsumnmark=new int[nm_len];
   cumsum(nmark,nm_len,cumsumnmark);
   for(int j=0;j<m_len;j++){
      int indx=0;
      for(int k=0;k<nm_len;k++){
         if(m[j]<=cumsumnmark[k])break;
         indx++;
    }
      i[j]=m[j]-indx;
   }
   delete[] cumsumnmark;
}

/*------------------------------------------------------------------
 read a distk file -- infile into a CLSmpos or CLSqtlpos 
 ------------------------------------------------------------------*/
template <class T> //T: mpos or qtlpos
void read_pos(T& pos,char infile[]){
   int n=pos.getn();
   ifstream inf(infile);
   for(int i=0;i<n;i++){
      inf>>pos.getid()[i]
      >>pos.getch()[i]
      >>pos.getm()[i]
      >>pos.getdist()[i];
   }
   inf.close();
}

/*------------------------------------------------------------------
 write a CLSmpos or CLSqtlpos into a distk file -- outfile 
 -------------------------------------------------------------------*/
template <class T> //T: mpos or qtlpos
void write_pos(T& pos,char outfile[]){
   int n=pos.getn();
   ofstream outf(outfile);
   for(int i=0;i<n;i++){
      outf<<pos.getid()[i]<<' '
      <<pos.getch()[i]<<' '
      <<pos.getm()[i]<<' '
      <<pos.getdist()[i]<<'\n';
   }
   outf<<'\n';
   outf.close();
}

/********************************************************
 intervalMapping functions
 ********************************************************/

/*-----------------------------------------
 _Loglik_: calculate loglikelihood
 y: vector of length n
 P: n by m matrix (m: number of components)
 G: m by k matrix (k: number of parameters)
 W: n by l matrix (covariates)
 a: vector of length l
 b: vector of length k
 -----------------------------------------*/
double _Loglik_(double* y,double** P,double** G,double** W,int n,int m,int k,int l,
   double* a,double* b,double sigma){
   double* mu=new double[m];
   double* y0=new double[n];

   if(m>1) arr_prod(G,b,m,k,mu);
      else if(m==1) mu[0]=0.0;
         else {error(_("m: wrong info...\n"));}
   for(int i=0;i<n;i++){
      y0[i] = y[i];
      for(int j=0;j<l;j++) y0[i] -= W[i][j]*a[j];
   }
   double lik=0.0;
   double tmp;
   for(int i=0;i<n;i++){
      tmp=0.0;
      for(int j=0;j<m;j++){
         tmp += P[i][j]*dnorm(y0[i],mu[j],sigma);
      }
      if(tmp>0) lik += log(tmp);
   }
   delete[] mu; delete[] y0;
   return(lik);
}

/*--------------------------------------------
 update pi (i.e., z), b(i.e., beta) and sigma
 y: vector of length n
 P: n by m matrix
 G: m by k matrix
 W: n by l matrix (covariates)
 a: vector of length l
 b: vector of length k
 z: n by m matrix
 ---------------------------------------------*/
void fz(double* y,double** P,double** G,double** W, int n,int m,int k,int l,
  double* a,double* b,double sigma,double** z){
// update pi (i.e.,z)
   double *mu=new double[m];
   double* y0=new double[n];
   double tmp;

   for(int i=0;i<n;i++){
      y0[i] = y[i];
      for(int j=0;j<l;j++) y0[i] -= W[i][j]*a[j];
   }

   if(m>1) arr_prod(G,b,m,k,mu);
      else if(m==1) mu[0]=0.0;
         else {error(_("m: wrong info...\n"));}
   for(int i=0;i<n;i++){
      tmp=0.0;
      for(int j=0;j<m;j++){
         z[i][j]=P[i][j]*dnorm(y0[i],mu[j],sigma);
         tmp += z[i][j];
      }
      if(tmp>0.0) for(int j=0;j<m;j++){
         z[i][j] /= tmp;
      }
   }
   delete[] mu; delete[] y0;
}

//CM-step in ECM algorithm
void fa(double* y,double** z,double** G,double** W,int n,int m,int k,int l,
   double* b,double* a){
   double *mu=new double[m];
   double** A0=new double*[l]; for(int i=0;i<l;i++) A0[i]=new double[l];
   double* a0=new double[l];
   double tmp;

   if(m>1) arr_prod(G,b,m,k,mu);
      else if(m==1) mu[0]=0.0;
         else {error(_("m: wrong info...\n"));}
   for(int i=0;i<l;i++){
      a0[i]=0.0;
      for(int ir=0;ir<n;ir++){
         tmp=y[ir];
         for(int ic=0;ic<m;ic++){
            tmp -= z[ir][ic]*mu[ic];
         }
         a0[i] += W[ir][i]*tmp;
      }
   }
   for(int i=0;i<l;i++){
      for(int j=0;j<l;j++){
         A0[i][j]=0.0;
         for(int ir=0;ir<n;ir++){
            A0[i][j] += W[ir][i]*W[ir][j];
         }
      }
   }

   ginv(A0,l,l,A0); arr_prod(A0,a0,l,l,a);
//  solve(A0, l, a0, a);

   for(int i=0;i<l;i++) delete[] A0[i];
   delete[] mu; delete[] A0; delete[] a0;
}

//M-step in EM algorithm
void fb(double* y,double** z,double** G,double** W,int n,int m,int k,int l,
   double* a,double* b){
   double** A0=new double*[k]; for(int i=0;i<k;i++) A0[i]=new double[k];
   double* b0=new double[k];
   double* y0=new double[n];

   for(int i=0;i<n;i++){
      y0[i] = y[i];
      for(int j=0;j<l;j++) y0[i] -= W[i][j]*a[j];
   }
   for(int i=0;i<k;i++){
      b0[i]=0.0;
      for(int ir=0;ir<n;ir++){
         for(int ic=0;ic<m;ic++){
            b0[i] += y0[ir]*z[ir][ic]*G[ic][i];
         }
      }
   }
   for(int i=0;i<k;i++){
      for(int j=0;j<k;j++){
         A0[i][j]=0.0;
         for(int ir=0;ir<n;ir++){
            for(int ic=0;ic<m;ic++){
               A0[i][j] += z[ir][ic]*G[ic][i]*G[ic][j];
            }
         }
      }
   }

   ginv(A0,k,k,A0); arr_prod(A0,b0,k,k,b);
//  solve(A0, k, b0, b);

   for(int i=0;i<k;i++) delete[] A0[i];
   delete[] A0; delete[] b0; delete[] y0;
}

void fsigma(double* y,double** z,double** G,double** W,int n,int m,int k,int l,
   double* a,double* b,double& sigma){
   double* mu=new double[m];
   double* y0=new double[n];
   double s=0.0;

   if(m>1) arr_prod(G,b,m,k,mu);
      else if(m==1) mu[0]=0.0;
         else {error(_("m: wrong info...\n"));}
   for(int i=0;i<n;i++){
      y0[i] = y[i];
      for(int j=0;j<l;j++) y0[i] -= W[i][j]*a[j];
   }
   for(int ir=0;ir<n;ir++){
      for(int ic=0;ic<m;ic++){
         s += z[ir][ic]*(y0[ir]-mu[ic])*(y0[ir]-mu[ic]);
      }
   }
   sigma = sqrt(s/n);
   delete[] mu; delete[] y0;
}

double mimEst(double* y,double** P,double** G,double** W,int n,int m,int k,int l,
   double* a,double* b,double& sigma,int init,int iter,double tol){
   #define INF 1e+308
   double** z=new double*[n]; for(int i=0;i<n;i++) z[i]=new double[m];
   double lik=0.0,lik1,lik2;
   double mx,la,la1;
   if(!init){
     if(k>0) for(int j=0;j<k;j++) b[j] = 0.0;
        else if(m>1){error(_("mimEst: either m or k wrong...\n")); }
   }
   if(sigma<0.0) sigma=sd(y,n);
   lik1 = -INF;
   lik2 = -INF;
   la = INF;

   do{
      fz(y,P,G,W,n,m,k,l,a,b,sigma,z);
      fa(y,z,G,W,n,m,k,l,b,a);
      if(k>0) fb(y,z,G,W,n,m,k,l,a,b);
      fsigma(y,z,G,W,n,m,k,l,a,b,sigma);

      lik2 = lik1;
      lik1 = lik;
      lik = _Loglik_(y,P,G,W,n,m,k,l,a,b,sigma);
      if(lik==lik1) break;
      la1 = la;
      la = lik1+(lik-lik1)/(1-(lik-lik1)/(lik1-lik2));
      mx = abs(la-la1);
      iter--;
      if(iter<0){
         Rprintf("mim: convergence failed...\n");
         break;
      }
   }while(mx>tol);

   for(int i=0;i<n;i++) delete[] z[i];
   delete[] z;
   return lik;
}

/********************************************************
 mtcmim functions
 ********************************************************/

/*-----------------------------------------
 Loglik: calculate loglikelihood
 y: n by p matrix
 P: n by np matrix, probability for a mixing component
 ma: n by p matrix (means from covariates a)
 mb: np by p matrix (means from G_j)
 invS: inverse of p by p residual covariance matrix sigma
 detS: det(sigma)
 -----------------------------------------*/
double Loglik(double** y,int n,int p,double** P,int np,
   double** ma,double** mb,double** invS,double detS){
   # define PI 3.141592653589793
   double* y0=new double[p];
   double den=pow(2*PI,p/2.0)*sqrt(detS);
   double tt, tmp;

   double lik=0.0;
   for(int in=0;in<n;in++){
      tt=0.0;
      if(np>1)for(int i=0;i<np;i++){
         if(P[in][i]>0){
            tmp=0.0;
            for(int j=0;j<p;j++) y0[j]=y[in][j]-ma[in][j];
            for(int ip=0;ip<p;ip++){
               for(int jp=0;jp<p;jp++){
                  tmp += (y0[ip]-mb[i][ip])*invS[ip][jp]*(y0[jp]-mb[i][jp]);
               }
            }
            tmp = exp(-tmp/2.0);
            tt += P[in][i]*tmp/den;
         }
      }
      else if(np==1){
         tmp=0.0;
         for(int j=0;j<p;j++) y0[j]=y[in][j]-ma[in][j];
         for(int ip=0;ip<p;ip++){
            for(int jp=0;jp<p;jp++){
               tmp += y0[ip]*invS[ip][jp]*y0[jp];
            }
         }
         tmp = exp(-tmp/2.0);
         tt += tmp/den;
      }
      else {error(_("np: wrong...\n"));}

      if(tt>0) lik += log(tt);
   }

   delete[] y0;
   return(lik);
}

/*-----------------------------------------
 fma: calculate means associated with W and a
 W: n by m matrix: covariates including 1
 nws: nws_j columns of W for y_j, detailed by ws
 a: covariate effects
 ma: n by p matrix (means from a)
 -----------------------------------------*/
void fma(int n,int p,double** W,int* nws,int* ws,double* a,double** ma){
   int* ii=new int[p+1];
   ii[0]=0;
   for(int i=0;i<p;i++) {ii[i+1] = ii[i]+nws[i];}
   for(int i=0;i<n;i++){
      for(int j=0;j<p;j++){
         ma[i][j]=0.0;
         for(int k=0;k<nws[j];k++){
            ma[i][j] += W[i][ws[ii[j]+k]-1]*a[ii[j]+k];
         }
      }
   }
   delete[] ii;
}

/*-----------------------------------------
 fmb: calculate means associated with G and b
 G: np by m genetic matrices
 b: qtl effects
 mb: np by p matrix (means from G_j)
 -----------------------------------------*/
void fmb(int p,double** G,int np,int* ngs,int* gs,double* b,double** mb){
   if(np>1){
      int* ii=new int[p+1];
      ii[0]=0;
      for(int i=0;i<p;i++) {ii[i+1] = ii[i]+ngs[i];}
      for(int i=0;i<np;i++){
         for(int j=0;j<p;j++){
            mb[i][j]=0.0;
            for(int k=0;k<ngs[j];k++){
               mb[i][j] += G[i][gs[ii[j]+k]-1]*b[ii[j]+k];
            }
         }
      }
      delete[] ii;
   }
   else if(np==1)for(int j=0;j<p;j++) mb[0][j]=0.0;
   else {error(_("np: wrong...\n"));}
}
/*------------------------------------------------------------------
 update pi (i.e., z), a(covariate effects), b(i.e., beta) and sigma
 ------------------------------------------------------------------*/
void fz(double** y,int n,int p,double** P,int np,double** ma,double** mb,
   double** invS,double** z){
   double* y0=new double[p];
   double tt,tmp;

   if(np>1) for(int i=0;i<n;i++){
      tt=0.0;
      for(int j=0;j<np;j++){
         tmp=0.0;
         if(P[i][j]>0){
            for(int k=0;k<p;k++) y0[k]=y[i][k]-ma[i][k];
            for(int ip=0;ip<p;ip++){
               for(int jp=0;jp<p;jp++){
                  tmp += (y0[ip]-mb[j][ip])*invS[ip][jp]*(y0[jp]-mb[j][jp]);
               }
            }
            tmp = exp(-tmp/2.0);
            tmp=P[i][j]*tmp;
         }
         z[i][j]=tmp;
         tt += tmp;
      }
      if(tt>0.0) for(int j=0;j<np;j++){
         z[i][j] /= tt;
      }
   }
   else if(np==1) for(int i=0;i<n;i++) z[i][0]=1.0;
   else {error(_("np: wrong...\n"));}
   delete[] y0;
}

//CM-step in ECM algorithm
void fa(double** y,int n,int p,double** W,int* nws,int* ws,
   double** z,int np,double** ma,double** mb,double** invS,double* a){
   int* kk=new int[p+1];
   double* y0=new double[p];
   int kmax=0;
   kk[0]=0;
   for(int i=0;i<p;i++){
      kk[i+1] = kk[i]+nws[i];
      if(nws[i]>kmax) kmax=nws[i];
   }
   double* x=new double[kmax];
   double* x0=new double[kmax];
   double** A=new double*[kmax]; for(int i=0;i<kmax;i++) A[i]=new double[kmax];

   int k;
   double tmp;
   for(int l=0;l<p;l++){
      k=nws[l]; if(k<1) continue;

      for(int ii=0;ii<k;ii++){
         x0[ii]=0.0;
         for(int i=0;i<n;i++){
            for(int t=0;t<p;t++){
               if(t==l) y0[t]=y[i][t];
               else y0[t]=y[i][t]-ma[i][t];
            }
            for(int j=0;j<np;j++){
               tmp=0.0;
               for(int t=0;t<p;t++) tmp += invS[l][t]*(y0[t]-mb[j][t]);
               x0[ii] += z[i][j]*W[i][ws[kk[l]+ii]-1]*tmp;
            }
         }
      }
      for(int i=0;i<k;i++){
         for(int j=0;j<k;j++){
            tmp=0.0; 
            for(int t=0;t<n;t++) tmp += W[t][ws[kk[l]+i]-1]*W[t][ws[kk[l]+j]-1];
            A[i][j]=invS[l][l]*tmp;
         }
      }
      ginv(A,k,k,A); arr_prod(A,x0,k,k,x);
//      cholsl(A,k,x0,x);
//      solve(A,k,x0,x);
      for(int i=0;i<k;i++) a[kk[l]+i]=x[i];
      for(int i=0;i<n;i++){
         tmp=0.0;
         for(int j=0;j<k;j++){
            tmp += W[i][ws[kk[l]+j]-1]*x[j];
         }
         ma[i][l]=tmp;
      }

   }

   for(int i=0;i<kmax;i++) delete[] A[i];
   delete[] x; delete[] x0; delete[] A;
   delete[] kk; delete[] y0;
}

//M-step in EM algorithm
void fb(double** y,int n,int p,double** G,int np,int* ngs,int* gs,
   double** z,double** ma,double** mb,double** invS,double* b){
   if(np<=1){error(_("b: no exits...\n"));}

   int* kk=new int[p+1];
   double* y0=new double[p];
   int kmax=0;
   kk[0]=0;
   for(int i=0;i<p;i++){
      kk[i+1] = kk[i]+ngs[i];
      if(ngs[i]>kmax) kmax=ngs[i];
   }
   double* x=new double[kmax];
   double* x0=new double[kmax];
   double** A=new double*[kmax]; for(int i=0;i<kmax;i++) A[i]=new double[kmax];

   int k;
   double tt1,tt2,tmp;
   for(int l=0;l<p;l++){
      k=ngs[l]; if(k<1) continue;

      for(int ii=0;ii<k;ii++){
         x0[ii]=0.0;
         for(int i=0;i<n;i++){
            for(int t=0;t<p;t++) y0[t]=y[i][t]-ma[i][t];
            for(int j=0;j<np;j++){
               tmp=0.0;
               for(int t=0;t<p;t++){
                  if(t==l) tmp += invS[l][t]*y0[t];
                  else tmp += invS[l][t]*(y0[t]-mb[j][t]);
               }
               x0[ii] += z[i][j]*G[j][gs[kk[l]+ii]-1]*tmp;
            }
         }
      }
      for(int i=0;i<k;i++){
         for(int j=0;j<k;j++){
            tt1=0.0; 
            for(int s=0;s<np;s++){
               tt2=0.0;
               for(int t=0;t<n;t++){
                  tt2 += z[t][s];
               }
               tt1 += tt2*G[s][gs[kk[l]+i]-1]*G[s][gs[kk[l]+j]-1];
            }
            A[i][j]=invS[l][l]*tt1;
         }
      }
      ginv(A,k,k,A); arr_prod(A,x0,k,k,x);
//      cholsl(A,k,x0,x);
//      solve(A,k,x0,x);
      for(int i=0;i<k;i++) b[kk[l]+i]=x[i];
      for(int i=0;i<np;i++){
         tt1=0.0;
         for(int j=0;j<k;j++){
            tt1 += G[i][gs[kk[l]+j]-1]*x[j];
         }
         mb[i][l]=tt1;
      }

   }

   for(int i=0;i<kmax;i++) delete[] A[i];
   delete[] x; delete[] x0; delete[] A;
   delete[] kk; delete[] y0;
}

void fS(double** y,int n,int p,double** z,int np,double** ma,double** mb,double** sigma){
   double* y0=new double[p];
   double s;

   for(int is=0;is<p;is++){
      for(int js=0;js<p;js++){
         s=0.0;
         for(int i=0;i<n;i++){
            for(int j=0;j<p;j++) y0[j]=y[i][j]-ma[i][j];
            for(int j=0;j<np;j++){
               s += z[i][j]*(y0[is]-mb[j][is])*(y0[js]-mb[j][js]);
            }
         }
         sigma[is][js]=s/n;
      }
   }
   delete[] y0;
}

/*------------------------------------------------------------
 estimates: a(covariate effects), b(i.e., beta) and sigma
 y: n by p, traits
 P: n by np, mixing proportions
 G: np by nG, genetic matrix
 ngs: vector of length p, ngs_j QTL for y_j (detailed by gs)
 W: n by nW, covariates
 ------------------------------------------------------------*/
double mtcmimEst(double** y,int n,int p,double** P,int np,
   double** G,int* ngs,int* gs,double** W,int* nws,int* ws,
   double* a,double* b,double** sigma,int init,int iter,double tol){
   #define INF 1e+308
   double** z=new double*[n]; for(int i=0;i<n;i++) z[i]=new double[np];
   double** ma=new double*[n]; for(int i=0;i<n;i++) ma[i]=new double[p];
   double** mb=new double*[np]; for(int i=0;i<np;i++) mb[i]=new double[p];
   double** invS=new double*[p]; for(int i=0;i<p;i++) invS[i]=new double[p];
   double detS;

   int na=0, nb=0;
   for(int i=0;i<p;i++) {na += nws[i]; nb += ngs[i];}
   if(!init){
      for(int i=0;i<nb;i++) b[i] = 0.0;
      for(int i=0;i<na;i++) a[i]=0.0;
      for(int i=0;i<p;i++)
         for(int j=0;j<p;j++){
            if(i==j) sigma[i][j]=1.0; else sigma[i][j]=0.0;
         }
   }
   detS=inv_det(sigma,p,invS);
   fma(n,p,W,nws,ws,a,ma);
   fmb(p,G,np,ngs,gs,b,mb);

   double lik=0.0,lik1=-INF,lik2=-INF;
   double mx,la=INF,la1;

   do{
      fz(y,n,p,P,np,ma,mb,invS,z);
      fa(y,n,p,W,nws,ws,z,np,ma,mb,invS,a);
      if(np>1) fb(y,n,p,G,np,ngs,gs,z,ma,mb,invS,b);
      fS(y,n,p,z,np,ma,mb,sigma);
      detS=inv_det(sigma,p,invS);

      lik2 = lik1;
      lik1 = lik;
      lik = Loglik(y,n,p,P,np,ma,mb,invS,detS);

      if(lik==lik1) break;
      la1 = la;
      la = lik1+(lik-lik1)/(1-(lik-lik1)/(lik1-lik2));
      mx = abs(la-la1);
      iter--;
      if(iter<0){
         Rprintf("mtcmim: convergence failed...\n");
         break;
      }
   }while(mx>tol);

   for(int i=0;i<n;i++) delete[] z[i];
   for(int i=0;i<n;i++) delete[] ma[i];
   for(int i=0;i<np;i++) delete[] mb[i];
   for(int i=0;i<p;i++) delete[] invS[i];
   delete[] z; delete[] ma; delete[] mb; delete[] invS;

   return lik;
}

/*----------------
 calculate P(Q|M)
 pp: BC-1, RIL-selfing-2, RIL-brother-sister-mating-3
 ----------------*/
double getp(int m1,int m2,double d,int pp=1){
   double p;
   p=haldane_inv(d/100);
   if(pp==2) p=2*p/(1+2*p);
   else if(pp==3) p=4*p/(1+6*p);
   else if(pp!=1){
      error(_("Only allowed: BC-1, RIL-selfing-2 or RIL-brother-sister-mating-3...\n"));
   }
   if(m1==m2) p=1-p;

   return p;
}
void fP(int** A,int nP,int nQ,int** mdat,int n,int nm,
   double** mpos,int* dists_ch,int* dists_mid,double* dists_d,int* mid,int nmid,
   double**P,int pp){
   double* d=new double[nQ];
   double* dtmp=new double[nQ];
   int* x=new int[nQ+2];
   double d0,d1;
   int nd0,nd,m;

   for(int i=0;i<n;i++){
      for(int j=0;j<nP;j++){
         nd0=0;
         for(int k=0;k<nmid;k++){
            m=mid[k];
            nd=0;
            for(int l=0;l<nQ;l++) if(dists_mid[l]==m){dtmp[nd]=dists_d[l];nd++;}
            sort(dtmp,nd,d);
            d0=mpos[m-1][3];
            if(d[nd-1]>d0+0.001){
               d1=mpos[m][3];
               for(int t=0;t<nd;t++)x[t+1]=A[j][nd0+t];
               x[0]=mdat[i][m-1]; x[nd+1]=mdat[i][m];
               P[i][j] *= getp(x[0],x[1],d[0]-d0,pp);
               P[i][j] *= getp(x[nd],x[nd+1],d1-d[nd-1],pp);
               for(int t=1;t<nd;t++){
                  P[i][j] *= getp(x[t],x[t+1],d[t]-d[t-1],pp);
               }
               P[i][j] /= getp(x[0],x[nd+1],d1-d0,pp);
            }else{
               for(int t=0;t<nd;t++)x[t+1]=A[j][nd0+t];
               x[0]=mdat[i][m-1]; x[nd+1]=mdat[i][m-1];
               for(int t=0;t<nd+1;t++){
                  P[i][j] *= getp(x[t],x[t+1],0.0,pp);
               }
            }
            nd0 += nd;
         }
      }
   }

   delete[] d; delete[] dtmp; delete[] x;
}


