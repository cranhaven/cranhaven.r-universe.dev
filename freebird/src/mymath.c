#include "mymath.h"

double sign(double x){
    return (x > 0) ? 1 : ((x < 0) ? -1 : 0);
}

double max(double x,double y){
    return (x > y) ? x : y;
}

double max_abs_vec(double * x, int n){
    int i;
    double tmp = fabs(x[0]);

    for(i=1; i<n ; i++){
        tmp = max(tmp, fabs(x[i]));
    }
    return tmp;
}

double max_vec(double * x, int n){
    int i;
    double tmp = x[0];

    for(i=1; i<n ; i++){
        tmp = max(tmp, x[i]);
    }
    return tmp;
}

void max_selc(double *x, double vmax, double *x_s, int n, int *n_s, double z){
    int i,tmp;
    double thresh = vmax-z;

    tmp = 0;
    for(i=0; i<n ; i++){
        if(x[i]>thresh){
            x_s[tmp] = x[i];
            tmp++;
        }
    }
    *n_s = tmp;
}

double min(double x,double y){
    return (x < y) ? x : y;
}

double l1norm(double * x, int n){
    int i;
    double tmp=0;

    for(i=0; i<n; i++)
        tmp += fabs(x[i]);
    return tmp;
}

// ||x||_1;
double l1norm_ac(double * x, int * xa_idx, int n){
    int i,b_idx;
    double tmp=0;

    for(i=0; i<n; i++) {
        b_idx = xa_idx[i];
        tmp += fabs(x[b_idx]);
    }
    return tmp;
}

// ||x||_2^2;
double l2norm(double * x, int * xa_idx, int n){
    int i,b_idx;
    double tmp=0;

    for(i=0; i<n; i++) {
        b_idx = xa_idx[i];
        tmp += x[b_idx]*x[b_idx];
    }
    return tmp;
}

// ||x-y||_2^2;
double dif_vec_l2norm(double * x, double * y, int n){
    int i;
    double tmp,sum;

    sum=0;
    for(i=0; i<n; i++) {
        tmp = x[i]-y[i];
        sum += tmp*tmp;
    }
    return sum;
}

// ||x-y||_2^2;
double dif_vec_l2norm_as(double * x, double * y, int * xa_idx, int n){
    int i,b_idx;
    double tmp,sum;

    sum=0;
    for(i=0; i<n; i++) {
        b_idx = xa_idx[i];
        tmp = x[b_idx]-y[b_idx];
        sum += tmp*tmp;
    }
    return sum;
}

// <x,y-z>;
double inner_prod2(double * x, double * y, double * z, int n){
    int i;
    double tmp=0;

    for(i=0; i<n; i++) {
        tmp += x[i]*(y[i]-z[i]);
    }
    return tmp;
}

// <x,y-z>;
double inner_prod2_as(double * x, double * y, double * z, int * xa_idx, int n){
    int i,b_idx;
    double tmp=0;

    for(i=0; i<n; i++) {
        b_idx = xa_idx[i];
        tmp += x[b_idx]*(y[b_idx]-z[b_idx]);
    }
    return tmp;
}


void fabs_vc(double *v_in, double *v_out, int n){
    int i;

    for(i=0; i<n; i++)
        v_out[i] = fabs(v_in[i]);
}

void max_fabs_vc(double *v_in, double *v_out, double *vmax, int *n1, int n, double z){
    int i,cnt;
    double tmp, v_abs;

    tmp = 0;
    cnt = 0;
    for(i=0; i<n; i++){
        v_abs = fabs(v_in[i]);
        v_out[i] = v_abs;
        tmp = max(tmp, v_abs);
    }
    *vmax = tmp;
    *n1 = n;
}

void sort_up_bubble(double *v, int n){
    int i,j;
    double tmp;
    int ischanged;

    for(i=n-1; i>=0; i--){
        ischanged = 0;
        for(j=0; j<i; j++){
            if(v[j]>v[j+1]){
                tmp = v[j];
                v[j] = v[j+1];
                v[j+1] = tmp;
                ischanged = 1;
            }
        }
        if(ischanged==0)
            break;
    }
}

// ||y-Ax||_2^2;
double dif_l2norm(double *r, double *y, double *A, double *x, int n, int m, int size_a, int * idx_a)
{
    int i,j,w_idx;
    double tmp,l2n;

    l2n = 0;
    for(i=0;i<n;i++){
        tmp=0;
        for(j=0;j<size_a;j++){
            w_idx = idx_a[j];
            tmp+=A[w_idx*n+i]*x[w_idx];
        }
        r[i] = y[i]-tmp;
        l2n += r[i]*r[i];
    }
    return l2n;
}

// ||y-Ax||_2^2;
double dif_l2norm_as(double *r, double *y, double *A, double *x, int *xa_idx, int n, int m)
{
    int i,j,b_idx;
    double tmp,l2n;

    l2n = 0;
    for(i=0;i<n;i++){
        tmp=0;
        for(j=0;j<m;j++){
            b_idx = xa_idx[j];
            tmp+=A[b_idx*n+i]*x[b_idx];
        }
        r[i] = y[i]-tmp;
        l2n += r[i]*r[i];
    }
    return l2n;
}

void get_residual(double *r, double *y, double *A, double *x, int *xa_idx, int *nn, int *mm)
{
    int i,j,b_idx;
    int n,m;
    double tmp;
    n = *nn;
    m = *mm;

    for(i=0;i<n;i++){
        tmp=0;
        for(j=0;j<m;j++){
            b_idx = xa_idx[j];
            tmp+=A[b_idx*n+i]*x[b_idx];
        }
        r[i] = y[i]-tmp;
    }
}

void get_dual(double *u, double *r, double *mmu, int *nn)
{
    int i,n;
    double mu, zv;
    mu = *mmu;
    n = *nn;
    zv = 1;
    for(i=0;i<n;i++){
        u[i] = r[i]/mu;
    }
    euc_proj(u, zv, n); //euclidean projection
}

void get_dual1(double *u, double *r, double *mmu, int *nn)
{
    int i,n;
    double mu, zv;
    mu = *mmu;
    n = *nn;
    zv = 1;
    for(i=0;i<n;i++){
        u[i] = r[i]/mu;
        if(u[i]>zv)
            u[i] = zv;
        if(u[i]<-zv)
            u[i] = -zv;
    }
}

void get_dual2(double *u, double *r, double *mmu, int *nn)
{
    int i,n;
    double mu, zv, tmp_sum;
    mu = *mmu;
    n = *nn;
    zv = 1;
    tmp_sum = 0;
    for(i=0;i<n;i++){
        u[i] = r[i]/mu;
        tmp_sum += u[i]*u[i];
    }
    tmp_sum = sqrt(tmp_sum);
    if(tmp_sum>=zv){
        for(i=0;i<n;i++){
            u[i] = u[i]/tmp_sum;
        }
    }
}

void get_grad(double *g, double *A, double *u, int *dd, int *nn)
{
    int i,j;
    int d,n;

    d = *dd;
    n = *nn;

    for(i=0;i<d;i++){
        g[i]=0;
        for(j=0;j<n;j++){
            g[i] -= A[i*n+j]*u[j];
        }
    }
}

void get_base(double *base, double *u, double *r, double *mmu, int *nn)
{
    int i,n;
    double mu,tmp;
    mu = *mmu;
    n = *nn;
    tmp = 0;
    for(i=0;i<n;i++){
        tmp += u[i]*u[i];
    }

    *base = 0;
    for(i=0;i<n;i++){
        *base += u[i]*r[i];
    }
    *base -= mu*tmp/2;
}

void lineaization(double *XX, double *XY, double *beta0, double *beta1, double *beta_tild, double *g, int *idx_a, double T, double threshold, int size_a, int intercept, int dim)
{
    int j,k,w_idx;

    for(j=0; j<dim; j++){
        g[j] = 0;
        for(k=0; k<size_a; k++){
            w_idx = idx_a[k];
            g[j] += XX[w_idx*dim+j]*beta0[w_idx];
        }
        g[j] = g[j] - XY[j];
        beta_tild[j] = beta0[j] - g[j]/T;
    }
    for(j=0; j<dim; j++){
        if(j==0 && intercept==1)
            beta1[j] = beta_tild[j];
        else {
            if(fabs(beta_tild[j])<=threshold)
                beta1[j] = 0;
            else{
                if(beta_tild[j]>threshold)
                    beta1[j] = beta_tild[j] - threshold;
                else
                    beta1[j] = beta_tild[j] + threshold;
            }
        }
    }
}


void lineaization_lasso(double *XX, double *XY, double *beta0, double *beta1, double *beta_tild, double *g, int *idx_a, int *idx_i, int *size_a, int *idx_a1, int *idx_i1, int *size_a1, double T, double threshold, int intercept, int dim)
{
    int j;

    //for(j=0; j<dim; j++){
    //    g[j] = 0;
    //    for(k=0; k<(*size_a); k++){
    //        w_idx = idx_a[k];
    //        g[j] += XX[w_idx*dim+j]*beta0[w_idx];
    //    }
    //    g[j] = g[j] - XY[j];
    //    beta_tild[j] = beta0[j] - g[j]/T;
    //}
    *size_a1 = 0;
    for(j=0; j<dim; j++){
        beta_tild[j] = beta0[j] - g[j]/T;
        if(j==0 && intercept==1){
            beta1[j] = beta_tild[j];
        }
        else {
            if(fabs(beta_tild[j])<=threshold)
                beta1[j] = 0;
            else{
                if(beta_tild[j]>threshold)
                    beta1[j] = beta_tild[j] - threshold;
                else
                    beta1[j] = beta_tild[j] + threshold;
            }
        }
        if (beta1[j] == 0)
            idx_i1[j] = 1;
        else {
            idx_a1[*size_a1] = j;
            (*size_a1)++;
            idx_i1[j] = 0;
        }
    }
}


void lineaization_lasso_dantzig(double *XX, double *XY, double *beta0, double *beta1, double *beta_tild, double *g, int *idx_a, int *idx_scr, int *size_a, int *idx_a1, int *idx_i1, int *size_a1, double T, double threshold, int intercept, int dim)
{
    int j;

    *size_a1 = 0;
    for(j=0; j<dim; j++){
        beta_tild[j] = beta0[j] - g[j]/T;
        if(idx_scr[j]==0 && intercept==1){
            beta1[j] = beta_tild[j];
        }
        else {
            if(fabs(beta_tild[j])<=threshold)
                beta1[j] = 0;
            else{
                if(beta_tild[j]>threshold)
                    beta1[j] = beta_tild[j] - threshold;
                else
                    beta1[j] = beta_tild[j] + threshold;
            }
        }
        if (beta1[j] == 0)
            idx_i1[j] = 1;
        else {
            idx_a1[*size_a1] = j;
            (*size_a1)++;
            idx_i1[j] = 0;
        }
    }
}



void lineaization_clime(double *beta0, double *beta1, double *beta_tild, double *g, int *idx_a, int *size_a, int *idx_a1, int *idx_i1, int *size_a1, double T, double threshold, int dim)
{
    int j;

    *size_a1 = 0;
    for(j=0; j<dim; j++){
        beta_tild[j] = beta0[j] - g[j]/T;
        if(fabs(beta_tild[j])<=threshold)
            beta1[j] = 0;
        else{
            if(beta_tild[j]>threshold)
                beta1[j] = beta_tild[j] - threshold;
            else
                beta1[j] = beta_tild[j] + threshold;
        }
        if (beta1[j] == 0)
            idx_i1[j] = 1;
        else {
            idx_a1[*size_a1] = j;
            (*size_a1)++;
            idx_i1[j] = 0;
        }
    }
}
