#include "MVRc.h"
using namespace std;

extern "C" {

//============//
// MVR_stat.c //
//============//
double sum(double* a, int n)
{
    double s = 0.0;
    for (int i = 0; i < n; ++i) {
        s += a[i];
    }
    return s;
}

double sd(double* a, double mean, int n)
{
    double sse = 0.0;
    for (int i = 0; i < n; ++i) {
        double e = a[i] - mean;
        sse += e*e;
    }
    return sqrt(sse/(double)n);
}

//============//
// MVR_rand.c //
//============//
#ifdef __MVR_C_R__
int randi()
{
    return (int)(unif_rand()*RANDF_CONST);
}

double randd()
{
    return unif_rand();
}

void MVR_rand_init()
{
    GetRNGstate();
}

void MVR_rand_end()
{
    PutRNGstate();
}

#else
int randi()
{
    return (rand()&0x7fff) | ((rand()&0x7fff) << 15);
}

//double randf()
//{
//    return (double)(randi())/RANDF_CONST;
//}

double randd()
{
    return (double)(randi())/RANDD_CONST1 + (double)(randi())/RANDD_CONST2;
}

void MVR_rand_init()
{
    srand(time(NULL));
}

void MVR_rand_end() {}
#endif

void rand_unif(double* data, int n)
{
    for (int i = 0; i < n; ++i) {
        data[i] = randd();
    }
}

//n must be even, otherwise memory overflows
void rand_norm_std(double* data, int n)
{
    double x1, x2, w;
    
    for (int i = 0; i < n; i += 2) {
        do {
            x1 = 2.0 * randd() - 1.0;
            x2 = 2.0 * randd() - 1.0;
            w = x1 * x1 + x2 * x2;
        } while ( w >= 1.0 );
        
        w = sqrt( (-2.0 * log( w ) ) / w );
        data[i] = (double)(x1 * w);
        data[i+1] = (double)(x2 * w);
    }
}

//n must be even, otherwise memory overflows
void rand_norm(double* data, int n, double mean, double s)
{
    double x1, x2, w;
    
    for (int i = 0; i < n; i += 2) {
        do {
            x1 = 2.0 * randd() - 1.0;
            x2 = 2.0 * randd() - 1.0;
            w = x1 * x1 + x2 * x2;
        } while ( w >= 1.0 );
        
        w = sqrt( (-2.0 * log( w ) ) / w ) * s;
        data[i] = (double)(x1 * w) + mean;
        data[i+1] = (double)(x2 * w) + mean;
    }
}

void rand_spl_row(double* data, int nr, int nc, double* spl, int s, int* pool)
{
    Rboolean flag_size = FALSE;
    if (s > (nr >> 1)) {
        s = nr - s;
        flag_size = TRUE;
    }
    
    //random int in [0, nr), size of s
    for (int i = 0; i < nr; ++i) pool[i] = i;
    int pool_size = nr;
    
    for (int i = 0; i < s; ++i) {
        int i_spl = randi()%pool_size;
        --pool_size;
        int tmp = pool[i_spl];
        pool[i_spl] = pool[pool_size];
        pool[pool_size] = tmp;
    }
    
    //if flag_size, keep unselected: 0 ~ pool_size-1
    //if not, keep selected: pool_size ~ nr-1
    int ir0 = pool_size,
        ir_end = nr;
    if (flag_size) {
        ir0 = 0;
        ir_end = pool_size;
        s = nr-s;
    }
    int r_des = 0;
    for (int ir = ir0; ir < ir_end; ++ir) {
        int r_src = pool[ir];
        for (int c = 0; c < nc; ++c) {
            spl[r_des+c*s] = data[r_src+c*nr];
        }
        ++r_des;
    }
}

void rand_spl_row2(double* data, int nr, int nc, double* spl, int s)
{
    //int* pool = (int*) malloc(nr*sizeof(int));
    int* pool = new int[nr];
    rand_spl_row(data, nr, nc, spl, s, pool);
    //free(pool);
    delete [] pool;
}

//==============//
// MVR_kmeans.c //
//==============//
/*
Rewritten based on kmeans_MacQueen from R distribution.
*/
Rboolean MVR_kmeans_MacQueen(double *x, double *cen,
                             int *cl, int *nc, double *wss,
                             int n, int p, int k,
                             int maxiter)
{
    /* first assign each point to the nearest cluster centre */
    int inew = 0;
    int i, j, c;
    for(i = 0; i < n; i++) {
        double best = R_PosInf;
        for(j = 0; j < k; j++) {
            double dd = 0.0;
            for(c = 0; c < p; c++) {
                double tmp = x[i+n*c] - cen[j+k*c];
                dd += tmp * tmp;
            }
            if(dd < best) {
                best = dd;
                inew = j+1;
            }
        }
        cl[i] = inew;//if(cl[i] != inew) cl[i] = inew;
    }
   /* and recompute centres as centroids */
    for(j = 0; j < k*p; j++) cen[j] = 0.0;
    for(j = 0; j < k; j++) nc[j] = 0;
    for(i = 0; i < n; i++) {
        int it = cl[i] - 1; nc[it]++;
        for(c = 0; c < p; c++) cen[it+c*k] += x[i+c*n];
    }
    for(j = 0; j < k*p; j++) cen[j] /= nc[j % k];
    
    Rboolean flag_conv = FALSE;
    for(int iter = 0; iter < maxiter; iter++) {
        flag_conv = TRUE;
        for(i = 0; i < n; i++) {
            double best = R_PosInf;
            for(j = 0; j < k; j++) {
                double dd = 0.0;
                for(c = 0; c < p; c++) {
                    double tmp = x[i+n*c] - cen[j+k*c];
                    dd += tmp * tmp;
                }
                if(dd < best) {
                    best = dd;
                    inew = j;
                }
            }
            
            int iold = cl[i] - 1;
            if(iold != inew) {
                flag_conv = FALSE;
                cl[i] = inew + 1;
                nc[iold]--; nc[inew]++;
                /* update old and new cluster centres */
                for(c = 0; c < p; c++) {
                    cen[iold+k*c] += (cen[iold+k*c] - x[i+n*c])/nc[iold];
                    cen[inew+k*c] += (x[i+n*c] - cen[inew+k*c])/nc[inew];
                }
            }
        }
        
        if(flag_conv) break;
    }
    
    for(j = 0; j < k; j++) wss[j] = 0.0;
    for(i = 0; i < n; i++) {
        int it = cl[i] - 1;
        for(c = 0; c < p; c++) {
            double tmp = x[i+n*c] - cen[it+k*c];
            wss[it] += tmp * tmp;
        }
    }
    
    return flag_conv;
}

//===========//
// MVR_sub.c //
//===========//
void MVR_km_clustering(double* x,
                       double* x_unq,
                       
                       double* centers,
                       int* cl,//vec of m
                       int* nc,//vec of k
                       double* wss,//vec of k
                       double* tot_wss,
                       int* perror,
                       
                       int* pm,
                       int* pmm,
                       int* pp,
                       int* pk,
                       int* pnstart,
                       int* pmaxiter)
{
    int m = *pm,
        mm = *pmm,
        p = *pp,
        k = *pk,
        nstart = *pnstart,
        maxiter = *pmaxiter,
        kxp = k*p;
    
    MVR_rand_init();
    
    //int* rand_spl_pool = (int*) malloc(mm*sizeof(int));
    int* rand_spl_pool = new int[mm];
    
    rand_spl_row(x_unq, mm, p, centers, k, rand_spl_pool);
    
    Rboolean flag_conv = MVR_kmeans_MacQueen(x, centers,
                                             cl, nc, wss,
                                             m, p, k,
                                             maxiter);
    *perror = flag_conv ? 0 : 1;
    double best = sum(wss, k);
    
    if (nstart > 1) {
        double *best_cen = centers,
               *best_wss = wss;
        int *best_cl = cl,
            *best_nc = nc;
        
        //double* centers2 = (double*) malloc(kxp*sizeof(double));
        //int* cl2 = (int*) malloc(m*sizeof(int));
        //int* nc2 = (int*) malloc(k*sizeof(int));
        //double* wss2 = (double*) malloc(k*sizeof(double));
        double* centers2 = new double[kxp];
        int* cl2 = new int[m];
        int* nc2 = new int[k];
        double* wss2 = new double[k];
        
        for (int i = 1; i < nstart; ++i) {
            rand_spl_row(x_unq, mm, p, centers2, k, rand_spl_pool);
            flag_conv = MVR_kmeans_MacQueen(x, centers2,
                                            cl2, nc2, wss2,
                                            m, p, k,
                                            maxiter);
            double z = sum(wss2, k);
            if (z < best) {
                *perror = flag_conv ? 0 : 1;
                best = z;
                
                best_cen = centers2,
                best_cl = cl2,
                best_nc = nc2,
                best_wss = wss2;
            }
        }
        
        if (best_cen != centers) {
            for (int i = 0; i < kxp; ++i) centers[i] = best_cen[i];
            for (int i = 0; i < m; ++i) cl[i] = best_cl[i];
            for (int i = 0; i < k; ++i) nc[i] = best_nc[i];
            for (int i = 0; i < k; ++i) wss[i] = best_wss[i];
        }
        
        best_cen = NULL;
        best_cl = NULL;
        best_nc = NULL;
        best_wss = NULL;
        
        //free(centers2);
        //free(cl2);
        //free(nc2);
        //free(wss2);
        delete [] centers2;
        delete [] cl2;
        delete [] nc2;
        delete [] wss2;
    }
    
    *tot_wss = best;
    
    //free(rand_spl_pool);
    delete [] rand_spl_pool;
    MVR_rand_end();
}


void MVR_withinsumsq(int* pn,
                     int* pp,
                     int* pk,
                     int* pB,
                     double* lWk_bo,
                     int* pnstart,
                     int* pmaxiter,
                     int* perror
                     )
{
    int B = *pB,
        n = *pn,
        p = *pp,
        k = *pk,
        nstart = *pnstart,
        maxiter = *pmaxiter;
    MVR_rand_init();
    
    //double* centers = (double*) malloc(k*p*sizeof(double));
    //int* cl = (int*) malloc(n*sizeof(int));
    //int* nc = (int*) malloc(k*sizeof(int));
    //double* wss = (double*) malloc(k*sizeof(double));
    double* centers = new double[k*p];
    int* cl = new int[n];
    int* nc = new int[k];
    double* wss = new double[k];
    
    int ref_size = n*p;
    if (ref_size & 1) ++ref_size;
    //double* ref = (double*) malloc(ref_size*sizeof(double));
    //int* rand_spl_pool = (int*) malloc(n*sizeof(int));
    double* ref = new double[ref_size];
    int* rand_spl_pool = new int[n];
    for (int b = 0; b < B; ++b) {
        rand_norm_std(ref, ref_size);
        double best = R_PosInf;
        for (int i = 0; i < nstart; ++i) {
            rand_spl_row(ref, n, p, centers, k, rand_spl_pool);
            Rboolean flag_conv = MVR_kmeans_MacQueen(ref, centers,
                                                     cl, nc, wss,
                                                     n, p, k,
                                                     maxiter);
            double z = sum(wss, k);
            if (z < best) {
                *perror = flag_conv ? 0 : 1;
                best = z;
            }
        }
        lWk_bo[b] = log(best);
    }
    
    //free(centers);
    //free(cl);
    //free(nc);
    //free(wss);
    //free(rand_spl_pool);
    //free(ref);
    delete [] centers;
    delete [] cl;
    delete [] nc;
    delete [] wss;
    delete [] rand_spl_pool;
    delete [] ref;
    
    MVR_rand_end();
}

}
