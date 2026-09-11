#include "myomp.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <complex.h>
#include <R.h>
#include <Rmath.h>

typedef struct deep_neural_network {
	struct deep_neural_network *child;
	uint32_t n_in;
	uint32_t n_out;
	double complex *coef;
} dnn;

typedef struct iso_tree {
    uint32_t size;
    uint8_t type;
    uint8_t level;
    struct iso_tree *left;
    struct iso_tree *right;
    double *lincon;
    uint32_t nv_lat;
    double threshold;
} iTrees;

typedef struct vec_proj {
	double v;
	uint32_t i;
} vector;

static double *H;
static uint32_t *subs;
static vector *proj;
double complex *x;
double complex *y;

static inline double cfun(uint32_t n) {
    return 2.0 * (H[n - 2] - (double) (n - 1) / (double) n);
}

static inline double complex cs_sign(double complex x) {
    return x / (1.0 + cabs(x));
}

static inline void swap(uint32_t *a, uint32_t *b) {
    uint32_t tmp = *b;
	*b = *a;
	*a = tmp;
}

static inline void sample(uint32_t nr, uint32_t psi) {
    uint32_t i;
    subs = (uint32_t *) calloc(nr, sizeof(uint32_t));
    if (subs) {
        if (psi == nr) {
            // #pragma omp for simd
            for (i = 0; i < nr; i++)
                subs[i] = 1;
        } 
        else if (psi < nr) {
            for (i = 0; i < psi; i++) subs[i] = 1;
            for (i = 0; i < nr; i++) /** FIXME: change rand because it is not good for openMP */
                swap(&subs[i], &subs[(uint32_t) (unif_rand() * (double) nr) % nr]);
        }
        else {
        	free(subs);
        }
    }
}

static inline void init_proj(uint32_t nr, uint32_t psi) {
    uint32_t i, j;
    if (subs && proj) {
        for (i = j = 0; i < nr && j < psi; i++) {
            proj[j].i = i * (uint32_t) (subs[i] > 0);
            proj[j].v = 0.0;
            j += (uint32_t) (subs[i] > 0);
        }
    }
}

static dnn * net_init(uint32_t n_in, uint8_t n_ly) {
    uint32_t i, j, n_out;
    double const isqrtn = 1.0 / sqrt(n_in);
    dnn *layer = (dnn *) calloc(1, sizeof(dnn));
    if (layer) {
        layer->n_in = n_in;
        n_out = 1 + rpois(0.5 * (double) n_in);
        layer->n_out = n_out;
        layer->coef = (double complex *) malloc(n_in * n_out * sizeof(double complex));
        if (layer->coef) {
            for (i = 0; i < n_in; i++) {
                for (j = 0; j < n_out; j++) {
                    layer->coef[j * n_in + i] = rnorm(0.0, isqrtn) + I * rnorm(0.0, isqrtn);
                }
            }
        }
        layer->child = n_ly ? net_init(layer->n_out, n_ly - 1) : NULL;
    }
    return layer;
}

static void net_free(dnn *net) {
    if (net) {
	if (net->child) net_free(net->child);
    	if (net->coef) free(net->coef);
    	free(net);
    }
}

static double complex ** out_vec_alloc(dnn *layer, uint8_t n_ly, uint32_t nr) {
    dnn *next = layer;
    uint8_t i = 0;
    double complex **vecs = (double complex **) calloc(n_ly + 1, sizeof(double complex *));
    if (vecs && layer) {
        do {
            vecs[i] = (double complex *) calloc(next->n_out * nr, sizeof(double complex));
            next = next->child;
            i++;
        } while(next);
    }
    return vecs;
}

static void out_vec_free(double complex **vecs, uint8_t nly) {
    uint8_t i = 0;
    if (vecs) {
        for (; i <= nly; i++) if (vecs[i]) free(vecs[i]);
        free(vecs);
    }
}

static int cmp_vec(void const *aa, void const *bb) {
    vector a = *(vector *) aa;
    vector b = *(vector *) bb;
    return (int) (a.v > b.v) - (int) (a.v < b.v);
}

static inline double get_split(uint32_t pstrt, uint32_t psi, uint32_t *szl, uint32_t *szr) {
    uint32_t i, pos;
    double df, mxd = 0.0;
    pos = pstrt;
    for (i = pstrt; i < pstrt + psi - 1; i++) {
        df = proj[i + 1].v - proj[i].v;
        pos += (int32_t) (df > mxd) * (i - pos);
        mxd += (double) (df > mxd) * (df - mxd);
    }
    *szl = pos - pstrt;
    *szr = psi - *szl;
    return runif(proj[pos].v, proj[pos + 1].v);
}

static iTrees * iTree(double complex *X, uint32_t pstrt, uint32_t psi, uint32_t nv, uint8_t e, uint8_t const l) {
    uint32_t i, j;
    double p = nan(""), sm = 0.0;
    uint32_t szl = 0, szr = 0;
    iTrees *my_tree = NULL;
    double *w = NULL;
    my_tree = (iTrees *) calloc(1, sizeof(iTrees));
    my_tree->lincon = (double *) calloc((nv << 1), sizeof(double));
    w = my_tree->lincon;
    if (my_tree && w) {
        if (e >= l || psi <= 1) {GetRNGstate();
            my_tree->size = psi;
            my_tree->type = 0;
        }
        else {
            my_tree->level = e;
            my_tree->type = 1;
            /* Draw random projection */
            for (i = 0; i < (nv << 1); i++) {
                w[i] = rnorm(0.0, 1.0);
                sm += w[i] * w[i];
            }
            /* Normalize the projection vector */
            sm = sm > 0.0 ? 1.0 / sqrt(sm) : 1.0;
            for (i = 0; i < (nv << 1); i++) {
                w[i] *= sm;
            }
            /* Compute the projection vector */
            if (proj) {
                for (i = pstrt; i < pstrt + psi; i++) {
                    proj[i].v = 0.0;
                    for (j = 0; j < nv; j++) {
                        proj[i].v += creal(X[proj[i].i * nv + j]) * w[j << 1];
                        proj[i].v += cimag(X[proj[i].i * nv + j]) * w[1 | (j << 1)];
                    }
                }
                /* Sort the projection subvector */
                qsort(&proj[pstrt], psi, sizeof(vector), cmp_vec);
                /* Get threshold and update size left and right */
                p = get_split(pstrt, psi, &szl, &szr);
                /* Create next set of trees */
                e++;
                my_tree->left = iTree(X, pstrt, szl, nv, e, l);
                my_tree->right = iTree(X, pstrt + szl, szr, nv, e, l);
            }
            my_tree->threshold = p;
        }
    }
    return my_tree;
}

static double path_length(double complex *x, uint32_t nv, iTrees *tree, uint8_t e) {
    double res = (double) e;
    double prjx = 0.0;
    uint32_t i;
    if (tree->type == 0) {
        if (tree->size > 1) res += cfun(tree->size);
        return res;
    }
    else {
        for (i = 0; i < nv; i++) {
            prjx += creal(x[i]) * tree->lincon[i << 1];
            prjx += cimag(x[i]) * tree->lincon[1 | (i << 1)];
        }
        e++;
        return path_length(x, nv, prjx <= tree->threshold ? tree->left : tree->right, e);
    }
}

static void free_tree(iTrees *tree) {
    if (tree) {
	if (tree->lincon) free(tree->lincon);
    	if (tree->type) {
        	free_tree(tree->left);
        	free_tree(tree->right);
    	}
    	free(tree);
    }
}

static void iso_model(uint32_t t, double *res, double complex *dta_row_maj, uint32_t nr, uint32_t nv, uint32_t psi) {
    uint32_t i, j, k = 0, l = 0;
    uint8_t lev = (uint8_t) (log2((double) (psi + (psi < 1))) + 0.5);
    uint8_t const nly = (uint8_t) (2 + rpois(1.0));
    dnn *next;
    dnn *net_mod = net_init(nv, nly);
    double complex **vecs = out_vec_alloc(net_mod, nly, nr);
    double complex bias;
    double rln, imn, rlx, imx, tmp_db;
    double val, nrmc;
    double const it = 1.0 / (double)(t + 1);
    iTrees *mytree = NULL;
    psi = (uint32_t) (psi > nr) * nr + (uint32_t) (psi <= nr) * psi;
    nrmc = -1.0 / cfun((uint32_t) psi);
    lev = lev > 30 ? 30 : lev;
    if (net_mod && vecs && proj) {
        next = net_mod;
        for (i = 0; i < nr; i++) {
            for (j = 0; j < next->n_out; j++) {
                for (k = 0; k < next->n_in; k++) {
                    vecs[l][next->n_out * i + j] += \
                    dta_row_maj[next->n_in * i + k] * \
                    next->coef[next->n_in * j + k];
                }
            }
        }
        for (j = 0; j < next->n_out; j++) {
            rlx = rln = creal(vecs[l][j]);
            imx = imn = cimag(vecs[l][j]);
            for (i = 1; i < nr; i++) {  
                tmp_db = creal(vecs[l][next->n_out * i + j]);
                rlx += (double) (tmp_db > rlx) * (tmp_db - rlx);
                rln += (double) (tmp_db < rln) * (tmp_db - rln);
                tmp_db = cimag(vecs[l][next->n_out * i + j]);
                imx += (double) (tmp_db > imx) * (tmp_db - imx);
                imn += (double) (tmp_db < imn) * (tmp_db - imn);
            }
            bias = runif(-rlx, -rln) - I * runif(imn, imx);
            for (i = 0; i < nr; i++) {
                vecs[l][next->n_out * i + j] = \
                cs_sign(vecs[l][next->n_out * i + j] - bias);
            }
        }
        k = next->n_out;
        next = next->child;
        l++;
        while(next) {
            for (i = 0; i < nr; i++) {
                for (j = 0; j < next->n_out; j++) {
                    for (k = 0; k < next->n_in; k++) {
                        vecs[l][next->n_out * i + j] += \
                        vecs[l-1][next->n_in * i + k] * \
                        next->coef[next->n_in * j + k];
                    }
                }
            }
            for (j = 0; j < next->n_out; j++) {
                rlx = rln = creal(vecs[l][j]);
                imx = imn = cimag(vecs[l][j]);
                for (i = 1; i < nr; i++) {  
                    tmp_db = creal(vecs[l][next->n_out * i + j]);
                    rlx += (double) (tmp_db > rlx) * (tmp_db - rlx);
                    rln += (double) (tmp_db < rln) * (tmp_db - rln);
                    tmp_db = cimag(vecs[l][next->n_out * i + j]);
                    imx += (double) (tmp_db > imx) * (tmp_db - imx);
                    imn += (double) (tmp_db < imn) * (tmp_db - imn);
                }
                bias = runif(-rlx, -rln) - I * runif(imn, imx);
                for (i = 0; i < nr; i++) {
                    vecs[l][next->n_out * i + j] = \
                    cs_sign(vecs[l][next->n_out * i + j] - bias);
                }
            }
            k = next->n_out;
            next = next->child;
            l++;
        }
    }
    net_free(net_mod);
    proj = (vector *) malloc(psi * sizeof(vector));
    sample(nr, psi);
    init_proj(nr, psi);
    if (proj && subs) {
        mytree = iTree(vecs[nly], 0, psi, k, 0, lev);
        if (mytree) {
            for (i = 0; i < nr; i++) {
                val = path_length(&vecs[nly][k * i], k, mytree, 0) * nrmc;
                res[i] *= (double) t * it;
                res[i] += it * log1p(- pow(2.0, val)); /* * path_length... */
            }
        }
    }
    if (subs) free(subs);
    if (proj) free(proj);
    out_vec_free(vecs, nly);    
    free_tree(mytree);
}

extern void dif(double *res, double *dta, int *dimD, int *nt, int *nss) {
    uint32_t i, t;
    /* srand(time(NULL)); */
    GetRNGstate();
    H = (double *) malloc(dimD[0] * sizeof(double));
    double complex *dta_row_major = (double complex *) \
        malloc(dimD[0] * dimD[1] * sizeof(double complex));
    if (dta_row_major && H) {
        H[0] = 1.0;
        for (i = 1; i < (uint32_t) *nss; i++) 
            H[i] = H[i - 1] + 1.0 / (1.0 + (double) i);
        for (i = 0; i < dimD[0]; i++) {
            for (t = 0; t < dimD[1]; t++) {
                dta_row_major[dimD[1] * i + t] = \
                    dta[dimD[0] * t + i] + I * 0.0;
            }
        }
        for (t = 0; t < (uint32_t) *nt; t++) {
            iso_model(t, res, dta_row_major, \
                (uint32_t) *dimD, (uint32_t) dimD[1], \
                (uint32_t) *nss);
        }
        for (i = 0; i < (uint32_t) *dimD; i++) {
            res[i] = 1.0 - exp(res[i]);
        }
    }
    PutRNGstate();
    if (dta_row_major) free(dta_row_major);
    if (H) free(H);
}

/*
data(iris)
dyn.load("test.so")
dta <- iris[, 1L:4L]
anom <- .C("dif", res = double(nrow(dta)), as.matrix(scale(log(dta))), dim(dta), 1000L, 32L)$res
graphics.off()
hist(anom)
X11()
thresh <- quantile(anom, prob = 0.95)
pairs(dta, col = iris$Species, pch = c(".", "+")[1+(anom > thresh)], cex = 2)
*/

#ifdef DEBUG
#include "../../.data/iris.h"
int main() {
    int i = 0, ntrees = 1000, nss = 32;
    double res[N] = {0};
    int dim[2] = {N, P};
    dif(res, x_iris, dim, &ntrees, &nss);
    for (; i < N; i++) printf("%g ", res[i]);
    printf("\n");
    return 0;
}
#endif

