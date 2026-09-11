#include "myomp.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <R.h>
#include <Rmath.h>

/* GIF algorithm by max cuts and enhanced scores */

/**
 * @brief Isolation tree structure
 * 
 */
typedef struct iso_tree {
    uint32_t size;
    uint8_t type;
    uint8_t level;
    struct iso_tree *left;
    struct iso_tree *right;
    double *lincon;
    double threshold;
} iTrees;

typedef struct vec_proj {
	double v; /* Value */
	uint32_t i; /* Original data index */
} vector;

static double *H;
static uint32_t *subs;
static vector *proj;
// #pragma omp threadprivate(subs, proj)

/**
 * @brief Comparison function between the values of structure `vec_proj`
 * 
 * @param aa Pointer to the first element to compare
 * @param bb Pointer to the second element to compare
 * @return in
 */
static int cmp_vec(void const *aa, void const *bb) {
    vector a = *(vector *) aa;
    vector b = *(vector *) bb;
    return (int) (a.v > b.v) - (int) (a.v < b.v);
}

/**
 * @brief Normalizing factors (i.e., vector of harmonic numbers)
 * 
 * @param n an integer number
 * @return double 
 */
static inline double cfun(uint32_t n) {
    return 2.0 * (H[n - 2] - (double) (n - 1) / (double) n);
}

/**
 * @brief Swap information at two pointers to `uint32_t`
 * 
 * @param a First pointer to swap
 * @param b Second pointer to swap
 */
static inline void swap(uint32_t *a, uint32_t *b) {
    uint32_t tmp = *b;
	*b = *a;
	*a = tmp;
}

/**
 * @brief Subsampling routine
 * 
 * @param nr Number of records in the dataset
 * @param psi Number of subsamples
 */
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
            for (i = 0; i < nr; i++) {
                swap(&subs[i], &subs[(uint32_t) (unif_rand() * (double) nr) % nr]);
            }
        }
        else {
            free(subs);
        }
    }
}

/**
 * @brief Initializing projection vector
 * 
 * @param nr Number of records in the dataset
 * @param psi Number of subsamples
 */
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

/**
 * @brief Get split based on the most separated values in the projection
 * 
 * @param pstrt Position of subsamples in the sorted vector
 * @param psi Number of subsamples
 * @param szl Pointer to the size of the new left branch
 * @param szr Pointer to the size of the new right branch
 * @return double The splitting value for the projected values
 */
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

/**
 * @brief Create an isolation tree
 * 
 * @param X Pointer to input data (matrix stored in column-major format)
 * @param pstrt Position of subsamples in the sorted vector
 * @param psi Number of subsamples
 * @param nr Number of records
 * @param nv Number of variables
 * @param e Current tree height (or depth)
 * @param l Height limit (or depth limit)
 * @return iTrees 
 */
static iTrees * iTree(double *X, uint32_t pstrt, uint32_t psi, uint32_t nr, uint32_t nv, uint8_t e, uint8_t const l) {
    uint32_t i, j;
    double p = nan(""), sm = 0.0;
    uint32_t szl = 0, szr = 0;
    iTrees *my_tree = NULL;
    double *w = NULL;
    my_tree = (iTrees *) calloc(1, sizeof(iTrees));
    my_tree->lincon = (double *) calloc(nv , sizeof(double));
    w = my_tree->lincon;
    if (my_tree && w) {
        if (e >= l || psi <= 1) {
            my_tree->size = psi;
            my_tree->type = 0;
        }
        else {
            my_tree->level = e;
            my_tree->type = 1;
            /* Draw random projection */
            for (i = 0; i < nv; i++) {
                w[i] = rnorm(0.0, 1.0);
                sm += w[i] * w[i];
            }
            /* Normalize the projection vector */
            sm = sm > 0.0 ? 1.0 / sqrt(sm) : 1.0;
            for (i = 0; i < nv; i++) {
                w[i] *= sm;
            }
            /* Compute the projection vector */
            if (proj) {
                for (i = pstrt; i < pstrt + psi; i++) {
                    proj[i].v = 0.0;
                    for (j = 0; j < nv; j++) {
                        proj[i].v += X[nr * j + proj[i].i] * w[j];
                    }
                }
                /* Sort the projection subvector */
                qsort(&proj[pstrt], psi, sizeof(vector), cmp_vec);
                /* Get threshold and update size left and right */
                p = get_split(pstrt, psi, &szl, &szr);
                /* Create next set of trees */
                e++;
                my_tree->left = iTree(X, pstrt, szl, nr, nv, e, l);
                my_tree->right = iTree(X, pstrt + szl, szr, nr, nv, e, l);
            }
            my_tree->threshold = p;
        }
    }
    return my_tree;
}


/**
 * @brief Create a Generalized Isolation Forest (GIF)
 * 
 * @param X Pointer to input data (matrix stored in column-major format) 
 * @param dimX Pointer to number of rows and columns of `X`
 * @param nt Pointer to number of trees
 * @param nss Pointer to subsampling size
 * @return iTrees* Pointer to a Generalized Isolation Forest (a set of iTrees)
 */
static iTrees ** iForest(double *X, int *dimX, int *nt, int *nss) {
    iTrees **Forest = NULL;
    uint32_t psi = *(uint32_t *) nss;
    uint32_t nr = (uint32_t) dimX[0];
    uint32_t nv = (uint32_t) dimX[1];
    uint32_t i, t = *(uint32_t *) nt;
    uint8_t const l = (uint8_t) (log2((double) (psi + (psi < 1))) + 0.5);
    psi = (uint32_t) (psi > nr) * nr + (uint32_t) (psi <= nr) * psi;

    if (l > 30) return NULL;
    GetRNGstate();
    Forest = (iTrees **) calloc(t, sizeof(iTrees *));
    proj = (vector *) malloc(psi * sizeof(vector));
    // #pragma omp parallel for
    if (Forest && proj) for (i = 0; i < t; i++) {
            sample(nr, psi);
            init_proj(nr, psi);
            if (subs) {
		Forest[i] = iTree(X, 0, psi, nr, nv, 0, l);
		free(subs);
	    }
    }
    PutRNGstate();
    if (proj) free(proj);
    return Forest;
}

/**
 * @brief Compute the isolation score (or path length)
 * 
 * @param x Pointer to an input vector
 * @param nv Number of variables (i.e. length of `x`) 
 * @param tree Pointer to a tree in the forest
 * @param e current path length 
 * @return double Isolation score
 */
static double path_length(double *x, uint32_t nv, iTrees *tree, uint8_t e) {
    double res = (double) e;
    double prjx = 0.0;
    uint32_t i;
    if (tree->type == 0) {
        if (tree->size > 1) res += cfun(tree->size);
        return res;
    }
    else {
        for (i = 0; i < nv; i++) {
            prjx += x[i] * tree->lincon[i];
        }
        e++;
        return path_length(x, nv, prjx <= tree->threshold ? tree->left : tree->right, e);
    }
}

/**
 * @brief Compute the anomaly score of a generalized isolation forest
 * 
 * @param x Pointer to an input vector
 * @param nv Number of variables
 * @param Forest Pointer to a foreset (i.e., double pointer to trees)
 * @param t Pointer to number of trees in the forest
 * @param psi Pointer to number of subsamples used to construct the trees in the forestuint32_t t, 
 * @return double 
 */
static double enhanced_anomaly_score(double *x, int nv, iTrees **Forest, int *t, int *psi) {
    double avglen = 0.0;
    uint32_t i;
    uint32_t it = (uint32_t) *t;
    double const ic = -1.0 / cfun((uint32_t) *psi);
    for (i = 0; i < it; i++) {
        avglen += pow(2.0, ic * path_length(x, (uint32_t) nv, Forest[i], 0));
    }
    avglen /= (double) it;
    return avglen;
}

/**
 * @brief Free the memory allocated for a tree
 * 
 * @param tree 
 */
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

/**
 * @brief Free the memory allocated for the forest
 * 
 * @param Forest Pointer to pointer of the allocated tress
 * @param t Number of trees in the forest
 */
static inline void free_forest(iTrees **Forest, int *t) {
    uint32_t i;
    for (i = 0; i < (uint32_t) *t; i++) {
        free_tree(Forest[i]);
    }
    if (Forest) free(Forest);
}

extern void gif(double *res, double *dta, int *dimD, int *nt, int *nss) {
    uint32_t i, j;
    iTrees **forest;
    double *dat;
    
    if (*nss <= 0) return;
    if (*nt <= 0) return;
    if (dimD[0] <= 0 || dimD[1] <= 0) return;

    H = (double *) malloc(dimD[0] * sizeof(double));
    dat = (double *) malloc(dimD[1] * sizeof(double));
    if (H && dat) {
        H[0] = 1.0;
        for (i = 1; i < (uint32_t) *nss; i++) 
            H[i] = H[i - 1] + 1.0 / (1.0 + (double) i);
        forest = iForest(dta, dimD, nt, nss);
        if (forest) {
            for (i = 0; i < (uint32_t) dimD[0]; i++) {
                for (j = 0; j < (uint32_t) dimD[1]; j++)
                    dat[j] = dta[dimD[0] * j + i];
                res[i] = enhanced_anomaly_score(dat, dimD[1], forest, nt, nss);
            }
        }
        free_forest(forest, nt);
    }
    if (H) free(H);
    if (dat) free(dat);
}

/*
data(iris)
dyn.load("test.so")
dta <- iris[, 1L:4L]
anom <- .C("gif", res = double(nrow(dta)), as.matrix(scale(dta)), dim(dta), 10000L, 108L)$res
hist(anom)
X11()
pairs(dta, col = iris$Species, pch = c(".", "+")[1+(anom > 0.5)], cex = 2)
mean(anom > 0.5)
*/
