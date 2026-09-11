#include "myomp.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Lapack.h>
#include <R_ext/Boolean.h>

double *H; /* Pointer to store normalizing factors */

typedef struct node {
    double split; /* Used for one reference object */
    int proto; /* Index used for one or three reference objects */
    int proto_left; /* Index used for two or three reference objects */
    int proto_right; /* Index used for two or three reference objects */
    int dep;
    bool type; /* it is `false` for terminal leaves */
    int size;
    struct node *left;
    struct node *right;
} node;

typedef struct dblvec {
    double v;
    double vl[2];
    double vr[2];
    int i;
} dblvec;

dblvec *idx;

/**
 * @brief Normalizing factors (i.e., vector of harmonic numbers)
 *
 * @param n an integer number
 * @return double
 */
static inline double cfun(uint64_t n) {
	return 2.0 * (H[n - 2] - (double) (n - 1) / (double) n);
}

/**
 * @brief Function to allocate the structure of a node in a tree
 *
 * @return A pointer to a node structure
 */
static inline node * alloc_node(void) {
        node *n = (node *) calloc(1, sizeof(node));
        return n;
}

/*i*
 * @brief Function to free the allocated memory of a tree (or branch of a tree)
 *
 * @param n A pointer to a node structure
 * @parma nt Numbers of tree to free
 */
static void free_node(node *n, int nt) {
    int i;
    if (n) {
        for (i = 0; i < nt; i++) {
            n[i].split = 0.0;
            n[i].proto = 0;
            n[i].proto_left = 0;
            n[i].proto_right = 0;
            n[i].dep = 0;
            n[i].type = false;
            n[i].size = 0;
            if(n[i].left) free_node(n[i].left, 1);
            if(n[i].right) free_node(n[i].right, 1);
        }
        free(n);
    }
}

 /**
 * @brief Comparison function between two items from a `dblvec` structure using a single prototypes
 *
 * @param aa Pointer to the memory of the first item to compare
 * @param bb Pointer to the memory of the second item to compare
 *
 * @return int
 */
static int cmp_single_vec(void const *aa, void const *bb) {
    double a = ((dblvec *) aa)->v;
    double b = ((dblvec *) bb)->v;
    return (a > b) * 2 - 1;
}

 /**
 * @brief Comparison function between two items from a `dblvec` structure using paired prototypes
 *
 * @param aa Pointer to the memory of the first item to compare
 * @param bb Pointer to the memory of the second item to compare
 *
 * @return int
 */
static int cmp_paired_vec(void const *aa, void const *bb) {
    double a = ((dblvec *) aa)->vl[0] - ((dblvec *) aa)->vr[0];
    double b = ((dblvec *) bb)->vl[0] - ((dblvec *) bb)->vr[0];
    return (a > 0.0 && b < 0.0) * 2 - 1;
}

 /**
 * @brief Comparison function between two items from a `dblvec` structure using two prototypes and pivotal one
 *
 * @param aa Pointer to the memory of the first item to compare
 * @param bb Pointer to the memory of the second item to compare
 *
 * @return int
 */
static int cmp_pivotal_vec(void const *aa, void const *bb) {
    /* Steinhaus transform */
    double al = 2.0 * ((dblvec *) aa)->vl[0] / (((dblvec *) aa)->vl[1] + ((dblvec *) aa)->vl[0] + ((dblvec *) aa)->v);
    double bl = 2.0 * ((dblvec *) bb)->vl[0] / (((dblvec *) bb)->vl[1] + ((dblvec *) bb)->vl[0] + ((dblvec *) bb)->v);
    double ar = 2.0 * ((dblvec *) aa)->vr[0] / (((dblvec *) aa)->vr[0] + ((dblvec *) aa)->vr[1] + ((dblvec *) aa)->v);
    double br = 2.0 * ((dblvec *) aa)->vr[0] / (((dblvec *) bb)->vr[0] + ((dblvec *) bb)->vr[1] + ((dblvec *) bb)->v);
    double a = al - ar;
    double b = bl - br;
    return (a > 0.0 && b < 0.0) * 2 - 1;
}

/**
 * @brief Binary search for single prototype
 * 
 * @param vec Pointer to a presorted vector of length n
 * @param n Number of values in the array `vec`
 * @param v Value to determine the split of the vector
 * @return int Poisition of the first element in the array that is great than `v`
 */
static inline int binary_search_signle(dblvec *vec, int n, double v) {
    if (vec == NULL || n <= 0) return 0; /* Binary search failed */
    int buf, s = 0, i = n >> 1;
    while (n > s + 1) {
        buf = (int)(vec[i].v <= v);
        s = buf * i + (1 - buf) * s;
        n = buf * n + (1 - buf) * i;
        i = (n + s) >> 1;
    }
    return n; /* Binary search successed*/
}

/**
 * @brief Binary search for two prototypes
 * 
 * @param vec Pointer to a presorted vector of length n
 * @param n Number of values in the array `vec`
 * @return int Poisition of the first element in the array 
 *             where the left distance is greater than the right distance
 */
static inline int binary_search_paired(dblvec *vec, int n) {
    if (vec == NULL || n <= 0) return 0; /* Binary search failed */
    int buf, s = 0, i = n >> 1;
    while (n > s + 1) {
        buf = (int)(vec[i].vl[0] <= vec[i].vr[0]);
        s = buf * i + (1 - buf) * s;
        n = buf * n + (1 - buf) * i;
        i = (n + s) >> 1;
    }
    return n; /* Binary search successed*/
}

/**
 * @brief Binary search for two prototypes and pivot one
 * 
 * @param vec Pointer to a presorted vector of length n
 * @param n Number of values in the array `vec`
 * @return int Poisition of the first element in the array 
 *             where the left distance is greater than the right distance
 */
static inline int binary_search_pivotal(dblvec *vec, int n) {
    if (vec == NULL || n <= 0) return 0; /* Binary search failed */
    double al, ar;
    int buf, s = 0, i = n >> 1;
    while (n > s + 1) {
        al = 2.0 * vec->vl[0] / (vec->vl[0] + vec->vl[1] + vec->v);
        ar = 2.0 * vec->vr[0] / (vec->vr[0] + vec->vr[1] + vec->v);
        buf = (int)(al <= ar);
        s = buf * i + (1 - buf) * s;
        n = buf * n + (1 - buf) * i;
        i = (n + s) >> 1;
    }
    return n; /* Binary search successed*/
}

/**
 * @brief Proximity Isolation Tree with One Prototype
 *
 * @param nd Pointer to a `node` structure
 * @param idx Pointer to a dblvec structure of length `n`
 * @param n Number of data points in the current node
 * @param k Current Depth of the node in the tree
 * @param l Maximum Depth of the tree
 * @param dst_fun S-Expression for a distance (or dissimilarity) function
 * @param Rnv S-Expression for an R environment 
 */
static void pit_single(node *nd, dblvec *idx, 
                       int n, int k, int l,
                       SEXP dst_fun, SEXP Rnv) {
    int whp, i;
    SEXP dst = PROTECT(NEW_NUMERIC(1));
    SEXP id =  PROTECT(NEW_INTEGER(1));
    if (__builtin_expect(nd && idx && n > 0 && l > 0, 1)) {
        if (__builtin_expect(k >= l || n <= 1, 0)) {
            nd->size = n;
            nd->type = false;
            nd->dep = k;
        }
        else {
            nd->type = true;
            nd->dep = k;
            whp = (int)runif(0.0, (double)n);
            nd->proto = idx[whp].i;
            INTEGER(id)[0] = nd->proto + 1;
            defineVar(install("j"), id, Rnv);
            for (i = 0; i < n; i++) {
                INTEGER(id)[0] = idx[i].i + 1;
                defineVar(install("i"), id, Rnv);
                dst = eval(dst_fun, Rnv);
                idx[i].v = *(REAL(dst));
            }
            qsort(idx, n, sizeof(dblvec), cmp_single_vec);
            nd->split = runif(idx[0].v, idx[n - 1].v);
            i = binary_search_signle(idx, n, nd->split);
            nd->left = alloc_node();
            nd->right = alloc_node();
            if (__builtin_expect(nd->left && nd->right, 1)) {
                pit_single(nd->left, idx, i, k + 1, l, dst_fun, Rnv);
                pit_single(nd->right, &idx[i], n - i, k + 1, l, dst_fun, Rnv);
            }
            else {
                free_node(nd->left, 1);
                free_node(nd->right, 1);
                nd->proto = 0;
                nd->type = false;
                nd->split = nan("");
            }
        }
    }
    UNPROTECT(2);
}

/**
 * @brief Proximity Isolation Tree with Two Prototypes
 *
 * @param nd Pointer to a `node` structure
 * @param idx Pointer to a dblvec structure of length `n`
 * @param n Number of data points in the current node
 * @param k Current Depth of the node in the tree
 * @param l Maximum Depth of the tree
 * @param dst_fun S-Expression for a distance (or dissimilarity) function
 * @param Rnv S-Expression for an R environment 
 */
static void pit_paired(node *nd, dblvec *idx, 
                       int n, int k, int l,
                       SEXP dst_fun, SEXP Rnv) {
    int whp, i;
    SEXP dst = PROTECT(NEW_NUMERIC(1));
    SEXP id =  PROTECT(NEW_INTEGER(1));
    if (__builtin_expect(nd && idx && n > 0 && l > 0, 1)) {
        if (__builtin_expect(k >= l || n <= 1, 0)) {
            nd->size = n;
            nd->type = false;
            nd->dep = k;
        }
        else {
            nd->type = true;
            nd->dep = k;
            whp = (int)runif(0.0, (double)n);
            nd->proto_left = idx[whp].i + 1;
            whp = (int)runif(0.0, (double)n);
            nd->proto_right = idx[whp].i + 1;
            for (i = 0; i < n; i++) {
                INTEGER(id)[0] = nd->proto_left;
                defineVar(install("j"), id, Rnv);
                INTEGER(id)[0] = idx[i].i + 1;
                defineVar(install("i"), id, Rnv);
                dst = eval(dst_fun, Rnv);
                idx[i].vl[0] = *(REAL(dst));
                INTEGER(id)[0] = nd->proto_right;
                defineVar(install("j"), id, Rnv);
                dst = eval(dst_fun, Rnv);
                idx[i].vr[0] = *(REAL(dst));                
            }
            nd->proto_left--;
            nd->proto_right--;
            qsort(idx, n, sizeof(dblvec), cmp_paired_vec);
            i = binary_search_paired(idx, n);
            nd->left = alloc_node();
            nd->right = alloc_node();
            if (__builtin_expect(nd->left && nd->right, 1)) {
                pit_single(nd->left, idx, i, k + 1, l, dst_fun, Rnv);
                pit_single(nd->right, &idx[i], n - i, k + 1, l, dst_fun, Rnv);
            }
            else {
                free_node(nd->left, 1);
                free_node(nd->right, 1);
                nd->proto = 0;
                nd->type = false;
                nd->split = nan("");
            }
        }
    }
    UNPROTECT(2);
}

/**
 * @brief Proximity Isolation Tree with Two Prototypes And a Pivot One
 *
 * @param nd Pointer to a `node` structure
 * @param idx Pointer to a dblvec structure of length `n`
 * @param n Number of data points in the current node
 * @param k Current Depth of the node in the tree
 * @param l Maximum Depth of the tree
 * @param dst_fun S-Expression for a distance (or dissimilarity) function
 * @param Rnv S-Expression for an R environment 
 */
static void pit_pivot(node *nd, dblvec *idx, 
                       int n, int k, int l,
                       SEXP dst_fun, SEXP Rnv) {
    int whp, i;
    SEXP dst = PROTECT(NEW_NUMERIC(1));
    SEXP id =  PROTECT(NEW_INTEGER(1));
    if (__builtin_expect(nd && idx && n > 0 && l > 0, 1)) {
        if (__builtin_expect(k >= l || n <= 1, 0)) {
            nd->size = n;
            nd->type = false;
            nd->dep = k;
        }
        else {
            nd->type = true;
            nd->dep = k;
            whp = (int)runif(0.0, (double)n);
            nd->proto = idx[whp].i + 1;
            whp = (int)runif(0.0, (double)n);
            nd->proto_left = idx[whp].i + 1;
            whp = (int)runif(0.0, (double)n);
            nd->proto_right = idx[whp].i + 1;
            INTEGER(id)[0] = nd->proto_left;
            defineVar(install("j"), id, Rnv);
            INTEGER(id)[0] = nd->proto;
            defineVar(install("i"), id, Rnv);
            dst = eval(dst_fun, Rnv);
            idx[0].vl[1] = *(REAL(dst));
            INTEGER(id)[0] = nd->proto_right;
            defineVar(install("j"), id, Rnv);
            dst = eval(dst_fun, Rnv);
            idx[0].vr[1] = *(REAL(dst));
            for (i = 0; i < n; i++) {
                INTEGER(id)[0] = nd->proto_left;
                defineVar(install("j"), id, Rnv);
                INTEGER(id)[0] = idx[i].i + 1;
                defineVar(install("i"), id, Rnv);
                dst = eval(dst_fun, Rnv);
                idx[i].vl[0] = *(REAL(dst));
                idx[i].vl[1] = idx[0].vl[1];
                INTEGER(id)[0] = nd->proto_right;
                defineVar(install("j"), id, Rnv);
                dst = eval(dst_fun, Rnv);
                idx[i].vr[0] = *(REAL(dst));                
                idx[i].vr[1] = idx[0].vr[1];
                INTEGER(id)[0] = nd->proto;
                defineVar(install("j"), id, Rnv);
                dst = eval(dst_fun, Rnv);
                idx[i].v = *(REAL(dst));
            }
            nd->proto--;
            nd->proto_left--;
            nd->proto_right--;
            qsort(idx, n, sizeof(dblvec), cmp_pivotal_vec);
            i = binary_search_pivotal(idx, n);
            nd->left = alloc_node();
            nd->right = alloc_node();
            if (__builtin_expect(nd->left && nd->right, 1)) {
                pit_single(nd->left, idx, i, k + 1, l, dst_fun, Rnv);
                pit_single(nd->right, &idx[i], n - i, k + 1, l, dst_fun, Rnv);
            }
            else {
                free_node(nd->left, 1);
                free_node(nd->right, 1);
                nd->proto = 0;
                nd->type = false;
                nd->split = nan("");
            }
        }
    }
    UNPROTECT(2);
}

/**
 * @brief Proximity Isolation Forest Algorithm
 *
 * @param prx Integer number for the type of trees to train
 * @param nt Number of proximity isolation trees with single prototype to train
 * @param n Number of data points stored in `*dt`
 * @param subs Number of subsamples to randomly select from the dataset `*dt`
 * @param l Maximum Depth of the tree
 * @param dst_fun S-Expression for a distance (or dissimilarity) function
 * @param Rnv S-Expression for an R environment 
 * 
 * @return a pointer to the memory containing an array of nested `node` structures
 */
static inline node * train_pif(int prx, int nt, int n, 
                               int subs, int l, 
                               SEXP dst_fun, SEXP Rnv) {
    int i, j;
    node *roots = (node *) calloc(nt, sizeof(node));
    idx = (dblvec *) calloc(subs, sizeof(dblvec));
    if (__builtin_expect(roots && idx, 1)) {
        switch (prx)
        {
        case 2:
            for (i = 0; i < nt; i++) {
                for (j = 0; j < subs; j++)  /* Subsampling with replacement */
                    idx[j].i = (int)runif(0.0, (double)n);
                pit_paired(&roots[i], idx, subs, 0, l, dst_fun, Rnv);
            }
            break;
        case 3:
            for (i = 0; i < nt; i++) {
                for (j = 0; j < subs; j++)  /* Subsampling with replacement */
                    idx[j].i = (int)runif(0.0, (double)n);
                pit_pivot(&roots[i], idx, subs, 0, l, dst_fun, Rnv);
            }
            break;
        default:
            for (i = 0; i < nt; i++) {
                for (j = 0; j < subs; j++)  /* Subsampling with replacement */
                    idx[j].i = (int)runif(0.0, (double)n);
                pit_single(&roots[i], idx, subs, 0, l, dst_fun, Rnv);
            }
            break;
        }
    }
    if (__builtin_expect(idx != NULL, 1)) free(idx);
    return roots;
}

/**
 * @brief Compute the isolation score (or path length) for single prototype
 * 
 * @param tr Pointer to a tree in the forest
 * @param e current path length 
 * @param dst_fun S-Expression for a metric function of choice
 * @param Rnv S-Expression for an R environment used 
 *
 * @return double Isolation score
 */
static double path_length_signle(node *tr, int e,
                                 SEXP dst_fun, SEXP Rnv) {
    double res = (double) e;
    if (!tr->type) {
        if (tr->size > 1) res += cfun(tr->size);
        return res;
    }
    else {
        SEXP dst = PROTECT(NEW_NUMERIC(1));
        SEXP id = PROTECT(NEW_INTEGER(1));
        INTEGER(id)[0] = tr->proto + 1;
        defineVar(install("j"), id, Rnv);
        dst = eval(dst_fun, Rnv);
        double ans = path_length_signle(REAL(dst)[0] <= tr->split ? tr->left : tr->right, e + 1, dst_fun, Rnv);
        UNPROTECT(2);
        return ans;
    }
}

/**
 * @brief Compute the anomaly score of a proximity isolation forest for single prototype
 * 
 * @param x Index to a data point in the data set
 * @param forest Pointer to a trained foreset
 * @param nt Number of trees in the forest
 * @param nss Number of subsamples used to construct the trees in the forest
 * @param dst_fun S-Expression for a metric function of choice
 * @param Rnv S-Expression for an R environment used 
 *
 * @return double 
 */
static inline double fuzzy_anomaly_score_single(int x, node *forest, 
                                                int nt, int nss, 
                                                SEXP dst_fun, SEXP Rnv) {
	int i;
	double avglen = 1.0;
	double const nrmc = -1.0 / cfun((int) nss);
    SEXP id = PROTECT(NEW_INTEGER(1));
    INTEGER(id)[0] = x + 1;
    defineVar(install("i"), id, Rnv);
	for (i = 0; i < nt; i++) {
		avglen += log1p(- pow(2.0, \
			path_length_signle(&forest[i], 0, dst_fun, Rnv) * nrmc));
	}
	avglen /= (double) nt;
    UNPROTECT(1);
	return 1.0 - exp(avglen);
}

/**
 * @brief Compute the isolation score (or path length) for two prototypes
 * 
 * @param tr Pointer to a tree in the forest
 * @param e current path length 
 * @param dst_fun S-Expression for a metric function of choice
 * @param Rnv S-Expression for an R environment used 
 *
 * @return double Isolation score
 */
static double path_length_paired(node *tr, int e,
                                 SEXP dst_fun, SEXP Rnv) {
    double res = (double) e;
    if (!tr->type) {
        if (tr->size > 1) res += cfun(tr->size);
        return res;
    }
    else {
        double ds[2] = {0};
        SEXP dst = PROTECT(NEW_NUMERIC(1));
        SEXP id = PROTECT(NEW_INTEGER(1));
        INTEGER(id)[0] = tr->proto_left + 1;
        defineVar(install("j"), id, Rnv);
        dst = eval(dst_fun, Rnv);
        ds[0] = REAL(dst)[0];        
        INTEGER(id)[0] = tr->proto_right + 1;
        defineVar(install("j"), id, Rnv);
        dst = eval(dst_fun, Rnv);
        ds[1] = REAL(dst)[0];        
        double ans = path_length_paired(ds[0] <= ds[1] ? tr->left : tr->right, e + 1, dst_fun, Rnv);
        UNPROTECT(2);
        return ans;
    }
}

/**
 * @brief Compute the anomaly score of a proximity isolation forest for two prototypes
 * 
 * @param x Index to a data point in the data set
 * @param forest Pointer to a trained foreset
 * @param nt Number of trees in the forest
 * @param nss Number of subsamples used to construct the trees in the forest
 * @param dst_fun S-Expression for a metric function of choice
 * @param Rnv S-Expression for an R environment used 
 *
 * @return double 
 */
static inline double fuzzy_anomaly_score_paired(int x, node *forest, 
                                                int nt, int nss, 
                                                SEXP dst_fun, SEXP Rnv) {
	int i;
	double avglen = 1.0;
	double const nrmc = -1.0 / cfun((int) nss);
    SEXP id = PROTECT(NEW_INTEGER(1));
    INTEGER(id)[0] = x + 1;
    defineVar(install("i"), id, Rnv);
	for (i = 0; i < nt; i++) {
		avglen += log1p(- pow(2.0, \
			path_length_paired(&forest[i], 0, dst_fun, Rnv) * nrmc));
	}
	avglen /= (double) nt;
    UNPROTECT(1);
	return 1.0 - exp(avglen);
}

/**
 * @brief Compute the isolation score (or path length) for two prototypes and pivot one
 * 
 * @param x Index to a data point in the data set
 * @param tr Pointer to a tree in the forest
 * @param e current path length 
 * @param dst_fun S-Expression for a metric function of choice
 * @param Rnv S-Expression for an R environment used 
 *
 * @return double Isolation score
 */
static double path_length_pivotal(int x, node *tr, int e,
                                 SEXP dst_fun, SEXP Rnv) {
    double res = (double) e;
    if (!tr->type) {
        if (tr->size > 1) res += cfun(tr->size);
        return res;
    }
    else {
        double ds[5] = {0};
        SEXP dst = PROTECT(NEW_NUMERIC(1));
        SEXP id = PROTECT(NEW_INTEGER(1));

        INTEGER(id)[0] = tr->proto + 1; /* Pivotal prototype */
        defineVar(install("i"), id, Rnv);

        INTEGER(id)[0] = tr->proto_left + 1;
        defineVar(install("j"), id, Rnv);
        dst = eval(dst_fun, Rnv); /* Pivotal distance from the left prototype */
        ds[0] = REAL(dst)[0];

        INTEGER(id)[0] = tr->proto_right + 1;
        defineVar(install("j"), id, Rnv);
        dst = eval(dst_fun, Rnv); /* Pivotal distance from the right prototype */
        ds[1] = REAL(dst)[0]; 

        INTEGER(id)[0] = x + 1; /* Observational point */
        defineVar(install("i"), id, Rnv);

        INTEGER(id)[0] = tr->proto_left + 1;
        defineVar(install("j"), id, Rnv);
        dst = eval(dst_fun, Rnv); /* Observational distance from the left prototype */
        ds[2] = REAL(dst)[0];
        
        INTEGER(id)[0] = tr->proto_right + 1;
        defineVar(install("j"), id, Rnv);
        dst = eval(dst_fun, Rnv); /* Observational distance from the right prototype */
        ds[3] = REAL(dst)[0];
        
        INTEGER(id)[0] = tr->proto + 1; /* Pivotal prototype */
        defineVar(install("j"), id, Rnv);
        dst = eval(dst_fun, Rnv); /* Observational distance from the pivotal prototype */
        ds[4] = REAL(dst)[0];
        
        double al = 2.0 * ds[2] / (ds[2] + ds[0] + ds[4]);
        double ar = 2.0 * ds[3] / (ds[3] + ds[1] + ds[4]);
        double ans = path_length_pivotal(x, al <= ar ? tr->left : tr->right, e + 1, dst_fun, Rnv);
        UNPROTECT(2);
        return ans;
    }
}

/**
 * @brief Compute the anomaly score of a proximity isolation forest for two prototypes and pivot one
 * 
 * @param x Index to a data point in the data set
 * @param forest Pointer to a trained foreset
 * @param nt Number of trees in the forest
 * @param nss Number of subsamples used to construct the trees in the forest
 * @param dst_fun S-Expression for a metric function of choice
 * @param Rnv S-Expression for an R environment used 
 *
 * @return double 
 */
static inline double fuzzy_anomaly_score_pivotal(int x, node *forest, 
                                                int nt, int nss, 
                                                SEXP dst_fun, SEXP Rnv) {
	int i;
	double avglen = 1.0;
	double const nrmc = -1.0 / cfun((int) nss);
	for (i = 0; i < nt; i++) {
		avglen += log1p(- pow(2.0, \
			path_length_pivotal(x, &forest[i], 0, dst_fun, Rnv) * nrmc));
	}
	avglen /= (double) nt;
	return 1.0 - exp(avglen);
}

/**
 * @brief R-warpper for Proximity Isolation Forests
 * 
 * @param dta S-Expression for an input list of data 
 * @param _prx S-Expression for the type of proximity isolation forest
 * @param _nt S-Expression for the total number of trees in the trained forest
 * @param _nss S-Expression for the number of subsamples 
 * @param max_depth S-Expression for the maximum depth of a tree in the forest
 * @param dst_fun S-Expression for a metric function of choice
 * @param Rnv S-Expression for an R environment used 
 * @return SEXP 
 */
SEXP pif(SEXP dta, SEXP _prx, SEXP _nt, SEXP _nss, 
         SEXP max_depth, SEXP dst_fun, SEXP Rnv) {
	SEXP res;
	int n, prx, nt, nss, l;
	int i;
	node *forest = NULL;

    GetRNGstate();

	PROTECT(_nt = AS_INTEGER(_nt));
	PROTECT(_nss = AS_INTEGER(_nss));
	PROTECT(_prx = AS_INTEGER(_prx));
    PROTECT(max_depth = AS_INTEGER(max_depth));

	n   = GET_LENGTH(dta);
	nt  = *(INTEGER(_nt));
	nss = *(INTEGER(_nss));
	prx = *(INTEGER(_prx));
    l = *(INTEGER(max_depth));

    PROTECT(dta = AS_LIST(dta));
	PROTECT(res = NEW_NUMERIC(n));

	H = (double *) malloc(nss * sizeof(double));
	if (__builtin_expect(H != NULL, 1)) {
		H[0] = 1.0;
		for (i = 1; i < nss; i++)
			H[i] = H[i - 1] + 1.0 / (1.0 + (double) i);
        forest = train_pif(prx, nt, n, nss, l, dst_fun, Rnv);
        switch (prx)
        {
        case 2: /* Paired algorithm */
            for (i = 0; i < n; i++)
                REAL(res)[i] = fuzzy_anomaly_score_paired(i, forest, nt, nss, dst_fun, Rnv);
            break;
            case 3: /* Pivotal algorithm */
            for (i = 0; i < n; i++)
                REAL(res)[i] = fuzzy_anomaly_score_pivotal(i, forest, nt, nss, dst_fun, Rnv);
            break;                        
            default: /* Single algorithm*/
            for (i = 0; i < n; i++)
                REAL(res)[i] = fuzzy_anomaly_score_single(i, forest, nt, nss, dst_fun, Rnv);
            break;
        }
        if (__builtin_expect(forest != NULL, 1))
			free_node(forest, nt);
	}
	if (__builtin_expect(H != NULL, 1)) free(H);

    PutRNGstate();
	UNPROTECT(6);
	return res;
}

