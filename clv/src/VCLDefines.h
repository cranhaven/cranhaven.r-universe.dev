#ifndef VCLDEFINES_H_
#define VCLDEFINES_H_

#include <math.h>
#include <R.h>
#include <Rinternals.h>

#include <R_ext/PrtUtil.h>

// typedefs
typedef double(*pMetricFunct)(SEXP,SEXP,int,int,int,int,int,double*);
typedef struct clv_item clv_Item;

// enums
enum metricesNames { EUCLIDEAN = 1, MANHATTAN, CORRELATION };

// distance functions
double clv_abs(double x);
double clv_euclideanMetric(const SEXP data1_sxp, const SEXP data2_sxp, int obj1, int obj2, int data1_len, int data2_len, int dim_num, double *null_mean);
double clv_manhattanMetric(const SEXP data1_sxp, const SEXP data2_sxp, int obj1, int obj2, int data1_len, int data2_len, int dim_num, double *null_mean);
double clv_correlationMetric(const SEXP data1_sxp, const SEXP data2_sxp, int obj1, int obj2, int data1_len, int data2_len, int dim_num, double *mean);

// statistical functions
SEXP clv_mean(const SEXP matrix, int obj_num, int dim);
SEXP clv_variance(const SEXP matrix, int obj_num, int dim, const SEXP mean);
SEXP clv_clusterCenters(const SEXP matrix, int obj_num, int dim, int clust_num, int *cluster_tab, int *cluster_size);
SEXP clv_clusterVariance(const SEXP matrix, int obj_num, int dim, int clust_num, int *cluster_tab, int *cluster_size, const SEXP mean);
SEXP clv_clustersSize(const SEXP cluster_tab_sxp, int clust_num);

// section of two partitioned subsets comming from the same dataset 
SEXP clv_clusteredSetsSection(SEXP cluster1_sxp, SEXP cluster2_sxp, SEXP dim);
// confusion matrix: M[i,j] = number of objects common for cluster "i" comming from first clustering 
//                            and cluster "j" comming from second one
SEXP confusionMatrix(SEXP cluster1_sxp, SEXP cluster2_sxp, SEXP clust_num);

// other
pMetricFunct getMetricFunct(int num);

#endif /* VCLDEFINES_H_*/
