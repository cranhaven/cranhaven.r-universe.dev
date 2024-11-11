// copied code directly from transport package mirror on github (June 2019)
#ifndef SHORTSIMPLEX_H
#define SHORTSIMPLEX_H
#include <math.h>
#include <R.h>
#include <R_ext/Utils.h>
#include <stdlib.h>


//' Runs the shortlist algorithm of Schuhmacher et al
//'
//' @param ss An integer pointer to the short list parameter slength
//' @param kk An integer pointer to the short list parameter kfound
//' @param pp An integer pointer to the short list parameter psearched
//' @param mm An integer pointer to the number of observations in sample 1
//' @param nn An integer pointer to the number of observations in sample 2
//' @param a An integer pointer of the sample weights of the first sample of data
//' @param b An integer pointer of the sample weights of the second sample of data
//' @param costm A double pointer to the cost matrix between samples
//' @param assignment An integer pointer with the assignment matrix
//' @param basis An integer pointer with the basis function matrix
//' @return void
//' @keywords internal
void shortsimplex(int *ss, int *kk, double *pp, 
                  int *mm, int *nn, int *a, int *b, 
                  double *costm, 
                  int *assignment, int *basis);
  
#endif //SHORTSIMPLEX_H
