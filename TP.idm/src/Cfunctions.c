
#include <math.h>
#include <R.h>
#include <string.h>

/*
Description:
  Compares two double values.
Parameters:
  x[in]  	first double
y[in]		second double
Return value:
  Returns -1 if x is lower than y.
Returns 1 if x is greater than y.
Returns 0 otherwise.
*/
  
  static int cmp_doubles(
    const double x,
    const double y)
{
    if (x < y) return -1;
    if (x > y) return 1;
    return 0;
  } // cmp_doubles

/*
Description:
  Sorts 'x' by increasing order with 'indx' and 'indy' alongside.
Parameters:
  x[inout]		pointer to vector's first element
indx[inout]		pointer to vector's first element
indy[inout]		pointer to vector's first element
n[in]			vector's length
Return value:
  This function doesn't return a value.
*/

static void sort_univ_index(
double *const x,
int *const indx,
int *const indy,
const int n)
{
double v;
int i, j, h, iv, iu;

for (h = 1; h <= n / 9; h = 3 * h + 1);
for (; h > 0; h /= 3) {
for (i = h; i < n; i++) {
v = x[i];
iv = indx[i];
iu = indy[i];
j = i;
while (j >= h && cmp_doubles(x[j - h], v) > 0) {
x[j] = x[j-h];
indx[j] = indx[j-h];
indy[j] = indy[j-h];
j -= h;
}
x[j] = v;
indx[j] = iv;
indy[j] = iu;
}
}
} // sort_univ_index

/*
Description:
Compares two int values.
Parameters:
x[in]		first int
y[in]		second int
Return value:
Returns -1 if x is lower than y.
Returns 1 if x is greater than y.
Returns 0 otherwise.
*/

static int cmp_ints(
const int x,
const int y)
{
if (x < y) return -1;
if (x > y) return 1;
return 0;
} // cmp_ints

/*
Description:
Sorts 'indx' by decreasing order with 'x' alongside.
Parameters:
indx[inout]		pointer to vector's first element
x[inout]		pointer to vector's first element
n[in]			vector's length
Return value:
  This function doesn't return a value.
*/

static void sort_int_rev_index(
int *const indx,
int *const x,
const int n)
{
int i, j, h, v, iv;

for (h = 1; h <= n / 9; h = 3 * h + 1);
for (; h > 0; h /= 3) {
for (i = h; i < n; i++) {
iv = indx[i];
v = x[i];
j = i;
while (j >= h && cmp_ints(indx[j - h], iv) < 0) {
indx[j] = indx[j-h];
x[j] = x[j-h];
j -= h;
}
indx[j] = iv;
x[j] = v;
}
}
} // sort_int_rev_index

/*
Description:
  Sorts 'indx' by increasing order with 'x', 'indy' and 'y' alongside.
Parameters:
  indx[inout]		pointer to vector's first element
x[inout]		pointer to vector's first element
indy[inout]		pointer to vector's first element
y[inout]		pointer to vector's first element
n[in]			vector's length
Return value:
This function doesn't return a value.
*/
  
  static void sort_back_univ_weights(
    int *const indx,
    double *const x,
    int *const indy,
    double *const y,
    const int n)
{
    double v, u;
    int i, j, h, iv, iu;
    
    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3) {
      for (i = h; i < n; i++) {
        iv = indx[i];
        v = x[i];
        iu = indy[i];
        u = y[i];
        j = i;
        while (j >= h && cmp_ints(indx[j - h], iv) > 0) {
          indx[j] = indx[j-h];
          x[j] = x[j-h];
          indy[j] = indy[j-h];
          y[j] = y[j-h];
          j -= h;
        }
        indx[j] = iv;
        x[j] = v;
        indy[j] = iu;
        y[j] = u;
      }
    }
  } // sort_back_univ_weights

/*
Description:
  Sorts time1 by increasing order with delta alongside.
In the subsets of constant time1 observations,
events are put first and censures last.
Parameters:
  time1[inout]	pointer to time1 first element
delta[inout]	pointer to delta first element
index[inout]	pointer to index first element
t1 [in]			time1 value (defines last index)
len[in]			length of time1, delta and index
Return value:
  Returns last index.
Remarks:
  Vectors time1, delta and index must have the same length.
*/
  
  static int sort_univ_surv_index(
    double *const time1,
    int *const delta,
    int *const index,
    const double t1,
    const int len)
{
    register int i = 0;
    int j, k, end = len/2;
    sort_univ_index(time1, delta, index, len); // sort time1 and delta with index
    if (time1[end] > t1) end = 0;
    for (; end < len; end++) {
      if (time1[end] > t1) break; // determine last index
    }
    while (i < end) { // loop through the sample until last index is reached
                      for (i++, j = 1; i < end && time1[i] == time1[i-1]; i++) { // loop through the sample until time1 changes or last index is reached
                                                                                 j++; // count equal times
                      }
                      if (j > 1) { // if there are equal times
                                   k = i-j;
                                   sort_int_rev_index(&delta[k], &index[k], j); // put censored observations last
                      }
    }
    return end;
  } // sort_univ_surv_index

/*
Description:
  Computes the Kaplan-Meier weights of a given delta vector.
Parameters:
  delta[in]		pointer to delta first element
len[in]			pointer to length of delta
end[in]			pointer to index limit
weights[out]	pointer to weights first element
Return value:
  This function doesn't return a value.
Remarks:
Vector weights has the same length as delta.
*/

void WeightsKaplanMeier(
const int *const delta,
const int *const len,
const int *const end,
double *const weights)
{
register int i;
double aux[2]; // declare auxiliary vector needed for the computation
for (i = 0, aux[0] = 1; i < *end; i++) { // loop through the sample until last index is reached
weights[i] = (double)delta[i]/(*len-i); // compute needed factor
aux[1] = 1-weights[i]; // factor needed for the computation
weights[i] *= aux[0]; // compute and save weight
aux[0] *= aux[1]; // compute and save factor needed for next iteration
}
return;
} // WeightsKaplanMeier

/*
Description:
Calls 'sort_univ_surv_index' to sort time1 and delta
and then calls 'WeightsKaplanMeier'.
Parameters:
time1[inout]	pointer to time1 first element
delta[inout]	pointer to delta first element
len[in]			pointer to length of time1 and delta
t1[in]			pointer to time1 value limit
weights[out]	pointer to weights first element
Return value:
This function doesn't return a value.
Remarks:
  Vectors time1 and delta must have the same length.
Vector weights has the same length as time1 and delta.
*/
  
  void WeightsKaplanMeierSort(
    double *const time1,
    int *const delta,
    const int *const len,
    const double *const t1,
    double *const weights)
{
    int i, index[*len];
    for (i = 0; i < *len; i++) index[i] = i;
    i = sort_univ_surv_index(time1, delta, index, *t1, *len); // sort time1 by increasing order with events first and censures last
    WeightsKaplanMeier(delta, len, &i, weights); // compute Kaplan-Meier weights
    sort_back_univ_weights(index, time1, delta, weights, *len); // put time1, delta and weights back to their previous order
    return;
  } // WeightsKaplanMeierSort
