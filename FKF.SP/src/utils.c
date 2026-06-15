#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>

/* Macro to transform an index of a 2-dimensional array into an index of a vector */
#define IDX(i, j, dim0) (i) + (j) * (dim0)

/* Print arrays */
void print_array(double *data, int i, int j, const char *lab)
{
    int icnt, jcnt;
    Rprintf("\n'%s':\n", lab);
    for (icnt = 0; icnt < i; icnt++)
    {
        for (jcnt = 0; jcnt < j; jcnt++)
        {
            Rprintf("%3.6f   ", data[IDX(icnt, jcnt, i)]);
        }
        Rprintf("\n");
    }
}

/* Print observation numbers */
void print_int_array(int *data, int i, int j, const char *lab)
{
    int icnt, jcnt;
    Rprintf("\n'%s':\n", lab);
    for (icnt = 0; icnt < i; icnt++)
    {
        for (jcnt = 0; jcnt < j; jcnt++)
        {
            Rprintf("%i   ", data[IDX(icnt, jcnt, i)]);
        }
        Rprintf("\n");
    }
}

/* Locate NA's in observations at time t*/
void locateNA(double *vec, int *NAindices, int *positions, int len)
{
    int j = 0;
    for (int i = 0; i < len; i++)
    {
        if (ISNAN(vec[i]))
            NAindices[i] = 1;
        else
        {
            NAindices[i] = 0;
            positions[j] = i;
            j++;
        }
    }
}

/* Number of NA's in observations at time t*/
int numberofNA(double *vec, int *NAindices, int *positions, int len)
{
    locateNA(vec, NAindices, positions, len);
    int sum = 0;
    for (int i = 0; i < len; i++)
        sum += NAindices[i];
    return sum;
}

/* Temporary reduced arrays when missing obverations are present */
void reduce_array(double *array_full, int dim0, int dim1,
                  double *array_reduced, int *pos, int len)
{
    for (int i = 0; i < len; i++)
    {
        for (int j = 0; j < dim1; j++)
            array_reduced[IDX(i, j, len)] = array_full[IDX(pos[i], j, dim0)];
    }
}
