#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <stdlib.h>
#include <Rmath.h>
#include <R.h>

int tri(int x)
{
    int out;
    out = ((x * x) + x)/2;
    return out;
}

int ceili(double x)
{
    int y;
    y = (int)ceil(x);
    return y;
}

int mycol(int x)
{
    int y;
    y = ceili(sqrt(2*(x+1)+.25)+0.5);
    return y;
}

int myrow(int x)
{
    int y;
    y = ((x+1)-((mycol(x)-2)*(mycol(x)-1)/2));
    return y;
}

int findMin(double *dist, int lb, int ub, int mmn)/* find minimum value over range of array */
{
    double myMin, r;
    int myMn, tt, ind;
    myMin = dist[mmn];
    myMn = mmn;
    tt = 0;
    for(ind = lb; ind<ub; ind++)
    {
        if(dist[ind] <= myMin)
        {
            if(dist[ind]==myMin)
            {
                tt++;
                GetRNGstate();
                r = unif_rand();
                PutRNGstate();
                if(r < pow(tt, -1))
                {
                    myMn = ind;
                }
            }
            else
            {
                myMn = ind;
                tt = 0;
            }
        }
        myMin =  dist[myMn];
    }
    return myMn;
}

int levelTwoCheck(int i, int j, int *l1names)
{
    int result=0;
    if(l1names[mycol(i)-1]==l1names[j-1] || l1names[myrow(i)-1]==l1names[j-1])
    {
        if(mycol(i) != j && myrow(i) != j)
        {
            result = 1;
        }
    }
    return result;
}

int findMin2(double *dist, int nrow, int i)/* identify minimum distance to a particular unit */
{
    unsigned mm, ii, iii, tt=0, t;
    double md, r;
    
    mm = findMin(dist, tri(i-2), tri(i-1), tri(i-2)); /* check column */
    md = dist[mm];
    t = ((i-1)*(i+2))/2;
    for(ii=1; ii<=(nrow-i); ii++)
    {
        if(dist[t]<=md)
        {
            if(dist[t]==md)
            {
                tt++;
                GetRNGstate();
                r = unif_rand();
                PutRNGstate();
                if(r < 1/tt)
                {
                    mm = t;
                }
            }
            else
            {
                mm=t;
                tt=0;
            }
        }
        md = dist[mm];
        t = t + i + ii-1;
    }
    return mm;
}


double * eliminate(int i, double *vec, int nrow)
{
    unsigned ii, t;
    for(ii=tri(i-2); ii < tri(i-1); ii++)
    {
        vec[ii] = HUGE_VAL;
    }
    t = ((i-1)*(i+2))/2;
    for(ii=1; ii<=(nrow-i); ii++)
    {
        vec[t] = HUGE_VAL;
        t = t + i + ii-1;
    }
    return vec;
}

int cmpfunc (const void * a, const void * b)
{
    return ( *(int*)b - *(int*)a );
}


int getElem(int x, int y)
{
    int out;
    out = tri(y-2) + x - 1;
    return out;
}

double maxDist(double *dist, unsigned int *matches, int num)
{
    unsigned i, j;
    double mymax=0;
    for(i=0; i<num;i++)
    {
        for(j=0; j<i;j++)
        {
            if(matches[i] > matches[j])
            {
                if(dist[getElem(matches[j], matches[i])]>mymax && dist[getElem(matches[j], matches[i])] < HUGE_VAL)
                {
                    mymax = dist[getElem(matches[j], matches[i])];
                }
            }
            else
            {
                if(dist[getElem(matches[i], matches[j])]>mymax && dist[getElem(matches[i], matches[j])] < HUGE_VAL)
                {
                    mymax = dist[getElem(matches[i], matches[j])];
                }
            }
        }
    }
    return mymax;
}

double * eliminatePairDist(int i, int j, double *vec)
{
    if(i > j)
    {
        vec[getElem(j, i)] = HUGE_VAL;
    }
    else
    {
        vec[getElem(i,j)] = HUGE_VAL;
    }
    return vec;
}
