#include <stdio.h>
#include <math.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

void stdf(int *col1, int *col2, int *n, int *k, double *result)
{
    int i;
    double sum, temp1, temp2;

    sum = 0;
    for( i = 0; i < *n; i++ ){
        temp1 = (*n + 0.5 - col1[i])/(*k);
        temp2 = (*n + 0.5 - col2[i])/(*k);
        if (temp1 < 1) {
          if (temp2 < 1) {
            sum += ((1 - temp1) + (1 - temp2) - (1 - temp1)*(1 - temp2));
          } else {
            sum += (1 - temp1);
          }
        } else if (temp2 < 1) {
          sum += (1 - temp2);
        }
    }
    *result = (sum/(*k));
}

void I3smith2(double *x, double *a, double *result)
{
    double lxa;

    lxa = log(*x)/(*a);
    *result = ((pnorm((((*a))/2 + lxa),0,1,1,0)) + ((*x)*exp((*a)*(*a)))*pnorm(((-1.5)*(*a) -
                                                                                         lxa),0,1,1,0));
}

void ellsmith2(double *t, double *a, double *result)
{
    double ahalf, delta;

    ahalf = ((*a)/2);
    delta = (log(t[0]/t[1]) / (*a));

    *result = (t[0]*pnorm((ahalf + delta),0,1,1,0) +
               t[1]*pnorm((ahalf - delta),0,1,1,0));
}

void ecGeneral(int *ranks, int *d, double *x, int *n, int *k, double *result)
{
    int i, j, count;
    double sum;

    sum = 0;
    for( i=0; i < (*n); i++){
        count = 0;
        for( j = i; j < ((*d - 1)*(*n) + i + 1); j = j + (*n)){
            if(ranks[j] >= (*n + 0.5 - (*k)*x[count])){
                sum += 1;
                break;
            }
            count = count + 1;
        }
    }
    *result = (sum/(*k));
}


void tildeLGeneral(int *ranks, int *d, double *k, double *tau, double *x, double *n, double *result)
{
    double sumI, sumO, temp1, temp2;
    int i, j, l, count;
    sumO = 0;

    for(i = 0; i < *k; i++){
        sumI = 0;
        temp1 = pow((i+1)/(*k + 1), *tau)*(*tau + 1);
        for( l=0; l < (*n); l++){
            count = 0;
            for( j = l; j < ((*d - 1)*(*n) + l + 1); j = j + (*n)){
                if(ranks[j] >= (*n + 1 - (*k)*x[count]*((i+1)/(*k + 1)))){
                    sumI += 1;
                    break;
                }
                count = count + 1;
            }
        }
        temp2 = (sumI/(*k))*((*k + 1)/(i+1));
        sumO += temp1*temp2;
    }
    *result = sumO/(*k);
}




void alphaGeneral(int *ranks, int*d, double *k, double *n, double *x, double *rho, double *result)
{
    double temp1, temp2, sumL, sumI, sumO1, sumO2;
    int i, j, l, count;
    sumO1 = 0;
    sumO2 = 0;

    for(i = 0; i < *k; i++){
        sumL = 0;
        sumI = 0;
         for( l=0; l < (*n); l++){
            count = 0;
            for( j = l; j < ((*d - 1)*(*n) + l + 1); j = j + (*n)){
                 if(ranks[j] >= (*n + 1 - (*k)*x[count]*((i+1)/(*k + 1)))){
                    sumL += 1;
                    break;
                }
                count = count + 1;
            }
        }
        temp1 = (sumL/(*k))*((*k + 1)/(i+1));
        temp2 = pow((i+1)/(*k + 1), *rho);
        for(j = 0; j < *k; j++){
            sumI += pow((i+1)/(*k + 1),*rho) - pow((j+1)/(*k + 1), *rho);
        }
        sumO1 += temp1*sumI;
        sumO2 += temp2*sumI;
    }
    *result = sumO1/sumO2;
}
