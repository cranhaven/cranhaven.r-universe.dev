/*******************************************************************************
score

Takes arguments r, x, and EPS:
    nn: The time series size.
    y: The data.

Changes the following vectors:
    S1[0]: Equal to S of eq 1 [1].
    S1[1]: Equal to sum(t[j] * (t[j]-1) * (2t[j]+5)) of eq 5 [1].
    S1[2]: Equal to sum(t[j] * (t[j]-1)) of eq 23.3.4 [2].

References
[1] Hamed KH (2008) Trend detection in hydrologic data: The Mann-Kendall trend
    test under the scaling hypothesis. Journal of Hydrology 349(3-4):350-363.
    doi:10.1016/j.jhydrol.2007.11.009
[2] Hipel KW, McLeod AI (1994) Time series modelling of water resources and
    environmental systems. Elsevier, Amsterdam
*******************************************************************************/

#include "trenchR.h"

void score(int *nn,double *y,int *S1)
{
    int i1, i2, j1, j2 = 0, k = 0, l1 = 0, l2, l3, l4, l5, nx = *nn, ny1 = nx*(nx-1)/2;
    int y1[ny1], mzeroplace[nx-1];

    for (i1 = 0; i1 < nx - 1; i1++) {
            for (j1 = i1 + 1; j1 < nx; j1++) {
                y1[k] = sign(y[j1]-y[i1]);
                k = k+1;
                }
                }

                for (i2 = 0; i2 < nx-1 ; i2++) {
                        if (y[i2+1] == y[i2]) {
                            mzeroplace[l1] = i2 + 2; // index of equal values
                l1 = l1 + 1;
                }
                }

                if (l1 == 0) {
                        S1[1] = 0;
                        S1[2] = 0;
                }
                else if (l1 == 1) {
                        S1[1] = 18;
                        S1[2] = 2;
                }
                else {
                        int differ[l1], t[l1];
                differ[0] = mzeroplace[0] - 1;

                for (l3 = 0; l3 < l1 ; l3++) {
                        t[l3] = 2;
                }

                for (l2 = 1; l2 < l1; l2++) {
                        differ[l2] = mzeroplace[l2] - mzeroplace[l2-1];
                }


                for (l5 = 1; l5 < l1; l5++) {
                        if (differ[l5] == 1) {
                            t[j2] = t[j2] + 1;
                }
                else {
                        j2 = j2 + 1;
                }
                }

                if (j2 == 0) {
                        S1[2] = t[j2]*(t[j2]-1);
                        S1[1] = (S1[2] * (2*t[j2]+5));
                }
                else {
                        int t1[j2+1], t2[j2+1];
                for (l4 = 0; l4 < j2 + 1 ; l4++) {
                        t2[l4] = t[l4]*(t[l4]-1);
                        t1[l4] = t2[l4]*(2*t[l4]+5);
                }
                S1[1] = sumint(j2+1,t1);
                S1[2] = sumint(j2+1,t2);
                }
                }
                S1[0] = sumint(ny1,y1);
                }
