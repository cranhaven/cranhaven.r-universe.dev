/*******************************************************************************
score0

Takes arguments r, x, and EPS:
    nn: The time series size.
    y: The data.

Changes vector s0:
    s0: Equal to the elements with index ij of eq 17 [1].

References
[1] Hamed KH (2008) Trend detection in hydrologic data: The Mann-Kendall trend
    test under the scaling hypothesis. Journal of Hydrology 349(3-4):350-363.
    doi:10.1016/j.jhydrol.2007.11.009
*******************************************************************************/

#include "trenchR.h"

void score0(int *nn,double *y,double *s0)
{
	int i, j, k = 0, nx = *nn;

	for (i = 0; i < nx - 1; i++) {
            for (j = i + 1; j < nx; j++) {
                s0[k] = (y[j]-y[i])/(j-i);
                k = k+1;
    }}
}
