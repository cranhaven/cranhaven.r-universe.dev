/*******************************************************************************
VstarSfunction

Takes arguments r, x, and EPS:
    nn: The time series size.
    H: A value of the Hurst parameter.

Changes vector y:
    y[0]: Equal to V(S) of eq 16 [1].

References
[1] Hamed KH (2008) Trend detection in hydrologic data: The Mann-Kendall trend
    test under the scaling hypothesis. Journal of Hydrology 349(3-4):350-363.
    doi:10.1016/j.jhydrol.2007.11.009
*******************************************************************************/

#include "trenchR.h"

void VstarSfunction(int *nn,double *H,double *y)
{
	int i, k, j, l, nx = *nn;
	double m1 = 0.0, m2 = 0.0, m3 = 0.0, s = 0.0, Hurst = *H;
	VECTOR y1;

    y1 = Vector(nx);

    for (i = 0; i < nx; i++) {
            y1[i] = 0.5*(pow(abs(i+1),2*Hurst) -2 * pow(abs(i),2*Hurst) +
                         pow(abs(i-1),2*Hurst));
    }
    // y1 is the autocorrelation for the given value of the Hurst parameter (eq 9 [1]).

    // The next equations calculate the double sum in eq 16 [1].

    for (i = 0; i < nx - 1; i++) {
            for (k = 0; k < nx - 1; k++) {
                for (j = i + 1; j < nx; j++) {
                    for (l = k + 1; l < nx; l++) {
                        m1 = y1[abs(j-l)] - y1[abs(i-l)] - y1[abs(j-k)] + y1[abs(i-k)];
                        m2 = 2 * sqrt((1 - y1[j-i]) * (1 - y1[l-k]));
                        m3 = asin(m1/m2);
                        s = s + m3;
    }}}}

	y[0] = (2 * s)/ PI;
	free_vector(y1);
}
