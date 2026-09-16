/* Code to calculate hypergeometric intersection probabilities when one urn contains a number of duplicates.
 *
 * Author: Alex T. Kalinka (alex.t.kalinka@gmail.com)
 *
 * Will be loaded as a shared object into R (dyn.load('name.so')) and called from within R. 
 *
 *
*/

#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include <cmath>
#include <vector>
#include <R.h>

extern "C" {

using namespace std;


void inters_distr_dup_opt(int *np, int *ap, int *bp, int *qp, double *dist, int *range, int *rlen, bool *verbose)
	{

	int v, n = *np, a = *ap, b = *bp, q = *qp, i, m, l, j, nab, mab, mmax, mmin, lmax, lmin;
	// Values that don't need to be inside the innermost loop.
	double A,B,C,D,E,F,G,H,I,J,K,M,N,P;
	double qs;
	float prog;
	vector<int> vrange;
	vector<double> lut (1,0.0);

	copy(range, range + *rlen, back_inserter(vrange));

	// Build look-up table for all lgamma calls we will need in our calculation.
	for(i = 1; i <= (n+q+1); i++){
		lut.push_back( lgamma(i) );
		}

	A = lut.at(n-q+1);
	B = lut.at(q+1);
	C = lut.at(n+1);
	D = lut.at(n-a+1);
	E = lut.at(a+1);
	F = lut.at(n+q+1);
	G = lut.at(n+q-b+1);
	H = lut.at(b+1);

	// Loop through all possible intersection sizes and calculate probabilities for each size.
	for(i = 0; i < vrange.size(); i++){

		if(*verbose){
			prog = ((i+0.0)/(vrange.size()-1))*100;

			Rprintf("\r   Calculating probabilities... %3.3f%%",prog);

			R_FlushConsole();
			R_ProcessEvents();
			}

		v = vrange.at(i);

		qs = 0;
		mmin = max((a-v-n+q),0);
		mmax = min((a-v),q);
		I = lut.at(b-v+1);

		for(m = mmin; m <= mmax; m++){

			lmin = max((a-n+q-m),0);
			lmax = min(v,(q-m));
			J = lut.at(m+1);
			K = lut.at(a-v-m+1);

			for(l = lmin; l <= lmax; l++){

				M = lut.at(v-l+1);
				N = lut.at(q-l-m+1);
				P = lut.at(n-q+l-a+m+1);

				for(j = 0; j <= l; j++){

					if( !((n+q-a-m-j-b+v+1) <= 0) ){

						qs = qs + exp( (A - M + B - lut.at(l-j+1) - lut.at(j+1) - N - J - P - K + lut.at(n+q-a-m-j+1) - lut.at(n+q-a-m-j-b+v+1) - I - C + D + E - F + G + H) );
						}
					
					}
				}
			}

		dist[i] = qs;
		}

	}


	}




