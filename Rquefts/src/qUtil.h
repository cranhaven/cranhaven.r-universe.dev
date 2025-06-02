/* Author: Robert Hijmans
   Date: April 2016
   
   License: GPL (>=3)
*/

#ifndef QUTIL_H_
#define QUTIL_H_

#include <vector>
#include <algorithm>

inline double approx(std::vector<double> xy, double x) {
	int s = xy.size() / 2;
	std::vector<double> X(xy.begin(), xy.begin() + s - 1);
	std::vector<double> Y(xy.begin()+s, xy.end());
	int n = X.size();
	double y = 0;
	if (x < X[0] ) {
		y = Y[0];
	} else if (x > X[n-1]) {
		y = Y[n-1];
	} else {
		for(int i=1; i<n; i++) {
			if (X[i] > x) {
				double slope = (Y[i] - Y[i-1]) / (X[i] - X[i-1]);
				y = Y[i-1] + (x - X[i-1]) * slope;
				break;
			}
		}
	}
	return(y);
}	



#endif
