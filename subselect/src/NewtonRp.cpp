#include <cmath>
#include "NewtonRp.h"

using namespace std;

namespace newtonrp  {

double lsrch(double x0,double (*f)(double),double (*f1)(double),double (*f2)(double),double lb,double ub,double precision)
{
	const double EPS1 = 1E-4;
	double x,x1,y,y1=0.,dy,dy2,cf,e0,e,dlt;

	x = x0;
	e = fabs((y=f(x)));
	do {

		e0 = e;
		dy = f1(x);
		dy2 = dy*dy;
		cf = y*f2(x);
		if (dy2 > -cf + EPS1) dlt = y*dy/(dy2+cf);
		else dlt = y*dy/dy2;
		do {
			x1 = x - dlt;
			if ( x < lb || x > ub || ( e = std::fabs((y1=f(x1))) ) >= e0) dlt /= 2;
		} while (e >= e0);
		x = x1;
		y = y1;

	}  while (e > precision);

	return x;
}

}


