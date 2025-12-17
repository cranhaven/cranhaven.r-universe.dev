#ifndef ZEROIN_H
#define ZEROIN_H

double zeroin(
	double a, double b, double (*f)(double x, void *ex), double tol, 
	void *ex);

#endif // ZEROIN_H
