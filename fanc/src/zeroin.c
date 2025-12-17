#define USE_FC_LEN_T
#include <float.h>

double dabs(double a) {
	return a > 0 ? a : -a;
}

double zeroin(
	double a, double b, double (*f)(double x, void *ex), double tol, 
	void *ex) 
{
	double c, d, e, eps, fa, fb, fc, tol1, xm, p, q, r, s;
	eps = DBL_EPSILON;
	tol1 = eps + 1;

	fa = f(a, ex);
	fb = f(b, ex);
	if (fa == 0 || fb == 0) goto l20;
	if (fa * (fb / dabs(fb)) <= 0)  goto l20;
	return 0;
l20:
	c = a;
	fc = fa;
	d = b - a;
	e = d;
l30:
	if (dabs(fc) < dabs(fb)) goto l40;
	a = b;
	b = c;
	c = a;
	fa = fb;
	fb = fc;
	fc = fa;
l40:
	tol1 = 2 * eps * dabs(b) + 0.5 * tol;
	xm = 0.5 * (c - b);
	if (dabs(xm) <= tol1 || fb == 0) goto l150;
	if (dabs(e) >= tol1 && dabs(fa) > dabs(fb)) goto l50;
	d = xm;
	e = d;
	goto l110;
l50:
	s = fb / fa;
	if (a != c) goto l60;
	p = 2 * xm * s;
	q = 1 - s;
	goto l70;
l60:
	q = fa / fc;
	r = fb / fc;
	p = s * (2 * xm * q * (q - r) - (b - a) * (r - 1));
	q = (q - 1) * (r - 1) * (s - 1);
l70:
	if (p <= 0) goto l80;
	q = -q;
	goto l90;
l80:
	p = -p;
l90:
	s = e;
	e = d;
	if (2 * p >= 3 * xm * q - dabs(tol1 * q) || p >= dabs(0.5 * s * q))
		goto l100;
	d = p / q;
	goto l110;
l100:
	d = xm;
	e = d;
l110:
	a = b;
	fa = fb;
	if (dabs(d) <= tol1) goto l120;
	b = b + d;
	goto l140;
l120:
	if (xm <= 0) goto l130;
	b = b + tol1;
	goto l140;
l130:
	b = b - tol1;
l140:
	fb = f(b, ex);
	if (fb * (fc / dabs(fc)) > 0) goto l20;
	goto l30;
l150:
	return b;
}
