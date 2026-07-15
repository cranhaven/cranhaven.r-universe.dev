/*
 * random.cpp
 *
 *  Created on: 12.3.2010
 *      Author: rmarko
 */

#include "general.h"
#include "error.h"
#include "random.h"

using namespace std ;

// ******** mrg32k5a from L'Ecuyer99  **********
//######################

#define norm 2.3283163396834613e-10
#define m1   4294949027.0
#define m2   4294934327.0
#define a12     1154721.0
#define a14     1739991.0
#define a15n    1108499.0
#define a21     1776413.0
#define a23      865203.0
#define a25n    1641052.0

double PseudoRandom::MRG32k5a() {
	long k;
	double p1, p2;
	/* Component 1 */
	p1 = a12 * s13 - a15n * s10;
	if (p1 > 0.0)
		p1 -= a14 * m1;
	p1 += a14 * s11;
	k = (long) (p1 / m1);
	p1 -= k * m1;
	if (p1 < 0.0)
		p1 += m1;
	s10 = s11;
	s11 = s12;
	s12 = s13;
	s13 = s14;
	s14 = p1;
	/* Component 2 */
	p2 = a21 * s24 - a25n * s20;
	if (p2 > 0.0)
		p2 -= a23 * m2;
	p2 += a23 * s22;
	k = (long) (p2 / m2);
	p2 -= k * m2;
	if (p2 < 0.0)
		p2 += m2;
	s20 = s21;
	s21 = s22;
	s22 = s23;
	s23 = s24;
	s24 = p2;
	/* Combination */
	if (p1 <= p2)
		return ((p1 - p2 + m1) * norm);
	else
		return ((p1 - p2) * norm);
}

void PseudoRandom::mrg32k5aAddSeed(int n, int *seed) {
	int i,j;
	unsigned int l16, h16;
	for (i=0; i < n; i++) {
		l16 = seed[i] & 0x0000ffff;
		h16 = seed[i] >> 16;
		s14 += (double) l16 + 1.0;
		s24 += (double) l16 + 1.0;
		if (s14 >= m1) { s14 -= m1; }
		if (s24 >= m2) { s24 -= m2; }
		for (j=0; j<5; j++) MRG32k5a();
		s14 += (double) h16 + 1.0;
		s24 += (double) h16 + 1.0;
		if (s14 >= m1) { s14 -= m1; }
		if (s24 >= m2) { s24 -= m2; }
		for (j=0; j<5; j++) MRG32k5a();
	}
}

void PseudoRandom::mrg32k5aSetSeed(int n, int *seed) {
	s10 = 12345.0;
	s11 = 12345.0;
	s12 = 12345.0;
	s13 = 12345.0;
	s14 = 12345.0;
	s20 = 12345.0;
	s21 = 12345.0;
	s22 = 12345.0;
	s23 = 12345.0;
	s24 = 12345.0;
	mrg32k5aAddSeed(n, seed);
}

double PseudoRandom::getBetween(double From, double To) {
	return From + getDouble() * (To - From);
}
int PseudoRandom::getBetween(int from, int to) {
	return from + int(getDouble() * (to - from));
}

