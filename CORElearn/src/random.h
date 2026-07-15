/*
 * random.h
 *
 *  Created on: 12.3.2010
 *      Author: rmarko
 */

#if !defined(RANDOM_H_)
#define RANDOM_H_

#include <cstdlib>
#include <ctime>

#include "contain.h"

class PseudoRandom {
private:
	// internal state of generator mrg32k5a from L'Ecuyer99
	double s10, s11, s12, s13, s14, s20, s21, s22, s23, s24;

	void mrg32k5aSetSeed(int n, int *seed);
	void mrg32k5aAddSeed(int n, int *seed);
	double MRG32k5a();

public:
	// constructors
	PseudoRandom() {
		initSeed((int) -time(NULL));
	}
	PseudoRandom(int seed) {
		initSeed(seed);
	}
	PseudoRandom(int n, int *seed) {
		initSeed(n, seed);
	}

	// interface
	inline void initSeed(int seed) {
		mrg32k5aSetSeed(1, &seed);
	}
	inline void initSeed(int n, int *seed) {
		mrg32k5aSetSeed(n, seed);
	}
	inline void addSeed(int n, int *seed) {
		mrg32k5aAddSeed(n, seed);
	}
	inline double getDouble() {
		return MRG32k5a();
	}
	int getBetween(int from, int to);
	double getBetween(double from, double to);


};

class PseudoRandomStreams {
private:
	marray<PseudoRandom> rng ;
public:
	PseudoRandomStreams() { }
	~PseudoRandomStreams() { destroy() ; }
	void initSeed(int m, int n, int *seed) {
		int idx ;
		rng.create(m) ;
		for (idx=0; idx<m; idx++) {
			rng[idx].initSeed(1, &idx) ;
			rng[idx].addSeed(n, seed) ;
		}
	}
	double getDouble(int idx) {
		return rng[idx].getDouble() ;
	}
	int getBetween(int idx, int from, int to) {
		return rng[idx].getBetween(from, to) ;
	}
	double getBetween(int idx, double from, double to) {
		return rng[idx].getBetween(from, to) ;
	}
	void destroy() { rng.destroy() ; }

};

#endif /* RANDOM_H_ */
