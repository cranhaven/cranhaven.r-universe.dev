#include "mmts.h"

mmts::mmts(void) { //default constructor
	ngroups=1;
	data = new mts[1];
}

mmts::mmts(const int ng) { //series constructor
	ngroups=ng;
	data = new mts[ngroups];
}

mmts::~mmts() { //destructor
	delete [] data;
}

//reset to new dimensions
void mmts::reset(const int ng) {
	delete [] data;
	ngroups=ng;
	data = new mts[ngroups];
}

//access
matrix mmts::operator()(const int ng, const int nr, const int tp) {
	return(data[ng-1](nr,tp));
}

void mmts::summary(void) {
	Rprintf("Nr groups: %d \n", ngroups); 
}

void mmts::print(void) {
	Rprintf("Nr groups: %d \n", ngroups); 
	for(int ng=0; ng<ngroups; ng++) data[ng].print();
}

