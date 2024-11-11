
/*********************************
*                                *
*       Author: Ingmar Visser    *
*                                *
*       Created: 19 may 2004     *
*       Modified: 19 may 2004    *
*                                *
*********************************/

#ifndef MMTS
#define MMTS 1

/********************************************************************
*                                                             		*
*   Class definition for multigroup multivariate mixed timeseries. 	*
*                                                             		*
********************************************************************/

#include <stdio.h>
#include <stdlib.h>

/* extern "C" { */
#include <R.h>
/* } */

#include "matrix.h"
#include "mts.h"
	
class mmts {
	
	//data members
	private:
		int ngroups;
		
	public:
		mts *data;
		
	public:
	//member methods: constructors
		mmts(void);
		mmts(const int ngroups);		
		
		~mmts();
		
	//reset to new dimensions
		void reset(const int ngr);
		
	//access
		inline int getNGroups(void) {return(ngroups);}
		inline int getIndReal(const int ng) {return(data[ng].getIndReal());}
		inline int getLength(const int ng, const int nr) {return(data[ng].getLength(nr));}
		inline double getWeight(const int ng, const int nr) {return(data[ng].getWeight(nr));}
		
		matrix operator()(const int ng, const int nr, const int tp);
		
	//print to (R) screen
		void summary(void);
		void print(void);
		
};

#endif
