
/*********************************
*                                *
*       Author: Ingmar Visser    *
*                                *
*       Created: 1 april 2004    *
*       Modified: 1 april 2004   *
*                                *
*********************************/

#ifndef MTS
#define MTS 1

/**************************************************************
*                                                             *
*       Class definition for multivariate mixed timeseries.   *
 		By the way: all values are represented as doubles
 		even when the values are really categorical
 		Conversion to ints is done as needed on the fly 
 		in the calling routines, ie the loglikelihood function
*                                                             *
**************************************************************/

#include <stdio.h>
#include <stdlib.h>

/* extern "C" { */
#include <R.h>
/* } */

#include "matrix.h"

#define MTSBOUNDS 1

class mts {
	
	//data members
	private:
		int vars; //for a vars-variate timeseries
		int *modes; // 1=continuous, 2 and up for dichotomous etc data
		int indReal;
		int *lengths;
		double *weights;
		int xm;
		
		matrix **elements; //mts is an array of 2D arrays of values, ie a series of matrices
		
	//member methods: constructors
	public: 
		mts(void);
		mts(const int vars, int *modes, const int indReal, int *lengths, double *weights, int xm);
		
	private:
		//used by the constructors/reset, internal use only
		void initialize(const int vars, int *modes, const int indReal, int *lengths, double *weights, int xm); // called by constructors
		
	public:
		~mts();
		
	public: 
	//reset to new dimensions
		void reset(const int vars, int *modes, const int indReal, int *lengths, double *weights, int xm);
		void mtsdata(double *data, const int vars, int *modes, const int indReal, int *lengths, double *weights, int xm); //reset from data
		
	//assigment
	
	//access functions
		double operator()(const int ir, int tp, const int vr); //general access
		matrix& operator()(const int ir, int tp); //access all vars at one occasion tp
		double operator()(const int tp); //acces for indReal=1 and var=1
		inline int getVars(void) {return(vars);}
		inline int getIndReal(void) {return(indReal);}
		int getLength(const int ir);
		double getWeight(const int ir);
		int getMode(const int vr);	
	
	//print to (R) screen
		void summary(void);
		void print(void);
		
};

#endif
