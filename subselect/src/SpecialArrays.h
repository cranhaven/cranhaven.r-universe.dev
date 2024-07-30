#ifndef ARRAYS
#define ARRAYS

#include <vector> 
#include "Sscma.h"
#include "ErrMReals.h"

using std::vector;

namespace extendedleaps {

const static double INF = std::numeric_limits<double>::infinity();         // Infinity

class symtwodarray   {	//  Symmetric two dimensional array class. Stores a symmetric matrix of error monitered reals in compact form
	public:
		explicit symtwodarray(const vind dim);	//  Constructor
		symtwodarray(const symtwodarray& org);	//  Copy Constructor
		symtwodarray& operator=(const symtwodarray& org);	//  Assignment Operator
		~symtwodarray(void);	//  Destructor 
		real&  operator() (const vind i,const vind j)	//  Subscripting: reading and writing.  Uses (,) sintaxe starting at (0,0)
		{ if (j<=i) return data[i][j];	else return data[j][i]; }
		const real  operator() (const vind i,const vind j) const  //  Subscripting: read only. Uses (,) sintaxe starting at (0,0)
		{ if (j<=i) return data[i][j];	else return data[j][i]; }
	private:
		vind	dimension;  
		vector< vector<real> >	data;
};

template <class I>
void symatpivot(I& rowind,const real& pivotvalue,const symtwodarray& im,symtwodarray& om,
				const vind vp,const vind t,bool& reliable,const double tol);
/* 
  Performs a pivot on a t*t sub-array of the symmetric matrix im, based on the diagonal element pivotvalue
  starting on the rows and columns indexed by vp+1. Uses rowind to map the ordered indices into
  the original matrix elements and stores the results on om. It sets reliable to true when the results are 
  numerically reliable (with the relative error controled by tol) and false otherwise. 
*/

template <class I>
void symatpivot(I& rowind,const real& pivotvalue,const symtwodarray& im,symtwodarray& om,
				const vind vp,const vind t,bool& reliable);
// A version of symatpivot with error checking turned off.

template <class I>
void vectorpivot(I& colind,const std::vector<real>& iv,std::vector<real>& ov,const symtwodarray& im,
				 const real& t1,const vind vp,const vind t,bool& reliable,double tol);
/*
   Performs a pivot on a t sub-array of the vector iv based on the symmetric matrix im with pivot element equal
   to the diagonal position indexd by vp. Starts on the elements indexed by vp+1, uses colind to map the ordered 
   indices into the original vector and matrix elements and stores the results on ov. It sets reliable to true 
   when the results are numerically reliable (with the relative error controled by tol) and false otherwise. 
*/

template <class I>
void vectorpivot(I& colind,const std::vector<real>& iv,std::vector<real>& ov,const symtwodarray& im,
				 const real& t1,const vind vp,const vind t,bool& reliable);
// A version of vectorpivot with error checking turned off.


class matvectarray  {	// One dimensional array whose elements can be stored in local memory OR refer to the row of a symmetric matrix
	public:
		matvectarray(const vind,symtwodarray*,vind const); 
		void setvalue(const vind j,const real val);	// Writing  own data.
		const real  operator[] (const vind j) const;	// Subscripting: read only. Uses [] sintaxe starting at [0]  
		void switchtoowndata(void)     {mat = 0; }
	private:
		vind		dimension;
		symtwodarray*	mat;
		vind		matrowind;
		vector<real>  owndata;	
		matvectarray(const matvectarray&);	//  Forbid copy construction
		matvectarray& operator=(const matvectarray&);	//  Forbid direct assignment
};

template <class I>
void vectorpivot(I& colind,const matvectarray& iv,matvectarray& ov,const symtwodarray& im,
				 const real& t1,const vind vp,const vind t,bool& reliable,const double tol);
 /*
   Performs a pivot on a t sub-array of the vector iv based on column indexed by vp of the symmetric matrix im 
   and the real number t1. Starts on the elements indexed by vp+1, uses colind to map the ordered indices 
   into the original vector and matrix elements and stores the results on ov. It sets reliable to  true when the 
   results are numerically reliable (with the relative error controled by tol) and false otherwise. 
*/

template <class I>
void vectorpivot(I& colind,const matvectarray& iv,matvectarray& ov,const symtwodarray& im,
				 const real& t1,const vind vp,const vind t,bool& reliable);
// A version of vectorpivot with error checking turned off.


template <class I>
void symatpivot(I& rowind,const real& pivotvalue,const symtwodarray& im,symtwodarray& om,
	const vind vp,const vind t,bool& reliable,const double tol)
{
	I colind(rowind);
	vind pivotind=rowind[vp-1];
	real t1,*c;

	reliable = true;
	rowind.reset(vp);
	for (vind i=0;i<t;rowind++,i++)  {
		t1 = im(rowind(),pivotind) / pivotvalue;
		colind.reset(vp);
		for (vind j=0;j<=i;colind++,j++) {
			c = &(om(i,j) = im(rowind(),colind()) - t1 * im(pivotind,colind()));
			if (!errcheck(c,tol)) reliable = false;
		}	
	}

	#ifdef COUNTING 
	fpcnt += t*(t+3)/2 + t*(t+1)/2;
	#endif

	return;
}

template <class I>
void symatpivot(I& rowind,const real& pivotvalue,const symtwodarray& im,symtwodarray& om,
				const vind vp,const vind t,bool& reliable)
{
	symatpivot(rowind,pivotvalue,im,om,vp,t,reliable,INF);
}


template <class I>
void vectorpivot(I& colind,const std::vector<real>& iv,std::vector<real>& ov,const symtwodarray& im,
	 const real& t1,const vind vp,const vind t,bool& reliable,const double tol)
{
	real *c;
	vind pivotind = colind[vp-1];

	reliable = true;
	colind.reset(vp);

	for (vind j=0;j<t;colind++,j++)  {
		c = &(ov[j] = iv[colind()] - t1 * im(pivotind,colind())); 
		if (!errcheck(c,tol)) reliable = false;
	}
	#ifdef COUNTING 
	fpcnt += t + 3*t;
	#endif

	return;
}

template <class I>
void vectorpivot(I& colind,const std::vector<real>& iv,std::vector<real>& ov,const symtwodarray& im,
				 const real& t1,const vind vp,const vind t,bool& reliable)
{
	vectorpivot(colind,iv,ov,im,t1,vp,t,reliable,INF);
}


template <class I>
void vectorpivot(I& colind,const matvectarray& iv,matvectarray& ov,const symtwodarray& im,
	const real& t1,const vind vp,const vind t,bool& reliable,const double tol)
{
	real c;
	vind pivotind = colind[vp-1];

	reliable = true;
	colind.reset(vp);

	for (vind j=0;j<t;colind++,j++)  {
		c = iv[colind()] - t1 * im(pivotind,colind());
		if (!errcheck(&c,tol)) reliable = false;
		else ov.setvalue(j,c);
	}
	#ifdef COUNTING 
	fpcnt += t + 3*t;
	#endif

	return;
}

template <class I>
void vectorpivot(I& colind,const matvectarray& iv,matvectarray& ov,const symtwodarray& im,
				 const real& t1,const vind vp,const vind t,bool& reliable)
{
	 vectorpivot(colind,iv,ov,im,t1,vp,t,reliable,INF);
}


}

#endif

