/*
	Monotone quadratic spline Schumaker's algorithm.
	
	This library implements Schumaker's algorithm to construct monotone quadratic interpolating spline.

	The main functions are:
	BuildMonotonSpline  
	MonotoneSplineValue
		
	The user needs to supply the vectors of inputs x,y and the length of these vectors n.
	The vectors of coefficients alpha, beta, gamma, h will be calculated and need to be of size 2n
	temp is working memory of size  3n.
	
	The details of this methods can be found in:
	
	L. L. Schumaker On Shape Preserving Quadratic Spline Interpolation
	SIAM Journal on Numerical Analysis, Vol. 20, No. 4 (Aug., 1983), pp. 854-864
	http://www.business.uzh.ch/professorships/qba/teaching/operationsResearch/lectures/CEF/SchumakerShapePreservingSplineSIAMJNA1983.pdf

	
	G. Beliakov and S. Liu. Parallel monotone spline interpolation and approximation
    on GPUs. In Designing scientific applications on GPUs, pages 295–310. CRC
    Press, Boca Raton, Flo., 2014. 


	
	This program is distributed under LGPL-3 Licence without any charge. 
	
	Copyright Gleb Beliakov, 2017
	gleb@deakin.edu.au
*/

template<typename Tx, typename Ty>
void CalculateBeta(Tx *u, Ty *v, double *b, int NN)
{
	int i=0;
	while(i<=(NN-2))
	{
		b[i]=double(v[i+1] - v[i])/double(u[i+1] - u[i]);
		i++;
	};

}

void CalculateDeltaGeneral( double *b, double *c, int NN)
{
	int i=1;
	while(i<=(NN-2))
	{
		if((b[i-1]*b[i])<=0)
			c[i]=0;
		else
			c[i]=(2*b[i-1]*b[i])/(b[i-1]+b[i]);
		i++;
	}
}


void CalculateDeltaFirst( double *b, double *c, int NN)
{
	if((b[0]*(2*b[0]-c[1]))<=0)
		c[0]=0;
	else
		c[0]=2*b[0] - c[1];
}
void CalculateDeltaLast( double *b, double *c, int NN)
{
	if((b[NN-2]*(2*b[NN-2]-c[NN-2]))<=0)
		c[NN-1]=0;
	else
		c[NN-1]=2*b[NN-2] - c[NN-2];
}

template<typename Tx, typename Ty>
void CalculateCoefficientsKnots( Tx *u, Ty *v, double *b, double *c,  double *h,  double *alpha, double *beta, double *gamma, int NN, int &T)
{
	int i=0,s=0;
	while(i<=(NN-2))
	{
		//decide whether an additional knot is necessary
		if(fabs(c[i]+c[i+1]- 2*b[i])<=0.1e-5)
		{
			//no additional knot 
			h[s]=u[i];
			alpha[s]=v[i];
			beta[s]=c[i];
			gamma[s]=(c[i+1]-c[i])/(2*(u[i+1]-u[i]));
			s++;
		}
		else
		{
			//adding a knot
			h[s]=u[i];
			//determine the position of the knot
			if((c[i+1] - b[i])*(c[i] - b[i])<0)
				h[s+1]=u[i+1] + (c[i] - b[i])*(u[i+1]-u[i])/(c[i+1] - c[i]);
			else
				h[s+1]=0.5*(u[i+1] + u[i]);

			//calculate coefficients
			double dtemp = (2*b[i] - c[i+1]) + ((c[i+1] - c[i])*(h[s+1] - u[i]))/
												(u[i+1] - u[i]);
			alpha[s]=v[i];
			beta[s]=c[i];
			gamma[s]=(dtemp - c[i])/(2*(h[s+1] - u[i]));
			alpha[s+1]=v[i] + c[i]*(h[s+1] - u[i]) + (dtemp - c[i])*(h[s+1] - u[i])/2;
			gamma[s+1]=(c[i+1] - dtemp)/(2*(u[i+1] - h[s+1]));
			beta[s+1]=dtemp;

			s++;
			s++;
		}
		i++;
	}
	h[s]=u[NN-1];
	T=s+1;
}


/**
* BuildMonotonSpline - Build the monotone quadratic spline
* @param xx           An array of size NN of abscissae 
* @param yy           An array of size NN of dependent variables
* @param NN           The size of he data for spline
* @param h         	  knots of the spline  size 2N
* @param alpha         	 spline coefficients
* @param beta         	 spline coefficients
* @param gamma         	 spline coefficients
* @param temp			 working array of size 3NN
*/		
template<typename Tx, typename Ty>		
int BuildMonotonSpline(Tx *xx, Ty *yy, int NN, // outputs
	double *h, double *alpha, double *beta, double *gamma, double * temp
)
{
	int T;
	Tx *u; Ty *v;
    double *c,*b; // temp variables
	b=&temp[0];
	c=&temp[1*NN];
	u=xx, v=yy;
 
	CalculateBeta(u,v,b,NN);
	CalculateDeltaGeneral(b,c,NN);
	CalculateDeltaFirst(b,c,NN);
	CalculateDeltaLast(b,c,NN);
	CalculateCoefficientsKnots(u,v,b,c,h,alpha,beta,gamma,NN,T);
	return T;
}

// Procedure to find the number of the interval where the point u is situated.
// Parameters: u - the point of interest, x the vector of knots, ileft,iright - the boundaries, l - the 
// solution.
void Bisection(double u, double *x, int ileft, int iright,int *l)
{
 int itemp,mid,temp;
 temp=ileft; itemp=iright;
 while ((itemp-temp)>1 ) {
     mid = (int) floor((temp+itemp) /2);
     if (u<x[mid-1]) itemp=mid; else temp=mid;
 }
 *l=temp;
}


/**
* MonotoneSplineValue - Calculate the value of the monotone quadratic spline at t
* @param t           the point where spline value is needed
* @param h         	  knots of the spline  size 2N
* @param alpha         	 spline coefficients
* @param beta         	 spline coefficients
* @param gamma         	 spline coefficients
* @param T			 the size of the above arrays
*                    all inputs but t are computed in  BuildMonotonSpline
*/
double MonotoneSplineValue(double t, double* h, double * alpha, double * beta, double * gamma, int T)
{
	double value;
	int i;
	Bisection(t,h,1,T,&i); i--;
	if(i<0)i=0;
	value=(t-h[i]);
	value=alpha[i] + value*(beta[i] + gamma[i]*value);
	return value;
};

/*
 example
 
   double x[4]={0.3,0.4,0.8,0.9};
   double y[4]={0.0,0.4,0.8,1.0};
   
   double h[8],alpha[8], beta[8],gamma[8];
   double temp[24];
   
   int T= BuildMonotonSpline(x,y,4,h,alpha,beta,gamma,temp);
   
   double t=0.5;
   double result=MonotoneSplineValue(t,h,alpha,beta,gamma,T);
   
   */
