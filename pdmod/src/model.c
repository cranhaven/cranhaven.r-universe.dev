/*
 *  model.c
 *  
 *
 *  Helper functions to calculate the model in C, much faster than R for fitting parameters.
 *
 */

#include <R.h>
#include <math.h>

const double EQUALITY_PRECISION =  1.0e-20;
const double NEAR_INFINITY = 1e100;
const double MIN_PER_DAY = 1440;
const double NO_H = 1;

void compute_next_ewma( double xPrev, double ewmaPrev, double learningRate, 
					    double h, double tau, double deltat, double threshold, double *result );
void compute_weight( double *fastMse, double *slowMse, double *fastWeight, double *slowWeight );

int double_equals( double a, double b )
{
	return ( fabs( a - b ) <= EQUALITY_PRECISION );
}

void compute_model( double *x, double *deltat, int *interference, int *length, 
	double *mFast, double *mSlow, double *n, double *g, double *h, double *tau, double *threshold, 
	double *a, double *b,
	double *est, double *estMse, double *weights, double *forecastMse, double *y, double *forecast )
{ 
	int print = 0;
	
	/* initialize first timestep */
	est[0] = 0, est[*length] = 0;
	estMse[0] = 1, estMse[*length] = 1;
	forecastMse[0] = 1;
	weights[0] = 0.5, weights[*length] = 0.5;
	forecast[0] = 0;
	
	if ( double_equals( *g, 0.0 ) )
	{
		y[0] = ( ISNA( x[0] ) ) ? 0 : 1;
	} else
	{
		y[0] = 0.0001;
	}

	
	double currentReciprocalSum = 0;
	double currentTimeSum = 0;
	double timeSinceReward = deltat[0];  /* more accurately, time since reward, or time since first trial in case of no reward */

	for( int i = 1; i < *length; i++ )
	{
		void R_CheckUserInterrupt(void);
		
		/* update estimates: fast, slow */
		double learningRate = (*mFast) * y[i - 1];
		compute_next_ewma( x[i - 1], est[i - 1], learningRate, NO_H, *tau, deltat[i], *threshold, &est[i] );
		
		learningRate = (*mSlow) * y[i - 1];
		compute_next_ewma( x[i - 1], est[i - 1 + (*length)], learningRate, NO_H, *tau, deltat[i], *threshold, &est[i + (*length)] );
		
		/* update error estimates: fast, slow, forecast */
		double mse = pow( x[i - 1] - est[i - 1], 2 );
		compute_next_ewma( mse, estMse[i - 1], *n, NO_H, *tau, deltat[i],  *threshold, &estMse[i] );
		
		mse = pow( x[i - 1] - est[i - 1 + (*length)], 2 );
		compute_next_ewma( mse, estMse[i - 1 + (*length)], *n, *h, *tau, deltat[i], *threshold, &estMse[i + (*length)] );
		
		mse = pow( x[i - 1] - forecast[i - 1], 2 );
		compute_next_ewma( mse, forecastMse[i - 1], *n, NO_H, *tau, deltat[i], *threshold, &forecastMse[i] );
		
		/* calculate y based on errors up till last time*/
		if ( double_equals( *g, 0.0 ) )
		{
			y[i] = ( ISNA( x[i - 1] ) ) ? 0 : 1;
		}
		else
		{
			double reciprocalError = 1 / forecastMse[i];
			if ( !R_FINITE( reciprocalError ) ) { reciprocalError = NEAR_INFINITY; }
			if ( ISNA( x[i - 1] ) ) { reciprocalError = 0; }
			currentReciprocalSum += reciprocalError;
			
			/* decay reciprocalSum if interference */
			/* note: previously used currentTimeSum, calculated from first trial (i.e. generally first reward),
			 change to use timeSinceReward, so use last reward rather than first 
			currentTimeSum += deltat[i]; */
			
			if ( interference[i] != 0 )
			{
				double f = 1 - (*a) * exp( -(*b) * timeSinceReward / MIN_PER_DAY );
				currentReciprocalSum *= f;
				if ( print != 0 ) Rprintf( " interference: cts = %lf, tsr = %lf, f = %lf \n", timeSinceReward, currentReciprocalSum, f );
			}
			y[i] = currentReciprocalSum / ( currentReciprocalSum + (*g) );
			
			/* update timeSinceReward for next iteration */
			if ( 0 == x[i] )
			{
				timeSinceReward += deltat[i];
			}
			else
			{
				timeSinceReward = deltat[i];
			}
		}
		
		/* calculate weights */
		compute_weight( &estMse[i], &estMse[i +(*length)], &weights[i], &weights[i + (*length)] );
		
		/* calculate forecast */
		forecast[i] = weights[i] * est[i] + weights[i + (*length)] * est[i + (*length)];
		
		if ( print != 0 ) Rprintf( "i = %d, forecast = %lf, est1 = %lf, est2 = %lf, delta = %lf, y = %lf \n", 
							   i, forecast[i], est[i], est[i + (*length)], forecastMse[i], y[i] ); 
		
	}
}


void compute_next_ewma( double xPrev, double ewmaPrev, double learningRate, 
	double h, double tau, double deltat, double threshold, double *result )
{
	if ( ISNA( xPrev ) )
	{
		*result = ewmaPrev;
	}
	else
	{
		*result = ewmaPrev + learningRate * ( xPrev - ewmaPrev );
		
		if ( deltat > threshold )
		{
			*result = pow( h, tau * deltat ) * (*result);
		}
	}
}




void compute_weight( double *fastMse, double *slowMse, double *fastWeight, double *slowWeight )
{
	double fastReciprocal = 1 / *fastMse;
	double slowReciprocal = 1 / *slowMse;
	
	if ( !R_FINITE( fastReciprocal ) ) { fastReciprocal = NEAR_INFINITY; }
	if ( !R_FINITE( slowReciprocal ) ) { slowReciprocal = NEAR_INFINITY; }
	
	*fastWeight = fastReciprocal / ( fastReciprocal + slowReciprocal );
	*slowWeight = slowReciprocal / ( fastReciprocal + slowReciprocal );
	
}

