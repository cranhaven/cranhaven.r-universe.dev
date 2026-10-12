/* functions1.c 
 * Part of the GMCPIC package.
 *
 * Copyright (C) 2013-2014  Samuel Soubeyrand <samuel.soubeyrand@inra.fr>
 *                          Vincent Garreta
 *                          Jean-Francois Rey <jean-francois.rey@inra.fr>
 *                          INRA - BioSP Site Agroparc - 84914 Avignon Cedex 9 - France
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 */

/*! \file functions1.c
 *  \breef Implement Monte Carlo estimation of p-value.
 *  \author Samuel Soubeyrand <Samuel.Soubeyrand@avignon.inra.fr>
 *  \author Vincent Garreta <Vincent.Garreta@paca.inra.fr>
 *  \date 06/002/2014
 *  \version 1.0.0
 *  \copyright GPL (>= 2)
 */

#include <R.h>
#include <Rmath.h>

/*! \brief Multinomial probability function
 *  \param d : table size
 *  \param p : vectors of probabilities (true proportions of variants)
 *  \param N : vector of observed frequencies
 *  \return a double
 */
double dmultinom(int d, double *p, int *N){
	
	int i;
	int nsample = 0;
	int coherent = 1;
	double res;
	
	for (i = 0; i < d; i++){
		nsample += N[i];
		if((p[i]==0.0)&(N[i]>0))
			coherent = 0;
	}
	
	res = lgammafn(((double) nsample) + 1.0);
	
	for (i = 0; i < d; i++)
		if(p[i]>0.0)
			res += ((double) N[i])*log(p[i]) - lgammafn(((double) N[i]) + 1.0);
	
	if(coherent==1){
		return exp(res);
	}else{
		return 0.0;
	}
}


/*! \brief Monte Carlo estimation of the p-value of the GMCPIC test for a fixed value of the calibration weight.
* \param d : d[0] number of variants (equal to the lengths of vectors p, N1 and N2)
* \param p : vectors of probabilities (true proportions of variants)
* \param N1: vector of observed frequencies for population 1 
* \param N2: vector of observed frequencies for population 2
* \param B : B[0] number of Monte Carlo simulations to compute the p-value
* \param resres : *resres p-value
* \return nothings but p_value is return in resres variable.
*/
void C_test_p_value(int *d, double *p, int *N1, int *N2, int *B, double *resres)
{
	int ns1=0, ns2=0;
	int i;
	int N1sim[1000], N2sim[1000]; /* maximum of 1000 variants */
	double dN, temp;
	double infM=0.0;
	
	if(d[0]>1000){
		resres[0] = -1;
		Rprintf("[C_test_p_value] :  Vectors p, N1 and N2 too large (maximum of 1000 variants) \n");
		
	}else{

		for (i = 0; i < d[0]; i++){
			ns1 += N1[i];
			ns2 += N2[i];
		}
		
		dN = dmultinom(d[0], p, N2); /* multinomial probability computed for N2 !!! */
		
		GetRNGstate(); /* to take the seed of the random number generator of R */

		for (i = 0; i < B[0]; i++){
			rmultinom(ns1, p, d[0], &N1sim[0]);
			rmultinom(ns2, p, d[0], &N2sim[0]);
			
			temp = dmultinom(d[0], p, &N2sim[0]);
			infM += (double) (dN >= temp);
		}

		PutRNGstate(); /* to give back the seed of the random number generator of R */

		resres[0] = infM/((double) B[0]);
	}
}

