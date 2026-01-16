#include "VCLDefines.h"

// --------------- distance between two partitionings -------------------

/* 
	Function evaluate dot product between two partitionings.
	Partitioning is thought as matrix where:
	
	M[i,j] = 1 when object i and j belongs to the same cluster and i!=j
	M[i,j] = 0 in other case
	
	Dot product is calculated as: 
	(we have two partitionings P nad P' and representing them matricies
	P[i,j] and P'[i,j])	

	<P,P'> = sum(for all i and j) P[i,j]*P'[i,j]

	This dot product satisfy Cauchy-Shwartz inequality <P,P'> <= <P,P>*<P',P'>
	as result we have <P,P'>/sqrt(<P,P>*<P',P'>)
*/

SEXP clv_dotProduct(SEXP cluster1_sxp, SEXP cluster2_sxp)
{
	int *clust_tab1;
	int *clust_tab2;
	int obj_num, protect_num = 0;

	obj_num = length(cluster1_sxp);
	clust_tab1 = INTEGER(cluster1_sxp);
	clust_tab2 = INTEGER(cluster2_sxp);

	int i, j;
	double numerator = 0, denominator1=0, denominator2=0;

	// compute dot products:  <P,P'>, <P,P>, <P',P'>
	for(i=0; i<obj_num; i++)
		for(j=(i+1); j<obj_num; j++)
		{
			if(clust_tab1[i] == clust_tab1[j]) denominator1 += 1 ;
			if(clust_tab2[i] == clust_tab2[j]) denominator2 += 1;
			if(clust_tab1[i] == clust_tab1[j] && clust_tab2[i] == clust_tab2[j]) numerator += 1;
		}

	SEXP result;
	PROTECT(result = allocVector(REALSXP, 1));
	protect_num++;
	REAL(result)[0] = numerator/sqrt(denominator1*denominator2);
	UNPROTECT(protect_num);
	return result;
}
