#include "VCLDefines.h"

/*
	
*/

SEXP whithinClusterScatterMatrix(SEXP data_sxp, SEXP cluster_sxp, SEXP cluster_center_sxp)
{
	int i,j,k, pos;
	int obj_num, clust_num, dim_num; 
	int protect_num = 0;
	
	obj_num = INTEGER( getAttrib( data_sxp, R_DimSymbol ) )[0];
	clust_num = INTEGER( getAttrib( cluster_center_sxp, R_DimSymbol ) )[0];
	dim_num = INTEGER( getAttrib( cluster_center_sxp, R_DimSymbol ) )[1];

	double *data, *cluster_center;
	int *cluster_tab;
	data = REAL( data_sxp );
	cluster_center = REAL( cluster_center_sxp );
	cluster_tab = INTEGER( cluster_sxp );

	SEXP wcls_matrix_sxp;
	PROTECT( wcls_matrix_sxp = allocMatrix(REALSXP, dim_num, dim_num) );
	protect_num++;
	double *wcls_matrix = REAL( wcls_matrix_sxp );

	// initilize matrix, set each position to 0	
	int dim_num_2 = dim_num*dim_num;
	for(i=0; i<dim_num_2; i++) wcls_matrix[i] = 0.0;

	// main loop - find whithin-cluster scatter matrix
	int cluster_num;
	for(i=0; i<obj_num; i++)
	{
		for(j=0; j<dim_num; j++)
			for(k=0; k<dim_num; k++)
			{
				wcls_matrix[j + k*dim_num] += ( data[ i + j*obj_num ] - cluster_center[ cluster_tab[i]-1 + j*clust_num ] )*
											  ( data[ i + k*obj_num ] - cluster_center[ cluster_tab[i]-1 + k*clust_num ] );
			}
	}

	UNPROTECT(protect_num);
	return wcls_matrix_sxp;
}

/*
	
*/

SEXP betweenClusterScatterMatrix( SEXP cluster_center_sxp, SEXP cluster_size_sxp, SEXP mean_sxp )
{
	int i,j,k, pos;
	int clust_num, dim_num; 
	int protect_num = 0;

	clust_num = INTEGER( getAttrib( cluster_center_sxp, R_DimSymbol ) )[0];
	dim_num = INTEGER( getAttrib( cluster_center_sxp, R_DimSymbol ) )[1];

	double *cluster_center, *mean;
	cluster_center = REAL( cluster_center_sxp );
	mean = REAL( mean_sxp );
	
	int *cluster_size;
	cluster_size = INTEGER( cluster_size_sxp );

	SEXP bcls_matrix_sxp;
	PROTECT( bcls_matrix_sxp = allocMatrix(REALSXP, dim_num, dim_num) );
	protect_num++;
	double *bcls_matrix = REAL( bcls_matrix_sxp );

	// initilize matrix, set each position to 0	
	int dim_num_2 = dim_num*dim_num;
	for(i=0; i<dim_num_2; i++) bcls_matrix[i] = 0.0;

	// main loop - find whithin-cluster scatter matrix
	int cluster_num;
	for(i=0; i<clust_num; i++)
	{
		for(j=0; j<dim_num; j++)
			for(k=0; k<dim_num; k++)
			{
				bcls_matrix[j + k*dim_num] += ( cluster_center[ i + j*clust_num ] - mean[ j ] )*
											  ( cluster_center[ i + k*clust_num ] - mean[ k ] )*
											  cluster_size[i];
			}
	}

	UNPROTECT(protect_num);
	return bcls_matrix_sxp;
}
