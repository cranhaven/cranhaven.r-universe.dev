#include "VCLDefines.h"

/*
	
*/

SEXP connectivity
(
	const SEXP data_sxp,				// data matrix  
	const SEXP clusters_sxp,			// vector with information about clusters (object num. -> cluster)
	const SEXP neighbour_num_sxp,		// number representing choosen metric
	const SEXP choosen_metric_sxp		// number representing choosen metric
)
{
	// additional variables esecially needed in loops and in functions as parameters
	int i, j, obj_num, dim_num, clust_num, protect_num;
	// define distance between choosen objects
	double dist;

	protect_num = 0;

	// "connectivity" index - result of our function
	SEXP connectivity_sxp;
	double *connectivity;

	PROTECT( connectivity_sxp = allocVector(REALSXP, 1) );
	protect_num++;

	connectivity = REAL(connectivity_sxp);
	connectivity[0] = 0.0;

	// table - which object belongs to which cluster
	int *cluster_tab = INTEGER(clusters_sxp);
	
	// get information about data matrix
	SEXP dim = NILSXP;
	PROTECT( dim = getAttrib(data_sxp, R_DimSymbol) );
	protect_num++;

	obj_num = INTEGER(dim)[0];
	dim_num = INTEGER(dim)[1];

	// additional variable 
	double *mean = NULL;
	pMetricFunct metric;

	metric = getMetricFunct(INTEGER(choosen_metric_sxp)[0]);
	if(INTEGER(choosen_metric_sxp)[0] == CORRELATION )
	{
		SEXP mean_sxp;
		PROTECT( mean_sxp = clv_mean(data_sxp, obj_num, dim_num) );
		protect_num++;
		mean = REAL(mean_sxp);
	}

	//extra variables used to compute connectivity index
	int neighbour_num = INTEGER(neighbour_num_sxp)[0];

	// tables needed to store information about nearest neighbours
	SEXP min_tab_sxp, obj_pos_sxp; 
	double *min_tab;
	int *obj_pos;
	PROTECT( min_tab_sxp = allocVector( REALSXP, neighbour_num ) );
	protect_num++;
	PROTECT( obj_pos_sxp = allocVector( INTSXP, neighbour_num ) );
	protect_num++;
	min_tab = REAL(min_tab_sxp);
	obj_pos = INTEGER(obj_pos_sxp);

	int k, prev_obj, tmp;
	double prev_dist;
	// loops - here indicies are computed
	for(i=0; i<obj_num; i++)
	{
		// clear information about previous object nearest neigbours (previous means 'i-1') 
		for(j=0; j<neighbour_num; j++) 
		{ 
			min_tab[j] = -1.0;
			obj_pos[j] = -1;
		}

		// search new neighbours for this data item
		for(j=0; j<obj_num; j++)
		{
			if ( i == j ) j++;
			if (j >= obj_num) break;
			dist = 0;
			prev_obj = j;
			dist = metric(data_sxp, data_sxp, i, j, obj_num, obj_num, dim_num, mean);
			
			// chech if item with number "j" is still candidate to be nearest neighbour
			for(k=0; k<neighbour_num; k++)
			{
				if( min_tab[k] > dist || (min_tab[k] == -1.0 && dist != -1.0) )
				{
					prev_dist = min_tab[k];
					min_tab[k] = dist;
					dist = prev_dist;
					
					tmp = prev_obj;
					prev_obj = obj_pos[k];
					obj_pos[k] = tmp;
				}
				else if ( min_tab[k] == -1.0 ) k = neighbour_num;
			}
		}

		// update connectivity index 
		for(k=0; k<neighbour_num; k++) 
		{
			if(cluster_tab[obj_pos[k]] != cluster_tab[i] )
				connectivity[0] += 1.0/(k+1);
		}
	}
	
	UNPROTECT(protect_num);
	return connectivity_sxp;
}

/*
	
*/

SEXP connectivityDissMx
(
	const SEXP diss_matrix_sxp,				// data matrix  
	const SEXP clusters_sxp,			// vector with information about clusters (object num. -> cluster)
	const SEXP neighbour_num_sxp		// number representing choosen metric
)
{
	// additional variables esecially needed in loops and in functions as parameters
	int i, j, obj_num, dim_num, clust_num, protect_num;
	// define distance between choosen objects
	double dist;

	protect_num = 0;

	// "connectivity" index - result of our function
	SEXP connectivity_sxp;
	double *connectivity;

	PROTECT( connectivity_sxp = allocVector(REALSXP, 1) );
	protect_num++;

	connectivity = REAL(connectivity_sxp);
	connectivity[0] = 0.0;

	// table - which object belongs to which cluster
	int *cluster_tab = INTEGER(clusters_sxp);
	obj_num = length(clusters_sxp);
	
	// pointer to dissimilarity matrix
	double *diss_matrix = REAL(diss_matrix_sxp);

	//extra variables used to compute connectivity index
	int neighbour_num = INTEGER(neighbour_num_sxp)[0];

	// tables needed to store information about nearest neighbours
	SEXP min_tab_sxp, obj_pos_sxp; 
	double *min_tab;
	int *obj_pos;
	PROTECT( min_tab_sxp = allocVector( REALSXP, neighbour_num ) );
	protect_num++;
	PROTECT( obj_pos_sxp = allocVector( INTSXP, neighbour_num ) );
	protect_num++;
	min_tab = REAL(min_tab_sxp);
	obj_pos = INTEGER(obj_pos_sxp);

	int k, prev_obj, tmp;
	double prev_dist;
	// loops - here indicies are computed
	for(i=0; i<obj_num; i++)
	{
		// clear information about previous object nearest neigbours (previous means 'i-1') 
		for(j=0; j<neighbour_num; j++) 
		{ 
			min_tab[j] = -1.0;
			obj_pos[j] = -1;
		}

		// search new neighbours for this data item
		for(j=0; j<obj_num; j++)
		{
			if ( i == j ) j++;
                        if (j >= obj_num) break;
			dist = 0;
			prev_obj = j;
			dist = diss_matrix[ i + j*obj_num ];
			
			// chech if item with number "j" is still candidate to be nearest neighbour
			for(k=0; k<neighbour_num; k++)
			{
				if( min_tab[k] > dist || (min_tab[k] == -1.0 && dist != -1.0) )
				{
					prev_dist = min_tab[k];
					min_tab[k] = dist;
					dist = prev_dist;
					
					tmp = prev_obj;
					prev_obj = obj_pos[k];
					obj_pos[k] = tmp;
				}
				else if ( min_tab[k] == -1.0 ) k = neighbour_num;
			}
		}

		// update connectivity index 
		for(k=0; k<neighbour_num; k++) 
		{
			if(cluster_tab[obj_pos[k]] != cluster_tab[i] )
				connectivity[0] += 1.0/(k+1);
		}
	}
	
	UNPROTECT(protect_num);
	return connectivity_sxp;
}
