#include "VCLDefines.h"

//----------------- internal measures ------------------------------------

/*
	
*/
double clv_norm(double *p_matrix, int p_obj_num, int p_dim, int p_obj_len)
{
	double tmp, result = 0;

	int i;
	for(i=0; i<p_dim; i++)
	{
		tmp = p_matrix[ p_obj_num + i*p_obj_len ];
		result += tmp*tmp;
	}

	return sqrt(result);
}

SEXP Scatt( const SEXP data_sxp,		// data matrix  
			const SEXP clusters_sxp,	// vector with information about clusters (object num. -> cluster)
			const SEXP clust_num_sxp, // number of clusters (table with one element)
			const SEXP choosen_metric_sxp	// number representing choosen metric
		  )
{

	// additional variables especially needed in loops and in functions as parameters
	int i, j, obj_num, dim_num, clust_num, protect_num;
	// define distance between choosen objects
	double dist;

	protect_num = 0;
	
	// declaration of intracluster distances (vectors)
	SEXP mean_sxp, variance_sxp;
	double *mean, *variance;
	
	// matrix (and pointer to the table) with cluster centers
	SEXP cluster_center_sxp, cluster_variance_sxp, cluster_size_sxp;
	double *cluster_center, *cluster_variance;
	int *cluster_size;

	// table - which object belongs to which cluster
	int *cluster_tab = INTEGER(clusters_sxp);

	// get information about data matrix
	SEXP dim = NILSXP;
	PROTECT( dim = getAttrib(data_sxp, R_DimSymbol) );
	protect_num++;

	obj_num = INTEGER(dim)[0];
	dim_num = INTEGER(dim)[1];

	// and number of clusters
	clust_num = INTEGER(clust_num_sxp)[0];

	// compute mean 
	PROTECT( mean_sxp = clv_mean(data_sxp, obj_num, dim_num) );
	protect_num++;

	// and variance
	PROTECT( variance_sxp = clv_variance(data_sxp, obj_num, dim_num, mean_sxp) );
	protect_num++;
	variance = REAL(variance_sxp);

	// vector with information about size of each cluster
	PROTECT( cluster_size_sxp = clv_clustersSize(clusters_sxp, clust_num) );
	protect_num++;
	cluster_size = INTEGER(cluster_size_sxp);
	
	PROTECT( cluster_center_sxp = clv_clusterCenters(data_sxp, obj_num, dim_num, clust_num, cluster_tab, cluster_size) );
	protect_num++;

	PROTECT( cluster_variance_sxp = clv_clusterVariance(data_sxp, obj_num, dim_num, clust_num, cluster_tab, cluster_size, cluster_center_sxp) );
	protect_num++;
	cluster_variance = REAL(cluster_variance_sxp);

	double sum_cls_var_norm = 0, tmp;
	int pos;

	// compute "stdev" value ( sum[ forall k in {1, ... ,cluster num.} ] ||sigma(C_k)||)
	for(i=0; i<clust_num; i++)
	{
		sum_cls_var_norm += clv_norm(cluster_variance, i, dim_num, clust_num); 
	}
	
	// compute norm of variance of dataset (||sigma(X)||)
	double var_norm = clv_norm(variance, 0, dim_num, 1); ;
	
	SEXP Scatt, stdev;
	PROTECT( Scatt = allocVector(REALSXP, 1) );
	protect_num++;
	PROTECT( stdev = allocVector(REALSXP, 1) );
	protect_num++;
	
	REAL(Scatt)[0] = sum_cls_var_norm/(clust_num*var_norm);
	REAL(stdev)[0] = sqrt(sum_cls_var_norm)/clust_num;
	
	// time to gather all particular indicies into one result list 
	int list_elem_num = 3;
	SEXP result_list;
	PROTECT( result_list = allocVector(VECSXP, list_elem_num) );
	protect_num++;
	
	SEXP names;
	PROTECT( names = allocVector(STRSXP, list_elem_num) );
	protect_num++;
	
	pos = 0;
	SET_STRING_ELT( names, pos++, mkChar("Scatt") );
	SET_STRING_ELT( names, pos++, mkChar("stdev") );
	SET_STRING_ELT( names, pos++, mkChar("cluster.center") );

	setAttrib( result_list, R_NamesSymbol, names );	

	pos = 0;
	SET_VECTOR_ELT( result_list, pos++, Scatt );
	SET_VECTOR_ELT( result_list, pos++, stdev );
	SET_VECTOR_ELT( result_list, pos++, cluster_center_sxp );
	
	UNPROTECT(protect_num);
	return result_list;
}

/*
	
*/

double clv_normOfCentersDiff(double *p_matrix, int p_obj_num1, int p_obj_num2, int p_dim, int p_obj_len)
{
	double tmp, result = 0;

	int i;
	for(i=0; i<p_dim; i++)
	{
		tmp = p_matrix[ p_obj_num1 + i*p_obj_len ] - p_matrix[ p_obj_num2 + i*p_obj_len ];
		result += tmp*tmp;
	}

	return sqrt(result);
}

SEXP Dis(const SEXP cluster_center_sxp)
{
	int i, j, k, clust_num, dim_num, protect_num = 0;

	double *clusters_centers;
	clusters_centers = REAL(cluster_center_sxp);

	SEXP dim;
	PROTECT( dim = getAttrib(cluster_center_sxp, R_DimSymbol) );
	protect_num++;

	clust_num = INTEGER(dim)[0];
	dim_num = INTEGER(dim)[1];

	double result = 0, tmp, diff, tmp_denominator;
	double Dmin = -1, Dmax = 0;
	for(i=0; i<clust_num; i++)
	{
		tmp_denominator = 0;
		for(j=0; j<clust_num; j++)
		{
			if(i != j)
			{
				// compute ||v_i - v_j|| (v_i, v_j - centers of clusters i and j)
				tmp = clv_normOfCentersDiff(clusters_centers, i, j, dim_num, clust_num);
				
				if( tmp > Dmax ) Dmax = tmp;
				if( Dmin == -1 || Dmin > tmp ) Dmin = tmp; 
			}
			else tmp = 0;
			tmp_denominator += tmp;
		}
		result += 1/tmp_denominator;
	}

	// prepare result
	SEXP Dis;
	PROTECT( Dis = allocVector(REALSXP, 1) );
	protect_num++;

	REAL(Dis)[0] = (Dmax/Dmin)*result;
	
	UNPROTECT(protect_num);
	return Dis; 
}

/*

*/

SEXP Dens_bw(const SEXP data_sxp, 
			 const SEXP cluster_sxp, 
			 const SEXP cluster_center_sxp, 
			 const SEXP stdev_sxp, 
			 const SEXP choosen_metric_sxp)
{
	// temporary variables 
	int i, j, k, pos, protect_num=0;
	// some constants 
	int clust_num, dim_num, obj_num;

	double *cluster_center;
	int *cluster_tab;
	cluster_center = REAL(cluster_center_sxp);
	cluster_tab = INTEGER(cluster_sxp);
	

	SEXP data_dim;
	PROTECT( data_dim = getAttrib(data_sxp, R_DimSymbol) );
	protect_num++;

	obj_num = INTEGER(data_dim)[0]; 

	SEXP dim;
	PROTECT( dim = getAttrib(cluster_center_sxp, R_DimSymbol) );
	protect_num++;

	clust_num = INTEGER(dim)[0];
	dim_num = INTEGER(dim)[1];

	SEXP density_middle_point_sxp;
	PROTECT( density_middle_point_sxp = allocMatrix(VECSXP, clust_num, clust_num) );
	protect_num++;
	
	SEXP* midp_coords_sxp;
	midp_coords_sxp = (SEXP*) R_alloc(clust_num*clust_num, sizeof(SEXP) );	

	// initilaize matix of vectors, each vector represent density_middle point between two cluter centers
	double *tmp_tab;
	for(i=0; i<clust_num; i++)
		for(j=0; j<clust_num; j++)
		{
			pos = j + i*clust_num;
			if(i<j) 
			{
				PROTECT( midp_coords_sxp[pos] = allocVector(REALSXP, dim_num) );
				tmp_tab = REAL(midp_coords_sxp[pos]);

				// compute density_middle point between center of cluster i and j
				for(k=0; k<dim_num; k++) 
					tmp_tab[k] = ( cluster_center[i + k*clust_num] + cluster_center[j + k*clust_num] )/2;

			}
			else PROTECT( midp_coords_sxp[pos] = R_NilValue );
			
			protect_num++;
			SET_VECTOR_ELT( density_middle_point_sxp, pos, midp_coords_sxp[pos] );
		}

	// time to get the pointer to the distance function
	double *mean = NULL;
	pMetricFunct metric;
	metric = getMetricFunct(INTEGER(choosen_metric_sxp)[0]);
	if( INTEGER(choosen_metric_sxp)[0] == CORRELATION )
	{
		SEXP mean_sxp;
		PROTECT( mean_sxp = clv_mean(data_sxp, obj_num, dim_num) );
		protect_num++;
		mean = REAL(mean_sxp);
	}

	// vector and matrix create to store information about density of clusters 
	// and "density" of space between centers of clusters 
	SEXP center_density_sxp, midp_density_sxp;
	int *center_density, *midp_density;
	double stdev;
	PROTECT( center_density_sxp = allocVector(INTSXP, clust_num) );
	protect_num++;
	PROTECT( midp_density_sxp = allocMatrix(INTSXP, clust_num, clust_num) );
	protect_num++;
	center_density = INTEGER(center_density_sxp);
	midp_density = INTEGER(midp_density_sxp);
	stdev = REAL(stdev_sxp)[0];

	for(i=0; i<clust_num; i++)
	{
		center_density[i] = 0;
		pos = i*clust_num;
		for(j=0; j<clust_num; j++) midp_density[ j + pos ] = 0;
	}

	// declare return value
	SEXP result_sxp;
	PROTECT( result_sxp = allocVector(REALSXP, 1) );
	protect_num++;

	// compute density over clusters centers and density_middle points between cluster centers 
	int density_center1, density_center2, density_middle;
	double dist;
	double tmp_sum = 0;
	for(i=0; i<clust_num; i++)
		for(j=i+1; j<clust_num; j++)
		{
			density_center1 = density_center2 = density_middle = 0; 
			for(k=0; k<obj_num; k++)
			{
				// if current object belongs to cluster i or j
				if( cluster_tab[k] == (i+1) || cluster_tab[k] == (j+1) )
				{
					// check if object "k" belongs to any of the following spheres:
					// B( v(i), stdev ), B( v(j), stdev ) or B( u(i,j), stdev )
					
					dist = metric(data_sxp, cluster_center_sxp, k, i, obj_num, clust_num, dim_num, mean);
					if( dist <= stdev ) density_center1++;

					//Rprintf("Dist from center %d to obejct %d: %f\n", i+1, k+1, dist);
					
					dist = metric(data_sxp, cluster_center_sxp, k, j, obj_num, clust_num, dim_num, mean);
					if( dist <= stdev ) density_center2++;

					//Rprintf("Dist from center %d to obejct %d: %f\n", j+1, k+1, dist);

					pos = j + i*clust_num;
					dist = metric(data_sxp, midp_coords_sxp[pos], k, 0, obj_num, 1, dim_num, mean);
					if( dist <= stdev ) density_middle++;

					//Rprintf("Dist from density_middle (%d,%d) to obejct %d: %f\n", i+1, j+1, k+1, dist);
				}
			}
			
			// check if Dens_bw index is well defined
			if( density_center1 != 0 && density_center2 != 0 ) 
				tmp_sum += density_middle/(1.0*( density_center1 > density_center2 ? density_center1 : density_center2 ));
			else 
			{
				// theoretically Dens_bw should be undefined in here 
				// (division by 0) but we will set it as +Inf
				REAL(result_sxp)[0] = R_PosInf; 
				UNPROTECT(protect_num);
				return result_sxp;
			}
			
		}
	
	REAL(result_sxp)[0] = (tmp_sum*2.0)/(clust_num*(clust_num-1));

	UNPROTECT(protect_num);
	return result_sxp;
}
