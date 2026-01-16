#include "VCLDefines.h"


// -------------------------------------------------------------------------------
// internal indicies computed using distance functions defined in VCLDefines.h


SEXP clusterScatterMeasures
(
	const SEXP p_data_sxp,		// data matrix  
	const SEXP p_clusters_sxp,	// vector with information about clusters (object num. -> cluster)
	const SEXP p_clust_num, 	// number of clusters (table with one element)
	const SEXP p_choosen_metric	// number representing choosen metric
)
{
	// additional variables especially needed in loops and in functions as parameters
	int i, j, obj_num, dim_num, clust_num, protect_num;
	// define distance between choosen objects
	double dist;

	protect_num = 0;
	
	// declaration of intracluster distances (vectors)
	SEXP max_intracluster_sxp, average_intracluster_sxp, centroid_intracluster_sxp;
	double *max_intracluster, *average_intracluster, *centroid_intracluster;

	// declaration of intercluster distances (matrices)
	SEXP single_linkage_sxp, complete_linkage_sxp, average_linkage_sxp;
	SEXP centroid_linkage_sxp, average_to_centroid_sxp, hausdorff_metric_sxp;
	double *single_linkage, *complete_linkage, *average_linkage;
	double *centroid_linkage, *average_to_centroid, *hausdorff_metric;
	
	// matrix (and pointer to the table) with cluster centers
	SEXP cluster_centers_sxp, cluster_size_sxp;
	double *cluster_centers;
	int *cluster_size;

	// table - which object belongs to which cluster
	int *cluster_tab = INTEGER(p_clusters_sxp);

	// get information about data matrix
	SEXP dim = NILSXP;
	PROTECT( dim = getAttrib(p_data_sxp, R_DimSymbol) );
	protect_num++;

	obj_num = INTEGER(dim)[0];
	dim_num = INTEGER(dim)[1];

	// and number of clusters
	clust_num = INTEGER(p_clust_num)[0];

	// mean may be needed - see corelation metric	
	SEXP mean_sxp;
	
	// pointer to metric function, mean, variance and cluster_variance 
	double *mean = NULL;

	// time to get pointer to distance function
	pMetricFunct metric;
	metric = getMetricFunct(INTEGER(p_choosen_metric)[0]);
	if(INTEGER(p_choosen_metric)[0] == CORRELATION )
	{
		SEXP mean_sxp;
		PROTECT( mean_sxp = clv_mean(p_data_sxp, obj_num, dim_num) );
		protect_num++;
		mean = REAL(mean_sxp);
	}
		
	// vectors with information about intracluster compactness cluster measures
	PROTECT( max_intracluster_sxp = allocVector(REALSXP,clust_num) );
	protect_num++;
	PROTECT( average_intracluster_sxp = allocVector(REALSXP,clust_num) );
	protect_num++;
	PROTECT( centroid_intracluster_sxp = allocVector(REALSXP,clust_num) );
	protect_num++;

	max_intracluster = REAL(max_intracluster_sxp);
	average_intracluster = REAL(average_intracluster_sxp);
	centroid_intracluster = REAL(centroid_intracluster_sxp);

	// setting zeros in each table
	for(i=0; i<(clust_num); i++)
		max_intracluster[i] = average_intracluster[i] = centroid_intracluster[i] = 0.0;

	// single, complete, average linkage matrix, its dimension: dim_num x dim_num
	PROTECT( single_linkage_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;
	PROTECT( complete_linkage_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;
	PROTECT( average_linkage_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;

	single_linkage = REAL(single_linkage_sxp);
	complete_linkage = REAL(complete_linkage_sxp);
	average_linkage = REAL(average_linkage_sxp);
		
	// centroid, "average to centroid" and "hausdorff metric" linkage matrix
	PROTECT( centroid_linkage_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;
	PROTECT( average_to_centroid_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;
	PROTECT( hausdorff_metric_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;

	centroid_linkage = REAL(centroid_linkage_sxp);
	average_to_centroid = REAL(average_to_centroid_sxp);
	hausdorff_metric = REAL(hausdorff_metric_sxp);
		
	// matrix with information about clusters centers and vector with cluster size
	PROTECT( cluster_centers_sxp = allocMatrix(REALSXP, clust_num, dim_num) );
	protect_num++;

	// vector with information about size of each cluster
	PROTECT( cluster_size_sxp = allocVector(INTSXP, clust_num) );
	protect_num++;

	cluster_centers = REAL(cluster_centers_sxp);
	cluster_size = INTEGER(cluster_size_sxp);

	// compute information about cluster centers and size (will be usefull)
	for(i=0; i<clust_num; i++) cluster_size[i] = 0; 
	for(i=0; i<obj_num; i++) cluster_size[cluster_tab[i]-1]++;

	// set zeros on every cluster_centers matrix position
	int clust_num_X_dim_num = clust_num*dim_num;
	for(i=0; i<clust_num_X_dim_num; i++)
		cluster_centers[i] = 0.0;

	// compute cluster centers
	for(i=0; i<obj_num; i++)
		// here it would be cluster_center[cluster_num][k] += p_data[j][k]/|C(k)| 
		// (|C(k)| - cluster size) if we had two dimensional table
		for(j=0; j<dim_num; j++) 
			cluster_centers[j*clust_num + cluster_tab[i]-1] += REAL(p_data_sxp)[j*obj_num + i]/cluster_size[cluster_tab[i]-1];

	// additional variable
	int pos;

	for(i=0; i<clust_num; i++)
		for(j=0; j<clust_num; j++)
		{
			pos = i*clust_num + j;
			if(i<j) single_linkage[pos] = -1.0;
			else single_linkage[pos] = 0.0;

			if( i == j ) hausdorff_metric[pos] = 0.0;
			else hausdorff_metric[pos] = -1.0;

			complete_linkage[pos] = average_linkage[pos] = 0.0;
			centroid_linkage[pos] = average_to_centroid[pos] = 0.0;
		}
	
	// we have cluster centers, we can compute "centroid linkage" matrix
	for(i=0; i<clust_num; i++)
		for(j=i+1; j<clust_num; j++)
		{
			dist = metric(cluster_centers_sxp, cluster_centers_sxp, i, j, clust_num, clust_num, dim_num, mean);
			centroid_linkage[i*clust_num + j] = dist;
		}

	// temporary variables used in main loops
	int cluster_i, cluster_j;
	
	//extra variables used to compute hausdorff metric
	SEXP min_sxp; 
	double *min_tab;
	PROTECT( min_sxp = allocVector( REALSXP, clust_num ) );
	protect_num++;
	min_tab = REAL(min_sxp);

	// main loops - here indicies are computed
	for(i=0; i<obj_num; i++)
	{
		cluster_i = cluster_tab[i]-1;
		for(j=0; j<clust_num; j++) min_tab[j] = -1.0;

		for(j=0; j<obj_num; j++)
		{
			dist = 0;
			cluster_j = cluster_tab[j]-1;

			if( i != j ) dist = metric(p_data_sxp, p_data_sxp, i, j, obj_num, obj_num, dim_num, mean);

			if( i < j )
			{
				if( cluster_tab[i] == cluster_tab[j] )
				{
					// INTRACLUSTER DISTANCES
					// "complete" and "average" diameter
					if( max_intracluster[cluster_i] < dist )
						max_intracluster[cluster_i] = dist;

					average_intracluster[cluster_i] += 2*(dist/(cluster_size[cluster_i]*(cluster_size[cluster_i]-1)));
				}
				else
				{
					// INTERCLUSTER  DISTANCES
					// proper position in table (which represents matrix) is needed
					if ( cluster_i < cluster_j ) pos = (cluster_i)*clust_num + cluster_j;
					else pos = (cluster_j)*clust_num + cluster_i;

					// "single", "complete" and "average" linkage
					if( single_linkage[pos] > dist || single_linkage[pos] == -1.0 )
						single_linkage[pos] = dist;
					if( complete_linkage[pos] < dist ) complete_linkage[pos] = dist;
					average_linkage[pos] += dist/(cluster_size[cluster_i]*cluster_size[cluster_j]);
				}
			}
			if( cluster_tab[i] != cluster_tab[j] )
			{
				// compute hausdorff metrics
				if( min_tab[cluster_j] > dist || min_tab[cluster_j] == -1 ) 
					min_tab[cluster_j] = dist;
			}
		}

		// compute distance between object and its cluster cener 
		dist = metric(p_data_sxp, cluster_centers_sxp, i, cluster_i, obj_num, clust_num, dim_num, mean);
		
		// compute "centroid" diameter
		centroid_intracluster[cluster_i] += dist/cluster_size[cluster_i];

		// compute "average to centroid" linkage and hausdorff metric
		for(j=0; j<clust_num; j++)
		{
			// hausdorff part
			if( hausdorff_metric[j*clust_num + cluster_i] < min_tab[j] )
				hausdorff_metric[j*clust_num + cluster_i] = min_tab[j];

			// "average to centroid" part
			dist = metric(p_data_sxp, cluster_centers_sxp, i, j, obj_num, clust_num, dim_num, mean);
			// division by |S|+|T| each "dist" (|C| - cardinality of cluster C)
			if( j < cluster_i ) 
				average_to_centroid[ clust_num*j + cluster_i ] += dist/(cluster_size[cluster_i]+cluster_size[j]);
			else if( j > cluster_i ) 
				average_to_centroid[ cluster_i*clust_num + j ] += dist/(cluster_size[cluster_i]+cluster_size[j]);
		}
		
	}

	// result matrices should be symetric
	int up, down;
	for(j=0; j<clust_num; j++)
		for(i=j+1; i<clust_num; i++)
		{
			down = i + j*clust_num;
			up   = j + i*clust_num;
			single_linkage[up] = single_linkage[down];
			complete_linkage[up] = complete_linkage[down];
			average_linkage[up] = average_linkage[down];	
			centroid_linkage[up] = centroid_linkage[down];
			average_to_centroid[up] = average_to_centroid[down];
		}

	// declaration and initialization of result list
	SEXP return_list, indicies_names;
	int clv_ind = 11;
	pos = 0;
	PROTECT( return_list = allocVector(VECSXP, clv_ind) );
	protect_num++;
	PROTECT( indicies_names = allocVector(STRSXP, clv_ind) );
	protect_num++;
	SET_STRING_ELT( indicies_names, pos++, mkChar("intracls.complete") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intracls.average") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intracls.centroid") );

	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.single") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.complete") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.average") );

	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.centroid") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.ave_to_cent") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.hausdorff") );
	
	SET_STRING_ELT( indicies_names, pos++, mkChar("cluster.center") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("cluster.size") );

	setAttrib( return_list, R_NamesSymbol, indicies_names );

	pos = 0;
	SET_VECTOR_ELT( return_list, pos++, max_intracluster_sxp );
	SET_VECTOR_ELT( return_list, pos++, average_intracluster_sxp );
	SET_VECTOR_ELT( return_list, pos++, centroid_intracluster_sxp );

	SET_VECTOR_ELT( return_list, pos++, single_linkage_sxp );
	SET_VECTOR_ELT( return_list, pos++, complete_linkage_sxp );
	SET_VECTOR_ELT( return_list, pos++, average_linkage_sxp );

	SET_VECTOR_ELT( return_list, pos++, centroid_linkage_sxp );
	SET_VECTOR_ELT( return_list, pos++, average_to_centroid_sxp );
	SET_VECTOR_ELT( return_list, pos++, hausdorff_metric_sxp );
	
	SET_VECTOR_ELT( return_list,  pos++, cluster_centers_sxp );
	SET_VECTOR_ELT( return_list,  pos++, cluster_size_sxp );
	
	UNPROTECT(protect_num);
	return return_list;
}


// internal indicies computed using dissimilarity matrix given by the user

SEXP clusterScatterMeasuresDissMx
(
	const SEXP p_diss_matrix_sxp,	// dissimilarity matrix  
	const SEXP p_clusters_sxp,		// vector with information about clusters (object num. -> cluster)
	const SEXP p_clust_num			// number of clusters (table with one element)
)
{
	// additional variables especially needed in loops and in functions as parameters
	int i, j, obj_num, dim_num, clust_num, protect_num;
	// define distance between choosen objects
	double dist;

	protect_num = 0;
	
	// declaration of intracluster distances (vectors)
	SEXP max_intracluster_sxp, average_intracluster_sxp;
	double *max_intracluster, *average_intracluster;

	// declaration of intercluster distances (matrices)
	SEXP single_linkage_sxp, complete_linkage_sxp, average_linkage_sxp, hausdorff_metric_sxp;
	double *single_linkage, *complete_linkage, *average_linkage, *hausdorff_metric;
	
	// matrix with size of each cluster
	SEXP cluster_size_sxp;
	int *cluster_size;

	// table - which object belongs to which cluster
	int *cluster_tab = INTEGER(p_clusters_sxp);

	// get information about dissimilarity matrix
	double *diss_matrix = REAL(p_diss_matrix_sxp);

	SEXP dim = NILSXP;
	PROTECT( dim = getAttrib(p_diss_matrix_sxp, R_DimSymbol) );
	protect_num++;

	obj_num = INTEGER(dim)[0];

	// and number of clusters
	clust_num = INTEGER(p_clust_num)[0];

	// vectors with information about intracluster compactness cluster measures
	PROTECT( max_intracluster_sxp = allocVector(REALSXP,clust_num) );
	protect_num++;
	PROTECT( average_intracluster_sxp = allocVector(REALSXP,clust_num) );
	protect_num++;

	max_intracluster = REAL(max_intracluster_sxp);
	average_intracluster = REAL(average_intracluster_sxp);

	// setting zeros in each table
	for(i=0; i<(clust_num); i++)
		max_intracluster[i] = average_intracluster[i] = 0.0;

	// single, complete, average linkage matrix, its dimension: clust_num x clust_num
	PROTECT( single_linkage_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;
	PROTECT( complete_linkage_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;
	PROTECT( average_linkage_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;

	single_linkage = REAL(single_linkage_sxp);
	complete_linkage = REAL(complete_linkage_sxp);
	average_linkage = REAL(average_linkage_sxp);
		
	// "hausdorff metric" linkage matrix
	PROTECT( hausdorff_metric_sxp = allocMatrix(REALSXP, clust_num, clust_num) );
	protect_num++;

	hausdorff_metric = REAL(hausdorff_metric_sxp);

	// vector with information about size of each cluster
	PROTECT( cluster_size_sxp = allocVector(INTSXP, clust_num) );
	protect_num++;

	cluster_size = INTEGER(cluster_size_sxp);

	// compute information about cluster centers and size (will be usefull)
	for(i=0; i<clust_num; i++) cluster_size[i] = 0; 
	for(i=0; i<obj_num; i++) cluster_size[cluster_tab[i]-1]++;

	// additional variable
	int pos;

	for(i=0; i<clust_num; i++)
		for(j=0; j<clust_num; j++)
		{
			pos = i*clust_num + j;
			if(i<j) single_linkage[pos] = -1.0;
			else single_linkage[pos] = 0.0;

			if( i == j ) hausdorff_metric[pos] = 0.0;
			else hausdorff_metric[pos] = -1.0;

			complete_linkage[pos] = average_linkage[pos] = 0.0;
		}

	// temporary variables used in main loops
	int cluster_i, cluster_j;
	
	//extra variables used to compute hausdorff metric
	SEXP min_sxp; 
	double *min_tab;
	PROTECT( min_sxp = allocVector( REALSXP, clust_num ) );
	protect_num++;
	min_tab = REAL(min_sxp);

	// main loops - here indicies are computed
	for(i=0; i<obj_num; i++)
	{
		cluster_i = cluster_tab[i]-1;
		for(j=0; j<clust_num; j++) min_tab[j] = -1.0;

		for(j=0; j<obj_num; j++)
		{
			dist = 0;
			cluster_j = cluster_tab[j]-1;

			if( i != j ) dist = diss_matrix[ i + j*obj_num ];
	
			if( i < j )
			{
				if( cluster_tab[i] == cluster_tab[j] )
				{
					// INTRACLUSTER DISTANCES
					// "complete" and "average" diameter
					if( max_intracluster[cluster_i] < dist )
						max_intracluster[cluster_i] = dist;

					average_intracluster[cluster_i] += dist/(cluster_size[cluster_i]*(cluster_size[cluster_i]-1));
				}
				else
				{
					// INTERCLUSTER  DISTANCES
					// proper position in table (which represents matrix) is needed
					if ( cluster_i < cluster_j ) pos = (cluster_i)*clust_num + cluster_j;
					else pos = (cluster_j)*clust_num + cluster_i;

					// "single", "complete" and "average" linkage
					if( single_linkage[pos] > dist || single_linkage[pos] == -1.0 )
						single_linkage[pos] = dist;
					if( complete_linkage[pos] < dist ) complete_linkage[pos] = dist;
					average_linkage[pos] += dist/(cluster_size[cluster_i]*cluster_size[cluster_j]);
				}
			}
			if( cluster_tab[i] != cluster_tab[j] )
			{
				// compute hausdorff metrics
				if( min_tab[cluster_j] > dist || min_tab[cluster_j] == -1 ) 
					min_tab[cluster_j] = dist;
			}
		}

		// compute  hausdorff metric
		for(j=0; j<clust_num; j++)
			// hausdorff part
			if( hausdorff_metric[j*clust_num + cluster_i] < min_tab[j] )
				hausdorff_metric[j*clust_num + cluster_i] = min_tab[j];
	}

	// result matrices should be symetric
	int up, down;
	for(j=0; j<clust_num; j++)
		for(i=j+1; i<clust_num; i++)
		{
			down = i + j*clust_num;
			up   = j + i*clust_num;
			single_linkage[up] = single_linkage[down];
			complete_linkage[up] = complete_linkage[down];
			average_linkage[up] = average_linkage[down];
		}

	// declaration and initialization of result list
	SEXP return_list, indicies_names;
	int clv_ind = 7;
	pos = 0;
	PROTECT( return_list = allocVector(VECSXP, clv_ind) );
	protect_num++;
	PROTECT( indicies_names = allocVector(STRSXP, clv_ind) );
	protect_num++;
	SET_STRING_ELT( indicies_names, pos++, mkChar("intracls.complete") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intracls.average") );

	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.single") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.complete") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.average") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("intercls.hausdorff") );

	SET_STRING_ELT( indicies_names, pos++, mkChar("cluster.size") );

	setAttrib( return_list, R_NamesSymbol, indicies_names );

	pos = 0;
	SET_VECTOR_ELT( return_list, pos++, max_intracluster_sxp );
	SET_VECTOR_ELT( return_list, pos++, average_intracluster_sxp );

	SET_VECTOR_ELT( return_list, pos++, single_linkage_sxp );
	SET_VECTOR_ELT( return_list, pos++, complete_linkage_sxp );
	SET_VECTOR_ELT( return_list, pos++, average_linkage_sxp );
	SET_VECTOR_ELT( return_list, pos++, hausdorff_metric_sxp );

	SET_VECTOR_ELT( return_list,  pos++, cluster_size_sxp );
	
	UNPROTECT(protect_num);
	return return_list;
}
