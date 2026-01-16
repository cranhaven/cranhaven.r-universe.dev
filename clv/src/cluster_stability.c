#include "VCLDefines.h"

/*.
	file contains functions that speed up computations of cluster stablity
*/

// cluster size - 2-nd version 
// (one argument changed - function ready to work with R functions via .Call interface)

SEXP clv_clustersSizeExt(const SEXP cluster_tab_sxp, const SEXP clust_num_sxp)
{
	return clv_clustersSize(cluster_tab_sxp, INTEGER(clust_num_sxp)[0]);
}

// functions connected only with "clust.stab.opt.assign" R method (version for non-hierarhical)
// preapared to use with .C interface

void clv_updateStabbResults(int* obj_cls_freq, 	//vector[i] - how many times i-th object were clustered 
			    int* obj_assgn_freq,//vector[i] - how many times i-th object were clustered properly
			    int* opt_assign, 	//vector - optimal assignment between two clusterings
			    int* section_matrix,//matrix - see "cls.set.section" result
			    int* other 		//vector with 3 values: section size, subset 1 and 2 numbers 
			   )
{
	int i;
	int sect_size = other[0];
	int set1 = other[1] - 1;
	int set2 = other[2] - 1;

	for(i=0; i<sect_size; i++)
	{
		obj_cls_freq[ section_matrix[i] ] += 1; // 
		if( opt_assign[ section_matrix[i + sect_size * set1] ] == section_matrix[i + sect_size * set2] ) 
			obj_assgn_freq[ section_matrix[i] - 1 ] += 1;
	}
}

// functions connected only with "clust.stab.opt.assign" R method (version for hierahical clustering)
// preapared to use with .C interface

void clv_updateStabbResults2(int* obj_assgn_freq,//matrix - how many times i-th object were clustered properly 
			     			 //(row mean on how many clusters data were partitioned)
			     int* opt_assign, 	 //vector - optimal assignment between two clusterings
			     int* section_matrix,//matrix - see "cls.set.section" result
			     int* other 	 //vector with 5 values: section size, subset 1 and 2 numbers, row, row number
			    )
{
	int i;
	int sect_size = other[0];
	int set1 = other[1] - 1;
	int set2 = other[2] - 1;
	int row = other[3] - 1;
	int row_number = other[4];

	for(i=0; i<sect_size; i++)
	{ 
		if( opt_assign[ section_matrix[ i + sect_size * set1 ] ] == section_matrix[ i + sect_size * set2 ] ) 
			obj_assgn_freq[ (section_matrix[i] - 1) * row_number + row ] += 1;
	}
}

