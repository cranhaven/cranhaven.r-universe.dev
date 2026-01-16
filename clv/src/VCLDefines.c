#include "VCLDefines.h"

// distance functions

double clv_abs(double x)
{
	if(x<0) return -x;
	else return x;
}

double clv_euclideanMetric(const SEXP data1_sxp, const SEXP data2_sxp, int obj1, int obj2, int data1_len, int data2_len, int dim_num, double *null_mean)
{
	int i, posx, posy;
	double result = 0;
	double *data1 = REAL(data1_sxp);
	double *data2 = REAL(data2_sxp);
	for(i=0; i<dim_num; i++)
	{
		posx = i*data1_len + obj1;
		posy = i*data2_len + obj2;
		result += (data1[posx] - data2[posy]) * (data1[posx] - data2[posy]);
	}
	return sqrt(result);
}

double clv_manhattanMetric(const SEXP data1_sxp, const SEXP data2_sxp, int obj1, int obj2, int data1_len, int data2_len, int dim_num, double *null_mean)
{
	int i, posx, posy;
	double result = 0;
	double *data1 = REAL(data1_sxp);
	double *data2 = REAL(data2_sxp);
	for(i=0; i<dim_num; i++)
	{
		posx = i*data1_len + obj1;
		posy = i*data2_len + obj2;
		result += clv_abs( data1[posx] - data2[posy] );
	}
	return result;
}

double clv_correlationMetric(const SEXP data1_sxp, const SEXP data2_sxp, int obj1, int obj2, int data1_len, int data2_len, int dim_num, double *mean)
{
	int i, posx, posy;
	double *data1 = REAL(data1_sxp);
	double *data2 = REAL(data2_sxp);
	
	double numerator = 0.0;
	double xdenominator = 0.0, ydenominator = 0.0;
	for(i=0; i<dim_num; i++)
	{
		posx = i*data1_len + obj1;
		posy = i*data2_len + obj2;
		numerator += (data1[posx] - mean[i])*(data2[posy] - mean[i]);
		xdenominator += (data1[posx] - mean[i])*(data1[posx] - mean[i]);
		ydenominator += (data2[posy] - mean[i])*(data2[posy] - mean[i]);
	}
	return 1 - numerator/sqrt(xdenominator*ydenominator);
}

// statistic functions

SEXP clv_mean(const SEXP matrix, int obj_num, int dim)
{
	int i,j,pos;
	SEXP result;	// mean  
	PROTECT( result = allocVector(REALSXP,dim) );
	
	for(i=0; i<dim; i++) REAL(result)[i] = 0.0;
	for(i=0; i<dim; i++)
	{
		pos = i*obj_num;
		for(j=0; j<obj_num; j++)
			REAL(result)[i] += REAL(matrix)[pos + j]/obj_num;
	}

	UNPROTECT(1);
	return result;
}

SEXP clv_variance(const SEXP matrix, int obj_num, int dim, const SEXP mean)
{
	int i,j,pos;
	SEXP result;	// variance  
	PROTECT( result = allocVector(REALSXP,dim) );
	double tmp;

	for(i=0; i<dim; i++) REAL(result)[i] = 0.0;
	for(i=0; i<dim; i++)
	{
		pos = i*obj_num;
		for(j=0; j<obj_num; j++)
		{
			tmp = REAL(matrix)[pos+j] - REAL(mean)[i];
			REAL(result)[i] += (tmp*tmp)/obj_num;
		}
	}

	UNPROTECT(1);
	return result;
}

SEXP clv_clusterCenters(const SEXP data_sxp, int obj_num, int dim_num, int clust_num, int *cluster_tab, int *cluster_size)
{
	int i,j, protect_num = 0;
	SEXP result;
	PROTECT( result = allocMatrix(REALSXP, clust_num, dim_num) );
	protect_num++;

	double *cluster_centers = REAL(result);

	// set zeros on every cluster_centers and cluster_variance matrix position
	int clust_num_X_dim_num = clust_num*dim_num;
	for(i=0; i<clust_num_X_dim_num; i++)
		cluster_centers[i] = 0.0;

	for(i=0; i<obj_num; i++)
	{
		// here it would be cluster_center[cluster_num][k] += data[j][k]/|C(k)| 
		// (|C(k)| - cluster size) if we had two dimensional table
		for(j=0; j<dim_num; j++) 
			cluster_centers[j*clust_num + cluster_tab[i]-1] += REAL(data_sxp)[j*obj_num + i]/cluster_size[cluster_tab[i]-1];
	}

	UNPROTECT(protect_num);
	return result;
}


SEXP clv_clusterVariance(const SEXP data_sxp, int obj_num, int dim_num, int clust_num, int *cluster_tab, int *cluster_size, const SEXP mean)
{
	int i,j, protect_num = 0;
	SEXP result;
	PROTECT( result = allocMatrix(REALSXP, clust_num, dim_num) );
	protect_num++;

	double *cluster_variance = REAL(result);

	// set zeros on every cluster_centers and cluster_variance matrix position
	int clust_num_X_dim_num = clust_num*dim_num;
	for(i=0; i<clust_num_X_dim_num; i++)
		cluster_variance[i] = 0.0;

	double tmp;
	for(i=0; i<obj_num; i++)
		for(j=0; j<dim_num; j++)
		{
			tmp = REAL(data_sxp)[j*obj_num + i] - REAL(mean)[j*clust_num + cluster_tab[i]-1];
			cluster_variance[j*clust_num + cluster_tab[i]-1] += (tmp*tmp)/cluster_size[cluster_tab[i]-1];
		}

	UNPROTECT(protect_num);
	return result;
}

/*
	Function count cardinality of each cluster.
	Warning: cluster_tab must be integer type
*/

SEXP clv_clustersSize(const SEXP cluster_tab_sxp, int clust_num)
{
	int i, protect_num = 0;
	SEXP cluster_size_sxp;
	PROTECT( cluster_size_sxp = allocVector(INTSXP, clust_num) );
	protect_num++;

	int *cluster_size, *cluster_tab;
	cluster_size = INTEGER(cluster_size_sxp);
	cluster_tab = INTEGER(cluster_tab_sxp);

	int obj_num = length(cluster_tab_sxp);

	// compute information about cluster centers and size (will be usefull)
	for(i=0; i<clust_num; i++) cluster_size[i] = 0; 
	for(i=0; i<obj_num; i++) cluster_size[cluster_tab[i]-1]++;

	UNPROTECT(protect_num);
	return cluster_size_sxp;
}

// various functions

pMetricFunct getMetricFunct(int num)
{
	pMetricFunct metric;

	switch(num)
	{
		case EUCLIDEAN:
			metric = clv_euclideanMetric;
			break;
		case MANHATTAN:
			metric = clv_manhattanMetric;
			break;
		case CORRELATION:
			metric = clv_correlationMetric;
			return metric;
		default: 
			error("Invalid distance function in .Call(\"internalIndicies\", ...)"); 
	}
	return metric;
} 

// "vcl" product item - structure needed in 'clusteredSetsSection' function
struct clv_item {
	int item_num;
	int clust_num1;
	int clust_num2;
	clv_Item *next;
};

/*
	Function finds section of two subsets becomming from one data set.
	Subsets are representing as matricies (n x 2). 
	First collumn of each input matrix gives information about object number in all set.
	Numbers are sorted from smallest to bigest one.
	Second collumn give information to which cluster this object belongs. 
*/
SEXP clv_clusteredSetsSection(SEXP cluster1_sxp, SEXP cluster2_sxp, SEXP dim)
{
	int *item_num_tab1, *item_num_tab2;
	int *clust_num_tab1, *clust_num_tab2;
	int set_size1, set_size2;
	int protect_num = 0;

	set_size1 = INTEGER(dim)[0];
	set_size2 = INTEGER(dim)[1];
	
	item_num_tab1 = INTEGER(cluster1_sxp);
	item_num_tab2 = INTEGER(cluster2_sxp);
	clust_num_tab1 = item_num_tab1 + set_size1;
	clust_num_tab2 = item_num_tab2 + set_size2;

	int sets_section_size = 0;
	clv_Item *first_item, *last_item, *tmp_item;

	first_item = last_item = NULL;
	int i, j;

	// gather all objects common for two subsets in "one way" list
	for( i=0, j=0; i<set_size1 && j<set_size2;)
		if(item_num_tab1[i] < item_num_tab2[j]) i++;
		else if(item_num_tab1[i] > item_num_tab2[j]) j++;
		else 
		{
			sets_section_size++;
			tmp_item = (clv_Item *) R_alloc(1,sizeof(clv_Item));
			tmp_item->item_num = item_num_tab1[i];
			tmp_item->clust_num1 = clust_num_tab1[i];
			tmp_item->clust_num2 = clust_num_tab2[j];
			tmp_item->next = NULL;

			if(last_item == NULL) first_item = last_item = tmp_item;
			else
			{
				last_item->next = tmp_item;
				last_item = tmp_item;
			}
			i++;
			j++;
		}
	
	int *item_num_tab;
	SEXP result_matrix_sxp;

	// as result we have matrix (n x 3)
	// n - number of objects (rows) in section
	// first column - information about object number 
	// 2nd, 3rd column - cluster numbers where object is assigned
	PROTECT(result_matrix_sxp = allocMatrix(INTSXP, sets_section_size, 3));
	protect_num++;

	item_num_tab = INTEGER(result_matrix_sxp);
	clust_num_tab1 = item_num_tab + sets_section_size;
	clust_num_tab2 = item_num_tab + 2*sets_section_size;

	// fill result matrix and free memory
	i=0;
	while(first_item != NULL)
	{
		item_num_tab[i] = first_item->item_num;
		clust_num_tab1[i] = first_item->clust_num1;
		clust_num_tab2[i] = first_item->clust_num2;
		i++;
		tmp_item = first_item;
		first_item = first_item->next; 
		// free(tmp_item) <-- if we use R_alloc we do not release memory in this way;
	}
	
	UNPROTECT(protect_num);
	return result_matrix_sxp;
}

/*
	Confusion Matrix.

	Function takes two vectors representing different partitioning for the same set of objects.
	For those partitionings confusion matrix is computed. 
	M[i,j] = number of objects common for cluster "i" comming from first clustering 
			 and cluster "j" comming from second one
	In output matrix rows represents first clustering, columns second one.
*/

SEXP confusionMatrix(SEXP cluster1_sxp, SEXP cluster2_sxp, SEXP clust_num)
{
	int *clust_num_tab1, *clust_num_tab2;
	int set_size1, set_size2;
	int clust_num1, clust_num2; 
	int protect_num = 0;

	set_size1 = length(cluster1_sxp);
	set_size2 = length(cluster1_sxp);

	clust_num1 = INTEGER(clust_num)[0];
	clust_num2 = INTEGER(clust_num)[1];
	
	clust_num_tab1 = INTEGER(cluster1_sxp);
	clust_num_tab2 = INTEGER(cluster2_sxp);
	
	SEXP confusion_matrix_sxp;
	PROTECT( confusion_matrix_sxp = allocMatrix(INTSXP, clust_num1, clust_num2) );
	protect_num++;
	int *confusion_matrix;
	confusion_matrix = INTEGER(confusion_matrix_sxp);

	int i, clns = clust_num1*clust_num2;
	// reset all vaules in newly created matrix to 0
	for(i=0; i<clns; i++)
		confusion_matrix[i] = 0;

	// compute cofusion matrix
	for(i=0; i<set_size1; i++)
		confusion_matrix[ (clust_num_tab2[i]-1)*clust_num1 + clust_num_tab1[i]-1 ]++;

	UNPROTECT(protect_num);
	return confusion_matrix_sxp;
}

SEXP clusterAttrib
(
	const SEXP p_data_sxp,		// data matrix  
	const SEXP p_clusters_sxp,	// vector with information about clusters (object num. -> cluster)
	const SEXP p_clust_num 	// number of clusters (table with one element)
)
{
	// additional variables especially needed in loops and in functions as parameters
	int i, j, obj_num, dim_num, clust_num, protect_num;

	protect_num = 0;

	// matrix (and pointer to the table) with cluster centers
	SEXP cluster_center_sxp, cluster_size_sxp;
	double *cluster_center;
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
	
	// pointer to metric function, mean, variance and cluster_variance 
	SEXP mean_sxp;
	double *mean = NULL;
	PROTECT( mean_sxp = clv_mean(p_data_sxp, obj_num, dim_num) );
	protect_num++;
	mean = REAL(mean_sxp);
		
	// matrix with information about clusters centers and vector with cluster size
	PROTECT( cluster_center_sxp = allocMatrix(REALSXP, clust_num, dim_num) );
	protect_num++;

	// vector with information about size of each cluster
	PROTECT( cluster_size_sxp = allocVector(INTSXP, clust_num) );
	protect_num++;

	cluster_center = REAL(cluster_center_sxp);
	cluster_size = INTEGER(cluster_size_sxp);

	// compute information about cluster centers and size (will be usefull)
	for(i=0; i<clust_num; i++) cluster_size[i] = 0; 
	for(i=0; i<obj_num; i++) cluster_size[cluster_tab[i]-1]++;

	// set zeros on every cluster_centers matrix position
	int clust_num_X_dim_num = clust_num*dim_num;
	for(i=0; i<clust_num_X_dim_num; i++)
		cluster_center[i] = 0.0;

	double* data = REAL(p_data_sxp);

	// compute cluster centers
	for(i=0; i<obj_num; i++)
		// here it would be cluster_center[cluster_num][k] += p_data[j][k]/|C(k)| 
		// (|C(k)| - cluster size) if we had two dimensional table
		for(j=0; j<dim_num; j++) 
			cluster_center[j*clust_num + cluster_tab[i]-1] += data[j*obj_num + i]/cluster_size[cluster_tab[i]-1];


	// declaration and initialization of result list
	SEXP return_list, indicies_names;
	int clv_ind = 3;
	int pos = 0;
	PROTECT( return_list = allocVector(VECSXP, clv_ind) );
	protect_num++;
	PROTECT( indicies_names = allocVector(STRSXP, clv_ind) );
	protect_num++;
	SET_STRING_ELT( indicies_names, pos++, mkChar("mean") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("cluster.center") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("cluster.size") );

	setAttrib( return_list, R_NamesSymbol, indicies_names );

	pos = 0;
	SET_VECTOR_ELT( return_list, pos++, mean_sxp );
	SET_VECTOR_ELT( return_list, pos++, cluster_center_sxp );
	SET_VECTOR_ELT( return_list, pos++, cluster_size_sxp );

	UNPROTECT(protect_num);
	return return_list;
}
