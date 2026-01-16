#include "VCLDefines.h"

// --------------- distance between two partitionings -------------------

/*
	As input we have to wectors with information about two different partitionings of the same set.
	Both vectors should have the same length and should be INTSXP type. 
	As output we got list with four values used to compute standard external criteria like
	Rand Statistic, Jaccard Coeficient, Folkes and Mallows index etc.
	Checking all pairs of objects we want to compute indicies:
	SS - number of pairs where both points belongs to the same cluster in both partitionings
	SD - number of pairs where both points belongs to the same cluster in first partitioning but 
		 in second one do not
	DS - number of pairs where in first partitioning both point belongs to different clusters 
		 but in second one do not
	DD - number of pairs where both objects belongs to different clusters in both partitionings
*/

SEXP standardExternalMeasures(SEXP confusion_matrix_sxp)
{
	int i,j, pos;
	int *confusion_matrix;
	int clust_num_i, clust_num_j; 
	int protect_num = 0;

	confusion_matrix = INTEGER( confusion_matrix_sxp );
	clust_num_i = INTEGER( getAttrib( confusion_matrix_sxp, R_DimSymbol ) )[0];
	clust_num_j = INTEGER( getAttrib( confusion_matrix_sxp, R_DimSymbol ) )[1];

	double  sum = 0, sum_2 = 0;
	double *sum_in_row, *sum_in_col;
	sum_in_row = (double*) R_alloc( clust_num_i, sizeof(double) );
	sum_in_col = (double*) R_alloc( clust_num_j, sizeof(double) );
	
	for(i=0; i<clust_num_i; i++) sum_in_row[i] = 0.0;
	for(j=0; j<clust_num_j; j++) sum_in_col[j] = 0.0;

	for(j=0; j<clust_num_j; j++)
	{
		pos = j*clust_num_i;
		for(i=0; i<clust_num_i; i++)
		{
			sum_in_col[j] += confusion_matrix[ i + pos ]*1.0;
			sum_in_row[i] += confusion_matrix[ i + pos ]*1.0;
			sum += confusion_matrix[ i + pos ]*1.0;
			sum_2 += confusion_matrix[ i + pos ]*(confusion_matrix[ i + pos ]/2.0); 
		}
	}

	double index_tab[4];
	index_tab[0]= index_tab[1] = index_tab[2] = index_tab[3] = 0.0;
	for(i=0; i<clust_num_i; i++)
		index_tab[1] += sum_in_row[i]*(sum_in_row[i]/2.0);
	for(i=0; i<clust_num_j; i++)
		index_tab[2] += sum_in_col[i]*(sum_in_col[i]/2.0); 

	// prepare result values
	double tmp1,tmp2;
	tmp1 = index_tab[1];
	tmp2 = index_tab[2];

	index_tab[0] = sum_2 - sum/2.0;
	index_tab[1] = tmp1 - sum_2;
	index_tab[2] = tmp2 - sum_2;
	index_tab[3] = sum_2 + sum*(sum/2.0) - tmp1 - tmp2;

	// result list 
	SEXP result_list, indicies_names;
	pos = 0;
	PROTECT( result_list = allocVector(VECSXP, 4) );
	protect_num++;
	PROTECT( indicies_names = allocVector(STRSXP, 4) );
	protect_num++;
	SET_STRING_ELT( indicies_names, pos++, mkChar("SS") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("SD") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("DS") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("DD") );
	setAttrib( result_list, R_NamesSymbol, indicies_names );
	
	pos = 0;

	SEXP index_tab_sxp[4];
	for(i=0; i<4; i++)
	{
		PROTECT(index_tab_sxp[i] = allocVector(REALSXP, 1));
		protect_num++;
		REAL(index_tab_sxp[i])[0] = index_tab[i];
		SET_VECTOR_ELT( result_list, pos++, index_tab_sxp[i] );
	}

	UNPROTECT(protect_num++);
	return result_list;
}


//---------------------------------------------------

/*
	Function compute the same values as function above - slower comutation. 
*/

SEXP standardExternalMeasuresSlow(SEXP cluster1_sxp, SEXP cluster2_sxp)
{
	int *clust_num_tab1, *clust_num_tab2;
	int set_size; 
	int protect_num = 0;

	set_size = length(cluster1_sxp);
	
	clust_num_tab1 = INTEGER(cluster1_sxp);
	clust_num_tab2 = INTEGER(cluster2_sxp);

	// temporary results
	int index_tab[4],i,j;
	index_tab[0]= index_tab[1] = index_tab[2] = index_tab[3] = 0;	

	// no comments
	for(i=0; i<set_size; i++)
		for(j=i+1; j<set_size; j++)
		{
			if(clust_num_tab1[i] == clust_num_tab1[j] && 
			   clust_num_tab2[i] == clust_num_tab2[j]
			  ) index_tab[0]++;
			else if(clust_num_tab1[i] == clust_num_tab1[j] && 
					clust_num_tab2[i] != clust_num_tab2[j]
				   ) index_tab[1]++;
			else if(clust_num_tab1[i] != clust_num_tab1[j] && 
					clust_num_tab2[i] == clust_num_tab2[j]
				   ) index_tab[2]++;
			else index_tab[3]++;
		}

	// result list 
	SEXP result_list, indicies_names;
	int pos = 0;
	PROTECT( result_list = allocVector(VECSXP, 4) );
	protect_num++;
	PROTECT( indicies_names = allocVector(STRSXP, 4) );
	protect_num++;
	SET_STRING_ELT( indicies_names, pos++, mkChar("SS") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("SD") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("DS") );
	SET_STRING_ELT( indicies_names, pos++, mkChar("DD") );
	setAttrib( result_list, R_NamesSymbol, indicies_names );
	
	pos = 0;

	SEXP index_tab_sxp[4];
	for(i=0; i<4; i++)
	{
		PROTECT(index_tab_sxp[i] = allocVector(INTSXP, 1));
		protect_num++;
		INTEGER(index_tab_sxp[i])[0] = index_tab[i];
		SET_VECTOR_ELT( result_list, pos++, index_tab_sxp[i] );
	}

	UNPROTECT(protect_num++);
	return result_list;
}
