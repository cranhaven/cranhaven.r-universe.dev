#include "VCLDefines.h"

//------------ stablity based cluster analysis ------------------

/*
	Function search maximum available value in one row of input matrix.
	Available means value not forbidden by the same row in "allowed_collumns" matrix.
*/

int clv_findMaxAvailableInRow(int* matrix, int* allowed_columns, int row_num, int col_num, int row)
{
	int max_val = 0;
	int max_val_index = -1;
	int i;

	for(i=0; i<col_num; i++)
		if( allowed_columns[ row + i*row_num ] == 1 && 
			matrix[ row + i*row_num ] >= max_val
		  )
		{
			max_val = matrix[ row + i*row_num ];
			max_val_index = i;
		}

	return max_val_index;
}

/*
	Check conflict.
	Function test if there is two the same values in input vector.
*/

int clv_checkConflict(int* opt_assignment, int row_num, int pos)
{
	int result = -1;
	int i;
	for(i=0; i<row_num; i++)
		if(i != pos && opt_assignment[i] != -1 && opt_assignment[i] == opt_assignment[pos]) return i;
	return result;
}

/*
	"Throw a coin" function. 
	0 - heads, 1 - tails
*/

int clv_throwTheCoin(void)
{
	GetRNGstate();
	double result = unif_rand();
	PutRNGstate();
	return ( result<0.5 ? 0 : 1 );
}

/*
	Optimal Assigment.

	As imput we have integer, positive matrix where row number <= column number.
	As output we get a vector where on i'th position (means number of row in input matrix)
	column number is assigned to this row. 
	Sum of all values in output vector is "supreme" (with satistical precision). 
*/


SEXP clv_optimalAssignment(SEXP confusion_matrix_sxp)
{
	int i,j,protect_num = 0;
	
	PROTECT(confusion_matrix_sxp);
	protect_num++;

	int *confusion_matrix;
	confusion_matrix = INTEGER(confusion_matrix_sxp);
	
	// dimensions 
	SEXP dim;
	PROTECT( dim = getAttrib(confusion_matrix_sxp, R_DimSymbol) );
	protect_num++;
	
	int row_num, col_num, min_dim;
	row_num = INTEGER(dim)[0];
	col_num = INTEGER(dim)[1];
	min_dim = ( col_num < row_num ? col_num : row_num );
	
	// optimal vector 
	SEXP opt_assignment_sxp;
	PROTECT( opt_assignment_sxp = allocVector(INTSXP, min_dim) );
	protect_num++;

	int* opt_assignment;
	opt_assignment = INTEGER( opt_assignment_sxp );
	
	for(i=0; i<min_dim; i++)
		opt_assignment[i] = -1;

	// additional matrix which store information about conflicts between rows 
	// (means clusters comming from first partitioning) 
	SEXP constraint_matrix_sxp;
	PROTECT( constraint_matrix_sxp = allocMatrix(INTSXP, row_num, col_num) );
	protect_num++;

	int* constraint_matrix;
	constraint_matrix = INTEGER(constraint_matrix_sxp);

	// set initial values into constraint_matrix 
	// (M[i,j] = 1 means that cluster with number i which comes from first partitioning 
	// can be assigned to cluster with number j comming from second partitioning )
	int dim_mult = row_num*col_num;
	for(i=0; i<dim_mult; i++)
		constraint_matrix[i] = 1;

	int conflict_pos;

	// main loop - it finds statisticaly optimal assignment 
	for(i=0; i<row_num; i++)
	{
		opt_assignment[i] = clv_findMaxAvailableInRow( confusion_matrix, constraint_matrix, 
													   row_num, col_num, i );
		constraint_matrix[ i + row_num*opt_assignment[i]] = 0;
		conflict_pos = clv_checkConflict(opt_assignment, row_num, i);

		j = i;
		// task of this loop is to solve all conflicts if there are any
		while(conflict_pos != -1)
		{
			// Compare matrix[conflict][opt_assign[conflict]] and  matrix[i][opt_assign[i]]
			// In case when first value is less than second one "conflict_pos" 
			// (strictly connected to "opt_assignment" vector) have to be changed. 
			// Also if we have equality in 50% cases "conflict_pos" will be changed
			if( confusion_matrix[ conflict_pos + row_num*opt_assignment[conflict_pos] ] < confusion_matrix[ j + row_num*opt_assignment[j] ] ||
				(
					confusion_matrix[ conflict_pos + row_num*opt_assignment[conflict_pos] ] == confusion_matrix[ j + row_num*opt_assignment[j] ] && 
					clv_throwTheCoin() 
				)
			  )
				j = conflict_pos;

			// find new column for conflicted row
			opt_assignment[j] = clv_findMaxAvailableInRow( confusion_matrix, constraint_matrix, 
														   row_num, col_num, j);
			// this column cannot be selected for the second time
			constraint_matrix[ j + row_num*opt_assignment[j]] = 0;
			conflict_pos = clv_checkConflict(opt_assignment, row_num, j);
		}
	}

	UNPROTECT(protect_num++);
	return opt_assignment_sxp;
}
