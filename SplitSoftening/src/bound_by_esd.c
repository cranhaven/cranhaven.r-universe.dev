/*
	This code implements the same thing like "probabilistic splits" in C4.5.
	see: J. Ross Quinlan, C4.5: programs for machine learning, Morgan Kaufmann Publishers Inc., 1993
	see: https://www.rulequest.com/Personal/c4.5r8.tar.gz
*/

#include "R.h"

#define Below(v,t) (v <= t + 1E-6)
#define Max(a,b) ((a)>(b) ? a : b) 
#define Min(a,b) ((a)<(b) ? a : b)
#define Boolean int
#define true 1
#define false 0

/**	Finds children of the given node. The parent node and the children
	are specified by an index to the array in param childref.
	The given node must not be a leaf (it's children must exist).
	It is required, that indices of children are greater than the index
	of their parent.
	param node is an index of the node, which's children are to be found.
	param left will be set to the index of the left son of the node.
	param right will be set to the index of the right son of the node.
		If left or right is NULL, the appropriate son is not searched.
	param childref describes tree structure
*/
void findChildren(int node, int *left, int *right, int *childref) {
	if ( left )
	{
		*left = childref[node] - 1;
	}
	if ( right )
	{
		*right = childref[node];
	}
}

/**	Classifies a case identified by the index using only a subtree.
	param datacase is an index to the data, identifying the case 
		to be classified.
	param branch is an index to the tree structure, identifying a root
		of the subtree (branch), that has to be used for classification.
	param data matrix of data
	param ndata number of the cases in data
	param dim dimension of data
	params var, splits, ncat, childref, yval, nclass: decision tree structure
	return index of the class
*/
int classInBranch(int datacase, int branch,
                  double *data, int *ndata, int *dim, int *treesize, int *var,
                  double *splits, int *ncat,
                  int *childref, double *yval, int *nclass)
{
	int sn = branch;
	while (var[sn] >= 0) {
		int *left = NULL;
		int *right = NULL;
		if ((data[ndata[0]*var[sn]+datacase]<=splits[sn])
			==(ncat[sn]==-1))
			left = &sn;
		else right = &sn;
		findChildren(sn, left, right, childref);
	}
	int maxClass = -1;
	double maxProb = .0;
	int cn;
	for (cn = 0; cn < nclass[0]; cn++) {
		if (yval[sn+cn*treesize[0]] > maxProb) { /*todo: yval[...] == maxProb */
			maxProb = yval[sn+cn*treesize[0]];
			maxClass = cn;
		}
	}
	return maxClass;
}
/**	Computes "lower bound" and "upper bound" values of soft tree
	using error standard deviation - as C4.5 does.
*/
void bounds_by_esd(double *data, int *class, int *ndata, int *dim,
                   int *treesize, int *var,
                   double *splits, int *ncat, /*double *lb, double *ub,*/
                   int *childref, double *yval, int *nclass, double *bounds)
{
	int ts_min = 0;
	int ts_count = ndata[0];
	int node, i, split_data_index;
	int Class1, Class2, leftBranch, rightBranch, lowersBranch, uppersBranch;
	double *for_sort = R_Calloc(ts_count, double); /* buffer for sorting */
	int *index = R_Calloc(ts_count, int); /* defines order of sorted data */
	int *local_index = R_Calloc(ts_count, int); /* used for sorting a part of data */
	int *aux_index = R_Calloc(ts_count, int);
	int split_count = treesize[0]/2;
	/* The following three arrays indicate for each split the data cases
	   that go through the split (in the nonsoft version of the tree)
	   as follows: for each i in 0..(split_data_index-1)
	     split_indices[i] is an index of the split in the tree
	     datarange_starts[i] is the first data case, appropriate to the split
	     (in the data ordering defined by the array index).
	     datarange_sizes[i] is the count of the data cases appropriate.
	   The value of split_data_index grows during tracing the tree
	   from 1 to split_count.
	*/ 
	int *split_indices = R_Calloc(split_count, int);
	int *datarange_starts = R_Calloc(split_count, int);
	int *datarange_sizes = R_Calloc(split_count, int);

	int *LHSErr = R_Calloc(ts_count, int);
	int *RHSErr = R_Calloc(ts_count, int);
	int *ThreshErrs = R_Calloc(ts_count, int);
	float Se, Limit, Lower, Upper;
	int Errors, BaseErrors, LastI;
	Boolean LeftThresh=false;

	/* Initialize */
	split_indices[0] = 0;
	datarange_starts[0] = 0;
	datarange_sizes[0] = ts_count;
	split_data_index = 1;
	for (i = 0; i < ts_count; i++) index[i] = i;
	memcpy(aux_index, index, ts_count*sizeof(int));
	/* Trace the tree from the root and compute softening parameters */
	for (node = 0; node < treesize[0]; node++) {
		if (var[node] == -1) continue; /* the node is a leaf */
		/* Search a record for the current split to get appropriate data */
		for (i = 0; split_indices[i] != node; i++) {
			if (i >= split_data_index ) {
				error("Internal error in bounds_by_esd.\n");
			}
		}
		ts_min = datarange_starts[i];
		ts_count = datarange_sizes[i];
		/* Prepare arrays and sort data by the variable tested in the split */
		for (i = 0; i < ts_count; i++) local_index[i] = i;
		for (i = ts_min; i < ts_min+ts_count; i++) {
			for_sort[i] = data[ndata[0]*var[node]+index[i]];
		}
		rsort_with_index(for_sort+ts_min, local_index, ts_count);
		for (i = 0; i < ts_count; i++) {
			index[ts_min+i] = aux_index[ts_min+local_index[i]];
		}
		memcpy(aux_index+ts_min, index+ts_min, ts_count*sizeof(int));
		/* Find and store data subsets for children of the split */
		findChildren(node, &leftBranch, &rightBranch, childref);
		for (i = 0; i < ts_count; i++) {
			if ((data[ndata[0]*var[node]+index[ts_min+i]]<=splits[node])
			!=(ncat[node]==-1)) /* the != is a dangerous XOR !!! */
			break;
		}
		lowersBranch = (ncat[node]==-1) ? leftBranch : rightBranch;
		uppersBranch = (ncat[node]==-1) ? rightBranch : leftBranch;
		if (var[lowersBranch] >= 0) {
			split_indices[split_data_index] = lowersBranch;
			datarange_starts[split_data_index] = ts_min;
			datarange_sizes[split_data_index] = i;
			split_data_index++;
		}
		if (var[uppersBranch] >= 0) {
			split_indices[split_data_index] = uppersBranch;
			datarange_starts[split_data_index] = ts_min+i;
			datarange_sizes[split_data_index] = ts_count-i;
			split_data_index++;
		}
		for (i = 0; i < ts_count; i++) {
			/*  See how this item would be classified if its
			value were on each side of the threshold  */

			Class1 = classInBranch(index[ts_min+i], leftBranch,
								   data, ndata, dim, treesize, var,
								   splits, ncat,
								   childref, yval, nclass);
			Class2 = classInBranch(index[ts_min+i], rightBranch,
								   data, ndata, dim, treesize, var,
								   splits, ncat,
								   childref, yval, nclass);
			LHSErr[i] = (Class1 != class[index[ts_min+i]] ? 1 : 0);
			RHSErr[i] = (Class2 != class[index[ts_min+i]] ? 1 : 0); 
		}
		/*  Set Errors to total errors if take above thresh branch,
			and BaseErrors to errors if threshold has original value  */

		Errors = BaseErrors = 0;
		for (i = 0; i < ts_count; i++) {
			Errors += RHSErr[i];

			if ( Below(data[ndata[0]*var[node]+index[ts_min+i]], splits[node]) )
			{
			BaseErrors += LHSErr[i];
			}
			else
			{
			BaseErrors += RHSErr[i];
			}
		}

		/*  Calculate standard deviation of the number of errors  */

		Se = sqrt((BaseErrors+0.5) * (ts_count-BaseErrors+0.5) / (ts_count+1));
		Limit = BaseErrors + Se;

		/*  Set ThreshErrs[i] to the no. of errors if the threshold were i  */

		for (i = 0; i < ts_count; i++) {
			ThreshErrs[i] = Errors = Errors + LHSErr[i] - RHSErr[i];
		}

		/*  Choose Lower and Upper so that if threshold were set to
			either, the number of items misclassified would be one
			standard deviation above BaseErrors  */

		LastI = 0;
		Lower = Min(splits[node], data[ndata[0]*var[node]+index[ts_min+LastI]]);
		Upper = Max(splits[node], data[ndata[0]*var[node]+index[ts_min+ts_count-1]]);
		while ( LastI < (ts_count-1) &&
				data[ndata[0]*var[node]+index[ts_min+LastI+1]]
			 == data[ndata[0]*var[node]+index[ts_min+LastI]] ) LastI++;

		while ( LastI < (ts_count-1) )
		{
			i = LastI + 1;
			while ( i < (ts_count-1) &&
					data[ndata[0]*var[node]+index[ts_min+i+1]]
				 == data[ndata[0]*var[node]+index[ts_min+i]] ) i++;

			if ( ! LeftThresh &&
			 ThreshErrs[LastI] > Limit &&
			 ThreshErrs[i] <= Limit &&
			 Below(data[ndata[0]*var[node]+index[ts_min+i]], splits[node]) )
			{
			Lower = data[ndata[0]*var[node]+index[ts_min+i]] -
				(data[ndata[0]*var[node]+index[ts_min+i]]
				 - data[ndata[0]*var[node]+index[ts_min+LastI]])
				 * (Limit - ThreshErrs[i]) /
				(ThreshErrs[LastI] - ThreshErrs[i]);
			LeftThresh = true;
			}
			else
			if ( ThreshErrs[LastI] <= Limit &&
			 ThreshErrs[i] > Limit &&
			 ! Below(data[ndata[0]*var[node]+index[ts_min+i]], splits[node]) )
			{
			Upper = data[ndata[0]*var[node]+index[ts_min+LastI]] +
				(data[ndata[0]*var[node]+index[ts_min+i]]
				- data[ndata[0]*var[node]+index[ts_min+LastI]])
				 * (Limit - ThreshErrs[LastI]) /
				(ThreshErrs[i] - ThreshErrs[LastI]);
			if ( Upper < splits[node] ) Upper = splits[node];
			}

			LastI = i;
		}

		bounds[node] = Lower;
		bounds[treesize[0]+node] = Upper;
	}
	R_Free(index);
	R_Free(local_index);
	R_Free(aux_index);
	R_Free(for_sort);
	R_Free(split_indices);
	R_Free(datarange_starts);
	R_Free(datarange_sizes);
	R_Free(LHSErr);
	R_Free(RHSErr);
	R_Free(ThreshErrs);
}

