#include <Rinternals.h>

SEXP shortestPathsInformation(SEXP sM){
/* INPUT
 * M: an adjacency matrix (in Fechnerian scaling context, matrices of the psychometric increments
 *    of the first and second kind 
 */

/****************************************************/
/* Get dimension of M                               */
	int nr = 0, nc = 0;
	SEXP sDim = getAttrib(sM, R_DimSymbol);
	if(isInteger(sDim) && LENGTH(sDim) == 2 &&
	   INTEGER(sDim)[0] == INTEGER(sDim)[1]){
		nr = INTEGER(sDim)[0]; 
		nc = INTEGER(sDim)[1];
	}else error("invalid dimensions");

/****************************************************/
/* initialization                                   */
 	int i, j, id, pred[nr][nc];
 	int nodeclosest, nodefrom;
 	int done_init[nc], done[nc]; //boolean 
	double M[nr][nc], distance[nr][nc], mindist; 
  
/****************************************************/
/* set init values                                  */ 
for(j=0; j < nc; j++){
	 done_init[j] = 0;

	for(i=0; i < nr; i++){
		M[i][j] = REAL(sM)[i+nr*j];
		distance[i][j] = -1; pred[i][j] = -1;
	}

} 

/****************************************************/
/* shortest paths algorithm                         */
/* nodefrom: a given source vertex (row stimulus) for which 
	to determine information about shortest paths to the 
	column stimuli (as target vertices) 
 */
 	for(id=0; id<nr; id++){
	// 	id = 0;
	// 
		for(j = 0; j<nc; j++){
			done[j] = done_init[j]; 
		} // end j
	
		nodefrom = id; distance[id][nodefrom] = 0.0;
	
	 	for(i=0; i<nr; i++){
	 		nodeclosest = -1;
			mindist = -1.0;
				for(j = 0; j < nc; j++){
					if( (done[j] == 0) && (distance[id][j] >= 0) && 
					 	(distance[id][j] <= mindist || mindist == -1) 
					   ){
						mindist = distance[id][j];
						nodeclosest = j;
				 	} // end if
			 	} // end j
			 	done[nodeclosest] = 1;			
			 	for(j = 0; j < nc; j++){
				 	if( (done[j] == 0) && ( (distance[id][j] == -1) ||
				 		(distance[id][j] > distance[id][nodeclosest] + M[nodeclosest][j]) )
				 	   ){		 			
					 	distance[id][j] = distance[id][nodeclosest] + M[nodeclosest][j];
					 	pred[id][j] = nodeclosest;
					 } // end if
			 	} // end j
		 	} // end i
	 	
	 } // end id

	/****************************************************/
	/* Organizing output                                */ 
	SEXP sOutDistance, sOutPredecessor, dims;
	SEXP result = PROTECT(allocVector(VECSXP,2));
	
	PROTECT(sOutDistance = allocVector(REALSXP, nr*nc));
	double *OutDistance = REAL(sOutDistance);
	PROTECT(sOutPredecessor = allocVector(REALSXP, nr*nc));
	double *OutPredecessor = REAL(sOutPredecessor);
				
	for(i = 0; i < nr; i++){
		for (j = 0; j < nc; j++) {
			OutDistance[i+nr*j] = distance[i][j];
			OutPredecessor[i+nr*j] = pred[i][j];
		}
	}
	
	PROTECT(dims = allocVector(INTSXP, 2));
	INTEGER(dims)[0] = nc; 
	INTEGER(dims)[1] = nr;
	setAttrib(sOutDistance, R_DimSymbol, dims);
	setAttrib(sOutPredecessor, R_DimSymbol, dims);
	
	SET_VECTOR_ELT(result, 0, sOutDistance);
	SET_VECTOR_ELT(result, 1, sOutPredecessor);
	
	UNPROTECT(4);
	
	return result;
}
