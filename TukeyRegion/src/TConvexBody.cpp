// TConvexBody.cpp
// By Pavlo Mozharovskyi
// Last changed 21.08.2017
// Functions for computing the region's elements

#include "TukeyRegion.h"

/* Return facets of the convex body, each defined by three vertices.          */
void getQHFacets(TMatrix &points, vector<TVariables> &facets, bool triangulate,
                 int* pExitcode){
  int n = points.size();int d = points[0].size();
	char *options;
	string tmpStr;
	if (d <= 3){
	  if (triangulate){
	    tmpStr = "qhull Qt Qs QbB Pp";
	    options = strdup(tmpStr.c_str());
	  }else{
	    tmpStr = "qhull Qs QbB Pp";
	    options = strdup(tmpStr.c_str());
	  }
	}else{
	  if (triangulate){
		  tmpStr = "qhull Qx Qt Qs QbB Pp";
	    options = strdup(tmpStr.c_str());
	  }else{
		  tmpStr = "qhull Qx Qs QbB Pp";
	    options = strdup(tmpStr.c_str());
	  }
	}
	double *data = new double[n * d];
	for (int i = 0; i < n; i++){
		for (int j = 0; j < d; j++){
			data[i * d + j] = points[i][j];
		}
	}
	*pExitcode = qh_new_qhull (d, n, data, false, options, NULL, NULL);
	//FILE *errfile = fopen("err.txt", "w");
	//FILE *tmpstdout = fopen("out.txt", "w");
	//int exitcode = qh_new_qhull (d, n, data, false, options,
  //                            tmpstdout, errfile);
	//fclose(tmpstdout);
	//fclose(errfile);
	//Rcout << exitcode << endl;
	if (!(*pExitcode)) {
	  //Rcout << qh num_facets << endl;
		facetT *facet;
		vertexT *vertex, **vertexp;
		int numFacets = qh num_facets;
		facets.resize(numFacets);
		//facets.resize(0);
		//Rcout << facets.size() << endl;
		qh_vertexneighbors();
		int i = 0;
		FORALLfacets {
			(facets)[i] = TVariables(0);
			//TVariables aFacet(0);
			FOREACHvertex_ (facet->vertices){
				(facets)[i].push_back(qh_pointid(vertex->point));
				//aFacet.push_back(qh_pointid(vertex->point));
			  //Rcout << qh_pointid(vertex->point) << " ";
			}
			//Rcout << facets.size() << endl;
			//facets.push_back(aFacet);
			i++;
			//Rcout << endl;
		}
	}
	qh_freeqhull(qh_ALL);
	free(options);
	//int a = facets.size();
	//cout << a << endl;
	delete[] data;
}

double getQHVolume(TMatrix &points, int* pExitcode){
  int n = points.size();int d = points[0].size();
  double *data = new double[n * d];
  for (int i = 0; i < n; i++){
    for (int j = 0; j < d; j++){
      data[i * d + j] = points[i][j];
    }
  }
  string tmpStr = "qhull FA";
  char* options = strdup(tmpStr.c_str());
  //FILE *errfile = fopen("err.txt", "w");
  //FILE *tmpstdout = fopen("out.txt", "w");
  *pExitcode = qh_new_qhull (d, n, data, false, options, NULL, NULL);
  //*pExitcode = qh_new_qhull (d, n, data, false, options, tmpstdout, errfile);
  //fclose(tmpstdout);
  //fclose(errfile);
  double vol = qh totvol;
  qh_freeqhull(qh_ALL);
  free(options);
  delete[] data;
  if (!(*pExitcode)){
    return vol;
  }else{
    return -1;//pExitcode;
  }
}

void getQHDelaunay(TMatrix &points, vector<TVariables> &facets,
                   vector<double> &volumes, int* pExitcode){
  int n = points.size();int d = points[0].size();
  double *data = new double[n * d];
  for (int i = 0; i < n; i++){
    for (int j = 0; j < d; j++){
      data[i * d + j] = points[i][j];
    }
  }
  char* options;
  string tmpStr;
  if (d <= 3){
    tmpStr = "qhull d Qt Fa Qbb Qx Pp QJ";
    options = strdup(tmpStr.c_str());
  }else{
    tmpStr = "qhull d Qt Fa Qbb Qx Qs Pp QJ";
    options = strdup(tmpStr.c_str());
  }
  //FILE *errfile = fopen("err.txt", "w");
  //FILE *tmpstdout = fopen("out.txt", "w");
  *pExitcode = qh_new_qhull (d, n, data, false, options, NULL, NULL);
  //*pExitcode = qh_new_qhull (d, n, data, false, options, tmpstdout, errfile);
  //fclose(tmpstdout);
  //fclose(errfile);
  //Rcout << *pExitcode << endl;
  if (!(*pExitcode)) {
    //Rcout << qh num_facets << endl;
    facetT *facet;
    vertexT *vertex, **vertexp;
    int numFacets = qh num_facets;
    facets.resize(numFacets);
    volumes.resize(numFacets);
    //Rcout << facets.size() << endl;
    qh_vertexneighbors();
    int i = 0;
    FORALLfacets {
      volumes[i] = facet->f.area;
      (facets)[i] = TVariables(0);
      FOREACHvertex_ (facet->vertices){
        (facets)[i].push_back(qh_pointid(vertex->point));
        //Rcout << qh_pointid(vertex->point) << " ";
      }
      //Rcout << facets.size() << endl;
      i++;
      //Rcout << endl;
    }
  }
  qh_freeqhull(qh_ALL);
  free(options);
  //int a = facets.size();
  //cout << a << endl;
  delete[] data;
}

void getQHVertices(TMatrix &points, TVariables &vertexIndices, int* pExitcode){
  vertexIndices.resize(0);
  int n = points.size();int d = points[0].size();
  char *options;
  string tmpStr;
  if (d <= 3){
    tmpStr = "qhull Qt QbB Pp";
    options = strdup(tmpStr.c_str());
  }else{
    tmpStr = "qhull Qt Qx Qs QbB Pp";
    options = strdup(tmpStr.c_str());
  }
  double *data = new double[n * d];
  for (int i = 0; i < n; i++){
    for (int j = 0; j < d; j++){
      data[i * d + j] = points[i][j];
    }
  }
  *pExitcode = qh_new_qhull (d, n, data, false, options, NULL, NULL);
  //FILE *errfile = fopen("err.txt", "w");
  //FILE *tmpstdout = fopen("out.txt", "w");
  //int exitcode = qh_new_qhull (d, n, data, false, options,
  //                            tmpstdout, errfile);
  //fclose(tmpstdout);
  //fclose(errfile);
  //Rcout << exitcode << endl;
  if (!(*pExitcode)){
    vertexT *vertex, **vertexp;
    vertexIndices.resize(qh num_vertices);
    int counter = 0;
    FORALLvertices{
      vertexIndices[counter++] = qh_pointid(vertex->point);
    }
  }
  qh_freeqhull(qh_ALL);
  free(options);
  delete[] data;
}

void getQHBarycenter(TMatrix &points,  TPoint &center, int* pExitcode){
  // Initialization
  int d = points[0].size();
  double totVol = 0;
  for (int j = 0; j < d; j++){
    center[j] = 0;
  }
  // Perform Delaunay triangulation of the convex hull
  vector<TVariables> facetMatrix;
  vector<double> volumes;
  getQHDelaunay(points, facetMatrix, volumes, pExitcode);
  // Weight-verage constituting simplices
  for (int i = 0; i < volumes.size(); i++){
    // Calculate simplex's center of gravity (average of verties)
    TPoint tmpCenter(d);
    for (int j = 0; j < d + 1; j++){
      for (int k = 0; k < d; k++){
        tmpCenter[k] += points[facetMatrix[i][j]][k];
      }
    }
    // ... and weight-add it to the center
    for (int j = 0; j < d; j++){
      center[j] += (tmpCenter[j] / (d + 1)) * volumes[i];
    }
    // Augment the total volume
    totVol += volumes[i];
  }
  // Divide through the total volume
  for (int i = 0; i < d; i++){
    center[i] /= totVol;
  }
}

int fitlerHalfspaces(TMatrix &X, vector<unsigned long long> &facets,
                     TPoint &center, TVariables &indicesHalfspacesNR){
  int n = X.size();int d = X[0].size();int numFacets = facets.size();
  // Obtain centred matrix
  TMatrix Y(n);
  for (int i = 0; i < n; i++){
    Y[i] = TPoint(d);
    for (int j = 0; j < d; j++){
      Y[i][j] = X[i][j] - center[j];
    }
  }
  // Obtain normed normals
  TMatrix normals(numFacets);
  for (int i = 0; i < numFacets; i++){ // For each facet candidate do:
    // - decode facet == obtain indices of the points from X it lies on
    TVariables pointIndices;
    getFacetPoints(facets[i], n, d, &pointIndices);
    // - get facet's normal
    TMatrix A(d - 1);for (int i = 0; i < d - 1; i++){A[i] = TPoint(d);}
    for (int j = 0; j < d - 1; j++){
      for (int k = 0; k < d; k++){
        A[j][k] = Y[pointIndices[j + 1]][k] - Y[pointIndices[0]][k];
      }
    }
    getNormal(A, &normals[i]);
    // - normalize facet's normal
    double tmpNorm = 0;
    for (int j = 0; j < d; j++){
      tmpNorm += pow(normals[i][j], 2);
    }
    tmpNorm = sqrt(tmpNorm);
    for (int j = 0; j < d; j++){
      normals[i][j] /= tmpNorm;
    }
    // - scale the facet and correct the facet's direcion
    double b = 0;
    for (int j = 0; j < d; j++){
      b += Y[pointIndices[0]][j] * normals[i][j];
    }
    for (int j = 0; j < d; j++){
      normals[i][j] /= b;
      //Rcout << normals[i][j] << " ";
    }
    //Rcout << endl;
  }
  // Identify nonredundant hyperplanes
  int exitcode = -1;
  getQHVertices(normals, indicesHalfspacesNR, &exitcode);
  //Rcout << "getQHVertices(exitcode): " << exitcode << endl;
  return indicesHalfspacesNR.size();
}

/* By Rainer Dyckerhoff, slightly modified by Pavlo Mozharovskyi              */
/* Solves a uniquely solvable system of linear equations                      */
bool solveUnique(TMatrix A, TPoint b, TPoint *x){
	int imax,jmax;
	int d = A[0].size();
	int* colp = new int[d];
	x->resize(d);
	double amax;
	for (int k = 0; k < d - 1; k++) {
		imax = k;
		amax = fabs(A[k][k]);
		colp[k] = k;
		// Spaltenmaximum finden
		for (int i = k + 1; i < d; i++) {
			if (fabs(A[i][k]) > amax) {
				amax = fabs(A[i][k]);
				imax = i;
			}
		}
		// Spaltenmaximum gleich null => complete pivoting
		if (amax < eps_pivot) {
			for (int j = k + 1; j < d; j++) {
				for (int i = k; i < d; i++) {
					if (fabs(A[i][j]) > amax) {
						amax = fabs(A[i][j]);
						imax = i;
						jmax = j;
					}
				}
			}
			if (amax < eps_pivot) {
				delete[] colp;
				return false;
			}
			// Spaltentausch
			for (int i = 0; i < d; i++) {
				double tmp = A[i][k];
				A[i][k] = A[i][jmax];
				A[i][jmax] = tmp;
			}
			colp[k] = jmax;
		}
		// Zeilentausch
		if (imax != k) {
			for (int j = k; j < d; j++) {
				double tmp = A[k][j];
				A[k][j] = A[imax][j];
				A[imax][j] = tmp;
			}
			double tmp = b[k];
			b[k] = b[imax];
			b[imax] = tmp;
		}
		// Elimination
		for (int i = k + 1; i < d; i++) {
			double factor = A[i][k] / A[k][k];
			for (int j = k + 1; j < d; j++){
				A[i][j] -= factor * A[k][j];
			}
			b[i] -= factor * b[k];
		}
	}
	// Ruecksubstituition
	colp[d - 1] = d - 1;
	for (int k = d - 1; k >= 0; k--) {
		(*x)[k] = b[k] / A[k][k];
		for (int i = k - 1; i >= 0; i--) b[i] -= (*x)[k] * A[i][k];
	}
	// Spaltenvertauschungen rueckgaengig machen
	for (int k = d - 1; k >= 0; k--) {
		if (colp[k] != k) {
			double temp = (*x)[k];
			(*x)[k] = (*x)[colp[k]];
			(*x)[colp[k]] = temp;
		}
	}
	delete[] colp;
	return true;
}

int CompareVectors(TPoint a, TPoint b){
	int i = 0;int d = a.size();
	while (i < d - 1 && fabs(a[i] - b[i]) < eps){i++;}
	return a[i] < b[i];
}

/* Return vertices (without order) of the resulting convex body defined as an */
/* intersection of the n-d-coded facets from 'facets'.                        */
/* Inner point is supposed to be the average                                  */
unsigned long long getVertices(TMatrix &X, vector<unsigned long long> &facets,
	TMatrix *vertices){
	int n = X.size();int d = X[0].size();int numFacets = facets.size();
	vertices->resize(0);
	// Obtain center
	TPoint center(d);
	for (int i = 0; i < n; i++){
		for (int j = 0; j < d; j++){
			center[j] += X[i][j];
		}
	}
	for (int j = 0; j < d; j++){
		center[j] /= n;
	}
	// Obtain centred matrix
	TMatrix Y(n);
	for (int i = 0; i < n; i++){
		Y[i] = TPoint(d);
		for (int j = 0; j < d; j++){
			Y[i][j] = X[i][j] - center[j];
		}
	}
	// Obtain normed normals
	TMatrix normals(numFacets);
	for (int i = 0; i < numFacets; i++){ // For each facet candidate do:
		// - decode facet == obtain indices of the points from X it lies on
		TVariables pointIndices;
		getFacetPoints(facets[i], n, d, &pointIndices);
		// - get facet's normal
		TMatrix A(d - 1);for (int i = 0; i < d - 1; i++){A[i] = TPoint(d);}
		for (int j = 0; j < d - 1; j++){
			for (int k = 0; k < d; k++){
				A[j][k] = Y[pointIndices[j + 1]][k] - Y[pointIndices[0]][k];
			}
		}
		getNormal(A, &normals[i]);
		// - normalize facet's normal
		double tmpNorm = 0;
		for (int j = 0; j < d; j++){
			tmpNorm += pow(normals[i][j], 2);
		}
		tmpNorm = sqrt(tmpNorm);
		for (int j = 0; j < d; j++){
			normals[i][j] /= tmpNorm;
		}
		// - scale the facet and correct the facet's direcion
		double b = 0;
		for (int j = 0; j < d; j++){
			b += Y[pointIndices[0]][j] * normals[i][j];
		}
		for (int j = 0; j < d; j++){
			normals[i][j] /= b;
		}
	}
	// Obtain the vertices of the convex body
	vector<TVariables> vertexBundles;
	int exitcode;
	getQHFacets(normals, vertexBundles, true, &exitcode);
	int numVertices = vertexBundles.size();
	TMatrix tmpVertices(numVertices);
	TPoint b(d, 1);
	TMatrix A(d);for (int i = 0; i < d; i++){A[i] = TPoint(d);}
	for (int i = 0; i < numVertices; i++){
		for (int j = 0; j < d; j++){
			for (int k = 0; k < d; k++){
				A[j][k] = normals[vertexBundles[i][j]][k];
			}
		}
		solveUnique(A, b, &tmpVertices[i]);
	}
	// Filter unique vertices
	sort(tmpVertices.begin(), tmpVertices.end(), CompareVectors);
	vertices->resize(0);
	vertices->push_back(tmpVertices[0]);
	int prevIndex = 0;
	for (int i = 1; i < numVertices; i++){
		bool equalToPrev = true;
		for (int j = 0; j < d; j++){
			if (fabs(tmpVertices[prevIndex][j] - tmpVertices[i][j]) > eps){
				equalToPrev = false;break;
			}
		}
		if (!equalToPrev){
			vertices->push_back(tmpVertices[i]);
			prevIndex = i;
		}
	}
	// Shift back
	for (int i = 0; i < vertices->size(); i++){
		for (int j = 0; j < d; j++){
			(*vertices)[i][j] += center[j];
		}
	}
	// Return the number of vertices of the resulting convex body
	return vertices->size();
}

/* Return vertices (without order) of the resulting convex body defined as an */
/* intersection of the n-d-coded facets from 'facets'.                        */
/* Inner Point ('center') should be provide                                   */
unsigned long long getVertices(TMatrix &X, vector<unsigned long long> &facets,
                               TPoint &center, TMatrix *vertices){
  int n = X.size();int d = X[0].size();int numFacets = facets.size();
  vertices->resize(0);
  // Obtain centred matrix
  TMatrix Y(n);
  for (int i = 0; i < n; i++){
    Y[i] = TPoint(d);
    for (int j = 0; j < d; j++){
      Y[i][j] = X[i][j] - center[j];
    }
  }
  // Obtain normed normals
  TMatrix normals(numFacets);
  for (int i = 0; i < numFacets; i++){ // For each facet candidate do:
    // - decode facet == obtain indices of the points from X it lies on
    TVariables pointIndices;
    getFacetPoints(facets[i], n, d, &pointIndices);
    // - get facet's normal
    TMatrix A(d - 1);for (int i = 0; i < d - 1; i++){A[i] = TPoint(d);}
    for (int j = 0; j < d - 1; j++){
      for (int k = 0; k < d; k++){
        A[j][k] = Y[pointIndices[j + 1]][k] - Y[pointIndices[0]][k];
      }
    }
    getNormal(A, &normals[i]);
    // - normalize facet's normal
    double tmpNorm = 0;
    for (int j = 0; j < d; j++){
      tmpNorm += pow(normals[i][j], 2);
    }
    tmpNorm = sqrt(tmpNorm);
    for (int j = 0; j < d; j++){
      normals[i][j] /= tmpNorm;
    }
    // - scale the facet and correct the facet's direcion
    double b = 0;
    for (int j = 0; j < d; j++){
      b += Y[pointIndices[0]][j] * normals[i][j];
    }
    for (int j = 0; j < d; j++){
      normals[i][j] /= b;
    }
  }
  // Identify vertices as intersection of nonredundant hyperplanes
  vector<TVariables> vertexBundles;
  int exitcode;
  getQHFacets(normals, vertexBundles, false, &exitcode);
  //Rcout << vertexBundles.size() << " vertices identified." << endl;
  // Remove vertex bundles involving more than 'd' hyperplanes
  //TVariables toRemove(0);
  //for (int i = 0; i < vertexBundles.size(); i++){
    //for (int j = 0; j < vertexBundles[i].size(); j++){
      //Rcout << vertexBundles[i][j] << " ";
    //}
    //Rcout << endl;
    //if (vertexBundles[i].size() > d){
      //toRemove.push_back(i);
    //}
  //}
  //sort(toRemove.begin(), toRemove.end());
  //for (int i = toRemove.size() - 1; i >= 0; i--){
    //vertexBundles.erase(vertexBundles.begin() + toRemove[i]);
  //}
  // Obtain the vertices of the convex body
  int numVertices = vertexBundles.size();
  TMatrix tmpVertices(numVertices);
  TPoint b(d, 1);
  TMatrix A(d);for (int i = 0; i < d; i++){A[i] = TPoint(d);}
  for (int i = 0; i < numVertices; i++){
    //if (vertexBundles[i].size() == d){
      // If a non-degenerate point
      for (int j = 0; j < d; j++){
        for (int k = 0; k < d; k++){
          A[j][k] = normals[vertexBundles[i][j]][k];
        }
      }
      solveUnique(A, b, &tmpVertices[i]);
    //}else{
    if (vertexBundles[i].size() > d){
      // Search for a non-degenarate (d)-tuple of hyperplanes. For this :
      // Loop through combinations = (d)-tuples
      int nHplns = vertexBundles[i].size();
      TVariables counters(d);
      for (int j = 0; j < d - 1; j++){counters[j] = j;}counters[d - 1] = d - 2;
      //Rcout << "Loop through-over " << nHplns << " combinations." << endl;
      while (counters[0] != nHplns - d){ // Get current combination
        int j = d - 1;
        while (j > 0 && counters[j] == nHplns - d + j){j--;}
        counters[j]++;int k = j + 1;
        while (k < d){counters[k] = counters[k - 1] + 1;k++;}
        // For the current (d)-tuple do:
        // a) Prepare linear equations' system
        for (int k = 0; k < d; k++){
          for (int l = 0; l < d; l++){
            A[k][l] = normals[vertexBundles[i][counters[k]]][l];
          }
        }
        // b) Solve linear equations' system
        solveUnique(A, b, &tmpVertices[i]);
        //for (int k = 0; k < d; k++){
          //Rcout << tmpVertices[i][k] << " ";
        //}
        // c) Check the solution
        bool notInfNan = true;
        for (int k = 0; k < d; k++){
          if(std::isinf(tmpVertices[i][k]) || std::isnan(tmpVertices[i][k])){
            notInfNan = false;
            break;
          }
        }
        //Rcout << notInfNan << " ";
        bool isInside = true;
        if (notInfNan){
          for (int k = 0; k < normals.size(); k++){
            double tmpSum = 0;
            for (int l = 0; l < d; l++){
              tmpSum += normals[k][l] * tmpVertices[i][l];
            }
            if (tmpSum - eps > 1.){
              isInside = false;
              break;
            }
          }
        }else{
          isInside = false;
        }
        //Rcout << isInside;
        //Rcout << endl;
        // d) If the solution contains neither Infs nor Nans,
        // ... go to the following vertex
        if (isInside){
          break;
        }
      }
    }
  }
  // Filter unique vertices
  sort(tmpVertices.begin(), tmpVertices.end(), CompareVectors);
  vertices->resize(0);
  vertices->push_back(tmpVertices[0]);
  int prevIndex = 0;
  for (int i = 1; i < numVertices; i++){
    bool equalToPrev = true;
    for (int j = 0; j < d; j++){
      if (fabs(tmpVertices[prevIndex][j] - tmpVertices[i][j]) > eps){
        equalToPrev = false;break;
      }
    }
    if (!equalToPrev){
      vertices->push_back(tmpVertices[i]);
      prevIndex = i;
    }
  }
  // Remove vertex bundles containing Inf or NaN
  TVariables toRemove(0);
  //toRemove.resize(0);
  for (int i = 0; i < vertices->size(); i++){
    for (int j = 0; j < (*vertices)[i].size(); j++){
      if (std::isinf((*vertices)[i][j]) || std::isnan((*vertices)[i][j])){
        toRemove.push_back(i);
        break;
      }
      //Rcout << vertices[i][j] << " ";
    }
    //Rcout << endl;
    //if (vertices[i].size() > d){
      //toRemove.push_back(i);
    //}
  }
  sort(toRemove.begin(), toRemove.end());
  for (int i = toRemove.size() - 1; i >= 0; i--){
    vertices->erase(vertices->begin() + toRemove[i]);
  }
  // Shift back
  for (int i = 0; i < vertices->size(); i++){
    for (int j = 0; j < d; j++){
      (*vertices)[i][j] += center[j];
    }
  }
  // Return the number of vertices of the resulting convex body
  return vertices->size();
  return 0;
}

unsigned long long getFacets(TMatrix &X, vector<unsigned long long> &halfspaces,
                               TPoint &center, TVariables *facets){
  int n = X.size();int d = X[0].size();int numHalfspaces = halfspaces.size();
  // Obtain centred matrix
  TMatrix Y(n);
  for (int i = 0; i < n; i++){
    Y[i] = TPoint(d);
    for (int j = 0; j < d; j++){
      Y[i][j] = X[i][j] - center[j];
    }
  }
  // Obtain normed normals
  TMatrix normals(numHalfspaces);
  for (int i = 0; i < numHalfspaces; i++){ // For each facet candidate do:
    // - decode facet == obtain indices of the points from X it lies on
    TVariables pointIndices;
    getFacetPoints(halfspaces[i], n, d, &pointIndices);
    // - get facet's normal
    TMatrix A(d - 1);for (int i = 0; i < d - 1; i++){A[i] = TPoint(d);}
    for (int j = 0; j < d - 1; j++){
      for (int k = 0; k < d; k++){
        A[j][k] = Y[pointIndices[j + 1]][k] - Y[pointIndices[0]][k];
      }
    }
    getNormal(A, &normals[i]);
    // - normalize facet's normal
    double tmpNorm = 0;
    for (int j = 0; j < d; j++){
      tmpNorm += pow(normals[i][j], 2);
    }
    tmpNorm = sqrt(tmpNorm);
    for (int j = 0; j < d; j++){
      normals[i][j] /= tmpNorm;
    }
    // - scale the facet and correct the facet's direcion
    double b = 0;
    for (int j = 0; j < d; j++){
      b += Y[pointIndices[0]][j] * normals[i][j];
    }
    for (int j = 0; j < d; j++){
      normals[i][j] /= b;
    }
  }
  // Get (nonredundant) facets of the region
  double* normalsRaw = new double[numHalfspaces * d];
  for (int i = 0; i < numHalfspaces; i++){
    for (int j = 0; j < d; j++){
      normalsRaw[i * d + j] = normals[i][j];
    }
  }
  int* vertexIndices = new int[numHalfspaces + 1];
  convhull(normalsRaw, numHalfspaces, d, vertexIndices);
  facets->resize(vertexIndices[0]);
  for (int i = 0; i < vertexIndices[0]; i++){
    (*facets)[i] = vertexIndices[i + 1];
  }
  delete[] vertexIndices;
  delete[] normalsRaw;
  return facets->size();
}

/* Return vertices (without order) of the resulting convex body defined as an */
/* intersection of halfspaces with normals 'normals' and borders 'bs'.        */
/* Inner Point ('center') should be provide                                   */
unsigned long long getVertices(TMatrix &normals, TPoint &bs, TPoint &center,
                               TMatrix *vertices){
  int d = normals[0].size();int numFacets = normals.size();
  vertices->resize(0);
  // Obtain normed normals
  TMatrix curNormals(numFacets);
  for (int i = 0; i < numFacets; i++){ // For each facet candidate do:
    curNormals[i].resize(d);
    // - substract the center
    double curB = 0;
    for (int j = 0; j < d; j++){
      curB += normals[i][j] * center[j];
    }
    curB = bs[i] - curB;
    // - scale the facet and correct the facet's direcion
    for (int j = 0; j < d; j++){
      curNormals[i][j] = normals[i][j] / curB;
    }
  }
  // Obtain the vertices of the convex body
  vector<TVariables> vertexBundles;
  // cout << "Start qhull ... ";
  int exitcode;
  getQHFacets(curNormals, vertexBundles, true, &exitcode);
  // cout << "end." << endl;
  int numVertices = vertexBundles.size();
  TMatrix tmpVertices(numVertices);
  TPoint b(d, 1);
  TMatrix A(d);for (int i = 0; i < d; i++){A[i] = TPoint(d);}
  for (int i = 0; i < numVertices; i++){
    for (int j = 0; j < d; j++){
      for (int k = 0; k < d; k++){
        A[j][k] = normals[vertexBundles[i][j]][k];
      }
    }
    solveUnique(A, b, &tmpVertices[i]);
  }
  // Filter unique vertices
  sort(tmpVertices.begin(), tmpVertices.end(), CompareVectors);
  vertices->resize(0);
  vertices->push_back(tmpVertices[0]);
  int prevIndex = 0;
  for (int i = 1; i < numVertices; i++){
    bool equalToPrev = true;
    for (int j = 0; j < d; j++){
      if (fabs(tmpVertices[prevIndex][j] - tmpVertices[i][j]) > eps){
        equalToPrev = false;break;
      }
    }
    if (!equalToPrev){
      vertices->push_back(tmpVertices[i]);
      prevIndex = i;
    }
  }
  // Shift back
  for (int i = 0; i < vertices->size(); i++){
    for (int j = 0; j < d; j++){
      (*vertices)[i][j] += center[j];
    }
  }
  // Return the number of vertices of the resulting convex body
  return vertices->size();
}

/* Obtain halfspaces shaping the Tukey region                                 */
int getHalfspaces(TMatrix &X, int intTau, vector<unsigned long long> &facets,
                  TMatrix *normals, TPoint *bs){
  int n = X.size();int d = X[0].size();int numFacets = facets.size();
  // Obtain normed normals
  normals->resize(numFacets);
  bs->resize(numFacets);
  for (int i = 0; i < numFacets; i++){ // For each facet candidate do:
    // - decode facet == obtain indices of the points from X it lies on
    TVariables pointIndices(d);
    getFacetPoints(facets[i], n, d, &pointIndices);
    // - get facet's normal
    TMatrix A(d - 1);for (int i = 0; i < d - 1; i++){A[i] = TPoint(d);}
    for (int j = 0; j < d - 1; j++){
      for (int k = 0; k < d; k++){
        A[j][k] = X[pointIndices[j + 1]][k] - X[pointIndices[0]][k];
      }
    }
    getNormal(A, &(*normals)[i]);
    // - normalize facet's normal
    double tmpNorm = 0;
    for (int j = 0; j < d; j++){
      tmpNorm += pow((*normals)[i][j], 2);
    }
    tmpNorm = sqrt(tmpNorm);
    for (int j = 0; j < d; j++){
      (*normals)[i][j] /= tmpNorm;
    }
    // - obtain b
    (*bs)[i] = 0;
    for (int j = 0; j < d; j++){
      (*bs)[i] += X[pointIndices[0]][j] * (*normals)[i][j];
    }
    // - direct the normal ouwards
    int numAbove = 0;
    int numBelow = 0;
    for (int j = 0; j < n; j++){
      double tmp = 0;
      for (int k = 0; k < d; k++){
        tmp += X[j][k] * (*normals)[i][k];
      }
      if (tmp - eps > (*bs)[i]){numAbove++;}else{numBelow++;}
      // if (tmp > (*bs)[i]){numAbove++;}else{numBelow++;} // REMOVE!!!
    }
    // Rcout << numAbove << " above and " << numBelow << " below" << endl;
    if ((numAbove >= numBelow) ||
    ((n - d - intTau <= d + intTau) && (numAbove > intTau))){ // change the direction if necessary
    //if (numAbove >= numBelow){ //REMOVE!!!
      for (int j = 0; j < d; j++){
        (*normals)[i][j] = -(*normals)[i][j];
      }
      (*bs)[i] = -(*bs)[i];
    }
  }
  return 0;
}

/* Obtain inner point of the region                                           */
bool getInnerPoint(TMatrix &normals, TPoint &bs, TPoint *innerPoint){
  // Initialize
  int d = normals[0].size();//int numFacets = normals.size();
  innerPoint->resize(0); // DEBUG!!!
  innerPoint->resize(d);
  bool exists = true;
  // Find inner point by averaging
  TPoint obj = TPoint(d);
  //for (int i = 0; i < 2 * d; i++){
  //for (int i = 0; i < 2; i++){
  int i = 0;
    // Choose an axis (and direction)
    if (i % 2 == 0){
      obj[i/2] = -1.;
    }else{
      obj[i/2] = 1.;
    }
    // Add the border point in (inverse) axis direction to the average
    TPoint tmp;
    TPoint newBs(normals.size());
    for (int i = 0; i < normals.size(); i++){
      newBs[i] = bs[i] - sqrt(eps);
    }
    int res = solveLP(obj, normals, newBs, &tmp); // find the border point
    if (res){ // fail
      //Rcout << "Intersection of the regions halfspaces is empty" << endl;
      return false;
    }else{
      //Rcout << "Intersection of the regions halfspaces is not empty" << endl;
    }
    //Rcout << "lpPoint: ";
    for (int j = 0; j < d; j++){
      (*innerPoint)[j] += tmp[j];
      //Rcout << tmp[j] << " ";
    }
    //Rcout << endl;
    // Restore the axis
    obj[i/2] = 0;
  //}
  // Calculate the inner point
  //Rcout << "Inner point: ";
  for (int j = 0; j < d; j++){
    //(*innerPoint)[j] /= (double)(2 * d);
    //(*innerPoint)[j] /= (double)(2);
    //Rcout << (*innerPoint)[j] << " ";
  }
  //Rcout << endl;
  // Check whether it is not on a facet
  TVariables iNormals(0);
  double minEps = -DBL_MIN;
  checkInnerPoint(normals, bs, *innerPoint, iNormals, minEps);
  //Rcout << "Initial inner point precision: " << minEps << endl;
  // Print binding halfspaces
  //for (int i = 0; i < iNormals.size(); i++){
  //  for (int j = 0; j < d; j++){
  //    Rcout << normals[iNormals[i]][j] << " ";
  //  }
  //  Rcout << ": " << bs[iNormals[i]] << endl;
  //}
  if (minEps > sqrt(eps) / 2){
    //if (minEps > 0){
    //Rcout << "Inner point found immediately!" << endl;
    return true;
  }
  // Detect an unexplained anomaly
  if (iNormals.size() == 0){
    //stop("Unexplained anomaly with the inner point");
    Rcout << "Unexplained anomaly with the inner point" << endl;
    return false;
  }
  // Adjust inner point if necessary
  TVariables tmpINormals(0);
  double curDistance = 2; // start being far from the facet of singularity
  // Take 'innerPoint' as the starting point
  TPoint oldInnerPoint(*innerPoint);
  // Find the direction in which to move 'innerPoint'
  TPoint theNormal(d); // adjustment direction
  for (int j = 0; j < d; j++){
    for (int i = 0; i < iNormals.size(); i++){
      theNormal[j] += normals[iNormals[i]][j];
    }
    theNormal[j] /= iNormals.size();
  }
  //Rcout << "Number of binding constraints: " << iNormals.size() << endl;
  // While there is room for adjustment
  while(curDistance > sqrt(eps) / 2){
    // Compute new inner point and check it
    for (int j = 0; j < d; j++){
      //(*innerPoint)[j] = normals[iNormal][j] * (bs[iNormal] - curDistance);
      (*innerPoint)[j] = oldInnerPoint[j] - theNormal[j] * curDistance;
    }
    checkInnerPoint(normals, bs, *innerPoint, tmpINormals, minEps);
    //Rcout << "Inner point precision: " << minEps << endl;
    curDistance /= (double)2; // move inner point two times closer to the facet
    if (minEps > sqrt(eps) / 2){
      break;
    }
  }
  // Perform final computation and check of the inner point (to be sure)
  // Compute new inner point and check it
  for (int j = 0; j < d; j++){
    //(*innerPoint)[j] = normals[iNormal][j] * (bs[iNormal] - curDistance);
    (*innerPoint)[j] = oldInnerPoint[j] - theNormal[j] * curDistance;
  }
  checkInnerPoint(normals, bs, *innerPoint, tmpINormals, minEps);
  //Rcout << "Inner point precision: " << minEps << endl;
  if (minEps > sqrt(eps) / 2){
    return true; // success
  }else{
    return false; // the convex body is probably singular
  }
}

/* Check whether a point is in the interrior of the halfspace intersection    */
bool checkInnerPoint(TMatrix &normals, TPoint &bs, TPoint &innerPoint,
                     TVariables &iNormals, double &minEps){
  // Initialize the minimum distance to the facet
  minEps = DBL_MAX;
  int d = normals[0].size();
  // For each normal ...
  for (int i = 0; i < normals.size(); i++){
    // Calculate the position of the 'innerPoint' on it
    double tmpPrj = 0;
    for (int j = 0; j < d; j++){
      tmpPrj += normals[i][j] * innerPoint[j];
    }
    // Get teh distance to the hyperplane
    double curEps = bs[i] - tmpPrj;
    // Add the normal 'innerPoint' is too close to
    if (curEps < eps){
      iNormals.push_back(i);
    }
    // Update if the smallest
    if (curEps < minEps){
      minEps = curEps;
    }
  }
  // Deside whether thi is an inner point
  if (minEps < 0){
    return false;
  }else{
    return true;
  }
}
