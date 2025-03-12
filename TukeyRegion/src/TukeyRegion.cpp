// TukeyRegion.cpp
// By Pavlo Mozharovskyi
// Last changed 06.12.2017
// C++-callable functions for computing the Tukey region and its elements

#include "TukeyRegion.h"

//#define EOF (-1)

//#ifdef __cplusplus
//extern "C" {
//#endif

/* Export functions --------------------------------------------------------- */

//__declspec( dllexport ) void __cdecl TukeyRegionBFS(double *data, int *n, int *d, double *tau, int* numFacets, int* facets){
void TukeyRegionBFS(double *data, int *n, int *d, double *tau, int* numFacets, int* facets){
  int intDepth = floor(*tau * *n + 1./(10 * *n));
  TMatrix X(*n);
  TransformData(data, *n, *d, &X);
  vector<unsigned long long> halfspacesBFS;
  int numRidges = 0;
  TRegion(X, intDepth, 3, 1, &halfspacesBFS, &numRidges);
  sort(halfspacesBFS.begin(), halfspacesBFS.end());
  *numFacets = halfspacesBFS.size();
  for (int i = 0; i < halfspacesBFS.size(); i++){
    TVariables pointNumbers;
    getFacetPoints(halfspacesBFS[i], *n, *d, &pointNumbers);
    for (int j = 0; j < *d; j++){
      facets[i**d + j] = pointNumbers[j];
    }
  }
}

//__declspec( dllexport ) void __cdecl TukeyRegionCmb(double *data, int *n, int *d, double *tau, int* numFacets, int* facets){
void TukeyRegionCmb(double *data, int *n, int *d, double *tau, int* numFacets, int* facets){
  int intDepth = floor(*tau * *n + 1./(10 * *n));
  TMatrix X(*n);
  TransformData(data, *n, *d, &X);
  vector<unsigned long long> halfspacesCmb;
  TRegionCmb(X, intDepth, &halfspacesCmb);
  sort(halfspacesCmb.begin(), halfspacesCmb.end());
  *numFacets = halfspacesCmb.size();
  for (int i = 0; i < halfspacesCmb.size(); i++){
    TVariables pointNumbers;
    getFacetPoints(halfspacesCmb[i], *n, *d, &pointNumbers);
    for (int j = 0; j < *d; j++){
      facets[i**d + j] = pointNumbers[j];
    }
  }
}

//__declspec( dllexport ) void __cdecl TukeyRegionBruteForce(double *data, int *n, int *d, double *tau, int* numFacets, int* facets){
void TukeyRegionBruteForce(double *data, int *n, int *d, double *tau, int* numFacets, int* facets){
  int intDepth = floor(*tau * *n + 1./(10 * *n));
  TMatrix X(*n);
  TransformData(data, *n, *d, &X);
  vector<unsigned long long> halfspacesBF;
  TRegionBruteForce(X, intDepth, &halfspacesBF);
  sort(halfspacesBF.begin(), halfspacesBF.end());
  *numFacets = halfspacesBF.size();
  for (int i = 0; i < halfspacesBF.size(); i++){
    TVariables pointNumbers;
    getFacetPoints(halfspacesBF[i], *n, *d, &pointNumbers);
    for (int j = 0; j < *d; j++){
      facets[i**d + j] = pointNumbers[j];
    }
  }
}

//__declspec( dllexport ) void __cdecl TukeyRegionBFSVertices(double *data, int *n, int *d, double *tau, int *numFacets, int *numVertices, double *vertices, double *volume, int* found){
void TukeyRegionBFSVertices(double *data, int *n, int *d, double *tau, int *numHalfsaces, int *numFacets, int *numVertices, double *vertices, double *volume, int* found){
  int intDepth = floor(*tau * *n + 1./(10 * *n));
  vector<vector<double> > X(*n);
  TransformData(data, *n, *d, &X);
  vector<unsigned long long> halfspacesBFS;
  if (*d == 2){
    TRegionBruteForce(X, intDepth, &halfspacesBFS);
  }else{
    int numRidges = 0;
    TRegion(X, intDepth, 3, 1, &halfspacesBFS, &numRidges);
  }
  *numHalfsaces = halfspacesBFS.size();
  vector<vector<double> > normals;
  vector<double> bs;
  getHalfspaces(X, intDepth - 1, halfspacesBFS, &normals, &bs);
  vector<double> innerPoint(*d);
  for (int i = 0; i < *n; i++){
    for (int j = 0; j < *d; j++){
      innerPoint[j] += X[i][j];
    }
  }
  for (int i = 0; i < *d; i++){
    innerPoint[i] /= (double)(*n);
  }
  int res = getInnerPoint(normals, bs, &innerPoint);
  if (res){
    *found = 0;
  }else{
    *found = 1;
    TMatrix vertexMatrix;
    *numVertices = (int)getVertices(X, halfspacesBFS, innerPoint, &vertexMatrix);
    for (int i = 0; i < *numVertices; i++){
    	for (int j = 0; j < *d; j++){
    		vertices[i * *d + j] = vertexMatrix[i][j];
    	}
    }
    vector<TVariables> facets;
    int exitcode;
    getQHFacets(vertexMatrix, facets, false, &exitcode);
    *numFacets = facets.size();
    *volume = convvol(vertices, *numVertices, *d);
  }
}

void TukeyRegionBFSStats(double *data, int *n, int *d, double *tau, int *numHalfsaces, int *numFacets, int *found){
  int intDepth = floor(*tau * *n + 1./(10 * *n));
  vector<vector<double> > X(*n);
  TransformData(data, *n, *d, &X);
  vector<unsigned long long> halfspacesBFS;
  if (*d == 2){
    TRegionBruteForce(X, intDepth, &halfspacesBFS);
  }else{
    int numRidges = 0;
    TRegion(X, intDepth, 3, 1, &halfspacesBFS, &numRidges);
  }
  *found = 1;
  //cout << "Halfspaces calculates" << endl;
  *numHalfsaces = halfspacesBFS.size();
  vector<double> innerPoint(*d);
  for (int i = 0; i < *n; i++){
    for (int j = 0; j < *d; j++){
      innerPoint[j] += X[i][j];
    }
  }
  for (int i = 0; i < *d; i++){
    innerPoint[i] /= (double)(*n);
  }
  //vector<vector<double> > normals;
  //vector<double> bs;
  //getHalfspaces(X, halfspacesBFS, &normals, &bs);
  //int res = getInnerPoint(normals, bs, &innerPoint);
  int res = 0;
  if (res){
    *found = 0;
  }else{
    *found = 1;
    TVariables facets;
    *numFacets = (int)getFacets(X, halfspacesBFS, innerPoint, &facets);
  }
  //cout << "Facets obtained" << endl;
}

void TrBfsHfsp(double *data, int *n, int *d, double *tau, int *numHalfspaces, int *halfspaces){
  int intDepth = floor(*tau * *n + 1./(10 * *n));
  vector<vector<double> > X(*n);
  TransformData(data, *n, *d, &X);
  vector<unsigned long long> halfspacesBFS;
  if (*d == 2){
    TRegionBruteForce(X, intDepth, &halfspacesBFS);
  }else{
    int numRidges = 0;
    TRegion(X, intDepth, 3, 1, &halfspacesBFS, &numRidges);
  }
  *numHalfspaces = halfspacesBFS.size();
  TVariables pointIndices(*d);
  for (int i = 0; i < *numHalfspaces; i++){
    getFacetPoints(halfspacesBFS[i], *n, *d, &pointIndices);
    for (int j = 0; j < *d; j++){
      halfspaces[i * *d + j] = pointIndices[j] + 1;
    }
  }
}

void TrCmbHfsp(double *data, int *n, int *d, double *tau, int *numHalfspaces, int *halfspaces){
  int intDepth = floor(*tau * *n + 1./(10 * *n));
  vector<vector<double> > X(*n);
  TransformData(data, *n, *d, &X);
  vector<unsigned long long> halfspacesCmb;
  if (*d == 2){
    TRegionBruteForce(X, intDepth, &halfspacesCmb);
  }else{
    TRegionCmb(X, intDepth, &halfspacesCmb);
  }
  *numHalfspaces = halfspacesCmb.size();
  TVariables pointIndices(*d);
  for (int i = 0; i < *numHalfspaces; i++){
    getFacetPoints(halfspacesCmb[i], *n, *d, &pointIndices);
    for (int j = 0; j < *d; j++){
      halfspaces[i * *d + j] = pointIndices[j] + 1;
    }
  }
}

void TrFacets(double *data, int intTau, int *n, int *d, int *numHalfspaces, int *halfspaces, int *numFacets, int *facets, int *found){
  vector<vector<double> > X(*n);
  TransformData(data, *n, *d, &X);
  vector<unsigned long long> halfspacesBFS(*numHalfspaces);
  TVariables facet(*d);
  for (int i = 0; i < *numHalfspaces; i++){
    for (int j = 0; j < *d; j++){
      facet[j] = halfspaces[i * *d + j] - 1;
    }
    halfspacesBFS[i] = getFacetCode(facet, *n);

  }
  vector<double> innerPoint(*d);
  //for (int i = 0; i < *n; i++){
  //  for (int j = 0; j < *d; j++){
  //    innerPoint[j] += X[i][j];
  //  }
  //}
  //for (int i = 0; i < *d; i++){
  //  innerPoint[i] /= (double)(*n);
  //}
  vector<vector<double> > normals;
  vector<double> bs;
  getHalfspaces(X, intTau, halfspacesBFS, &normals, &bs);
  int res = getInnerPoint(normals, bs, &innerPoint);
  if (res){
    *found = 0;
    //cout << "Inner point NOT found." << endl;
  }else{
    *found = 1;
    //cout << "Inner point FOUND." << endl;
    TVariables facetsTmp;
    *numFacets = (int)getFacets(X, halfspacesBFS, innerPoint, &facetsTmp);
    for (int i = 0; i < *numFacets; i++){
      facets[i] = facetsTmp[i] + 1;
    }
  }
}

//void TukeyDepthLiftVolume(double *data, int *n, int *d, double *volume){
//  *volume = vol_hfspex(data, *n, *d);
//}

/* Export functions (end) --------------------------------------------------- */

//#ifdef __cplusplus
//}
//#endif
