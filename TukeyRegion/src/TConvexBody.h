// TConvexBody.h
// By Pavlo Mozharovskyi
// Last changed 21.08.2017
// Header for functions computing the region's elements

unsigned long long getVertices(TMatrix &X, vector<unsigned long long> &facets,
	TMatrix *vertices);
int getHalfspaces(TMatrix &X, int intTau, vector<unsigned long long> &facets,
                  TMatrix *normals, TPoint *bs);
bool getInnerPoint(TMatrix &normals, TPoint &bs, TPoint *innerPoint);
unsigned long long getVertices(TMatrix &X, vector<unsigned long long> &facets,
                               TPoint &center, TMatrix *vertices);
unsigned long long getVertices(TMatrix &normals, TPoint &bs,
                               TPoint &center, TMatrix *vertices);
void getQHFacets(TMatrix &points, vector<TVariables> &facets, bool triangulate, 
                 int* pExitcode);
unsigned long long getFacets(TMatrix &X, vector<unsigned long long> &halfspaces,
                             TPoint &center, TVariables *facets);
double getQHVolume(TMatrix &points, int* exitcode);
void getQHDelaunay(TMatrix &points, vector<TVariables> &facets, 
                   vector<double> &volumes, int* pExitcode);
void getQHVertices(TMatrix &points, TVariables &vertexIndices, int* pExitcode);
void getQHBarycenter(TMatrix &points,  TPoint &center, int* pExitcode);
int fitlerHalfspaces(TMatrix &X, vector<unsigned long long> &facets, 
                     TPoint &center, TVariables &indicesHalfspacesNR);
bool checkInnerPoint(TMatrix &normals, TPoint &bs, TPoint &innerPoint, 
                     TVariables &iNormals, double &minEps);
