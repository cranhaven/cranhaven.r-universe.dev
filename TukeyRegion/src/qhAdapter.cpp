// qhAdapter.cpp
// By Pavlo Mozharovskyi
// Last changed 12.08.2016
// Adapter for the QHULL routines

#include "TukeyRegion.h"

int convhull(double *points, int n, int d, int *vertexIndices){
  char *options;
  string tmpStr;
	if (d <= 3){
		//options = "qhull Qt QbB Pp";
		//options = "qhull QbB Pp";
		tmpStr = "qhull Qs Qbb Pp";
		options = strdup(tmpStr.c_str());
	}else{
		//options = "qhull Qt Qx Qs QbB Pp";
		//options = "qhull QbB Pp";
		tmpStr = "qhull Qx Qs Qbb Pp";
	  options = strdup(tmpStr.c_str());
	}
	//FILE *errfile = fopen("err.txt", "w");
	//FILE *tmpstdout = fopen("out.txt", "w");
	int exitcode = qh_new_qhull (d, n, points, false, options, NULL,
		NULL);
	//int exitcode = qh_new_qhull (d, n, points, false, options,
		//tmpstdout, errfile);
	//fclose(tmpstdout);
	//fclose(errfile);
	if (!exitcode){
		//facetT *facet;
		vertexT *vertex, **vertexp;
		//unsigned int numFacets = qh num_facets;
		//std::vector<int> vertices(numFacets*d);
		int counter = 1;
		FORALLvertices{
			vertexIndices[counter++] = qh_pointid(vertex->point);
		}
		*vertexIndices = counter - 1;
		//int i=0;
		//FORALLfacets {
		//	int j=0;
		//	FOREACHvertex_ (facet->vertices) {
		//		if (j < d){
		//			vertices[i+n * j++] = 1 + qh_pointid(vertex->point);
		//		}
		//	}
		//	i++;
		//}
		//std::sort(vertices.begin(), vertices.end());
		//int counter = 2;
		//int curVertex = vertices[0];vertexIndices[1] = curVertex;
		//for (int i = 0; i < numFacets * d; i++){
		//	if (vertices[i] > curVertex){
		//		curVertex = vertices[i];
		//		vertexIndices[counter++] = curVertex;
		//	}
		//}
		//*vertexIndices = counter - 1;
	}
	qh_freeqhull(qh_ALL);
	free(options);
	return exitcode;
}

double convvol(double *points, int n, int d){
  string tmpStr = "qhull FA";
  char* options = strdup(tmpStr.c_str());
  //FILE *errfile = fopen("err.txt", "w");
  //FILE *tmpstdout = fopen("out.txt", "w");
	int exitcode = qh_new_qhull(d, n, points, false, options, NULL,
		NULL);
	//int exitcode = qh_new_qhull (d, n, points, false, options,
                              //tmpstdout, errfile);
	//fclose(tmpstdout);
	//fclose(errfile);
	double vol = qh totvol;
	qh_freeqhull(qh_ALL);
	free(options);
	if (!exitcode){
		return vol;
	}else{
		return -1;//exitcode;
	}
}
