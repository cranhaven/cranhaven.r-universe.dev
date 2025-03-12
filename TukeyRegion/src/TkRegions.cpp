// TkRegions.cpp
// By Pavlo Mozharovskyi
// Last changed 17.11.2021
// The main gate to computation of the Tukey region by recursive algorithm

#include "TkRegions.h"

//#include <chrono>
//chrono::duration<double> time_getFacets;
//chrono::duration<double> time_checkFacets_total;
//chrono::duration<double> time_getBasisComplement;
//chrono::duration<double> time_getProjection;
//chrono::duration<double> time_preparePlane;
//chrono::duration<double> time_before_total;
//chrono::duration<double> time_collectFacets;
//chrono::duration<double> time_centering;
//chrono::duration<double> time_mainCycle_total;

bool TRegionTau(TMatrix X, int intDepth, int algStart, int numStart,
                vector<unsigned long long>* facets,
                queue<TVariables*>* ridges){
  //chrono::time_point<chrono::steady_clock> t3, t4;
  //t3 = chrono::high_resolution_clock::now();
  // Step 1
  facets->resize(0);
  int n = X.size();
  int d = X[0].size();
  int intTau = intDepth - 1;
  binaryHypermatrixCmb<unsigned char> A(n, d - 1);
  queue<TVariables*> Q;
  bstree<unsigned long long> F;
	// Step 2
	//TVariables *firstCmb = new TVariables();
	//getFirstCombination2(X, intTau, firstCmb);
	/* To use one initial combination only. */
	//firstCmb->resize(d - 1);
	//A.setIfNotSet(*firstCmb);
	//Q.push(firstCmb);
	/* ------------------------------------ */
	/* To use all (d-1)-tupels lying outside as initial combinations */
	/* Go through all necessary combinations */
	/*sort(firstCmb->begin(), firstCmb->end());
	TVariables counters(d - 1);
	for (int i = 0; i < d - 2; i++){counters[i] = i;}counters[d - 2] = d - 3;
	int numOut = firstCmb->size();
	while (counters[0] != numOut - d + 1){ // Get current combination
		int i = d - 2;
		while (i > 0 && counters[i] == numOut - d + i + 1){i--;}
		counters[i]++;int j = i + 1;
		while (j < d - 1){counters[j] = counters[j - 1] + 1;j++;}
		TVariables *curCmb = new TVariables(d - 1);
		for (int k = 0; k < d - 1; k++){(*curCmb)[k] = (*firstCmb)[counters[k]];}
		//A.setIfNotSet(*curCmb);
		//Q.push(curCmb);
	}*/
	/* ------------------------------------------------------------- */
	/* Experimental (but working as the above fragment) code */
	if (ridges->empty()){
	  vector<TVariables*> iRidges(0);
	  initRidges(X, intTau, algStart, numStart, iRidges);
	  // TODO: Inefficient if there are many ridges to paste
	  for (int i = 0; i < iRidges.size(); i++){
	    A.setIfNotSet(*iRidges[i]);
	    Q.push(iRidges[i]);
	  }
	}else{
	  while(!ridges->empty()){
	    TVariables* tmpRidge = ridges->front();
	    ridges->pop();
	    A.setIfNotSet(*tmpRidge);
	    Q.push(tmpRidge);
	  }
	}
	/* ----------------- */
	//t4 = chrono::high_resolution_clock::now();
	//time_before_total += t4 - t3;
	//t3 = chrono::high_resolution_clock::now();
	// Step 3
	// Initialize temporatory variables
	TMatrix plane(d - 2);
	for (int i = 1; i < d - 1; i++) {
		plane[i - 1] = TPoint(d);
	}
	TMatrix basis(2); basis[0] = TPoint(d); basis[1] = TPoint(d);
	TMatrix curXPrj;
	curXPrj.resize(X.size());
	for (int i = 0; i < X.size(); i++) {
		curXPrj[i] = TPoint(2);
	}
	while(!Q.empty()){
		//chrono::time_point<chrono::steady_clock> t1, t2;
		TVariables *curCmb = Q.front();
		Q.pop();
		ridges->push(curCmb);
		// Step 4
		//t1 = chrono::high_resolution_clock::now();
		//TMatrix plane(d - 2);
		for (int i = 1; i < d - 1; i++){
			//plane[i - 1] = TPoint(d);
			for (int j = 0; j < d; j++){
				plane[i - 1][j] = X[(*curCmb)[i]][j] - X[(*curCmb)[0]][j];
			}
		}
		//t2 = chrono::high_resolution_clock::now();
		//time_preparePlane += t2 - t1;
		//t1 = chrono::high_resolution_clock::now();
		getBasisComplement(plane, &basis);
		//t2 = chrono::high_resolution_clock::now();
		//time_getBasisComplement += t2 - t1;
		//t1 = chrono::high_resolution_clock::now();
		//TMatrix curXPrj;
		getProjection(X, basis, &curXPrj);
		//t2 = chrono::high_resolution_clock::now();
		//time_getProjection += t2 - t1;
		//t1 = chrono::high_resolution_clock::now();
		TPoint center(curXPrj[(*curCmb)[0]]);
		for (int i = 0; i < n; i++){
			for (int j = 0; j < 2; j++){
				curXPrj[i][j] -= center[j];
			}
		}
		TVariables facetCandidates;
		//t2 = chrono::high_resolution_clock::now();
		//time_centering += t2 - t1;
		//if (curCmb->size() != 2) {
		//	cout << "Combination size is " << curCmb->size() << "." << endl;
		//}
		//t1 = chrono::high_resolution_clock::now();
		bool boolTmp = getFacets(curXPrj, intTau, curCmb, &facetCandidates);
		//t2 = chrono::high_resolution_clock::now();
		//time_getFacets += t2 - t1;
		//t1 = chrono::high_resolution_clock::now();
		if (boolTmp){
			//if (facetCandidates.size() > 2) {
			//	cout << "Number of founded facets is " << facetCandidates.size() << "." << endl;
			//}
			sort(facetCandidates.begin(), facetCandidates.end());
			// Step 5
			for (int i = 0; i < facetCandidates.size(); i++){
				// a
				TVariables newCmb(*curCmb);int pos = 0;
				for (int j = 0; j < newCmb.size(); j++){
					if (facetCandidates[i] < newCmb[j]){break;}
					pos++;
				}
				newCmb.insert(newCmb.begin() + pos, facetCandidates[i]);
				// b
				unsigned long long newFacetCode = getFacetCode(newCmb, n);
				if (F.insert_unique(newFacetCode)){
					// c
					for (int j = 0; j < newCmb.size(); j++){
						if (newCmb[j] == facetCandidates[i]){continue;}
						int tmp = newCmb[j];
						newCmb.erase(newCmb.begin() + j);
						if (A.setIfNotSet(newCmb)){
							TVariables* tmpCmb = new TVariables(newCmb);
							Q.push(tmpCmb);
						}
						newCmb.insert(newCmb.begin() + j, tmp);
					}
				}
			}
		}
		// delete curCmb; // because it is needed in 'ridges' passed to next level
		//t2 = chrono::high_resolution_clock::now();
		//time_checkFacets_total += t2 - t1;
	}// Step 6
	//t4 = chrono::high_resolution_clock::now();
	//time_mainCycle_total += t4 - t3;
	//t3 = chrono::high_resolution_clock::now();
	facets->resize(F.nodes.size());
	for (int i = 0; i < F.nodes.size(); i++){
		(*facets)[i] = F.nodes[i]->value;
	}
	sort(facets->begin(), facets->end());
	//t4 = chrono::high_resolution_clock::now();
	//time_collectFacets += t4 - t3;
	return true;
}

List TukeyRegionTau(NumericMatrix data, int depth,
                    String method,
                    bool trgFacets,
                    bool checkInnerPoint,
                    bool retHalfspaces,
                    bool retHalfspacesNR,
                    bool retInnerPoint,
                    bool retVertices,
                    bool retFacets,
                    bool retVolume,
                    bool retBarycenter,
                    queue<TVariables*>* ridges,
                    IntegerMatrix halfspaces,
                    NumericVector innerPoint,
                    int verbosity){
  // Check data consistency

  //Rcpp::Nullable< Rcpp::IntegerMatrix > halfspaces = R_NilValue,
  //Rcpp::Nullable< Rcpp::NumericVector > innerPoint = R_NilValue){

  //IntegerMatrix rhalfspaces = wrap(halfspaces);
  //NumericVector rinnerPoint = wrap(innerPoint);

  //Rcout << halfspaces.nrow() << " " << halfspaces.ncol() << endl;

  // Check input consistency
  int algStart = 3;
  int numStart = -1;
  //bool dataIsMatrix = Rf_isMatrix(data);
  //if (!dataIsMatrix){
  //  stop("Argument 'data' should be a matrix");
  //}
  int n = data.nrow();
  int d = data.ncol();
  if (d < 2 || n <= d){
    stop("Argument 'data' should be a matrix with at least d = 2 columns and \
at least d + 1 rows");
  }
  if (depth < 1 || depth > (int)(n / 2)){
    stop("Argument 'depth' should be between 1 and (number of rows in \
'data')/2");
  }
  int intAlgRegion = 0;
  if (method == "bfs"){
    intAlgRegion = 1;
  }else{
    if (method == "cmb"){
      intAlgRegion = 2;
    }else{
      if (method == "bf"){
        intAlgRegion = 3;
      }else{
        stop("Argument 'method' should be a string with value either \
'bfs' (breadth-first search) or 'cmb' (combinatorial) or 'bf' (brute force)");
      }
    }
  }
  if (verbosity < 0 || verbosity > 2){
    stop("Argument 'verbosity' should be an integer equal 0, 1, or 2");
  }

  // Transform data to the STL format
  List ret = List::create();
  ret.push_back(data, "data");
  ret.push_back(depth, "depth");
  if (verbosity >= 2){
    Rcout << "Data constitutes " << n << " points of dimension " << d << endl;
    Rcout << "Required depth level is " << depth << endl;
  }

  TMatrix X(n);
  for (int i = 0; i < n; i++){
    X[i].resize(d);
    for (int j = 0; j < d; j++){
      X[i][j] = data(i, j);
    }
  }
  // Calculate halfspaces
  vector<unsigned long long> hfspCodes;
  TVariables tmpHalfspace(d);
  bool hfspFound = false;
  if (halfspaces.nrow() > d && halfspaces.ncol() == d){
    // If halfspaces are provided, just reassign them
    int nHfsp = halfspaces.nrow();
    hfspCodes.resize(nHfsp);
    for (int i = 0; i < nHfsp; i++){
      for (int j = 0; j < d; j++){
        tmpHalfspace[j] = halfspaces(i,j) - 1;
      }
      sort(tmpHalfspace.begin(), tmpHalfspace.end());
      hfspCodes[i] = getFacetCode(tmpHalfspace, n);
    }
    hfspFound = true;
    if (verbosity >= 2){
      Rcout << "Provided " << nHfsp << " halfspaces adopted" << endl;
    }
  }else{
    if (verbosity >= 2){
      Rcout << "Halfspaces are not provided, calculating them" << endl;
    }
    if (d == 2){
      // if (verbosity >= 1){
      //   Rcout << "As dimension = 2 the brute-force method is employed" << endl;
      // }
      if (intAlgRegion == 1){
        if (verbosity >= 2){
          Rcout << "The breadth-first search method is employed" << endl;
        }
        hfspFound = TRegion2D(X, depth, algStart, numStart, &hfspCodes);
      }
      if (intAlgRegion == 2){
        if (verbosity >= 2){
          Rcout << "The combinatorial method is employed" << endl;
        }
        hfspFound = TRegionCmb2D(X, depth, &hfspCodes);
      }
      if (intAlgRegion == 3){
        if (verbosity >= 2){
          Rcout << "The brute-force method is employed" << endl;
        }
        hfspFound = TRegionBruteForce(X, depth, &hfspCodes);
      }
    }else{
      // Choose the algorithm to calculate the region
      if (intAlgRegion == 1){
        if (verbosity >= 2){
          Rcout << "The breadth-first search method is employed" << endl;
        }
        hfspFound = TRegionTau(X, depth, algStart, numStart, &hfspCodes,
                               ridges);
        double numRidges = ridges->size();
        ret.push_back(numRidges, "numRidges");
      }
      if (intAlgRegion == 2){
        if (verbosity >= 2){
          Rcout << "The combinatorial method is employed" << endl;
        }
        hfspFound = TRegionCmb(X, depth, &hfspCodes);
      }
      if (intAlgRegion == 3){
        if (verbosity >= 2){
          Rcout << "The brute-force method is employed" << endl;
        }
        hfspFound = TRegionBruteForce(X, depth, &hfspCodes);
      }
    }
  }
  // Return the result about the existence of halfspaces
  if (hfspFound){
    if (verbosity >= 1){
      Rcout << hfspCodes.size() << " halfspaces found" << endl;
    }
    ret.push_back(true, "halfspacesFound");
  }else{
    if (verbosity >= 1){
      Rcout << "No halfspaces could be found, the Tukey region does not" <<
        " exist for required depth level" << endl;
    }
    ret.push_back(false, "halfspacesFound");
    ret.attr("class") = "TukeyRegion";
    return ret;
  }
  // Add halfpaces to the return list
  if (retHalfspaces){
    IntegerMatrix retHfsp(hfspCodes.size(), d);
    TVariables pointNumbers;
    for (int i = 0; i < hfspCodes.size(); i++){
      getFacetPoints(hfspCodes[i], n, d, &pointNumbers);
      for (int j = 0; j < d; j++){
        retHfsp(i,j) = pointNumbers[j] + 1;
      }
    }
    ret.push_back(retHfsp, "halfspaces");
  }
  // Calculate inner point of the region
  if (retInnerPoint || retHalfspacesNR || retVertices || retFacets ||
      retVolume || retBarycenter){
    if (verbosity >= 2){
      Rcout << "Computation of the Tukey region ..." << endl;
    }
    // Start by obtaining inner point of the region
    TPoint theInnerPoint(d);
    bool innerPointFound = false;
    // Obtain inner point
    if (innerPoint.length() == d && !checkInnerPoint){
      // If inner point is given and we trust it
      for (int j = 0; j < d; j++){
        theInnerPoint[j] = innerPoint(j);
      }
      innerPointFound = true;
      if (verbosity >= 1){
        Rcout << "Provided inner point adopted without checking" << endl;
      }
    }else{
      // Calculate halfspaces bordering the region
      vector<vector<double> > normals;
      vector<double> bs;
      getHalfspaces(X, depth - 1, hfspCodes, &normals, &bs);
      if (innerPoint.length() == d){
        // If 'innerPoint' is given, just check it
        for (int j = 0; j < d; j++){
          theInnerPoint[j] = innerPoint(j);
          //Rcout << theInnerPoint[j] << " ";
        }
        //Rcout << endl;
        innerPointFound = true;
        for (int i = 0; i < normals.size(); i++){
          double tmpPrj = 0;
          for (int j = 0; j < d; j++){
            tmpPrj += normals[i][j] * theInnerPoint[j];
          }
          if (tmpPrj - eps > bs[i]){
            if (verbosity >= 2){
              Rcout << i << ": " << tmpPrj - eps << " and " << bs[i] << endl;
            }
            innerPointFound = false;
            if (verbosity >= 1){
              Rcout << "Provided inner point failed the check" << endl;
            }
            break;
          }
        }
      }else{
        // Position potential inner point into the average
        //for (int i = 0; i < n; i++){
        //  for (int j = 0; j < d; j++){
        //    theInnerPoint[j] += X[i][j];
        //  }
        //}
        //for (int i = 0; i < d; i++){
        //  theInnerPoint[i] /= (double)n;
        //}
        // Calculate the inner point
        innerPointFound = getInnerPoint(normals, bs, &theInnerPoint);
      }
    }
    // Add the result to the return list
    if (innerPointFound){
      ret.push_back(true, "innerPointFound");
      if (verbosity >= 1){
        Rcout << "Inner point found" << endl;
      }
      if (retInnerPoint){
        ret.push_back(wrap(theInnerPoint), "innerPoint");
      }
    }else{
      ret.push_back(false, "innerPointFound");
      if (verbosity >= 1){
        Rcout << "Inner point not found, the Tukey region does not" <<
          " exist for required depth level" << endl;
      }
      ret.attr("class") = "TukeyRegion";
      return ret;
    }
    // Add the non-redundant halfspaces to the list if required
    if (retHalfspacesNR){
      // Identify nonredundant halfspaces
      TVariables indicesHalfspacesNR(0);
      fitlerHalfspaces(X, hfspCodes, theInnerPoint, indicesHalfspacesNR);
      // Prepare output
      IntegerMatrix retHfspNR(indicesHalfspacesNR.size(), d);
      TVariables pointNumbers;
      for (int i = 0; i < indicesHalfspacesNR.size(); i++){
        getFacetPoints(hfspCodes[indicesHalfspacesNR[i]], n, d, &pointNumbers);
        for (int j = 0; j < d; j++){
          retHfspNR(i,j) = pointNumbers[j] + 1;
        }
      }
      ret.push_back(retHfspNR, "halfspacesNR");
      if (verbosity >= 2){
        Rcout << "Non-redundant halfspaces identified" << endl;
      }
    }
    // If user demands to calculate the shape of the region
    if (retVertices || retFacets || retVolume || retBarycenter){
      if (verbosity >= 2){
        Rcout << "Computation of the Tukey region's shape ..." << endl;
      }
      // Calculate vertices of the region polytope
      TMatrix vertexMatrix;
      getVertices(X, hfspCodes, theInnerPoint, &vertexMatrix);
      if (verbosity >= 2){
        Rcout << vertexMatrix.size() << " vertices computed" << endl;
      }
      // Add vertices to the return list
      if (retVertices || retFacets){
        NumericMatrix vertices(vertexMatrix.size(), d);
        for (int i = 0; i < vertexMatrix.size(); i++){
          for (int j = 0; j < d; j++){
            vertices(i,j) = vertexMatrix[i][j];
          }
        }
        ret.push_back(vertices, "vertices");
      }
      // Caclulate facets of the region polytope
      if (retFacets){
        ret.push_back(trgFacets, "triangulated");
        vector<TVariables> facetVector;
        int exitcode;
        getQHFacets(vertexMatrix, facetVector, trgFacets, &exitcode);
        // Add facets to the return list
        if (trgFacets){
          // If facest are triangulated, each facet("triangle") contains
          // ... exactly 'd' vertices, so we return an integer matrix
          IntegerMatrix facets(facetVector.size(), d);
          for (int i = 0; i < facetVector.size(); i++){
            for (int j = 0; j < facetVector[i].size(); j++){
              facets(i,j) = facetVector[i][j] + 1;
            }
          }
          ret.push_back(facets, "facets");
          if (verbosity >= 2){
            Rcout << facetVector.size() <<
              " facets computed with triangulation" << endl;
          }
        }else{
          List facets = List::create();
          for (int i = 0; i < facetVector.size(); i++){
            for (int j = 0; j < facetVector[i].size(); j++){
              facetVector[i][j]++;
            }
            facets.push_back(wrap(facetVector[i]));
          }
          ret.push_back(facets, "facets");
          if (verbosity >= 2){
            Rcout << facetVector.size() <<
              " facets computed without triangulation" << endl;
          }
        }
      }
      // Return region's volume
      if (retVolume){
        int exitcode = -1;
        double volume = getQHVolume(vertexMatrix, &exitcode);
        ret.push_back(volume, "volume");
        if (verbosity >= 2){
          Rcout << "Region's volume is equal to " << volume << endl;
        }
      }
      // Return region's center of gravity
      if (retBarycenter){
        // The center
        TPoint center(d);
        // If region is a simplex
        if (vertexMatrix.size() == d + 1){
          if (verbosity >= 2){
            Rcout << "Region is a simplex, ";
          }
          for (int i = 0; i < d; i++){
            for (int j = 0; j < d + 1; j++){
              center[i] += vertexMatrix[j][i];
            }
            center[i] /= d + 1;
          }
        }else{
          if (verbosity >= 2){
            Rcout << "Region is not a simplex, ";
          }
          int exitcode = -1;
          getQHBarycenter(vertexMatrix, center, &exitcode);
        }
        ret.push_back(wrap(center), "barycenter");
        if (verbosity >= 2){
          Rcout << "barycenter computed" << endl;
        }
      }
      if (verbosity >= 1){
        Rcout << "Tukey region's shape computed" << endl;
      }
    }
  }
  ret.attr("class") = "TukeyRegion";
  return ret;
}
