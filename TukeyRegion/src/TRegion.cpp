// TRegion.cpp
// By Pavlo Mozharovskyi
// Last changed 28.06.2017
// The main gate to computation of the Tukey region

#include "TRegion.h"

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

/* Brute force calculation of the Tukey depth region.                         */
/* Calculates all hyperplanes which cut off n*'tau' points from 'X',          */
/* and saves them coded by 'getFacetCode' in vector 'facets'.                 */
/* 'X' should be in general position.                                         */
/* Returns the status of success.                                             */
bool TRegionBruteForce(TMatrix X, int intDepth, vector<unsigned long long>* facets){
	/* Initialization */
	facets->clear();
	int n = X.size();
	int d = X[0].size();
	int intTau = intDepth - 1;
	/* Main cycle that goes through combinations = (d)-tuples */
	TVariables counters(d);
	for (int i = 0; i < d - 1; i++){counters[i] = i;}counters[d - 1] = d - 2;
	while (counters[0] != n - d){ // Get current combination
		int i = d - 1;
		while (i > 0 && counters[i] == n - d + i){i--;}
		counters[i]++;int j = i + 1;
		while (j < d){counters[j] = counters[j - 1] + 1;j++;}
		/* For the current d-tuple do: */
		/* - if it is a facet then save it */
		if (checkFirstCombination(X, intTau, counters)){
			facets->push_back(getFacetCode(counters, n));
		}
	}
	return true;
}

/* Calculation of the Tukey depth region using combinatorial approach.        */
/* Calculates all hyperplanes which cut off n*'tau' points from 'X',          */
/* and saves them coded by 'getFacetCode' in vector 'facets'.                 */
/* 'X' should be in general position.                                         */
/* Returns the status of success.                                             */
bool TRegionCmb(TMatrix X, int intDepth, vector<unsigned long long>* facets){
	/* Initialization */
	facets->clear();
	int n = X.size();
	int d = X[0].size();
	int intTau = intDepth - 1;
	bstree<unsigned long long> F;
	/* Main cycle that goes through combinations = (d - 1)-tuples */
	TVariables counters(d - 1);
	for (int i = 0; i < d - 2; i++){counters[i] = i;}counters[d - 2] = d - 3;
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
	while (counters[0] != n - d + 1){ // Get current combination
		int i = d - 2;
		while (i > 0 && counters[i] == n - d + i + 1){i--;}
		counters[i]++;int j = i + 1;
		while (j < d - 1){counters[j] = counters[j - 1] + 1;j++;}
		/* For the current (d - 1)-tuple do: */
		/* a) - get the complementary 2-dimensional space */
		//TMatrix plane(d - 2);
		for (int i = 1; i < d - 1; i++){
			//plane[i - 1] = TPoint(d);
			for (int j = 0; j < d; j++){
				plane[i - 1][j] = X[counters[i]][j] - X[counters[0]][j];
			}
		}
		getBasisComplement(plane, &basis);
		/* b) - project points onto the found 2-dimensional space */
		//TMatrix curXPrj;
		getProjection(X, basis, &curXPrj);
		TPoint center(curXPrj[counters[0]]);
		for (int i = 0; i < n; i++){
			for (int j = 0; j < 2; j++){
				curXPrj[i][j] -= center[j];
			}
		}
		/* c) - find the two points forming two facets with this (d-1)-tuple */
		TVariables facetCandidates;
		if (getFacets(curXPrj, intTau, &counters, &facetCandidates)){
			sort(facetCandidates.begin(), facetCandidates.end());
			for (int i = 0; i < facetCandidates.size(); i++){ // For each facet:
				/* Form the facet's d-tuple */
				TVariables newCmb(counters);int pos = 0;
				for (int j = 0; j < newCmb.size(); j++){
					if (facetCandidates[i] < newCmb[j]){break;}
					pos++;
				}
				newCmb.insert(newCmb.begin() + pos, facetCandidates[i]);
				/* Save the facet if it is new */
				unsigned long long newFacetCode = getFacetCode(newCmb, n);
				if (F.insert_unique(newFacetCode)){
					facets->push_back(newFacetCode);
				}
			}
		}
	}
	sort(facets->begin(), facets->end());
	return true;
}

/* Calculation of the Tukey depth region using breadth-first search algorithm */
/* over combinations.                                                         */
/* Calculates all hyperplanes which cut off n*'tau' points from 'X',          */
/* and saves them coded by 'getFacetCode' in vector 'facets'.                 */
/* 'X' should be in general position.                                         */
/* Returns the status of success.                                             */
bool TRegion(TMatrix X, int intDepth, int algStart, int numStart,
             vector<unsigned long long>* facets, int* numRidges){
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
	vector<TVariables*> iRidges(0);
	initRidges(X, intTau, algStart, numStart, iRidges);
	if (iRidges.size() == 0){
	  return false;
	}
	// TODO: Inefficient if there are many ridges to paste
	for (int i = 0; i < iRidges.size(); i++){
	  A.setIfNotSet(*iRidges[i]);
	  Q.push(iRidges[i]);
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
	*numRidges = 0; // zero the ridges' counter'
	while(!Q.empty()){
		//chrono::time_point<chrono::steady_clock> t1, t2;
		TVariables *curCmb = Q.front();
		Q.pop();
		(*numRidges)++;
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
		delete curCmb;
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

bool TRegionCheckDepth(TMatrix X, int intDepth, int algRegion, int algStart,
                       int numStart, vector<unsigned long long>* facets,
                       TPoint* innerPoint, int* numRidges){
  // Calculate halfspaces
  int n = X.size();
  int d = X[0].size();
  bool hfspFound = false;
  if (d == 2){
    hfspFound = TRegionBruteForce(X, intDepth, facets);
  }else{
    // Choose the algorithm to calculate the region
    if (algRegion == 1){
      hfspFound = TRegion(X, intDepth, algStart, numStart, facets, numRidges);
    }
    if (algRegion == 2){
      hfspFound = TRegionCmb(X, intDepth, facets);
    }
    if (algRegion == 3){
      hfspFound = TRegionBruteForce(X, intDepth, facets);
    }
  }
  if (!hfspFound){
    return false;
  }
  // Calculate inner point of the region
  bool innerPointFound = false;
  innerPoint->resize(d);
  for (int j = 0; j < d; j++){
    (*innerPoint)[j] = 0;
  }
  // Calculate halfspaces bordering the region
  vector<vector<double> > normals;
  vector<double> bs;
  getHalfspaces(X, intDepth - 1, *facets, &normals, &bs);
  // Position potential inner point into the average
  for (int i = 0; i < n; i++){
    for (int j = 0; j < d; j++){
      (*innerPoint)[j] += X[i][j];
    }
  }
  for (int j = 0; j < d; j++){
    (*innerPoint)[j] /= (double)n;
  }
  // Calculate the inner point
  return getInnerPoint(normals, bs, innerPoint);
}

/* The modification of the "TRegionCmb" funciton for the 2-dimensional case   */
bool TRegionCmb2D(TMatrix X, int intDepth, vector<unsigned long long>* facets){
  /* Initialization */
  facets->clear();
  int n = X.size();
  int d = X[0].size();
  int intTau = intDepth - 1;
  bstree<unsigned long long> F;
  /* Main cycle that goes through points */
  // Initialize temporary variables
  // DELETE THIS FRAGMENT
  // TMatrix plane(d - 2);
  // for (int i = 1; i < d - 1; i++) {
  //   plane[i - 1] = TPoint(d);
  // }
  // DELETE THIS FRAGMENT (END)
  TMatrix basis(2); basis[0] = TPoint(d); basis[1] = TPoint(d);
  basis[0][0] = 1;basis[0][1] = 1;basis[1][0] = 0;basis[1][1] = 1;
  TMatrix curXPrj;
  curXPrj.resize(X.size());
  for (int i = 0; i < X.size(); i++) {
    curXPrj[i] = TPoint(2);
  }
  TVariables counters(1);
  for (int iter = 0; iter < n; iter++){ // Get current combination
    counters[0] = iter;
    /* For the current point do: */
    /* a) - project points onto the found 2-dimensional space */
    getProjection(X, basis, &curXPrj);
    TPoint center(curXPrj[counters[0]]);
    for (int i = 0; i < n; i++){
      for (int j = 0; j < 2; j++){
        curXPrj[i][j] -= center[j];
      }
    }
    /* b) - find the two points forming two facets with this point */
    TVariables facetCandidates;
    if (getFacets(curXPrj, intTau, &counters, &facetCandidates)){
      sort(facetCandidates.begin(), facetCandidates.end());
      for (int i = 0; i < facetCandidates.size(); i++){ // For each facet:
        /* Form the facet's d-tuple */
        TVariables newCmb(counters);int pos = 0;
        for (int j = 0; j < newCmb.size(); j++){
          if (facetCandidates[i] < newCmb[j]){break;}
          pos++;
        }
        newCmb.insert(newCmb.begin() + pos, facetCandidates[i]);
        /* Save the facet if it is new */
        unsigned long long newFacetCode = getFacetCode(newCmb, n);
        if (F.insert_unique(newFacetCode)){
          facets->push_back(newFacetCode);
        }
      }
    }
  }
  sort(facets->begin(), facets->end());
  return true;
}

/* The modification of the "TRegion" funciton for the 2-dimensional case   */
bool TRegion2D(TMatrix X, int intDepth, int algStart, int numStart,
               vector<unsigned long long>* facets){
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
  vector<TVariables*> iRidges(0);
  initRidges2D(X, intTau, algStart, numStart, iRidges);
  if (iRidges.size() == 0){
    return false;
  }
  for (int i = 0; i < iRidges.size(); i++){
    A.setIfNotSet(*iRidges[i]);
    Q.push(iRidges[i]);
  }
  /* ----------------- */
  //t4 = chrono::high_resolution_clock::now();
  //time_before_total += t4 - t3;
  //t3 = chrono::high_resolution_clock::now();
  // Step 3
  // Initialize temporatory variables
  TMatrix plane(d - 2);
  if (d > 2){
    for (int i = 1; i < d - 1; i++) {
      plane[i - 1] = TPoint(d);
    }
  }
  TMatrix basis(2); basis[0] = TPoint(d); basis[1] = TPoint(d);
  if (d == 2){
    basis[0][0] = 1;basis[0][1] = 1;basis[1][0] = 0;basis[1][1] = 1;
  }
  TMatrix curXPrj;
  curXPrj.resize(X.size());
  for (int i = 0; i < X.size(); i++) {
    curXPrj[i] = TPoint(2);
  }
  while(!Q.empty()){
    //chrono::time_point<chrono::steady_clock> t1, t2;
    TVariables *curCmb = Q.front();
    Q.pop();
    // Step 4
    //t1 = chrono::high_resolution_clock::now();
    if (d > 2){
      //TMatrix plane(d - 2);
      for (int i = 1; i < d - 1; i++){
        //plane[i - 1] = TPoint(d);
        for (int j = 0; j < d; j++){
          plane[i - 1][j] = X[(*curCmb)[i]][j] - X[(*curCmb)[0]][j];
        }
      }
    }
    //t2 = chrono::high_resolution_clock::now();
    //time_preparePlane += t2 - t1;
    //t1 = chrono::high_resolution_clock::now();
    if (d > 2){
      getBasisComplement(plane, &basis);
    }
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
    delete curCmb;
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
