// TukeyRegionR.cpp
// By Pavlo Mozharovskyi
// Last changed 22.09.2017
// The main R(cpp)-adapter to the C++-functionality

#include "TukeyRegion.h"

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

// [[Rcpp::export]]
List TukeyRegion(NumericMatrix data, int depth,
                 String method = "bfs",
                 bool trgFacets = false,
                 bool checkInnerPoint = true,
                 bool retHalfspaces = true,
                 bool retHalfspacesNR = false,
                 bool retInnerPoint = false,
                 bool retVertices = false,
                 bool retFacets = false,
                 bool retVolume = false,
                 bool retBarycenter = false,
                 IntegerMatrix halfspaces = IntegerMatrix(0),
                 NumericVector innerPoint = NumericVector(1),
                 int verbosity = 0){
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

  // Transfrom data to the STL format
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
        int numRidges = 0;
        hfspFound = TRegion(X, depth, algStart, numStart, &hfspCodes,
                            &numRidges);
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

// [[Rcpp::depends(ddalpha)]]
// [[Rcpp::export]]
List TukeyMedian(NumericMatrix data,
                 String algMedian = "bsbarydepth",
                 String method = "bfs",
                 bool trgFacets = true,
                 bool retHalfspaces = false,
                 bool retHalfspacesNR = false,
                 bool retInnerPoint = false,
                 bool retVertices = true,
                 bool retFacets = true,
                 bool retVolume = false,
                 bool retBarycenter = true,
                 int verbosity = 0){
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
  int intAlgMedian = 0;
  if (algMedian == "cutintwo"){
    intAlgMedian = 1;
  }else{
    if (algMedian == "downwards"){
      intAlgMedian = 2;
    }else{
      if (algMedian == "upwards"){
        intAlgMedian = 3;
      }else{
        if (algMedian == "bsbarydepth"){
          intAlgMedian = 4;
        }else{
          stop("Argument 'algMedian' should be a string with value either \
'cutintwo' or 'downwards' or 'upwards'");
        }
      }
    }
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
  // Transfrom data to the STL format
  TMatrix X(n);
  for (int i = 0; i < n; i++){
    X[i].resize(d);
    for (int j = 0; j < d; j++){
      X[i][j] = data(i, j);
    }
  }
  if (verbosity >= 2){
    Rcout << "Data constitutes " << n << " points of dimension " << d << endl;
    Rcout << "Computation of Tukey median" << endl;
  }
  // Define passing variables
  int medDepth = -1; // depth of the median
  int savedDepth = -2; // depth of the last calculated region
  int numRegions = 0; // number of copmuted regions
  vector<unsigned long long> hfspCodes(0);
  vector<unsigned long long> tmpHfspCodes;
  TPoint theInnerPoint(d);
  TPoint tmpInnerPoint(d);
  bool curState = false; // indicator whether last computation was successful
  int numRidges = 0; // number of checked ridges if using 'bfs' algorithm
  int tmpNumRidges = 0; // temporary variable for the number of ridges
  // Choose the algorithm
  if (intAlgMedian == 1){
    if (verbosity >= 2){
      Rcout << "Search of the median will be performed by cutting the " <<
        "domain in two parts" << endl;
    }
    // Following the article by Liu, Luo, and Zuo, cut in half
    int upperDepth = (n - d + 2) / 2 + 1; // unreachable upper bound
    // Reachable lower bound
    int lowerDepth = n / (double)(d + 1) + (1 - (double)1/(2 * (d + 1)));
    int curDepth = -1; // last tried depth
    while (upperDepth > lowerDepth){
      curDepth = (lowerDepth + upperDepth) / (double)2 + 0.25;
      //if (curState && curDepth == lowerDepth){
      //  break;
      //}
      if (curDepth == lowerDepth && hfspCodes.size() > 0){
        curState = true;
        break;
      }
      if (verbosity >= 2){
        Rcout << "Checking existence of region for depth value " << curDepth <<
          endl;
      }
      curState = TRegionCheckDepth(X, curDepth, intAlgRegion, algStart,
                                   numStart, &tmpHfspCodes, &tmpInnerPoint,
                                   &tmpNumRidges);
      numRegions++;
      //curState = TRegionCheckDepth(X, 3, intAlgRegion, algStart, numStart,
      //                             &hfspCodes, &theInnerPoint);
      //curState = TRegionCheckDepth(X, 2, intAlgRegion, algStart, numStart,
      //                             &hfspCodes, &theInnerPoint);
      //curState = TRegionCheckDepth(X, 1, intAlgRegion, algStart, numStart,
      //                             &hfspCodes, &theInnerPoint);
      //curState = TRegionCheckDepth(X, 2, intAlgRegion, algStart, numStart,
      //                             &hfspCodes, &theInnerPoint);
      //curState = true;
      // Update the bounds
      if (curState){
        hfspCodes = tmpHfspCodes;
        theInnerPoint = tmpInnerPoint;
        savedDepth = curDepth;
        medDepth = curDepth;
        lowerDepth = curDepth;
        numRidges = tmpNumRidges;
        if (verbosity >= 1){
          Rcout << "Depth region found for depth value " << curDepth << endl;
        }
      }else{
        upperDepth = curDepth;
        if (verbosity >= 1){
          Rcout << "Depth region not found for depth value " << curDepth <<
            endl;
        }
      }
    }
  }
  if (intAlgMedian == 2){
    if (verbosity >= 2){
      Rcout << "Search of the median will be performed by checking depth " <<
        "values starting from above" << endl;
    }
    // Following the article by Liu, Luo, and Zuo, check all, from above
    int upperDepth = (n - d + 2) / 2; // reachable upper bound
    int curDepth = upperDepth; // last tried depth
    while (curDepth > 0){
      if (verbosity >= 2){
        Rcout << "Checking existence of region for depth value " << curDepth <<
          endl;
      }
      curState = TRegionCheckDepth(X, curDepth, intAlgRegion, algStart,
                                   numStart, &tmpHfspCodes, &tmpInnerPoint,
                                   &tmpNumRidges);
      numRegions++;
      if (curState){
        hfspCodes = tmpHfspCodes;
        theInnerPoint = tmpInnerPoint;
        savedDepth = curDepth;
        medDepth = curDepth;
        numRidges = tmpNumRidges;
        if (verbosity >= 1){
          Rcout << "Depth region found for depth value " << curDepth << endl;
        }
        break;
      }else{
        curDepth--;
        if (verbosity >= 1){
          Rcout << "Depth region not found for depth value " << curDepth <<
            endl;
        }
      }
    }
  }
  if (intAlgMedian == 3){
    if (verbosity >= 2){
      Rcout << "Search of the median will be performed by checking depth " <<
        "values starting from below" << endl;
    }
    // Following the article by Liu, Luo, and Zuo, check all, from below
    int upperDepth = (n - d + 2) / 2 + 1; // unreachable upper bound
    int curDepth = 1; // last tried depth
    while(curDepth < upperDepth){
      if (verbosity >= 2){
        Rcout << "Checking existence of region for depth value " << curDepth <<
          endl;
      }
      curState = TRegionCheckDepth(X, curDepth, intAlgRegion, algStart,
                                   numStart, &tmpHfspCodes, &tmpInnerPoint,
                                   &tmpNumRidges);
      numRegions++;
      if (curState){
        hfspCodes = tmpHfspCodes;
        theInnerPoint = tmpInnerPoint;
        savedDepth = curDepth;
        medDepth = curDepth;
        curDepth++;
        numRidges = tmpNumRidges;
        if (verbosity >= 1){
          Rcout << "Depth region found for depth value " << curDepth << endl;
        }
      }else{
        curState = true;
        if (verbosity >= 1){
          Rcout << "Depth region not found for depth value " << curDepth <<
            endl;
        }
        break;
      }
    }
  }
  if (intAlgMedian == 4){
    if (verbosity >= 2){
      Rcout << "Search of the median will be performed by cutting the " <<
        "domain in two parts and updating lower bound " <<
          "due to the depth of the barycenter" << endl;
    }
    // Following the article by Liu, Luo, and Zuo, cut in half
    int upperDepth = (n - d + 2) / 2 + 1; // unreachable upper bound
    // Compute componentwise median
    TPoint cmedian(d);
    double* tmpOneCoord = new double[n]; // one coordinate of X
    for (int j = 0; j < d; j++){
      for (int i = 0; i < n; i++){
        tmpOneCoord[i] = X[i][j];
      }
      cmedian[j] = quick_select(tmpOneCoord, n);
    }
    // Compute its depth
    Environment ddalpha_env("package:ddalpha");
    Function ddalpha_depthhalfspace = ddalpha_env["depth.halfspace"];
    int cmedDepth = round(as<double>(ddalpha_depthhalfspace(wrap(cmedian), data,
                                                  true,
                                                  _["method"] = "plane")) * n);
    if (verbosity >= 2){
      Rcout << "Depth of the componentwise median: " << cmedDepth << endl;
    }
    delete[] tmpOneCoord;
    // Reachable lower bound is the largest of the two
    int lowerDepth = n / (double)(d + 1) + (1 - (double)1/(2 * (d + 1)));
    if (cmedDepth > lowerDepth){
      lowerDepth = cmedDepth;
    }
    int curDepth = -1; // last tried depth
    if (verbosity >= 1){
      Rcout << "Lower depth bound for binary search: " << lowerDepth << endl;
      Rcout << "Upper depth bound for binary search: " << upperDepth - 1 <<
        endl;
    }
    while (upperDepth > lowerDepth){
      curDepth = (lowerDepth + upperDepth) / (double)2 + 0.25;
      if (curDepth == lowerDepth && hfspCodes.size() > 0){
        curState = true;
        break;
      }
      if (verbosity >= 2){
        Rcout << "Checking existence of region for depth value " << curDepth <<
          endl;
      }
      curState = TRegionCheckDepth(X, curDepth, intAlgRegion, algStart,
                                   numStart, &tmpHfspCodes, &tmpInnerPoint,
                                   &tmpNumRidges);
      numRegions++;
      // Update the bounds
      if (curState){
        hfspCodes = tmpHfspCodes;
        theInnerPoint = tmpInnerPoint;
        savedDepth = curDepth;
        numRidges = tmpNumRidges;
        if (verbosity >= 2){
          Rcout << "Computation of the Tukey region's shape ..." << endl;
        }
        // Calculate vertices of the region polytope
        TMatrix vertexMatrix;
        getVertices(X, hfspCodes, theInnerPoint, &vertexMatrix);
        if (verbosity >= 2){
          Rcout << vertexMatrix.size() << " vertices computed" << endl;
        }
        if (verbosity >= 1){
          Rcout << "Depth region found for depth value " << curDepth << endl;
        }
        // Compute barycenter
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
        if (verbosity >= 2){
          Rcout << "region's barycenter computed" << endl;
        }
        medDepth = curDepth;
        lowerDepth = curDepth;
        // Compute depth of the barycenter
        curDepth = round(as<double>(ddalpha_depthhalfspace(wrap(center), data,
                                                true,
                                                _["method"] = "plane")) * n);
        if (curDepth > medDepth){
          medDepth = curDepth;
          lowerDepth = curDepth;
        }
        if (verbosity >= 1){
          Rcout << "Point (barycenter) found for depth value " <<
            curDepth << endl;
        }
      }else{
        upperDepth = curDepth;
        if (verbosity >= 1){
          Rcout << "Depth region not found for depth value " << curDepth <<
            endl;
        }
      }
    }
  }
  // If median finally found
  if (curState){
    if (medDepth == savedDepth){
      // Prepare the saved output
      IntegerMatrix halfspaces(hfspCodes.size(), d);
      TVariables pointNumbers;
      for (int i = 0; i < hfspCodes.size(); i++){
        getFacetPoints(hfspCodes[i], n, d, &pointNumbers);
        for (int j = 0; j < d; j++){
          halfspaces(i,j) = pointNumbers[j] + 1;
        }
      }
      // Prepared output - no new halfspaces search
      List ret = TukeyRegion(data, medDepth, method,
                             trgFacets, true, retHalfspaces, retHalfspacesNR,
                             retInnerPoint, retVertices, retFacets, retVolume,
                             retBarycenter, halfspaces, wrap(theInnerPoint),
                             verbosity);
      ret.push_back(numRidges, "numRidges");
      ret.push_back(numRegions, "numRegions");
      ret.attr("class") = "TukeyRegion";
      return ret;
    }else{
      // No output prepared, calculate
      List ret = TukeyRegion(data, medDepth, method,
                             trgFacets, true, retHalfspaces, retHalfspacesNR,
                             retInnerPoint, retVertices, retFacets, retVolume,
                             retBarycenter, IntegerMatrix(0), NumericVector(1),
                             verbosity);
      numRegions++;
      ret.push_back(numRegions, "numRegions");
      ret.attr("class") = "TukeyRegion";
      return ret;
    }
  }else{
    // Median could not be computed
    List ret = List::create();
    ret.push_back(medDepth, "depth");
    ret.push_back(numRegions, "numRegions");
    ret.push_back(false, "innerPointFound");
    return ret;
  }
}

// [[Rcpp::depends(rgl)]]
// [[Rcpp::export]]
void TukeyRegionPlot(List region, bool newPlot = true, bool drawPoints = true,
                     bool drawRidges = true,
                     CharacterVector colorBackground = "white",
                     CharacterVector colorPoints = "red",
                     CharacterVector colorFacets = "blue",
                     CharacterVector colorRidges = "green",
                     double lwd2D = 1,
                     int lty2D = 1,
                     double alpha = 1){
  // Input consistency check
  NumericMatrix points = as<NumericMatrix>(region["data"]);
  int n = points.nrow();
  int d = points.ncol();
  if (d != 2 && d != 3){
    stop("Region can be visualized in dimensions 2 and 3 only");
  }
  if (!region.containsElementNamed("facets")){
    stop("No facets computed, visualization impossible");
  }
  //if (!region.inherits("TukeyRegion")){
  //  stop("Argument 'region' should be an object of class 'TukeyRegion'");
  //}
  //if (!region.inherits("lm")) stop("Input must be a linear model");
  //Rcout << "Object seen." << endl;
  //NumericVector resid = as<NumericVector>(region["residuals"]);
  //Rcout << resid(1) << endl;

  //Environment global_env = Environment::global_env();
  //if(global_env.exists(std::string("package:rgl"))){
  //  Rcout << "rgl loaded." << endl;
  //}else{
  //  Rcout << "rgl not loaded." << endl;
  //}
  //return global_env.ls(true);

  int depth = (int)region["depth"];
  NumericMatrix vertices = as<NumericMatrix>(region["vertices"]);
  // If the dimension of the data is 2:
  if (d == 2){
    // Determine the dimensions of the plot
    double xMin = DBL_MAX;
    double yMin = DBL_MAX;
    double xMax = DBL_MIN;
    double yMax = DBL_MIN;
    if (drawPoints){ // then points determine the dimensions
      for (int i = 0; i < n; i++){
          if (points(i,0) < xMin){
            xMin = points(i,0);
          }
          if (points(i,0) > xMax){
            xMax = points(i,0);
          }
          if (points(i,1) < yMin){
            yMin = points(i,1);
          }
          if (points(i,1) > yMax){
            yMax = points(i,1);
          }
      }
    }else{ // then vertices determine the dimensions
      if (as<bool>(region["triangulated"])){ // 'facets' is a matrix
        IntegerMatrix facets = as<IntegerMatrix>(region["facets"]);
        int nFacets = facets.nrow();
        for (int i = 0; i < nFacets; i++){
          for (int j = 0; j < d; j++){
            if (vertices(facets(i,j) - 1,0) < xMin){
              xMin = vertices(facets(i,j) - 1,0);
            }
            if (vertices(facets(i,j) - 1,0) > xMax){
              xMax = vertices(facets(i,j) - 1,0);
            }
            if (vertices(facets(i,j) - 1,1) < yMin){
              yMin = vertices(facets(i,j) - 1,1);
            }
            if (vertices(facets(i,j) - 1,1) > yMax){
              yMax = vertices(facets(i,j) - 1,1);
            }
          }
        }
      }else{ // 'facets' is a list
        List facets = as<List>(region["facets"]);
        int nFacets = facets.length();
        for (int i = 0; i < nFacets; i++){
          // Transform to temporary variables
          IntegerVector tmpVertices = as<IntegerVector>(facets[i]);
          int nTmpVertices = tmpVertices.length();
          for (int j = 0; j < nTmpVertices; j++){
            if (vertices(tmpVertices(j) - 1,0) < xMin){
              xMin = vertices(tmpVertices(j) - 1,0);
            }
            if (vertices(tmpVertices(j) - 1,0) > xMax){
              xMax = vertices(tmpVertices(j) - 1,0);
            }
            if (vertices(tmpVertices(j) - 1,1) < yMin){
              yMin = vertices(tmpVertices(j) - 1,1);
            }
            if (vertices(tmpVertices(j) - 1,1) > yMax){
              yMax = vertices(tmpVertices(j) - 1,1);
            }
          }
        }
      }
    }
    // Load the 'graphics' environments
    Environment graphics_env("package:graphics");
    // Create plot
    if (newPlot){
      Function graphics_plot = graphics_env["plot"];
      graphics_plot(R_NilValue,
                    _["xlim"] = NumericVector::create(xMin, xMax),
                    _["ylim"] = NumericVector::create(yMin, yMax),
                    _["xlab"] = "x",
                    _["ylab"] = "y",
                    _["main"] = "Tukey region of depth " +
                      to_string(depth));
//                      (ostringstream() << depth).str());
    }
    // Plot the sample's points
    if (drawPoints){
      Function graphics_points = graphics_env["points"];
      graphics_points(points(_, 0), points(_, 1), _["col"] = colorPoints);
    }
    // Plot the region
    Function graphics_lines = graphics_env["lines"];
    if (as<bool>(region["triangulated"])){ // if facets triangulated - no ridges
      IntegerMatrix facets = as<IntegerMatrix>(region["facets"]);
      int nFacets = facets.nrow();
      for (int i = 0; i < nFacets; i++){
        for (int j = 0; j < d; j++){
          graphics_lines(NumericVector::create(vertices(facets(i,0) - 1,0),
                                               vertices(facets(i,1) - 1,0)),
                         NumericVector::create(vertices(facets(i,0) - 1,1),
                                               vertices(facets(i,1) - 1,1)),
                                               _["col"] = colorFacets,
                                               _["lwd"] = NumericVector::create(lwd2D),
                                               _["lty"] = NumericVector::create(lty2D));
        }
      }
    }else{ //if facets are not triangulated - same thing as we are in 2D
      List facets = as<List>(region["facets"]);
      int nFacets = facets.length();
      for (int i = 0; i < nFacets; i++){
        // Transform to temporary variables
        IntegerVector tmpVertices = as<IntegerVector>(facets[i]);
        int nTmpVertices = tmpVertices.length();
        for (int j = 0; j < nTmpVertices; j++){
          graphics_lines(NumericVector::create(vertices(tmpVertices(0) - 1,0),
                                               vertices(tmpVertices(1) - 1,0)),
                         NumericVector::create(vertices(tmpVertices(0) - 1,1),
                                               vertices(tmpVertices(1) - 1,1)),
                                               _["col"] = colorFacets,
                                               _["lwd"] = NumericVector::create(lwd2D),
                                               _["lty"] = NumericVector::create(lty2D));
        }
      }
    }
    return;
  }
  // If dimension of the data is 3:
  // Load the 'rgl' environment
  Environment rgl_env("package:rgl");
  // Create scene
  if (newPlot){
    Function rgl_open = rgl_env["rgl.open"];
    rgl_open();
  }
  Function rgl_bg = rgl_env["rgl.bg"];
  rgl_bg(_["color"] = colorBackground);
  // Define radius of a single point
  double radius = 0.025;
  // Plot the sample's points
  if (drawPoints){
    Function rgl_spheres = rgl_env["rgl.spheres"];
    //Function rgl_points3d = rgl_env["points3d"];
    rgl_spheres(points(_,0), points(_,1), points(_,2), radius,
                List::create(_["color"] = colorPoints));
    //rgl_points3d(points(_,0), points(_,1), points(_,2),
    //             _["color"] = "red");
  }
  // Plot the region
  if (as<bool>(region["triangulated"])){ // if facets triangulated - no ridges
    // Assemble matrix of simple triangles
    IntegerMatrix facets = as<IntegerMatrix>(region["facets"]);
    int nFacets = facets.nrow();
    NumericMatrix triangles(nFacets * d, d);
    for (int i = 0; i < nFacets; i++){
      for (int j = 0; j < d; j++){
        for (int k = 0; k < d; k++)
          triangles(i * d + j,k) = vertices(facets(i,j) - 1,k);
      }
    }
    // Draw simple triagnles
    Function rgl_triangles = rgl_env["rgl.triangles"];
    rgl_triangles(triangles(_,0), triangles(_,1), triangles(_,2),
                  _["color"] = colorFacets, _["alpha"] = alpha);
  }else{ //if facets are not triangulated - Delaunay and qhull each of them
    // Prepare data
    List facets = as<List>(region["facets"]);
    int nFacets = facets.length();
    int d = points.ncol();
    Function rgl_triangles = rgl_env["rgl.triangles"];
    Function rgl_lines = rgl_env["rgl.lines"];
    // Go through all facets
    NumericMatrix aTriangle(d, d);
    NumericMatrix aLine(2, d);
    TPoint trianglesX(0);TPoint ridgesX(0);
    TPoint trianglesY(0);TPoint ridgesY(0);
    TPoint trianglesZ(0);TPoint ridgesZ(0);
    for (int i = 0; i < nFacets; i++){
      // Transform to temporary variables
      IntegerVector tmpVertices = as<IntegerVector>(facets[i]) - 1;
      TVariables fVertices = as<TVariables>(tmpVertices);

      /*
      if (fVertices.size() == d){ // if the facet is a simplex
        // Plot the facet directly
        for (int j = 0; j < d; j++){
          for (int k = 0; k < d; k++){
            aTriangle(j,k) = vertices(fVertices[j],k);
          }
        }
        rgl_triangles(aTriangle(_,0), aTriangle(_,1), aTriangle(_,2), _["color"] = "green");
        continue;
      }else{
        // Triangulate the facet and find its ridges
      }
      */

      // Define the facet's two-domensional basis
      // (we do not care about the orthonormality because of affine invariance)
      TMatrix basis(2);basis[0] = TPoint(3);basis[1] = TPoint(3);
      basis[0][0] = vertices(fVertices[1],0) - vertices(fVertices[0],0);
      basis[0][1] = vertices(fVertices[1],1) - vertices(fVertices[0],1);
      basis[0][2] = vertices(fVertices[1],2) - vertices(fVertices[0],2);
      basis[1][0] = vertices(fVertices[2],0) - vertices(fVertices[0],0);
      basis[1][1] = vertices(fVertices[2],1) - vertices(fVertices[0],1);
      basis[1][2] = vertices(fVertices[2],2) - vertices(fVertices[0],2);
      double norm0 = sqrt(pow(basis[0][0], 2) + pow(basis[0][1], 2) + pow(basis[0][2], 2));
      double prj1 = basis[1][0] * basis[0][0] + basis[1][1] * basis[0][1] + basis[1][2] * basis[0][2];
      basis[1][0] = basis[1][0] - prj1 / norm0 * basis[0][0] / norm0;
      basis[1][1] = basis[1][1] - prj1 / norm0 * basis[0][1] / norm0;
      basis[1][2] = basis[1][2] - prj1 / norm0 * basis[0][2] / norm0;
      // Project onto the two-dimensional subspace
      TMatrix fVertices2d(fVertices.size());
      for (int j = 0; j < fVertices.size(); j++){
        fVertices2d[j] = TPoint(2);
        fVertices2d[j][0] = (vertices(fVertices[j],0) -
          vertices(fVertices[0],0)) * basis[0][0] +
          (vertices(fVertices[j],1) - vertices(fVertices[0],1)) * basis[0][1] +
          (vertices(fVertices[j],2) - vertices(fVertices[0],2)) * basis[0][2];
        fVertices2d[j][1] = (vertices(fVertices[j],0) -
          vertices(fVertices[0],0)) * basis[1][0] +
          (vertices(fVertices[j],1) - vertices(fVertices[0],1)) * basis[1][1] +
          (vertices(fVertices[j],2) - vertices(fVertices[0],2)) * basis[1][2];
        //Rcout << fVertices2d[j][0] << "    " << fVertices2d[j][1] << endl;
      }
      //Rcout << endl;
      // Sort the point indices
      vector<IndexRec> angles(fVertices.size() - 1);
      for (int j = 1; j < fVertices.size(); j++){
        angles[j - 1] = IndexRec(fVertices[j], atan2(fVertices2d[j][1], fVertices2d[j][0]));
      }
      sort(angles.begin(), angles.end(), compareAsc);
      angles.insert(angles.begin(), IndexRec(fVertices[0], 0));
      angles.insert(angles.end(), IndexRec(fVertices[0], 0));
      //if (i == 22){
      //  for (int j = 0; j < angles.size(); j++){
      //    Rcout << angles[j].index + 1 << ": " << angles[j].value << endl;
      //  }
      //  Rcout << endl;
      //}
      // Plot sequentually facet's triangles and ridges
      for (int j = 0; j < angles.size() - 1; j++){
        aLine(0,0) = vertices(angles[j].index,0);
        aLine(0,1) = vertices(angles[j].index,1);
        aLine(0,2) = vertices(angles[j].index,2);
        aLine(1,0) = vertices(angles[j + 1].index,0);
        aLine(1,1) = vertices(angles[j + 1].index,1);
        aLine(1,2) = vertices(angles[j + 1].index,2);
        ridgesX.push_back(vertices(angles[j].index,0));
        ridgesY.push_back(vertices(angles[j].index,1));
        ridgesZ.push_back(vertices(angles[j].index,2));
        ridgesX.push_back(vertices(angles[j + 1].index,0));
        ridgesY.push_back(vertices(angles[j + 1].index,1));
        ridgesZ.push_back(vertices(angles[j + 1].index,2));
        //rgl_lines(aLine(_,0), aLine(_,1), aLine(_,2), _["color"] = "green");
        if ((j != 0) && (j != angles.size() - 2)){
          aTriangle(2,0) = vertices(angles[0].index,0);
          aTriangle(2,1) = vertices(angles[0].index,1);
          aTriangle(2,2) = vertices(angles[0].index,2);
          aTriangle(0,0) = vertices(angles[j].index,0);
          aTriangle(0,1) = vertices(angles[j].index,1);
          aTriangle(0,2) = vertices(angles[j].index,2);
          aTriangle(1,0) = vertices(angles[j + 1].index,0);
          aTriangle(1,1) = vertices(angles[j + 1].index,1);
          aTriangle(1,2) = vertices(angles[j + 1].index,2);
          trianglesX.push_back(vertices(angles[0].index,0));
          trianglesY.push_back(vertices(angles[0].index,1));
          trianglesZ.push_back(vertices(angles[0].index,2));
          trianglesX.push_back(vertices(angles[j].index,0));
          trianglesY.push_back(vertices(angles[j].index,1));
          trianglesZ.push_back(vertices(angles[j].index,2));
          trianglesX.push_back(vertices(angles[j + 1].index,0));
          trianglesY.push_back(vertices(angles[j + 1].index,1));
          trianglesZ.push_back(vertices(angles[j + 1].index,2));
          //rgl_triangles(aTriangle(_,0), aTriangle(_,1), aTriangle(_,2), _["color"] = "yellow");
        }
      }
    }
    rgl_triangles(wrap(trianglesX), wrap(trianglesY), wrap(trianglesZ), _["color"] = colorFacets, _["alpha"] = alpha);
    if (drawRidges){
      rgl_lines(wrap(ridgesX), wrap(ridgesY), wrap(ridgesZ), _["color"] = colorRidges, _["alpha"] = alpha);
    }
  }
}

//// [[Rcpp::export(print.TukeyRegion)]]
//void TRegionPrint(List region){
//  Rcout << "Print called." << endl;
//  NumericMatrix points = as<NumericMatrix>(region["data"]);
//  int n = points.nrow();
//  int d = points.ncol();
//  Rcout << "Object of class 'TukeyRegion':" << endl;
//  Rcout << "Input data is a " << n << "x" << d << " matrix" << endl;
//}

// [[Rcpp::export]]
void TukeyRegionSummary(List region){
  NumericMatrix points = as<NumericMatrix>(region["data"]);
  int n = points.nrow();
  int d = points.ncol();
  int depth = as<int>(region["depth"]);
  Rcout << "Object of class 'TukeyRegion':" << endl << endl;
  Rcout << "Input data is a " << n << "x" << d << " matrix" << endl;
  Rcout << "Required depth level is " << depth << endl;
  if (!as<bool>(region["halfspacesFound"])){
    Rcout << endl;
    Rcout << "No halfspaces could be found, the Tukey region does not" <<
      " exist for required depth level" << endl;
    return;
  }
  if (region.containsElementNamed("halfspaces")){
    Rcout << endl;
    NumericMatrix halfspaces = as<NumericMatrix>(region["halfspaces"]);
    Rcout << halfspaces.nrow() << " halfspaces define the region" << endl;
  }
  if (!as<bool>(region["innerPointFound"])){
    Rcout << endl;
    Rcout << "Inner point not found, the Tukey region does not" <<
      " exist for required depth level" << endl;
    return;
  }
  if (region.containsElementNamed("innerPoint")){
    Rcout << endl;
    NumericVector innerPoint = as<NumericVector>(region["innerPoint"]);
    Rcout << "Inner point of the region is located in " << endl <<
      innerPoint << endl;
  }
  if (region.containsElementNamed("halfspacesNR")){
    Rcout << endl;
    NumericMatrix halfspacesNR = as<NumericMatrix>(region["halfspacesNR"]);
    Rcout << halfspacesNR.nrow() << " halfspaces are non-redundant" << endl;
  }
  if (region.containsElementNamed("vertices")){
    Rcout << endl;
    NumericMatrix vertices = as<NumericMatrix>(region["vertices"]);
    Rcout << "The region has " << vertices.nrow() << " vertices";
    if(region.containsElementNamed("facets")){
      if (as<bool>(region["triangulated"])){
        NumericMatrix facets = as<NumericMatrix>(region["facets"]);
        Rcout << " and " << facets.nrow() << " hypertriangles defining facets";
      }else{
        List facets = as<List>(region["facets"]);
        Rcout << " and " << facets.length() << " facets";
      }
    }
  }
  if (region.containsElementNamed("volume") ||
      region.containsElementNamed("barycenter")){
    Rcout << endl;
  }
  if (region.containsElementNamed("volume")){
    double volume = as<double>(region["volume"]);
    Rcout << "The region's volume equals " << volume << endl;
  }
  if (region.containsElementNamed("barycenter")){
    NumericVector barycenter = as<NumericVector>(region["barycenter"]);
    Rcout << "The region's barycenter is in " << endl << barycenter << endl;
  }
//  Rcout << "Summary called." << endl;
}

// [[Rcpp::export]]
List TukeyKRegions(NumericMatrix data, int maxDepth,
                 String method = "bfs",
                 bool trgFacets = false,
                 bool checkInnerPoint = true,
                 bool retHalfspaces = true,
                 bool retHalfspacesNR = false,
                 bool retInnerPoint = false,
                 bool retVertices = false,
                 bool retFacets = false,
                 bool retVolume = false,
                 bool retBarycenter = false,
                 int verbosity = 0){
  // Create the return structure for Regions
  IntegerMatrix halfspaces = IntegerMatrix(0);
  NumericVector innerPoint = NumericVector(1);
  List ret = List::create();
  queue<TVariables*>* ridges = new queue<TVariables*>[1];
  List ret1 = TukeyRegionTau(data, 1, method, trgFacets, checkInnerPoint,
                             retHalfspaces, retHalfspacesNR, retInnerPoint,
                             retVertices, retFacets, retVolume,
                             retBarycenter, ridges, halfspaces, innerPoint,
                             verbosity);
  ret.push_back(ret1);
  delete[] ridges;
  if (maxDepth > 1){
    // Declare the pointer for ridges
    ridges = new queue<TVariables*>[1];
    for (int i = 2; i <= maxDepth; i++){
      // Go through all regions
      List retTmp = TukeyRegionTau(data, i, method, trgFacets, checkInnerPoint,
                                   retHalfspaces, retHalfspacesNR,
                                   retInnerPoint, retVertices, retFacets,
                                   retVolume, retBarycenter, ridges,
                                   halfspaces, innerPoint, verbosity);
      ret.push_back(retTmp);
    }
    delete[] ridges;
  }
  ret.attr("class") = "TukeyRegionsList";
  return ret;
}

// [[Rcpp::export]]
List TukeyRegions(NumericMatrix data, NumericVector depths,
                  String method = "bfs",
                  bool trgFacets = false,
                  bool checkInnerPoint = true,
                  bool retHalfspaces = true,
                  bool retHalfspacesNR = false,
                  bool retInnerPoint = false,
                  bool retVertices = false,
                  bool retFacets = false,
                  bool retVolume = false,
                  bool retBarycenter = false,
                  int verbosity = 0){
  int nDepths = depths.length();
  IntegerMatrix halfspaces = IntegerMatrix(0);
  NumericVector innerPoint = NumericVector(1);
  List ret = List::create();
  for (int i = 0; i < nDepths; i++){
    List retTmp = TukeyRegion(data, depths(i), method, trgFacets,
                              checkInnerPoint, retHalfspaces, retHalfspacesNR,
                              retInnerPoint, retVertices, retFacets,
                              retVolume, retBarycenter,
                              halfspaces, innerPoint, verbosity);
    ret.push_back(retTmp);
  }
  ret.attr("class") = "TukeyRegionsList";
  return ret;
}

// [[Rcpp::export]]
List TukeyKMedian(NumericMatrix data,
                  String algMedian = "upwards",
                  String method = "bfs",
                  bool trgFacets = true,
                  bool retHalfspaces = false,
                  bool retHalfspacesNR = false,
                  bool retInnerPoint = false,
                  bool retVertices = true,
                  bool retFacets = true,
                  bool retVolume = false,
                  bool retBarycenter = true,
                  int verbosity = 0){
  // Check input consistency
  int n = data.nrow();
  int d = data.ncol();
  if (d < 2 || n <= d){
    stop("Argument 'data' should be a matrix with at least d = 2 columns and \
           at least d + 1 rows");
  }
  if (algMedian != "upwards" || method != "bfs"){
    stop("This function only treats arguments 'algMedian' 'upwards' and 'method' \
'bfs', for other combinations of there two arguments use funciton \
'TukeyMedian'.");
  }
  if (verbosity < 0 || verbosity > 2){
    stop("Argument 'verbosity' should be an integer equal 0, 1, or 2");
  }
  // Until which depth to check
  int maxDepth = floor((n - d + 1) / 2);
  // Create the return structure for Regions
  IntegerMatrix halfspaces = IntegerMatrix(0);
  NumericVector innerPoint = NumericVector(1);
  List retRegion;
  List retLast;
  // Calculate the first region
  queue<TVariables*>* ridges = new queue<TVariables*>[1];
  retRegion = TukeyRegionTau(data, 1, method, trgFacets, true,
                             retHalfspaces, retHalfspacesNR, retInnerPoint,
                             retVertices, retFacets, retVolume,
                             retBarycenter, ridges, halfspaces, innerPoint,
                             verbosity);
  delete[] ridges;
  if (maxDepth < 2){
    return retRegion;
  }
  // Calculate and check the resting regions
  ridges = new queue<TVariables*>[1];
  for (int i = 2; i <= maxDepth; i++){
    // Go through all regions
    retLast = TukeyRegionTau(data, i, method, trgFacets, true,
                             retHalfspaces, retHalfspacesNR,
                             retInnerPoint, retVertices, retFacets,
                             retVolume, retBarycenter, ridges,
                             halfspaces, innerPoint, verbosity);
    if (as<bool>(retLast["innerPointFound"])){
      retRegion = retLast;
    }else{
      delete[] ridges;
      return retRegion;
    }
  }
  // Median could not be computed
  List ret = List::create();
  ret.push_back(maxDepth, "depth");
  ret.push_back(false, "innerPointFound");
  return ret;
}
