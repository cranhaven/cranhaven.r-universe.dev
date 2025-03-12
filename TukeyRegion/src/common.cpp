// common.cpp
// By Pavlo Mozharovskyi
// Last changed 22.09.2017
// Commonly used functions

#include "TukeyRegion.h"

//#include <chrono>
//chrono::duration<double> time_pureSorting;
//chrono::duration<double> time_createAngles;

TDMatrix asMatrix(double* arr, int n, int d){
  TDMatrix mat = new double*[n];
  for (int i = 0; i < n; i++)
    mat[i] = arr + i*d;
  return mat;
}

int Compare(SortIndex &si1, SortIndex &si2){
  return si2.value < si1.value;
}

void Swap(SortIndex *si1, SortIndex *si2){
  SortIndex siTmp = *si1;
  *si1 = *si2;
  *si2 = siTmp;
}

int Compare(Feval &fe1, Feval &fe2){
  return fe1.val < fe2.val;
}

void Swap(Feval *fe1, Feval *fe2){
  Feval feTmp = *fe1;
  *fe1 = *fe2;
  *fe2 = feTmp;
}

int Compare(Coords &co1, Coords &co2){
  if (co1.d == co2.d){
    if (co1.d - co1.nCoords == co2.d - co2.nCoords){
      for (int i = 0; i < co1.d - co1.nCoords - 1; i++){
        if (co1.fixedCoords[i] != co2.fixedCoords[i]){
          return co1.fixedCoords[i] < co2.fixedCoords[i];
        }
      }
      return co1.fixedCoords[co1.d - co1.nCoords - 1] <
        co2.fixedCoords[co2.d - co2.nCoords - 1];
    }else{
      return co1.d - co1.nCoords < co2.d - co2.nCoords;
    }
  }else{
    return co1.d < co2.d;
  }
}

void Swap(Coords *co1, Coords *co2){
  Coords coTmp = *co1;
  *co1 = *co2;
  *co2 = coTmp;
}

/*
 *  This Quickselect routine is based on the algorithm described in
 *  "Numerical recipes in C", Second Edition,
 *  Cambridge University Press, 1992, Section 8.5, ISBN 0-521-43108-5
 *  This code by Nicolas Devillard - 1998. Public domain.
 *
 *  This code has been downloaded from http://ndevilla.free.fr/median/median/
 */
//#define ELEM_SWAP(a,b) { register elem_type t=(a);(a)=(b);(b)=t; }
#define ELEM_SWAP(a,b) { elem_type t=(a);(a)=(b);(b)=t; }
elem_type quick_select(elem_type arr[], int n) {
  int low, high ;
  int median;
  int middle, ll, hh;

  low = 0 ; high = n-1 ; median = (low + high) / 2;
  for (;;) {
    if (high <= low) /* One element only */
      return arr[median] ;

    if (high == low + 1) {  /* Two elements only */
      if (arr[low] > arr[high])
      ELEM_SWAP(arr[low], arr[high]) ;
      return arr[median] ;
    }

    /* Find median of low, middle and high items; swap into position low */
    middle = (low + high) / 2;
    if (arr[middle] > arr[high])    ELEM_SWAP(arr[middle], arr[high]) ;
    if (arr[low] > arr[high])       ELEM_SWAP(arr[low], arr[high]) ;
    if (arr[middle] > arr[low])     ELEM_SWAP(arr[middle], arr[low]) ;

    /* Swap low item (now in position middle) into position (low+1) */
    ELEM_SWAP(arr[middle], arr[low+1]) ;

    /* Nibble from each end towards middle, swapping items when stuck */
    ll = low + 1;
    hh = high;
    for (;;) {
      do ll++; while (arr[low] > arr[ll]) ;
      do hh--; while (arr[hh]  > arr[low]) ;

      if (hh < ll)
        break;

      ELEM_SWAP(arr[ll], arr[hh]) ;
    }

    /* Swap middle item (in position low) back into correct position */
    ELEM_SWAP(arr[low], arr[hh]) ;

    /* Re-set active partition */
    if (hh <= median)
      low = ll;
    if (hh >= median)
      high = hh - 1;
  }
}
#undef ELEM_SWAP
/*
int bin_searchl_routine(double* x, int n, double val){
  int min = 0;
  int max = n + 1;
  int mid = (min + max) / 2;
  while((x[mid - 1] > val) || (val >= x[mid])){
    mid = (min + max) / 2;
    if (mid == 0){
      return 0;
    }
    if (mid == n){
      if ((x[0] == val) && (x[mid - 1] == val)){
        // return -1; // coincide with all the points
        return n;
      }else{
        return n;
      }
    }
    if (x[mid - 1] > val){
      max = mid;
    }else{
      min = mid;
    }
  }
  return mid;
}

int bin_searchr_routine(double* x, int n, double val){
  int min = 0;
  int max = n + 1;
  int mid = (min + max) / 2;
  while((x[mid - 1] >= val) || (val > x[mid])){
    mid = (min + max) / 2;
    if (mid == 0){
      if ((x[0] == val) && (x[mid - 1] == val)){
        // return -1; // coincide with all the points
        return n;
      }else{
        return n;
      }
    }
    if (mid == n){
      return 0;
    }
    if (x[mid - 1] >= val){
      max = mid;
    }else{
      min = mid;
    }
  }
  return n - mid;
}
*/

// Returns number of combinations without repetitions of 'k' out of 'n' numbers
unsigned long long choose(unsigned long long n, unsigned long long k){
  unsigned long long r = n--; unsigned long long d = 2;
  while (d <= k) { r *= n--; r /= d++; }
  return r;
}

// Finds points in 'XPrj' cutting off 'intTau' points from 'XPrj'
// by a line through them from the origin, and saves their numbers in 'XPrj'
// in 'facetIndices'.
// Returns false if no points found, true otherwise.
bool getFacets(TMatrix &XPrj, int intTau, TVariables *curCmb,
               vector<int> *facetIndicis){
  //chrono::time_point<chrono::steady_clock> t1, t2;
  int numPoints = XPrj.size();facetIndicis->resize(0);
  // Calculate angles and separate zero projections
  //t1 = chrono::high_resolution_clock::now();
  vector<IndexRec> posAngles(numPoints);
  vector<IndexRec> negAngles(numPoints);
  int numPosAngles = 0; int numNegAngles = 0;
  int posInCurCmb = 0;
  for (int i = 0; i < numPoints; i++){
    //if (hypot(XPrj[i][0], XPrj[i][1]) > eps_HD2){
    if (i == (*curCmb)[posInCurCmb]) {
      posInCurCmb = (posInCurCmb >= curCmb->size() - 1)
      ? posInCurCmb : posInCurCmb + 1;
      continue;
    } else {
      double curAngle = atan2(XPrj[i][1], XPrj[i][0]);
      if (curAngle >= M_PI){curAngle = -M_PI;}
      if (curAngle >= 0){
        posAngles[numPosAngles++] = IndexRec(i, curAngle);
      }else{
        negAngles[numNegAngles++] = IndexRec(i, M_PI + curAngle);
      }
    }
  }
  posAngles.resize(numPosAngles); negAngles.resize(numNegAngles);
  //t2 = chrono::high_resolution_clock::now();
  //time_createAngles += t2 - t1;
  int numNonZero = numPosAngles + numNegAngles;
  //if (numNonZero != numPoints - 2) {
  //	cout << numNonZero << endl;
  //}
  if (numNonZero < intTau + 1){ // if not enough points
    return false;
  }
  //t1 = chrono::high_resolution_clock::now();
  sort(posAngles.begin(), posAngles.end(), compareAsc);
  sort(negAngles.begin(), negAngles.end(), compareAsc);
  //t2 = chrono::high_resolution_clock::now();
  //time_pureSorting += t2 - t1;
  if (negAngles.size() == 0){ // if no points under the abscisse
    if (posAngles.size() >= intTau + 1){
      facetIndicis->push_back(posAngles[intTau].index);
      int curIndex = posAngles[posAngles.size() - intTau - 1].index;
      if (curIndex != (*facetIndicis)[facetIndicis->size() - 1]){
        facetIndicis->push_back(curIndex);
      }
      return true;
    }
    return false;
  }
  if (posAngles.size() == 0){ // if no points above the ordinate
    if (negAngles.size() >= intTau + 1){
      facetIndicis->push_back(negAngles[intTau].index);
      int curIndex = negAngles[negAngles.size() - intTau - 1].index;
      if (curIndex != (*facetIndicis)[facetIndicis->size() - 1]){
        facetIndicis->push_back(curIndex);
      }
      return true;
    }
    return false;
  }
  int posIndex = -1;int negIndex = -1;
  int numInHalfspace = posAngles.size();
  bool posSide = true;
  // Main cycle - check all points' angles
  while (posIndex + 1 < posAngles.size() && negIndex + 1 < negAngles.size()){
    if (posAngles[posIndex + 1].value < negAngles[negIndex + 1].value){
      posIndex++;
      if (posSide){
        numInHalfspace--;
      }else{
        posSide = true;
      }
    }else{
      negIndex++;
      if (!posSide){
        numInHalfspace++;
      }else{
        posSide = false;
      }
    }
    if (numInHalfspace == intTau ||
        numNonZero - numInHalfspace - 1 == intTau){
      if (posSide){
        facetIndicis->push_back(posAngles[posIndex].index);
      }else{
        facetIndicis->push_back(negAngles[negIndex].index);
      }
    }

  }
  if (posIndex + 1 == posAngles.size()){ // if any points below abscisse left
    if (intTau >= numInHalfspace &&
        intTau <= numInHalfspace + negAngles.size() - negIndex - 2){
      facetIndicis->push_back(
          negAngles[negIndex + intTau - numInHalfspace + 1].index);
    }
    if (intTau >= numNonZero - (numInHalfspace + negAngles.size()
                                  - negIndex - 2) - 1 &&
                                    intTau <= numNonZero - numInHalfspace - 1){
      int curIndex = negAngles[negIndex +
        ((numNonZero - numInHalfspace - 1) - intTau) + 1].index;
      if (curIndex != (*facetIndicis)[facetIndicis->size() - 1]){
        facetIndicis->push_back(curIndex);
      }
    }
  }
  if (negIndex + 1 == negAngles.size()){ // if any points above abscisse left
    if (intTau >= numInHalfspace - (posAngles.size() - posIndex - 2) &&
        intTau <= numInHalfspace){
      facetIndicis->push_back(
          posAngles[posIndex + numInHalfspace - intTau + 1].index);
    }
    if (intTau >= numNonZero - numInHalfspace - 1 &&
        intTau <= numNonZero -
        (numInHalfspace - (posAngles.size() - posIndex - 2)) - 1){
      int curIndex = posAngles[posIndex +
        (intTau - (numNonZero - numInHalfspace - 1)) + 1].index;
      if (curIndex != (*facetIndicis)[facetIndicis->size() - 1]){
        facetIndicis->push_back(curIndex);
      }
    }
  }
  if (facetIndicis->size() > 0){ // if any points found
    return true;
  }
  return false;
}

// Generates a vector uniformly distributed on the unit unit sphere in R^d
// and saves it into 'normal'
void getRndNormal(int d, TPoint *normal){
  boost::random::mt19937_64 rEngine;
  rEngine.seed(time(NULL));
  boost::random::normal_distribution<double> normDist;
  normal->resize(d);
  double norm = 0;
  for (int i = 0; i < d; i++){
    (*normal)[i] = normDist(rEngine);
    norm += pow((*normal)[i], 2);
  }
  norm = sqrt(norm);
  for (int i = 0; i < d; i++){
    (*normal)[i] /= norm;
  }
}

// Obtains the starting combination of points cuttig off 'intTau' points
// from 'X' by a hyperplane through them, and saves their numbers in 'X'
// in 'cmb'.
// Returns true if succeeded in MAX_ITER_FIRST tries, false otherwise.
bool getFirstCombination(TMatrix &X, int intTau, TVariables *cmb){
  int n = X.size();
  int d = X[0].size();
  int corIntTau = intTau;
  cmb->resize(d);
  int numIter = 0;
  // Main cycle: try MAX_ITER_FIRST times
  TPoint norm(d);
  TMatrix curPrj(n);
  for (int i = 0; i < n; i++){
    curPrj[i] = TPoint(1);
  }
  while (numIter < MAX_ITER_FIRST){
    // Generate normal
    getRndNormal(d, &norm);
    TMatrix basis(1);basis[0] = norm;
    // Project X onto the normal
    getProjection(X, basis, &curPrj);
    vector<IndexRec> projectionFirst(n);
    for (int i = 0; i < n; i++){
      projectionFirst[i].index = i;
      projectionFirst[i].value = curPrj[i][0];
    }
    sort(projectionFirst.begin(), projectionFirst.end(), compareAsc);
    // Choose points # 'intTau',...,'intTau + d - 1' from below
    for (int i = 0; i < d; i++){
      (*cmb)[i] = projectionFirst[corIntTau + i].index;
    }
    // Check whether it cuts of 'intTau' points
    if (checkFirstCombination(X, intTau, *cmb)){
      //Rcout << "Combination found during " << numIter + 1 << " iteration." << endl;
      //cmb->erase(cmb->end() - 1);
      sort(cmb->begin(), cmb->end());
      // Add all the points lying below the ridge
      for (int i = 0; i < corIntTau; i++){
        cmb->push_back(projectionFirst[i].index);
      }
      return true;
    }
    numIter++;
  }
  return false;
}

// Projects 'X' onto basis 'basis', and saves it into 'XPrj'
void getProjection(TMatrix &X, TMatrix &basis, TMatrix* XPrj){
  //XPrj->resize(X.size());
  int d = X[0].size();
  int newDim = basis.size();
  for (int i = 0; i < X.size(); i++){
    //(*XPrj)[i] = TPoint(newDim);
    for (int j = 0; j < newDim; j++){
      (*XPrj)[i][j] = 0;
      for (int k = 0; k < d; k++){
        (*XPrj)[i][j] += X[i][k]*basis[j][k];
      }
    }
  }
}

// Returns 8-byte code of a hyperplane (potential facet) lying on d points
// with their numbers in 'facet' as
// #0 + #1*n + #2*n^2 + ... #(d-1)*n^(d-1)
unsigned long long getFacetCode(TVariables &facet, int n){
  unsigned long long result = 0;
  for (int i = 0; i < facet.size(); i++){
    result += facet[i]*pow(n, i);
  }
  return result;
}

// Decodes the facet as in 'getFacetCode', and save point numbers in
// 'pointNumbers'
void getFacetPoints(unsigned long long facetCode, int n, int d,
                    TVariables *pointNumbers){
  pointNumbers->resize(d);
  unsigned long long curFacetCode = facetCode;
  for (int i = 0; i < d; i++){
    (*pointNumbers)[i] = curFacetCode % n;
    curFacetCode /= n;
  }
}

// Returns true if a hyperplane through d points enumerated in 'cmb'
// cuts of 'intTau' points from 'X', false otherwise
bool checkFirstCombination(TMatrix &X, int intTau, TVariables &cmb){
  int n = X.size();
  int d = X[0].size();
  TMatrix A(d - 1);
  for (int i = 1; i < d; i++){
    A[i - 1] = TPoint(d);
    for (int j = 0; j < d; j++){
      A[i - 1][j] = X[cmb[i]][j] - X[cmb[0]][j];
    }
  }
  TPoint normal;
  getNormal(A, &normal);
  TPoint projection(n);
  for (int i = 0; i < n; i++){
    for (int j = 0; j < d; j++){
      projection[i] += X[i][j] * normal[j];
    }
  }
  int pointsAbove = 0;int pointsBelow = 0;
  for (int i = 0; i < n; i++){
    if (projection[i] > projection[cmb[0]] + eps){pointsAbove++;}
    if (projection[i] < projection[cmb[0]] - eps){pointsBelow++;}
  }
  if (pointsAbove == intTau || pointsBelow == intTau){return true;}
  return false;
}

// By Rainer Dyckerhoff
// Calculates vector normal to hyperplane through d - 1 points from 'A'
// and the origin, and returns false if it is not possible
bool getNormal(TMatrix &A, TPoint *normal){
  int imax,jmax;
  int d = A[0].size();
  int* colp = new int[d];
  normal->resize(d);
  double amax;
  for (int k = 0; k < d - 1; k++) {
    imax = k;
    amax = abs(A[k][k]);
    colp[k] = k;
    // Spaltenmaximum finden
    for (int i = k + 1; i < d - 1; i++) {
      if (abs(A[i][k]) > amax) {
        amax = abs(A[i][k]);
        imax = i;
      }
    }
    // Spaltenmaximum gleich null => complete pivoting
    if (amax < eps_pivot) {
      for (int j = k + 1; j < d; j++) {
        for (int i = k; i < d - 1; i++) {
          if (abs(A[i][j]) > amax) {
            amax = abs(A[i][j]);
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
      for (int i = 0; i < d - 1; i++) {
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
    }
    // Elimination
    for (int i = k + 1; i < d - 1; i++) {
      double factor = A[i][k] / A[k][k];
      for (int j = k + 1; j < d; j++) A[i][j] -= factor * A[k][j];
    }
  }
  // R?cksubstituition
  colp[d - 1] = d - 1;
  (*normal)[d - 1] = -1;
  for (int k = d - 2; k >= 0; k--) {
    (*normal)[k] = A[k][d-1] / A[k][k];
    for (int i = k - 1; i >= 0; i--) A[i][d-1] -= (*normal)[k] * A[i][k];
  }
  // Spaltenvertauschungen r?ckg?ngig machen
  for (int k = d - 1; k >= 0; k--) {
    if (colp[k] != k) {
      double temp = (*normal)[k];
      (*normal)[k] = (*normal)[colp[k]];
      (*normal)[colp[k]] = temp;
    }
  }
  delete[] colp;
  return true;
}

// By Rainer Dyckerhoff
// Calculates 2-dimensional plane normal hyperplane through d - 2 points
// from 'A' and the origin, and returns false if it is not possible
bool getBasisComplement(TMatrix &A, TMatrix* basis){
  int d = A[0].size();
  //basis->resize(2);(*basis)[0] = TPoint(d);(*basis)[1] = TPoint(d);
  int imax, jmax;
  int* colp = new int[d];
  double amax;
  for (int k = 0; k < d - 2; k++) {
    imax = k;
    amax = abs(A[k][k]);
    colp[k] = k;
    // Spaltenmaximum finden
    for (int i = k + 1; i < d - 2; i++) {
      if (abs(A[i][k]) > amax) {
        amax = abs(A[i][k]);
        imax = i;
      }
    }
    // Spaltenmaximum gleich null => complete pivoting
    if (amax < eps_pivot) {
      //			cout << "Complete pivoting!\n";
      for (int j = k + 1; j < d; j++) {
        for (int i = k; i < d - 2; i++) {
          if (abs(A[i][j]) > amax) {
            amax = abs(A[i][j]);
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
      for (int i = 0; i < d - 2; i++) {
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
    }
    // Elimination
    for (int i = k + 1; i < d - 2; i++) {
      double factor = A[i][k] / A[k][k];
      for (int j = k + 1; j < d; j++) A[i][j] -= factor * A[k][j];
    }
  }
  // R?cksubstituition
  colp[d - 2] = d - 2;
  colp[d - 1] = d - 1;
  (*basis)[0][d - 2] = -1;
  (*basis)[0][d - 1] =  0;
  (*basis)[1][d - 2] =  0;
  (*basis)[1][d - 1] = -1;
  for (int k = d - 3; k >= 0; k--) {
    (*basis)[0][k] = A[k][d - 2] / A[k][k];
    (*basis)[1][k] = A[k][d - 1] / A[k][k];
    for (int i = k - 1; i >= 0; i--) {
      A[i][d - 2] -= (*basis)[0][k] * A[i][k];
      A[i][d - 1] -= (*basis)[1][k] * A[i][k];
    }
  }
  // Spaltenvertauschungen r?ckg?ngig machen
  for (int k = d - 1; k >= 0; k--) {
    if (colp[k] != k) {
      for (int l = 0; l < 2; l++) {
        double temp = (*basis)[l][k];
        (*basis)[l][k] = (*basis)[l][colp[k]];
        (*basis)[l][colp[k]] = temp;
      }
    }
  }
  delete[] colp;
  return true;
}

double dot_prod(TPoint &x, TPoint &y, int d){
  double prd = 0;
  for (int i = 0; i < d; i++){prd += x[i]*y[i];}
  return prd;
}

void prod(TPoint &x, double y, int d, TPoint *z){
  z->resize(d);for (int i = 0; i < d; i++){(*z)[i] = x[i]*y;}
}

void mult(TMatrix &x, TPoint &y, int k, int d, TPoint *z){
  z->resize(k);
  for (int i = 0; i < k; i++){
    double sum = 0;
    for (int j = 0; j < d; j++){
      sum += x[i][j]*y[j];
    }
    (*z)[i] = sum;
  }
}

void div(TPoint &x, double y, int d, TPoint *z){
  z->resize(d);for (int i = 0; i < d; i++){(*z)[i] = x[i]/y;}
}

void add(TPoint &x, TPoint &y, int d, TPoint *z){
  z->resize(d);for (int i = 0; i < d; i++){(*z)[i] = x[i] + y[i];}
}

void sub(TPoint &x, TPoint &y, int d, TPoint *z){
  z->resize(d);for (int i = 0; i < d; i++){(*z)[i] = x[i] - y[i];}
}

double norm(TPoint x, int d){
  double sqSum = 0;
  for (int i = 0; i < d; i++){sqSum += pow(x[i], 2);}
  return sqrt(sqSum);
}

void get_basis(TPoint &dir, double trd, TMatrix *z){
  int d = dir.size();z->resize(d - 1);
  boost::random::mt19937_64 rEngine;rEngine.seed(time(NULL));
  boost::random::uniform_on_sphere<double, TPoint> unifSphrDist(d - 1);
  for (int i = 0; i < d - 1; i++){
    TPoint subDir = unifSphrDist(rEngine);double sum = 0;
    (*z)[i] = TPoint(d);
    for (int j = 0; j < d - 1; j++){
      (*z)[i][j] = subDir[j];sum += subDir[j]*dir[j];
    }
    (*z)[i][d - 1] = (trd - sum)/dir[d - 1];
  }
}

void op_Gram_Schmidt(TMatrix &x, TMatrix *z){
  int d = x[0].size();int m = x.size();z->resize(m);
  for (int i = 0; i < m; i++){
    TPoint b = x[i];
    for (int j = 0; j < i; j++){
      TPoint tmpPrd;
      prod((*z)[j], dot_prod(x[i], (*z)[j], d), d, &tmpPrd);
      sub(b, tmpPrd, d, &b);
    }
    div(b, norm(b, d), d, &(*z)[i]);
  }
}

void OrthogonalizeProjection(TMatrix &projection, TMatrix *oProjection){
  int n = projection.size();int d = projection[0].size();int dNew = d - 1;
  TMatrix subset(dNew);for (int i = 0; i < dNew; i++){subset[i] = projection[i];}
  TMatrix onBasis;op_Gram_Schmidt(subset, &onBasis);
  oProjection->resize(n);
  for (int i = 0; i < n; i++){
    (*oProjection)[i] = TPoint(dNew);
    for (int j = 0; j < dNew; j++){
      (*oProjection)[i][j] = dot_prod(projection[i], onBasis[j], d);
    }
  }
}

void TransformData(double *rawData, int n, int d, TMatrix *data){
  data->resize(n);
  for (int i = 0; i < n; i++){
    (*data)[i].resize(d);
    for (int j = 0; j < d; j++){
      (*data)[i][j] = rawData[i * d + j];
    }
  }
}

/* Generate n univarite normal deviates.                                      */
/* The Box-Muller method is chosen for the reasons of platform independence,  */
/* as here only functions "rand()" and "srand()" are used, and, conditioned   */
/* on that, fastness of implementation.                                       */
/* It is advicable to consider a better random number genarator.              */
void genNormDevs(unsigned int n, double mu, double sigma, TPoint *devs){
  // Initialize random-number generator
  boost::mt19937_64 rEngine;
  rEngine.seed(time(NULL));
  boost::uniform_real<double> unifDist;
  // Number of the Box-Muller iteration
  devs->resize(n);
  unsigned int n2 = (n + 1) / 2;
  // Box-Muller iteration
  for (int i = 0; i < n2; i++){
    //double u1 = (double)rand() / RAND_MAX;
    double u1 = unifDist(rEngine);
    //double u2 = (double)rand() / RAND_MAX;
    double u2 = unifDist(rEngine);
    double z1 = sqrt(-2 * log(u1)) * cos(2 * M_PI * u2);
    double z2 = sqrt(-2 * log(u1)) * sin(2 * M_PI * u2);
    (*devs)[2 * i] = mu + sigma*z1;
    if (2 * i + 1 < n){
      (*devs)[2 * i + 1] = mu + sigma*z2;
    }
  }
}

/* Solve a linear programming problem using glpk                              */
int solveLP(TPoint &obj, TMatrix &A, TPoint &b, TPoint *result){
  // Prepare the environment
  Environment Rglpk_env("package:Rglpk");
  Function Rglpk_Rglpk_solve_LP = Rglpk_env["Rglpk_solve_LP"];
  // Transform the problem to the Rglpk-format:
  // 1. Create the structures
  int nrow = A.size();
  int ncol = A[0].size();
  NumericVector Rglpk_obj = NumericVector(ncol);
  NumericMatrix Rglpk_mat = NumericMatrix(nrow, ncol);
  CharacterVector Rglpk_dir = CharacterVector(nrow);
  NumericVector Rglpk_rhs = NumericVector(nrow);
  List Rglpk_bounds = List::create();
  NumericVector lower_ind = NumericVector(ncol);
  NumericVector lower_val = NumericVector(ncol);
  NumericVector upper_ind = NumericVector(ncol);
  NumericVector upper_val = NumericVector(ncol);
  // 2. Fill the structures
  for (int i = 0; i < ncol; i++){
    Rglpk_obj(i) = obj[i];
    lower_ind(i) = i + 1;
    lower_val(i) = R_NegInf;
    upper_ind(i) = i + 1;
    upper_val(i) = R_PosInf;
  }
  for (int i = 0; i < nrow; i++){
    Rglpk_dir(i) = "<=";
    Rglpk_rhs(i) = b[i];
    for (int j = 0; j < ncol; j++){
      Rglpk_mat(i,j) = A[i][j];
    }
  }
  // 3. Create the list of bounds
  List bounds_lower = List::create();
  bounds_lower.push_back(lower_ind, "ind");
  bounds_lower.push_back(lower_val, "val");
  List bounds_upper = List::create();
  bounds_upper.push_back(upper_ind, "ind");
  bounds_upper.push_back(upper_val, "val");
  Rglpk_bounds.push_back(bounds_upper, "upper");
  Rglpk_bounds.push_back(bounds_lower, "lower");
  // // The call itself
  List lps = Rglpk_Rglpk_solve_LP(Rglpk_obj, Rglpk_mat, Rglpk_dir, Rglpk_rhs,
                                  Rglpk_bounds);
  // Extract the solution
  // Rcout << "Status:" << as<int>(lps["status"]) << std::endl;
  if (as<int>(lps["status"]) == 0){
    result->resize(ncol);
    for (int i = 0; i < ncol; i++){
      (*result)[i] = as<NumericVector>(lps["solution"])(i);
    }
    return 0;
  }else{
    return -1;
  }
}

//bounds <- list(lower = list(ind = c(1L, 3L), val = c(-Inf, 2)),
//               upper = list(ind = c(1L, 2L), val = c(4, 100)))
//  Rglpk_solve_LP(obj, mat, dir, rhs, types, max, bounds)
//https://www.rdocumentation.org/packages/Rglpk/versions/0.6-3/topics/Rglpk_solve_LP

/* Solve a linear programming problem                                         */
// int _solveLP(TPoint &obj, TMatrix &A, TPoint &b, TPoint *result){
//   // Create LP-structures
//   glp_prob *lp;
//   lp = glp_create_prob();
//   glp_term_out(GLP_OFF);
//   // Add rows
//   int nrow = A.size();
//   glp_add_rows(lp, nrow);
//   for (int i = 0; i < nrow; i++){
//     glp_set_row_bnds(lp, i + 1, GLP_UP, b[i], b[i]);
//   }
//   // Add columns
//   int ncol = A[0].size();
//   glp_add_cols(lp, ncol);
//   for (int i = 0; i < ncol; i++){
//     glp_set_col_bnds(lp, i + 1, GLP_FR, 0., 0.);
//     glp_set_obj_coef(lp, i + 1, obj[i]);
//   }
//   // Set constraint matrix
//   int ne = nrow * ncol;
//   int *rowIndices = new int[ne];
//   int *colIndices = new int[ne];
//   double *values = new double[ne];
//   for (int i = 0; i < nrow; i++){
//     for (int j = 0; j < ncol; j++){
//       rowIndices[i * ncol + j] = i + 1;
//       colIndices[i * ncol + j] = j + 1;
//       values[i * ncol + j] = A[i][j];
//     }
//   }
//   // Set the direction of optimization
//   glp_set_obj_dir(lp, GLP_MAX);
//   // Load constraint matrix
//   glp_load_matrix(lp, ne, &rowIndices[-1], &colIndices[-1], &values[-1]);
//   // Execute linear solver
//   glp_simplex(lp, NULL);
//   // Collect the result
//   delete[] rowIndices;
//   delete[] colIndices;
//   delete[] values;
//   Rcout << "StatusC:" << glp_get_status(lp) << std::endl;
//   if (glp_get_status(lp) == GLP_OPT){
//     result->resize(ncol);
//     for (int i = 0; i < ncol; i++){
//       (*result)[i] = glp_get_col_prim(lp, i + 1);
//     }
//     //Rcout << "Row statuses: ";
//     //for (int i = 0; i < nrow; i++){
//     //  Rcout << glp_get_row_stat(lp, i + 1) << "(" << glp_get_row_prim(lp, i + 1) << ") ";
//     //}
//     //Rcout << endl;
//     glp_delete_prob(lp);
//     return 0; // success
//   }else{
//     glp_delete_prob(lp);
//     return -1; // fail
//   }
// }

int initRidges(TMatrix &X, int intTau, int method, int nRidges,
               vector<TVariables*> &ridges){
  int d = X[0].size();
  int n = X.size();
  ridges.resize(0);
  if (method == 3){
    // Pick from the convex hull
    vector<TVariables> facets(0);
    int exitcode;
    getQHFacets(X, facets, true, &exitcode);
    // Try to find at least one good ridge
    bool found = false;
    int iFound = -1;
    int jFound = -1;
    TVariables facetCandidates;
    for (int i = 0; i < facets.size(); i++){
      // Construct and check all 'd' ridges
      int swapIVertex = facets[i][0];
      TVariables curCmb(facets[i].begin() + 1, facets[i].end());
      sort(curCmb.begin(), curCmb.end());
      for (int j = -1; j < d - 1; j++){
        if (j > -1){
          // Replace the 'd'th index (to keep length = 'd - 1')
          int tmpI = curCmb[j];
          curCmb[j] = swapIVertex;
          swapIVertex = tmpI;
        }
        // Check whether the ridge yields further facets
        // a) Prepare data structures
        int n = X.size();
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
        // b) Project onto the plane orthogonal to the ridge
        for (int i = 1; i < d - 1; i++){
          for (int j = 0; j < d; j++){
            plane[i - 1][j] = X[curCmb[i]][j] - X[curCmb[0]][j];
          }
        }
        getBasisComplement(plane, &basis);
        getProjection(X, basis, &curXPrj);
        TPoint center(curXPrj[curCmb[0]]);
        for (int i = 0; i < n; i++){
          for (int j = 0; j < 2; j++){
            curXPrj[i][j] -= center[j];
          }
        }
        // d) Obtain facet candidates
        //Rcout << "Points to cut: " << intTau << endl;
        bool boolTmp = getFacets(curXPrj, intTau, &curCmb, &facetCandidates);
        if (boolTmp){
          if (facetCandidates.size() >= 2){
            // e) Save successful combination
            //if (i == 0){
              //Rcout << "Combination found during first iteration." << endl;
            TVariables* pARidge = new TVariables(curCmb);
            ridges.push_back(pARidge);
            jFound = j;
            found = true;
            break;
            //}
          }
        }
      }
      if (found){
        iFound = i;
        break;
      }
    }
    if(!found){
      return 0;
    }else{
      // If one ridge only is to return
      if (nRidges == 1){
        return 1;
      }
      // The Xiaohui's last trick
      if (nRidges == -1){
        // Collect all ridges defined by points above the 'facetCandidates'
        for (int i = 0; i < facetCandidates.size(); i++){
          // Find the outer-pointing orthogonal direction
          TVariables curCmb(*(ridges[0]));
          curCmb.push_back(facetCandidates[i]);
          TMatrix A(d - 1);
          for (int j = 1; j < d; j++){
            A[j - 1] = TPoint(d);
            for (int k = 0; k < d; k++){
              A[j - 1][k] = X[curCmb[j]][k] - X[curCmb[0]][k];
            }
          }
          TPoint normal;
          getNormal(A, &normal);
          TPoint projection(n);
          for (int j = 0; j < n; j++){
            for (int k = 0; k < d; k++){
              projection[j] += X[j][k] * normal[k];
            }
          }
          int pointsAbove = 0;int pointsBelow = 0;
          for (int j = 0; j < n; j++){
            if (projection[j] > projection[curCmb[0]] + eps){pointsAbove++;}
            if (projection[j] < projection[curCmb[0]] - eps){pointsBelow++;}
          }
          //Rcout << "Points above = " << pointsAbove << ", points below = " << pointsBelow << endl;
          if (pointsBelow <= intTau){
            for (int j = 0; j < d; j++){
              normal[j] = -normal[j];
            }
            for (int j = 0; j < n; j++){
              projection[j] = 0;
              for (int k = 0; k < d; k++){
                projection[j] += X[j][k] * normal[k];
              }
            }
          }
          // Add the point itself
          for (int j = 0; j < d - 1; j++){
            TVariables* pARidge = new TVariables(*(ridges[0]));
            (*pARidge)[j] = facetCandidates[i];
            sort(pARidge->begin(), pARidge->end());
            ridges.push_back(pARidge);
          }
          // Identify and add all above points
          for (int j = 0; j < n; j++){
            if (projection[j] > projection[curCmb[0]] + eps){
              //Rcout << "+ ";
              // Add a ridge co-defined by this point
              for (int k = 0; k < d - 1; k++){
                TVariables* pARidge = new TVariables(*(ridges[0]));
                (*pARidge)[k] = j;
                sort(pARidge->begin(), pARidge->end());
                ridges.push_back(pARidge);
              }
            }
          }
        }
        //Rcout << ridges.size() << " initialized in total." << endl;
        return ridges.size();
      }
      int nCmb = 1;
      TVariables curCmb(d - 1);
      for (int i = 0; i < facets.size(); i++){
        // Construct all 'd' ridges
        sort(facets[i].begin(), facets[i].end());
        int swapIVertex = facets[i][0];
        for (int j = 0; j < d - 1; j++){
          curCmb[j] = facets[i][j + 1];
        }
        for (int j = -1; j < d - 1; j++){
          if (j > -1){
            // Replace the 'd'th index (to keep length = 'd - 1')
            int tmpI = curCmb[j];
            curCmb[j] = swapIVertex;
            swapIVertex = tmpI;
          }
          if (i == iFound && j == jFound){continue;}
          TVariables* pARidge = new TVariables(curCmb);
          ridges.push_back(pARidge);
          nCmb++;
          if (nRidges > 1 && nCmb >= nRidges){break;}
        }
      }
      return nCmb;
    }
  }
  TVariables *firstCmb = new TVariables(); // the first combination
  firstCmb->resize(d - 1);
  if (method == 1){
    // Guess by checking ordered points on random projections
    getFirstCombination(X, intTau, firstCmb);
  }
  if (nRidges == 1){
    // To use one initial combination only
    firstCmb->resize(d - 1);
    //sort(firstCmb->begin(), firstCmb->end());
    ridges.push_back(firstCmb);
    return 1;
  }else{
    // To use all (d-1)-tupels lying outside as initial combinations
    TVariables counters(d - 1);
    // Go through all necessary combinations:
    for (int i = 0; i < d - 2; i++){counters[i] = i;}counters[d - 2] = d - 3;
    int numOut = firstCmb->size();
    int nCmb = 0;
    while (counters[0] != numOut - d + 1){ // Get current combination
      int i = d - 2;
      while (i > 0 && counters[i] == numOut - d + i + 1){i--;}
      counters[i]++;int j = i + 1;
      while (j < d - 1){counters[j] = counters[j - 1] + 1;j++;}
      // For current combination do :
      TVariables *curCmb = new TVariables(d - 1);
      for (int k = 0; k < d - 1; k++){(*curCmb)[k] = (*firstCmb)[counters[k]];}
      sort(curCmb->begin(), curCmb->end()); // sort after choosing
      // ... to keep the first (that is guaranteed a good one) combination
      ridges.push_back(curCmb);
      nCmb++;
      if (nRidges > 0 && nCmb >= nRidges){break;}
    }
    return ridges.size();
  }
  return 0;
}

int initRidges2D(TMatrix &X, int intTau, int method, int nRidges,
                 vector<TVariables*> &ridges){
  int d = X[0].size();
  int n = X.size();
  ridges.resize(0);
  if (method == 3){
    // Pick from the convex hull
    vector<TVariables> facets(0);
    int exitcode;
    getQHFacets(X, facets, true, &exitcode);
    // Try to find at least one good ridge
    bool found = false;
    int iFound = -1;
    int jFound = -1;
    TVariables facetCandidates;
    for (int i = 0; i < facets.size(); i++){
      // Construct and check all 'd' ridges
      int swapIVertex = facets[i][0];
      TVariables curCmb(facets[i].begin() + 1, facets[i].end());
      sort(curCmb.begin(), curCmb.end());
      for (int j = -1; j < d - 1; j++){
        if (j > -1){
          // Replace the 'd'th index (to keep length = 'd - 1')
          int tmpI = curCmb[j];
          curCmb[j] = swapIVertex;
          swapIVertex = tmpI;
        }
        // Check whether the ridge yields further facets
        // a) Prepare data structures
        int n = X.size();
        TMatrix plane(d - 2);
        if (d > 2){
          for (int i = 1; i < d - 1; i++) {
            plane[i - 1] = TPoint(d);
          }
        }
        TMatrix basis(2); basis[0] = TPoint(d); basis[1] = TPoint(d);
        TMatrix curXPrj;
        curXPrj.resize(X.size());
        for (int i = 0; i < X.size(); i++) {
          curXPrj[i] = TPoint(2);
        }
        // b) Project onto the plane orthogonal to the ridge
        if (d > 2){
          for (int i = 1; i < d - 1; i++){
            for (int j = 0; j < d; j++){
              plane[i - 1][j] = X[curCmb[i]][j] - X[curCmb[0]][j];
            }
          }
        }
        if (d == 2){
          basis[0][0] = 1;basis[0][1] = 1;basis[1][0] = 0;basis[1][1] = 1;
        }else{
          getBasisComplement(plane, &basis);
        }
        getProjection(X, basis, &curXPrj);
        TPoint center(curXPrj[curCmb[0]]);
        for (int i = 0; i < n; i++){
          for (int j = 0; j < 2; j++){
            curXPrj[i][j] -= center[j];
          }
        }
        // d) Obtain facet candidates
        //Rcout << "Points to cut: " << intTau << endl;
        bool boolTmp = getFacets(curXPrj, intTau, &curCmb, &facetCandidates);
        if (boolTmp){
          if (facetCandidates.size() >= 2){
            // e) Save successful combination
            //if (i == 0){
            //Rcout << "Combination found during first iteration." << endl;
            TVariables* pARidge = new TVariables(curCmb);
            ridges.push_back(pARidge);
            jFound = j;
            found = true;
            break;
            //}
          }
        }
      }
      if (found){
        iFound = i;
        break;
      }
    }
    if(!found){
      return 0;
    }else{
      // If one ridge only is to return
      if (nRidges == 1){
        return 1;
      }
      // The Xiaohui's last trick
      if (nRidges == -1){
        // Collect all ridges defined by points above the 'facetCandidates'
        for (int i = 0; i < facetCandidates.size(); i++){
          // Find the outer-pointing orthogonal direction
          TVariables curCmb(*(ridges[0]));
          curCmb.push_back(facetCandidates[i]);
          TMatrix A(d - 1);
          for (int j = 1; j < d; j++){
            A[j - 1] = TPoint(d);
            for (int k = 0; k < d; k++){
              A[j - 1][k] = X[curCmb[j]][k] - X[curCmb[0]][k];
            }
          }
          TPoint normal;
          getNormal(A, &normal);
          TPoint projection(n);
          for (int j = 0; j < n; j++){
            for (int k = 0; k < d; k++){
              projection[j] += X[j][k] * normal[k];
            }
          }
          int pointsAbove = 0;int pointsBelow = 0;
          for (int j = 0; j < n; j++){
            if (projection[j] > projection[curCmb[0]] + eps){pointsAbove++;}
            if (projection[j] < projection[curCmb[0]] - eps){pointsBelow++;}
          }
          //Rcout << "Points above = " << pointsAbove << ", points below = " << pointsBelow << endl;
          if (pointsBelow <= intTau){
            for (int j = 0; j < d; j++){
              normal[j] = -normal[j];
            }
            for (int j = 0; j < n; j++){
              projection[j] = 0;
              for (int k = 0; k < d; k++){
                projection[j] += X[j][k] * normal[k];
              }
            }
          }
          // Add the point itself
          for (int j = 0; j < d - 1; j++){
            TVariables* pARidge = new TVariables(*(ridges[0]));
            (*pARidge)[j] = facetCandidates[i];
            sort(pARidge->begin(), pARidge->end());
            ridges.push_back(pARidge);
          }
          // Identify and add all above points
          for (int j = 0; j < n; j++){
            if (projection[j] > projection[curCmb[0]] + eps){
              //Rcout << "+ ";
              // Add a ridge co-defined by this point
              for (int k = 0; k < d - 1; k++){
                TVariables* pARidge = new TVariables(*(ridges[0]));
                (*pARidge)[k] = j;
                sort(pARidge->begin(), pARidge->end());
                ridges.push_back(pARidge);
              }
            }
          }
        }
        //Rcout << ridges.size() << " initialized in total." << endl;
        return ridges.size();
      }
      int nCmb = 1;
      TVariables curCmb(d - 1);
      for (int i = 0; i < facets.size(); i++){
        // Construct all 'd' ridges
        sort(facets[i].begin(), facets[i].end());
        int swapIVertex = facets[i][0];
        for (int j = 0; j < d - 1; j++){
          curCmb[j] = facets[i][j + 1];
        }
        for (int j = -1; j < d - 1; j++){
          if (j > -1){
            // Replace the 'd'th index (to keep length = 'd - 1')
            int tmpI = curCmb[j];
            curCmb[j] = swapIVertex;
            swapIVertex = tmpI;
          }
          if (i == iFound && j == jFound){continue;}
          TVariables* pARidge = new TVariables(curCmb);
          ridges.push_back(pARidge);
          nCmb++;
          if (nRidges > 1 && nCmb >= nRidges){break;}
        }
      }
      return nCmb;
    }
  }
  TVariables *firstCmb = new TVariables(); // the first combination
  firstCmb->resize(d - 1);
  if (method == 1){
    // Guess by checking ordered points on random projections
    getFirstCombination(X, intTau, firstCmb);
  }
  if (nRidges == 1){
    // To use one initial combination only
    firstCmb->resize(d - 1);
    //sort(firstCmb->begin(), firstCmb->end());
    ridges.push_back(firstCmb);
    return 1;
  }else{
    // To use all (d-1)-tupels lying outside as initial combinations
    TVariables counters(d - 1);
    // Go through all necessary combinations:
    for (int i = 0; i < d - 2; i++){counters[i] = i;}counters[d - 2] = d - 3;
    int numOut = firstCmb->size();
    int nCmb = 0;
    while (counters[0] != numOut - d + 1){ // Get current combination
      int i = d - 2;
      while (i > 0 && counters[i] == numOut - d + i + 1){i--;}
      counters[i]++;int j = i + 1;
      while (j < d - 1){counters[j] = counters[j - 1] + 1;j++;}
      // For current combination do :
      TVariables *curCmb = new TVariables(d - 1);
      for (int k = 0; k < d - 1; k++){(*curCmb)[k] = (*firstCmb)[counters[k]];}
      sort(curCmb->begin(), curCmb->end()); // sort after choosing
      // ... to keep the first (that is guaranteed a good one) combination
      ridges.push_back(curCmb);
      nCmb++;
      if (nRidges > 0 && nCmb >= nRidges){break;}
    }
    return ridges.size();
  }
  return 0;
}
