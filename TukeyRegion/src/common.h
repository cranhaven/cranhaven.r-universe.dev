// common.h
// By Pavlo Mozharovskyi
// Last changed 22.09.2017
// Commonly used definitions

#define M_PI 3.14159265358979323846
#define MAX_ITER_FIRST 10000000
const double eps_HD2 = 1e-10;
const double eps_pivot = 1e-10;
const double eps = 1e-8;

typedef vector<double> TPoint;
typedef vector<vector<double> > TMatrix;
typedef vector<int> TVariables;

struct IndexRec{
  int index;
  double value;
  IndexRec(int index = -1, double value = 0){
    this->index = index;
    this->value = value;
  }
};


static int compareAsc(IndexRec x, IndexRec y){
  return (x.value < y.value);
}

unsigned long long choose(unsigned long long n, unsigned long long k);
bool getFacets(TMatrix &XPrj, int tau, TVariables *curCmb, TVariables *facets);
void getRndNormal(int d, TPoint *normal);
bool getBasisComplement(TMatrix &A, TMatrix *basis);
bool getNormal(TMatrix &A, TPoint *normal);
void getProjection(TMatrix &X, TMatrix &basis, TMatrix *XPrj);
unsigned long long getFacetCode(TVariables &facet, int n);
void getFacetPoints(unsigned long long facetCode, int n, int d,
                    TVariables *pointNumbers);
bool getFirstCombination(TMatrix &X, int intTau, TVariables *cmb);
bool checkFirstCombination(TMatrix &X, int intTau, TVariables &cmb);
void OrthogonalizeProjection(TMatrix &projection, TMatrix *oProjection);
void TransformData(double *rawData, int n, int d, TMatrix *data);
void genNormDevs(unsigned int n, double mu, double sigma, TPoint *devs);
int solveLP(TPoint &obj, TMatrix &A, TPoint &b, TPoint *result);
int initRidges(TMatrix &X, int intTau, int method, int nRidges,
               vector<TVariables*> &ridges);
int initRidges2D(TMatrix &X, int intTau, int method, int nRidges,
                 vector<TVariables*> &ridges);

typedef double** TDMatrix;

TDMatrix asMatrix(double* arr, int n, int d);

struct SortIndex{
  double value;
  int index;
};

struct Feval{
  double* arg;
  double val;
};

struct Coords{
  int pointIndex;
  int totalIndex;
  int d;
  int nCoords;
  int* coords;
  int* fixedCoords;
};

template<typename T>
int Compare(T &p1, T &p2) {
  return p1 < p2;
}

template<typename T>
void Swap(T *p1, T *p2) {
  T pTmp = *p1;
  *p1 = *p2;
  *p2 = pTmp;
};

int Compare(SortIndex &si1, SortIndex &si2);

void Swap(SortIndex *si1, SortIndex *si2);

int Compare(Feval &fe1, Feval &fe2);

void Swap(Feval *fe1, Feval *fe2);

int Compare(Coords &co1, Coords &co2);

void Swap(Coords *co1, Coords *co2);

#define elem_type double

// const double eps = 10e-8; // precision constant

elem_type quick_select(elem_type arr[], int n);

/* -------------------------------------------------------------------------- */
/* quickSort from http://www.proggen.org/doku.php?id=algo:quicksort           */
/* (modified, templated)                                                      */
/* -------------------------------------------------------------------------- */
template<typename T>
void quick_sort(T *values, int left, int right, int(*cmp)(T& x, T& y),
                void(*swap)(T* x, T* y)){
  int i = left, j = right; // Z?hlindizes
  // Pivot-Element (Array-Mitte) bestimmen
  T pivot = values[(left + right) >> 1];
  // Solange Paare suchen und tauschen, bis sich die Z?hlindizes "getroffen"
  // haben
  do{
    // Paar finden, dass getauscht werden muss
    while (cmp(values[i], pivot)){++i;}
    while (cmp(pivot, values[j])){--j;}
    // Wenn sich die Z?hlindizes noch nicht "getroffen" haben, dann
    // tauschen und weiterz?hlen. Sollten die Z?hlindizes gleich sein, dann
    // z?hle einfach weiter und unterbrich die Schleife
    if (i < j){swap(&values[i], &values[j]);++i;--j;
    }else{if (i == j){++i;--j;break;}}
  }while (i <= j);
  // Wenn die Teillisten mehr als ein Element enthalten, dann wende quickSort
  // auf sie an
  if (left < j){quick_sort(values, left, j, cmp, swap);}
  if (i < right){quick_sort(values, i, right, cmp, swap);}
}
template<typename T>
void quick_sort(T *values, int left, int right){
  quick_sort(values, left, right, Compare, Swap);
}

//int bin_searchl_routine(double* x, int n, double val);
//int bin_searchr_routine(double* x, int n, double val);
