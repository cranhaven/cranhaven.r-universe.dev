#include <algorithm>
#include <string>
#include <vector>
#include <numeric>
#include <iostream>
#define R_NO_REMAP 
#include <R.h>
#include <Rdefines.h>

struct CMP_INT {
  bool operator()(int x, int y) {
    if(x<-2147483647)
      return false;
    if(y<-2147483647)
      return true;
    return x-y < 0;
  }
};

struct CMP_CHAR {
  bool operator()(SEXP x, SEXP y) {
    return strcmp(CHAR(x), CHAR(y)) < 0;
  }
} cmp_char;


extern "C" SEXP sortcpp(SEXP x) {
  
  //std::cout << TYPEOF(x);
  x = PROTECT(Rf_duplicate(x));
  switch(TYPEOF(x))
  {
    case INTSXP:
      std::sort(INTEGER(x),INTEGER(x)+LENGTH(x));
      break;
    case REALSXP:
      std::sort(REAL(x),REAL(x)+LENGTH(x));
      break;
    case LGLSXP:
      std::sort(LOGICAL(x),LOGICAL(x)+LENGTH(x));
      break;
    case STRSXP:
      std::sort(STRING_PTR(x), STRING_PTR(x) + LENGTH(x), cmp_char);
      break;
    default:
      UNPROTECT(1);
      error_return("Unsupported type for sort.")
      ;
  }
  UNPROTECT(1);
  return x;
}



struct CMP_INT2 {
  int* start;
  CMP_INT2(int* start) : start(start) {};
  bool operator()(int x, int y) {
    int a=*(start+x-1);
    int b=*(start+y-1);
    if(a<-2147483647)
      return false;
    if(b<-2147483647)
      return true;
    return a-b < 0;
  }
};

struct CMP_REAL {
  double* start;
  CMP_REAL(double* start) : start(start) {};
  bool operator()(int x, int y) {
    return *(start+x-1) - *(start+y-1) < 0;
  }
};

struct CMP_CHAR2 {
  SEXP* start;
  CMP_CHAR2(SEXP* start) : start(start) {};
  bool operator()(int x, int y) {
    return strcmp(CHAR(*(start+x-1)),CHAR(*(start+y-1))) < 0; // shift compensates for difference between R and C indexing
  }
};


void internalOrder(int* index,SEXP x)
{
  for (int i = 0; i < LENGTH(x); i++)
  {
    //std::cout << *(start+i);
    index[i] = i+1; //shift compensates for difference between R and C indexing
  }
  switch(TYPEOF(x))
  {
  case INTSXP:
  {
    int* start = &INTEGER(x)[0];
    std::sort(index,index+LENGTH(x), CMP_INT2(start));
    break;
  }
  case REALSXP:
  {
    double* start = &REAL(x)[0];
    std::sort(index,index+LENGTH(x), CMP_REAL(start));
    break;
  }
  case LGLSXP:
  {
    int* start = &INTEGER(x)[0];
    std::sort(index,index+LENGTH(x), CMP_INT2(start));
    break;
  }
  case STRSXP:
  {
    SEXP* start = &STRING_PTR(x)[0];
    std::sort(index,index+LENGTH(x), CMP_CHAR2(start));
    break;
  }
  default:
    UNPROTECT(1);
    Rf_error("Unsupported type for sort.")
      ;
  }
}

extern "C" SEXP ordercpp(SEXP x) {
  
  //std::cout << TYPEOF(x);
  //int* xpoint = INTEGER(x); //Pointer to int array of input x
  SEXP result = PROTECT(Rf_allocVector(INTSXP,LENGTH(x)));
  int* respoint = INTEGER(result);
  internalOrder(respoint,x);
  
  //   std::vector<int> v;  //   v.assign(respoint,respoint + LENGTH(result));  //int* a = &v[0];
  
  UNPROTECT(1);
  return(result);
  
}


template<typename T>
void nmatch(T astart, T bstart, std::vector<int> &indexsA, std::vector<int> &indexsB, int* apoint, int* bpoint, int alength, int blength)
{
  int a1 =0, a2 = 0, b1 =0, b2 = 0;
  while (a2 < alength || b2 < blength)
  {
    while(a2<alength && b2<blength && *(astart+*(apoint+a2)-1)==*(bstart+*(bpoint+b2)-1))
    {
      indexsA.push_back(*(apoint+a2));
      indexsB.push_back(*(bpoint+b2));
      b2++;
    }
    if(b1!=b2)
    {
      if(a2<(alength-1))
      {
        a2++;
        if(*(astart+*(apoint+a2)-1)==*(astart+*(apoint+a1)-1))
          b2=b1;
        
        else
          b1=b2;
      }
      else
        b1=b2;
      a2=++a1;
      continue;
    }
    if(a2<alength && (b2>=blength || *(astart+*(apoint+a2)-1)<*(bstart+*(bpoint+b2)-1)))
    {
      indexsA.push_back(*(apoint+a2));
      indexsB.push_back(blength+1);
      
      a1=++a2;
      continue;
    }
    if(b2<blength && (a2>=alength || *(astart+*(apoint+a2)-1)>*(bstart+*(bpoint+b2)-1)))
    {
      indexsA.push_back(alength+1);
      indexsB.push_back(*(bpoint+b2));
      b1=++b2;
      continue;
    }
  }
  return;
  
}

template<typename T>
void cmatch(T astart, T bstart, std::vector<int> &indexsA, std::vector<int> &indexsB, int* apoint, int* bpoint, int alength, int blength)
{
  int a1 =0, a2 = 0, b1 =0, b2 = 0;
  
  while (a2 < alength || b2 < blength)
  {
    while(a2<alength && b2<blength && strcmp(CHAR(*(astart+*(apoint+a2)-1)),CHAR(*(bstart+*(bpoint+b2)-1)))==0)
    {
      indexsA.push_back(*(apoint+a2));
      indexsB.push_back(*(bpoint+b2));
      b2++;
    }
    if(b1!=b2)
    {
      if(a2<(alength-1))
      {
        a2++;
        if(strcmp(CHAR(*(astart+*(apoint+a2)-1)),CHAR(*(astart+*(apoint+a1)-1)))==0)
          b2=b1;
        
        else
          b1=b2;
      }
      else
        b1=b2;
      a2=++a1;
      continue;
    }
    if(a2<alength && (b2>=blength || strcmp(CHAR(*(astart+*(apoint+a2)-1)),CHAR(*(bstart+*(bpoint+b2)-1)))<0))
    {
      indexsA.push_back(*(apoint+a2));
      indexsB.push_back(blength+1);
      
      a1=++a2;
      continue;
    }
    if(b2<blength && (a2>=alength || strcmp(CHAR(*(astart+*(apoint+a2)-1)),CHAR(*(bstart+*(bpoint+b2)-1)))>0))
    {
      indexsA.push_back(alength+1);
      indexsB.push_back(*(bpoint+b2));
      b1=++b2;
      continue;
    }
  }
  
  return;
  
}

extern "C" SEXP matches(SEXP a, SEXP b)
{
  
  int alength = LENGTH(a);
  int blength = LENGTH(b);
  SEXP sortedA = PROTECT(Rf_allocVector(INTSXP,alength));
  int* apoint = INTEGER(sortedA);
  internalOrder(apoint,a);
  SEXP sortedB = PROTECT(Rf_allocVector(INTSXP,blength));
  int* bpoint = INTEGER(sortedB);
  internalOrder(bpoint,b);
  std::vector<int> indexsA;
  indexsA.reserve(alength);
  std::vector<int> indexsB;
  indexsB.reserve(blength);

  switch(TYPEOF(a))
  {
  case INTSXP:
  {
    int* astart = &INTEGER(a)[0];
    int* bstart = &INTEGER(b)[0];
    nmatch(astart,bstart,indexsA,indexsB,apoint,bpoint,alength,blength);
    break;
  }
  case REALSXP:
  {
    double* astart = &REAL(a)[0];
    double* bstart = &REAL(b)[0];
    nmatch(astart,bstart,indexsA,indexsB,apoint,bpoint,alength,blength);
    break;
  }
  case LGLSXP:
  {
    int* astart = &INTEGER(a)[0];
    int* bstart = &INTEGER(b)[0];
    nmatch(astart,bstart,indexsA,indexsB,apoint,bpoint,alength,blength);
    
    break;
  }
  case STRSXP:
  {
    SEXP* astart = &STRING_PTR(a)[0];
    SEXP* bstart = &STRING_PTR(b)[0];
    cmatch(astart,bstart,indexsA,indexsB,apoint,bpoint,alength,blength);
    break;
  }
  default:
    UNPROTECT(2);
    Rf_error("Unsupported type for matching.")
      ;
  }
  
  //Unfortunate overhead needed to convert vector to SEXP
  SEXP result1 = PROTECT(Rf_allocVector(INTSXP,indexsA.size()));
  SEXP result2 = PROTECT(Rf_allocVector(INTSXP,indexsB.size()));
  int* respoint1 = INTEGER(result1);
  int* respoint2 = INTEGER(result2);
  //Will this work?  If so, then we could probably dispense with allocating new vectors above (and instead allocate of size 0)
  //respoint1 = &indexsA.front();
  //respoint1 = &indexsB.front();
  std::copy(indexsA.begin(),indexsA.end(),respoint1);
  std::copy(indexsB.begin(),indexsB.end(),respoint2);
  SEXP combined = PROTECT(Rf_allocVector(VECSXP,2));
  SET_VECTOR_ELT(combined,0,result1);
  SET_VECTOR_ELT(combined,1,result2);
  UNPROTECT(5);
  return(combined);
  
}
