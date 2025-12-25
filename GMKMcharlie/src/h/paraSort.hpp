# pragma once
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <RcppParallel.h>
# include "macros.hpp"
# include "dnyTasking.hpp"
// using namespace Rcpp;
using namespace RcppParallel;


// T is the target type.
template<typename T, typename compFun>
struct paraSortBottom: public Worker
{
  INT blockSize;
  compFun *cp;
  T *v, *vend;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, blockSize)) break;
      T *start = v + objI,
        *end = std::min<T*> (start + blockSize, vend);
      std::sort(start, end, *cp);
    }
  }


  paraSortBottom(T *begin, T *end, INT blockSize_,
                 compFun *comp, int maxCore)
  {
    v = begin;
    vend = end;
    cp = comp;
    blockSize = blockSize_;
    dynamicTasking dt(maxCore, end - begin); dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};


template<typename T, typename compFun>
struct paraMergeOneRound: public Worker
{
  INT blockSize; // The last block size shall be less than blockSize.
  compFun *cp;
  vec<T> &targetV;
  vec<T> &merger;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, blockSize * 2)) break;
      T *begin = &targetV[objI];
      T *upperBound = &*targetV.end();
      T *mid = std::min<T*> (begin + blockSize, upperBound);
      T *end = std::min<T*> (mid + blockSize, upperBound);
      T *mergerBegin = &merger[objI];
      std::merge(begin, mid, mid, end, mergerBegin, *cp);
    }
  }


  paraMergeOneRound(INT blockSize, compFun *cp, vec<T> &targetV,
                    vec<T> &merger, int maxCore):
    blockSize(blockSize), cp(cp), targetV(targetV), merger(merger)
  {
    dynamicTasking dt(maxCore, targetV.size()); dT = &dt;
    merger.resize(targetV.size());
    parallelFor(0, maxCore, *this);
    targetV.swap(merger);
  }
};


template<typename T, typename compFun>
struct paraInplaceMergeOneRound: public Worker
{
  INT blockSize; // The last block size shall be less than blockSize.
  compFun *cp;
  vec<T> &targetV;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI, blockSize * 2)) break;
      T *begin = &targetV[objI];
      T *upperBound = &*targetV.end();
      T *mid = std::min<T*> (begin + blockSize, upperBound);
      T *end = std::min<T*> (mid + blockSize, upperBound);
      std::inplace_merge(begin, mid, end, *cp);
    }
  }


  paraInplaceMergeOneRound(INT blockSize, compFun *cp,
                           vec<T> &targetV, int maxCore):
    blockSize(blockSize), cp(cp), targetV(targetV)
  {
    dynamicTasking dt(maxCore, targetV.size()); dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};




// blocks is the number of blocks where std::sort() would happen.
template<typename T, typename compFun>
inline void paraSort(vec<T> &targetV, vec<T> &auxContainer,
                     compFun *cp, int maxCore, INT blocks = 0)
{
  if(blocks == 0) blocks = maxCore * 2; // Tests shows fewer blocks results in higher speed.
  blocks = std::max<INT> (1, std::min<INT> (targetV.size(), blocks));
  INT blockSize = std::max<INT> (1, targetV.size() / blocks);
  paraSortBottom<T, compFun> (&targetV[0], &*targetV.end(), blockSize, cp, maxCore);
  INT targetVsize = targetV.size();
  while(blockSize < targetVsize)
  {
    paraMergeOneRound<T, compFun> (blockSize, cp, targetV, auxContainer, maxCore);
    blockSize *= 2;
  }
}




// No extra container is needed.
template<typename T, typename compFun>
inline void paraSort(vec<T> &targetV, compFun *cp, int maxCore, INT blocks = 0)
{
  if(blocks == 0) blocks = maxCore * 2; // Tests shows fewer blocks results in higher speed.
  blocks = std::max<INT> (1, std::min<INT> (targetV.size(), blocks));
  INT blockSize = std::max<INT> (1, targetV.size() / blocks);
  paraSortBottom<T, compFun> (&targetV[0], &*targetV.end(), blockSize, cp, maxCore);
  INT targetVsize = targetV.size();
  while(blockSize < targetVsize)
  {
    paraInplaceMergeOneRound<T, compFun> (blockSize, cp, targetV, maxCore);
    blockSize *= 2;
  }
}






















