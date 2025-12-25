# pragma once
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]
# include <Rcpp.h>
# include <RcppParallel.h>
# include "macros.hpp"
# include "dnyTasking.hpp"
// using namespace Rcpp;
using namespace RcppParallel;


// we only consider sorting consecutive sequence
// paraSortBottom(T*begin, T*end, unsigned blockSize, compFun*comp, unsigned maxCore)
template<typename T, typename paraSortIndtype, typename compFun>
struct paraSortBottom: public Worker
{
  paraSortIndtype blockSize;
  paraSortIndtype blocks;
  compFun *cp;
  T *v, *vend;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      T *start = v + objI * blockSize, *end = NULL;
      end = std::min<T*> (start + blockSize, vend);
      std::sort(start, end, *cp);
    }
  }


  paraSortBottom(T *begin, T *end, paraSortIndtype blocks,
                 compFun *comp, unsigned maxCore)
  {
    v = begin; vend = end;
    cp = comp;
    this->blocks = blocks;
    blockSize = (end - begin) / blocks; // Could be 0.
    maxCore = std::max<unsigned> (1, std::min<unsigned> (maxCore, blocks));
    dynamicTasking dt(maxCore, blocks); dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};




template<typename T, class compFun>
void findAllLowerBound(T *targetRgStart,
                       T *target, T *targetEnd,
                       T *lookUpSt, T *lookUpEnd,
                       T **rst, compFun *comp)
{
  if(targetEnd <= target) return;
  T *mid = target + (targetEnd - target) / 2, *lb = NULL;
  lb = std::lower_bound(lookUpSt, lookUpEnd, *mid, *comp);
  rst[mid - targetRgStart] = lb;


  // Left half
  findAllLowerBound(targetRgStart,
                    target, mid,
                    lookUpSt,
                    std::min(lb + 1, lookUpEnd + 0),
                    rst, comp);


  // Right half
  findAllLowerBound(targetRgStart,
                    mid + 1, targetEnd,
                    std::min(lb, lookUpEnd), lookUpEnd,
                    rst, comp);
}




template<typename T, class compFun>
void findAllUpperBound(T *targetRgStart,
                       T *target, T *targetEnd,
                       T *lookUpSt, T *lookUpEnd,
                       T **rst, bool minus1, compFun *comp)
{
  if(targetEnd <= target) return;


  T *mid = target + (targetEnd - target) / 2, *ub = NULL;
  ub = std::upper_bound(lookUpSt, lookUpEnd, *mid, *comp);
  if(minus1) --ub;
  rst[mid - targetRgStart] = ub;


  // left half
  findAllLowerBound(targetRgStart,
                    target, mid,
                    lookUpSt, std::min(ub + 1, lookUpEnd),
                    rst, comp);


  // right half
  findAllLowerBound(targetRgStart,
                    mid + 1, targetEnd,
                    std::min(ub, lookUpEnd), lookUpEnd,
                    rst, comp);
}




template<typename T, typename paraSortIndtype, class compFun>
struct paraMergeSingle: public Worker
{
  paraSortIndtype blockSize;
  T *leftStart, *leftEnd, *rightStart, *rightEnd;
  T *mergeStart; // ranges shall not overlap
  T **rightBlocksSt;
  compFun *comp;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      T *thisBlockSt = leftStart + objI * blockSize,
        *thisBlockEnd = std::min(thisBlockSt + blockSize, leftEnd + 0);
      T *mergeTo = (thisBlockSt - leftStart) + (rightBlocksSt[objI] - rightStart) + mergeStart;
      std::merge(thisBlockSt, thisBlockEnd, rightBlocksSt[objI],
                 rightBlocksSt[objI + 1], mergeTo, *comp);
    }
  }


  paraMergeSingle(std::size_t blocks, T *leftStart, T *leftEnd,
                  T *rightStart, T *rightEnd,
                  T *mergeStart, compFun *comp, unsigned maxCore):
    leftStart(leftStart), leftEnd(leftEnd),
    rightStart(rightStart), rightEnd(rightEnd),
    mergeStart(mergeStart), comp(comp)
  {
    if(true)
    {
      std::size_t tmpSize = leftEnd - leftStart;
      blockSize = std::max<std::size_t> (1, tmpSize / blocks);
      blocks = tmpSize / blockSize;
      std::size_t res = tmpSize % blockSize;
      if(res != 0) ++blocks;
    }


    std::vector<T*> rightBlocksStContainer(blocks + 1);
    rightBlocksSt = &rightBlocksStContainer[0];
    {
      std::vector<T> searchItem(blocks);
      for(std::size_t i = 0, iend = blocks; i < iend; ++i)
        searchItem[i] = leftStart[i * blockSize];
      findAllLowerBound(&searchItem[0], &searchItem[0],
                        &*searchItem.end(),
                        rightStart, rightEnd,
                        &rightBlocksSt[0], comp);
      rightBlocksStContainer.front() = rightStart;
      rightBlocksStContainer.back() = rightEnd;
    }
    maxCore = std::max<unsigned> (1, std::min<unsigned> (maxCore, blocks));
    dynamicTasking dt(maxCore, blocks); dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};




template<typename T, typename paraSortIndtype, class compFun>
struct paraMerge2blocksConsecutive: public Worker
{
  paraSortIndtype blockSize;
  paraSortIndtype blocks;
  T *start, *End;
  T *mergeStart; // ranges shall not overlap
  compFun *comp;
  dynamicTasking *dT;


  void operator() (std::size_t st, std::size_t end)
  {
    for(;;)
    {
      std::size_t objI = 0;
      if(!dT->nextTaskID(objI)) break;
      T *firstBlockSt = start + 2 * objI * blockSize,
        *firstBlockEnd = firstBlockSt + blockSize;
      T *secondBlockSt = firstBlockEnd, *secondBlockEnd = secondBlockSt + blockSize;
      if(objI * 2 == blocks - 2) secondBlockEnd = End;
      T *mergeTo = (firstBlockSt - start) + mergeStart;
      std::merge(firstBlockSt, firstBlockEnd, secondBlockSt, secondBlockEnd, mergeTo, *comp);
    }
  }


  paraMerge2blocksConsecutive(
    paraSortIndtype blockSize, paraSortIndtype blocks,
    T *start, T *End, T *mergeStart, compFun *comp, unsigned maxCore):
    blockSize(blockSize), blocks(blocks), start(start),
    End(End), mergeStart(mergeStart), comp(comp)
  {
    maxCore = std::max<unsigned> (1, std::min<unsigned> (maxCore, blocks / 2));
    dynamicTasking dt(maxCore, blocks / 2); dT = &dt;
    parallelFor(0, maxCore, *this);
  }
};



template<typename targetType, typename paraSortIndtype>
struct lessThan
{
  targetType *target;
  bool operator() (const paraSortIndtype x, const paraSortIndtype y)
  {
    return target[x] < target[y];
  }
  bool compare(const paraSortIndtype x, const paraSortIndtype y)
  {
    return target[x] < target[y];
  }
  targetType content(const paraSortIndtype x)
  {
    return target[x];
  }
  bool greaterThan(const paraSortIndtype x, const paraSortIndtype y)
  {
    return target[x] > target[y];
  }
};




template<typename T, typename paraSortIndtype, class compFun> // sortTarget is not the original vector
inline void paraSort(
    std::vector<T> &sortTarget,
    std::vector<T> &merger, // a container of the same size of sortTarget.
    unsigned maxCore,
    paraSortIndtype bottomSortBlocks = 0, // better be the least power of 2 greater than maxCore
    paraSortIndtype paraMergeSingleBlocks = 0,
    compFun *comp = nullptr)
{

  if(bottomSortBlocks == 0)
  {
    bottomSortBlocks = 1;
    while(true)
    {
      if(bottomSortBlocks * bottomSortBlocks * bottomSortBlocks > sortTarget.size())
      {
        bottomSortBlocks /= 2;
        break;
      }
      else bottomSortBlocks *= 2;
    }
  }


  if(paraMergeSingleBlocks == 0)
  {
    paraMergeSingleBlocks = maxCore * maxCore;
  }


  paraSortBottom<T, paraSortIndtype, compFun>
    sortedRg(&sortTarget[0], &*sortTarget.end(), bottomSortBlocks, comp, maxCore);


  paraSortIndtype blockSize = sortedRg.blockSize;
  paraSortIndtype blocks = sortedRg.blocks;


  if(blocks <= 1) return;


  merger.resize(sortTarget.size());
  while(blocks > 1)
  {

    T *leftStart = &sortTarget[0], *leftEnd = leftStart + blockSize,
      *rightStart = leftEnd, *rightEnd = rightStart + blockSize,
      *mergeStart = leftStart - &sortTarget[0] + &merger[0];


    std::size_t whichBlock = 0;
    if(whichBlock == blocks - 2) rightEnd = &*sortTarget.end();


    // if (blocks < maxCore * maxCore)
    if(true)
    {
      while(true)
      {
        paraMergeSingle<T, paraSortIndtype, compFun> (
            paraMergeSingleBlocks, leftStart, leftEnd,
            rightStart, rightEnd, mergeStart, comp, maxCore);


        whichBlock += 2;
        if(whichBlock > blocks - 2) break;


        leftStart += blockSize * 2;
        leftEnd = leftStart + blockSize;
        rightStart = leftEnd;
        rightEnd = rightStart + blockSize;
        mergeStart = leftStart - &sortTarget[0] + &merger[0];
        if(whichBlock == blocks - 2) rightEnd = &*sortTarget.end();
      }
    }
    else
    {
      paraMerge2blocksConsecutive<T, paraSortIndtype, compFun> (
          blockSize, blocks, &sortTarget[0], &*sortTarget.end(),
          &merger[0], comp, maxCore);
    }


    sortTarget.swap(merger);
    blockSize *= 2;
    blocks /= 2;
  }


}




// don delete the below! They could be very useful for future debugging purposes




// // [[Rcpp::export]]
// IntegerVector testParaSort(NumericVector x, int maxCore = 7, int bottomSortBlocks=0,
//                   int paraMergeSingleBlocks=0)
// {
//
//   lessThan<double, int>comp;
//   comp.target = &x[0];
//   vec<eint>xorder(x.size());
//   for(eint i=0,iend=x.size();i!=iend;++i)xorder[i]=i;
//
//
//   paraSort<eint, lessThan<double, int> >(xorder, maxCore, bottomSortBlocks, paraMergeSingleBlocks, &comp);
//   IntegerVector rst(xorder.begin(), xorder.end());
//   return rst + 1;
// }




// // [[Rcpp::export]]
// IntegerVector testFindAllLowerBound(NumericVector x, NumericVector lookUp)
// {
//   vec<double*>v(x.size());
//   findAllLowerBound<double, lessThan>(&x[0],
//                                       &x[0], &*x.end(),
//                                       &lookUp[0], &*lookUp.end(),
//                                       &v[0], NULL);
//                                       IntegerVector rst(v.size());
//                                       for(int i=0,iend=rst.size();i!=iend;++i)
//                                       {
//                                         rst[i] = v[i] - &lookUp[0] + 1;
//                                       }
//                                       return rst;
// }
//
//
// // [[Rcpp::export]]
// IntegerVector testFindAllUpperBound(NumericVector x, NumericVector lookUp, bool minus1=1)
// {
//   vec<double*>v(x.size());
//   findAllUpperBound<double, lessThan>(&x[0],
//                                       &x[0], &*x.end(),
//                                       &lookUp[0], &*lookUp.end(),
//                                       &v[0], minus1, NULL);
//                                       IntegerVector rst(v.size());
//                                       for(int i=0,iend=rst.size();i!=iend;++i)
//                                       {
//                                         rst[i] = v[i] - &lookUp[0] + 1;
//                                       }
//                                       return rst;
// }


/*

template<typename T, class compFun>
inline void order(T*x, T*y, compFun*comp, T*endBarrier=NULL)
{
  // if(y>=endBarrier)return;
  if(comp->greaterThan(*x, *y))std::swap(*x, *y);
}


// when divior is power of 2
inline std::size_t quotient(std::size_t x, unsigned char power)
{
  return x >> power;
}
inline std::size_t residual(std::size_t x, unsigned char power)
{
  return (((std::size_t)1 << power) - 1) & x;
}


// don't delete ! prototype function
// whichStep always starts from 1
// blockSize is power of 2
// blocks is power of 2
// inline std::size_t numberOfTasks(std::size_t blockSize, std::size_t blocks, std::size_t whichStep)
// {
//   // the merged block size would be blockSize * 2
//   // std::size_t mergedBlockSize = blockSize * 2;
//   if(whichStep == 1)return blockSize / (blocks / 2);
//   std::size_t slicesInMergedBlock = 1 << whichStep;
//   return blockSize * 2 / slicesInMergedBlock * ((slicesInMergedBlock - 2) / 2) / (blocks / 2);
// }


inline std::size_t numberOfTasks(unsigned char blockSizePower,
                                 unsigned char blocksPower,
                                 unsigned char whichStep)
{
  if(whichStep == 1)return 1 << (blockSizePower + blocksPower - 1);
  std::size_t slicesInMergedBlock = (std::size_t)1 << whichStep;
  // Rcout << "(blockSizePower - whichStep + blocksPower - 1) = "<<
    // (blockSizePower - whichStep + blocksPower - 1)<<"\n";
  int shift = blockSizePower - whichStep + blocksPower - 1;
  // Rcout<<shift<<"\n";
  if(shift>=0)return (slicesInMergedBlock - 2)<<shift;
  else return (slicesInMergedBlock - 2)>>(0-shift);
}


// whichStep always starts from 1
// inline std::size_t swapPairGap(std::size_t blockSize, std::size_t whichStep)
// {
//   return blockSize >> (whichStep - 1);
// }


inline std::size_t swapPairGap(unsigned char blockSizePower, unsigned char whichStep)
{
  return (std::size_t)1 << (blockSizePower - (whichStep - 1));
}


// give index of the array based on the task index
inline std::size_t giveIndex(std::size_t objI, std::size_t tasksInMerger,
                             unsigned char mergerSizePower,
                             unsigned char sliceSizePower)
{
  std::size_t A = objI / tasksInMerger, B = objI % tasksInMerger;
  return (A << mergerSizePower) + ((((B >> sliceSizePower) * 2) + 1) << sliceSizePower)
    + residual(B, sliceSizePower);
}


// x must not be 0
inline unsigned char findPowerOfPowerOf2(std::size_t x)
{
  unsigned char rst = 0;
  while(true)
  {
    x >>= 1;
    if(x != 0)
    {
      ++rst;
    }
    else break;
  }
  return rst;
}


// // [[Rcpp::export]]
// void testIndexGivenAndGap(int arraySize, int whichStep, int blocks)
// {
//   // std::size_t blockSize = arraySize / blocks;
//   unsigned char blocksPower = findPowerOfPowerOf2(blocks);
//   unsigned char blockSizePower = findPowerOfPowerOf2(arraySize / blocks);
//   std::size_t tasks = numberOfTasks(blockSizePower, blocksPower, whichStep);
//   Rcout<<"tasks = "<<tasks<<"\n";
//   Rcout << "gap = " << swapPairGap(blockSizePower, whichStep) << "\n";
//   unsigned numberOfMergedBlocks = blocks / 2;
//   unsigned tasksInMerger= tasks / numberOfMergedBlocks;
//   unsigned char mergerSizePower = findPowerOfPowerOf2(arraySize / blocks * 2);
//
//
//   Rcout<< "numberOfMergedBlocks = " << numberOfMergedBlocks <<"\n";
//   Rcout<< "tasksInMerger = " << tasksInMerger<<"\n";
//   Rcout<< "mergerSizePower = " << (int)mergerSizePower <<"\n";
//
//
//   unsigned char sliceSizePower = findPowerOfPowerOf2(arraySize / blocks * 2 / (1 << whichStep));
//   Rcout << "sliceSizePower = " << (int)sliceSizePower << "\n";
//
//
//   for(std::size_t i=0; i!=tasks; ++i)
//   {
//     Rcout << "task index = " << i << ", array index = " <<
//       giveIndex(i, tasksInMerger, mergerSizePower, sliceSizePower)<<"\n";
//   }
// }


inline std::size_t step1giveIndex(std::size_t objI, unsigned char blockSizePower)
{
  std::size_t A = quotient(objI, blockSizePower), // which block
    B = residual(objI, blockSizePower);
  return (A << (blockSizePower + 1)) + B;
}


// // [[Rcpp::export]]
// int teststep1giveIndex(int objI, int blockSizePower)
// {
//   return step1giveIndex(objI, blockSizePower);
// }


template<typename T, class compFun>
struct stage0: public Worker
{
  T*v, *vend;
  compFun*comp;
  dynamicTasking*dT;
  void operator()(std::size_t st, std::size_t end)
  {
    for(std::size_t I=st;I!=end;++I)
    {
      for(;;)
      {
        std::size_t objI = 0;
        if(!dT->nextTaskID(objI))break;
        order(v + objI * 2, v + objI * 2 + 1, comp, vend);
      }
    }
  }
  stage0(std::size_t NofTasks, T*v, T*vend, compFun*comp, unsigned maxCore):
   v(v), vend(vend), comp(comp)
  {
    dT = new dynamicTasking(maxCore, NofTasks);
    parallelFor(0, maxCore, *this);
    delete dT;
  }
};





template<typename T, class compFun>
struct stage1: public Worker
{
  unsigned char blockSizePower;
  std::size_t gap;
  T*v, *vend;
  compFun*comp;
  dynamicTasking*dT;
  void operator()(std::size_t st, std::size_t end)
  {
    for(std::size_t I=st;I!=end;++I)
    {
      for(;;)
      {
        std::size_t objI = 0;
        if(!dT->nextTaskID(objI))break;
        std::size_t i = step1giveIndex(objI, blockSizePower);
        order(v + i, v + i + gap, comp, vend);
      }
    }
  }
  stage1(unsigned char blockSizePower, std::size_t gap,
         T*v, T*vend, std::size_t NofTasks, compFun*comp, unsigned maxCore):
    blockSizePower(blockSizePower), gap(gap), v(v), vend(vend) , comp(comp)
  {
    dT = new dynamicTasking(maxCore, NofTasks);
    parallelFor(0, maxCore, *this);
    delete dT;
  }
};




template<typename T, class compFun>
struct stagei: public Worker
{
  unsigned char mergerSizePower;
  unsigned char sliceSizePower;
  std::size_t tasksInMerger;
  // std::size_t NofTasks;
  std::size_t gap;
  T*v, *vend;
  compFun*comp;
  dynamicTasking*dT;
  void operator()(std::size_t st, std::size_t end)
  {
      for(;;)
      {
        std::size_t objI = 0;
        if(!dT->nextTaskID(objI))break;
        std::size_t i = giveIndex(objI, tasksInMerger, mergerSizePower, sliceSizePower);
        order(v + i, v + i + gap, comp, vend);
      }
  }
  stagei(std::size_t NofTasks, unsigned char mergerSizePower, unsigned char sliceSizePower,
         std::size_t tasksInMerger, std::size_t gap, T*v, T*vend,
         compFun*comp, unsigned maxCore):
    mergerSizePower(mergerSizePower), sliceSizePower(sliceSizePower),
    tasksInMerger(tasksInMerger), gap(gap), v(v),
    vend(vend), comp(comp)
  {
    dT = new dynamicTasking(maxCore, NofTasks);
    parallelFor(0, maxCore, *this);
    delete dT;
  }
};




std::size_t leastLargerEqualPower2(std::size_t x)
{
  std::size_t rst = 1;
  while(rst < x)
  {
    rst *= 2;
  }
  return rst;
}




template<typename T, class compFun>
void batcherOddEvenSort(T*v, T*vend, compFun*comp, unsigned maxCore)
{
  if(vend - v <= 8)
  {
    if(comp==NULL)std::sort(v, vend);
    else std::sort(v, vend, *comp);
    return;
  }
  std::size_t NofAtoms = leastLargerEqualPower2(vend - v);
  stage0<T, compFun>(NofAtoms / 2, v, vend, comp, maxCore);


  // stage1(T*v, T*vend, std::size_t gap, std::size_t NofTasks, compFun*comp, unsigned maxCore):
  for(unsigned I=0, Iend = findPowerOfPowerOf2(NofAtoms/2); I != Iend; ++I)
  {
    unsigned char blockSizePower = I + 1, blocksPower = Iend + 1 - blockSizePower;
    unsigned char whichStep = 1;
    std::size_t NofTasks = numberOfTasks(blockSizePower, blocksPower, whichStep);
    // std::size_t gap = swapPairGap(blockSizePower, whichStep);
    std::size_t gap = 1 << (I + 1);
    // stage1(unsigned char blockSizePower, std::size_t gap,
    //      T*v, T*vend, std::size_t NofTasks, compFun*comp, unsigned maxCore)
    stage1<T, compFun>(blockSizePower, gap, v, vend, NofTasks, comp, maxCore);


    // Rcout<<"NofTasks = "<<NofTasks<<", gap = "
    //        <<gap<<", blockSizePower = "<<(int)blockSizePower<<", blocksPower = "
    //        <<(int)blocksPower<<", whichStep = "<<(int)whichStep<<"\n";



    gap /= 2;
    // ++blockSizePower;
    // --blocksPower;
    ++whichStep;



    // longjmp(env,1);


    while (gap >= 1)
    {
      NofTasks = numberOfTasks(blockSizePower, blocksPower, whichStep);


      std::size_t tasksInMerger = NofTasks >> (blocksPower - 1);
      // std::size_t tasksInMerger = NofTasks >> (blocksPower);

      // Rcout<<"NofTasks = "<<NofTasks<<", gap = "
      //      <<gap<<", blockSizePower = "<<(int)blockSizePower<<", blocksPower = "
      //      <<(int)blocksPower<<", whichStep = "<<(int)whichStep<<", tasksInMerger = "<<
      // tasksInMerger<<"\n";


      // longjmp(env,1);


      stagei<T, compFun>(NofTasks, blockSizePower + 1, blockSizePower + 1 - whichStep,
                         tasksInMerger, gap, v, vend, comp, maxCore);

      // stagei(std::size_t NofTasks, unsigned char mergerSizePower, unsigned char sliceSizePower,
      //    std::size_t tasksInMerger, std::size_t gap, T*v, T*vend,
      //    compFun*comp, unsigned maxCore)


      //if(gap==1&&I==1)longjmp(env,1);


      gap /= 2;
      // ++blockSizePower;
      // --blocksPower;
      ++whichStep;
    }
  }
  return;
}




// // [[Rcpp::export]]
// IntegerVector testBatcherSort(NumericVector x, int maxCore = 7)
// {
//
//   // int jval = setjmp (env);
//   // if(jval)return 0;
//
//   lessThan<double, eint>comp;
//   comp.target = &x[0];
//   vec<eint>xorder(x.size());
//   for(eint i=0,iend=x.size();i!=iend;++i)xorder[i]=i;
//   batcherOddEvenSort<eint, lessThan<double, eint> >(&xorder[0], &*xorder.end(), &comp, maxCore);
//
//
//   // paraSort<eint, lessThan<double, int> >(xorder, maxCore, bottomSortBlocks, paraMergeSingleBlocks, &comp);
//   IntegerVector rst(xorder.begin(), xorder.end());
//   return rst + 1;
// }

*/









































