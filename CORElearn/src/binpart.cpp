#include <climits>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "contain.h"
#include "utils.h"
#include "binpart.h"

#if !defined(LLONG_MAX)
#define LLONG_MAX 9223372036854775807LL
#endif

using namespace std ;


booleanT binPartition::incLeft(void)
{
  // initial increment
  if (left[0] == 0)
  {
    left[0] = 1 ;
    return mTRUE ;
  }

  int i, position = 0 ;
  while (mTRUE)
  {
    // have we reached the sentinel
    if (left[position] == 1)
    {
      if (position == N-2) // is this the last position
         return mFALSE ;
      // otherwise shift right
      position ++ ;
      left[position] = 0 ; // we increment to 1 in the next sentence
    }
    if (left[position] < N - position) // is there still room for increment
    {
      left[position] ++ ;
      for (i=position - 1 ; i >= 0 ; i--) // set also others
        left[i] = left[i+1] + 1 ;
      return mTRUE ;
    }
    position ++ ;
  }
  return mFALSE ;
}



booleanT binPartition::increment(void)
{
   if (incLeft() )
   {
     // due to readability reasons we will return smaller partition
     int filled = 0 ;
     while (left[filled] != 1)
       filled ++ ;
     booleanT selected = mTRUE ;
     booleanT reversed = mFALSE ;
     if (filled +1 > N/2)
     {
       selected = mFALSE ;
       reversed = mTRUE ;
     }
     
     // set values in partition
     leftPartition.init(reversed) ;
     while (filled >= 0)
     {
       leftPartition[left[filled]] = selected ;
       filled --  ;
     }

     return mTRUE ;
   }
   else 
     return mFALSE ;
}


double binPartition::noPositions(void)
{
   double noP = pow((double)2.0,(double)(N-1)) ;
   if (isNaN(noP))
	   return DBL_MAX ;
   else return noP ;
}

