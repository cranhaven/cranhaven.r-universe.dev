
#include "general.h"
#if defined(DEBUG_NEW)
#include "new_new.h"
#include "error.h"
 
using namespace std ;

const int MAX_SET = 100000; // high limit

void* Set[MAX_SET];
int    SetSize = 0;

void Add(void *ptr)
{
  if ( SetSize > MAX_SET )  // overflow
  {
     merror("NEW_NEW::Add","pointer table size overflow") ;
//     exit( ERR_ALLOC_FULL );
  }
  else
     Set[SetSize++] = ptr;
}

booleanT Member(void *ptr)
{
  int i;
  booleanT IsMember = mFALSE;
  for (i=0; i<SetSize; i++) {
      if (ptr == Set[i]) {
         Set[i] = Set[--SetSize];  // zbrisemo referencirani kazalec
         IsMember = mTRUE;
         break; // for loop
      }
  }
  return IsMember;
}

/* ---------------------------------------------------------------------- */
// Zaseganje pomnilnika

void *forced_new(unsigned long size)
{
  if (size<=0)
    return 0;
  if (size > 10000)
     merror("NEW_NEW::forced_new","allocating very large block") ;

  void *ptr = ALLOC(size);
  if (!ptr)
  {
     merror("NEW_NEW::forced_new","not enough memory available") ;
     // exit( ERR_ALLOC_NULL );
  }
  Add(ptr);
  return ptr;
}


void* operator new(size_t size)
{
  return forced_new(size);
}


void  operator delete(void* ptr)
{

  if (!ptr)
  {
//     merror("NEW_NEW::oprator delete""warning, deallocating null pointer") ;
     return ;
  }
  else
    if (!Member(ptr))
    {
       merror("NEW_NEW::operator delete","deallocating unexisting pointer") ;
       return;
    }

  FREE(ptr);

}


#endif

