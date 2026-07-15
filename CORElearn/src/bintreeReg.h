#if !defined(BINTREEREG_H)
#define BINTREEREG_H

#include "exprReg.h"
#include "constrctReg.h"
#include "contain.h"
#include "mstring.h"
#include "binnodeReg.h"


// the basic binary tree
class bintreeReg
{
protected:
   binnodeReg *root ;
   void dup(binnodeReg *Source, binnodeReg* &Target) ;

public:

   bintreeReg() { root=0; }
   ~bintreeReg() { destroy(root); }
   bintreeReg(bintreeReg &Copy) ;
   int operator== (bintreeReg &) { return 0 ; }
   int operator< (bintreeReg &) { return 0 ; }
   int operator> (bintreeReg &) { return 0 ; }
   bintreeReg& operator= (bintreeReg &Source) ;
   void copy(bintreeReg &Source) ;
   void destroy(void) { destroy(root) ; root = 0 ; }
   void destroy(binnodeReg *branch);
   int noLeaves(void) const { return noLeaves(root) ; }
   int noLeaves(binnodeReg *branch) const;
   int degreesOfFreedom(void) const { return degreesOfFreedom(root) ; }
   int degreesOfFreedom(binnodeReg *branch) const ;
};


#endif

