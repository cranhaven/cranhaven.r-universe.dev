#include "general.h"
#include "error.h"

/********************************************************************
*
*   Name:   class bintreeReg
*
*   Description: base class, representing binary tree
*
*********************************************************************/

#include "bintreeReg.h"

using namespace std ;


bintreeReg::bintreeReg(bintreeReg &Copy)
{
   root = 0 ;
   copy(Copy) ;
}

//************************************************************
//
//                       destroy
//                       -------
//
//          recursivly destroys entire binary tree
//
//************************************************************
void bintreeReg::destroy(binnodeReg *branch)
{
   if (branch)
   {
      destroy(branch->left);
      destroy(branch->right);

      delete branch ;
   }
}

//************************************************************
//
//                       noLeaves
//                       --------
//
//          counts the leaves in the binary tree
//
//************************************************************
int bintreeReg::noLeaves(binnodeReg *branch) const
{
   if (branch->left==0) // && (branch->right==0))
      return 1 ;
   return noLeaves(branch->left) + noLeaves(branch->right) ;
}


// **********************************************************************
//
//                      copy
//                      --------
//
//      copies source tree
//
// **********************************************************************
void bintreeReg::copy(bintreeReg &Source)
{
   if (&Source != this)
   {
     destroy() ;
     if (Source.root)
       dup(Source.root,root) ;
     else
       root = 0 ;
   }
}


// **********************************************************************
//
//                      operator =
//                      --------
//
//      copies source tree
//
// **********************************************************************
bintreeReg& bintreeReg::operator=(bintreeReg &Source)
{
   copy (Source) ;
   return *this ;
}


//**********************************************************************
//
//                      dup
//                      -------
//
//      duplicates source booleanT exprRegession into target
//
//**********************************************************************
void bintreeReg::dup(binnodeReg *Source, binnodeReg* &Target)
{
    Target = new binnodeReg ;
    Target->copy(*Source) ;

    if (Source->left)
      dup(Source->left, Target->left) ;
    else
      Target->left = 0 ;
    if (Source->right)
      dup(Source->right, Target->right ) ;
    else
      Target->right = 0 ;
}




//************************************************************
//
//                       degreesOfFreedom
//                       ----------------
//
//          counts the occurences of all attributes
//
//************************************************************
int bintreeReg::degreesOfFreedom(binnodeReg *branch) const
{
   if (branch->left==0) // && (branch->right==0))
      return branch->Model.degreesOfFreedom() ; ;
   return branch->Construct.degreesOfFreedom() + 
     degreesOfFreedom(branch->left) + degreesOfFreedom(branch->right) ;
}
