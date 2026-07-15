#if !defined(BINNODEREG_H)
#define BINNODEREG_H

#include <cfloat>

#include "exprReg.h"
#include "constrctReg.h"
#include "contain.h"


// node for binary tree
class binnodeReg
{
public:
    nodeType Identification ;
    exprReg Model ;
    constructReg Construct ;
    double weight, weightLeft ;
    double averageClassValue, minClassValue, maxClassValue ;
    double stdDevClass ;
    double MSE, MAE ;
    marray<int> DTrain ;
    marray<double> NAnumValue ;
    marray<int> NAdiscValue ;
    binnodeReg *left,*right;
    binnodeReg(void) {  left = right = 0 ; Identification = leaf ;
       weight = weightLeft = averageClassValue = minClassValue = maxClassValue = stdDevClass = MSE = MAE = -DBL_MAX ; }
    binnodeReg& operator= (binnodeReg &Source) { copy(Source); return *this ; } 
    void copy(binnodeReg &Source) ;
};

typedef binnodeReg* PbinnodeReg ;

#endif
