#include "general.h"
#include "error.h"
#include "binnodeReg.h"



void binnodeReg::copy(binnodeReg &Source)
{
    Identification = Source.Identification ;
    Model = Source.Model ;
    Construct = Source.Construct ;
    weight = Source.weight ;
    weightLeft = Source.weightLeft ;
    averageClassValue = Source.averageClassValue ;
    minClassValue = Source.minClassValue ;
    maxClassValue = Source.maxClassValue ;
    stdDevClass = Source.stdDevClass ;
    MSE = Source.MSE ;
    MAE = Source.MAE ;
    DTrain = Source.DTrain ;
    NAnumValue = Source.NAnumValue ;
    NAdiscValue = Source.NAdiscValue ;

    // pointers to left and right should be handled seperately

}


