#include "general.h"
#include "error.h"
#include "binnode.h"



void binnode::copy(binnode &Source)
{
    Identification = Source.Identification ;
    Model = Source.Model ;
    Construct = Source.Construct ;
    weight = Source.weight ;
    weightLeft = Source.weightLeft ;
    DTrain = Source.DTrain ;
    NAnumValue = Source.NAnumValue ;
    NAdiscValue = Source.NAdiscValue ;
    Classify = Source.Classify;     
    majorClass = Source.majorClass ;
}


