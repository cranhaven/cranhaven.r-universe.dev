#if !defined(BINNODE_H)
#define BINNODE_H

#include "expr.h"
#include "constrct.h"
#include "contain.h"


// node for binary tree
class binnode
{
public:
    nodeType Identification ;
    expr Model ;
    construct Construct ;
    double weight, weightLeft ;
    marray<int> DTrain ;
    marray<double> NAnumValue ;
    marray<int> NAdiscValue ;
    marray<double> Classify ;      // weights of all classes in node
    int majorClass ;
 
    binnode *left,*right;

    binnode(void) { left = right = 0 ; Identification = leaf; weight = weightLeft = -DBL_MAX ; majorClass = -1 ; }
    void copy(binnode &Source) ;
    void operator=(binnode &Source)  { copy(Source) ; }
};

typedef binnode* Pbinnode ;

#endif
