*************************************************************************
Changes in version 1.18
*************************************************************************    
NEW FEATURES
    o add a new argument "cndkernel" in the function "qsammon", when the class of "x" is "cndkernmatrix" 
    o add a new argument "qkernel" in the function "qsammon", when the class of "x" is "qkernmatrix" 

FIXES
    o fixed error in the function "qsammon": in feature space, we should use the square root of qkernmatrix instead of Euclidean distance 
    o fixed error messages which the function "qsammon" prints. The position of these two messages ("Warning : MaxHalves exceeded." and "Optimisation terminated - TolFun exceeded.") should be exchanged
**********************************************************
Changes in version 1.19
**********************************************************
NEW FEARURES
     o added a new cnd kernel function called "norcnd"，the kernel function denotes K(x,y) = ||x-y||^2
**********************************************************




