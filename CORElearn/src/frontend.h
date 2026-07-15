#if !defined(FRONTEND_H)
#define FRONTEND_H

enum demandType { none=0,
                  leafDistribution=1,
                  constructs=2, distributionAndConstructs=3,
                  priorDiscretization=4, priorDiscretizationOut = 5,
                  residuals=6, ordEval=7, ordEvalNorm=8,
                  ordEvalNormClDiff1=9,ordEvalNormAttrDiff1=10
} ;

#include "general.h"

void outVersion(FILE *fout) ;

#if !defined(R_PORT)
void mainMenu(featureTree* gFT) ;
void singleEstimation(featureTree* const Tree) ;
void allSplitsEstimation(featureTree* const Tree) ;
void singleTree(featureTree* const Tree) ;
void allSplitsTree(featureTree* const Tree) ;
void singleRF(featureTree* const Tree) ;
void allSplitsRF(featureTree* const Tree) ;
void domainCharacteristics(featureTree* const Tree);
//void evalAttrVal(featureTree*  Tree, demandType demand) ;
//void evalOrdAttrVal(featureTree*  Tree, demandType demand)  ;
void evalOrdAttrValNorm(featureTree*  Tree, demandType demand) ;
void runOrdEvalInst(featureTree* const Tree) ;
//void evalOrdClassNorm(featureTree*  Tree)  ;
void saveSingleRF(featureTree* const Tree) ;
//void saveLargeRF(featureTree* const Tree) ;
void loadSingleRF(featureTree* const Tree) ;
void singleEstimationReg(featureTree* const Tree) ;
void allSplitsEstimationReg(const featureTree *Tree) ;
void singleTreeReg(featureTree* const Tree) ;
void allTreeReg(featureTree* const Tree, demandType special=none) ;
void domainCharacteristicsReg(featureTree* const Tree);
FILE* prepareDistrFile(int fileIdx, Options *opt) ;
#endif // if !defined(R_PORT)



#endif
