#if !defined(ESTIMATORREG_H)
#define ESTIMATORREG_H

#include "regtree.h"
#include "estimator.h"  // to get constants
#include "contain.h"
#include "utils.h"
#include "kdtree.h"

extern const int constNAdiscretizationIntervals ;
extern const int constAverageExamplesPerInterval ;

class estimationReg
{
friend class regressionTree ;
friend class constructReg ;

    const regressionTree *fTree ;
    Options eopt ;
    mmatrix<int> DiscValues ;
    mmatrix<double> NumValues ;
    marray<double> contDiffA, discDiffA ;
    double diffC ;
    marray<double> weight;
    marray<marray<double> > NAdiscValue, NAnumValue ;
    marray<double> minValue, maxValue, valueInterval;
    marray<double> step ;
    // mmatrix<double> NumDistance, DiscDistance ;
    marray<int> discNoValues ;
    marray<sortRec> distanceArray ; // manipulation of the closest
    marray<sortRec> distSort ;  // array for finding K closest

    int currentNumSize, currentDiscSize, discUpper, numUpper ;
    int NoIterations ;
    int kNearestEqual, kDensity ;
    double varianceDistanceDensity ;
    double priorMSE ;
    //double Gini2EntropyConst ;  

    kdTree kdT ;

    double CAdiff(int AttrNo, int I1, int I2) ;
    double DAdiff(int AttrNo, int I1, int I2) ;
    void prepareDistanceFactors(int current, double &distanceSum, int distanceType) ;
    void prepareDistanceFactorsKD(int current, double &distanceSum, int distanceType) ;
    //void computeDistances(int Example) ;
    //double CaseDistance(int I1) ;
    double caseDist(int I1, int I2) ;
    inline double NAnumDiff(int AttrIdx, double Value) ;

#if defined(RAMP_FUNCTION)
    marray<double> DifferentDistance, EqualDistance, CAslope ;
    inline double CARamp(int AttrIdx, double distance) ;
#endif
#if defined(MAHALANOBIS)
    mmatrix<double> covMI ; // inverse of covariance matrix of numeric variables
#endif

public:
    marray<int> OriginalDTrain ;
    marray<double> NumEstimation, DiscEstimation, splitPoint ;
    int noDiscrete, noNumeric, TrainSize ;

	estimationReg(regressionTree *fTreeParent, marray<int> &DTrain,
                 marray<double> &pDTrain, int TrainSize) ;
    ~estimationReg() { }
    int estimate(int selectedEstimator, int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, 
		         attributeCount &bestType) ;
    int estimateConstruct(int selectedEstimator, int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo,
		                   attributeCount &bestType, marray<constructReg> &DiscConstruct, marray<constructReg> &ContConstruct) ;
    void estBinarized(int selectedEstimator, int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, 
		              int firstFreeDiscSlot) ;

    //void CRsimilarity(int contAttrFrom, int contAttrTo,
    //                           int discAttrFrom, int discAttrTo) ;
    void CReliefDensity(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void MSE(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo) ;
    void Combination(int contAttrFrom, int contAttrTo,int discAttrFrom, int discAttrTo, int selectedEstimator) ;
    void MEofModel(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int selectedEstimator) ;
    void RReliefFbestK(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void RReliefF(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int usedEstimator) ;
    void adjustTables(int newContSize, int newDiscSize) ;
    void prepareContAttr(int attrIdx) ;
    void prepareDiscAttr(int attrIdx, int noValues) ;
    void binarizeGeneral(int selectedEstimator, constructReg &nodeConstruct, double &bestEstimation, int firstFreeDiscSlot) ;
    void binarizeBreiman(constructReg &nodeConstruct, double &bestEstimation) ;
    double bestSplitGeneral(int selectedEstimator, constructReg &nodeConstruct, double &bestEstimation, int firstFreeDiscSlot) ;
	double bestMSEsplit(constructReg &nodeConstruct, double &bestEstimation) ;
    void kdTest(int bucket, int kNear, double &kdIni, double &kdSearch, double &pqSearch, int &kdBetter) ;
    double ConceptVariation(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo) ;
    double CVmodified(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo) ;
    void advisor(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo) ;
    booleanT isMyopic(int selectedEstimator) ;
    double discretizeGreedy(int ContAttrIdx, int maxBins, marray<double> &Bounds) ;
    void discretizeEqualFrequency(int ContAttrIdx, int noIntervals, marray<double> &Bounds) ;
    void discretizeEqualWidth(int ContAttrIdx, int noIntervals, marray<double> &Bounds) ;

}  ;

#endif
