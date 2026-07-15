#if !defined(ESTIMATOR_H)
#define ESTIMATOR_H

#include "ftree.h"
#include "contain.h"
#include "utils.h"

const int constNAdiscretizationIntervals = 5 ;
const int constAverageExamplesPerInterval = 5 ;

enum oeDistanceType {kEqual=0, expRank=1,  bestK=2} ;
//enum oeConfidenceInterval { ciTwoSided=1,ciUpper=2,ciLower=3};

class estimation
{
friend class featureTree ;
friend class construct ;
friend class expr ;
friend class rf ;

    const featureTree *fTree ;
    Options eopt ;
    mmatrix<int> DiscValues ;
    mmatrix<double> NumValues ;
//    marray<double> contDiffA, discDiffA ;
//    double diffC ;
    marray<double> weight;
    mmatrix<marray<double> > NAdiscValue, NAnumValue ;
    marray<double> minValue, maxValue, valueInterval, step ;
    mmatrix<double> NumDistance, DiscDistance ;
    marray<int> discNoValues ;
    marray<int> originalDTrain ; // needed in ordEvalInst
    marray<marray<sortRec> > distanceArray, diffSorted ; // manipulation of the nearest examples
    //marray<sortRec> distanceRarray, diffRsorted ;// R variant
	marray<sortRec>	distanceEarray, diffEsorted  ;
    marray<sortRec> distanceEHarray, diffEHsorted,
                    distanceEMarray, diffEMsorted ; // E variant: manipulation of the nearest examples

    int currentNumSize, currentDiscSize, discUpper, numUpper ;
    int NoIterations ;
    int kNearestEqual, kDensity ;
    double varianceDistanceDensity ;
    int noClasses ;
    int noNAdiscretizationIntervals ;
	marray<double> weightDisc, weightNum ;
    typedef double (estimation::*PfImpurity)(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    typedef double (estimation::*PfImpurityGain)(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    typedef double (estimation::*PfImpurityDistr)(marray<double> &distr) ;
	typedef double (estimation::*PfDistStep)(int c1, int c2, mmatrix<int> &noClassAttrVal) ;

    PfImpurity fImpurity ;
    PfImpurityGain fImpurityGain ;
    PfImpurityDistr fImpurityUniform ;
    PfDistStep fDistStep ;
    int preparedEstimator ; //, activeEstimator ;

    double CAdiff(int AttrNo, int I1, int I2) ;
    double DAdiff(int AttrNo, int I1, int I2) ;
    double CARamp(int AttrIdx, double distance) ;
    void prepareDistanceFactors(int distanceType) ;
    void RprepareDistanceFactors(int distanceType) ; // for E variant of Relief
    void EprepareDistanceFactors(oeDistanceType distanceType) ;
    void computeDistances(int Example) ;
    double CaseDistance(int I1) ;
    double WeightedCaseDistance(int I1) ;
    void findHitMiss(int current, int &hit, int &miss) ;
    inline double NAnumDiff(int AttrIdx, int ClassValue, double Value) ;
    void stratifiedExpCostSample(marray<int> &sampleIdx, int sampleSize, int domainSize, marray<double> &probClass, marray<int> &noExInClass) ;
	void computeDistancesOrd(int Example)  ;
	void computeDistancesOrdClDiff1(int Example)  ;
	inline double DAdiffOrd(int AttrIdx, int I1, int I2)  ;
	inline double DAdiffSign(int AttrIdx, int I1, int I2)  ;
    inline double CAdiffSign(int AttrIdx, int I1, int I2) ;
	inline double CAdiffSignRamp(int AttrIdx, int I1, int I2) ;
    void prepare3clDistanceFactors(int current, oeDistanceType distanceType) ;
    void prepareImpurityFunction(int selectedEstimator) ;
    double infGainImpurity(double weight, marray<int> &noExInClass) ;
    double gainRatio(double impuriy, double weight, double weightLeft, marray<int> &noExInClassLeft, double weightRight, marray<int> &noExInClassRight);
    double infGain(double impurity, double weight, double weightLeft, marray<int> &noExInClassLeft, double weightRight, marray<int> &noExInClassRight);
    double infGainImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double gainRatio(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double infGain(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double accuracyGain(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double accuracyImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double DKMImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double DKMgain(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double giniImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double giniGain(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double MDLimpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double MDLgain(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double ReliefMyopicFast(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double infGainCostImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double DKMcostImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double giniOnDistribution(marray<double> &dist) ;
    double DKMonDistribution(marray<double> &dist) ;
    double infOnDistribution(marray<double> &dist) ;
    double accuracyOnDistribution(marray<double> &dist) ;
    double DistAngle(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double DistAUC(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double DistEuclid(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double infEqual(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double EqualDKM(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double giniEqual(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double zeroImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    //double EuclidImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    //double hellingerImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double EuclidHellingerImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double distanceImpGain(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double DistHellinger(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double EqualHellinger(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double BhattacharyyaImpurity(int weight, mmatrix<int> &noClassAttrVal, int valIdx) ;
    double BhattacharyyaImpFast(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double BhattacharyyaFast(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double BhattacharyyaCond(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double gainUniform(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal) ;
    double accUniform(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);
    double stepHellinger(int c1, int c2, mmatrix<int> &noClassAttrVal) ;
    double stepEuclid(int c1, int c2, mmatrix<int> &noClassAttrVal) ;
    double stepAngle(int c1, int c2, mmatrix<int> &noClassAttrVal) ;
    double stepAUC(int c1, int c2, mmatrix<int> &noClassAttrVal) ;

#if defined(RAMP_FUNCTION)
    marray<double> DifferentDistance, EqualDistance, CAslope ;
#endif


public:
    int noDiscrete, noNumeric, TrainSize ;
    marray<double> NumEstimation, DiscEstimation ;
    marray<double> splitPoint ;
    estimation(const featureTree *fTreeParent, marray<int> &DTrain,
                marray<double> &pDTrain, int TrainSize) ;
    ~estimation() { }
    void initialize(marray<int> &inDTrain, marray<double> &inpDTrain, int inTrainSize) ;
    void destroy(void) ;
    int estimate(int selectedEstimator, int contAttrFrom, int contAttrTo,
                         int discAttrFrom, int discAttrTo, attributeCount &bestType) ;
    int estimateConstruct(int selectedEstimator, int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, attributeCount &bestType) ;
    int estimateSelected(marray<int> &rankList, int noSelected, attributeCount &bestType) ;
    void ReliefF(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefFbestK(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void Relief(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo) ;
    void ReliefFmerit(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefFavgC(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefFexpC(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefFpa(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefFpe(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefFsmp(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefRcost(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefEcost(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType) ;
    void ReliefFcostKukar(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo) ;
    //void infGain(int discAttrFrom, int discAttrTo) ;
    //void gainRatio(int discAttrFrom, int discAttrTo) ;
    //void mdl(int discAttrFrom, int discAttrTo) ;
    //void MDLsmp(int discAttrFrom, int discAttrTo) ;
    //void ReliefMyopic(int discAttrFrom, int discAttrTo) ;
    //void Accuracy(int discAttrFrom, int discAttrTo) ;
    //double binAccEst(mmatrix<int> &noClassAttrVal, int noValues) ;
    //void BinAccuracy(int discAttrFrom, int discAttrTo) ;
    //void Gini(int discAttrFrom, int discAttrTo) ;
    //void DKM(int discAttrFrom, int discAttrTo) ;
    //void DKMc(int discAttrFrom, int discAttrTo) ;
    //void gainRatioC(int discAttrFrom, int discAttrTo) ;
    booleanT isMyopic(int selectedEstimator) ;
	//void aVReliefF(int discAttrFrom, int discAttrTo, marray<marray<double> > &result, int distanceType) ;
    void adjustTables(int newContSize, int newDiscSize) ;
    void prepareContAttr(int attrIdx) ;
    void prepareDiscAttr(int attrIdx, int noValues) ;
 	void binarizeGeneral(construct &nodeConstruct, int firstFreeDiscSlot) ;
    double bestSplitGeneral(construct &nodeConstruct, int firstFreeDiscSlot) ;
    double impuritySplit(construct &nodeConstruct, double &bestEstimation) ;
    double impuritySplitSample(construct &nodeConstruct, double &bestEstimation);
    double discretizeGreedy(int ContAttrIdx, int maxBins, marray<double> &Bounds, int firstFreeDiscSlot) ;
	void estBinarized(int selectedEstimator, int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int firstFreeDiscSlot) ;
    double CVVilalta(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo) ;
    double CVmodified(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo) ;
    void discretizeEqualFrequency(int ContAttrIdx, int noIntervals, marray<double> &Bounds) ;
    void discretizeEqualWidth(int ContAttrIdx, int noIntervals, marray<double> &Bounds) ;
    double estImpurityDisc(int discidx) ;
    double distMulticlassEvaluation(double priorImpurity, int weight, marray<int> &attrVal, mmatrix<int> &noClassAttrVal);

    void prepareDistanceFactors(int distanceType, marray<marray<sortRec> > &distanceArray,
    		 marray<marray<sortRec> > &diffSorted, mmatrix<double> &DiscDistance, mmatrix<double> &NumDistance ) ;
    void computeDistances(int Example, mmatrix<double> &DiscDistance, mmatrix<double> &NumDistance);
    double CaseDistance(int I1, mmatrix<double> &DiscDistance, mmatrix<double> &NumDistance) ;

/*    void ordAvReliefF(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &resultCpAp, marray<marray<double> > &resultCpAn,
			marray<marray<double> > &resultCpAe, marray<marray<double> > &resultCnAp,
			marray<marray<double> > &resultCnAn, marray<marray<double> > &resultCnAe,
			marray<marray<double> > &resultCeAp, marray<marray<double> > &resultCeAn,
			marray<marray<double> > &resultCeAe, int distanceType) ;
	void ordAV3(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &resultCpAp, marray<marray<double> > &resultCpAn,
			marray<marray<double> > &resultCpAe, marray<marray<double> > &resultCnAp,
			marray<marray<double> > &resultCnAn, marray<marray<double> > &resultCnAe,
			marray<marray<double> > &resultCeAp, marray<marray<double> > &resultCeAn,
			marray<marray<double> > &resultCeAe, int distanceType) ;
	void ordAV3dAnorm(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) ;
	void ordAV3dA(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) ;
	void ordAV3dC(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) ;
	void ordAV3dCnorm(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) ;
    void ordAVdAeqNorm1(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) ;
	void ordClassdAeqNorm(int contAttrFrom, int contAttrTo, int distanceType,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
	        mmatrix<marray<double> > &reinfPosRnd, mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd) ;
*/
    void ordAVdAeq(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, oeDistanceType distanceType) ;
    void ordAVdAeqNorm(int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
	        mmatrix<marray<double> > &reinfPosRnd, mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd) ;
    void ordEvalInst(int selectedInstance, int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
    	        marray<double> &reinfPos, marray<double> &reinfNeg, marray<double> &anchor,
    	        marray<marray<double> > &reinfPosRnd, marray<marray<double> > &reinfNegRnd, marray<marray<double> > &anchorRnd) ;
    void ordEvalInst3(int selectedInstance, int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
    		marray<double> &reinfPos, marray<double> &reinfNeg, marray<double> &anchor,
    		marray<marray<double> > &reinfPosRnd, marray<marray<double> > &reinfNegRnd, marray<marray<double> > &anchorRnd);
    void oeExpDistr(int discAttrFrom, int discAttrTo, marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor) ;
    void ordAVdAeqNormClDiff1(int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
    	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
    	        mmatrix<marray<double> > &reinfPosRnd, mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd) ;
    void ordAVdAeqNormAttrDiff1(int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
    	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
    	        mmatrix<marray<double> > &reinfPosRnd, mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd) ;
    //void setActive(int estimator) ;
}  ;

#endif
