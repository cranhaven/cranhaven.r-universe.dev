#include <cfloat>
#include <climits>

#include "general.h"
#include "ftree.h"
#include "estimator.h"
#include "utils.h"
#include "rndforest.h"
#include "rfUtil.h"
#include "options.h"
#include "error.h"

using namespace std ;

void forestTree::copy(forestTree &Source)
{
	ib = Source.ib ;
	oob = Source.oob ;
	oobIdx = Source.oobIdx ;
	t = Source.t ;
}

//************************************************************
//
//                      buildForest
//                      ------------
//
//               builds random forest
//
//************************************************************
int featureTree::buildForest(void) {
	int i, rndAux[3];

	for (i = 0 ; i < 3; i++)
		rndAux[i] = randBetween(0,INT_MAX);

	forest.create(opt->rfNoTrees) ;

	// generate a random generator for each tree separately to ensure deterministic result for parallel execution
	rndStr.initSeed(opt->rfNoTrees, 3, rndAux) ;

	int trainSize = NoTrainCases ;
	if (opt->rfNoSelAttr==0)
		rfNoSelAttr = Mmax(1, intRound(sqrt(double(noAttr)))) ;
	else if (opt->rfNoSelAttr==-1)
		rfNoSelAttr = Mmax(1, 1+int(mlog2(double(noAttr)))) ;
	else if (opt->rfNoSelAttr==-2 || opt->rfNoSelAttr >= noAttr)
		rfNoSelAttr = noAttr ;
	else rfNoSelAttr = opt->rfNoSelAttr ;

	rootWeight = trainSize ;
	marray<double> weight(trainSize, 1.0), wProb(noAttr+1), eProb(noAttr+1) ;

	// prepare weighs for no weighting (set equal weights)
	eProb[0] = 0.0 ;
	//#pragma omp parallel for shared(eProb)
	for (i=1 ; i <= noAttr; i++) {
		eProb[i] = double(i)/ noAttr ;
	}
	eProb[noAttr] = 1.0 ;

	if (opt->rfPropWeightedTrees > 0) {
		// estimate attributes
		attributeCount attrType ;
		double minEval = DBL_MAX  ;
		estimation Estimator(this, DTraining, weight, trainSize) ;
		Estimator.estimate(estReliefFexpRank, 0, noNumeric, 1, noDiscrete, attrType) ;
		for (i=1 ; i <= noAttr; i++) {
			if (AttrDesc[i].continuous)
				wProb[i] = Estimator.NumEstimation[AttrDesc[i].tablePlace] ;
			else
				wProb[i] =  Estimator.DiscEstimation[AttrDesc[i].tablePlace] ;
			if (wProb[i] < minEval)
				minEval = wProb[i] ;
		}
		// prepare cumulative probability distribution
		double eSum = 0.0 ;
		if (minEval < 0) {
			double negReduction = 10.0 ;
			minEval = -minEval/negReduction + 1e-6 ; ;
			for (i=1 ; i <= noAttr; i++) {
				if (wProb[i] < 0)
					wProb[i] = wProb[i]/negReduction + minEval;
				else
					wProb[i] += minEval ;
				eSum += wProb[i] ;
			}
		}
		else
			for (i=1 ; i <= noAttr; i++)
				eSum += wProb[i] ;
		wProb[0] = 0.0 ;
		for (i=1 ; i <= noAttr; i++)
			wProb[i] = wProb[i-1] + wProb[i]/eSum ;
		wProb[noAttr] = 1.0 ;
	}
	else wProb.copy(eProb) ;

	mmatrix<int> oobEval(trainSize, noClasses+1, 0) ;
	// build forest
	// printf("\niter accuracy\n") ;
	int noAvailableEst = 5 ;
	marray<int> availEst(noAvailableEst) ;
	availEst[0] = estReliefFexpRank ;
	availEst[1] = estGainRatio ;
	availEst[2] = estMDL ;
	availEst[3] = estGini ;
	availEst[4] = estMyopicReliefF ;

	// to maintain identical results for multithreaded version compute randomization sequentially in advance
	if (opt->rfSampleProp==0)
		for (i = 0 ; i < opt->rfNoTrees ; i++)
			bootstrapSample(trainSize, DTraining, forest[i].ib, forest[i].oob, forest[i].oobIdx) ;
	else
		for (i = 0 ; i < opt->rfNoTrees ; i++)
			randomSample(trainSize, opt->rfSampleProp, DTraining, forest[i].ib, forest[i].oob, forest[i].oobIdx) ;

	marray<int> estim(opt->rfNoTrees, opt->selectionEstimator ) ;
	if (opt->rfMultipleEst)
		//#pragma omp parallel for shared(estim,availEst,noAvailableEst)
		for (i = 0 ; i < opt->rfNoTrees ; i++){
			estim[i] = availEst[i % noAvailableEst] ;
		}

#pragma omp parallel for // schedule(dynamic, 1)
	for (int it = 0 ; it < opt->rfNoTrees ; it++) {

		if ( it/(double)opt->rfNoTrees < opt->rfPropWeightedTrees) {
			if (opt->rfNoTerminals == 0)
				forest[it].t.root = buildForestTree(forest[it].ib.len(), forest[it].ib, estim[it], wProb, it) ;
			else
				forest[it].t.root = rfBuildLimitedTree(opt->rfNoTerminals, forest[it].ib.len(), forest[it].ib, estim[it], wProb, it) ;
		}
		else {
			if (opt->rfNoTerminals == 0)
				forest[it].t.root = buildForestTree(forest[it].ib.len(), forest[it].ib, estim[it], eProb, it) ;
			else
				forest[it].t.root = rfBuildLimitedTree(opt->rfNoTerminals, forest[it].ib.len(), forest[it].ib, estim[it], eProb, it) ;
		}
		rfConsolidateTree(forest[it].t.root) ;

		// oobEstimate = oobEvaluate(forest[it].t.root, DTraining, forest[it].oob, oobEval) ;
	}
	// regularization
	rfA.create(opt->rfNoTrees, 1.0/opt->rfNoTrees) ;
	if (noClasses == 2 && opt->rfRegType==1) // global regularization
		rfRegularize() ;

	// compute average accuracy and margin
	marray<double> margin(trainSize) ;
	marray<int> maxOther(trainSize) ;
	double varMargin;
	oobEvaluate(oobEval) ;
	avgOobAccuracy = oobAccuracy(oobEval) ;
	avgOobMargin = oobMargin(oobEval, maxOther, varMargin) ;
	avgOobCorrelation = varMargin / sqr(oobSTD(maxOther));
	rndStr.destroy() ;
	return 1 ;
}

//************************************************************
//
//                      oobInplaceEvaluate
//                      -----------------
//
//               evaluates trees computed so far
//
//************************************************************
double featureTree::oobInplaceEvaluate(binnode *rootNode, marray<int> &dSet, marray<booleanT> &oobSet, mmatrix<int> &oob) {
	// for data set dSet, oobSet[i] contains indicator wheather dSet[i] is an out of bag instance
	int i, j, max, oobCorrect=0, valid=0  ;
	marray<double> probDist(noClasses+1) ;
	for (i=0; i < dSet.filled() ; i++) {
		if (oobSet[i]) {
			// update with current tree
			probDist.init(0.0) ;
			max = rfTreeCheck(rootNode,dSet[i], probDist) ; // prediction
			// prediction with majority class (disregarding costs)
			//max=1;
			//for (j=2 ; j<=noClasses ; j++)
			//  if (probDist[j] > probDist[max])
			//    max=j;
			oob(i,max)++ ;
		}
		// compute oob estimate
		max = 1 ;
		for (j=2 ; j<=noClasses ; j++)
			if (oob(i,j) > oob(i,max))
				max=j;
		if (oob(i, max) > 0) {
			valid ++ ;
			if (DiscData(dSet[i], 0) == max)
				oobCorrect++ ;
		}
	}
	return double(oobCorrect)/valid ;
}


//************************************************************
//
//                      buildForestTree
//                      ---------------
//
//             builds one tree of random forest
//
//************************************************************
binnode* featureTree::buildForestTree(int TrainSize, marray<int> &DTrain, int attrEstimator, const marray<double> &attrProb, int rndIdx) {

	binnode* Node = rfPrepareLeaf(TrainSize, DTrain);

	// stopping criterion
	if (rfTime2stop(Node) )
	{
		rfRevertToLeaf(Node);
		return Node ;
	}


	// for estimation of the attributes, constructs, binarization, and discretization
	marray<double> pDTrain(TrainSize, 1.0) ;
	estimation *Estimator = new estimation(this, DTrain, pDTrain, TrainSize) ;
	Estimator->eopt.selectionEstimator = attrEstimator ;

	// select/build splitting attribute/construct
	if (rfBuildConstruct(*Estimator, Node, attrProb, rndIdx) == -DBL_MAX ) {
		rfRevertToLeaf(Node);
		delete Estimator ;
		return Node;
	}
	delete Estimator ;

	marray<int> LeftTrain, RightTrain ;
	int LeftSize = 0, RightSize = 0;

	// split the data according to attribute (call by reference)
	rfSplit(DTrain, TrainSize, Node, LeftTrain, LeftSize, RightTrain, RightSize) ;

	Node->weightLeft = LeftSize ;
	if (LeftSize==0 || RightSize==0 || LeftSize < opt->minNodeWeightRF || RightSize < opt->minNodeWeightRF){
		//if (LeftSize==0 || RightSize==0) {
		rfRevertToLeaf(Node) ;
		return Node;
	}

	// recursively call building on both partitions
	Node->left  = buildForestTree(LeftSize, LeftTrain, attrEstimator, attrProb, rndIdx ) ;
	Node->right = buildForestTree(RightSize, RightTrain, attrEstimator, attrProb, rndIdx ) ;
	return  Node;
}

//************************************************************
//
//                      buildLimitedTree
//                      ---------------
//
//       builds one tree of random forest limited in size
//
//************************************************************
binnode* featureTree::rfBuildLimitedTree(int noTerminal, int TrainSize, marray<int> &DTrain,
		int attrEstimator, const marray<double> &attrProb, int rndIdx) {
	// create root node and put it into priority list

	binnode *rtNode = rfPrepareLeaf(TrainSize, DTrain) ;
	if (rfTime2stop(rtNode)) {
		rfRevertToLeaf(rtNode) ;
		return rtNode ;
	}

	marray<BinNodeRec> pq(noTerminal) ; // priority queue of the nodes
	BinNodeRec nodeEl ;
	nodeEl.value = rtNode ;

	// for estimation of the attributes
	marray<double> pDTrain(TrainSize, 1.0) ;
	estimation Estimator(this, DTrain, pDTrain, TrainSize) ;
	Estimator.eopt.selectionEstimator = attrEstimator ;
	if ((nodeEl.key = rfBuildConstruct(Estimator, rtNode, attrProb, rndIdx)) == -DBL_MAX)  {
		rfRevertToLeaf(rtNode) ;
		return rtNode ;
	}

	// add to priority queue
	pq.addPQmax(nodeEl) ;

	marray<int> LeftTrain, RightTrain ;
	int LeftSize = 0, RightSize = 0;
	binnode *Node ;

	while (pq.filled()>0  && pq.filled() < noTerminal-1) {
		// expand highest priority node
		pq.deleteMaxPQmax(nodeEl) ;
		Node = nodeEl.value ;

		// split the data according to attribute (call by reference)
		rfSplit(Node->DTrain, Node->DTrain.filled(), Node, LeftTrain, LeftSize, RightTrain, RightSize) ;
		Node->weightLeft = LeftSize ;
		if (LeftSize==0 || RightSize==0 || LeftSize < opt->minNodeWeightRF || RightSize < opt->minNodeWeightRF){
			//if (LeftSize==0 || RightSize==0) {   // is the resulting split inappropriate
			rfRevertToLeaf(Node) ;
			--noTerminal ;
		}
		else {
			Node->left = rfPrepareLeaf(LeftSize, LeftTrain) ;
			if (time2stop(Node->left))   {
				rfRevertToLeaf(Node->left) ;
				--noTerminal ;
			}
			else {
				pDTrain.create(LeftSize, 1.0) ;
				Estimator.initialize(LeftTrain,pDTrain,LeftSize) ;
				if ((nodeEl.key = rfBuildConstruct(Estimator, Node->left, attrProb, rndIdx)) == -DBL_MAX) {
					rfRevertToLeaf(Node->left) ;
					--noTerminal ;
				}
				else {
					nodeEl.value = Node->left ;
					pq.addPQmax(nodeEl) ;
				}
			}
			Node->right = rfPrepareLeaf(RightSize, RightTrain) ;
			if (time2stop(Node->right))   {
				rfRevertToLeaf(Node->right) ;
				--noTerminal ;
			}
			else {
				pDTrain.create(RightSize, 1.0) ;
				Estimator.initialize(RightTrain,pDTrain,RightSize) ;
				if ((nodeEl.key = rfBuildConstruct(Estimator, Node->right, attrProb, rndIdx)) == -DBL_MAX) {
					rfRevertToLeaf(Node->right) ;
					--noTerminal ;
				}
				else {
					nodeEl.value = Node->right ;
					pq.addPQmax(nodeEl) ;
				}
			}
		}
	}
	// turn remaining into leaves
	for (int i = 0 ; i < pq.filled() ; i++)
		rfRevertToLeaf(pq[i].value) ;

	return rtNode ;

}

//************************************************************
//
//                        rfTime2stop
//                        ---------
//
//            check the various stopping criteria
//
//************************************************************
booleanT featureTree::rfTime2stop(binnode *Node)
{
	// absolute training weight (number of examples) is too small
	if (Node->weight < opt->minNodeWeightRF)
		return mTRUE ;

	// proportion of training examples is too small
	if (Node->weight/rootWeight < opt->relMinNodeWeight)
		return mTRUE ;


	// proportion of majority class is bigger than parameter
	if (Node->Classify[Node->majorClass]/Node->weight >= opt->majorClassProportion)
		return mTRUE ;

	// minimal weight of non-majority class to allow further splitting
	if (Node->weight - Node->Classify[Node->majorClass] < opt->minNonMajorityWeight)
		return mTRUE ;


	return mFALSE ;
}


//************************************************************
//
//                      rfConsolidateTree
//                      ------------------
//
//       deletes unnecessary data in interior nodes and leaves
//
//************************************************************
void featureTree::rfConsolidateTree(binnode *branch) {
	if (branch->Identification != leaf) {
		//branch->DTrain.destroy() ;
		branch->Classify.destroy();
		rfConsolidateTree(branch->left) ;
		rfConsolidateTree(branch->right) ;
	}
}


//************************************************************
//
//                      rfBuildConstruct
//                      ----------------
//
//                  builds one node of the random forests' tree
//
//************************************************************
double featureTree::rfBuildConstruct(estimation &Estimator, binnode* Node, const marray<double> &attrProb, int rndIdx) {
	int i, j ;
	// select attributes to take into consideration and rank them (in case of inadequate rank, take next)
	marray<int>  selAttr(noAttr+1) ;
	selAttr.setFilled(noAttr+1) ;
	if (rfNoSelAttr == noAttr)
		// no selection - pure bagging, all are selected, just simplest rank
		for (i=0 ; i <= noAttr ; i ++)
			selAttr[i] = i ;
	else {
		// rank attributes according to the roulette wheel
		marray<sortRec> wheel(noAttr+1) ;
		// copy cumulative distribution
		wheel[0].key = 0.0 ;
		wheel[0].value = 0 ;
		for (i=1 ; i <= noAttr ; i++) {
			wheel[i].key = attrProb[i];
			wheel[i].value = i ;
		}
		double rndNum, gap ;
		int last = noAttr ;
		selAttr[0] = 0 ;
		for (i=1 ; i <= noAttr ; i++) {
			rndNum = rndStr.getBetween(rndIdx, 0.0, wheel[last].key) ;
			for (j=1 ; j <= last ; j++) {
				if (rndNum <= wheel[j].key )
					break;
			}
			selAttr[i] = wheel[j].value ; // selected
			// remove j-th
			gap = wheel[j].key - wheel[j-1].key ;
			for ( ; j < last ; ++j) {
				wheel[j].key = wheel[j+1].key - gap ;
				wheel[j].value = wheel[j+1].value ;
			}
			--last ;
		}
	}

	// estimate the attributes
	attributeCount bestType ;
	int bestIdx = Estimator.estimateSelected(selAttr, rfNoSelAttr, bestType) ;
	if (bestIdx == -1)
		return -DBL_MAX ;

	makeSingleAttrNode(Node, Estimator, bestIdx, bestType) ;
	if (bestType == aDISCRETE)
		return Estimator.DiscEstimation[bestIdx] ;
	else
		return Estimator.NumEstimation[bestIdx];
}


//************************************************************
//
//						rfCheck
//                      ---------------
//
//   returns probability distribution of the instance caseIdx computed by forest
//
//************************************************************
void featureTree::rfCheck(int caseIdx, marray<double> &probDist) const {
	marray<double> distr(noClasses+1) ;
	probDist.init(0.0) ;
	int i, j, max ;
	for (i=0 ; i < opt->rfNoTrees ; i++) {
		max = rfTreeCheck(forest[i].t.root, caseIdx, distr) ;

		if (opt->rfPredictClass) {
			//  max = 1 ;
			//for (j=2 ; j<=noClasses ; j++)
			//  if (distr[j] > distr[max])
			//     max=j;
			probDist[max] += 1.0 ;
			//probDist[max] += rfA[i] ;
		}
		else { // predict with distribution
			for (j=1 ; j <= noClasses ; j++)
				probDist[j] += distr[j] ;
		}
	}
	double sum = 0.0 ;
	for (j=1 ; j <= noClasses ; j++)
		sum += probDist[j] ;
	for (j=1 ; j <= noClasses ; j++)
		probDist[j] /= sum ;
}


//************************************************************
//
//						rfCheckReg
//                      ---------------
//
//   returns regularized probability distribution of the instance caseIdx computed by forest
//
//
//************************************************************
void featureTree::rfCheckReg(int caseIdx, marray<double> &probDist) const {
	marray<double> distr(noClasses+1) ;
	probDist.init(0.0) ;
	int i, max;
	double score = rfA0, maxScore = 0.0 ;
	for (i=0 ; i < opt->rfNoTrees ; i++) {
		max = rfTreeCheck(forest[i].t.root, caseIdx, distr) ;
		// for two class problems only
		maxScore += fabs(rfA[i]) ;
		if (max == 1)
			score += rfA[i] ;
		else  score -= rfA[i] ;
	}
	if (score >= 0) {
		probDist[1] = score/maxScore ;
		probDist[2] = 1.0 - probDist[1] ;
	}
	else {
		probDist[2] = -score/maxScore ;
		probDist[1] = 1.0 - probDist[2] ;
	}
}



// importance of attributes
//************************************************************
//
//                      varImportance
//                      ---------------
//
//          evaluates importance of the attributes by forest
//
//************************************************************
void featureTree::varImportance(marray<double> &varEval) {
	marray<int> discOrig(NoCases), discTemp(NoCases) ;
	marray<double> contOrig(NoCases), contTemp(NoCases) ;
	discOrig.setFilled(NoCases) ;
	discTemp.setFilled(NoCases) ;
	contOrig.setFilled(NoCases) ;
	contTemp.setFilled(NoCases) ;
	mmatrix<int> oob(NoTrainCases, noClasses+1) ;
	marray<int> maxOther(NoTrainCases) ; // dummy placeholder
	double varMargin ; // dummy placeholder
	for (int iA = 1 ; iA <= noAttr ; iA++) {
		if (AttrDesc[iA].continuous) {
			// save original values of instances and reshuffle it
			NumData.outColumn(AttrDesc[iA].tablePlace, contOrig) ;
			contTemp.copy(contOrig) ;
			contTemp.shuffle() ;
			NumData.inColumn(contTemp, AttrDesc[iA].tablePlace) ;
		}
		else {
			DiscData.outColumn(AttrDesc[iA].tablePlace, discOrig) ;
			discTemp.copy(discOrig) ;
			discTemp.shuffle() ;
			DiscData.inColumn(discTemp, AttrDesc[iA].tablePlace) ;
		}
		// compute margin
		oobEvaluate(oob) ;
		varEval[iA] = avgOobMargin - oobMargin(oob, maxOther, varMargin) ;

		if (AttrDesc[iA].continuous)
			NumData.inColumn(contOrig, AttrDesc[iA].tablePlace) ;
		else
			DiscData.inColumn(discOrig, AttrDesc[iA].tablePlace) ;

	}
}

// importance of attribute values
//************************************************************
//
//                      avImportance
//                      ------------
//
//     evaluates importance of the nominal attributes' values  by forest
//
//************************************************************
void featureTree::avImportance(marray<marray<double> > &avEval) {
	marray<int> discOrig(NoCases), discTemp(NoCases) ;
	marray<double> contOrig(NoCases), contTemp(NoCases) ;
	discOrig.setFilled(NoCases) ;
	discTemp.setFilled(NoCases) ;
	contOrig.setFilled(NoCases) ;
	contTemp.setFilled(NoCases) ;
	mmatrix<int> oob(NoTrainCases, noClasses+1) ;
	marray<double> avMarginOrig, avMarginShuffled ;
	for (int iA = 1 ; iA < noDiscrete ; iA++) {
		DiscData.outColumn(iA, discOrig) ;
		avMarginOrig.create(AttrDesc[DiscIdx[iA]].NoValues+1) ;
		oobEvaluate(oob) ;
		oobMarginAV(oob, AttrDesc[DiscIdx[iA]].NoValues, discOrig, avMarginOrig) ;
		discTemp.copy(discOrig) ;
		discTemp.shuffle() ;
		// shuffleChange(AttrDesc[DiscIdx[iA]].NoValues, discTemp) ;
		DiscData.inColumn(discTemp, iA) ;

		// evaluate changes
		oobEvaluate(oob) ;

		// compute margins
		avMarginShuffled.create(AttrDesc[DiscIdx[iA]].NoValues+1) ;
		oobMarginAV(oob, AttrDesc[DiscIdx[iA]].NoValues, discOrig, avMarginShuffled) ;
		for (int iV=0 ; iV <= AttrDesc[DiscIdx[iA]].NoValues ; iV++)
			avEval[iA][iV] = avMarginOrig[iV] - avMarginShuffled[iV] ;
		DiscData.inColumn(discOrig, iA) ;
	}
}



//************************************************************
//
//                      oobEvaluate
//                      ------------
//
//          evaluation of the oob instances
//
//************************************************************
void featureTree::oobEvaluate(mmatrix<int> &oob) const {
	marray<double> distr(noClasses+1) ;
	int iT, i, max ;
	oob.init(0) ;
	for (iT = 0 ; iT < opt->rfNoTrees ; iT++) {
		for (i=0 ; i < NoTrainCases ; i++)
			if (forest[iT].oob[i]) {
				max = rfTreeCheck(forest[iT].t.root, DTraining[i], distr) ;
				//max=1;
				//for (j=2 ; j<=noClasses ; j++)
				//   if (distr[j] > distr[max])
				//      max=j;
				oob(i, max)++ ;
			}
	}
}

//************************************************************
//
//                      oobMargin
//                      ----------
//
//    estimates margin of the training instances by the oob sample
//
//************************************************************
double featureTree::oobMargin(mmatrix<int> &oob, marray<int> &maxOther, double &varMargin) {
	int sum, correctClass, i, j ;
	double margin, sumMargin = 0.0, margin2 = 0.0 ;
	for (i=0 ; i < NoTrainCases ; i++) {
		sum = 0 ;
		correctClass = DiscData(DTraining[i], 0) ;
		if (correctClass > 1)
			maxOther[i] = 1 ;
		else maxOther[i] = 2 ;
		for (j=1 ; j <=noClasses ; j++) {
			sum += oob(i,j) ;
			if (j != correctClass && oob(i,j) > oob(i,maxOther[i]))
				maxOther[i] = j ;
		}
		if (sum > 0)
			margin = (oob(i, correctClass) - oob(i,maxOther[i])) / double(sum) ;
		else margin = 0.0 ;
		sumMargin += margin ;
		margin2 += sqr(margin) ;
	}
	double avgMargin = sumMargin / double(NoTrainCases) ;
	varMargin = (margin2 / NoTrainCases) - sqr(avgMargin) ;
	return avgMargin ;
}


//************************************************************
//
//                      oobMarginAV
//                      ------------
//
//    estimates margin of the training instances by the oob sample
//    the score is estimated separately for each original attribute's value
//
//
//************************************************************
void featureTree::oobMarginAV(mmatrix<int> &oob, int noVal, marray<int> &origVal,
		marray<double> &avMargin) {
	int sum, correctClass, i, j, maxOther ;
	double margin ;
	avMargin.init(0.0) ;
	marray<int> noCases(avMargin.len(), 0) ;
	for (i=0 ; i < NoTrainCases ; i++) {
		sum = 0 ;
		correctClass = DiscData(DTraining[i], 0) ;
		if (correctClass > 1)
			maxOther = 1 ;
		else maxOther = 2 ;
		for (j=1 ; j <=noClasses ; j++) {
			sum += oob(i,j) ;
			if (j != correctClass && oob(i,j) > oob(i,maxOther))
				maxOther = j ;
		}
		if (sum > 0)
			margin = (oob(i, correctClass) - oob(i,maxOther)) / double(sum) ;
		else margin = 0.0 ;
		if (origVal[i] != NAdisc) {
			avMargin[origVal[i]] += margin ;
			++ noCases[origVal[i]] ;
			avMargin[0] += margin ;
			++ noCases[0] ;
		}
	}
	for (int k=0 ; k <= noVal ; k++)
		avMargin[k] /= double(noCases[k]) ;
}


//************************************************************
//
//                      oobSTD
//                      -------
//
//   computes standard deviation of the forest with oob instances
//
//************************************************************
double featureTree::oobSTD(marray<int> &maxOther) {
	marray<double> distr(noClasses+1) ;
	int iT, i, max, p1,p2,all ;
	double sd = 0.0 ;
	for (iT = 0 ; iT < opt->rfNoTrees ; iT++) {
		p1 = p2 = all = 0 ;
		for (i=0 ; i < NoTrainCases ; i++)
			if (forest[iT].oob[i]) {
				all++ ;
				max = rfTreeCheck(forest[iT].t.root, DTraining[i], distr) ;
				//max=1;
				//for (j=2 ; j<=noClasses ; j++)
				//   if (distr[j] > distr[max])
				//      max=j;
				if (DiscData(DTraining[i], 0) == max)
					p1++ ;
				else if (max == maxOther[i])
					p2++ ;
			}
		sd += sqrt((p1+p2)/double(all)+sqr((p1-p2)/double(all))) ;
	}
	sd /= opt->rfNoTrees ;
	return sd ;
}

//************************************************************
//
//                      oobAccuracy
//                      ------------
//
//     computes accuracy of the forst with oob evaluation
//
//************************************************************
double featureTree::oobAccuracy(mmatrix<int> &oob) {
	int i, j,max, correct=0 ;
	for (i=0 ; i < NoTrainCases ; i++) {
		max = 1 ;
		for (j=2 ; j<=noClasses ; j++)
			if (oob(i,j) > oob(i,max))
				max=j;
		if (DiscData(DTraining[i], 0) == max)
			correct++ ;
	}
	return double(correct)/NoTrainCases ;
}



//************************************************************
//
//                      rfResultHead
//                      ----------------
//
//              prints header of random forest results
//
//************************************************************
void featureTree::rfResultHead(FILE *to) const
{
	fprintf(to,"\n%3s %5s %5s %5s   %5s %5s %6s %5s %5s %5s",
			"idx", "oobAc","oobMg", "oobRo", "accur","cost","infSc","AUC","Brier","Kappa") ;
	if (noClasses == 2)
		fprintf(to,"  %5s %5s","sens","spec") ;
	fprintf(to,"\n") ;
	printLine(to,"-",80) ;
}


//************************************************************
//
//                      rfResultLine
//                      ---------------
//
//        prints results of one random forest into a single line
//
//************************************************************
void featureTree::rfResultLine(FILE *to, int idx,
		double oobAccuracy, double oobMargin, double oobCorrelation,
		double TestAccuracy, double TestCost, double TestInf, double TestAuc,
		double sensitivity, double specificity, double brier, double kappa) const
{
	char idxStr[32] ;
	if (idx>=0) snprintf(idxStr, 32, "%3d",idx);
	else if (idx == -1) strcpy(idxStr,"avg") ;
	else if (idx == -2) strcpy(idxStr,"std") ;
	else strcpy(idxStr,"???") ;

	fprintf(to,"%3s %5.3f %5.3f %5.3f   %5.3f %5.3f %6.3f %5.3f %5.3f %5.3f",
			idxStr, oobAccuracy, oobMargin, oobCorrelation,
			TestAccuracy,  TestCost, TestInf, TestAuc, brier, kappa) ;
	if (noClasses == 2) {
		fprintf(to,"  %5.3f %5.3f",sensitivity,specificity) ;
	}
	fprintf(to, "\n") ;

}


//************************************************************
//
//                      printAttrEval
//                      -------------
//
//        prints random forests' attribute value evaluation results
//
//************************************************************
void featureTree::printAttrEval(FILE *to, marray<int> &idx, marray<marray<double> > &attrEval) {
	char idxStr[32] ;
	int i, j, iA ;
	// print header
	fprintf(to, "\n%18s", "Attribute name") ;
	for (i=0 ; i < attrEval.filled() ; i++) {
		if (idx[i]>=0) snprintf(idxStr, 32, "%3d",idx[i]);
		else if (idx[i] == -1) strcpy(idxStr,"avg") ;
		else if (idx[i] == -2) strcpy(idxStr,"std") ;
		else strcpy(idxStr,"???") ;

		fprintf(to, "  %6s",idxStr) ;
	}
	fprintf(to,"\n") ;
	for (j=0 ; j < 18 + 7*attrEval.filled() ; j++)
		fprintf(to,"-") ;
	for (iA=1 ; iA <= noAttr; iA++) {
		fprintf(to, "\n%18s",AttrDesc[iA].AttributeName) ;
		for (i=0 ; i < attrEval.filled() ; i++)
			fprintf(to,"  %6.3f",attrEval[i][iA]) ;
	}
	fprintf(to,"\n") ;
}


//**********************************************************************
//
//                         split
//                         -----
////
//    split the data acording to given feature into the left and right branch
//
//**********************************************************************
void featureTree::rfSplit(marray<int> &DTrain, int TrainSize, binnode* Node,
		marray<int> &LeftTrain, int &LeftSize,
		marray<int> &RightTrain, int &RightSize){
	double cVal ;
	int   dVal ;
	//  data for split
	marray<int> exLeft(TrainSize) ;
	marray<int> exRight(TrainSize) ;
	LeftSize = RightSize = 0 ;
	int k ;
	// split the examples
	switch  (Node->Identification)
	{
	case continuousAttribute:
		for (k=0  ; k < TrainSize ; k++)
		{
			cVal = Node->Construct.continuousValue(DiscData, NumData, DTrain[k]) ;
			if (isNAcont(cVal))
				cVal = Node->NAnumValue[Node->Construct.root->attrIdx] ;

			if (cVal <= Node->Construct.splitValue){ // || fabs(cVal - Node->Construct.splitValue) < epsilon) {
				//if (cVal <= Node->Construct.splitValue || fabs(Node->Construct.splitValue - cVal) < teps) {
				exLeft[LeftSize] = DTrain[k];
				LeftSize ++ ;
			}
			else  {
				exRight[RightSize] = DTrain[k];
				RightSize ++ ;
			}
		}
		break ;
	case discreteAttribute:
		for (k=0  ; k < TrainSize ; k++)  {
			dVal = Node->Construct.discreteValue(DiscData, NumData, DTrain[k]) ; ;
			if (dVal == NAdisc)
				dVal = Node->NAdiscValue[Node->Construct.root->attrIdx] ;
			if (Node->Construct.leftValues[dVal]) {
				exLeft[LeftSize] = DTrain[k];
				LeftSize ++ ;
			}
			else  {
				exRight[RightSize] = DTrain[k];
				RightSize ++ ;
			}
		}
		break ;
	case leaf:
		merror("featureTree::rfSplit", "node type cannot be leaf") ;
		break ;
	}
	// try not to waste space ;
	LeftTrain.create(LeftSize) ;
	for (k = 0; k < LeftSize ; k++)
		LeftTrain[k] = exLeft[k] ;

	RightTrain.create(RightSize) ;
	for (k = 0; k < RightSize ; k++)
		RightTrain[k] = exRight[k] ;
}

//************************************************************
//
//                      rfTreeCheck
//                      -----------
//
//        computes classification for single case in one tree
//
//************************************************************
int featureTree::rfTreeCheck(binnode *branch, int caseIdx, marray<double> &probDist) const
{
	switch (branch->Identification)  {
	case leaf:
		branch->Model.predict(branch, caseIdx, probDist) ;
		return branch->majorClass ;
	case continuousAttribute:
	{
		double contValue = branch->Construct.continuousValue(*dData, *nData, caseIdx) ;
		if (isNAcont(contValue))
			contValue = branch->NAnumValue[branch->Construct.root->attrIdx] ;
		if (contValue <= branch->Construct.splitValue)
			//if (contValue <= branch->Construct.splitValue || fabs(branch->Construct.splitValue-contValue) < epsilon)
			return rfTreeCheck(branch->left, caseIdx, probDist) ;
		else
			return rfTreeCheck(branch->right, caseIdx,probDist) ;
	}
	break ;
	case discreteAttribute:
	{
		int discValue = branch->Construct.discreteValue(*dData, *nData, caseIdx) ;
		if (discValue == NAdisc)
			discValue = branch->NAdiscValue[branch->Construct.root->attrIdx] ;
		if (branch->Construct.leftValues[discValue])
			return rfTreeCheck(branch->left, caseIdx, probDist) ;
		else
			return rfTreeCheck(branch->right, caseIdx,probDist) ;
	}
	break ;
	default:
		merror("featureTree::check", "invalid branch identification") ;
		return -1 ;
	}
}

//************************************************************
//
//                      rfNearCheck
//                      ----------
//
//        computes classification for a single case with a forest
//        but taking locality into account
//
//************************************************************
void featureTree::rfNearCheck(int caseIdx, marray<double> &probDist) {
	marray<IntSortRec> near(NoCases) ;
	int i, j, max, iT ;
	for (i=0 ; i < NoCases ; i++) {
		near[i].key = 0 ;
		near[i].value = i ;
	}
	// use Breiman's proximity measure:
	//   number of trees where instances are in the same leaf
	marray<double> distr(noClasses+1) ;
	for (iT=0 ; iT < opt->rfNoTrees ; iT++)
		rfFindNearInTree(forest[iT].t.root, caseIdx, near) ;
	near.setFilled(NoCases) ;
	if (dData == &DiscPredictData)  // testing case caseIdx is possibly included, so we effectively exclude it
		near[caseIdx].key = 0 ;
	// sort
	int k = Mmin(opt->rfkNearestEqual, NoTrainCases-1) ;
	near.sortKlargest(k) ;
	//near.sortKsmallest(k) ; // original bug

	// compute average margin of tree on those near cases
	// we need original data set for that

	// store pointer to prediction data and set it to training
	mmatrix<int> *tdData = dData ;
	dData = &DiscData ;
	mmatrix<double> *tnData = nData ;
	nData = &NumData ;

	marray<sortRec> treeMg(opt->rfNoTrees) ;
	for (iT=0 ; iT < opt->rfNoTrees ; iT++) {
		treeMg[iT].key = 0 ;
		treeMg[iT].value = iT ;
	}
	int treeCount, maxOther ;
	double sumTreeMg = 0.0 ;
	for (iT=0 ; iT < opt->rfNoTrees ; iT++)  {
		treeCount = 0 ;
		for (i=near.filled()-1 ; i > near.filled()-1-k ; i--)
			if (forest[iT].oobIdx.member(near[i].value)) {
				treeCount++ ;
				max = rfTreeCheck(forest[iT].t.root, near[i].value, distr) ;
				if (DiscData(near[i].value,0)==1)
					maxOther = 2 ;
				else maxOther = 1 ;
				for (j=maxOther+1 ; j <= noClasses; j++)
					if (j != DiscData(near[i].value,0) && distr[j] > distr[maxOther])
						maxOther = j ;
				treeMg[iT].key += distr[DiscData(near[i].value,0)] - distr[maxOther] ;
			}
		if (treeCount > 0)
			treeMg[iT].key /= double(treeCount) ;
		if (treeMg[iT].key > 0)
			sumTreeMg += treeMg[iT].key ;
	}
	treeMg.setFilled(opt->rfNoTrees) ;
	// restore prediction data back
	dData = tdData ;
	nData = tnData ;

	// treeMg.qsortDsc() ;
	// weight the prediction with the computed margin on near cases
	double treeWeight ;
	probDist.init(0.0) ;
	for (iT=0 ; iT < treeMg.filled() ; iT++) {
		if (treeMg[iT].key <= 0)
			continue ;
		max = rfTreeCheck(forest[treeMg[iT].value].t.root, caseIdx, distr) ;
		treeWeight = treeMg[iT].key/sumTreeMg  ;
		if (opt->rfPredictClass)
			probDist[max] += treeWeight ;
		else  // predict with distribution
			for (j=1 ; j <= noClasses ; j++)
				probDist[j] += distr[j]*treeWeight ;

	}
	double sum = 0.0 ;
	for (j=1 ; j <= noClasses ; j++)
		sum += probDist[j] ;
	for (j=1 ; j <= noClasses ; j++)
		probDist[j] /= sum ;
}


//************************************************************
//
//                      rfFindNearInTree
//                      ----------------
//
//        find the nearest cases - the ones that fall in the same leaf
//
//************************************************************
void featureTree::rfFindNearInTree(binnode *branch, int caseIdx, marray<IntSortRec> &near) const
{
	switch (branch->Identification)  {
	case leaf:
	{
		for (int i=0 ; i < branch->DTrain.len() ; i++)
			near[branch->DTrain[i]].key++ ;
		return ;
	}
	case continuousAttribute:
	{
		double contValue = branch->Construct.continuousValue(*dData, *nData, caseIdx) ;
		if (isNAcont(contValue))
			contValue = branch->NAnumValue[branch->Construct.root->attrIdx] ;
		if (contValue <= branch->Construct.splitValue)
			//if (contValue <= branch->Construct.splitValue || fabs(branch->Construct.splitValue - contValue) < epsilon)
			rfFindNearInTree(branch->left, caseIdx, near) ;
		else
			rfFindNearInTree(branch->right, caseIdx,near) ;
	}
	return ;
	case discreteAttribute:
	{
		int discValue = branch->Construct.discreteValue(*dData, *nData, caseIdx) ;
		if (discValue == NAdisc)
			discValue = branch->NAdiscValue[branch->Construct.root->attrIdx] ;
		if (branch->Construct.leftValues[discValue])
			rfFindNearInTree(branch->left, caseIdx, near) ;
		else
			rfFindNearInTree(branch->right, caseIdx, near) ;
	}
	return ;
	default:
		merror("featureTree::rfFindNearInTree", "invalid branch identification") ;
		return  ;
	}
}

void featureTree::rfRevertToLeaf(binnode *Node) {
	Node->Construct.destroy() ;
	Node->NAnumValue.destroy() ;
	Node->NAdiscValue.destroy() ;
	Node->Identification = leaf ;
}

binnode* featureTree::rfPrepareLeaf(int TrainSize, marray<int> &DTrain) {
	binnode* Node = new binnode ;
	Node->weight = TrainSize ;
	Node->Classify.create(noClasses+1, 0.0) ;
	int i, j ;
	// compute class distribution and weight of a node
	for (i=0 ; i < TrainSize ; i++)
		Node->Classify[DiscData(DTrain[i],0)] += 1.0 ;
	Node->majorClass = 1 ;
	for (j=2 ; j <= noClasses ; j++)
		if (Node->Classify[j] > Node->Classify[Node->majorClass])
			Node->majorClass = j ;

	// create leaf, label it properly
	Node->Identification = leaf ;
	Node->DTrain.copy(DTrain) ;
	Node->DTrain.setFilled(TrainSize) ;
	Node->Model.createMajority(Node->majorClass) ;
	Node->Model.gFT = this ;
	Node->left = Node->right = 0 ;

	// prepare things for interior node

	// compute most probable values used instead of missing values
	Node->NAdiscValue.create(noDiscrete) ;
	marray<marray<int> > NAdiscCounter(noDiscrete) ;

	for (i=0 ; i < noDiscrete ; i++)
		NAdiscCounter[i].create(AttrDesc[DiscIdx[i]].NoValues +1, 0) ;

	for (i=0; i < noDiscrete ; i++)
		for (j=0 ; j < TrainSize ; j++)
			NAdiscCounter[i][DiscData(DTrain[j],i)] ++ ;

	int max ;
	for (i=0 ; i < noDiscrete ; i++)   {
		max = 1 ;
		for (j=2; j <= AttrDesc[DiscIdx[i]].NoValues ;  j++)
			if (NAdiscCounter[i][j] > NAdiscCounter[i][max])
				max = j ;
		Node->NAdiscValue[i] = max ;
	}

	//  numeric attribute missing values - use the average attribute value instead
	Node->NAnumValue.create(noNumeric) ;
	marray<int> NAcontWeight(noNumeric,0) ;
	marray<double> NAcontSum(noNumeric,0.0) ;

	for (i=0; i < noNumeric ; i++)   {
		for (j=0 ; j < TrainSize ; j++)
			if ( ! isNAcont(NumData(j,i)))   {
				NAcontWeight[i] ++ ;
				NAcontSum[i] += NumData(j,i) ;
			}
		if (NAcontWeight[i] > 0)
			Node->NAnumValue[i] =  NAcontSum[i]/NAcontWeight[i] ;
		else
			Node->NAnumValue[i] = (maxValue[i] + minValue[i]) / 2.0 ;
	}
	return Node ;
}

//************************************************************
//
//                           writeRF
//                           ---------
//
//                     writes forest to given file
//
//************************************************************
int featureTree::writeRF(const char* TreeFileName) const
{
	FILE *fout ;
	if ((fout=fopen(TreeFileName,"w"))==NULL)
	{
		merror("Cannot create output random forest file", TreeFileName);
		return 0;
	}
	int i ;
	if (forest.defined()) {
		fprintf(fout,"list(modelType=\"randomForest\", rfNoTrees=%d, noClasses=%d, noAttr=%d, noNumeric=%d, noDiscrete=%d, discNoValues=c(",
				opt->rfNoTrees, noClasses, noAttr, noNumeric, noDiscrete-1) ;
		booleanT first = mTRUE ;
		for (i=1 ; i < noDiscrete; i++)
			if (first) {
				fprintf(fout,"%d",AttrDesc[DiscIdx[i]].NoValues) ;
				first = mFALSE ;
			}
			else
				fprintf(fout,",%d",AttrDesc[DiscIdx[i]].NoValues) ;
		fprintf(fout,"),\n   trees=list(\n") ;

		for (i=0 ; i < opt->rfNoTrees; i++)
			rfWriteTree(fout,5,i) ;

		fprintf(fout,")\n)\n");
	}

	if (ferror(fout))
	{
		merror("Error at writing random forest to file ",TreeFileName) ;
		fclose(fout) ;
		return 0 ;
	}

	fclose(fout) ;
	return 1 ;
}



int featureTree::getSize(binnode *branch) const {
	if (branch->Identification==leaf)
		return 1;
	else
		return (getSize(branch->left) + getSize(branch->right));
}


int featureTree::getSumOverLeaves(binnode *branch, int depth) const {
	if (branch->Identification==leaf)
		return depth;
	else
		return (getSumOverLeaves(branch->left, depth+1) + getSumOverLeaves(branch->right, depth+1));
}


void featureTree::rfWriteTree(FILE* fout, int indent, int treeIdx) const {
	if (forest[treeIdx].t.root ==0)
		merror("featureTree::rfWriteTree","nonexisting random forest tree") ;
	else{
		if (treeIdx >0)
			fprintf(fout,",\n");
		fprintf(fout,"%*s",indent," ");
		fprintf(fout,"list(treeIdx=%d, structure=list(",treeIdx) ;
		rfWriteSubTree(fout,indent+2,forest[treeIdx].t.root) ;
		fprintf(fout,"))") ;
	}
}


void featureTree::rfWriteSubTree(FILE* fout, int indent, binnode *branch) const {
	//fprintf(fout,"%*s",indent," ") ;
	char NAdirection[10]  ;
	fprintf(fout, "nodeId=") ;

	switch (branch->Identification)  {
	case leaf: {
		fprintf(fout, "\"leaf\", classify=c(") ;
		booleanT first = mTRUE ;
		for (int i=1 ; i <= noClasses ; i++)
			if (first) {
				fprintf(fout, "%g",branch->Classify[i]) ; // /branch->weight ) ;
				first = mFALSE ;
			}
			else
				fprintf(fout, ",%g", branch->Classify[i]) ; // /branch->weight) ;
		fprintf(fout, "), weight=%g", branch->weight) ;
	}
	break ;
	case continuousAttribute:
		if (branch->NAnumValue[branch->Construct.root->attrIdx] <= branch->Construct.splitValue)
			// || fabs(branch->Construct.splitValue - branch->NAnumValue[branch->Construct.root->attrIdx])< epsilon)
			strcpy(NAdirection, "left");
		else strcpy(NAdirection, "right") ;
		fprintf(fout, "\"numericSplit\", attr=%d, split=%g, NAdefault=\"%s\",\n", branch->Construct.root->attrIdx+1, branch->Construct.splitValue, NAdirection) ;
		//fprintf(fout, "N%d<=%g,NA=%c (%g)\n", branch->Construct.root->attrIdx+1, branch->Construct.splitValue, NAdirection, branch->NAnumValue[branch->Construct.root->attrIdx] ) ;
		fprintf(fout,"%*s",indent+5," ") ;
		fprintf(fout,"leftTree=list(");
		rfWriteSubTree(fout, indent+5, branch->left) ;
		fprintf(fout,"),\n%*srightTree=list(",indent+5," ");
		rfWriteSubTree(fout, indent+5, branch->right) ;
		fprintf(fout,")");
		break ;
	case discreteAttribute: {
		fprintf(fout, "\"discreteSplit\", attr=%d, leftValues=c(", branch->Construct.root->attrIdx) ;
		if (branch->Construct.leftValues[branch->NAdiscValue[branch->Construct.root->attrIdx]])
			strcpy(NAdirection, "left");
		else strcpy(NAdirection, "right") ;
		booleanT first = mTRUE ;
		for (int i=1 ; i <= AttrDesc[DiscIdx[branch->Construct.root->attrIdx]].NoValues; i++)
			if (branch->Construct.leftValues[i]) {
				if (first){
					fprintf(fout, "%d", i) ;
					first = mFALSE ;
				}
				else
					fprintf(fout, ",%d", i) ;
			}
		fprintf(fout, "), NAdefault=\"%s\",\n", NAdirection) ;
		fprintf(fout,"%*s",indent+5," ") ;
		fprintf(fout,"leftTree=list(");
		rfWriteSubTree(fout, indent+2, branch->left) ;
		fprintf(fout,"),\n%*srightTree=list(",indent+5, " ");
		rfWriteSubTree(fout, indent+2, branch->right) ;
		fprintf(fout,")");
	}
	break ;
	}
}




booleanT featureTree::readForest(char *fileName) {
	destroy() ;
	FILE* fin ;
	if ((fin=fopen(fileName,"r"))==NULL)
	{
		merror("Cannot open random forest file", fileName);
		return mFALSE;
	}
	int i ;
	char errBuf[MaxNameLen] ;
	int retInt ;
	retInt = fscanf(fin," list( modelType = \"randomForest\" , rfNoTrees = %d , noClasses = %d , noAttr = %d , noNumeric = %d , noDiscrete = %d , discNoValues = c(", &opt->rfNoTrees, &noClasses,&noAttr,&noNumeric,&noDiscrete) ;
	if (retInt != 5) {
		merror("There were errors while reading random forest (1), file ", fileName);
		return mFALSE ;
	}
	++noDiscrete ;
	marray<int> discNoValues(noDiscrete, 0) ;
	discNoValues[0] = noClasses ;
	booleanT first = mTRUE ;
	for (i=1 ; i < noDiscrete ;i++)
		if (first) {
			retInt = fscanf(fin, "%d" ,&(discNoValues[i])) ;
			if (retInt != 1) {
				merror("There were errors while reading random forest (2), file ", fileName);
				return mFALSE ;
			}

			first = mFALSE ;
		}
		else {
			retInt = fscanf(fin, " , %d",&(discNoValues[i])) ;
			if (retInt != 2) {
				merror("There were errors while reading random forest (3), file ", fileName);
				return mFALSE ;
			}
		}

	retInt = fscanf(fin, " ) , trees = list(") ;
	marray<char * > discAttrNames(noDiscrete, 0), discValNames(noDiscrete, 0), numAttrNames(noNumeric,0) ;

	isRegression=mFALSE ;
	dscFromR(noDiscrete, discNoValues, noNumeric, discAttrNames, discValNames, numAttrNames) ;
	forest.create(opt->rfNoTrees) ;
	opt->rfAttrEvaluate = mFALSE ; // there is no in and out-of-bag set
	opt->rfkNearestEqual = 0 ; // instances are not stored

	for (i=0 ; i < opt->rfNoTrees ; i++) {
		forest[i].t.root = readTree(fin, i) ;
		snprintf(errBuf, MaxNameLen, " %d", i) ;
		if (forest[i].t.root == 0) {
			merror("There were errors while reading tree with index", errBuf);
			forest.destroy() ;
			return mFALSE ;
		}
		if (i < opt->rfNoTrees-1)
			retInt = fscanf(fin," , ") ;
	}
	retInt = fscanf(fin," ) ") ;

	if (ferror(fin)) {
		merror("There were errors while reading random forest (4) from file ", fileName);
		forest.destroy() ;
		fclose(fin) ;
		return mFALSE ;
	}
	fclose(fin) ;
	return mTRUE ;
}

binnode* featureTree::readTree(FILE* fin, int treeIdx) {
	int idx ;
	int retInt ;
	retInt = fscanf(fin," list( treeIdx = %d, structure = list(",&idx) ;
	if (retInt != 1 || idx != treeIdx) {
		merror("rfInterface::readTree", "invalid tree index") ;
		return 0 ;
	}
	binnode *rootNode = readNode(fin) ;
	retInt = fscanf(fin," ) )") ;
	return rootNode ;
}

binnode* featureTree::readNode(FILE* fin) {
	char endChar ;
	char idBuf[MaxNameLen], buf[MaxNameLen];
	int i, leftValue, attrIdx ;
	booleanT first ;
	double splitValue ;
	int retInt ;
	binnode *current = new binnode() ;
	retInt = fscanf(fin," nodeId = \"") ;
	fscanfUntil(fin,idBuf, '"', MaxNameLen) ;
	if (strcmp(idBuf, "leaf") == 0) {
		current->Identification = leaf ;
		current->Classify.create(noClasses+1, 0.0) ;
		current->majorClass = 1 ;
		first = mTRUE ;
		for (i=1; i <= noClasses ; i++) {
			if (first) {
				retInt = fscanf(fin," , classify = c(%lg", &(current->Classify[i])) ;
				if (retInt != 1) {
					merror("There were errors while reading random forest file (1)", "");
					delete current ;
					return 0 ;
				}
				first = mFALSE ;
			}
			else {
				retInt = fscanf(fin," , %lg", &(current->Classify[i])) ;
				if (retInt != 1) {
					merror("There were errors while reading random forest file (2)", "");
					delete current ;
					return 0 ;
				}
			}

			if (current->Classify[i] > current->Classify[current->majorClass])
				current->majorClass = i ;
		}

		// create leaf, label it properly
		current->Model.createMajority(current->majorClass) ;
		current->Model.gFT = this ;

		retInt = fscanf(fin," ) , weight = %lg", &(current->weight)) ;
		if (retInt != 1) {
			merror("There were errors while reading random forest file (3)", "") ;
			delete current ;
			return 0 ;
		}

		current->left = current->right = 0 ;
		return current ;
	}
	else if (strcmp(idBuf,"discreteSplit")==0) {
		current->Identification = discreteAttribute ;
		retInt = fscanf(fin, " , attr = %d , leftValues = c(",&attrIdx) ;
		if (retInt != 1) {
			merror("There were errors while reading random forest file (4)", "");
			delete current ;
			return 0 ;
		}

		current->Construct.createSingle(attrIdx, aDISCRETE ) ;
		current->Construct.gFT = this ;
		current->Construct.noValues = AttrDesc[DiscIdx[attrIdx]].NoValues ;
		current->Construct.leftValues.create(AttrDesc[DiscIdx[attrIdx]].NoValues+1, mFALSE) ;
		do {
			retInt = fscanf(fin," %d", &leftValue) ;
			if (retInt != 1) {
				merror("There were errors while reading random forest file (5)", "");
				delete current ;
				return 0 ;
			}
			current->Construct.leftValues[leftValue] = mTRUE ;
			endChar = getc(fin) ;
		} while (endChar != ')') ;
		current->NAdiscValue.create(noDiscrete, 0);
		retInt = fscanf(fin, " , NAdefault = \"") ;
		fscanfUntil(fin,buf, '"', MaxNameLen) ;
		if (strcmp(buf,"left")==0)
			current->NAdiscValue[attrIdx] = leftValue;
		else if (strcmp(buf,"right")==0) {
			for (int v=1 ; v <= AttrDesc[DiscIdx[attrIdx]].NoValues; v++)
				if (!current->Construct.leftValues[v]) {
					current->NAdiscValue[attrIdx] = v ;
					break ;
				}
		}
		else {
			merror("featureTree::readNode","invalid NAdefault indicator in tree structure") ;
			delete current ;
			return 0 ;
		}
		retInt = fscanf(fin," , leftTree=list(") ;
		current->left = readNode(fin) ;
		retInt = fscanf(fin," ) , rightTree=list(") ;
		current->right = readNode(fin) ;
		retInt = fscanf(fin," )") ;
		return current ;
	}
	else if (strcmp(idBuf,"numericSplit")==0) {
		current->Identification = continuousAttribute ;
		retInt = fscanf(fin, " , attr = %d , split = %lf , NAdefault = \"", &attrIdx, &splitValue) ;
		if (retInt != 2) {
			merror("There were errors while reading random forest file (6)", "");
			delete current ;
			return 0 ;
		}

		fscanfUntil(fin,buf, '"', MaxNameLen) ;
		--attrIdx ;
		current->Construct.createSingle(attrIdx, aCONTINUOUS ) ;
		current->Construct.gFT = this ;
		current->Construct.splitValue = splitValue ;
		current->NAnumValue.create(noNumeric, NAcont) ;

		if (strcmp(buf,"left")==0)
			current->NAnumValue[attrIdx] = splitValue -1 ;
		else if (strcmp(buf,"right")==0)
			current->NAnumValue[attrIdx] = splitValue +1 ;
		else {
			merror("featureTree::readNode","invalid NAdefault indicator in tree structure") ;
			delete current ;
			return 0 ;
		}
		retInt = fscanf(fin," , leftTree = list(") ;
		current->left = readNode(fin) ;
		retInt = fscanf(fin," ) , rightTree = list(") ;
		current->right = readNode(fin) ;
		retInt = fscanf(fin," )") ;
		return current ;
	}
	else {
		merror("featureTree::readNode","invalid attribute type in tree structure") ;
		delete current ;
		return 0 ;
	}
}


/*
int featureTree::tempSaveForest(char *fName) {
   learnRF = mTRUE ;
   setDataSplit(opt->splitIdx) ;
   randSeed(opt->rfRndSeed) ;

   FILE *fout ;
   if ((fout=fopen(fName,"w"))==NULL)
   {
      merror("Cannot create output random forest file", fName);
      return 0;
   }
     fprintf(fout,"randomForest(rfNoTrees=%d, noClasses=%d, noAttr=%d, noNumeric=%d, noDiscrete=%d, discNoValues=[",
		 opt->rfNoTrees, noClasses, noAttr, noNumeric, noDiscrete-1) ;
	 booleanT first = mTRUE ;
	 for (int i=1 ; i < noDiscrete; i++)
		 if (first) {
			 fprintf(fout,"%d",AttrDesc[DiscIdx[i]].NoValues) ;
			 first = mFALSE ;
		 }
		 else
			 fprintf(fout,",%d",AttrDesc[DiscIdx[i]].NoValues) ;
	 fprintf(fout,"]\n") ;

   forest.create(opt->rfNoTrees) ;

   int trainSize = NoTrainCases, it, i ;
   if (opt->rfNoSelAttr==0)
	    rfNoSelAttr = Mmax(1, intRound(sqrt(double(noAttr)))) ;
   else if (opt->rfNoSelAttr==-1)
	    rfNoSelAttr = Mmax(1, 1+int(log2(double(noAttr)))) ;
   else if (opt->rfNoSelAttr==-2 || opt->rfNoSelAttr >= noAttr)
      rfNoSelAttr = noAttr ;
   else rfNoSelAttr = opt->rfNoSelAttr ;

   rootWeight = trainSize ;
   marray<double> weight(trainSize, 1.0), wProb(noAttr+1), eProb(noAttr+1) ;

   // prepare weighs for no weighting (set equal weights)
   eProb[0] = 0.0 ;
   for (i=1 ; i <= noAttr; i++)
      eProb[i] = double(i)/ noAttr ;
   eProb[noAttr] = 1.0 ;

   if (opt->rfPropWeightedTrees > 0) {
		// estimate attributes
		attributeCount attrType ;
		double minEval = DBL_MAX  ;
		estimation Estimator(this, DTraining, weight, trainSize) ;
		Estimator.estimate(estReliefFexpRank, 0, noNumeric, 1, noDiscrete, attrType) ;
		for (i=1 ; i <= noAttr; i++) {
			if (AttrDesc[i].continuous)
				wProb[i] = Estimator.NumEstimation[AttrDesc[i].tablePlace] ;
			else
				wProb[i] =  Estimator.DiscEstimation[AttrDesc[i].tablePlace] ;
			if (wProb[i] < minEval)
				minEval = wProb[i] ;
		}
		// prepare cumulative probability distribution
		double eSum = 0.0 ;
		if (minEval < 0) {
			double negReduction = 10.0 ;
			minEval = -minEval/negReduction + 1e-6 ; ;
			for (i=1 ; i <= noAttr; i++) {
				if (wProb[i] < 0)
					wProb[i] = wProb[i]/negReduction + minEval;
				else
					wProb[i] += minEval ;
			    eSum += wProb[i] ;
			}
		}
		else
			for (i=1 ; i <= noAttr; i++)
     			eSum += wProb[i] ;
		wProb[0] = 0.0 ;
		for (i=1 ; i <= noAttr; i++)
			wProb[i] = wProb[i-1] + wProb[i]/eSum ;
		wProb[noAttr] = 1.0 ;
   }
   else wProb.copy(eProb) ;

   mmatrix<int> oobEval(trainSize, noClasses+1, 0) ;
   // build forest
   // printf("\niter accuracy\n") ;
   int selEst = opt->selectionEstimator ;
   int noAvailableEst = 5 ;
   marray<int> availEst(noAvailableEst) ;
   availEst[0] = estReliefFexpRank ;
   availEst[1] = estGainRatio ;
   availEst[2] = estMDL ;
   availEst[3] = estGini ;
   availEst[4] = estMyopicReliefF ;
   //availEst[5] = estReliefFexpRank ;
   for (it = 0 ; it < opt->rfNoTrees ; it++) {
	   // prepare training data
	   if (opt->rfSampleProp==0)
		   bootstrapSample(trainSize, DTraining, forest[it].ib, forest[it].oob, forest[it].oobIdx) ;
	   else
           randomSample(trainSize, opt->rfSampleProp, DTraining, forest[it].ib, forest[it].oob, forest[it].oobIdx) ;

	   if (opt->rfMultipleEst)
           opt->selectionEstimator = availEst[it % noAvailableEst] ;
	   if ( it/double(opt->rfNoTrees) < opt->rfPropWeightedTrees) {
		   if (opt->rfNoTerminals ==0)
	         forest[it].t.root = buildForestTree(trainSize, forest[it].ib, opt->selectionEstimator, wProb) ;
		   else
			 forest[it].t.root = rfBuildLimitedTree(opt->rfNoTerminals, trainSize, forest[it].ib, opt->selectionEstimator, wProb) ;
	   }
	   else {
   	      if (opt->rfNoTerminals ==0)
  	         forest[it].t.root = buildForestTree(trainSize, forest[it].ib, opt->selectionEstimator, eProb) ;
		   else
			 forest[it].t.root = rfBuildLimitedTree(opt->rfNoTerminals, forest[it].ib.len(), forest[it].ib, opt->selectionEstimator, eProb) ;
	   }
       rfWriteTree(fout,2,it) ;
       forest[it].t.destroy() ;

       // oobEstimate = oobEvaluate(forest[it].t.root, DTraining, forest[it].oob, oobEval) ;
       //printf("%03d %.3f\n",it, oobEstimate) ;
   }
   fprintf(fout,")\n") ;

   opt->selectionEstimator = selEst ;
   fclose(fout) ;
   return 1 ;
}
 */
