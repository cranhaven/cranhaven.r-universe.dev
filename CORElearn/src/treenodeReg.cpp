#include <cstdlib>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "regtree.h"
#include "utils.h"
#include "estimatorReg.h"
#include "constrctReg.h"

using namespace std ;

// ************************************************************
//
//                singleAttributeModel
//                --------------------
//
//    builds constructReg consisting of single attribute
//
// ************************************************************
booleanT regressionTree::singleAttributeModel(marray<int> &DTrain, marray<double> &pDTrain, 
		int TrainSize, binnodeReg* Node)
{
	// estimate the attributes
	estimationReg Estimator(this, DTrain, pDTrain, TrainSize) ;
	attributeCount bestType ;
	int i, j ;
	int NoDiscConstructs = 0, NoContConstructs = 0;

	if (CachedConstructs.filled())
	{
		for (i=0; i < CachedConstructs.filled() ; i++) {
			if (CachedConstructs[i].countType == aDISCRETE)
				NoDiscConstructs ++ ;
			else
				NoContConstructs ++ ;
		}
		Estimator.adjustTables(noNumeric + NoContConstructs,
				noDiscrete + NoDiscConstructs) ;

		int discCount = 0, contCount = 0 ;
		for (i=0; i < CachedConstructs.filled() ; i++)
		{
			if (CachedConstructs[i].countType == aDISCRETE)
			{
				for (j=0 ; j < Estimator.TrainSize ; j++)
					Estimator.DiscValues.Set(j, noDiscrete+discCount,
							CachedConstructs[i].discreteValue(Estimator.DiscValues,Estimator.NumValues, j)) ;
				Estimator.prepareDiscAttr(noDiscrete + discCount, 2) ;
				discCount ++ ;
			}
			else
				if (CachedConstructs[i].countType == aCONTINUOUS)
				{   // continuous constructReg
					for (j=0 ; j < Estimator.TrainSize ; j++)
						Estimator.NumValues.Set(j, noNumeric+contCount,
								CachedConstructs[i].continuousValue(Estimator.DiscValues,Estimator.NumValues, j)) ;
					Estimator.prepareContAttr(noNumeric + contCount) ;
					contCount ++ ;
				}
				else
					merror("regressionTree::singleAttributeModel", "invalid cached constructReg count detected") ;
		}
	}

	int bestIdx = Estimator.estimate(opt->selectionEstimatorReg, 1, noNumeric+NoContConstructs, 0, noDiscrete+NoDiscConstructs, bestType) ;

	// copy primary estimationRegs
	for (int attrIdx = 1 ; attrIdx <= noAttr; attrIdx++)
		if (AttrDesc[attrIdx].continuous)
			primaryEstimate[attrIdx] = Estimator.NumEstimation[AttrDesc[attrIdx].tablePlace] ;
		else
			primaryEstimate[attrIdx] =  Estimator.DiscEstimation[AttrDesc[attrIdx].tablePlace] ;

	if (bestIdx == -1)
		return mFALSE ;
	double bestEstimate  ;
	if (bestType == aCONTINUOUS)
		bestEstimate = Estimator.NumEstimation[bestIdx] ;
	else
		bestEstimate = Estimator.DiscEstimation[bestIdx] ;

	if ( (opt->selectionEstimatorReg == estRReliefFexpRank || opt->selectionEstimatorReg == estRReliefFkEqual ||
			opt->selectionEstimatorReg == estRReliefFbestK || opt->selectionEstimatorReg == estRReliefFdistance ||
			opt->selectionEstimatorReg == estRReliefFsqrDistance) && bestEstimate < opt->minReliefEstimate)
		return mFALSE ;


	if ( (bestType == aCONTINUOUS && bestIdx < noNumeric) ||
			(bestType == aDISCRETE && bestIdx < noDiscrete) )
		makeSingleAttrNode(Node, Estimator, bestIdx, bestType) ;
	else
	{
		// cached constructReg was selected - find out which one
		int idx, selectedConstruct = -1 ;
		if (bestType == aCONTINUOUS)
			idx = bestIdx - noNumeric ;
		else
			idx = bestIdx - noDiscrete ;
		for (i=0 ; i < CachedConstructs.filled() ; i++)
		{
			if (CachedConstructs[i].countType  == bestType)
			{
				if (idx == 0)
				{
					selectedConstruct = i ;
					break ;
				}
				idx -- ;
			}
		}
#if defined(DEBUG)
    		  if (selectedConstruct == -1)
    			  merror("regressionTree::singleAttributeModel", "the selected constructReg index is invalid") ;
#endif
    		  makeConstructNode(Node, Estimator, CachedConstructs[selectedConstruct]) ;
	}
	return mTRUE ;
}



// ************************************************************
//
//                makeSingleAttrNode
//                ------------------
//
//    prepares single attribute to use in the node
//
// ************************************************************
void regressionTree::makeSingleAttrNode(binnodeReg* Node, estimationReg &Estimator, int bestIdx, attributeCount bestType)
{
	Node->Construct.createSingle(bestIdx, bestType) ;
	Node->Construct.gRT = this ;

	if (bestType == aCONTINUOUS)
	{
		Node->Identification = continuousAttribute ;
		Node->Construct.splitValue = bestSplit(Node->Construct, Estimator) ;
	}
	else
	{
		Node->Identification = discreteAttribute ;
		Node->Construct.leftValues.create(AttrDesc[DiscIdx[bestIdx]].NoValues+1,mFALSE) ;
		Node->Construct.noValues = AttrDesc[DiscIdx[bestIdx]].NoValues ;
		binarize(Node->Construct, Estimator) ;
	}
}


// ************************************************************
//
//                makeConstructNode
//                ------------------
//
//    prepares constructReg for use in the node
//
// ************************************************************
void regressionTree::makeConstructNode(binnodeReg* Node, estimationReg &Estimator, constructReg &Construct)
{
	Node->Construct = Construct ;
	Node->Construct.gRT = this ;

	if (Construct.countType == aCONTINUOUS)
	{
		Node->Identification = continuousAttribute ;
		Node->Construct.splitValue = bestSplit(Construct, Estimator) ;
	}
	else
	{
		Node->Identification = discreteAttribute ;
		Node->Construct.leftValues.create(3, mFALSE) ;
		Node->Construct.noValues = 2 ;
		Node->Construct.leftValues[1] = mTRUE ;
	}
}




// ************************************************************
//
//                conjunct
//                --------
//
//    builds constructReg consisting of conjunction of attribute values
//
// ************************************************************
double regressionTree::conjunct(estimationReg &Estimator, constructReg &bestConjunct, 
		marray<constructReg> &stepCache, marray<double> &stepCacheEst)
{
	// original attributes are already estimated

	// prepare attribute values
	marray<constructReg> Candidates(noAttr*10) ; // estimationReg of the size
	int bestConjunctIdx = prepareAttrValues(Estimator, Candidates) ;

	if (Candidates.filled() == 0) // there are no candidates
		return -DBL_MAX ;

	attributeCount bestConstructType = aDISCRETE ;
	marray<constructReg> ContConstructs(0) ;
	// now start beam search with possibly new estimator
	if (opt->selectionEstimatorReg != opt->constructionEstimatorReg)
	{
		bestConjunctIdx=Estimator.estimateConstruct(opt->constructionEstimatorReg, 1, 1,
				noDiscrete, noDiscrete+Candidates.filled(), bestConstructType,
				Candidates, ContConstructs) ;
	}
	if (bestConjunctIdx == -1)
		return -DBL_MAX ;

	double bestConjunctEst = Estimator.DiscEstimation[bestConjunctIdx] ;
	bestConjunct = Candidates[bestConjunctIdx - noDiscrete] ;

	// select the best for the beam
	marray<constructReg> Beam(opt->beamSize) ;
	selectBeam(Beam, stepCache, stepCacheEst, Candidates, Estimator, aDISCRETE) ;
	stepCache.setFilled(0) ;

	// form and estimate the conjunctions
	int j, idx, iBeam, iCandidate ;
	Estimator.adjustTables(0, noDiscrete + Beam.len() * Candidates.filled() ) ;
	marray<constructReg> Working(Beam.len() * Candidates.filled() ) ;
	for (int size=1 ; size < opt->maxConstructSize ; size++)
	{
		idx = 0 ;
		// create the new conjunctions
		for (iBeam=0 ; iBeam < Beam.filled() ; iBeam++)
			for (iCandidate=0 ; iCandidate < Candidates.filled() ; iCandidate++)
			{
				if (Beam[iBeam].containsAttribute(Candidates[iCandidate]))
					continue ;
				Working[idx].Conjoin(Beam[iBeam], Candidates[iCandidate]) ;
				for (j=0 ; j < Estimator.TrainSize ; j++)
					Estimator.DiscValues.Set(j, noDiscrete+idx,
							Working[idx].discreteValue(Estimator.DiscValues,Estimator.NumValues, j)) ;
				Estimator.prepareDiscAttr(noDiscrete + idx, 2) ;
				idx ++ ;
			}

		Working.setFilled(idx) ;
		if (Working.filled() == 0) // there are no new constructs
			break ;
		bestConjunctIdx = Estimator.estimateConstruct(opt->constructionEstimatorReg, 1, 1,
				noDiscrete, noDiscrete + Working.filled(),
				bestConstructType, Working, ContConstructs) ;
		if (bestConjunctIdx == -1)
			break ;
		// check the best
		if (bestConjunctEst < Estimator.DiscEstimation[bestConjunctIdx])
		{
			bestConjunctEst = Estimator.DiscEstimation[bestConjunctIdx] ;
			bestConjunct = Working[bestConjunctIdx - noDiscrete] ;
		}

		// select the new beam
		selectBeam(Beam, stepCache, stepCacheEst, Working, Estimator, aDISCRETE) ;
	}

	return bestConjunctEst ;
}


// ************************************************************
//
//                selectBeam
//                ----------
//
//    select beam for beam search in constructRegive induction
//
// ************************************************************
void regressionTree::selectBeam(marray<constructReg> &Beam, marray<constructReg> &stepCache, 
		marray<double> &stepCacheEst, marray<constructReg> &Candidates,
		estimationReg &Estimator, attributeCount aCount)
{
	marray<sortRec> SortArray(Candidates.filled()) ;
	marray<double> BeamEst(Beam.len()) ;
	int i ;
	switch (aCount)
	{
	case aDISCRETE:
		for (i=0 ; i < Candidates.filled() ; i++)
		{
			SortArray[i].key = Estimator.DiscEstimation[noDiscrete+i] ;
			SortArray[i].value = i ;
		}
		break ;
	case aCONTINUOUS:
		for (i=0 ; i < Candidates.filled() ; i++)
		{
			SortArray[i].key = Estimator.NumEstimation[noNumeric+i] ;
			SortArray[i].value = i ;
		}
		break ;
	default:
		merror("regressionTree::selectBeam", "invalid attribute count detected") ;
	}
	SortArray.setFilled(Candidates.filled()) ;

	// establish heap initially
	for (i = SortArray.filled()/2 ; i >= 1 ; i--)
		SortArray.pushdownAsc(i, SortArray.filled()) ;

	// put first into the beam without checking
	Beam[0] = Candidates[SortArray[0].value] ;
	BeamEst[0] = SortArray[0].key ;
	mswap(SortArray[SortArray.filled()-1], SortArray[0]) ;
	SortArray.pushdownAsc(1, SortArray.filled()-1) ;

	int pos, beamIdx = 1 ;
	booleanT newConstruct ;

	// do as much heap sort as neccessary
	i = SortArray.filled()-1 ;
	while (i >= 1 && beamIdx < Beam.len())
	{
		i-- ;
		mswap(SortArray[i], SortArray[0]) ;
		SortArray.pushdownAsc(1, i) ;

		// check this constructReg if it is equal to any of
		// tose with the same estimate
		pos = 1 ;
		newConstruct = mTRUE ;
		while (i+pos < SortArray.filled() &&
				SortArray[i].key + epsilon >= SortArray[i+pos].key )
		{
			// constructs have the same estimate, are they really identical
			if (Candidates[SortArray[i].value] == Candidates[SortArray[i+pos].value])
			{
				newConstruct = mFALSE ;
				break ;
			}
			pos ++ ;
		}
		if (newConstruct)
		{
			// copy it to the beam
			Beam[beamIdx] = Candidates[SortArray[i].value] ;
			BeamEst[beamIdx] = SortArray[i].key ;
			beamIdx ++ ;
		}
	}
	Beam.setFilled(beamIdx ) ;
	BeamEst.setFilled(beamIdx ) ;

	// fill the step cache
	int cacheIdx = 0;
	beamIdx = 0 ;
	while (cacheIdx < stepCache.len() && beamIdx < Beam.filled() )
	{
		while (cacheIdx < stepCache.filled() && BeamEst[beamIdx] <= stepCacheEst[cacheIdx])
			cacheIdx ++ ;
		if (cacheIdx < stepCache.len())
		{
			if (stepCache.filled() < stepCache.len())
				stepCache.setFilled(stepCache.filled()+1) ;
			for (i=stepCache.filled()-1; i > cacheIdx ; i--)
			{
				stepCache[i] = stepCache[i-1] ;
				stepCacheEst[i] = stepCacheEst[i-1] ;
			}
			stepCache[cacheIdx] = Beam[beamIdx] ;
			stepCacheEst[cacheIdx] = BeamEst[beamIdx] ;
		}
		beamIdx ++ ;
		cacheIdx++ ;
	}
}



// ************************************************************
//
//                prepareAttrValues
//                -----------------
//
//    changes attributes' values into constructs
//
// ************************************************************
int regressionTree::prepareAttrValues(estimationReg &Estimator, marray<constructReg> &Candidates)
{      
	// Estimator already contains estimationRegs of original attributes

	// temporary value for building
	constructReg tempAttrValue ;
	tempAttrValue.gRT = this ;
	tempAttrValue.countType = aDISCRETE ;
	tempAttrValue.compositionType = cCONJUNCTION ;
	tempAttrValue.root = new constructRegNode ;
	tempAttrValue.root->left = tempAttrValue.root->right = 0 ;
	tempAttrValue.root->nodeType = cnDISCattrValue ;
	int i, j ;

	// attribute values from discrete attributes
	for (i=0 ; i < noDiscrete ; i++)
		if ((opt->selectionEstimatorReg == estRReliefFexpRank || opt->selectionEstimatorReg == estRReliefFkEqual ||
				opt->selectionEstimatorReg == estRReliefFbestK || opt->selectionEstimatorReg == estRReliefFdistance ||
				opt->selectionEstimatorReg == estRReliefFsqrDistance) && Estimator.DiscEstimation[i] <  opt->minReliefEstimate )
			continue;
		else
		{
			// if there is not enough space, make it !
			if (AttrDesc[DiscIdx[i]].NoValues + Candidates.filled() >= Candidates.len())
				Candidates.enlarge(AttrDesc[DiscIdx[i]].NoValues + Candidates.filled() ) ;
			tempAttrValue.root->attrIdx = i ;
			for (j=1 ; j <= AttrDesc[DiscIdx[i]].NoValues ; j++)
			{
				tempAttrValue.root->valueIdx = j ;
				Candidates.addEnd(tempAttrValue) ;
			}
		}

	// attribute values from continuous attributes
	tempAttrValue.root->nodeType = cnCONTattrValue ;
	marray<double> Bounds ;
	int boundIdx ;
	double lowerBound, upperBound ;
	for (i=1 ; i < noNumeric ; i++)
		if ((opt->selectionEstimatorReg == estRReliefFexpRank || opt->selectionEstimatorReg == estRReliefFkEqual ||
				opt->selectionEstimatorReg == estRReliefFbestK || opt->selectionEstimatorReg == estRReliefFdistance ||
				opt->selectionEstimatorReg == estRReliefFsqrDistance) && Estimator.NumEstimation[i] <  opt->minReliefEstimate)
			continue;
		else
		{
			tempAttrValue.root->attrIdx = i ;
			// discretize attribute
			Estimator.discretizeGreedy(i, 0, Bounds) ;
			// if there is not enough space available, prepare it
			if (Bounds.filled()+1 + Candidates.filled() >= Candidates.len())
				Candidates.enlarge(Bounds.filled()+1 + Candidates.filled() ) ;
			lowerBound = upperBound = -DBL_MAX ;
			boundIdx = 0 ;
			// add all the discretized intervals
			while ( boundIdx < Bounds.filled() )
			{
				lowerBound = upperBound ;
				upperBound = Bounds[boundIdx] ;
				tempAttrValue.root->lowerBoundary = lowerBound ;
				tempAttrValue.root->upperBoundary = upperBound ;
				Candidates.addEnd(tempAttrValue) ;
				boundIdx ++ ;
			}
			tempAttrValue.root->lowerBoundary = upperBound ;
			tempAttrValue.root->upperBoundary = DBL_MAX ;
			Candidates.addEnd(tempAttrValue) ;
		}

	if (Candidates.filled() == 0) // there are no candidates
		return -1;

	// estimate the Candidates and eliminate useless
	Estimator.adjustTables(0, noDiscrete + Candidates.filled()) ;
	for (i=0; i < Candidates.filled() ; i++)
	{
		for (j=0 ; j < Estimator.TrainSize ; j++)
			Estimator.DiscValues.Set(j, noDiscrete+i,
					Candidates[i].discreteValue(Estimator.DiscValues,Estimator.NumValues, j)) ;
		Estimator.prepareDiscAttr(noDiscrete + i, 2) ;
	}
	attributeCount bestConstructType ;
	int bestConjunctIdx = Estimator.estimate(opt->selectionEstimatorReg, 1, 1,
			noDiscrete, noDiscrete+Candidates.filled(), bestConstructType) ;
	int idx = 0 ;
	for (i=0 ; i < Candidates.filled() ; i++)
	{
		if ((opt->selectionEstimatorReg == estRReliefFexpRank || opt->selectionEstimatorReg == estRReliefFkEqual ||
				opt->selectionEstimatorReg == estRReliefFbestK || opt->selectionEstimatorReg == estRReliefFdistance ||
				opt->selectionEstimatorReg == estRReliefFsqrDistance) && Estimator.DiscEstimation[noDiscrete+i] >=  opt->minReliefEstimate)
		{
			if (idx != i)
			{
				Candidates[idx] = Candidates[i] ;
				Estimator.DiscEstimation[noDiscrete+idx] = Estimator.DiscEstimation[noDiscrete+i] ;
				Estimator.DiscValues.changeColumns(noDiscrete+idx, noDiscrete+i) ;
				Estimator.prepareDiscAttr(noDiscrete + idx, 2) ;
				if (noDiscrete + i == bestConjunctIdx)
					bestConjunctIdx = noDiscrete + idx ;
			}
			idx ++ ;
		}
	}
	Candidates.setFilled(idx) ;
	return bestConjunctIdx ;
}



// ************************************************************
//
//                summand
//                --------
//
//    builds constructReg consisting of summary of continuous attributes
//
// ************************************************************
double regressionTree::summand(estimationReg &Estimator, constructReg &bestConstruct, 
		marray<constructReg> &stepCache, marray<double> &stepCacheEst)
{
	// original attributes are already estimated

	// prepare candidates
	marray<constructReg> Candidates(noNumeric-1) ; // the size
	Estimator.adjustTables(noNumeric + (noNumeric-1)*opt->beamSize, 0) ;

	int bestConstructIdx = prepareContAttrs(Estimator, cSUM, Candidates, bestConstruct) ;
	if (Candidates.filled() == 0) // there are no candidates
		return -DBL_MAX ;

	double bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;

	int i, j ;
	attributeCount bestConstructType = aCONTINUOUS ;
	marray<constructReg> DiscConstructs(0) ;
	// now start beam search with possibly new estimator
	if (opt->selectionEstimatorReg != opt->constructionEstimatorReg)
	{
		for (i=0 ; i < Candidates.filled() ; i++)
		{
			// fill the values
			for (j=0 ; j < Estimator.TrainSize ; j++)
				Estimator.NumValues.Set(j, noNumeric+i, Candidates[i].continuousValue(Estimator.DiscValues,Estimator.NumValues, j)) ;

			Estimator.prepareContAttr(noNumeric + i) ;
		}
		bestConstructIdx=Estimator.estimateConstruct(opt->constructionEstimatorReg, noNumeric,
				noNumeric +Candidates.filled(),0, 0, bestConstructType,
				DiscConstructs, Candidates) ;
		if (bestConstructIdx == -1)
			return -DBL_MAX ;

		bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;
		bestConstruct = Candidates[bestConstructIdx - noNumeric] ;
	}

	// select the best for the beam
	marray<constructReg> Beam(opt->beamSize) ;
	selectBeam(Beam, stepCache, stepCacheEst, Candidates, Estimator, aCONTINUOUS) ;
	stepCache.setFilled(0) ;

	// form and estimate the sums
	int idx, iBeam, iCandidate ;
	marray<constructReg> Working(Beam.len() * Candidates.filled() ) ;
	for (int size=1 ; size < opt->maxConstructSize ; size++)
	{
		idx = 0 ;
		// create the new sum
		for (iBeam=0 ; iBeam < Beam.filled() ; iBeam++)
			for (iCandidate=0 ; iCandidate < Candidates.filled() ; iCandidate++)
			{
				if (Beam[iBeam].containsAttribute(Candidates[iCandidate]))
					continue ;
				Working[idx].add(Beam[iBeam], Candidates[iCandidate]) ;
				for (j=0 ; j < Estimator.TrainSize ; j++)
					Estimator.NumValues.Set(j, noNumeric+idx,
							Working[idx].continuousValue(Estimator.DiscValues,Estimator.NumValues, j)) ;
				Estimator.prepareContAttr(noNumeric + idx) ;
				idx ++ ;
			}

		Working.setFilled(idx) ;
		if (Working.filled() == 0) // there are no new constructs
			break ;
		bestConstructIdx = Estimator.estimateConstruct(opt->constructionEstimatorReg, noNumeric,
				noNumeric + Working.filled(), 0, 0, bestConstructType,
				DiscConstructs, Working) ;
		if (bestConstructIdx == -1)
			break ;
		// check the best
		if (bestConstructEst < Estimator.NumEstimation[bestConstructIdx])
		{
			bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;
			bestConstruct = Working[bestConstructIdx - noNumeric] ;
		}

		// select the new beam
		selectBeam(Beam, stepCache, stepCacheEst, Working, Estimator, aCONTINUOUS) ;
	}

	return bestConstructEst ;
}



// ************************************************************
//
//                multiplicator
//                -------------
//
//    builds constructReg consisting of summary of continuous attributes
//
// ************************************************************
double regressionTree::multiplicator(estimationReg &Estimator, constructReg &bestConstruct, 
		marray<constructReg> &stepCache, marray<double> &stepCacheEst)
{
	// original attributes are already estimated

	// prepare candidates
	marray<constructReg> Candidates(noNumeric-1) ; // the size
	Estimator.adjustTables(noNumeric + (noNumeric-1)*opt->beamSize, 0) ;

	int bestConstructIdx = prepareContAttrs(Estimator, cPRODUCT, Candidates, bestConstruct) ;
	if (Candidates.filled() == 0 || bestConstructIdx == -1) // there are no candidates
		return -DBL_MAX ;

	double bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;

	int i, j ;
	attributeCount bestConstructType = aCONTINUOUS;
	marray<constructReg> DiscConstructs(0) ;
	// now start beam search with possibly new estimator
	if (opt->selectionEstimatorReg != opt->constructionEstimatorReg)
	{
		for (i=0 ; i < Candidates.filled() ; i++)
		{
			// fill the values
			for (j=0 ; j < Estimator.TrainSize ; j++)
				Estimator.NumValues.Set(j, noNumeric+i, Candidates[i].continuousValue(Estimator.DiscValues,Estimator.NumValues, j)) ;

			Estimator.prepareContAttr(noNumeric + i) ;
		}
		bestConstructIdx=Estimator.estimateConstruct(opt->constructionEstimatorReg, noNumeric,
				noNumeric +Candidates.filled(),0, 0, bestConstructType,
				DiscConstructs, Candidates) ;
		if (bestConstructIdx == -1)
			return -DBL_MAX ;

		bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;
		bestConstruct = Candidates[bestConstructIdx - noNumeric] ;
	}

	// select the best for the beam
	marray<constructReg> Beam(opt->beamSize) ;
	selectBeam(Beam, stepCache, stepCacheEst, Candidates, Estimator,aCONTINUOUS) ;
	stepCache.setFilled(0) ;

	// form and estimate the products
	int idx, iBeam, iCandidate ;
	marray<constructReg> Working(Beam.len() * Candidates.filled() ) ;
	for (int size=1 ; size < opt->maxConstructSize ; size++)
	{
		idx = 0 ;
		// create the new sum
		for (iBeam=0 ; iBeam < Beam.filled() ; iBeam++)
			for (iCandidate=0 ; iCandidate < Candidates.filled() ; iCandidate++)
			{
				if (Beam[iBeam].containsAttribute(Candidates[iCandidate]))
					continue ;
				Working[idx].multiply(Beam[iBeam], Candidates[iCandidate]) ;
				for (j=0 ; j < Estimator.TrainSize ; j++)
					Estimator.NumValues.Set(j, noNumeric+idx,
							Working[idx].continuousValue(Estimator.DiscValues,Estimator.NumValues, j)) ;
				Estimator.prepareContAttr(noNumeric + idx) ;
				idx ++ ;
			}

		Working.setFilled(idx) ;
		if (Working.filled() == 0) // there are no new constructs
			break ;
		bestConstructIdx = Estimator.estimateConstruct(opt->constructionEstimatorReg, noNumeric,
				noNumeric + Working.filled(), 0, 0, bestConstructType,
				DiscConstructs, Working) ;
		if (bestConstructIdx == -1)
			break ;
		// check the best
		if (bestConstructEst < Estimator.NumEstimation[bestConstructIdx])
		{
			bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;
			bestConstruct = Working[bestConstructIdx - noNumeric] ;
		}

		// select the new beam
		selectBeam(Beam, stepCache, stepCacheEst, Working, Estimator, aCONTINUOUS) ;
	}

	return bestConstructEst ;
}



// ************************************************************
//
//                prepareContAttrs
//                ----------------
//
//    selects good attributes for construction
//
// ************************************************************
int regressionTree::prepareContAttrs(estimationReg &Estimator, constructComposition composition, 
		marray<constructReg> &Candidates, constructReg &bestCandidate)
{      
	// Estimator already contains estimationRegs of original attributes

	// temporary value for building
	constructReg tempAttrValue ;
	tempAttrValue.gRT = this ;
	tempAttrValue.countType = aCONTINUOUS ;
	tempAttrValue.compositionType = composition ;
	tempAttrValue.gRT = this ;
	tempAttrValue.root = new constructRegNode ;
	tempAttrValue.root->left = tempAttrValue.root->right = 0 ;
	tempAttrValue.root->nodeType = cnCONTattribute ;

	int bestIdx = -1, bestCandidateIdx = -1 ;
	double bestEst = -DBL_MAX ;
	// select from continuous attributes
	for (int i=1 ; i < noNumeric ; i++)
		if ( (opt->selectionEstimatorReg == estRReliefFexpRank || opt->selectionEstimatorReg == estRReliefFkEqual ||
				opt->selectionEstimatorReg == estRReliefFbestK || opt->selectionEstimatorReg == estRReliefFdistance ||
				opt->selectionEstimatorReg == estRReliefFsqrDistance) &&  Estimator.NumEstimation[i] <  opt->minReliefEstimate)
			continue;
		else
		{
			tempAttrValue.root->attrIdx = i ;
			Estimator.NumEstimation[noNumeric + Candidates.filled()] = Estimator.NumEstimation[i] ;
			Candidates.addEnd(tempAttrValue) ;
			if (Estimator.NumEstimation[i] > bestEst)
			{
				bestEst = Estimator.NumEstimation[i] ;
				bestIdx = i ;
				bestCandidateIdx = Candidates.filled()-1 ;
			}
		}

	if (Candidates.filled() == 0) // there are no candidates
		return -1;
	else
		bestCandidate = Candidates[bestCandidateIdx] ;

	return bestIdx ;
}




// ************************************************************
//
//                       binarization       
//                       ------------
//
//              calls proper binarization procedure
//
// ************************************************************
void regressionTree::binarize(constructReg &nodeConstruct, estimationReg &Estimator) 
{
	double bestEstimation;
	if (opt->selectionEstimatorReg==estMSEofMean)
		Estimator.binarizeBreiman(nodeConstruct, bestEstimation) ;
	else
		Estimator.binarizeGeneral(opt->selectionEstimatorReg, nodeConstruct, bestEstimation, Estimator.noDiscrete) ;
}


// ************************************************************
//
//                       bestSplit       
//                       ---------
//
//       calls proper binarization procedure for continuous attribute
//
// ************************************************************
double regressionTree::bestSplit(constructReg &nodeConstruct, estimationReg &Estimator)
{
	double bestEstimation ;
	if (opt->selectionEstimatorReg==estMSEofMean)  // MSE of mean value
		return Estimator.bestMSEsplit(nodeConstruct, bestEstimation) ;
	else
		return Estimator.bestSplitGeneral(opt->selectionEstimatorReg, nodeConstruct, bestEstimation, Estimator.noDiscrete) ;
}


