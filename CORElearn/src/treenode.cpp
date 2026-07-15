#include <cstdlib>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "ftree.h"
#include "utils.h"
#include "estimator.h"
#include "constrct.h"
#include "options.h"

using namespace std ;

// ************************************************************
//
//                singleAttributeModel
//                --------------------
//
//    builds construct consisting of single attribute
//
// ************************************************************
booleanT featureTree::singleAttributeModel(estimation &Estimator, binnode* Node)
{
	// estimate the attributes
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
							CachedConstructs[i].discreteValue(Estimator.DiscValues, Estimator.NumValues, j)) ;
				Estimator.prepareDiscAttr(noDiscrete + discCount, 2) ;
				discCount ++ ;
			}
			else
				if (CachedConstructs[i].countType == aCONTINUOUS)
				{   // numeric construct
					for (j=0 ; j < Estimator.TrainSize ; j++)
						Estimator.NumValues.Set(j, noNumeric+contCount,
								CachedConstructs[i].continuousValue(Estimator.DiscValues, Estimator.NumValues,j)) ;
					Estimator.prepareContAttr(noNumeric + contCount) ;
					contCount ++ ;
				}
				else
					merror("featureTree::singleAttributeModel", "invalid cached construct count detected") ;
		}
	}

	int bestIdx = Estimator.estimate(opt->selectionEstimator, 0, noNumeric+NoContConstructs, 1, noDiscrete+NoDiscConstructs, bestType) ;

	// copy primary estimations
	//for (int attrIdx = 1 ; attrIdx <= noAttr; attrIdx++)
	//   if (AttrDesc[attrIdx].continuous)
	//      primaryEstimate[attrIdx] = Estimator.NumEstimation[AttrDesc[attrIdx].tablePlace] ;
	//    else
	//       primaryEstimate[attrIdx] =  Estimator.DiscEstimation[AttrDesc[attrIdx].tablePlace] ;


	if (bestIdx == -1)
		return mFALSE ;
	double bestEstimate  ;
	if (bestType == aCONTINUOUS)
		bestEstimate = Estimator.NumEstimation[bestIdx] ;
	else
		bestEstimate = Estimator.DiscEstimation[bestIdx] ;

	if ( bestEstimate < opt->minReliefEstimate &&
			(opt->selectionEstimator == estReliefFkEqual || opt->selectionEstimator == estReliefFexpRank ||
					opt->selectionEstimator == estReliefFbestK || opt->selectionEstimator == estRelief ||
					opt->selectionEstimator == estReliefFmerit || opt->selectionEstimator == estReliefFdistance ||
					opt->selectionEstimator == estReliefFsqrDistance) )
		return mFALSE ;


	if ( (bestType == aCONTINUOUS && bestIdx < noNumeric) ||
			(bestType == aDISCRETE && bestIdx < noDiscrete) )
		makeSingleAttrNode(Node, Estimator, bestIdx, bestType) ;
	else
	{
		// cached construct was selected - find out which one
		int idx, selectedConstruct = -1 ;
		if (bestType == aCONTINUOUS)
			idx =
					- noNumeric ;
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
    			  merror("featureTree::singleAttributeModel", "the selected construct index is invalid") ;
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
void featureTree::makeSingleAttrNode(binnode* Node, estimation &Estimator, int bestIdx, attributeCount bestType)
{
	Node->Construct.createSingle(bestIdx, bestType) ;
	Node->Construct.gFT = this ;

	if (bestType == aCONTINUOUS)
	{
		Node->Identification = continuousAttribute ;
		if (opt->binaryEvaluateNumericAttributes && Estimator.isMyopic(opt->selectionEstimator))
			Node->Construct.splitValue = Estimator.splitPoint[bestIdx];
		else
			Node->Construct.splitValue = Estimator.bestSplitGeneral(Node->Construct, Estimator.noDiscrete) ;
	}
	else
	{
		Node->Identification = discreteAttribute ;
		//Node->Construct.leftValues.create(AttrDesc[DiscIdx[bestIdx]].NoValues+1) ;
		Node->Construct.noValues = AttrDesc[DiscIdx[bestIdx]].NoValues ;
		Estimator.binarizeGeneral(Node->Construct, Estimator.noDiscrete) ;
	}
}


// ************************************************************
//
//                makeConstructNode
//                ------------------
//
//    prepares construct for use in the node
//
// ************************************************************
void featureTree::makeConstructNode(binnode* Node, estimation &Estimator, construct &Construct)
{
	Node->Construct = Construct ;
	Node->Construct.gFT = this ;

	if (Construct.countType == aCONTINUOUS)
	{
		Node->Identification = continuousAttribute ;
		Node->Construct.splitValue = Estimator.bestSplitGeneral(Construct,Estimator.noDiscrete) ;
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
//    builds construct consisting of conjunction of attribute values
//
// ************************************************************
double featureTree::conjunct(estimation &Estimator, construct &bestConjunct, marray<construct> &stepCache,
		marray<double> &stepCacheEst)
{
	// original attributes are already estimated

	// prepare attribute values
	marray<construct> Candidates(noAttr*10) ; // estimation of the size
	int bestConjunctIdx = prepareAttrValues(Estimator, Candidates) ;

	if (Candidates.filled() == 0) // there are no candidates
		return -DBL_MAX ;

	attributeCount bestConstructType = aDISCRETE ;
	marray<construct> ContConstructs(0) ;
	// now start beam search with possibly new estimator
	if (opt->selectionEstimator != opt->constructionEstimator)
	{
		bestConjunctIdx=Estimator.estimateConstruct(opt->constructionEstimator, 1, 1,
				noDiscrete, noDiscrete+Candidates.filled(), bestConstructType) ;
		if (bestConjunctIdx == -1)
			return -DBL_MAX ;
	}

	double bestConjunctEst = Estimator.DiscEstimation[bestConjunctIdx] ;
	bestConjunct = Candidates[bestConjunctIdx - noDiscrete] ;

	// select the best for the beam
	marray<construct> Beam(opt->beamSize) ;
	selectBeam(Beam, stepCache, stepCacheEst, Candidates, Estimator, aDISCRETE) ;
	stepCache.setFilled(0) ;

	// form and estimate the conjunctions
	int j, idx, iBeam, iCandidate ;
	Estimator.adjustTables(0, noDiscrete + Beam.len() * Candidates.filled() ) ;
	marray<construct> Working(Beam.len() * Candidates.filled() ) ;
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
							Working[idx].discreteValue(Estimator.DiscValues, Estimator.NumValues,j)) ;
				Estimator.prepareDiscAttr(noDiscrete + idx, 2) ;
				idx ++ ;
			}

		Working.setFilled(idx) ;
		if (Working.filled() == 0) // there are no new constructs
			break ;
		bestConjunctIdx = Estimator.estimateConstruct(opt->constructionEstimator, 0, 0,
				noDiscrete, noDiscrete + Working.filled(), bestConstructType) ;
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
//    select beam for beam search in constructive induction
//
// ************************************************************
void featureTree::selectBeam(marray<construct> &Beam, marray<construct> &stepCache,
		marray<double> &stepCacheEst, marray<construct> &Candidates,
		estimation &Estimator, attributeCount aCount)
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
		merror("featureTree::selectBeam", "invalid attribute count detected") ;
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

		// check this construct if it is equal to any of
		// those with the same estimate
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
int featureTree::prepareAttrValues(estimation &Estimator, marray<construct> &Candidates)
{
	// Estimator already contains estimations of original attributes

	// temporary value for building
	construct tempAttrValue ;
	tempAttrValue.countType = aDISCRETE ;
	tempAttrValue.compositionType = cCONJUNCTION ;
	tempAttrValue.root = new constructNode ;
	tempAttrValue.root->left = tempAttrValue.root->right = 0 ;
	tempAttrValue.root->nodeType = cnDISCattrValue ;
	int i, j ;

	// attribute values from nominal attributes
	for (i=1 ; i < noDiscrete ; i++)
		if (Estimator.DiscEstimation[i] <  opt->minReliefEstimate &&
				(opt->selectionEstimator == estReliefFkEqual || opt->selectionEstimator == estReliefFexpRank ||
						opt->selectionEstimator == estReliefFbestK || opt->selectionEstimator == estRelief ||
						opt->selectionEstimator == estReliefFmerit || opt->selectionEstimator == estReliefFdistance ||
						opt->selectionEstimator == estReliefFsqrDistance) )
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
	for (i=0 ; i < noNumeric ; i++)
		if (Estimator.NumEstimation[i] <  opt->minReliefEstimate &&
				(opt->selectionEstimator == estReliefFkEqual || opt->selectionEstimator == estReliefFexpRank ||
						opt->selectionEstimator == estReliefFbestK || opt->selectionEstimator == estRelief ||
						opt->selectionEstimator == estReliefFmerit || opt->selectionEstimator == estReliefFdistance ||
						opt->selectionEstimator == estReliefFsqrDistance) )
			continue;
		else
		{
			tempAttrValue.root->attrIdx = i ;
			// discretize attribute
			Estimator.discretizeGreedy(i, 0, Bounds, noDiscrete) ;
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
					Candidates[i].discreteValue(Estimator.DiscValues, Estimator.NumValues,j)) ;
		Estimator.prepareDiscAttr(noDiscrete + i, 2) ;
	}
	attributeCount bestConstructType ;
	int bestConjunctIdx = Estimator.estimate(opt->selectionEstimator, 0, 0,
			noDiscrete, noDiscrete+Candidates.filled(), bestConstructType) ;
	int idx = 0 ;
	for (i=0 ; i < Candidates.filled() ; i++)
	{
		if (Estimator.DiscEstimation[noDiscrete+i] >=  opt->minReliefEstimate &&
				(opt->selectionEstimator == estReliefFkEqual || opt->selectionEstimator == estReliefFexpRank ||
						opt->selectionEstimator == estReliefFbestK || opt->selectionEstimator == estRelief ||
						opt->selectionEstimator == estReliefFmerit || opt->selectionEstimator == estReliefFdistance ||
						opt->selectionEstimator == estReliefFsqrDistance) )
		{
			if (idx != i)
			{
				Candidates[idx] = Candidates[i] ;
				Estimator.DiscEstimation[noDiscrete+idx] = Estimator.DiscEstimation[noDiscrete+i] ;
				Estimator.DiscValues.changeColumns(noDiscrete+idx, noDiscrete+i) ;
				Estimator.prepareDiscAttr(noDiscrete + idx, 2) ;
				if (i+noDiscrete==bestConjunctIdx)
					bestConjunctIdx = idx + noDiscrete ;
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
//    builds construct consisting of summary of numeric attributes
//
// ************************************************************
double featureTree::summand(estimation &Estimator, construct &bestConstruct, marray<construct> &stepCache,
		marray<double> &stepCacheEst)
{
	// original attributes are already estimated

	// prepare candidates
	marray<construct> Candidates(noNumeric) ; // the size
	Estimator.adjustTables(noNumeric + noNumeric*opt->beamSize, 0) ;

	int bestConstructIdx = prepareContAttrs(Estimator, cSUM, Candidates, bestConstruct) ;
	if (Candidates.filled() == 0) // there are no candidates
		return -DBL_MAX ;

	double bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;

	int i, j ;
	attributeCount bestConstructType = aCONTINUOUS ;
	marray<construct> DiscConstructs(0) ;
	// now start beam search with possibly new estimator
	if (opt->selectionEstimator != opt->constructionEstimator)
	{
		for (i=0 ; i < Candidates.filled() ; i++)
		{
			// fill the values
			for (j=0 ; j < Estimator.TrainSize ; j++)
				Estimator.NumValues.Set(j, noNumeric+i, Candidates[i].continuousValue(Estimator.DiscValues, Estimator.NumValues,j)) ;

			Estimator.prepareContAttr(noNumeric + i) ;
		}
		bestConstructIdx=Estimator.estimateConstruct(opt->constructionEstimator, noNumeric,
				noNumeric +Candidates.filled(),0, 0, bestConstructType) ;
		if (bestConstructIdx == -1)
			return -DBL_MAX ;

		bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;
		bestConstruct = Candidates[bestConstructIdx - noNumeric] ;
	}

	// select the best for the beam
	marray<construct> Beam(opt->beamSize) ;
	selectBeam(Beam, stepCache, stepCacheEst, Candidates, Estimator, aCONTINUOUS) ;
	stepCache.setFilled(0) ;

	// form and estimate the sums
	int idx, iBeam, iCandidate ;
	marray<construct> Working(Beam.len() * Candidates.filled() ) ;
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
							Working[idx].continuousValue(Estimator.DiscValues, Estimator.NumValues, j)) ;
				Estimator.prepareContAttr(noNumeric + idx) ;
				idx ++ ;
			}

		Working.setFilled(idx) ;
		if (Working.filled() == 0) // there are no new constructs
			break ;
		bestConstructIdx = Estimator.estimateConstruct(opt->constructionEstimator, noNumeric,
				noNumeric + Working.filled(), 0, 0, bestConstructType) ;
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
//    builds construct consisting of summary of numeric attributes
//
// ************************************************************
double featureTree::multiplicator(estimation &Estimator, construct &bestConstruct, marray<construct> &stepCache,
		marray<double> &stepCacheEst)
{
	// original attributes are already estimated

	// prepare candidates
	marray<construct> Candidates(noNumeric) ; // the size
	Estimator.adjustTables(noNumeric + noNumeric*opt->beamSize, 0) ;

	int bestConstructIdx = prepareContAttrs(Estimator, cPRODUCT, Candidates, bestConstruct) ;
	if (Candidates.filled() == 0 || bestConstructIdx == -1) // there are no candidates
		return -DBL_MAX ;

	double bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;

	int i, j ;
	attributeCount bestConstructType = aCONTINUOUS;
	marray<construct> DiscConstructs(0) ;
	// now start beam search with possibly new estimator
	if (opt->selectionEstimator != opt->constructionEstimator)
	{
		for (i=0 ; i < Candidates.filled() ; i++)
		{
			// fill the values
			for (j=0 ; j < Estimator.TrainSize ; j++)
				Estimator.NumValues.Set(j, noNumeric+i, Candidates[i].continuousValue(Estimator.DiscValues, Estimator.NumValues,j)) ;

			Estimator.prepareContAttr(noNumeric + i) ;
		}
		bestConstructIdx=Estimator.estimateConstruct(opt->constructionEstimator, noNumeric,
				noNumeric +Candidates.filled(),0, 0, bestConstructType) ;
		if (bestConstructIdx == -1)
			return -DBL_MAX ;

		bestConstructEst = Estimator.NumEstimation[bestConstructIdx] ;
		bestConstruct = Candidates[bestConstructIdx - noNumeric] ;
	}

	// select the best for the beam
	marray<construct> Beam(opt->beamSize) ;
	selectBeam(Beam, stepCache, stepCacheEst, Candidates, Estimator,aCONTINUOUS) ;
	stepCache.setFilled(0) ;

	// form and estimate the products
	int idx, iBeam, iCandidate ;
	marray<construct> Working(Beam.len() * Candidates.filled() ) ;
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
							Working[idx].continuousValue(Estimator.DiscValues, Estimator.NumValues,j)) ;
				Estimator.prepareContAttr(noNumeric + idx) ;
				idx ++ ;
			}

		Working.setFilled(idx) ;
		if (Working.filled() == 0) // there are no new constructs
			break ;
		bestConstructIdx = Estimator.estimateConstruct(opt->constructionEstimator, noNumeric,
				noNumeric + Working.filled(), 0, 0, bestConstructType) ;
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
int featureTree::prepareContAttrs(estimation &Estimator, constructComposition composition,
		marray<construct> &Candidates, construct &bestCandidate)
{
	// Estimator already contains estimations of original attributes

	// temporary value for building
	construct tempAttrValue ;
	tempAttrValue.countType = aCONTINUOUS ;
	tempAttrValue.compositionType = composition ;
	tempAttrValue.root = new constructNode ;
	tempAttrValue.root->left = tempAttrValue.root->right = 0 ;
	tempAttrValue.root->nodeType = cnCONTattribute ;

	int bestIdx = -1, bestCandidateIdx = -1 ;
	double bestEst = -DBL_MAX ;
	// select from numeric attributes
	for (int i=0 ; i < noNumeric ; i++)
		if (Estimator.NumEstimation[i] <  opt->minReliefEstimate &&
				(opt->selectionEstimator == estReliefFkEqual || opt->selectionEstimator == estReliefFexpRank ||
						opt->selectionEstimator == estReliefFbestK || opt->selectionEstimator == estRelief ||
						opt->selectionEstimator == estReliefFmerit || opt->selectionEstimator == estReliefFdistance ||
						opt->selectionEstimator == estReliefFsqrDistance) )
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



