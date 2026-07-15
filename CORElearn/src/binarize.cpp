#include <cfloat>

#include "general.h"
#include "error.h"
#include "contain.h"
#include "utils.h"
#include "estimator.h"
#include "binpart.h"
#include "options.h"

//extern Options *opt ;

using namespace std ;


// ************************************************************
//
//                       binarizeGeneral
//                       ----------------
//
//    creates binary split of attribute values according to the split's  estimate;
//             search is either exhaustive, greedy or random depending
//                on the number of computations for each
//
// ************************************************************
void estimation::binarizeGeneral(construct &nodeConstruct, int firstFreeDiscSlot)
{

	int i, NoValues = nodeConstruct.noValues ;
	nodeConstruct.leftValues.create(NoValues+1, mFALSE) ;
	attributeCount bestType ;

	if (firstFreeDiscSlot == 0)
		firstFreeDiscSlot = noDiscrete ;


	if (NoValues < 2)   {
		return ;
	}

	booleanT binaryEvaluationBefore = eopt.binaryEvaluation ;
	eopt.binaryEvaluation = mFALSE ;


	if (NoValues == 2) { // already binary
		nodeConstruct.leftValues[1] = mTRUE ;
    	return ;
	}


	int attrValue ;
	int bestIdx ;
	// double bestEstimation = -DBL_MAX ;

	if (NoValues > eopt.maxValues4Greedy) {
	    // random binarization
		marray<int>  valueCount(NoValues+1, 0) ;
		for (i=0 ; i < TrainSize ; i++)	{
			attrValue = nodeConstruct.discreteValue(DiscValues, NumValues, i) ;
			valueCount[attrValue] ++ ;
		}
		int validValues = TrainSize - valueCount[NAdisc] ;
		if ( validValues <= eopt.minNodeWeightEst/2.0) { // split will be invalid anyway
			nodeConstruct.leftValues.init(mFALSE) ;
		}
		double targetLeftVals = randBetween(eopt.minNodeWeightEst, validValues/2.0) ;
		int leftVal = 0 ;
		// randomly order the value indexes
		marray<int> order(NoValues+1) ;
		for (i = 0 ; i <= NoValues; ++i)
			order[i] = i ;
		for (i = 1 ; i < NoValues; ++i) { // shuffle all expect 0th, last can be skipped
			mswap(order[i], order[randBetween(i, NoValues)]) ;
		}
		for (int idx=1 ; idx <= NoValues ; ++idx) 	{
			leftVal += valueCount[order[idx]] ;
            if (leftVal == validValues)  // do not allow all relevant values on the left
            	break ;
			nodeConstruct.leftValues[ order[idx] ] = mTRUE ; // add this value to the left
			if (leftVal >= targetLeftVals)
				break ;
		}
	}
	else if ( NoValues <= eopt.maxValues4Exhaustive) { // &&(exhaustivePositions * 0.8 <= greedyPositions ||exhaustivePositions < eopt.discretizationSample))
		// exhaustive search
		binPartition Generator(NoValues) ;
		double exhaustivePositions = Generator.noPositions() ;

		adjustTables(0, firstFreeDiscSlot + int(exhaustivePositions)) ;
		marray<marray<booleanT> >  leftValues( (int)exhaustivePositions ) ;
		int noIncrements = 0, noLeft, noRight ;
		while (Generator.increment() )
		{
			// save partition
			leftValues[noIncrements] = Generator.leftPartition ;
			// count values to check if partition  is valid
			noLeft = noRight = 0 ;
			// compute data column
			for (i=0 ; i < TrainSize ; i++)
			{
				attrValue = nodeConstruct.discreteValue(DiscValues, NumValues, i) ;
				if (attrValue == NAdisc)
					DiscValues.Set(i, firstFreeDiscSlot + noIncrements, NAdisc) ;
				else
					if (leftValues[noIncrements][attrValue]) {
						DiscValues.Set(i, firstFreeDiscSlot + noIncrements, 1) ;
						noLeft ++ ;
					}
					else {
						DiscValues.Set(i, firstFreeDiscSlot + noIncrements, 2) ;
						noRight ++ ;
					}
			}
			if (noLeft >= eopt.minNodeWeightEst && noRight > eopt.minNodeWeightEst) {
			  prepareDiscAttr(firstFreeDiscSlot + noIncrements, 2) ;
			  noIncrements++ ;
			}
		}
		// check validity of partitions
        if (noIncrements == 0) 
			nodeConstruct.leftValues.init(mTRUE) ; // put all to left, getting invalid split
		else if (noIncrements == 1)  // no need to estimate
		   nodeConstruct.leftValues =  leftValues[0] ;
		else {
		   // estimate and select best
		   bestIdx = estimate(eopt.selectionEstimator, 0, 0,
				firstFreeDiscSlot, firstFreeDiscSlot+noIncrements, bestType) ;
		   if (bestIdx >= firstFreeDiscSlot)
		      nodeConstruct.leftValues =  leftValues[bestIdx - firstFreeDiscSlot] ;
		   else // invalid estimates
			   nodeConstruct.leftValues.init(mTRUE) ; // put all to left, getting invalid split
		   //bestEstimation =  DiscEstimation[bestIdx] ;
		}
	}
	else  {	// greedy search
		adjustTables(0, firstFreeDiscSlot + NoValues) ;
		marray<marray<booleanT> >  leftValues(NoValues) ;
		marray<int>  noLeft(NoValues), noRight(NoValues) ; // number of instances after a split
		marray<booleanT> currentBest(NoValues+1, mFALSE) ;
		int i, j, added ;
		double bestEstimation = -DBL_MAX ;

		for (int filled=1 ; filled < NoValues ; filled++)
		{
			added = 0 ;
			for (j=1 ; j <= NoValues ; j++)
				if (currentBest[j] == mFALSE)
				{
					currentBest[j] = mTRUE ;
					leftValues[added] = currentBest ;
					noLeft[added] = noRight[added] = 0 ;

					// compute data column
					for (i=0 ; i < TrainSize ; i++)
					{
						attrValue = nodeConstruct.discreteValue(DiscValues, NumValues, i) ;
						if (attrValue == NAdisc)
							DiscValues.Set(i, firstFreeDiscSlot + added, NAdisc) ;
						else
							if (leftValues[added][attrValue]) {
								DiscValues.Set(i, firstFreeDiscSlot + added, 1) ;
								noLeft[added] ++ ;
							}
							else {
								DiscValues.Set(i, firstFreeDiscSlot + added, 2) ;
								noRight[added] ++ ;
							}

					}
					prepareDiscAttr(firstFreeDiscSlot + added, 2) ;

					currentBest[j] = mFALSE ;
					added ++ ;
				}
			bestIdx = estimate(eopt.selectionEstimator, 0, 0,
					firstFreeDiscSlot, firstFreeDiscSlot + added, bestType) ;
			if (bestIdx >= firstFreeDiscSlot) // valid estimate
			   currentBest = leftValues[bestIdx - firstFreeDiscSlot] ;
			else break ;

			if (DiscEstimation[bestIdx] > bestEstimation)
			{
				// check if split is valid
				if ( noLeft[bestIdx - firstFreeDiscSlot] >= eopt.minNodeWeightEst && noRight[bestIdx - firstFreeDiscSlot] >= eopt.minNodeWeightEst ) {
				   bestEstimation = DiscEstimation[bestIdx] ;
				   nodeConstruct.leftValues =  currentBest ;
				}
				else if (bestEstimation == -DBL_MAX) { // no added yet
				   nodeConstruct.leftValues =  currentBest ; // add anyway, but do not update bestEstimation
				}
			}
		}
	}
	eopt.binaryEvaluation = binaryEvaluationBefore ;

}



//************************************************************
//
//                        bestSplitGeneral
//                        ----------------
//
//            finds best split for numeric attribute with selected estimator
//
//************************************************************
double estimation::bestSplitGeneral(construct &nodeConstruct, int firstFreeDiscSlot)
{
	if (firstFreeDiscSlot == 0)
		firstFreeDiscSlot = noDiscrete ;

	marray<sortRec> sortedAttr(TrainSize) ;
	int i, j ;
	int OKvalues = 0 ;
	double attrValue ;
	for (j=0 ; j < TrainSize ; j++)
	{
		attrValue = nodeConstruct.continuousValue(DiscValues, NumValues, j) ;
		if (isNAcont(attrValue))
			continue ;
		sortedAttr[OKvalues].key = attrValue ;
		sortedAttr[OKvalues].value = j ;
		OKvalues ++ ;
	}
	if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
	{
		return -DBL_MAX ; // smaller than any value, so all examples will go into one branch
	}
	sortedAttr.setFilled(OKvalues) ;
	sortedAttr.qsortAsc() ;

	// select only unique values but skip also smallest and largest values - we do not want split there
	int lastUnique = 0, lower = int(eopt.minNodeWeightEst+0.5), upper =  int(OKvalues - eopt.minNodeWeightEst) ;
	sortedAttr[lastUnique] = sortedAttr[lower] ;
	for ( i = lower+1; i < upper ; i++)
	{
		if (sortedAttr[i].key != sortedAttr[lastUnique].key)
		{
			lastUnique ++ ;
			sortedAttr[lastUnique] = sortedAttr[i] ;
		}
	}
	OKvalues = lastUnique+1 ;
	if (OKvalues <= 1)	{
		return - DBL_MAX ; // smaller than any value, so all examples will go into one branch
	}


	if (eopt.discretizationSample == 1) { // greedy selection of splitting point
		int splitIdx = randBetween(0, OKvalues-1) ;
		return (sortedAttr[splitIdx].key + sortedAttr[splitIdx+1].key)/2.0 ;
	}

	int sampleSize ;
	if (eopt.discretizationSample==0)
		sampleSize = OKvalues - 1;
	else
		sampleSize = Mmin(eopt.discretizationSample, OKvalues-1) ;
	marray<int> splits(sampleSize) ;
	randomizedSample(splits, sampleSize, OKvalues-1) ;

	attributeCount bestType ;

	adjustTables(0, firstFreeDiscSlot + sampleSize) ;
	for (j=0 ; j < sampleSize ; j++)
	{
		// compute data column
		for (i=0 ; i < TrainSize ; i++)
		{
			attrValue = nodeConstruct.continuousValue(DiscValues, NumValues, i) ;
			if (isNAcont(attrValue))
				DiscValues.Set(i, firstFreeDiscSlot + j, NAdisc) ;
			else
				if ( attrValue <= sortedAttr[splits[j]].key )
					DiscValues.Set(i, firstFreeDiscSlot + j, 1) ;
				else
					DiscValues.Set(i, firstFreeDiscSlot + j, 2) ;
		}
		prepareDiscAttr(firstFreeDiscSlot + j, 2) ;
	}

	booleanT binaryEvaluationBefore = eopt.binaryEvaluation ;
	eopt.binaryEvaluation = mFALSE ;

	// estimate and select best
	int bestIdx = estimate(eopt.selectionEstimator, 0, 0,
			firstFreeDiscSlot, firstFreeDiscSlot + sampleSize, bestType) ;
	eopt.binaryEvaluation = binaryEvaluationBefore ;

	if (bestIdx >= firstFreeDiscSlot)
		return (sortedAttr[splits[bestIdx-firstFreeDiscSlot]].key + sortedAttr[splits[bestIdx-firstFreeDiscSlot]+1].key)/2.0 ;
	else
		return - DBL_MAX ;
}



//************************************************************
//
//                        discretizeGreedy
//                        -----------------
//
//     finds best discretization of numeric attribute with
//      greedy algorithm and returns its estimated quality
//
//************************************************************
double estimation::discretizeGreedy(int ContAttrIdx, int maxBins, marray<double> &Bounds, int firstFreeDiscSlot)
{
	Bounds.setFilled(0) ;

	if (firstFreeDiscSlot == 0)
		firstFreeDiscSlot = noDiscrete ;

	marray<sortRec> sortedAttr(TrainSize) ;
	int i, j, idx ;
	int OKvalues = 0 ;
	for (j=0 ; j < TrainSize ; j++)
	{
		if (isNAcont(NumValues(j, ContAttrIdx)))
			continue ;
		sortedAttr[OKvalues].key = NumValues(j, ContAttrIdx) ;
		sortedAttr[OKvalues].value = j ;
		OKvalues ++ ;
	}
	if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
	{
		// all values of the attribute are missing or equal
		return - DBL_MAX ;
	}
	sortedAttr.setFilled(OKvalues) ;
	sortedAttr.qsortAsc() ;

	// eliminate duplicates 
	int lastUnique = 0 ;
	for (j=1 ; j < OKvalues ; j ++)
	{
		if (sortedAttr[j].key != sortedAttr[lastUnique].key)
		{
			lastUnique ++ ;
			sortedAttr[lastUnique] = sortedAttr[j] ;
		}
	}
	OKvalues = lastUnique+1 ;
	sortedAttr.setFilled(OKvalues) ;

	if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
	{
		// all values of the attribute are missing or equal
		return - DBL_MAX ;
	}

	booleanT binaryEvaluationBefore = eopt.binaryEvaluation ;
	eopt.binaryEvaluation = mFALSE ;

	int sampleSize ;
	// we use all the available values only if explicitly demanded
	if (eopt.discretizationSample==0)
		sampleSize = OKvalues -1;
	else
		sampleSize = Mmin(eopt.discretizationSample, OKvalues-1) ;

	marray<int> splits(sampleSize) ;
	randomizedSample(splits, sampleSize, OKvalues-1) ;

	attributeCount bestType ;
	double attrValue ;

	adjustTables(0, firstFreeDiscSlot + sampleSize) ;

	// greedy search
	marray<double> currentBounds(sampleSize) ;
	int currentIdx ;
	double bestEstimate = - DBL_MAX, bound ;
	int currentLimit=0 ; // number of times the current dicretization was worse than the best discretization
	int currentNoValues = 2 ;
	while (currentLimit <= eopt.discretizationLookahead && sampleSize > 0 && (maxBins==0 || currentNoValues <= maxBins))
	{
		// compute data columns
		for (i=0 ; i < TrainSize ; i++)
		{
			attrValue = NumValues(i, ContAttrIdx) ;
			idx = 0 ;
			while (idx < currentBounds.filled()  &&  attrValue > currentBounds[idx])
				idx++ ;
			idx ++ ; // changes idx to discrete value
			for (j=0 ; j < sampleSize ; j++)
			{
				if (isNAcont(attrValue))
					DiscValues.Set(i, firstFreeDiscSlot + j, NAdisc) ;
				else
					if (attrValue <= sortedAttr[splits[j]].key)
						DiscValues.Set(i, firstFreeDiscSlot + j, idx) ;
					else
						DiscValues.Set(i, firstFreeDiscSlot + j, idx+1) ;
			}
		}
		for (j=0 ; j < sampleSize ; j++)
			prepareDiscAttr(firstFreeDiscSlot + j, currentNoValues) ;
		// estimate and select best
		currentIdx = estimate(eopt.selectionEstimator, 0, 0, firstFreeDiscSlot, firstFreeDiscSlot + sampleSize, bestType) ;
		if (currentIdx >= firstFreeDiscSlot) {
		   bound = (sortedAttr[splits[currentIdx - firstFreeDiscSlot]].key
			    	+ sortedAttr[splits[currentIdx - firstFreeDiscSlot]+1].key)/2.0 ;
		   currentBounds.addToAscSorted(bound) ;
		   if (DiscEstimation[currentIdx] > bestEstimate) {
			  bestEstimate = DiscEstimation[currentIdx] ;
			  Bounds = currentBounds ;
			  currentLimit = 0 ;
		    }
		    else
			   currentLimit ++ ;
		    splits[currentIdx-firstFreeDiscSlot] = splits[--sampleSize] ;
		    currentNoValues ++ ;
		}
		else break ;
	}

	eopt.binaryEvaluation = binaryEvaluationBefore ;

	return bestEstimate ;
}



//************************************************************
//
//                        estBinarized
//                        ------------
//
//       estimate attribute as if they were binarized
//
//************************************************************
void estimation::estBinarized(int selectedEstimator, int contAttrFrom, int contAttrTo,
		int discAttrFrom, int discAttrTo, int firstFreeDiscSlot)
{
	if (firstFreeDiscSlot == 0)
		firstFreeDiscSlot = noDiscrete ;

	booleanT binaryEvaluationBefore = eopt.binaryEvaluation ;
	eopt.binaryEvaluation = mFALSE ;

	attributeCount bestType ;
	int addedAttr = 0, i, j, NoValues, noPartitions, iDisc, iCont, estIdx ;
	int NoDiscEstimated = discAttrTo - discAttrFrom ;
	int NoContEstimated = contAttrTo - contAttrFrom ;
	marray<int> discFrom(NoDiscEstimated), discTo(NoDiscEstimated), contFrom(NoContEstimated), contTo(NoContEstimated) ;
	int discAttrValue ;

	// estimated size
	int adjustedSize = firstFreeDiscSlot + NoDiscEstimated* 4 ;
	if (! isMyopic(selectedEstimator)) {
		// for ReliefF like
		adjustedSize += NoContEstimated * eopt.discretizationSample ;
	}

	adjustTables(0, adjustedSize) ;

	// prepare splits of discrete attributes

	for (iDisc=discAttrFrom ; iDisc < discAttrTo; iDisc++)
	{
		NoValues = discNoValues[iDisc] ;
		estIdx = iDisc - discAttrFrom ;

		if (NoValues < 2)
		{
			discFrom[estIdx] = discTo[estIdx] = -1 ;
		}
		else  if (NoValues == 2) // already binary, we estimate it
		{
			adjustTables(0, firstFreeDiscSlot + addedAttr + 1) ;
			for (i=0 ; i < TrainSize ; i++)
				DiscValues.Set(i, firstFreeDiscSlot + addedAttr, DiscValues(i,iDisc)) ;

			prepareDiscAttr(firstFreeDiscSlot+addedAttr, 2) ;
			discFrom[estIdx] = firstFreeDiscSlot + addedAttr ;
			discTo[estIdx] = firstFreeDiscSlot + addedAttr + 1 ;
			addedAttr ++ ;
			continue ;
		}
		else {

			binPartition Generator(NoValues) ;
			noPartitions = 0 ;
			adjustTables(0,  firstFreeDiscSlot + addedAttr + int(Mmin(Generator.noPositions(), (double)(eopt.discretizationSample)))) ;
			discFrom[estIdx] = firstFreeDiscSlot + addedAttr ;
			while (Generator.increment() )
			{
				// compute data column
				for (i=0 ; i < TrainSize ; i++)
				{
					discAttrValue = DiscValues(i, iDisc) ;
					if (discAttrValue == NAdisc)
						DiscValues.Set(i, firstFreeDiscSlot + addedAttr, NAdisc) ;
					else
						if (Generator.leftPartition[discAttrValue])
							DiscValues.Set(i, firstFreeDiscSlot + addedAttr, 1) ;
						else
							DiscValues.Set(i, firstFreeDiscSlot + addedAttr, 2) ;
				}
				prepareDiscAttr(firstFreeDiscSlot + addedAttr, 2) ;
				addedAttr++ ;
				noPartitions++ ;
				if (noPartitions >= eopt.discretizationSample)
					break ;
			}
			discTo[estIdx] = firstFreeDiscSlot + addedAttr ;

		}
	}

	// prepare numeric features

	booleanT binaryBefore = eopt.binaryEvaluateNumericAttributes ;
	int lowCont = 0, highCont = 0 ;
	if (isMyopic(selectedEstimator)) {
		// for these estimators numeric features are made binary during estimation
		eopt.binaryEvaluateNumericAttributes = mTRUE ;
		lowCont = contAttrFrom ;
		highCont = contAttrTo ;
	}
	else { // for ReliefF and company explicitly create binary splits

		marray<sortRec> sortedAttr(TrainSize) ;
		int OKvalues  ;
		double contAttrValue ;
		int sampleSize ;
		marray<int> splits(TrainSize), sortedCopy(TrainSize) ;

		for (iCont=contAttrFrom ; iCont < contAttrTo; iCont++)
		{
			estIdx = iCont - contAttrFrom ;
			contFrom[estIdx] = firstFreeDiscSlot + addedAttr ;
			OKvalues = 0 ;

			for (j=0 ; j < TrainSize ; j++)
			{
				contAttrValue = NumValues(j, iCont) ;
				if (isNAcont(contAttrValue))
					continue ;
				sortedAttr[OKvalues].key = contAttrValue ;
				sortedAttr[OKvalues].value = j ;
				OKvalues ++ ;
			}
			if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
			{
				contTo[estIdx] = -1 ;
				continue ;
			}
			sortedAttr.setFilled(OKvalues) ;
			sortedAttr.qsortAsc() ;

			int lastUnique = 0 ;
			for (i=1 ; i < OKvalues ; i++)
			{
				if (sortedAttr[i].key != sortedAttr[lastUnique].key)
				{
					lastUnique ++ ;
					sortedAttr[lastUnique] = sortedAttr[i] ;
				}
			}
			OKvalues = lastUnique+1 ;
			if (OKvalues <= 1)
			{
				contTo[estIdx] = -1 ;
				continue ;
			}


			if (eopt.discretizationSample==0)
				sampleSize = OKvalues -1;
			else
				sampleSize = Mmin(eopt.discretizationSample, OKvalues-1) ;

			randomizedSample(splits, sampleSize, OKvalues-1) ;

			adjustTables(0, firstFreeDiscSlot + addedAttr+ sampleSize) ;
			for (j=0 ; j < sampleSize ; j++)
			{
				// compute data column
				for (i=0 ; i < TrainSize ; i++)
				{
					contAttrValue = NumValues(i,iCont) ;
					if (isNAcont(contAttrValue))
						DiscValues.Set(i, firstFreeDiscSlot + addedAttr, NAdisc) ;
					else
						if ( contAttrValue <= sortedAttr[splits[j]].key )
							DiscValues.Set(i, firstFreeDiscSlot + addedAttr, 1) ;
						else
							DiscValues.Set(i, firstFreeDiscSlot + addedAttr, 2) ;
				}
				prepareDiscAttr(firstFreeDiscSlot + addedAttr, 2) ;
				addedAttr ++ ;
			}

			contTo[estIdx] = firstFreeDiscSlot + addedAttr ;
		}
	}

	estimate(selectedEstimator, lowCont, highCont, firstFreeDiscSlot, firstFreeDiscSlot + addedAttr, bestType) ;

	eopt.binaryEvaluateNumericAttributes  = binaryBefore ; // restore value
	eopt.binaryEvaluation = binaryEvaluationBefore ;


	int iBin ;
	for (iDisc=discAttrFrom ; iDisc < discAttrTo; iDisc++)
	{
		estIdx = iDisc - discAttrFrom ;
		DiscEstimation[iDisc] = -DBL_MAX ;
		for (iBin=discFrom[estIdx] ; iBin < discTo[estIdx] ; iBin++)
			if (DiscEstimation[iBin] > DiscEstimation[iDisc])
				DiscEstimation[iDisc] = DiscEstimation[iBin] ;
	}

	if ( ! isMyopic(selectedEstimator )) {
		// for ReliefF & co. set estimate of numeric attribute as the best of its binary splits
		for (iCont=contAttrFrom ; iCont < contAttrTo; iCont++)
		{
			estIdx = iCont - contAttrFrom ;
			NumEstimation[iCont] = -DBL_MAX ;
			for (iBin=contFrom[estIdx] ; iBin < contTo[estIdx] ; iBin++)
				if (DiscEstimation[iBin] > NumEstimation[iCont])
					NumEstimation[iCont] = DiscEstimation[iBin] ;
		}
	}
}



//************************************************************
//
//                        discretizeEqualFrequency
//                        -----------------------
//
//     discretize numeric attribute with a fixed number of intervals
//        with approximately the same number of examples in each interval
//
//************************************************************
void estimation::discretizeEqualFrequency(int ContAttrIdx, int noIntervals, marray<double> &Bounds)
{
	Bounds.setFilled(0) ;

	marray<sortRec> sortedAttr(TrainSize) ;
	int j ;
	int OKvalues = 0 ;
	for (j=0 ; j < TrainSize ; j++)
	{
		if (isNAcont(NumValues(j, ContAttrIdx)))
			continue ;
		sortedAttr[OKvalues].key = NumValues(j, ContAttrIdx) ;
		sortedAttr[OKvalues].value = 1 ;  // later used as a counter for number of unique values
		OKvalues ++ ;
	}
	if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
	{
		// all values of the attribute are missing
		return  ;
	}
	sortedAttr.setFilled(OKvalues) ;
	sortedAttr.qsortAsc() ;

	// eliminate and count duplicates
	int lastUnique = 0 ;
	for (j=1 ; j < OKvalues ; j++)
	{
		if (sortedAttr[j].key != sortedAttr[lastUnique].key)
		{
			lastUnique ++ ;
			sortedAttr[lastUnique] = sortedAttr[j] ;
		}
		else
			sortedAttr[lastUnique].value ++ ;
	}
	sortedAttr.setFilled(lastUnique+1) ;

	// value lastUnique equals upper bound of the array, the actual number of unique values is 1 larger

	if (lastUnique < 1)
	{
		// all the cases have missing value of the attribute or only one OK
		return  ;
	}
	if (lastUnique < noIntervals)
	{
		// all unique values should form boundaries)

		Bounds.create(lastUnique) ;
		Bounds.setFilled(lastUnique) ;
		for (j=0 ; j < lastUnique ; j++)
			Bounds[j] = (sortedAttr[j].key + sortedAttr[j+1].key)/2.0 ;
		return ;
	}

	Bounds.create(noIntervals-1) ;

	int noDesired = int(ceil(double(OKvalues) / noIntervals)) ;
	double boundry ;

	int grouped = 0 ;
	for (j = 0 ; j < lastUnique ; j++)
	{
		if (grouped + sortedAttr[j].value < noDesired)
			grouped += sortedAttr[j].value ;
		else {
			// form new boundry
			boundry = (sortedAttr[j].key + sortedAttr[j+1].key) / 2.0 ;
			Bounds.addEnd(boundry) ;
			grouped = 0 ;
		}
	}
}

//************************************************************
//
//                        discretizeEqualWidth
//                        -----------------------
//
//     discretize numeric attribute with a fixed number of intervals of equal width
//
//************************************************************
void estimation::discretizeEqualWidth(int ContAttrIdx, int noIntervals, marray<double> &Bounds)
{
	Bounds.setFilled(0) ;

	int j=0 ;
	while (j < TrainSize && isNAcont(NumValues(j, ContAttrIdx)))
		j++ ;
	if (j == TrainSize)
		return ; // all values are missing
	double value, minVal, maxVal ;
	minVal = maxVal = NumValues(j, ContAttrIdx) ;
	for (++j ; j < TrainSize ; j++)
	{
		value = NumValues(j, ContAttrIdx) ;
		if (isNAcont(value))
			continue ;
		else if (value < minVal)
			minVal = value ;
		else if (value > maxVal)
			maxVal = value ;
	}
	if (minVal == maxVal)    //  only one non missing value
		return  ;
    double intervalWidth = (maxVal - minVal) / noIntervals ;
	Bounds.create(noIntervals-1) ;

	for (int i = 1 ; i < noIntervals ; i++)
	{
		value = minVal + i * intervalWidth ;
		Bounds.addEnd(value) ;
	}
}

