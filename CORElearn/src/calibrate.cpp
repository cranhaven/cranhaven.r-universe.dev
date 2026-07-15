/*
 * calibrate.cpp
 *
 *  Created on: Mar 31, 2009
 *      Author: rmarko
 */

#include <cfloat>

#include "general.h"
#include "contain.h"
#include "error.h"
#include "utils.h"
#include "dataStore.h"
#include "calibrate.h"

using namespace std ;

double Calibrate::cal(double p) {
	// find appropriate with bisection. the highest interval entry has to be the sentinel
	if (p >= interval[interval.len()-1])
		return calProb[interval.len()-1];

	int lower = 0;
	int upper = interval.len() - 1;
	int middle;

	while (lower <= upper) {
		middle = (lower + upper) / 2;
		if (p > interval[middle])
			lower = middle + 1;
		else
			upper = middle - 1;
	}
	// p is smaller than calProb[lower]
	return calProb[lower];
}

void Calibrate::calFromTo(int from, int to, marray<double> &p) {
	for (int i = from; i <= to; i++)
		if (p[i] > 0)
			p[i] = cal(p[i]);
}

void Calibrate::isoRegCal(marray<sort3Rec> &y) {
	sortAndUnify(y);

	// based on code for isotonic regression by  Lutz Duembgen, 23.02.2000 written for MathCad
	int j, n = y.filled() - 1;
	double nw;
	marray<double> weight(y.filled(),0);
	marray<int> index(y.filled(),0);
	// An interval of indices is represented by its left endpoint
	// ("index") and its "weight" (sum of w's).
	marray<double> ghat(y.filled(),0);
	int ci = 0;
	index[ci] = 0;
	weight[ci] = y[0].weight;
	ghat[ci] = y[0].value;
	// ci is the number of the interval considered currently.
	// ghat[ci] is the mean of y-values within this interval.
	for (j = 1; j <= n; j++) {
		// a new index interval, {j}, is created:
		ci = ci + 1;
		index[ci] = j;
		weight[ci] = y[j].weight;
		ghat[ci] = y[j].value;
		while (ci >= 1 && ghat[ci - 1] >= ghat[ci]) {
			// "pair adjacent violators":
			nw = weight[ci - 1] + weight[ci];
			ghat[ci - 1] = ghat[ci - 1] + (weight[ci] / nw) * (ghat[ci] - ghat[ci - 1]);
			weight[ci - 1] = nw;
			--ci;
		}
	}
	interval.create(ci + 1);
	calProb.create(ci + 1);
	w.create(ci + 1);
	//% Now define ghat for all indices:
	/*
            while (n >= 1) {
              for (j=index[ci]; j <=n ; j++)
                ghat[j] = ghat[ci];
                n = index[ci]-1;
                interval[ci] = y[index[ci]].key ;
                value[ci] = ghat[ci] ;
                ci = ci-1;
            }
	 */
	// shift by 1 position
	interval[ci] = y[index[ci]].key ; // 1.0;  sentinel
	while (ci > 0) {
		calProb[ci] = ghat[ci];
		w[ci] = weight[ci];
		//interval[ci-1] = y[index[ci]].key ;
		interval[ci - 1] = y[index[ci] - 1].key + (y[index[ci]].key - y[index[ci] - 1].key) * weight[ci - 1] / (weight[ci] + weight[ci - 1]);
		--ci;
	}
	calProb[ci] = ghat[ci]; // for position 0
	w[ci] = weight[ci];
}
// expects y filled with predictions(key) and actual scores(value), weight reflect number of such instances (usually 1)
void Calibrate::binIsoCal(marray<sort3Rec> &y, int noInitialBins) {
	binningCal(y, noInitialBins);
	marray<sort3Rec> yb (interval.len());
	sort3Rec sr;
	for (int b = 0; b < interval.len(); ++b) {
		sr.weight = w[b];
		sr.key = interval[b];
		sr.value = calProb[b];
		yb.addEnd(sr);
	}
	yb.setFilled(interval.len()) ;
	isoRegCal(yb);
}
void Calibrate::binningCal(marray<sort3Rec> &y, int noBins) {
	sortAndUnify(y);
	mergeConsequtiveTrue(y);
	int i;

	if ( y.filled() < noBins) {
		// degenerated case
		interval.create(y.filled());
		calProb.create(y.filled());
		w.create(y.filled()) ;
		for (i=0 ; i < y.filled() ; i++) {
			if (i==y.filled()-1)
				interval[y.filled()-1] = y[i].key ; // 1.0;   sentinel
			else // set interval to weighted middle point to next
				interval[i] = y[i].key + (y[i+1].key - y[i].key) * y[i].weight / (y[i].weight + y[i+1].weight);
			calProb[i] = y[i].value ;
			w[i] = y[i].weight ;
		}
		return ;
	}
	double totalWeight = 0.0;
	for (i = 0; i < y.filled(); i++)
		totalWeight += y[i].weight;
	double expBinWeight = totalWeight / noBins;
	interval.create(noBins);
	calProb.create(noBins, 0);
	marray<double> binWeight(noBins, 0), actualSum(noBins, 0);
	int binIdx = 0;
	marray<int> binBorder(noBins, -1);
	double  allBinsSum=0.0;
	double distToBorder ;
	// tries to set bins exactly on delimiters
	for (i = 0; i < y.filled(); i++) {
		binWeight[binIdx] += y[i].weight;
		calProb[binIdx] += y[i].value * y[i].weight;
		distToBorder = expBinWeight * (binIdx+1) - allBinsSum -binWeight[binIdx] ;
		if (distToBorder <= epsilon) { // instead of zero, which may not be reached for the last bin
			// decide in which bin to put the last one, note that distToBorder is negative
			if (-distToBorder <= y[i].weight / 2 || fabs(binWeight[binIdx] - y[i].weight)< epsilon) {
				// keep it where it is
				if (i<y.filled()-1)
					// set interval to weighted middle point to next
					interval[binIdx] = y[i].key + (y[i+1].key - y[i].key) * y[i].weight / (y[i].weight + y[i+1].weight);
				else
					interval[binIdx] = y[i].key;
			}
			else { //move last to the next bin
				calProb[binIdx] -= y[i].value * y[i].weight;
				calProb[binIdx+1] = y[i].value * y[i].weight;
				binWeight[binIdx] -= y[i].weight;
				binWeight[binIdx+1] = y[i].weight;
				//interval[binIdx] = y[i-1].key;
				// set interval to weighted middle point to next
				interval[binIdx] = y[i-1].key + (y[i].key - y[i-1].key) * y[i-1].weight / (y[i].weight + y[i - 1].weight);
			}
			allBinsSum += binWeight[binIdx];
			calProb[binIdx] /= binWeight[binIdx] ;
			++binIdx;
		}
	}
	// the last one was not computed yet
	interval[noBins - 1] =  y[y.filled()-1].key; // 1.0;  // sentinel
	w = binWeight;
}

// expects y filled with predictions(key) and actual scores(value), weight reflect number of such instances (usually 1)
void Calibrate::mergeCal(marray<sort3Rec> &yi, int noInitialBins) {
	sortAndUnify(yi); //merges the same by prediction(key)
	mergeConsequtiveTrue(yi);
	// call isotonic regression
	isoRegCal(yi);
	marray<sort3Rec> y (interval.len());
	sort3Rec sr;
	for (int b = 0; b < interval.len(); ++b) {
		sr.weight = w[b];
		sr.key = interval[b];
		sr.value = calProb[b];
		y.addEnd(sr);
	}
	y.setFilled(interval.len()) ;

	int i, j;

	double gain = 0, maxGain;
	int bestI = 0, bestJ = 1;
	booleanT tryMerging = mTRUE;
	// slow algorithm, worst case is O(N^2), to speed up one needs different data structure (e.g. heap), which precomputes all possible mergers

	int binsLeft = y.filled()-1 ;
	while (tryMerging) {
		maxGain = -DBL_MAX;
		// find the most similar to merge
		i = 0;
		while (i < y.filled()) {
			// find next nonempty index
			j = i + 1;
			while (j < y.filled() && y[j].value == -DBL_MAX)
				j++;
			if (j >= y.filled()) {
				// merge best of this round
				if (maxGain < 0 || binsLeft <= noInitialBins) {
					tryMerging = mFALSE;
					break;
				}
				y[bestI].key = y[bestJ].key; // store margin
				y[bestI].value = (y[bestI].value * y[bestI].weight + y[bestJ].value * y[bestJ].weight)/(y[bestI].weight+y[bestJ].weight); // sum of ones
				y[bestI].weight += y[bestJ].weight; // sum of instances
				y[bestJ].value = -DBL_MAX;
				binsLeft-- ;
				break; // go to next iteration of merging
			}
			gain = mdlGain(y[i].value, y[i].weight, y[j].value, y[j].weight);
			if (gain > maxGain) {
				bestI = i;
				bestJ = j;
				maxGain = gain;
			}
			i = j; // this candidate we already have
		}
	}
	// delete empty cells
	int lastFull = 0;
	for (i = 1; i < y.filled(); i++) {
		if (y[i].value != -DBL_MAX) {
			// copy to next free position
			++lastFull;
			y[lastFull] = y[i];
		}
	}
	y.setFilled(lastFull+1);
	//y.RemoveRange(lastFull + 1, y.Count - lastFull - 1);
	//y.TrimExcess();

	// call isotonic regression on merged bins
	isoRegCal(y);
}
void Calibrate::writeCSV(char *calFileName) {
	FILE *fs ;
	if ((fs= fopen(calFileName, "w"))==NULL) {
		merror("Cannot create calibration file",calFileName);
		return ;
	}
	fprintf(fs,"interval,calProb,weight\n");
	for (int i = 0; i < interval.len(); i++)
		fprintf(fs,"%e,%e,%e\n", interval[i], calProb[i], w[i]);
}
void Calibrate::sortAndUnify(marray<sort3Rec> &y) {
	y.qsortAsc();
	// transorms to unique key array
	int i, first = 0;
	for (i = 1; i < y.filled(); i++)
		if (y[i].key != y[first].key) {
			// copy to next unique position
			++first;
			y[first] = y[i];
		}
		else {
			if (y[first].value == y[i].value)
				y[first].weight += y[i].weight;
			else {
				y[first].value = (y[first].weight * y[first].value + y[i].weight * y[i].value) / (y[first].weight + y[i].weight);
				y[first].weight += y[i].weight;
			}
		}
	y.setFilled(first+1);
	//y.RemoveRange(first + 1, y.Count - first - 1);
	//y.TrimExcess();
}
void Calibrate::mergeConsequtiveTrue(marray<sort3Rec> &y) {
	// merge the same by true value (value) and assumes different keys (predictions)
	int i, first = 0;
	for (i = 1; i < y.filled(); i++) {
		if (y[i].value != y[first].value) {
			// copy to next unique position
			++first;
			y[first] = y[i];
		}
		else {
			y[first].key = (y[first].key*y[first].weight + y[i].key*y[i].weight)/(y[first].weight+y[i].weight);  // store only the center of interval
			y[first].weight += y[i].weight;
		}
	}
	y.setFilled(first+1);
	//y.RemoveRange(first + 1, y.Count - first - 1);
	//y.TrimExcess();
}
double Calibrate::MDLimpurity2cl(double weight, double no1) {
	marray<double> Multinom(2);
	// encoding number of examples in each class
	Multinom[0] = no1;
	Multinom[1] = weight - no1;
	double MDL = multinomLog2(Multinom);

	// encoding prior decoder
	Multinom[0] = 1; // no classes -1
	Multinom[1] = weight;
	MDL += multinomLog2(Multinom);

	return MDL;
}
double Calibrate::mdlGain(double true1, double weight1, double true2, double weight2) {
	double mdlPrior = MDLimpurity2cl(weight1 + weight2, true1 * weight1 + true2 * weight2);
	double mdl1 = MDLimpurity2cl(weight1, true1 * weight1);
	double mdl2 = MDLimpurity2cl(weight2, true2 * weight2);
	return (mdlPrior - mdl1 - mdl2);
}
int Calibrate::getCalibration(marray<double> &intrvl, marray<double> &prob) {
	intrvl.copy(interval) ;
	prob.copy(calProb) ;
	return interval.len() ;
}


