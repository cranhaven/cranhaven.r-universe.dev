/*
 * calibrate.h
 *
 *  Created on: Mar 31, 2009
 *      Author: rmarko
 */

#if !defined(CALIBRATE_H)
#define CALIBRATE_H

#include "general.h"
#include "contain.h"
#include "utils.h"

class Calibrate {

	static void sortAndUnify(marray<sort3Rec> &y);
	static void mergeConsequtiveTrue(marray<sort3Rec> &y);
	static double MDLimpurity2cl(double weight, double no1);
	static double mdlGain(double true1, double weight1, double true2,
			double weight2);

public:
	marray<double> interval, calProb, w;

	Calibrate() {}
	double cal(double p);
	void calFromTo(int From, int To, marray<double> &p);
	void isoRegCal(marray<sort3Rec> &y);
	void binIsoCal(marray<sort3Rec> &y, int noInitialBins);
	void binningCal(marray<sort3Rec> &y, int noBins);
	void mergeCal(marray<sort3Rec> &y, int noInitialBins);
	void writeCSV(char* calFileName);
	void readCSV(char* calFileName);
	int getCalibration(marray<double> &intrvl, marray<double> &prob);
};

#endif /* CALIBRATE_H */

