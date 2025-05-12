#include "OPF.h"
#include <stdio.h>

void CheckInputData(float TrPercentage, float EvalPercentage, float TestPercentage)
{
	Rprintf("\nSummation of set percentages = %.1f ...", TrPercentage + EvalPercentage + TestPercentage);
	if ((float)(TrPercentage + EvalPercentage + TestPercentage) != (float)1.0){
		Error("Percentage summation is not equal to 1", "CheckInputData"); return;
	}
	Rprintf(" OK");

	Rprintf("\nChecking set percentages ...");
	if (TrPercentage == 0.0f || TestPercentage == 0.0f){
		Error("Percentage of either training set or test set is equal to 0", "CheckInputData"); return;
	}
	Rprintf(" OK");
}

void c_opf_split(int *argc, char **argv)
{
	errorOccurred = 0;	

	if (*argc != 6)
	{
		REprintf("\nusage opf_split <P1> <P2> <P3> <P4> <P5>");
		REprintf("\nP1: input dataset in the OPF file format");
		REprintf("\nP2: percentage for the training set size [0,1]");
		REprintf("\nP3: percentage for the evaluation set size [0,1] (leave 0 in the case of no learning)");
		REprintf("\nP4: percentage for the test set size [0,1]");
		REprintf("\nP5: normalize features? 1 - Yes  0 - No\n\n");
		return;
	}
	Subgraph *g = NULL, *gAux = NULL, *gTraining = NULL, *gEvaluating = NULL, *gTesting = NULL;
	float training_p = atof(argv[2]), evaluating_p = atof(argv[3]), testing_p = atof(argv[4]);
	int normalize = atoi(argv[5]);
	char fileName[4096];

	CheckInputData(training_p, evaluating_p, testing_p); if(errorOccurred) return;

	Rprintf("\nReading data set ...");
	
	g = ReadSubgraph(argv[1]); if(errorOccurred) return;
	Rprintf(" OK");
	

	if (normalize){
		opf_NormalizeFeatures(g); if(errorOccurred) return;
	}

	Rprintf("\nSplitting data set ...");
	
	opf_SplitSubgraph(g, &gAux, &gTesting, training_p + evaluating_p); if(errorOccurred) return;

	if (evaluating_p > 0){
		opf_SplitSubgraph(gAux, &gTraining, &gEvaluating, training_p / (training_p + evaluating_p)); if(errorOccurred) return;
	}
	else{
		gTraining = CopySubgraph(gAux); if(errorOccurred) return;
	}

	Rprintf(" OK");
	

	Rprintf("\nWriting data sets to disk ...");
	
	snprintf(fileName, 4096, "%s.training.dat", argv[1]);
	WriteSubgraph(gTraining, fileName); if(errorOccurred) return;
	if (evaluating_p > 0){
		snprintf(fileName, 4096, "%s.evaluating.dat", argv[1]);
		WriteSubgraph(gEvaluating, fileName); if(errorOccurred) return;
	}
	snprintf(fileName, 4096, "%s.testing.dat", argv[1]);
	WriteSubgraph(gTesting, fileName); if(errorOccurred) return;
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	DestroySubgraph(&g);
	DestroySubgraph(&gAux);
	DestroySubgraph(&gTraining);
	DestroySubgraph(&gEvaluating);
	DestroySubgraph(&gTesting);
	Rprintf(" OK\n");
}
