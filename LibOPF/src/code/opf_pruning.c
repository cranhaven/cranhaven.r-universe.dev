#include "OPF.h"

void c_opf_pruning(int *argc, char **argv)
{
	errorOccurred = 0;	
	opf_PrecomputedDistance = 0;
	if ((*argc != 5) && (*argc != 4))
	{
		REprintf("\nusage opf_pruning <P1> <P2> <P3> <P4>");
		REprintf("\nP1: training set in the OPF file format");
		REprintf("\nP2: evaluating set in the OPF file format");
		REprintf("\nP3: percentage of accuracy [0,1]");
		REprintf("\nP4: precomputed distance file (leave it in blank if you are not using this resource\n");
		return;
	}

	int n, i, isize, fsize;
	float time, desiredAcc = atof(argv[3]), prate;
	char fileName[4096];
	FILE *f = NULL;
	timer tic, toc;

	if (*argc == 5)
		opf_PrecomputedDistance = 1;
	Rprintf("\nReading data files ...");
	
	Subgraph *gTrain = ReadSubgraph(argv[1]), *gEval = ReadSubgraph(argv[2]); if(errorOccurred) return;
	Rprintf(" OK");
	

	if (opf_PrecomputedDistance){
		opf_DistanceValue = opf_ReadDistances(argv[4], &n); if(errorOccurred) return;
	}

	isize = gTrain->nnodes;
	Rprintf("\nPruning training set ...");
	
	gettimeofday(&tic, NULL);
	opf_OPFPruning(&gTrain, &gEval, desiredAcc); if(errorOccurred) return;
	gettimeofday(&toc, NULL);
	Rprintf(" OK");
	
	fsize = gTrain->nnodes;

	prate = (1 - fsize / (float)isize) * 100;
	Rprintf("\nFinal pruning rate: %.2f%%", prate);
	

	Rprintf("\n\nWriting classifier's model file ...");
	
	snprintf(fileName, 4096, "%s.classifier.opf", argv[1]);
	opf_WriteModelFile(gTrain, fileName);
	Rprintf(" OK");
	
	Rprintf(" OK");
	
	snprintf(fileName, 4096, "%s.prate.pr", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f\n", prate);
	fclose(f);

	time = ((toc.tv_sec - tic.tv_sec) * 1000.0 + (toc.tv_usec - tic.tv_usec) * 0.001) / 1000.0;
	Rprintf("\nPruning time: %f seconds\n", time);
	
	snprintf(fileName, 4096, "%s.time", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f\n", time);
	fclose(f);

	Rprintf("\nDeallocating memory ...");
	DestroySubgraph(&gTrain);
	DestroySubgraph(&gEval);
	if (opf_PrecomputedDistance)
	{
		for (i = 0; i < n; i++)
			free(opf_DistanceValue[i]);
		free(opf_DistanceValue);
	}
	Rprintf(" OK\n");
}
