#include "OPF.h"

void c_opfknn_classify(int *argc, char **argv)
{
	errorOccurred = 0;
	opf_PrecomputedDistance = 0;
	if ((*argc != 3) && (*argc != 2))
	{
		REprintf("\nusage opfknn_classify <P1> <P2>");
		REprintf("\nP1: test set in the OPF file format");
		REprintf("\nP2: precomputed distance file (leave it in blank if you are not using this resource\n");
		return;
	}

	int n, i;
	float time;
	char fileName[4096];
	FILE *f = NULL;
	timer tic, toc;

	if (*argc == 3)
		opf_PrecomputedDistance = 1;
	Rprintf("\nReading data files ...");
	
	snprintf(fileName, 4096, "%s.classifier.opf", argv[1]);
	Subgraph *gTest = ReadSubgraph(argv[1]), *gTrain = opf_ReadModelFile(fileName); if(errorOccurred) return;
	Rprintf(" OK");
	

	if (opf_PrecomputedDistance){
		opf_DistanceValue = opf_ReadDistances(argv[2], &n); if(errorOccurred) return;
	}

	Rprintf("\nClassifying test set ...");
	
	gettimeofday(&tic, NULL);
	opf_OPFknnClassify(gTrain, gTest); if(errorOccurred) return;
	gettimeofday(&toc, NULL);
	Rprintf(" OK");
	

	Rprintf("\nWriting output file ...");
	
	snprintf(fileName, 4096, "%s.out", argv[1]);
	opf_WriteOutputFile(gTest, fileName);
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	DestroySubgraph(&gTrain);
	DestroySubgraph(&gTest);
	if (opf_PrecomputedDistance)
	{
		for (i = 0; i < n; i++)
			free(opf_DistanceValue[i]);
		free(opf_DistanceValue);
	}
	Rprintf(" OK\n");

	time = ((toc.tv_sec - tic.tv_sec) * 1000.0 + (toc.tv_usec - tic.tv_usec) * 0.001) / 1000.0;
	Rprintf("\nTesting time: %f seconds\n", time);
	

	snprintf(fileName, 4096, "%s.time", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f\n", time);
	fclose(f);
}
