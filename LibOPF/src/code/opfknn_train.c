#include "OPF.h"

void c_opfknn_train(int *argc, char **argv)
{
	errorOccurred = 0;
	opf_PrecomputedDistance = 0;

	if ((*argc != 5) && (*argc != 4))
	{
		REprintf("\nusage opf_train <P1> <P2>");
		REprintf("\nP1: training set in the OPF file format");
		REprintf("\nP2: evaluating set in the OPF file format (used to learn k)");
		REprintf("\nP3: kmax");
		REprintf("\nP4: precomputed distance file (leave it in blank if you are not using this resource)\n");
		return;
	}

	int n, i, kmax = atoi(argv[3]);
	char fileName[4096];
	FILE *f = NULL;
	timer tic, toc;
	double time;

	if (*argc == 5)
		opf_PrecomputedDistance = 1;

	Rprintf("\nReading data file ...");
	
	Subgraph *Train = ReadSubgraph(argv[1]); if(errorOccurred) return;
	Subgraph *Eval = ReadSubgraph(argv[2]); if(errorOccurred) return;
	Rprintf(" OK");
	

	if (opf_PrecomputedDistance){
		opf_DistanceValue = opf_ReadDistances(argv[4], &n); if(errorOccurred) return;
	}

	Rprintf("\nTraining OPF classifier ...");
	
	gettimeofday(&tic, NULL);
	opf_OPFknnTraining(Train, Eval, kmax); if(errorOccurred) return;
	gettimeofday(&toc, NULL);
	Rprintf(" OK");
	

	Rprintf("\nWriting classifier's model file ...");
	
	snprintf(fileName, 4096, "%s.classifier.opf", argv[1]);
	opf_WriteModelFile(Train, fileName);
	Rprintf(" OK");
	

	Rprintf("\nWriting output file ...");
	
	snprintf(fileName, 4096, "%s.out", argv[1]);
	opf_WriteOutputFile(Train, fileName);
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	
	DestroySubgraph(&Train);
	DestroySubgraph(&Eval);
	if (opf_PrecomputedDistance)
	{
		for (i = 0; i < n; i++)
			free(opf_DistanceValue[i]);
		free(opf_DistanceValue);
	}
	Rprintf(" OK\n");

	time = ((toc.tv_sec - tic.tv_sec) * 1000.0 + (toc.tv_usec - tic.tv_usec) * 0.001) / 1000.0;
	Rprintf("\nTraining time: %f seconds\n", time);
	

	snprintf(fileName, 4096, "%s.time", argv[1]);
	f = fopen(fileName, "a");
	fprintf(f, "%f\n", time);
	fclose(f);
}
