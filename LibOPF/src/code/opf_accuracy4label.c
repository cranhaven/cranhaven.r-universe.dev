#include "OPF.h"

void c_opf_accuracy4label(int *argc, char **argv)
{
	errorOccurred = 0;	

	if (*argc != 2)
	{
		REprintf("\nusage opf_accuracyforlabel <P1>");
		REprintf("\nP1: data set in the OPF file format\n");
		return;
	}

	int i;
	float *Acc = NULL;
	FILE *f = NULL;
	char fileName[4096];

	Rprintf("\nReading data file ...");
	
	Subgraph *g = ReadSubgraph(argv[1]); if(errorOccurred) return;
	Rprintf(" OK");
	

	Rprintf("\nReading output file ...");
	
	snprintf(fileName, 4096, "%s.out", argv[1]);
	f = fopen(fileName, "r");
	if (!f)
	{
		REprintf("\nunable to open file %s", argv[2]);
		return;
	}

	for (i = 0; i < g->nnodes; i++)
	{
		if (fscanf(f, "%d", &g->node[i].label) != 1)
		{
			Error("Error reading node label", "opf_Accuracy"); return;
		}
	}
	fclose(f);
	Rprintf(" OK");
	

	Rprintf("\nComputing accuracy ...");
	
	Acc = opf_Accuracy4Label(g); if(errorOccurred) return;
	for (i = 1; i <= g->nlabels; i++)
		Rprintf("\nClass %d: %.2f%%", i, Acc[i] * 100);
	

	Rprintf("\nWriting accuracy in output file ...");
	
	snprintf(fileName, 4096, "%s.acc", argv[1]);
	f = fopen(fileName, "a");
	for (i = 1; i <= g->nlabels; i++)
	{
		fprintf(f, "%f", Acc[i] * 100);
		fprintf(f, "\n");
	}
	fprintf(f, "\n");
	fclose(f);
	Rprintf(" OK");
	

	Rprintf("\nDeallocating memory ...");
	
	DestroySubgraph(&g);
	Rprintf(" OK\n");

	free(Acc);
}
