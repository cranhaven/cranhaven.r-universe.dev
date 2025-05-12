#include "OPF.h"
#include <stdio.h>
#include <time.h>

void c_opf_info(int *argc, char **argv)
{
	errorOccurred = 0;	

	if (*argc != 2)
	{
		REprintf("\nusage opf_info <P1>");
		REprintf("\nP1: OPF file\n");
		return;
	}

	Subgraph *g = NULL;
	FILE *fp = NULL;
	int ndata, nfeats, nlabels;
	char msg[4096];

	if ((fp = fopen(argv[1], "rb")) == NULL)
	{
		snprintf(msg, 4096, "Unable to open file %s", argv[1]);
		Error(msg, "opf_info"); return;
	}

	if (fread(&ndata, sizeof(int), 1, fp) != 1)
	{
		REprintf("\n Could not read number of samples");
		return;
	}
	if (fread(&nlabels, sizeof(int), 1, fp) != 1)
	{
		REprintf("\n Could not read number of labels");
		return;
	}

	if (fread(&nfeats, sizeof(int), 1, fp) != 1)
	{
		REprintf("\n Could not read number of features");
		return;
	}

	Rprintf("\nInformations about %s file\n --------------------------------", argv[1]);
	Rprintf("\nData size: %d", ndata);
	Rprintf("\nFeatures size: %d", nfeats);
	Rprintf("\nLabels number: %d", nlabels);
	Rprintf("\n--------------------------------\n");

	DestroySubgraph(&g);
	fclose(fp);
}
