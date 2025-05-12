#include <stdio.h>
#include <stdlib.h>
#include "OPF.h"

void c_txt2opf(int *argc, char **argv)
{
	errorOccurred = 0;
	if (*argc != 3)
	{
		REprintf("\nusage txt2opf <P1> <P2>\n");
		REprintf("\nP1: input file name in the OPF ASCII format");
		REprintf("\nP2: output file name in the OPF binary format\n");
		return;
	}

	Rprintf("\nProgram to convert files written in the OPF ASCII format to the OPF binary format.");

	FILE *fpIn = NULL, *fpOut = NULL;
	int n, nfeats, nclasses, i, j, label;
	int id;
	float aux;

	fpIn = fopen(argv[1], "r");
	fpOut = fopen(argv[2], "wb");

	/*writing the number of samples*/
	if (fscanf(fpIn, "%d", &n) != 1)
	{
		REprintf("\n Could not read number of samples");
		return;
	}
	Rprintf("\n number of samples: %d", n);
	fwrite(&n, sizeof(int), 1, fpOut);

	/*writing the number of classes*/
	if (fscanf(fpIn, "%d", &nclasses) != 1)
	{
		REprintf("\n Could not read number of classes \n");
		return;
	}

	Rprintf("\n number of classes: %d", nclasses);
	fwrite(&nclasses, sizeof(int), 1, fpOut);

	/*writing the number of features*/
	if (fscanf(fpIn, "%d", &nfeats) != 1)
	{
		REprintf("\n Could not read number of features \n");
		return;
	}

	Rprintf("\n number of features: %d", nfeats);
	fwrite(&nfeats, sizeof(int), 1, fpOut);

	/*writing data*/
	for (i = 0; i < n; i++)
	{
		if (fscanf(fpIn, "%d", &id) != 1)
		{
			REprintf("\n Could not read sample id at line %d \n", i);
			return;
		}
		fwrite(&id, sizeof(int), 1, fpOut);

		if (fscanf(fpIn, "%d", &label) != 1)
		{
			REprintf("\n Could not read sample label at line %d \n", i);
			return;
		}
		fwrite(&label, sizeof(int), 1, fpOut);

		for (j = 0; j < nfeats; j++)
		{
			if (fscanf(fpIn, "%f", &aux) != 1)
			{
				REprintf("\n Could not read sample features at line %d, feature %d  \n", i, j);
				return;
			}

			fwrite(&aux, sizeof(float), 1, fpOut);
		}
	}

	fclose(fpIn);
	fclose(fpOut);
}
