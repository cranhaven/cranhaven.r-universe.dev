#include "OPF.h"
#include <stdio.h>

void c_opf_distance(int *argc, char **argv)
{
	errorOccurred = 0;	

	if (*argc != 4)
	{
		REprintf("\nusage opf_distance <P1> <P2> <P3>");
		REprintf("\nP1: Dataset in the OPF file format");
		REprintf("\nP2: Distance ID\n");
		REprintf("\n	1 - Euclidean");
		REprintf("\n	2 - Chi-Square");
		REprintf("\n	3 - Manhattan (L1)");
		REprintf("\n	4 - Canberra");
		REprintf("\n	5 - Squared Chord");
		REprintf("\n	6 - Squared Chi-Squared");
		REprintf("\n	7 - BrayCurtis");
		REprintf("\nP3: Distance normalization? 1- yes 0 - no");
		return;
	}

	Subgraph *sg = ReadSubgraph(argv[1]); if(errorOccurred) return;
	char fileName[4096];
	snprintf(fileName, 4096, "%s.distances.dat", argv[1]);
	FILE *fp = fopen(fileName, "wb"); //Changed to send to tempdir
	int i, j, distance = atoi(argv[2]), normalize = atoi(argv[3]);
	float **Distances = NULL, max = -FLT_MAX;

	fwrite(&sg->nnodes, sizeof(int), 1, fp);

	Distances = (float **)malloc(sg->nnodes * sizeof(float *));
	for (i = 0; i < sg->nnodes; i++)
		Distances[i] = (float *)malloc(sg->nnodes * sizeof(int));

	switch (distance)
	{
	case 1:
		Rprintf("\n	Computing euclidean distance ...");
		for (i = 0; i < sg->nnodes; i++)
		{
			for (j = 0; j < sg->nnodes; j++)
			{
				if (i == j)
					Distances[i][j] = 0.0;
				else
					Distances[sg->node[i].position][sg->node[j].position] = opf_EuclDist(sg->node[i].feat, sg->node[j].feat, sg->nfeats);
				if (Distances[sg->node[i].position][sg->node[j].position] > max)
					max = Distances[sg->node[i].position][sg->node[j].position];
			}
		}
		break;
	case 2:
		Rprintf("\n	Computing chi-square distance ...\n");
		for (i = 0; i < sg->nnodes; i++)
		{
			for (j = 0; j < sg->nnodes; j++)
			{
				if (i == j)
					Distances[i][j] = 0.0;
				else
					Distances[sg->node[i].position][sg->node[j].position] = opf_ChiSquaredDist(sg->node[i].feat, sg->node[j].feat, sg->nfeats);
				if (Distances[sg->node[i].position][sg->node[j].position] > max)
					max = Distances[sg->node[i].position][sg->node[j].position];
			}
		}
		break;
	case 3:
		Rprintf("\n	Computing Manhattan distance ...\n");
		for (i = 0; i < sg->nnodes; i++)
		{
			for (j = 0; j < sg->nnodes; j++)
			{
				if (i == j)
					Distances[i][j] = 0.0;
				else
					Distances[sg->node[i].position][sg->node[j].position] = opf_ManhattanDist(sg->node[i].feat, sg->node[j].feat, sg->nfeats);
				if (Distances[sg->node[i].position][sg->node[j].position] > max)
					max = Distances[sg->node[i].position][sg->node[j].position];
			}
		}
		break;
	case 4:
		Rprintf("\n	Computing Canberra distance ...\n");
		for (i = 0; i < sg->nnodes; i++)
		{
			for (j = 0; j < sg->nnodes; j++)
			{
				if (i == j)
					Distances[i][j] = 0.0;
				else
					Distances[sg->node[i].position][sg->node[j].position] = opf_CanberraDist(sg->node[i].feat, sg->node[j].feat, sg->nfeats);
				if (Distances[sg->node[i].position][sg->node[j].position] > max)
					max = Distances[sg->node[i].position][sg->node[j].position];
			}
		}
		break;
	case 5:
		Rprintf("\n	Computing Squared Chord distance ...\n");
		for (i = 0; i < sg->nnodes; i++)
		{
			for (j = 0; j < sg->nnodes; j++)
			{
				if (i == j)
					Distances[i][j] = 0.0;
				else
					Distances[sg->node[i].position][sg->node[j].position] = opf_SquaredChordDist(sg->node[i].feat, sg->node[j].feat, sg->nfeats);
				if (Distances[sg->node[i].position][sg->node[j].position] > max)
					max = Distances[sg->node[i].position][sg->node[j].position];
			}
		}
		break;
	case 6:
		Rprintf("\n	Computing Squared Chi-squared distance ...\n");
		for (i = 0; i < sg->nnodes; i++)
		{
			for (j = 0; j < sg->nnodes; j++)
			{
				if (i == j)
					Distances[i][j] = 0.0;
				else
					Distances[sg->node[i].position][sg->node[j].position] = opf_SquaredChiSquaredDist(sg->node[i].feat, sg->node[j].feat, sg->nfeats);
				if (Distances[sg->node[i].position][sg->node[j].position] > max)
					max = Distances[sg->node[i].position][sg->node[j].position];
			}
		}
		break;
	case 7:
		Rprintf("\n	Computing Bray Curtis distance ...\n");
		for (i = 0; i < sg->nnodes; i++)
		{
			for (j = 0; j < sg->nnodes; j++)
			{
				if (i == j)
					Distances[i][j] = 0.0;
				else
					Distances[sg->node[i].position][sg->node[j].position] = opf_BrayCurtisDist(sg->node[i].feat, sg->node[j].feat, sg->nfeats);
				if (Distances[sg->node[i].position][sg->node[j].position] > max)
					max = Distances[sg->node[i].position][sg->node[j].position];
			}
		}
		break;
	default:
		REprintf("\nInvalid distance ID ...\n");
	}

	if (!normalize)
		max = 1.0;
	for (i = 0; i < sg->nnodes; i++)
	{
		for (j = 0; j < sg->nnodes; j++)
		{
			Distances[i][j] /= max;
			fwrite(&Distances[i][j], sizeof(float), 1, fp);
		}
	}

	Rprintf("\n\nDistances generated ...\n");
	
	Rprintf("\n\nDeallocating memory ...\n");
	for (i = 0; i < sg->nnodes; i++)
		free(Distances[i]);
	free(Distances);

	DestroySubgraph(&sg);
	fclose(fp);
}
