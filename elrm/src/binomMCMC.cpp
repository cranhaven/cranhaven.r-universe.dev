// binomMCMC.cpp 
//

#include "functions.h"
#include <time.h>
#include <math.h>

// weightings for v
double* weightings;

const int SIZE = 500;

int* sequence;

// Function that finds the next v.
// Depends on xMatrix, mMatrix and v (seed set R).
Matrix nextV(Matrix xMatrix, Matrix mMatrix, Array2D<int> v, int r)
{
	Matrix temp((int)xMatrix.RowNo(),1,0);
	int i,j,index = 0;
	double crossproduct = 0;

	int* sample = new int[r];

	bool valid;

	do
	{
		valid = true;
		
		// double runif = (float) rand()/RAND_MAX;
		double r_unif = runif(0,1);
		
		double acc = 0;
		
		if(r_unif == 1)
		{
			index = v.m - 1;
		}
		else
		{
			for(i = 0; i < v.m; i++)
			{
				acc += weightings[i];
				
				if(r_unif <= acc)
				{
					index = i;
					
					break;
				}
			}
		}
		
		rsample(sample, sequence, r, mMatrix.RowNo());

		for(i = 0; i < r; i++)
		{
			if(abs(v.data[index][i]) > mMatrix(sample[i],0))
			{
				valid = false;

				break;
			}
		}
		
		if(valid)
		{
			for(i = 0; i < (int)xMatrix.ColNo(); i++)
			{
				crossproduct = 0;
				
				for(j = 0; j < r; j++)
				{
					crossproduct += (xMatrix(sample[j],i)*v.data[index][j]);
				}
				
				if(crossproduct != 0)
				{
					valid = false;
					
					break;
				}
			}
		}
	}
	while(!valid);

/* 	for(i = 0; i < (int)mMatrix.RowNo(); i++)
	{
		temp(i,0) = 0;
	} */

	for(i = 0; i < r; i++)
	{
		temp(sample[i],0) = v.data[index][i];
	}

	delete [] sample;

	return temp;
}

// Function that obtains a legal value of d.
// Depends on yCurrent, mMatrix, and v (the seed set R).
int getd(Matrix v, Matrix mMatrix, Matrix yCurrent)
{
	List<int> dList;
	List<double> probList;
	
	bool flag = true;

	int i = 0;
		
	int d = 0;

	double p, calc = 0;

	double max_p = 0;

	do
	{
		p = 0;

		for(i = 0; i < (int)mMatrix.RowNo(); i++)
		{
			calc = (int)(yCurrent(i,0) + d*v(i,0));
			
			if(calc <= (int)mMatrix(i,0) && calc >= 0)
			{
				p += log(nCk(mMatrix(i,0),yCurrent(i,0) + d*v(i,0)));
			}
			else
			{
				flag = false;

				break;
			}
		}

		if(flag)
		{
			probList.add(p);
			dList.add(d);
			d++;

			max_p = max(p,max_p);
		}
	}
	while(flag);

	flag = true;

	d = -1;

	do
	{
		p = 0;

		for(i = 0; i < (int)mMatrix.RowNo(); i++)
		{
			calc = (int)(yCurrent(i,0) + d*v(i,0));

			if(calc <= (int)mMatrix(i,0) && calc >= 0)
			{
				p += log(nCk(mMatrix(i,0),yCurrent(i,0) + d*v(i,0)));
			}
			else
			{
				flag = false;

				break;
			}
		}

		if(flag)
		{
			probList.add(p);
			dList.add(d);
			d--;

			max_p = max(p,max_p);
		}
	}
	while(flag);

	for(i = 0; i < probList.getSize(); i++)
	{
		probList.set(probList.get(i)-max_p,i);
	}

	double sum = 0;

	for(i = 0; i < probList.getSize(); i++)
	{
		sum += exp(probList.get(i));
	}

	for(i = 0; i < probList.getSize(); i++)
	{
		probList.set(exp(probList.get(i))/sum,i);
	}

	// double r_unif = (double)rand() / ((double)(RAND_MAX)+(double)(1));
	double r_unif = runif(0,1);

	double acc = 0;

	for(i = 0; i < probList.getSize(); i++)
	{
		acc += probList.get(i);

		if(r_unif < acc)
		{
			d = dList.get(i);

			break;
		}
	}
	
	dList.clear();
	probList.clear();

	return d;
}

// Function that writes the Markov Chain to a file.
void writeToFile(ofstream & outfile, Matrix* sample, int n)
{	
	if(outfile.is_open())
	{
		for(int i = 0; i < n; i++)
		{
			outfile << ~sample[i];
		}
	}
}


// Function that writes the Markov Chain to a file.
void writeToFile(ofstream & outfile, Matrix sample)
{	
	if(outfile.is_open())
	{
		outfile << sample;
	}
}
  
// Function called by R to produce the Markov chain.
// Markov Chain is written to a file.
extern "C" void MCMC(int* yColumn, int* mColumn, int* xCols, int* n1, int* zCols, int* n2, int* r, char** datafilename, char** outfilename, int* num_iterations, int* keepObservedSufficientStat)
{	
	// srand((unsigned int)time(NULL));

	// entire data file
	List<List<double>*> dataset;

	// array of combinations of legal v's
	Array2D<int> v;

	// user input variables
	List<int> xColumns;
	List<int> zColumns;

	int i, j = 0;

	for(i = 0; i < *n1; i++)
	{
		xColumns.add(xCols[i]);
	}

	for(i = 0; i < *n2; i++)
	{
		zColumns.add(zCols[i]); 
	}

	dataset = readDataFile(*datafilename);

	Matrix dMatrix(dataset.getSize(), dataset.get(0)->getSize(), 0);

	for(i = 0; i < (int)dMatrix.RowNo(); i++)
	{
		double* temp = dataset.get(i)->toArray();

		for(j = 0; j < (int)dMatrix.ColNo(); j++)
		{
			dMatrix(i,j) = temp[j];
		}
	}

	// explanatory variables (not of interest)
	Matrix xMatrix((int)dMatrix.RowNo(), xColumns.getSize(),0);

	for(i = 0; i < xColumns.getSize(); i++)
	{
		for(j = 0; j < (int)dMatrix.RowNo(); j++)
		{
			xMatrix(j,i) = dMatrix(j,xColumns.get(i)-1);
		}
	}

	// explanatory variables (of interest)
	Matrix zMatrix((int)dMatrix.RowNo(), zColumns.getSize(),0);

	for(i = 0; i < zColumns.getSize(); i++)
	{
		for(j = 0; j < (int)dMatrix.RowNo(); j++)
		{
			zMatrix(j,i) = dMatrix(j,zColumns.get(i)-1);
		}
	}

	// array of combinations of legal v's
	Matrix yMatrix((int)dMatrix.RowNo(),1,0);

	for(i = 0; i < (int)dMatrix.RowNo(); i++)
	{
		yMatrix(i,0) = dMatrix(i,*yColumn-1);
	}

	// current state of the Markov chain
	Matrix yCurrent = yMatrix;

	// sample size (per observation)
	Matrix mMatrix((int)dMatrix.RowNo(),1,0);

	for(i = 0; i < (int)dMatrix.RowNo(); i++)
	{
		mMatrix(i,0) = dMatrix(i,*mColumn-1);
	}

	int* mCopy = new int[(int)mMatrix.RowNo()];

	for(i = 0; i < (int)mMatrix.RowNo(); i++)
	{
		mCopy[i] = (int)mMatrix(i,0);
	}

	quicksort(mCopy,(int)mMatrix.RowNo());

	v = findR(*r, min(*r,(int)mMatrix.RowNo()));

	weightings = getWeightings(v,(int)yMatrix.RowNo());

	sequence = new int[yMatrix.RowNo()];

	for(i = 0; i < (int)yMatrix.RowNo(); i++)
	{
		sequence[i] = i;
	}
	
	Matrix sample[SIZE];

	// ofstream outfile;
	ofstream outfile;
	outfile.open(*outfilename, ios::out | ios::trunc);
	outfile.close();
	outfile.open(*outfilename, ios::out | ios::app);

	if(*keepObservedSufficientStat == 1)
	{
		sample[0] = yCurrent;
	}

	for(i = *keepObservedSufficientStat; i < *num_iterations; i++)
	{
		// jump to next state and update the markov chain

		Matrix vCurrent = nextV(xMatrix, mMatrix, v, *r);
		double d = getd(vCurrent, mMatrix, yCurrent);
		yCurrent += vCurrent*d;
		sample[i%SIZE] = Matrix(yCurrent);

		if((i+1) % SIZE == 0)
		{	
			writeToFile(outfile,sample,SIZE);
		}
		else if((i+1) == *num_iterations)
		{
			writeToFile(outfile,sample,(*num_iterations % SIZE));
		}
	}

	outfile.close();
}
