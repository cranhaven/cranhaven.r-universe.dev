
#ifndef ALUN_UTIL_ALLOCATOR_H
#define ALUN_UTIL_ALLOCATOR_H

namespace util{
class Allocator
{
public:
	static inline int *cleanAllocInt(int n)
	{
		int *x = new int[n];
		for (int i=0; i<n; i++)
			x[i] = 0;
		return x;
	}

	static inline int **cleanAllocInt(int n, int m)
	{
		int **x = new int*[n];
		for (int i=0; i<n; i++)
		{
			x[i] = new int[m];
			for (int j=0; j<m; j++)
				x[i][j] = 0;
		}
		return x;
	}

	static inline int ***cleanAllocInt(int n, int m, int l)
	{
		int ***x = new int**[n];
		for (int i=0; i<n; i++)
		{
			x[i] = new int*[m];
			for (int j=0; j<m; j++)
			{
				x[i][j] = new int[l];
				for (int k=0; k<l; k++)
					x[i][j][k] = 0;
			}
		} return x;
	}

	static inline void cleanFree(int **x)
	{
		if (*x != 0)
		{
			delete [] *x;
			*x = 0;
		}
	}

	static inline void cleanFree(int ***x, int n)
	{
		if ((*x) != 0)
		{
			for (int i=0; i<n; i++)
				if ((*x)[i] != 0)
				{
					delete [] (*x)[i];
					(*x)[i] = 0;
				}
			delete [] (*x);
			*x = 0;
		}
	}

	static inline void cleanFree(int ****x, int n, int m)
	{
		if (*x != 0)
		{
			for (int i=0; i<n; i++)
			{
				if ((*x)[i] != 0)
				{
					for (int j=0; j<m; j++)
					{
						delete [] (*x)[i][j];
						(*x)[i][j] = 0;
					}
					delete [] (*x)[i];
					(*x)[i] = 0;
				}
			}
			delete [] *x;
			*x = 0;
		}
	}


	static inline double *cleanAlloc(int n)
	{
		double *x = new double[n];
		for (int i=0; i<n; i++)
			x[i] = 0;
		return x;
	}

	static inline double **cleanAlloc(int n, int m)
	{
		double **x = new double*[n];
		for (int i=0; i<n; i++)
		{
			x[i] = new double[m];
			for (int j=0; j<m; j++)
				x[i][j] = 0;
		}
		return x;
	}

	static inline double ***cleanAlloc(int n, int m, int l)
	{
		double ***x = new double**[n];
		for (int i=0; i<n; i++)
		{
			x[i] = new double*[m];
			for (int j=0; j<m; j++)
			{
				x[i][j] = new double[l];
				for (int k=0; k<l; k++)
					x[i][j][k] = 0;
			}
		} return x;
	}

	static inline void cleanFree(double **x)
	{
		if (*x != 0)
		{
			delete [] *x;
			*x = 0;
		}
	}

	static inline void cleanFree(double ***x, int n)
	{
		if ((*x) != 0)
		{
			for (int i=0; i<n; i++)
				if ((*x)[i] != 0)
				{
					delete [] (*x)[i];
					(*x)[i] = 0;
				}
			delete [] (*x);
			*x = 0;
		}
	}

	static inline void cleanFree(double ****x, int n, int m)
	{
		if (*x != 0)
		{
			for (int i=0; i<n; i++)
			{
				if ((*x)[i] != 0)
				{
					for (int j=0; j<m; j++)
					{
						delete [] (*x)[i][j];
						(*x)[i][j] = 0;
					}
					delete [] (*x)[i];
					(*x)[i] = 0;
				}
			}
			delete [] *x;
			*x = 0;
		}
	}
};
}
#endif // ALUN_UTIL_ALLOCATOR_H
