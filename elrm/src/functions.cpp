// functions.cpp
//

#include "functions.h"
#include <math.h>
#include <R.h>
#include "Rmath.h"

// Swap two elements in an array of integers.
// NOTE: The array passed is modified (elements are swapped).
extern void swap(int* s, int a, int b)
{
	int temp=s[a];
	s[a]=s[b];
	s[b]=temp;
}

// Calculate the factorial of an integer number.
// A double type is used to hold large values.
extern double factorial(double n)
{
	double result = 1;

	if(n < 20)
	{
		for(int i = 2; i <= n; i++)
		{
			result = result*i;
		}
	}
	else
	{
		result = pow((2*n + (1.0/3.0))*PI, 0.5)*pow(n,n)*exp(-n);
	}
	
	return result;

}

// combinatorial calculator
extern double nCk(double n, double k)
{
	double result = 0;

	if(n == 0 || n == k || k == 0)
	{
		result = 1;
	}
	else if(n < 20)
	{
		result = factorial(n) / (factorial(n-k)*factorial(k));
	}
	else
	{
		double numer = ((double)1.0)/sqrt(2*PI);

		numer *= sqrt(n/(k*(n-k)));

		double denom = pow(k/n,k)*pow((n-k)/n,n-k);

		result = numer/denom;
	}

	return result;
}

// Randomly permute an array of integers.
// NOTE: the array passed is modified (sorted).
extern void randPerm(int* vec, int n)
{ 
	int j;
	
	GetRNGstate();
	
	for(int i = 0; i < n; i++)
	{
		// generate a random number j such that i <= j < n and
		// swap the element present at index j with the element 
		// present at current index i
		j = i + rand_int(n-i);

		swap(vec, i, j);
	}
	
	PutRNGstate();
}

// Random sample of size k from a vector of size n > k
// Note: repeats are not allowed
extern void rsample(int* sample, int* vec, int k, int n)
{ 
	int i,j,r;
	bool valid = true;
	
	GetRNGstate();

	for(i = 0; i < k; i++)
	{
		do
		{
			valid = true;

			r = rand_int(n);

			for(j = 0; j < i; j++)
			{
				if(r == sample[j])
				{
					valid = false;

					break;
				}
			}

		}
		while(!valid);

		sample[i] = vec[r];
	}
	
	PutRNGstate();
}

// Permute an array of integers.
// Note: If the array passed is sorted, repeated calls to permute 
//       will produce all the permutations (until 0 is returned).
extern int permute(int*  str, int len)
{
	int key=len-1;
	int newkey=len-1;
	
	/*The key value is the first value from the 
		end which is smaller than the value to 
		its immediate right*/
	while( (key>0)&&(str[key] <= str[key-1]))
		{key--;}
	key--;
	
	/*If key<0 the data is in reverse sorted order, 
		which is the last permutation.  */
	if(key <0)
		return 0;
	
	/*str[key+1] is greater than str[key] because 
		of how key was found. If no other is greater, 
		str[key+1] is used*/
	
	newkey=len-1;
	while((newkey > key) && (str[newkey] <= str[key]))
		{
		newkey--;
		}
	
	
	swap(str,key,newkey);

	/*variables len and key are used to walk through 
		the tail, exchanging pairs from both ends of 
		the tail.  len and key are reused to save 
		memory*/
	len--;
	key++;
	/*The tail must end in sorted order to produce the 
		next permutation.*/
	while(len>key)
		{
		swap(str,len,key);
		key++;
		len--;
		}
	
	return 1;
}

void partition(int* data, int n, int& pivot_index)
{
	// initialize values
	int pivot = data[0];
	int too_big_index = 1;
	int too_small_index = n-1;

	while(too_big_index <= too_small_index)
	{
		while(too_big_index < n && data[too_big_index] <= pivot)
		{
			too_big_index++;
		}

		while(data[too_small_index] > pivot)
		{
			too_small_index--;
		}

		if(too_big_index < too_small_index)
		{
			swap(data,too_big_index,too_small_index);
		}
	}

	// move the pivot elements to its correct position.
	pivot_index = too_small_index;
	data[0] = data[pivot_index];
	data[pivot_index] = pivot;
}

// Sort an array of integers from smallest to largest.
// NOTE: The array passed as an argument is modified (sorted).
extern void quicksort(int* data, int n)
{
	int pivot_index;
	int n1;
	int n2;

	if(n > 1)
	{
		// Partition the array, and set the pivot index.
		partition(data,n,pivot_index);

		// Compute the sizes of the subarrays.
		n1 = pivot_index;
		n2 = n - n1 - 1;

		// Recursive calls will now sort the subarrays.
		quicksort(data, n1);
		quicksort((data + pivot_index + 1), n2);
	}
}

// Find all unique integers in a list of integers.
extern List<int*> unique(List<int*> list, int len)
{
	for(int i = 0; i < list.getSize(); i++)
	{
		for(int j = i+1; j < list.getSize(); j++)
		{
			bool equal = true;

			for(int k = 0; k < len; k++)
			{
				if(list.get(i)[k] != list.get(j)[k])
				{
					equal = false;

					break;
				}
			}

			if(equal)
			{
				list.remove(j);

				j--;
			}
		}
	}

	return list;
}

int GCD(int a, int b)
{
    int Remainder;

    while( b != 0 )
    {
        Remainder = a % b;
        a = b;
        b = Remainder;
    }

    return a;
}

bool isCoprime(int a, int b)
{
	if(GCD(a,b) == 1)
	{
		return true;
	}
	else
	{
		return false;
	}
}

List<int> coprime(int n)
{
	List<int> coprimeList;
	
	coprimeList.add(n);

	for(int i = n-1; i > 0; i--)
	{
		bool flag = true;

		for(int j = 0; j < coprimeList.getSize(); j++)
		{
			int element = coprimeList.get(j);

			if(!isCoprime(i,element))
			{
				flag = false;
			}
		}

		if(flag)
		{
			coprimeList.add(i);
		}
	}

	return coprimeList;
}

List<List<int>*> listCoprimes(int n)
{
	int i = 0;

	List<List<int>*> list;

	if((n % 2) != 0)
	{
		n = n+1;
	}

	List<int*> vList;

	int max = n/2;

	for(i = max; i > 0; i--)
	{
		List<int>* coprimes = new List<int>;

		*coprimes = coprime(i);

		list.add(coprimes);
	}

	return list;
}

Array2D<int> findRHelper2(int r, int n)
{
	Array2D<int>* v = new Array2D<int>;

	int i,j,k = 0;

	List<List<int>*> list = listCoprimes(r);

	if(r == 2)
	{
		v->m = 1;
	}
	else
	{
		v->m = r-2;
	}

	v->n = n;
	v->data = new int*[v->m];

	for(i = 0; i < list.getSize(); i++)
	{
		v->data[i] = new int[n];

		int sum = 0;

		for(j = 0; j < n; j++)
		{
			if(j < list.get(i)->getSize())
			{
				v->data[i][j] = list.get(i)->get(j);

				sum = sum + v->data[i][j];
			}
			else if(sum < r)
			{
				v->data[i][j] = 1;

				sum = sum + v->data[i][j];
			}
			else
			{
				v->data[i][j] = 0;
			}
		}
	}

	k = 0;

	for(i = list.getSize(); i < v->m; i++)
	{
		v->data[i] = new int[n];
		v->data[i][0] = list.get(k)->get(0);

		int sum = v->data[i][0];

		for(j = 1; j < n; j++)
		{
			if(sum < r)
			{
				v->data[i][j] = 1;

				sum = sum + v->data[i][j];
			}
			else
			{
				v->data[i][j] = 0;
			}
		}

		k++;
	}


	return *v;
}

List<int*> findRHelper(int r)
{
	int i,t = 0;

	List<int*> vList;
	
	for(t = 2; t <= r; t+=2)
	{
		Array2D<int> vtemp = findRHelper2(t,r);
				
		for(i = 0; i < vtemp.m; i++)
		{
			vList.add(vtemp.data[i]);
		}
		
		vList = unique(vList,r);
	}

	return vList;
}

int vecSum(int* vec, int n)
{
	int sum = 0;

	for(int i = 0; i < n; i++)
	{
		sum = vec[i] + sum;
	}

	return sum;
}

bool check(int* vec, int n, int r)
{
	bool check = true;

	int sum = vecSum(vec,n);

	// check that the sum of |v_i| < r and is even
	// and if it is possible for the sum to
	// equal zero by assigning negative signs
	if(sum > r || vec[n-1] > sum/2 || (sum % 2) != 0)
	{
		check = false;
	}

	return check;
}

Array2D<int> findR(int r, int n)
{
	int i,j,k = 0;

	List<int*> vTemp = findRHelper(r);

	Array2D<int> v;

	v.m = vTemp.getSize();
	v.n = n;
	v.data = new int*[v.m];

	int* zeros = new int[v.m];

	for(i = 0; i < v.m; i++)
	{
		v.data[i] = new int[v.n];

		bool found = false;
		
		for(j = 0; j < v.n; j++)
		{
			v.data[i][j] = vTemp.get(i)[j];

			if(v.data[i][j] == 0 && !found)
			{
				zeros[i] = j;

				found = true;
			}
		}

		if(!found)
		{
			zeros[i] = v.n;
		}
	}
	
	List<int*> vList;
	
	for(j = 0; j < v.m; j++)
	{	
		List<int*> tempList;
		
		for(i = 1; i < zeros[j]; i++)
		{
			quicksort(v.data[j],zeros[j]);
			
			do  
			{
				int* temp = new int[n];
				
				if(check(v.data[j],zeros[j],r))
				{
					for(k = 0; k < zeros[j]; k++)
					{
						if(k < i)
						{
							temp[k] = v.data[j][k];
						}
						else
						{
							temp[k] = -v.data[j][k];
						}
					}

					for(k = zeros[j]; k < n; k++)
					{
						temp[k] = 0;
					}
					
					if(vecSum(temp,n) == 0)
					{			
						quicksort(temp,n);
						
						tempList.add(temp);
					}
				}
			}
			while(permute(v.data[j],zeros[j]));
		}
		
		vList.concat(unique(tempList,n));
	}
	
	vList = unique(vList,n);
	
	for(i = 0; i < v.m; i++)
	{
		delete[] v.data[i];
	}
	
	v.m = vList.getSize();
	v.n = n;
	v.data = new int*[v.m];
	
	for(i = 0; i < v.m; i++)
	{
		v.data[i] = new int[v.n];
		
		for(j = 0; j < v.n; j++)
		{
			v.data[i][j] = vList.get(i)[j];
		}
	}
	
	return v;
}

// Read a data file (must not include column headers).
extern List<List<double>*> readDataFile(char* filename)
{
	string line;
	string buf;
	List<List<double>*> tokens;

	ifstream in(filename);

	while(getline(in, line, '\n'))
	{	
		List<double>* list = new List<double>;

		stringstream ss(line);
		
		while (ss >> buf)
		{
			char* temp = new char[256];
			
			strcpy(temp, buf.c_str());
	
			char * pEnd;
			double dbl;
			dbl = strtod (temp,&pEnd);

			list->add(dbl);
		}  
		
		if(!list->isEmpty())
		{
			tokens.add(list);
		}
	}

	in.close();

	return tokens;
}

List<int> unique(int* vec, int n)
{
	List<int> unique;

	for(int i = 0; i < n; i++)
	{
		if(unique.contains(vec[i]) == -1)
		{
			unique.add(vec[i]);
		}
	}

	return unique;
}

// Obtain weightings for seed vectors in the set R.
extern double* getWeightings(Array2D<int> array, int n)
{
	double* weights = new double[array.m];

	int i,j,k = 0;

	for(i = 0; i < array.m; i++)
	{
		List<int> u = unique(array.data[i],array.n);

		int contains_zero = u.contains(0);

		if(contains_zero != -1)
		{
			u.remove(u.contains(0));
		}

		List<int> copies;

		for(j = 0; j < u.getSize(); j++)
		{
			copies.add(0);

			for(k = 0; k < array.m; k++)
			{
				if(array.data[i][k] == u.get(j))
				{
					copies.set(copies.get(j) + 1,j);
				}
			}
		}

		double sum = 0;
		double factProduct = 1;

		for(j = 0; j < copies.getSize(); j++)
		{
			sum = copies.get(j) + sum;
			factProduct = factProduct*factorial(copies.get(j));
		}

		double simplify = n-sum;

		for(j = (int)(n-sum+1); j < n; j++)
		{
			simplify = simplify*j;
		}
		
		weights[i] = simplify/factProduct;
	}

	double sum_weights = 0;

	for(i = 0; i < array.m; i++)
	{
		sum_weights = sum_weights + weights[i];
	}

	for(i = 0; i < array.m; i++)
	{
		weights[i] = weights[i] / sum_weights;
	}

	return weights;
}

// Parse a an array of (integer) characters to a list of integers.
extern List<int> atoiList(char* char_array)
{
    string buf; 
    stringstream ss(char_array);

    List<int> tokens;

    while (ss >> buf)
	{
		if(buf.length() != 0)
		{
			tokens.add(atoi(buf.c_str()));
		}
	}

	return tokens;
}

// Obtain a random integer between 0 (inclusive) and N (exclusive).
extern int rand_int(int n)
{
	// return (int)(n * rand() / (RAND_MAX + 1.0));
	return(floor(runif(0,n)));
}

// Round a double to a given precision.
double roundDouble(double doValue, int nPrecision)
{
    static const double doBase = 10.0;
    double doComplete5, doComplete5i;
    
    doComplete5 = doValue * pow(doBase, (double) (nPrecision + 1));
    
    if(doValue < 0.0)
        doComplete5 -= 5.0;
    else
        doComplete5 += 5.0;
    
    doComplete5 /= doBase;
    modf(doComplete5, &doComplete5i);
    
    return doComplete5i / pow(doBase, (double) nPrecision);
}


