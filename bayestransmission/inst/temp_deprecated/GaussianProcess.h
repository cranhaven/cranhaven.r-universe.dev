
class GaussianProcess: public Object
{
private:
	Random *rand;

	static void inverse(int n, double **a, double **inv, double *det)
	{
		double tol = 0.0000000001;

		double **x = new double*[n];
		for (int i=0; i<n; i++)
		{
			x[i] = new double[2*n];
			for (int j=0; j<n; j++)
				x[i][j] = a[i][j];
			for (int j=n; j<2*n; j++)
				x[i][j] = 0;
			x[i][n+i] = 1;
		}

		for (int i=0; i<n; i++)
		{
			if (fabs(x[i][i]) < tol) 
			{
				cerr << "Matrix is not invertible\n";
				for (int ii=0; ii<n; ii++)
				{
					for (int jj=0; jj<n; jj++)
						fprintf(stderr,"%20.10f\t",a[ii][jj]);
					cerr << "\n";
				}
		 		throw(new runtime_error("Matrix not invertible."));
			}

			for (int j=0; j<n; j++)
				if (j != i)
				{
					double z = x[j][i] / x[i][i];
					for (int k=0; k<2*n; k++)
						x[j][k] -= x[i][k]*z;
				}
		}

		*det = 1;

		for (int i=0; i<n; i++)
		{
			double z = x[i][i];
			for (int j=0; j<2*n; j++)
				x[i][j] /= z;

			*det *= z;
		}

		for (int i=0; i<n; i++)
			for (int j=0; j<n; j++)
				inv[i][j] = x[i][j+n];

		for (int i=0; i<n; i++)
			delete x[i];
		delete x;
	}

	static void cholesky(int n, double **a, double **l)
	{
		double tol = 0.0000001;

		for (int i=0; i<n; i++)
			for (int j=0; j<n; j++)
				l[i][j] = 0;

		for (int i=0; i<n; i++)
		{
			for (int j=0; j<i; j++)
			{
				double tot = 0;
				for (int k=0; k<j; k++)
					tot += l[i][k]*l[j][k];

				if (fabs(a[i][j]-a[j][i]) > tol)
				{
					cerr << "Matrix is not symmetrical\n";
					for (int ii=0; ii<n; ii++)
					{
						for (int jj=0; jj<n; jj++)
							fprintf(stderr,"%20.10f\t",a[ii][jj]);
						cerr << "\n";
					}
					throw(new runtime_error("Matrix not symmetrical."));
				}

				l[i][j] = (a[i][j] - tot) / l[j][j];
			}

			double tot = 0;
			for (int k=0; k<i; k++)
				tot += l[i][k] * l[i][k];

			tot = a[i][i] - tot;
			if (tot < 0)
			{
				cerr << "Matrix is not positive definite\n";
				for (int ii=0; ii<n; ii++)
				{
					for (int jj=0; jj<n; jj++)
						fprintf(stderr,"%20.10f\t",a[ii][jj]);
					cerr << "\n";
				}
				throw(new runtime_error("Matrix not positive definite."));
			}

			l[i][i] = sqrt(tot);
		}
	}

public:

	GaussianProcess(Random *r)
	{
		rand = r;
	}

/**
	Probability density functions.
*/

	static double logdmvnorm(double *x, int p, double *m, double **s)
	{
		double **v = new double*[p];
		for (int i=0; i<p; i++)
			v[i] = new double[p];

		double d = 0;
		inverse(p,s,v,&d);
		
		double res = 0;
		for (int i=0; i<p; i++)
			for (int j=0; j<p; j++)
				res += (x[i]-m[i]) * v[i][j] * (x[j]-m[j]);
		res =   - 0.5 * ( res + log(d) + p * Random::log2pi );

		for (int i=0; i<p; i++)
			delete v[i];
		delete v;

		return res;
	}

	static double logdmvlognorm(double *x, int p, double *m, double **s)
	{
		double *y = new double[p];
		double logprod = 0;

		for (int i=0; i<p; i++)
		{
			y[i] = log(x[i]);
			logprod += y[i];
		}

		return logdmvnorm(y,p,m,s) - logprod;
	}
	
/**
	Random number generators.
*/

	void rmvnorm(double *x, int p, double *m, double **s)
	{
		double **c = new double*[p];
		for (int i=0; i<p; i++)
			c[i] = new double[p];

		cholesky(p,s,c);

		double *z = new double[p];
		for (int i=0; i<p; i++)
			z[i] = rand->rnorm();

		for (int i=0; i<p; i++)
		{
			x[i] = m[i];
			for (int j=0; j<=i; j++)
				x[i] += c[i][j] * z[j];
		}

		for (int i=0; i<p; i++)
			delete c[i];
		delete c;
		delete z;
	}

	void rwishart(double **x, int p, double n, double **v)
	{
		if (n < p-1)
		 	throw(new runtime_error("Wishart degrees of freedom too small."));
			
		double **l = new double*[p];
		double **a = new double*[p];
		double **y = new double*[p];

		for (int i=0; i<p; i++)
		{
			l[i] = new double[p];
			a[i] = new double[p];
			y[i] = new double[p];
		}

		cholesky(p,v,l);

		for (int i=0; i<p; i++)
		{
			for (int j=i; j<p; j++)
				a[i][j] = 0;
			for (int j=0; j<i; j++)
				a[i][j] = rand->rnorm();
			a[i][i] = sqrt(rand->rchisq(n-i));
		}

		for (int i=0; i<p; i++)
		{
			for (int j=0; j<p; j++)
			{
				y[i][j] = 0;
				for (int k=0; k<p; k++)
					y[i][j] += l[i][k] * a[k][j];
			}
		}

		for (int i=0; i<p; i++)
			for (int j=0; j<p; j++)
			{
				x[i][j] = 0;
				for (int k=0; k<p; k++)
					x[i][j] += y[i][k] * y[j][k];
			}


		for (int i=0; i<p; i++)
		{
			delete y[i];
			delete a[i];
			delete l[i];
		}

		delete y;
		delete a;
		delete l;
	}

	void rinvwishart(double **x, int p, double n, double **v)
	{
		if (n < p+1)
		 	throw(new runtime_error("Inverse Wishart degrees of freedom too small."));

		double d = 0;
		double **b = new double*[p];
		for (int i=0; i<p; i++)
			b[i] = new double[p];
		
		inverse(p,v,x,&d);
		rwishart(b,p,n,x);
		inverse(p,b,x,&d);

		for (int i=0; i<p; i++)
			delete b[i];
		delete b;
	}

	void rnorminvwishart(double *m, double **s, int p, double *mu, double lambda, double nu, double **psi)
	{
		rinvwishart(s,p,nu,psi);

		double *zero = new double[p];
		for (int i=0; i<p; i++)
			zero[i] = 0;

		rmvnorm(m,p,zero,s);
		
		for (int i=0; i<p; i++)
			m[i] = m[i] / sqrt(lambda) + mu[i];
		
		delete zero;
	}
};
