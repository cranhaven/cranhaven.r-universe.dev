#include <Rcpp.h>
#include <Rmath.h>

using namespace Rcpp;



void random01qvector_r(int *z, int n, int q){
  IntegerVector x(n), s(q);
  NumericVector rn = rnorm(10);
  for (int i = 0; i < n; i++)
    x[i] = i+1;
  IntegerVector s0 = sample(x, q, FALSE);
  //int u = s0[1];
  //double l = rn[1];
  for (int i = 0; i < n; i++)
    z[i] = 0;
  for (int i = 0; i < q; i++)
    z[s0[i]] = 1;
}


void updatebestsolution(double *obj, double *objbest, int *z, int *zbest,
                        int q, int *ass, int *assbest, int n,
    				            int *it, int *itbest)
{
	*objbest = *obj;
	for (int i = 1; i <= q; i++)
		zbest[i] = z[i];
	for (int i = 1; i <= n; i++)
		assbest[i] = ass[i];
  *itbest = *it;
}



void copyvector(int *xin, int *xout, int n){
  for (int i = 0; i <= n; i++)
		xout[i] = xin[i];
}

void bestassignement(double ***multdist, double **dist, int *z, double *objfun, int *jmin, int n, int p, int q){
  // dato z_p, calcola la matrice x_ij
	double MinRow;
	for (int i = 0;  i <= n; i++)
		for (int j = 0; j <= p; j++)
			dist[i][j] = 0;
	for (int k = 1; k <= q; k++)
		if (z[k] >= 0.99){
		for (int i = 0;  i <= n; i++)
			for (int j = 0; j <= p; j++)
				dist[i][j] += multdist[i][j][k] ;
		}

	*objfun = 0.0;
	for (int i = 1; i <= n; i++){
		MinRow = INT_MAX;
		jmin[i] = 0;
		for (int j = 1; j <= p; j++){
			if (dist[i][j] <= MinRow){
				MinRow = dist[i][j];
				jmin[i] = j;
			}
		} // next j
		*objfun += MinRow;
	} // next i
 }  // end function

void bestvariables(double ***multdist, double *sumdist, int *iass, double *obj, int *z, int n, int p, int q, int qstar){
  //double zero = 0.0;
	*obj = 0;
	for (int i = 0; i <= q; i++)
		z[i] = 0;
	for (int k = 1; k <= q; k++){
		sumdist[k] = 0;
		for (int i = 1; i <= n; i++)
			sumdist[k]  += multdist[i][iass[i]][k];
		}

int num_var = 0;
while( num_var < qstar ){
	double min_dist = INT_MAX;
	int min_q = 0;
	for (int k = 1; k <= q; k++){
		if (sumdist[k] < min_dist){
			min_dist = sumdist[k];
			min_q = k;
		}
	}
	z[min_q] = 1;
	*obj += min_dist;
	sumdist[min_q] = INT_MAX;
	num_var += 1;
} // end while
}

void localzed(int *z, int* zlocal, int q, int kin, int kout){
   for (int i = 0; i <= q; i++)
		zlocal[i] = z[i];
   zlocal[kin] = 1;
   zlocal[kout] = 0;
}


// [[Rcpp::export]]
List VarSelH(NumericVector a, int n, int p, int q, int qstar, int MaxIt) {
  NumericVector zbis(q);
  NumericVector iassbis(n);
  double ***dist;
  double **totaldist;
  double *sumdist;
  int *z, *zbest;
  int *iass, *assbest;
  double objnew;
  double epsilon = 0.001;
  double pi = 2.14;
  int a_size = a.size();
  if (a_size != n*p*q)
    return Rcpp::List::create(Named("Msg", "Dimensione Matrice Dati Input Sbagliata ") );
  if (qstar > q)
    return Rcpp::List::create(Named("Msg", "Numero variabili troppo alto ") );
  dist = new double**[n+1];
  for(int i=0; i<=n; i++){
      dist[i] = new double*[p+1];
		  for(int j=0; j<=p;j++){
			    dist[i][j] = new double[q + 1];
		  }}
  for (int i = 1; i <= n; i++)
      for (int j = 1; j <= p; j++)
        for (int k = 1; k <= q; k++)
            dist[i][j][k] = (double) a[(i - 1) + n*(j - 1) + (n*p)*(k - 1)];
  totaldist = new double*[n + 1];
  for (int i = 0; i <= n; i++)
    totaldist[i] = new double[p + 1];
  z = new int[q + 1];
  zbest = new int[q + 1];
  iass = new int[n + 1];
  assbest = new int[n + 1];
  sumdist = new double[q + 1];
  //int maxlastupdate = 1000*q;
  //srand(1);
  int it = 0;
  int itbest = 0;
  double ubbest = INT_MAX;
  double objold = INT_MAX;
  int answer = 0;
  int localit = 0;
  //while( it - itbest < maxlastupdate){
  while (it < MaxIt + 1){
    //random01qvector(z, q, qstar);
    random01qvector_r(z, q, qstar);
    answer = 0;
    localit = 0;
    objold = INT_MAX;
    while (answer == 0){
      localit += 1;
      it += 1;
      if (localit%2 == 1){
        bestassignement(dist, totaldist, z, &objnew, iass, n, p, q);
		  } else {
			  bestvariables(dist, sumdist, iass, &objnew, z, n, p, q, qstar);
		  }
		  if (objnew + epsilon < ubbest)
			  updatebestsolution(&objnew, &ubbest, z, zbest, q, iass, assbest, n, &it, &itbest);
		  if (objnew + epsilon >= objold){
			  answer = 1;  // exit while
		} else {
			objold = objnew; // update incument
		}
	} // chiude il while answer
} // chiude il while random restart


  for (int k = 0; k < q; k++)
    zbis[k] = zbest[k + 1];
  for (int i = 0; i < n; i++)
    iassbis[i] = assbest[i + 1];
  pi = ubbest;
  return Rcpp::List::create(Named("obj", pi),
                            Named("x",zbis),
                            Named("ass", iassbis),
                            Named("bestit", itbest));
}


