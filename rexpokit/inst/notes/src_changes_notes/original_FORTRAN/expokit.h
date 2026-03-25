/* 
 * my_expokit.f
 */

extern "C"
{

// Expokit
void dgpadm_(int *ideg, int *m, double *t, double *H, int *ldh, 
  double *wsp, int *lwsp, int *ipiv, int *iexph, int *ns, int *iflag);

}




/* 
 * my_matexp.f:
 */

extern "C"
{

// Wrappers with initializations
void wrapdgpadm_(int *ideg, int *m, double *t, double *H, int *ldh,
	double *wsp, int *lwsp, int *ipiv, int *iexph, int *ns, int *iflag);

// DGEXPV contains an additional check ensuring sums to 1, for Markov-chain applications
void wrapalldgexpv_(int *n,int *m,double *t, double *v, double *w,
  double *tol, double *anorm, double *wsp, int *lwsp, int *iwsp, 
  int *liwsp, int *itrace, int *iflag, int *ia, int *ja, double *a, 
  int *nz, double *res);


void wrapalldmexpv_(int *n, int *m, double *t, double *v, double *w, 
  double *tol, double *anorm, double *wsp, int *lwsp, int *iwsp, 
  int *liwsp, int *itrace, int *iflag, int *ia, int *ja, double *a, 
  int *nz, double *res);

// This returns just one row (?) of the transition matrix, useful in Lagrange;
// same inputs as wrapalldmexpv_.
void wrapsingledmexpv_(int *n, int *m, double *t, double *v, 
  double *w, double *tol, double *anorm, double *wsp, int *lwsp,
  int *iwsp, int *liwsp, int *itrace, int *iflag, int *ia, int *ja, 
  double *a, int *nz, double *res);



// This returns just one row (?) of the transition matrix, useful in Lagrange;
// same inputs as wrapalldmexpv_.
void wrapsingledgexpv_(int *n, int *m, double *t, double *v, 
  double *w, double *tol, double *anorm, double *wsp, int *lwsp,
  int *iwsp, int *liwsp, int *itrace, int *iflag, int *ia, int *ja, 
  double *a, int *nz, double *res);


// The myDMEXPV etc. functions provide direct access to EXPOKIT functions;
// This should be faster, especially for sparse matrices
// Here, you input v (starting probabilities) and it fills in w, which are the
// output probabilities (in output list item #5)
void mydmexpv_(int *n, int *m, double *t, double *v, double *w, 
  double* tol, double *anorm, double *wsp, int *lwsp, int *iwsp, 
  int *liwsp, int *itrace, int *iflag, int *ia, int *ja, double *a, 
  int *nz);

void mydgexpv_(int *n, int *m, double *t, double *v, double *w, 
  double *tol, double *anorm, double *wsp, int *lwsp, int *iwsp, 
  int *liwsp, int *itrace, int *iflag, int *ia, int *ja, double *a, 
  int *nz);

}
