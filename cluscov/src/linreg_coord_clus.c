//
//  linreg_coord_clus.c
//
//
//  Created by Emmanuel Tsyawo on 3/26/19.
//

#include <R.h>
#include <Rinternals.h>
#define absval(x) ((x) >=0.0 ? (x):(-(x)))
#define sgn(a) ((a) == 0 ? 0 : ((a) > 0  ? 1 : -1 )) // sign function
#define max_2(a,b) ((a) >= (b) ? (a) : (b))  // max of two values
#define min_2(a,b) ((a) <= (b) ? (a) : (b))  // min of two values


/* Compile using:
 * gcc -c linreg_coord_clus.c matply.c
 * or in R
 * R CMD SHLIB linreg_coord_clus.c matply.c
 */

// take a product xab = xa*xb
void matply(double *xa, double *xb,double *xab, int *nra, int *nca,int *ncb){
    double sum ;
    for(int i=0; i< *nra;i++){
        for (int j=0; j<*ncb; j++) {
            sum = 0.0;
            for (int k=0; k<*nca; k++) {
                sum =  (double) (sum + (xa[ k*(*nra)+i])*(xb[ j*(*nca)+k])) ;
            }
            xab[ j*(*nra)+i] = sum ;
        }
    }
}

// take product xa*xb excluding column ica in xa and row ica in xb
void matply_sk1(double *xa, double *xb,double *xab, int *nra, int *nca,int *ncb, int *ica)
{
    double sum ;
    for(int i=0; i< *nra;i++){
        for (int j=0; j<*ncb; j++) {
            sum = 0.0;
            for (int k=0; k<*nca; k++) {
                if (k!=*ica) { //skip column ica in xa and row ica in xb
                    sum =  (double) (sum + (xa[ k*(*nra)+i])*(xb[ j*(*nca)+k])) ;
                }
            }
            xab[ j*(*nra)+i] = sum ;
        }
    }
}


// add matrices/vectors xm = xa - xb
void vecsub(double *xa, double *xb, double *xm, int *n)
{
    int i;
    for (i=0; i<*n; i++) {
        xm[i] = xa[i] - xb[i];
    }
}

// take the dot product two vectors a, b with length n each.
double dotprod(double *a, double *b, int *n)
{
    int i;
    double ans=0.0;
    for (i=0; i<*n; i++) {
        ans += a[i]*b[i];
    }
    return ans;
}

// compute dot products of two columns taken from matrices a and b of nr
// number of rows each. column indices are ica and icb
double dotprod_col_ex(double *a, double *b, int *nr, int *ica, int *icb)
{
    int i;
    double ans=0.0;
    ans = 0.0;
    for (i=0; i<*nr; i++) {
        ans += a[(*ica-1)*(*nr)+i]*b[(*icb-1)*(*nr)+i];
    }
    return ans;
}

// cordinate descent for linear regression with covariate clustering.

void linreg_coord_clus(double *Y, double *X, double *coefs, int *clus, int *klus,
                        double *ervec, double *Xdot, double *clusmns, int *nrX, int *ncX,
                        int *k, int *nC)
{
    double az = 1e-20, tol = 1e-10, RSS0,RSS1,dev,dr,bj,vp,tmp;
    int ncY=1, cnt=0,j,l,vi,h,id,maxcnt = 1000;

    // compute initial sum of squared of errors
    matply(X, coefs, ervec, nrX, ncX, &ncY); //compute ervec = X*coefs
    vecsub(Y, ervec, ervec, nrX); // compute ervec <-- Y-ervec
    RSS0=dotprod(ervec, ervec, nrX); // compute initial residual sum of squares (RSS0)

    // commence iteration
    for(;;)
    {
        cnt+=1; //increment counter by 1
        for (j=0; j<*ncX; j++){
            // compute X*coefs excluding column j in X and element j in coefs
            matply_sk1(X, coefs,ervec, nrX, ncX, &ncY, &j); // ervec = X[,-j]*coef[-j]
            vecsub(Y, ervec, ervec, nrX); // compute ervec <-- Y-ervec
            id = j+1; // dotprod_col_ex() needs id, that counts from 1,..,ncX
            dr=dotprod_col_ex(ervec, X, nrX, &ncY, &id); // compute dot(ervec,X[,j])
            bj = dr/Xdot[j];

            if(j>=*nC){
                vp = absval(clusmns[0]-bj); vi = 0;
                for(l=1;l<*k;l++){
                    tmp = absval(clusmns[l]-bj);
                    if(tmp<vp){ // identify nearest cluster of covariate j
                     vp=tmp; vi=l;
                    } // end if (tmp<vp)
                    } // end for(l=

                // vi is the identified cluster
                if(vi!=klus[j]){
                    klus[j] = vi; // update cluster assignment
                    tmp = bj; h = 1;
                    for(l=0; l<*ncX; l++){
                        if(klus[l]==klus[j]){
                            tmp+= coefs[l]; h+=1;
                            }// end if
                        } //end for (l=0
                    coefs[j] = tmp/((double)(h)); // assign cluster mean
                    clusmns[klus[j]] = coefs[j]; // update cluster mean
                    } // end if(vi!=clus[j])
                }else{
                    coefs[j] = bj; // update coefs
                    } // end if(j>nC)

        }// end for j

        // compute sum of squared residuals
        matply(X, coefs, ervec, nrX, ncX, &ncY); //compute ervec = X*coefs
        vecsub(Y, ervec, ervec, nrX); // compute ervec <-- Y-ervec
        RSS1=dotprod(ervec, ervec, nrX); // compute residual sum of squares (RSS1)

        // compute deviation
        dev = (RSS0-RSS1)/(az + absval(RSS0));

        // print update
        Rprintf("Inner iter = %d, RSS = %.5f, and reldev = %.5f \n",cnt,RSS1,dev);

        // check for convergence
        if (dev<=tol) {
            break;
        }
        if (cnt>=maxcnt) {
            Rprintf("Warning: maximum number of iterations reached. \n");
            break;
        }
        RSS0 = RSS1; //update RSS0
        // update cluster assignments if not converged. clus contains desired clusterings.
        for(l=*nC; l<*ncX; l++){
            clus[l-*nC]=klus[l];
        } //end for (l=0

    }// end for(;;)
} // end void function
