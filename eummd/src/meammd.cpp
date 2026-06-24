#include <vector>
#include <random>
#include <algorithm>

#include "eummd.h"
#include "medianHeuristic.h"


std::vector<double> mergeTwoAlreadySortedTwo(std::vector<double>& A, 
                                             std::vector<double>& B){

    // allocate space for C
    std::vector<double>::size_type n = A.size() + B.size();
    std::vector<double> C(n);

    //iterators
    std::vector<double>::const_iterator a_iter = A.begin();
    std::vector<double>::const_iterator b_iter = B.begin();
    //iterator for C is not constant, because assigning values form A and B
    std::vector<double>::iterator c_iter = C.begin();
    
    //Filling C while comparing A and B
    while (  (a_iter != A.end()) && (b_iter != B.end()) && (c_iter != C.end()) ){
        if ( (*a_iter) < (*b_iter) ){
            *c_iter = *a_iter;
            ++a_iter;
        } else {
            *c_iter = *b_iter;
            ++b_iter;
        }
        ++c_iter;
    }

    //now fill in the rest
    //only a *maximum* of one of these two while loops will run
    //at least one of the iterators reached the end in the previous while
    while ( (a_iter != A.end()) && (c_iter != C.end()) ){
        *c_iter = *a_iter;
        ++a_iter;
        ++c_iter;
    }
    while ( (b_iter != B.end()) && (c_iter != C.end()) ){
        *c_iter = *b_iter;
        ++b_iter;
        ++c_iter;
    }

    //return the merged vector
    return C;
}



// this projects a collection of multivariate observations
// to univariate observations
std::vector<double> projection(const std::vector<double>& X, int n, int d, std::vector<double> u){
    std::vector<double> proj(n);
    for (int i=0; i < n; ++i){
        proj[i] = 0;
        for (int j=0; j < d; ++j){
            proj[i] += X[i*d+j] * u[j];
        }
    }
    return proj;
}


// X: rows n, columns d
// y is the centre point, length 
// returns z ( ||X[1] - y|||,  ||X[n] - y|||, ..., ||X[n] - y||| )
// where norm is 2-norm
std::vector<double> projdistance(std::vector<double> X, int n, int d, std::vector<double> y, int nmethod=0){
    std::vector<double> z(n);
    double L = 0;
    for (int i=0; i < n; ++i){
        z[i] = 0;
        for (int j=0; j < d; ++j){
            L = X[i*d+j] - y[j];
            if (nmethod==0){
                z[i] += L*L;
            } else {
                z[i] += std::abs(L);
            }
        }
        if (nmethod==0){
            z[i] = std::sqrt(z[i]);
        }
    }
    return z;
}



// https://www.cplusplus.com/reference/random/uniform_real_distribution/
// https://stackoverflow.com/questions/21516575/fill-a-vector-with-random-numbers-c
//
// unifSd
//
// Generates a vector of length dimension d
// Important to get dimension correct!!!
//
std::vector<double> unifSd(std::size_t d, std::default_random_engine& gen){
    std::vector<double> vec(d);
    std::uniform_real_distribution<double> runif(0.0,1.0);
    double norm = 0;
    double obs = 0;
    for (std::vector<double>::iterator it=vec.begin(); it != vec.end(); ++it){
        obs = runif(gen);
        norm += (obs*obs);
        *it = obs;
    }

    if (norm > 0){
        //renormalise
        norm = std::sqrt(norm);
        for (std::vector<double>::iterator it=vec.begin(); it != vec.end(); ++it){
            *it /= norm;
        }
    } else { 
        //fill with 1/sqrt(d)
        obs = 1.0 / std::sqrt(d);
        for (std::vector<double>::iterator it=vec.begin(); it != vec.end(); ++it){
            *it = obs;
        }
    }
    return vec;
}



/**
 * Update the mean, given the last mean, new observation
 * and number of observations.
 *
 * Return updated mean.
 *    Formula xbar_{n} = (n-1)/n xbar_{n-1} + xn/n
 *
 * @param xbar The previous mean.
 *
 * @param xn The new observation
 *
 * @param n The number of elements seen so far (including xn).
 *
 * @return The vector of random values.
 * 
 */
double update_mean(double xbar, double xn, std::size_t n){
    if (n==0){
        xbar=0.0;
    } else{
        double a = (n-1.0) / n;
        double b = 1.0 / n;
        xbar = a*xbar + b*xn;
    }
    return xbar;
}


/**
 * Update a vector of averages
 *
 * @param av Vector of means/averages, passed by reference.
 *
 * @param vec Vector of values, used to update averages.
 *
 * @param n Number of observations seen so far.
 *
 * @details Run through vector and updates each average.
 *
 * @see Call update_mean
 *
 * @return Does not return anything; passes vectors by reference.
 */
void update_mean_vec(std::vector<double>& av, std::vector<double>& vec, std::size_t n){
    // iterators
    std::vector<double>::iterator itav = av.begin();
    std::vector<double>::const_iterator itvec = vec.begin();

    // run through vectors and update means (av) 
    for ( ; ( itav != av.end() ) && ( itvec != vec.end() ); ++itav, ++itvec){
        *itav = update_mean(*itav, *itvec, n);
    }
}



/**
 * Subroutine for meammd_proj_pval_faster
 *
 * Computes MMD statistic for X and Y, along with another numperm
 * MMD statistics for permutations.
 *
 * @param Xstart Start of X vector.
 *
 * @param Xend End of X vector.
 *
 * @param Ystart Start of Y vector.
 *
 * @param Yend End of Y vector.
 *
 * @param numperm int number of permutations.
 *
 * @param seednum The value of the seed. This is important, because need
 *                same permutations for each projection
 * 
 *
 * @details Basically the same as cpp_emmd_pval_faster, except not
 *          computing pvalue, just saving MMD statistics.
 *          Also calls median heuristic computation.
 *
 * @see medianHeuristicAlreadySorted, cpp_emmd_pval_faster
 *
 * @return vector of numperm+1 MMD statistics.
 */
std::vector<double> cpp_meammd_proj_pval_faster_sub(std::vector<double>::const_iterator Xstart,
                                                    std::vector<double>::const_iterator Xend,
                                                    std::vector<double>::const_iterator Ystart,
                                                    std::vector<double>::const_iterator Yend,
                                                    int numperm, 
                                                    int seednum){

    //MMD_u temp value
    double MMD_u = 0.0;

    // extract vectors Xu and Yu
    std::vector<double> Xu(Xstart, Xend);
    std::vector<double> Yu(Ystart, Yend);

    // create vector to store MMD values
    std::vector<double> MMD_vec;
    // 1 for MMD and 1 for each MMD_perm (numperm)
    MMD_vec.reserve(numperm+1);

    // sort X and Y
    // O(n1 log n1 + n2 log n2) = O(n log n)
    std::sort(Xu.begin(), Xu.end());
    std::sort(Yu.begin(), Yu.end());

    // merge two sorted vectors into Z
    // O(n)
    std::vector<double> Zu = mergeTwoAlreadySortedTwo(Xu, Yu);

    // if beta is NOT a positive number, then compute using median heuristic
    // O(n log n)
    double beta = -0.1;
    if ( !(beta > 0) ){
        // // making a copy, just in case
        // std::vector<double> Zcopy;
        // Zcopy.insert(Zcopy.end(), Zstart, Zend);
        double medianDiff = medianHeuristicAlreadySorted(Zu);
        // beta is set to 1/ median difference
        beta = 1.0 / medianDiff;
    }

    // the triangular sum of the whole set; used in all calculations 
    // O(n)
    double T4 = lapKernSSD(Zu, beta);

    const std::vector<double>::size_type n1 = Xu.size();
    const std::vector<double>::size_type n2 = Yu.size();

    // create permutation
    std::vector<bool> Xperm(n1, true);
    std::vector<bool> Yperm(n2, false);

    // concatenating permutations into perm
    std::vector<bool> perm(Xperm);
    perm.insert(perm.end(), Yperm.begin(), Yperm.end());

    //make new vector, where X at front, Y at end
    //this is different from Z, which is completely sorted
    //This is only for MMDstar; just to extract X and Y properly
    //otherwise need to find permutation of 0s and 1s that 
    //will extract X and Y from Z correctly in compute_eummd_faster
    //other option is to extract X and Y in this method
    std::vector<double> XYu(Xu);
    XYu.insert( XYu.end(), Yu.begin(), Yu.end() );
    std::vector<double>::const_iterator XYustart = XYu.begin();

    // compute MMD statistic for Xu and Yu
    MMD_u = compute_eummd_faster(XYustart, n1, n2, 
                                       perm.begin(), perm.end(), 
                                       T4, beta);

    // save it; first position in vector
    MMD_vec.push_back(MMD_u);

    // Now for permutations; this ensures same permutations
    // are used for each projection
    // initiate random generator
    std::random_device rd;
    std::mt19937 g;
    // set the seed, if greater than 0
    if (seednum > 0){
        g.seed(seednum);
    } else {
        // need seed so that reproduces permutations for other projections
        g.seed(rd());
    }

    // now do permutations on the projection, and save with push_back
    for (int i=0; i < numperm; ++i){
        //shuffle permutation and compute MMD
        std::shuffle(perm.begin(), perm.end(), g);

        // pass Z, which is sorted
        MMD_u = compute_eummd_faster(Zu.begin(), n1, n2, 
                                    perm.begin(), perm.end(), 
                                    T4, beta);
        // save MMD_u value
        MMD_vec.push_back(MMD_u);
    } // end of perms

    return(MMD_vec);
} //end of cpp_meammd_proj_pval_faster_sub 



/**
 * Compute pval MMD with multiple projections
 * Other version will use distance
 *
 * Need to shuffle matrix
 */
std::vector<double> cpp_meammd_proj_pval_faster(double* X, double* Y, 
                                                int nX, int dX,
                                                int nY, int dY,
                                                int numperm, 
                                                int numproj,
                                                int seednum, 
                                                double beta, 
                                                int twosided, 
                                                int boundedminpval){

    // return vector is first pval, then statistic
    // there is no beta, because that depends on each projection
    std::vector<double> returnvec;

    if (dX != dY){
        //error, dimensions incorrect
        returnvec.push_back(-2.0); 
        returnvec.push_back(-2.0);
        return returnvec;
    }
    std::size_t dZ = dX;

    //int should really be std::size_t
    std::size_t Xsize = nX*dX;
    std::size_t Ysize = nY*dY;
    std::size_t Zsize = Xsize + Ysize;
    //std::size_t nZ = nX + nY;

    // create vector for matrix
    std::vector<double> Z;
    Z.reserve(Zsize);
    //insert X then Y
    Z.insert(Z.end(), &X[0], &X[0]+Xsize);
    Z.insert(Z.end(), &Y[0], &Y[0]+Ysize);

    // create generator for projections
    std::default_random_engine gen;
    std::random_device rd;
    if (seednum > 0){
        gen.seed(seednum);
    } else {
        gen.seed(rd());
    }

    // just for intialisation; take a projection
    // u vector generation
    std::vector<double> u = unifSd(dZ, gen);
    std::vector<double> Zproj = projection(Z, nX+nY, dZ, u);
    std::vector<double> MMD_vec(numperm+1, 0.0);
    std::vector<double> MMDbar_vec(numperm+1, 0.0);
    std::vector<double>::const_iterator Xstart;
    std::vector<double>::const_iterator Xend;
    std::vector<double>::const_iterator Ystart;
    std::vector<double>::const_iterator Yend;

    // run through projections
    std::size_t n = 0;
    for (int j=0; j < numproj; ++j){
        // obtain projection and projection to Zproj
        u = unifSd(dZ, gen);
        Zproj = projection(Z, nX+nY, dZ, u);

        // extract iterators for start/end of X and Y
        Xstart = Zproj.begin();
        Xend = Xstart + nX;
        Ystart = Xend;
        Yend = Zproj.end();

        //Compute all MMD_vec's 
        //update MMD_vec - DO NOT use MMDbar_vec
        MMD_vec = cpp_meammd_proj_pval_faster_sub(Xstart, Xend, 
                                                  Ystart, Yend, 
                                                  numperm, seednum);
        // update MMDbar using MMD_vec
        ++n;
        update_mean_vec(MMDbar_vec, MMD_vec, n);
    }

    // now count values below MMDbar; first value in bar_vec
    double MMDbar = MMDbar_vec[0];
    int MMD_count_below = 0;
    // starting from beginning+1
    for (std::vector<double>::const_iterator itMMD = MMD_vec.begin()+1; 
         itMMD != MMD_vec.end(); ++itMMD){

        if (*itMMD < MMDbar){
            ++MMD_count_below;
        }
    }

    //now return the threshold
    double pval = MMD_count_below / (numperm + 1.0);

    //make one-sided
    // pval = convertTwoSidedPvalueToOneSided(pval);
    //pval =  1 - std::abs(1 - 2*pval) ;

    //make one-sided
    //twosided = 0 means false
    //twosided = 1 means true
    if (twosided==1){
        //make two sided
        pval =  1 - std::abs(1 - 2*pval) ;
    } else {
        //one sided; counting below, so must make 
        // large values into small
        pval = 1 - pval;
    }

    // quick check for minimum possible p-value; avoids pval=0
    // 1 / 2(numperm+1)
    if (boundedminpval==1){
        double pmin = 0.5 / (numperm+1.0);
        if (pval < pmin)
            pval = pmin;
    }

    returnvec.push_back(pval);
    returnvec.push_back(MMDbar);
    return(returnvec);
}



/* FastMMD calculation with cpp sort
 *
 * Fast Exact MMD for univariate vectors. Uses the Laplacian kernel.
 * Not passing by reference, because do not want to modify original values.
 *
 * @param X std::vector<double>
 *
 * @param Y std::vector<double>
 *
 * @param beta The parameter/bandwidth for the kernel.
 *
 * @see lapKernSSD
 *
 * @details A better implementation could avoid creating X and Y 
 *          vectors and only work on Z. Would need to modify
 *          mergeTwoAlreadySorted, and also would make code a little
 *          more complicated; not sure how much it would improve speed.
 *
 * @return The MMD between these two vectors.
 */
double emmd_ptr_alt_MH(std::vector<double>::const_iterator Zstart, 
                       const std::vector<double>::size_type nX, 
                       const std::vector<double>::size_type nY, 
                       double beta){

    std::vector<double>::const_iterator Xstart = Zstart;
    std::vector<double>::const_iterator Xend = Xstart + nX;
    std::vector<double>::const_iterator Ystart = Xend;
    std::vector<double>::const_iterator Yend = Ystart + nY;

    // create **deep** copies of vectors; we will work on these.
    std::vector<double> X(Xstart, Xend);
    std::vector<double> Y(Ystart, Yend);

    //first, get the lengths of the vectors
    std::vector<double>::size_type n1 = X.size();
    std::vector<double>::size_type n2 = Y.size();

    // second, sort each set
    std::sort(X.begin(), X.end());
    std::sort(Y.begin(), Y.end());


    //*next, merge the two sorted vectors into a third, sorted vector
    std::vector<double> Z = mergeTwoAlreadySortedTwo(X, Y);

    // if beta is NOT a positive number, then compute using median heuristic
    if ( !(beta > 0) ){
        //*next, use medianHeuristic to get beta, using sorted Z
        double medianDiff = medianHeuristicAlreadySorted(Z);
        // beta is set to 1/ median difference
        beta = 1 / medianDiff;
    }


    //third, get the first two sorted sums
    double T1 = lapKernSSD(X, beta);
    double T2 = lapKernSSD(Y, beta);

    //fourth compute L based on Z, combined vectors
    double T4 = lapKernSSD(Z, beta);


    //fifth, now do scalar computations to get the solution
    double T3 = T4 - T1 - T2;

    //now compute MMD - need to multiple T1 and T2 by 2
    double c1 = 0;
    if (n1 > 1){
        c1 =  2.0 / ( n1 * (n1 - 1) );
    }
    double c2 = 0;
    if (n2 > 1){
        c2 =  2.0 / ( n2 * (n2 - 1) );
    }
    double c3 = -2.0 / (n1 * n2);

    double MMD_u = c1*T1 + c2*T2 + c3*T3;

    return MMD_u;
}



/**
 * Passing a vector Z instead
 *
 */
double emmd_pval_MH_alt(std::vector<double>::iterator Zstart, 
                        const std::vector<double>::size_type nX, 
                        const std::vector<double>::size_type nY, 
                        int numperm, int seednum, double beta, 
                        int twosided, int boundedminpval){

    std::vector<double>::iterator Zend = Zstart + nX + nY;

    // if beta is NOT a positive number, then compute using median heuristic
    if ( !(beta > 0) ){
        std::vector<double> Zcopy;
        //Zcopy.reserve(nX+nY);
        Zcopy.insert(Zcopy.end(), Zstart, Zend);
        beta = 1 / medianHeuristic(Zcopy);
    }

    // Will use iterators to pass  vector to FastHSIC
    double MMDstar = emmd_ptr_alt_MH(Zstart, nX, nY, beta);

    // std::cout << "cpp_emmd_pval_MH_alt MMDstar: " << MMDstar <<std::endl;

    // initiate random generator
    std::random_device rd;
    std::mt19937 g;
    // set the seed, if greater than 0
    if (seednum > 0){
        g.seed(seednum);
    } else {
        g.seed(rd());
    }

    double MMDperm = 0.0;
    int MMD_count_below = 1;

    //run the permutations
    for (int i=0; i < numperm; ++i){
        //shuffle and subset
        std::shuffle(Zstart, Zend, g);
        MMDperm = emmd_ptr_alt_MH(Zstart, nX, nY, beta);

        if (MMDperm < MMDstar){
            ++MMD_count_below;
        }
    }

    //now return the threshold
    double pval = MMD_count_below / (numperm + 1.0);

    //make one-sided
    //twosided = 0 means false
    //twosided = 1 means true
    if (twosided==1){
        //make two sided
        pval =  1 - std::abs(1 - 2*pval) ;
    } else {
        //one sided; counting below, so must make 
        // large values into small
        pval = 1 - pval;
    }

    // quick check for minimum possible p-value; avoids pval=0
    // 1 / 2(numperm+1)
    if (boundedminpval==1){
        double pmin = 0.5 / (numperm+1.0);
        if (pval < pmin)
            pval = pmin;
    }


    return pval;
}


/**
 * Hommel method for combining p-values with arbitrary dependency
 * want to return min_{i=1, 2, ..., n} { n (sum_{j=1}^{n} 1/j ) p_(i) / i } 
 * where p_(i) is ith smallest element of vector p.
 */
double hommelCombine(std::vector<double> pvec){
    std::size_t n = pvec.size();
    std::sort(pvec.begin(), pvec.end());
    double q = 1.0;
    double L = 0.0;
    double i = 0.0;
    // temporary variable
    double scaledp = 0.0; 
    for (std::vector<double>::iterator it = pvec.begin(); it != pvec.end(); ++it){
        i += 1;
        scaledp = (*it) / i;
        if (scaledp < q)
            q = scaledp;
        L += 1/i;
    }
    q = n * L * q;
    return q;
}



/**
 * Fisher's method for combingin p-values with arbitrarty dependency
 * want to return cdf value for 
 * stat = -2 * sum log(pvec)
 * k = length(pvec)
 * where cdf is computed by
 * pchisq(stat, 2*k, lower_tail=true)
 */
// double fisherCombine(std::vector<double> pvec){
//     int k = pvec.size();
//     double logsum = 0.0;
//     for (std::vector<double>::iterator it = pvec.begin(); it != pvec.end(); ++it){
//         logsum += std::log(*it);
//     }
//     double stat = -2 * logsum;
//     return ( 1 - cdf_chisq(stat, 2.0*k) );
// }


/**
 * Compute pval MMD with disyances to each point
 * from Heller and Heller (2016)
 *
 * relies on euMMdPermPval
 */
double cpp_meammd_dist_pval(double* X, double* Y, 
                        int nX, int dX,
                        int nY, int dY,
                        int numperm, 
                        int seednum, 
                        double beta, 
                        int pmethod,
                        int nmethod, 
                        int twosided, 
                        int boundedminpval){

    if (dX != dY){
        //error, dimensions incorrect
        return -1.0;
    }
    int dZ = dX;
    int nZ = nX + nY;
    int Xsize = nX*dX;
    int Ysize = nY*dY;
    int Zsize = Xsize + Ysize;

    // create vector Z
    std::vector<double> Z;
    Z.reserve(Zsize);
    //insert X then Y
    Z.insert(Z.end(), &X[0], &X[0]+Xsize);
    Z.insert(Z.end(), &Y[0], &Y[0]+Ysize);

    // need temporary vector for unviariate Z after distances
    std::vector<double> Zdist;
    Zdist.reserve(nZ);

    //create a vector of p-values
    std::vector<double> pval(nZ);
    //temporary p-value
    double p = 0.0;

    for (int i=0; i < nZ; ++i){

        // create temporary vector of length d
        std::vector<double>::const_iterator pos = Z.begin() + i*dZ;
        std::vector<double> v(pos, pos+dZ);

        // compute distances to v - now a univariate vector
        Zdist = projdistance(Z, nZ, dZ, v, nmethod);

        if (i < nX){
            // swap X[i] with X[nX]
            std::swap(Zdist[i], Zdist[nX-1]);
            // then swap X[nX] with last element of Y, Y[nY]
            std::swap(Zdist[nX-1], Zdist[nZ-1]);
            // then will find emmd with X[1..nX-1], Y[1...nY]
            // (last elements are ignored)
            p = emmd_pval_MH_alt(Zdist.begin(), nX-1, nY, numperm, seednum, beta, 
                                 twosided, boundedminpval);
            
        } else {
            // j = i - nX;
            // swap Y[j] with Y[nY]; but actually use index i
            std::swap(Zdist[i], Zdist[nZ-1]);
            // then will find emmd with X[1..nX], Y[1...nY-1]
            // (last elements are ignored)
            p = emmd_pval_MH_alt(Zdist.begin(), nX, nY-1, numperm, seednum, beta, 
                                 twosided, boundedminpval);
        }
        //p-value cannot be zero, must be at least 1/(numperm+1)
        pval[i] = p;

        // empty the vector v?
    }




    //now do Hommel combination
    double q = 0;
    if (pmethod==0){
        q = hommelCombine(pval);
    } else {
        //q = fisherCombine(pval);
        q = -1.0;
    }
    
    return q;
}


/**
 * Compute MMD with multiple projections; no shuffling
 *
 * Generate projection vector, project, find mmd
 *
 * Need to pass gen by reference...
 */
double cpp_meammd_proj_stat(double* X, double* Y, 
                        int nX, int dX,
                        int nY, int dY,
                        int numproj, 
                        int seednum,
                        double beta){

    if (dX != dY){
        //error, dimensions incorrect
        return -1.0;
    }
    std::size_t dZ = dX;

    //int should really be std::size_t
    std::size_t Xsize = nX*dX;
    std::size_t Ysize = nY*dY;
    std::size_t Zsize = Xsize + Ysize;
    //std::size_t nZ = nX + nY;

    // create vector for matrix
    std::vector<double> Z;
    Z.reserve(Zsize);
    //insert X then Y
    Z.insert(Z.end(), &X[0], &X[0]+Xsize);
    Z.insert(Z.end(), &Y[0], &Y[0]+Ysize);

    // create generator for projections
    //std::default_random_engine gen(seednum);
    std::default_random_engine gen;
    std::random_device rd;
    if (seednum > 0){
        gen.seed(seednum);
    } else {
        gen.seed(rd());
    }

    // just for intialisation
    // u vector generation
    std::vector<double> u = unifSd(dZ, gen);
    std::vector<double> Zproj = projection(Z, nX+nY, dZ, u);

    // MMD for this trial, and average MMD
    double MMDbar = 0.0;
    double MMDval = 0.0;

    for (int j=0; j < numproj; ++j){
        //get projection trial
        u = unifSd(dZ, gen);
        Zproj = projection(Z, nX+nY, dZ, u);

        // compute emmd
        MMDval = emmd_ptr_alt_MH(Zproj.begin(), nX, nY, beta);
        MMDbar = update_mean(MMDbar, MMDval, j+1);
    }
    return MMDbar;
}



/**
 * Compute MMD with multiple projections; no shuffling
 *
 * Generate projection vector, project, find mmd
 *
 * Need to pass gen by reference...
 */
double cpp_meammd_mult_proj(std::vector<double> Z, 
                        int nX, 
                        int nY,
                        int dZ,
                        int numproj, 
                        double beta, 
                        std::default_random_engine& gen){


    // just for intialisation
    // u vector generation
    std::vector<double> u = unifSd(dZ, gen);
    std::vector<double> Zproj = projection(Z, nX+nY, dZ, u);

    // MMD for this trial, and average MMD
    double MMDbar = 0.0;
    double MMDval = 0.0;

    for (int j=0; j < numproj; ++j){
        //get projection trial
        u = unifSd(dZ, gen);
        Zproj = projection(Z, nX+nY, dZ, u);

        // compute emmd
        MMDval = emmd_ptr_alt_MH(Zproj.begin(), nX, nY, beta);
        MMDbar = update_mean(MMDbar, MMDval, j+1);
    }
    return MMDbar;
}



/**
 * Shuffling matrix
 *
 * in vector form, and shuffling rows
 * Probably can do more efficiently.
 * The & let's the vector be modified, and changes in place will be returned.
 */
void shufflematrix(std::vector<double> &Z, int n, int d, std::vector<int> Zindex){
    //make copy
    std::vector<double> Zcopy(Z);

    // now shuffle the n ROWS of length d
    for (int i=0; i < n; ++i){
        for (int j=0; j < d; ++j){
            Z.at(i*d+j) = Zcopy.at(  Zindex.at(i)*d+j   );
        }
    }
    // returned by ref
} // end of shufflematrix


/*
 * Compute pval MMD with multiple projections
 * Other version will use distance
 *
 * Need to shuffle matrix
 */
std::vector<double> cpp_meammd_proj_pval(double* X, double* Y, 
                                         int nX, int dX,
                                         int nY, int dY,
                                         int numperm, 
                                         int numproj,
                                         int seednum, 
                                         double beta){


    // return vector is first pval, then statistic
    // there is no beta, because that depends on each projection
    std::vector<double> returnvec;

    if (dX != dY){
        //error, dimensions incorrect
        returnvec.push_back(-2.0); 
        returnvec.push_back(-2.0);
        return returnvec;
    }

    size_t dZ = dX;

    //int should really be size_t
    size_t Xsize = nX*dX;
    size_t Ysize = nY*dY;
    size_t Zsize = Xsize + Ysize;
    size_t nZ = nX + nY;

    // create vector for matrix
    std::vector<double> Z;
    Z.reserve(Zsize);
    //insert X then Y
    Z.insert(Z.end(), &X[0], &X[0]+Xsize);
    Z.insert(Z.end(), &Y[0], &Y[0]+Ysize);

    //now create indices 0, 1, ..., n-1
    // these will be 
    std::vector<int> Zindex(nX + nY);
    std::iota(Zindex.begin(), Zindex.end(), 0);

    // initiate random generator
    std::random_device rd;
    std::mt19937 g;
    // set the seed, if greater than 0
    if (seednum > 0){
        g.seed(seednum);
    } else {
        g.seed(rd());
    }

    // create generator for projections
    //std::default_random_engine gen(seednum);
    std::default_random_engine gen;
    if (seednum > 0){
        gen.seed(seednum);
    } else {
        gen.seed(rd());
    }


    //get the MMD value for this split, with random projections
    double MMDstarbar = cpp_meammd_mult_proj(Z, nX, nY, dZ, numproj, beta, gen);

    double MMDperm = 0.0;
    int MMD_count_below = 1;

    //run the permutations
    for (int i=0; i < numperm; ++i){
        //shuffle index vector
        std::shuffle(Zindex.begin(), Zindex.end(), g);
        //shuffle matrix according to index
        shufflematrix(Z, nZ, dZ, Zindex);

        //compute MMD from projections for shuffled matrix
        //MMDperm = cpp_meammd_mult_proj(Z, nX, nY, dZ, numproj, beta, gen);
        MMDperm = cpp_meammd_mult_proj(Z, nX, nY, dZ, numproj, beta, gen);

        if (MMDperm < MMDstarbar){
            ++MMD_count_below;
        }
    }

    //now return the threshold
    double pval = MMD_count_below / (numperm + 1.0);

    //make one-sided
    // pval = convertTwoSidedPvalueToOneSided(pval);
    pval =  1 - std::abs(1 - 2*pval) ;
//     return pval;

    returnvec.push_back(pval);
    returnvec.push_back(MMDstarbar);
    return(returnvec);
}
