#include <algorithm>
#include <iostream>
#include <vector>
#include <math.h>
#include <random>

#include "medianHeuristic.h"

/**
 * Merge two already sorted vectors into a third vector. 
 *
 * Note that it is a template function and the values are of type
 * Comparable (although this is hidden in vectype).
 * Not exposed.
 *
 * @param A First vector, sorted.
 * 
 * @param B Second vector, sorted.
 * 
 * @return C The merged vector, the sorted vector which is the merge of two other sorted vectors.
 */
std::vector<double> mergeTwoAlreadySorted(std::vector<double>& A, 
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




/* Sorted sum of distances with Laplacian Kernel
 *
 * Compute the sorted sum of distances for a vector, 
 * while using the Laplacian kernel.
 *
 * @param Z Some kind of vector. 
 *
 * @param beta The parameter for the Laplacian kernel.
 *
 * @see sortedSumofDist
 *
 * @return The sorted sum of distances using the Laplacian 
 *         kernel with parameter beta.
 */
    double lapKernSSD(const std::vector<double>& Z, double beta){
        // row sum for first row
        double R = 0.0;
        // row sum for second row
        double S = 0.0;

        // not starting for loop from 0, but from 1
        // note that not i-1 but is i in R += i * D
        // because of indexing from 0, and running to n-1
        // so create iterator and increase by 1
        std::vector<double>::const_iterator it = Z.begin();
        double Z_i = *it;
        double Z_i_1 = *it;
        ++it;
        for (; it != Z.end(); ++it){
            Z_i = *it;
            R = (R+1) * std::exp(  -beta * (Z_i - Z_i_1)  );
            S += R;
            Z_i_1 = Z_i;
        }
        return S;
    }


/* FastMMD calculation
 * 
 * Computes MMD statistic from 
 *
 * @param Zstart Constant iterator at start of Z vector.
 *
 * @param n1 Size of first sample.
 *
 * @param n2 Size of second sample.
 *
 * @param permstart Constant iterator at start of perm bool vector.
 *
 * @param beta The parameter/bandwidth for the kernel.
 *
 * @details Assumes Z is sorted.
 *
 * @see lapKernSSD
 *
 * @return MMD statistic.
 */
double compute_eummd_faster(std::vector<double>::const_iterator Zstart,
                           const std::vector<double>::size_type n1, 
                           const std::vector<double>::size_type n2, 
                           std::vector<bool>::const_iterator permstart, 
                           std::vector<bool>::const_iterator permend, 
                           const double T4, 
                           const double beta){

    // MMD_u statistic
    double MMD_u = 0.0;

    // initialise X and Y vectors from Z and reserve space for push_back
    std::vector<double> X; 
    X.reserve(n1);
    std::vector<double> Y; 
    Y.reserve(n2);

    //define iterators
    std::vector<double>::const_iterator itZ = Zstart;
    std::vector<double>::const_iterator Zend = Zstart + n1+n2 ;

    std::vector<bool>::const_iterator itperm = permstart;
    // permend already defined

    // run through permutation (filter); if true, goes to X, else goes to Y
    // code is a bit longer, but verctor only run through once now
    // Could also filter Z into X and Y using std::copy_if
    // https://stackoverflow.com/questions/21204676/modern-way-to-filter-stl-container
    for ( ; (itperm != permend) && (itZ != Zend); ++itperm, ++itZ){
        if (*itperm==true){
           X.push_back(*itZ);
        } else {
           Y.push_back(*itZ);
        }
    }


    // compute other terms; two O(n) steps
    double T1 = lapKernSSD(X, beta);
    double T2 = lapKernSSD(Y, beta);
    double T3 = T4 - T1 - T2;

    //now compute MMD - need to multiply T1 and T2 by 2
    double c1 = 0;
    if (n1 > 1){
        c1 =  2.0 / ( n1 * (n1 - 1) );
    }
    double c2 = 0;
    if (n2 > 1){
        c2 =  2.0 / ( n2 * (n2 - 1) );
    }
    double c3 = -2.0 / (n1 * n2);

    MMD_u = c1*T1 + c2*T2 + c3*T3;
    return(MMD_u);
}


std::vector<double> cpp_eummd_pval_faster(std::vector<double> X, 
                                          std::vector<double> Y, 
                                         double beta, int numperm, int seednum, 
                                         int twosided, int boundedminpval){


    // return vector is first pval, then statistic
    std::vector<double> returnvec;

    // sort each set
    // O(n1 log n1 + n2 log n2) = O(n log n)
    std::sort(X.begin(), X.end());
    std::sort(Y.begin(), Y.end());

    // merge two sorted vectors into Z
    // O(n)
    std::vector<double> Z = mergeTwoAlreadySorted(X, Y);

    // if beta is NOT a positive number, then compute using median heuristic
    // O(n log n)
    if ( !(beta > 0) ){
        // // making a copy, just in case
        // std::vector<double> Zcopy;
        // Zcopy.insert(Zcopy.end(), Zstart, Zend);
        double medianDiff = medianHeuristicAlreadySorted(Z);
        // beta is set to 1/ median difference
        beta = 1.0 / medianDiff;
    }

    // the triangular sum of the whole set; used in all calculations 
    // O(n)
    double T4 = lapKernSSD(Z, beta);

    const std::vector<double>::size_type n1 = X.size();
    const std::vector<double>::size_type n2 = Y.size();

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
    std::vector<double> XY(X);
    XY.insert( XY.end(), Y.begin(), Y.end() );
    std::vector<double>::const_iterator XYstart = XY.begin();
    double MMDstar = compute_eummd_faster(XYstart, n1, n2, 
                                         perm.begin(), perm.end(), 
                                         T4, beta);

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
    //counting the value already computed
    int MMD_count_below = 1;

    //run the permutations
    for (int i=0; i < numperm; ++i){
        //shuffle permutation and compute MMD
        std::shuffle(perm.begin(), perm.end(), g);
        // pass Z, which is sorted
        MMDperm = compute_eummd_faster(Z.begin(), n1, n2, 
                                      perm.begin(), perm.end(), 
                                      T4, beta);

        if (MMDperm < MMDstar){
            ++MMD_count_below;
        }
    }

    //now return the threshold; this will be the pvalue
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

    // return vector is first pval, then statistic
    returnvec.push_back(pval);
    returnvec.push_back(MMDstar);
    returnvec.push_back(beta);
    return returnvec;
} // end of cpp_eummd_pval_faster



// only called once by cpp_eummd
// may compute beta if < 0
std::vector<double> eummd_ptr_alt(std::vector<double>::const_iterator Zstart, 
                const std::vector<double>::size_type nX, 
                const std::vector<double>::size_type nY, 
                double beta){

    // return vector is first statistic, then beta 
    std::vector<double> returnvec;

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

    //third, merge the two sorted vectors into a third
    std::vector<double> Z = mergeTwoAlreadySorted(X, Y);

    //(optional) compute median heuristic
    // if beta is NOT a positive number, then compute using median heuristic
    // O(n log n)
    if ( !(beta > 0) ){
        // // making a copy, just in case
        // std::vector<double> Zcopy;
        // Zcopy.insert(Zcopy.end(), Zstart, Zend);
        double medianDiff = medianHeuristicAlreadySorted(Z);
        // beta is set to 1/ median difference
        beta = 1.0 / medianDiff;
    }

    //fourth, get the first two sorted sums
    double T1 = lapKernSSD(X, beta);
    double T2 = lapKernSSD(Y, beta);

    //fifth, compute the triangular sum for all the values
    double T4 = lapKernSSD(Z, beta);

    //sixth, now do scalar computations to get the solution
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

    // push statistic and beta to returnvec
    returnvec.push_back(MMD_u);
    returnvec.push_back(beta);
    return returnvec;
} //end eummd_ptr_alt



std::vector<double> cpp_eummd(std::vector<double> X, 
                              std::vector<double> Y, double beta){
     
    std::vector<double> Z(X); 
    // then insert Y into it
    Z.insert(Z.end(), Y.begin(), Y.end());

    return eummd_ptr_alt(Z.begin(), X.size(), Y.size(), beta);
}
