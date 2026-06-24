#include<vector>        // std::vector
#include<algorithm>     // std::sort
#include<numeric>     // std::iota

//for std::size_t
#include <cstddef>
// for std::abs
//#include <cstdlib>
// #include <math.h>
#include <cmath>


/**
 * Partial Insertion sort
 * Sorts the vector between two given indices.
 * Follows Insertion Sort approach from CLRS.
 *
 * @param A Vector of values
 *
 * @param p_index, starting index of vector to be sorted
 *
 * @param r_index, final index of vector to be sorted
 *
 * @return Nothing returned, vector modified in-place
 */
template <class vectype>
void insertionSortPartial(vectype& A, std::size_t p_index, std::size_t r_index){
    typename vectype::iterator p = A.begin();
    typename vectype::iterator rr = A.begin();
    // advance iterators to appropriate place; rr is r+1
    std::advance(p, p_index);
    std::advance(rr, r_index+1);
    for (typename vectype::iterator j = p+1; j < rr; ++j){

        typename vectype::value_type key = *j;
        typename vectype::iterator i = j-1;
        while ( (i >= p) && (*i > key) ){
            *(i+1) = *i;
            --i;
        }
        *(i+1) = key;
    }
}

/**
 *  Partition function from Quicksort algorithm.
 *
 *  @details Rearranges A[p..r] in place, selecting 
 *           A[r] as the pivot element, and then 
 *           final order of A[p..r] has
 *           all elements < A[r], A[r] and all elements > A[r],
 *           and returns position of pivot.
 *           Reference: CLRS, Section 7.1.
 *  
 * @param A Vector
 * @param p left index, >= 0
 * @param r right index, less than length of A
 *  
 * @return index of pivot element (index where A[r] is moved) and modifies array in place. 
 *  Example:
 *       [2, 8, 7, 1, 3, 5, 6, 4]
 *       is partitioned round 4 to become
 *       [2, 1, 3, 4, 7, 5, 6, 8]
 *       (from CLRS, Fig. 7.1)
 *  
 */
template <class vectype>
std::size_t partition(vectype& A, std::size_t p_index, std::size_t r_index){
    // set up iterators p and r
    typename vectype::iterator r = A.begin();
    typename vectype::iterator p = A.begin();
    std::advance(p, p_index);
    std::advance(r, r_index);

    // obtaining last element
    //auto x = *r;
    typename vectype::value_type x = *r;

    // another iterator
    typename vectype::iterator i = p-1; 

    for (typename vectype::iterator j = p; j < r; ++j){
        //compare elements to last element
        if (*j < x){
            ++i;
            std::swap(*i, *j);
        }
    }
    //swap A[i] and A[j]
    std::swap(*r, *(i+1));
    // use distance to obtain index
    return std::distance(A.begin(), i+1);
}


/**
 *  Partition function from Quicksort algorithm, around element provided.
 *
 *  @details Calls partition function above after finding element 
 *           x in A and swapping x with last element in A.
 *           If x is not in A, then returns -1 and leaves A unchanged.
 *           Reference: CLRS, Section 7.1.
 *  
 * @param A Vector
 * @param x Element in A
 *  
 * @return index of pivot element (index where A[r] is moved) and modifies array in place. 
 *  Example:
 *       [2, 8, 7, 1, 3, 5, 6, 4]
 *       is partitioned round 4 to become
 *       [2, 1, 3, 4, 7, 5, 6, 8]
 *       (from CLRS, Fig. 7.1)
 *  
 */
template <class vectype>
std::size_t partitionElement(vectype& A, typename vectype::value_type x){
    // r is initialised to almost end of array
    typename vectype::iterator r = A.end() - 1;

    //find the index in A of x; saved as index
    typename vectype::iterator i = A.begin();
    bool notFound = true;
    //index initialised as last element
    typename vectype::iterator index = r;
    while ((notFound) and (i < r)){
        if (*i == x){
            index = i;
            notFound = false;
        }
        ++i;
    }
    //if not found, index will be r
    // check actually found, or index==r, else return error; which is -1
    if ( (notFound) and (*index != x) ){
        return -1;
    }

    // so we have found the index where A[index] = x
    // now swap A[index]==x with A[r]; can replace A[r] with x
    *index = *r;
    *r = x;

    //now call original parition function
    std::size_t r_index = std::distance(A.begin(), r);
    return partition(A, 0, r_index);
}



/**
 *  Get index of the (lower) median in a SORTED vector of length n
 *
 * @details Since the vector is assumed to be sorted, we have two cases.
 *          If n is odd, then the middle element is returned.
 *          (e.g. for 5 elements, the index to the 3rd element, 
 *          which in Python is index=2, is returned.)
 *          If n is even, then the 'lower' middle element is returned.
 *          (e.g. for 4 elements, the index to the 2nd element, 
 *          which in Python is index=1, is returned.)
 *
 * @param n The length of a list/vector.
 *  
 * @return index of index of the (lower) median
 *
 *  Example:
 *       n=4, returns 1 (2nd element).
 *       n=5, returns 2 (3rd element).
 *       n=6, returns 2 (3nd element).
 *
 */
std::size_t getMedianIndex(std::size_t n){
    // get remainder, which is either 0 (perfect division) or 1 (remainder)
    std::size_t r = n % 2;
    // transform remainder because if perfect division, want to subtract 1
    r = 1 - r;
    std::size_t index = (n / 2) - r;
    // check degenerate case
    if (n==0)
        index = 0;
    return index;
}


/**
 *  Get index of for the median difference in a set of (ordered) differences
 *
 * @param n The length of a list/vector.
 *  
 * @details Suppose there are n elements; then there are n*(n-1)/2 pairs, and
 *          the median will be the middle elements of n*(n-1)/2.
 *          Uses getMedianIndex.
 *
 * @return index of index of the (lower) median for the differences
 *
 *  Example:
 *       n=3, then there are 6/2 = 3 pairs, so returns 1 (2nd element).
 *       n=4, then there are 12/2=6 pairs, returns 2 (3rd element).
 *       n=5, then there are 20/2=10 pairs, returns 4 (5th element).
 *
 */
std::size_t getMedianIndexForDiffs(std::size_t n){
    std::size_t nn = n*(n-1) / 2;
    std::size_t m = getMedianIndex(nn);
    return( m );
}


/**
 * Get vector of medians, where the medians are from the blocks of q=5
 *
 * @param A A list of numbers
 *
 * @details Note also that A is sorted in blocks of q=5; this is 
 *          part of the original implementation. See CLRS, Section 9.3
 *          for more details.
 *
 *
 * @returns Returns a vector of the medians from the blocks of q=5.
 *          There might also be a last group of size less than 5, in which
 *          case the (lower) median of that is included.
 *
 */
template <class vectype>
vectype getMedianVector(vectype& A){
    std::size_t m, n, p, q, r, rr, rem; 
    n = A.size();

    //=======//
    // Step 1:
    //=======//
    // 'magic' quotient is 5
    q = 5;

    // number of groups of 5; integer division
    m = n / q;

    // is there an extra small group?
    rem = n % q;
    // any extra vector
    rr = 0;
    if ( rem > 0 )
       rr = 1; 

    //=======//
    // Step 2:
    //=======//
    // initialize vector
    vectype medVector(m+rr); 

    // median index for an array of length q
    std::size_t medIndex = getMedianIndex(q);

    // now work through array, A[p..r], increasing p and r by q
    p = 0;
    // must subtract 1, so r = 0 + 5 - 1 = 4, for [p, q] = [0, 4]
    r = p + q - 1;
    while (r < n){
        // insertion sort A[p..r] 
        insertionSortPartial(A, p, r);

        // increment starting point
        p += q;
        // increment r, right-hand limit of array
        r = p + q - 1;
    }

    // sort any remaining elements
    if (p < n)
        insertionSortPartial(A, p, n-1);


    //=======//
    // Step 3:
    //=======//
    // fill median vector with values

    // end of groups of q in medVector
    typename vectype::iterator m_it = medVector.begin();
    std::advance(m_it, m);

    // location of medIndex
    typename vectype::iterator thisMediter = A.begin();
    std::advance(thisMediter, medIndex);

    // now walk through the vector and extract groups
    for (typename vectype::iterator j = medVector.begin(); j < m_it; ++j){
        // extract median
        *j = *thisMediter;

        // increment median location to next q block
        thisMediter += q;
    }

    // check for any additional elements
    if (rem > 0){
        // partial sort last remaining elements; actually, all done already
        //insertionSortPartial(A, m*q, n-1);

        // get the index
        thisMediter = A.begin() + getMedianIndex(n - m*q) + m*q;

        // will be last indexed
        *m_it = *thisMediter;
    }

    return medVector;
}



/**
 *  Finds the ith smallest element in the array A in worst-case
 *  O(n) time. Reference: CLRS, Section 9.3.
 *
 * @param A List of length n
 *
 * @param integer, 1 <= i <= n
 *
 *  @details: Requires the functions 
 *             - insertionSortPartial
 *             - partitionElement
 *            Warning: 
 *                 The order of the elements is changed in the course of the search, 
 *                 but this is an unwanted side effect so passing vector 
 *                 by VALUE and NOT by reference.
 *
 *  @returns ith smallest element in A.
 *
 */
template <class vectype>
typename vectype::value_type select(vectype A, std::size_t i){
    //length of A
    std::size_t n = A.size();

    //=======//
    // Step 0:
    //=======//
    // If A has length 1, return that element.
    // This is needed later and is one of the stopping conditions.
    if (n==1){
        // no need for iterator; established that there is one element.
        return A[0];
    }

    // error checking based on i begin too small or too large
    if ( (i < 1) or (i > n) ){
        throw "select needs parameter 1 <= i <= n, where n is length of A.";
    }

    //=======//
    // Steps 1 and 2:
    //=======//
    vectype medianVector = getMedianVector(A);

    //=======//
    // Step 3:
    //=======//
    // We now have the vector of medians
    // RECURSIVELY use select to find the median of these medians
    // This will return a value when the length of the vector is 1
    // NOTE: NEED TO ADD 1, because need RANK, not index
    //       (previously, when not adding 1, select would have accepted 0 
    //        and would still have worked.)
    typename vectype::value_type medOfMed = select( medianVector, getMedianIndex(medianVector.size()) + 1 );


    //=======//
    // Step 4:
    //=======//
    // Now partition around median of medians; k_index is the index.
    // Index is the number of elements on the low side of the partition.
    // Rememeber C++ counts from zero.
    std::size_t k = partitionElement(A, medOfMed);

    //=======//
    // Step 4:
    //=======//
    // Recursively call subarrays to find ith element
    // need to compare with k+1, rather than k
    if ( i==(k+1) ){
        return medOfMed;
    } else if ( i < (k+1) ) {
        // use select on  subarray B = A[0:(k+1)]
        vectype B( A.begin()+0, A.begin()+(k+1) );
        return select(B, i);
    } else {
        // use select on subarray B = A[(k+1):n]
        vectype B( A.begin()+(k+1), A.end() );
        return select(B, i-k-1);
    }
}




/**
 *  Computes the weighted median as defined in 
 *  Johnson & Mizoguchi (1978).
 *  Using implementation given in Croux & Rousseuw (1990)
 *
 * @param A Vector of numbers (unsorted)
 *
 * @param W Vector of weights, such that W[i] is the weight of A[i].
 *          Uses type weighttype, because weights (probably std::size_t) are
 *          different to contents of vector A.
 *
 * @details  Given an unordered sequence of n numbers
 *           a_1, a_2, ..., a_n,
 *           not necessarily sorted, and the n weights
 *           w(a_1), w(a_2), ..., w(a_n), the weighted median can be computed.
 *           Suppose that an ordering on the a_i's is
 *           a_i1, a_i2, ..., a_in, i.e. a_i1 <= a_i2 <= ... <= a_in.
 *           Then the weighted median is a_im, where m is the index satisfying
 *           sum_{j <= m} w(a_ij)  >=  sum_{j > m} w(a_ij)
 *           and 
 *           sum_{j < m} w(a_ij)  <=  sum_{j >= m} w(a_ij).
 *           In other words, a_im is the element on the boundary of the sums
 *           of the weights.
 *
 *  @returns Weighted median, according to Johnson and Mizoguchi (1978),
 *           element of A.
 *
 *  Warning:
 *       Order of A and W is changed, so passed by value.
 *
 */
template <class vectype, class weighttype>
typename vectype::value_type weightedMedian(vectype A, weighttype W){

    // length of A
    std::size_t n = A.size();

    // temporary length of A, which will be changed later
    std::size_t nn = n;

    // will store a_m in here later
    typename vectype::value_type Amed;

    // temporary vectors for A and W
    vectype Atemp (n);
    weighttype Wtemp (n);

    //create iterators for use later
    typename vectype::iterator ia, ka;
    typename weighttype::iterator iw, kw;
    typename vectype::const_iterator iaend, kaend;
    typename weighttype::const_iterator iwend, kwend;


    //total of weights
    std::size_t wtotal = 0;
    for (typename weighttype::const_iterator iwc = W.begin(); iwc != W.end(); ++iwc){
        wtotal += *iwc;
    }

    // rest of weights; i.e. the weights excluded, and left, mid, right
    std::size_t wrest = 0;
    std::size_t wleft = 0;
    std::size_t wmid = 0;
    std::size_t wright = 0;

    // count checks that while loop is not infinite
    std::size_t count = 0;
    std::size_t maxcount = 100;
    // control for while loop; weighted median not found
    bool notFound = true;
    while (notFound and (count < maxcount)){
        // increment count
        count += 1;

        // find median of A, use select, pass a copy of part of A[0:nn]
        Amed = select( vectype(A.begin(), A.begin()+nn) , (std::size_t) nn/2 + 1);

        // re-initialise weights on the left, middle and right
        wleft = 0;
        wmid = 0;
        wright = 0;

        // run through and compute weights on left, right and middle
        ia = A.begin();
        iw = W.begin();
        iaend = A.begin() + nn;
        iwend = W.begin() + nn;
        for (; ( (ia != iaend) and (iw != iwend) ); ++ia, ++iw){
            if (*ia < Amed){
                wleft += *iw;
            } else {
                if (*ia > Amed) {
                    wright += *iw;
                } else {
                    wmid += *iw;
                }
            }
        }

        // reset iterators; do not need to do it in if, only need to do it here
        ia = A.begin();
        iw = W.begin();
        // another iterator, no need to keep track of k, can infer from ka
        ka = Atemp.begin();
        kw = Wtemp.begin();
        kaend = Atemp.begin() + nn;
        kwend = Wtemp.begin() + nn;

        // now check if twice the left weights (and rest) is more than total
        if (  (2*wrest + 2*wleft) > wtotal  ){
            for (; ( (ia != iaend) and (iw != iwend) ); ++ia, ++iw){
                // checking here that have not reached end of Atemp and Wtemp
                if ( (*ia < Amed) and (ka != kaend) and (kw != kwend) ){
                    *ka = *ia;
                    *kw = *iw;
                    ++ka;
                    ++kw;
                }
            }
            // now can set new length of vector nn, using distance from k
            nn = std::distance(Atemp.begin(), ka);
        } else {

            if (  (2*wrest + 2*wleft + 2*wmid) > wtotal  )
                notFound = false;
            else{
                for (; ( (ia != iaend) and (iw != iwend) ); ++ia, ++iw){
                    // checking here that not reached end of Atemp and Wtemp
                    if ( (*ia > Amed) and (ka != kaend) and (kw != kwend) ){
                        *ka = *ia;
                        *kw = *iw;
                        ++ka;
                        ++kw;
                    }
                }
                // now can set new length of vector nn, using distance from k
                nn = std::distance(Atemp.begin(), ka);
                // also need to modify wrest; remaining weight
                wrest = wrest + wleft + wmid;
            } // end of nested if-else

        } // end main if-else block

        // copy Atemp into A, Wtemp into W
        // reset iterators again
        ia = A.begin();
        iw = W.begin();
        ka = Atemp.begin();
        kw = Wtemp.begin();
        // nn will be the same as k now, after if statement
        iaend = A.begin() + nn;
        iwend = W.begin() + nn;
        kaend = Atemp.begin() + nn;
        kwend = Wtemp.begin() + nn;
        for (; ( (ia != iaend) and (iw != iwend) and (ka != kaend) and (kw != kwend)); 
                ++ia, ++iw, ++ka, ++kw) {
            *ia = *ka;
            *iw = *kw;
        }

    } //end of while
    if (count >= maxcount){
        //TODO: test this exception, if possible
        throw "weightedMedian did not converge!";
    }

    return Amed;
}



/**
 * Kth Pair of differences X-X following the Croux & Rousseuw (1990) implementation.
 *
 * Implementation is O( n log n ).
 *
 * @param X Vector of length n.
 *
 * @param K (int) value between 1 and m*n, inclusive.
 *
 * @details Relies on several function above, including
 *          - weightedMedian O(n)
 *          - select O(n)
 *          Function changes order of X, so passed by value rather than by reference.
 *
 * @returns The Kth smallest number z where z = |x_i - x_j| among
 *          all n(n-1)//2 pairs { |x_i - x_j| : i < j }.
 */
template <class vectype>
typename vectype::value_type kthDiffAlreadySorted(vectype X, std::size_t K){

    std::size_t n = X.size();
    std::size_t Kmax = n*(n-1) / 2;
    if (  (K < 1) or (K > Kmax)  ){
        throw "K parameter in kthDiff(AlreadySorted) must be 1 <= K <= n*(n-1)/2 (for X vector of size n).";
    }

    // helps to find K
    std::size_t jhelp = n*(n+1) / 2;
    K = K + jhelp;

    // need to create several auxilliary vectors
    // initialising all to zero
    std::vector<std::size_t> Left(n, 0);
    std::vector<std::size_t> Right(n, 0);
    std::vector<std::size_t> Weight(n, 0);
    std::vector<std::size_t> P(n, 0);
    std::vector<std::size_t> Q(n, 0);
    // A is called work in original Croux and Rousseuw implementation
    vectype A(n, 0.0);

    // need to initialise Left[i] = n- i + 1
    std::size_t c = 0;
    for (std::vector<std::size_t>::iterator iL = Left.begin(); iL != Left.end(); iL++){
        *iL = n - c + 1;
        ++c;
    }
    // need to fill Right[i] = n; could have done this in initialisation, 
    // but prefer to explicitly do it here
    for (std::vector<std::size_t>::iterator iR = Right.begin(); iR != Right.end(); iR++){
        *iR = n;
    }

    // these two values keep track of how many candidates for kthDiff are left
    std::size_t nL = jhelp;
    std::size_t nR = n*n;

    // temporary variables
    std::size_t j, sumP, sumQ;
    typename vectype::value_type trial, Qn;


    // helper iterators/indices
    std::vector<std::size_t>::iterator iL, iR, jW, iP, iQ; 
    typename vectype::const_iterator iX, n_minus_jhelpX, jX;
    typename vectype::iterator jA;
    
    bool notFound = true;
    while ( notFound and ((nR - nL) > n) ){

        // initialise iterators
        iL = Left.begin();
        iR = Right.begin();
        iX = X.begin();
        jW = Weight.begin();
        jA = A.begin();

        for (; (iL != Left.end()) and (iR != Right.end()) and (iX != X.end()); 
                ++iL, ++iR, ++iX){

            // reset n_minus_jhelp_x
            n_minus_jhelpX = X.begin();

            if ( (*iL <= *iR) and (jW != Weight.end()) and (jA != A.end()) ){
                *jW = *iR - *iL + 1;
                jhelp = *iL + (*jW / 2);

                // need to advance iterator
                std::advance(n_minus_jhelpX, n-jhelp);

                *jA = *iX - *n_minus_jhelpX;
                ++jW;
                ++jA;
            }

        }

        // calling weighted median but need to be a bit careful here, 
        // because only want the first j elements, from 0 to j-1
        vectype Asub = vectype(A.begin(), jA);
        std::vector<std::size_t> Wsub = std::vector<std::size_t>(Weight.begin(), jW);
        trial = weightedMedian( Asub, Wsub );

        j = 0;
        iX = X.end();
        iP = P.end();
        jX = X.end() - 1;
        for (; (iP != P.begin()) and (iX != X.begin()); --iX, --iP){
            while (  (j < n) and (jX >= X.begin()) and  ( (*(iX-1) - *jX) < trial )  ){
                ++j;
                --jX;
            }
            *(iP-1) = j;
        }

        j = n+1;
        // i from 0 to n-1, steps of +1
        iX = X.begin();
        jX = X.begin(); 
        iQ = Q.begin();
        for (; (iX != X.end()) and (iQ != Q.end()); ++iX, ++iQ){
            //find appropriate j for Q
            while (  ( jX != X.end() ) and ( *iX - *jX ) > trial  ){
                ++jX;
                --j;
            }
            *iQ = j;
        }

        // add up P
        sumP = 0;
        for (iP = P.begin(); iP != P.end(); ++iP){
            sumP += *iP;
        }


        sumQ = 0;
        for (iQ = Q.begin(); iQ != Q.end(); ++iQ){
            sumQ += (*iQ - 1);
        }

        if (K <= sumP){
            iP = P.begin();
            iR = Right.begin();
            for (; (iP != P.end()) and (iR != Right.end()); ++iP, ++iR){
                *iR = *iP;
            }
            nR = sumP;
        } else {
            if (K > sumQ){
                iQ = Q.begin();
                iL = Left.begin();
                for (; (iQ != Q.end()) and (iL != Left.end()); ++iQ, ++iL){
                    *iL = *iQ;
                }
                nL = sumQ;
            } else {
                Qn = trial;
                notFound = false;
            }
        } //end of if-else block

    } // end of while


    if (notFound){
        // using iterators, it is not pretty but it works
        jA = A.begin();
        iL = Left.begin()+1;
        iR = Right.begin()+1;
        iX = X.begin()+1;
        for (; (iL != Left.end()) and (iR != Right.end()) and (iX != X.end()); 
                ++iL, ++iR, ++iX){
            if (*iL <= *iR){
                for (std::size_t jj = *iL; jj <= *iR; ++jj){
                    jX = X.begin();
                    std::advance(jX, n-jj);
                    // check everything is within the limits
                    if ( (jA != A.end()) and (X.begin() <= jX) and (jX <= X.end()) ){
                        *jA = *iX - *jX;
                        ++jA;
                    }
                }
            }
        }
        // select the (K - nL)th smallest element in A[1:j-1]
        Qn = select(  vectype( A.begin(), jA ), K-nL  );
    }

    return Qn;
}


/**
 * Kth Pair of differences X-X following the Croux & Rousseuw (1990) implementation.
 *
 * Implementation is O( n log n ).
 *
 * @param X Vector of length n.
 *
 * @param K (int) value between 1 and m*n, inclusive.
 *
 * @details Relies on several function above, including
 *          - kthDiffAlreadySorted
 *          - mergesort_bottomup_iterative O(n log n)
 *          - weightedMedian O(n)
 *          - select O(n)
 *          Function changes order of X, so passed by value rather than by reference.
 *
 * @returns The Kth smallest number z where z = |x_i - x_j| among
 *          all n(n-1)//2 pairs { |x_i - x_j| : i < j }.
 */
template <class vectype>
typename vectype::value_type kthDiff(vectype X, std::size_t K){
    std::size_t n = X.size();
    std::size_t Kmax = n*(n-1) / 2;
    if (  (K < 1) or (K > Kmax)  ){
        throw "K parameter in kthDiff must be 1 <= K <= n*(n-1)/2 (for X vector of size n).";
    }

    // sort the elements
    std::sort(X.begin(), X.end());

    return kthDiffAlreadySorted(X, K);
}


/**
 * Median heuristic using the Croux & Rousseuw (1990) implementation.
 *
 * @param X Vector of length n.
 *
 * @details Uses getMedianIndexForDiffs to get index, adds 1 for rank, and then
 *          uses kthDiff function above to obtain the median absolute difference.
 *
 * @returns Absolute value of Kth smallest difference; among all distinct
 *          pairs of X, i.e. Kth smallest element in { |x_i - x_j| | i < j}.
 */
// template <class vectype>
// typename vectype::value_type medianHeuristic(vectype X){

double medianHeuristic(std::vector<double> X){
    // median index + 1, because we need the RANK, not the index.
    std::size_t m = getMedianIndexForDiffs( X.size() ) + 1;
    // use kthDiff function
    return( kthDiff(X, m) );
}

/**
 * Median heuristic using the Croux & Rousseuw (1990) implementation.
 *
 * @param X Vector of length n.
 *
 * @details Uses getMedianIndexForDiffs to get index, adds 1 for rank, and then
 *          uses kthDiff function above to obtain the median absolute difference.
 *
 * @returns Absolute value of Kth smallest difference; among all distinct
 *          pairs of X, i.e. Kth smallest element in { |x_i - x_j| | i < j}.
 */
double medianHeuristicAlreadySorted(std::vector<double> X){
    // median index + 1, because we need the RANK, not the index.
    std::size_t m = getMedianIndexForDiffs( X.size() ) + 1;
    // use kthDiff function
    return( kthDiffAlreadySorted(X, m) );
}



/**
 * Compute median heuristic by going through all pairs
 * Need to pass dimensions, d and n
 *
 * kmethod: 1 for Laplacian kernel, anything else is Gaussian kernel
 */
double naive_multiv_medianHeuristic(const std::vector<double>& Z, 
                                    int dZi, 
                                    int nZi, 
                                    int kmethod=1){
    // arg will be the 1-norm
    double arg=0;
    // difference in values; used for both Laplacian and Gaussian kernels
    double diff=0;

    // cast to size_t
    std::size_t dZ = static_cast<std::size_t>(dZi);
    std::size_t nZ = static_cast<std::size_t>(nZi);


    // number of diffs
    std::size_t numDiffs = nZ * (nZ-1) / 2;

    //now create indices
    std::vector<std::size_t> Zindex(nZ);
    std::iota(Zindex.begin(), Zindex.end(), 0);

    // reserve and push_back is apparently fast
    std::vector<double> diffsVec;
    diffsVec.reserve(numDiffs);
    
    // i runs from 0, 1, ..., nZ-2
    // j runs from i+1 to nZ-1
    for (std::size_t i = 0; i < (nZ-1); ++i){
        for (std::size_t j = i+1; j < nZ; ++j){
            // arg is the 1-norm; need to reset
            arg = 0;
            for (std::size_t alpha=0; alpha < dZ; ++alpha){
                diff = Z.at(Zindex.at(i)*dZ +alpha) - Z.at(Zindex.at(j)*dZ +alpha);
                if (kmethod==1){
                    arg += std::fabs( diff ); 
                } else {
                    arg += diff * diff;
                }
            }
            // fill vector 
            diffsVec.push_back(arg);
        }
    }

    //now sort (lazy way)
    std::sort( diffsVec.begin(), diffsVec.end() );

    // and extract middle value
    std::size_t mid = getMedianIndex(numDiffs);

    return diffsVec.at(mid);
}


