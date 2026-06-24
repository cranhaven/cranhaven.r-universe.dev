#ifndef GUARD_eummd_h
#define GUARD_eummd_h


/* euMMD: Efficient univariate maximum mean discrepancy
 *
 * Fast Exact MMD for univariate vectors. Uses the Laplacian kernel.
 * Not passing by reference, because do not want to modify original values.
 * Returns a p-value.
 *
 * @param X std::vector<double>
 *
 * @param Y std::vector<double>
 *
 * @param beta The parameter/bandwidth for the kernel.
 *
 * @param numperm The number of permutations. The p-value is obtained
 *                by permutations.
 *
 * @param seednum The seed for the random number generator for the permutations.
 *
 * @details Complexity is O( n log n + Ln)
 *          L is number of permutations.
 *          O(n log n) step comes from median heuristic and initial sorts.
 *          Only two sorts: one for X and one for Y (uses std::sort).
 *          Note that this alternative method for extracting permutations
 *          using booleans will result in a different p-value, because
 *          the permutations are different.
 *          For example: nX=2, nY=3
 *          Z = [1, 2, 3, 4, 5]
 *          B = [T, T, F, F, F]
 *          Suppose the first perm of Z is:
 *          [2, 5][1, 4, 3]
 *          same as:
 *          [T, F, T, F, F]
 *          Using the permutation will result in:
 *          [1, 3][2, 4, 5]
 *
 *
 * @param seednum The value of the seed.
 * 
 * @see lapKernSSD
 *
 * @return A vector of three elements; the first is the p-value for the test, 
 *         the second is the statistic and the the third is the value of beta 
 *         used.
 */
std::vector<double> cpp_eummd_pval_faster(std::vector<double> X, 
                                          std::vector<double> Y, 
                                          double beta, int numperm, int seednum, 
                                          int twosided, int boundedminpval);


/* euMMD: calculation of statistic
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
 * @return A vector of two elements; the first is the statistic and 
 *         the second is the value of beta used.
 */
std::vector<double> cpp_eummd(std::vector<double> X, 
                              std::vector<double> Y, 
                              double beta);


// exposing for meammd
// std::vector<double> mergeTwoAlreadySorted(std::vector<double>& A, 
//                                           std::vector<double>& B);


// exposing for meammd
double lapKernSSD(const std::vector<double>& Z, double beta);

// exposing for meammd
double compute_eummd_faster(std::vector<double>::const_iterator Zstart,
                           const std::vector<double>::size_type n1, 
                           const std::vector<double>::size_type n2, 
                           std::vector<bool>::const_iterator permstart, 
                           std::vector<bool>::const_iterator permend, 
                           const double T4, 
                           const double beta);

#endif
