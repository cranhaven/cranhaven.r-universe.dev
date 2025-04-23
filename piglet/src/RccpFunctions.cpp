#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <unordered_set>

// Check for OpenMP support
#ifdef _OPENMP
#include <omp.h>
#define PARALLEL_FOR _Pragma("omp parallel for")
#else
#define PARALLEL_FOR
#pragma message("OpenMP not supported. Compilation will proceed without parallel execution.")
#endif

// ------------------------------------------------------------------------------
// 4. allele_diff_indices_parallel2
//' Calculate SNPs or their count for each germline-input sequence pair with optional parallel execution.
//'
//' This function compares germline sequences (`germs`) and input sequences (`inputs`)
//' and identifies single nucleotide polymorphisms (SNPs) or their counts, with optional parallel execution.
//' The comparison ignores specified non-mismatch characters (e.g., gaps or ambiguous bases).
//'
//' @param germs A vector of strings representing germline sequences.
//' @param inputs A vector of strings representing input sequences.
//' @param X The threshold index from which to return SNP indices or counts (default: 0).
//' @param parallel A boolean flag to enable parallel processing (default: FALSE).
//' @param return_count A boolean flag to return the count of mutations instead of their indices (default: FALSE).
//' @param non_mismatch_chars_nullable A set of characters that are ignored when comparing sequences (default: 'N', '.', '-').
//' @return A list of integer vectors (if `return_count = FALSE`) or a vector of integers (if `return_count = TRUE`).
//'
//' @examples
//' # Example usage
//' germs <- c("ATCG", "ATCC")
//' inputs <- c("ATTG", "ATTA")
//' X <- 0
//'
//' # Return indices of SNPs
//' result_indices <- allele_diff_indices_parallel2(germs, inputs, X, 
//' parallel = TRUE, return_count = FALSE)
//' print(result_indices)  # list(c(4), c(3, 4))
//'
//' # Return counts of SNPs
//' result_counts <- allele_diff_indices_parallel2(germs, inputs, X, 
//' parallel = FALSE, return_count = TRUE)
//' print(result_counts)  # c(1, 2)
//'
//' @name allele_diff_indices_parallel2
//' @export
// [[Rcpp::export]]
Rcpp::RObject allele_diff_indices_parallel2(std::vector<std::string> germs, 
                                           std::vector<std::string> inputs, 
                                           int X = 0, 
                                           bool parallel = false, 
                                           bool return_count = false, 
                                           Rcpp::Nullable<Rcpp::CharacterVector> non_mismatch_chars_nullable = R_NilValue) {
  // Set default non-mismatch characters
  std::unordered_set<char> non_mismatch_chars = {'N', '.', '-'};
  // If non_mismatch_chars is provided by the user, convert it to std::unordered_set<char>
  if (non_mismatch_chars_nullable.isNotNull()) {
    Rcpp::CharacterVector char_vec(non_mismatch_chars_nullable);
    non_mismatch_chars.clear();
    for (const auto& c : char_vec) {
      non_mismatch_chars.insert(Rcpp::as<std::string>(c)[0]);
    }
  }
  
 if (germs.size() != inputs.size()) {
   Rcpp::stop("The size of germs and inputs must be the same.");
 }
 
 size_t num_sequences = germs.size();
 auto pad_with_ns = [](std::string& seq, size_t target_length) {
   if (seq.size() < target_length) {
     seq.append(target_length - seq.size(), 'N');
   }
 };
 
 if (!parallel) {
   if (return_count) {
     std::vector<int> mutation_counts(num_sequences);
     for (size_t i = 0; i < num_sequences; ++i) {
       std::string germ = germs[i];
       std::string input = inputs[i];
       size_t max_length = std::max(germ.size(), input.size());
       pad_with_ns(germ, max_length);
       pad_with_ns(input, max_length);
       int count = 0;
       for (size_t j = 0; j < max_length; ++j) {
         if (j >= static_cast<size_t>(X) && germ[j] != input[j] &&
             non_mismatch_chars.find(input[j]) == non_mismatch_chars.end() &&
             non_mismatch_chars.find(germ[j]) == non_mismatch_chars.end()) {
           count++;
         }
       }
       mutation_counts[i] = count;
     }
     return Rcpp::wrap(mutation_counts);
   } else {
     Rcpp::List snp_list(num_sequences);
     for (size_t i = 0; i < num_sequences; ++i) {
       std::string germ = germs[i];
       std::string input = inputs[i];
       size_t max_length = std::max(germ.size(), input.size());
       pad_with_ns(germ, max_length);
       pad_with_ns(input, max_length);
       std::vector<int> snp_indices;
       for (size_t j = 0; j < max_length; ++j) {
         if (j >= static_cast<size_t>(X) && germ[j] != input[j] &&
             non_mismatch_chars.find(input[j]) == non_mismatch_chars.end() &&
             non_mismatch_chars.find(germ[j]) == non_mismatch_chars.end()) {
           snp_indices.push_back(j + 1);
         }
       }
       snp_list[i] = Rcpp::wrap(snp_indices);
     }
     return snp_list;
   }
 }
 
 if (parallel) {
   if (return_count) {
     std::vector<int> mutation_counts(num_sequences);
     PARALLEL_FOR
     for (size_t i = 0; i < num_sequences; ++i) {
       std::string germ = germs[i];
       std::string input = inputs[i];
       size_t max_length = std::max(germ.size(), input.size());
       pad_with_ns(germ, max_length);
       pad_with_ns(input, max_length);
       int count = 0;
       for (size_t j = 0; j < max_length; ++j) {
         if (j >= static_cast<size_t>(X) && germ[j] != input[j] &&
             non_mismatch_chars.find(input[j]) == non_mismatch_chars.end() &&
             non_mismatch_chars.find(germ[j]) == non_mismatch_chars.end()) {
           count++;
         }
       }
       mutation_counts[i] = count;
     }
     return Rcpp::wrap(mutation_counts);
   } else {
     Rcpp::List snp_list(num_sequences);
     PARALLEL_FOR
     for (size_t i = 0; i < num_sequences; ++i) {
       std::string germ = germs[i];
       std::string input = inputs[i];
       size_t max_length = std::max(germ.size(), input.size());
       pad_with_ns(germ, max_length);
       pad_with_ns(input, max_length);
       std::vector<int> snp_indices;
       for (size_t j = 0; j < max_length; ++j) {
         if (j >= static_cast<size_t>(X) && germ[j] != input[j] &&
             non_mismatch_chars.find(input[j]) == non_mismatch_chars.end() &&
             non_mismatch_chars.find(germ[j]) == non_mismatch_chars.end()) {
           snp_indices.push_back(j + 1);
         }
       }
       snp_list[i] = Rcpp::wrap(snp_indices);
     }
     return snp_list;
   }
 }
 
 return Rcpp::wrap(R_NilValue);
}

// ------------------------------------------------------------------------------
// 5. insert_gaps2_vec
//' Insert gaps into an ungapped sequence based on a gapped reference sequence.
//'
//' This function inserts gaps (e.g., `.` or `-`) into an ungapped sequence (`ungapped`)
//' to match the positions of gaps in a reference sequence (`gapped`). It ensures that
//' the aligned sequence has the same gap structure as the reference.
//'
//' @param gapped A vector of strings representing the reference sequences with gaps.
//' @param ungapped A vector of strings representing the sequences without gaps.
//' @param parallel A boolean flag to enable parallel processing (default: FALSE).
//' @return A vector of strings with gaps inserted to match the gapped reference.
//'
//' @examples
//' # Example usage
//' gapped <- c("caggtc..aact", "caggtc---aact")
//' ungapped <- c("caggtcaact", "caggtcaact")
//'
//' # Sequential execution
//' result <- insert_gaps2_vec(gapped, ungapped, parallel = FALSE)
//' print(result)  # "caggtc..aact", "caggtc---aact"
//'
//' # Parallel execution
//' result_parallel <- insert_gaps2_vec(gapped, ungapped, parallel = TRUE)
//' print(result_parallel)
//'
//' @name insert_gaps2_vec
//' @export
// [[Rcpp::export]]
std::vector<std::string> insert_gaps2_vec(const std::vector<std::string>& gapped,
                                         const std::vector<std::string>& ungapped,
                                         bool parallel = false) {
 if (gapped.size() != ungapped.size()) {
   Rcpp::stop("The size of gapped and ungapped vectors must be the same.");
 }
 const std::unordered_set<char>& gap_chars = {'.', '-'};
 size_t num_sequences = gapped.size();
 std::vector<std::string> results(num_sequences);
 
 auto process_sequence = [&gap_chars](const std::string& gapped, const std::string& ungapped) -> std::string {
   std::string result;
   size_t ungapped_index = 0;
   for (char gap_char : gapped) {
     if (gap_chars.find(gap_char) != gap_chars.end()) {
       result.push_back(gap_char);
     } else {
       if (ungapped_index < ungapped.size()) {
         result.push_back(ungapped[ungapped_index]);
         ++ungapped_index;
       } else {
         break;
       }
     }
   }
   return result;
 };
 
 if (parallel) {
   PARALLEL_FOR
   for (size_t i = 0; i < num_sequences; ++i) {
     results[i] = process_sequence(gapped[i], ungapped[i]);
   }
 } else {
   for (size_t i = 0; i < num_sequences; ++i) {
     results[i] = process_sequence(gapped[i], ungapped[i]);
   }
 }
 
 return results;
}
