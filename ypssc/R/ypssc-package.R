
packageName = "ypssc"

####################################################################################################################################
################################## ypssc-package-doc ###############################################################################
# >>
#'
#' @details
#'
#-----------------------------------------------------------------------------------------------------------------------------------
#' \describe{\item{_**What is `ypssc`?**_}{
#-----------------------------------------------------------------------------------------------------------------------------------
#'
#'   **`ypssc`** is an extension for NetSurfP-2.0 which is specifically designed to analyze the results of bottom-up proteomics that
#'   is primarily analyzed with MaxQuant. We call this tool _**Yeast Proteome Secondary Structure Calculator**_ (**`ypssc`**).
#'
#'   \out{<hr>}
#'
#'   **Functionalities in `ypssc`**:
#'
#'   1. [`findSecondary`]
#'
#'   2. [`findAlpha`]
#'
#'   3. [`findBeta`]
#'
#'   4. [`findChain`]
#'
#'      (Click the above links to find out more about these functionalities and their usage.)
#'
#'   \out{<hr>}
#'
#'   **Note:** NetSurfP
#'
#'   - NetSurfP-1.0 is a prediction tool for secondary structures using neural network.
#'
#'   - NetSurfP-2.0 is an extension of NetSurfP-1.0 which utilized deep neural network to predict secondary structures with the
#'     accuracy of 85%. In addition to accuracy, this tool presents reduced computational time compared to other methods.
#'
#'   - NetSurfP-2.0 is designed to be user friendly and efficient in calculation time of large number of sequences. In addition to
#'     that the output of the calculation is available in many formats that would make further data analysis even easier.
#'
#'   - NetSurfP-2.0 is available as a web-sever (http://www.cbs.dtu.dk/services/NetSurfP-2.0/) which can accept up to 4000 sequences
#'     at a time.
#'
#' }}
#'
#' \out{<hr>}
#'
#-----------------------------------------------------------------------------------------------------------------------------------
#' \describe{\item{_**Why this package?**_}{
#-----------------------------------------------------------------------------------------------------------------------------------
#'
#'   This tool is designed to process large number of yeast peptides that produced as a results of whole yeast cell proteome digestion
#'   and provide a coherent picture of secondary structure of proteins. NetSurfP-2.0 is not designed to do this task.
#'
#'   \out{<hr>}
#'
#'   **Drawbacks of NetSurfP-2.0**
#'
#'   - First, NetSurfP-2.0 is not designed to accept as many peptides at once, therefore the process of uploading the sequences and
#'     waiting for the calculations to be complete is extremely time consuming.
#'
#'   - Second, even if all sequences uploaded successfully and the results are back, it would be almost impossible to combine the
#'     results that have been produced for each individual peptide  (hundreds of thousands of spread sheets) to get a coherent
#'     picture of the secondary structure of the proteins.
#'
#'   \out{<hr>}
#'
#'   **Advantages of `ypssc`**
#'
#'   - **`ypssc`**, on one hand benefits forms the accuracy of NetSurfP-2.0 to calculate secondary structure and on the other hand
#'     address the issue of analyzing so many peptides with NetSurfP-2.0 by eliminating the need for direct analysis of the peptides
#'     from bottom-up proteomics.
#'
#'   - Instead of direct analysis of peptides by NetSurfP-2.0 which raises the problem of combining the results of peptides to
#'     proteins, the whole yeast proteome has been analyzed once by NetSurfP-2.0 and kept as Secondary Structure Database for Yeast
#'     Proteome (SSDYP). Then the peptides form the experiment are matched and compared to this database to extract secondary
#'     structure of the peptides.
#'
#' }}
#'
#' \out{<hr>}
#'
#-----------------------------------------------------------------------------------------------------------------------------------
#' \describe{\item{_**Methodology**_}{
#-----------------------------------------------------------------------------------------------------------------------------------
#'
#'   The SSDYP contains structural information for all amino acids of whole yeast proteome (Over 3000,000 amino acids) which contains
#'   over 6700 proteins. For a hypothetical protein, the SSDYP contains the ID of the protein, amino acids with numbers and structural
#'   information for each amino acid. Focusing on the hypothetical protein, in the real sample, there are many peptides identified
#'   from the hypothetical protein. **`ypssc`** first finds all the peptides that belongs to the hypothetical protein and arrange them
#'   based on the numbers of the amino acids; then it removes the parts of the protein that have been identified more than once in
#'   multiple peptides and collapses the population of identified peptides in the sample into one sequence that represents the
#'   coverage of the hypothetical protein. The result would show that which part of the protein is identified, and which part is
#'   missing. Then, **`ypssc`** matches the the sequence that identified in the sample with SSDYP to find the structural information
#'   about amino acids.
#'
#' }}
#'
#' \out{<hr>}
#'
####################################################################################################################################
#'
#' @seealso [`findSecondary`], [`findAlpha`], [`findBeta`], [`findChain`]
#'
# <<
################################## ypssc-package-doc ###############################################################################
####################################################################################################################################
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
