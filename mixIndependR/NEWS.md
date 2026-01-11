# mixIndependR 1.0.0 -2021.03.16
- updated "AlleleShare" and it is not compatible with odd sample size.
- update the version number and maintainer's email address.

# mixIndependR 0.4.4 -2021.01.07
- add citation information and publication link

# mixIndependR 0.4.3 -2020.11.29
- Fix the bugs about exports of functions "read_vcf_gt" and "splitGenotype".
- Update the NAMESPACE file to including the missing functions.

# mixIndependR 0.4.2 -2020.11.26
# Bug Fix
- Fix the bugs about dependency and support in "Description".
- Optimize the size of plots in vignettes.

# mixIndependR 0.4.1 -2020.11.24
# Bug Fix
- Fix the bug in "GenotypeFreq" when expect = FALSE
# Update Vignettes
- Attach dataset "mixexample" and update the example. 


# mixIndependR 0.4.0 -2020.11.24
# Function Optimizing
- Optimize most functions and replace loops with parallel calculation.
# New Functions
- Add function "read_vcf_gt" and "splitGenotype", and make data importing compatible with excel, csv and vcf format for genotype.
- Remove the function "HWE.Fisher" for the moment
# Vignette
- Update for the new version.

# mixIndependR 0.3.0 -2020.10.05
# New Functions
- add two functions "mixIndependK" and "mixIndependX"
- remove the function "Prop_Pvalue"
# Vignette
- Vignettes are built


# mixIndependR 0.2.3 -2020.09.24
# Optimization
- optimize the function "Prop_Pvalue" by adding a logical variable "print".


# mixIndependR 0.2.2 -2020.09.20
# Optimization
- simplify function "counta"


# mixIndependR 0.2.1 -2020.03.11
# Fix Bugs
- Fix the error that "AlleleShare_Table" cannot deal with samples with odd sample size when "replicate=F"
# Add New Function
- Add a function named as "Prop_Pvalue" which can generate a bundle of p-values for one sample
# Update Description Files
- The Edition


# mixIndependR 0.2.0 -2020.02.01
# Fix bugs
- Fix format conflict in "AlleleShare_Table"
- Solve "NA" problem in “Dis_SimuChisq”
# Update Description Files
- Add "References" to "DistAlleleShare", "ExpProAlleleShare", "GenotypeFreq", "HWE_Fisher", and "RxpHetero"
- Correct "Usage" to include more information of input.
- Add "examples" to all R functions.
# New .R files
- "RxpHetero" to calculate average heterozygosity of observed or under Hardy-Weinberg Equilibrium
- "FreqAlleleShare" to build observed distribution for No. of shared alleles
- "FreqHetero" build observed distribution for No. of heterozygous loci
- "RealProAlleleShare" to calculate real density of shared alleles on each loci.
- "ComposPare_K" to generate a dataframe including observed and expected data of No. of heterozygous loci for easily plotting.
- "ComposPare_X" to generate a dataframe including observed and expected data of No. of sharing alleles for easily plotting.
# Update Old .R files
- Add column names to "AlleleShare_Table"


* Added a `NEWS.md` file to track changes to the package.
