## Asymmetric Linkage Disequilibrium (ALD) for Polymorphic Genetic Data

**asymLD** package for R computes asymmetric LD measures (ALD) for multi-allelic genetic data. These measures are identical to the correlation measure (r) for bi-allelic data. We first described the ALD measure in the following article:

_Thomson, Glenys, and Richard M. Single. "Conditional Asymmetric Linkage Disequilibrium (ALD): Extending the Biallelic r2 Measure." Genetics 198.1 (2014): 321-331._[link](http://www.genetics.org/content/198/1/321)

**Abstract**
For multiallelic loci, standard measures of linkage disequilibrium provide an incomplete description of the correlation of variation at two loci, especially when there are different numbers of alleles at the two loci. We have developed a complementary pair of conditional asymmetric linkage disequilibrium (ALD) measures. Since these measures do not assume symmetry, they more accurately describe the correlation between two loci and can identify heterogeneity in genetic variation not captured by other symmetric measures. For biallelic loci the ALD are symmetric and equivalent to the correlation coefficient r. The ALD measures are particularly relevant for disease-association studies to identify cases in which an analysis can be stratified by one of more loci. A stratified analysis can aid in detecting primary disease-predisposing genes and additional disease genes in a genetic region. The ALD measures are also informative for detecting selection acting independently on loci in high linkage disequilibrium or on specific amino acids within genes. For SNP data, the ALD statistics provide a measure of linkage disequilibrium on the same scale for comparisons among SNPs, among SNPs and more polymorphic loci, among haplotype blocks of SNPs, and for fine mapping of disease genes. The ALD measures, combined with haplotype-specific homozygosity, will be increasingly useful as next-generation sequencing methods identify additional allelic variation throughout the genome.


### Usage

To compute ALD in a data set, you can use the function **compute.ALD** within the asymLD package, as follows:
```S
ald.results <- compute.ALD(dat, tolerance = 0.005)
```

Parameter **dat** is a data.frame with 5 required variables:

  - *haplo.freq* A numeric vector of haplotype frequencies.
  - *locus1* A character vector indentifying the first locus.
  - *locus2* A character vector indentifying the second locus.
  - *allele1* A character vector indentifying the allele at locus 1.
  - *allele2* A character vector indentifying the allele at locus 2.

Parameter **tolerance** is a threshold for the sum of the haplotype frequencies. If the sum of the haplotype frequencies is greater than 1+tolerance or less than 1-tolerance an error is returned.

The function returns a dataframe (in the above example **ald.results**) with the following components:

- *locus1*	The name of the first locus.
- *locus2*	The name of the second locus.
- *F.1*	Homozygosity (expected under HWP) for locus 1.
- *F.1.2*	Conditional homozygosity for locus1 given locus2.
- *F.2*	Homozygosity (expected under HWP) for locus 2.
- *F.2.1*	Conditional homozygosity for locus2 given locus1.
- *ALD.1.2*	Asymmetric LD for locus1 given locus2.
- *ALD.2.1*	Asymmetric LD for locus2 given locus1.


### Examples

#### Example 1. HLA frequencies

This is an example of ALD measure using haplotype frequencies from Wilson (2010)

```r
library(asymLD)
data(hla.freqs)
hla.a_b <- hla.freqs[hla.freqs$locus1=="A" & hla.freqs$locus2=="B",]
head(hla.a_b)
```

```
##     haplo.freq locus1 locus2 allele1 allele2
## 170    0.06189      A      B    0101    0801
## 171    0.04563      A      B    0201    4402
## 172    0.04318      A      B    0301    0702
## 173    0.03103      A      B    0201    4001
## 174    0.02761      A      B    0301    3501
## 175    0.01929      A      B    0205    1503
```

```r
compute.ALD(hla.a_b)
```

```
##   locus1 locus2       F.1    F.1.2        F.2     F.2.1   ALD.1.2  ALD.2.1
## 1      A      B 0.1021811 0.340332 0.04876543 0.1903394 0.5150291 0.3857872
```

```r
hla.c_b <- hla.freqs[hla.freqs$locus1=="C" & hla.freqs$locus2=="B",]
compute.ALD(hla.c_b)
```
```
##   locus1 locus2        F.1     F.1.2        F.2     F.2.1   ALD.1.2    ALD.2.1
## 1      C      B 0.08637241 0.7350216 0.05040254 0.4520268 0.8425979  0.6503396
```

Note that there is substantially less variablity (higher ALD) for HLA\*C conditional on HLA\*B than for HLA\*B conditional on HLA\*C, indicating that the overall variation for C is relatively low given specific B alleles.


#### Example 2. SNP frequencies

This is an example using SNP data where results are symmetric and equal to the ordinary correlation measure (r).


```r
data(snp.freqs)
snps <- c("rs1548306", "rs6923504", "rs4434496", "rs7766854")
compute.ALD(snp.freqs[snp.freqs$locus1==snps[2] & snp.freqs$locus2==snps[3],])
```

```
##      locus1    locus2       F.1     F.1.2      F.2    F.2.1   ALD.1.2     ALD.2.1
## 1 rs6923504 rs4434496 0.5385803 0.5971276 0.700556 0.738551 0.3562095  0.3562095
```

```r
snp.freqs$locus <- paste(snp.freqs$locus1, snp.freqs$locus2, sep="-")
by(snp.freqs,list(snp.freqs$locus),compute.ALD)
```

```
## : rs1548306-rs4434496
##      locus1    locus2       F.1     F.1.2       F.2    F.2.1   ALD.1.2     ALD.2.1
## 1 rs1548306 rs4434496 0.5593208 0.5869306 0.7005549 0.719316 0.2503054 0.2503054
## -------------------------------------------------------- 
## : rs1548306-rs6923504
##      locus1    locus2       F.1     F.1.2       F.2     F.2.1   ALD.1.2    ALD.2.1
## 1 rs1548306 rs6923504 0.5593208 0.6528273 0.5385803 0.6364877 0.4606378 0.4606378
## -------------------------------------------------------- 
## : rs1548306-rs7766854
##      locus1    locus2       F.1     F.1.2       F.2    F.2.1   ALD.1.2    ALD.2.1
## 1 rs1548306 rs7766854 0.5593208 0.5869306 0.7005549 0.719316 0.2503054  0.2503054
## -------------------------------------------------------- 
## : rs4434496-rs7766854
##      locus1    locus2      F.1 F.1.2      F.2 F.2.1 ALD.1.2 ALD.2.1
## 1 rs4434496 rs7766854 0.700556     1 0.700556     1       1       1
## -------------------------------------------------------- 
## : rs6923504-rs4434496
##      locus1    locus2       F.1     F.1.2      F.2    F.2.1   ALD.1.2    ALD.2.1
## 1 rs6923504 rs4434496 0.5385803 0.5971276 0.700556 0.738551 0.3562095 0.3562095
## -------------------------------------------------------- 
## : rs6923504-rs7766854
##      locus1    locus2       F.1     F.1.2      F.2    F.2.1   ALD.1.2    ALD.2.1
## 1 rs6923504 rs7766854 0.5385803 0.5971276 0.700556 0.738551 0.3562095 0.3562095
```
Note that in all of the above examples ALD.1.2 is equal to ALD.2.1 due to symmetry in the SNP data.

In the following example we show that the ALD measures are equal to the r correlation due to symmetry for bi-allelic SNPs.

```r
p.AB <- snp.freqs$haplo.freq[1]
p.Ab <- snp.freqs$haplo.freq[2]
p.aB <- snp.freqs$haplo.freq[3]
p.ab <- snp.freqs$haplo.freq[4]
p.A <- p.AB + p.Ab
p.B <- p.AB + p.aB
r.squared <- (p.AB - p.A*p.B)^2 / (p.A*(1-p.A)*p.B*(1-p.B))
sqrt(r.squared) #the r correlation measure
```

```
## [1] 0.4606378
```

```r
compute.ALD(snp.freqs[snp.freqs$locus1==snps[1] & snp.freqs$locus2==snps[2],])
```

```
##      locus1    locus2       F.1     F.1.2       F.2     F.2.1   ALD.1.2   ALD.2.1  
## 1 rs1548306 rs6923504 0.5593208 0.6528273 0.5385803 0.6364877 0.4606378  0.4606378
```