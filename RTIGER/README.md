# Quick Guide to RTIGER

1.[Introduction](#Introduction)

2.[Installation](#Installation)
  
>  2.1.[Pre-requisites](##Pre-requisites)

>  2.2.[Problems installing Julia](##Problems)

3.[Preparing input data](#Preparing)

4.[Using RTIGER](#Using)

> 4.1. [Running RTIGER](##Running)

> 4.2. [Autotune the R parameter](##Autotune)


5.[RTIGER Output](#RTIGER)


## Introduction
Accurate identification of meiotic crossing-over sites (COs) is essential for correct genotyping of recombining samples. RTIGER is a method for predicting genome-wide COs using allele-counts at pre-defined SNP marker positions. RTIGER trains a Hidden Markov Model (HMM) where genomic states (homozygous parent_1, homozygous parent_2 or heterozygous) correspond to the hidden state and the allele-counts as the observed variable. COs are identified as transitions in the HMM state.

To account for variation in the coverage of sequencing data, RTIGER uses Viterbi Path Algorithm and the `rigidity` parameter. This parameter defines the minimum number of SNP markers required to support a state-transition. This filters out low-confidence state-transitions, improving COs identification performance.

<!-- ################################################################################ -->
## Installation
### Pre-Requisites:
* R: Version > 3.6
* **RECOMENDED**: Julia-1.0.5 (Which versions of Julia can be supported?): Julia needs to be installed and available in the environment [Link to Julia](https://www.geeksforgeeks.org/how-to-setup-julia-path-to-environment-variable/?ref=lbp)
  - Julia 1.4.1 Works as well but slightly slower.
  - REQUIRED R LIBRARIES for Julia will be installed from the R terminal just after installation of the package.

RTIGER is an R package meant to be used for Biological data. For that reason, we rely in some Bioconductor libraries that needs to be installed beofrehand to ensure a smooth installation of the package from CRAN. To install Bioconductor and the packages needed, run on the R terminal:
```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(version = "3.14")

BiocManager::install(c("GenomicRanges", "GenomeInfoDb", "TailRank", "IRanges", "Gviz"))
```
Now we are ready to install RTIGER. As mentioned before, RTIGER is an CRAN package and can be installed with the simple command line:
```
install.packages("RTIGER")
```
Neverhteless, to get the latest and debugged versions, we recommend you install it from github using the devtools package:
```
install.packages("devtools")
library(devtools)
install_github("rfael0cm/RTIGER")
```
Once the RTIGER package is installed, next is to install the JULIA libraries needed to run the analysis. We have simplified this step by just running the following function:
```
library(RTIGER)
setupJulia()
```
Now, we have all the necessary prerequisists to start using RTIGER and obtain the maximum information from our data sets. RTIGER needs specific data format to work. On the next section we will describe how it must be enoced and saved.

### Problems installing Julia

Working at the interface between R and Julia might be a bit troublesome to make it work. If when runing the function `setupJulia()`, R is unable to find Julia, this might be a solution:
First, download the binaries from the [julia webpage](https://julialang.org/downloads/oldreleases/). As mentioned before, we recommend the version 1.0.5 of Julia. Unzip the file in your machine and the run the following script:
```
setupJulia(JULIA_HOME = full_path_to_Julia_bin_folder)
```

<!-- ################################################################################ -->
## Preparing input data
RTIGER uses the allele-count information at the SNP marker positions. The SNP markers correspond to differences between the two genotypes (i.e. parent_1 vs parent_2). RTIGER requires as input one allele-count file for each sample. The allele-count file should be in tab-separated value format, where each row corresponds to a SNP marker. The format of the file is described below:

|Column | Field | Type | Description |
|---|---|---|----|
|1|SeqID|String| Chromosme ID|
|2|Pos|init(>=0)| Position of the SNP marker|
|3|RefA|char| Reference allele|
|4|RefC| int(>=0)| Number of reads with reference alele|
|5|AltA|char|Alternate allele|
|6|AltF|int(>=0)|Number of reads with alternate allele|

Ther order of the columns is **EXTREMELY IMPORTANT**. RTIGER ensures that the data type of each column is the correct. But the interpretation of **references allele** and **alternate allele** is completely arbitrary and it is the user who defines them. Moreover, the chromosome and position is crucial to run our algorithm since we group together consecutive SNPs from the same chromosome.

The SNPs can be identified using any generic SNP identification pipeline. For example look this [method](https://www.ebi.ac.uk/sites/ebi.ac.uk/files/content.ebi.ac.uk/materials/2014/140217_AgriOmics/dan_bolser_snp_calling.pdf).

SNPs in repetitive regions should be filtered out. Further, as crossing-over usually takes place in syntenic regions between the two genome, for best results, only SNPs in syntenic regions should be selected as markers. If whole genome assemblies are present for both genomes, then this can be easily achieved using methods like [SyRI](https://genomebiology.biomedcentral.com/articles/10.1186/s13059-019-1911-0).

**NOTE 1**: RTIGER assumes that all samples have similar sequencing coverage and, hence, similar distribution of the allele-count values. It does not check or normalise for sequencing coverage variation.

**NOTE 2**: Crossing-over resolution depends on sequenced marker density. Low sequencing coverage could result in few informative markers, which in turn could decrease resolution CO prediction.

**NOTE 3**: RTIGER is designed to be robust against individual outliers, however, the user should check for "bad" markers, i.e. marker positions that are prone to mismapping. These markers result in high allele-count at that position.<!--  as well as in a high, artificial agreement in this position across samples. -->

## Using RTIGER 

### Running RTIGER

To run RTIGER, first we need to invoke julia and the package. To this to work, be sure that everythink run smoothly on the installation section.
```
library(RTIGER)
setupJulia()
sourceJulia()
```
The function `sourceJulia()` will need to be run every single time that we load RTIGER. The two previous lines of code should be run always.
If the machine has several versions of Julia, it can be specified which version to use in the `setupJulia()` function:
```
setupJulia(JULIA_HOME = "/directory/to/Julia/binaries")
```

#### Creating input objects
The primary input for RTIGER is a data-frame termed `expDesign`. The first column of `expDesign` should have paths to allele-count files for all samples and the second column should have unique samples IDs. The columnames should be "files" and "name" to specify the file path and sample name respectivley. We recommend to use full path to the files to avoid further problems.
An example of how to create the data-frame using the example data provided by our package:

```
# Get paths to example allele count files originating from a
# cross between Col-0 and Ler accession of the A.thaliana
file_paths = list.files(system.file("extdata",  package = "RTIGER"), full.names = TRUE)

# Get sample names
sampleIDs <- basename(file_paths)

# Create the expDesign object
expDesign = data.frame(files=file_paths, name=sampleIDs)

print(expDesign)
```
RTIGER also requires chromosome lengths for the parent_1. These need to be provided as a named vector where the values are chromosome lengths and the names are chromosome ids. For our example, we have included the length of the five autosomal chroomosomes in Arabidopsis Thaliana and it can be invoqued as:

```
# Get chromosome lengths for the example data included in the package
chr_len <- RTIGER::ATseqlengths
names(chr_len) <- c('Chr1' , 'Chr2', 'Chr3', 'Chr4', 'Chr5')
print(chr_len)
```
**Note**: It is important the that the names of the vector match exactly with the chromosome names in your data files. If any of the files uses another coding for the chromosomes, RTIGER will rise and error. Be consistent!

#### Finding crossing-over sites using RTIGER

RTIGER does model training, COs identification, per sample and summary plots creation using a single function:

```
myres = RTIGER(expDesign = expDesign,
               outputdir = "/srv/netscratch/dep_mercier/grp_schneeberger/projects/SynSearch/tests",
               seqlengths = chr_len,
               rigidity = 20,
               save.results = TRUE)
```
The `rigidity` parameter defines the required minimum number of continuous markers that together support a state change of the HMM model. Smaller `rigidity` values increase the sensitivity in detecting COs that are close to each other, but may result in false-positive CO identification because of variation in sequencing coverage. Larger `rigidity` values improve precision but COs that are close to each other might not be identified. **See section [Autotune the R parameter](##Autotune)**.
A second internal step is run, called post-processing. On this step, we fine tune the limits of each state by looking closely to the borders of two consecutive states. We can do that in an efficent manner since we look into state segments which the length is equal to the rigidity value. If the user would prefer to cancel this step, it can be done by setting `post.processing=FALSE`.
Look documentation for more information about this funcion.

### Autotune the R parameter

Our R package includes an algorithm designed to assist users in selecting the optimal R value for their data set. The algorithm achieves this by minimizing false positive segments (FP) and false negative segments (FN) through the RTIGER method. FP segments are those that do not exist in the actual data set, while FN segments are those that are not annotated by the model.

The algorithm takes into account two parameters - sequencing depth and the number of CO per megabase - to determine the best R value for the given experiment. Users can either provide these values themselves using the `average_coverage` and `crossovers_per_megabase` parameters or allow the algorithm to compute them directly from the dataset. The `average_coverage` is computed as the mean coverage at all positions in the experiment, while the of CO per megabase is computed after an initial round of model fitting using the user-provided parameters.

+ `average_coverage`: To achieve conservative results, set this parameter to the lowest average coverage value in one of your samples, or to the lowest average coverage value in a sufficiently large region of one of your samples. Lower values will result in more conservative (higher) estimates of false positive segment rates. If this parameter is not provided, it will be computed as the mean of all data points.

+ `crossovers_per_megabase`: To achieve conservative results, set this parameter to the highest ratio of crossovers per megabase in any of your samples. Higher values will result in more conservative (higher) estimates of false positive segment rates. If this parameter is not provided, it will be computed as the average across all samples.

```
myres = RTIGER(expDesign = expDesign,
               outputdir = "/srv/netscratch/dep_mercier/grp_schneeberger/projects/SynSearch/tests",
               seqlengths = chr_len,
               rigidity = 20,
               autotune = TRUE,
               save.results = TRUE)
```
This function will first compute an rhmm model with the R value provided by the user, find the optimum R parameter and then fit the model with this value.

Alternatively, if the user has already trained a model with RTIGER, they can use the `optimize_R()` function to find the optimal R value. This function requires the RTIGER object and the same parameters mentioned previously (`average_coverage` and `crossovers_per_megabase`) to estimate the value. If these values are not provided, they will be computed experimentally. For more experienced users who want a more detailed understanding of how the rigidity value was selected, the function can be run with the option `save_it = TRUE` and a directory specified in `savedir`. This will save the plots in the desired directory. For a more in-depth interpretation of the plots, we refer readers to the supplementary documentation provided in our [manuscript]( https://doi.org/10.1093/plphys/kiad191).


```
best_R = optimize_R(myres)
```
**Note:** This funciton will only provide the optimum R value and generate the plots. If the user wants to use this suggested R value it should train again the model from scratch but using the `best_R` value and `autotune = FALSE`:


```
myres = RTIGER(expDesign = expDesign,
               outputdir = "/srv/netscratch/dep_mercier/grp_schneeberger/projects/SynSearch/tests",
               seqlengths = chr_len,
               rigidity = best_R,
               autotune = FALSE,
               save.results = TRUE)
```

## RTIGER Output:
RTIGER identifies COs for each sample level and provides summary plots and statistics for each sample as well as for the entire population.
If the user wants to acces to the data and obtain their own analysis done with R, they can be retrieved accesing inside the specific class:
```
myViterbi = myres@Viterbi
```
This object is a list of Genomic Ranges objects that contain the raw info and the decoded Viterbi using the RTIGER algorithm.
#### Per sample output
RTIGER creates a folder for each sample in the `outputdir`. This folder contains:

* `GenotypePlot.pdf`: Graphical representation of the allele-counts, allele-count ratio, and genotypes.
* `GenotypeBreaks.bed`: BED file providing genomic regions corresponding to different genotypes
* `P1/P2/Het.bed`: BED files containing the markers present in genomic regions having genotype: homozygous parent 1, homozygous parent 2, or heterozygous, respectively
* `P1/P2.bw`: BigWig file containing the number of reads per marker position supporting parent 1 and parent 2, respectively
* `CountRatio.bw`: BigWig file containing the ratio of number of reads supporting parent 1 to number of reads supporting number 2 at the marker positions

#### Summary plots for the population (THE PLOTS AND THE DESCRIPTION HERE NEEDS MORE WORK)
RTIGER creates four summary plots after aggregating results for all samples. 

* `COs-per-Chromosome.pdf`: Distribution of number of cross-overs per chromosome.
* `CO-count-perSample.pdf`: Number of cross-overs in each sample.
* `Goodness-Of-fit.pdf`: Histogram for the reference allele count for each state.
* `GenomicFrequencies.pdf`: Distribution of cross-overs along the length of chromosomes.


### Analysing backcrossed populations
Backcrossed populations are formed by crossing a hybrid organism with one of its parent. These populations are different from the populations based on outcrossing as only two genomic states are possible (homozygous for the backrossed parent and heterozygous for both parents). To identify COs in such population, set `nstates=2` in the RTIGER command.
```{r echo=TRUE, eval=FALSE}
myres = RTIGER(expDesign = expDesign, 
               outputdir = "PATH/TO/OUTPUT/DIR",
               seqlengths = chr_len,
               rigidity = 200, 
               nstates=2)
```


<!-- myres = RTIGER(expDesign = expDesign, outputdir = paste0("/srv/netscratch/dep_mercier/grp_schneeberger/projects/rtiger/test/"), seqlengths =chr_len, rigidity = 200) -->
    


<!-- ## Running RTIGER: -->
<!--     ## Examples of using other running modes for RTIGER (changing parameters)? -->
<!--     ## Generating CO-Lists from rtiger_out object (if not generated automatically) -->
<!--     ## Description of using other ease-of-life features provided by RTIGER     -->
    


