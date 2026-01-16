# Doblin

## Developed by the <http://www.serohijoslab.org/>

## Follow the Readme or [Download the Doblin user guide](https://github.com/DavidGagneLeroux/doblin/blob/master/vignettes/doblin.pdf) for a complete step-by-step on how to use our tool.

## Overview

As microbial populations exhibit complex lineage dynamics, interpreting such data poses considerable challenges. To address this, we developed *Doblin*, an R-based pipeline designed to extract meaningful insights from complex DNA barcoding time series data obtained through longitudinal sampling. *Doblin* employs a clustering approach to group relative abundance trajectories based on their shape. This method effectively clusters lineages with similar relative abundance patterns, thereby reflecting comparable fitness levels. *Doblin's* final product reveals the dominant and persistent trends within the complex dataset, providing clearer insights into microbial population dynamics.

![](https://github.com/DavidGagneLeroux/doblin/blob/master/vignettes/images/doblin_readme.jpg?raw=true)

The primary purpose of *Doblin* is to furnish an open-source toolkit for the preliminary analysis of abundance time series obtained via *Next Generation Sequencing* (NGS), thereby laying the groundwork for ecological and evolutionary studies of microbial populations.

## Quick Setup Guide

*Before installing the pipeline, make sure R is installed on your device.*

**Step 1:** Open Terminal (compatible on both Linux & Windows Terminal).

**Step 2:** Change the current working directory to the location where you want to clone the *Doblin* repository.

**Step 3:** Clone the repository using git command. Then, set your working directory to the `doblin/` folder.

```
    ~$ git clone https://github.com/DavidGagneLeroux/doblin
    ~$ cd doblin
```
    
## Quick Start Guide

*Make sure you have already gone through the **Quick Setup Guide** above.*

### Command Line Usage

To execute the main script, use the following command line:
```
  ~$ Rscript ./demo/main.R -t [MIN_FREQUENCY] -o [OUTPUT_DIR] -n [INPUT_FILE_NAME] -i [INPUT_FILE] -c [TIME_CUTOFF]
```
### Explanation of Arguments
```
-t: Minimum frequency above which barcodes are assigned colors [default: 0.0005].
The barcodes that do not reach the minimum frequency are colored in grey. 
This argument is used when plotting the dynamics. 
-o: Output directory [default: current working directory].
-n: Input file name.
-i: Input file.
-c: Minimum duration, in terms of time points, for which lineages must persist to be eligible for clustering.
```
### Example

Here is an example of how to use the command line with `demo_input.csv` provided in the `doblin/inst/extdata/` folder:
```
  ~$ Rscript ./demo/main.R -t 0.0005 -o ~/Documents/doblin/ -n test -i ~/Documents/doblin/inst/extdata/demo_input.csv -c 12
```
`demo_input.csv` contains data from a forward evolutionary simulation of a bacterial population under Wright-Fisher's model. The format of the input file is a csv file containing the barcode extraction results over **3 columns** (ID, Time, Reads), where 

**ID**: Consensus sequence that identifies a group of barcodes.

**Time**: Integer representing the time at which the data was measured.

**Reads**: Number of barcodes counted at a given time for a given consensus sequence.


### Visualizing the Dynamics of the Dataset

Running the pipeline interactively allows the user to delve into each step of the analysis and make informed decisions during the process.

```
Processing the command line...
Step 0: Processing CSV file...
Do you want to run this pipeline in an interactive way?(y/n): y
```
As a first step, we recommend visualizing the dataset. Note that linear-scale plots can be very resource-intensive and may take several minutes to generate.

```
Do you want to plot the dynamics of your dataset?(y/n): y
Step 1: Plotting the dynamics...
1.1 Reshaping input file into long-format dataframe...
1.2 Retrieving the first 50 barcodes with the highest maximum frequencies...
1.3 Assigning colors to lineages having reached the minimum frequency threshold among the 50 most dominant barcoded lines...
Do you want to plot a log-scale model, a linear-scale model or both? (logarithmic/linear/both): both
Plotting in progress...
Rendering linear-scale area plot. This may take a few minutes...
```
The following figures represent respectively the log-transformed and the linear representations of the dataset's dynamics.

![](https://github.com/DavidGagneLeroux/doblin/blob/master/vignettes/images/test_line.jpg?raw=true)

![](https://github.com/DavidGagneLeroux/doblin/blob/master/vignettes/images/test_area.jpg?raw=true)

### Diversity

The user can decide whether or not to display the diversity indices of the dataset. 

```
Do you want to plot the diversity of your dataset?(y/n): y
2.1 Calculating the diversity...
2.2 Plotting the diversity...
```

When q = 0, the index counts the absolute diversity in the sample, equivalent to species richness in ecological studies. When q = 1, the index weights each barcode lineage by its frequency, equivalent to the exponential of Shannon entropy H. When q → ∞, the index is the reciprocal of the proportional abundance of the most common barcode lineages, focusing only on higher-frequency lineages. The following figure illustrates the temporal evolution of the dataset's diversity indices.

![](https://github.com/DavidGagneLeroux/doblin/blob/master/vignettes/images/test_diversity.jpg?raw=true)

### Clonal Clusters

To infer clonal clusters, we employ a clustering approach based on the trajectory of frequency patterns exhibited by barcoded lineages. This entails grouping together lineages that demonstrate similar frequency patterns. To enhance the precision of our clustering approach, we omit barcodes failing to attain a **minimum mean frequency** as well as those lacking an **adequate number of time points** (controlled by the -c argument in the command line). This stringent criterion ensures that all barcode lineages included in the clustering analysis possess a sufficient number of data points for meaningful pairwise comparisons. Consequently, the lineage clustering process prioritizes dominant and persistent barcodes while excluding those that rapidly become extinct.

For this example, we'll set the minimum average frequency at `0.00005`.

```
Step 3: Clustering...
Specify a minimum mean frequency below which lineages are not taken into account during clustering (ex: 0.00005): 0.00005
3.1 Filtering the input data...
3.2 Clustering the filtered data...
```
Select a linkage/agglomeration method from those available in `stats::hclust()`:

```
Enter an agglomeration method (refer to stats::hclust() R documentation): average
```
Choose a similarity metric (Pearson correlation or DTW) for computing the pairwise distance matrix. If Pearson correlation is selected, specify a method for computing covariances in the presence of missing values, as outlined in the `stats::cor()` R documentation.

```
Enter the metric to be used to measure similarity between two time-series (pearson/dtw) : pearson
Enter a method for computing covariances in the presence of missing values. Please refer to stats::cor() R documentation (ex: pairwise.complete.obs) : pairwise
3.2.1 Computing the relative clusters for ALL thresholds between 0.1 and maximum height of hierarchical clustering... 
3.2.2 Filtering the hierarchical clustering results...
```
Clusters with a small number of lineages may be disproportionately affected by sequencing errors. To mitigate this issue, users are prompted to specify a minimum number of members per cluster (e.g., X=8). However, disregarding clusters with fewer than X members could lead to the omission of dominant clusters. In such instances, users are required to provide a minimum average frequency that must be attained by at least one of the lines within potentially disregarded clusters for them to be considered.For this example, we'll set the minimum average frequency to `0.0001`.
```
Enter the minimum number of members per cluster for test : 8
Enter the minimum average frequency to rescue small clusters: 0.0001
Warning message:
  By ignoring clusters with fewer than 8 members, you are potentially ignoring dominant clusters.
```
To determine the optimal clustering threshold in our hierarchical clustering process, we consider three overarching trends:

**Sensitivity to Sequencing Error**: Clusters with very few lineages may be overly sensitive to sequencing errors. Hence, their corresponding LOESS trajectories may not accurately represent underlying dynamics.

**Similarity Among Clusters**: When the threshold is set too low, numerous clusters emerge, but many of them exhibit similar dynamics. This abundance of similar clusters may not provide meaningful insights.

**Distinctiveness of Clusters**: Conversely, setting the threshold too high results in only a few clusters, potentially grouping together barcodes with diverse dynamics. This can obscure important variations within the dataset.

Our approach involves identifying the cross-over point between the smallest distance between cluster centroids (represented by their respective LOESS average) and the number of clusters. To accomplish this, we compute relative clusters for thresholds ranging from 0.1 to the maximum height of the hierarchical clustering tree. This step empowers users to visualize potential clusters across varying thresholds, enabling informed decision-making regarding the selection of an appropriate threshold. By balancing between sensitivity, cluster distinctiveness, and similarity, users can effectively identify the optimal threshold for their analysis. The figure below illustrates the relationship between the number of clusters (blue curve), and the smallest distance between cluster centroids (black curve). As the distance between clusters diminishes, the number of generated clusters tends to increase. Optimal clustering typically occurs around the crossover point (in this case: 0.2).

![](https://github.com/DavidGagneLeroux/doblin/blob/master/vignettes/images/test_threshold_selection.jpg?raw=true)

```
3.2.3 Quantifying the hierarchical clustering...
3.2.4 Enter the chosen threshold for the clustering of test : 0.2
```
Once a specific threshold has been provided, *Doblin* plots every resulting cluster as well as the final clonal dynamics. It's important to note that the clonal dynamics consist of the LOESS curves of each resulting cluster.

```
3.2.5 Plotting the resulting clusters...
DONE
```



