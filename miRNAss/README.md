This is the version 1.2 of the miRNAss R package
-------------------------------------------------
MiRNAss is a machine learning method specifically designed for pre-miRNA prediction. It takes advantage of unlabeled sequences to improve the prediction rates even when there are just a few positive examples, and when the negative examples are unreliable or are not good representatives of its class. Furthermore, the method can automatically search for negative examples if the user is unable to provide them. MiRNAss can find a good boundary to divide the pre-miRNAs from other groups of sequences; it automatically optimizes the threshold that defines the classes boundaries, and thus, it is robust to high class imbalance. Each step of the method is scalable and can handle large volumes of data.

Contact
- Cristian Yones <cyones(at)sinc.unl.edu.ar>
- sinc(i):  http://fich.unl.edu.ar/sinc/

Package installation
--------------------
This library uses some well-known R packages. The following must be installed:

CRAN packages:
- RSpectra (Version >= 0.12-0).
- CORElearn, (Version >= 1.48.0).
- Rcpp (Version >= 0.12.8).

The library was developed and tested in R version 3.3.2 and 3.4.0.

MiRNAss can be installed from the CRAN repository executing the following command from the R console:

> install.packages("miRNAss")

Alternatively, the package (called miRNAss_x.xx.x_linux.tar.gz for Linux machines and miRNAss_x.xx.x_windows.zip for Windows) can be download from SourceForge. Then, using an IDE such as RStudio, install it as a local package. If you do not have an IDE, install the package from R console with the following commands:

> install.packages("<path_to_package>", repos = NULL, type = "source")

Usage
-----
After install the package, load it with the following command:

> library("miRNAss")

The following command is the simplest way to execute miRNAss:

> miRNAss(features, labels)

Where:
‘features’ is a data frame with the features extracted from  sequences, one sequence per row and one numeric feature per column.
‘labels’ is a numeric vector where the i-th element has a value of 1 if it is a well-known pre-miRNA, a -1 if it is not a pre-miRNA, and zero if it is an unknown sequence that has to be classified (predicted) by the method.

For more help about all the parameters and a full example execute:

> help(miRNAss)

Datasets and test scripts
-------------------------
Once installed the package, a set of experiments and comparisons with other methods can be done. The scripts and the data of these experiments can be found in:

http://sourceforge.net/projects/sourcesinc/files/mirnass/miRNAss-experiments.zip

To run these tests, after unzip the file, set this directory as the working directory and simply run each script with the function "source":

> setwd('<path_to_the_unzipped_directory>')
> source("2_delta-mirBase.R")

This will generate one csv file for each test in the “results” folder. It is important to point that most of these experiments are computationally expensive and could take quite a while (about 40 minutes for the experiment 2_delta-mirBase.R in an intel i7 PC).
You can plot the results executing:

> source("plotResults.R")

The figures will be saved in the folder "results".
