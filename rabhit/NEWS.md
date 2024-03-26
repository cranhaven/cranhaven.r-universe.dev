Version 0.2.5: February 6, 2023
-------------------------------------------------------------------------------

General:

+ Removed deprecated function of dplyr

Version 0.2.4: September 14, 2022
-------------------------------------------------------------------------------

General:

+ Generalized the createFullHaplotype to allow other genes as anchor.

Version 0.2.3: March 14, 2022
-------------------------------------------------------------------------------

General:

+ Fixed a bug in the haplotype plot, cases where a gene is within the haplotype table but not in the gene_order flag.

Version 0.2.1: February 16, 2022
-------------------------------------------------------------------------------

General:

+ updated the package for changes in dependencies for the haplotype plots.
+ add a new function `readHaplotypeDb` for reading haplotype tables created from `createFullHaplotype`. For tables downloaded from VDJbase.org, reading the files into R is recommended with the new function.

Version 0.1.6: June 10, 2020
-------------------------------------------------------------------------------

Backwards Incompatible Changes:

+ From this version the gene order within plots can be determined by the genes_order variable. The variable receives a vector of the ordered genes. If not supplied the default order for the genes is by the GENE.loc order which can be found in the package using `data("GENE.loc")`. 
+ Hereby, the gene_order variable has been removed. 

General:

+ updated the package for TRB haplotypes.

Version 0.1.5: June 10, 2020
-------------------------------------------------------------------------------

Backwards Incompatible Changes:

+ Changed default expected data format from the Change-O data format to the AIRR Rearrangement standard. For example, where functions used the column name V_CALL (Change-O) as the default to identify the field that stored the V gene calls, they now use v_call (AIRR). Scripts that relied on default values (previously, toHap_col="V_CALL"), will now fail if calls to the functions are not updated to reflect the correct value for the data. If data are in the Change-O format, the current default value toHap_col="v_call" will fail to identify the column with the V gene calls as the column v_call doesn't exist. In this case, toHap_col="V_CALL" needs to be specified in the function call.
+ For consistency with the style of the new data format default, field names in all other user exposed data structures have been updated to use the same font case style.

General:

+ Updated dependencies to R >= 3.5.0, dplyr >= 1.0.0, ggplot2 >= 3.2.0, alakazam >= 1.0.0, tigger >= 1.0.0, and tidyr >=1.0.0.

Version 0.1.4:  January 28, 2020
-------------------------------------------------------------------------------
+ Fixed bug within the `hapHeatmap` visualization.

Version 0.1.3.999:  November 18, 2019
-------------------------------------------------------------------------------
+ Adapted `deletionsByVpooled` to infer single chromosome deletion from light chain repertoire.
+ Added html5 interactive output for `deletionHeatmap` visualization.

Version 0.1.3:  September 8, 2019
-------------------------------------------------------------------------------
+ Optimized `hapHeatmap` visualization, r base graph.

Version 0.1.2:  July 25, 2019
-------------------------------------------------------------------------------
+ Improved visualization of short read sequences annotation.

Version 0.1.1:  May 8, 2019
-------------------------------------------------------------------------------
+ Improved `createFullHaplotype` run-time
+ Fixed a bug wherein `createFullHaplotype` miss-handled novel allele labels

Version 0.1.0:  March 4, 2019
-------------------------------------------------------------------------------

+ Initial public release.
