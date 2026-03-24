## Version: 1.0.0

Contained very basic information and only the make_state_matrix that can be useful

## Version: 1.1.0

### Changes

- Added:

    - `fast_clara_jaccard`	Fast CLARA-like clustering using Jaccard dissimilarity
    - `get_cluster_sequences`	Extract sequences of length k within clusters
    - `sequence_stats()` to compute frequency, conditional probability, and relative risk of sequences by cluster

- A vignette displaying a basic workflow is available 

### Bug Fixes

- `fast_jaccard_dist`	were corrected 

### Comments on current version 

This version will hopeful permit to run basic analyses of electronic health records. Further examples and functions are expected soon.  


## Version: 1.2.0

### Changes

Corrected, modified and integrated `sequence_stats()` and `get_cluster_sequences`. The sequence frenquencies are computed by patient not on the total number of sequences

Corrected some spelling in the vignette.

### To do

Plot methods for sequences



