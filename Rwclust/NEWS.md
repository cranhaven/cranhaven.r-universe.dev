# 0.1.0

This is the first user-facing version of the package. The following changes have been made:
* default enforcement for the length of the random walk `k` prevents pathological edge-cases where `k=1`. 
* `rwclust` now returns an S3 object with generic `plot` method
* generic methods for `weights` and `adjacency` extract the edge weights and adjacency matrix from `rwclust` S3 object

# 0.0.1

This is the first development release. It ensures the user interface is working correctly and vignettes work. 