# blockTools
R package for Design of Randomized Experiments

`blockTools` blocks units into experimental blocks with one unit per treatment condition by creating a measure of multivariate distance between all possible pairs of units.  Users can set the maximum, minimum, or an allowable range of differences between units on one variable.  `blockTools` also randomly assigns units to treatment conditions, and can diagnose potential interference between units assigned to different treatment conditions.  Users can write outputs to `.tex` and `.csv` files.

# Examples

At the R prompt, type:
```
# load the example data: 
data(x100) 

# create blocked pairs:
out <- block(x100, id.vars = "id", block.vars = c("b1", "b2")) 

# assign one member of each pair to treatment/control:
assg <- assignment(out) 

# detect unit pairs with different treatment assignments 
#   that are within 1 unit of each other on variable "b1":
diag <- diagnose(assg, x100, id.vars = "id", suspect.var = "b1", suspect.range = c(0,1)) 
```

To view the results:
```
# The blocked pairs:
out$blocks 

# The assigned pairs:
assg

# Those pairs with small distances on "b1" between them:
diag
```

# Installation

Install `blockTools` with

```
install.packages("blockTools")
```

If you have access to the private repository, this package can be installed via

```
devtools::install_github("ryantmoore/blockTools", 
                         auth_token = "<your PAT for this private repo>")
```
