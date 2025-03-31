
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r
library(MethEvolSIM)
```

MethEvolSIM is an R package for simulating DNA methylation dynamics on
different genomic structures along a given tree. It allows simulating
methylation and demethylation events at loci within two different types
of genomic structures: regions with low methylation or with high
methylation. The data can be interpreted as:

1- CpG sites inside or outside CpG islands or

2- any type of methylation site in regions of higher or lower
methylation or

3- CpG islands inside or outside genomic archipelagos (regions with many
CpG islands).

In this document, unless specified otherwise, we use the first
interpretation.

# A first example

As a simple example with default parameter values we show a simulation
of evolution of a simplified genomic structure on a tree:

``` r
# Define the tree:
tree <- "((a:1, b:1):2, c:2);"
# Define the genomic structure. Here 1 island ("U") containing 100 CpGs 
# surrounded by 2 non-island structures ("M") each containing 10 CpGs
infoStr <- data.frame(n = c(10, 100, 10),
                      globalState = c("M", "U", "M"))
# Simulate 1 replicate of data at the tree tips
output <- simulate_evolData(infoStr = infoStr, tree = tree)
#> Using default parameter values
#> Simulating data at root and letting it evolve along given tree:  ((a:1, b:1):2, c:2);
```

In the simulation output 0 stands for unmethylated, 1 for methylated and
0.5 for partially methylated or hemimethylated.

``` r
# In replicate 1, tip 2: name of the tip of the tree
output$data[[1]][[2]]$name
#> [1] "b"
# In replicate 1, tip 2: sequence of methylation states of left non-island structure
output$data[[1]][[2]]$seq[[1]]
#>  [1] 1 1 1 1 1 1 1 1 1 1
# In replicate 1, tip 2: sequence of methylation states of island structure
output$data[[1]][[2]]$seq[[2]]
#>   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#>  [38] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#>  [75] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# In replicate 1, tip 2: sequence of methylation states of right non-island structure
output$data[[1]][[2]]$seq[[3]]
#>  [1] 0 0 1 1 1 1 1 1 1 1
```

# Model parameters

## Initial methylation state distribution

Structures are categorized as CpG islands or non-islands (alternatively
regions with low/high methylation or archipelagos/non-archipelagos). In
the root of the genealogy we assume that each structure samples an
equilibrium probability triple $(\pi_u,\pi_p, \pi_m)$ for the
methylation states: unmethylated, partially-methylated and methylated,
respectively. Current parameter values favor prevalent homogeneity in
methylation levels within structures, most CpG islands predominantly
unmethylated and a small proportion predominantly methylated, and most
non-islands predominantly methylated and a small proportion
predominantly unmethylated. The sampling of the equilibrium probability
triple is done using two distinct Beta distributions to sample the
corresponding methylation frequencies.

**Island parameterization: alpha_pI, beta_pI, alpha_mI, beta_mI**

MethEvolSIM samples $\pi_p$ from the first Beta distribution. Current
parameterization uses as initial parameter values a relatively small
$\alpha_{I_p}$ and relatively large $\beta_{I_p}$ to encourage minor
assignment of partially methylated states.

The second Beta distribution, scaled by $1 - \pi_p$, corresponds to the
proportion of methylated and unmethylated states. Currently both
parameter values ($\alpha_{I_m}$ and $\beta_{I_m}$) set to small values
while ${\alpha_{I_m} < \beta_{I_m}}$ to favour shifting the stochastic
choice toward homogeneous states with a higher proportion of
predominantly unmethylated islands, and assigned to $\pi_m$. Finally, we
set $\pi_u$ as $1 - \pi_p - \pi_m$.

**Non-island parameterization: alpha_pNI, beta_pNI, alpha_mNI,
beta_mNI**

Following the same rationale as for island parameterization, each
non-island structure samples a common equilibrium probability triple
from two Beta distributions with parameters $\alpha_{NI_p}$,
$\beta_{NI_p}$, $\alpha_{NI_m}$ and $\beta_{NI_m}$. The parameter values
are currently set to favor the characteristic high overall methylation
level.

**Parameter ranges** The alpha and beta parameters of a Beta
distribution must be greater than 0, that is in (0, $\infty$).

## DNA methylation evolution processes

DNA methylation state evolution happens by two structure-specific
events: CpG single site events (SSEs) and CpG island wide events (IWEs).

### Island-wide events (IWEs)

IWEs change the island methylation probabilities and some of the CpG
sites in the CpG island can simultaneously change their state at the
same time. Different CpG islands, however, are assumed to evolve
independently of each other. IWEs occur per CpG island at a rate denoted
by $\mu$. Consequently, over a branch of length $l$, the expected number
of IWEs per island is given by $\mu \cdot l$.

**IWE parameterization: mu. Parameter range:** Should be a small number,
but it can take any value from 0, that is in $[0, \infty)$

### Single-site events (SSEs)

In addition to IWEs we allow for single site events (SSEs), which change
the methylation states of single CpG sites within and between CpG
islands. In our model SSEs account for two dynamics: independent of each
other (SSEi), and assuming correlations between adjacent sites (SSEc).

As SSEc transition probabilities are dependent on adjacent sites we use
Gillespie’s $\tau$-leap approximation. to allow for CpG sites to o
update their neighbouring state and rates of change after short time
steps. Therefore, we first discretize the branch intervals between IWEs
in time intervals of default length $dt =0.01$. For each CpG position
and each time interval the rate matrix for the transitions between the
states unmethylated, partially methylated and methylated results from
the addition of the rate matrices of the two types of SSE events, so
that $\mathbb{E}(R_i + R_c)=1$, $R_i$ and $R_c$ representing the rate
factors of independent and correlated processes. The independent process
samples the new methylation state from the structure’s equilibrium
frequencies and the correlated process copies the left or right neighbor
state with equal probabilities. The proportion of each of the two SSE
types is determined by the model parameter $\iota$.

Different positions $j$ have different SSEi rates $R_{i,j}$ coming from
a discretized gamma distribution with 3 categories, expected value
$\iota$ and shape parameters $\alpha_{R_{i}}$ and
${\beta_{R_{i}}}=\frac{\alpha_{R_{i}}}{\iota}$. The probability to be in
each respective rate category is 1/3. For the rate of collaborative SSEs
$R_c = 1-\iota$, the probability of considering left of right neighbour
is equal.

**SSE process parameterization: iota, alpha_Ri** **Parameter ranges**
The alpha parameter of a Gamma distribution must be greater than 0, that
is (0, $\infty$). Iota must be between 0 (non-included) and 1, that is
in (0,1\]. Note that due to numerical limitations, the smallest value of
both parameters used by MethEvolSIM is 0.01.

# Simulation Setup

## Structural distribution of CpG sites

The initial spatial distribution of CpG sites in islands and non-island
structures is given by the user through a data frame with 2 columns
named “n” and “globalState” , which encode the structural information.
Each row provides the information of a different genomic structure.

- **n** number of positions for each structure (e.g. number of CpG
  sites).

- **globalState** encodes island and non-island structures with the
  character value “U” for unmethylated regions (e.g. CpG islands) and
  “M” for methylated regions (e.g. non-CpG islands).

``` r
# Example 3 structures of length 100 each:
# one non-island containing 100 CpG sites,
# one island containing 100 CpG sites,
# and one non-island containing 100 CpG sites
infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
infoStr
#>     n globalState
#> 1 100           M
#> 2 100           U
#> 3 100           M
```

``` r
# Example for 2 islands: first of length 1, second of length 15
infoStr <- data.frame(n = c(1, 15),
                      globalState = c("U", "U"))
infoStr
#>    n globalState
#> 1  1           U
#> 2 15           U
```

## Initial methylation state distribution

The initial methylation state distribution assigns a triple of
methylation equilibrium frequencies() $\pi_u$ for unmethylated, $\pi_p$
for partially-methylated, and $\pi_m$ for methylated) to each of the
simulated genomic structures. This assignation can be done in two ways:

1 - Sampled as specified in the section “Model parameters: Initial
methylation state distribution” when the given `infoStr` only contains
`n` and `globalState` columns.

2 - Determined by the user providing in `infoStr` additional columns:
`u_eqFreq` for $\pi_u$, `p_eqFreq` for $\pi_p$ and `m_eqFreq` for
$\pi_m$. Note that when given, the triple of frequency values for each
of the structures needs to sum up to 1, so that for each row $i$,
$\pi_u(i)+\pi_p(i)+\pi_m(i)=1$.

``` r
# Example 3 structures of length 100 with customized initial methylation state
# equilibrium frequencies
infoStr <- data.frame(n = c(100, 100, 100),
                      globalState = c("M", "U", "M"),
                      u_eqFreq = c(0.1, 0.8, 0.1),
                      p_eqFreq = c(0.1, 0.1, 0.1),
                      m_eqFreq = c(0.8, 0.1, 0.8))
infoStr
#>     n globalState u_eqFreq p_eqFreq m_eqFreq
#> 1 100           M      0.1      0.1      0.8
#> 2 100           U      0.8      0.1      0.1
#> 3 100           M      0.1      0.1      0.8
```

## Parameter values

The default parameter values used by MethEvolSIM can be obtained using
the function `get_parameterValues()`, which returns a data frame with
the columns named as specified in the section “Model parameters”:

``` r
default_paramValues <- get_parameterValues()
default_paramValues
#>   alpha_pI beta_pI alpha_mI beta_mI alpha_pNI beta_pNI alpha_mNI beta_mNI  mu
#> 1      0.1       1      0.1     0.5       0.1        1       0.5      0.1 0.1
#>   alpha_Ri iota         Ri1         Ri2       Ri3
#> 1      0.1  0.3 2.80506e-06 0.005770752 0.8942264
```

Additionally, it returns the three rates of change for SSEi
process,`Ri1`, `Ri2`, and `Ri3` (see section “Model Parameters:
Single-site events (SSEs)”).

The section “Model parameters” provides further information about each
of the parameters, including the range of possible values for each
parameter. To simulate data with a different parameterization, the new
value of the chosen parameter(s) can be modified in the data frame, but
for the cases of `Ri1`, `Ri2`, and `Ri3`, which are modified through
`iota` and `alpha_Ri`.

``` r
# Example: modification of parameter iota to value 0.2
default_paramValues$iota <- 0.2
default_paramValues
#>   alpha_pI beta_pI alpha_mI beta_mI alpha_pNI beta_pNI alpha_mNI beta_mNI  mu
#> 1      0.1       1      0.1     0.5       0.1        1       0.5      0.1 0.1
#>   alpha_Ri iota         Ri1         Ri2       Ri3
#> 1      0.1  0.2 2.80506e-06 0.005770752 0.8942264
```

Additional details regarding the `get_parameterValues()` function can be
found in its documentation.

# Simulation of DNA Methylation Data

DNA Methylation Data is simulated using three R6 classes:
`singleStructureGenerator`, `combiStructureGenerator`, and
`treeMultiRegionSimulator`. Instances of the `singleStructureGenerator`
class represent individual genomic structures (e.g. islands or
non-islands) and are encapsulated within instances of the
`combiStructureGenerator` class, which, in turn, represent the simulated
data structure (e.g. a sequence consisting of a non-island containing
100 CpGs, an island containing 100 CpGs, and a non-island containing 100
CpGs). Finally, instances of the `treeMultiRegionSimulator` class
contain a `combiStructureGenerator` object at each node of a given tree,
including its root and leaves.

Generally, simulations are conducted as explained in subsection
“Simulation of Data Evolution”, with a documented user-interface
function explaining the output structure: `simulate_evolData()`.
Alternativelly, the user can simulate initial data so that the root of
the tree is fixed as explained in subsection “Simulation of Initial
Data”. The latter option generates objects of class
`combiStructureGenerator` containing objects of class
`singleStructureGenerator`. In the subsection “Simulation of Initial
Data: Exploration of Initial Data” an example to interact with the
classes is given, while further information about the attributes and
methods of each class is provided in their documentation.

## Simulation of Initial Data

The initial data can be generated as an object of class
`combiStructureGenerator`.

``` r
# Example with customized initial methylation frequencies and customized parameter values
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005
initial_customD <- simulate_initialData(infoStr = custom_infoStr, params = custom_params)
# Returns customized parameters and simulated data
initial_customD$params
#>   alpha_pI beta_pI alpha_mI beta_mI alpha_pNI beta_pNI alpha_mNI beta_mNI    mu
#> 1      0.1       1      0.1     0.5       0.1        1       0.5      0.1 0.005
#>   alpha_Ri iota         Ri1         Ri2       Ri3
#> 1      0.1  0.3 2.80506e-06 0.005770752 0.8942264
initial_customD$data
#> <combiStructureGenerator>
#>   Public:
#>     add_offspring_index: function (i) 
#>     branch_evol: function (branch_length, dt, testing = FALSE) 
#>     clone: function (deep = FALSE) 
#>     copy: function () 
#>     get_island_index: function () 
#>     get_island_number: function () 
#>     get_IWE_events: function () 
#>     get_mu: function () 
#>     get_name: function () 
#>     get_offspring_index: function () 
#>     get_own_index: function () 
#>     get_parent_index: function () 
#>     get_singleStr: function (i) 
#>     get_singleStr_number: function () 
#>     initialize: function (infoStr, params = NULL, testing = FALSE) 
#>     set_IWE_events: function (a) 
#>     set_name: function (a) 
#>     set_offspring_index: function (i) 
#>     set_own_index: function (i) 
#>     set_parent_index: function (i) 
#>     set_singleStr: function (singStrList) 
#>   Private:
#>     interval_evol: function (interval_length, dt, testing = FALSE) 
#>     IWE_events: NULL
#>     IWE_rate: 0.005
#>     mu: 0.005
#>     name: NULL
#>     offspring_index: NULL
#>     own_index: NULL
#>     parent_index: NULL
#>     set_IWE_rate: function () 
#>     singleStr: list
#>     singleStr_globalState: M U M
#>     SSE_evol: function (dt, testing = FALSE)
```

``` r
# Example with sampled initial methylation frequencies and default parameter values
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"))
initialD <- simulate_initialData(infoStr = custom_infoStr)
# Returns default parameters
initialD$params
#>   alpha_pI beta_pI alpha_mI beta_mI alpha_pNI beta_pNI alpha_mNI beta_mNI  mu
#> 1      0.1       1      0.1     0.5       0.1        1       0.5      0.1 0.1
#>   alpha_Ri iota         Ri1         Ri2       Ri3
#> 1      0.1  0.3 2.80506e-06 0.005770752 0.8942264
initialD$data
#> <combiStructureGenerator>
#>   Public:
#>     add_offspring_index: function (i) 
#>     branch_evol: function (branch_length, dt, testing = FALSE) 
#>     clone: function (deep = FALSE) 
#>     copy: function () 
#>     get_island_index: function () 
#>     get_island_number: function () 
#>     get_IWE_events: function () 
#>     get_mu: function () 
#>     get_name: function () 
#>     get_offspring_index: function () 
#>     get_own_index: function () 
#>     get_parent_index: function () 
#>     get_singleStr: function (i) 
#>     get_singleStr_number: function () 
#>     initialize: function (infoStr, params = NULL, testing = FALSE) 
#>     set_IWE_events: function (a) 
#>     set_name: function (a) 
#>     set_offspring_index: function (i) 
#>     set_own_index: function (i) 
#>     set_parent_index: function (i) 
#>     set_singleStr: function (singStrList) 
#>   Private:
#>     interval_evol: function (interval_length, dt, testing = FALSE) 
#>     IWE_events: NULL
#>     IWE_rate: 0.1
#>     mu: 0.1
#>     name: NULL
#>     offspring_index: NULL
#>     own_index: NULL
#>     parent_index: NULL
#>     set_IWE_rate: function () 
#>     singleStr: list
#>     singleStr_globalState: M U M
#>     SSE_evol: function (dt, testing = FALSE)
```

To get the parameter values of a `combiStructureGenerator` object, it
can alternativelly be given as argument to the function
`get_parameterValues()`.

``` r
combiStr_object <- initialD$data
get_parameterValues(rootData = combiStr_object)
#>   alpha_pI beta_pI alpha_mI beta_mI alpha_pNI beta_pNI alpha_mNI beta_mNI  mu
#> 1      0.1       1      0.1     0.5       0.1        1       0.5      0.1 0.1
#>   alpha_Ri iota         Ri1         Ri2       Ri3
#> 1      0.1  0.3 2.80506e-06 0.005770752 0.8942264
```

### Exploration of Initial Data

The methods of the class `combiStructureGenerator` can be used to access
each of the contained structures:

``` r
# E.g. access fist structure (non-island)
combiStr_object$get_singleStr(1)
```

Each contained structure is an object of `singleStructureGenerator` and
its methods can be used to get the contained information.

``` r
# E.g. get the methylation equilibrium frequencies of the first singleStructureGenerator
combiStr_object$get_singleStr(1)$get_eqFreqs()
#> [1] 0.77331702 0.19727515 0.02940782
```

``` r
# E.g. get the sequence of methylation states of the second singleStructureGenerator
# Encoded as: 1 for unmethylated, 2 for partially-methylated, 3 for methylated
combiStr_object$get_singleStr(2)$get_seq()
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```

## Simulation of Data Evolution

The evolution data is generated with the function `simulate_evolData()`.
It returns the parameters used (`$params`), the length of the time step
used for the Gillespie’s $\tau$-leap approximation (`$dt`, default
0.01), the tree used (`$tree`), and the simulated data (`$data`). The
description of the simulated data is available in the function’s
documentation.

It takes as parameters:

- `tree`: A string representing a tree in Newick format.

- `infoStr`: A dataframe with the information of the number of CpGs
  corresponding to each island and non-island region (columns named `n`
  and `globalState`). Additionally the initial methylation frequencies
  can be specified adding the columns `u_eqFreq`, `p_eqFreq` and
  `m_eqFreq`.

- `dt`: The length of time steps for Gillespie’s $\tau$-leap
  approximation. Numerical, default is 0.01.

- `n_rep`: The number of replicates. Integer, default is 1.

- `only_tip`: Logical indicating whether to extract data only for tips
  (TRUE, default) or to extract the information for all the tree
  branches (FALSE).

- `params`: Default NULL for simulation with default parameter values.
  When customized parameter values are used the output dataframe of
  `get_parameterValues()` can be modified and given to the function.
  E.g:

``` r
# Example with customized initial methylation frequencies, customized parameter values 
# and default dt (0.01)
tree <- "((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);"
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005
evolD <- simulate_evolData(infoStr = custom_infoStr, tree = tree, params = custom_params, n_rep = 3, only_tip = TRUE)
#> Simulating data at root and letting it evolve along given tree:  ((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);
#> Simulating data at root and letting it evolve along given tree:  ((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);
#> Simulating data at root and letting it evolve along given tree:  ((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);
```

``` r
# Returns customized parameters, tree used, time step length for SSE process used (dt) 
# and simulated data
evolD$data
evolD$dt
evolD$tree
evolD$params
```

``` r
# The simulated data is returned as a list. Each element of the list corresponds to a
# simulation replicate:
length(evolD$data)
#> [1] 3
rep1 <- evolD$data[[1]]
rep2 <- evolD$data[[2]]
rep3 <- evolD$data[[3]]
```

``` r
# When only_tip is TRUE, each replicate contains for each tip: its name and the 
# sequence of methylation states. E.g.
rep1_tip1 <- rep1[[1]]
rep1_tip1$name
#> [1] "a"
# Seq is a list with the methylation states of each simulated structure.
# In this example, 3 structures: non-island, island and non-island ...
length(rep1_tip1$seq)
#> [1] 3
# Each with 100 CpGs
length(rep1_tip1$seq[[1]])
#> [1] 100
```

When simulations are conducted with `only_tip = FALSE` the `$data`
output contains more information.

``` r
tree <- "((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);"
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005
evolD <- simulate_evolData(infoStr = custom_infoStr, tree = tree, params = custom_params, n_rep = 3, only_tip = FALSE)
#> Simulating data at root and letting it evolve along given tree:  ((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);
#> Simulating data at root and letting it evolve along given tree:  ((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);
#> Simulating data at root and letting it evolve along given tree:  ((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);
```

``` r
# When only_tip is FALSE. $data output contains the simulated data and the 
# information on the relationship between tree branches.
names(evolD$data)
#> [1] "sim_data"     "branchInTree"
```

``` r
# The information of the tree branches can be accessed using $branchInTree. 
treeStr <- evolD$data$branchInTree
# It is a list in which each element index represents the index of the branch
root <- treeStr[[1]] # First branch (or tree root)
b2 <- treeStr[[2]] # Second branch 
# ...
# Each branch contains information of the offspring and parent indexes.
root$parent_index # The tree root does not have parent branches
#> NULL
root$offspring_index # The tree root has 3 daughter branches: 2, 5 and 6. 
#> [1] 2 5 6
b2$parent_index # Branch 2 therefore has as parent branch the root.
#> [1] 1
b2$offspring_index # Branch 2 also has 2 daughter branches: 3 and 4
#> [1] 3 4
```

``` r
# The simulated data can be accessed using $sim_data.
sim_data <- evolD$data$sim_data
# As before, each list element corresponds to a simulation replicate. 
# E.g. replicate 3
rep3 <- sim_data[[3]]
# In each replicate, each element of the list corresponds to a tree branch
# the indexes correspond to the information in $branchInTree
root <- rep3[[1]]
b2 <- rep3[[2]]
b3 <- rep3[[3]]
```

``` r
# Each tree branch contains:
# - the branch name (NULL for tree root and inner nodes and 
# tip name for the tree tips):
root$name
#> NULL
b2$name
#> NULL
b3$name
#> [1] "a"
```

``` r
# - the information on IWE events that happened in that branch. 
root$IWE # The root always has NULL because its branch length is 0
#> NULL
# The rest of the branches have FALSE when no IWE happened or a list containing
# $islands corresponds to the island index(es) to which the event(s) happened 
# $times corresponds to the branch time in which the event happened.
b2$IWE
#> [1] FALSE
b3$IWE
#> [1] FALSE
```

``` r
# - A list with each element representing the sequence of methylation states of 
# each simulated island and non-island
# Encoded as 0 for unmethylated, 0.5 for partially methylated and 1 for methylated
root$seq[[1]] # First sequence (non-island)
#>   [1] 1.0 0.5 1.0 1.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 0.5 0.0 1.0 1.0 1.0
#>  [19] 0.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 0.5 1.0 1.0 1.0 0.5 1.0 1.0 1.0 1.0 1.0
#>  [37] 1.0 0.0 0.5 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.5 1.0 1.0 1.0 1.0
#>  [55] 1.0 0.5 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.5 1.0 0.0 1.0 1.0 1.0 1.0
#>  [73] 1.0 1.0 1.0 0.5 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 0.5 1.0 1.0 1.0 1.0 1.0
#>  [91] 1.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0
root$seq[[2]] # Second sequence (island)
#>   [1] 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.5 0.0 0.0 0.0
#>  [19] 0.0 0.0 0.0 0.5 0.0 1.0 1.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0
#>  [37] 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 1.0 0.0 0.0 0.5 0.0 0.5
#>  [55] 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.5
#>  [73] 0.5 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0
#>  [91] 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0
```

``` r
# - A list with each element representing the methylation frequencies for the states: unmethylated, partially-methylated and methylated
root$eqFreqs[[1]][1] # First structure of root (non-island) equilibrium frequency for unmethylated state
#> [1] 0.1
root$eqFreqs[[1]][2] # First structure of root (non-island) equilibrium frequency for partially-methylated state
#> [1] 0.1
root$eqFreqs[[1]][3] # First structure of root (non-island) equilibrium frequency for methylated state
#> [1] 0.8
```

- `rootData`: Alternative argument to `infoStr` (only one can be given).
  It enables simulating replicates of evolution along a tree with fixed
  data at root. When used, it takes the output of
  `simulate_initialData()$data`. If customized parameter values are to
  be used, then the customized parameters are given to
  `simulate_initialData()` instead to `simulate_evolData()`.

``` r
# Example with customized initial methylation frequencies, customized parameter values and default dt (0.01)
tree <- "((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);"
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005
initialD <- simulate_initialData(infoStr = custom_infoStr, params = custom_params)$data
evolD <- simulate_evolData(rootData =initialD, tree = tree)
#> Parameter values set as in given rootData
#> Simulating evolution of given data at root along given tree:  ((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);
```
