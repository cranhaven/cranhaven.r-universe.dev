## MorphSim 

R package for simulating discrete character data along rooted phylogenies. Through integration with established R packages, MorphSim enables the simulation of biologically meaningful datasets across a variety of scenarios. 


### Installation

MorphSim is available on CRAN:
```r
install.packages("MorphSim")
```

The development version can be installed from GitHub:
```r
library(devtools)
devtools::install_github("https://github.com/fossilsim/morphsim")
```

### Dependencies

MorphSim depends on R (≥ 3.5) and imports ape, FossilSim, and phangorn. TreeSim is suggested for tree simulation.

### License

GPL-3  

## Quick Start Tutorials 
For a more in-depth explanation of the functionality of MorphSim see the introductory vignette.

## Example 1: Simulating Data Along a Phylogenetic Tree - Basic set up
To simulate morphological data you will first need a phylogenetic tree. MorphSim can take either, a time tree or a tree with branch lengths in genetic distance as input. In this example we will use a time tree. You can simulate a tree using the existing R package `TreeSim`

```r
install.packages("TreeSim")
library(TreeSim)
```

#### Simulate a birth death tree using TreeSim
```r
lambda = 0.1         # speciation rate
mu = 0.05            # extinction rate
tips = 5             # number of extant tips

tree = TreeSim::sim.bd.taxa(n = tips, 
                         numbsim = 1, 
                         lambda = lambda, 
                         mu = mu)[[1]]                       
```
#### Simulate Morphological data using MorphSim. 
MorphSim lets you specify a variety of parameters for your simulations. In this example, we will focus on a few key parameters to get started.
- **k**: the maximum number of character states. This can be a single integer greater than 1 or a vector of integers if you want to simulate more than 1 partitions. In the following example, we are therefore simulating two partitions, the first with 2 states and the second with 3.
- **timetree**: if using a time tree specify here.
- **tree**: if using a genetic distance tree specify here.
- **partitions**: specify the number of traits per partition. This must match the number of states specified in k.
- **trait.num**: specify the total number of traits 
- **br.rates**: This can be a single integer (strict clock) or a vector of integers (relaxed clock). This must be provided when using a time tree to convert the branch lengths into genetic distance.
- **ancestral**: return the states at the internal nodes in the tree.

> Setting up a simulation as follows will simulate characters under an Mk (equal rates) model with a strict clock.


```r
morpho_data <-  sim.morpho(k = c(2,3), 
                           time.tree = tree,
                           tree = NULL,
                           partition = c(10,10),
                           trait.num = 20,
                           br.rates = 0.1,
                           ancestral = TRUE)  

```
#### Explore the simulated data
Morphsim has a plotting function which allows you to plot the simulated data along the branches of the tree. 
```r
plot(morpho_data, 
     trait= 1) # specify which trait you want to plot along the tree
```
This function returns a number of characteristics about your data. It calculates the consistency index, the retention index, identifies convergent traits, and the number of extinct and extant taxa.
```r
stats <- stats.morpho(morpho_data)
```




## Example 2: Simulating Biologically Realistic Data Sets
The previous example, simulated characters according to an Mk model under a strict clock. However, there are more complex models we may want to use for simulations. Firstly, with MorphSim we can use a relaxed clock model where each branch has a different rate. We can use an existing R package, `simclock` to simulate rates along our tree which we can input into MorphSim 


####  Simulate branch lengths using a independent log normal relaxed clock model

```r
devtools::install_github("dosreislab/simclock")
reltt <- simclock::relaxed.tree(tree, 
                                model="iln", 
                                r=.04e-1,     # mean mutation rate
                                s2=.26e-2)   # diffusion rate
```
For the character data we can relax the assumptions of the Mk model in a number of ways as described below. The br.rates argument accepts any numeric vector of rates, one per branch, in the same order as tree$edge. Here we pass reltt$edge.length from simclock, but any custom vector of branch rates can be used directly, for example, br.rates = rep(0.1, nrow(tree$edge)) for a strict clock, or a user-defined vector reflecting prior knowledge about lineage-specific rates.

- **ACRV**: Here you can specify if you would like to model among character rate variation. This is commonly used for morphological data sets as traits may evolve at different rates. There are three input options here, `gamma`, `lgn`, and `user`. Gamma uses a discrete gamma distribution, lgn will use a discrete lognormal, and user allows the user to specify rates.

- **ACRV.ncats**: the number of rate categories you want to simulate under

- **alpha.gamma**: the shape of the alpha distribution

- **variable**: When set to true this will only return traits which vary across taxa, i.e., the MkV model

```r
morpho_data <-  sim.morpho(k = c(2,3), 
                           time.tree = tree,
                           tree = NULL,
                           partition = c(10,10),
                           trait.num = 20,
                           br.rates = reltt$edge.length,
                           ACRV = "gamma",
                           alpha.gamma = 1,
                           ACRV.ncats = 4,
                           variable = TRUE)

```

> Note: If you choose to simulate data under an ACRV using a lognormal distribution, you will need to provide values for the `meanlog` and the `sdlog`. Similarly for the user option the rates should be passed through `define.ACRV.rates`

### Ordered characters
In morphological datasets, transitions between character states are often restricted, meaning that a change can only occur between specific states. For example, a character might be able to transition from 0 → 1, but not directly from 0 → 2; the species must pass through the intermediate state. These are known as correlated characters. In MorphSim, we can simulate such characters by defining a Q matrix that enforces these allowed transitions.

Define a Q matrix the allows 0 &harr; 1 &harr; 2

```r
ord_Q <- matrix(c(
  -0.5, 0.5, 0.0,
  0.3333333, -0.6666667, 0.3333333,
  0.0, 0.5, -0.5
), nrow = 3, byrow = TRUE)
```
This Q matrix can then be used to simulate characters

```r
morpho_data <-  sim.morpho(k = 3, 
                           define.Q = ord_Q,
                           time.tree = tree,
                           tree = NULL,
                           trait.num = 10,
                           br.rates = reltt$edge.length,
                           ACRV = "gamma",
                           alpha.gamma = 1,
                           ACRV.ncats = 4,
                           variable = TRUE)
```

## Example 3: Simulating Data sets with Sampled Ancestors
MorphSim integrates with `FossilSim`, allowing fossils simulated in FossilSim to be incorporated when simulating character traits. Because MorphSim tracks where along branches transitions occur, it can generate traits that are specific to the time and lineage in which a fossil is sampled.

```r
install_github("fossilsim/fossilsim")
```
#### Simulate fossil and extant species sampling

```r
rate = 0.1 # fossil sampling rate
f = FossilSim::sim.fossils.poisson(rate = rate, 
                                   tree = tree, 
                                   root.edge = F)

rho = 0.5 # extant species sampling rate
f2 = FossilSim::sim.extant.samples(fossils = f, 
                                   tree = tree, 
                                   rho = rho)
```

This fossils object can be passed into the `sim.morpho` function

```r
morpho_data <-  sim.morpho(k = c(2,3), 
                           time.tree = tree,
                           tree = NULL,
                           partition = c(10,10),
                           trait.num = 20,
                           br.rates = reltt$edge.length,
                           ACRV = "gamma",
                           alpha.gamma = 1,
                           ACRV.ncats = 4,
                           variable = TRUE,
                           fossil = f2)
```
We now have character data for all the tips in the true tree, as well as all the sampled ancestors. The inclusion of fossils also adds a number of options to the plotting function. We can choose if we want to plot the fossils along the tree, and to show the reconstructed version. 

```r
plot(x = morpho_data, 
     trait= 1, 
     timetree = T, 
     show.fossil = T,
     root.edge = T, 
     reconstructed = T)
```

## Example 4: Simulating Parsimony Informative characters
MorphSim allows you to filter simulated characters based on their parsimony informativeness. By changing the default `parsimony = NULL` to either `"standard"` or `"strict"`, you can control which traits are simulated in your dataset.

- **standard**: Retains characters with at least two effective states, including autapomorphies (traits unique to a single taxon)

- **strict**: Only retains parsimony-informative characters (traits shared by at least two taxa)

```r
morpho_data <-  sim.morpho(k = c(2,3), 
                           time.tree = tree,
                           tree = NULL,
                           partition = c(10,10),
                           trait.num = 20,
                           br.rates = 0.1,
                           parsimony = "standard",   # alternatively use "strict" to exclude autapomorphies
                           ancestral = TRUE,
                           fossil = f2)
``` 

