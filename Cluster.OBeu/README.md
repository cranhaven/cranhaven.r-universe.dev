Cluster.OBeu
================
Kleanthis Koupidis, Charalampos Bratsas, Jaroslav Kuchar
November 9, 2016

\#[Cluster.OBeu](https://okgreece.github.io/Cluster.OBeu/index.html)
Εstimate and return the necessary parameters for cluster analysis
visualizations, used in [OpenBudgets.eu](http://openbudgets.eu/). It
involves a set of techniques and algorithms used to find and divide into
groups the Budget data of municipalities across Europe, described by the
[OpenBudgets.eu data model](https://github.com/openbudgets/data-model).

The available clustering algorithms are hierarchical, kmeans from R
base, pam, clara, fuzzy from [cluster
package](https://CRAN.R-project.org/package=cluster) and model based
algorithms from [mclust
package](https://CRAN.R-project.org/package=mclust). It can be used to
find the appropriate clustering algorithm and/or the appropriate
clustering number of the input data according to the internal and
stability measures from [clValid
package](https://CRAN.R-project.org/package=clValid).

This package can generally be used to estimate clustering parameters,
extract and convert them to JSON format and use them as input in a
different graphical interface and also can be used in data that are not
described by the [OpenBudgets.eu data
model](https://github.com/openbudgets/data-model).

You can see detailed information
[here](https://okgreece.github.io/Cluster.OBeu/).

``` r
# install Cluster.OBeu- cran stable version
install.packages(Cluster.OBeu) 
# or
# alternatively install the development version from github
devtools::install_github("okgreece/Cluster.OBeu")
```

Load library `Cluster.OBeu`

``` r
library(Cluster.OBeu)
```

\#Cluster Analysis in a call

`cl.analysis` can be used to estimate clustering model parameters and/or
number of clusters needed for visualization of clusters and other
clustering measures as list object.

``` r
cluster_data = cl.analysis( city_data, cl.aggregate = "sum", 
                            cl.meth = "pam", clust.numb = NULL, dist = "euclidean", tojson = T) # json string format

jsonlite::prettify(cluster_data) # use prettify of jsonlite library to add indentation to the returned JSON string
```

\#Cluster Analysis on OpenBudgets.eu platform

`open_spending.cl` is designed to estimate and return the clustering
model measures of [OpenBudgets.eu](http://openbudgets.eu/) datasets.

The input data must be a JSON link according to the [OpenBudgets.eu data
model](https://github.com/openbudgets/data-model). There are different
parameters that a user could specify, e.g. `dimensions`,
`measured.dimensions` and `amounts` should be defined by the user, to
form the dimensions of the dataset. `open_spending.cl` estimates and
returns the json data that are described with the [OpenBudgets.eu data
model](https://github.com/openbudgets/data-model), using `cl.analysis`
function.

``` r
#Store the link in a variable
json_link='http://ws307.math.auth.gr/rudolf/public/api/3/cubes/budget-athens-revenue-2007__93458/aggregate?drilldown=budgetPhase.prefLabel%7CadministrativeClassification.prefLabel&aggregates=amount.sum'

clustering = open_spending.cl(
  json_data =  json_link, 
  dimensions ="administrativeClassification.prefLabel",
  measured.dimensions ="budgetPhase.prefLabel",
  amounts = "amount.sum",
  cl.method = "fanny",
  cl.num = 3
  )
# Pretty output using prettify of jsonlite library
jsonlite::prettify(clustering,indent = 2)
```
