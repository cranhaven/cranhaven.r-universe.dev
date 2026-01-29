# interplex 0.1.2

Per updated CRAN requirements, this patch moves several packages that are used in converters from `Suggests:` to `Imports:`.
It relieves the previously imposed conditions on tests (though not on examples) that use them. The exceptions are {TDA} and {igraphdata}, which are not used by any converters.

The patch also removes the `@docType` {roxygen2} tag in favor of `"_PACKAGE"` and fixes a broken code link and several URLs.
Finally, it uses `utils::getFromNamespace()` to invoke commands from {simplextree} v0.9.1 that were discontinued it v1.0.0.

# interplex 0.1.1

Per CRAN requirements, this patch imposes conditions on tests and examples that use packages in `Suggests:`.

# interplex 0.1.0

This first release includes converters between the following data structures:

* complete lists of simplices, used by the {TDA} package
* simplex tree instances of class 'Rcpp_SimplexTree',
  provided by the {simplextree} package
* simplex trees as implemented in Python GUDHI through the {reticulate} interface
* objects of class 'igraph', provided by the {igraph} package
* objects of class 'network', provided by the {network} package

Coercion among the graph/network classes is done using methods from the {intergraph} package. Simplicial complexes are only directly coerced from the 'igraph' class.
