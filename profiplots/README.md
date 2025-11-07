# **`profiplots`**

The `profiplots` ([Docs](https://datascience.profinitservices.cz/sablony/profiplots/) | [![](https://gitlab.com/gitlab-com/gitlab-artwork/raw/master/logo/logo-square.png){width="20px"}](https://git.profinit.eu/datascience/sablony/profiplots) | [Report issue](https://git.profinit.eu/datascience/sablony/profiplots-r/-/issues)) is an R package that provides an easy way to let your R plots (both GGplot and baseR graphics!) look more professional & follow the new [Profinit](https://profinit.eu)'s visual guidelines.

Usefull links:

* [Documentation](https://datascience.profinitservices.cz/sablony/profiplots-r/)
* [Git repository](https://git.profinit.eu/datascience/sablony/profiplots-r)
* [Issues](https://git.profinit.eu/datascience/sablony/profiplots-r/-/issues) to report a bug or request a feature.


## Installation

**A) From CRAN**

```
# Install released version from CRAN 
install.packages("profiplots")        # NOTE: Not ready yet
```

**B) From package documentation site** (internal only).

As the package is hosted on GitLab Pages, it is possible to install it on your machine this way. It is quite simple!

1. __Download the package__

Download the package manually from this link: [profiplots](https://datascience.profinitservices.cz/sablony/profiplots-r/profiplots_latest.tar.gz).


2. __Install the package from a file__

``` r
install.packages('path/to/profiplots_latest.tar.gz', repos=NULL, type='source')
```



## Usage

Quick start:

```
library(profiplots)
profiplots::set_theme("blue-red", "blue-red")

# now, you can continue with your ggplot or baseR graphics as usual
```

Read the [reference page](https://datascience.profinitservices.cz/sablony/profiplots-r/reference//). You might be interested in the [graph gallery page](https://datascience.profinitservices.cz/sablony/profiplots-r/articles/plots-gallery.html) with the most Frequently Used Charts (WIP).
