 <!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mabafaba/lifx?branch=master&svg=true)](https://ci.appveyor.com/project/mabafaba/lifx)
[![CRAN status](https://www.r-pkg.org/badges/version/lifx)](https://CRAN.R-project.org/package=lifx)

## 'LIFX' R package 

The 'LIFX' R package is an interface to the ['LIFX' smart bulb api](https://api.developer.lifx.com/docs). It lets you view and change your lights' states, for example like this:

```
lx_color(hue = 200,saturation = 0.3,brightness = 0.5, )
```

install with github from devtools:
```{r}
devtools::install_github("mabafaba/lifx", build_vignettes = TRUE)
```

Once on CRAN, you can install it with:

```{r}
install.packages("lifx")
```

Show vignettes / user manual:

```{r}
browseVignettes("lifx")
```

