library(knitr)
# please note that rendered images are rendered in the project-root
# you need to copy those to the vignettes directory
# you can not use 'fig.path' because the path is used in the <img> tag which
# then refers to the relative path you type in
knitr::knit("vignettes/DSMolgenisArmadillo.Rmd.orig",
  output = "vignettes/DSMolgenisArmadillo.Rmd"
)
knitr::knit("vignettes/development.Rmd.orig",
  output = "vignettes/development.Rmd"
)
knitr::knit("vignettes/workspaces.Rmd.orig",
  output = "vignettes/workspaces.Rmd"
)
