# oesr
Distributing OES Functions

# Overview

This package divides the work of plotting treatment and control outcomes into two steps:

1. use `oes_prep()` to create a tidy data table of results to be plotted;
2. use `oes_plot()` to plot the tidy data object created by `oes_prep()`.

# Example Session

The sample session below installs and uses the package functions.

You only need to install the package once (per update).

First, provide the path to the tar.gz:

```r
# Try installing straight from the web:
tar.gz_path <- "https://github.com/gsa-oes/oesr/blob/main/oesr_0.0.1.tar.gz"

# If this fails, download the tar.gz, and provide the path to the tar.gz:
# (This must be specific to where you downloaded/saved the file.)
# (If the .gz extension is removed, just use .tar.)

tar.gz_path <- "~/Desktop/oesr_0.0.1.tar.gz"
```

Then, install the package:

```r
install.packages(tar.gz_path, repos = NULL, type = "source")
```

Then, load and attach the package (and, for now, needed `tidyverse`):

```r
library(oesr)
library(tidyverse)
```

Then, use it to plot a simulated treatment effect:

```r
# Simulate some data yourself:
df <- tibble(tr = rbinom(100, 1, 0.4), y = rnorm(100) + tr)
lm_out <- lm(y ~ tr, data = df)

# Plot with OES style, specifying treatment `vars`:
lm_out %>% 
  oes_prep() %>%
  oes_plot(treatment_vars = "tr")

# Use data included in the package:
data("df_oes")
fit <- lm(y1 ~ x1, df_oes)

# Plot with OES style, specifying treatment `arms`:
fit %>% 
  oes_prep() %>%
  oes_plot(treatment_arms = 1)

# View the help file in R:
help(oes_plot)
```

# FAQ

1. How do I deal with this `grid.Call()` Lato font error?

If you get an error like
```
1: In grid.Call(L_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
  no font could be found for family "Lato"
```

then install the Lato font.

2. How do I install the Lato font?

To get the Lato font, download it from https://fontmeme.com/fonts/lato-font/. 

Then, on macOS, 

- open Pages, and in the menus go to Format - Font - Show Fonts.
- Then open Manage Fonts in the three-dot menu at the upper left of the Fonts window. 
- Then click the `+` sign to add a font, and point the selection dialogue to the "lato" folder you downloaded.

3. How do I deal with this `grid.Call()` polygon error?

If you get an error that says `grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, polygon edge not found`, try reinstalling X11 from https://www.xquartz.org/, then reinstalling the `ggplot2` package.
