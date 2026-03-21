# Contemporary Portfolio Optimization with R

&copy; 2013-2018 Ronald Hochreiter / <ron@hochreiter.net>

This package aims at implementing something along the lines of a tidy portfolio optimization framework, simplifying the whole process from data to decision as good as possible.

More about Finance with R can be found at http://finance-r.com/

## Main Motivation

The main motivation is to create a `R` package that simplifies the process of portfolio optimization as much as possible. Furthermore providing an approach to portfolio optimization which is completely agnostic to risk measures and optimization methods. Finally the approach should naturally fit into the contemporary `R` piping concept using packages like `magrittr`.
 
## Usage

### Instant gratification

```
# Load the package
library(portfolio.optimization)

# Use any scenario data, e.g. the one provided with the package
data(sp100w17av30s)

# Do a portfolio optimization in one line
weights(optimal.portfolio(scenario.set))
```

### Piping using `magrittr`

Furthermore, everything should be pipeable and such is the design of the package, i.e.

```
# The above initial portfolio optimization can be piped as follows 
scenario.set %>% 
  optimal.portfolio %>% 
  weights

# Of course, this is interesting if you change lots of parameters and keeps your
# portfolio models readable and well-shaped for communication
scenario.set %>% 
  portfolio.model %>% 
  objective("expected.shortfall") %>% 
  alpha(0.1) %>% 
  upper.bound(0.2) %>%
  optimal.portfolio %>% 
  weights
```

## Further examples

There are some tutorials built into the package, which you may e.g. open with the following commands:

```
file.edit(po.tutorial("101"))
file.edit(po.tutorial("compare"))
```
