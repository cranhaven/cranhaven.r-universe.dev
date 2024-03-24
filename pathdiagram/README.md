# `"pathdiagram"`

`pathdiagram` provides simple functions to draw basic PLS path diagrams in [R](https://www.r-project.org/), just for illustrating purposes (i.e. adding diagrams when writing reports with `knitr` or `Sweave`).


## Motivation

The one and only reason to create `pathdiagram` was the need to plot a <a href="http://en.wikipedia.org/wiki/Path_analysis_(statistics)" target="_blank">path diagram</a> in R. Since I was writing the tutorials and demos for the package `plspm` I realized that I badly needed path diagrams accompanying my examples. I first tried using the `grid` package. It was fine but it didn't allow me to have complete control over the looking of the objects (mainly with color of lines, borders, text, etc). Then I tried to use the package `diagram` but I got the same restrictions. Finally, after some short experiments, I decided to create `pathdiagram`.  


## Installation

Stable version on [CRAN](https://cran.r-project.org/package=pathdiagram)
```ruby
# stable version 
install.packages("pathdiagram")
```

Development version on [github](https://github.com/gastonstat/pathdiagram)
```ruby
# development version 
library(devtools)
install_github('pathdiagram',  username='gastonstat')
```

## Some Examples

```ruby
# graphic specifications of manifest variables
ingredients = list(
  eggs = manifest("Eggs", x = 0.25, y = 0.8, width = 0.1, height = 0.08),
  milk = manifest("Milk", x = 0.25, y = 0.65, width = 0.1, height = 0.08),
  flour = manifest("Flour", x = 0.25, y = 0.5, width = 0.1, height = 0.08),
  sugar = manifest("Sugar", x = 0.25, y = 0.35, width = 0.1, height = 0.08),
  butter = manifest("Butter", x = 0.25, y = 0.2, width = 0.1, height = 0.08)
)

# graphic specifications of latent variables
pancakes = latent("Pancakes", x = 0.8, y = 0.65, rx = 0.08, ry = 0.06)
waffles = latent("Waffles", x = 0.8, y = 0.35, rx = 0.08, ry = 0.06)


# open a new wall
wall()

# draw latent variables
draw(pancakes)
draw(waffles)

# draw ingredients
for (i in 1:5) {
  draw(ingredients[[i]])
}

# add arrows
for (i in 1:5) {
  arrow(from = ingredients[[i]], to = pancakes, start = "east", end = "west")
  arrow(from = ingredients[[i]], to = waffles, start = "east", end = "west")
}
```

## Author Contact 

[www.gastonsanchez.com](http://www.gastonsanchez.com)

Gaston Sanchez (`gaston.stat at gmail.com`)

