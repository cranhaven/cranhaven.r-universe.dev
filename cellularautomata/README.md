An R package for creating cellular automata.

```{r}
library(cellularautomata)
```

## Create the plot of a cellular automaton

You can generate a cellular automaton using the `ca` function, specifying the Wolfram rule. For example:

```{r}
ca(18) |> plot()
ca(30) |> plot()
ca(45) |> plot()
ca(195) |> plot()
```

![Rule 18](vignettes/figs/rule18.png)

![Rule 30](vignettes/figs/rule30.png)

![Rule 45](vignettes/figs/rule45.png)

![Rule 195](vignettes/figs/rule195.png)

## Animations

You can get an animation of a cellular automaton using `plot(animate = TRUE)`:

```{r}
ca(30, ncols = 20, steps = 30) |> plot(animate = TRUE)
```

![Rule 30](vignettes/figs/rule30.gif)

## Polar coordinates

By default the line is wrapped, meaning it is actually a circle, with the end connected to the beginning.

You can plot it using polar coordinates:

```{r}
ca(193, steps = 50) |> plot(time_flow = "up", circle = TRUE)
```

![Rule 193, polar coordinates](vignettes/figs/rule193-polar.png)

This also works for animations:

```{r}
ca(193, ncols = 25, steps = 100) |> plot(circle = TRUE, animate = TRUE)
```

![Rule 193, polar coordinates](vignettes/figs/rule193-polar.gif)

## Acknowledgements

Original function created by [Nicola Procopio](https://github.com/nickprock/automata2graph).
