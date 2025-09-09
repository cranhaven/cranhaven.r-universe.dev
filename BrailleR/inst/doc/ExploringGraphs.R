## ---- setupLibrarys, message = FALSE, warnings = FALSE------------------------
library(ggplot2)
library(BrailleR)

## ----Basic plot Describe------------------------------------------------------
basicPlot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point()

Describe(basicPlot)

## ----VI implicit call---------------------------------------------------------
basicPlot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point()

basicPlot

## ----VI explicit call---------------------------------------------------------
VI(basicPlot)

## ----VI geomHLine, fig.show='hide'--------------------------------------------
hline <- ggplot(mtcars, aes(mpg, cyl)) +
  geom_hline(yintercept = 5)
hline

## ----VI geomPoint, fig.show='hide'--------------------------------------------
point <- ggplot(mtcars, aes(mpg, cyl)) +
  geom_point()
point

## ----VI geomBar, fig.show='hide'----------------------------------------------
bar <- ggplot(mtcars, aes(mpg)) +
  geom_histogram()
bar

## ----VI geomLine, fig.show='hide'---------------------------------------------
line <- ggplot(mtcars, aes(mpg, wt)) +
  geom_line()
line

## ----VI geomBoxplot, fig.show='hide'------------------------------------------
boxplot <- ggplot(mtcars, aes(mpg, as.factor(cyl))) +
  geom_boxplot()
boxplot

## ----VI geomSmooth, fig.show='hide'-------------------------------------------
smooth <- ggplot(mtcars, aes(mpg, wt)) +
  geom_smooth()
smooth

## ----VI geomRibbon, fig.show='hide'-------------------------------------------
ribbon <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_area(aes(y = price))
ribbon

## ----VI geomBlank, fig.show='hide'--------------------------------------------
blank <- ggplot(BOD, aes(x = demand, y = Time)) +
  geom_line() +
  expand_limits(x = c(15, 23, 6), y = c(30))
blank

## ----svg geom line, eval = FALSE, echo=FALSE----------------------------------
#  line <- ggplot(mtcars, aes(mpg, wt)) +
#    geom_line()
#  MakeAccessibleSVG(line)

## ----svg geom point, eval = FALSE, echo=FALSE---------------------------------
#  point <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#    geom_point()
#  MakeAccessibleSVG(point)

## ----svg geom bar, eval = FALSE, echo=FALSE-----------------------------------
#  bar <- ggplot(iris, aes(Sepal.Length)) +
#    geom_bar()
#  MakeAccessibleSVG(bar)

## ---- svg geom smooth, eval = FALSE, echo=FALSE-------------------------------
#  smooth <- ggplot(mtcars, aes(mpg, wt)) +
#    geom_smooth()
#  MakeAccessibleSVG(smooth)

