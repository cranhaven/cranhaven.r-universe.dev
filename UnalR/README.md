# UnalR <a href='https://estadisticaun.github.io/UnalR/'><img src='man/figures/Logo.png' align="right" height="134" /></a>

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-ago/UnalR)](https://cran.r-project.org/package=UnalR)
[![CRAN RStudio Mirror Downloads](https://cranlogs.r-pkg.org/badges/UnalR?color=green)](https://www.r-pkg.org/pkg/UnalR)
[![CRAN RStudio Mirror Downloads](https://cranlogs.r-pkg.org/badges/grand-total/UnalR)](https://www.r-pkg.org/pkg/UnalR)
[![R-CMD-check](https://github.com/estadisticaun/UnalR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/estadisticaun/UnalR/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/estadisticaun/UnalR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/estadisticaun/UnalR/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/estadisticaun/UnalR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/estadisticaun/UnalR?branch=master)
[![RStudio Community](https://img.shields.io/badge/community-UnalR-blue?style=social&logo=rstudio&logoColor=75AADB)](https://forum.posit.co/new-topic?title=&tags=UnalR)
<!-- badges: end -->

El paquete `UnalR` proporciona métodos y herramientas para la gestión y disposición de estadísticas institucionales. Su objetivo principal es disponer, facilitar y optimizar la disposición de microdatos y la visualización de las cifras y estadísticas oficiales de la [Universidad Nacional de Colombia](https://unal.edu.co) las cuales se encuentran disponibles, dentro del [sistema de planeación](https://planeacion.unal.edu.co/home/) institucional, en el componente de [estadísticas oficiales](https://estadisticas.unal.edu.co/home/). Contiene una biblioteca de funciones gráficas, tanto estáticas como interactivas, que ofrece numerosos tipos de gráficos con una sintaxis altamente configurable y simple. Entre estos, encontramos la visualización de tablas `HTML`, series, gráficos de barras y circulares, mapas, boxplots, radar charts, treemaps, drilldown, etc. Todo lo anterior apoyado en bibliotecas de `JavaScript`.

El paquete `UnalR` permanecerá en su estado actual y se mantendrá solo con correcciones de errores.

## Instalación

Puede instalar la versión publicada de `UnalR` (*versión menor*) desde [GitHub](https://github.com/) con:

``` r
# Versión oficial (estable)
install.packages("UnalR")
# Última versión (dev)
devtools::install_github("estadisticaun/UnalR")
remotes::install_github("estadisticaun/UnalR")
```

## Uso

Es difícil describir de manera sucinta cómo funciona `UnalR` porque encarna una profunda filosofía de visualización que abarca las principales librerías para representar de forma dinámica e interactiva datos usando `JavaScript` (**htmlwidgets**).

## Ejemplo

``` r
library(UnalR)
example(topic = Plot.Apiladas, package = "UnalR")
```
![](man/figures/ExampleApiladas1.png)
![](man/figures/ExampleApiladas2.png)

``` r
example(topic = Plot.Barras, package = "UnalR")
```
![](man/figures/ExampleBarras1.png)
![](man/figures/ExampleBarras2.png)

``` r
example(topic = Plot.Boxplot, package = "UnalR")
```
![](man/figures/ExampleBoxplot1.png)
![](man/figures/ExampleBoxplot2.png)
![](man/figures/ExampleBoxplot3.png)

``` r
example(topic = PPlot.Histograma, package = "UnalR")
```
![](man/figures/ExampleHistograma1.jpeg)
![](man/figures/ExampleHistograma2.jpeg)

``` r
example(topic = Plot.Mapa, package = "UnalR")
```
![](man/figures/ExampleMapa1.png)
![](man/figures/ExampleMapa2.png)

``` r
example(topic = Plot.Mundo, package = "UnalR")
```
![](man/figures/ExampleMundo1.jpeg)
![](man/figures/ExampleMundo2.jpeg)
![](man/figures/ExampleMundo3.jpeg)

``` r
example(topic = Plot.Series, package = "UnalR")
```
![](man/figures/ExampleSeries1.png)
![](man/figures/ExampleSeries2.png)

``` r
example(topic = Tabla, package = "UnalR")
```
![](man/figures/ExampleTabla1.png)

``` r
example(topic = Plot.Treemap, package = "UnalR")
```
![](man/figures/ExampleTreemap1.PNG)
![](man/figures/ExampleTreemap2.PNG)
![](man/figures/ExampleTreemap3.PNG)
![](man/figures/ExampleTreemap4.PNG)

## Licencia

Este paquete es un software gratuito y de código abierto, con licencia GPL-3.

## Ayuda

Si requiere ayuda para usar `UnalR`:

  * Para problemas de instalación, comuníquese a los correos electrónicos proporcionados en la documentación del paquete, normalmente respondemos con prontitud y usted también ayudará a futuros usuarios.

Si cree que ha encontrado un error:

  * Instale la versión de desarrollo de `UnalR` usando `devtools` (*ver arriba*) y vea si eso ayuda.
  * Consulte los [problemas de github](https://github.com/estadisticaun).

¡Gracias por tu interés en `UnalR`!
