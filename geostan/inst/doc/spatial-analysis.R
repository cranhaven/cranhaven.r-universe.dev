## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = 'center')
library(spData)

## ----echo = FALSE-------------------------------------------------------------
# function for getting colors, breaks, and labels for mapping
map_pars <- function(x, 
                     brks = quantile(x, probs = seq(0, 1, by = 0.2), na.rm = TRUE), 
                     cols = c("#A8554EFF", "gray95", "#5D74A5FF")) {

  # put x values into bins
  x_cut <- cut(x, breaks = brks, include.lowest = TRUE)
  
  # labels for each bin
  lbls <- levels( cut(x, brks, include.lowest = TRUE) )
  
  # colors 
  rank <- as.numeric( x_cut )  
  max_rank <- max( rank , na.rm = TRUE )
  pal_fun <- colorRampPalette( cols )
  pal <- pal_fun( max_rank )
  colors <-  pal[ rank ]

  # return list
  ls <- list(brks = brks, lbls = lbls, pal = pal, col = colors)
  return( ls )
}

## ----eval = FALSE-------------------------------------------------------------
#  if (!require('devtools')) install.packages('devtools')
#  devtools::install_github("connordonegan/geostan")

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("geostan")

## ----message = FALSE----------------------------------------------------------
library(geostan)
library(sf)
data(world, package = "spData")

## -----------------------------------------------------------------------------
world <- st_transform(world, crs = 'ESRI:54030')

## -----------------------------------------------------------------------------
## https://data.worldbank.org
france <- grep("France", world$name_long)
world$gdpPercap[ france ] <- 43068
world$lifeExp[ france ] <- 82

norway <- grep("Norway", world$name_long)
world$gdpPercap[ norway ] <- 97666
world$lifeExp[ norway ] <- 82.1

## -----------------------------------------------------------------------------
world <- subset(world, name_long != "Antarctica")

## ----fig.width = 7, fig.height = 5, fig.cap = "*Choropleth maps of GDP per capita and life expectancy.*"----
# store geometry for countries
world_geom <- st_geometry(world)

# show two maps at once, with nice font
ogpar <- par(mfrow = c(2, 1),
             mar = rep(0, 4))

# GDP per capita
pars <- map_pars(world$gdpPercap / 1e3)
plot(world_geom,
     col = pars$col,
     lwd = .2)
legend("bottomleft",
       fill = pars$pal,
       title = 'GDP per capita\n($1,000s)',
       legend = pars$lbls,
       bty = 'n'
)
rm(pars)

# life expectancy
pars <- map_pars(world$lifeExp)
plot(world_geom,
  col = pars$col,
  lwd = .2)
legend("left",
     fill = pars$pal,
     title = 'Life Expectancy',
     legend = pars$lbls,
     bty = 'n'
     )
par(ogpar)

## -----------------------------------------------------------------------------
log_x <- log10( world$gdpPercap )
y <- world$lifeExp
cor.test(log_x, y)

## -----------------------------------------------------------------------------
## remove missing values
world <- subset(world, !is.na(gdpPercap) & !is.na(lifeExp))

## leaving 162 observations
nrow(world)

## ----fig.width = 5------------------------------------------------------------
A <- shape2mat(world, "B", method = "rook")

## ----fig.height = 3.5---------------------------------------------------------
# edges with geometry
E <- edges(A, shape = world) 
graph <- st_geometry(E)

ogpar <- par(mar = rep(0, 4))
# plot countries
plot(world_geom, lwd = .1)
# add graph nodes
plot(graph, add = TRUE, type = 'p')
# add graph edges
plot(graph, add = TRUE, type = 'l')
par(ogpar)

## ----eval = FALSE-------------------------------------------------------------
#  moz_idx <- grep("Mozambique", world$name_long)
#  mad_idx <- grep("Madagascar", world$name_long)

## ----eval = FALSE-------------------------------------------------------------
#  A[moz_idx, mad_idx] <- A[mad_idx, moz_idx] <- TRUE

## -----------------------------------------------------------------------------
connect <- function(country_a, country_b,
	names_vec = world$name_long, matrix = A, add = TRUE) {
  stopifnot( country_a %in% names_vec )
  stopifnot( country_b %in% names_vec )
  a_idx <- which(names_vec == country_a)
  b_idx <- which( names_vec == country_b)
  matrix[a_idx, b_idx] <- matrix[b_idx, a_idx] <- add
  return( matrix )
}

## -----------------------------------------------------------------------------
A <- connect("Mozambique", "Madagascar")
A <- connect("Australia", "New Zealand")
A <- connect("Philippines", "Malaysia")
A <- connect("Japan", "Republic of Korea")
A <- connect("Fiji", "Vanuatu")
A <- connect("Solomon Islands", "Vanuatu")
A <- connect("Solomon Islands", "Papua New Guinea")
A <- connect("Australia", "Papua New Guinea")
A <- connect("Haiti", "Jamaica")
A <- connect("Bahamas", "United States")
A <- connect("Dominican Republic", "Puerto Rico")
A <- connect("Trinidad and Tobago", "Venezuela")
A <- connect("Sri Lanka", "India")
A <- connect("Cyprus", "Turkey")
A <- connect("Cyprus", "Lebanon")
A <- connect("Norway", "Iceland")

## remove connections between South American and France
A <- connect("Suriname", "France", add = FALSE)
A <- connect("Brazil", "France", add = FALSE)

## ----fig.width = 5, fig.height = 3.5------------------------------------------
graph <- st_geometry( edges(A, shape = world) )
ogpar <- par(mar = rep(0, 4))
plot(world_geom, lwd = .1)
plot(graph, add = TRUE, type = 'p')
plot(graph, add = TRUE, type = 'l')
par(ogpar)

## ----eval = FALSE-------------------------------------------------------------
#  E <- edges(A, shape = world)
#  st_write(E, "world.gpkg", layer = "edge list")

## -----------------------------------------------------------------------------
fit_lm <- stan_glm(lifeExp ~ log(gdpPercap), data = world, iter = 800, quiet = TRUE)

## -----------------------------------------------------------------------------
print(fit_lm)

## ----fig.width = 7, fig.height = 3--------------------------------------------
plot(fit_lm)

## -----------------------------------------------------------------------------
fdf <- fitted(fit_lm)
head(fdf)

## ----fig.width = 4.5, fig.height = 4, fig.align = 'center'--------------------
rdf <- resid(fit_lm)
moran_plot(rdf$mean, A)

## -----------------------------------------------------------------------------
cars <- prep_car_data(A, quiet = TRUE)
fit_car <- stan_car(lifeExp ~ 1, data = world, car_parts = cars, iter = 800, quiet = TRUE)

## -----------------------------------------------------------------------------
print(fit_car)

## -----------------------------------------------------------------------------
theta <- spatial(fit_car)$mean

## -----------------------------------------------------------------------------
pars <- map_pars(theta)
ogpar <- par(mar = rep(0, 4))
plot(st_geometry(world),
  col = pars$col,
  lwd = .2)
legend("left",
     fill = pars$pal,
     title = 'Spatial trend (LE)',
     legend = pars$lbls,
     bty = 'n'
     )
par(ogpar)

## -----------------------------------------------------------------------------
# lifeExpt detrended
dy <- resid(fit_car)$mean

# log per capita GDP detrended
fit_carx <- stan_car(log(gdpPercap) ~ 1, data = world, car = cars, iter = 1e3, quiet = TRUE)
dx <- resid(fit_carx)$mean

## -----------------------------------------------------------------------------
# adjusted correlation
cor.test(dx, dy)

## ----message = FALSE, warning = FALSE-----------------------------------------
W <- row_standardize(A)
sars <- prep_sar_data(W)

## -----------------------------------------------------------------------------
fit_sar <- stan_sar(lifeExp ~ log(gdpPercap), data = world, sar_parts = sars, centerx = TRUE,
                    iter = 800, quiet = TRUE)

## -----------------------------------------------------------------------------
plot(fit_sar)

## -----------------------------------------------------------------------------
world <- transform(world,
                  sx = scale(log(gdpPercap), scale = T, center = T),
                  sy = scale(lifeExp, scale = T, center = T)
                   )

fit_scaled <- stan_sar(sy ~ sx, data = world, sar_parts = sars, iter = 800, quiet = TRUE)
print(fit_scaled)

## ----fig.width = 5, fig.height = 5--------------------------------------------
gdp <- range(world$gdpPercap)
min_gdp <- gdp[1]
max_gdp <- gdp[2]
pdf <- data.frame(gdpPercap = seq(min_gdp, max_gdp, length.out = 200))

## -----------------------------------------------------------------------------
preds <- predict(fit_sar, newdata = pdf)

## -----------------------------------------------------------------------------
head(preds)

## ----fig.width = 4.5, fig.height = 4, fig.align = 'center'--------------------
# scale GDP
preds <- transform(preds, gdpPercap = gdpPercap / 1e3)

## yrange <- c(min(preds$`2.5%`), max(preds$`97.5%`))
yrange <- c(57, 85)

plot(preds$gdpPercap, preds$mean,
     t = 'l',
     ylim = yrange,
     axes = F, 
     xlab = "GDP per capita ($1,000s)",
     ylab = "Life expectancy")
axis(1)
axis(2)

# add credible intervals
lines(preds$gdpPercap, preds$`2.5%`, lty = 3)
lines(preds$gdpPercap, preds$`97.5%`, lty = 3)

# show actual gdp values
rug(world$gdpPercap / 1e3, lwd = 0.25)

## ----echo = TRUE--------------------------------------------------------------
# function for getting colors, breaks, and labels for mapping
map_pars <- function(x, 
                     brks = quantile(x, probs = seq(0, 1, by = 0.2), na.rm = TRUE), 
                     cols = c("#A8554EFF", "gray95", "#5D74A5FF")) {

  # put x values into bins
  x_cut <- cut(x, breaks = brks, include.lowest = TRUE)
  
  # labels for each bin
  lbls <- levels( cut(x, brks, include.lowest = TRUE) )
  
  # colors 
  rank <- as.numeric( x_cut )  
  max_rank <- max( rank , na.rm = TRUE )
  pal_fun <- colorRampPalette( cols )
  pal <- pal_fun( max_rank )
  colors <-  pal[ rank ]

  # return list
  ls <- list(brks = brks, lbls = lbls, pal = pal, col = colors)
  return( ls )
}

