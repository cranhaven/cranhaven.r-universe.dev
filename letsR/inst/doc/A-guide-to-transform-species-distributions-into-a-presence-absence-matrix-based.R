## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.height = 5, fig.width = 4, fig.align = 'center')

## ---- message = FALSE, warning = FALSE----------------------------------------
library(letsR)

data("Phyllomedusa")

## ---- fig.height=5, fig.width=4, fig.align='center'---------------------------
# Plot
## Color settings and assignment
colors <- rainbow(length(unique(Phyllomedusa$binomial)),
                  alpha = 0.5)
position <- match(Phyllomedusa$binomial,
                  unique(Phyllomedusa$binomial))
colors <- colors[position]
## Plot call
plot(sf::st_geometry(Phyllomedusa), col = colors, lty = 0,
     main = "Spatial polygons of Phyllomedusa")
data("wrld_simpl")
plot(sf::st_geometry(wrld_simpl), add = TRUE)

## -----------------------------------------------------------------------------
PAM <- lets.presab(Phyllomedusa, xmn = -93, xmx = -29,
                   ymn = -57, ymx = 15, res = 1)

## -----------------------------------------------------------------------------
summary(PAM)

## -----------------------------------------------------------------------------
plot(PAM)

## -----------------------------------------------------------------------------
plot(PAM, name = "Phyllomedusa hypochondrialis")

## -----------------------------------------------------------------------------
presab <- PAM$Presence_and_Absence_Matrix

## -----------------------------------------------------------------------------
# Print only the first 5 rows and 3 columns
presab[1:5, 1:3]

## ---- message=FALSE, warning=FALSE--------------------------------------------
pro <- paste("+proj=eqdc +lat_0=-32 +lon_0=-60 +lat_1=-5",
             "+lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA", 
             "+units=m +no_defs")
SA_EC <- terra::crs(pro)
PAM_proj <- lets.presab(Phyllomedusa, xmn = -4135157,
                        xmx = 4707602,
                        ymn = -450000, ymx = 5774733,
                        res = 100000,
                        crs.grid = SA_EC)

## -----------------------------------------------------------------------------
summary(PAM_proj)

## ---- message=FALSE, warning=FALSE--------------------------------------------
plot(PAM_proj)
# Add projected country boundaries
data("wrld_simpl")
plot(sf::st_transform(sf::st_geometry(wrld_simpl), pro), add = TRUE)

## -----------------------------------------------------------------------------
# 90% cover
PAM_90 <- lets.presab(Phyllomedusa, xmn = -93,
                      xmx = -29, ymn = -57,
                      ymx = 15, res = 1,
                      cover = 0.9)
plot(PAM_90)

## -----------------------------------------------------------------------------
PAM_keep_cells <- lets.presab(Phyllomedusa, xmn = -93,
                              xmx = -29, ymn = -57,
                              ymx = 15, res = 1,
                              remove.cells = FALSE)

## -----------------------------------------------------------------------------
summary(PAM_keep_cells)

## -----------------------------------------------------------------------------
species <- c(rep("sp1", 100), rep("sp2", 100),
             rep("sp3", 100), rep("sp4", 100))
x <- runif(400, min = -69, max = -51)
y <- runif(400, min = -23, max = -4)
xy <- cbind(x, y)

## -----------------------------------------------------------------------------
PAM_points <- lets.presab.points(xy, species, xmn = -93, xmx = -29,
                          ymn = -57, ymx = 15)
plot(PAM_points)

## -----------------------------------------------------------------------------
# Grid 
sp.r <- terra::as.polygons(terra::rast(xmin = -93, xmax = -29,
                                ymin = -57, ymax = 15,
                                resolution = 5))
# Give an ID to the cell
sp.r$ID <- 1:length(sp.r)
plot(sp.r, border = rgb(.5, .5, .5))
plot(sf::st_geometry(wrld_simpl[1]), add = T, fill = F)

## -----------------------------------------------------------------------------
resu <- lets.presab.grid(Phyllomedusa, sp.r, "ID")

## -----------------------------------------------------------------------------
rich_plus1 <- rowSums(resu$PAM[, -1]) + 1
colfunc <- colorRampPalette(c("#fff5f0", "#fb6a4a", "#67000d"))
colors <- c("white", colfunc(max(rich_plus1)))
plot(resu$grid, border = "gray40",
     col = colors[rich_plus1])
plot(sf::st_geometry(wrld_simpl), add = TRUE)

