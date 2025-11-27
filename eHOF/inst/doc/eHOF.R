## ----prep, echo=FALSE, results='hide'-----------------------------------------
suppressPackageStartupMessages(library(vegdata))
tmp <- tempdir()
options(tv_home = tmp)
dir.create(tmp)
dir.create(file.path(tmp, 'Species'))
dir.create(file.path(tmp, 'Popup'))
dir.create(file.path(tmp, 'Data'))
file.copy(from = file.path(path.package("vegdata"), 'tvdata', 'Popup'), to = tmp, recursive = TRUE)
file.copy(from = file.path(path.package("vegdata"), 'tvdata', 'Species'), to = tmp, recursive = TRUE)
file.copy(from = file.path(path.package("vegdata"), 'tvdata', 'Data'), to = tmp, recursive = TRUE)

## ----load, messages=FALSE-----------------------------------------------------
library(eHOF)

## ----4site.echo, results='hide', message=FALSE--------------------------------
library(vegdata)
db <- 'elbaue'
site <- tv.site(db)
veg <- tv.veg(db, taxval = FALSE, spcnames = 'Numbers')
obs <- tv.obs(db)
# taxa <- tax(unique(obs$TaxonUsageID), verbose=TRUE)
taxa <- tax('all')
names(veg) <- sub('.0', '', names(veg), fixed=TRUE)
names(veg) <- taxa$LETTERCODE[match(names(veg), taxa$TaxonUsageID)]

## ----5veg.2, results='hide'---------------------------------------------------
veg.sqrt <- tv.veg(db, cover.transform='sqrt', tax=FALSE, spcnames='Numbers')
names(veg.sqrt) <- sub('.0', '', names(veg.sqrt), fixed=TRUE)
names(veg.sqrt) <- taxa$LETTERCODE[match(names(veg.sqrt), taxa$TaxonUsageID)]

## ----5veg.3, results='hide'---------------------------------------------------
veg.pa <- tv.veg(db, cover.transform='pa', tax=FALSE, spcnames='Numbers')
names(veg.pa) <- sub('.0', '', names(veg.pa), fixed=TRUE)
names(veg.pa) <- taxa$LETTERCODE[match(names(veg.pa), taxa$TaxonUsageID)]

## ----modeltypes, warning=FALSE, results='hide'--------------------------------
data(acre)
sel <- c('ELYMREP', 'VEROPES', 'CONSREG', 'DESUSOP', 'VEROARV', 'ARTE#VU', 'ACHIMIL')
mo <- HOF(acre[match(sel, names(acre))], acre.env$PH_KCL, M=1, bootstrap=NULL)
par(mar=c(4,4,1,1)+.1)
autolayout(7)
par(mar=c(4,4,1,1)+.1)
for(i in 1:7) plot(mo[[i]], model = eHOF.modelnames[i], marginal ='n')

## ----percentageVEG, results='hide', warning=FALSE-----------------------------
mods <- HOF(veg, site$MGL, M=100, family = poisson, bootstrap = NULL)

## ----printPerc----------------------------------------------------------------
mods

## ----7sqrt, results='hide', warning=FALSE-------------------------------------
mods.sq <- HOF(veg.sqrt, site$MGL, M=10, family= poisson, freq.limit=10, bootstrap=NULL)
plot(mods.sq)

## ----7pres-abs, results='hide', warning=FALSE---------------------------------
mods.pa <- HOF(veg.pa, site$MGL, M=1, bootstrap=NULL)
plot(mods.pa)

## ----RanRep-------------------------------------------------------------------
Rrep <- taxa$LETTERCODE[taxa$TaxonName == 'Ranunculus repens']
Para(mods.pa[[Rrep]])

## ----paraplot, eval = TRUE, warning=FALSE, out.width='.5\\textwidth'----------
plot(mods.pa[[Rrep]], para=TRUE, onlybest=FALSE)

## ----sqrt-2, results='hide', warning=FALSE, out.width='.5\\textwidth'---------
mod.sqrt <- eHOF:::HOF.default(veg.sqrt[[Rrep]], site$MGL, M=10, family=poisson, bootstrap = 10)
plot(mod.sqrt, marginal='point', para=TRUE, onlybest=FALSE, newdata=seq(min(mod.sqrt$range), max(mod.sqrt$range), length.out=10000) )
Para(mod.sqrt)

