### R code from vignette source 'semtable.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: semtable.Rnw:25-26
###################################################
  if(exists(".orig.enc")) options(encoding = .orig.enc)


###################################################
### code chunk number 2: texcopy
###################################################
if(!dir.exists("theme")) dir.create("theme")
library(stationery)
## If theme directory does not have required images or TeX files
## we need to retrieve them and put them in "theme" directory. 
texfiles <- c("guidePreambleHeader.tex", "guidePreambleSweavel.tex", "preambleFooter.tex")
getFiles(texfiles, pkg = "stationery")
logos <- c("logoleft.pdf", "logo-vert.pdf")
## This will retrieve logo files from a designated package:
try(getFiles(logos, pkg = "crmda", overwrite = FALSE))
## If you do not have a file after that, 
## the following will manufacture a blank images for placeholders
if(!file.exists("theme/logoleft.pdf")){
  blankPDF(file = "theme/logoleft.pdf", height=1.2, width=1.2, messg = "")
}
if(!file.exists("theme/logo-vert.pdf")){
  blankPDF(file = "theme/logo-vert.pdf", height=2, width=1.5, messg = "")
}


###################################################
### code chunk number 3: ignoremeRsetup
###################################################
tdir <- "tmpout"
if(!dir.exists(tdir)) dir.create(tdir, showWarnings=FALSE)
opts.orig <- options()
options(width=80, prompt=" ", continue="  ")
options(useFancyQuotes = FALSE) 
set.seed(12345)
par.orig <- par(no.readonly=TRUE)
options(SweaveHooks=list(fig=function() par(ps=10)))
pdf.options(onefile=FALSE,family="Times",pointsize=10)


###################################################
### code chunk number 4: ignoremeRsetup2
###################################################
require(lavaan)
library(semTable)
tempdir <- "tmpout"
if(!dir.exists(tempdir)) dir.create(tempdir)


###################################################
### code chunk number 5: semtable.Rnw:250-255
###################################################
## The example from lavaan's docs
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
'


###################################################
### code chunk number 6: semtable.Rnw:262-269
###################################################
if(file.exists("fit1.rds")){
	fit1 <- readRDS("fit1.rds")
} else {
    fit1 <- cfa(HS.model, data = HolzingerSwineford1939,
            std.lv = TRUE, meanstructure = TRUE)
    saveRDS(fit1, "fit1.rds")
}


###################################################
### code chunk number 7: semtable.Rnw:275-276
###################################################
summary(fit1)


###################################################
### code chunk number 8: semtable.Rnw:290-293
###################################################
vlabs <- c("x1" = "Vis 1", "x2" = "Vis 2", x3 = "Vis 3", x4 = "Txt 1", x5 = "Txt 2", x6 = "Txt 3", x7 = "Speed 1", x8 = "Speed 2", x9 = "Speed 3")

fit1.t1 <- semTable(fit1, columns = c("estse", "p"), paramSets = c("loadings", "intercepts",  "residualvariances", "latentcovariances"), fits = c("chisq", "rmsea"), file = file.path(tempdir, "fit1.t1"), varLabels = vlabs, type = "latex",  table.float = TRUE, caption = "Holzinger Swineford CFA in a longtable Float", label = "tab:HS10", longtable=TRUE)


###################################################
### code chunk number 9: semtable.Rnw:305-306 (eval = FALSE)
###################################################
## cat(fit1.t1)


###################################################
### code chunk number 10: semtable.Rnw:311-313 (eval = FALSE)
###################################################
## fn <- file.path(tempdir, "fit1.t12.tex")
## cat(fit1.t1, file = fn)


###################################################
### code chunk number 11: HS10
###################################################
fit1.t1h <- semTable(fit1, columns = c("estse", "p"), paramSets = c("loadings", "intercepts",  "residualvariances", "latentcovariances"),
                    fits = c("chisq", "rmsea"), file = file.path(tempdir, "fit1.t1h.html"),
                    varLabels = vlabs, type = "html", print.results = FALSE)


###################################################
### code chunk number 12: semtable.Rnw:337-338 (eval = FALSE)
###################################################
## browseURL(file.path(tempdir, "fit1.t1.html"))


###################################################
### code chunk number 13: semtable.Rnw:341-347
###################################################
## Try CSV output next
fit1.t1c <- semTable(fit1, columns = c("estse", "p"),
                    fits = c("chisq", "rmsea"), file = file.path(tempdir, "fit1.t1c"),
                    varLabels = vlabs, type = "csv", print.results = FALSE)
## Go inspect this file with a spread sheet program:
attr(fit1.t1c, "file")


###################################################
### code chunk number 14: semtable.Rnw:354-355
###################################################
fit1.t1c2 <- markupConvert(attr(fit1.t1, "markedResults"), type = "csv")


###################################################
### code chunk number 15: fit1.t3
###################################################
## floating table
fit1.t3 <- semTable(fit1, columns = c("est", "se", "p"), paramSets = c("loadings"), fits = c("chisq", "rmsea"), file = file.path(tempdir, "fit1.t3"),                    varLabels = vlabs, longtable=FALSE, table.float = TRUE, caption = "Table Floated (not a longtable)", label = "tab:fit1.t3")


###################################################
### code chunk number 16: fit1.t4
###################################################
## floating longtable
fit1.t4 <- semTable(fit1, columns = c("est", "estsestars"), paramSets = c("loadings"), fits = c("chisq", "rmsea"), file = file.path(tempdir, "fit1.t4"), varLabels = vlabs, longtable = TRUE, table.float=TRUE, caption = "Table Floated (longtable)", label = "tab:fit1.t4")


###################################################
### code chunk number 17: semtable.Rnw:405-407
###################################################
##columnLabels
fit1.t5 <- semTable(fit1, fits = c("chisq", "rmsea"), paramSets = c("loadings"),  columns = c("est", "se", "p"), columnLabels = c(se = "S.E."), file = file.path(tempdir, "fit1.t5"), print.results = FALSE)


###################################################
### code chunk number 18: labl100
###################################################
##columnLabels
fit1.t6 <- semTable(list("A Fancy Fitted Model" = fit1), fits = c("chisq", "rmsea"), paramSets = c("loadings"), paramSetLabels = c("loadings" = "Loading Estimates(ML robust)"), columns = c("estsestars"), columnLabels = c("estsestars" = "Estimates(Std.Errors)"), file = file.path(tempdir, "fit1.t6"), table.float=TRUE, caption="Demonstrate Flexibility with Column and Parameter Set Labels", label = "tab:fit1.t6")


###################################################
### code chunk number 19: labl110
###################################################
## Test alternative latent variable labels
vl <- c(vlabs, visual = "Seeing", textual = "Thumb Texting", speed = "Speed")
fit1.t7 <- semTable(fit1, fits = c("chisq", "rmsea"), paramSets = c("loadings", "intercepts"),  columns = c("eststars", "p"), columnLabels = c("eststars" = "Est(SE)"), file = file.path(tempdir, "fit1.t7"), varLabels = vl, longtable = TRUE, type = "latex", table.float=TRUE, caption="Variable Labels can include parameter sections", label = "tab:fit1.t7")


###################################################
### code chunk number 20: labl30
###################################################
fit4.t2 <- semTable(fit1, paramSets = c("loadings"), fits = c("rmsea", "cfi", "chisq"), fitLabels = c(rmsea = "Root M.SQ.E.A", cfi = "CompFitIdx", chisq = "chisq"), type = "latex", table.float = TRUE, caption = "Customized Fits and Labels", label = "tab:fit1.t8")


###################################################
### code chunk number 21: grps200
###################################################
if(file.exists("fit1.g.rds")){
	fit1.g <- readRDS("fit1.g.rds")
} else {
    fit1.g <- cfa(HS.model, data = HolzingerSwineford1939, std.lv = TRUE, group = "school", estimator = "MLR")
    saveRDS(fit1.g, "fit1.g.rds")
}


###################################################
### code chunk number 22: grps210
###################################################
## 2 groups table
fit1.gt1 <- semTable(fit1.g, columns = c("estsestars", "p"), columnLabels = c(estsestars = "Est(Std.Err.)", p = "p-value"), file = file.path(tempdir, "fit1.gt1"), table.float = TRUE, caption = "A Two Group Model", label = "tab:fit1.gt1", longtable=TRUE)


###################################################
### code chunk number 23: grps220
###################################################
## Now name particular group by name
fit1.gt2 <- semTable(fit1.g, columns = c("estsestars", "p"), paramSets = c("loadings", "intercepts", "residualvariances"), columnLabels = c(estsestars = "Est w/stars", p = "p-value"), file = file.path(tempdir, "fit1.gt2"), groups = "Pasteur", table.float = TRUE, caption = "Group 'Pasteur' Group from the 2 Model", label = "tab:fit1.gt2")


###################################################
### code chunk number 24: grps220
###################################################
## Name particular group by number
fit1.gt3 <- semTable(fit1.g, columns = c("estsestars", "p"), paramSets = c("loadings"), columnLabels = c(estsestars = "Est w/stars", p = "p-value"), file = file.path(tempdir, "fit1.gt3"), groups = 2, table.float = TRUE, caption = "Group '2' from the 2 Model is 'Grant-White'", label = "tab:fit1.gt3")


###################################################
### code chunk number 25: sbs10
###################################################
if(file.exists("fit1.std.rds")){
	fit1.std <- readRDS("fit1.std.rds")
} else {
    fit1.std <- update(fit1, std.lv = TRUE, std.ov = TRUE, meanstructure = TRUE) 
    saveRDS(fit1.std, "fit1.std.rds")
}


###################################################
### code chunk number 26: sbs20
###################################################
# include 2 models in table request
fit1.t2 <- semTable(list("Ordinary" = fit1, "Standardized" = fit1.std), columns=c("estse", "z", "p"), file = file.path(tempdir, "fit1.2.1"), table.float = TRUE, longtable = TRUE, caption = "Ordinary and Standardized CFA Estimates", label = "tab:fit1.t2")


###################################################
### code chunk number 27: sbs30
###################################################
fit1.t2.2 <- semTable(list("Ordinary" = fit1, "Standardized" = fit1.std), columns = list("Ordinary" = c("estse", "z", "p"), "Standardized" = c("estse")), columnLabels = c(estse = "Est(S.E.)", z = "Z", se = "SE"), file = file.path(tempdir, "fit1.t2.2"), table.float = TRUE, longtable = TRUE, caption = "Customizing Column Selections by Model", label = "tab:fit1.t2.2" )


###################################################
### code chunk number 28: sem00
###################################################
regmodel1 <- 'visual  =~ x1 + x2 + x3 
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9
             visual ~ textual + speed
'


###################################################
### code chunk number 29: sem10
###################################################
if(file.exists("fit2.rds")){
	fit2 <- readRDS("fit2.rds")
} else {
    fit2 <- sem(regmodel1, data = HolzingerSwineford1939, std.lv = FALSE, meanstructure = TRUE)
    saveRDS(fit2, "fit2.rds")
}


###################################################
### code chunk number 30: sem20
###################################################
if(file.exists("fit2.std.rds")){
	fit2.std <- readRDS("fit2.std.rds")
} else {
    fit2.std <- update(fit2, std.lv = TRUE, std.ov = TRUE, meanstructure = TRUE)
    saveRDS(fit2.std, "fit2.std.rds")
}


###################################################
### code chunk number 31: semtable.Rnw:561-562
###################################################
fit2.t <- semTable(list("Ordinary" = fit2, "Standardized" = fit2.std), fits = "rmsea", columns = list("Ordinary" = c("est", "se", "p"), "Standardized" = c("estsestars")), columnLabels = c("est" = "Est", "se" = "Std.Err.", "p" = "p", "estsestars" = "Standardized Est."), paramSets = c("loadings", "intercepts", "slopes", "latentcovariances"), file = file.path(tempdir, "fit2.t1"), type = "latex", table.float = TRUE, longtable = TRUE, caption = "SEM, Standardized or Not", label = "tab:fit2.t")


###################################################
### code chunk number 32: semtable.Rnw:565-568
###################################################
# Change output format to csv
cat(markupConvert(attr(fit2.t, "markedResults"), type = "csv"), file = file.path(tempdir, "fit2.t.converted.csv"))
cat(markupConvert(attr(fit2.t, "markedResults"), type = "html"), file = file.path(tempdir, "fit2.t.converted.html"))


###################################################
### code chunk number 33: semtable.Rnw:576-582
###################################################
regmodel2 <- 'visual  =~ x1 + x2 + x3
              textual =~ x4 +  x6
              speed   =~  x8 + x9
              visual ~ speed
              textual ~ speed 
'


###################################################
### code chunk number 34: sem110
###################################################
if(file.exists("fit3.rds")){
	fit3 <- readRDS("fit3.rds")
} else {
    fit3 <- sem(regmodel2, data = HolzingerSwineford1939, std.lv = TRUE,  meanstructure = TRUE)
    saveRDS(fit3, "fit3.rds")
}

if(file.exists("fit3.std.rds")){
	fit3.std <- readRDS("fit3.std.rds")
} else {
    fit3.std <- update(fit3, std.lv = TRUE, std.ov = TRUE)
    saveRDS(fit3.std, "fit3.std.rds")
}


###################################################
### code chunk number 35: fit3.std.t1
###################################################
fit3.std.t1 <- semTable(list("Mod 1" = fit2, "Mod 1 std" = fit2.std, "Mod 2" = fit3, "Mod 3 std" = fit3.std), paramSets = c("loadings", "slopes", "intercepts", "residualvariances", "latentvariances", "latentcovariances"),  columns = c("estsestars"), type = "latex", file = file.path(tempdir, "fit3.std.t1"), table.float = TRUE, longtable = TRUE, caption = "Several SEMs with Differing Parameters", label = "tab:fit3.std.t1")


###################################################
### code chunk number 36: semtable.Rnw:612-612
###################################################



###################################################
### code chunk number 37: sem200
###################################################
if(file.exists("fit3.g2.rds")){
	fit3.g2 <- readRDS("fit3.g2.rds")
} else {
    fit3.g2 <- sem(regmodel1, data = HolzingerSwineford1939, group = "school")
    saveRDS(fit3.g2, "fit3.g2.rds")
}
#



###################################################
### code chunk number 38: semtable.Rnw:622-623
###################################################
fit3.g2.2 <- semTable(fit3.g2, paramSets = c("loadings", "slopes", "intercepts"), columns = c("estsestars"), fits = c("chisq", "rmsea", "cfi"), type = "latex", file = file.path(tempdir, "fit3.g2"), table.float=TRUE, longtable=TRUE, caption = "SEM with Two Groups", label = "tab:fit3.g2")


###################################################
### code chunk number 39: sem500
###################################################
## Model 5 - Mediation model with equality constraints
model5 <-
    '
    # latent variable definitions
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + e*y2 + d*y3 + y4
    dem65 =~ y5 + e*y6 + d*y7 + y8
    # regressions
    dem60 ~ a*ind60
    dem65 ~ c*ind60 + b*dem60
    # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8

    # indirect effect (a*b)
    ## := operator defines new parameters
    ab := a*b

    ## total effect
    total := c + (a*b)
    '


###################################################
### code chunk number 40: sem510
###################################################
if(file.exists("fit5.rds")){
	fit5 <- readRDS("fit5.rds")
} else {
    fit5 <- sem(model5, data=PoliticalDemocracy)
    saveRDS(fit5, "fit5.rds")
}

if(file.exists("fit5boot.rds")){
	fit5boot <- readRDS("fit5boot.rds")
} else {
    fit5boot <- sem(model5, data=PoliticalDemocracy, se = "bootstrap", bootstrap = 500)
    saveRDS(fit5boot, "fit5boot.rds")
}


###################################################
### code chunk number 41: sem520
###################################################
fit5.t2 <- semTable(list("ML estimates" = fit5, "Bootstrapped SE" = fit5boot), columns = c("estsestars", "rsquare"), file = file.path(tempdir, "fit5.2"), type = "latex", longtable = TRUE, table.float = TRUE, caption = "Comparing ML and Bootstrapped Estimates", label = "tab:fit5t2")


###################################################
### code chunk number 42: sem530
###################################################
##'
## Model 5b - Revision of Model 5s
model5b <-
    '
    # Cut some indicators from the measurement model
    ind60 =~ x1 + x2 
    dem60 =~ y1 + e*y2 + d*y3 + y4
    dem65 =~ y5 + e*y6 + d*y7 
    # regressions
    dem60 ~ a*ind60
    dem65 ~ c*ind60 + b*dem60
    # cut out the residual correlations
    # indirect effect (a*b)
    ## := operator defines new parameters
    ab := a*b
    ## total effect
    total := c + (a*b)
    '


###################################################
### code chunk number 43: sem550
###################################################
if(file.exists("fit5b.rds")){
	fit5b <- readRDS("fit5.rds")
} else {
    fit5b <- sem(model5b, data=PoliticalDemocracy, se = "bootstrap", bootstrap = 500)
    saveRDS(fit5b, "fit5b.rds")
}


###################################################
### code chunk number 44: semtable.Rnw:721-722
###################################################
fit5.5b <- semTable(list("Model 5" = fit5boot, "Model 5b" = fit5b), columns = c("estsestars", "rsquare"), file = file.path(tempdir, "fit5.5"), type = "latex", longtable = TRUE, table.float = TRUE, caption = "Models 5 and 5b", label = "tab:fit5.5b")


###################################################
### code chunk number 45: sessioninfo
###################################################
sessionInfo()
if(!is.null(warnings())){
    print("Warnings:")
    warnings()}


###################################################
### code chunk number 46: RoptionsRestore
###################################################
## Don't delete this. It puts the interactive session options
## back the way they were. If this is compiled within a session
## it is vital to do this.
options(opts.orig)
par(par.orig)


