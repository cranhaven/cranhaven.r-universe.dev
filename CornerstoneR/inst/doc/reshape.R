## ----setup, include = FALSE---------------------------------------------------
# show grouped code output instead of single lines
# use '#>' for R outputs
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----Indometh, echo=FALSE, results='asis'-------------------------------------
IndomethParts = rbind(head(Indometh, 3), head(Indometh[Indometh$Subject == 4, ], 3))
knitr::kable(IndomethParts, caption = "Rows 1-3 and 34-36 of Indometh Dataset")

## ----Indometh wide, echo=FALSE, results='asis'--------------------------------
Indometh.wide = CornerstoneR::reshapeWide(Indometh, "time", "conc", "Subject", character(0)
                                          , list(drop = TRUE, aggr.fun = ""), return.results = TRUE
                                          )
knitr::kable(Indometh.wide$reshapeWide[, 1:5], caption = "Indometh Dataset in Wide Format")

## ----reshapeWideMenu, echo=FALSE, fig.cap="Reshape to Wide: Menu"-------------
knitr::include_graphics("imgs/reshapeWide01.png")

## ----reshapeWideVariables, echo=FALSE, fig.cap="Reshape to Wide: Variable Selection"----
knitr::include_graphics("imgs/reshapeWide02.png")

## ----reshapeWideRScript, echo=FALSE, fig.cap="Reshape to Wide: R Script"------
knitr::include_graphics("imgs/reshapeWide03.png")

## ----reshapeWideResultMenu, echo=FALSE, fig.cap="Reshape to Wide: Result Menu"----
knitr::include_graphics("imgs/reshapeWide04.png")

## ----reshapeWideResultDataset, echo=FALSE, fig.cap="Reshape to Wide: Result Dataset"----
knitr::include_graphics("imgs/reshapeWide05.png")

## ----reshapeWideScriptVariables, echo=FALSE, fig.cap="Reshape to Wide: Script Variables"----
knitr::include_graphics("imgs/reshapeWide06.png")

## ----reshapeWideAggregation, echo=FALSE, results='asis'-----------------------
Indometh.man = data.frame(Subject = rep(1, 6)
                          , time = rep(c(0.25, 0.5), each = 3)
                          , conc = c(1.5, 1.67, 1.58, 0.94, 0.89, 0.72)
                          , repet = rep(c(1, 2, 3), 2)
                          )
knitr::kable(Indometh.man, caption = "Example Dataset with Repetitions")

## ----IndomethAggrMean, echo=FALSE, results='asis'-----------------------------
Indometh.man.wide = CornerstoneR::reshapeWide(Indometh.man[, 1:3], "time", "conc", "Subject", character(0)
                                          , list(drop = TRUE, aggr.fun = "first, mean, sd"), return.results = TRUE
                                          )
knitr::kable(Indometh.man.wide$reshapeWide, caption = "Example Dataset in Wide Format")

## ----Indometh manual wide, echo=FALSE, results='asis'-------------------------
Indometh.man.wide = CornerstoneR::reshapeWide(Indometh.man, "time", "conc", "Subject", "repet"
                                          , list(drop = TRUE, aggr.fun = "maxby(repet), minby(repet)")
                                          , return.results = TRUE
                                          )
knitr::kable(Indometh.man.wide$reshapeWide, caption = "Example Dataset in Wide Format")

## ----reshapeLongSample1, echo=FALSE, results='asis'---------------------------
knitr::kable(data.frame(Place = c("Europe", "USA", "Asia")
                        , Temp.Jan = c(0, -10, 20)
                        , Temp.July = c(20, 15, 50)
                        ))

## ----reshapeLongSample2, echo=FALSE, results='asis'---------------------------
knitr::kable(data.frame(Place = rep(c("Europe", "USA", "Asia"), 2)
                        , variable = rep(c("Jan", "July"), each = 3)
                        , value = c(0, -10, 20, 20, 15, 50)
                        ))

## ----reshapeLongMenu, echo=FALSE, fig.cap="Reshape to Long: Menu"-------------
knitr::include_graphics("imgs/reshapeLong01.png")

## ----reshapeLongVariables, echo=FALSE, fig.cap="Reshape to Long: Variable Selection"----
knitr::include_graphics("imgs/reshapeLong02.png")

## ----reshapeLongRScript, echo=FALSE, fig.cap="Reshape to Long: R Script"------
knitr::include_graphics("imgs/reshapeLong03.png")

## ----reshapeLongRScriptVarsMenu, echo=FALSE, fig.cap="Reshape to Long: R Script Variables Menu"----
knitr::include_graphics("imgs/reshapeLong04.png")

## ----reshapeLongRScriptVars, echo=FALSE, fig.cap="Reshape to Long: R Script Variables"----
knitr::include_graphics("imgs/reshapeLong05.png")

## ----reshapeLongResultMenu, echo=FALSE, fig.cap="Reshape to Long: Result Menu"----
knitr::include_graphics("imgs/reshapeLong06.png")

## ----reshapeLongResultDataset, echo=FALSE, fig.cap="Reshape to Long: Result Dataset"----
knitr::include_graphics("imgs/reshapeLong07.png")

## ----reshapeTransposeSample, echo=FALSE, results='asis'-----------------------
knitr::kable(data.frame(Statistic = c("Count", "Mean", "St. Dev.")
                        , MPG = c(398, 23.52, 7.82)
                        , Displacement = c(406, 194.78, 104.92)
                        , Horsepower = c(400, 105.08, 38.77)
                        , Weight = c(406, 2979.41, 847.00)
                        , Acceleration = c(406, 15.52, 2.80)
                        ))

## ----reshapeTransposeResult, echo=FALSE, results='asis'-----------------------
knitr::kable(data.frame(colnames = c("MPG", "Displacement", "Horsepower", "Weight", "Acceleration")
                        , Count = c(398, 406, 400, 406, 406)
                        , Mean = c(23.52, 194.78, 105.08, 2979.41, 15.52)
                        , StDev = c(7.82, 104.92, 38.77, 847.00, 2.80)
                        ))

## ----reshapeTransposeMenu, echo=FALSE, fig.cap="Transpose Data: Menu"---------
knitr::include_graphics("imgs/reshapeTranspose01.png")

## ----reshapeTransposeVariableSeleciton, echo=FALSE, fig.cap="Transpose Data: Variable Selection"----
knitr::include_graphics("imgs/reshapeTranspose02.png")

## ----reshapeTransposeRScript, echo=FALSE, fig.cap="Transpose Data: R Script"----
knitr::include_graphics("imgs/reshapeTranspose03.png")

## ----reshapeTransposeResultMenu, echo=FALSE, fig.cap="Transpose Data: Result Menu"----
knitr::include_graphics("imgs/reshapeTranspose04.png")

## ----reshapeTransposeResultDataset, echo=FALSE, fig.cap="Transpose Data: Result Dataset"----
knitr::include_graphics("imgs/reshapeTranspose05.png")

## ----reshapeTransposeScriptVariables, echo=FALSE, fig.cap="Transpose Data: Script Variables"----
knitr::include_graphics("imgs/reshapeTranspose06.png")

