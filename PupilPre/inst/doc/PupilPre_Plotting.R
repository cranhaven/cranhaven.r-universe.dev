## ----global_options, include=FALSE---------------------------------------
library(ggplot2)
library(gridExtra)

knitr::opts_chunk$set(fig.width=6, fig.height=4, fig.align="center", warning=FALSE)

## ---- eval = TRUE, echo=FALSE, results='hide', message=FALSE-------------
library(PupilPre)
# Load some proccessed data
data(Pupilex7)
dat <- Pupilex7

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil",
             Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, 
             Cond2Labels = NA, ErrorBar = TRUE, PupilPreTheme = TRUE) 

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil",
             Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, 
             Cond2Labels = NA, ErrorBar = TRUE, PupilPreTheme = TRUE) + 
  ggtitle("Averaged Data")

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil",
             Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA,
             ErrorBar = TRUE, PupilPreTheme = FALSE) + 
  theme(axis.text = element_text(size = 15))

## ---- eval= TRUE, fig.show='hold', fig.height=5, results='asis', message=FALSE----
ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil", 
             Condition1 = "talker", Condition2 = NULL, 
             Cond1Labels = c(CH1 = "Chinese 1", CH8 = "Chinese 3",
                             CH9 = "Chinese 2", CH2 = "Chinese 4", EN3 = "English 1"),
             Cond2Labels = NA, ErrorBar = TRUE, PupilPreTheme = TRUE)

## ---- eval= TRUE, fig.show='hold', fig.width=7, fig.height=5, results='asis', message=FALSE----
ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil", 
             Condition1 = "talker", Condition2 = "ExpLev", 
             Cond1Labels = c(CH1 = "Chinese 1", CH8 = "Chinese 3",
                             CH9 = "Chinese 2", CH2 = "Chinese 4", EN3 = "English 1"),
             Cond2Labels = c(H = "High Exp", L = "Low Exp"), 
             ErrorBar = TRUE, PupilPreTheme = TRUE)

## ---- eval= TRUE, fig.show='hold', fig.width=7, fig.height=8, results='asis', message=FALSE----
gridExtra::grid.arrange(

ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil", 
             Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, 
             Cond2Labels = NA, ErrorBar = TRUE, PupilPreTheme = TRUE),

ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil", 
             Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, 
             Cond2Labels = NA, ErrorBar = FALSE, 
             ErrorBand = TRUE, PupilPreTheme = TRUE),
	
nrow=2, ncol=1)

## ---- eval= TRUE, fig.show='hold', fig.width=7, fig.height=8, results='asis', message=TRUE----
gridExtra::grid.arrange(

ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil", 
             Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, 
             Cond2Labels = NA, ErrorBar = FALSE, ErrorBand = TRUE, 
             ErrorType = "CI", ConfLev = 95, CItype = "pointwise", 
             PupilPreTheme = TRUE), 

ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil", 
             Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, 
             Cond2Labels = NA, ErrorBar = FALSE, ErrorBand = TRUE, 
             ErrorType = "CI", ConfLev = 95, CItype = "simultaneous", 
             PupilPreTheme = TRUE),
	
nrow=2, ncol=1)

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil", 
             Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, 
             Cond2Labels = NA, Averaging = "Subject", ErrorBar = TRUE, 
             ErrorType = "SE", PupilPreTheme = TRUE) 

## ---- eval= TRUE, fig.show='hold', fig.width=7, fig.height=8, results='asis', message=FALSE----
ppl_plot_avg_cdiff(data = dat, Column = "Pupil", 
                   Condition = list(talker = c("EN3", "CH1")),
                   ErrorBar = TRUE, PupilPreTheme = TRUE)

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
ppl_plot_avg_contour(data = dat, Column = "Pupil", Var = "TRIAL_INDEX", 
                     VarLabel = "Trial", xlim = c(0,2000), PupilPreTheme = TRUE, 
                     Colors = c("gray20", "gray90"))

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
ppl_plot_avg_contour(data = dat, Column = "Pupil", Var = "TRIAL_INDEX", 
                     VarLabel = "Trial", xlim = c(0,2000), PupilPreTheme = TRUE, 
                     Colors = c("red", "green")) + ggtitle("Pupil dilation")

## ---- eval= TRUE, results='asis', message=FALSE--------------------------
plt <- ppl_plot_avg(data = dat, xlim = c(0, 2000), Column = "Pupil",
                    Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA,
                    Cond2Labels = NA, Averaging = "Subject", ErrorBar = TRUE,
                    ErrorType = "SE", PupilPreTheme = TRUE) 

## ---- eval= TRUE, results='asis', message=FALSE--------------------------
df <- plt$data

## ---- eval= TRUE, echo=FALSE, results='asis', message=FALSE--------------
knitr::kable(head(df))

## ---- eval= FALSE, results='asis', message=FALSE-------------------------
#  plot_events(pupiltest4, Column = "Pupil", Device = "pdf", Grouping = "Subject", path = "Auto", Nrow = 2, Ncol = 3, width = 11, height = 8.5)

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  plot_compare_app(dat)

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  plot_summary_app(dat)

