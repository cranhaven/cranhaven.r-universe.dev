## ----example_plot, echo=TRUE, warning=FALSE, message=FALSE, fig.width=6, fig.height=5----
library(Tendril)

data("TendrilData")

test <- Tendril(mydata = TendrilData,
                rotations = Rotations,
                AEfreqThreshold = 9,
                Tag = "Comment",
                Treatments = c("placebo", "active"),
                Unique.Subject.Identifier = "subjid",
                Terms = "ae",
                Treat = "treatment",
                StartDay = "day",
                SubjList = SubjList,
                SubjList.subject = "subjid",
                SubjList.treatment = "treatment"
)
  
plot(test)
plot(test, coloring = "p.adj")
plot(test, term = c("AE40", "AE42", "AE44"))


## ----tendrildata_head, echo=FALSE, warning=FALSE, message=FALSE---------------

head(TendrilData)

## ----call_Tendril, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE---------
test <- Tendril(mydata = TendrilData,
                rotations = Rotations,
                AEfreqThreshold = 9,
                Tag = "Comment",
                Treatments = c("placebo", "active"),
                Unique.Subject.Identifier = "subjid",
                Terms = "ae",
                Treat = "treatment",
                StartDay = "day",
                SubjList = SubjList,
                SubjList.subject = "subjid",
                SubjList.treatment = "treatment"
)

## ----call_TendrilPerm, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE-----
perm <- TendrilPerm(test,
                    "AE40",
                    n.perm = 500)
perm2 <- TendrilPerm(test,
                     "AE42",
                     n.perm = 500,
                     perm.from.day = 120)

## ----plot_tendril, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE, fig.width=6, fig.height=5----
plot(test)
plot(test, coloring = "p.adj")
plot(test, term = "AE40", coloring = "fish")
plot(test, term = paste("AE", c(40, 42, 44), sep = ""), coloring = "Terms")

## ----plot_TendrilPerm, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE, fig.width=6, fig.height=5----
plot(perm)
plot(perm, coloring = "OR", percentile = TRUE)
plot(perm2, coloring = "p.adj", percentile = TRUE)

## ----plot_timeseries, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE, fig.width=6, fig.height=5----
plot_timeseries(test)
plot_timeseries(test, term = paste("AE", c(40, 42, 44), sep = ""))

## ----full_example, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE--------
#  #load library
#  library("Tendril")
#  #compute tendril data
#  data.tendril <- Tendril(mydata = TendrilData,
#                          rotations = Rotations,
#                          AEfreqThreshold = 9,
#                          Tag = "Comment",
#                          Treatments = c("placebo", "active"),
#                          Unique.Subject.Identifier = "subjid",
#                          Terms = "ae",
#                          Treat = "treatment",
#                          StartDay = "day",
#                          SubjList = SubjList,
#                          SubjList.subject = "subjid",
#                          SubjList.treatment = "treatment",
#                          filter_double_events = FALSE,
#                          suppress_warnings = FALSE)
#  
#  #compute permutations
#  data.tendril <- TendrilPerm(tendril = data.tendril,
#                              PermTerm="AE40",
#                              n.perm = 200,
#                              perm.from.day = 1)
#  
#  #do plot
#  p <- plot(data.tendril$tendril)
#  
#  #plot permutations
#  p <- plot(data.tendril)
#  
#  #plot permutations and percentile
#  p <- plot(data.tendril, percentile = TRUE)
#  
#  #save tendril coordinates and stat results
#  write.table(data.tendril$tendril$data, "mydata.txt", sep="\t", row.names = FALSE)
#  
#  #save permutation coordinates
#  write.table(data.tendril$perm.data, "my_permutation_data.txt", sep="\t", row.names = FALSE)
#  
#  #save permutation percentiles
#  write.table(data.tendril$tendril.pi, "my_percentile_data.txt", sep="\t", row.names = FALSE)
#  

