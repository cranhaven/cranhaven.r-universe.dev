## -----------------------------------------------------------------------------
require(figuRes2); require(stringr)
data(outputplan)
# Run the next line to see first 6 rows
# head(outputplan)

## -----------------------------------------------------------------------------
# loadplan=F presumes outputplan exists in the Global environment
refresh.outputplan(loadplan=F) 
# Note the additional columns created these will get passed to annotate.page
names(outputplan)

## ---- eval=F------------------------------------------------------------------
#  # Replace the paths for your machine.
#  
#  default.settings(
#          my.path = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/",
#          od = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/output/",
#          dd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/dddata/",
#          cd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/code/",
#          logd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/log/")
#  
#  # This code creates directory at the locations specified.
#  dir.create(file.path(cd), showWarnings = FALSE)
#  dir.create(file.path(dd), showWarnings = FALSE)
#  dir.create(file.path(od), showWarnings = FALSE)
#  dir.create(file.path(logd), showWarnings = FALSE)
#  

## ---- eval=T------------------------------------------------------------------
data(outputplan)
data(benrisk2.data)
data(summary.lineplot.data)
data(boxplot.driver)
data(demog.data)
data(forest.data)
data(cdf.data)
data(km.data)
data(raw.lineplot.data)
data(lineplot.data)

## ---- eval=F------------------------------------------------------------------
#  write.csv(file=file.path(my.path, "dddata", "outputplan.csv"), outputplan)
#  write.csv(file=file.path(my.path, "dddata", "benrisk2.data.csv"), benrisk2.data)
#  write.csv(file=file.path(my.path, "dddata", "summary.lineplot.data.csv"), summary.lineplot.data)
#  write.csv(file=file.path(my.path, "dddata", "g_bslchar.csv"), demog.data)
#  write.csv(file=file.path(my.path, "dddata", "forest.data.csv"), forest.data)
#  write.csv(file=file.path(my.path, "dddata", "cdf.data.csv"), cdf.data)
#  write.csv(file=file.path(my.path, "dddata", "km.data.csv"), km.data)
#  write.csv(file=file.path(my.path, "dddata", "raw.lineplot.data.csv"), raw.lineplot.data)
#  write.csv(file=file.path(my.path, "dddata", "lineplot_example.csv"), raw.lineplot.data)

## ---- eval=T------------------------------------------------------------------
data(continuous_by_visit_and_treatment) # driver1
data(category_by_visit) # driver2
data(scatter_smooth) # driver 3
data(scatter_smooth_facet) # driver 4
data(scatterplot_with_smoother) # driver5
data(cdf_weight) # driver8
data(priordens) # driver9

## ---- eval=F------------------------------------------------------------------
#  writeLines(driver1, con=paste0(cd,"continuous_by_visit_and_treatment.r"))
#  writeLines(driver2, con=paste0(cd,"category_by_visit.r"))
#  writeLines(driver3, con=paste0(cd,"scatter_smooth.r"))
#  writeLines(driver4, con=paste0(cd,"scatter_smooth_facet.r"))
#  writeLines(driver5, con=paste0(cd,"scatterplot_with_smoother.r"))
#  writeLines(driver8, con=paste0(cd,"cdf_weight.r"))
#  writeLines(driver9, con=paste0(cd,"priordens.r"))

## ---- eval=F------------------------------------------------------------------
#  # Write boxplot.driver file to code directory
#  writeLines(boxplot.driver, con=file.path(cd, "boxplot.driver.txt"))

## ---- eval=F------------------------------------------------------------------
#  outputplan.bp <- subset(outputplan, PlotStyle == "Boxplot" & Response != "")
#  
#   for(i in 1:nrow(outputplan.bp )){
#     tx  <- boxplot.driver
#     tx  <- gsub(pattern = "@ProgramName", replace = paste(outputplan.bp$Study[i],
#                      outputplan.bp$ULHead1[i]), x = tx)
#     tx  <- gsub(pattern = "@DataFileName", replace = outputplan.bp$csv[i], x = tx)
#     tx  <- gsub(pattern = "@OutputName", replace = outputplan.bp$outputfile[i], x = tx)
#     tx  <- gsub(pattern = "@RESPONSE", replace = outputplan.bp$Response[i], x = tx)
#     tx  <- gsub(pattern = "@ProtocolID", replace = outputplan.bp$ulh1[i], x = tx)
#     tx  <- gsub(pattern = "@PlotStyle", replace = outputplan.bp$PlotStyle[i], x = tx)
#     tx  <- gsub(pattern = "@FigureTitle", replace = outputplan.bp$HarpTitle[i], x = tx)
#     tx  <- gsub(pattern = "@TableRef", replace = outputplan.bp$TableID[i], x = tx)
#     tx  <- gsub(pattern = "@Author", replace = outputplan.bp$Author[i], x = tx)
#     tx  <- gsub(pattern = "@Date", replace = Sys.Date(), x = tx)
#     writeLines(tx, con=paste0(cd,outputplan.bp$rcode[i]))
#   }

## ---- eval=FALSE--------------------------------------------------------------
#  remove(list=ls())

## ---- results='hide', eval=F--------------------------------------------------
#  # require(figuRes2)
#  # ./workflowdemo should still be the working directory.
#  #setwd(file.path(getwd(), "workflowdemo"))
#  default.settings(
#          my.path = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/",
#          od = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/output/",
#          dd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/dddata/",
#          cd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/code/",
#          logd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/log/")

## ---- eval=FALSE--------------------------------------------------------------
#  data(outputplan)
#  head(outputplan)
#  refresh.outputplan(loadplan=F)

## ---- eval=FALSE--------------------------------------------------------------
#  run.specific(as.character(outputplan$rcode[1]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[2]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[3]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[4]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[5]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[6]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[7]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[8]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[9]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[12]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[13]), toPDF=T)
#  run.specific(as.character(outputplan$rcode[14]), toPDF=T)
#  # In practice it may be easier to work with the filename, e.g.,

## ---- eval=FALSE--------------------------------------------------------------
#  all_in_one(UseSubset = "RunThis", filename= "allinone.PDF" , reportNR=FALSE)

