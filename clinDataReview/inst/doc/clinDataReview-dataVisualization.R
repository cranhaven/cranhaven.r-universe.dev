## ----options, echo = FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------

library(knitr)
opts_chunk$set(
    echo = TRUE, 
    results = 'asis', 
    warning = FALSE, 
    error = FALSE, message = FALSE, cache = FALSE,
    fig.width = 8, fig.height = 7,
    fig.path = "./figures_vignette/",
    fig.align = 'center')
options(width = 170)#, stringsAsFactors = FALSE
options(warn = 1)#instead of warn = 0 by default -> to have place where warnings occur in the call to Sweave function


## ----checkPandocAvailability, echo = FALSE------------------------------------------------------------------------------------------------------------------------------
hasPandoc <- rmarkdown::pandoc_available()

## ----loadLibraries------------------------------------------------------------------------------------------------------------------------------------------------------

library(clinDataReview)
library(plotly)


## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------

library(clinUtils)

data(dataADaMCDISCP01)
labelVars <- attr(dataADaMCDISCP01, "labelVars")

varsLB <- c(
	"PARAM", "PARAMCD", "USUBJID", "TRTP", 
	"ADY", "VISITNUM", "VISIT", "LBSTRESN"
)
dataLB <- dataADaMCDISCP01$ADLBC[, varsLB]

varsAE <- c("USUBJID", "AESOC", "AEDECOD", "ASTDY", "AENDY", "AESEV")
dataAE <- dataADaMCDISCP01$ADAE[, varsAE]

varsDM <- c("RFSTDTC", "USUBJID")
dataDM <- dataADaMCDISCP01$ADSL[, varsDM]


## ----patientProfilesCreate, results = "hide", eval = hasPandoc & interactive()------------------------------------------------------------------------------------------
#  # create a directory to store the patient profiles:
#  patientProfilesDir <- "patientProfiles"
#  dir.create(patientProfilesDir)
#  
#  # get examples of parameters for the report
#  configDir <- system.file("skeleton", "config", package = "clinDataReview")
#  params <- getParamsFromConfig(
#  	configDir = configDir,
#  	configFile = "config-patientProfiles.yml"
#  )
#  # create patient profile with only one panel for the demo
#  params$patientProfilesParams <- params$patientProfilesParams[1]
#  # use dataset from the clinUtils package
#  params$pathDataFolder <- system.file("extdata", "cdiscpilot01", "SDTM", package = "clinUtils")
#  # store patient profile in this folder:
#  params$patientProfilePath <- patientProfilesDir
#  
#  # create patient profiles
#  pathTemplate <- clinDataReview::getPathTemplate(params$template)
#  file.copy(from = pathTemplate, to = ".")
#  report <- rmarkdown::render(
#  	input = basename(pathTemplate),
#  	envir = new.env()
#  )
#  unlink(basename(pathTemplate))
#  unlink(basename(report))

## ----reportingVignette, eval = FALSE------------------------------------------------------------------------------------------------------------------------------------
#  
#  vignette("clinDataReview-reporting", "clinDataReview")
#  

## ----timeProfiles, eval = hasPandoc-------------------------------------------------------------------------------------------------------------------------------------

labParam <- "ALT"
dataPlot <- subset(dataLB, PARAMCD == labParam)

visitLab <- with(dataPlot, tapply(ADY, VISIT, median))
names(visitLab) <- sub("-", "\n", names(visitLab))

# link to patient profiles
if(interactive())
	dataPlot$patientProfilePath <- paste0(
	    "patientProfiles/subjectProfile-", 
	    sub("/", "-", dataPlot$USUBJID), ".pdf"
	)

scatterplotClinData(
    data = dataPlot, 
    xVar = "ADY",
    yVar = "LBSTRESN",
    aesPointVar = list(color = "TRTP", fill = "TRTP"),
    aesLineVar = list(group = "USUBJID", color = "TRTP"),
    linePars = list(size = 0.5, alpha = 0.7),
    hoverVars = c("USUBJID", "VISIT", "ADY", "LBSTRESN", "TRTP"),
    labelVars = labelVars,
    xPars = list(breaks = visitLab, labels = names(visitLab)),
    title = paste("Actual value of", 
        getLabelParamcd(
            paramcd = labParam, data = dataLB, paramcdVar = "PARAMCD", paramVar = "PARAM"
        )
    ),
    # include link to patient profiles:
    pathVar = if(interactive())	"patientProfilePath",
	subtitle = paste(
		"Profile plot by subject",
		"Points are positioned at relative day",
		"Visits are positioned based on median relative day across subjects.",
		sep = "\n"
	),
	verbose = TRUE,
    table = FALSE
)


## ----scatterplot, eval = hasPandoc--------------------------------------------------------------------------------------------------------------------------------------
# format data long -> wide format (one column per lab param)
dataPlot <- subset(dataLB, PARAMCD %in% c("ALT", "ALB"))
dataPlot <- stats::aggregate(
	LBSTRESN ~ USUBJID + VISIT + VISITNUM + PARAMCD, 
	data = dataPlot,
	FUN = mean
)
dataPlotWide <- stats::reshape(
	data = dataPlot,
	timevar = "PARAMCD", idvar = c("USUBJID", "VISIT", "VISITNUM"),
	direction = "wide"
)
colnames(dataPlotWide) <- sub("^LBSTRESN.", "", colnames(dataPlotWide))

# link to patient profiles
if(interactive())
	dataPlotWide$patientProfilePath <- paste0(
	    "patientProfiles/subjectProfile-", 
	    sub("/", "-", dataPlotWide$USUBJID), ".pdf"
	)

# scatterplot per visit
scatterplotClinData(
    data = dataPlotWide, 
    xVar = "ALT", yVar = "ALB",
    xLab = getLabelParamcd(
        paramcd = "ALT", data = dataLB, paramcdVar = "PARAMCD", paramVar = "PARAM"
    ),
    yLab = getLabelParamcd(
        paramcd = "ALB", data = dataLB, paramcdVar = "PARAMCD", paramVar = "PARAM"
    ),
    aesPointVar = list(color = "USUBJID", fill = "USUBJID"),
    facetPars = list(facets = ~ VISIT),
    labelVars = labelVars,
    pathVar = if(interactive())	"patientProfilePath",
    table = FALSE
)


## ----eDishPlot, eval = hasPandoc----------------------------------------------------------------------------------------------------------------------------------------

dataALT <- subset(dataLB, PARAMCD == "ALT")
dataBILI <- subset(dataLB, PARAMCD == "BILI")

byVar <- c("USUBJID", "VISIT")

dataPlot <- merge(
    x = dataALT, y = dataBILI[, c(byVar, "LBSTRESN")], 
    by = c("USUBJID", "VISIT"), 
    suffixes = c(".ALT", ".BILI"),
    all = TRUE
)
labelVars[paste0("LBSTRESN.", c("ALT", "BILI"))] <-
    paste(
        "Actual value of", 
        getLabelParamcd(
            paramcd = c("ALT", "BILI"), data = dataLB, paramcdVar = "PARAMCD", paramVar = "PARAM"
        )
    )

# link to patient profiles
if(interactive())
	dataPlot$patientProfilePath <- paste0(
	    "patientProfiles/subjectProfile-", 
	    sub("/", "-", dataPlot$USUBJID), ".pdf"
	)

# scatterplot per visit
scatterplotClinData(
    data = dataPlot, 
    xVar = "LBSTRESN.ALT", yVar = "LBSTRESN.BILI",
    xLab = getLabelParamcd(
        paramcd = "ALT", data = dataLB, paramcdVar = "PARAMCD", paramVar = "PARAM"
    ),
    yLab = getLabelParamcd(
        paramcd = "BILI", data = dataLB, paramcdVar = "PARAMCD", paramVar = "PARAM"
    ),
    aesPointVar = list(color = "VISIT", fill = "VISIT"),
    xTrans = "log10", yTrans = "log10",
    hoverVars = "USUBJID",
    labelVars = labelVars,
    table = FALSE, 
    pathVar = if(interactive())	"patientProfilePath"
)


## ----timeProfileIntervalPlot, eval = hasPandoc--------------------------------------------------------------------------------------------------------------------------

# link to patient profiles
if(interactive())
	dataAE$patientProfilePath <- paste0(
	    "patientProfiles/subjectProfile-", 
	    sub("/", "-", dataAE$USUBJID), ".pdf"
	)
timeProfileIntervalPlot(
    data = dataAE,
    paramVar = "USUBJID",
    # time-variables
    timeStartVar = "ASTDY",
    timeEndVar = "ASTDY",
    colorVar = "AESEV",
    hoverVars = c("USUBJID", "AEDECOD", "ASTDY", "AENDY", "AESEV"),
    labelVars = labelVars,
    table = FALSE, 
	pathVar = if(interactive())	"patientProfilePath",
    tableVars = c("USUBJID", "AEDECOD", "ASTDY", "AENDY", "AESEV")
)


## ----timeProfileIntervalPlot-shapeVariables, eval = hasPandoc-----------------------------------------------------------------------------------------------------------

# create variable to indicate status of start/end date
dataAE$AESTFLG <- ifelse(is.na(dataAE$ASTDY), "Missing start", "Complete")
dataAE$AEENFLG <- ifelse(is.na(dataAE$AENDY), "Missing end", "Complete")
shapePalette <- c(
    `Missing start` = "triangle-left", 
    `Complete` = "square-open", 
    `Missing end` = "triangle-right"
)

# 'simple'-imputation:
# if start is missing, 'Missing' symbol displayed at end interval
dataAE$AESTDYIMP <- with(dataAE, ifelse(is.na(ASTDY), AENDY, ASTDY))
# if end is missing, 'Missing' symbol displayed at start interval
dataAE$AEENDYIMP <- with(dataAE, ifelse(is.na(AENDY), ASTDY, AENDY))

timeProfileIntervalPlot(
    data = dataAE,
    paramVar = "USUBJID", 
    # time-variables
    timeStartVar = "AESTDYIMP", timeStartLab = "Start day",
    timeEndVar = "AEENDYIMP", timeEndLab = "End day",
    # shape variables
    timeStartShapeVar = "AESTFLG",
    timeStartShapeLab = "Status of start date",
    timeEndShapeVar = "AEENFLG",
    timeEndShapeLab = "Status of end date",
    shapePalette = shapePalette,
    hoverVars = c("USUBJID", "AEDECOD", "AESEV", "ASTDY", "AESTFLG", "AENDY", "AEENFLG"),
    labelVars = labelVars,
    table = FALSE, 
    tableVars = c("USUBJID", "AEDECOD", "AESEV", "ASTDY", "AESTFLG", "AENDY", "AEENFLG"),
    pathVar = if(interactive())	"patientProfilePath"
)


## ----computeSummaryStatistics-categoricalVariable, eval = requireNamespace("inTextSummaryTable", quietly = TRUE)--------------------------------------------------------

# total counts: Safety Analysis Set (patients with start date for the first treatment)
dataTotal <- subset(dataDM, RFSTDTC != "")

## patient profiles report

if(interactive()){

	# add path in data
	
	dataAE$patientProfilePath <- paste0(
	    "patientProfiles/subjectProfile-", 
	    sub("/", "-", dataAE$USUBJID), ".pdf"
	)

	# add link in data (for attached table)
	dataAE$patientProfileLink <- with(dataAE,
	    paste0(
	        '<a href="', patientProfilePath, 
	        '" target="_blank">', USUBJID, '</a>'
	    )
	)

	# Specify extra summarizations besides the standard stats
	# When the data is summarized,
	# the patient profile path are summarized
	# as well across patients
	# (the paths should be collapsed with: ', ')
	statsExtraPP <- list(
	    statPatientProfilePath = function(data)	
	      toString(sort(unique(data$patientProfilePath))),
	    statPatientProfileLink = function(data)
	      toString(sort(unique(data$patientProfileLink)))
	)
	
}

# get counts (records, subjects, % subjects) + stats with subjects profiles path
statsPP <- c(
	inTextSummaryTable::getStats(type = "count"),
    if(interactive())
		list(
        	patientProfilePath = quote(statPatientProfilePath),
        	patientProfileLink = quote(statPatientProfileLink)
    	)
)

dataAE$AESEV <- factor(
	dataAE$AESEV,
	levels = c("MILD", "MODERATE", "SEVERE")
)
dataAE$AESEVN <- as.numeric(dataAE$AESEV)

# compute adverse event table
tableAE <- inTextSummaryTable::computeSummaryStatisticsTable(
    
    data = dataAE,
    rowVar = c("AESOC", "AEDECOD"),
    dataTotal = dataTotal,
    labelVars = labelVars,
    
    # The total across the variable used for the nodes
	# should be specified
    rowVarTotalInclude = c("AESOC", "AEDECOD"),
	
	rowOrder = "total",
    
    # statistics of interest
    # include columns with patients
    stats = statsPP, 
    # add extra 'statistic': concatenate subject IDs
    statsExtra = if(interactive())	statsExtraPP

)
knitr::kable(head(tableAE),
    caption = paste("Extract of the Adverse Event summary table",
        "used for the sunburst and barplot visualization"
    )
)


## ----sunburst, eval = hasPandoc & requireNamespace("inTextSummaryTable", quietly = TRUE)--------------------------------------------------------------------------------

dataSunburst <- tableAE

dataSunburst$m <- as.numeric(dataSunburst$m)

sunburstClinData(
    data = dataSunburst,
    vars = c("AESOC", "AEDECOD"),
    valueVar = "m", 
	valueLab = "Number of adverse events",
    pathVar = if(interactive())	"patientProfileLink",
    pathLab = clinUtils::getLabelVar(var = "USUBJID", labelVars = labelVars),
    table = FALSE,
    labelVars = labelVars
)


## ----treemap, eval = hasPandoc & requireNamespace("inTextSummaryTable", quietly = TRUE)---------------------------------------------------------------------------------

dataTreemap <- tableAE

dataTreemap$m <- as.numeric(dataTreemap$m)

treemapClinData(
    data = dataTreemap,
    vars = c("AESOC", "AEDECOD"),
    valueVar = "m",
    valueLab = "Number of adverse events",
    pathVar = if(interactive()) "patientProfileLink",
    pathLab = clinUtils::getLabelVar(var = "USUBJID", labelVars = labelVars),
    table = FALSE,
    labelVars = labelVars
)


## ----barplot, eval = hasPandoc & requireNamespace("inTextSummaryTable", quietly = TRUE)---------------------------------------------------------------------------------

dataPlot <- subset(tableAE, AEDECOD != "Total")

dataPlot$n <- as.numeric(dataPlot$n)

# create plot
barplotClinData(
    data = dataPlot,
    xVar = "AEDECOD", 
    yVar = "n",
    yLab = "Number of patients with adverse events",
	textVar = "n",
    labelVars = labelVars,
    pathVar = if(interactive())	"patientProfileLink",
    pathLab = clinUtils::getLabelVar(var = "USUBJID", labelVars = labelVars),
    table = FALSE
)


## ----getData-continuousVariable-----------------------------------------------------------------------------------------------------------------------------------------

dataVSDIABP <- subset(dataADaMCDISCP01$ADVS, 
	PARAMCD == "DIABP" & ANL01FL == "Y" &
	AVISIT %in% c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8")
)

# add link to patient profiles report

# add path in data
if(interactive()){
	
	dataVSDIABP$patientProfilePath <- paste0(
		"patientProfiles/subjectProfile-", 
		sub("/", "-", dataVSDIABP$USUBJID), ".pdf"
	)

	# add link in data (for attached table)
	dataVSDIABP$patientProfileLink <- with(dataVSDIABP,
		paste0(
			'<a href="', patientProfilePath, 
			'" target="_blank">', USUBJID, '</a>'
		)
	)

}


## ----computeSummaryStatistics-continuousVariable, eval = requireNamespace("inTextSummaryTable", quietly = TRUE)---------------------------------------------------------

# Specify extra summarizations besides the standard stats
# When the data is summarized,
# the patient profile path are summarized
# as well across patients
# (the paths should be collapsed with: ', ')
if(interactive())
	statsExtraPP <- list(
	    statPatientProfilePath = function(data)	
	      toString(sort(unique(data$patientProfilePath))),
	    statPatientProfileLink = function(data)
	      toString(sort(unique(data$patientProfileLink)))
	)
	
# get default counts + stats with subjects profiles path
statsPP <- c(
    inTextSummaryTable::getStats(x = dataVSDIABP$AVAL, type = "summary"),
	if(interactive())
	    list(
	        patientProfilePath = quote(statPatientProfilePath),
	        patientProfileLink = quote(statPatientProfileLink)
	    )
)

# compute summary table of actual value
summaryTableCont <- inTextSummaryTable::computeSummaryStatisticsTable(
    
    data = dataVSDIABP,
    rowVar = c("AVISIT", "ATPT"),
	var = "AVAL",
	
    labelVars = labelVars,
	
    # statistics of interest
    # for DT output, include columns with patients
    stats = statsPP, 
    # add extra 'statistic': concatenate subject IDs
    statsExtra = if(interactive())	statsExtraPP

)
knitr::kable(head(summaryTableCont, 1))

## ----errorbarClinData, eval = hasPandoc & requireNamespace("inTextSummaryTable", quietly = TRUE)------------------------------------------------------------------------

dataPlot <- subset(summaryTableCont, !isTotal)

errorbarClinData(
	data = dataPlot,
	xVar = "AVISIT",
	colorVar = "ATPT",
	
	# use non-rounded statistics for the plot
	yVar = "statMean", 
	yErrorVar = "statSE", 
	
	# display rounded stats in the hover + n: number of subjects
	hoverVars = c("AVISIT", "ATPT", "n", "Mean", "SE"),
	
	yLab = "Mean", yErrorLab = "Standard Error",
	title = "Diastolic Blood Pressure summary profile by actual visit and and analysis timepoint",
	
	# include lines connecting the error bars
	mode = "markers+lines", 
	
	table = FALSE, labelVars = labelVars,
	pathVar = if(interactive())	"patientProfileLink",
	pathLab = clinUtils::getLabelVar(var = "USUBJID", labelVars = labelVars)

)


## ----boxplot, eval = hasPandoc------------------------------------------------------------------------------------------------------------------------------------------

dataPlot <- subset(dataADaMCDISCP01$ADVS, 
	PARAMCD == "DIABP" & ANL01FL == "Y" &
	AVISIT %in% c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8")
)

# link to patient profiles
if(interactive())
	dataPlot$patientProfilePath <- paste0(
		"patientProfiles/subjectProfile-", 
		sub("/", "-", dataPlot$USUBJID), ".pdf"
	)

boxplotClinData(
	data = dataPlot,
	xVar = "AVISIT", 
	yVar = "AVAL",
	colorVar = "TRTA",
	facetVar = "ATPT",
	title = "Diastolic Blood Pressure distribution by actual visit and analysis timepoint",
	yLab = "Actual value of the Diastolic Blood Pressure parameter (mmHg)",
	labelVars = labelVars,
	pathVar = if(interactive())	"patientProfilePath",
	pathLab = clinUtils::getLabelVar(var = "USUBJID", labelVars = labelVars),
	table = FALSE
)


## ----'objectsList1', results = 'asis', echo = FALSE---------------------------------------------------------------------------------------------------------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Potassium (mmol/L)\n", sep = "")
xList[[1]]

## ----'objectsList2', results = 'asis', echo = FALSE---------------------------------------------------------------------------------------------------------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Sodium (mmol/L)\n", sep = "")
xList[[2]]

## ----lab-profile-loop, results = "asis", eval = hasPandoc---------------------------------------------------------------------------------------------------------------

# consider only restricted set of lab parameters
dataPlot <- subset(dataLB, PARAMCD %in% c("SODIUM", "K"))

# link to patient profiles
if(interactive())
	dataPlot$patientProfilePath <- paste0(
	    "patientProfiles/subjectProfile-", 
	    sub("/", "-", dataPlot$USUBJID), ".pdf"
	)

# 1) create plot+table for each laboratory parameter:
library(plyr) # for ddply
plotsLab <- dlply(dataPlot, "PARAMCD", function(dataLBParam){
      
      paramcd <- unique(dataLBParam$PARAMCD)
      
      scatterplotClinData(
          data = dataLBParam, 
          xVar = "ADY",
          yVar = "LBSTRESN",
          aesPointVar = list(color = "TRTP", fill = "TRTP"),
          aesLineVar = list(group = "USUBJID", color = "TRTP"),
          labelVars = labelVars,
          title = paste("Actual value of", 
              getLabelParamcd(
                  paramcd = paramcd, data = dataLBParam, paramcdVar = "PARAMCD", paramVar = "PARAM"
              )
          ),
          # include link to patient profiles:
          pathVar = if(interactive())	"patientProfilePath",
          table = FALSE, 
          # important: each plot should have an unique ID!
          # for unique relationship of interactivity between plot <-> table
          id = paste("labProfileLoop", paramcd, sep = "-")
      )
      
    })

# include this output in the report:
listLabels <- getLabelParamcd(
    paramcd = names(plotsLab), data = dataLB, paramcdVar = "PARAMCD", paramVar = "PARAM"
)
clinUtils::knitPrintListObjects(
    xList = plotsLab, 
    titles = listLabels, titleLevel = 4
)


## ----watermark----------------------------------------------------------------------------------------------------------------------------------------------------------
# create a file with a 'EXPLORATORY' watermark
file <- tempfile(pattern = "watermark", fileext = ".png")
getWatermark(file = file)

# create plot
barplotClinData(
  data = subset(tableAE, AEDECOD != "Total"),
  xVar = "AEDECOD", 
  yVar = "n",
  yLab = "Number of patients with adverse events",
  textVar = "n",
  labelVars = labelVars,
  # include the watermark
  watermark = file,
  table = FALSE
)

## ----palettes-default-get, results = 'markup'---------------------------------------------------------------------------------------------------------------------------

# display default palettes
colorsDefault <- getOption("clinDataReview.colors")
str(colorsDefault)
shapesDefault <- getOption("clinDataReview.shapes")
str(shapesDefault)


## ----palettes-default-example, eval = hasPandoc-------------------------------------------------------------------------------------------------------------------------

timeProfileIntervalPlot(
    data = dataAE,
    paramVar = "USUBJID",
    # time-variables
    timeStartVar = "ASTDY",
    timeEndVar = "AENDY",
    colorVar = "AESEV",
    timeStartShapeVar = "AESTFLG",
    timeEndShapeVar = "AEENFLG",
    labelVars = labelVars
)


## ----palettes-customGeneral-set-----------------------------------------------------------------------------------------------------------------------------------------

# change palettes for the entire R session
options(clinDataReview.colors = c("gold", "pink", "cyan"))
options(clinDataReview.shapes = clinShapes)


## ----palettes-customGeneral-example, eval = hasPandoc-------------------------------------------------------------------------------------------------------------------

timeProfileIntervalPlot(
    data = dataAE,
    paramVar = "USUBJID",
    # time-variables
    timeStartVar = "ASTDY",
    timeEndVar = "AENDY",
    colorVar = "AESEV",
    timeStartShapeVar = "AESTFLG",
    timeEndShapeVar = "AEENFLG",
    labelVars = labelVars
)


## ----palettes-default-reset---------------------------------------------------------------------------------------------------------------------------------------------

# change palettes for the entire R session
options(clinDataReview.colors = colorsDefault)
options(clinDataReview.shapes = shapesDefault)


## ----sessionInfo, echo = FALSE------------------------------------------------------------------------------------------------------------------------------------------

print(sessionInfo())


