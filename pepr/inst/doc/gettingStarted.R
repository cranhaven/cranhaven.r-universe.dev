## ----collapse=TRUE------------------------------------------------------------
library('pepr')
branch = "master"
projectConfigFile = system.file("extdata",
	paste0("example_peps-", branch),
	"example_basic",
	"project_config.yaml",
	package="pepr")

## -----------------------------------------------------------------------------
p = pepr::Project(file=projectConfigFile)

## -----------------------------------------------------------------------------
p

## ----collapse=TRUE------------------------------------------------------------
sampleTable(p)

## -----------------------------------------------------------------------------
config(p)

