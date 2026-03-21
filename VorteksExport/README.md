# RVorteksExport
Export a dataframe to Vorteks (VVE, VDM or VPE)

Example usage when importing the R package from github:
```
install.packages("devtools")
library(devtools)
install_github('lansdon-qualta/RVorteksExport')
library(VorteksExport)

df = data.frame(a=c(1,2,3,4,5), b=c(11,22,33,44,55))
ExportDataframeToVVE(df)
ExportDataframeToVDM(df)
ExportDataframeToVPE(df)
```

Note: A current version of Vorteks is required for this to work! (Later than 3/2021)
