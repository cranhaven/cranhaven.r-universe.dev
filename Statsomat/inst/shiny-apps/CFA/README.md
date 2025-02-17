
# Introduction
The Statsomat/CFA app is a web-based
application for automated Confirmatory Factor Analysis (CFA) based mainly on the R package `lavaan` and created with the Shiny
technology. The Statsomat/CFA app is hosted on [shinyapps.io](https://www.shinyapps.io/) and 
is one of several apps which can be accessed via the webpage of *Statsomat* (see https://statsomat.com), a nonprofit portal with the aim of developing, 
collecting and maintaining open-source apps for automated data analysis, interpretation and explanation. You can also access the app directly here https://statsomat.shinyapps.io/Confirmatory-factor-analysis/. 


# Installation 
There is no need to install the Statsomat/CFA app since it runs in the browser. If you really want to run it locally, then clone the repository and run the app from the project folder: 

```
shiny::runApp()
```

Before running the app locally, please consider to install required packages (check them in `global.R` and `report_kernel.Rmd`). 


# Example Usage
The dataset HolzingerSwineford1939.csv extracted from the R package `lavaan` is contained in the repository and can be used as an example. Select only the variables `x1-x9` for a CFA. Type this model into the *Type Your Model* text area block, generate the report and finally download the report. 
```
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
```

Follow also the *Instructions* described directly on the webpage of the app https://statsomat.shinyapps.io/Confirmatory-factor-analysis/. 

# Functionality
The user uploads its data as a CSV file, types the CFA model in `lavaan` model syntax directly in 
the browser and generates a PDF report. The report contains a data-driven interpretation and explanation of the 
CFA in plain English. The R code for the generation of the tables and graphics is included in the report and 
enables locally reproducibles results. The current version supports only approximately continuous data. Other restrictions to the data may apply. 


# Tests 
The app was calibrated and tested by using the HolzingerSwineford1939 dataset contained in the R package `lavaan`
and (simulated) data cases from literature. 



# Community 
1) Contribute to the software:
You are welcome to improve and extend the functionality of the app. If you want to make a pull request, please check that you can run test cases locally without any errors or warnings. Please consider to test your changes also on [shinyapps.io](https://www.shinyapps.io/). While uploading, ignore the `Error in eval(x, envir = envir)`, it is a non-fatal error, also related to related to https://github.com/rstudio/packrat/issues/385 and https://github.com/rstudio/rsconnect/issues/429.  

2) Report issues or problems with the software:
Please open an issue in this repository to report any bugs. 

3) Seek support:
We try to answer all questions in reasonable time  but general support is limited. 
