## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(qacBase)
library(dplyr)
library(knitr)
library(kableExtra)

## ---- echo=FALSE--------------------------------------------------------------
d <- data.frame(Function=c("[**recodes**](../reference/recodes.html)",
                         "[**standardize**](../reference/standardize.html)",
                         "[**normalize**](../reference/normalize.html)"),
                Description=c("`recodes()` provides a simple way to recode 
                              the values of numeric, character, or factor
                              variables. See the [vignette](recodes.html) 
                              for examples.",
                              
                              "`standardize()` transforms all the 
                              numeric variables in a data frame to same
                              mean and standard deviation (mean=0 sd=1,
                              by default),
                              without modifying character, factor, or dummy
                              coded variables.",
                              
                               "`normalize()` transforms all the 
                              numeric variables in a data frame to same
                              range of values ([0, 1] by default). Again,
                              character and factor variables are left
                              unchanged."))

kbl(d) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")


## ---- echo=FALSE--------------------------------------------------------------
d <- data.frame(Function=c("[**contents**](../reference/contents.html)", 
                           "[**df_plot**](../reference/df_plot.html)",
                           "[**barcharts**](../reference/barcharts.html)",
                           "[**histograms**](../reference/histograms.html)",
                           "[**densities**](../reference/densities.html)"
                          ),
                Description=c("`contents()` provides a comprehensive 
                              description of a data frame. The output is
                              **much** more detailed than that provided by
                              the base `summary.data.frame()` function, 
                              and is easier to read and understand. This
                              function should be your first stop when 
                              looking at a new dataset.",
                              
                              "`dfPlot()` helps you visual a data frame. 
                              Variable are grouped by type (numeric, integer,
                              character, factor, date) and color coded. The
                              percent of missing data for each variable is
                              also displayed, along with the total number
                              of variables and cases.",
                              
                              "`barcharts()` provides bar charts of all the
                              character or factor variables in a data frame,
                              within a single graph.",
                              
                              "`histograms()` provides histograms of the all
                              quantitative variables in a data frame,
                              within a single graph.",
                              
                              "`densities()` provides density charts of all
                              the quantitative variables in a data frame,
                              within a single graph."
                              ))

kbl(d) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")


## ---- echo=FALSE--------------------------------------------------------------
d <- data.frame(Function=c(
  "[**qstats**](../reference/qstats.html)",
  "[**univariate_plot**](../reference/univariate_plot.html)",
  "[**scatter**](../reference/scatter.html)",
  "[**cor_plot**](../reference/cor_plot.html)",
  "[**groupdiff**](../reference/groupdiff.html)"),
                
   Description=c("`qstats()` allows you to easily calculate any number
                 of descriptive statistics (e.g., n, mean, sd) for a
                 quantiatative variable. The results can be broken down by the
                 levels of one of more categorical variables
                 (groups). Any function that produces a single number can
                  be used. See the [vignette](qstats.html) for examples.",
                  
                   "`univariatePlot()` provides a detailed
                   visualization of the distribution of values in 
                   a quantiative variable. The graph contains a
                   histrogram, jittered dot plot, density curve,
                   and boxplot, Annotations provide statistics
                   such as *n*, *mean*, *sd*, *median*, *min*,
                              *max*, *skew*, and *outliers*.",
                              
                   "`scatter()` generates a scatter plot and line
                   of best fit with 95% confidence interval
                   displaying the relationship between two
                   quantiative variables. Annotations include
                   the slope, correlation coefficient
                   (r), r-squared, and p_value. Oultiers
                   (determinded by studentized residuals) are
                   flagged. Optionally, marginal distributions
                   (histograms, boxplots, density curves, 
                    violin plots) can be added to the margins of the plot.",
                    
                    "`corplot()` plots the correlations among
                 numeric variables in a data frame. Variables
                 can be sorted to place variables with similar correlation
                 patterns together.",
                 "`groupdiff()` compares groups on a quantitative
                 outcome using either a parametric (ANOVA) or nonparametric
                 (Kruskal-Wallis) test. Summary statistics, pair-wise
                 group differences (post-hoc comparisons), and plots are
                 provided."
                              ))

kbl(d) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")


## ---- echo=FALSE--------------------------------------------------------------
d <- data.frame(Function=c("[**tab**](../reference/tab.html)", 
                          "[**crosstab**](../reference/crosstab.html)"
                          ),
                Description=c("`tab()` generates a frequency table and 
                              bar chart for a categorical
                              variable. There are many options including
                              sorting categories by frequency, adding
                              cumulative frequencies and percents, and
                              combining infrequent categories into an 
                              'Other' category. See the
                              [vignette](tab.html) for examples.",
                              
                              "`crosstab()` generates a two-way frequency
                              table from two categorical variables. There 
                              are many options including cell, row, and column
                              percents, plotting options, and a chi-square 
                              test of independence. See the
                              [vignette](crosstab.html) for examples."
                              ))

kbl(d) %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")


