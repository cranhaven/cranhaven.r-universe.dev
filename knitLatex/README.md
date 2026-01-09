---
output:
  md_document:
    variant: markdown_github
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# Introduction

This package includes several helper functions for working with knitr and latex.
It includes xTab for creating traditional latex tables, lTab for generating
Longtable environments, and sTab for generating a Supertabular environment.
Additionally, this package contains a knitr_sethooks function. knitr_sethooks
serves two purposes. First, it fixes a well-known bug which distorts the
"results='asis'" command when used in conjunction with userdifiend commands.
Second, it provides a com command (\<\<com=TRUE\>\>=) which renders the output
from knitr as a latex command.

In the examples below, the code is shown as it would be provided in a knitr
chunk, with the resultant latex depcited below. If this was an actual latex
file, setting results = 'asis' would eliminate the comment markings (which is
generally what you would want). Additionally, the examples below show only the
basic usages of each function. For a more detailed examination, you should view
the vigenttes or the help documentation. To see a complete working example, see
the documentation provided inst/example (if you are viewing on github).

**Important Notes**

* When passing latex commands to an xTab argument, backslashes must be escaped.
  For example, you need to pass '\\\\hline' to produce '\\hline' in the latex
  document. Similarly you must pass'\\\\\\\\' to produce '\\\\'.
* For all of the examples that follow, we will be using the first ten rows and
  five columns of the mtcars data.frame, which we have saved in the variable
  'cars'.
* The following examples write the latex tables to stdout for demonstration
  purposes. To see an actual .Rnw file, and the resultant pdf file select the
  links below. The pdf file is written to be a knitLatex tutorial in and of
  itself. However, reading the .Rnw file can provide further insight into the
  uses of this package.
    + [example.Rnw](./inst/examples/example.Rnw)
    + [example.pdf](./inst/examples/example.pdf)

# xTab


xTab is a function that produces a standard latex table/tabular environment



## Basics

### Standard xTab table

To produce a latex table, simply pass a matrix or a data.frame to the xTab
function.


```r
xTab(cars)
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

### Labels

Pass a string into the 'label' option. When not set, defaults to NULL.


```r
xTab(cars, label = 'tab:mytable')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \label{tab:mytable}
## \end{center}
## \end{table}
```

### Captions

Place a caption at the top of the table.


```r
xTab(cars, caption.top = 'my table')
## \begin{table}[ht]
## \begin{center}
## \caption{my table}
## \begin{tabular}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

Place a caption at the bottom of the table


```r
xTab(cars, caption.bottom = 'my table')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \caption{my table}
## \end{center}
## \end{table}
```

### Booktabs

Setting booktabs = TRUE sets the defaults of the toprule, midrule, and
bottomrule arguments to \\toprule, \\midrule, and \\bottomrule respectively.
When using booktabs rules, regardless of whether you set booktabs = TRUE or set
them individually, make sure to include \\usepackage{booktabs} in your latex
document. When booktabs is not set, xTab looks for the value of
kLat.xTab.booktabs, then kLat.booktabs, then defaults to FALSE.


```r
xTab(cars, booktabs = TRUE)
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \toprule
## mpg & cyl & disp & hp & drat \\
## \midrule
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \bottomrule
## \end{tabular}
## \end{center}
## \end{table}
```

If any of those options are explicitly set, the booktabs value has no effect. 


```r
xTab(cars, booktabs = TRUE, midrule = '\\hline')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \toprule
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \bottomrule
## \end{tabular}
## \end{center}
## \end{table}
```

### Table Position

If not set, xTAb will look for the value of kLat.xTab.position. If not set,
defaults to 'ht'


```r
xTab(cars, position = '!htbp')
## \begin{table}[!htbp]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

## Headers

By default, xTab will use the column names for the head of the table(as
demonstrated in the above examples). To customize the table head simply pass in
the appropriate latex to the 'head' argument. The values of toprule and midrule
are still used and should not be set in the head argument. If you do not want
them included in your custom header, set either or both to NULL


```r
xTab(cars,
     head = 'col1 & col 2 & col3 & \\eta & col5 \\\\')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
## col1 & col 2 & col3 & \eta & col5 \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

Pass in NULL to avoid a table head. In the case of a NULL head, toprule and
midrule will not be used.


```r
xTab(cars, head = NULL)
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

To preserve the top and midrules, pass an empty string to head. Often, people
want a toprule with no head or midrule. In that case, pass an empty string into
the head argument and NULL into midule. You can then use the default toprule
value (as depicted below), explicitly set toprule, or set booktabs = TRUE to set
the toprule and bottomrule simultaneously.


```r
lTab(cars, head = '', midrule = NULL)
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## 
## 
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

## Rows

### Rownames

When including row names in a table, by default xTab will use an empty column
name for the 'rownames' column. When rows is not set, xTab looks for the value
of kLat.xTab.rows, then klat.rows, then defaults to false.


```r
xTab(cars, rows = TRUE)
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
##  & mpg & cyl & disp & hp & drat \\
## \hline
## Mazda RX4 & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Mazda RX4 Wag & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Datsun 710 & 22.8 & 4 & 108.0 &  93 & 3.85 \\
## Hornet 4 Drive & 21.4 & 6 & 258.0 & 110 & 3.08 \\
## Hornet Sportabout & 18.7 & 8 & 360.0 & 175 & 3.15 \\
## Valiant & 18.1 & 6 & 225.0 & 105 & 2.76 \\
## Duster 360 & 14.3 & 8 & 360.0 & 245 & 3.21 \\
## Merc 240D & 24.4 & 4 & 146.7 &  62 & 3.69 \\
## Merc 230 & 22.8 & 4 & 140.8 &  95 & 3.92 \\
## Merc 280 & 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

### Rownames with custom header

When providing a custom head with rows set to TRUE, remember to account for the
extra column produced by the row names


```r
xTab(cars,
     rows =  TRUE,
     head = 'rows & col1 & col2 & col3 & \\eta & col5 \\\\')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
## rows & col1 & col2 & col3 & \eta & col5 \\
## \hline
## Mazda RX4 & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Mazda RX4 Wag & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Datsun 710 & 22.8 & 4 & 108.0 &  93 & 3.85 \\
## Hornet 4 Drive & 21.4 & 6 & 258.0 & 110 & 3.08 \\
## Hornet Sportabout & 18.7 & 8 & 360.0 & 175 & 3.15 \\
## Valiant & 18.1 & 6 & 225.0 & 105 & 2.76 \\
## Duster 360 & 14.3 & 8 & 360.0 & 245 & 3.21 \\
## Merc 240D & 24.4 & 4 & 146.7 &  62 & 3.69 \\
## Merc 230 & 22.8 & 4 & 140.8 &  95 & 3.92 \\
## Merc 280 & 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

### Row separator

Any arbitrary latex command can be inserted between each row, but the most
common are \\hline and \\midrule. To use \\midrule, \\usepackage{booktabs} must
be declared in the preamble of the latex document, but booktabs = TRUE **does
not** need to be set on the table. When rowsep is not set, xTab looks for the
value of kLat.xTab.rowsep, then kLat.rowsep, then defaults to an empty string.


```r
xTab(cars, rowsep = '\\hline')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\\hline
## 21 & 6 & 160 & 110 & 3.9 \\\hline
## 22.8 & 4 & 108 & 93 & 3.85 \\\hline
## 21.4 & 6 & 258 & 110 & 3.08 \\\hline
## 18.7 & 8 & 360 & 175 & 3.15 \\\hline
## 18.1 & 6 & 225 & 105 & 2.76 \\\hline
## 14.3 & 8 & 360 & 245 & 3.21 \\\hline
## 24.4 & 4 & 146.7 & 62 & 3.69 \\\hline
## 22.8 & 4 & 140.8 & 95 & 3.92 \\\hline
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```


```r
xTab(cars, rowsep = '\\midrule')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\\midrule
## 21 & 6 & 160 & 110 & 3.9 \\\midrule
## 22.8 & 4 & 108 & 93 & 3.85 \\\midrule
## 21.4 & 6 & 258 & 110 & 3.08 \\\midrule
## 18.7 & 8 & 360 & 175 & 3.15 \\\midrule
## 18.1 & 6 & 225 & 105 & 2.76 \\\midrule
## 14.3 & 8 & 360 & 245 & 3.21 \\\midrule
## 24.4 & 4 & 146.7 & 62 & 3.69 \\\midrule
## 22.8 & 4 & 140.8 & 95 & 3.92 \\\midrule
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

## Columns

### Column alginment

Explicilty set the column definitions. If this is set, colsep will have no effect
and you must handle column separation within this declaration. Defaults to 'r' for
numeric vector columns and 'l' for character vector columns.


```r
xTab(cars, coldef ='rlc|l|p{5cm}')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rlc|l|p{5cm}}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

### Column separator

Place any arbitrary latex between each column. Will have no effect if coldef is
set.


```r
xTab(cars, colsep = '|')
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{r|r|r|r|r}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \hline
## \end{tabular}
## \end{center}
## \end{table}
```

# sTab

sTab is a function that produces a supertabular environment

## Basics

### Standard sTab table

To produce a latex table, simply pass a matrix or a data.frame to the sTab
function.


```r
sTab(cars)
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

### Labels

Pass a string into the 'label' option. When not set, defaults to NULL.


```r
sTab(cars, label = 'tab:mytable')
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## \label{tab:mytable}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

### Captions

Place a caption at the top of the table.


```r
sTab(cars, caption.top = 'my table')
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \topcaption{my table}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

Place a caption at the bottom of the table


```r
sTab(cars, caption.bottom = 'my table')
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \bottomcaption{my table}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

### Booktabs

Setting booktabs = TRUE sets the defaults of the toprule, midrule, and
bottomrule arguments to \\toprule, \\midrule, and \\bottomrule respectively.
When using booktabs rules, regardless of whether you set booktabs = TRUE or set
them individually, make sure to include \\usepackage{booktabs} in your latex
document. When booktabs is not set, sTab looks for the value of
kLat.sTab.booktabs, then kLat.booktabs, then defaults to FALSE.


```r
sTab(cars, booktabs = TRUE)
## \tablehead{\toprule
## mpg & cyl & disp & hp & drat \\
## \midrule}
## \tabletail{\bottomrule}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

If any of those options are explicitly set, the booktabs value has no effect. 


```r
sTab(cars, booktabs = TRUE, midrule = '\\hline')
## \tablehead{\toprule
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\bottomrule}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

## Headers

### Head

By default, sTab will use the column names for the head of the table(as
demonstrated in the above examples). The head argument is diplayed at the top of
the table on each page that the table spans. To customize the table head simply
pass in the appropriate latex to the 'head' argument. The values of toprule and
midrule are still used and should not be set in the head argument. If you do not
want them included in your custom header, set either or both to NULL


```r
sTab(cars,
     head = 'col1 & col 2 & col3 & \\eta & col5 \\\\')
## \tablehead{\hline
## col1 & col 2 & col3 & \eta & col5 \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

Pass in NULL to avoid a table head. In the case of a NULL head, toprule and
midrule will not be used.


```r
sTab(cars, head = NULL)
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

To preserve the top and midrules, pass an empty string to head. Often, people
want a toprule with no head or midrule. In that case, pass an empty string into
the head argument and NULL into midule. You can then use the default toprule
value (as depicted below), explicitly set toprule, or set booktabs = TRUE to set
the toprule and bottomrule simultaneously.


```r
sTab(cars, head = '', midrule = NULL)
## \tablehead{\hline
## 
## }
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

### FirstHead

In a supertabular environment, it is possible to present a different first head
(i.e. header on first page of table only).


```r
sTab(cars,
     firsthead = 'f1 & f2 & f3 & f4 & f5 \\\\')
## \tablefirsthead{\hline
## f1 & f2 & f3 & f4 & f5 \\
## \hline}
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

**Important Note**

As demonstrated in the above example, both head and firsthead use the toprule
and midrule commands by default. If you desire to use different commands for the
head and firsthead (or if you want one, but not both to use a top and
midrule), you must set both toprule and midrule to NULL and manually insert the
commands into head and firsthead as shown below examples.


```r
sTab(cars, toprule = NULL, midrule = NULL,
     firsthead = '\\toprule\nf1 & f2 & f3 & f4 & f5 \\\\\nmidrule',
     head = '\\hline\n col1 & col2 & col3 & cll4 & col5 \\\\\n\\hline')
## \tablefirsthead{
## \toprule
## f1 & f2 & f3 & f4 & f5 \\
## midrule
## }
## \tablehead{
## \hline
##  col1 & col2 & col3 & cll4 & col5 \\
## \hline
## }
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```


```r
sTab(cars, toprule = NULL, midrule = NULL,
     firsthead = '\\toprule\nf1 & f2 & f3 & f4 & f5 \\\\\nmidrule',
     head = '\\toprule')
## \tablefirsthead{
## \toprule
## f1 & f2 & f3 & f4 & f5 \\
## midrule
## }
## \tablehead{
## \toprule
## }
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

## Rows

### Rownames

When including row names in a table, by default sTab will use an empty column
name for the 'rownames' column. When rows is not set, sTab looks for the value
of kLat.sTab.rows, then klat.rows, then defaults to false.


```r
sTab(cars, rows = TRUE)
## \tablehead{\hline
##  & mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## Mazda RX4 & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Mazda RX4 Wag & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Datsun 710 & 22.8 & 4 & 108.0 &  93 & 3.85 \\
## Hornet 4 Drive & 21.4 & 6 & 258.0 & 110 & 3.08 \\
## Hornet Sportabout & 18.7 & 8 & 360.0 & 175 & 3.15 \\
## Valiant & 18.1 & 6 & 225.0 & 105 & 2.76 \\
## Duster 360 & 14.3 & 8 & 360.0 & 245 & 3.21 \\
## Merc 240D & 24.4 & 4 & 146.7 &  62 & 3.69 \\
## Merc 230 & 22.8 & 4 & 140.8 &  95 & 3.92 \\
## Merc 280 & 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

### Rownames with custom header

When providing a custom head with rows set to TRUE, remember to account for the
extra column produced by the row names


```r
sTab(cars,
     rows =  TRUE,
     head = 'rows & col1 & col2 & col3 & \\eta & col5 \\\\')
## \tablehead{\hline
## rows & col1 & col2 & col3 & \eta & col5 \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## Mazda RX4 & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Mazda RX4 Wag & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Datsun 710 & 22.8 & 4 & 108.0 &  93 & 3.85 \\
## Hornet 4 Drive & 21.4 & 6 & 258.0 & 110 & 3.08 \\
## Hornet Sportabout & 18.7 & 8 & 360.0 & 175 & 3.15 \\
## Valiant & 18.1 & 6 & 225.0 & 105 & 2.76 \\
## Duster 360 & 14.3 & 8 & 360.0 & 245 & 3.21 \\
## Merc 240D & 24.4 & 4 & 146.7 &  62 & 3.69 \\
## Merc 230 & 22.8 & 4 & 140.8 &  95 & 3.92 \\
## Merc 280 & 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

### Row separator

Any arbitrary latex command can be inserted between each row, but the most
common are \\hline and \\midrule. To use \\midrule, \\usepackage{booktabs} must
be declared in the preamble of the latex document, but booktabs = TRUE **does
not** need to be set on the table. When rowsep is not set, sTab looks for the
value of kLat.sTab.rowsep, then kLat.rowsep, then defaults to an empty string.


```r
sTab(cars, rowsep = '\\hline')
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\\hline
## 21 & 6 & 160 & 110 & 3.9 \\\hline
## 22.8 & 4 & 108 & 93 & 3.85 \\\hline
## 21.4 & 6 & 258 & 110 & 3.08 \\\hline
## 18.7 & 8 & 360 & 175 & 3.15 \\\hline
## 18.1 & 6 & 225 & 105 & 2.76 \\\hline
## 14.3 & 8 & 360 & 245 & 3.21 \\\hline
## 24.4 & 4 & 146.7 & 62 & 3.69 \\\hline
## 22.8 & 4 & 140.8 & 95 & 3.92 \\\hline
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```


```r
sTab(cars, rowsep = '\\midrule')
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rrrrr}
## 21 & 6 & 160 & 110 & 3.9 \\\midrule
## 21 & 6 & 160 & 110 & 3.9 \\\midrule
## 22.8 & 4 & 108 & 93 & 3.85 \\\midrule
## 21.4 & 6 & 258 & 110 & 3.08 \\\midrule
## 18.7 & 8 & 360 & 175 & 3.15 \\\midrule
## 18.1 & 6 & 225 & 105 & 2.76 \\\midrule
## 14.3 & 8 & 360 & 245 & 3.21 \\\midrule
## 24.4 & 4 & 146.7 & 62 & 3.69 \\\midrule
## 22.8 & 4 & 140.8 & 95 & 3.92 \\\midrule
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

## Columns

### Column alginment

Explicilty set the column definitions. If this is set, colsep will have no effect
and you must handle column separation within this declaration. Defaults to 'r' for
numeric vector columns and 'l' for character vector columns.


```r
sTab(cars, coldef ='rlc|l|p{5cm}')
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{rlc|l|p{5cm}}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

### Column separator

Place any arbitrary latex between each column. Will have no effect if coldef is
set.


```r
sTab(cars, colsep = '|')
## \tablehead{\hline
## mpg & cyl & disp & hp & drat \\
## \hline}
## \tabletail{\hline}
## \begin{center}
## \begin{supertabular}{r|r|r|r|r}
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{supertabular}
## \end{center}
```

# lTab

lTab is a function for creating a longtable environment

## Basics

### Standard lTab table

To produce a latex table, simply pass a matrix or a data.frame to the lTab
function.


```r
lTab(cars)
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

### Labels

Pass a string into the 'label' option. When not set, defaults to NULL.


```r
lTab(cars, label = 'tab:mytable')
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \label{tab:mytable}
## \end{longtable}
## \end{center}
```

### Captions

The lTab environment can place a caption in the head, firsthead, foot, or last
foot. When placing captions in the firsthad or lastfoot, it is important to set
these options (even if they are empty strings) or strange bugs can occur.


```r
lTab(cars, caption.head = 'my caption in head')
## \begin{center}
## \begin{longtable}{rrrrr}
## \caption{my caption in head}\\
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```


```r
lTab(cars,
     firsthead = 'f1 & f2 & f3 & f4 & f5 \\\\',
     caption.firsthead = 'my caption in firsthead')
## \begin{center}
## \begin{longtable}{rrrrr}
## \caption{my caption in firsthead}\\
## \hline
## f1 & f2 & f3 & f4 & f5 \\
## \hline
## \endfirsthead
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```


```r
lTab(cars, caption.foot = 'my caption in foot')
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \caption{my caption in foot}\\
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```


```r
lTab(cars,
     lastfoot = '\\hline',
     caption.lastfoot = 'my caption in last foot')
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## \caption{my caption in last foot}\\
## \hline
## \endlastfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

### Booktabs

Setting booktabs = TRUE sets the defaults of the toprule, midrule, and
bottomrule arguments to \\toprule, \\midrule, and \\bottomrule respectively.
When using booktabs rules, regardless of whether you set booktabs = TRUE or set
them individually, make sure to include \\usepackage{booktabs} in your latex
document. When booktabs is not set, lTab looks for the value of
kLat.lTab.booktabs, then kLat.booktabs, then defaults to FALSE.


```r
lTab(cars, booktabs = TRUE)
## \begin{center}
## \begin{longtable}{rrrrr}
## \toprule
## mpg & cyl & disp & hp & drat \\
## \midrule
## \endhead
## \bottomrule
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

If any of those options are explicitly set, the booktabs value has no effect. 


```r
lTab(cars, booktabs = TRUE, midrule = '\\hline')
## \begin{center}
## \begin{longtable}{rrrrr}
## \toprule
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \bottomrule
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

## Headers

### Head

By default, lTab will use the column names for the head of the table(as
demonstrated in the above examples). The head argument is displayed at the top of
the table on each page that the table spans. To customize the table head simply
pass in the appropriate latex to the 'head' argument. The values of toprule and
midrule are still used and should not be set in the head argument. If you do not
want them included in your custom header, set either or both to NULL


```r
lTab(cars,
     head = 'col1 & col 2 & col3 & \\eta & col5 \\\\')
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## col1 & col 2 & col3 & \eta & col5 \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

Pass in NULL to avoid a table head. In the case of a NULL head, toprule and
midrule will not be used.


```r
lTab(cars, head = NULL)
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

To preserve the top and midrules, pass an empty string to head. Often, people
want a toprule with no head or midrule. In that case, pass an empty string into
the head argument and NULL into midule. You can then use the default toprule
value (as depicted below), explicitly set toprule, or set booktabs = TRUE to set
the toprule and bottomrule simultaneously.


```r
lTab(cars, head = '', midrule = NULL)
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## 
## 
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

### FirstHead

In a supertabular environment, it is possible to present a different first head
(i.e. header on first page of table only).


```r
lTab(cars,
     firsthead = 'f1 & f2 & f3 & f4 & f5 \\\\')
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## f1 & f2 & f3 & f4 & f5 \\
## \hline
## \endfirsthead
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

**Important Note**

As demonstrated in the above example, both head and firsthead use the toprule
and midrule commands by default. If you desire to use different commands for the
head and firsthead (or if you want one, but not both to use a top and
midrule), you must set both toprule and midrule to NULL and manually insert the
commands into head and firsthead as shown below examples.


```r
lTab(cars, toprule = NULL, midrule = NULL,
     firsthead = '\\toprule\nf1 & f2 & f3 & f4 & f5 \\\\\nmidrule',
     head = '\\hline\n col1 & col2 & col3 & cll4 & col5 \\\\\n\\hline')
## \begin{center}
## \begin{longtable}{rrrrr}
## 
## \toprule
## f1 & f2 & f3 & f4 & f5 \\
## midrule
## 
## \endfirsthead
## 
## \hline
##  col1 & col2 & col3 & cll4 & col5 \\
## \hline
## 
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```


```r
lTab(cars, toprule = NULL, midrule = NULL,
     firsthead = '\\toprule\nf1 & f2 & f3 & f4 & f5 \\\\\nmidrule',
     head = '\\toprule')
## \begin{center}
## \begin{longtable}{rrrrr}
## 
## \toprule
## f1 & f2 & f3 & f4 & f5 \\
## midrule
## 
## \endfirsthead
## 
## \toprule
## 
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

## Rows

### Rownames

When including row names in a table, by default lTab will use an empty column
name for the 'rownames' column. When rows is not set, lTab looks for the value
of kLat.lTab.rows, then klat.rows, then defaults to false.


```r
lTab(cars, rows = TRUE)
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
##  & mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## Mazda RX4 & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Mazda RX4 Wag & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Datsun 710 & 22.8 & 4 & 108.0 &  93 & 3.85 \\
## Hornet 4 Drive & 21.4 & 6 & 258.0 & 110 & 3.08 \\
## Hornet Sportabout & 18.7 & 8 & 360.0 & 175 & 3.15 \\
## Valiant & 18.1 & 6 & 225.0 & 105 & 2.76 \\
## Duster 360 & 14.3 & 8 & 360.0 & 245 & 3.21 \\
## Merc 240D & 24.4 & 4 & 146.7 &  62 & 3.69 \\
## Merc 230 & 22.8 & 4 & 140.8 &  95 & 3.92 \\
## Merc 280 & 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

### Rownames with custom header

When providing a custom head with rows set to TRUE, remember to account for the
extra column produced by the row names


```r
lTab(cars,
     rows =  TRUE,
     head = 'rows & col1 & col2 & col3 & \\eta & col5 \\\\')
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## rows & col1 & col2 & col3 & \eta & col5 \\
## \hline
## \endhead
## \hline
## \endfoot
## Mazda RX4 & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Mazda RX4 Wag & 21.0 & 6 & 160.0 & 110 & 3.90 \\
## Datsun 710 & 22.8 & 4 & 108.0 &  93 & 3.85 \\
## Hornet 4 Drive & 21.4 & 6 & 258.0 & 110 & 3.08 \\
## Hornet Sportabout & 18.7 & 8 & 360.0 & 175 & 3.15 \\
## Valiant & 18.1 & 6 & 225.0 & 105 & 2.76 \\
## Duster 360 & 14.3 & 8 & 360.0 & 245 & 3.21 \\
## Merc 240D & 24.4 & 4 & 146.7 &  62 & 3.69 \\
## Merc 230 & 22.8 & 4 & 140.8 &  95 & 3.92 \\
## Merc 280 & 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

### Row separator

Any arbitrary latex command can be inserted between each row, but the most
common are \\hline and \\midrule. To use \\midrule, \\usepackage{booktabs} must
be declared in the preamble of the latex document, but booktabs = TRUE **does
not** need to be set on the table. When rowsep is not set, lTab looks for the
value of kLat.lTab.rowsep, then kLat.rowsep, then defaults to an empty string.


```r
lTab(cars, rowsep = '\\hline')
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\\hline
## 21 & 6 & 160 & 110 & 3.9 \\\hline
## 22.8 & 4 & 108 & 93 & 3.85 \\\hline
## 21.4 & 6 & 258 & 110 & 3.08 \\\hline
## 18.7 & 8 & 360 & 175 & 3.15 \\\hline
## 18.1 & 6 & 225 & 105 & 2.76 \\\hline
## 14.3 & 8 & 360 & 245 & 3.21 \\\hline
## 24.4 & 4 & 146.7 & 62 & 3.69 \\\hline
## 22.8 & 4 & 140.8 & 95 & 3.92 \\\hline
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```


```r
lTab(cars, rowsep = '\\midrule')
## \begin{center}
## \begin{longtable}{rrrrr}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\\midrule
## 21 & 6 & 160 & 110 & 3.9 \\\midrule
## 22.8 & 4 & 108 & 93 & 3.85 \\\midrule
## 21.4 & 6 & 258 & 110 & 3.08 \\\midrule
## 18.7 & 8 & 360 & 175 & 3.15 \\\midrule
## 18.1 & 6 & 225 & 105 & 2.76 \\\midrule
## 14.3 & 8 & 360 & 245 & 3.21 \\\midrule
## 24.4 & 4 & 146.7 & 62 & 3.69 \\\midrule
## 22.8 & 4 & 140.8 & 95 & 3.92 \\\midrule
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

## Columns

### Column alginment

Explicilty set the column definitions. If this is set, colsep will have no effect
and you must handle column separation within this declaration. Defaults to 'r' for
numeric vector columns and 'l' for character vector columns.


```r
lTab(cars, coldef ='rlc|l|p{5cm}')
## \begin{center}
## \begin{longtable}{rlc|l|p{5cm}}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```

### Column separator

Place any arbitrary latex between each column. Will have no effect if coldef is
set.


```r
lTab(cars, colsep = '|')
## \begin{center}
## \begin{longtable}{r|r|r|r|r}
## \hline
## mpg & cyl & disp & hp & drat \\
## \hline
## \endhead
## \hline
## \endfoot
## 21 & 6 & 160 & 110 & 3.9 \\
## 21 & 6 & 160 & 110 & 3.9 \\
## 22.8 & 4 & 108 & 93 & 3.85 \\
## 21.4 & 6 & 258 & 110 & 3.08 \\
## 18.7 & 8 & 360 & 175 & 3.15 \\
## 18.1 & 6 & 225 & 105 & 2.76 \\
## 14.3 & 8 & 360 & 245 & 3.21 \\
## 24.4 & 4 & 146.7 & 62 & 3.69 \\
## 22.8 & 4 & 140.8 & 95 & 3.92 \\
## 19.2 & 6 & 167.6 & 123 & 3.92 \\
## \end{longtable}
## \end{center}
```
