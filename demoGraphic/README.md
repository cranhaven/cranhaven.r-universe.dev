# demoGraphic

The goal of demoGraphic is providing five main function cont_table(), cat_table(), demo_table(), smd_plot(), mydocx(). These are used to calculate the mean, standard deviation, pvalue was performanced on t-test for continuous variables, the contigency table and propotion, chi square test or fisher exact test were performanced to get p value, the standardized difference between two groups. 

## Installation

You can install the released version of demoGraphic from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("demoGraphic")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Create data set

set.seed(2018)

group <-round(abs(rnorm(500)*10),0) %% 2

cont_1 <-round(abs(rnorm(500)*10),0)
cont_2 <-round(abs(rnorm(500)*50),0)
cont_3 <-round(abs(rnorm(500)*30),0)

cat_binary <-round(abs(rnorm(500)*10),0) %% 2
cat_multi_1 <-round(abs(rnorm(500)*10),0) %% 3
cat_multi_2 <-round(abs(rnorm(500)*10),0) %% 4
cat_multi_3 <-round(abs(rnorm(500)*10),0) %% 5


data<-data.frame(group, cont_1, cont_2, cont_3, cat_binary, cat_multi_1, cat_multi_2, cat_multi_3)
data$group <- factor(data$group, levels = c(0,1), labels = c("Unexposed","Exposed"))
data[,c(6,7,8)] <- lapply(data[,c(6,7,8)], as.factor)


# ===================================================
# 1. testing continuous variable

# 1a. Test one variable

t <- cont_table("cont_2","group", data)


t$smd_table # smd table
t$demo_table # demographic table


p <- smd_plot(t$smd_table) # plot smd 
p


# write smd table with file name smd_table.docx and save it in directory
mydocx(t$smd_table,"smd_table") 


#=====================================

# 1b. Test multiple variables

var <- c("cont_1","cont_2","cont_3")


t <- cont_table(var,"group", data)

t$smd_table # smd table
t$demo_table # demographic table


p <- smd_plot(t$smd_table) # plot smd 
p

# write smd table with file name smd_table.docx and save it in directory
mydocx(t$smd_table,"smd_table") 


# ===================================================
# 2. testing catgorical variable

# 2a. Test one variable

t <- cat_table("cat_binary","group", data)


t$smd_table # smd table
t$demo_table # demographic table


p <- smd_plot(t$smd_table) # plot smd 
p

# write smd table with file name smd_table.docx and save it in directory
mydocx(t$smd_table,"smd_table") 


#=================================


# 2b. Test multiple variables

var <- c("cat_binary","cat_multi_1","cat_multi_2","cat_multi_3")

t <- cat_table(var,"group", data)

t$smd_table # smd table
t$demo_table # demographic table


p <- smd_plot(t$smd_table) # plot smd 
p

# write smd table with file name smd_table.docx and save it in directory
mydocx(t$smd_table,"smd_table") 



#======================================

# 3. Test demo_table function (continuous and categorical variables together)
# 3a. One variable

t <- demo_table("cat_binary","group", data)

t$smd_table # smd table
t$demo_table # demographic table


p <- smd_plot(t$smd_table) # plot smd 
p

# write smd table with file name smd_table.docx and save it in directory
mydocx(t$smd_table,"smd_table") 


#===================================

# 3b. Multiple variables


var <- c("cat_binary","cont_1","cont_2","cont_3","cat_multi_1","cat_multi_2","cat_multi_3")

t <- demo_table(var,"group", data)

t$smd_table # smd table
t$demo_table # demographic table


p <- smd_plot(t$smd_table) # plot smd 
p


# write smd table with file name smd_table.docx and save it in directory
mydocx(t$demo_table,"demo_table") 


```

