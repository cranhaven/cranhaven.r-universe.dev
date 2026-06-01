## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rcldf)
library(dplyr)
library(ggplot2)
library(patchwork)

theme_set(theme_classic())

## ----eval = FALSE-------------------------------------------------------------
# library(devtools)
# install_github("SimonGreenhill/rcldf", dependencies = TRUE)
# library(rcldf)

## ----eval = FALSE-------------------------------------------------------------
# library(rcldf)
# 
# # Load from a local directory:
# ds <- cldf('/path/to/dir/wals_1a_cldf')
# 
# # or load from a specific metadatafile:
# ds <- cldf('/path/to/dir/wals_1a_cldf/StructureDataset-metadata.json')
# 
# # or load from zenodo:
# df <- cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip?download=1")
# 
# # or load from github:
# df <- cldf('https://github.com/lexibank/abvd')

## -----------------------------------------------------------------------------
library(rcldf)
ds <- cldf(system.file("extdata/huon", package="rcldf"))

# this dataset has 4 tables:
ds 

# more details:
summary(ds)

## -----------------------------------------------------------------------------
get_foreign_keys(ds)


schema(ds)

## -----------------------------------------------------------------------------
names(ds$tables)

# let's look at the languages -- 
head(ds$tables$LanguageTable)

# and the parameters - in this case the words in the wordlist
head(ds$tables$ParameterTable)

# and finally, the lexical items themselves:
ds$tables$ValueTable

## -----------------------------------------------------------------------------
ds <- cldf(system.file("extdata/huon", package="rcldf"), load_bib=TRUE)

# or if you loaded the CLDF without sources the first time you can add them now:
ds <- read_bib(ds)

ds$sources

## -----------------------------------------------------------------------------
as.cldf.wide(ds, 'FormTable')

## -----------------------------------------------------------------------------
get_table_from('LanguageTable', system.file("extdata/huon", package="rcldf"))

## -----------------------------------------------------------------------------
print(ds$citation)

## -----------------------------------------------------------------------------
get_details(system.file("extdata/huon", package="rcldf"))

## ----eval=FALSE---------------------------------------------------------------
# glott <- load_glottolog()
# conc <- load_concepticon()
# clts <- load_clts()
# dplace <- load_dplace()

## -----------------------------------------------------------------------------
datasets()

## -----------------------------------------------------------------------------
semitic <- load_dataset('kitchensemitic')

## -----------------------------------------------------------------------------
get_cache_dir()

## ----eval=FALSE---------------------------------------------------------------
# set_cache_dir('/path/somewhere')

## ----eval=FALSE---------------------------------------------------------------
# usethis::edit_r_environ()
# # now add
# # RCLDF_CACHE_DIR=<where you want the data saved>

## -----------------------------------------------------------------------------
list_cache_files()

## -----------------------------------------------------------------------------
cldf(list_cache_files()[1, 'Path'])

## ----eval=FALSE---------------------------------------------------------------
# df <- cldf("https://zenodo.org/record/7844558/files/grambank/grambank-v1.0.3.zip?download=1", cache_dir="~/data/grambank")

## -----------------------------------------------------------------------------
grambank <- cldf("https://zenodo.org/records/7844558/files/grambank/grambank-v1.0.3.zip?download=1")
grambank

# or: 
# grambank <- load_dataset('grambank')

## -----------------------------------------------------------------------------
summary(grambank)

## -----------------------------------------------------------------------------
plot_languages(grambank, color_by='Macroarea')

## -----------------------------------------------------------------------------
languages <- grambank$tables$LanguageTable |>
    select(ID, Name, Macroarea, Latitude, Longitude)    
    # only selecting some columns above to make it easier to see    
languages    

## -----------------------------------------------------------------------------
grambank$tables$ParameterTable |> filter(ID=='GB028')

## -----------------------------------------------------------------------------
plot_parameter(grambank, parameter='GB028')

## -----------------------------------------------------------------------------
values <- grambank$tables$ValueTable |> 
    filter(Parameter_ID=='GB028') |>
    select(ID, Language_ID, Parameter_ID, Value, Source)


## -----------------------------------------------------------------------------
dplace <- cldf("https://github.com/D-PLACE/dplace-cldf")
summary(dplace)

## -----------------------------------------------------------------------------
dplace$tables$ValueTable |> filter(Var_ID=='EA113')

## -----------------------------------------------------------------------------
# get languages from DPLACE
dplanguages <- dplace$tables$LanguageTable |> select(ID, Glottocode)

# get values for EA113 and merge with language information
ea113 <- dplace$tables$ValueTable |> 
    filter(Var_ID=='EA113') |> 
    select(Soc_ID, Value) |>
    left_join(dplanguages, join_by(Soc_ID==ID))

# rename `Value` to EA113
ea113 <- ea113 |> mutate(EA113=Value) |> select(Glottocode, EA113)

head(ea113)

## -----------------------------------------------------------------------------
gb028 <- values |> 
    mutate(Glottocode=Language_ID, GB028=Value) |>
    select(Glottocode, GB028)
head(gb028)

## -----------------------------------------------------------------------------
df <- gb028 |> inner_join(ea113) |> na.omit()
head(df)

## -----------------------------------------------------------------------------
p1 <- ggplot(df, aes(x=GB028)) + geom_histogram(stat='count')
p2 <- ggplot(df, aes(x=EA113)) + geom_histogram(stat='count')

p1 / p2   #  patchwork

## -----------------------------------------------------------------------------
tab <- table(df$GB028, df$EA113)
tab

chisq.test(tab)

