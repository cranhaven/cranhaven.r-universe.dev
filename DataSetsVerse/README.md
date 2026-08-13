# Introduction to DataSetsVerse

The `DataSetsVerse` is a metapackage that brings together a curated collection of R packages containing domain-specific datasets. It includes *time series data, educational metrics, crime records, medical datasets, and oncology research data*.

Designed to provide researchers, analysts, educators, and data scientists with centralized access to structured and well-documented datasets, this metapackage facilitates:

- Reproducible research

- Data exploration

- Teaching applications

across a wide range of domains.

## Installation

To install and activate the `DataSetsVerse` package, use the following:

```r

install.packages("DataSetsVerse")

 
library(DataSetsVerse)

```


## Using the DataSetsVerse() Function

Once the package is loaded, you can call the DataSetsVerse() function to display the list of included dataset packages and their versions:

```r

DataSetsVerse()

```


## Important Note on Detaching Packages

`DataSetsVerse` imports and depends on several subpackages. Therefore, you cannot detach an individual subpackage (like OncoDataSets) while DataSetsVerse is still loaded.

Example of an Error


```r 

# This will raise an error
detach("package:OncoDataSets", unload = TRUE)

# To properly unload a subpackage, you must first detach DataSetsVerse

detach("package:DataSetsVerse", unload = TRUE)

# Now you can safely detach the subpackage
detach("package:OncoDataSets", unload = TRUE)

```


By installing the `DataSetsVerse` package this will attach the following `Datasets Packages` to your R session:

- `timeSeriesDataSets`

- `educationR`

- `crimedatasets`

- `MedDataSets`

- `OncoDataSets`


## Included Packages

### timeSeriesDataSets

A comprehensive collection of time series datasets from multiple domains including:

- Economics

- Finance

- Energy

- Healthcare

Each dataset includes a suffix to denote its structure. Examples:

`AirPassengers_ts`: Monthly airline passengers (1949–1960)

`taylor_30_min_df_ts`: Half-hourly electricity demand

### educationR

- Contains datasets related to:

- Student performance

- Learning methods

- Test scores

- Absenteeism

Each dataset includes a suffix to denote its structure. Examples:

`Develop_tbl_df`: Dev Students: 2-Year & 4-Year College Demographics

`Devmath_tbl_df`: Fall '95 Developmental Math: Failed Student Scores

### crimedatasets

- Focuses exclusively on:

- Crimes and criminal activities

- Criminology

- Socio-economic analysis related to crime

Each dataset includes a suffix to denote its structure. Examples:

`TerrorismGlobal_table`: Global Terrorism Database (GTD) Yearly Summaries

`USATerror_data_df`: Terrorism Incidents in the USA (1968-1974)

### MedDataSets

Medical datasets covering:

- Drug effectiveness

- Vaccine trials

- Survival rates

- Public health and treatments

Each dataset includes a suffix to denote its structure. Examples:

`Aids2_df`: Australian AIDS Survival Data

`Cushings_df`: Diagnostic Tests on Patients with Cushing's Syndrome

### OncoDataSets

Provides rich datasets focused on cancer research, including:

- Survival rates

- Genetic studies

- Biomarkers

Cancer types (melanoma, leukemia, breast, ovarian, lung, etc.)

Each dataset includes a suffix to denote its structure. Examples:

`UKLungCancerDeaths_df`: Lung Cancer Deaths among UK Physicians

`USCancerStats_df`: US Cancer Incidence, Mortality, and Survival Changes

