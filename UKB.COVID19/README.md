
# UKB.COVID19

<!-- badges: start -->
<!-- badges: end -->

The goal of UKB.COVID19 is to assist with the UK Biobank (UKB) COVID-19 data processing, risk factor association tests and to generate SAIGE GWAS phenotype file.

Note: to access the UKB datasets, you need to register as an UKB researcher. If you are already an approved UKB researcher with a project underway, and wish to receive these datasets for COVID-19 research purposes, you can register to receive these data by logging into the Access Management System (AMS).

## Installation

You can install the released version of UKB.COVID19 from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("UKB.COVID19")
```
OR
```r
install.packages("remotes")
remotes::install_github("bahlolab/UKB.COVID19")
```
OR
```r
install.packages("devtools")
devtools::install_github("bahlolab/UKB.COVID19")
```

## Example

### Risk factor

This is a basic example which shows you how to creat a covariate file with risk factors using UKBB main tab data:
Note: the ukb.tab file must include fields: f.eid, f.31.0.0, f.34.0.0, f.189.0.0, f.21001., f.21000., f.20161.

``` r
library(UKB.COVID19)
covar <- risk_factor(ukb.data=covid_example("sim_ukb.tab.gz"), 
                         ABO.data=covid_example("sim_covid19_misc.txt.gz"),
                         hesin.file=covid_example("sim_hesin.txt.gz"),
                         res.eng=covid_example("sim_result_england.txt.gz"))
```

### COVID-19 susceptibility

This is an example which shows you how to generate a file with COVID-19 susceptibility phenotypes:

``` r
susceptibility <- makePhenotypes(ukb.data=covid_example("sim_ukb.tab.gz"),
                        res.eng=covid_example("sim_result_england.txt.gz"),
                        death.file=covid_example("sim_death.txt.gz"),
                        death.cause.file=covid_example("sim_death_cause.txt.gz"),
                        hesin.file=covid_example("sim_hesin.txt.gz"),
                        hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"),
                        hesin_oper.file=covid_example("sim_hesin_oper.txt.gz"),
                        hesin_critical.file=covid_example("sim_hesin_critical.txt.gz"),
                        code.file=covid_example("coding240.txt.gz"),
                        pheno.type = "susceptibility")
```

### COVID-19 severity

This is an example which shows you how to generate a file with COVID-19 severity phenotypes:

``` r
severity <- makePhenotypes(ukb.data=covid_example("sim_ukb.tab.gz"),
                        res.eng=covid_example("sim_result_england.txt.gz"),
                        death.file=covid_example("sim_death.txt.gz"),
                        death.cause.file=covid_example("sim_death_cause.txt.gz"),
                        hesin.file=covid_example("sim_hesin.txt.gz"),
                        hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"),
                        hesin_oper.file=covid_example("sim_hesin_oper.txt.gz"),
                        hesin_critical.file=covid_example("sim_hesin_critical.txt.gz"),
                        code.file=covid_example("coding240.txt.gz"),
                        pheno.type = "severity")
```

### COVID-19 mortality

This is an example which shows you how to generate a file with COVID-19 mortality phenotype:
Note: the code file is udner /UKB.COVID19/inst/extdata/coding240.txt.gz

``` r
mortality <- makePhenotypes(ukb.data=covid_example("sim_ukb.tab.gz"),
                        res.eng=covid_example("sim_result_england.txt.gz"),
                        death.file=covid_example("sim_death.txt.gz"),
                        death.cause.file=covid_example("sim_death_cause.txt.gz"),
                        hesin.file=covid_example("sim_hesin.txt.gz"),
                        hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"),
                        hesin_oper.file=covid_example("sim_hesin_oper.txt.gz"),
                        hesin_critical.file=covid_example("sim_hesin_critical.txt.gz"),
                        code.file=covid_example("coding240.txt.gz"),
                        pheno.type = "mortality")
```

### Association tests between COVID-19 and comorbidities

This is an example which shows you how to generate a file with all comorbidities in ICD-10 code and how to perform the association tests between comorbidities and COVID-19:
Note: the code file is udner /UKB.COVID19/inst/extdata/ICD10.coding19.txt.gz

``` r
# generate comorbidity file
comorb <- comorbidity_summary(ukb.data=covid_example("sim_ukb.tab.gz"),
                              hesin.file=covid_example("sim_hesin.txt.gz"), 
                              hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"), 
                              ICD10.file=covid_example("ICD10.coding19.txt.gz"),
                              primary = FALSE,
                              Date.start = "16/03/2020")

# association tests 
comorb.asso <- comorbidity_asso(pheno=susceptibility,
                                covariates=covar,
                                cormorbidity=comorb,
                                population="white",
                                cov.name=c("sex","age","bmi","SES","smoke","inAgedCare"),
                                phe.name="pos.neg",
                                ICD10.file=covid_example("ICD10.coding19.txt.gz"))

```

### Sample QC

This is a basic example which shows you how to create sample QC files, and sample inclusion / exclusion lists for specified software:

``` r
sampleQC(ukb.data=covid_example("sim_ukb.tab.gz"), 
         withdrawnFile=covid_example("sim_withdrawn.csv.gz"), 
         ancestry="all", 
         software="SAIGE", 
         outDir=covid_example("results"))
```

### Variant QC

This is a basic example which shows you how to create SNP inclusion lists (SNPID and rsID formats) for given MAF/INFO filters. Also outputs list of SNPs to be used for genetic Relatedness Matrix (GRM) calculations.

``` r
variantQC(snpQcFile=covid_example("sim_ukb_snp_qc.txt.gz"), 
          mfiDir=covid_example("alleleFreqs"), 
          mafFilt=0.001, 
          infoFilt=0.5, 
          outDir=covid_example("results"))
```

### Generate GWAS files

This is a basic example which shows you how to generate files for GWAS Software. SAIGE and Plink currently supported.

``` r
makeGWASFiles(ukb.data=covid_example("sim_ukb.tab.gz"), 
              pheno=phe, 
              covariates=covar, 
              phe.name="hospitalisation", 
              cov.name=NULL, 
              includeSampsFile=NULL, 
              software="SAIGE", 
              outDir=covid_example("results"), 
              prefix="hospitalisation")
```
