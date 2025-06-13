medicalrisk
======

medicalrisk is a collection of tools and data for performing patient-level medical 
risk adjustment using administrative data.

The data sets include `icd9cm_charlson` and `icd9cm_elixhauser`, which are data
frames that map ICD-9-CM codes to comorbidities, using the Charlson
and Elixhauser classifications respectively.  The mapping comes from the [paper by 
Quan in 2005](http://www.ncbi.nlm.nih.gov/pubmed/16224307), and uses 
the [CMS ICD-9-CM table.](https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html)  
With a properly formatted source data frame of patients and ICD-9-CM values, 
the `cast` or `dcast` methods can be used to generate comorbidity tables.

The `add_charlson_index_df` method will calculate the Charlson Comorbidity Index 
for each patient.

For data sources that present ICD-9-CM codes as a comma-separated listing, the 
`melt_icd9list` method will convert such data tables into a one-row-per-code format
suitable for `join` or `merge` to the `icd9cm_*` tables.

Data Sources
------

For implementation, testing, and examples several external data sources are used:

* [ICD-9-CM code list from CMS](https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html)
* [Vermont Uniform Hospital Discharge Data Set](http://healthvermont.gov/research/hospital-utilization/RECENT_PU_FILES.aspx)
* [Risk Stratification Index](http://my.clevelandclinic.org/anesthesiology/outcomes-research/risk-stratification-index.aspx)
