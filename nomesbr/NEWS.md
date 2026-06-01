# nomesbr 0.0.9

* fix authors DESCRIPTION

# nomesbr 0.0.8

* fixes package doc vignette rebuilding error in windows olrdel as per https://cran.rstudio.org/web/checks/check_results_nomesbr.html

# nomesbr 0.0.7

* fixes to package documentation as requested for cran resubmission

# nomesbr 0.0.6

* submit to cran

# nomesbr 0.0.5

* fix s2 column created with limpar_nomes
* update tabular_problemas... cond name espaco_Til ...
* added basic unit test for tabular_problemas
* improved doc citing Lucas reference

# nomesbr 0.0.4

* improved cleaning step - 'remove repeated de de, da da...'

# nomesbr 0.0.3

* fixes for some regexes part positions - DE LAS
* improved unit test for obter_dic_nomes_proprios_compostos
* added function simplifica_PARTICULAS_AGNOMES_PATENTES capturing in new column detected regex before replacing
* included use_github_action 'check-standard'
* improved minimally global timing message for limpar_nomes

# nomesbr 0.0.2

* pkgdown site
* added some abbreviations to regexes (JR, FL, DR, SGTO)
* moved patent cleaning from cleaning to simplification function 
* adjusted download_nomes_compostos , created simple test to check if on cache

# nomesbr 0.0.1

* initial version wrapped as a package
