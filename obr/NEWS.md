# obr 0.2.4

* Removed non-existent pkgdown URL from DESCRIPTION.

# obr 0.2.3

* Examples now cache to `tempdir()` instead of the user's home directory,
  fixing CRAN policy compliance for `\donttest` examples.
* Cache directory is now configurable via `options(obr.cache_dir = ...)`.

# obr 0.2.2

* Fix README URLs flagged by CRAN incoming checks (301 redirects).
* Add "Databank" to WORDLIST.

# obr 0.2.1

* Update WORDLIST with domain-specific terms (EFO, FSR, WTR, CPIH, etc.)

# obr 0.2.0

## New datasets

### Economic and Fiscal Outlook (EFO)
* `get_efo_fiscal()` — five-year fiscal projections (net borrowing components) from the latest Budget
* `get_efo_economy(measure)` — quarterly economic projections: `"inflation"` (CPI, CPIH, RPI, RPIX), `"labour"` (employment, unemployment, participation), or `"output_gap"`
* `list_efo_economy_measures()` — list available economy measures

### Welfare Trends Report (WTR)
* `get_welfare_spending()` — working-age welfare spending split by incapacity and non-incapacity (% of GDP, from 1978-79)
* `get_incapacity_spending()` — incapacity benefits spending by benefit type (ESA, IB, Invalidity Benefit, Sickness Benefit, SDA) as % of GDP
* `get_incapacity_caseloads()` — combined incapacity benefit caseloads and prevalence since 2008-09

### Fiscal Risks and Sustainability Report (FSR)
* `get_pension_projections()` — 50-year state pension spending projections (% of GDP) under alternative demographic and triple-lock scenarios

# obr 0.1.0

* Initial release
* `get_psnb()`, `get_psnd()`, `get_expenditure()`, `get_receipts()` for Public Finances Databank aggregates
* `get_public_finances()` for all Databank series in tidy long format
* `get_forecasts()` and `list_forecast_series()` for Historical Official Forecasts Database
* `clear_cache()` to remove locally cached files
* Data sourced from the OBR Public Finances Databank and Historical Official Forecasts Database
