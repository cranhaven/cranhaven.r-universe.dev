# obr

[![CRAN status](https://www.r-pkg.org/badges/version/obr)](https://CRAN.R-project.org/package=obr) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/obr)](https://cran.r-project.org/package=obr) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

An R package for accessing data published by the [Office for Budget Responsibility](https://obr.uk/) (OBR).

## What is the OBR?

The Office for Budget Responsibility is the UK's independent fiscal watchdog. It was created in 2010 by the coalition government to provide an independent check on the government's fiscal plans - a role previously held by HM Treasury itself.

The distinction matters. HM Treasury is the government department that *sets* fiscal policy: it decides tax rates, spending plans, and how much the government intends to borrow. The OBR's job is to *scrutinise* those plans independently, producing its own economic and fiscal forecasts that are not influenced by ministers. Think of it as the equivalent of the Bank of England's independence for monetary policy, but applied to public finances.

In practice, this means the OBR publishes forecasts at each Budget and Autumn Statement showing whether it thinks the government is on track to meet its own fiscal rules - and it has no political incentive to be optimistic.

---

## Installation

```r
install.packages("obr")

# Or install the development version from GitHub
# install.packages("devtools")
devtools::install_github("charlescoverdale/obr")
```

---

## Key OBR datasets

| Dataset | What it contains | Frequency |
|---|---|---|
| [Public Finances Databank](https://obr.uk/public-finances-databank-2019-20/) | Outturn data on PSNB, PSND, receipts, and expenditure back to 1946-47 | Monthly |
| [Historical Official Forecasts Database](https://obr.uk/data/) | Every forecast the OBR (and pre-OBR Treasury) has published for key fiscal and economic variables since 1970 | Each fiscal event |
| [Economic and Fiscal Outlook](https://obr.uk/efo/economic-and-fiscal-outlook-march-2026/) | The flagship publication at each Budget - detailed projections across 5 years | Each Budget / Autumn Statement |
| [Fiscal Sustainability Report](https://obr.uk/frs/fiscal-risks-and-sustainability-july-2025/) | Long-run projections over 50 years, covering ageing, health, and debt dynamics | Annual |
| [Welfare Trends Report](https://obr.uk/wtr/welfare-trends-report-october-2024/) | Spending trends across the benefits system | Annual |

This package covers all five datasets listed above.

---

## Why does this package exist?

All OBR data is freely available at [obr.uk](https://obr.uk). The problem is *how* it is available: as Excel files with non-standard layouts, inconsistent headers, and footnote-laden sheets that require significant wrangling before they are usable in R.

For example, the Public Finances Databank has column headers buried in row 4 of the spreadsheet, data starting in row 8, and trailing footnote numbers appended to column names. The Historical Forecasts Database stores forecasts as a vintage matrix - rows are fiscal events, columns are fiscal years - which needs reshaping into a long format before it can be plotted or analysed.

This package handles all of that automatically. One function call returns a clean, tidy data frame. Data is cached locally so subsequent calls are instant.

```r
# Without this package
path <- "~/Downloads/Public_finances_databank_March_2025.xlsx"
raw  <- readxl::read_excel(path, sheet = "Aggregates (£bn)", col_names = FALSE)
series_names <- as.character(unlist(raw[4, ]))
# ... 30 more lines of wrangling ...

# With this package
library(obr)
get_psnb()
```

---

## Functions

### Public Finances Databank

| Function | Returns |
|---|---|
| `get_psnb()` | Annual Public Sector Net Borrowing in £bn |
| `get_psnd()` | Annual Public Sector Net Debt in £bn |
| `get_expenditure()` | Annual Total Managed Expenditure in £bn |
| `get_receipts()` | Tax receipts broken down by type, in £bn |
| `get_public_finances()` | All aggregate series in tidy long format |

### Historical Forecasts Database

| Function | Returns |
|---|---|
| `list_forecast_series()` | Data frame of available series (no download needed) |
| `get_forecasts(series)` | Every OBR forecast for a given series, in tidy long format |

### Economic and Fiscal Outlook (EFO)

| Function | Returns |
|---|---|
| `get_efo_fiscal()` | Five-year net borrowing component projections from the latest Budget (£bn, annual) |
| `get_efo_economy(measure)` | Quarterly economic projections: `"inflation"`, `"labour"`, or `"output_gap"` |
| `list_efo_economy_measures()` | Available economy measures (no download needed) |

### Welfare Trends Report (WTR)

| Function | Returns |
|---|---|
| `get_welfare_spending()` | Working-age welfare spending split by incapacity/non-incapacity (% GDP, from 1978-79) |
| `get_incapacity_spending()` | Incapacity benefit spending by benefit type (ESA, IB, etc.) as % GDP |
| `get_incapacity_caseloads()` | Combined incapacity caseloads and prevalence since 2008-09 |

### Fiscal Risks and Sustainability Report (FSR)

| Function | Returns |
|---|---|
| `get_pension_projections()` | 50-year state pension spending projections (% GDP) under demographic and triple-lock scenarios |

### Cache management

| Function | What it does |
|---|---|
| `clear_cache()` | Deletes all locally cached OBR files |

All download functions accept `refresh = TRUE` to force a fresh download from the OBR website.

---

## Examples

### 1. How much did COVID cost the UK?

```r
library(obr)

psnb <- get_psnb()
psnb[psnb$year %in% c("2018-19", "2019-20", "2020-21", "2021-22", "2022-23"), ]
#>       year  psnb_bn
#>  2018-19     42.5
#>  2019-20     57.1
#>  2020-21    317.8   # ← COVID year
#>  2021-22    144.8
#>  2022-23     87.6
```

The UK borrowed £318bn in 2020-21 - roughly seven times the pre-pandemic level - to fund furlough, bounce-back loans, and emergency NHS spending.

---

### 2. How has the OBR's borrowing forecast changed over time?

The OBR first forecast 2024-25 borrowing at **£37bn** (March 2022). By November 2025, that estimate had risen to **£149bn** - four times the original figure.

```r
psnb_fc <- get_forecasts("PSNB")
fc_2425 <- psnb_fc[psnb_fc$fiscal_year == "2024-25", c("forecast_date", "value")]
fc_2425
#>    forecast_date   value
#>     March 2022    36.5
#>  November 2022    84.3
#>     March 2023    85.4
#>  November 2023    84.6
#>     March 2024    87.2
#>   October 2024   127.5
#>     March 2025   137.3
#>  November 2025   149.5
```

The `get_forecasts()` function returns every published forecast across all fiscal events, making it straightforward to visualise forecast drift and assess how fiscal plans have evolved.

---

### 3. Has the UK ever run a surplus - and how likely is it to again?

```r
psnb <- get_psnb()

# Years with a surplus (negative PSNB = receipts exceed spending)
psnb[psnb$psnb_bn < 0, ]
#>       year  psnb_bn
#>  1969-70    -0.5
#>  1970-71    -1.3
#>  1971-72    -0.1
#>  1988-89    -9.0
#>  1989-90    -8.0
#>  1990-91    -0.1
#>  1997-98   -12.7
#>  1998-99   -14.5
#>  1999-00   -17.9
#>  2000-01    -0.5
```

The UK last ran a surplus in 2000-01. In the 24 years since, the government has borrowed every year. Combine with `get_forecasts("PSNB_pct")` to see whether the OBR projects any future surpluses.

---

### 4. Where does government revenue come from?

```r
receipts <- get_receipts()

# Top tax sources in 2023-24
r <- receipts[receipts$year == "2023-24", ]
r <- r[order(-r$value), ]
head(r[, c("series", "value")], 8)
#>                              series   value
#>   Public sector current receipts   1101.5
#>                       Income tax    290.4
#>                              VAT    183.1
#>    National insurance contributions 182.4
#>                    Corporation tax   88.4
#>                       Council tax   44.9
#>                         Fuel duty   24.5
#>                    Stamp duties     18.4
```

Income tax, VAT, and National Insurance together account for around 60% of all government receipts. Breaking this down over time reveals long-run shifts - such as the rising share of income tax as fiscal drag pulls more earners into higher bands.

---

### 5. What does the March 2026 Budget forecast for borrowing?

```r
efo <- get_efo_fiscal()
efo[efo$series == "Net borrowing", ]
#>   fiscal_year        series value_bn
#>       2025-26 Net borrowing    132.7
#>       2026-27 Net borrowing    115.5
#>       2027-28 Net borrowing     96.5
#>       2028-29 Net borrowing     86.0
#>       2029-30 Net borrowing     63.4
#>       2030-31 Net borrowing     59.0
```

The EFO detailed tables also include the full breakdown: current receipts, current expenditure, depreciation, net investment, and net borrowing - enabling you to see exactly how the deficit is projected to narrow.

---

### 6. Is the UK's incapacity benefits bill rising?

```r
welfare <- get_welfare_spending()
# Working-age incapacity spending, last 10 years
ic <- welfare[welfare$series == "Working-age incapacity benefits spending" &
              welfare$year >= "2014-15", ]
ic
#>        year                                      series value
#>     2014-15 Working-age incapacity benefits spending  1.44
#>     2015-16 Working-age incapacity benefits spending  1.33
#>     ...
#>     2023-24 Working-age incapacity benefits spending  1.78
#>     2024-25 Working-age incapacity benefits spending  2.02
#>     2025-26 Working-age incapacity benefits spending  2.16

# Number of people on incapacity benefits
cases <- get_incapacity_caseloads()
cases[cases$series == "Share of working age population", ]
#>  2008-09   Share of working age population  6.80
#>  ...
#>  2023-24   Share of working age population  6.82
```

Incapacity benefit spending and caseloads have risen sharply since the pandemic - a key driver of welfare reform debate in 2025.

---

### 7. What happens to the state pension bill as the UK ages?

```r
proj <- get_pension_projections()

# Central demographic projection: pension spending rises from 5% to 7.7% of GDP
central <- proj[proj$scenario_type == "Demographic scenarios" &
                proj$scenario == "Central projection", ]
head(central[, c("fiscal_year", "pct_gdp")], 5)
#>   fiscal_year pct_gdp
#>       2023-24    4.56
#>       2024-25    4.95
#>       2025-26    5.06
#>       2026-27    5.13
#>       2027-28    5.05

tail(central[, c("fiscal_year", "pct_gdp")], 5)
#>   fiscal_year pct_gdp
#>       2069-70    7.73
#>       2070-71    7.82
#>       2071-72    7.77
#>       2072-73    7.66
#>       2073-74    7.65
```

The OBR's central projection has the state pension rising from 4.6% of GDP today to 7.7% by 2073-74 as the UK population ages. The FSR also publishes scenarios for higher/lower life expectancy and different triple-lock uprating assumptions.

---

## Related packages

This package is part of a family of R packages for UK economic and fiscal data. They share a consistent interface - named functions, tidy data frames, local caching - and are designed to work together. All outputs join cleanly on date columns.

| Package | What it covers |
|---|---|
| [`ons`](https://github.com/charlescoverdale/ons) | ONS data (GDP, inflation, unemployment, wages, trade, house prices, population) |
| [`boe`](https://github.com/charlescoverdale/boe) | Bank of England data (Bank Rate, SONIA, gilt yields, exchange rates, mortgage rates) |
| [`hmrc`](https://github.com/charlescoverdale/hmrc) | HMRC tax receipts, corporation tax, stamp duty, R&D credits, and tax gap data |
| [`readecb`](https://github.com/charlescoverdale/readecb) | European Central Bank data (policy rates, HICP, exchange rates, yield curves) |
| [`readoecd`](https://github.com/charlescoverdale/readoecd) | OECD data (GDP, unemployment, inflation, trade across 38 member countries) |
| [`fred`](https://github.com/charlescoverdale/fred) | US Federal Reserve (FRED) data (800,000+ economic time series) |
| [`inflateR`](https://github.com/charlescoverdale/inflateR) | Adjust values for inflation using CPI or GDP deflator data |

---

## Keeping data up to date

The two core datasets - the Public Finances Databank and Historical Official Forecasts Database - are accessed via stable generic URLs that the OBR keeps pointed at the latest file. They update automatically whenever you call `refresh = TRUE`.

The EFO, WTR, and FSR functions work differently. The OBR publishes each edition at a new URL containing the publication date (e.g. `march-2026-economic-and-fiscal-outlook-...`), and does not maintain a generic redirect. This means the URLs in the package are hardcoded to a specific edition: calls to `get_efo_fiscal()` will always return the March 2026 Budget forecasts until the package is updated, even after the OBR publishes a new edition.

The OBR publishes on a roughly predictable schedule - the EFO twice a year (March and October/November), the FSR and WTR once a year each. This package will be updated to track each new edition, meaning **2–3 patch releases per year**. Check the [NEWS](https://github.com/charlescoverdale/obr/blob/main/NEWS.md) to see which edition each function currently reflects. If you need data from the absolute latest publication before a package update is available, download directly from [obr.uk](https://obr.uk).

---

## Issues

Please report bugs or requests at <https://github.com/charlescoverdale/obr/issues>.

## Keywords

Office for Budget Responsibility, OBR, UK fiscal forecasts, economic forecasts, GDP forecast, inflation forecast, public finances, government borrowing, fiscal policy, UK budget, R package
