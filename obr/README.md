# obr

[![CRAN status](https://www.r-pkg.org/badges/version/obr)](https://CRAN.R-project.org/package=obr) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/obr)](https://cran.r-project.org/package=obr) [![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/obr)](https://CRAN.R-project.org/package=obr) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

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
| [Monthly profiles](https://obr.uk/monthly-public-finances-briefing/) | The EFO forecast for receipts, spending, and the CGNCR apportioned across the months of the fiscal year, against which each month's outturn is judged | Each EFO |

This package covers all six datasets listed above.

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

## Provenance and reproducibility

Every data-returning function returns an `obr_tbl`: a `data.frame` with attached metadata recording the OBR publication, the publication vintage, the source URL, when the data was retrieved, and the MD5 fingerprint of the underlying spreadsheet. The provenance prints automatically as a header above the data.

```r
library(obr)
get_efo_fiscal()
#> # obr_tbl: <rows> x <cols>
#> # Source:       OBR Economic and Fiscal Outlook, <vintage>
#> # URL:          https://obr.uk/download/<vintage-slug>/
#> # Retrieved:    <timestamp>
#> # File MD5:     <md5 prefix>
#> # Package:      obr <version>
#>
#> <data rows ...>
```

Use `obr_provenance()` to extract the metadata as a list, or `summary()` for the full provenance card. Provenance survives `[` subsetting and is stripped only on explicit `as.data.frame()`. This means an analysis pinned against an `obr_tbl` always carries the audit trail of which OBR publication produced the numbers.

```r
psnb <- get_psnb()
obr_provenance(psnb)
# Returns a list with: publication, vintage, source_url,
# retrieved (POSIXct), file_md5, package_version, notes
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
| `obr_forecast_panel(series)` | Wide real-time panel: rows = forecast vintages, columns = fiscal years |
| `obr_compare_vintages(a, b, what)` | Tidy diff between two vintages of the same EFO table; returns `value_a`, `value_b`, and `revision = value_b - value_a` |
| `obr_actual_vs_forecast(series)` | Joins HFD forecasts against PFD outturn for the same series; returns `value_forecast`, `value_actual`, and `error` |
| `get_forecast_revisions(unit)` | EFO-to-EFO PSNB revisions decomposed into policy, classifications, and underlying components |

### Economic and Fiscal Outlook (EFO)

| Function | Returns |
|---|---|
| `obr_efo_catalogue()` | All 39 detailed-forecast tables this package recognises, with id, section, title, layout, and default `metric_type` / `unit`. No download needed. |
| `get_efo_table(id)` | Generic fetcher: pass a catalogue id (e.g. `"6.13"`, `"1.19"`, `"4.1"`) and get the parsed contents in the standard v0.4 schema. Covers ~35 tables across receipts, expenditure, debt, GDP, labour, inflation, housing, and household balance sheets. |
| `get_efo_fiscal()` | Net borrowing components (Table 6.5). Convenience wrapper over `get_efo_table("6.5")`. |
| `get_efo_economy(measure)` | Quarterly economic projections: `"inflation"`, `"labour"`, or `"output_gap"`. Convenience wrapper over `get_efo_table()`. |
| `list_efo_economy_measures()` | The three measures available via `get_efo_economy()` (no download needed). |
| `get_monthly_profiles()` | The EFO forecast apportioned across the twelve months of the fiscal year (receipts and spending profiles, or the CGNCR breakdown). The reference point for "borrowing so far this year vs the OBR profile" in the run-up to a fiscal event. |

### Welfare Trends Report (WTR)

| Function | Returns |
|---|---|
| `get_welfare_spending()` | Working-age welfare spending split by incapacity/non-incapacity (% GDP, from 1978-79) |
| `get_incapacity_spending()` | Incapacity benefit spending by benefit type (ESA, IB, etc.) as % GDP |
| `get_incapacity_caseloads()` | Combined incapacity caseloads and prevalence since 2008-09 |

### Fiscal Risks and Sustainability Report (FSR)

| Function | Returns |
|---|---|
| `get_pension_projections()` | **Deprecated in 0.5.1.** The OBR restructured the FSR workbooks in July 2026 and the state pension spending scenarios this returned are no longer published in that form. The series now lives in the FSR Chapter 3 workbook (Chart 3.11); see [obr.uk/frs](https://obr.uk/frs/fiscal-risks-and-sustainability-july-2026/). |

### Policy Measures Database (PMD)

| Function | Returns |
|---|---|
| `get_policy_measures()` | Every tax (since 1970) and spending (since 2010) measure scored at a UK fiscal event, with Exchequer effect by fiscal year in GBP million |
| `policy_measures_summary()` | Net Exchequer effect aggregated by fiscal event and fiscal year |

### Fiscal rules

| Function | Returns |
|---|---|
| `obr_fiscal_rules()` | The three Charter for Budget Responsibility rules (stability, investment, welfare cap) with metric, target description, and source Charter version. Numerical headroom is not shipped as a constant (changes at every fiscal event); derive it with `obr_headroom()`. |
| `obr_headroom()` | The current budget surplus path from EFO Table 6.5: the margin against the stability rule in every forecast year. Pass `target_year` to flag the year the rule bites on. |

### Cache management

| Function | What it does |
|---|---|
| `clear_cache()` | Deletes all locally cached OBR files |

All download functions accept `refresh = TRUE` to force a fresh download from the OBR website.

### Standard tidy long schema (v0.4.0)

The EFO, PFD, HFD, WTR, and FSR functions all return the same six columns so outputs from different publications can be `rbind()`'d, joined, and plotted the same way:

| Column | Type | Values |
|---|---|---|
| `period` | character | `"2024-25"`, `"2025Q1"`, etc. |
| `period_type` | character | `"fiscal_year"`, `"quarter"`, `"calendar_year"` |
| `series` | character | The variable name (e.g. `"CPI"`, `"PSNB"`, `"Net borrowing"`) |
| `metric_type` | character | `"level"`, `"yoy_pct"`, `"index"`, `"pct"`, `"pct_pts"` |
| `value` | double | The numeric value |
| `unit` | character | `"gbp_bn"`, `"pct"`, `"index"`, `"count_k"`, etc. |

`get_forecasts()` adds `forecast_date` as a leading column. `get_forecast_revisions()` and `get_policy_measures()` use specialised multi-dimensional schemas (migration to the standard layout queued for a later release).

The `metric_type` column resolves a class of bug from earlier versions where, for example, CPI Index values and CPI year-on-year growth values lived in the same `value` column with no machine-readable distinction. See `vignette("efo-forecasts")` for a walkthrough.

### Vignettes

- `vignette("efo-forecasts")`: working with the v0.4.0 schema, splitting Index from YoY, combining EFO with PFD outturn, comparing two vintages.
- `vignette("vintages")`: pinning to a specific EFO vintage for reproducible analysis.
- `vignette("forecast-revisions")`: decomposing PSNB revisions across vintages.
- `vignette("policy-measures")`: working with the Policy Measures Database.

### Provenance and vintage control

| Function | What it does |
|---|---|
| `obr_provenance(x)` | Extracts the source URL, vintage, retrieval time, and file fingerprint attached to any returned `obr_tbl` |
| `summary(x)` | Prints the full provenance card alongside the structural summary |
| `obr_efo_vintages()` | Lists every EFO published since June 2010, with publication dates and URL slugs |
| `obr_as_of(date)` | Returns the EFO that was current on a given calendar date |
| `obr_pin(vintage)` | Sets a session-wide EFO vintage; `get_efo_*` then defaults to that vintage |
| `obr_unpin()` | Clears any pin set by `obr_pin()` |
| `obr_pinned()` | Returns the currently pinned vintage, or `NULL` |

---

## Examples

### 1. How much did COVID cost the UK?

```r
library(obr)

psnb <- get_psnb()
psnb[psnb$period %in% c("2018-19", "2019-20", "2020-21", "2021-22", "2022-23"),
     c("period", "value", "unit")]
#>       period   value   unit
#>     2018-19    42.5  gbp_bn
#>     2019-20    57.1  gbp_bn
#>     2020-21   317.8  gbp_bn   # ← COVID year
#>     2021-22   144.8  gbp_bn
#>     2022-23    87.6  gbp_bn
```

The UK borrowed £318bn in 2020-21 - roughly seven times the pre-pandemic level - to fund furlough, bounce-back loans, and emergency NHS spending.

---

### 2. How has the OBR's borrowing forecast changed over time?

The OBR first forecast 2024-25 borrowing at **£37bn** (March 2022). By November 2025, that estimate had risen to **£149bn** - four times the original figure.

```r
psnb_fc <- get_forecasts("PSNB")
fc_2425 <- psnb_fc[psnb_fc$period == "2024-25", c("forecast_date", "value")]
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
psnb[psnb$value < 0, c("period", "value")]
#>       period   value
#>     1969-70    -0.5
#>     1970-71    -1.3
#>     1971-72    -0.1
#>     1988-89    -9.0
#>     1989-90    -8.0
#>     1990-91    -0.1
#>     1997-98   -12.7
#>     1998-99   -14.5
#>     1999-00   -17.9
#>     2000-01    -0.5
```

The UK last ran a surplus in 2000-01. In the 24 years since, the government has borrowed every year. Combine with `get_forecasts("PSNB_pct")` to see whether the OBR projects any future surpluses.

---

### 4. Where does government revenue come from?

```r
receipts <- get_receipts()

# Top tax sources in 2023-24
r <- receipts[receipts$period == "2023-24", ]
r <- r[order(-r$value), ]
head(r[, c("series", "value", "unit")], 8)
#>                              series   value    unit
#>   Public sector current receipts   1101.5  gbp_bn
#>                       Income tax    290.4  gbp_bn
#>                              VAT    183.1  gbp_bn
#>    National insurance contributions 182.4  gbp_bn
#>                    Corporation tax   88.4  gbp_bn
#>                       Council tax   44.9  gbp_bn
#>                         Fuel duty   24.5  gbp_bn
#>                    Stamp duties     18.4  gbp_bn
```

Income tax, VAT, and National Insurance together account for around 60% of all government receipts. Breaking this down over time reveals long-run shifts - such as the rising share of income tax as fiscal drag pulls more earners into higher bands.

---

### 5. What does the March 2026 Budget forecast for borrowing?

```r
efo <- get_efo_fiscal()
efo[efo$series == "Net borrowing", c("period", "series", "value", "unit")]
#>      period        series  value    unit
#>     2025-26 Net borrowing  132.7  gbp_bn
#>     2026-27 Net borrowing  115.5  gbp_bn
#>     2027-28 Net borrowing   96.5  gbp_bn
#>     2028-29 Net borrowing   86.0  gbp_bn
#>     2029-30 Net borrowing   63.4  gbp_bn
#>     2030-31 Net borrowing   59.0  gbp_bn
```

The EFO detailed tables also include the full breakdown: current receipts, current expenditure, depreciation, net investment, and net borrowing - enabling you to see exactly how the deficit is projected to narrow.

### 5a. Reproducing an analysis as it would have looked on a past date

Because the OBR revises its forecast at every Budget and Statement, an analysis run today and the same analysis run six months from now can return materially different numbers. The vintage layer pins to a specific EFO so the analysis is reproducible.

```r
# What did the OBR forecast for 2027-28 borrowing in October 2024 vs March 2026?
oct_2024 <- get_efo_fiscal(vintage = "October 2024")
mar_2026 <- get_efo_fiscal(vintage = "March 2026")

oct_2024[oct_2024$series == "Net borrowing" & oct_2024$period == "2027-28", ]
mar_2026[mar_2026$series == "Net borrowing" & mar_2026$period == "2027-28", ]

# Or in one step, with the v0.4.0 helper:
obr_compare_vintages("October 2024", "March 2026", what = "fiscal")
#>   period period_type        series metric_type   unit value_a value_b revision
#>  2027-28 fiscal_year Net borrowing       level gbp_bn    81.7    96.5     14.8
#>  ...

# Or pin once and let every subsequent EFO call use that vintage
obr_pin("October 2024")
get_efo_fiscal()                  # uses October 2024
get_efo_economy("inflation")      # also uses October 2024
obr_unpin()

# Find which EFO was current on a given date
obr_as_of("2024-12-15")
#> [1] "October 2024"
```

---

### 6. Is the UK's incapacity benefits bill rising?

```r
welfare <- get_welfare_spending()
# Working-age incapacity spending, last 10 years
ic <- welfare[welfare$series == "Working-age incapacity benefits spending" &
              welfare$period >= "2014-15", ]
ic[, c("period", "series", "value", "unit")]
#>      period                                   series  value  unit
#>     2014-15 Working-age incapacity benefits spending   1.44   pct
#>     2015-16 Working-age incapacity benefits spending   1.33   pct
#>     ...
#>     2023-24 Working-age incapacity benefits spending   1.78   pct
#>     2024-25 Working-age incapacity benefits spending   2.02   pct
#>     2025-26 Working-age incapacity benefits spending   2.16   pct

# Number of people on incapacity benefits (in thousands)
cases <- get_incapacity_caseloads()
cases[cases$series == "Claimants", c("period", "value", "unit")]
#>     2008-09  2360  count_k
#>     ...
#>     2024-25  3140  count_k
```

Incapacity benefit spending and caseloads have risen sharply since the pandemic - a key driver of welfare reform debate in 2025.

---

### 7. Every tax measure in a Budget

```r
# All tax measures scored from 2025-26 onwards
pmd <- get_policy_measures(type = "tax", since = "2025-26")

# Filter to measures from a specific event, ordered by 2025-26 effect
oct24 <- pmd[grepl("October 2024", pmd$event) &
             pmd$fiscal_year == "2025-26", ]
oct24 <- oct24[order(-oct24$value_mn), ]
head(oct24[, c("measure", "head", "value_mn")])
```

The PMD covers every measure scored at a UK fiscal event: 1970 onwards for tax, 2010 onwards for spending. Combine `search =` and `since =` to pull a thematic time series, e.g. all alcohol-duty measures since 2010.

```r
get_policy_measures(type = "tax", search = "alcohol", since = "2010-11")
```

### 7a. What does the OBR say about the fiscal rules?

```r
obr_fiscal_rules()
```

Returns the three Charter for Budget Responsibility rules in force (stability rule, investment rule, welfare cap), with their target metric, direction of pass, and the source Charter version. Numerical headroom is not shipped as a constant because it changes at every fiscal event; derive it from `get_efo_fiscal()` or consult the EFO press release for the relevant vintage.

---

### 8. Browsing the full EFO databank

```r
# Every detailed-forecast table the package recognises
cat <- obr_efo_catalogue()
nrow(cat)  # 39 (35 currently parseable, 2 cross-references, 2 complex layouts)

# All Debt-section tables
cat[cat$section == "Debt", c("table_id", "title")]
#>   table_id                                                 title
#>      6.11  Public sector net debt year-on-year changes
#>      6.12  Total gross financing
#>      6.13  Composition of public sector net debt
#>      6.14  Composition of public sector net worth
#>      6.16  Central government debt interest by financing component
#>      6.17  Outstanding stocks, debt interest payments and effective rates

# Pull any one
get_efo_table("6.13")             # composition of net debt, % of GDP
get_efo_table("1.19")             # CPI category inflation, quarterly
get_efo_table("6.16")             # debt interest by financing component
get_efo_table("6.5", vintage = "October 2024")
```

The `get_efo_table()` dispatcher routes each id to a layout-specific parser and returns the standard v0.4 schema. The headline wrappers (`get_efo_fiscal()`, `get_efo_economy()`) remain as convenience entry points; under the hood they call the dispatcher.

---

## Limitations

A few things `obr` deliberately does not do, and a few caveats worth knowing.

- **Forecast coverage is partial**. The EFO databank exposes around 70 detailed-forecast tables across the economy, receipts, expenditure, and sustainability sections. `obr` currently surfaces the headline aggregates (Table 6.5), inflation (sheet 1.7), labour (sheet 1.6), and output gap (sheet 1.14). Receipts by tax, expenditure by function, sector wage / productivity / FX series, and the FSR long-run scenarios are on the roadmap for v0.5.0.
- **EFO and PFD use the standard v0.4.0 schema; FRD and PMD do not yet.** `get_forecast_revisions()` and `get_policy_measures()` return their existing multi-dimensional schemas (forecast date x revision component, fiscal event x measure x head). Migration to the v0.4.0 layout is queued for a later release.
- **Vintage table is hardcoded and needs maintenance.** `obr_efo_vintages()` returns a static list of EFOs. When a new EFO publishes, the package needs a release to recognise it as a pinnable vintage. Until then, the dynamic resolver will still find the live file but `obr_pin("November 2026")` will error.
- **The `classify_metric_type()` heuristic is regex-based.** It correctly handles the OBR's current naming conventions (Index, deflator, inflation, growth, rate). New series with unusual names may default to `metric_type = "level"`; the per-measure default supplied by `get_efo_economy()` corrects this for the inflation sheet but may need tuning if OBR adds a new sheet.
- **UK only.** The package wraps OBR publications; it does not cover the US Congressional Budget Office, the Australian Parliamentary Budget Office, or other fiscal-watchdog equivalents.
- **No Python equivalent.** A port to Python is not planned; users wanting the same data in Python should use the OBR's published Excel files directly or contribute a port.
- **Network access is required on first use.** Cached files persist across sessions but `clear_cache()` or a refresh argument forces a re-download. There is no offline mode.

---

## Related packages

| Package | Description |
|---|---|
| [`ons`](https://github.com/charlescoverdale/ons) | UK Office for National Statistics data |
| [`hmrc`](https://github.com/charlescoverdale/hmrc) | HM Revenue & Customs tax data |
| [`boe`](https://github.com/charlescoverdale/boe) | Bank of England data |
| [`fred`](https://github.com/charlescoverdale/fred) | US Federal Reserve (FRED) data |
| [`debtkit`](https://github.com/charlescoverdale/debtkit) | Debt sustainability analysis |
| [`yieldcurves`](https://github.com/charlescoverdale/yieldcurves) | Yield curve fitting (Nelson-Siegel, Svensson) |
| [`inflateR`](https://github.com/charlescoverdale/inflateR) | Inflation adjustment for UK price series |
| [`inflationkit`](https://github.com/charlescoverdale/inflationkit) | Inflation analysis |
| [`greenbook`](https://github.com/charlescoverdale/greenbook) | HM Treasury Green Book CBA primitives |
| [`magentabook`](https://github.com/charlescoverdale/magentabook) | HM Treasury Magenta Book evaluation primitives |

---

## Keeping data up to date

The Public Finances Databank is accessed via a stable URL that the OBR keeps pointed at the latest file. The Historical Official Forecasts Database, EFO, WTR, and FSR functions all use a dynamic URL resolver that probes the OBR's recent fiscal events (most recent first) and uses whichever URL is live, so the package automatically picks up new editions without a code change.

If the resolver cannot find a live URL (for example because of a temporary network outage, or because the OBR has moved to an unfamiliar slug pattern), it falls back to the last-known-good URL and emits an explicit warning rather than failing silently. The returned `obr_tbl` always records the URL that was actually used, so any analysis carries its own audit trail.

The OBR publishes on a roughly predictable schedule: the EFO twice a year (March and October/November), the FSR each summer, the WTR annually. The package's fallback URLs are refreshed at each release; check the [NEWS](https://github.com/charlescoverdale/obr/blob/main/NEWS.md) for the current fallback edition.

---

## Issues

Please report bugs or requests at <https://github.com/charlescoverdale/obr/issues>.

## Keywords

Office for Budget Responsibility, OBR, UK fiscal forecasts, economic forecasts, GDP forecast, inflation forecast, public finances, government borrowing, fiscal policy, UK budget, R package
