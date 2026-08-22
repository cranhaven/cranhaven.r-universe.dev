# lapop R package

These are helper functions to wrangle labels and produce visualizations of 'AmericasBarometer' data following LAPOP Lab's editorial guidelines.

🔗 **Package website:** [https://lapop-central.github.io/lapop/](https://lapop-central.github.io/lapop/)

---

## 🛠️ Installation

To install the package from CRAN in your console, run:

```r
install.packages(lapop)
```

For the most recent version, please use:
```r
pak::pak("lapop-central/lapop")

### OR

devtools::install_github("lapop-central/lapop", force = TRUE)
```

---

## ⚙️ Workflow: 'AmericasBarometer' Variable & Value Labels

For the full online guide, see:

<a href="https://lapop-central.github.io/lapop/articles/lapop-r-labels.html" target="_blank">📖 LAPOP Data Guide for R Users</a>

### 1. Data Structure

'AmericasBarometer' datasets are distributed in Stata `.dta` format with multilingual metadata (question wording and response options) embedded as attributes. These support cross-national and longitudinal comparability.

### 2. Preferred Loading Method

Use:

```r
readstata13::read.dta13()
```

to preserve the full metadata structure.

Other methods such as `haven::read_dta()` or `rio::import()` may fail to import the STATA attributes.

### 3. Variable Labels (Question Wording)

- Stored in the `expansion.fields` attribute.
- Use `lpr_extract_notes()` to convert into a tidy data frame.
- Assign preferred language labels with `lpr_set_attr()` using the appropriate `noteid`.

### 4. Value Labels (Response Options)

- Stored in the `label.table` attribute.
- Use `lpr_set_ros()` to assign these response labels in English, Spanish, or Portuguese.

---

## 🎨 Workflow: 'AmericasBarometer' Data Visualization

<a href="https://lapop-central.github.io/lapop/articles/lapop-visualization.html" target="_blank">📖 LAPOP Visualization Guide</a>

1. Load the package in R:

   ```r
   library(lapop)
   ```

2. LAPOP Lab fonts is automatically loaded, yet you can also manually if needed:

   ```r
   lapop_fonts()
   ```

3. Apply the 'AmericasBarometer' design effects with:

   ```r
   lpr_data()
   ```

4. Choose the appropriate `lpr` graph type:
   - Histograms: `lpr_hist()`
   - Cross-country comparison: `lpr_cc()`
   - Time series: `lpr_ts()`
   - Breakdown by covariates: `lpr_mover()`  

<p>5. Store the output in an R object.</p>

   - File names: .csv and graphics files should have the same name. Their names should be in the following standard format: CountryYear_DVcode(s)_IVcode(s)_graphtype.extension
   - Examples:
      
      - mex21_countfair1_hist.csv
      - hnd_b4_ts.svg
      - ab23_vic1ext_pais_cc.svg
   - There will be some cases that do not easily fit this standard. Use your best judgment.

6. Use the corresponding `lapop` plotting function to produce the visualization:

   - Examples: `lapop_hist()`, `lapop_cc()`, `lapop_ts()`, etc.

<p>7. Export the figure to your machine with:</p>

   ```r
   lapop_save()
   ```
---

## 🤝 Workflow: Contributing to the `lapop` R Package
Create a new branch for your feature or bug fix.


Document exported functions with roxygen2 comments.
Add or update tests in tests/testthat/.

Commit your changes and push the branch to your fork.
Submit a pull request with a clear description of the change.
1. **Fork** the repository and clone it to your local machine.
2. **Create a new branch** for your feature or fix.
3. Add or modify code in `R/` folder.
4. Document the function with roxygen2 comments.
5. Run `devtools::document()` to update `NAMESPACE` and `man/`.
6. Run `devtools::test()` and `devtools::check()`
7. Commit your changes and push the branch to your fork.
8. Submit a **pull request** with a clear description of the change.

If you find a bug, please consider contributing to the lapop package — we spent all our money on coffee, empanadas, and data cleaning.

---
[![](https://cranlogs.r-pkg.org/badges/lapop)](https://cran.r-project.org/package=lapop)
