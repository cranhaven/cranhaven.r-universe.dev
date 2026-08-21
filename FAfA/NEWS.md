# NEWS

### Version 1.2 - 2026-08-01

* Added `.fafa` project files that restore the dataset, preprocessing state, factor dictionary, and interface settings.
* Added reproducible R, standalone HTML, and PDF workflow reports together with a downloadable audit log.
* Added anonymized diagnostic reports that omit data, variable names, model syntax, file paths, and user details.
* Added session-scoped caches for factor retention, EFA, EGA, CFA, reliability, and existing assumption checks.
* Added English and Turkish interface switching without restarting the application or clearing inputs.
* Added opt-in `shinytest2` browser tests for the critical upload, reverse-scoring, navigation, and language flow.
* Added a visual dimension builder that generates Stratified Alpha strata automatically from saved factor definitions.
* Updated the Item Weighting attribution to Kılıç (2026) and linked the published article DOI.
* Completed Turkish translations for data selection, missing-data terminology, Item Drop Out messages, EFA reporting, Item Weighting, and the application overview.
* Added APA 7 Word reports for project summaries, assumption checks, EFA, CFA, EGA, and measurement invariance outputs.
* Added model-specific Dynamic Fit Index cutoffs to CFA while retaining conventional cutoffs for comparison.
* Relicensed FAfA under AGPL-3 and added visible source-code and warranty notices to the application.
* Integrated the Dynamic Fit Index simulation into FAfA under AGPL-3 and removed the archived external package dependency.
* Fixed correlation labels in the EFA heatmap when "Show correlation values" is selected.
* Added analysis-safe normalization for Turkish and special characters in imported variable names.
* Added automatic reverse-scoring with per-item scale detection and a visible scoring audit table.
* Updated the missingness workflow so excluded variables are removed from the Missingness Map and downstream analyses.
* Prevented empty CSV downloads when outlier removal has not been applied.
* Added a reproducible random seed setting to dataset splitting.
* Added a shared CFA factor-indicator dictionary for reliability, Stratified Alpha, CR, and AVE workflows.
* Expanded the CFA model builder with second-order, bifactor, and complex cross-loading models; multi-select lists now remain open while items are selected.
* Added lavaan syntax import and export, a larger resizable syntax editor, and the chi-square/df fit index.
* Added PNG (300 dpi), SVG, and JPG (300 dpi) downloads for CFA and EGA plots.
* Applied GUI plot settings to downloaded CFA diagrams and added distinct higher-order CFA filenames.
* Bundled an offline interface font and added a Segoe UI-first font fallback stack.
* Replaced the multi-panel scree output with a single standard eigenvalue plot and added PNG (300 dpi) and SVG downloads.
* Restored selectable colour palettes and PNG (300 dpi) and SVG downloads for the EFA correlation heatmap.
* Fixed APA 7 Word downloads so the generated DOCX files open correctly in Microsoft Word.
* Added Lubbe's (2019) permutation parallel analysis for categorical items, with reproducible permutation, reference-quantile, and random-seed settings.
* Added Bootstrap Exploratory Graph Analysis (`bootEGA`) with dimension and item stability results, downloadable tables and plots, and an APA 7 Word report.
* Fixed Measurement Invariance LRT tables so concise model names are shown instead of serialized lavaan objects.
* Added group-wise ordinal category diagnostics and an audited, analysis-only adjacent-category merge when a response category is empty within a group.

### Version 0.5 - 2025-12-12

This release introduces major modules for data preprocessing and item analysis, alongside enhancements to the CFA workflow.

* **Missing Value Module:** Added a new module for comprehensive missing data diagnostics and imputation. Users can now visualize missingness patterns and apply strategies ranging from listwise deletion to advanced imputation methods such as MICE and missForest.
* **Item Drop Out:** Introduced the Item Drop Out module, formerly Item-Rest, to support automated item elimination strategies for optimizing scale properties and factor structures.
* **CFA Module Updates:** Enhanced the CFA Model Builder to simplify covariance and model specification.
* **Reliability Analysis:** Added the Stratified Alpha coefficient for multidimensional reliability analysis.
