# FastRet 1.1.3

*   Fixed issues mentioned by CRAN reviewer after initial submission of FastRet (v1.1.2):
    *   __Comment 1__: *Please do not modify the .GlobalEnv. This is not allowed by the CRAN policies. -> R/patch.R*
    *   __Solution__: Moved `patch.R` from the `R` folder to `misc/scripts`, which is excluded from the package build using `.Rbuildignore`. The file is conditionally sourced by the private function `start_gui_in_devmode()` if available, allowing its use during development without including it in the package.
    *   __Comment 2__: *Please add \value to .Rd files regarding exported methods and explain the functions' results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, document that too, e.g., \value{No return value, called for side effects} or similar) -> Missing Rd-tags: adjust_frm.Rd: \value, analyzeCDNames.Rd: \value, getCDs.Rd: \value, getCDsFor1Molecule.Rd: \value, read_rpadj_xlsx.Rd: \value*
    *   __Solution__: Added `\value` tags to the mentioned `.Rd` files describing the functions' return values.
    *   __Comment 3__: *If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")*
    *   __Solution__: Added *Bonini et al. (2020) <doi:10.1021/acs.analchem.9b05765>* as reference to the description part of the DESCRIPTION file, listing  it as *Related work*. This reference is used in the documentation for `read_retip_hilic_data()` and `ram_cache`. No additional references are used in the package documentation.
*   Added Fadi Fadil as a contributor. Fadi measured the example datasets shipped with FastRet.
*   Added ORCID IDs for contributors as described in [CRAN's checklist for submissions](https://cran.r-project.org/web/packages/submission_checklist.html).

# FastRet 1.1.2

*   Wrapped examples of `read_rp_xlsx()` and `read_rpadj_xlsx()` into `donttest` to prevent note `Examples with CPU time > 2.5 times elapsed time: ...`. By now I'm pretty sure the culprit is the `xlsx` package, which uses a java process for reading the file. Maybe we should switch to openxlsx or readxl in the future.

# FastRet 1.1.1

*   Improved examples of `preprocess_data()` to prevent note `Examples with CPU time > 2.5 times elapsed time: preprocess_data (CPU=2.772, elapsed=0.788)`.

# FastRet 1.1.0

*   Added RAM caching to `getCDs()`

# FastRet 1.0.3

*   Initial CRAN Submission.

    Rejected because the following examples caused at least one of the following notes on the CRAN testing machines: `CPU time > 5s`, `CPU time > 2.5 times elapsed time`. In this context, `CPU time` is calculated as the sum of the measured `user` and `system` times.

    | function             | user      | system | elapsed | ratio     |
    | -------------------- | --------- | ------ | ------- | --------- |
    | check_lm_suitability | **5.667** | 0.248  | 2.211   | **2.675** |
    | predict.frm          | 2.477     | 0.112  | 0.763   | **3.393** |
    | getCDs               | 2.745     | 0.089  | 0.961   | **2.949** |
