Tests and Coverage
================
21 October, 2024 12:26:00

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                          | Coverage (%) |
|:------------------------------------------------|:------------:|
| pannotator                                      |    45.32     |
| [R/run_app.R](../R/run_app.R)                   |     0.00     |
| [R/fct_helpers.R](../R/fct_helpers.R)           |     5.43     |
| [R/app_config.R](../R/app_config.R)             |     7.00     |
| [R/app_ui.R](../R/app_ui.R)                     |    50.94     |
| [R/mod_leaflet_map.R](../R/mod_leaflet_map.R)   |    51.85     |
| [R/mod_360_image.R](../R/mod_360_image.R)       |    60.38     |
| [R/mod_control_form.R](../R/mod_control_form.R) |    67.01     |
| [R/app_server.R](../R/app_server.R)             |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file | n | time | error | failed | skipped | warning | icon |
|:---|---:|---:|---:|---:|---:|---:|:---|
| [test-app.R](testthat/test-app.R) | 1 | 0.169999999984 | 0 | 0 | 0 | 0 |  |
| [test-fct_helpers.R](testthat/test-fct_helpers.R) | 1 | 0.009999999980 | 0 | 0 | 0 | 0 |  |
| [test-golem-recommended.R](testthat/test-golem-recommended.R) | 10 | 0.769999999960 | 0 | 0 | 1 | 0 | \+ |

<details open>
<summary>
Show Detailed Test Results
</summary>

| file | context | test | status | n | time | icon |
|:---|:---|:---|:---|---:|---:|:---|
| [test-app.R](testthat/test-app.R#L2) | app | multiplication works | PASS | 1 | 0.169999999984 |  |
| [test-fct_helpers.R](testthat/test-fct_helpers.R#L2) | fct_helpers | multiplication works | PASS | 1 | 0.009999999980 |  |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L3) | golem-recommended | app ui | PASS | 2 | 0.579999999987 |  |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L13) | golem-recommended | app server | PASS | 4 | 0.069999999978 |  |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L24_L26) | golem-recommended | app_sys works | PASS | 1 | 0.029999999999 |  |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L36_L42) | golem-recommended | golem-config works | PASS | 2 | 0.059999999998 |  |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L72) | golem-recommended | app launches | SKIPPED | 1 | 0.029999999999 | \+ |

| Failed | Warning | Skipped |
|:-------|:--------|:--------|
| !      | \-      | \+      |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                             |
|:---------|:----------------------------------|
| Version  | R version 4.4.0 (2024-04-24 ucrt) |
| Platform | x86_64-w64-mingw32/x64            |
| Running  | Windows 10 x64 (build 19045)      |
| Language | English_Australia                 |
| Timezone | Australia/Sydney                  |

| Package  | Version |
|:---------|:--------|
| testthat | 3.2.1.1 |
| covr     | 3.6.4   |
| covrpage | 0.2     |

</details>
<!--- Final Status : skipped/warning --->
