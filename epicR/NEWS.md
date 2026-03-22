# epicR 1.0.0

## Major changes

* Modularized C++ engine into separate compilation units (`model_input.cpp`, `model_output.cpp`, `model_events.cpp`).
* Replaced hardcoded input values with JSON-based configuration files (`inst/config/config_canada.json`, `inst/config/config_us.json`).
* Added multi-jurisdiction support (Canada and US).
* Added `simulate()` function that manages sessions automatically, replacing manual `init_session()` / `run()` / `terminate_session()` workflow.
* Removed MI, stroke, and heart failure modules from the model.

## New features

* `simulate()` gains `n_agents` parameter for setting population size.
* `simulate()` gains `return_events` option for returning individual-level event data.
* Real-time progress bar during simulation runs.
* User-level configuration management with `get_user_config()`, `set_user_config()`, and `reset_user_configs()`.
* Validation functions now support jurisdiction-specific comparisons.
* New `validate_exacerbation()` function consolidates exacerbation validation by sex.
* Added exacerbation counts by calendar time and sex to extended outputs.
* Added cost tracking outputs (`cumul_cost_gold_ctime`, `cumul_qaly_gold_ctime`) to extended results.

## Bug fixes

* Fixed out-of-bounds array access in `n_exac_by_medication_class` (array sized `[3]` but `exac_status` can be 1--4).
* Fixed event code inconsistencies and exacerbation counts per GOLD stage.
* Fixed crash when using `simulate()` with `return_events = TRUE`.
* Fixed auto-termination logic to prevent premature session cleanup.

## Other improvements

* Reduced installed package size (compiled shared library reduced from ~6.5 MB to ~0.4 MB).
* Improved memory management in the C++ engine.
* Internal functions (`init_session()`, `run()`, `terminate_session()`) are no longer exported.
* Improved documentation and vignettes.
* Added comprehensive test suite.