# BET 0.6.0

- Added reusable asymptotic calibration for bivariate independence with continuous margins, based on the joint Gaussian limit of full-sample and internally reranked subsample interaction vectors.
- Added `BEAST.asymptotic.calibrate()` and `BEAST.asymptotic()`, with saved calibration objects and explicit compatibility checks.
- Preserved existing BET 0.5.4 full-basis BEAST behavior and native scientific implementation.
- Added lightweight regression tests and corrected documentation and source-package compiler metadata.
