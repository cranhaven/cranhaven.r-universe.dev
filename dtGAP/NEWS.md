# dtGAP 0.0.2

## New Features

* `save_dtGAP()`: Export dtGAP visualizations to PNG, PDF, or SVG files with
  customizable dimensions and resolution.
* `select_vars` parameter in `dtGAP()`: Display-only variable filtering for
  heatmap panels while the tree is trained on all variables.
* `fit` and `user_var_imp` parameters in `dtGAP()`: Supply a pre-trained tree
  (rpart, party, or caret) directly, with automatic model detection and
  optional user-provided variable importance.
* `interactive` parameter in `dtGAP()`: Launch a Shiny-based interactive
  heatmap viewer via InteractiveComplexHeatmap.
* `compare_dtGAP()`: Compare two or more tree models side-by-side on a single
  wide canvas.
* Random forest extension via `partykit::cforest`:
    - `train_rf()`: Train a conditional random forest and extract variable
      importance.
    - `rf_summary()`: Ensemble-level summary with variable importance barplot
      and representative tree identification.
    - `rf_dtGAP()`: Visualize any individual tree from the forest using the
      full dtGAP pipeline.

## Bug Fixes

* Fix `formatC()` error in `prepare_tree()` for cforest trees that lack
  numeric p-values.

## Documentation

* Updated vignette with usage examples for all new features.
* Updated README with new feature descriptions and code examples.

# dtGAP 0.0.1

* Initial release.
* Core `dtGAP()` function for supervised decision-tree visualization using the
  GAP framework.
* Support for rpart, party, C5.0, and caret tree models.
* Confusion matrix maps, decision-tree matrix maps, predicted class membership
  maps, and evaluation panels.
* Row and column proximity with seriation support.
* Classification and regression tasks.
* Seven built-in datasets: Psychosis_Disorder, penguins, wine, diabetes,
  train_covid/test_covid, wine_quality_red, galaxy.
