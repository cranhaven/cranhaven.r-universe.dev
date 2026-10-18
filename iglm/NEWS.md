# iglm 1.2.6

## Descriptive and Goodness-of-Fit Capabilities

* **Unified Degree Distributions**: The separate `degree_distribution` and `spilloverdegree_distribution` methods of class `iglm.data` have been merged into a unified, flexible `degree_distribution(x_i, x_j, y_i, y_j, mode)` method. Users can compute degree distributions on induced subnetworks where $x_i, x_j, y_i,$ and $y_j$ are constrained to specific values.
  * Attributes can be filtered using exact values (e.g., `0` or `1`), discrete sets, continuous above-mean indicators (where $x_i > \bar{x}$ for continuous/count covariates), or custom filtering functions.
  * Users can isolate directional spillover pathways: outgoing ties quantify spillover sending capacity, while incoming ties quantify spillover exposure.
* **Extended Network Descriptive Distributions**: The `edgewise_shared_partner_distribution`, `dyadwise_shared_partner_distribution`, and `geodesic_distance_distribution` methods of `iglm.data` now also accept `x_i`, `x_j`, `y_i`, `y_j`, and `mode` arguments to evaluate structural properties on attribute-conditioned subgraphs.
* **Enhanced Goodness-of-Fit (`assess`)**: Because the `assess` method evaluates descriptive statistics specified on the left-hand side of formula terms, the entire goodness-of-fit toolbox seamlessly extends to these generalized, attribute-conditioned distributions.
* **Global vs. Local Modes**: Added the `mode` parameter (`"global"` by default, or `"local"`) to all descriptive distributions, allowing users to restrict evaluations either to all network ties or solely to ties between units with overlapping neighborhoods ($\mathcal{N}_i \cap \mathcal{N}_j \neq \emptyset$).

## Consistency and Alignment of Terminology

* **`family` Parameter**: Renamed the `type` parameter (`type_x`, `type_y`) to `family` (`family_x`, `family_y`) to align with standard R conventions (such as `glm()`) for specifying the conditional distributions of attributes.
* **Unified Unit Terminology**: Standardized terminology throughout the package code, function arguments (e.g., `n_units` replacing `n_actor`), and documentation to consistently refer to network entities as "units" rather than mixing "actors" and "units".
* **Shorthand Distribution Aliases**: Added convenient shorthand methods to the `iglm.data` class:
  * `deg_dist()` for `degree_distribution()`
  * `geo_dist()` for `geodesic_distance_distribution()`
  * `esp_dist()` for `edgewise_shared_partner_distribution()`
  * `dsp_dist()` for `dyadwise_shared_partner_distribution()`

## Labeling of Attributes and Terms

* **Custom Attribute and Network Labels**: Users can now supply custom labels via `label_x`, `label_y`, and `label_z` when constructing `iglm.data` objects.
* **Enhanced Output Formatting**: `summary()` and `print()` methods for both `iglm` fitted models and `iglm.data` objects display informative, domain-specific labels instead of generic canonical identifiers (e.g., `attribute_republican` and `spillover_republican_turnout`), with options to toggle between labeled and canonical names.

## Robustness and Input Validation

* **Formula Scope Verification**: Added environment checks ensuring that all objects referenced in model formulas exist in the calling environment before estimation begins.
* **Numerical Safeguards**: Strengthened optimization and estimation routines to prevent silent propagation of `NA` or `NaN` values.
* **Model Term Validation (`check.IglmTerm`)**: Introduced comprehensive validation helpers for model terms, checking mandatory arguments, expected data types (scalars, matrices), allowed categorical values, and positional-to-named argument mappings.
* **Descriptive Formula Traps**: Added informative error messages when formulas are incorrectly specified (e.g., passing raw vectors or lists instead of an `iglm.data` object on the LHS).



