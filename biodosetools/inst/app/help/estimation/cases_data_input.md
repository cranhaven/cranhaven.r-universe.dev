#### Data format

The only required data are:
- The cell distributions of dicentrics `CX`, where `X` can be `0, 1, 2, 3`...

The rest of columns will be calculated automatically after clicking <button class="btn btn-default action-button inputs-button shiny-bound-input small-action-button"  type="button">Calculate parameters</button>:

- `N` is the total number of cells.
- `X` is the number of aberrations.
- `y` is the observed yield, and `y_err` is its standard error.
- `DI` is the dispersion index $\sigma^{2}/y$.
- `u` is the $u$-value, which for a Poisson distribution should be unity.
